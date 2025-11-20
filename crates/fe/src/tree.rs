use std::{
    collections::{HashMap, HashSet, VecDeque},
    io::{self, stdout},
    sync::mpsc::{self, Receiver, Sender},
    thread,
    time::{Duration, Instant},
};

use camino::{Utf8Path, Utf8PathBuf};
use common::{
    cache::remote_git_cache_dir,
    config::Config,
    dependencies::{DependencyArguments, DependencyLocation, ExternalDependencyEdge, RemoteFiles},
};
use crossterm::{
    event::{self, Event as CrosstermEvent, KeyCode, KeyEvent, KeyModifiers},
    execute,
    terminal::{self, EnterAlternateScreen, LeaveAlternateScreen},
};
use ratatui::{
    Terminal,
    backend::CrosstermBackend,
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, List, ListItem, ListState, Paragraph, Wrap},
};
use resolver::{
    ResolutionHandler, Resolver,
    files::{FilesResolver, FilesResource},
    git::{GitDependencyDescription, GitResolver, GitResource},
    graph::{
        DiGraph, GraphResolutionHandler, GraphResolver, GraphResolverImpl, NodeIndex,
        petgraph::{self, visit::EdgeRef},
    },
};
use smol_str::SmolStr;
use url::Url;

const SPINNER_FRAMES: &[&str] = &["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"];

pub fn print_tree(path: &Utf8PathBuf) {
    let mut resolver = tree_resolver();
    let canonical_path = match path.canonicalize_utf8() {
        Ok(path) => path,
        Err(_) => {
            eprintln!("Error: Invalid or non-existent directory path: {path}");
            return;
        }
    };

    let ingot_url = match Url::from_directory_path(canonical_path.as_str()) {
        Ok(url) => url,
        Err(_) => {
            eprintln!("Error: Invalid directory path: {path}");
            return;
        }
    };

    let (tx, rx) = mpsc::channel();
    let worker_path = ingot_url.clone();

    let worker = thread::spawn(move || {
        run_tree_worker(worker_path, &mut resolver, tx);
    });

    if let Err(err) = run_ui(rx) {
        eprintln!("Failed to render tree UI: {err}");
    }

    let _ = worker.join();
}

type IngotGraphResolver =
    resolver::graph::GraphResolverImpl<FilesResolver, TreeHandler, (SmolStr, DependencyArguments)>;

fn tree_resolver() -> IngotGraphResolver {
    let files_resolver = FilesResolver::new().with_required_file("fe.toml");
    resolver::graph::GraphResolverImpl::new(files_resolver)
}

fn run_tree_worker(ingot_url: Url, resolver: &mut IngotGraphResolver, tx: Sender<TreeEvent>) {
    let mut handler = TreeHandler::new(tx.clone(), ingot_url.clone());

    tx.send(TreeEvent::Initialized {
        root: ingot_url.clone(),
    })
    .ok();
    handler.send_snapshot();

    match resolver.graph_resolve(&mut handler, &ingot_url) {
        Ok(()) => {}
        Err(err) => {
            tx.send(TreeEvent::LocalResolutionFailed(err.to_string()))
                .ok();
        }
    }

    for unresolvable_node in resolver.take_diagnostics() {
        tx.send(TreeEvent::Diagnostic(format!(
            "❌ Failed to resolve ingot dependency '{}': {}",
            unresolvable_node.0, unresolvable_node.1
        )))
        .ok();
    }

    for diagnostic in resolver.node_resolver.take_diagnostics() {
        tx.send(TreeEvent::Diagnostic(format!(
            "❌ File resolution error: {diagnostic}"
        )))
        .ok();
    }

    let remote_requests = handler.take_remote_dependencies();
    resolve_remote_tree(remote_requests, &ingot_url, tx.clone());

    tx.send(TreeEvent::Finished).ok();
}

fn run_ui(rx: Receiver<TreeEvent>) -> io::Result<()> {
    terminal::enable_raw_mode()?;
    let mut stdout = stdout();
    execute!(stdout, EnterAlternateScreen)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;
    terminal.clear()?;

    let mut app = TreeApp::new();
    let tick_rate = Duration::from_millis(100);
    let mut last_tick = Instant::now();

    loop {
        while let Ok(event) = rx.try_recv() {
            app.handle_worker_event(event);
            app.invalidate_rows();
        }

        terminal.draw(|frame| app.draw(frame))?;

        if app.should_quit {
            break;
        }

        let timeout = tick_rate
            .checked_sub(last_tick.elapsed())
            .unwrap_or(Duration::from_secs(0));
        if event::poll(timeout)? {
            if let CrosstermEvent::Key(key) = event::read()? {
                app.on_key(key);
            }
        }

        if last_tick.elapsed() >= tick_rate {
            app.on_tick();
            last_tick = Instant::now();
        }
    }

    terminal::disable_raw_mode()?;
    execute!(terminal.backend_mut(), LeaveAlternateScreen)?;
    terminal.show_cursor()?;
    Ok(())
}

#[derive(Clone, Default)]
struct NodeMetadata {
    name: Option<String>,
    version: Option<String>,
}

impl NodeMetadata {
    fn from_config(config: &Config) -> Self {
        Self {
            name: config.metadata.name.as_deref().map(ToString::to_string),
            version: config.metadata.version.as_ref().map(ToString::to_string),
        }
    }

    fn invalid() -> Self {
        Self::default()
    }
}

#[derive(Clone)]
struct TreeSnapshot {
    root: Url,
    graph: DiGraph<Url, (SmolStr, DependencyArguments)>,
    metadata: HashMap<Url, NodeMetadata>,
    pending_remote: Vec<PendingRemote>,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
struct RemoteKey {
    parent: Url,
    alias: SmolStr,
    description: String,
}

impl RemoteKey {
    fn new(parent: Url, alias: SmolStr, description: String) -> Self {
        Self {
            parent,
            alias,
            description,
        }
    }
}

#[derive(Clone)]
struct PendingRemote {
    parent: Url,
    alias: SmolStr,
    description: String,
}

#[derive(Clone)]
struct RemoteGraphData {
    key: RemoteKey,
    root: Url,
    graph: DiGraph<Url, (SmolStr, DependencyArguments)>,
    metadata: HashMap<Url, NodeMetadata>,
}

struct RemoteStatusUpdate {
    key: RemoteKey,
    state: RemoteFetchState,
}

#[derive(Clone)]
enum RemoteFetchState {
    Pending,
    Resolving,
    Resolved,
    Failed(String),
}

enum TreeEvent {
    Initialized { root: Url },
    LocalGraphUpdated(TreeSnapshot),
    Diagnostic(String),
    LocalResolutionFailed(String),
    RemoteStatus(RemoteStatusUpdate),
    RemoteGraphReady(RemoteGraphData),
    Finished,
}

struct TreeHandler {
    metadata: HashMap<Url, NodeMetadata>,
    remote_dependencies: Vec<ExternalDependencyEdge>,
    pending_remote: Vec<PendingRemote>,
    root: Url,
    sender: Sender<TreeEvent>,
    graph: DiGraph<Url, (SmolStr, DependencyArguments)>,
    node_indices: HashMap<Url, NodeIndex>,
}

impl TreeHandler {
    fn new(sender: Sender<TreeEvent>, root: Url) -> Self {
        let mut graph = DiGraph::new();
        let mut node_indices = HashMap::new();
        let root_idx = graph.add_node(root.clone());
        node_indices.insert(root.clone(), root_idx);

        Self {
            metadata: HashMap::new(),
            remote_dependencies: Vec::new(),
            pending_remote: Vec::new(),
            root,
            sender,
            graph,
            node_indices,
        }
    }

    fn snapshot(&self) -> TreeSnapshot {
        TreeSnapshot {
            root: self.root.clone(),
            graph: self.graph.clone(),
            metadata: self.metadata.clone(),
            pending_remote: self.pending_remote.clone(),
        }
    }

    fn take_remote_dependencies(&mut self) -> Vec<ExternalDependencyEdge> {
        std::mem::take(&mut self.remote_dependencies)
    }

    fn send_snapshot(&self) {
        let snapshot = self.snapshot();
        let _ = self.sender.send(TreeEvent::LocalGraphUpdated(snapshot));
    }

    fn ensure_node(&mut self, url: &Url) -> NodeIndex {
        if let Some(&idx) = self.node_indices.get(url) {
            idx
        } else {
            let idx = self.graph.add_node(url.clone());
            self.node_indices.insert(url.clone(), idx);
            idx
        }
    }

    fn add_edge(
        &mut self,
        parent: &Url,
        child: &Url,
        alias: SmolStr,
        arguments: DependencyArguments,
    ) {
        let parent_idx = self.ensure_node(parent);
        let child_idx = self.ensure_node(child);
        let existing = self
            .graph
            .edges_connecting(parent_idx, child_idx)
            .any(|edge| edge.weight().0 == alias);
        if !existing {
            self.graph
                .add_edge(parent_idx, child_idx, (alias, arguments));
        }
    }
}

impl ResolutionHandler<FilesResolver> for TreeHandler {
    type Item = Vec<(Url, (SmolStr, DependencyArguments))>;

    fn handle_resolution(&mut self, ingot_url: &Url, resource: FilesResource) -> Self::Item {
        if let Some(config_file) = resource
            .files
            .iter()
            .find(|f| f.path.file_name() == Some("fe.toml"))
        {
            match Config::parse(&config_file.content) {
                Ok(config) => {
                    for diagnostic in &config.diagnostics {
                        let _ = self.sender.send(TreeEvent::Diagnostic(format!(
                            "❌ Config validation error at {ingot_url}: {diagnostic}"
                        )));
                    }

                    self.metadata
                        .insert(ingot_url.clone(), NodeMetadata::from_config(&config));
                    self.ensure_node(ingot_url);

                    let mut edges = Vec::new();
                    for dependency in config.dependencies(ingot_url) {
                        let alias = dependency.alias;
                        let arguments = dependency.arguments;
                        match dependency.location {
                            DependencyLocation::Local(local) => {
                                self.add_edge(
                                    ingot_url,
                                    &local.url,
                                    alias.clone(),
                                    arguments.clone(),
                                );
                                edges.push((local.url, (alias, arguments)));
                            }
                            DependencyLocation::Remote(remote) => {
                                let description = describe_remote_target(&remote);
                                self.pending_remote.push(PendingRemote {
                                    parent: ingot_url.clone(),
                                    alias: alias.clone(),
                                    description: description.clone(),
                                });
                                self.remote_dependencies.push(ExternalDependencyEdge {
                                    parent: ingot_url.clone(),
                                    alias,
                                    arguments,
                                    remote,
                                });
                            }
                        }
                    }
                    self.send_snapshot();
                    edges
                }
                Err(err) => {
                    let _ = self.sender.send(TreeEvent::Diagnostic(format!(
                        "❌ Invalid fe.toml file at {ingot_url}: {err}"
                    )));
                    self.metadata
                        .insert(ingot_url.clone(), NodeMetadata::invalid());
                    self.ensure_node(ingot_url);
                    self.send_snapshot();
                    vec![]
                }
            }
        } else {
            self.metadata
                .insert(ingot_url.clone(), NodeMetadata::invalid());
            self.ensure_node(ingot_url);
            self.send_snapshot();
            vec![]
        }
    }
}

impl GraphResolutionHandler<Url, DiGraph<Url, (SmolStr, DependencyArguments)>> for TreeHandler {
    type Item = ();

    fn handle_graph_resolution(
        &mut self,
        _ingot_url: &Url,
        _graph: DiGraph<Url, (SmolStr, DependencyArguments)>,
    ) -> Self::Item {
    }
}

fn resolve_remote_tree(
    requests: Vec<ExternalDependencyEdge>,
    ingot_url: &Url,
    tx: Sender<TreeEvent>,
) {
    if requests.is_empty() {
        return;
    }

    let checkout_root = tree_remote_checkout_root(ingot_url);
    let git_resolver = GitResolver::new(checkout_root);
    let mut graph_resolver: GraphResolverImpl<
        GitResolver,
        TreeRemoteHandler,
        (SmolStr, DependencyArguments),
    > = GraphResolverImpl::new(git_resolver);

    for request in requests {
        let description = tree_git_description_from_request(&request);
        let key = RemoteKey::new(
            request.parent.clone(),
            request.alias.clone(),
            description.to_string(),
        );

        let _ = tx.send(TreeEvent::RemoteStatus(RemoteStatusUpdate {
            key: key.clone(),
            state: RemoteFetchState::Resolving,
        }));

        let mut handler = TreeRemoteHandler::new();
        match graph_resolver.graph_resolve(&mut handler, &description) {
            Ok(graph) => {
                if let Some(root_url) = handler.ingot_url(&description).cloned() {
                    let metadata = handler.take_metadata();
                    let _ = tx.send(TreeEvent::RemoteGraphReady(RemoteGraphData {
                        key: key.clone(),
                        root: root_url,
                        graph,
                        metadata,
                    }));
                    let _ = tx.send(TreeEvent::RemoteStatus(RemoteStatusUpdate {
                        key,
                        state: RemoteFetchState::Resolved,
                    }));
                }
            }
            Err(err) => {
                let _ = tx.send(TreeEvent::RemoteStatus(RemoteStatusUpdate {
                    key: key.clone(),
                    state: RemoteFetchState::Failed(err.to_string()),
                }));
            }
        }

        for diagnostic in handler.take_diagnostics() {
            let _ = tx.send(TreeEvent::Diagnostic(format!("❌ {diagnostic}")));
        }

        for diagnostic in graph_resolver.take_diagnostics() {
            let _ = tx.send(TreeEvent::Diagnostic(format!(
                "❌ Remote dependency error for {}: {}",
                diagnostic.0, diagnostic.1
            )));
        }

        for diagnostic in graph_resolver.node_resolver.take_diagnostics() {
            let _ = tx.send(TreeEvent::Diagnostic(format!("ℹ️ {diagnostic}")));
        }
    }
}

fn tree_remote_checkout_root(ingot_url: &Url) -> Utf8PathBuf {
    if let Some(root) = remote_git_cache_dir() {
        return root;
    }

    let mut ingot_path = Utf8PathBuf::from_path_buf(
        ingot_url
            .to_file_path()
            .expect("ingot URL should map to a local path"),
    )
    .expect("ingot path should be valid UTF-8");
    ingot_path.push(".fe");
    ingot_path.push("git");
    ingot_path
}

fn tree_git_description_from_request(request: &ExternalDependencyEdge) -> GitDependencyDescription {
    let mut description = GitDependencyDescription::new(
        request.remote.source.clone(),
        request.remote.rev.to_string(),
    );
    if let Some(path) = request.remote.path.clone() {
        description = description.with_path(path);
    }
    description
}

fn describe_remote_target(remote: &RemoteFiles) -> String {
    match remote.path.as_ref() {
        Some(path) => format!("{}#{} ({})", remote.source, remote.rev, path),
        None => format!("{}#{}", remote.source, remote.rev),
    }
}

struct TreeRemoteHandler {
    metadata: HashMap<Url, NodeMetadata>,
    ingot_urls: HashMap<GitDependencyDescription, Url>,
    diagnostics: Vec<String>,
}

impl TreeRemoteHandler {
    fn new() -> Self {
        Self {
            metadata: HashMap::new(),
            ingot_urls: HashMap::new(),
            diagnostics: Vec::new(),
        }
    }

    fn ingot_url(&self, description: &GitDependencyDescription) -> Option<&Url> {
        self.ingot_urls.get(description)
    }

    fn take_diagnostics(&mut self) -> Vec<String> {
        std::mem::take(&mut self.diagnostics)
    }

    fn take_metadata(&mut self) -> HashMap<Url, NodeMetadata> {
        std::mem::take(&mut self.metadata)
    }
}

impl ResolutionHandler<GitResolver> for TreeRemoteHandler {
    type Item = Vec<(GitDependencyDescription, (SmolStr, DependencyArguments))>;

    fn handle_resolution(
        &mut self,
        description: &GitDependencyDescription,
        resource: GitResource,
    ) -> Self::Item {
        self.ingot_urls
            .insert(description.clone(), resource.ingot_url.clone());

        let config_path = resource.ingot_path.join("fe.toml");
        let config_content = match std::fs::read_to_string(&config_path) {
            Ok(content) => content,
            Err(error) => {
                self.diagnostics.push(format!(
                    "Remote file error at {}: {error}",
                    resource.ingot_url
                ));
                return vec![];
            }
        };

        let config = match Config::parse(&config_content) {
            Ok(config) => config,
            Err(error) => {
                self.diagnostics.push(format!(
                    "Invalid remote fe.toml file in {}: {error}",
                    resource.ingot_url
                ));
                return vec![];
            }
        };

        if !config.diagnostics.is_empty() {
            for diagnostic in &config.diagnostics {
                self.diagnostics.push(format!(
                    "Erroneous remote fe.toml file in {}: {diagnostic}",
                    resource.ingot_url
                ));
            }
        }

        self.metadata.insert(
            resource.ingot_url.clone(),
            NodeMetadata::from_config(&config),
        );

        let mut dependencies = Vec::new();
        for dependency in config.dependencies(&resource.ingot_url) {
            let alias = dependency.alias;
            let arguments = dependency.arguments;
            match dependency.location {
                DependencyLocation::Local(local) => match tree_relative_path_within_checkout(
                    resource.checkout_path.as_path(),
                    &local.url,
                ) {
                    Ok(relative_path) => {
                        let mut next_description = GitDependencyDescription::new(
                            description.source.clone(),
                            description.rev.clone(),
                        );
                        if let Some(path) = relative_path {
                            next_description = next_description.with_path(path);
                        }
                        dependencies.push((next_description, (alias, arguments)));
                    }
                    Err(error) => {
                        self.diagnostics.push(format!(
                            "Remote dependency '{}' in {} points outside the repository: {error}",
                            alias, resource.ingot_url
                        ));
                    }
                },
                DependencyLocation::Remote(remote) => {
                    let mut next_description = GitDependencyDescription::new(
                        remote.source.clone(),
                        remote.rev.to_string(),
                    );
                    if let Some(path) = remote.path.clone() {
                        next_description = next_description.with_path(path);
                    }
                    dependencies.push((next_description, (alias, arguments)));
                }
            }
        }

        dependencies
    }
}

impl
    GraphResolutionHandler<
        GitDependencyDescription,
        DiGraph<GitDependencyDescription, (SmolStr, DependencyArguments)>,
    > for TreeRemoteHandler
{
    type Item = DiGraph<Url, (SmolStr, DependencyArguments)>;

    fn handle_graph_resolution(
        &mut self,
        _description: &GitDependencyDescription,
        graph: DiGraph<GitDependencyDescription, (SmolStr, DependencyArguments)>,
    ) -> Self::Item {
        let mut converted = DiGraph::new();
        let mut node_map = HashMap::new();

        for node_idx in graph.node_indices() {
            if let Some(url) = self.ingot_urls.get(&graph[node_idx]) {
                let new_idx = converted.add_node(url.clone());
                node_map.insert(node_idx, new_idx);
            }
        }

        for edge in graph.edge_references() {
            if let (Some(&from_idx), Some(&to_idx)) =
                (node_map.get(&edge.source()), node_map.get(&edge.target()))
            {
                converted.add_edge(from_idx, to_idx, edge.weight().clone());
            }
        }

        converted
    }
}

fn tree_relative_path_within_checkout(
    checkout_path: &Utf8Path,
    target_url: &Url,
) -> Result<Option<Utf8PathBuf>, String> {
    let path_buf = target_url
        .to_file_path()
        .map_err(|_| "target URL is not a file URL".to_string())?;
    let utf8_path = Utf8PathBuf::from_path_buf(path_buf)
        .map_err(|_| "non UTF-8 path encountered in remote dependency".to_string())?;
    let relative = utf8_path
        .strip_prefix(checkout_path)
        .map_err(|_| "path escapes the checked-out repository".to_string())?;
    if relative.as_str().is_empty() {
        Ok(None)
    } else {
        Ok(Some(relative.to_owned()))
    }
}

struct TreeApp {
    should_quit: bool,
    list_state: ListState,
    rows: Vec<TreeRow>,
    selected: usize,
    collapsed: HashSet<TreeItemId>,
    root: Option<Url>,
    local_graph: Option<GraphCache>,
    metadata: HashMap<Url, NodeMetadata>,
    remote_nodes: HashMap<RemoteKey, RemoteNodeState>,
    logs: VecDeque<String>,
    finished: bool,
    resolution_error: Option<String>,
    needs_recompute: bool,
    spinner_index: usize,
}

impl TreeApp {
    fn new() -> Self {
        Self {
            should_quit: false,
            list_state: ListState::default(),
            rows: Vec::new(),
            selected: 0,
            collapsed: HashSet::new(),
            root: None,
            local_graph: None,
            metadata: HashMap::new(),
            remote_nodes: HashMap::new(),
            logs: VecDeque::new(),
            finished: false,
            resolution_error: None,
            needs_recompute: true,
            spinner_index: 0,
        }
    }

    fn draw(&mut self, frame: &mut ratatui::Frame<'_>) {
        self.ensure_rows();

        let layout = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(4),
                Constraint::Min(5),
                Constraint::Length(7),
            ])
            .split(frame.size());

        frame.render_widget(self.instructions_block(), layout[0]);
        self.render_tree(frame, layout[1]);
        frame.render_widget(self.logs_block(), layout[2]);
    }

    fn instructions_block(&self) -> Paragraph<'_> {
        let mut lines = Vec::new();
        if let Some(root) = &self.root {
            lines.push(Line::from(vec![
                Span::raw("Root: "),
                Span::styled(root.as_str(), Style::default().fg(Color::Cyan)),
            ]));
        } else {
            lines.push(Line::from("Resolving root ingot..."));
        }

        if let Some(err) = &self.resolution_error {
            lines.push(Line::from(Span::styled(
                format!("Error: {err}"),
                Style::default().fg(Color::Red),
            )));
        } else if self.finished {
            lines.push(Line::from(Span::styled(
                "Resolution complete. Press q to exit.",
                Style::default().fg(Color::Green),
            )));
        } else {
            lines.push(Line::from(vec![
                Span::styled(self.spinner(), Style::default().fg(Color::Yellow)),
                Span::raw(" Resolving dependencies..."),
            ]));
        }

        lines.push(Line::from(
            "Controls: ↑/↓ or j/k to move • ←/→ or h/l to collapse/expand • Enter/Space to toggle • q to quit",
        ));

        Paragraph::new(lines)
            .block(Block::default().borders(Borders::ALL).title("Status"))
            .wrap(Wrap { trim: true })
    }

    fn render_tree(&mut self, frame: &mut ratatui::Frame<'_>, area: Rect) {
        if self.rows.is_empty() {
            let message = if self.finished {
                "No dependencies found.".to_string()
            } else {
                format!("{} Waiting for dependency data...", self.spinner())
            };
            let empty = Paragraph::new(message)
                .block(
                    Block::default()
                        .borders(Borders::ALL)
                        .title("Dependency Tree"),
                )
                .wrap(Wrap { trim: true });
            frame.render_widget(empty, area);
            return;
        }

        let items: Vec<ListItem> = self
            .rows
            .iter()
            .map(|row| ListItem::new(Line::from(row.text.clone())).style(row.style))
            .collect();

        self.list_state
            .select(Some(self.selected.min(self.rows.len().saturating_sub(1))));

        let tree = List::new(items)
            .block(
                Block::default()
                    .borders(Borders::ALL)
                    .title("Dependency Tree"),
            )
            .highlight_style(
                Style::default()
                    .fg(Color::Yellow)
                    .add_modifier(Modifier::BOLD),
            )
            .highlight_symbol("▶ ");

        frame.render_stateful_widget(tree, area, &mut self.list_state);
    }

    fn logs_block(&self) -> Paragraph<'static> {
        let lines: Vec<Line> = self
            .logs
            .iter()
            .rev()
            .take(5)
            .map(|line| Line::from(line.clone()))
            .collect();

        Paragraph::new(lines)
            .block(Block::default().borders(Borders::ALL).title("Messages"))
            .wrap(Wrap { trim: true })
    }

    fn handle_worker_event(&mut self, event: TreeEvent) {
        match event {
            TreeEvent::Initialized { root } => {
                self.root = Some(root);
            }
            TreeEvent::LocalGraphUpdated(snapshot) => {
                self.root = Some(snapshot.root.clone());
                self.metadata = snapshot.metadata;
                self.local_graph = Some(GraphCache::from_graph(snapshot.graph));
                for remote in snapshot.pending_remote {
                    let key = RemoteKey::new(
                        remote.parent.clone(),
                        remote.alias.clone(),
                        remote.description.clone(),
                    );
                    self.remote_nodes
                        .entry(key)
                        .or_insert_with(|| RemoteNodeState::pending(remote));
                }
            }
            TreeEvent::Diagnostic(msg) => {
                self.push_log(msg);
            }
            TreeEvent::LocalResolutionFailed(msg) => {
                self.resolution_error = Some(msg.clone());
                self.push_log(msg);
            }
            TreeEvent::RemoteStatus(update) => {
                if let Some(state) = self.remote_nodes.get_mut(&update.key) {
                    state.status = update.state;
                }
            }
            TreeEvent::RemoteGraphReady(data) => {
                let cache =
                    RemoteGraphCache::from_graph(data.root.clone(), data.graph, data.metadata);
                let entry = self
                    .remote_nodes
                    .entry(data.key.clone())
                    .or_insert_with(|| {
                        RemoteNodeState::pending(PendingRemote {
                            parent: data.key.parent.clone(),
                            alias: data.key.alias.clone(),
                            description: data.key.description.clone(),
                        })
                    });
                entry.graph = Some(cache);
                entry.status = RemoteFetchState::Resolved;
            }
            TreeEvent::Finished => {
                self.finished = true;
            }
        }
    }

    fn on_key(&mut self, key: KeyEvent) {
        if key.code == KeyCode::Char('c') && key.modifiers.contains(KeyModifiers::CONTROL) {
            self.should_quit = true;
            return;
        }

        match key.code {
            KeyCode::Char('q') => self.should_quit = true,
            KeyCode::Down | KeyCode::Char('j') => self.move_selection(1),
            KeyCode::Up | KeyCode::Char('k') => self.move_selection(-1),
            KeyCode::Left | KeyCode::Char('h') => self.adjust_collapse(true),
            KeyCode::Right | KeyCode::Char('l') => self.adjust_collapse(false),
            KeyCode::Enter | KeyCode::Char(' ') => self.toggle_collapse(),
            _ => {}
        }
    }

    fn on_tick(&mut self) {
        self.spinner_index = (self.spinner_index + 1) % SPINNER_FRAMES.len();
        self.invalidate_rows();
    }

    fn spinner(&self) -> &'static str {
        SPINNER_FRAMES[self.spinner_index % SPINNER_FRAMES.len()]
    }

    fn move_selection(&mut self, delta: isize) {
        if self.rows.is_empty() {
            return;
        }
        let len = self.rows.len() as isize;
        let mut next = self.selected as isize + delta;
        if next < 0 {
            next = 0;
        } else if next >= len {
            next = len - 1;
        }
        self.selected = next as usize;
    }

    fn toggle_collapse(&mut self) {
        if let Some(row) = self.rows.get(self.selected) {
            if row.has_children {
                if !self.collapsed.insert(row.id.clone()) {
                    self.collapsed.remove(&row.id);
                }
                self.invalidate_rows();
            }
        }
    }

    fn adjust_collapse(&mut self, collapse: bool) {
        if let Some(row) = self.rows.get(self.selected) {
            if collapse {
                if row.has_children {
                    self.collapsed.insert(row.id.clone());
                    self.invalidate_rows();
                }
            } else if self.collapsed.remove(&row.id) {
                self.invalidate_rows();
            }
        }
    }

    fn push_log(&mut self, message: String) {
        if self.logs.len() >= 50 {
            self.logs.pop_front();
        }
        self.logs.push_back(message);
    }

    fn invalidate_rows(&mut self) {
        self.needs_recompute = true;
    }

    fn ensure_rows(&mut self) {
        if self.needs_recompute {
            self.recompute_rows();
            self.needs_recompute = false;
        }
    }

    fn recompute_rows(&mut self) {
        self.rows = self.build_rows();
        if self.selected >= self.rows.len() {
            self.selected = self.rows.len().saturating_sub(1);
        }
    }

    fn build_rows(&self) -> Vec<TreeRow> {
        let mut rows = Vec::new();
        if let (Some(root), Some(graph)) = (&self.root, &self.local_graph) {
            let mut seen = HashSet::new();
            if graph.node_index(root).is_some() {
                self.collect_local_rows(&mut rows, root, TreePrefix::Root, None, &mut seen, graph);
            }
        }
        rows
    }

    fn collect_local_rows(
        &self,
        rows: &mut Vec<TreeRow>,
        node_url: &Url,
        prefix: TreePrefix,
        alias: Option<SmolStr>,
        seen: &mut HashSet<Url>,
        graph: &GraphCache,
    ) {
        let id = TreeItemId::Local(node_url.clone());
        let will_close_cycle = seen.contains(node_url);
        let is_in_cycle = graph
            .node_index(node_url)
            .map(|idx| graph.cycle_nodes.contains(&idx))
            .unwrap_or(false);
        let label = self.node_label(node_url, alias.as_deref(), will_close_cycle);
        let mut style = Style::default();
        if is_in_cycle {
            style = style.fg(Color::Red);
        }

        let has_children =
            self.local_has_children(node_url, graph) || self.has_remote_children(node_url);

        rows.push(TreeRow {
            id: id.clone(),
            text: format!("{}{}", prefix.new_prefix(), label),
            style,
            has_children,
        });

        if will_close_cycle || self.collapsed.contains(&id) {
            return;
        }

        seen.insert(node_url.clone());

        let mut children = self.local_children(node_url, graph);
        children.sort_by(|a, b| a.alias.cmp(&b.alias));

        let remote_children = self.remote_children(node_url);
        let total = children.len() + remote_children.len();
        let mut processed = 0usize;
        let indent = prefix.child_indent();

        for child in children {
            processed += 1;
            let next_prefix = if processed == total {
                TreePrefix::Last(indent.clone())
            } else {
                TreePrefix::Fork(indent.clone())
            };
            self.collect_local_rows(
                rows,
                &child.url,
                next_prefix,
                Some(child.alias),
                seen,
                graph,
            );
        }

        for remote_key in remote_children {
            processed += 1;
            let next_prefix = if processed == total {
                TreePrefix::Last(indent.clone())
            } else {
                TreePrefix::Fork(indent.clone())
            };
            self.collect_remote_rows(rows, remote_key, next_prefix);
        }

        seen.remove(node_url);
    }

    fn collect_remote_rows(&self, rows: &mut Vec<TreeRow>, key: RemoteKey, prefix: TreePrefix) {
        if let Some(state) = self.remote_nodes.get(&key) {
            let label = match &state.status {
                RemoteFetchState::Pending => {
                    format!("{} 🌐 {} (queued)", self.spinner(), state.info.alias)
                }
                RemoteFetchState::Resolving => {
                    format!(
                        "{} 🌐 {} resolving {}",
                        self.spinner(),
                        state.info.alias,
                        state.info.description
                    )
                }
                RemoteFetchState::Resolved => {
                    format!(
                        "🌐 {} resolved {}",
                        state.info.alias, state.info.description
                    )
                }
                RemoteFetchState::Failed(err) => {
                    format!("🌐 {} failed: {err}", state.info.alias)
                }
            };
            let id = TreeItemId::RemotePlaceholder(key.clone());
            let has_children =
                matches!(state.status, RemoteFetchState::Resolved) && state.graph.is_some();
            rows.push(TreeRow {
                id: id.clone(),
                text: format!("{}{}", prefix.new_prefix(), label),
                style: Style::default().fg(Color::Blue),
                has_children,
            });

            if self.collapsed.contains(&id) {
                return;
            }

            if let Some(graph) = &state.graph {
                let child_prefix = TreePrefix::Last(prefix.child_indent());
                let mut seen = HashSet::new();
                self.collect_remote_node(
                    rows,
                    &key,
                    &graph.root,
                    child_prefix,
                    None,
                    &mut seen,
                    graph,
                );
            }
        }
    }

    fn collect_remote_node(
        &self,
        rows: &mut Vec<TreeRow>,
        key: &RemoteKey,
        node_url: &Url,
        prefix: TreePrefix,
        alias: Option<SmolStr>,
        seen: &mut HashSet<Url>,
        graph: &RemoteGraphCache,
    ) {
        let id = TreeItemId::RemoteNode(key.clone(), node_url.clone());
        let will_close_cycle = seen.contains(node_url);
        let is_in_cycle = graph
            .node_index(node_url)
            .map(|idx| graph.cycle_nodes.contains(&idx))
            .unwrap_or(false);
        let label = graph.node_label(node_url, alias.as_deref(), will_close_cycle);
        let mut style = Style::default();
        if is_in_cycle {
            style = style.fg(Color::Red);
        }

        let has_children = graph.has_children(node_url);

        rows.push(TreeRow {
            id: id.clone(),
            text: format!("{}{}", prefix.new_prefix(), label),
            style,
            has_children,
        });

        if will_close_cycle || self.collapsed.contains(&id) {
            return;
        }

        seen.insert(node_url.clone());

        let mut children = graph.children(node_url);
        children.sort_by(|a, b| a.alias.cmp(&b.alias));
        let indent = prefix.child_indent();

        for (index, child) in children.iter().enumerate() {
            let child_prefix = if index == children.len() - 1 {
                TreePrefix::Last(indent.clone())
            } else {
                TreePrefix::Fork(indent.clone())
            };
            self.collect_remote_node(
                rows,
                key,
                &child.url,
                child_prefix,
                Some(child.alias.clone()),
                seen,
                graph,
            );
        }

        seen.remove(node_url);
    }

    fn node_label(&self, url: &Url, alias: Option<&str>, will_close_cycle: bool) -> String {
        let metadata = self.metadata.get(url);
        let base = if let Some(info) = metadata {
            let name = info.name.as_deref().unwrap_or("null");
            let version = info.version.as_deref().unwrap_or("null");
            match alias {
                Some(alias) if alias != name => format!("➖ {name} as {alias} v{version}"),
                _ => format!("➖ {name} v{version}"),
            }
        } else {
            "➖ [invalid fe.toml]".to_string()
        };

        if will_close_cycle {
            format!("{base} 🔄 [cycle]")
        } else {
            base
        }
    }

    fn local_children(&self, url: &Url, graph: &GraphCache) -> Vec<LocalChild> {
        graph
            .children(url)
            .into_iter()
            .map(|child| LocalChild {
                url: child.url,
                alias: child.alias,
            })
            .collect()
    }

    fn has_remote_children(&self, url: &Url) -> bool {
        self.remote_nodes
            .values()
            .any(|remote| remote.info.parent == *url)
    }

    fn local_has_children(&self, url: &Url, graph: &GraphCache) -> bool {
        graph.has_children(url)
    }

    fn remote_children(&self, url: &Url) -> Vec<RemoteKey> {
        self.remote_nodes
            .iter()
            .filter_map(|(key, state)| {
                if state.info.parent == *url {
                    Some(key.clone())
                } else {
                    None
                }
            })
            .collect()
    }
}

struct GraphCache {
    graph: DiGraph<Url, (SmolStr, DependencyArguments)>,
    index_map: HashMap<Url, NodeIndex>,
    cycle_nodes: HashSet<NodeIndex>,
}

impl GraphCache {
    fn from_graph(graph: DiGraph<Url, (SmolStr, DependencyArguments)>) -> Self {
        let mut index_map = HashMap::new();
        for idx in graph.node_indices() {
            index_map.insert(graph[idx].clone(), idx);
        }
        let cycles = find_cycle_nodes(&graph);
        Self {
            graph,
            index_map,
            cycle_nodes: cycles,
        }
    }

    fn node_index(&self, url: &Url) -> Option<NodeIndex> {
        self.index_map.get(url).copied()
    }

    fn has_children(&self, url: &Url) -> bool {
        self.node_index(url)
            .map(|idx| {
                self.graph
                    .edges_directed(idx, petgraph::Direction::Outgoing)
                    .next()
                    .is_some()
            })
            .unwrap_or(false)
    }

    fn children(&self, url: &Url) -> Vec<LocalChild> {
        if let Some(idx) = self.node_index(url) {
            self.graph
                .edges_directed(idx, petgraph::Direction::Outgoing)
                .map(|edge| LocalChild {
                    url: self.graph[edge.target()].clone(),
                    alias: edge.weight().0.clone(),
                })
                .collect()
        } else {
            Vec::new()
        }
    }
}

struct RemoteNodeState {
    info: PendingRemote,
    status: RemoteFetchState,
    graph: Option<RemoteGraphCache>,
}

impl RemoteNodeState {
    fn pending(info: PendingRemote) -> Self {
        Self {
            info,
            status: RemoteFetchState::Pending,
            graph: None,
        }
    }
}

struct RemoteGraphCache {
    root: Url,
    graph: DiGraph<Url, (SmolStr, DependencyArguments)>,
    index_map: HashMap<Url, NodeIndex>,
    cycle_nodes: HashSet<NodeIndex>,
    metadata: HashMap<Url, NodeMetadata>,
}

impl RemoteGraphCache {
    fn from_graph(
        root: Url,
        graph: DiGraph<Url, (SmolStr, DependencyArguments)>,
        metadata: HashMap<Url, NodeMetadata>,
    ) -> Self {
        let mut index_map = HashMap::new();
        for idx in graph.node_indices() {
            index_map.insert(graph[idx].clone(), idx);
        }
        let cycles = find_cycle_nodes(&graph);
        Self {
            root,
            graph,
            index_map,
            cycle_nodes: cycles,
            metadata,
        }
    }

    fn node_index(&self, url: &Url) -> Option<NodeIndex> {
        self.index_map.get(url).copied()
    }

    fn has_children(&self, url: &Url) -> bool {
        self.node_index(url)
            .map(|idx| {
                self.graph
                    .edges_directed(idx, petgraph::Direction::Outgoing)
                    .next()
                    .is_some()
            })
            .unwrap_or(false)
    }

    fn children(&self, url: &Url) -> Vec<LocalChild> {
        if let Some(idx) = self.node_index(url) {
            self.graph
                .edges_directed(idx, petgraph::Direction::Outgoing)
                .map(|edge| LocalChild {
                    url: self.graph[edge.target()].clone(),
                    alias: edge.weight().0.clone(),
                })
                .collect()
        } else {
            Vec::new()
        }
    }

    fn node_label(&self, url: &Url, alias: Option<&str>, will_close_cycle: bool) -> String {
        let metadata = self.metadata.get(url);
        let base = if let Some(info) = metadata {
            let name = info.name.as_deref().unwrap_or("null");
            let version = info.version.as_deref().unwrap_or("null");
            match alias {
                Some(alias) if alias != name => format!("➖ {name} as {alias} v{version}"),
                _ => format!("➖ {name} v{version}"),
            }
        } else {
            "➖ [invalid fe.toml]".to_string()
        };

        if will_close_cycle {
            format!("{base} 🔄 [cycle]")
        } else {
            base
        }
    }
}

struct LocalChild {
    url: Url,
    alias: SmolStr,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
enum TreeItemId {
    Local(Url),
    RemotePlaceholder(RemoteKey),
    RemoteNode(RemoteKey, Url),
}

struct TreeRow {
    id: TreeItemId,
    text: String,
    style: Style,
    has_children: bool,
}

#[derive(Clone)]
enum TreePrefix {
    Root,
    Fork(String),
    Last(String),
}

impl TreePrefix {
    fn new_prefix(&self) -> String {
        match self {
            TreePrefix::Root => "".to_string(),
            TreePrefix::Fork(p) => format!("{p}├── "),
            TreePrefix::Last(p) => format!("{p}└── "),
        }
    }

    fn child_indent(&self) -> String {
        match self {
            TreePrefix::Root => "".to_string(),
            TreePrefix::Fork(p) => format!("{p}│   "),
            TreePrefix::Last(p) => format!("{p}    "),
        }
    }
}

fn find_cycle_nodes<N, E>(graph: &DiGraph<N, E>) -> HashSet<NodeIndex> {
    let mut cycles = HashSet::new();
    for scc in petgraph::algo::kosaraju_scc(graph) {
        if scc.len() > 1 {
            cycles.extend(scc);
        } else {
            let node = scc[0];
            if graph
                .neighbors_directed(node, petgraph::Direction::Outgoing)
                .any(|n| n == node)
            {
                cycles.insert(node);
            }
        }
    }
    cycles
}
