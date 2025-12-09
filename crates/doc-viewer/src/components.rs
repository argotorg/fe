//! Leptos components for documentation UI
//!
//! These components use fine-grained reactivity for efficient rendering.
//! The design follows an FRP-style approach where data flows through signals.

use leptos::prelude::*;

use crate::model::{
    DocChild, DocChildKind, DocContent, DocIndex, DocItem, DocItemKind, DocModuleTree,
    DocVisibility,
};
use crate::markdown::render_markdown;

/// Root documentation app component
#[component]
pub fn DocApp(
    /// The documentation index containing all items
    index: DocIndex,
    /// Currently selected item path
    #[prop(into)]
    initial_path: String,
) -> impl IntoView {
    // Reactive signal for current path
    let (current_path, set_current_path) = signal(initial_path);

    // Clone index for the memo
    let index_for_item = index.clone();
    let current_item = Memo::new(move |_| {
        let path = current_path.get();
        index_for_item.find_by_path(&path).cloned()
    });

    // Search state
    let (search_query, set_search_query) = signal(String::new());
    let index_for_search = index.clone();
    let search_results = Memo::new(move |_| {
        let query = search_query.get();
        if query.len() < 2 {
            Vec::new()
        } else {
            index_for_search
                .search(&query)
                .into_iter()
                .cloned()
                .collect::<Vec<_>>()
        }
    });

    view! {
        <div class="doc-app">
            <DocSidebar
                modules=index.modules.clone()
                current_path=current_path
                on_select=move |path| set_current_path.set(path)
            />

            <main class="doc-main">
                <DocSearch
                    query=search_query
                    on_input=move |q| set_search_query.set(q)
                    results=search_results
                    on_select=move |path| {
                        set_search_query.set(String::new());
                        set_current_path.set(path);
                    }
                />

                {move || {
                    current_item.get().map(|item| view! { <DocItemView item=item /> })
                }}
            </main>
        </div>
    }
}

/// Sidebar navigation component
#[component]
pub fn DocSidebar(
    modules: Vec<DocModuleTree>,
    current_path: ReadSignal<String>,
    on_select: impl Fn(String) + Clone + Send + Sync + 'static,
) -> impl IntoView {
    // Flatten the tree structure for rendering (avoids recursive component issues)
    let flattened = flatten_module_tree(&modules, 0);

    view! {
        <nav class="doc-sidebar">
            <div class="sidebar-header">
                <h1><a href="/">"Fe Docs"</a></h1>
            </div>
            <div class="sidebar-nav">
                {flattened
                    .into_iter()
                    .map(|entry| {
                        let on_select_clone = on_select.clone();
                        view! {
                            <DocNavEntry
                                entry=entry
                                current_path=current_path
                                on_select=on_select_clone
                            />
                        }
                    })
                    .collect_view()}
            </div>
        </nav>
    }
}

/// Flattened navigation entry
#[derive(Clone)]
struct NavEntry {
    name: String,
    path: String,
    kind: NavEntryKind,
    depth: usize,
}

#[derive(Clone, Copy)]
enum NavEntryKind {
    Module,
    Item(DocItemKind),
}

/// Flatten the module tree into a list of entries
fn flatten_module_tree(modules: &[DocModuleTree], depth: usize) -> Vec<NavEntry> {
    let mut entries = Vec::new();

    for module in modules {
        entries.push(NavEntry {
            name: module.name.clone(),
            path: module.path.clone(),
            kind: NavEntryKind::Module,
            depth,
        });

        // Add child modules
        entries.extend(flatten_module_tree(&module.children, depth + 1));

        // Add direct items
        for item in &module.items {
            entries.push(NavEntry {
                name: item.name.clone(),
                path: item.path.clone(),
                kind: NavEntryKind::Item(item.kind),
                depth: depth + 1,
            });
        }
    }

    entries
}

/// A single navigation entry (non-recursive)
#[component]
fn DocNavEntry(
    entry: NavEntry,
    current_path: ReadSignal<String>,
    on_select: impl Fn(String) + Clone + Send + Sync + 'static,
) -> impl IntoView {
    let path = entry.path.clone();
    let path_for_active = path.clone();
    let path_for_click = path.clone();

    let indent = format!("{}rem", entry.depth as f32 * 0.75);

    view! {
        <a
            class="doc-nav-entry"
            class:active=move || current_path.get() == path_for_active
            class:module=matches!(entry.kind, NavEntryKind::Module)
            href="#"
            on:click=move |e: leptos::ev::MouseEvent| {
                e.prevent_default();
                on_select(path_for_click.clone());
            }
            style:padding-left=indent
        >
            {match entry.kind {
                NavEntryKind::Module => view! { <span class="doc-kind-badge mod">"mod"</span> }.into_any(),
                NavEntryKind::Item(kind) => view! { <DocKindBadge kind=kind /> }.into_any(),
            }}
            <span class="doc-nav-name">{entry.name}</span>
        </a>
    }
}

/// Search component with results dropdown
#[component]
pub fn DocSearch(
    query: ReadSignal<String>,
    on_input: impl Fn(String) + Send + Sync + 'static,
    results: Memo<Vec<DocItem>>,
    on_select: impl Fn(String) + Clone + Send + Sync + 'static,
) -> impl IntoView {
    let (focused, set_focused) = signal(false);

    view! {
        <div class="doc-search">
            <input
                type="text"
                class="doc-search-input"
                placeholder="Search documentation..."
                prop:value=move || query.get()
                on:input=move |e| on_input(event_target_value(&e))
                on:focus=move |_| set_focused.set(true)
                on:blur=move |_| set_focused.set(false)
            />

            {move || {
                let show = focused.get() && !results.get().is_empty();
                if show {
                    let on_select_clone = on_select.clone();
                    Some(view! {
                        <div class="doc-search-results">
                            {results
                                .get()
                                .into_iter()
                                .map(|item| {
                                    let path = item.path.clone();
                                    let on_select_item = on_select_clone.clone();
                                    view! {
                                        <button
                                            class="doc-search-result"
                                            on:mousedown=move |_| on_select_item(path.clone())
                                        >
                                            <DocKindBadge kind=item.kind />
                                            <span class="doc-search-result-name">{item.name.clone()}</span>
                                            <span class="doc-search-result-path">{item.path.clone()}</span>
                                        </button>
                                    }
                                })
                                .collect_view()}
                        </div>
                    })
                } else {
                    None
                }
            }}
        </div>
    }
}

/// Main item documentation view
#[component]
pub fn DocItemView(item: DocItem) -> impl IntoView {
    let has_docs = item.docs.is_some();
    let has_children = !item.children.is_empty();
    let has_source = item.source.is_some();

    let docs = item.docs.clone();
    let children = item.children.clone();
    let source = item.source.clone();

    view! {
        <article class="doc-item">
            <header class="doc-item-header">
                <div class="doc-item-title">
                    <DocKindBadge kind=item.kind />
                    <h1>{item.name.clone()}</h1>
                    <DocVisibilityBadge visibility=item.visibility />
                </div>
                <div class="doc-item-path">{item.path.clone()}</div>
            </header>

            <section class="doc-signature">
                <DocCodeBlock code=item.signature.clone() lang="fe".to_string() />
            </section>

            {if has_docs {
                docs.map(|d| view! { <DocContentView content=d /> })
            } else {
                None
            }}

            {if has_children {
                Some(view! { <DocChildrenView children=children /> })
            } else {
                None
            }}

            {if has_source {
                source.map(|src| {
                    view! {
                        <footer class="doc-source">
                            <a href=format!("{}#L{}", src.file, src.line) target="_blank">
                                "View source"
                            </a>
                        </footer>
                    }
                })
            } else {
                None
            }}
        </article>
    }
}

/// Rendered documentation content
#[component]
pub fn DocContentView(content: DocContent) -> impl IntoView {
    let body_html = render_markdown(&content.body);
    let sections = content.sections;

    view! {
        <div class="doc-content">
            <div class="doc-body" inner_html=body_html />

            {if !sections.is_empty() {
                Some(view! {
                    <div class="doc-sections">
                        {sections
                            .into_iter()
                            .map(|section| {
                                let section_html = render_markdown(&section.content);
                                view! {
                                    <div class="doc-section">
                                        <h3>{section.name}</h3>
                                        <div inner_html=section_html />
                                    </div>
                                }
                            })
                            .collect_view()}
                    </div>
                })
            } else {
                None
            }}
        </div>
    }
}

/// Children (fields, methods, variants) view
#[component]
pub fn DocChildrenView(children: Vec<DocChild>) -> impl IntoView {
    // Group children by kind
    let fields: Vec<_> = children
        .iter()
        .filter(|c| c.kind == DocChildKind::Field)
        .cloned()
        .collect();
    let variants: Vec<_> = children
        .iter()
        .filter(|c| c.kind == DocChildKind::Variant)
        .cloned()
        .collect();
    let methods: Vec<_> = children
        .iter()
        .filter(|c| c.kind == DocChildKind::Method)
        .cloned()
        .collect();
    let assoc_types: Vec<_> = children
        .iter()
        .filter(|c| c.kind == DocChildKind::AssocType)
        .cloned()
        .collect();

    view! {
        <div class="doc-children">
            {if !fields.is_empty() {
                Some(view! { <DocChildSection title="Fields" children=fields /> })
            } else {
                None
            }}

            {if !variants.is_empty() {
                Some(view! { <DocChildSection title="Variants" children=variants /> })
            } else {
                None
            }}

            {if !assoc_types.is_empty() {
                Some(view! { <DocChildSection title="Associated Types" children=assoc_types /> })
            } else {
                None
            }}

            {if !methods.is_empty() {
                Some(view! { <DocChildSection title="Methods" children=methods /> })
            } else {
                None
            }}
        </div>
    }
}

/// A section of child items
#[component]
fn DocChildSection(title: &'static str, children: Vec<DocChild>) -> impl IntoView {
    view! {
        <section class="doc-child-section">
            <h2>{title}</h2>
            <div class="doc-child-list">
                {children.into_iter().map(|child| view! { <DocChildItem child=child /> }).collect_view()}
            </div>
        </section>
    }
}

/// Individual child item
#[component]
fn DocChildItem(child: DocChild) -> impl IntoView {
    let (expanded, set_expanded) = signal(false);
    let has_docs = child.docs.is_some();
    let is_private = child.visibility == DocVisibility::Private;
    let signature = child.signature.clone();
    let docs = child.docs.clone();

    view! {
        <div class="doc-child-item">
            <div class="doc-child-header">
                {if has_docs {
                    Some(view! {
                        <button
                            class="doc-expand-btn"
                            on:click=move |_| set_expanded.update(|e| *e = !*e)
                        >
                            {move || if expanded.get() { "▼" } else { "▶" }}
                        </button>
                    })
                } else {
                    None
                }}

                <code class="doc-child-signature">{signature}</code>

                {if is_private {
                    Some(view! { <span class="doc-visibility-badge private">"private"</span> })
                } else {
                    None
                }}
            </div>

            {move || {
                if expanded.get() && has_docs {
                    docs.clone().map(|d| {
                        let html = render_markdown(&d);
                        view! { <div class="doc-child-docs" inner_html=html /> }
                    })
                } else {
                    None
                }
            }}
        </div>
    }
}

/// Code block with syntax highlighting
#[component]
pub fn DocCodeBlock(code: String, lang: String) -> impl IntoView {
    view! {
        <pre class="doc-code-block">
            <code class=format!("language-{}", lang)>
                {code}
            </code>
        </pre>
    }
}

/// Kind badge component
#[component]
pub fn DocKindBadge(kind: DocItemKind) -> impl IntoView {
    let class = format!("doc-kind-badge {}", kind.as_str());
    view! {
        <span class=class>{kind.as_str()}</span>
    }
}

/// Visibility badge component
#[component]
pub fn DocVisibilityBadge(visibility: DocVisibility) -> impl IntoView {
    if visibility == DocVisibility::Private {
        Some(view! { <span class="doc-visibility-badge private">"private"</span> })
    } else {
        None
    }
}

/// Not found fallback
#[component]
pub fn DocNotFound() -> impl IntoView {
    view! {
        <div class="doc-not-found">
            <h1>"Item not found"</h1>
            <p>"The requested documentation item could not be found."</p>
        </div>
    }
}
