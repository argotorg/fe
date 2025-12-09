//! SSR-friendly Leptos components for documentation
//!
//! These components are designed for server-side rendering without signals.
//! They take plain data and render to HTML.

use leptos::prelude::*;

use crate::markdown::render_markdown;
use crate::model::{DocChild, DocContent, DocIndex, DocItem, DocModuleTree};

/// Full documentation page component for SSR with islands
#[component]
pub fn DocPage(
    /// Page title
    title: String,
    /// The documentation index
    index: DocIndex,
    /// Currently selected item path
    current_path: String,
    /// Whether editor supports goto source
    supports_goto_source: bool,
) -> impl IntoView {
    let item = index.find_by_url(&current_path).cloned();

    view! {
        <!DOCTYPE html>
        <html lang="en">
            <head>
                <meta charset="UTF-8"/>
                <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
                <title>{title}</title>
                <style>{include_str!("styles.css")}</style>
            </head>
            <body>
                <div class="doc-layout">
                    <DocSidebarSSR
                        modules=index.modules.clone()
                        current_path=current_path.clone()
                    />
                    <main class="doc-content">
                        // SSR content - live updates handled by scripts.js
                        {match item {
                            Some(item) => view! { <DocItemViewSSR item=item supports_goto_source=supports_goto_source /> }.into_any(),
                            None => view! {
                                <div class="not-found">
                                    <h1>"Select an item"</h1>
                                    <p>"Choose an item from the sidebar to view its documentation."</p>
                                </div>
                            }.into_any(),
                        }}
                    </main>
                </div>
                // Live update scripts (pure JS, no WASM needed)
                <script>{include_str!("scripts.js")}</script>
            </body>
        </html>
    }
}


/// Sidebar navigation for SSR
#[component]
pub fn DocSidebarSSR(
    modules: Vec<DocModuleTree>,
    current_path: String,
) -> impl IntoView {
    view! {
        <nav class="doc-sidebar">
            <div class="sidebar-header">
                <h1><a href="/">"Fe Docs"</a></h1>
                <label class="auto-follow-toggle" title="Auto-follow cursor position in editor">
                    <input type="checkbox" id="auto-follow" onchange="toggleAutoFollow(this.checked)"/>
                    <span class="toggle-slider"></span>
                    <span class="toggle-label">"Follow cursor"</span>
                </label>
                <input type="search" id="search" placeholder="Search..." oninput="doSearch(this.value)"/>
                <div id="search-results"></div>
            </div>
            <div class="sidebar-nav">
                {modules.into_iter().map(|module| {
                    let is_current = module.url_path() == current_path;
                    view! {
                        <DocModuleNav
                            module=module
                            current_path=current_path.clone()
                            is_current=is_current
                        />
                    }
                }).collect_view()}
            </div>
        </nav>
    }
}

/// Module navigation item
#[component]
fn DocModuleNav(
    module: DocModuleTree,
    current_path: String,
    is_current: bool,
) -> impl IntoView {
    let class = if is_current { "nav-item current" } else { "nav-item" };

    view! {
        <div class=class>
            <a href=format!("/doc/{}", module.url_path()) class="nav-module">
                {module.name.clone()}
            </a>
            {if !module.items.is_empty() {
                Some(view! {
                    <ul class="nav-items">
                        {module.items.iter().map(|item| {
                            let item_current = item.url_path() == current_path;
                            let item_class = if item_current { "current" } else { "" };
                            view! {
                                <li class=item_class>
                                    <a href=format!("/doc/{}", item.url_path())>
                                        <span class=format!("kind-badge {}", item.kind.as_str())>
                                            {item.kind.as_str()}
                                        </span>
                                        " "
                                        {item.name.clone()}
                                    </a>
                                </li>
                            }
                        }).collect_view()}
                    </ul>
                })
            } else {
                None
            }}
        </div>
    }
}

/// Documentation item view for SSR
#[component]
pub fn DocItemViewSSR(item: DocItem, supports_goto_source: bool) -> impl IntoView {
    let has_docs = item.docs.is_some();
    let has_children = !item.children.is_empty();
    let has_source = item.source.is_some();
    let item_path = item.url_path();

    view! {
        <article class="doc-item">
            <div class="item-header">
                <span class=format!("kind-badge {}", item.kind.as_str())>
                    {item.kind.display_name()}
                </span>
                <h1>{item.name.clone()}</h1>
            </div>

            <pre class="signature"><code>{item.signature.clone()}</code></pre>

            {if has_docs {
                item.docs.map(|docs| view! { <DocContentSSR content=docs /> })
            } else {
                None
            }}

            {if has_children {
                Some(view! { <DocChildrenSSR children=item.children.clone() /> })
            } else {
                None
            }}

            {if has_source {
                item.source.map(|src| view! {
                    <div class="source-link">
                        "Defined in " <code>{src.display_file.clone()}</code>
                        " at line " {src.line}
                        {if supports_goto_source {
                            Some(view! {
                                <button
                                    class="goto-source-btn"
                                    data-path=item_path.clone()
                                    onclick="gotoSource(this.dataset.path)"
                                >
                                    "Go to Source"
                                </button>
                            })
                        } else {
                            None
                        }}
                    </div>
                })
            } else {
                None
            }}
        </article>
    }
}

/// Documentation content (body + sections)
#[component]
fn DocContentSSR(content: DocContent) -> impl IntoView {
    let body_html = render_markdown(&content.body);

    view! {
        <div class="docs" inner_html=body_html />
    }
}

/// Children (fields, methods, variants)
#[component]
fn DocChildrenSSR(children: Vec<DocChild>) -> impl IntoView {
    view! {
        <h2>"Members"</h2>
        <dl class="members">
            {children.into_iter().map(|child| {
                let kind_name = child.kind.display_name().to_lowercase();
                let kind_class = format!("kind-badge {}", &kind_name);
                view! {
                    <dt>
                        <span class=kind_class>{kind_name}</span>
                        " "
                        <code>{child.name.clone()}</code>
                    </dt>
                    {child.docs.map(|docs| view! { <dd>{docs}</dd> })}
                }
            }).collect_view()}
        </dl>
    }
}

/// Index view showing all items grouped by kind
#[component]
#[allow(unused_variables)]
fn DocIndexView(index: DocIndex) -> impl IntoView {
    view! {
        <h1>"Fe Documentation"</h1>
        <p>"Select an item from the sidebar to view its documentation."</p>
    }
}

/// 404 Not found view
#[component]
pub fn DocNotFoundSSR(path: String, index: DocIndex) -> impl IntoView {
    view! {
        <!DOCTYPE html>
        <html lang="en">
            <head>
                <meta charset="UTF-8"/>
                <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
                <title>"Not Found - Fe Documentation"</title>
                <style>{include_str!("styles.css")}</style>
            </head>
            <body>
                <div class="doc-layout">
                    <DocSidebarSSR modules=index.modules.clone() current_path="".to_string() />
                    <main class="doc-content">
                        <div class="not-found">
                            <h1>"Item Not Found"</h1>
                            <p>"The documentation item " <code>{path}</code> " could not be found."</p>
                            <p class="not-found-hint">"It may have been renamed or removed."</p>
                        </div>
                    </main>
                </div>
                <script>{include_str!("scripts.js")}</script>
            </body>
        </html>
    }
}
