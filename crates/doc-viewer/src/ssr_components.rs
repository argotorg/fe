//! SSR-friendly Leptos components for documentation
//!
//! These components are designed for server-side rendering without signals.
//! They take plain data and render to HTML.

use leptos::prelude::*;

use crate::markdown::render_markdown;
use crate::model::{
    DocChild, DocChildKind, DocContent, DocIndex, DocItem, DocItemKind, DocModuleTree,
    DocTraitImpl,
};

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
                            Some(item) => view! { <DocItemViewSSR item=item index=index.clone() supports_goto_source=supports_goto_source /> }.into_any(),
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

    // Group items by kind
    let items_by_kind = {
        use std::collections::BTreeMap;
        let mut groups: BTreeMap<u8, (DocItemKind, Vec<_>)> = BTreeMap::new();
        for item in &module.items {
            let order = item.kind.display_order();
            groups
                .entry(order)
                .or_insert_with(|| (item.kind, Vec::new()))
                .1
                .push(item);
        }
        groups.into_values().collect::<Vec<_>>()
    };

    view! {
        <div class=class>
            <a href=format!("/doc/{}", module.url_path()) class="nav-module">
                {module.name.clone()}
            </a>
            {if !module.items.is_empty() {
                Some(view! {
                    <div class="nav-groups">
                        {items_by_kind.into_iter().map(|(kind, items)| {
                            view! {
                                <div class="nav-kind-group">
                                    <h4 class="nav-kind-header">{kind.plural_name()}</h4>
                                    <ul class="nav-items">
                                        {items.into_iter().map(|item| {
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
                                </div>
                            }
                        }).collect_view()}
                    </div>
                })
            } else {
                None
            }}
        </div>
    }
}

/// Documentation item view for SSR
#[component]
pub fn DocItemViewSSR(item: DocItem, index: DocIndex, supports_goto_source: bool) -> impl IntoView {
    let has_docs = item.docs.is_some();
    let has_children = !item.children.is_empty();
    let has_source = item.source.is_some();
    let item_path = item.url_path();
    let is_module = item.kind == DocItemKind::Module;

    // For modules, find the module tree to get member items
    let module_items = if is_module {
        find_module_items(&index.modules, &item.path)
    } else {
        None
    };

    // Build breadcrumb segments from path
    let breadcrumbs: Vec<(String, String)> = {
        let segments: Vec<&str> = item.path.split("::").collect();
        let mut result = Vec::new();
        let mut accumulated_path = String::new();

        for (i, segment) in segments.iter().enumerate() {
            if i > 0 {
                accumulated_path.push_str("::");
            }
            accumulated_path.push_str(segment);

            // Last segment is current item (no link needed)
            let is_last = i == segments.len() - 1;
            let url = if is_last {
                String::new() // No link for current item
            } else {
                // Link to parent as module
                format!("/doc/{}/mod", accumulated_path)
            };

            result.push((segment.to_string(), url));
        }
        result
    };

    view! {
        <article class="doc-item">
            // Breadcrumb path navigation
            <nav class="breadcrumb">
                {breadcrumbs.into_iter().enumerate().map(|(i, (name, url))| {
                    let separator = if i > 0 { Some(view! { <span class="breadcrumb-sep">"::"</span> }) } else { None };
                    if url.is_empty() {
                        view! {
                            <>
                                {separator}
                                <span class="breadcrumb-current">{name}</span>
                            </>
                        }.into_any()
                    } else {
                        view! {
                            <>
                                {separator}
                                <a href=url class="breadcrumb-link">{name}</a>
                            </>
                        }.into_any()
                    }
                }).collect_view()}
            </nav>

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

            // Module members section (for modules only)
            {if let Some(items) = module_items {
                Some(view! { <DocModuleMembersSSR items=items /> })
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

            {if !item.trait_impls.is_empty() {
                Some(view! { <DocTraitImplsSSR impls=item.trait_impls.clone() /> })
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

/// Children (fields, methods, variants) grouped by kind
#[component]
fn DocChildrenSSR(children: Vec<DocChild>) -> impl IntoView {
    // Group children by kind
    let children_by_kind = {
        use std::collections::BTreeMap;
        let mut groups: BTreeMap<u8, (DocChildKind, Vec<DocChild>)> = BTreeMap::new();
        for child in children {
            let order = child.kind.display_order();
            groups
                .entry(order)
                .or_insert_with(|| (child.kind, Vec::new()))
                .1
                .push(child);
        }
        groups.into_values().collect::<Vec<_>>()
    };

    view! {
        <div class="children-sections">
            {children_by_kind.into_iter().map(|(kind, items)| {
                view! {
                    <section class="children-section">
                        <h2>{kind.plural_name()}</h2>
                        <dl class="members">
                            {items.into_iter().map(|child| {
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
                    </section>
                }
            }).collect_view()}
        </div>
    }
}

/// Implementations section for types (trait impls and inherent impls)
/// Inspired by rustdoc's collapsible impl blocks with method details
#[component]
fn DocTraitImplsSSR(impls: Vec<DocTraitImpl>) -> impl IntoView {
    // Separate trait impls from inherent impls
    let (trait_impls, inherent_impls): (Vec<_>, Vec<_>) =
        impls.into_iter().partition(|i| !i.trait_name.is_empty());

    let has_trait_impls = !trait_impls.is_empty();
    let has_inherent_impls = !inherent_impls.is_empty();

    // Render each section separately to avoid deep type nesting
    let inherent_section = if has_inherent_impls {
        view! {
            <section class="inherent-impls" id="implementations">
                <h2>"Implementations"<a href="#implementations" class="anchor">"\u{00a7}"</a></h2>
                <div class="impl-list">
                    {inherent_impls.into_iter().enumerate().map(|(idx, impl_)| {
                        let anchor_id = format!("impl-{}", idx);
                        view! { <ImplBlockSSR impl_=impl_ anchor_id=anchor_id /> }.into_any()
                    }).collect_view()}
                </div>
            </section>
        }.into_any()
    } else {
        view! { <></> }.into_any()
    };

    let trait_section = if has_trait_impls {
        view! {
            <section class="trait-impls" id="trait-implementations">
                <h2>"Trait Implementations"<a href="#trait-implementations" class="anchor">"\u{00a7}"</a></h2>
                <div class="impl-list">
                    {trait_impls.into_iter().map(|impl_| {
                        let anchor_id = format!("impl-{}", impl_.trait_name.replace(['<', '>', ' ', ','], "_"));
                        view! { <ImplBlockSSR impl_=impl_ anchor_id=anchor_id /> }.into_any()
                    }).collect_view()}
                </div>
            </section>
        }.into_any()
    } else {
        view! { <></> }.into_any()
    };

    view! {
        <div class="implementations">
            {inherent_section}
            {trait_section}
        </div>
    }
}

/// A single impl block with collapsible methods (rustdoc-style)
#[component]
fn ImplBlockSSR(impl_: DocTraitImpl, anchor_id: String) -> impl IntoView {
    let has_methods = !impl_.methods.is_empty();
    let is_trait_impl = !impl_.trait_name.is_empty();

    // Header text: "impl TraitName" for trait impls, full signature for inherent
    let header_display = if is_trait_impl {
        format!("impl {}", impl_.trait_name)
    } else {
        impl_.signature.clone()
    };

    // Pre-render optional sections to break type nesting
    let signature_section = if is_trait_impl {
        view! {
            <pre class="rust impl-signature"><code>{impl_.signature.clone()}</code></pre>
        }.into_any()
    } else {
        view! { <></> }.into_any()
    };

    let methods_section = if has_methods {
        view! {
            <div class="impl-items">
                {impl_.methods.into_iter().map(|method| {
                    let method_anchor = format!("method.{}", method.name);
                    view! { <MethodItemSSR method=method anchor_id=method_anchor /> }.into_any()
                }).collect_view()}
            </div>
        }.into_any()
    } else {
        view! { <></> }.into_any()
    };

    view! {
        <details class="impl-block toggle" open=true id=anchor_id.clone()>
            <summary>
                <span class="impl-header">
                    <a href=format!("#{}", anchor_id) class="anchor">"\u{00a7}"</a>
                    <h3><code>{header_display}</code></h3>
                </span>
            </summary>
            <div class="impl-content">
                {signature_section}
                {methods_section}
            </div>
        </details>
    }
}

/// A single method with collapsible docs (rustdoc-style)
#[component]
fn MethodItemSSR(method: crate::model::DocImplMethod, anchor_id: String) -> impl IntoView {
    let has_docs = method.docs.is_some();

    let docblock = if let Some(docs) = method.docs {
        view! { <div class="method-docblock">{docs}</div> }.into_any()
    } else {
        view! { <></> }.into_any()
    };

    view! {
        <details class="method-item toggle" open=has_docs id=anchor_id.clone()>
            <summary>
                <div class="method-header">
                    <a href=format!("#{}", anchor_id) class="anchor">"\u{00a7}"</a>
                    <h4 class="code-header"><code>{method.signature}</code></h4>
                </div>
            </summary>
            {docblock}
        </details>
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

/// Find module items from the module tree by path
fn find_module_items(
    modules: &[DocModuleTree],
    path: &str,
) -> Option<Vec<crate::model::DocModuleItem>> {
    for module in modules {
        if module.path == path {
            return Some(module.items.clone());
        }
        // Recursively search children
        if let Some(items) = find_module_items(&module.children, path) {
            return Some(items);
        }
    }
    None
}

/// Module members section showing links to items defined in the module
#[component]
fn DocModuleMembersSSR(items: Vec<crate::model::DocModuleItem>) -> impl IntoView {
    if items.is_empty() {
        return view! { <></> }.into_any();
    }

    // Group items by kind
    let items_by_kind = {
        use std::collections::BTreeMap;
        let mut groups: BTreeMap<u8, (DocItemKind, Vec<_>)> = BTreeMap::new();
        for item in items {
            let order = item.kind.display_order();
            groups
                .entry(order)
                .or_insert_with(|| (item.kind, Vec::new()))
                .1
                .push(item);
        }
        groups.into_values().collect::<Vec<_>>()
    };

    view! {
        <section class="module-members">
            <h2>"Items"</h2>
            {items_by_kind.into_iter().map(|(kind, items)| {
                view! {
                    <div class="member-group">
                        <h3>{kind.plural_name()}</h3>
                        <ul class="member-list">
                            {items.into_iter().map(|item| {
                                let url = format!("/doc/{}", item.url_path());
                                let badge_class = format!("kind-badge {}", item.kind.as_str());
                                let kind_str = item.kind.as_str();
                                view! {
                                    <li>
                                        <a href=url>
                                            <span class=badge_class>
                                                {kind_str}
                                            </span>
                                            " "
                                            <code>{item.name}</code>
                                        </a>
                                    </li>
                                }
                            }).collect_view()}
                        </ul>
                    </div>
                }
            }).collect_view()}
        </section>
    }.into_any()
}
