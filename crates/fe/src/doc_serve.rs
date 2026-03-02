use std::sync::Arc;

use axum::{Router, response::Html, routing::get};
use fe_web::model::DocIndex;

pub struct DocServeConfig {
    pub port: u16,
    pub host: String,
}

pub async fn serve_docs(
    index: DocIndex,
    config: DocServeConfig,
    scip_json: Option<String>,
    source_link_base: Option<String>,
) -> std::io::Result<()> {
    let html = Arc::new(generate_html(
        &index,
        scip_json.as_deref(),
        source_link_base.as_deref(),
    ));

    let app = Router::new().fallback(get(move || {
        let html = Arc::clone(&html);
        async move { Html((*html).clone()) }
    }));

    let addr = format!("{}:{}", config.host, config.port);
    let listener = tokio::net::TcpListener::bind(&addr)
        .await
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::AddrInUse, e))?;
    axum::serve(listener, app)
        .await
        .map_err(std::io::Error::other)
}

fn generate_html(
    index: &DocIndex,
    scip_json: Option<&str>,
    source_link_base: Option<&str>,
) -> String {
    let mut value = serde_json::to_value(index).expect("serialize DocIndex");
    fe_web::static_site::inject_html_bodies(&mut value);
    let json = serde_json::to_string(&value).expect("serialize JSON");
    let title = if let Some(root) = index.modules.first() {
        format!("{} — Fe Documentation", root.name)
    } else {
        "Fe Documentation".to_string()
    };
    fe_web::assets::html_shell_full(&title, &json, scip_json, source_link_base)
}

#[cfg(test)]
mod tests {
    use super::*;
    use fe_web::model::*;

    #[test]
    fn generate_html_produces_valid_output() {
        let mut index = DocIndex::new();
        index.modules = vec![DocModuleTree {
            name: "mylib".into(),
            path: "mylib".into(),
            children: vec![],
            items: vec![],
        }];

        let html = generate_html(&index, None, None);
        assert!(html.contains("<!DOCTYPE html>"));
        assert!(html.contains("mylib"));
        assert!(html.contains("Fe Documentation"));
        assert!(html.contains("FE_DOC_INDEX"));
    }

    #[test]
    fn generate_html_empty_index() {
        let index = DocIndex::new();
        let html = generate_html(&index, None, None);
        assert!(html.contains("<!DOCTYPE html>"));
        assert!(html.contains("Fe Documentation"));
    }
}
