use std::sync::Arc;

use axum::{response::Html, routing::get, Router};
use fe_web::model::DocIndex;

pub struct DocServeConfig {
    pub port: u16,
    pub host: String,
}

pub async fn serve_docs(index: DocIndex, config: DocServeConfig) -> std::io::Result<()> {
    let html = Arc::new(generate_html(&index));

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
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))
}

fn generate_html(index: &DocIndex) -> String {
    let mut value = serde_json::to_value(index).expect("serialize DocIndex");
    fe_web::static_site::inject_html_bodies(&mut value);
    let json = serde_json::to_string(&value).expect("serialize JSON");
    let title = if let Some(root) = index.modules.first() {
        format!("{} — Fe Documentation", root.name)
    } else {
        "Fe Documentation".to_string()
    };
    fe_web::assets::html_shell(&title, &json)
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

        let html = generate_html(&index);
        assert!(html.contains("<!DOCTYPE html>"));
        assert!(html.contains("mylib"));
        assert!(html.contains("Fe Documentation"));
        assert!(html.contains("FE_DOC_INDEX"));
    }

    #[test]
    fn generate_html_empty_index() {
        let index = DocIndex::new();
        let html = generate_html(&index);
        assert!(html.contains("<!DOCTYPE html>"));
        assert!(html.contains("Fe Documentation"));
    }
}
