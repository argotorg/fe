use std::future::Future;
use std::ops::ControlFlow;
use std::pin::Pin;
use std::task::{Context, Poll};

use async_lsp::{AnyEvent, AnyNotification, AnyRequest, LspService, ResponseError};
use serde_json::Value;
use tower::Service;

use crate::lsp_actor::service::CanHandle;

pub struct WithFallbackService<A, B> {
    primary: A,
    fallback: B,
}

impl<A, B> WithFallbackService<A, B> {
    pub fn new(primary: A, fallback: B) -> Self {
        Self { primary, fallback }
    }
}

impl<A, B, F> Service<AnyRequest> for WithFallbackService<A, B>
where
    A: Service<AnyRequest, Response = Value, Error = ResponseError, Future = F>
        + CanHandle<AnyRequest>,
    B: Service<AnyRequest, Response = Value, Error = ResponseError, Future = F>,
    F: Future<Output = Result<Value, ResponseError>> + Send + 'static,
{
    type Response = serde_json::Value;
    type Error = ResponseError;
    type Future = F;

    fn poll_ready(&mut self, cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        match self.primary.poll_ready(cx) {
            Poll::Ready(Ok(())) => self.fallback.poll_ready(cx),
            other => other,
        }
    }

    fn call(&mut self, req: AnyRequest) -> Self::Future {
        if self.primary.can_handle(&req) {
            self.primary.call(req)
        } else {
            self.fallback.call(req)
        }
    }
}

impl<A, B> LspService for WithFallbackService<A, B>
where
    A: LspService<
            Response = Value,
            Error = ResponseError,
            Future = Pin<Box<dyn Future<Output = Result<Value, ResponseError>> + Send + 'static>>,
        > + CanHandle<AnyRequest>
        + CanHandle<AnyNotification>
        + CanHandle<AnyEvent>,
    B: LspService<
            Response = Value,
            Error = ResponseError,
            Future = Pin<Box<dyn Future<Output = Result<Value, ResponseError>> + Send + 'static>>,
        >,
{
    fn notify(&mut self, notif: AnyNotification) -> ControlFlow<async_lsp::Result<()>> {
        if self.primary.can_handle(&notif) {
            self.primary.notify(notif)
        } else {
            self.fallback.notify(notif)
        }
    }

    fn emit(&mut self, event: AnyEvent) -> ControlFlow<async_lsp::Result<()>> {
        if self.primary.can_handle(&event) {
            self.primary.emit(event)
        } else {
            self.fallback.emit(event)
        }
    }
}
