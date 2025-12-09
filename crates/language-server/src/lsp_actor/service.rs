use std::future::Future;
use std::ops::ControlFlow;
use std::pin::Pin;
use std::sync::Arc;
use std::task::{Context, Poll};

use act_locally::actor::ActorRef;
use act_locally::message::MessageKey;
use act_locally::types::ActorError;
use serde_json::Value;
use tracing::info;

// use async_lsp::can_handle::CanHandle;
use async_lsp::{AnyEvent, AnyNotification, AnyRequest, Error, LspService, ResponseError};
use std::any::TypeId;
use tower::Service;

use crate::lsp_actor::LspDispatcher;

pub struct LspActorService<S> {
    pub(super) actor_ref: ActorRef<S, LspActorKey>,
    pub(super) dispatcher: Arc<LspDispatcher>,
}

impl<S> LspActorService<S> {
    pub fn with(actor_ref: ActorRef<S, LspActorKey>) -> Self {
        let dispatcher = LspDispatcher::new();
        Self {
            actor_ref,
            dispatcher: Arc::new(dispatcher),
        }
    }
}

type BoxReqFuture<Error> = Pin<Box<dyn Future<Output = Result<Value, Error>> + Send>>;
impl<S: 'static> Service<AnyRequest> for LspActorService<S> {
    type Response = serde_json::Value;
    type Error = ResponseError;
    // type Future = Pin<Box<dyn Future<Output = Result<Self::Response, Self::Error>> + Send>>;
    type Future = BoxReqFuture<Self::Error>;

    fn poll_ready(&mut self, _cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        Poll::Ready(Ok(()))
    }

    fn call(&mut self, req: AnyRequest) -> Self::Future {
        let method = req.method.clone();
        info!("got LSP request: {method:?}");
        let actor_ref = self.actor_ref.clone();
        let dispatcher = self.dispatcher.clone();
        let method_log = method.clone().to_owned();
        let result = Box::pin(async move {
            let dispatcher = dispatcher.as_ref();
            let ask = actor_ref.ask::<_, Self::Response, _>(dispatcher, req);
            let lsp_result: Result<Self::Response, _> = ask.await.map_err(|e| match e {
                ActorError::HandlerNotFound => ResponseError::new(
                    async_lsp::ErrorCode::METHOD_NOT_FOUND,
                    "Method not found".to_string(),
                ),
                _ => ResponseError::new(
                    async_lsp::ErrorCode::INTERNAL_ERROR,
                    format!("There was an internal error... {e:?}"),
                ),
            });
            info!("Prepared LSP response for: {method_log:?}");
            lsp_result
        });
        info!("Prepared future for LSP request: {method:?}");
        result
    }
}

impl<S: 'static> LspService for LspActorService<S> {
    fn notify(&mut self, notif: AnyNotification) -> ControlFlow<async_lsp::Result<()>> {
        let method = notif.method.clone();
        let dispatcher = self.dispatcher.clone();
        match self.actor_ref.tell(dispatcher.as_ref(), notif) {
            Ok(()) => ControlFlow::Continue(()),
            Err(ActorError::HandlerNotFound) => {
                tracing::warn!("Method not found for notification `{}`", method);
                ControlFlow::Continue(())
            }
            Err(e) => ControlFlow::Break(Err(Error::Response(ResponseError::new(
                async_lsp::ErrorCode::INTERNAL_ERROR,
                format!("Failed to send notification: {e:?} for notification `{method}`"),
            )))),
        }
    }

    fn emit(&mut self, event: AnyEvent) -> ControlFlow<async_lsp::Result<()>> {
        let type_name = event.type_name();
        let dispatcher = self.dispatcher.clone();
        match self.actor_ref.tell(dispatcher.as_ref(), event) {
            Ok(()) => ControlFlow::Continue(()),
            Err(ActorError::HandlerNotFound) => {
                tracing::warn!("Method not found for event: {:?}", type_name);
                ControlFlow::Continue(())
            }
            Err(e) => ControlFlow::Break(Err(Error::Response(ResponseError::new(
                async_lsp::ErrorCode::INTERNAL_ERROR,
                format!("Failed to emit event: {e:?}"),
            )))),
        }
    }
}

pub(crate) trait CanHandle<T> {
    fn can_handle(&self, item: &T) -> bool;
}

impl<S> CanHandle<AnyRequest> for LspActorService<S> {
    fn can_handle(&self, req: &AnyRequest) -> bool {
        self.dispatcher
            .wrappers
            .contains_key(&LspActorKey::from(&req.method))
    }
}

impl<S> CanHandle<AnyNotification> for LspActorService<S> {
    fn can_handle(&self, notif: &AnyNotification) -> bool {
        self.dispatcher
            .wrappers
            .contains_key(&LspActorKey::from(&notif.method))
    }
}

impl<S> CanHandle<AnyEvent> for LspActorService<S> {
    fn can_handle(&self, event: &AnyEvent) -> bool {
        self.dispatcher
            .wrappers
            .contains_key(&LspActorKey::from(event.inner_type_id()))
    }
}

#[derive(Debug, Clone)]
pub enum LspActorKey {
    ByMethod(String),
    ByTypeId(TypeId),
}

impl LspActorKey {
    pub fn of<T: 'static>() -> Self {
        Self::ByTypeId(TypeId::of::<T>())
    }
}

impl std::fmt::Display for LspActorKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LspActorKey::ByMethod(method) => write!(f, "Method({method})"),
            LspActorKey::ByTypeId(type_id) => write!(f, "Custom({type_id:?})"),
        }
    }
}

impl From<String> for LspActorKey {
    fn from(method: String) -> Self {
        LspActorKey::ByMethod(method)
    }
}

impl From<&String> for LspActorKey {
    fn from(method: &String) -> Self {
        LspActorKey::ByMethod(method.clone())
    }
}

impl From<&str> for LspActorKey {
    fn from(method: &str) -> Self {
        LspActorKey::ByMethod(method.to_string())
    }
}

impl From<TypeId> for LspActorKey {
    fn from(type_id: TypeId) -> Self {
        LspActorKey::ByTypeId(type_id)
    }
}

impl From<LspActorKey> for MessageKey<LspActorKey> {
    fn from(val: LspActorKey) -> Self {
        MessageKey(val)
    }
}

impl PartialEq for LspActorKey {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LspActorKey::ByMethod(a), LspActorKey::ByMethod(b)) => a == b,
            (LspActorKey::ByTypeId(a), LspActorKey::ByTypeId(b)) => a == b,
            _ => false,
        }
    }
}

impl std::hash::Hash for LspActorKey {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            LspActorKey::ByMethod(method) => {
                0u8.hash(state);
                method.hash(state);
            }
            LspActorKey::ByTypeId(type_id) => {
                1u8.hash(state);
                type_id.hash(state);
            }
        }
    }
}

impl Eq for LspActorKey {}
