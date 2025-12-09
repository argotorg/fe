use std::sync::Arc;

use act_locally::{
    handler::{AsyncFunc, AsyncMutatingFunc},
    message::{Message, MessageDowncast, MessageKey, Response, ResponseDowncast},
    types::ActorError,
};
use async_lsp::{
    AnyEvent, ResponseError,
    lsp_types::{notification::Notification, request::Request},
};
use tracing::error;

use super::{
    LspActor,
    service::{LspActorKey, LspActorService},
};

impl<S: 'static> LspActor<S> for LspActorService<S> {
    fn handle_request<R: Request>(
        &mut self,
        handler: impl for<'b> AsyncFunc<'b, S, R::Params, R::Result, ResponseError>
        + 'static
        + Send
        + Sync,
    ) -> &mut Self {
        let param_handler = Box::new(
            move |params: Box<dyn Message>| -> Result<Box<dyn Message>, ActorError> {
                let params = params.downcast::<serde_json::Value>().map_err(|_| {
                    error!("Failed to downcast params to serde_json::Value");
                    ActorError::DowncastError
                })?;
                let typed_params: R::Params = serde_json::from_value(*params).map_err(|e| {
                    error!("Deserialization error: {:?}", e);
                    ActorError::CustomError(Box::new(e))
                })?;

                Ok(Box::new(typed_params) as Box<dyn Message>)
            },
        );

        Arc::get_mut(&mut self.dispatcher)
            .expect("Failed to get mutable reference to dispatcher")
            .register_wrapper(MessageKey(R::METHOD.into()), param_handler);

        self.actor_ref
            .register_handler_async(MessageKey(R::METHOD.into()), handler);

        let result_unwrapper = Box::new(
            move |result: Box<dyn Response>| -> Result<Box<dyn Response>, ActorError> {
                let lsp_result = *result
                    .downcast::<R::Result>()
                    .map_err(|_| ActorError::DowncastError)?;

                let json_value = serde_json::to_value(lsp_result)
                    .map_err(|e| ActorError::CustomError(Box::new(e)))?;
                // error!("Unwrapped json result: {:?}", &json_value);
                Ok(Box::new(json_value) as Box<dyn Response>)
            },
        );

        Arc::get_mut(&mut self.dispatcher)
            .expect("Failed to get mutable reference to dispatcher")
            .register_unwrapper(MessageKey::new(R::METHOD.into()), result_unwrapper);

        self
    }

    fn handle_request_mut<R: Request>(
        &mut self,
        handler: impl for<'b> AsyncMutatingFunc<'b, S, R::Params, R::Result, ResponseError>
        + 'static
        + Send
        + Sync,
    ) -> &mut Self {
        let param_handler = Box::new(
            move |params: Box<dyn Message>| -> Result<Box<dyn Message>, ActorError> {
                let params = params.downcast::<serde_json::Value>().map_err(|_| {
                    error!("Failed to downcast params to serde_json::Value");
                    ActorError::DowncastError
                })?;
                let typed_params: R::Params = serde_json::from_value(*params).map_err(|e| {
                    error!("Deserialization error: {:?}", e);
                    ActorError::CustomError(Box::new(e))
                })?;

                Ok(Box::new(typed_params) as Box<dyn Message>)
            },
        );

        Arc::get_mut(&mut self.dispatcher)
            .expect("Failed to get mutable reference to dispatcher")
            .register_wrapper(MessageKey(R::METHOD.into()), param_handler);

        self.actor_ref
            .register_handler_async_mutating(MessageKey(R::METHOD.into()), handler);

        let result_unwrapper = Box::new(
            move |result: Box<dyn Response>| -> Result<Box<dyn Response>, ActorError> {
                let lsp_result = *result
                    .downcast::<R::Result>()
                    .map_err(|_| ActorError::DowncastError)?;

                let json_value = serde_json::to_value(lsp_result)
                    .map_err(|e| ActorError::CustomError(Box::new(e)))?;
                // error!("Unwrapped json result: {:?}", &json_value);
                Ok(Box::new(json_value) as Box<dyn Response>)
            },
        );

        Arc::get_mut(&mut self.dispatcher)
            .expect("Failed to get mutable reference to dispatcher")
            .register_unwrapper(MessageKey::new(R::METHOD.into()), result_unwrapper);

        self
    }

    fn handle_notification<N: Notification>(
        &mut self,
        handler: impl for<'b> AsyncFunc<'b, S, N::Params, (), ResponseError> + 'static + Send + Sync,
    ) -> &mut Self {
        let param_handler = Box::new(
            move |params: Box<dyn Message>| -> Result<Box<dyn Message>, ActorError> {
                let params = params.downcast::<serde_json::Value>().map_err(|_| {
                    error!("Failed to downcast params to serde_json::Value");
                    ActorError::DowncastError
                })?;
                let typed_params: N::Params = serde_json::from_value(*params).map_err(|e| {
                    error!("Deserialization error: {:?}", e);
                    ActorError::CustomError(Box::new(e))
                })?;
                Ok(Box::new(typed_params) as Box<dyn Message>)
            },
        );

        Arc::get_mut(&mut self.dispatcher)
            .expect("Failed to get mutable reference to dispatcher")
            .register_wrapper(MessageKey::new(N::METHOD.into()), param_handler);

        self.actor_ref
            .register_handler_async(MessageKey::new(N::METHOD.into()), handler);
        self
    }

    fn handle_notification_mut<N: Notification>(
        &mut self,
        handler: impl for<'b> AsyncMutatingFunc<'b, S, N::Params, (), ResponseError>
        + 'static
        + Send
        + Sync,
    ) -> &mut Self {
        let param_handler = Box::new(
            move |params: Box<dyn Message>| -> Result<Box<dyn Message>, ActorError> {
                let params = params.downcast::<serde_json::Value>().map_err(|_| {
                    error!("Failed to downcast params to serde_json::Value");
                    ActorError::DowncastError
                })?;
                let typed_params: N::Params = serde_json::from_value(*params).map_err(|e| {
                    error!("Deserialization error: {:?}", e);
                    ActorError::CustomError(Box::new(e))
                })?;
                Ok(Box::new(typed_params) as Box<dyn Message>)
            },
        );

        Arc::get_mut(&mut self.dispatcher)
            .expect("Failed to get mutable reference to dispatcher")
            .register_wrapper(MessageKey::new(N::METHOD.into()), param_handler);

        self.actor_ref
            .register_handler_async_mutating(MessageKey::new(N::METHOD.into()), handler);
        self
    }

    fn handle_event<E: Send + Sync + 'static>(
        &mut self,
        handler: impl for<'b> AsyncFunc<'b, S, E, (), ResponseError> + 'static + Send + Sync,
    ) -> &mut Self {
        let wrapper = Box::new(
            move |message: Box<dyn Message>| -> Result<Box<dyn Message>, ActorError> {
                let event = message
                    .downcast::<AnyEvent>()
                    .expect("Failed to downcast message to AnyEvent");
                let inner = event.downcast::<E>().expect("Failed to downcast event");
                Ok(Box::new(inner))
            },
        );

        Arc::get_mut(&mut self.dispatcher)
            .expect("Failed to get mutable reference to dispatcher")
            .register_wrapper(LspActorKey::of::<E>().into(), wrapper);

        self.actor_ref
            .register_handler_async(LspActorKey::of::<E>().into(), handler);
        self
    }

    fn handle_event_mut<E: Send + Sync + 'static>(
        &mut self,
        handler: impl for<'b> AsyncMutatingFunc<'b, S, E, (), ResponseError> + Send + Sync + 'static,
    ) -> &mut Self {
        let wrapper = Box::new(
            move |message: Box<dyn Message>| -> Result<Box<dyn Message>, ActorError> {
                let event = message
                    .downcast::<AnyEvent>()
                    .expect("Failed to downcast message to AnyEvent");
                let inner = event.downcast::<E>().expect("Failed to downcast event");
                Ok(Box::new(inner))
            },
        );

        Arc::get_mut(&mut self.dispatcher)
            .expect("Failed to get mutable reference to dispatcher")
            .register_wrapper(LspActorKey::of::<E>().into(), wrapper);

        self.actor_ref
            .register_handler_async_mutating(LspActorKey::of::<E>().into(), handler);
        self
    }
}
