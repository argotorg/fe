//! # async-lsp-streams
//!
//! This crate provides an extension to the `async-lsp` library, allowing easy creation of
//! stream-based handlers for LSP requests and notifications.

use async_lsp::lsp_types::{notification, request};
use async_lsp::router::Router;
use async_lsp::{ClientSocket, ResponseError};
use futures::{Stream, StreamExt};
use std::fmt::{Debug, Display};
use std::pin::Pin;
use std::task::{Context, Poll};
use tokio::sync::{broadcast, mpsc, oneshot};
use tracing::instrument::WithSubscriber;

/// A stream of LSP request messages with their response channels.
pub struct RequestStream<Params, Result> {
    receiver: mpsc::Receiver<(
        Params,
        oneshot::Sender<std::result::Result<Result, ResponseError>>,
    )>,
}

impl<Params, Result> Stream for RequestStream<Params, Result> {
    type Item = (
        Params,
        oneshot::Sender<std::result::Result<Result, ResponseError>>,
    );

    fn poll_next(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        self.receiver.poll_recv(cx)
    }
}

/// A stream of LSP notification messages.
pub struct NotificationStream<Params> {
    receiver: mpsc::Receiver<Params>,
}

impl<Params> Stream for NotificationStream<Params> {
    type Item = Params;

    fn poll_next(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        self.receiver.poll_recv(cx)
    }
}

/// A stream of LSP event messages.
pub struct EventStream<E> {
    receiver: mpsc::Receiver<E>,
}

impl<E> Stream for EventStream<E> {
    type Item = E;

    fn poll_next(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        self.receiver.poll_recv(cx)
    }
}

/// An extension trait for `RouterBuilder` to add stream-based handlers.
#[allow(dead_code)]
pub trait RouterStreams {
    /// Creates a stream for handling a specific LSP request.
    fn request_stream<R>(&mut self) -> RequestStream<R::Params, R::Result>
    where
        R: request::Request,
        R::Result: Debug;

    /// Creates a stream for handling a specific LSP notification.
    fn notification_stream<N>(&mut self) -> NotificationStream<N::Params>
    where
        N: notification::Notification;

    /// Creates a stream for handling a specific LSP event.
    fn event_stream<E>(&mut self) -> EventStream<E>
    where
        E: Send + Sync + 'static;
}

impl<State> RouterStreams for Router<State> {
    fn request_stream<R>(&mut self) -> RequestStream<R::Params, R::Result>
    where
        R: request::Request,
        R::Result: Debug,
    {
        let (tx, rx) = mpsc::channel(100);
        self.request::<R, _>(move |_, params| {
            let tx = tx.clone();
            async move {
                let (response_tx, response_rx) = oneshot::channel();
                tx.send((params, response_tx)).await.unwrap();
                response_rx.await.unwrap()
            }
        });
        RequestStream { receiver: rx }
    }

    fn notification_stream<N>(&mut self) -> NotificationStream<N::Params>
    where
        N: notification::Notification,
    {
        let (tx, rx) = mpsc::channel(100);
        self.notification::<N>(move |_, params| {
            let tx = tx.clone();
            tokio::spawn(async move {
                tx.send(params).await.unwrap();
            });
            std::ops::ControlFlow::Continue(())
        });
        NotificationStream { receiver: rx }
    }

    fn event_stream<E>(&mut self) -> EventStream<E>
    where
        E: Send + Sync + 'static,
    {
        let (tx, rx) = mpsc::channel(100);
        self.event::<E>(move |_, event| {
            let tx = tx.clone();
            tokio::spawn(async move {
                if let Err(e) = tx.send(event).await {
                    tracing::error!("Failed to send event to stream: {}", e);
                }
            });
            std::ops::ControlFlow::Continue(())
        });
        EventStream { receiver: rx }
    }
}

/// Extension trait for piping streams to sinks (handles spawn+while boilerplate)
pub trait StreamPipeExt: Stream + Sized {
    /// Pipe stream items back into the LSP event system via `client.emit()`
    fn pipe_emit(self, client: ClientSocket)
    where
        Self: Send + 'static,
        Self::Item: Send + Debug + Display + 'static;

    /// Pipe stream items to a broadcast channel (for WebSocket notifications)
    #[allow(dead_code)]
    fn pipe_to_broadcast<T>(self, tx: broadcast::Sender<T>)
    where
        Self: Send + 'static,
        Self::Item: Into<T> + Send,
        T: Clone + Send + 'static;
}

impl<S: Stream + Sized> StreamPipeExt for S {
    fn pipe_emit(self, client: ClientSocket)
    where
        Self: Send + 'static,
        Self::Item: Send + Debug + Display + 'static,
    {
        tokio::spawn(
            async move {
                let mut stream = Box::pin(self);
                while let Some(event) = stream.next().await {
                    let _ = client.clone().emit(event);
                }
            }
            .with_current_subscriber(),
        );
    }

    fn pipe_to_broadcast<T>(self, tx: broadcast::Sender<T>)
    where
        Self: Send + 'static,
        Self::Item: Into<T> + Send,
        T: Clone + Send + 'static,
    {
        tokio::spawn(
            async move {
                let mut stream = Box::pin(self);
                while let Some(item) = stream.next().await {
                    let _ = tx.send(item.into());
                }
            }
            .with_current_subscriber(),
        );
    }
}
