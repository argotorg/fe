use async_lsp::ClientSocket;
use driver::DriverDataBase;

pub struct Backend {
    pub(super) client: ClientSocket,
    pub(super) db: DriverDataBase,
    #[allow(dead_code)] // TODO: salsa3-compatible parallelism
    pub(super) workers: tokio::runtime::Runtime,
}

impl Backend {
    pub fn new(client: ClientSocket) -> Self {
        let db = DriverDataBase::default();

        let workers = tokio::runtime::Builder::new_multi_thread()
            .worker_threads(1)
            .enable_all()
            .build()
            .unwrap();
        Self {
            client,
            db,
            workers,
        }
    }
}
