pub use crate::register_client::register_client_impl::*;

pub(crate) mod register_process_impl {
    use std::collections::HashMap;
    use std::sync::Arc;

    use log::debug;

    use tokio::net::TcpListener;
    use tokio::sync::{Mutex, OwnedSemaphorePermit, Semaphore};

    use crate::atomic_register::atomic_register_impl::CallbackType;
    use crate::manager::sectors_manager_public;
    use crate::transfer::transfer_impl::{serialize_fail, serialize_success};
    use crate::{
        build_atomic_register, build_sectors_manager, deserialize_register_command,
        ClientRegisterCommand, ClientRegisterCommandContent, Configuration, RegisterClient,
        RegisterCommand, SectorIdx, StatusCode,
    };

    use super::InternalCommand;

    enum WorkerCommand {
        Client(
            ClientRegisterCommand,
            crate::atomic_register::atomic_register_impl::CallbackType,
        ),
        System(crate::SystemRegisterCommand),
    }

    fn get_sector_id(cmd: RegisterCommand) -> SectorIdx {
        match cmd {
            RegisterCommand::Client(cmd) => cmd.header.sector_idx,
            RegisterCommand::System(cmd) => cmd.header.sector_idx,
        }
    }
    static NUM_WORKERS: usize = 50;
    static MIN_WORKERS: usize = 4;
    #[derive(Clone)]
    struct InstanceState {
        config: Configuration,
        sectors_manager: Arc<dyn sectors_manager_public::SectorsManager>,
        register_client: Arc<dyn RegisterClient>,
        // we create a worker for each sector id
        worker_map: Arc<
            Mutex<
                HashMap<
                    SectorIdx,
                    (
                        Arc<Semaphore>,
                        tokio::sync::mpsc::UnboundedSender<WorkerCommand>,
                    ),
                >,
            >,
        >,
        // we keep track of the worker handles to join them later
        worker_handles: Arc<Mutex<Vec<tokio::task::JoinHandle<()>>>>,
        n_processes: u8,
        active_workers: Arc<Semaphore>,
    }
    impl InstanceState {
        async fn new(
            config: Configuration,
            self_tx: tokio::sync::mpsc::UnboundedSender<InternalCommand>,
        ) -> Self {
            let _ = tokio::fs::create_dir(config.public.storage_dir.clone()).await;
            let sectors_manager = build_sectors_manager(config.public.storage_dir.clone()).await;
            // let (self_tx, self_rx) = tokio::sync::mpsc::unbounded_channel();
            let register_client: Arc<dyn RegisterClient> =
                crate::register_client::register_client_impl::build_register_client(
                    config.public.self_rank,
                    self_tx,
                    config.public.tcp_locations.clone(),
                    config.hmac_system_key,
                );
            let worker_map = Arc::new(tokio::sync::Mutex::new(HashMap::<
                SectorIdx,
                (
                    Arc<Semaphore>,
                    tokio::sync::mpsc::UnboundedSender<WorkerCommand>,
                ),
            >::new()));
            let worker_handles: Arc<Mutex<Vec<tokio::task::JoinHandle<()>>>> =
                Arc::new(Mutex::new(Vec::new()));
            let n_processes = config.public.tcp_locations.len() as u8;
            Self {
                config,
                sectors_manager,
                register_client,
                worker_map,
                worker_handles,
                n_processes,
                active_workers: Arc::new(Semaphore::new(
                    (NUM_WORKERS / n_processes as usize).max(MIN_WORKERS),
                )),
            }
        }

        pub(crate) async fn start_connection_handler(
            &mut self,
            stream: tokio::net::TcpStream,
            sock_addr: std::net::SocketAddr,
        ) {
            let id_from = self
                .config
                .public
                .tcp_locations
                .iter()
                .position(|x| x.0 == sock_addr.ip().to_string());
            if id_from.is_none() {
                debug!(
                    "[connection_handler {}] Connection from unknown address {:?}",
                    self.config.public.self_rank, sock_addr
                );
            } else {
                debug!(
                    "[connection_handler {}] Connection from {:?}",
                    self.config.public.self_rank,
                    id_from.unwrap() as i32
                );
            }
            let (mut reader, writer) = stream.into_split();
            let protected_writer = Arc::new(Mutex::new(writer));
            loop {
                match deserialize_register_command(
                    &mut reader,
                    &self.config.hmac_system_key,
                    &self.config.hmac_client_key,
                )
                .await
                {
                    Ok(cmd) => {
                        debug!(
                            "[connection_handler {}] command from {:?} : {:?}",
                            self.config.public.self_rank,
                            sock_addr,
                            get_cmd_name(&cmd.0)
                        );
                        self.handle_command(cmd, protected_writer.clone()).await;
                        debug!(
                            "[connection_handler {}] command from {:?} handled",
                            self.config.public.self_rank, sock_addr
                        );
                    }
                    Err(e) => {
                        log::debug!("Error deserializing command: {}", e);
                        return;
                    }
                }
            }
        }
        pub(crate) async fn start_self_connection_handler(
            &mut self,
            mut self_rx: tokio::sync::mpsc::UnboundedReceiver<InternalCommand>,
        ) {
            while let Some(cmd) = self_rx.recv().await {
                debug!(
                    "[self_connection_handler {}] handling command: {:?}",
                    self.config.public.self_rank,
                    get_system_cmd_name(&cmd)
                );
                let cmd_deref = (*cmd).clone();

                // debug!("[self_connection_handler] sending command to worker: {:?}", );
                let map = self.worker_map.lock().await;
                debug!("[self connection]got map");
                let worker_tx = map.get(&cmd.header.sector_idx).unwrap().1.clone();
                drop(map);
                debug!("[self connection] release map");
                worker_tx.send(WorkerCommand::System(cmd_deref)).unwrap();
            }
        }

        async fn handle_command(
            &mut self,
            cmd: (RegisterCommand, bool),
            writer: Arc<Mutex<tokio::net::tcp::OwnedWriteHalf>>,
        ) {
            let (cmd, hmac_ok) = cmd;
            debug!(
                "[connection_handler {}] handling command: {:?}  ",
                self.config.public.self_rank,
                get_cmd_name(&cmd)
            );
            if let RegisterCommand::Client(client_cmd) = cmd.clone() {
                debug!(
                    "[handle command ] got client command on sector {} out of {} ",
                    client_cmd.header.sector_idx, self.config.public.n_sectors
                );
                if client_cmd.clone().header.sector_idx >= self.config.public.n_sectors {
                    let mut writer_lock = writer.lock().await;
                    let _ = serialize_fail(
                        StatusCode::InvalidSectorIndex,
                        client_cmd,
                        &mut *writer_lock,
                        &self.config.hmac_client_key,
                    )
                    .await;

                    return;
                }
                if !hmac_ok {
                    let mut writer_lock = writer.lock().await;
                    let _ = serialize_fail(
                        StatusCode::AuthFailure,
                        client_cmd,
                        &mut *writer_lock,
                        &self.config.hmac_client_key,
                    )
                    .await;
                    return;
                }
            }

            let mut map = self.worker_map.lock().await;
            debug!("[handle command ] got map");
            let (sem, worker_tx) = match map.get(&get_sector_id(cmd.clone())) {
                Some(tx) => (tx.0.clone(), tx.1.clone()),
                None => {
                    let (tx, rx) = tokio::sync::mpsc::unbounded_channel();
                    let self_cloned = self.clone();
                    let cmd_cloned = cmd.clone();
                    let worker = tokio::spawn(async move {
                        self_cloned
                            .start_worker(rx, get_sector_id(cmd_cloned))
                            .await;
                    });
                    self.worker_handles.lock().await.push(worker);
                    let sem = Arc::new(Semaphore::new(1));
                    map.insert(get_sector_id(cmd.clone()), (sem.clone(), tx.clone()));
                    (sem, tx)
                }
            };
            drop(map);
            debug!("[handle command ] release map");

            worker_tx
                .send(self.worker_command(cmd, writer, sem).await)
                .unwrap();
        }

        async fn worker_command(
            &self,
            cmd: RegisterCommand,
            writer: Arc<Mutex<tokio::net::tcp::OwnedWriteHalf>>,
            sem: Arc<Semaphore>,
        ) -> WorkerCommand {
            // debug!(
            //     "[worker] worker on sector {} got command: {:?}",
            //     self.get_cmd_name(&cmd)
            // );
            match cmd {
                RegisterCommand::Client(cmd) => {
                    let lock = sem.clone().acquire_owned().await.unwrap();
                    debug!(
                        "[worker {}] mutex acquired for sector {}",
                        self.config.public.self_rank, cmd.header.sector_idx
                    );
                    // let callback = Box::new(move |mut op_cmp| Box::pin(async move {}));
                    let callback = build_callback(
                        writer.clone(),
                        lock,
                        cmd.header.sector_idx,
                        self.config.hmac_client_key,
                    );
                    WorkerCommand::Client(cmd, callback)
                }
                RegisterCommand::System(cmd) => WorkerCommand::System(cmd),
            }
        }
        async fn start_worker(
            &self,
            mut rx: tokio::sync::mpsc::UnboundedReceiver<WorkerCommand>,
            sector_idx: SectorIdx,
        ) {
            debug!(
                "[worker {} ] Starting worker for sector {}",
                self.config.public.self_rank, sector_idx
            );
            let mut register = build_atomic_register(
                self.config.public.self_rank,
                sector_idx,
                self.register_client.clone(),
                self.sectors_manager.clone(),
                self.n_processes,
            )
            .await;
            while let Some(cmd) = rx.recv().await {
                let permit = self.active_workers.clone().acquire_owned().await.unwrap();
                match cmd {
                    WorkerCommand::Client(client_cmd, callback) => {
                        debug!(
                            "[worker] handling client command on sector {}: {:?}",
                            sector_idx,
                            get_client_cmd_name(&client_cmd)
                        );
                        let _ = register.client_command(client_cmd, callback).await;
                    }
                    WorkerCommand::System(system_cmd) => {
                        debug!(
                            "[worker] handling system command on sector {}: {:?}",
                            sector_idx,
                            get_system_cmd_name(&system_cmd)
                        );
                        let _ = register.system_command(system_cmd).await;
                    }
                }
                drop(permit);
                debug!("[worker] command on sector {} handled", sector_idx);
            }
        }
    }
    pub(crate) fn get_cmd_name(cmd: &RegisterCommand) -> String {
        match cmd {
            RegisterCommand::Client(cmd) => format!("Client, {}", get_client_cmd_name(cmd)),
            RegisterCommand::System(cmd) => format!("System, {}", get_system_cmd_name(cmd)),
        }
    }
    pub(crate) fn get_client_cmd_name(cmd: &ClientRegisterCommand) -> String {
        let cmd = cmd.content.clone();
        match cmd {
            ClientRegisterCommandContent::Read { .. } => "Read".to_string(),
            ClientRegisterCommandContent::Write { .. } => "Write".to_string(),
        }
    }
    pub(crate) fn get_system_cmd_name(cmd: &crate::SystemRegisterCommand) -> String {
        let cmd = cmd.content.clone();
        match cmd {
            crate::SystemRegisterCommandContent::ReadProc => "ReadProc".to_string(),
            crate::SystemRegisterCommandContent::Value { .. } => "Value".to_string(),
            crate::SystemRegisterCommandContent::WriteProc { .. } => "WriteProc".to_string(),
            crate::SystemRegisterCommandContent::Ack => "Ack".to_string(),
        }
    }

    pub(crate) async fn run_register_process_impl(config: Configuration) {
        debug!(
            "[register_process] Starting register process on rank {}",
            config.public.self_rank
        );
        let addr = config.public.tcp_locations[config.public.self_rank as usize - 1].clone();
        let listener = TcpListener::bind(&addr).await.unwrap();

        let (self_tx, self_rx) = tokio::sync::mpsc::unbounded_channel();
        let state = InstanceState::new(config, self_tx).await;
        let mut cloned_state = state.clone();

        tokio::spawn(async move { cloned_state.start_self_connection_handler(self_rx).await });
        // Arc::new(crate::register_client::register_client_impl::register_client());
        // channel to each worker
        // map sectorid -> worker, vector of handles
        while let Ok((stream, sock_addr)) = listener.accept().await {
            let mut cloned_state = state.clone();
            // handle?
            tokio::spawn(async move {
                cloned_state
                    .start_connection_handler(stream, sock_addr)
                    .await
            });
        }
    }
    fn build_callback(
        writer: Arc<Mutex<tokio::net::tcp::OwnedWriteHalf>>,
        lock: OwnedSemaphorePermit,
        sector_idx: SectorIdx,
        hmac_client_key: [u8; 32],
    ) -> CallbackType {
        Box::new(move |op_succ| {
            Box::pin(async move {
                let mut writer_lock = writer.lock().await;
                serialize_success(op_succ, &mut *writer_lock, &hmac_client_key)
                    .await
                    .unwrap();
                drop(writer_lock);
                debug!("[callback] sector {} mutex released", sector_idx);
                drop(lock);
            })
        })
    }
}
