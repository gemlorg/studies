pub(crate) mod register_client_impl {
    use std::sync::Arc;

    use log::{debug, error};
    use tokio::sync::mpsc::{UnboundedReceiver, UnboundedSender};

    use crate::{
        register_client_public,
        register_process::register_process_impl::{get_cmd_name, get_system_cmd_name},
        serialize_register_command, Broadcast, RegisterClient, RegisterCommand,
        SystemRegisterCommand,
    };
    pub(crate) type InternalCommand = Arc<SystemRegisterCommand>;

    struct RegisterClientImpl {
        register_id: u8,
        self_tx: tokio::sync::mpsc::UnboundedSender<InternalCommand>,
        tcp_locations: Vec<(String, u16)>,
        sys_hmac_key: [u8; 64],
        txs: Vec<tokio::sync::mpsc::UnboundedSender<InternalCommand>>,
    }
    impl RegisterClientImpl {
        pub(crate) fn new(
            register_id: u8,
            self_tx: tokio::sync::mpsc::UnboundedSender<InternalCommand>,
            tcp_locations: Vec<(String, u16)>,
            sys_hmac_key: [u8; 64],
        ) -> Self {
            let mut txs = Vec::new();

            for i in 1..tcp_locations.len() + 1 {
                let (tx, rx): (
                    UnboundedSender<InternalCommand>,
                    UnboundedReceiver<InternalCommand>,
                ) = tokio::sync::mpsc::unbounded_channel();
                txs.push(tx);

                if i == register_id as usize {
                    continue;
                }
                tokio::spawn(start_connection(
                    tcp_locations[i - 1].clone(),
                    rx,
                    sys_hmac_key,
                    i,
                    register_id,
                ));
            }
            Self {
                register_id,
                self_tx,
                tcp_locations,
                sys_hmac_key,
                txs,
            }
        }
    }
    async fn start_connection(
        loc: (String, u16),
        mut rx: tokio::sync::mpsc::UnboundedReceiver<InternalCommand>,
        mut sys_hmac_key: [u8; 64],
        _target: usize,
        _register_id: u8,
    ) {
        // wait 300 ms for the tcp  connections to start
        tokio::time::sleep(std::time::Duration::from_millis(300)).await;
        // if the connection is not established, return error
        match tokio::net::TcpStream::connect(loc.clone()).await {
            Ok(mut stream) => {
                debug!(
                    "[ClientRegister {} -> {}] Connection started",
                    _register_id, _target
                );
                while let Some(cmd) = rx.recv().await {
                    // debug!("[connection_handler] command from {:?} : {:?}", loc, cmd);
                    let cmd = RegisterCommand::System((*cmd).clone());
                    debug!(
                        "[ClientRegister {} -> {}] Got command to send: {:?}",
                        _register_id,
                        _target,
                        get_cmd_name(&cmd)
                    );
                    // send the command to the register
                    if let Err(e) =
                        serialize_register_command(&cmd, &mut stream, &sys_hmac_key).await
                    {
                        debug!("[connection_handler] Error sending to {:?}: {:?}", loc, e);
                        return;
                    }
                    debug!(
                        "[ClientRegister {} -> {}] Sent command",
                        _register_id, _target
                    );
                }
            }
            Err(_) => {
                error!("Connection to {:?} failed", loc);
                // if the connection is not established, return error
            }
        }
        debug!("[WARNING]Connection to {:?} closed", loc);
    }

    #[async_trait::async_trait]
    impl RegisterClient for RegisterClientImpl {
        async fn send(&self, msg: register_client_public::Send) {
            debug!(
                "[ClientRegister {} -> {}] Start Sending: {:?}",
                self.register_id,
                msg.target,
                get_system_cmd_name(&msg.cmd),
            );
            if msg.target == self.register_id {
                let _ = self.self_tx.send(msg.cmd);
            } else {
                let _ = self.txs[msg.target as usize - 1].send(msg.cmd);
            }
        }
        async fn broadcast(&self, msg: Broadcast) {
            for i in 1..self.tcp_locations.len() + 1 {
                self.send(register_client_public::Send {
                    cmd: msg.cmd.clone(),
                    target: i as u8,
                })
                .await;
            }
        }
    }

    pub(crate) fn build_register_client(
        register_id: u8,
        self_tx: tokio::sync::mpsc::UnboundedSender<InternalCommand>,
        tcp_locations: Vec<(String, u16)>,
        sys_hmac_key: [u8; 64],
    ) -> Arc<dyn RegisterClient> {
        Arc::new(RegisterClientImpl::new(
            register_id,
            self_tx,
            tcp_locations,
            sys_hmac_key,
        ))
    }
}
