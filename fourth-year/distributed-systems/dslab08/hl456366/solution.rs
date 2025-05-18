use std::collections::{HashMap, HashSet};
use std::net::SocketAddr;
use std::sync::Arc;
use std::time::Duration;

use log::debug;
use serde::{Deserialize, Serialize};
use tokio::net::UdpSocket;
use uuid::Uuid;

use module_system::{Handler, ModuleRef, System, TimerHandle};

/// A message, which disables a process. Used for testing.
pub struct Disable;

/// A message, which enables a process. Used for testing.
pub struct Enable;

struct Init;

#[derive(Clone)]
struct Timeout;

pub struct FailureDetectorModule {
    enabled: bool,
    timeout_handle: Option<TimerHandle>,
    delta: Duration,
    delay: Duration,
    // TODO add whatever fields necessary.
    statuses: HashMap<Uuid, Status>,
    addresses: HashMap<Uuid, SocketAddr>,
    socket: Arc<UdpSocket>,
    ident: Uuid,
}

#[derive(PartialEq)]
pub enum Status {
    Alive,
    AliveResponed,
    SuspectedResponeded,
    Suspected,
}

impl FailureDetectorModule {
    pub async fn new(
        system: &mut System,
        delta: Duration,
        addresses: &HashMap<Uuid, SocketAddr>,
        ident: Uuid,
    ) -> ModuleRef<Self> {
        let addr = addresses.get(&ident).unwrap();
        let socket = Arc::new(UdpSocket::bind(addr).await.unwrap());
        let statuses = addresses
            .keys()
            // .filter(|k| **k != ident)
            .map(|k| (*k, Status::Alive))
            .collect::<HashMap<_, _>>();

        let module_ref = system
            .register_module(Self {
                enabled: true,
                timeout_handle: None,
                delta,
                delay: delta,
                // TODO initialize the fields you added
                statuses,
                addresses: addresses.clone(), // .into_iter()
                // .filter(|(k, _)| *k != ident)
                // .collect()
                socket: socket.clone(),
                ident,
            })
            .await;

        tokio::spawn(deserialize_and_forward(socket, module_ref.clone()));

        module_ref.send(Init).await;

        module_ref
    }
}

#[async_trait::async_trait]
impl Handler<Init> for FailureDetectorModule {
    async fn handle(&mut self, self_ref: &ModuleRef<Self>, _msg: Init) {
        self.timeout_handle = Some(self_ref.request_tick(Timeout, self.delay).await);
    }
}

/// New operation arrived at a socket.
#[async_trait::async_trait]
impl Handler<DetectorOperationUdp> for FailureDetectorModule {
    async fn handle(&mut self, _self_ref: &ModuleRef<Self>, item: DetectorOperationUdp) {
        // print!("id: {:?} handling message: {:?}\n", self.ident, item.0);
        // if self.enabled {
        //     unimplemented!();
        // }
        if !self.enabled {
            return;
        }
        let (operation, sender) = (item.0, item.1);
        match operation {
            DetectorOperation::HeartbeatRequest => {
                match bincode::serialize(&DetectorOperation::HeartbeatResponse(self.ident)) {
                    Ok(msg) => {
                        self.socket.send_to(&msg, sender).await.unwrap();
                    }
                    Err(err) => {
                        debug!("Can't serialize HeartbeatResponse ({})!", err);
                    }
                };
            }
            DetectorOperation::HeartbeatResponse(uuid) => {
                match self.statuses.get_mut(&uuid) {
                    Some(status) => {
                        *status = match status {
                            Status::Alive => Status::AliveResponed,
                            Status::Suspected => Status::SuspectedResponeded,
                            Status::AliveResponed => Status::AliveResponed,
                            Status::SuspectedResponeded => Status::SuspectedResponeded,
                        };
                    }
                    None => {}
                };
            }
            DetectorOperation::AliveRequest => {
                match bincode::serialize(&DetectorOperation::AliveInfo(
                    self.statuses
                        .iter()
                        .filter(|(_, status)| {
                            **status == Status::Alive || **status == Status::AliveResponed
                        })
                        .map(|(uuid, _)| *uuid)
                        .collect(),
                )) {
                    Ok(msg) => {
                        self.socket.send_to(&msg, sender).await.unwrap();
                    }
                    Err(err) => {
                        debug!("Can't serialize AliveInfo ({})!", err);
                    }
                };
            }
            DetectorOperation::AliveInfo(_uuids) => {}
        }
    }
}

/// Called periodically to check send broadcast and update alive processes.
#[async_trait::async_trait]
impl Handler<Timeout> for FailureDetectorModule {
    async fn handle(&mut self, self_ref: &ModuleRef<Self>, _msg: Timeout) {
        if !self.enabled {
            return;
        }
        // if self.enabled {
        //     unimplemented!();
        // }
        match &self.timeout_handle {
            Some(handle) => {
                handle.stop().await;
            }
            None => {}
        };

        for (_uuid, status) in self.statuses.iter_mut() {
            let final_status = match status {
                Status::Alive => Status::Suspected,
                Status::AliveResponed => Status::Alive,
                Status::SuspectedResponeded => {
                    self.delay += self.delta;
                    Status::Alive
                }
                Status::Suspected => Status::Suspected,
            };

            // self.statuses.insert(*uuid, final_status);
            *status = final_status;
        }

        match bincode::serialize(&DetectorOperation::HeartbeatRequest) {
            Ok(msg) => {
                for (_uid, addr) in self.addresses.iter() {
                    self.socket.send_to(&msg, addr).await.unwrap();
                }
            }
            Err(err) => {
                debug!("Can't serialize AliveRequest ({})!", err);
            }
        };

        self.timeout_handle = Some(self_ref.request_tick(Timeout, self.delay).await);
    }
}

#[async_trait::async_trait]
impl Handler<Disable> for FailureDetectorModule {
    async fn handle(&mut self, _self_ref: &ModuleRef<Self>, _msg: Disable) {
        self.enabled = false;
    }
}

#[async_trait::async_trait]
impl Handler<Enable> for FailureDetectorModule {
    async fn handle(&mut self, _self_ref: &ModuleRef<Self>, _msg: Enable) {
        self.enabled = true;
    }
}

async fn deserialize_and_forward(
    socket: Arc<UdpSocket>,
    module_ref: ModuleRef<FailureDetectorModule>,
) {
    let mut buffer = vec![0];
    while let Ok((len, sender)) = socket.peek_from(&mut buffer).await {
        if len == buffer.len() {
            buffer.resize(2 * buffer.len(), 0);
        } else {
            socket.recv_from(&mut buffer).await.unwrap();
            match bincode::deserialize(&buffer) {
                Ok(msg) => module_ref.send(DetectorOperationUdp(msg, sender)).await,
                Err(err) => {
                    debug!("Invalid format of detector operation ({})!", err);
                }
            }
        }
    }
}

struct DetectorOperationUdp(DetectorOperation, SocketAddr);

#[derive(Serialize, Deserialize, Debug)]
pub enum DetectorOperation {
    /// Request to receive a heartbeat.
    HeartbeatRequest,
    /// Response to heartbeat, contains uuid of the receiver of HeartbeatRequest.
    HeartbeatResponse(Uuid),
    /// Request to receive information about working processes.
    AliveRequest,
    /// Vector of processes which are alive according to AliveRequest receiver.
    AliveInfo(HashSet<Uuid>),
}
