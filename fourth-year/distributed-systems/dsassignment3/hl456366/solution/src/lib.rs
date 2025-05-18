use std::{
    collections::{HashMap, HashSet},
    time::SystemTime,
};

use log::debug;
use module_system::{Handler, ModuleRef, System, TimerHandle};

use client_handler::client_message_handler::*;
pub use domain::*;
use system_handler::message_handler::*;
mod client_handler;
mod domain;
mod system_handler;
mod utils;
use crate::utils::raft_utils::Utils;
use serde::{Deserialize, Serialize};
use tokio::sync::mpsc::UnboundedSender;
use uuid::Uuid;

#[derive(Serialize, Deserialize, Debug)]
struct PersistentState {
    current_term: u64,
    voted_for: Option<Uuid>,
    log: Vec<LogEntry>,
    log_offset: usize,
    snapshot_last_term: Option<u64>,
}
impl PersistentState {
    fn default(cofig: &ServerConfig, first_log_entry_timestamp: SystemTime) -> Self {
        Self {
            current_term: 0,
            voted_for: None,
            log: vec![LogEntry {
                content: LogEntryContent::Configuration {
                    servers: cofig.servers.clone(),
                },
                term: 0,
                timestamp: first_log_entry_timestamp,
            }],
            log_offset: 0,
            snapshot_last_term: None,
        }
    }
}
#[derive(Clone)]
struct Timeout;

struct Init;

#[derive(Clone)]
struct Heartbeat;

#[derive(Serialize, Deserialize, Debug)]
struct SnapshotInfo {
    last_included_index: usize,
    last_included_term: u64,
    last_config: Option<HashSet<Uuid>>,
    client_sessions: Option<HashMap<Uuid, ClientSession>>,
    data: Vec<u8>,
    offset: usize,
}
#[derive(Default)]
enum ProcessType {
    #[default]
    Follower,
    Candidate {
        votes_received: HashSet<Uuid>,
    },
    Leader {
        // volatile state on leaders
        next_index: HashMap<Uuid, usize>,
        match_index: HashMap<Uuid, usize>,
        responded: HashSet<Uuid>,
        client_sessions: HashMap<Uuid, UnboundedSender<ClientRequestResponse>>,
        snapshots: HashMap<Uuid, SnapshotInfo>,
    },
}

struct RaftSenderWrapper {
    message_sender: Box<dyn RaftSender>,
    my_id: Uuid,
}

impl RaftSenderWrapper {
    async fn send(&self, target: &Uuid, message: RaftMessage) {
        assert!(
            *target != self.my_id,
            "Sender should not be the same as self, {:?} -> {:?}, message: {:?}",
            self.my_id,
            target,
            message
        );
        self.message_sender.send(target, message).await;
    }
}

#[non_exhaustive]
pub struct Raft {
    // persistent storage
    persistent_state: PersistentState,
    // volatile storage
    commit_index: usize,
    last_applied: usize,
    // other fields
    config: ServerConfig,
    state_machine: Box<dyn StateMachine>,
    stable_storage: Box<dyn StableStorage>,
    message_sender: RaftSenderWrapper,
    // my fields
    timer_handle: Option<TimerHandle>,
    heartbeat_handle: Option<TimerHandle>,
    process_type: ProcessType,
    current_leader: Option<Uuid>,
    client_sessions: HashMap<Uuid, ClientSession>,
}

impl Raft {
    /// Registers a new `Raft` module in the `system`, initializes it and
    /// returns a `ModuleRef` to it.
    pub async fn new(
        system: &mut System,
        config: ServerConfig,
        first_log_entry_timestamp: SystemTime,
        mut state_machine: Box<dyn StateMachine>,
        stable_storage: Box<dyn StableStorage>,
        message_sender: Box<dyn RaftSender>,
    ) -> ModuleRef<Self> {
        let persistent_state = match stable_storage.get(&config.self_id.to_string()).await {
            Some(state) => bincode::deserialize(&state).unwrap(),
            None => PersistentState::default(&config, first_log_entry_timestamp),
        };

        let self_id = config.self_id;

        let machine_data = stable_storage.get(&format!("{}-snapshot", self_id)).await;
        if machine_data.is_some() {
            state_machine
                .as_mut()
                .initialize(&machine_data.unwrap())
                .await
        }
        let self_ref = system
            .register_module(Self {
                persistent_state,
                commit_index: 0,
                last_applied: 0,
                config,
                state_machine,
                stable_storage,
                message_sender: RaftSenderWrapper {
                    message_sender,
                    my_id: self_id,
                },
                timer_handle: None,
                heartbeat_handle: None,

                process_type: ProcessType::Follower,
                current_leader: None,
                client_sessions: HashMap::new(),
            })
            .await;
        self_ref.send(Init).await;
        self_ref
    }
}

#[async_trait::async_trait]
impl Handler<RaftMessage> for Raft {
    async fn handle(&mut self, self_ref: &ModuleRef<Self>, msg: RaftMessage) {
        if let RaftMessageContent::RequestVote { .. } = msg.content {
            if self.current_leader.is_some() {
                return;
            }
        }
        debug!(
            "[{:?} -> {:?}] {:?}",
            msg.header.source.as_u128() % 100,
            self.config.self_id.as_u128() % 100,
            msg.content
        );
        // save state before handling rpc
        self.save_state().await;
        //ignore if term is less than current term
        if msg.header.term < self.persistent_state.current_term {
            debug!(
                "[{:?}] Ignoring message from {:?} with term {:?} less than current term {:?}",
                self.config.self_id.as_u128() % 100,
                msg.header.source.as_u128() % 100,
                msg.header.term,
                self.persistent_state.current_term
            );
            return;
        }
        // convert to follower if term is greater than current term
        if msg.header.term > self.persistent_state.current_term {
            self.convert_to_follower(msg.header.term).await;
        }
        match msg.content {
            RaftMessageContent::AppendEntries(append_entries_args) => {
                self.handle_append_entries(msg.header, append_entries_args, self_ref)
                    .await
            }
            RaftMessageContent::AppendEntriesResponse(append_entries_response_args) => {
                self.handle_append_entries_response(msg.header, append_entries_response_args)
                    .await
            }
            RaftMessageContent::RequestVote(request_vote_args) => {
                self.handle_request_vote(msg.header, request_vote_args)
                    .await
            }
            RaftMessageContent::RequestVoteResponse(request_vote_response_args) => {
                self.handle_request_vote_response(msg.header, request_vote_response_args, self_ref)
                    .await
            }
            RaftMessageContent::InstallSnapshot(install_snapshot_args) => {
                self.handle_install_snapshot(msg.header, install_snapshot_args, self_ref)
                    .await
            }
            RaftMessageContent::InstallSnapshotResponse(install_snapshot_response_args) => {
                self.handle_install_snapshot_response(msg.header, install_snapshot_response_args)
                    .await
            }
        }
    }
}

#[async_trait::async_trait]
impl Handler<ClientRequest> for Raft {
    async fn handle(&mut self, _self_ref: &ModuleRef<Self>, msg: ClientRequest) {
        debug!(
            "[Client -> {:?}] {:?}",
            self.config.self_id.as_u128() % 100,
            msg.content
        );
        if let ClientRequestContent::Snapshot = msg.content {
            self.handle_snapshot_request(msg.reply_to).await;
            return;
        }
        match self.process_type {
            ProcessType::Leader { .. } => self.leader_handle_client_request(msg).await,
            _ => self.follower_handle_client_request(msg).await,
        }
    }
}

#[async_trait::async_trait]
impl Handler<Init> for Raft {
    async fn handle(&mut self, self_ref: &ModuleRef<Self>, _msg: Init) {
        self.reset_timer(self_ref).await;
    }
}
#[async_trait::async_trait]
impl Handler<Timeout> for Raft {
    async fn handle(&mut self, _self_ref: &ModuleRef<Self>, _msg: Timeout) {
        match &mut self.process_type {
            ProcessType::Follower => {
                if self.persistent_state.voted_for == self.current_leader {
                    self.nominate().await;
                }
            }
            ProcessType::Candidate { .. } => self.nominate().await,
            ProcessType::Leader {
                next_index: _,
                match_index: _,
                responded,
                ..
            } => {
                if responded.len() < self.config.servers.len() / 2 {
                    self.convert_to_follower(self.persistent_state.current_term)
                        .await;
                } else {
                    responded.clear();
                }
            }
        }
    }
}
#[async_trait::async_trait]
impl Handler<Heartbeat> for Raft {
    async fn handle(&mut self, _self_ref: &ModuleRef<Self>, _msg: Heartbeat) {
        if let ProcessType::Leader { .. } = &mut self.process_type {
            debug!(
                "[{:?}] Sending heartbeat",
                self.config.self_id.as_u128() % 100
            );
            for server_id in self.config.servers.clone() {
                if server_id != self.config.self_id {
                    self.send_heartbeat(server_id).await;
                }
            }
        }
    }
}

// TODO you can implement handlers of messages of other types for the Raft struct.
