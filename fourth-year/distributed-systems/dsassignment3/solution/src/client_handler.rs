pub(crate) mod client_message_handler {
    use std::cmp::max;
    use std::collections::HashMap;
    use std::time::SystemTime;

    use log::debug;
    use module_system::Module;
    use tokio::sync::mpsc::UnboundedSender;

    use crate::{domain::*, ProcessType, Raft};

    use crate::utils::raft_utils::Utils;
    // // mod domain;
    // mod lib;

    use async_trait::async_trait;
    #[async_trait]
    pub trait RaftClientMessageHandler: Module {
        async fn leader_handle_client_request(&mut self, request: ClientRequest);

        async fn follower_handle_client_request(&mut self, msg: ClientRequest);

        async fn handle_snapshot_request(
            &mut self,
            reply_to: UnboundedSender<ClientRequestResponse>,
        );
    }

    #[async_trait]
    impl RaftClientMessageHandler for Raft {
        async fn handle_snapshot_request(
            &mut self,
            reply_to: UnboundedSender<ClientRequestResponse>,
        ) {
            debug!(
                "[{}] Snapshot received, offset: {}, num_applied: {}",
                self.config.self_id.as_u128() % 100,
                self.persistent_state.log_offset,
                self.last_applied
            );
            let apply_index = self.last_applied;
            let log_offset = self.persistent_state.log_offset;
            assert!(log_offset <= apply_index + 1);
            if log_offset == apply_index + 1 {
                self.reply_snapshot(reply_to, false, apply_index).await;
                return;
            };
            let serialized = self.state_machine.serialize().await;
            debug!(
                "[{}] Snapshotting state machine, data: {:?}",
                self.config.self_id.as_u128() % 100,
                serialized
            );
            self.stable_storage
                .put(&self.snapshot_key(), &serialized)
                .await
                .unwrap();

            self.persistent_state.snapshot_last_term =
                Some(self.persistent_state.log[self.log_index(apply_index)].term);
            self.persistent_state.log_offset = apply_index + 1;
            self.persistent_state.log = self.persistent_state.log
                [(self.persistent_state.log_offset - log_offset)..]
                .to_vec();
            self.save_state().await;
            self.reply_snapshot(reply_to, true, apply_index).await;
        }
        async fn leader_handle_client_request(&mut self, request: ClientRequest) {
            let header = self.get_header();
            if let ProcessType::Leader {
                next_index,
                match_index,
                responded: _,
                client_sessions,
                ..
            } = &mut self.process_type
            {
                let mut cid = uuid::Uuid::from_u128(self.persistent_state.log.len() as u128);
                let entry: LogEntry = match request.content {
                    ClientRequestContent::Command {
                        command,
                        client_id,
                        sequence_num,
                        lowest_sequence_num_without_response,
                    } => {
                        cid = client_id;
                        if self.client_sessions.contains_key(&client_id) {
                            self.client_sessions
                                .get_mut(&client_id)
                                .unwrap()
                                .lowest_sequence_num_without_response = max(
                                self.client_sessions
                                    .get(&client_id)
                                    .unwrap()
                                    .lowest_sequence_num_without_response,
                                lowest_sequence_num_without_response,
                            );
                        }
                        LogEntry {
                            content: LogEntryContent::Command {
                                data: command,
                                client_id,
                                sequence_num,
                                lowest_sequence_num_without_response,
                            },
                            term: self.persistent_state.current_term,
                            timestamp: SystemTime::now(),
                        }
                    }
                    ClientRequestContent::Snapshot => unreachable!(),
                    ClientRequestContent::AddServer { new_server: _ } => {
                        unimplemented!("Cluster membership changes omitted")
                    }
                    ClientRequestContent::RemoveServer { old_server: _ } => {
                        unimplemented!("Cluster membership changes omitted")
                    }
                    ClientRequestContent::RegisterClient => {
                        self.client_sessions.insert(
                            cid,
                            ClientSession {
                                last_activity: SystemTime::now(),
                                responses: HashMap::new(),
                                lowest_sequence_num_without_response: 0,
                            },
                        );
                        LogEntry {
                            content: LogEntryContent::RegisterClient,
                            term: self.persistent_state.current_term,
                            timestamp: SystemTime::now(),
                        }
                    }
                };

                self.persistent_state.log.push(entry.clone());
                client_sessions.insert(cid, request.reply_to.clone());
                for server_id in self.config.servers.iter() {
                    if *server_id != self.config.self_id
                        && match_index[server_id] + 1 == next_index[server_id]
                        && next_index[server_id] + 1 == self.persistent_state.log.len()
                    {
                        self.message_sender
                            .send(
                                server_id,
                                RaftMessage {
                                    header: header.clone(),
                                    content: RaftMessageContent::AppendEntries(AppendEntriesArgs {
                                        prev_log_index: match_index[server_id],
                                        prev_log_term: self.persistent_state.log
                                            [match_index[server_id]]
                                            .term,
                                        entries: vec![entry.clone()],
                                        leader_commit: self.commit_index,
                                    }),
                                },
                            )
                            .await;
                    }
                }
                self.try_make_commit();
                self.apply_commits().await;
            } else {
                unreachable!("should be leader")
            }
        }

        async fn follower_handle_client_request(&mut self, msg: ClientRequest) {
            match msg.content {
                ClientRequestContent::Command {
                    command: _,
                    client_id,
                    sequence_num,
                    lowest_sequence_num_without_response: _,
                } => {
                    msg.reply_to
                        .send(ClientRequestResponse::CommandResponse(
                            CommandResponseArgs {
                                client_id,
                                sequence_num,
                                content: CommandResponseContent::NotLeader {
                                    leader_hint: self.current_leader,
                                },
                            },
                        ))
                        .unwrap();
                }
                ClientRequestContent::RegisterClient => {
                    msg.reply_to
                        .send(ClientRequestResponse::RegisterClientResponse(
                            RegisterClientResponseArgs {
                                content: RegisterClientResponseContent::NotLeader {
                                    leader_hint: self.current_leader,
                                },
                            },
                        ))
                        .unwrap();
                }
                _ => {
                    unimplemented!("ClientRequestContent/Snapshots not implemented")
                }
            };
        }
    }
}
