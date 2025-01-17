pub(crate) mod raft_utils {
    use std::{cmp::min, collections::HashSet, ops::RangeInclusive, time::Duration};

    use async_trait::async_trait;
    use log::debug;
    use module_system::{Module, ModuleRef};

    use crate::*;
    use rand::Rng;
    use uuid::Uuid;

    use crate::Raft;

    #[async_trait]
    pub(crate) trait Utils: Module {
        async fn send_heartbeat(&mut self, server_id: Uuid);
        async fn request_vote(&mut self);
        async fn nominate(&mut self);
        async fn convert_to_follower_of(&mut self, leader_id: Uuid);
        async fn convert_to_follower(&mut self, term: u64);
        async fn send_content_to(&self, target: &Uuid, content: RaftMessageContent);
        async fn send_vote(&self, target: &Uuid, vote_granted: bool);
        async fn apply_commits(&mut self);
        fn try_make_commit(&mut self);
        async fn send_entries(&self, target: &Uuid, result: bool, i: usize);
        fn get_header(&self) -> RaftMessageHeader;
        async fn restart_heartbeat_timer(&mut self, self_ref: &ModuleRef<Self>);
        async fn reset_timer(&mut self, self_ref: &ModuleRef<Self>);
        async fn save_state(&mut self);
        fn log_index(&self, i: usize) -> usize;
        fn log_len(&self) -> usize;
        fn snapshot_key(&self) -> String;
        async fn reply_snapshot(
            &mut self,
            reply_to: UnboundedSender<ClientRequestResponse>,
            success: bool,
            last_applied: usize,
        );
        fn leader_snapshot_key(&self, server_id: Uuid) -> String;

        async fn send_snapshot_response(&self, target: &Uuid, offset: usize, last_index: usize);
    }

    #[async_trait]
    impl Utils for Raft {
        async fn send_snapshot_response(&self, target: &Uuid, offset: usize, last_index: usize) {
            let content =
                RaftMessageContent::InstallSnapshotResponse(InstallSnapshotResponseArgs {
                    offset,
                    last_included_index: last_index,
                });
            self.send_content_to(target, content).await;
        }
        fn leader_snapshot_key(&self, server_id: Uuid) -> String {
            format!("{:?}-{:?}-snapshot", self.config.self_id, server_id)
        }
        async fn reply_snapshot(
            &mut self,
            reply_to: UnboundedSender<ClientRequestResponse>,
            success: bool,
            last_applied: usize,
        ) {
            let content = if success {
                SnapshotResponseContent::SnapshotCreated {
                    last_included_index: last_applied,
                }
            } else {
                SnapshotResponseContent::NothingToSnapshot {
                    last_included_index: last_applied,
                }
            };
            reply_to
                .send(ClientRequestResponse::SnapshotResponse(
                    SnapshotResponseArgs { content },
                ))
                .unwrap();
        }
        fn snapshot_key(&self) -> String {
            format!("{:?}-snapshot", self.config.self_id)
        }
        fn log_len(&self) -> usize {
            self.persistent_state.log.len() + self.persistent_state.log_offset
        }
        fn log_index(&self, i: usize) -> usize {
            assert!(
                i >= self.persistent_state.log_offset,
                "Out of bounds, i: {}, log_offset: {}, log_len: {}",
                i,
                self.persistent_state.log_offset,
                self.log_len()
            );
            i - self.persistent_state.log_offset
        }
        async fn save_state(&mut self) {
            self.stable_storage
                .put(
                    &self.config.self_id.to_string(),
                    &bincode::serialize(&self.persistent_state).unwrap(),
                )
                .await
                .unwrap();
        }
        async fn reset_timer(&mut self, self_ref: &ModuleRef<Self>) {
            if let Some(timer_handle) = self.timer_handle.take() {
                timer_handle.stop().await;
            }
            let election_timeout = select_random_timeout(&self.config.election_timeout_range);
            self.timer_handle = Some(self_ref.request_tick(Timeout, election_timeout).await);
        }

        async fn restart_heartbeat_timer(&mut self, self_ref: &ModuleRef<Self>) {
            if let Some(heartbeat_handle) = self.heartbeat_handle.take() {
                heartbeat_handle.stop().await;
            }
            self.heartbeat_handle = Some(
                self_ref
                    .request_tick(Heartbeat, self.config.heartbeat_timeout)
                    .await,
            );
        }

        fn get_header(&self) -> RaftMessageHeader {
            RaftMessageHeader {
                term: self.persistent_state.current_term,
                source: self.config.self_id,
            }
        }

        async fn send_entries(&self, target: &Uuid, result: bool, i: usize) {
            let content = RaftMessageContent::AppendEntriesResponse(AppendEntriesResponseArgs {
                success: result,
                last_verified_log_index: i,
            });
            self.send_content_to(target, content).await;
        }
        fn try_make_commit(&mut self) {
            if self.commit_index + 1 >= self.persistent_state.log.len() {
                return;
            }
            let next_commit_index = self.commit_index + 1;
            if let ProcessType::Leader {
                next_index: _,
                match_index,
                responded: _,
                client_sessions: _,
                ..
            } = &mut self.process_type
            {
                let match_indexes = match_index.values().collect::<Vec<_>>();
                let num_approves = match_indexes
                    .iter()
                    .filter(|&&x| x >= &next_commit_index)
                    .count();
                if num_approves + 1 > self.config.servers.len() / 2 {
                    self.commit_index = next_commit_index;
                    self.try_make_commit();
                }
            }
        }
        async fn apply_commits(&mut self) {
            assert!(self.last_applied + 1 >= self.persistent_state.log_offset);
            assert!(self.commit_index + 1 >= self.persistent_state.log_offset);
            debug!(
                "[{:?}] Applying commits, last_applied: {:?}, commit_index: {:?}, log_offset: {:?}, log_len: {:?}",
                self.config.self_id.as_u128() % 100,
                self.last_applied,
                self.commit_index,
                self.persistent_state.log_offset,
                self.log_len()
            );
            while self.last_applied < self.commit_index {
                self.last_applied += 1;
                let entry = &self.persistent_state.log[self.log_index(self.last_applied)];
                match &entry.content {
                    LogEntryContent::Command {
                        data,
                        client_id: id,
                        sequence_num,
                        lowest_sequence_num_without_response: _,
                    } => {
                        if let ProcessType::Leader {
                            next_index: _,
                            match_index: _,
                            responded: _,
                            client_sessions,
                            ..
                        } = &mut self.process_type
                        {
                            if let Some(sender) = client_sessions.get(id) {
                                let session = self.client_sessions.get_mut(id);
                                if session.is_none() {
                                    sender
                                        .send(ClientRequestResponse::CommandResponse(
                                            CommandResponseArgs {
                                                client_id: *id,
                                                sequence_num: *sequence_num,
                                                content: CommandResponseContent::SessionExpired,
                                            },
                                        ))
                                        .unwrap();
                                    return;
                                }
                                let session = session.unwrap();
                                debug!(
                                    "[{:?} -> Client {:?}] session: {:?}\n, logentry: {:?}",
                                    self.config.self_id.as_u128() % 100,
                                    id.as_u128() % 100,
                                    session,
                                    entry
                                );
                                if SystemTime::now()
                                    .duration_since(session.last_activity)
                                    .unwrap()
                                    > self.config.session_expiration
                                {
                                    sender
                                        .send(ClientRequestResponse::CommandResponse(
                                            CommandResponseArgs {
                                                client_id: *id,
                                                sequence_num: *sequence_num,
                                                content: CommandResponseContent::SessionExpired,
                                            },
                                        ))
                                        .unwrap();
                                    return;
                                }
                                if session.lowest_sequence_num_without_response > *sequence_num {
                                    sender
                                        .send(ClientRequestResponse::CommandResponse(
                                            CommandResponseArgs {
                                                client_id: *id,
                                                sequence_num: *sequence_num,
                                                content: CommandResponseContent::SessionExpired,
                                            },
                                        ))
                                        .unwrap();
                                    return;
                                }

                                if let Some(res) = session.responses.get(sequence_num) {
                                    sender
                                        .send(ClientRequestResponse::CommandResponse(
                                            CommandResponseArgs {
                                                client_id: *id,
                                                sequence_num: *sequence_num,
                                                content: CommandResponseContent::CommandApplied {
                                                    output: res.to_vec(),
                                                },
                                            },
                                        ))
                                        .unwrap();
                                    return;
                                }
                                self.client_sessions.get_mut(id).unwrap().last_activity =
                                    SystemTime::now();

                                let res = self.state_machine.apply(data).await;
                                debug!(
                                    "[{:?} -> Client {:?}] {:?}",
                                    self.config.self_id.as_u128() % 100,
                                    id.as_u128() % 100,
                                    CommandResponseContent::CommandApplied {
                                        output: res.clone()
                                    }
                                );
                                self.client_sessions
                                    .get_mut(id)
                                    .unwrap()
                                    .responses
                                    .insert(*sequence_num, res.clone());
                                sender
                                    .send(ClientRequestResponse::CommandResponse(
                                        CommandResponseArgs {
                                            client_id: *id,
                                            sequence_num: *sequence_num,
                                            content: CommandResponseContent::CommandApplied {
                                                output: res,
                                            },
                                        },
                                    ))
                                    .unwrap();
                            }
                        }
                    }
                    LogEntryContent::Configuration { servers: _ } => {
                        unreachable!("Configuration can't be a command")
                    }
                    LogEntryContent::RegisterClient => {
                        if let ProcessType::Leader {
                            next_index: _,
                            match_index: _,
                            responded: _,
                            client_sessions,
                            ..
                        } = &mut self.process_type
                        {
                            let client_id = Uuid::from_u128(self.last_applied as u128);
                            if let Some(sender) = client_sessions.get(&client_id) {
                                debug!(
                                    "[{:?} -> Client] {:?}",
                                    self.config.self_id.as_u128() % 100,
                                    RegisterClientResponseContent::ClientRegistered { client_id }
                                );
                                sender
                                    .send(ClientRequestResponse::RegisterClientResponse(
                                        RegisterClientResponseArgs {
                                            content:
                                                RegisterClientResponseContent::ClientRegistered {
                                                    client_id,
                                                },
                                        },
                                    ))
                                    .unwrap();
                            }
                        }
                    }
                    LogEntryContent::NoOp => {}
                }
            }
        }

        async fn send_vote(&self, target: &Uuid, vote_granted: bool) {
            let content =
                RaftMessageContent::RequestVoteResponse(RequestVoteResponseArgs { vote_granted });
            self.send_content_to(target, content).await;
        }
        async fn send_content_to(&self, target: &Uuid, content: RaftMessageContent) {
            self.message_sender
                .send(
                    target,
                    RaftMessage {
                        header: self.get_header(),
                        content,
                    },
                )
                .await;
        }

        async fn convert_to_follower(&mut self, term: u64) {
            debug!(
                "[{:?}] convert to follower",
                self.config.self_id.as_u128() % 100
            );
            if self.heartbeat_handle.is_some() {
                self.heartbeat_handle.take().unwrap().stop().await;
            }
            self.process_type = ProcessType::Follower;
            self.persistent_state.current_term = term;
            self.current_leader = None;
            self.persistent_state.voted_for = None;
        }

        async fn convert_to_follower_of(&mut self, leader_id: Uuid) {
            debug!(
                "[{:?}] convert to follower",
                self.config.self_id.as_u128() % 100
            );
            if self.heartbeat_handle.is_some() {
                self.heartbeat_handle.take().unwrap().stop().await;
            }
            self.process_type = ProcessType::Follower;
            self.current_leader = Some(leader_id);
            self.persistent_state.voted_for = Some(leader_id);
        }

        async fn nominate(&mut self) {
            self.process_type = ProcessType::Candidate {
                votes_received: HashSet::new(),
            };
            debug!(
                "[system] Server {:?} has nominated itself",
                self.config.self_id.as_u128() % 100
            );
            self.persistent_state.current_term += 1;
            self.persistent_state.voted_for = Some(self.config.self_id);
            self.current_leader = None;
            if 1 > self.config.servers.len() / 2 {
                debug!(
                    "[{:?}] Server has converted to leader",
                    self.config.self_id.as_u128() % 100
                );
                self.process_type = ProcessType::Leader {
                    // Initialize next_index to the length of the log of the leader
                    next_index: self
                        .config
                        .servers
                        .iter()
                        .map(|server_id| (*server_id, self.log_len()))
                        .collect(),
                    // Initialize match_index to 0 of each server
                    match_index: self
                        .config
                        .servers
                        .iter()
                        .map(|server_id| (*server_id, 0))
                        .collect(),
                    responded: HashSet::new(),
                    client_sessions: HashMap::new(),
                    snapshots: HashMap::new(),
                };
                self.persistent_state.log.push(LogEntry {
                    content: LogEntryContent::NoOp,
                    term: self.persistent_state.current_term,
                    timestamp: SystemTime::now(),
                });
                self.current_leader = Some(self.config.self_id);

                for server_id in self.config.servers.clone() {
                    if server_id != self.config.self_id {
                        self.send_heartbeat(server_id).await;
                    }
                }
            } else {
                self.save_state().await;

                self.request_vote().await;
            }
        }

        async fn request_vote(&mut self) {
            for server_id in self.config.servers.clone() {
                if server_id != self.config.self_id {
                    self.message_sender
                        .send(
                            &server_id,
                            RaftMessage {
                                header: self.get_header(),
                                content: RaftMessageContent::RequestVote(RequestVoteArgs {
                                    last_log_index: self.persistent_state.log.len() - 1,
                                    last_log_term: self.persistent_state.log.last().unwrap().term,
                                }),
                            },
                        )
                        .await;
                }
            }
        }
        async fn send_heartbeat(&mut self, server_id: Uuid) {
            let snapshot_key = self.snapshot_key();
            let log_len = self.log_len();
            if let ProcessType::Leader {
                next_index,
                match_index,
                responded: _,
                client_sessions: _,
                snapshots,
            } = &mut self.process_type
            {
                debug!(
                    "[{:?}] Sending heartbeat to {:?}, next_index: {:?}, match_index: {:?}, log_offset: {:?}",
                    self.config.self_id.as_u128() % 100,
                    server_id.as_u128()% 100,
                    next_index[&server_id],
                    match_index[&server_id],
                    self.persistent_state.log_offset

                );
                if match_index[&server_id] + 1 >= self.persistent_state.log_offset {
                    assert!(snapshots.get(&server_id).is_none());

                    let term = if next_index[&server_id] > self.persistent_state.log_offset {
                        self.persistent_state.log
                            [next_index[&server_id] - 1 - self.persistent_state.log_offset]
                            .term
                    } else {
                        self.persistent_state.snapshot_last_term.unwrap()
                    };
                    let entries = if match_index[&server_id] + 1 == next_index[&server_id] {
                        self.persistent_state.log[next_index[&server_id]
                            - self.persistent_state.log_offset
                            ..min(
                                log_len,
                                next_index[&server_id] + self.config.append_entries_batch_size,
                            ) - self.persistent_state.log_offset]
                            .to_vec()
                    } else {
                        vec![]
                    };
                    debug!("entries: {:?}", entries.len());

                    let content = RaftMessageContent::AppendEntries(AppendEntriesArgs {
                        prev_log_index: next_index[&server_id] - 1,
                        prev_log_term: term,
                        entries,
                        leader_commit: self.commit_index,
                    });

                    debug!(
                        "[{:?} -> {:?}] successful send , term: {:?}",
                        self.config.self_id.as_u128() % 100,
                        server_id.as_u128() % 100,
                        term
                    );

                    self.send_content_to(&server_id, content).await;
                } else {
                    let mut snapshot_info = snapshots.get(&server_id);
                    if snapshot_info.is_none() {
                        let data = self.stable_storage.get(&snapshot_key).await.unwrap();
                        debug!(
                            "[{:?}] Init Sending snapshot to {:?}, data: {:?}",
                            self.config.self_id.as_u128() % 100,
                            server_id.as_u128() % 100,
                            data.len()
                        );
                        snapshots.insert(
                            server_id,
                            SnapshotInfo {
                                data,
                                offset: 0,
                                last_included_index: 0,
                                last_included_term: 0,
                                last_config: None,
                                client_sessions: None,
                            },
                        );
                        snapshot_info = snapshots.get(&server_id);
                    }
                    let SnapshotInfo { data, offset, .. } = snapshot_info.unwrap();
                    let len = min(offset + self.config.snapshot_chunk_size, data.len());
                    let done = len == data.len();
                    let content = RaftMessageContent::InstallSnapshot(InstallSnapshotArgs {
                        last_included_index: self.persistent_state.log_offset - 1,
                        last_included_term: self.persistent_state.snapshot_last_term.unwrap(),
                        last_config: if *offset == 0 {
                            Some(self.config.servers.clone())
                        } else {
                            None
                        },
                        client_sessions: if *offset == 0 {
                            Some(self.client_sessions.clone())
                        } else {
                            None
                        },
                        offset: *offset,
                        data: data[*offset..len].to_vec(),
                        done,
                    });

                    debug!(
                        "[{:?} -> {:?}] succs to {:?}, offset: {:?}, len: {:?}, done: {:?}",
                        self.config.self_id.as_u128() % 100,
                        server_id.as_u128() % 100,
                        server_id.as_u128() % 100,
                        offset,
                        len,
                        done
                    );

                    if done {
                        snapshots.remove(&server_id);
                        match_index.insert(server_id, self.persistent_state.log_offset - 1);
                        let nid = next_index.get_mut(&server_id).unwrap();
                        if *nid < self.persistent_state.log_offset {
                            *nid = self.persistent_state.log_offset;
                        }
                    }
                    self.send_content_to(&server_id, content).await;
                }
            }
        }
    }

    fn select_random_timeout(range: &RangeInclusive<Duration>) -> Duration {
        let start = range.start().as_millis();
        let end = range.end().as_millis();
        let random_millis = rand::thread_rng().gen_range(start..=end);
        Duration::from_millis(random_millis as u64)
    }
}
