pub(crate) mod message_handler {
    use std::{
        collections::{HashMap, HashSet},
        time::SystemTime,
    };

    use log::debug;
    use module_system::{Module, ModuleRef};

    use crate::{domain::*, ProcessType, Raft};
    use crate::{utils::raft_utils::*, SnapshotInfo};

    use async_trait::async_trait;
    #[async_trait]
    pub trait RaftMessageHandler: Module {
        async fn handle_append_entries(
            &mut self,
            header: RaftMessageHeader,
            args: AppendEntriesArgs,
            self_ref: &ModuleRef<Self>,
        );

        async fn handle_append_entries_response(
            &mut self,
            header: RaftMessageHeader,
            args: AppendEntriesResponseArgs,
        );

        async fn handle_request_vote(&mut self, header: RaftMessageHeader, args: RequestVoteArgs);

        async fn handle_request_vote_response(
            &mut self,
            header: RaftMessageHeader,
            args: RequestVoteResponseArgs,
            self_ref: &ModuleRef<Self>,
        );
        async fn handle_install_snapshot(
            &mut self,
            header: RaftMessageHeader,
            args: InstallSnapshotArgs,
            self_ref: &ModuleRef<Self>,
        );
        async fn handle_install_snapshot_response(
            &mut self,
            header: RaftMessageHeader,
            args: InstallSnapshotResponseArgs,
        );
    }

    #[async_trait]
    impl RaftMessageHandler for Raft {
        // Receiver implementation:
        // 1. Reply false if term < currentTerm (§3.3)
        // 2. Reply false if log doesn't contain an entry at prevLogIndex whose term matches prevLogTerm (§3.5)
        // 3. If an existing entry conflicts with a new one (same index but different terms), delete the existing entry and all that follow it (§3.5)
        // 4. Append any new entries not already in the log
        // 5. If leaderCommit > commitIndex, set commitIndex =
        // min(leaderCommit, index of last new entry)
        async fn handle_append_entries(
            &mut self,
            header: RaftMessageHeader,
            args: AppendEntriesArgs,
            self_ref: &ModuleRef<Self>,
        ) {
            self.reset_timer(self_ref).await;
            self.convert_to_follower_of(header.source).await;
            if self.log_len() <= args.prev_log_index
                || (args.prev_log_index + 1 == self.persistent_state.log_offset
                    && args.prev_log_term != self.persistent_state.snapshot_last_term.unwrap())
                || (args.prev_log_index + 1 > self.persistent_state.log_offset
                    && self.persistent_state.log[self.log_index(args.prev_log_index)].term
                        != args.prev_log_term)
            {
                self.send_entries(
                    &header.source,
                    false,
                    args.prev_log_index + args.entries.len(),
                )
                .await;
            } else {
                let mut i = args.prev_log_index + 1;
                let mut are_same = true;
                for entry in args.entries {
                    if i < self.persistent_state.log_offset {
                        i += 1;
                        continue;
                    }
                    if are_same
                        && (i >= self.log_len()
                            || self.persistent_state.log[self.log_index(i)].term != entry.term)
                    {
                        self.persistent_state.log.truncate(self.log_index(i));
                        are_same = false;
                    }
                    if !are_same {
                        self.persistent_state.log.push(entry);
                    }
                    i += 1;
                }
                if self.commit_index < args.leader_commit {
                    self.commit_index = std::cmp::min(args.leader_commit, i - 1);
                    self.apply_commits().await;
                }
                self.send_entries(&header.source, true, i - 1).await;
            }
        }

        async fn handle_append_entries_response(
            &mut self,
            header: RaftMessageHeader,
            args: AppendEntriesResponseArgs,
        ) {
            // If the process type is a leader, proceed with handling the response

            if let ProcessType::Leader {
                next_index,
                match_index,
                responded,
                ..
            } = &mut self.process_type
            {
                responded.insert(header.source);
                if args.success {
                    // debug!(
                    //     "[{:?}] Append entries response success, new match index: {:?}",
                    //     self.config.self_id.as_u128() % 100,
                    //     args.last_verified_log_index
                    // );
                    // Update match_index and next_index
                    let source = &header.source;
                    let last_verified_log_index = args.last_verified_log_index;

                    let current_match_index = match_index.get_mut(source).unwrap();
                    if *current_match_index < last_verified_log_index {
                        *current_match_index = last_verified_log_index;
                    }
                    let current_next_index = next_index.get_mut(source).unwrap();
                    if *current_next_index <= last_verified_log_index {
                        *current_next_index = last_verified_log_index + 1;
                    }
                    assert!(match_index[source] + 1 == next_index[source]);

                    // Send append entries if the indices are mismatched
                    if match_index[source] + 1 < self.log_len() {
                        self.send_heartbeat(header.source).await;
                    }
                } else {
                    next_index.insert(header.source, next_index[&header.source] - 1);
                    self.send_heartbeat(header.source).await;
                }
            }
            self.try_make_commit();
            self.apply_commits().await;
        }

        async fn handle_request_vote(&mut self, header: RaftMessageHeader, args: RequestVoteArgs) {
            // debug!(
            //     "[{:?}] Handling vote, term: {:?}, log_len: {:?}, args: {:?}",
            //     self.config.self_id.as_u128() % 100,
            //     self.persistent_state.current_term,
            //     self.log_len(),
            //     args
            // );
            assert!(self.current_leader.is_none());
            assert!(self.persistent_state.current_term == header.term);

            let mut vote = false;
            if self.log_len() - 1 > args.last_log_index
                || (self.log_len() - 1 == args.last_log_index
                    && (self.persistent_state.log.is_empty()
                        || self.persistent_state.log.last().unwrap().term != args.last_log_term))
            {
                vote = false;
            } else if let ProcessType::Leader { .. } = self.process_type {
                vote = false;
            } else if self.persistent_state.voted_for.is_none()
                || self.persistent_state.voted_for == Some(header.source)
            {
                vote = true;
            }
            self.send_vote(&header.source, vote).await;
        }

        async fn handle_request_vote_response(
            &mut self,
            header: RaftMessageHeader,
            args: RequestVoteResponseArgs,
            _self_ref: &ModuleRef<Self>,
        ) {
            if let ProcessType::Candidate { votes_received } = &mut self.process_type {
                if args.vote_granted {
                    votes_received.insert(header.source);
                    if votes_received.len() + 1 > self.config.servers.len() / 2 {
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
                        self.reset_timer(_self_ref).await;
                        self.restart_heartbeat_timer(_self_ref).await;

                        for server_id in self.config.servers.clone() {
                            if server_id != self.config.self_id {
                                self.send_heartbeat(server_id).await;
                            }
                        }
                    }
                }
            }
        }

        async fn handle_install_snapshot(
            &mut self,
            header: RaftMessageHeader,
            args: InstallSnapshotArgs,
            self_ref: &ModuleRef<Self>,
        ) {
            self.reset_timer(self_ref).await;
            self.convert_to_follower_of(header.source).await;
            let mut current_snapshot = self
                .stable_storage
                .get(&self.leader_snapshot_key(header.source))
                .await;

            if args.offset == 0 {
                let snapshot_info = SnapshotInfo {
                    last_included_index: args.last_included_index,
                    last_included_term: args.last_included_term,
                    last_config: Some(args.last_config.unwrap()),
                    client_sessions: Some(args.client_sessions.unwrap()),
                    data: vec![],
                    offset: 0,
                };
                self.stable_storage
                    .put(
                        &self.leader_snapshot_key(header.source),
                        &bincode::serialize(&snapshot_info).unwrap(),
                    )
                    .await
                    .unwrap();
                current_snapshot = Some(bincode::serialize(&snapshot_info).unwrap());
            }
            let mut snap: SnapshotInfo = bincode::deserialize(&current_snapshot.unwrap()).unwrap();

            if args.offset != snap.data.len() {
                debug!(
                    "[{:?}] got faulty snapshot request, ignoring: offset: {:?}, len: {:?}",
                    self.config.self_id.as_u128() % 100,
                    args.offset,
                    snap.data.len()
                );
                return;
            }
            snap.offset = args.offset + args.data.len();
            snap.data.extend_from_slice(&args.data);
            if !args.done {
                self.stable_storage
                    .put(
                        &self.leader_snapshot_key(header.source),
                        &bincode::serialize(&snap).unwrap(),
                    )
                    .await
                    .unwrap();

                self.send_snapshot_response(&header.source, args.offset, args.last_included_index)
                    .await;
                return;
            }
            self.stable_storage
                .put(&self.leader_snapshot_key(header.source), &[])
                .await
                .unwrap();

            if snap.last_included_index >= self.persistent_state.log_offset {
                self.stable_storage
                    .put(&self.snapshot_key(), snap.data.as_slice())
                    .await
                    .unwrap();
                if snap.last_included_index < self.log_len()
                    && snap.last_included_term
                        == self.persistent_state.log[self.log_index(snap.last_included_index)].term
                {
                    self.persistent_state.log = self.persistent_state.log
                        [(self.log_index(snap.last_included_index) + 1)..]
                        .to_vec();
                } else {
                    self.persistent_state.log = vec![];
                }

                self.persistent_state.snapshot_last_term = Some(snap.last_included_term);
                self.persistent_state.log_offset = snap.last_included_index + 1;
                self.commit_index = std::cmp::max(self.commit_index, snap.last_included_index);
                self.last_applied = self.commit_index;
                self.client_sessions = snap.client_sessions.unwrap();
                self.save_state().await;
                self.state_machine.initialize(&snap.data).await;
            }
        }

        async fn handle_install_snapshot_response(
            &mut self,
            header: RaftMessageHeader,
            args: InstallSnapshotResponseArgs,
        ) {
            if let ProcessType::Leader {
                snapshots,
                responded,
                ..
            } = &mut self.process_type
            {
                let total_len = snapshots.get(&header.source).unwrap().data.len();
                let snap = snapshots.get_mut(&header.source).unwrap();
                snap.offset = args.offset + self.config.snapshot_chunk_size;
                responded.insert(header.source);
                if snap.offset < total_len {
                    self.send_heartbeat(header.source).await;
                }
            }
        }
    }
}
