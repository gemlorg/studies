pub(crate) mod atomic_register_impl {
    use log::debug;
    use uuid::Uuid;

    use crate::atomic_register_public::AtomicRegister;
    use crate::{
        Broadcast, ClientRegisterCommand, ClientRegisterCommandContent, OperationReturn,
        OperationSuccess, ReadReturn, RegisterClient, SectorIdx, SectorVec, SectorsManager,
        SystemCommandHeader, SystemRegisterCommand, SystemRegisterCommandContent,
    };
    use core::fmt;
    use std::future::Future;
    use std::pin::Pin;
    use std::sync::Arc;
    pub(crate) type CallbackType =
        Box<dyn FnOnce(OperationSuccess) -> Pin<Box<dyn Future<Output = ()> + Send>> + Send + Sync>;
    struct Callback {
        callback: CallbackType,
        request_identifier: u64,
    }
    impl Callback {
        fn new(callback: CallbackType, request_identifier: u64) -> Self {
            Self {
                callback,
                request_identifier,
            }
        }
        fn call(self, op_return: OperationReturn) -> Pin<Box<dyn Future<Output = ()> + Send>> {
            (self.callback)(OperationSuccess {
                request_identifier: self.request_identifier,
                op_return,
            })
        }
    }

    #[derive(Clone)]
    struct RegisterOp(u64, u8, SectorVec);
    impl fmt::Debug for RegisterOp {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "RegisterOp({}, {}, _)", self.0, self.1)
        }
    }
    impl Eq for RegisterOp {}
    impl PartialEq for RegisterOp {
        fn eq(&self, other: &Self) -> bool {
            (self.0, self.1) == (other.0, other.1)
        }
    }
    impl PartialOrd for RegisterOp {
        fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
            Some((self.0, self.1).cmp(&(other.0, other.1)))
        }
    }
    impl Ord for RegisterOp {
        fn cmp(&self, other: &Self) -> std::cmp::Ordering {
            (self.0, self.1).cmp(&(other.0, other.1))
        }
    }
    pub(crate) struct AtomicRegisterImpl {
        self_ident: u8,
        sector_idx: SectorIdx,
        register_client: Arc<dyn RegisterClient>,
        sectors_manager: Arc<dyn SectorsManager>,
        processes_count: u8,
        reading: bool,
        writing: bool,
        write_phase: bool,
        writeval: Option<SectorVec>,
        callback: Option<Callback>,
        acklist: Vec<bool>,
        readlist: Vec<Option<RegisterOp>>,
        op_id: Uuid,
        readval: Option<SectorVec>,
    }

    impl AtomicRegisterImpl {
        //this is either init or recovery
        pub(crate) fn new(
            self_ident: u8,
            sector_idx: SectorIdx,
            register_client: Arc<dyn RegisterClient>,
            sectors_manager: Arc<dyn SectorsManager>,
            processes_count: u8,
        ) -> Self {
            debug!(
                "[system] id {} creating/recovering AtomicRegisterImpl",
                self_ident
            );
            Self {
                self_ident,
                sector_idx,
                register_client,
                sectors_manager,
                processes_count,
                reading: false,
                writing: false,
                write_phase: false,
                writeval: None,
                callback: None,
                acklist: vec![false; processes_count as usize],
                readlist: vec![None; processes_count as usize],
                op_id: Uuid::new_v4(),
                readval: None,
            }
        }
        // upon event < sbeb, Deliver | p [READ_PROC, id] > do
        // trigger < sl, Send | p, [VALUE, id, ts, wr, val] >;
        async fn handle_read_proc(&mut self, cmd: SystemCommandHeader) {
            debug!(
                "[system] id {} handling ReadProc from {} ",
                self.self_ident, cmd.process_identifier
            );
            let (ts, wr) = self.sectors_manager.read_metadata(self.sector_idx).await;
            let data = self.sectors_manager.read_data(self.sector_idx).await;
            self.register_client
                .send(crate::Send {
                    cmd: Arc::new(SystemRegisterCommand {
                        header: SystemCommandHeader {
                            process_identifier: self.self_ident,
                            msg_ident: cmd.msg_ident,
                            sector_idx: self.sector_idx,
                        },
                        content: SystemRegisterCommandContent::Value {
                            timestamp: ts,
                            write_rank: wr,
                            sector_data: data,
                        },
                    }),
                    target: cmd.process_identifier,
                })
                .await;
            debug!(
                "[system] id {} sent Value to {} ",
                self.self_ident, cmd.process_identifier
            );
        }

        // upon event <sl, Deliver | q, [VALUE, id, ts', wr', v'] > such that id == op_id and !write_phase do
        // readlist[q] := (ts', wr', v');
        // if #(readlist) > N / 2 and (reading or writing) then
        //     readlist[self] := (ts, wr, val);
        //     (maxts, rr, readval) := highest(readlist);
        //     readlist := [ _ ] `of length` N;
        //     acklist := [ _ ] `of length` N;
        //     write_phase := TRUE;
        //     if reading = TRUE then
        //         trigger < sbeb, Broadcast | [WRITE_PROC, op_id, maxts, rr, readval] >;
        //     else
        //         (ts, wr, val) := (maxts + 1, rank(self), writeval);
        //         store(ts, wr, val);
        //         trigger < sbeb, Broadcast | [WRITE_PROC, op_id, maxts + 1, rank(self), writeval] >;
        async fn handle_value(
            &mut self,
            cmd: SystemCommandHeader,
            ts: u64,
            wr: u8,
            data: SectorVec,
        ) {
            if self.write_phase || self.op_id != cmd.msg_ident {
                debug!(
                    "[system] id {} ignoring Value from {} ",
                    self.self_ident, cmd.process_identifier
                );
                return;
            }
            debug!(
                "[system] id {} handling Value from {} ",
                self.self_ident, cmd.process_identifier
            );
            self.readlist[cmd.process_identifier as usize - 1] = Some(RegisterOp(ts, wr, data));
            debug!(
                "[system] id {} readlist is now: {:?}",
                self.self_ident, self.readlist
            );
            if self.readlist.iter().filter(|x| x.is_some()).count()
                > (self.processes_count as usize) / 2
                && (self.reading || self.writing)
            {
                debug!("[syste] id {} triggering write phase", self.self_ident);

                let (ts, wr) = self.sectors_manager.read_metadata(self.sector_idx).await;
                let data = self.sectors_manager.read_data(self.sector_idx).await;
                self.readlist[self.self_ident as usize - 1] = Some(RegisterOp(ts, wr, data));

                let RegisterOp(maxts, rr, readval) = self
                    .readlist
                    .clone()
                    .iter()
                    .filter_map(|x| x.clone())
                    .max()
                    .unwrap();

                self.readval = Some(readval.clone());
                self.readlist = vec![None; self.processes_count as usize];
                self.acklist = vec![false; self.processes_count as usize];
                self.write_phase = true;
                if self.reading {
                    self.register_client
                        .broadcast(Broadcast {
                            cmd: Arc::new(SystemRegisterCommand {
                                header: SystemCommandHeader {
                                    // self or command?
                                    process_identifier: self.self_ident,
                                    msg_ident: cmd.msg_ident,
                                    sector_idx: cmd.sector_idx,
                                },
                                content: SystemRegisterCommandContent::WriteProc {
                                    timestamp: maxts,
                                    write_rank: rr,
                                    data_to_write: readval,
                                },
                            }),
                        })
                        .await;
                } else {
                    let ts = maxts + 1;
                    let wr = self.self_ident;
                    let val = self.writeval.clone().unwrap();
                    self.sectors_manager
                        .write(cmd.sector_idx, &(val.clone(), ts, wr))
                        .await;

                    self.register_client
                        .broadcast(Broadcast {
                            cmd: Arc::new(SystemRegisterCommand {
                                header: SystemCommandHeader {
                                    process_identifier: self.self_ident,
                                    msg_ident: cmd.msg_ident,
                                    sector_idx: self.sector_idx,
                                },
                                content: SystemRegisterCommandContent::WriteProc {
                                    timestamp: ts,
                                    write_rank: wr,
                                    data_to_write: val,
                                },
                            }),
                        })
                        .await;
                }
            }
        }
        // upon event < sbeb, Deliver | p, [WRITE_PROC, id, ts', wr', v'] > do
        // if (ts', wr') > (ts, wr) then
        //     (ts, wr, val) := (ts', wr', v');
        //     store(ts, wr, val);
        // trigger < sl, Send | p, [ACK, id] >;
        async fn handle_write_proc(
            &mut self,
            cmd: SystemCommandHeader,
            ts_other: u64,
            wr_other: u8,
            data_other: SectorVec,
        ) {
            debug!(
                "[system] id {} handling WriteProc from {} ",
                self.self_ident, cmd.process_identifier
            );
            let (ts, wr) = self.sectors_manager.read_metadata(self.sector_idx).await;
            if (ts_other, wr_other) > (ts, wr) {
                self.sectors_manager
                    .write(cmd.sector_idx, &(data_other.clone(), ts_other, wr_other))
                    .await;
            }
            self.register_client
                .send(crate::Send {
                    cmd: Arc::new(SystemRegisterCommand {
                        header: SystemCommandHeader {
                            process_identifier: self.self_ident,
                            msg_ident: cmd.msg_ident,
                            sector_idx: cmd.sector_idx,
                        },
                        content: SystemRegisterCommandContent::Ack,
                    }),
                    target: cmd.process_identifier,
                })
                .await;
        }

        //    upon event < sl, Deliver | q, [ACK, id] > such that id == op_id and write_phase do
        // acklist[q] := Ack;
        // if #(acklist) > N / 2 and (reading or writing) then
        //     acklist := [ _ ] `of length` N;
        //     write_phase := FALSE;
        //     if reading = TRUE then
        //         reading := FALSE;
        //         trigger < nnar, ReadReturn | readval >;
        //     else
        //         writing := FALSE;
        //         trigger < nnar, WriteReturn >;

        async fn handle_ack(&mut self, cmd: SystemCommandHeader) {
            if self.op_id != cmd.msg_ident || !self.write_phase {
                return;
            }
            debug!(
                "[system] id {} handling Ack from {} ",
                self.self_ident, cmd.process_identifier
            );
            self.acklist[cmd.process_identifier as usize - 1] = true;
            debug!(
                "[system] id {} acklist is now: {:?}",
                self.self_ident, self.acklist
            );
            if self.acklist.iter().filter(|x| **x).count() > (self.processes_count as usize) / 2
                && (self.reading || self.writing)
            {
                debug!(
                    "[system] id {} triggering end of write phase",
                    self.self_ident
                );
                self.acklist = vec![false; self.processes_count as usize];
                self.write_phase = false;
                if self.reading {
                    self.reading = false;
                    let call = self.callback.take();
                    self.callback = None;
                    call.unwrap()
                        .call(OperationReturn::Read(ReadReturn {
                            read_data: self.readval.clone().unwrap(),
                        }))
                        .await;
                } else {
                    self.writing = false;
                    let call = self.callback.take();
                    self.callback = None;
                    call.unwrap().call(OperationReturn::Write).await;
                }
            }
        }
    }

    #[async_trait::async_trait]
    impl AtomicRegister for AtomicRegisterImpl {
        /// (N,N)-AtomicRegister algorithm.
        ///upon event < nnar, Read > do
        // op_id := generate_unique_id();
        // readlist := [ _ ] `of length` N;
        // acklist := [ _ ] `of length` N;
        // reading := TRUE;
        // trigger < sbeb, Broadcast | [READ_PROC, op_id] >;

        async fn client_command(
            &mut self,
            cmd: ClientRegisterCommand,
            success_callback: Box<
                dyn FnOnce(OperationSuccess) -> Pin<Box<dyn Future<Output = ()> + Send>>
                    + Send
                    + Sync,
            >,
        ) {
            self.op_id = Uuid::new_v4();
            self.readlist = vec![None; self.processes_count as usize];
            self.acklist = vec![false; self.processes_count as usize];

            match cmd.content {
                ClientRegisterCommandContent::Read => {
                    debug!(
                        "[client] id {} handling Read from {} ",
                        self.self_ident, cmd.header.request_identifier
                    );
                    self.reading = true;
                }
                ClientRegisterCommandContent::Write { data } => {
                    debug!(
                        "[client] id {} handling Write from {} ",
                        self.self_ident, cmd.header.request_identifier
                    );
                    self.writeval = Some(data);
                    self.writing = true;
                }
            }
            debug!("[system] id {} broadcasting ReadProc", self.self_ident);
            self.callback = Some(Callback::new(
                success_callback,
                cmd.header.request_identifier,
            ));
            self.register_client
                .broadcast(Broadcast {
                    cmd: Arc::new(SystemRegisterCommand {
                        header: SystemCommandHeader {
                            process_identifier: self.self_ident,
                            msg_ident: self.op_id,
                            sector_idx: self.sector_idx,
                        },
                        content: SystemRegisterCommandContent::ReadProc,
                    }),
                })
                .await;
        }

        /// Handle a system command.
        ///
        /// This function corresponds to the handlers of READ_PROC, VALUE, WRITE_PROC
        /// and ACK messages in the (N,N)-AtomicRegister algorithm.
        async fn system_command(&mut self, cmd: SystemRegisterCommand) {
            match cmd.clone().content {
                SystemRegisterCommandContent::ReadProc => self.handle_read_proc(cmd.header).await,
                SystemRegisterCommandContent::Value {
                    timestamp,
                    write_rank,
                    sector_data,
                } => {
                    self.handle_value(cmd.header, timestamp, write_rank, sector_data)
                        .await
                }
                SystemRegisterCommandContent::WriteProc {
                    timestamp,
                    write_rank,
                    data_to_write,
                } => {
                    self.handle_write_proc(cmd.header, timestamp, write_rank, data_to_write)
                        .await
                }
                SystemRegisterCommandContent::Ack => self.handle_ack(cmd.header).await,
            }
        }
    }
}
