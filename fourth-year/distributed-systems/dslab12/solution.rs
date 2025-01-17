use log::debug;
use module_system::{Handler, ModuleRef, System};
use std::{future, sync::Arc};
use tokio::sync::{Mutex, Semaphore};

/// Marker trait indicating that a broadcast implementation provides
/// guarantees specified in the assignment description.
pub(crate) trait ReliableBroadcast<const N: usize> {}

#[async_trait::async_trait]
pub(crate) trait ReliableBroadcastRef<const N: usize>: Send + Sync + 'static {
    async fn send(&self, msg: Operation);
}

#[async_trait::async_trait]
impl<T, const N: usize> ReliableBroadcastRef<N> for ModuleRef<T>
where
    T: ReliableBroadcast<N> + Handler<Operation> + Send,
{
    async fn send(&self, msg: Operation) {
        self.send(msg).await;
    }
}

/// Marker trait indicating that a client implementation
/// follows specification from the assignment description.
pub(crate) trait EditorClient {}

#[async_trait::async_trait]
pub(crate) trait ClientRef: Send + Sync + 'static {
    async fn send(&self, msg: Edit);
}

#[async_trait::async_trait]
impl<T> ClientRef for ModuleRef<T>
where
    T: EditorClient + Handler<Edit> + Send,
{
    async fn send(&self, msg: Edit) {
        self.send(msg).await;
    }
}

/// Actions (edits) which can be applied to a text.
#[derive(Clone, Debug, PartialEq)]
// #[cfg_attr(test, derive(PartialEq, Debug))]
pub(crate) enum Action {
    /// Insert the character at the position.
    Insert { idx: usize, ch: char },
    /// Delete a character at the position.
    Delete { idx: usize },
    /// A _do nothing_ operation. `Nop` cannot be issued by a client.
    /// `Nop` can only be issued by a process or result from a transformation.
    Nop,
}

impl Action {
    /// Apply the action to the text.
    pub(crate) fn apply_to(&self, text: &mut String) {
        match self {
            Action::Insert { idx, ch } => {
                text.insert(*idx, *ch);
            }
            Action::Delete { idx } => {
                text.remove(*idx);
            }
            Action::Nop => {
                // Do nothing.
            }
        }
    }
}

/// Client's request to edit the text.
#[derive(Clone)]
pub(crate) struct EditRequest {
    /// Total number of operations a client has applied to its text so far.
    pub(crate) num_applied: usize,
    /// Action (edit) to be applied to a text.
    pub(crate) action: Action,
}

/// Response to a client with action (edit) it should apply to its text.
#[derive(Clone)]
pub(crate) struct Edit {
    pub(crate) action: Action,
}

#[derive(Clone, Debug)]
pub(crate) struct Operation {
    /// Rank of a process which issued this operation.
    pub(crate) process_rank: usize,
    /// Action (edit) to be applied to a text.
    pub(crate) action: Action,
}

impl Operation {
    // Add any methods you need.
}

/// Process of the system.
pub(crate) struct Process<const N: usize> {
    /// Rank of the process.
    rank: usize,
    /// Reference to the broadcast module.
    broadcast: Box<dyn ReliableBroadcastRef<N>>,
    /// Reference to the process's client.
    client: Box<dyn ClientRef>,
    // Add any fields you need.
    // semaphore to ensure that only one client can add operations to the queue
    client_semaphore: Arc<Semaphore>,
    // table of semaphores for each of the processes. we need that to make sure that the operations are processed in rounds
    // table of length N
    system_semaphores: [Arc<Semaphore>; N],
    // internal semaphore to ensure that the text/variables are edited by only one process at a time
    internal_semaphore: Arc<Semaphore>,
    // make sure that client's operations are processed in order
    next_op: usize,
    log: Vec<Operation>,
    num_applied: usize,
    log_start_len: usize,
}

impl<const N: usize> Process<N> {
    pub(crate) async fn new(
        system: &mut System,
        rank: usize,
        broadcast: Box<dyn ReliableBroadcastRef<N>>,
        client: Box<dyn ClientRef>,
    ) -> ModuleRef<Self> {
        let self_ref = system
            .register_module(Self {
                rank,
                broadcast,
                client,
                // Add any fields you need.
                client_semaphore: Arc::new(Semaphore::new(1)),
                system_semaphores: std::array::from_fn(|_| Arc::new(Semaphore::new(1))),
                internal_semaphore: Arc::new(Semaphore::new(1)),
                next_op: 0,
                log: Vec::new(),
                num_applied: 0,
                log_start_len: 0,
            })
            .await;
        self_ref
    }

    // Add any methods you need.
}

#[async_trait::async_trait]
impl<const N: usize> Handler<Operation> for Process<N> {
    async fn handle(&mut self, _self_ref: &ModuleRef<Self>, msg: Operation) {
        debug!(
            "Process {} received system operation {:?} ",
            self.rank,
            msg.clone(),
        );
        // is it safe?
        let client_lock = self.client_semaphore.clone().try_acquire_owned();
        //first acquire the system semaphore
        let system_lock = self.system_semaphores[msg.process_rank]
            .clone()
            .acquire_owned()
            .await
            .unwrap();
        // then acquire the internal semaphore
        let internal_lock = self.internal_semaphore.acquire().await.unwrap();
        // adjust the number of recieved operations
        self.num_applied += 1;
        // handle the operation
        let broadcast_future = if self.log.len() == self.log_start_len {
            // log nop from myself only here

            self.log.push(Operation {
                process_rank: self.rank,
                action: Action::Nop,
            });
            debug!("Process {} broadcasting NOP", self.rank);
            self.client
                .send(Edit {
                    action: Action::Nop,
                })
                .await;
            self.broadcast.send(Operation {
                process_rank: self.rank,
                action: Action::Nop,
            })
            //write to the server
        } else {
            // ready
            Box::pin(future::ready(()))
        };
        // perform the transformation
        //Transform insert(p1, c1, r1) wrt. insert(p2, c2, r2) :-
        //   if p1 < p2: insert(p1, c1, r1)
        //   if p1 = p2 and r1 < r2: insert(p1, c1, r1)
        //   else: insert(p1 + 1, c1, r1)

        // Transform delete(p1, r1) wrt. delete(p2, r2) :-
        //   if p1 < p2: delete(p1, r1)
        //   if p1 = p2: NOP (do not modify text)
        //   else: delete(p1 - 1, r1)

        // Transform insert(p1, c1, r1) wrt. delete(p2, r2) :-
        //   if p1 <= p2: insert(p1, c1, r1)
        //   else: insert(p1 - 1, c1, r1)

        // Transform delete(p1, r1) wrt. insert(p2, c2, r2) :-
        //   if p1 < p2: delete(p1, r1)
        //   else: delete(p1 + 1, r1)
        // all actions in log from the current round
        // write the message back to the server
        // if not nop from itself, send back to the client
        let mut action = transform_action(msg.clone(), &self.log, self.log_start_len);

        self.log.push(Operation {
            process_rank: msg.process_rank,
            action: action.clone(),
        });
        self.client.send(Edit { action }).await;

        if (self.num_applied == N - 1) {
            debug!("Process {} finished round", self.rank);
            self.num_applied = 0;
            self.next_op += 1;
            self.log_start_len = self.log.len();
            self.client_semaphore.add_permits(1);
            for i in 0..N {
                self.system_semaphores[i].add_permits(1);
            }
        }
        drop(internal_lock);
        broadcast_future.await;
        // release internal lock
    }
}

fn transform_action(msg: Operation, log: &Vec<Operation>, log_start_len: usize) -> Action {
    let mut action = msg.action.clone();
    for current_action in log[log_start_len..].iter() {
        action = match action.clone() {
            Action::Insert { idx: idx1, ch: ch1 } => match current_action.action.clone() {
                Action::Insert { idx: idx2, ch: ch2 } => {
                    let idx = if idx1 < idx2
                        || (idx1 == idx2 && msg.process_rank < current_action.process_rank)
                    {
                        idx1
                    } else {
                        idx1 + 1
                    };
                    Action::Insert { idx, ch: ch1 }
                }
                Action::Delete { idx: idx2 } => {
                    let idx = if idx1 <= idx2 { idx1 } else { idx1 - 1 };
                    Action::Insert { idx, ch: ch1 }
                }
                Action::Nop => msg.action.clone(),
            },
            Action::Delete { idx: idx1 } => match current_action.action.clone() {
                Action::Insert { idx: idx2, ch: ch2 } => {
                    let idx = if idx1 < idx2 { idx1 } else { idx1 + 1 };
                    Action::Delete { idx }
                }
                Action::Delete { idx: idx2 } => {
                    let idx = if idx1 < idx2 { idx1 } else { idx1 - 1 };
                    if idx1 == idx2 {
                        Action::Nop
                    } else {
                        Action::Delete { idx }
                    }
                }
                Action::Nop => msg.action.clone(),
            },
            Action::Nop =>
            // do nothing
            {
                msg.action.clone()
            }
        };
    }
    action
}

#[async_trait::async_trait]
impl<const N: usize> Handler<EditRequest> for Process<N> {
    async fn handle(&mut self, _self_ref: &ModuleRef<Self>, request: EditRequest) {
        debug!(
            "Process {} received client operation {:?}",
            self.rank,
            request.action.clone()
        );
        let mut client_lock = self.client_semaphore.clone().acquire_owned().await.unwrap();
        let mut internal_lock = self.internal_semaphore.acquire().await.unwrap();
        while self.log.len() != self.log_start_len {
            debug!(
                "Process {} can't insert the client's operation in this round",
                self.rank
            );
            drop(internal_lock);
            client_lock = self.client_semaphore.clone().acquire_owned().await.unwrap();
            internal_lock = self.internal_semaphore.acquire().await.unwrap();
        }
        debug!("Process {}  continues client operation", self.rank);
        let action = transform_action(
            Operation {
                process_rank: self.rank,
                action: request.action.clone(),
            },
            &self.log,
            request.num_applied,
        );
        self.log.push(Operation {
            process_rank: self.rank,
            action: action.clone(),
        });
        self.client
            .send(Edit {
                action: request.action.clone(),
            })
            .await;

        self.broadcast
            .send(Operation {
                process_rank: self.rank,
                action: request.action,
            })
            .await;

        drop(internal_lock);
        debug!("Process {} sent client operation", self.rank);
    }
}
