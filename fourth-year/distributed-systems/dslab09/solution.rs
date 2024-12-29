use module_system::{Handler, ModuleRef};
use std::future::Future;
use std::pin::Pin;
use tokio::sync::oneshot::Sender;
use tokio::sync::{Mutex, Notify};
use uuid::Uuid;
// use arc
use std::sync::Arc;

#[derive(Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Debug)]
#[repr(u8)]
pub(crate) enum ProductType {
    Electronics,
    Toys,
    Books,
}

#[derive(Clone)]
pub(crate) struct StoreMsg {
    sender: ModuleRef<DistributedStore>,
    content: StoreMsgContent,
}

#[derive(Clone, Debug)]
pub(crate) enum StoreMsgContent {
    /// Transaction Manager initiates voting for the transaction.
    RequestVote(Transaction),
    /// If every process is ok with transaction, TM issues commit.
    Commit,
    /// System-wide abort.
    Abort,
}

#[derive(Clone)]
pub(crate) struct NodeMsg {
    content: NodeMsgContent,
}

#[derive(Clone, Debug)]
pub(crate) enum NodeMsgContent {
    /// Process replies to TM whether it can/cannot commit the transaction.
    RequestVoteResponse(TwoPhaseResult),
    /// Process acknowledges to TM committing/aborting the transaction.
    FinalizationAck,
}

pub(crate) struct TransactionMessage {
    /// Request to change price.
    pub(crate) transaction: Transaction,

    /// Called after 2PC completes (i.e., the transaction was decided to be
    /// committed/aborted by DistributedStore). This must be called after responses
    /// from all processes acknowledging commit or abort are collected.
    pub(crate) completed_callback:
        Box<dyn FnOnce(TwoPhaseResult) -> Pin<Box<dyn Future<Output = ()> + Send>> + Send>,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub(crate) enum TwoPhaseResult {
    Ok,
    Abort,
}

#[derive(Copy, Clone, Debug)]
pub(crate) struct Product {
    pub(crate) identifier: Uuid,
    pub(crate) pr_type: ProductType,
    pub(crate) price: u64,
}

#[derive(Copy, Clone, Debug)]
pub(crate) struct Transaction {
    pub(crate) pr_type: ProductType,
    pub(crate) shift: i32,
}

#[derive(Debug)]
pub(crate) struct ProductPriceQuery {
    pub(crate) product_ident: Uuid,
    pub(crate) result_sender: Sender<ProductPrice>,
}

#[derive(Copy, Clone, Debug)]
pub(crate) struct ProductPrice(pub(crate) Option<u64>);

/// Message which disables a node. Used for testing.
pub(crate) struct Disable;

/// DistributedStore.
/// This structure serves as TM.
// Add any fields you need.
pub(crate) struct DistributedStore {
    nodes: Vec<ModuleRef<Node>>,
    callback:
        Option<Box<dyn FnOnce(TwoPhaseResult) -> Pin<Box<dyn Future<Output = ()> + Send>> + Send>>,
    response: Option<TwoPhaseResult>,
    votes_ok: usize,
    votes_no: usize,
    votes_ack: usize,
}

impl DistributedStore {
    pub(crate) fn new(nodes: Vec<ModuleRef<Node>>) -> Self {
        Self {
            nodes,
            callback: None,
            response: None,
            votes_ok: 0,
            votes_no: 0,
            votes_ack: 0,
        }
    }
}

/// Node of DistributedStore.
/// This structure serves as a process of the distributed system.
// Add any fields you need.
pub(crate) struct Node {
    products: Vec<Product>,
    pending_transaction: Option<Transaction>,
    enabled: bool,
    sem: Arc<Mutex<Option<ProductType>>>,
    notify: Arc<Notify>,
}

impl Node {
    pub(crate) fn new(products: Vec<Product>) -> Self {
        let notify = Arc::new(Notify::new());
        notify.notify_one();
        Self {
            products,
            pending_transaction: None,
            enabled: true,
            sem: Arc::new(Mutex::new(None)),
            notify,
        }
    }
}

#[async_trait::async_trait]
impl Handler<NodeMsg> for DistributedStore {
    async fn handle(&mut self, self_ref: &ModuleRef<Self>, msg: NodeMsg) {
        match msg.content {
            NodeMsgContent::RequestVoteResponse(TwoPhaseResult::Ok) => {
                self.votes_ok += 1;
            }
            NodeMsgContent::RequestVoteResponse(TwoPhaseResult::Abort) => {
                self.votes_no += 1;
            }
            NodeMsgContent::FinalizationAck => {
                self.votes_ack += 1;
            }
        }
        if (self.votes_ok + self.votes_no) == self.nodes.len() {
            let response = if self.votes_no > 0 {
                StoreMsgContent::Abort
            } else {
                StoreMsgContent::Commit
            };
            self.response = Some(if self.votes_no > 0 {
                TwoPhaseResult::Abort
            } else {
                TwoPhaseResult::Ok
            });

            let mut futures = Vec::new();
            for node in self.nodes.iter() {
                futures.push(node.send(StoreMsg {
                    sender: self_ref.clone(),
                    content: response.clone(),
                }));
            }
            for fut in futures {
                fut.await;
            }
            self.votes_ok = 0;
            self.votes_no = 0;
        }
        if self.votes_ack == self.nodes.len() {
            self.votes_ack = 0;
            self.callback.take().unwrap()(self.response.unwrap()).await;
            self.callback = None;
            self.response = None;
        }
    }
}

#[async_trait::async_trait]
impl Handler<StoreMsg> for Node {
    async fn handle(&mut self, _self_ref: &ModuleRef<Self>, msg: StoreMsg) {
        if self.enabled {
            match msg.content {
                StoreMsgContent::RequestVote(transaction) => {
                    self.notify.notified().await;
                    let mut lock = self.sem.lock().await;
                    self.pending_transaction = Some(transaction);
                    *lock = Some(transaction.pr_type);
                    let mut response = None;
                    for item in self.products.iter() {
                        if item.pr_type != transaction.pr_type {
                            continue;
                        }
                        if transaction.shift < 0 && item.price <= (-transaction.shift) as u64 {
                            response = Some(TwoPhaseResult::Abort);
                            break;
                        }
                    }
                    if response.is_none() {
                        response = Some(TwoPhaseResult::Ok);
                    }
                    msg.sender
                        .send(NodeMsg {
                            content: NodeMsgContent::RequestVoteResponse(response.unwrap()),
                        })
                        .await;
                }
                StoreMsgContent::Commit => {
                    let mut lock = self.sem.lock().await;
                    let transaction = self.pending_transaction.unwrap();
                    for item in self.products.iter_mut() {
                        if item.pr_type == transaction.pr_type {
                            item.price = if transaction.shift < 0 {
                                item.price - (-transaction.shift) as u64
                            } else {
                                item.price + transaction.shift as u64
                            };
                        }
                    }
                    self.pending_transaction = None;
                    *lock = None;
                    self.notify.notify_one();
                    msg.sender
                        .send(NodeMsg {
                            content: NodeMsgContent::FinalizationAck,
                        })
                        .await;
                }
                StoreMsgContent::Abort => {
                    let mut lock = self.sem.lock().await;
                    self.pending_transaction = None;
                    *lock = None;
                    self.notify.notify_one();
                    msg.sender
                        .send(NodeMsg {
                            content: NodeMsgContent::FinalizationAck,
                        })
                        .await;
                }
            }
        }
    }
}

#[async_trait::async_trait]
impl Handler<ProductPriceQuery> for Node {
    async fn handle(&mut self, _self_ref: &ModuleRef<Self>, msg: ProductPriceQuery) {
        if self.enabled {
            let mut ind = None;
            for i in 0..self.products.len() {
                if self.products[i].identifier == msg.product_ident {
                    ind = Some(i);
                    break;
                }
            }
            if ind.is_none() {
                msg.result_sender.send(ProductPrice(None)).unwrap();
                return;
            }
            let typ = self.products[ind.unwrap()].pr_type;
            let mut lock = self.sem.lock().await;
            while *lock == Some(typ) {
                drop(lock);
                self.notify.notified().await;
                lock = self.sem.lock().await;
                self.notify.notify_one();
            }
            msg.result_sender
                .send(ProductPrice(Some(self.products[ind.unwrap()].price)))
                .unwrap();
            drop(lock);
        }
    }
}

#[async_trait::async_trait]
impl Handler<Disable> for Node {
    async fn handle(&mut self, _self_ref: &ModuleRef<Self>, _msg: Disable) {
        self.enabled = false;
    }
}

#[async_trait::async_trait]
impl Handler<TransactionMessage> for DistributedStore {
    async fn handle(&mut self, self_ref: &ModuleRef<Self>, msg: TransactionMessage) {
        let mut futures = Vec::new();
        self.callback = Some(msg.completed_callback);
        for node in self.nodes.iter() {
            futures.push(node.send(StoreMsg {
                sender: self_ref.clone(),
                content: StoreMsgContent::RequestVote(msg.transaction),
            }));
        }
        for fut in futures {
            fut.await;
        }
    }
}
