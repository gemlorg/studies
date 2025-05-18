use std::sync::Arc;
use std::time::Duration;
use tokio::sync::mpsc::{unbounded_channel, UnboundedReceiver, UnboundedSender};
use tokio::sync::Mutex;
//another mutex
pub trait Message: Send + 'static {}
impl<T: Send + 'static> Message for T {}

pub trait Module: Send + 'static {}
impl<T: Send + 'static> Module for T {}

#[async_trait::async_trait]
trait Handlee<T: Module + ?Sized>: Message {
    async fn get_handled(self: Box<Self>, module_ref: &ModuleRef<T>, module: &mut T);
}

#[async_trait::async_trait]
impl<M: Message, T: Handler<M>> Handlee<T> for M {
    async fn get_handled(self: Box<Self>, module_ref: &ModuleRef<T>, module: &mut T) {
        module.handle(module_ref, *self).await;
    }
}

/// A trait for modules capable of handling messages of type `M`.
#[async_trait::async_trait]
pub trait Handler<M: Message>: Module {
    /// Handles the message. A module must be able to access a `ModuleRef` to itself through `self_ref`.
    async fn handle(&mut self, self_ref: &ModuleRef<Self>, msg: M);
}

/// A handle returned by `ModuleRef::request_tick()`, can be used to stop sending further ticks.
// You can add fields to this struct
pub struct TimerHandle {
    stop_tx: tokio::sync::mpsc::UnboundedSender<()>,
}

impl TimerHandle {
    // tokyo unbonded some(tokio unwrapped channel)

    /// Stops the sending of ticks resulting from the corresponding call to `ModuleRef::request_tick()`.
    /// If the ticks are already stopped, does nothing.
    pub async fn stop(&self) {
        let _ = self.stop_tx.send(());
    }
}

impl Clone for TimerHandle {
    /// Creates a new reference to the same timer handle.
    fn clone(&self) -> Self {
        TimerHandle {
            stop_tx: self.stop_tx.clone(),
        }
    }
}
// You can add fields to this struct.
pub struct System {
    // array of modules
    stop_module_txs: Mutex<Vec<tokio::sync::mpsc::UnboundedSender<()>>>,
    stop_module_reply_rx: tokio::sync::mpsc::UnboundedReceiver<()>,
    stop_module_reply_tx: tokio::sync::mpsc::UnboundedSender<()>,
    stop_ticker_txs: Arc<Mutex<Vec<tokio::sync::mpsc::UnboundedSender<()>>>>,
}
pub(crate) enum SysMessage<T: Module + ?Sized> {
    Message(Box<dyn Handlee<T> + Send>),
}

impl System {
    /// Registers the module in the system.
    /// Returns a `ModuleRef`, which can be used then to send messages to the module.
    pub async fn register_module<T: Module>(&mut self, mut module: T) -> ModuleRef<T> {
        let (ch_tx, mut ch_rx): (
            UnboundedSender<SysMessage<T>>,
            UnboundedReceiver<SysMessage<T>>,
        ) = unbounded_channel();

        let (stop_tx, mut stop_rx): (UnboundedSender<()>, UnboundedReceiver<()>) =
            unbounded_channel();

        let mut stop_module_txs = self.stop_module_txs.lock().await;
        stop_module_txs.push(stop_tx.clone());

        let mref = ModuleRef {
            tx: ch_tx.clone(),
            stop_tickers: self.stop_ticker_txs.clone(),
        };

        let mref_clone = mref.clone();
        let feedback_tx = self.stop_module_reply_tx.clone();

        tokio::spawn(async move {
            loop {
                tokio::select! {
                    biased;

                    _ = stop_rx.recv() => {
                        break;
                    }
                    Some(SysMessage::Message(msg)) = ch_rx.recv() => {
                        msg.get_handled(&mref, &mut module).await;
                    }
                }
            }
            let _ = feedback_tx.send(());
        });
        mref_clone
    }

    /// Creates and starts a new instance of the system.
    pub async fn new() -> Self {
        let (feedback_tx, feedback_rx): (UnboundedSender<()>, UnboundedReceiver<()>) =
            unbounded_channel();
        System {
            stop_module_txs: Mutex::new(Vec::new()),
            stop_ticker_txs: Arc::new(Mutex::new(Vec::new())),
            stop_module_reply_rx: feedback_rx,
            stop_module_reply_tx: feedback_tx,
        }
    }

    /// Gracefully shuts the system down.
    pub async fn shutdown(&mut self) {
        let stop_channels = self.stop_module_txs.lock().await;
        let num_modules = stop_channels.len();
        for stop_channel in stop_channels.iter() {
            match stop_channel.send(()) {
                Ok(_) => {}
                Err(_) => {
                    // throw error
                    //
                }
            }
        }
        drop(stop_channels);

        let stop_tickers = self.stop_ticker_txs.lock().await;
        for stop_ticker in stop_tickers.iter() {
            match stop_ticker.send(()) {
                Ok(_) => {}
                Err(_) => {}
            }
        }
        drop(stop_tickers);
        for i in 0..num_modules {
            self.stop_module_reply_rx.recv().await;
        }
    }
}

/// A reference to a module used for sending messages.
// You can add fields to this struct.
pub struct ModuleRef<T: Module + ?Sized> {
    // A marker field required to inform the compiler about variance in T.
    // It can be removed if type T is used in some other field.
    // arc mutex module
    tx: tokio::sync::mpsc::UnboundedSender<SysMessage<T>>,
    stop_tickers: Arc<Mutex<Vec<tokio::sync::mpsc::UnboundedSender<()>>>>,
}

impl<T: Module> ModuleRef<T> {
    /// Sends the message to the module.
    pub async fn send<M: Message>(&self, msg: M)
    where
        T: Handler<M>,
    {
        match self.tx.send(SysMessage::Message(Box::new(msg))) {
            Ok(_) => {}
            Err(_) => {
                // throw error
            }
        }
    }

    /// Schedules a message to be sent to the module periodically with the given interval.
    /// The first tick is sent after the interval elapses.
    /// Every call to this function results in sending new ticks and does not cancel
    /// ticks resulting from previous calls.
    pub async fn request_tick<M>(&self, message: M, delay: Duration) -> TimerHandle
    where
        M: Message + Clone,
        T: Handler<M>,
    {
        // let (stop_tx, mut stop_rx) = unbounded_channel();
        let (stop_tx, mut stop_rx): (UnboundedSender<()>, UnboundedReceiver<()>) =
            unbounded_channel();

        let module_ref = self.clone();
        let del = delay;
        let msg = message.clone();

        let mut stop_tickers = self.stop_tickers.lock().await;
        stop_tickers.push(stop_tx.clone());

        //assert system is not shutdown
        tokio::spawn(async move {
            let mut interval = tokio::time::interval(del);
            interval.tick().await;
            loop {
                tokio::select! {
                    biased;
                    _ = stop_rx.recv() => {
                        break;
                    }
                    _ = interval.tick() => {
                        module_ref.send(msg.clone()).await;
                    }

                }
            }
        });

        TimerHandle {
            stop_tx: stop_tx.clone(),
        }
    }
}

impl<T: Module> Clone for ModuleRef<T> {
    /// Creates a new reference to the same module.
    fn clone(&self) -> Self {
        ModuleRef {
            tx: self.tx.clone(),
            stop_tickers: self.stop_tickers.clone(),
        }
    }
}
