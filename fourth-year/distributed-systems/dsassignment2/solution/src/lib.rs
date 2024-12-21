mod domain;

mod atomic_register;
mod manager;
mod register_client;
mod register_process;
mod transfer;

pub use crate::atomic_register::atomic_register_public::*;
pub use crate::domain::*;
pub use crate::manager::sectors_manager_public::*;
pub use crate::transfer::transfer_public::*;

pub use register_client_public::*;

pub async fn run_register_process(config: Configuration) {
    crate::register_process::register_process_impl::run_register_process_impl(config).await
}

pub mod register_client_public {
    use crate::SystemRegisterCommand;
    use std::sync::Arc;

    #[async_trait::async_trait]
    /// We do not need any public implementation of this trait. It is there for use
    /// in AtomicRegister. In our opinion it is a safe bet to say some structure of
    /// this kind must appear in your solution.
    pub trait RegisterClient: core::marker::Send + core::marker::Sync {
        /// Sends a system message to a single process.
        async fn send(&self, msg: Send);

        /// Broadcasts a system message to all processes in the system, including self.
        async fn broadcast(&self, msg: Broadcast);
    }

    pub struct Broadcast {
        pub cmd: Arc<SystemRegisterCommand>,
    }

    pub struct Send {
        pub cmd: Arc<SystemRegisterCommand>,
        /// Identifier of the target process. Those start at 1.
        pub target: u8,
    }
}
