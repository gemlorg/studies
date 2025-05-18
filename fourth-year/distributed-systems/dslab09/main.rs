mod public_test;
mod solution;

use crate::solution::{
    DistributedStore, Node, Product, ProductPrice, ProductPriceQuery, ProductType, Transaction,
    TransactionMessage, TwoPhaseResult,
};
use module_system::{ModuleRef, System};
use std::time::Duration;
use tokio::sync::oneshot::channel;
use uuid::Uuid;

async fn send_query(node: &ModuleRef<Node>, product_ident: Uuid) -> ProductPrice {
    let (result_sender, result_receiver) = channel::<ProductPrice>();
    node.send(ProductPriceQuery {
        product_ident,
        result_sender,
    })
    .await;
    result_receiver.await.unwrap()
}

#[tokio::main]
async fn main() {
    // Initialize the system and the store:
    let mut system = System::new().await;
    let (transaction_done_tx, transaction_done_rx) = channel();
    let laptop_id = Uuid::new_v4();
    let initial_laptop_price = 150000;
    let products = vec![
        Product {
            identifier: laptop_id,
            pr_type: ProductType::Electronics,
            price: initial_laptop_price,
        },
        Product {
            identifier: Uuid::new_v4(),
            pr_type: ProductType::Books,
            price: 5000,
        },
        Product {
            identifier: Uuid::new_v4(),
            pr_type: ProductType::Toys,
            price: 1000,
        },
    ];
    let node = system.register_module(Node::new(products)).await;
    let distributed_store = system
        .register_module(DistributedStore::new(vec![node.clone()]))
        .await;

    // Increase prices of electronics:
    let electronics_price_shift = 100;
    distributed_store
        .send(TransactionMessage {
            transaction: Transaction {
                pr_type: ProductType::Electronics,
                shift: electronics_price_shift,
            },
            completed_callback: Box::new(|result| {
                Box::pin(async move {
                    transaction_done_tx.send(result.clone()).unwrap();
                })
            }),
        })
        .await;
    let r = assert_eq!(Ok(TwoPhaseResult::Ok), transaction_done_rx.await);

    // Check the new price of the latop:
    tokio::time::sleep(Duration::from_millis(100)).await;
    for i in 0..10 {
        assert_eq!(
            initial_laptop_price + (electronics_price_shift as u64),
            send_query(&node, laptop_id).await.0.unwrap()
        );
    }
    let (transaction_done_tx, transaction_done_rx) = channel();
    distributed_store
        .send(TransactionMessage {
            transaction: Transaction {
                pr_type: ProductType::Electronics,
                shift: electronics_price_shift,
            },
            completed_callback: Box::new(|result| {
                Box::pin(async move {
                    transaction_done_tx.send(result).unwrap();
                })
            }),
        })
        .await;
    let r = assert_eq!(Ok(TwoPhaseResult::Ok), transaction_done_rx.await);
    for i in 0..10 {
        assert_eq!(
            initial_laptop_price + (2 * electronics_price_shift as u64),
            send_query(&node, laptop_id).await.0.unwrap()
        );
    }

    println!("System can execute a simple transaction!");
    system.shutdown().await;
}
