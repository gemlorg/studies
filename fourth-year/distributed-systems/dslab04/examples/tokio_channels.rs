use tokio::sync::mpsc::{channel, unbounded_channel};

#[tokio::main]
async fn main() {
    let (tx_u, mut rx_u) = unbounded_channel();
    tokio::spawn(async move {
        // Send a message
        // (sending to an unbounded channel is a synchronous function because it never has to wait)
        tx_u.send(7).unwrap();
    });

    let (tx_b, mut rx_b) = channel(10);
    let tx_b_clone = tx_b.clone();
    tokio::spawn(async move {
        // Send a message asynchronously
        // (it waits when the channel is full):
        tx_b_clone.send(8).await.unwrap();
    });
    tokio::spawn(async move {
        // Send a message synchronously
        // (it returns an error when the channel is full):
        tx_b.try_send(9).unwrap();
    });

    // Receive the messages:
    println!("Received: {}", rx_u.recv().await.unwrap());
    println!("Received: {}", rx_b.recv().await.unwrap());
    println!("Received: {}", rx_b.recv().await.unwrap());
}
