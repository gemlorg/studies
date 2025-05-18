#[cfg(test)]
mod tests {
    use crate::solution::{DetectorOperation, FailureDetectorModule};
    use crate::unwrap_alive_info;
    use module_system::System;
    use ntest::timeout;
    use std::net::SocketAddr;
    use std::time::Duration;
    use tokio::net::UdpSocket;
    use uuid::Uuid;

    #[tokio::test]
    #[timeout(300)]
    async fn data_on_wire_should_parse_with_bincode_for_single_node() {
        let mut system = System::new().await;

        let delay = Duration::from_millis(20);
        let (ident, addr): (Uuid, SocketAddr) =
            (Uuid::new_v4(), "127.0.0.1:17844".parse().unwrap());
        let addresses = [(ident, addr)].iter().cloned().collect();

        let send_addr: SocketAddr = "127.0.0.1:0".parse().unwrap();
        let sock = UdpSocket::bind(send_addr).await.unwrap();
        let _detector = FailureDetectorModule::new(&mut system, delay, &addresses, ident).await;
        assert_eq!(
            sock.send_to(
                bincode::serialize(&DetectorOperation::AliveRequest)
                    .unwrap()
                    .as_slice(),
                addr,
            )
            .await
            .unwrap(),
            4
        );

        let mut buf = [0; 256];
        let len = sock.recv(&mut buf).await.unwrap();
        let alive_info = unwrap_alive_info(bincode::deserialize(&buf[..len]).unwrap());

        assert_eq!(alive_info.len(), 1);
        assert_eq!(alive_info.iter().next().unwrap(), &ident);

        system.shutdown().await;
    }
}
