use assignment_2_solution::{
    run_register_process, serialize_register_command, Broadcast, ClientCommandHeader,
    ClientRegisterCommand, ClientRegisterCommandContent, Configuration, PublicConfiguration,
    RegisterClient, RegisterCommand, SectorVec, Send, SystemRegisterCommand,
};
use hmac::{Hmac, Mac};
use ntest::timeout;
use sha2::Sha256;
use std::collections::HashMap;
use std::convert::TryInto;
use std::sync::{Arc, Mutex};
use std::time::Duration;
use tempfile::tempdir;
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::net::TcpStream;
use tokio::task::JoinHandle;

#[tokio::test]
#[timeout(4000)]
async fn multiple_nodes() {
    let _ = env_logger::builder().is_test(true).try_init();

    let range = 1..3; // 2 nodes

    let mut addrs = Vec::<(String, u16)>::new();
    let mut tcp_port = 10_000;
    let hmac_client_key = [0x61; 32]; // =0x61 == 'a'

    for _ in range.clone() {
        addrs.push(("127.0.0.1".to_string(), tcp_port));
        tcp_port += 1;
    }

    let mut handles = vec![];

    for i in range {
        assert!(i > 0);
        let config = Configuration {
            public: PublicConfiguration {
                tcp_locations: addrs.clone(),
                self_rank: i,
                n_sectors: 20,
                storage_dir: tempdir().unwrap().into_path(),
            },
            hmac_system_key: [1; 64],
            hmac_client_key,
        };
        println!("spawn");
        handles.push(tokio::spawn(run_register_process(config)));
    }

    tokio::time::sleep(Duration::from_millis(300)).await;

    let request_identifier = 1778;

    let data1 = vec![0x66_u8; 4096];
    let data2 = vec![0x6e_u8; 4096];

    let read_cmd0 = RegisterCommand::Client(ClientRegisterCommand {
        header: ClientCommandHeader {
            request_identifier: request_identifier - 1,
            sector_idx: 12,
        },
        content: ClientRegisterCommandContent::Read {},
    });

    let write_cmd1 = RegisterCommand::Client(ClientRegisterCommand {
        header: ClientCommandHeader {
            request_identifier,
            sector_idx: 12,
        },
        content: ClientRegisterCommandContent::Write {
            data: SectorVec(data1.clone()),
        },
    });

    let read_cmd1 = RegisterCommand::Client(ClientRegisterCommand {
        header: ClientCommandHeader {
            request_identifier: request_identifier + 1,
            sector_idx: 12,
        },
        content: ClientRegisterCommandContent::Read {},
    });

    let write_cmd2 = RegisterCommand::Client(ClientRegisterCommand {
        header: ClientCommandHeader {
            request_identifier: request_identifier + 2,
            sector_idx: 12,
        },
        content: ClientRegisterCommandContent::Write {
            data: SectorVec(data2.clone()),
        },
    });

    let read_cmd2 = RegisterCommand::Client(ClientRegisterCommand {
        header: ClientCommandHeader {
            request_identifier: request_identifier + 3,
            sector_idx: 12,
        },
        content: ClientRegisterCommandContent::Read {},
    });

    const EXPECTED_WRITE_RESPONSES_SIZE: usize = 48;
    const EXPECTED_READ_RESPONSES_SIZE: usize = 48 + 4096;

    let mut write_response_buf = [0_u8; EXPECTED_WRITE_RESPONSES_SIZE];
    let mut read_response_buf = [0_u8; EXPECTED_READ_RESPONSES_SIZE];

    let compare = |buf: Vec<u8>, data| {
        assert_eq!(&data, &buf[16..(16 + 4096)]);
        // println!("{:?}", data);
    };

    // why not try with rank=2?
    let mut stream = TcpStream::connect(addrs[1].clone())
        .await
        .expect("Could not connect to TCP port");

    send_cmd(&read_cmd0, &mut stream, &hmac_client_key).await;

    stream
        .read_exact(&mut read_response_buf)
        .await
        .expect("Less data then expected");

    compare(read_response_buf.to_vec(), vec![0; 4096]);

    send_cmd(&write_cmd1, &mut stream, &hmac_client_key).await;

    stream
        .read_exact(&mut write_response_buf)
        .await
        .expect("Less data then expected");

    send_cmd(&read_cmd1, &mut stream, &hmac_client_key).await;

    stream
        .read_exact(&mut read_response_buf)
        .await
        .expect("Less data then expected");

    compare(read_response_buf.to_vec(), data1);

    send_cmd(&write_cmd2, &mut stream, &hmac_client_key).await;

    stream
        .read_exact(&mut write_response_buf)
        .await
        .expect("Less data then expected");

    send_cmd(&read_cmd2, &mut stream, &hmac_client_key).await;

    stream
        .read_exact(&mut read_response_buf)
        .await
        .expect("Less data then expected");

    compare(read_response_buf.to_vec(), data2);
}

type HmacSha256 = Hmac<Sha256>;

async fn send_cmd(register_cmd: &RegisterCommand, stream: &mut TcpStream, hmac_client_key: &[u8]) {
    let mut data = Vec::new();
    serialize_register_command(register_cmd, &mut data, &hmac_client_key)
        .await
        .unwrap();

    stream.write_all(&data).await.unwrap();
}

fn hmac_tag_is_ok(key: &[u8], data: &[u8]) -> bool {
    let boundary = data.len() - 32; // - HMAC
    let mut mac = HmacSha256::new_from_slice(key).unwrap();
    mac.update(&data[..boundary]);
    mac.verify_slice(&data[boundary..]).is_ok()
}
