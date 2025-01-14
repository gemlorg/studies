use assignment_2_solution::{
    run_register_process, serialize_register_command, ClientCommandHeader, ClientRegisterCommand,
    ClientRegisterCommandContent, Configuration, PublicConfiguration, RegisterCommand, SectorVec,
    MAGIC_NUMBER,
};
use hmac::{Hmac, Mac};
use ntest::timeout;
use sha2::Sha256;
use std::convert::TryInto;
use std::time::Duration;
use tempfile::tempdir;
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::net::TcpStream;

static HMAC_TAG_SIZE: usize = 32;

// #[tokio::test]
// #[timeout(400000)]
async fn simple_system() {
    let _ = env_logger::builder().is_test(true).try_init();

    let storage_dir = tempdir().unwrap();
    let hmac_client_key = [0x61; 32]; // 0x61 == 'a'
                                      // let hmac_client_key = [0x5; 32];

    let tcp_port = 10_000;

    let config = Configuration {
        public: PublicConfiguration {
            tcp_locations: vec![("127.0.0.1".to_string(), tcp_port)],
            self_rank: 1,
            n_sectors: 20,
            storage_dir: storage_dir.into_path(),
        },
        hmac_system_key: [1; 64],
        hmac_client_key,
    };

    println!("before spawn");
    let handle = tokio::spawn(run_register_process(config));
    println!("after spawn");
    tokio::join!(handle);
}

#[tokio::test]
#[timeout(4000)]
async fn garbage_sliding_window_test() {
    let _ = env_logger::builder().is_test(true).try_init();

    // given
    let hmac_client_key = [5; 32];
    let tcp_port = 30_287;
    let storage_dir = tempdir().unwrap();
    let request_identifier = 1778;

    let config = Configuration {
        public: PublicConfiguration {
            tcp_locations: vec![("127.0.0.1".to_string(), tcp_port)],
            self_rank: 1,
            n_sectors: 20,
            storage_dir: storage_dir.into_path(),
        },
        hmac_system_key: [1; 64],
        hmac_client_key,
    };

    tokio::spawn(run_register_process(config));

    tokio::time::sleep(Duration::from_millis(300)).await;
    let mut stream = TcpStream::connect(("127.0.0.1", tcp_port))
        .await
        .expect("Could not connect to TCP port");
    let write_cmd = RegisterCommand::Client(ClientRegisterCommand {
        header: ClientCommandHeader {
            request_identifier,
            sector_idx: 12,
        },
        content: ClientRegisterCommandContent::Write {
            data: SectorVec(vec![3; 4096]),
        },
    });

    // when
    send_cmd_garbage(&write_cmd, &mut stream, &hmac_client_key).await;

    // then
    const EXPECTED_RESPONSES_SIZE: usize = 48;
    let mut buf = [0_u8; EXPECTED_RESPONSES_SIZE];
    stream
        .read_exact(&mut buf)
        .await
        .expect("Less data then expected");

    // asserts for write response
    assert_eq!(&buf[0..4], MAGIC_NUMBER.as_ref());
    assert_eq!(buf[7], 0x42);
    assert_eq!(
        u64::from_be_bytes(buf[8..16].try_into().unwrap()),
        request_identifier
    );
    assert!(hmac_tag_is_ok(&hmac_client_key, &buf));
}

#[tokio::test]
#[timeout(4000)]
async fn single_process_system_completes_operations() {
    // given
    let _ = env_logger::builder().is_test(true).try_init();

    let hmac_client_key = [5; 32];
    let tcp_port = 20_287;
    let storage_dir = tempdir().unwrap();
    let request_identifier = 1778;

    let config = Configuration {
        public: PublicConfiguration {
            tcp_locations: vec![("127.0.0.1".to_string(), tcp_port)],
            self_rank: 1,
            n_sectors: 20,
            storage_dir: storage_dir.into_path(),
        },
        hmac_system_key: [1; 64],
        hmac_client_key,
    };

    tokio::spawn(run_register_process(config));

    tokio::time::sleep(Duration::from_millis(300)).await;
    let mut stream = TcpStream::connect(("127.0.0.1", tcp_port))
        .await
        .expect("Could not connect to TCP port");
    let write_cmd = RegisterCommand::Client(ClientRegisterCommand {
        header: ClientCommandHeader {
            request_identifier,
            sector_idx: 12,
        },
        content: ClientRegisterCommandContent::Write {
            data: SectorVec(vec![3; 4096]),
        },
    });

    // when
    send_cmd(&write_cmd, &mut stream, &hmac_client_key).await;

    // then
    const EXPECTED_RESPONSES_SIZE: usize = 48;
    let mut buf = [0_u8; EXPECTED_RESPONSES_SIZE];
    stream
        .read_exact(&mut buf)
        .await
        .expect("Less data then expected");

    // asserts for write response
    assert_eq!(&buf[0..4], MAGIC_NUMBER.as_ref());
    assert_eq!(buf[7], 0x42);
    assert_eq!(
        u64::from_be_bytes(buf[8..16].try_into().unwrap()),
        request_identifier
    );
    assert!(hmac_tag_is_ok(&hmac_client_key, &buf));
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

async fn send_cmd_garbage(
    register_cmd: &RegisterCommand,
    stream: &mut TcpStream,
    hmac_client_key: &[u8],
) {
    let garbage = [33_u8; 0x40];
    let mut data = Vec::new();
    serialize_register_command(register_cmd, &mut data, &hmac_client_key)
        .await
        .unwrap();

    stream.write_all(&garbage).await.unwrap();
    stream.write_all(&data).await.unwrap();
}
