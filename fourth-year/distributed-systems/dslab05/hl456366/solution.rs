use hmac::{Hmac, Mac};
use rustls::pki_types::pem::PemObject;
use rustls::pki_types::ServerName;
use rustls::{ClientConnection, RootCertStore, ServerConnection, StreamOwned};
use sha2::Sha256;
use std::io::{Read, Write};
use std::sync::Arc;

// Create a type alias:
type HmacSha256 = Hmac<Sha256>;

fn calculate_hmac_tag(message: Vec<u8>, secret_key: Vec<u8>) -> [u8; 32] {
    // Initialize a new MAC instance from the secret key:
    let mut mac = HmacSha256::new_from_slice(secret_key.as_slice()).unwrap();

    // Calculate MAC for the data (one can provide it in multiple portions):
    mac.update(message.as_slice());

    // Finalize the computations of MAC and obtain the resulting tag:
    let tag = mac.finalize().into_bytes();

    tag.into()
}

fn verify_hmac_tag(tag: &[u8], message: &[u8], secret_key: Vec<u8>) -> bool {
    // Initialize a new MAC instance from the secret key:
    let mut mac = HmacSha256::new_from_slice(secret_key.as_slice()).unwrap();

    // Calculate MAC for the data (one can provide it in multiple portions):
    mac.update(message);

    // Verify the tag:
    mac.verify_slice(tag).is_ok()
}

pub struct SecureClient<L: Read + Write> {
    stream: StreamOwned<ClientConnection, L>,
    hmac_key: Vec<u8>,
}

pub struct SecureServer<L: Read + Write> {
    stream: StreamOwned<ServerConnection, L>,
    hmac_key: Vec<u8>,
}

impl<L: Read + Write> SecureClient<L> {
    /// Creates a new instance of SecureClient.
    ///
    /// SecureClient communicates with SecureServer via `link`.
    /// The messages include a HMAC tag calculated using `hmac_key`.
    /// A certificate of SecureServer is signed by `root_cert`.
    /// We are connecting with `server_hostname`.
    pub fn new(
        link: L,
        hmac_key: &[u8],
        root_cert: &str,
        server_hostname: ServerName<'static>,
    ) -> Self {
        SecureClient {
            stream: Self::client_stream(link, root_cert, server_hostname),
            hmac_key: hmac_key.to_vec(),
        }
    }

    /// Sends the data to the server. The sent message follows the
    /// format specified in the description of the assignment.
    pub fn send_msg(&mut self, data: Vec<u8>) {
        let tag = calculate_hmac_tag(data.clone(), self.hmac_key.clone());
        let mut msg = vec![];
        msg.extend((data.len() as u32).to_be_bytes());
        msg.extend(data);
        msg.extend(tag);
        self.stream.write_all(&msg).unwrap();
        self.stream.flush().unwrap();
    }

    // Wrap `TcpStream` of a client in TLS. Writing to/reading from the new stream
    // will automatically apply TLS to the outgoing/incoming data:
    fn client_stream(
        stream: L,
        root_cert: &str,
        server_hostname: ServerName<'static>,
    ) -> StreamOwned<ClientConnection, L> {
        // Create an empty store for root certificates:
        let mut root_store = RootCertStore::empty();

        // Add to the store the root certificate of the server:
        root_store.add_parsable_certificates(rustls::pki_types::CertificateDer::from_pem_slice(
            root_cert.as_bytes(),
        ));

        // Create a TLS configuration for the client:
        let client_config = rustls::ClientConfig::builder()
            .with_root_certificates(root_store)
            .with_no_client_auth();

        // Create a TLS connection using the configuration prepared above.
        // "localhost" is the name of the remote server:
        let connection = ClientConnection::new(Arc::new(client_config), server_hostname).unwrap();

        // Wrap the TCP stream in TLS:
        StreamOwned::new(connection, stream)
    }
}

impl<L: Read + Write> SecureServer<L> {
    /// Creates a new instance of SecureServer.
    ///
    /// SecureServer receives messages from SecureClients via `link`.
    /// HMAC tags of the messages are verified against `hmac_key`.
    /// The private key of the SecureServer's certificate is `server_private_key`,
    /// and the full certificate chain is `server_full_chain`.
    pub fn new(
        link: L,
        hmac_key: &[u8],
        server_private_key: &str,
        server_full_chain: &str,
    ) -> Self {
        SecureServer {
            stream: Self::server_stream(link, server_private_key, server_full_chain),
            hmac_key: hmac_key.to_vec(),
        }
    }

    /// Receives the next incoming message and returns the message's content
    /// (i.e., without the message size and without the HMAC tag) if the
    /// message's HMAC tag is correct. Otherwise, returns `SecureServerError`.
    pub fn recv_message(&mut self) -> Result<Vec<u8>, SecureServerError> {
        let mut size_buf = [0u8; 4];
        self.stream.read_exact(&mut size_buf)?;
        let msg_size = u32::from_be_bytes(size_buf) as usize;

        let mut msg_with_tag = vec![0u8; msg_size + 32];
        self.stream.read_exact(&mut msg_with_tag)?;

        let (msg, received_hmac_tag) = msg_with_tag.split_at(msg_size);

        if verify_hmac_tag(received_hmac_tag, msg, self.hmac_key.clone()) {
            Ok(msg.to_vec())
        } else {
            Err(SecureServerError::InvalidHmac)
        }
    }
    // Wrap `TcpStream` of a server in TLS. Writing to/reading from the new stream
    // will automatically apply TLS to the outgoing/incoming data:
    fn server_stream(
        stream: L,
        server_private_key: &str,
        server_full_chain: &str,
    ) -> StreamOwned<ServerConnection, L> {
        // Load the certificate chain for the server:
        let certs = rustls::pki_types::CertificateDer::pem_slice_iter(server_full_chain.as_bytes())
            .flatten()
            .collect();

        // Load the private key for the server (for simplicity, we assume there is
        // provided one valid key, and it is an RSA private key):
        let private_key =
            rustls::pki_types::PrivateKeyDer::from_pem_slice(server_private_key.as_bytes())
                .unwrap();

        // Create a TLS configuration for the server:
        let server_config = rustls::ServerConfig::builder()
            .with_no_client_auth()
            .with_single_cert(certs, private_key)
            .unwrap();

        // Create a TLS connection using the configuration prepared above:
        let connection = ServerConnection::new(Arc::new(server_config)).unwrap();

        // Wrap the TCP strem in TLS:
        StreamOwned::new(connection, stream)
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum SecureServerError {
    /// The HMAC tag of a message is invalid.
    InvalidHmac,
}

impl From<std::io::Error> for SecureServerError {
    fn from(_: std::io::Error) -> Self {
        //we're not allowed to add error types, are we?
        SecureServerError::InvalidHmac
    }
}

// You can add any private types, structs, consts, functions, methods, etc., you need.
