use rand::rngs::OsRng;
use rsa::{Pkcs1v15Encrypt, RsaPrivateKey, RsaPublicKey};

fn encrypt(message: &str, public_key: &RsaPublicKey) -> Vec<u8> {
    // Encrypt the message using the PKCS1v15 scheme::
    public_key
        .encrypt(&mut OsRng, Pkcs1v15Encrypt, message.as_bytes())
        .unwrap()
}

fn decrypt(encrypted_message: &[u8], private_key: &RsaPrivateKey) -> String {
    // Decrypt the message using the PKCS1v15 scheme::
    let decrypted = private_key.decrypt(Pkcs1v15Encrypt, encrypted_message).unwrap();

    // Convert the decrypted message to String:
    String::from_utf8(decrypted).unwrap()
}

fn main() {
    let message = "Secret message";

    // Generate a new 2048-bit private key (this is currently the smallest key size
    // considered secure, and often 3072 and 4096-bit keys are used):
    let private_key = RsaPrivateKey::new(&mut OsRng, 2048).unwrap();

    // Derive a public key from the private key:
    let public_key = RsaPublicKey::from(&private_key);

    // Encrypt the message (the public key is used):
    let encrypted = encrypt(message, &public_key);
    println!("Encrypted data: {:?}", encrypted);

    // Decrypt the message (the private key is used):
    let decrypted = decrypt(&encrypted, &private_key);
    println!("Decrypted data: '{}'", decrypted);
}
