use rand::prelude::*;

use aes::cipher::block_padding::Pkcs7;
use aes::cipher::{BlockDecryptMut, BlockEncryptMut, KeyIvInit};
use aes::Aes128;
use cbc::{Decryptor, Encryptor};

// Create type aliases:
type Aes128CbcEnc = Encryptor<Aes128>;
type Aes128CbcDec = Decryptor<Aes128>;

fn encrypt(message: &str, key: &[u8], iv: &[u8]) -> Vec<u8> {
    // Create a new block mode instance from the key and the IV:
    let cipher = Aes128CbcEnc::new_from_slices(key, iv).unwrap();

    // Encrypt the message:
    cipher.encrypt_padded_vec_mut::<Pkcs7>(message.as_bytes())
}

fn decrypt(encrypted_message: &[u8], key: &[u8], iv: &[u8]) -> String {
    // Create a new block mode instance from the key and the IV:
    let cipher = Aes128CbcDec::new_from_slices(key, iv).unwrap();

    // Decrypt the message:
    let decrypted = cipher
        .decrypt_padded_vec_mut::<Pkcs7>(encrypted_message)
        .unwrap();

    // Convert the decrypted message to String:
    String::from_utf8(decrypted).unwrap()
}

fn main() {
    let message = "AES is fast for large amounts of data";

    // Generate random key and initialization vector:
    let key = rand::thread_rng().gen::<[u8; 16]>();
    let iv = rand::thread_rng().gen::<[u8; 16]>();

    // Encrypt the message:
    let encrypted = encrypt(message, &key, &iv);
    println!("Encrypted data: {:?}", encrypted);

    // Decrypt the message:
    let decrypted = decrypt(&encrypted, &key, &iv);
    println!("Decrypted data: '{}'", decrypted);
}
