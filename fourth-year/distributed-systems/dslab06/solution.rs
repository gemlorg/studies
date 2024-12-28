use sha2::{Digest, Sha256};
use std::path::PathBuf;
use std::{path::Path, sync::Arc};
use tokio::fs::File;
use tokio::io::AsyncReadExt;
use tokio::io::AsyncWriteExt;
use tokio::sync::Mutex;
// You can add here other imports from std or crates listed in Cargo.toml.

// You can add any private types, structs, consts, functions, methods, etc., you need.
// As always, you should not modify the public interfaces.

#[async_trait::async_trait]
pub trait StableStorage: Send + Sync {
    /// Stores `value` under `key`.
    ///
    /// Detailed requirements are specified in the description of the assignment.
    async fn put(&mut self, key: &str, value: &[u8]) -> Result<(), String>;

    /// Retrieves value stored under `key`.
    ///
    /// Detailed requirements are specified in the description of the assignment.
    async fn get(&self, key: &str) -> Option<Vec<u8>>;

    /// Removes `key` and the value stored under it.
    ///
    /// Detailed requirements are specified in the description of the assignment.
    async fn remove(&mut self, key: &str) -> bool;
}

struct StableStorageImpl {
    root_dir: PathBuf,
    mutex: Arc<Mutex<()>>,
}

impl StableStorageImpl {
    fn get_tmp_path(&self, key: &str) -> PathBuf {
        let filename = format!("{}.tmp", hash_key(key));
        self.root_dir.join(filename)
    }
    fn get_final_path(&self, key: &str) -> PathBuf {
        self.root_dir.join(hash_key(key))
    }
}
#[async_trait::async_trait]
impl StableStorage for StableStorageImpl {
    /// Stores `value` under `key`.
    ///
    /// Detailed requirements are specified in the description of the assignment.

    //     Write the data with a checksum (e.g., CRC32) to a temporary file dstdir/tmpfile.
    // Call the POSIX fsyncdata function on dstdir/tmpfile to ensure the data is actually transferred to a disk device (in Rust, one can use the tokio::fs::File::sync_data() method).
    // Call fsyncdata on dstdir to transfer the data of the modified directory to the disk device. (Again, in Rust, one can use the tokio::fs::File::sync_data() method. Even though the struct is called File, here it can be used for directories as well, for example: tokio::fs::File::open("dir").await.unwrap().sync_data().await.unwrap()).
    // Write the data (without the checksum) to dstdir/dstfile.
    // Call fsyncdata on dstdir/dstfile.
    // Call fsyncdata on dstdir (only necessary if dstfile did not exist before the previous step).
    // Remove dstdir/tmpfile.
    // Call fsyncdata on dstdir.
    async fn put(&mut self, key: &str, value: &[u8]) -> Result<(), String> {
        if key.len() > 255 || value.len() > 65535 {
            return Err("Invalid key or value length".to_string());
        }
        let mut _lock = self.mutex.lock().await;

        let tmp_file = self.get_tmp_path(key);
        let final_file = self.get_final_path(key);
        let checksum = compute_checksum(value);
        if final_file.exists() {
            return Err("Key already exists".to_string());
        }

        {
            let mut file = File::create(&tmp_file).await.map_err(|e| e.to_string())?;
            file.write_all(&checksum).await.map_err(|e| e.to_string())?; // Write checksum
            file.write_all(value).await.map_err(|e| e.to_string())?; // Write value
            file.sync_data().await.map_err(|e| e.to_string())?; // Ensure data is flushed
        }
        sync_dir_async(&self.root_dir)
            .await
            .map_err(|e| e.to_string())?;
        {
            let mut f = File::create(&final_file).await.map_err(|e| e.to_string())?;
            f.write_all(value).await.map_err(|e| e.to_string())?;
            f.sync_data().await.map_err(|e| e.to_string())?;
        }
        // read data and check the checksum is correct
        {
            let mut file = File::open(&tmp_file).await.map_err(|e| e.to_string())?;
            let mut buffer = Vec::new();
            file.read_to_end(&mut buffer)
                .await
                .map_err(|e| e.to_string())?;
            let checksum = buffer[..32].to_vec();
            let data = buffer[32..].to_vec();
            let computed_checksum = compute_checksum(&data);
            if checksum != computed_checksum {
                return Err("Checksum mismatch".to_string());
            }

            let mut f = File::create(&final_file).await.map_err(|e| e.to_string())?;
            f.write_all(&data).await.map_err(|e| e.to_string())?;
            f.sync_data().await.map_err(|e| e.to_string())?;
        }
        sync_dir_async(&self.root_dir)
            .await
            .map_err(|e| e.to_string())?;
        tokio::fs::remove_file(&tmp_file)
            .await
            .map_err(|e| e.to_string())?;
        sync_dir_async(&self.root_dir)
            .await
            .map_err(|e| e.to_string())?;
        Ok(())
    }

    /// Retrieves value stored under `key`.
    ///
    /// Detailed requirements are specified in the description of the assignment.
    async fn get(&self, key: &str) -> Option<Vec<u8>> {
        let _lock = self.mutex.lock().await; // lock the set

        let file_path = self.get_final_path(key);
        let mut file = File::open(&file_path).await.ok()?;
        let mut buffer = Vec::new();
        file.read_to_end(&mut buffer).await.ok()?;
        return Some(buffer);
    }

    /// Removes `key` and the value stored under it.
    ///
    /// Detailed requirements are specified in the description of the assignment.
    async fn remove(&mut self, key: &str) -> bool {
        let mut _lock = self.mutex.lock().await; // lock the set

        let file_path = self.get_final_path(key);
        if !file_path.exists() {
            return false;
        }
        let remove = tokio::fs::remove_file(&file_path).await.is_ok();
        if !remove {
            return false;
        }
        sync_dir_async(&self.root_dir).await.unwrap();

        true
    }
}

/// Creates a new instance of stable storage.
pub async fn build_stable_storage(root_storage_dir: PathBuf) -> Box<dyn StableStorage> {
    let key_map = Arc::new(Mutex::new(()));
    let storage = StableStorageImpl {
        root_dir: root_storage_dir,
        mutex: key_map,
    };
    Box::new(storage)
}

fn hash_key(key: &str) -> String {
    let mut hasher = Sha256::new();
    hasher.update(key);
    format!("{:x}", hasher.finalize())
}

async fn sync_dir_async(dir: &Path) -> Result<(), String> {
    let dir_file = File::open(dir).await.map_err(|e| e.to_string())?;
    dir_file.sync_data().await.map_err(|e| e.to_string())
}

fn compute_checksum(data: &[u8]) -> Vec<u8> {
    let mut hasher = Sha256::new();
    hasher.update(data);
    hasher.finalize().to_vec()
}
