pub mod sectors_manager_public {
    use crate::{SectorIdx, SectorVec};
    use std::path::PathBuf;
    use std::sync::Arc;

    #[async_trait::async_trait]
    pub trait SectorsManager: Send + Sync {
        /// Returns 4096 bytes of sector data by index.
        async fn read_data(&self, idx: SectorIdx) -> SectorVec;

        /// Returns timestamp and write rank of the process which has saved this data.
        /// Timestamps and ranks are relevant for atomic register algorithm, and are described
        /// there.
        async fn read_metadata(&self, idx: SectorIdx) -> (u64, u8);

        /// Writes a new data, along with timestamp and write rank to some sector.
        async fn write(&self, idx: SectorIdx, sector: &(SectorVec, u64, u8));
    }

    /// Path parameter points to a directory to which this method has exclusive access.
    /// Can assume that no concurrent access to the same sector will happen.
    pub async fn build_sectors_manager(path: PathBuf) -> Arc<dyn SectorsManager> {
        Arc::new(super::sectors_manager_impl::SectorsManagerImpl::new(path).await)
    }
}

// SectorsManager

// SectorsManager facilitates storing sectors data in the filesystem directory. Sector data shall be stored together with necessary basic information, such as the logical timestamp and the write rank (see the pseudocode of the atomic register algorithm).
// Sectors are numbered from 0 inclusive to Configuration.public.n_sectors (solution/src/domain.rs) exclusive. You can assume that Configuration.public.n_sectors will not exceed 2^21.
// If a sector was never written, we assume that both the logical timestamp and the write rank are 0, and that it contains 4096 zero bytes.
// No particular storage scheme is required, it must just provide atomic operations. No caching is necessary.
// The build_sectors_manager() function (solution/src/lib.rs) shall create an instance of SectorManager for, among others, unit testing. You can assume that the unit tests will not perform concurrent operations on the same sector, even though the trait is marked as Sync.
// SectorsManager is a specialized persistent storage for sector data and metadata. Such specialization allows for optimizing disk usage (see also the Technical Requirements section), and possibly performance.
// A directory for SectorsManager is provided in Configuration.public.storage_dir (solution/src/domain.rs).

mod sectors_manager_impl {

    use serde::{Deserialize, Serialize};
    use tokio::io::AsyncWriteExt;

    use super::sectors_manager_public::SectorsManager;
    use crate::{SectorIdx, SectorVec};
    use std::sync::Arc;
    use std::{collections::HashMap, path::PathBuf};
    use tokio::sync::RwLock;
    pub(crate) struct SectorsManagerImpl {
        path: PathBuf,
        map: Arc<RwLock<HashMap<SectorIdx, MetaData>>>,
    }
    impl SectorsManagerImpl {
        pub(crate) async fn new(path: PathBuf) -> Self {
            Self {
                path: path.clone(),
                map: restore_map(path).await,
            }
        }
        fn extend_path(&self, str: String) -> PathBuf {
            self.path.join(str)
        }
    }

    async fn restore_map(path: PathBuf) -> Arc<RwLock<HashMap<SectorIdx, MetaData>>> {
        let mut map: HashMap<u64, MetaData> = HashMap::new();
        let dir = std::fs::read_dir(path.clone()).unwrap();
        for entry in dir {
            let entry_path = entry.unwrap().path();
            if entry_path.extension().is_some() {
                std::fs::remove_file(entry_path).unwrap();
            } else if entry_path.is_file() {
                if let Some(meta) =
                    MetaData::from_file(entry_path.file_name().unwrap().to_str().unwrap())
                {
                    // can be two metadatas, the older(lower timestamp) will be overwritten
                    match map.get(&meta.id) {
                        Some(old_meta) => {
                            if (*old_meta).timestamp < meta.timestamp {
                                tokio::fs::remove_file(path.clone().join(old_meta.into_string()))
                                    .await
                                    .expect("Error removing old file");
                                map.insert(meta.id, meta);
                            } else {
                                tokio::fs::remove_file(entry_path)
                                    .await
                                    .expect("Error removing old file");
                            }
                        }
                        None => {
                            map.insert(meta.id, meta);
                        }
                    };
                }
            }
        }
        Arc::new(RwLock::new(map))
    }

    #[async_trait::async_trait]
    impl SectorsManager for SectorsManagerImpl {
        async fn read_data(&self, idx: SectorIdx) -> SectorVec {
            let map_lock = self.map.read().await;
            let meta = map_lock.get(&idx);
            match meta {
                Some(meta) => {
                    let data = match tokio::fs::read(self.extend_path(meta.into_string())).await {
                        Ok(data) => data,
                        Err(e) => panic!("Error reading data from sector {}: {}", idx, e),
                    };
                    SectorVec(data)
                }
                None => SectorVec(vec![0; 4096]),
            }
        }

        async fn read_metadata(&self, idx: SectorIdx) -> (u64, u8) {
            let map_lock = self.map.read().await;
            let meta = map_lock.get(&idx);
            match meta {
                Some(meta) => (meta.timestamp, meta.write_rank),
                None => (0, 0),
            }
        }

        async fn write(&self, idx: SectorIdx, sector: &(SectorVec, u64, u8)) {
            let (data, timestamp, write_rank) = sector;
            let meta = MetaData::new(idx, *timestamp, *write_rank);
            let path = self.extend_path(meta.into_string());
            let temp_path = path.clone().with_extension("tmp");
            let mut tmp_f = tokio::fs::File::create(temp_path.clone())
                .await
                .expect("Error creating tmp file");
            tmp_f
                .write_all(data.0.as_slice())
                .await
                .expect("Error writing data to tmp file");
            tmp_f.sync_data().await.unwrap();
            tokio::fs::rename(temp_path, path)
                .await
                .expect("Error renaming tmp file");
            if let Some(old_meta) = self.map.write().await.get(&idx) {
                tokio::fs::remove_file(self.extend_path(old_meta.into_string()))
                    .await
                    .expect("Error removing old file");
            }
            self.map.write().await.insert(idx, meta);
        }
    }
    #[derive(Serialize, Deserialize)]
    struct MetaData {
        id: SectorIdx,
        timestamp: u64,
        write_rank: u8,
    }
    impl MetaData {
        fn new(id: SectorIdx, timestamp: u64, write_rank: u8) -> Self {
            Self {
                id,
                timestamp,
                write_rank,
            }
        }
        fn from_file(file_name: &str) -> Option<Self> {
            let elems = file_name.split('_').collect::<Vec<&str>>();
            match elems[..] {
                [id, timestamp, write_rank] => Some(Self {
                    id: id.parse().ok()?,
                    timestamp: timestamp.parse().ok()?,
                    write_rank: write_rank.parse().ok()?,
                }),
                _ => None,
            }
        }
        fn into_string(&self) -> String {
            format!("{}_{}_{}", self.id, self.timestamp, self.write_rank)
        }
    }
}
