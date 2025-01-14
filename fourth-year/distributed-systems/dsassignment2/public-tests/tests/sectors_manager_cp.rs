use assignment_2_solution::{build_sectors_manager, SectorVec};
use ntest::timeout;
use std::path::PathBuf;
use tempfile::tempdir;

#[tokio::test]
#[timeout(200)]
async fn drive_can_store_data() {
    let _ = env_logger::builder().is_test(true).try_init();

    // given
    let root_drive_dir = tempdir().unwrap();
    let mut path = PathBuf::new();
    path.push(root_drive_dir.path());
    let sectors_manager = build_sectors_manager(path).await;

    // when
    sectors_manager
        .write(0, &(SectorVec(vec![2; 4096]), 1, 1))
        .await;
    let data = sectors_manager.read_data(0).await;

    // then
    assert_eq!(sectors_manager.read_metadata(0).await, (1, 1));
    assert_eq!(data.0.len(), 4096);
    assert_eq!(data.0, vec![2; 4096]);

    sectors_manager
        .write(0, &(SectorVec(vec![33; 4096]), 2, 3))
        .await;

    assert_eq!(sectors_manager.read_metadata(0).await, (2, 3));

    let data = sectors_manager.read_data(0).await;
    assert_eq!(data.0, vec![33; 4096]);

    assert_eq!(sectors_manager.read_metadata(1).await, (0, 0));
    assert_eq!(sectors_manager.read_metadata(100).await, (0, 0));
}
