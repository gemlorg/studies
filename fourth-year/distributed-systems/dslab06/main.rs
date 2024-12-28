mod public_test;
mod solution;

#[tokio::main]
async fn main() {
    // let root_storage_dir1 = std::env::temp_dir().join("stable_storage_data");
    //
    // tokio::fs::remove_dir_all(root_storage_dir1).await.unwrap();

    let root_storage_dir = std::env::temp_dir().join("stable_storage_data");
    tokio::fs::create_dir(&root_storage_dir).await.unwrap();

    {
        let mut storage = solution::build_stable_storage(root_storage_dir.clone()).await;
        storage.put("key", "value".as_bytes()).await.unwrap();
    } // "crash"

    {
        let storage = solution::build_stable_storage(root_storage_dir.clone()).await;
        let value = String::from_utf8(storage.get("key").await.unwrap()).unwrap();
        println!("Recovered value: '{}'", value);
    }

    tokio::fs::remove_dir_all(root_storage_dir).await.unwrap();
}
