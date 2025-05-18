mod public_test;
mod solution;

use module_system::{ModuleRef, System};
use rand::{rngs::StdRng, RngCore, SeedableRng};
use solution::{
    Node, PeerSamplingService, QueryInstallMsg, QueryResultPollCallback, QueryResultPollMsg,
    RandomnessSource, SyncTriggerMsg,
};
use std::{
    ops::{Deref, DerefMut},
    sync::{Arc, Mutex},
    time::{Duration, SystemTime},
};
use tokio::sync::mpsc::{unbounded_channel, UnboundedSender};
use uuid::Uuid;

#[tokio::main]
async fn main() {
    let (num_nodes, num_instances, divisor) = parse_command_line_args();
    let mut actual_val = 0_u64;

    // Set the system up.
    let mut system = System::new().await;
    let mut node_uids = Vec::with_capacity(num_nodes);
    let node_refs = Arc::new(Mutex::new(Vec::with_capacity(num_nodes)));
    for _i in 0..num_nodes {
        let uuid = Uuid::new_v4();
        node_uids.push(uuid);
        if uuid.as_u128() % divisor == 0 {
            actual_val += 1;
        }
        let rng_seed = uuid.as_u64_pair().0 ^ uuid.as_u64_pair().1;
        let pss_rs = Box::new(RandomnessSourceImpl {
            rand: StdRng::seed_from_u64(rng_seed),
        });
        let pss = Box::new(PeerSamplingServiceImpl::new(node_refs.clone(), pss_rs));
        let module_rs = Box::new(RandomnessSourceImpl {
            rand: StdRng::seed_from_u64(rng_seed),
        });
        let module_ref = Node::new(&mut system, uuid, module_rs, pss).await;
        let mut guard = node_refs.lock().unwrap();
        guard.deref_mut().push(module_ref);
    }

    // Send a query to one of the nodes.
    let initiator_uuid = install_query(&node_refs, &node_uids, divisor, num_instances).await;

    // Execute a couple of gossiping rounds.
    let (sender, mut receiver) = unbounded_channel();
    let start_time = SystemTime::now();
    for _i in 0..100 {
        poll_query(&node_refs, &initiator_uuid, sender.clone()).await;
        trigger_sync(&node_refs).await;
        tokio::time::sleep(Duration::from_millis(10)).await;
        print_progress(&start_time, actual_val, receiver.recv().await.unwrap());
    }

    // Shutdown everything.
    system.shutdown().await;
}

async fn install_query(
    node_refs: &Arc<Mutex<Vec<ModuleRef<Node>>>>,
    node_uids: &[Uuid],
    divisor: u128,
    num_instances: usize,
) -> Uuid {
    let node_idx = 0_usize;
    let predicate = move |u: &Uuid| u.as_u128() % divisor == 0;
    let msg = QueryInstallMsg {
        bits_per_instance: 32,
        num_instances,
        predicate: Arc::new(predicate),
    };
    let guard = node_refs.lock().unwrap();
    guard.deref().get(node_idx).unwrap().send(msg).await;
    *node_uids.get(node_idx).unwrap()
}

async fn trigger_sync(node_refs: &Arc<Mutex<Vec<ModuleRef<Node>>>>) {
    let guard = node_refs.lock().unwrap();
    for node_ref in guard.deref() {
        node_ref.send(SyncTriggerMsg {}).await;
    }
}

async fn poll_query(
    node_refs: &Arc<Mutex<Vec<ModuleRef<Node>>>>,
    query_initiator_uuid: &Uuid,
    reply_sender: UnboundedSender<Option<u64>>,
) {
    let callback: QueryResultPollCallback = Box::new(|val: Option<u64>| {
        Box::pin(async move {
            reply_sender.send(val).unwrap();
        })
    });
    let guard = node_refs.lock().unwrap();
    let msg = QueryResultPollMsg {
        initiator: *query_initiator_uuid,
        callback,
    };
    guard
        .deref()
        .get(guard.deref().len() - 1)
        .unwrap()
        .send(msg)
        .await;
}

fn print_progress(start_time: &SystemTime, actual_val: u64, estimated_val_opt: Option<u64>) {
    match start_time.elapsed() {
        Ok(dur) => print!("{}", dur.as_millis()),
        Err(_) => print!("?"),
    };
    print!(
        " | # nodes satisfying query: actual={} vs estimated=",
        actual_val
    );
    match estimated_val_opt {
        None => println!("?"),
        Some(val) => println!("{}", val),
    };
}

struct RandomnessSourceImpl {
    rand: StdRng,
}

impl RandomnessSource for RandomnessSourceImpl {
    fn next_u32(&mut self) -> u32 {
        self.rand.next_u32()
    }
}

struct PeerSamplingServiceImpl {
    nodes: Arc<Mutex<Vec<ModuleRef<Node>>>>,
    rs: Box<dyn RandomnessSource + Send>,
}

impl PeerSamplingServiceImpl {
    pub(crate) fn new(
        nodes: Arc<Mutex<Vec<ModuleRef<Node>>>>,
        rs: Box<dyn RandomnessSource + Send>,
    ) -> Self {
        Self { nodes, rs }
    }
}

impl PeerSamplingService for PeerSamplingServiceImpl {
    fn get_random_peer(&mut self) -> ModuleRef<Node> {
        let rand = self.rs.as_mut().next_u32();
        // choose a truly random
        // let rand = rand::random::<u32>();
        let guard = self.nodes.lock().unwrap();
        let peers = guard.deref();
        peers.get((rand as usize) % peers.len()).unwrap().clone()
    }
}

fn parse_command_line_args() -> (usize, usize, u128) {
    let args: Vec<String> = std::env::args().collect();
    match args.len() {
        1 => {
            println!("INFO: Assuming default parameter values.");
            (100, 16, 2)
        }
        4 => {
            let num_processes = match args.get(1).unwrap().parse::<usize>() {
                Ok(val) => {
                    if val == 0 || val > 1000 {
                        println!("ERROR: The number of processes should be in [1..1000]!");
                        print_usage(args.get(0).unwrap());
                        std::process::exit(1);
                    }
                    val
                }
                Err(_) => {
                    println!("ERROR: Unable to parse the number of processes!");
                    print_usage(args.get(0).unwrap());
                    std::process::exit(1);
                }
            };
            let num_instances = match args.get(2).unwrap().parse::<usize>() {
                Ok(val) => {
                    if val == 0 || val > 100 {
                        println!("ERROR: The number of instances should be in [1..100]!");
                        print_usage(args.get(0).unwrap());
                        std::process::exit(1);
                    }
                    val
                }
                Err(_) => {
                    println!("ERROR: Unable to parse the number of instances!");
                    print_usage(args.get(0).unwrap());
                    std::process::exit(1);
                }
            };
            let divisor = match args.get(3).unwrap().parse::<u128>() {
                Ok(val) => {
                    if val == 0 || val > num_processes as u128 {
                        println!("ERROR: The divisor should be in [1..{}]!", num_processes);
                        print_usage(args.get(0).unwrap());
                        std::process::exit(1);
                    }
                    val
                }
                Err(_) => {
                    println!("ERROR: Unable to parse the divisor!");
                    print_usage(args.get(0).unwrap());
                    std::process::exit(1);
                }
            };
            (num_processes, num_instances, divisor)
        }
        _ => {
            println!("ERROR: Three command-line arguments are accepted!");
            print_usage(args.get(0).unwrap());
            std::process::exit(1);
        }
    }
}

fn print_usage(prog_name: &String) {
    println!("The program:");
    println!("    demonstrates eventually-consistent gossip-based aggregation with probabilistic counting sketches.");
    println!("Usage:");
    println!(
        "    {} <num_processes> <num_instances> <divisor>",
        prog_name
    );
    println!("Where:");
    println!("    <num_processes> is the number of processes performing the query computation.");
    println!("    <num_instances> is the number of probabilistic counting sketch instances utilized in the computation.");
    println!(
        "    <divisor> is X such that roughly 1/X of the processes satisfy the query predicate."
    );
}
