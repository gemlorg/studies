use bitvec::vec::BitVec;
use bitvec::{prelude::Lsb0, view::BitView};
use log::debug;
use module_system::{Handler, ModuleRef, System};
use std::collections::{HashMap, HashSet};
use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;
use std::time::SystemTime;
use uuid::Uuid;

/// A source of randomness.
pub(crate) trait RandomnessSource {
    /// Generates a next pseudo-random u32 value selected
    /// from a uniform distribution.
    fn next_u32(&mut self) -> u32;
}

/// A conflict-free state-based replicated counter.
pub(crate) trait ConflictFreeReplicatedCounter<T> {
    /// Sets a given counter so that it counts
    /// no elements.
    fn set_to_zero(&mut self);

    /// Sets a given counter so that it counts
    /// an infinite number of elements (all possible).
    fn set_to_infinity(&mut self);

    /// Adds one more element to a given counter
    /// (increments the counter by one) by one using
    /// a given source of randomness.
    /// If the counter counts an infinite number of elements,
    /// an `Err` is returned and the given counter remains
    /// intact; otherwise, `Ok` is returned.
    fn try_count_one_more_element(&mut self, rs: &mut dyn RandomnessSource) -> Result<(), String>;

    /// Merges another counter with a given counter,
    /// so that, as a result, the given counter counts
    /// elements counted originally by both itself
    /// and the other counter. If the two counters are
    /// incompatible, `Err` is returned and the given
    /// counter remains intact; otherwise, `Ok` is returned.
    fn try_merge_with(&mut self, other: &Self) -> Result<(), String>;

    /// Returns the number of elements counted
    /// by a given counter.
    fn evaluate(&self) -> T;
}

/// An implementation of a probabilistic counting sketch.
#[derive(Clone, Debug)]
pub(crate) struct ProbabilisticCounter {
    // TODO: you may add any necessary fields here
    bits_per_instance: usize,
    num_instances: usize,
    // a bit vector for each sketch instance
    data: BitVec<u8>,
}

impl ProbabilisticCounter {
    /// The scaling factor used in probabilistic counting.
    const SCALING_FACTOR: f64 = 1.29281;

    /// Creates a new probabilistic counter
    /// with a given number of sketch instances and
    /// bits per instance. The counter
    /// counts no elements.
    pub(crate) fn new_zero(bits_per_instance: usize, num_instances: usize) -> Self {
        assert!(num_instances > 0);
        assert!(bits_per_instance > 0 && bits_per_instance <= u32::BITS as usize);
        assert!(bits_per_instance % 8 == 0);
        // TODO: implement
        let mut data = BitVec::new();
        data.resize(bits_per_instance * num_instances, false);

        Self {
            bits_per_instance,
            num_instances,
            data,
        }
    }

    /// Creates a new probabilistic counter
    /// with the same configuration as a given one.
    /// The new counter counts no elements.
    pub(crate) fn new_zero_with_same_config(other: &ProbabilisticCounter) -> Self {
        ProbabilisticCounter::new_zero(other.get_num_bits_per_instance(), other.get_num_instances())
    }

    /// Returns the number of sketch instances utilized
    /// by a given probabilistic counter.
    pub(crate) fn get_num_instances(&self) -> usize {
        // TODO: implement
        self.num_instances
    }

    /// Returns the number of bits per sketch instance
    /// utilized by a given probabilistic counter.
    pub(crate) fn get_num_bits_per_instance(&self) -> usize {
        // TODO: implement
        self.bits_per_instance
    }

    /// Given a u32 bit number drawn at random from a
    /// uniform distribution produces an a number from
    /// a geometric distribution with probability 1/2.
    /// The second parameter denotes the number of bits
    /// of the number that should be used.
    /// This function shall be used for selecting bits for
    /// incrementation of the sketches.
    pub(crate) fn uniform_u32_to_geometric(rand_no: u32, num_bits: usize) -> u32 {
        let rand_val = (rand_no as u64) & ((1_u64 << num_bits) - 1);
        let first_one = rand_val.view_bits::<Lsb0>().first_one();
        match first_one {
            None => 1,
            Some(idx) => idx as u32,
        }
    }

    /// Returns a given bit in a given instance of a given sketch.
    #[cfg(test)]
    pub(crate) fn get_bit(&self, instance_idx: usize, in_instance_bit_idx: usize) -> bool {
        // TODO: implement
        assert!(instance_idx < self.num_instances);
        assert!(in_instance_bit_idx < self.bits_per_instance);
        let idx = self.get_table_idx(instance_idx, in_instance_bit_idx);
        self.data[idx]
    }

    /// Sets a given bit in a given instance of a given sketch
    /// to the value provided as a parameter.
    #[cfg(test)]
    pub(crate) fn set_bit(&mut self, instance_idx: usize, in_instance_bit_idx: usize, val: bool) {
        // TODO: implement
        assert!(instance_idx < self.num_instances);
        assert!(in_instance_bit_idx < self.bits_per_instance);
        let idx = self.get_table_idx(instance_idx, in_instance_bit_idx);
        self.data.set(idx, val);
    }

    /// Returns a uniform random value that leads to
    /// setting a specific bit in the counter. In principle,
    /// this is used to partially revert function
    /// `uniform_u32_to_geometric` for testing.
    #[cfg(test)]
    pub(crate) fn geometric_to_sample_u32(geom_no: u32) -> u32 {
        assert!(geom_no < u32::BITS);
        1_u32 << geom_no
    }

    // TODO: you may add any extra methods here

    pub(crate) fn get_table_idx(&self, instance_idx: usize, in_instance_bit_idx: usize) -> usize {
        instance_idx * self.bits_per_instance + in_instance_bit_idx
    }

    pub(crate) fn get_table(&self, instance_idx: usize) -> &bitvec::slice::BitSlice<u8> {
        let start = self.get_table_idx(instance_idx, 0);
        let end = start + self.bits_per_instance;
        &self.data[start..end]
    }

    pub(crate) fn is_infinite(&self) -> bool {
        for i in 0..self.num_instances {
            if self.get_table(i).iter().all(|x| *x) {
                return true;
            }
        }
        false
    }
    pub(crate) fn is_zero(&self) -> bool {
        self.data.not_any()
    }
}

impl ConflictFreeReplicatedCounter<u64> for ProbabilisticCounter {
    /// Sets a given counter so that it counts
    /// no elements.

    fn set_to_zero(&mut self) {
        // TODO: implement
        self.data.fill(false);
    }

    /// Sets a given counter so that it counts
    /// an infinite number of elements (all possible).

    fn set_to_infinity(&mut self) {
        // TODO: implement
        self.data.fill(true);
    }

    /// Adds one more element to a given counter
    /// (increments the counter by one) by one using
    /// a given source of randomness.
    /// If the counter counts an infinite number of elements,
    /// an `Err` is returned and the given counter remains
    /// intact; otherwise, `Ok` is returned.

    fn try_count_one_more_element(&mut self, rs: &mut dyn RandomnessSource) -> Result<(), String> {
        // TODO: implement
        if self.is_infinite() {
            return Err("Counter is infinite".to_string());
        };

        for i in 0..self.num_instances {
            let rand_u32 = rs.next_u32();

            let geom = Self::uniform_u32_to_geometric(rand_u32, self.bits_per_instance);
            let bit_idx = if geom as usize >= self.bits_per_instance {
                self.bits_per_instance - 1
            } else {
                geom as usize
            };
            let pos = self.get_table_idx(i, bit_idx);
            self.data.set(pos, true);
        }
        Ok(())
    }

    /// Merges another counter with a given counter,
    /// so that, as a result, the given counter counts
    /// elements counted originally by both itself
    /// and the other counter. If the two counters are
    /// incompatible, `Err` is returned and the given
    /// counter remains intact; otherwise, `Ok` is returned.

    fn try_merge_with(&mut self, other: &Self) -> Result<(), String> {
        // TODO: implement
        if self.bits_per_instance != other.bits_per_instance
            || self.num_instances != other.num_instances
        {
            return Err("Configuration mismatch in counters".to_string());
        }
        self.data |= &other.data;

        Ok(())
    }

    /// Returns the number of elements counted
    /// by a given counter.

    fn evaluate(&self) -> u64 {
        // TODO: implement
        if self.is_zero() {
            return 0u64;
        }
        if self.is_infinite() {
            return u64::MAX;
        }
        let mut sum: u64 = 0;
        for i in 0..self.num_instances {
            let table = self.get_table(i);
            // find the first 0 in the table
            // guaranteed to be found by is_infinite check
            // HAS TO BE FIRST
            let count = table.iter().position(|x| !*x).unwrap() as u64;
            sum += count;
            debug!("count for row {}: {}", i, count);
        }
        let avg = sum as f64 / self.num_instances as f64;
        debug!("avg: {}", avg);
        let est = Self::SCALING_FACTOR * 2f64.powf(avg);
        debug!("est: {}", est);
        est.round() as u64
    }
}

/// A service allowing for sampling random nodes
/// from the system for gossiping.
pub(crate) trait PeerSamplingService {
    /// Returns a reference to a random Node
    /// in the system.
    fn get_random_peer(&mut self) -> ModuleRef<Node>;
}

#[derive(Clone)]
struct QueryState {
    counter: ProbabilisticCounter,
    predicate: Arc<dyn Fn(&Uuid) -> bool + Send + Sync>,
    id: u64,
    timestamp: SystemTime,
}

/// A node (process) in the system.
pub(crate) struct Node {
    uuid: Uuid,
    rs: Box<dyn RandomnessSource + Send>,
    pss: Box<dyn PeerSamplingService + Send>,
    // TODO: you may add any necessary fields here
    queries: HashMap<Uuid, QueryState>,
    next_id: u64,
}

/// A message used by a client to install
/// a query on a node.
pub(crate) struct QueryInstallMsg {
    pub(crate) bits_per_instance: usize,
    pub(crate) num_instances: usize,
    pub(crate) predicate: Arc<dyn Fn(&Uuid) -> bool + Send + Sync>,
}

/// A message used by a client to poll a node
/// to provide its current estimate of the query value.
pub(crate) struct QueryResultPollMsg {
    pub(crate) initiator: Uuid,
    pub(crate) callback: QueryResultPollCallback,
}

pub(crate) type QueryResultPollCallback =
    Box<dyn FnOnce(Option<u64>) -> Pin<Box<dyn Future<Output = ()> + Send>> + Send>;

/// A message that triggers a node to initiate
/// gossiping.
pub(crate) struct SyncTriggerMsg {}

/// A gossip message sent between two nodes.
pub(crate) struct SyncGossipMsg {
    // TODO: you may add any necessary fields here
    queries: Vec<(Uuid, QueryState)>,
}

impl Node {
    pub(crate) async fn new(
        system: &mut System,
        uuid: Uuid,
        rs: Box<dyn RandomnessSource + Send>,
        pss: Box<dyn PeerSamplingService + Send>,
    ) -> ModuleRef<Self> {
        let self_ref = system
            .register_module(Self {
                uuid,
                rs,
                pss,
                // TODO: you may add initialization of any added fields here
                queries: HashMap::new(),
                next_id: 0,
            })
            .await;
        self_ref
    }

    // TODO: you may add any extra methods here
}

fn get_id(id: Uuid) -> u32 {
    (id.as_u128() % 100) as u32
}

#[async_trait::async_trait]
impl Handler<QueryInstallMsg> for Node {
    async fn handle(&mut self, _self_ref: &ModuleRef<Self>, msg: QueryInstallMsg) {
        if msg.bits_per_instance == 0
            || msg.bits_per_instance > u32::BITS as usize
            || msg.bits_per_instance % 8 != 0
            || msg.num_instances == 0
        {
            return;
        }
        // TODO: implement
        let mut counter = ProbabilisticCounter::new_zero(msg.bits_per_instance, msg.num_instances);
        let initiator = self.uuid;

        if (msg.predicate)(&initiator) {
            counter.try_count_one_more_element(&mut *self.rs).unwrap();
        }
        self.next_id += 1;
        let qs = QueryState {
            predicate: msg.predicate,
            counter,
            id: self.next_id,
            // This is correct as long as a process doesn't crash and start to
            // use the same timestamp, however we don't consider this case
            // because we would need stable storage and ids
            // it's ok if a process crashes and starts with a new id
            timestamp: SystemTime::now(),
        };

        debug!("[{}] install query {}", get_id(initiator), qs.id % 100);

        self.queries.insert(initiator, qs);
    }
}

#[async_trait::async_trait]
impl Handler<QueryResultPollMsg> for Node {
    async fn handle(&mut self, _self_ref: &ModuleRef<Self>, msg: QueryResultPollMsg) {
        // TODO: implement
        let val = match self.queries.get(&msg.initiator) {
            None => None,
            Some(qs) => {
                let est = qs.counter.evaluate();
                debug!(
                    "[{}] query result of {}:  {}",
                    get_id(self.uuid),
                    get_id(msg.initiator),
                    (qs.id % 100)
                );
                Some(est)
            }
        };

        let fut = (msg.callback)(val);
        fut.await;
    }
}

#[async_trait::async_trait]
impl Handler<SyncTriggerMsg> for Node {
    async fn handle(&mut self, _self_ref: &ModuleRef<Self>, _msg: SyncTriggerMsg) {
        // TODO: implement
        let peer = self.pss.get_random_peer();

        let gossip_data: Vec<(Uuid, QueryState)> =
            self.queries.iter().map(|(k, v)| (*k, v.clone())).collect();

        let msg = SyncGossipMsg {
            queries: gossip_data,
        };

        peer.send(msg).await;
    }
}

#[async_trait::async_trait]
impl Handler<SyncGossipMsg> for Node {
    async fn handle(&mut self, _self_ref: &ModuleRef<Self>, msg: SyncGossipMsg) {
        // TODO: implement
        for (initiator, msg_qs) in msg.queries {
            let my_qs = self.queries.get(&initiator);
            let my_concern = (msg_qs.predicate)(&self.uuid);
            let mut result = ProbabilisticCounter::new_zero_with_same_config(&msg_qs.counter);
            match (my_qs, my_concern) {
                (None, true) => {
                    debug!(
                        "[{}] install query for {}: {}",
                        get_id(self.uuid),
                        get_id(initiator),
                        msg_qs.id % 100
                    );
                    // self.append_new_count(msg);
                    result.try_count_one_more_element(&mut *self.rs).unwrap();
                    result.try_merge_with(&msg_qs.counter).unwrap();
                }
                (None, false) => {
                    debug!(
                        "[{}] install query for {}: {}",
                        get_id(self.uuid),
                        get_id(initiator),
                        msg_qs.id % 100
                    );
                    result = msg_qs.counter;
                    // self.append_new(msg);
                }
                (Some(my_qs), true) => {
                    if my_qs.id == msg_qs.id {
                        result = msg_qs.counter;
                        result.try_merge_with(&my_qs.counter).unwrap();
                    } else {
                        if my_qs.id > msg_qs.id {
                            continue;
                        }
                        debug!(
                            "[{}] change query id for {}: {} -> {}",
                            get_id(self.uuid),
                            get_id(initiator),
                            my_qs.id % 100,
                            msg_qs.id % 100
                        );
                        result.try_count_one_more_element(&mut *self.rs).unwrap();
                        result.try_merge_with(&msg_qs.counter).unwrap();
                    }
                }
                (Some(my_qs), false) => {
                    if my_qs.id == msg_qs.id {
                        result = msg_qs.counter;
                        result.try_merge_with(&my_qs.counter).unwrap();
                    } else {
                        if my_qs.id > msg_qs.id {
                            continue;
                        }
                        debug!(
                            "[{}] change query id for {}: {} -> {}",
                            get_id(self.uuid),
                            get_id(initiator),
                            my_qs.id % 100,
                            msg_qs.id % 100
                        );
                        result = msg_qs.counter;
                    }
                }
            }
            let qs = QueryState {
                predicate: msg_qs.predicate,
                counter: result,
                id: msg_qs.id,
                timestamp: msg_qs.timestamp,
            };
            self.queries.insert(initiator, qs.clone());
        }
    }
}
