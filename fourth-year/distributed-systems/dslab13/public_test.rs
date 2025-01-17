#[allow(clippy::assertions_on_constants)]
#[cfg(test)]
pub(crate) mod tests {
    use crate::{
        solution::{
            ConflictFreeReplicatedCounter, Node, ProbabilisticCounter, QueryInstallMsg,
            QueryResultPollMsg, RandomnessSource, SyncTriggerMsg,
        },
        PeerSamplingServiceImpl,
    };
    use log::debug;
    use module_system::{ModuleRef, System};
    use ntest::timeout;
    use std::{
        ops::{Deref, DerefMut},
        sync::{Arc, Mutex},
        time::Duration,
    };
    use tokio::sync::mpsc::unbounded_channel;
    use uuid::Uuid;

    #[tokio::test]
    #[timeout(2000)]
    async fn pc_new_should_have_only_zero_bits() {
        // Given:
        let pc = ProbabilisticCounter::new_zero(16, 19);

        // When:

        // Then:
        assert_eq!(pc.get_num_bits_per_instance(), 16);
        assert_eq!(pc.get_num_instances(), 19);
        pc_check_bits(&pc, &[], true);
    }

    #[tokio::test]
    #[timeout(2000)]
    async fn pc_raw_bit_operations_and_distinguished_values_should_work() {
        // Given:
        let mut pc = ProbabilisticCounter::new_zero(32, 12);
        let selected_bits = vec![
            (0_usize, 1_usize),
            (0, 7),
            (0, 31),
            (1, 19),
            (1, 20),
            (1, 24),
            (2, 13),
            (3, 19),
            (9, 7),
            (9, 8),
            (9, 9),
            (11, 0),
            (11, 1),
            (11, 2),
            (11, 3),
            (11, 4),
            (11, 5),
            (11, 6),
            (11, 7),
        ];

        // When:
        pc_set_bits(&mut pc, &selected_bits, true);

        // Then:
        assert_eq!(pc.get_num_bits_per_instance(), 32);
        assert_eq!(pc.get_num_instances(), 12);
        pc_check_bits(&pc, &selected_bits, true);

        // When:
        pc.set_to_infinity();

        // Then:
        pc_check_bits(&pc, &[], false);

        // When:
        pc_set_bits(&mut pc, &selected_bits, false);

        // Then:
        pc_check_bits(&pc, &selected_bits, false);

        // When:
        pc.set_to_zero();

        // Then:
        pc_check_bits(&pc, &[], true);
    }

    #[tokio::test]
    #[timeout(2000)]
    async fn pc_incrementation_smoke_test() {
        // Given:
        let mut pc = ProbabilisticCounter::new_zero(32, 8);
        let rand_seq = vec![256_u32, 128, 64, 32, 16, 8, 4, 2, 1, 512, 1024];
        let mut rs = FixedRandomnessSequence::new(&rand_seq, false);
        let bit_seq = rand_seq
            .iter()
            .take(pc.get_num_instances())
            .enumerate()
            .map(|(i, v)| {
                (
                    i,
                    ProbabilisticCounter::uniform_u32_to_geometric(
                        *v,
                        pc.get_num_bits_per_instance(),
                    ) as usize,
                )
            })
            .collect::<Vec<(usize, usize)>>();

        // When:
        pc.try_count_one_more_element(&mut rs).unwrap();

        // Then:
        assert_eq!(rs.get_next_idx(), pc.get_num_instances());
        pc_check_bits(&pc, &bit_seq, true);
    }

    #[tokio::test]
    #[timeout(2000)]
    async fn pc_merging_and_evaluation_smoke_test() {
        // Given:
        let mut pc1 = ProbabilisticCounter::new_zero(32, 4);
        let mut pc2 = ProbabilisticCounter::new_zero(32, 4);
        let selected_bits1 = vec![(0_usize, 0_usize), (1, 2), (1, 5), (2, 0), (3, 2), (3, 5)];
        let selected_bits2 = vec![(0_usize, 0_usize), (1, 4), (1, 5), (2, 0), (3, 4), (3, 5)];

        // When:

        // Then:
        assert_eq!(pc1.evaluate(), 0);
        assert_eq!(pc2.evaluate(), 0);

        // When:
        pc1.set_to_infinity();
        pc2.try_merge_with(&pc1).unwrap();

        // Then:
        assert_eq!(pc_get_diff_position(&pc1, &pc2), None);
        assert_eq!(pc1.evaluate(), u64::MAX);
        assert_eq!(pc2.evaluate(), u64::MAX);

        // When:
        pc_set_bits(&mut pc1, &selected_bits1, false);
        pc_set_bits(&mut pc2, &selected_bits2, false);

        // Then:
        assert_eq!(pc1.evaluate(), 3);
        assert_eq!(pc2.evaluate(), 5);

        // When:
        pc2.try_merge_with(&pc1).unwrap();
        pc1.try_merge_with(&pc2).unwrap();

        // Then:
        assert_eq!(pc_get_diff_position(&pc1, &pc2), None);
        assert_eq!(pc1.evaluate(), 7);
        assert_eq!(pc2.evaluate(), 7);
    }

    #[tokio::test]
    #[timeout(10000)]
    async fn gossip_node_aggregates_should_converge() {
        env_logger::init();
        // Given:
        const NUM_NODES: usize = 10;
        assert!(NUM_NODES <= u32::BITS as usize);
        assert!(NUM_NODES > 2);
        let mut system = System::new().await;
        let uuids: Vec<Uuid> = (0..NUM_NODES).into_iter().map(|_| Uuid::new_v4()).collect();
        let node_refs = Arc::new(Mutex::new(Vec::with_capacity(NUM_NODES)));
        for (i, uuid) in uuids.iter().enumerate() {
            let pss_rs = Box::new(FixedRandomnessSequence::new(
                &(0..NUM_NODES as u32)
                    .into_iter()
                    .map(|n| (i as u32 + n) % NUM_NODES as u32)
                    .collect::<Vec<u32>>()[..],
                true,
            ));
            let node_rs = Box::new(FixedRandomnessSequence::new(
                &[ProbabilisticCounter::geometric_to_sample_u32(i as u32)],
                true,
            ));
            let pss = Box::new(PeerSamplingServiceImpl::new(node_refs.clone(), pss_rs));
            let node_ref = Node::new(&mut system, *uuid, node_rs, pss).await;
            let mut guard = node_refs.lock().unwrap();
            guard.deref_mut().push(node_ref);
        }
        // When:
        // - install a query;

        system_install_query(
            &node_refs,
            &[0],
            &[*uuids.get(0).unwrap(), *uuids.get(1).unwrap()],
            u32::BITS as usize,
            1,
        )
        .await;
        // - run a few rounds of gossiping;
        for _round in 0..(NUM_NODES * 2) {
            tokio::time::sleep(Duration::from_millis(25)).await;
            system_trigger_gossiping(&node_refs).await;
        }
        // - poll the result of the query;
        let result =
            *system_poll_query_results(&node_refs, &[NUM_NODES - 1], uuids.get(0).unwrap())
                .await
                .get(0)
                .unwrap();

        // Then:

        assert_eq!(result, Some(5));

        system_install_query(&node_refs, &[0], &[], u32::BITS as usize, 10).await;
        // - run a few rounds of gossiping;
        for _round in 0..(NUM_NODES * 2) {
            tokio::time::sleep(Duration::from_millis(25)).await;
            system_trigger_gossiping(&node_refs).await;
        }
        // - poll the result of the query;
        let result =
            *system_poll_query_results(&node_refs, &[NUM_NODES - 1], uuids.get(0).unwrap())
                .await
                .get(0)
                .unwrap();

        // Then:
        assert_eq!(result, Some(0));

        system_install_query(
            &node_refs,
            &[0],
            &[*uuids.get(0).unwrap(), *uuids.get(1).unwrap()],
            8,
            10,
        )
        .await;
        // - run a few rounds of gossiping;
        for _round in 0..(NUM_NODES * 2) {
            tokio::time::sleep(Duration::from_millis(25)).await;
            system_trigger_gossiping(&node_refs).await;
        }
        // - poll the result of the query;
        let result =
            *system_poll_query_results(&node_refs, &[NUM_NODES - 1], uuids.get(0).unwrap())
                .await
                .get(0)
                .unwrap();

        // Then:
        assert_eq!(result, Some(5));

        system_install_query(
            &node_refs,
            &[0],
            &[*uuids.get(0).unwrap(), *uuids.get(1).unwrap()],
            1,
            10,
        )
        .await;
        // - run a few rounds of gossiping;
        for _round in 0..(NUM_NODES * 2) {
            tokio::time::sleep(Duration::from_millis(25)).await;
            system_trigger_gossiping(&node_refs).await;
        }
        // - poll the result of the query;
        let result =
            *system_poll_query_results(&node_refs, &[NUM_NODES - 1], uuids.get(0).unwrap())
                .await
                .get(0)
                .unwrap();

        // Then:
        assert_eq!(result, Some(5));

        system_install_query(
            &node_refs,
            &[5],
            &[
                *uuids.get(0).unwrap(),
                *uuids.get(1).unwrap(),
                *uuids.get(2).unwrap(),
            ],
            32,
            32,
        )
        .await;
        // - run a few rounds of gossiping;
        for _round in 0..(NUM_NODES * 2) {
            tokio::time::sleep(Duration::from_millis(25)).await;
            system_trigger_gossiping(&node_refs).await;
        }
        // - poll the result of the query;
        let result = *system_poll_query_results(&node_refs, &[0], uuids.get(5).unwrap())
            .await
            .get(0)
            .unwrap();

        assert_eq!(result, Some(10));

        for i in 0..NUM_NODES {
            let mut i_ids = Vec::new();
            for j in 0..i {
                i_ids.push(*uuids.get(j).unwrap());
            }
            system_install_query(
                &node_refs,
                &[5],
                // &[
                //     *uuids.get(0).unwrap(),
                //     *uuids.get(1).unwrap(),
                //     *uuids.get(2).unwrap(),
                // ],
                // i ids
                &i_ids,
                32,
                32,
            )
            .await;
            for _round in 0..(1) {
                tokio::time::sleep(Duration::from_millis(25)).await;
                system_trigger_gossiping(&node_refs).await;
            }
        }

        // - run a few rounds of gossiping;
        for _round in 0..(NUM_NODES * 2) {
            tokio::time::sleep(Duration::from_millis(25)).await;
            system_trigger_gossiping(&node_refs).await;
        }
        // - poll the result of the query;
        let result = *system_poll_query_results(&node_refs, &[0], uuids.get(5).unwrap())
            .await
            .get(0)
            .unwrap();
        let res = 1.29281 * (2.0_f64.powf(NUM_NODES as f64 - 1.0));
        assert_eq!(result, Some(res.round() as u64));
        // Then:

        system.shutdown().await;
    }

    // -------------------------- HELPERS --------------------------

    pub(crate) fn pc_check_bits(pc: &ProbabilisticCounter, bits: &[(usize, usize)], val: bool) {
        let mut idx = 0;
        let mut prev_inst_idx = 0;
        let mut prev_bit_idx = 0;
        for inst_idx in 0..pc.get_num_instances() {
            for bit_idx in 0..pc.get_num_bits_per_instance() {
                let expected_val = if idx < bits.len() {
                    assert!(
                        bits[idx].0 < pc.get_num_instances(),
                        "wrong instance at index {}",
                        idx
                    );
                    assert!(
                        bits[idx].1 < pc.get_num_bits_per_instance(),
                        "wrong bit at index {}",
                        idx
                    );
                    assert!(
                        idx == 0
                            || bits[idx].0 > prev_inst_idx
                            || (bits[idx].0 == prev_inst_idx && bits[idx].1 > prev_bit_idx),
                        "unsorted entries ({}, {}) and then ({},{})",
                        prev_inst_idx,
                        prev_bit_idx,
                        bits[idx].0,
                        bits[idx].1
                    );
                    if inst_idx == bits[idx].0 && bit_idx == bits[idx].1 {
                        prev_inst_idx = bits[idx].0;
                        prev_bit_idx = bits[idx].1;
                        idx += 1;
                        val
                    } else {
                        !val
                    }
                } else {
                    !val
                };
                let actual_val = pc.get_bit(inst_idx, bit_idx);
                assert_eq!(
                    actual_val, expected_val,
                    "for instance {}, bit {}, expected value \"{}\" but got value \"{}\"",
                    inst_idx, bit_idx, expected_val, actual_val
                );
            }
        }
    }

    pub(crate) fn pc_set_bits(pc: &mut ProbabilisticCounter, bits: &[(usize, usize)], val: bool) {
        let mut prev_inst_idx = 0;
        let mut prev_bit_idx = 0;
        let mut first = true;
        for (inst_idx, bit_idx) in bits {
            assert!(
                first
                    || *inst_idx > prev_inst_idx
                    || (*inst_idx == prev_inst_idx && *bit_idx > prev_bit_idx),
                "unsorted entries ({}, {}) and then ({},{})",
                prev_inst_idx,
                prev_bit_idx,
                inst_idx,
                bit_idx
            );
            first = false;
            prev_inst_idx = *inst_idx;
            prev_bit_idx = *bit_idx;
            assert!(*inst_idx < pc.get_num_instances());
            assert!(*bit_idx < pc.get_num_bits_per_instance());
            pc.set_bit(*inst_idx, *bit_idx, val);
        }
    }

    pub(crate) fn pc_get_diff_position(
        first: &ProbabilisticCounter,
        second: &ProbabilisticCounter,
    ) -> Option<(usize, usize)> {
        assert_eq!(first.get_num_instances(), second.get_num_instances());
        assert_eq!(
            first.get_num_bits_per_instance(),
            second.get_num_bits_per_instance()
        );
        for inst_idx in 0..first.get_num_instances() {
            for bit_idx in 0..first.get_num_bits_per_instance() {
                if first.get_bit(inst_idx, bit_idx) != second.get_bit(inst_idx, bit_idx) {
                    return Some((inst_idx, bit_idx));
                }
            }
        }
        None
    }

    pub(crate) struct FixedRandomnessSequence {
        seq: Vec<u32>,
        repeat: bool,
        next: usize,
    }

    impl FixedRandomnessSequence {
        pub(crate) fn new(seq: &[u32], repeat: bool) -> Self {
            FixedRandomnessSequence {
                seq: seq.to_owned(),
                repeat,
                next: 0,
            }
        }
        pub(crate) fn get_next_idx(&self) -> usize {
            self.next
        }
    }

    impl RandomnessSource for FixedRandomnessSequence {
        fn next_u32(&mut self) -> u32 {
            if !self.repeat && self.next >= self.seq.len() {
                panic!("too many numbers generated by the sequence");
            }
            let res = *self.seq.get(self.next).unwrap();
            self.next += 1;
            if self.repeat && self.next >= self.seq.len() {
                self.next = 0;
            }
            res
        }
    }

    pub(crate) async fn system_install_query(
        node_refs: &Arc<Mutex<Vec<ModuleRef<Node>>>>,
        target_idxs: &[usize],
        satisfying_uuids: &[Uuid],
        bits_per_instance: usize,
        num_instances: usize,
    ) {
        let guard = node_refs.lock().unwrap();
        for idx in target_idxs {
            let uuids_copy: Vec<Uuid> = satisfying_uuids.to_vec();
            let msg = QueryInstallMsg {
                bits_per_instance,
                num_instances,
                predicate: Arc::new(move |u: &Uuid| uuids_copy.contains(u)),
            };
            guard.deref().get(*idx).unwrap().send(msg).await;
        }
    }

    pub(crate) async fn system_trigger_gossiping(node_refs: &Arc<Mutex<Vec<ModuleRef<Node>>>>) {
        let guard = node_refs.lock().unwrap();
        for node_ref in guard.deref() {
            node_ref.send(SyncTriggerMsg {}).await;
        }
    }

    pub(crate) async fn system_poll_query_results(
        node_refs: &Arc<Mutex<Vec<ModuleRef<Node>>>>,
        target_idxs: &[usize],
        initiator_uuid: &Uuid,
    ) -> Vec<Option<u64>> {
        let mut receivers = Vec::with_capacity(target_idxs.len());
        for idx in target_idxs {
            let (result_tx, result_rx) = unbounded_channel();
            receivers.push(result_rx);
            let msg = QueryResultPollMsg {
                initiator: *initiator_uuid,
                callback: Box::new(|val: Option<u64>| {
                    Box::pin(async move {
                        result_tx.send(val).unwrap();
                    })
                }),
            };
            let guard = node_refs.lock().unwrap();
            guard.deref().get(*idx).unwrap().send(msg).await;
        }
        let mut results = Vec::with_capacity(receivers.len());
        for mut r in receivers {
            results.push(r.recv().await.unwrap());
        }
        results
    }
}
