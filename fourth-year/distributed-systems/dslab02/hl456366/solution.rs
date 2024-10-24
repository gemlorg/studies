use std::sync::{Arc, Condvar, Mutex};
use std::thread::JoinHandle;
use std::collections::VecDeque;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;


type Task = Box<dyn FnOnce() + Send>;

// You can define new types (e.g., structs) if you need.
// However, they shall not be public (i.e., do not use the `pub` keyword).

/// The thread pool.
pub struct Threadpool {
    workers: Vec<JoinHandle<()>>,
    task_queue: Arc<(Mutex<VecDeque<Task>>, Condvar)>,
    is_running: Arc<AtomicBool>,
}

impl Threadpool {
    /// Create new thread pool with `workers_count` workers.
    pub fn new(workers_count: usize) -> Self {
        let task_queue = Arc::new((Mutex::new(VecDeque::new()), Condvar::new()));
        let is_running = Arc::new(AtomicBool::new(true));
        let mut workers = Vec::with_capacity(workers_count);

        for _ in 0..workers_count {
            let task_queue_clone = task_queue.clone();
            let is_running_clone = is_running.clone();
            workers.push(std::thread::spawn(move || {
                Self::worker_loop(task_queue_clone, is_running_clone);
            }));
        }
        Threadpool {
            workers,
            task_queue,
            is_running,
        }
    }

    /// Submit a new task.
    pub fn submit(&self, task: Task) {
        let (lock, cvar) = &*self.task_queue;
        let mut queue = lock.lock().unwrap();
        queue.push_back(task);
        cvar.notify_one();
    }

    // We suggest extracting the implementation of the worker to an associated
    // function, like this one (however, it is not a part of the public
    // interface, so you can delete it if you implement it differently):
    fn worker_loop(task_queue: Arc<(Mutex<VecDeque<Task>>, Condvar)>, is_running: Arc<AtomicBool>) {
        let (lock, cvar) = &*task_queue;

        loop {
            let mut queue = lock.lock().unwrap();
            while queue.is_empty() && is_running.load(Ordering::Relaxed) {
                queue = cvar.wait(queue).unwrap();
            }
            if !queue.is_empty() {
                let task = queue.pop_front().unwrap();
                drop(queue);
                task();
            } else {
                assert!(!is_running.load(Ordering::Relaxed));
                break;
            }
        }
    }
}

impl Drop for Threadpool {
    /// Gracefully end the thread pool.
    ///
    /// It waits until all submitted tasks are executed,
    /// and until all threads are joined.
    fn drop(&mut self) {
        self.is_running.store(false, Ordering::Relaxed);
        let (_, cvar) = &*self.task_queue;
        cvar.notify_all();
        for worker in self.workers.drain(..) {
            worker.join().unwrap();
        }
    }
}
