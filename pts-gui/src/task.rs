//! Multithreaded task system.

use parking_lot::Mutex;

lazy_static::lazy_static! {
    static ref TASKS: Tasks = {
        Tasks {
            thread_pool: Mutex::new(threadpool::ThreadPool::new(usize::max(4, num_cpus::get()))),
        }
    };
}

struct Tasks {
    thread_pool: Mutex<threadpool::ThreadPool>,
}

pub struct Sender<Y, U>(glib::Sender<Progress<Y, U>>);

impl<Y, U> Sender<Y, U> {
    /// Yield an in-progress value.
    pub fn emit(&self, value: Y) {
        self.0
            .send(Progress::Emit(value))
            .expect("failed to send emitted value");
    }
}

impl Tasks {
    fn task<T, Y, U>(&self, task: T) -> glib::Receiver<Progress<Y, U>>
    where
        T: 'static + Send + Fn(&Sender<Y, U>) -> anyhow::Result<U>,
        Y: 'static + Send,
        U: 'static + Send,
    {
        let (tx, rx) = glib::MainContext::channel(glib::PRIORITY_DEFAULT);

        self.thread_pool.lock().execute(move || {
            let sender = Sender(tx);
            let result = task(&sender);
            sender
                .0
                .send(Progress::Result(result))
                .expect("failed to send value");
        });

        rx
    }

    fn oneshot<T, U>(&self, task: T) -> glib::Receiver<anyhow::Result<U>>
    where
        T: 'static + Send + Fn() -> anyhow::Result<U>,
        U: 'static + Send,
    {
        let (tx, rx) = glib::MainContext::channel(glib::PRIORITY_DEFAULT);

        self.thread_pool.lock().execute(move || {
            let result = task();
            tx.send(result).expect("failed to send value");
        });

        rx
    }
}

/// The output of a task.
///
/// `Emit` is incremental output, which can be used for reporting progress.
/// `Result` is the result of a task.
pub enum Progress<Y, T> {
    Emit(Y),
    Result(anyhow::Result<T>),
}

/// Run the given task and associate its output to a channel.
pub fn incremental<T, Y, U>(task: T) -> glib::Receiver<Progress<Y, U>>
where
    T: 'static + Send + Fn(&Sender<Y, U>) -> anyhow::Result<U>,
    Y: 'static + Send,
    U: 'static + Send,
{
    TASKS.task(task)
}

/// Run the given task and associate its output to a channel.
///
/// This is different from `task` since it doesn't provide incremental (emitted)
/// progress.
pub fn oneshot<T, U>(task: T) -> glib::Receiver<anyhow::Result<U>>
where
    T: 'static + Send + Fn() -> anyhow::Result<U>,
    U: 'static + Send,
{
    TASKS.oneshot(task)
}
