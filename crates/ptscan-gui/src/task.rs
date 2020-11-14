//! Multithreaded task system.

use parking_lot::Mutex;
use std::{
    cell::RefCell,
    fmt,
    rc::Rc,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    },
};

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

pub struct Context<Y, U> {
    cancel: Arc<AtomicBool>,
    tx: glib::Sender<Progress<Y, U>>,
}

impl<Y, U> Context<Y, U> {
    /// Yield an in-progress value.
    pub fn emit(&self, value: Y) {
        self.tx
            .send(Progress::Emit(value))
            .expect("failed to emit value");
    }

    /// Test if the current task is cancelled.
    pub fn is_stopped(&self) -> bool {
        self.cancel.load(Ordering::Acquire)
    }

    pub fn as_token(&self) -> &ptscan::Token {
        ptscan::Token::from_raw(&self.cancel)
    }
}

/// Handle that when dropped cancels the task associated with it.
///
/// Note that this doesn't do anything unless the task is actively
/// monitoring for the cancellation flag. In particular, there's no
/// guarantee that the underlying task will actually stop working.
///
/// Note: relies on the `Drop` implementation.
pub struct Handle(Arc<AtomicBool>);

impl Drop for Handle {
    fn drop(&mut self) {
        self.0.store(true, Ordering::Release);
    }
}

impl fmt::Debug for Handle {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "Handle(cancelled: {})", self.0.load(Ordering::Acquire))
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

/// A oneshot task that will run until it produces a result.
#[must_use = "must be finalized with .build"]
pub struct Task<C, Y, U> {
    cancel: Arc<AtomicBool>,
    context: Rc<RefCell<C>>,
    task: Box<dyn FnOnce(&Context<Y, U>) -> anyhow::Result<U> + Send>,
    emit: Option<Box<dyn FnMut(&mut C, Y)>>,
    then: Option<Box<dyn FnOnce(&mut C, anyhow::Result<U>)>>,
}

impl<C, U> Task<C, (), U>
where
    C: 'static,
    U: 'static + Send,
{
    /// Run the given task and associate its output to a channel.
    ///
    /// This is a shorthand which assumes that the underlying task emits `()` for
    /// progress.
    pub fn oneshot<T>(context: &Rc<RefCell<C>>, task: T) -> Task<C, (), U>
    where
        T: 'static + Send + FnOnce(&Context<(), U>) -> anyhow::Result<U>,
    {
        Self::new(context, task)
    }
}

impl<C, Y, U> Task<C, Y, U>
where
    C: 'static,
    Y: 'static + Send,
    U: 'static + Send,
{
    /// Run the given task and associate its output to a channel.
    pub fn new<T>(context: &Rc<RefCell<C>>, task: T) -> Task<C, Y, U>
    where
        T: 'static + Send + FnOnce(&Context<Y, U>) -> anyhow::Result<U>,
    {
        Task {
            cancel: Arc::new(AtomicBool::new(false)),
            context: context.clone(),
            task: Box::new(task),
            emit: None,
            then: None,
        }
    }

    /// Register a handler to be fired when the oneshot is done.
    pub fn emit<Callback>(&mut self, emit: Callback)
    where
        Callback: 'static + FnMut(&mut C, Y),
    {
        self.emit = Some(Box::new(emit));
    }

    /// Register a handler to be fired when the oneshot is done.
    pub fn then<Callback>(&mut self, then: Callback)
    where
        Callback: 'static + FnOnce(&mut C, anyhow::Result<U>),
    {
        self.then = Some(Box::new(then));
    }

    /// Run the oneshot task with the current configuration.
    pub fn run(self) -> Handle {
        let Self {
            cancel,
            context,
            task,
            mut emit,
            mut then,
        } = self;

        let (tx, rx) = glib::MainContext::channel::<Progress<Y, U>>(glib::PRIORITY_DEFAULT);
        let cancel2 = cancel.clone();

        TASKS.thread_pool.lock().execute(move || {
            let context = Context { cancel, tx };
            let result = task(&context);

            context
                .tx
                .send(Progress::Result(result))
                .expect("failed to send value");
        });

        rx.attach(None, move |result| {
            let mut context = context.borrow_mut();

            let result = match result {
                Progress::Emit(result) => {
                    if let Some(emit) = &mut emit {
                        emit(&mut *context, result);
                    }

                    return glib::Continue(true);
                }
                Progress::Result(result) => result,
            };

            if let Some(then) = then.take() {
                then(&mut *context, result);
            }

            glib::Continue(false)
        });

        Handle(cancel2)
    }
}
