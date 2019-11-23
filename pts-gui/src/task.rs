//! Multithreaded task system.

use parking_lot::Mutex;
use std::{
    cell::RefCell,
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
            .expect("failed to send emitted value");
    }

    /// Test if the current task is cancelled.
    pub fn is_cancelled(&self) -> bool {
        self.cancel.load(Ordering::Acquire)
    }
}

pub struct Handle(Arc<AtomicBool>, Option<glib::SourceId>);

impl Handle {
    /// Cancel the task associated with the current handle.
    ///
    /// Note that this doesn't do anything unless the task is actively
    /// monitoring for the cancellation flag. In particular, there's no
    /// guarantee that the underlying task will actually stop working.
    pub fn cancel(mut self) {
        if let Some(source_id) = self.1.take() {
            self.0.store(true, Ordering::Release);
            glib::source::source_remove(source_id);
        }
    }
}

impl Drop for Handle {
    fn drop(&mut self) {
        if let Some(source_id) = self.1.take() {
            self.0.store(true, Ordering::Release);
            glib::source::source_remove(source_id);
        }
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
pub struct Task<C, T, Y, U> {
    cancel: Arc<AtomicBool>,
    context: Rc<RefCell<C>>,
    task: T,
    on_complete: Option<Box<dyn Fn(&mut C)>>,
    on_emit: Option<Box<dyn Fn(&mut C, Y)>>,
    on_ok: Option<Box<dyn Fn(&mut C, U)>>,
    on_err: Option<Box<dyn Fn(&mut C, anyhow::Error)>>,
}

impl<C, T, U> Task<C, T, (), U>
where
    C: 'static,
    T: 'static + Send + Fn(&Context<(), U>) -> anyhow::Result<U>,
    U: 'static + Send,
{
    /// Run the given task and associate its output to a channel.
    ///
    /// This is a shorthand which assumes that the underlying task emits `()` for
    /// progress.
    pub fn oneshot(context: &Rc<RefCell<C>>, task: T) -> Task<C, T, (), U> {
        Self::new(context, task)
    }
}

impl<C, T, Y, U> Task<C, T, Y, U>
where
    C: 'static,
    T: 'static + Send + Fn(&Context<Y, U>) -> anyhow::Result<U>,
    Y: 'static + Send,
    U: 'static + Send,
{
    /// Run the given task and associate its output to a channel.
    pub fn new(context: &Rc<RefCell<C>>, task: T) -> Task<C, T, Y, U> {
        Task {
            cancel: Arc::new(AtomicBool::new(false)),
            context: context.clone(),
            task,
            on_complete: None,
            on_emit: None,
            on_ok: None,
            on_err: None,
        }
    }

    /// Register a handler to be fired when the oneshot is completed.
    pub fn on_complete<Callback>(&mut self, on_complete: Callback)
    where
        Callback: 'static + Fn(&mut C),
    {
        self.on_complete = Some(Box::new(on_complete));
    }

    /// Register a handler to be fired when the oneshot is done.
    pub fn on_emit<Callback>(&mut self, on_emit: Callback)
    where
        Callback: 'static + Fn(&mut C, Y),
    {
        self.on_emit = Some(Box::new(on_emit));
    }

    /// Register a handler to be fired when the oneshot is done.
    pub fn on_ok<Callback>(&mut self, on_ok: Callback)
    where
        Callback: 'static + Fn(&mut C, U),
    {
        self.on_ok = Some(Box::new(on_ok));
    }

    /// Register a handler to be fired when the oneshot is errored.
    pub fn on_err<Callback>(&mut self, on_err: Callback)
    where
        Callback: 'static + Fn(&mut C, anyhow::Error),
    {
        self.on_err = Some(Box::new(on_err));
    }

    /// Run the oneshot task with the current configuration.
    pub fn run(self) -> Handle {
        let Self {
            cancel,
            context,
            task,
            on_complete,
            on_emit,
            on_ok,
            on_err,
        } = self;

        let (tx, rx) = glib::MainContext::channel::<Progress<Y, U>>(glib::PRIORITY_DEFAULT);
        let cancel2 = cancel.clone();

        TASKS.thread_pool.lock().execute(move || {
            let context = Context { cancel, tx };

            let result = task(&context);

            if context.is_cancelled() {
                return;
            }

            context
                .tx
                .send(Progress::Result(result))
                .expect("failed to send value");
        });

        let source_id = rx.attach(None, move |result| {
            let mut context = context.borrow_mut();

            let result = match result {
                Progress::Emit(result) => {
                    if let Some(on_emit) = &on_emit {
                        on_emit(&mut *context, result);
                    }

                    return glib::Continue(true);
                }
                Progress::Result(result) => result,
            };

            if let Some(on_complete) = &on_complete {
                on_complete(&mut *context);
            }

            match result {
                Ok(result) => {
                    if let Some(on_ok) = &on_ok {
                        on_ok(&mut *context, result);
                    }
                }
                Err(e) => {
                    if let Some(on_err) = &on_err {
                        on_err(&mut *context, e);
                    }
                }
            }

            glib::Continue(false)
        });

        Handle(cancel2, Some(source_id))
    }
}
