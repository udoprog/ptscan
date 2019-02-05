#ifndef PTS_THREADPOOL_H
#define PTS_THREADPOOL_H

#include <memory>

#include <ptscan.h>

namespace pts {
class ProcessHandle;
class ThreadPool;
class Scan;

class ThreadPool {
    friend class ProcessHandle;
    friend class ThreadPool;
    friend class Scan;

public:
    ThreadPool();
    ThreadPool(const ThreadPool &) = delete;
    ThreadPool(ThreadPool &&);
    ~ThreadPool();

    // Construct a new thread pool.
    static std::shared_ptr<ThreadPool> create();
private:
    explicit ThreadPool(pts_thread_pool_t *inner);

    pts_thread_pool_t *inner;
};
};

#endif // PTS_THREADPOOL_H
