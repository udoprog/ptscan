#ifndef PTS_THREADPOOL_H
#define PTS_THREADPOOL_H

#include <memory>

#include <ptscan.h>

namespace pts {
class ThreadPool {
public:
    ThreadPool();
    ThreadPool(pts_thread_pool_t *inner);
    ThreadPool(const ThreadPool &) = delete;
    ThreadPool(ThreadPool &&);
    ~ThreadPool();

    // Construct a new thread pool.
    static std::shared_ptr<ThreadPool> create();

    pts_thread_pool_t *ptr();
private:

    pts_thread_pool_t *inner;
};
};

#endif // PTS_THREADPOOL_H
