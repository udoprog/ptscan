#ifndef PTS_THREADPOOL_H
#define PTS_THREADPOOL_H

#include <ptscan.h>
#include <memory>

namespace pts {
class ThreadPool {
public:
    ThreadPool(const ThreadPool &) = delete;
    ~ThreadPool();

    // Construct a new thread pool.
    static std::shared_ptr<ThreadPool> create();

    pts_thread_pool_t *ptr();
private:
    ThreadPool(pts_thread_pool_t *inner);

    pts_thread_pool_t *inner;
};
};

#endif // PTS_THREADPOOL_H