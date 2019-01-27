#include <pts/ThreadPool.h>
#include <pts.h>

namespace pts {
ThreadPool::ThreadPool(pts_thread_pool_t *inner) : inner(inner)
{
}

std::shared_ptr<ThreadPool> ThreadPool::create()
{
    if (auto inner = pts_thread_pool_new()) {
        return std::shared_ptr<ThreadPool>(new ThreadPool(inner));
    }

    throw last_exception();
}

pts_thread_pool_t *ThreadPool::ptr()
{
    return inner;
}

ThreadPool::~ThreadPool()
{
    pts_thread_pool_free(inner);
}
}
