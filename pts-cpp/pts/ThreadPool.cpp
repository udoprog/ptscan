#include <pts/ThreadPool.h>
#include <pts/Exception.h>

namespace pts {
ThreadPool::ThreadPool() :
    inner(nullptr)
{
}

ThreadPool::ThreadPool(pts_thread_pool_t *inner) :
    inner(inner)
{
}

ThreadPool::ThreadPool(ThreadPool &&other) :
    inner(other.inner)
{
    other.inner = nullptr;
}

ThreadPool::~ThreadPool()
{
    if (inner) {
        pts_thread_pool_free(inner);
        inner = nullptr;
    }
}

std::shared_ptr<ThreadPool> ThreadPool::create()
{
    if (auto inner = pts_thread_pool_new()) {
        return std::make_shared<ThreadPool>(ThreadPool{inner});
    }

    throw last_exception();
}
}
