#include <pts.h>
#include <pts/Scanner.h>
#include <pts/ThreadPool.h>
#include <pts/ProcessHandle.h>
#include <pts/Filter.h>

namespace pts {
Scanner::Scanner(std::shared_ptr<ThreadPool> threadPool, pts_scanner_t* inner) :
    threadPool(threadPool),
    inner(inner)
{
}

std::shared_ptr<Scanner> Scanner::create(std::shared_ptr<ThreadPool> threadPool)
{
    pts_scanner_t *inner = pts_scanner_new(threadPool->ptr());

    if (inner == nullptr) {
        throw last_exception();
    }

    return std::shared_ptr<Scanner>(new Scanner(threadPool, inner));
}

void Scanner::scan(ProcessHandle &processHandle, Filter &filter, pts_scanner_progress_t *progress)
{
    if (!pts_scanner_scan(inner, processHandle.inner, filter.inner, progress)) {
        throw last_exception();
    }
}
}
