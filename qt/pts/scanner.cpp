#include <pts/Scanner.h>
#include <pts.h>

namespace pts {
Scanner::Scanner(std::shared_ptr<ThreadPool> threadPool) :
    threadPool(threadPool)
{
    pts_scanner_t *inner = pts_scanner_new(threadPool->ptr());

    if (inner == nullptr) {
        throw last_exception();
    }

    this->inner = inner;
}
}
