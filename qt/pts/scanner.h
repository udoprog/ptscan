#ifndef PTS_SCANNER_H
#define PTS_SCANNER_H

#include <memory>

#include <pts/ThreadPool.h>

namespace pts{
class Scanner
{
public:
    Scanner(std::shared_ptr<ThreadPool> threadPool);
private:
    // NB: hold on to a reference of the thread pool since it's used by pts_scanner_t.
    std::shared_ptr<ThreadPool> threadPool;
    pts_scanner_t *inner;
};
}

#endif // PTS_SCANNER_H
