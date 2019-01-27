#ifndef PTS_SCANNER_H
#define PTS_SCANNER_H

#include <memory>

#include <ptscan.h>

namespace pts {
class ProcessHandle;
class ThreadPool;
class Filter;

class Scanner
{
public:
    static std::shared_ptr<Scanner> create(std::shared_ptr<ThreadPool> threadPool);

    void scan(ProcessHandle &processHandle, Filter &filter, pts_scanner_progress_t *progress);
private:
    Scanner(std::shared_ptr<ThreadPool> threadPool, pts_scanner_t* inner);

    // NB: hold on to a reference of the thread pool since it's used by pts_scanner_t.
    std::shared_ptr<ThreadPool> threadPool;
    pts_scanner_t *inner;
};
}

#endif // PTS_SCANNER_H
