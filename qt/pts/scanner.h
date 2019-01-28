#ifndef PTS_SCANNER_H
#define PTS_SCANNER_H

#include <memory>
#include <functional>

#include <ptscan.h>

namespace pts {
class ProcessHandle;
class Filter;
class ThreadPool;

class ScanReporter {
public:
    std::function<void(uintptr_t)> report;
    std::function<void(bool)> done;
};

class Scanner
{
public:
    static std::shared_ptr<Scanner> create(std::shared_ptr<ThreadPool> threadPool);

    void scan(ProcessHandle &processHandle, Filter &filter, ScanReporter &reporter);
private:
    Scanner(std::shared_ptr<ThreadPool> threadPool, pts_scanner_t* inner);

    // NB: hold on to a reference of the thread pool since it's used by pts_scanner_t.
    std::shared_ptr<ThreadPool> threadPool;
    pts_scanner_t *inner;
};
}

#endif // PTS_SCANNER_H
