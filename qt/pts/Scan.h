#ifndef PTS_SCAN_H
#define PTS_SCAN_H

#include <memory>
#include <functional>
#include <optional>

#include <ptscan.h>
#include <pts/String.h>

namespace pts {
class ProcessHandle;
class Filter;
class ThreadPool;
class Token;
class ScanResult;
class Watch;
class Values;

class ScanReporter {
public:
    std::function<void(uintptr_t)> report;
};

class Scan
{
public:
    explicit Scan(std::shared_ptr<ThreadPool> threadPool, pts_scan_t* inner);
    Scan();
    Scan(const Scan &) = delete;
    Scan(Scan &&);
    ~Scan();

    static std::shared_ptr<Scan> create(std::shared_ptr<ThreadPool> threadPool);

    // Perform a scan.
    void scan(ProcessHandle &handle, Filter &filter, Token &token, ScanReporter &reporter);

    // Refresh the scan with value from the given handle.
    void refresh(ProcessHandle &handle, Values &values, Token &token, ScanReporter &reporter);

    // Access scan results.
    std::vector<ScanResult> results(uintptr_t limit);

    // Access the scan result at the given location.
    std::optional<ScanResult> at(uintptr_t);

    // Access the count of a scan.
    uintptr_t count();
private:
    // NB: hold on to a reference of the thread pool since it's used by pts_scan_t.
    std::shared_ptr<ThreadPool> threadPool;
    pts_scan_t *inner;
};

class ScanResult {
    friend class Scan;

public:
    ScanResult() = default;
    ScanResult(const ScanResult &) = default;
    ScanResult(ScanResult &&) = default;

    // Convert into a watch.
    // If there is a process handle available, the watch will be decorated with more information.
    // Otherwise the watch will just be a plain address.
    std::shared_ptr<Watch> asWatch(std::shared_ptr<ProcessHandle> &handle);

    // Display the scan result.
    String address(const std::shared_ptr<ProcessHandle> &handle) const;

    // Last scanned value.
    String value() const;

private:
    explicit ScanResult(const pts_scan_result_t* inner);

    const pts_scan_result_t *inner;
};
}

#endif // PTS_SCAN_H
