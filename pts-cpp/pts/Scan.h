#ifndef PTS_SCAN_H
#define PTS_SCAN_H

#include <memory>
#include <functional>
#include <optional>

#include <pts/String.h>
#include <ptscan.h>

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
    std::function<void(uintptr_t, uint64_t)> report;
};

class Scan
{
public:
    Scan();
    Scan(const Scan &) = delete;
    Scan(Scan &&);
    ~Scan();

    static std::shared_ptr<Scan> create(std::shared_ptr<ThreadPool> threadPool);

    // Perform the initial scan.
    void initial(
        const std::shared_ptr<ProcessHandle> &handle,
        const std::shared_ptr<Filter> &filter,
        std::shared_ptr<Token> &token,
        ScanReporter &reporter
    );

    // Perform an additional scan.
    // Returns an updated scan.
    std::shared_ptr<Scan> scan(
        const std::shared_ptr<ProcessHandle> &handle,
        const std::shared_ptr<Filter> &filter,
        std::shared_ptr<Token> &token,
        ScanReporter &reporter
    ) const;

    // Refresh the scan with value from the given handle.
    void refresh(const std::shared_ptr<ProcessHandle> &handle, std::shared_ptr<Values> &values, std::shared_ptr<Token> &token, ScanReporter &reporter);

    // Access scan results.
    std::vector<ScanResult> results(uintptr_t limit);

    // Access the scan result at the given location.
    std::optional<ScanResult> at(uintptr_t);

    // Access the count of a scan.
    uintptr_t count();

    // Return a copy of all values contained in the scan.
    Values values(uintptr_t limit) const;
private:
    explicit Scan(std::shared_ptr<ThreadPool> threadPool, pts_scan_t* inner);

    // NB: hold on to a reference of the thread pool since it's used by pts_scan_t.
    std::shared_ptr<ThreadPool> threadPool;
    pts_scan_t *inner;
};

class ScanResult {
    friend class Scan;

public:
    ScanResult(const ScanResult &) = default;

    // Convert into a watch.
    // If there is a process handle available, the watch will be decorated with more information.
    // Otherwise the watch will just be a plain address.
    std::shared_ptr<Watch> asWatch(std::shared_ptr<ProcessHandle> &handle);

    // Display the scan result.
    Address address() const;

    // Last scanned value.
    Value value() const;

private:
    explicit ScanResult(pts_scan_result_t inner);

    pts_scan_result_t inner;
};
}

#endif // PTS_SCAN_H
