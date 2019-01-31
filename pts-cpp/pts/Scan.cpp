#include <pts/Exception.h>
#include <pts/Scan.h>
#include <pts/ThreadPool.h>
#include <pts/ProcessHandle.h>
#include <pts/Filter.h>
#include <pts/Token.h>
#include <pts/Watch.h>
#include <pts/Values.h>
#include <pts/Address.h>
#include <vector>
#include <optional>

namespace pts {
Scan::Scan(std::shared_ptr<ThreadPool> threadPool, pts_scan_t* inner) :
    threadPool(threadPool),
    inner(inner)
{
}

Scan::Scan() :
    inner(nullptr)
{
}

Scan::Scan(Scan &&other) :
    inner(other.inner)
{
    other.inner = nullptr;
}

Scan::~Scan()
{
    if (inner) {
        pts_scan_free(inner);
        inner = nullptr;
    }
}

std::shared_ptr<Scan> Scan::create(std::shared_ptr<ThreadPool> threadPool)
{
    pts_scan_t *inner = pts_scan_new(threadPool->ptr());

    if (inner == nullptr) {
        throw last_exception();
    }

    return std::make_shared<Scan>(Scan{threadPool, inner});
}

void Scan::scan(ProcessHandle &processHandle, Filter &filter, Token &token, ScanReporter &reporter)
{
    pts_scan_progress_t progress {
        [](auto data, uintptr_t percentage) {
            auto d = reinterpret_cast<ScanReporter *>(data);
            (d->report)(percentage);
        },
    };

    if (!pts_scan_scan(inner, processHandle.inner, filter.inner, token.inner, &progress, reinterpret_cast<void *>(&reporter))) {
        throw last_exception();
    }
}

void Scan::refresh(ProcessHandle &processHandle, Values &values, Token &token, ScanReporter &reporter)
{
    pts_scan_progress_t progress {
        [](auto data, uintptr_t percentage) {
            auto d = reinterpret_cast<ScanReporter *>(data);
            (d->report)(percentage);
        },
    };

    if (!pts_scan_refresh(inner, processHandle.inner, values.inner, token.inner, &progress, reinterpret_cast<void *>(&reporter))) {
        throw last_exception();
    }
}

std::vector<ScanResult> Scan::results(uintptr_t limit)
{
    std::vector<ScanResult> out;
    pts_scan_results_iter_t *iter = pts_scan_results_iter(inner);
    pts_scan_result_t next;

    while (pts_scan_results_next(iter, &next) && limit > 0) {
        out.push_back(ScanResult{next});
        limit -= 1;
    }

    pts_scan_results_free(iter);
    return out;
}

std::optional<ScanResult> Scan::at(uintptr_t offset)
{
    pts_scan_result_t result;

    if (pts_scan_result_at(inner, offset, &result)) {
        return std::make_optional(ScanResult{result});
    }

    return {};
}

uintptr_t Scan::count()
{
    return pts_scan_count(inner);
}

Values Scan::values()
{
    return Values{pts_scan_values(inner)};
}

ScanResult::ScanResult(pts_scan_result_t inner) :
    inner(inner)
{
}

std::shared_ptr<Watch> ScanResult::asWatch(std::shared_ptr<ProcessHandle> &handle)
{
    pts_process_handle_t *p = nullptr;

    if (handle) {
        p = handle.get()->inner;
    }

    auto watch = pts_scan_result_as_watch(&inner, p);

    if (!watch) {
        throw last_exception();
    }

    return std::make_shared<Watch>(Watch{watch});
}

Address ScanResult::address() const
{
    return Address{pts_scan_result_address(&inner)};
}

String ScanResult::value() const
{
    String value;
    pts_scan_result_value(&inner, &value.inner);
    return value;
}
}
