#include <vector>
#include <optional>
#include <iostream>

#include <pts/Exception.h>
#include <pts/Scan.h>
#include <pts/ThreadPool.h>
#include <pts/ProcessHandle.h>
#include <pts/Filter.h>
#include <pts/Token.h>
#include <pts/Watch.h>
#include <pts/Value.h>
#include <pts/Values.h>
#include <pts/Address.h>

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
    pts_scan_t *inner = pts_scan_new(threadPool->inner);

    if (inner == nullptr) {
        throw last_exception();
    }

    return std::make_shared<Scan>(Scan{threadPool, inner});
}

void Scan::initial(const std::shared_ptr<ProcessHandle> &handle, const std::shared_ptr<Filter> &filter, std::shared_ptr<Token> &token, ScanReporter &reporter)
{
    pts_scan_progress_t progress {
        [](auto data, uintptr_t percentage, uint64_t count) {
            auto d = reinterpret_cast<ScanReporter *>(data);
            (d->report)(percentage, count);
        },
    };

    auto ok = pts_scan_initial(
        inner,
        handle->inner,
        filter->inner,
        token->inner,
        &progress,
        reinterpret_cast<void *>(&reporter)
    );

    if (!ok) {
        throw last_exception();
    }
}

std::shared_ptr<Scan> Scan::scan(
    const std::shared_ptr<ProcessHandle> &handle,
    const std::shared_ptr<Filter> &filter,
    std::shared_ptr<Token> &token,
    ScanReporter &reporter
) const
{
    pts_scan_progress_t progress {
        [](auto data, uintptr_t percentage, uint64_t count) {
            auto d = reinterpret_cast<ScanReporter *>(data);
            (d->report)(percentage, count);
        },
    };

    auto scan = pts_scan_scan(
        inner,
        handle->inner,
        filter->inner,
        token->inner,
        &progress,
        reinterpret_cast<void *>(&reporter)
    );

    if (!scan) {
        throw last_exception();
    }

    return std::make_shared<Scan>(Scan{threadPool, scan});
}

void Scan::refresh(
    const std::shared_ptr<ProcessHandle> &processHandle,
    std::shared_ptr<Values> &values,
    std::shared_ptr<Token> &token,
    ScanReporter &reporter
)
{
    pts_scan_progress_t progress {
        [](auto data, uintptr_t percentage, uint64_t count) {
            auto d = reinterpret_cast<ScanReporter *>(data);
            (d->report)(percentage, count);
        },
    };

    if (!pts_scan_refresh(inner, processHandle->inner, values->inner, token->inner, &progress, reinterpret_cast<void *>(&reporter))) {
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

Values Scan::values(uintptr_t limit) const
{
    return Values{pts_scan_values(inner, limit)};
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

Value ScanResult::value() const
{
    return Value{pts_scan_result_value(&inner)};
}
}
