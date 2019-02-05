#include <pts/ProcessHandle.h>
#include <pts/String.h>
#include <pts/Exception.h>
#include <pts/System.h>
#include <pts/Address.h>
#include <pts/Scan.h>
#include <pts/ThreadPool.h>
#include <pts/Addresses.h>
#include <pts/Values.h>
#include <pts/Token.h>

namespace pts
{
ProcessHandle::ProcessHandle(pts_process_handle_t *inner) :
    inner(inner)
{
}

ProcessHandle::ProcessHandle() :
    inner(nullptr)
{
}

ProcessHandle::ProcessHandle(ProcessHandle &&other) :
    inner(other.inner)
{
    other.inner = nullptr;
}

ProcessHandle::~ProcessHandle()
{
    if (inner) {
        pts_process_handle_free(inner);
        inner = nullptr;
    }
}

String ProcessHandle::pid() {
    String pid;
    pts_process_handle_pid(inner, &pid.inner);
    return pid;
}

String ProcessHandle::name() {
    String name;
    pts_process_handle_name(inner, &name.inner);
    return name;
}

void ProcessHandle::refreshThreads()
{
    if (!pts_process_handle_refresh_threads(inner)) {
        throw last_exception();
    }
}

void ProcessHandle::refreshModules()
{
    if (!pts_process_handle_refresh_modules(inner)) {
        throw last_exception();
    }
}

std::shared_ptr<ProcessHandle> ProcessHandle::open(process_id pid)
{
    pts_process_handle_t *handle = nullptr;
    pts_process_handle_open(pid, &handle);

    if (!handle) {
        return {};
    }

    return std::make_shared<ProcessHandle>(handle);
}

std::optional<Address> ProcessHandle::readPointer(const std::shared_ptr<Pointer> &pointer)
{
    Address address;

    if (pts_process_handle_read_pointer(inner, pointer->inner, &address.inner)) {
        return std::make_optional(address);
    }

    return {};
}

void ProcessHandle::readMemory(
    const std::shared_ptr<ThreadPool> &threadPool,
    const std::shared_ptr<Addresses> &addresses,
    const std::shared_ptr<Values> &values,
    std::shared_ptr<Values> &output,
    std::shared_ptr<Token> &cancel,
    ScanReporter &reporter
)
{
    pts_scan_progress_t progress {
        [](auto data, uintptr_t percentage, uint64_t count) {
            auto d = reinterpret_cast<ScanReporter *>(data);
            (d->report)(percentage, count);
        },
    };

    auto result = pts_process_handle_read_memory(
        inner,
        threadPool->inner,
        addresses->inner,
        values->inner,
        output->inner,
        cancel->inner,
        &progress,
        reinterpret_cast<void *>(&reporter)
    );

    if (!result) {
        throw last_exception();
    }
}
}
