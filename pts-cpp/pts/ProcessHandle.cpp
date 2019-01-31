#include <pts/ProcessHandle.h>
#include <pts/String.h>
#include <pts/Exception.h>
#include <pts/System.h>
#include <pts/Address.h>

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
}
