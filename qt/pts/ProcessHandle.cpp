#include <pts/ProcessHandle.h>
#include <pts/String.h>

namespace pts
{
ProcessHandle::ProcessHandle(pts_process_handle_t *handle) : handle(handle)
{
}

ProcessHandle::~ProcessHandle()
{
    pts_process_handle_free(handle);
}

std::string ProcessHandle::pid() {
    String pid;
    pts_process_handle_pid(this->handle, pid.ptr());
    return pid.string();
}

std::string ProcessHandle::name() {
    String name;
    pts_process_handle_name(this->handle, name.ptr());
    return name.string();
}

std::shared_ptr<ProcessHandle> ProcessHandle::open(process_id pid)
{
    pts_process_handle_t *handle = nullptr;
    pts_process_handle_open(pid, &handle);

    if (!handle) {
        return {};
    }

    std::shared_ptr<ProcessHandle> p{new ProcessHandle(handle)};
    return std::shared_ptr<ProcessHandle>{p};
}
}
