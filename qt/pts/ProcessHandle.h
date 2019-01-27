#ifndef PTS_PROCESSHANDLE_H
#define PTS_PROCESSHANDLE_H

#include <string>
#include <optional>

#include <ptscan.h>
#include <pts.h>

namespace pts
{
class ProcessHandle {
public:
    ProcessHandle(pts_process_handle_t *handle);
    ProcessHandle(const ProcessHandle &) = delete;
    ~ProcessHandle();

    std::string pid();
    std::string name();

    /// Open a process handle from a pid.
    static std::shared_ptr<ProcessHandle> open(process_id pid);

private:
    pts_process_handle_t *handle;
};
}

#endif // PTS_PROCESSHANDLE_H
