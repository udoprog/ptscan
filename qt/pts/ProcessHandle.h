#ifndef PTS_PROCESSHANDLE_H
#define PTS_PROCESSHANDLE_H

#include <string>
#include <optional>

#include <ptscan.h>
#include <pts.h>
#include <pts/String.h>

namespace pts
{
class Scanner;

class ProcessHandle {
    friend class Scanner;
public:
    ProcessHandle(const ProcessHandle &) = delete;
    ~ProcessHandle();

    String pid();
    String name();

    /// Open a process handle from a pid.
    static std::shared_ptr<ProcessHandle> open(process_id pid);

private:
    ProcessHandle(pts_process_handle_t *inner);

    pts_process_handle_t *inner;
};
}

#endif // PTS_PROCESSHANDLE_H
