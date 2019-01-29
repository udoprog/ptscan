#ifndef PTS_PROCESSHANDLE_H
#define PTS_PROCESSHANDLE_H

#include <string>
#include <optional>

#include <ptscan.h>
#include <pts.h>
#include <pts/String.h>

namespace pts
{
class Scan;
class ScanResult;

class ProcessHandle {
    friend class Scan;
    friend class ScanResult;

public:
    ProcessHandle();
    ProcessHandle(pts_process_handle_t *inner);
    /// Kill copy constructor to avoid copying the interior value.
    ProcessHandle(const ProcessHandle &) = delete;
    ProcessHandle(ProcessHandle &&);
    ~ProcessHandle();

    /// Open a process handle from a pid.
    static std::shared_ptr<ProcessHandle> open(process_id pid);

    /// Get the process ID of the process.
    String pid();
    /// Get the name of the process.
    String name();
    /// Refresh known threads about the process.
    void refreshThreads();
    /// Refresh known modules about the process.
    void refreshModules();

private:
    pts_process_handle_t *inner;
};
}

#endif // PTS_PROCESSHANDLE_H
