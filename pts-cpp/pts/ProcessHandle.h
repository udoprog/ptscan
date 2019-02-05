#ifndef PTS_PROCESSHANDLE_H
#define PTS_PROCESSHANDLE_H

#include <string>
#include <optional>

#include <pts/String.h>
#include <pts/System.h>
#include <pts/Pointer.h>
#include <ptscan.h>

namespace pts
{
class Scan;
class ScanResult;
class Address;
class ThreadPool;
class Addresses;
class Values;
class Token;
class ScanReporter;

class ProcessHandle {
    friend class Address;
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

    // Get the process ID of the process.
    String pid();
    // Get the name of the process.
    String name();
    // Refresh known threads about the process.
    void refreshThreads();
    // Refresh known modules about the process.
    void refreshModules();
    // Read the address of a pointer.
    std::optional<Address> readPointer(const std::shared_ptr<Pointer>& pointer);
    // Read a segment of memory.
    void readMemory(
        const std::shared_ptr<ThreadPool> &threadPool,
        const std::shared_ptr<Addresses> &addresses,
        const std::shared_ptr<Values> &values,
        std::shared_ptr<Values> &output,
        std::shared_ptr<Token> &cancel,
        ScanReporter &reporter
    );

private:
    pts_process_handle_t *inner;
};
}

#endif // PTS_PROCESSHANDLE_H
