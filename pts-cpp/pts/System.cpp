#include <pts/ProcessHandle.h>
#include <pts/String.h>
#include <pts/Exception.h>
#include <pts/System.h>

namespace pts {
namespace system {
std::vector<process_id> processes()
{
    std::vector<process_id> out;

    pts_system_processes_iter_t *iter = nullptr;

    if (!(iter = pts_system_processes_iter())) {
        throw pts::last_exception();
    }

    process_id p;

    while ((p = pts_system_processes_next(iter))) {
        out.push_back(p);
    }

    pts_system_processes_free(iter);
    return out;
}
}
}
