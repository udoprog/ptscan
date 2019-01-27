#include <pts.h>
#include <pts/ProcessHandle.h>
#include <pts/String.h>

namespace pts {
std::exception last_exception()
{
   auto error = pts_error_last();

   if (!error) {
       throw std::exception("no last error");
   }

   String message;
   pts_error_message(error, message.ptr());
   return std::exception(message.string().c_str());
}

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
