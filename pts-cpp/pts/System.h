#ifndef PTS_SYSTEM_H
#define PTS_SYSTEM_H

#include <vector>

#include <ptscan.h>

namespace pts {
// An opauq process identifier.
typedef pts_process_id_t *process_id;

namespace system {
std::vector<process_id> processes();
}
}

#endif // PTS_SYSTEM_H
