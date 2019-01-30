#ifndef PTS_H
#define PTS_H

#include <string>
#include <vector>
#include <optional>
#include <cstdint>
#include <ptscan.h>

namespace pts {
// Get the last ptscan error as an exception.
std::exception last_exception();
// An opauq process identifier.
typedef pts_process_id_t *process_id;

namespace system {
std::vector<process_id> processes();
}
}

#endif // PTS_H
