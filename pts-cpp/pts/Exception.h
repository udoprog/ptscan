#ifndef PTS_EXCEPTION_H
#define PTS_EXCEPTION_H

#include <string>
#include <vector>
#include <optional>
#include <cstdint>
#include <ptscan.h>

namespace pts {
// Get the last ptscan error as an exception.
std::exception last_exception();
}

#endif // PTS_EXCEPTION_H
