#ifndef PTS_STRING_H
#define PTS_STRING_H

#include <string>

#include <ptscan.h>

namespace pts {
class String {
public:
    ~String();

    pts_string_t *ptr();
    std::string string();
private:
    pts_string_t inner;
};
};

#endif // PTS_STRING_H
