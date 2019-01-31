#ifndef PTS_VALUE_H
#define PTS_VALUE_H

#include <pts/String.h>
#include <ptscan.h>

namespace pts
{
class Values;
class Watch;

class Value
{
friend class Values;
friend class Watch;

public:
    Value() = delete;
    Value(const Value &) = default;
    Value(Value &&) = default;

    // Display the value as a string.
    String display() const;
private:
    explicit Value(pts_value_t inner);
    pts_value_t inner;
};
}

#endif // PTS_VALUE_H
