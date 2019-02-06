#ifndef PTS_VALUE_H
#define PTS_VALUE_H

#include <pts/String.h>
#include <ptscan.h>

namespace pts
{
class Values;
class Watch;
class ScanResult;

class Value
{
    friend class Values;
    friend class Watch;
    friend class ScanResult;

public:
    Value();

    // String conveying the type of the value.
    Type type();

    // Display the value as a string.
    String display() const;
private:
    explicit Value(pts_value_t inner);
    pts_value_t inner;
};

inline Value::Value() :
    inner(pts_value_t({0}))
{
}

inline Value::Value(pts_value_t inner) :
    inner(inner)
{
}
}

#endif // PTS_VALUE_H
