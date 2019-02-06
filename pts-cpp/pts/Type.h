#ifndef PTS_TYPE_H
#define PTS_TYPE_H

#include <pts/String.h>
#include <ptscan.h>

namespace pts
{
class Values;
class Value;
class Filter;
class Watch;

class Type
{
    friend class Values;
    friend class Value;
    friend class Filter;
    friend class Watch;

    friend bool operator==(const Type &lhs, const Type& rhs) {
        return lhs.inner._0 == rhs.inner._0;
    }

public:
    Type();
    // Parse the given string into a type.
    static Type parse(const std::string &input);

    // Display the value as a string.
    String display() const;
    // Display as a human-readable string.
    String humanDisplay() const;
private:
    explicit Type(pts_type_t inner);
    pts_type_t inner;
};

inline Type::Type(pts_type_t inner) :
    inner(inner)
{
}

inline Type::Type() :
    inner(pts_type_t({0}))
{
}
}

#endif // PTS_TYPE_H
