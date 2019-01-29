#ifndef PTS_VALUES_H
#define PTS_VALUES_H

#include <ptscan.h>
#include <pts/String.h>

namespace pts {
class Scan;

class Values
{
    friend class Scan;
public:
    Values(uintptr_t size);
    // NB: Do not copy to avoid sporadic de-allocations.
    Values(const Values&) = delete;
    Values(Values&&);
    ~Values();

    // The length of the collection.
    uintptr_t length() const;

    // Get the value at the given position as a string.
    String valueAt(uintptr_t pos) const;
private:
    pts_values_t *inner;
};
}

#endif // PTS_VALUES_H
