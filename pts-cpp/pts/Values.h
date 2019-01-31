#ifndef PTS_VALUES_H
#define PTS_VALUES_H

#include <pts/String.h>
#include <ptscan.h>

namespace pts {
class Scan;

class Values
{
    friend class Scan;
public:
    // NB: Do not copy to avoid sporadic de-allocations.
    Values(const Values&) = delete;
    Values(Values&&);
    ~Values();

    // The length of the collection.
    uintptr_t length() const;

    // Get the value at the given position as a string.
    String valueAt(uintptr_t pos) const;
private:
    Values(pts_values_t *);

    pts_values_t *inner;
};
}

#endif // PTS_VALUES_H
