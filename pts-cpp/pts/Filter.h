#ifndef PTS_FILTER_H
#define PTS_FILTER_H

#include <memory>
#include <string>

#include <pts/String.h>
#include <pts/Value.h>
#include <ptscan.h>

namespace pts
{
class Scan;

class Filter
{
    friend class Scan;

public:
    Filter(pts_filter_t *);
    Filter(const Filter&) = delete;
    Filter(Filter&&);
    ~Filter();

    // Return the type of a filter.
    Type type() const;

    // Return a string which is a human-readable display of this filter.
    String display() const;

    static std::shared_ptr<Filter> parse(const std::string &input, const Type &type);
private:
    // Inner reference to structure.
    pts_filter_t *inner;
};
}

#endif // PTS_FILTER_H
