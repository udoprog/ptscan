#ifndef PTS_FILTER_H
#define PTS_FILTER_H

#include <memory>
#include <string>

#include <ptscan.h>

namespace pts
{
class Scanner;

class Filter
{
    friend class Scanner;

public:
    Filter(pts_filter_t *);
    ~Filter();

    // Return a string which is a human-readable display of this filter.
    std::string display() const;

    static std::shared_ptr<Filter> parse(const std::string &input);
private:
    // Inner reference to structure.
    pts_filter_t *inner;
};
}

#endif // PTS_FILTER_H
