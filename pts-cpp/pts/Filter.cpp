#include <pts/Filter.h>
#include <pts/String.h>
#include <pts/Exception.h>

namespace pts
{
Filter::Filter(pts_filter_t *inner) :
    inner(inner)
{
}

Filter::Filter(Filter &&other) :
    inner(other.inner)
{
    other.inner = nullptr;
}

Filter::~Filter()
{
    if (inner) {
        pts_filter_free(inner);
        inner = nullptr;
    }
}

String Filter::display() const
{
    pts::String display;
    pts_filter_display(inner, display.ptr());
    return display;
}

std::shared_ptr<Filter> Filter::parse(const std::string &input)
{
    if (auto inner = pts_filter_parse(input.data(), input.size())) {
        return std::shared_ptr<Filter>(new Filter(inner));
    } else {
        throw last_exception();
    }
}
}