#include <pts/Filter.h>
#include <pts/String.h>
#include <pts/Exception.h>
#include <pts/Type.h>

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

Type Filter::type() const
{
    return Type{pts_filter_type(inner)};
}

String Filter::display() const
{
    pts::String display;
    pts_filter_display(inner, &display.inner);
    return display;
}

std::shared_ptr<Filter> Filter::parse(const std::string &input, const Type &type)
{
    if (auto inner = pts_filter_parse(input.data(), input.size(), type.inner)) {
        return std::shared_ptr<Filter>(new Filter(inner));
    }

    throw last_exception();
}
}
