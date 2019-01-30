#include <pts/Values.h>

namespace pts {
Values::Values(uintptr_t size)
{
    inner = pts_values_new(size);
}

Values::Values(Values &&other) :
    inner(other.inner)
{
    other.inner = nullptr;
}

Values::~Values()
{
    if (inner) {
        pts_values_free(inner);
        inner = nullptr;
    }
}

uintptr_t Values::length() const
{
    return pts_values_length(inner);
}

String Values::valueAt(uintptr_t pos) const
{
    String value;
    pts_values_value_at(inner, pos, value.ptr());
    return value;
}
}
