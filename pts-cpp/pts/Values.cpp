#include <pts/Values.h>
#include <pts/Value.h>

namespace pts {
Values::Values() :
    inner(pts_values_new())
{
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

std::optional<Value> Values::at(uintptr_t pos) const
{
    pts_value_t value;

    if (pts_values_at(inner, pos, &value)) {
        return std::make_optional(Value{value});
    }

    return {};
}

void Values::push(Value value)
{
    pts_values_push(inner, value.inner);
}

void Values::pushType(Type type)
{
    pts_values_push_type(inner, type.inner);
}

Values Values::clone()
{
    return Values{pts_values_clone(inner)};
}

Values::Values(pts_values_t *inner) :
    inner(inner)
{
}
}
