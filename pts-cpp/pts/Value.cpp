#include <pts/Value.h>

namespace pts
{
String Value::display() const
{
    String string;
    pts_value_display(&inner, &string.inner);
    return string;
}

Value::Value(pts_value_t inner) :
    inner(inner)
{
}
}
