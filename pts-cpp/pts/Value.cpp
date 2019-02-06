#include <pts/Value.h>
#include <pts/Type.h>

namespace pts
{
Type Value::type()
{
    return Type{pts_value_type(&inner)};
}

String Value::display() const
{
    String string;
    pts_value_display(&inner, &string.inner);
    return string;
}
}
