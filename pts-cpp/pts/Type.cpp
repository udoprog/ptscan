#include <pts/Type.h>
#include <pts/String.h>

namespace pts
{
Type Type::parse(const std::string &input)
{
    return Type{pts_type_parse(input.data(), input.length())};
}

String Type::display() const
{
    String string;
    pts_type_display(&inner, &string.inner);
    return string;
}

String Type::humanDisplay() const
{
    String string;
    pts_type_human_display(&inner, &string.inner);
    return string;
}
}
