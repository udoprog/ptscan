#include <pts/String.h>

namespace pts
{
pts_string_t *String::ptr()
{
    return &inner;
}

std::string String::string()
{
    return std::string(inner.ptr, inner.len);
}

String::~String()
{
    pts_string_free(&inner);
}
}
