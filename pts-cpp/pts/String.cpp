#include <pts/String.h>

namespace pts
{
String::String() : inner({nullptr, 0, 0})
{
}

String::String(String&& other) : inner(other.inner)
{
    other.inner.ptr = nullptr;
}

#ifdef PTS_QT
QByteArray String::toQByteArray()
{
    return QByteArray(inner.ptr, int(inner.len));
}

QString String::toQString()
{
    return QString::fromUtf8(toQByteArray());
}
#endif // PTS_QT

std::string String::string()
{
    return std::string(inner.ptr, inner.len);
}

pts_string_t *String::ptr()
{
    return &inner;
}

String::~String()
{
    if (inner.ptr) {
        pts_string_free(&inner);
        inner.ptr = nullptr;
    }
}
}
