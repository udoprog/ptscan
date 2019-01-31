#include <pts/Watch.h>
#include <pts/Pointer.h>
#include <pts/Exception.h>
#include <pts/Value.h>

namespace pts
{
Watch::Watch() :
    inner(nullptr)
{
}

Watch::Watch(pts_watch_t *inner) :
    inner(inner)
{
}

Watch::Watch(Watch &&other) :
    inner(other.inner)
{
    other.inner = nullptr;
}

Watch::~Watch()
{
    if (inner) {
        pts_watch_free(inner);
        inner = nullptr;
    }
}

std::shared_ptr<Pointer> Watch::pointer()
{
    pts_pointer_t *pointer = pts_watch_get_pointer(inner);

    if (!pointer) {
        throw last_exception();
    }

    return std::make_shared<Pointer>(Pointer(pointer));
}

Value Watch::value()
{
    return Value{pts_watch_value(inner)};
}

String Watch::type()
{
    String out;
    pts_watch_display_type(inner, &out.inner);
    return out;
}

void Watch::setPointer(const std::shared_ptr<Pointer> pointer)
{
    pts_watch_set_pointer(inner, pointer->inner);
}
}
