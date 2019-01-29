#include "pts/Watch.h"
#include "pts/Pointer.h"
#include "pts.h"

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

String Watch::value()
{
    String out;
    pts_watch_display_value(inner, out.ptr());
    return out;
}

String Watch::type()
{
    String out;
    pts_watch_display_type(inner, out.ptr());
    return out;
}

void Watch::setPointer(const std::shared_ptr<Pointer> pointer)
{
    pts_watch_set_pointer(inner, pointer->inner);
}
}
