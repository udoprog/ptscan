#include "Watch.h"

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

String Watch::display()
{
    String out;
    pts_watch_display(inner, out.ptr());
    return out;
}
}
