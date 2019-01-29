#include <pts/Pointer.h>
#include <pts/String.h>
#include <pts.h>

namespace pts
{
Pointer::Pointer(pts_pointer_t *inner) :
    inner(inner)
{
}

Pointer::Pointer(Pointer &&other) :
    inner(other.inner)
{
    other.inner = nullptr;
}

Pointer::~Pointer()
{
    if (inner) {
        pts_pointer_free(inner);
    }
}

String Pointer::display() const
{
    pts::String display;
    pts_pointer_display(inner, display.ptr());
    return display;
}

std::shared_ptr<Pointer> Pointer::parse(const std::string &input)
{
    if (auto inner = pts_pointer_parse(input.data(), input.size())) {
        return std::shared_ptr<Pointer>(new Pointer(inner));
    } else {
        throw last_exception();
    }
}
}
