#include <pts/Address.h>
#include <pts/String.h>
#include <pts/ProcessHandle.h>

namespace pts
{
Address::Address() :
    inner()
{
}

String Address::display(const std::shared_ptr<ProcessHandle> &handle)
{
    pts_process_handle_t *p = nullptr;

    if (handle) {
        p = handle.get()->inner;
    }

    String display;
    pts_address_display(&inner, p, &display.inner);
    return display;
}

Address::Address(pts_address_t inner) :
    inner(inner)
{
}
}
