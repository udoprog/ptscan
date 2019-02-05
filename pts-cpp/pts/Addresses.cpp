#include <pts/Addresses.h>
#include <pts/Address.h>

namespace pts {
Addresses::Addresses() :
    inner(pts_addresses_new())
{
}

Addresses::Addresses(Addresses &&other) :
    inner(other.inner)
{
    other.inner = nullptr;
}

Addresses::~Addresses()
{
    if (inner) {
        pts_addresses_free(inner);
        inner = nullptr;
    }
}

uintptr_t Addresses::length() const
{
    return pts_addresses_length(inner);
}

std::optional<Address> Addresses::at(uintptr_t pos) const
{
    pts_address_t address;

    if (pts_addresses_at(inner, pos, &address)) {
        return std::make_optional(Address{address});
    }

    return {};
}

void Addresses::push(Address value)
{
    pts_addresses_push(inner, value.inner);
}

Addresses::Addresses(pts_addresses_t *inner) :
    inner(inner)
{
}
}
