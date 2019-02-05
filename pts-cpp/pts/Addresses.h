#ifndef PTS_ADDRESSES_H
#define PTS_ADDRESSES_H

#include <optional>

#include <pts/String.h>
#include <pts/Address.h>
#include <ptscan.h>

namespace pts {
class ProcessHandle;

class Addresses
{
friend class ProcessHandle;

public:
    Addresses();
    // NB: Do not copy to avoid sporadic de-allocations.
    Addresses(const Addresses&) = delete;
    Addresses(Addresses&&);
    ~Addresses();

    // The length of the collection.
    uintptr_t length() const;

    // Get the value at the given position as a string.
    std::optional<Address> at(uintptr_t pos) const;

    // Push a value onto the collection.
    void push(Address value);

private:
    explicit Addresses(pts_addresses_t *);

    pts_addresses_t *inner;
};
}

#endif // PTS_ADDRESSES_H
