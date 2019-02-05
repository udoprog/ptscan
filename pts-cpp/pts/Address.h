#ifndef PTS_ADDRESS_H
#define PTS_ADDRESS_H

#include <pts/String.h>
#include <ptscan.h>

namespace pts
{
class ProcessHandle;
class Watch;
class ScanResult;
class ProcessHandle;
class Addresses;

class Address
{
    friend class Watch;
    friend class ScanResult;
    friend class ProcessHandle;
    friend class Addresses;

public:
    Address();

    // Display the address as a string.
    String display(const std::shared_ptr<ProcessHandle> &handle);

private:
    explicit Address(pts_address_t inner);

    pts_address_t inner;
};
}

#endif // PTS_ADDRESS_H
