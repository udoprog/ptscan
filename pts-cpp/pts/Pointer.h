#ifndef PTS_POINTER_H
#define PTS_POINTER_H

#include <memory>
#include <string>

#include <pts/String.h>
#include <ptscan.h>

namespace pts
{
class Watch;
class ProcessHandle;

class Pointer
{
    friend class Watch;
    friend class ProcessHandle;

public:
    Pointer(const Pointer&) = delete;
    Pointer(Pointer&&);
    ~Pointer();

    // Return a string which is a human-readable display of this pointer.
    String display() const;

    static std::shared_ptr<Pointer> parse(const std::string &input);
private:
    Pointer(pts_pointer_t *);
    // Inner reference to structure.
    pts_pointer_t *inner;
};
}

#endif // PTS_POINTER_H
