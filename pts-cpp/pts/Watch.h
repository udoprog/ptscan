#ifndef PTS_WATCH_H
#define PTS_WATCH_H

#include <memory>

#include <pts/String.h>
#include <pts/Value.h>
#include <pts/Address.h>

namespace pts {
class ScanResult;
class Pointer;

class Watch
{
    friend class ScanResult;

public:
    Watch();
    Watch(const Watch &) = delete;
    Watch(Watch &&);
    ~Watch();

    // Get the pointer as a string.
    std::shared_ptr<Pointer> pointer();

    // Get the value of the watch as a string.
    Value value();

    // Get the type of the watch as a string.
    String type();

    void setPointer(const std::shared_ptr<Pointer> pointer);
private:
    Watch(pts_watch_t *inner);

    pts_watch_t *inner;
};
}

#endif // PTS_WATCH_H
