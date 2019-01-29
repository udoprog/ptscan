#ifndef PTS_WATCH_H
#define PTS_WATCH_H

#include <pts/String.h>

namespace pts {
class ScanResult;

class Watch
{
    friend class ScanResult;

public:
    Watch();
    Watch(const Watch &) = delete;
    Watch(Watch &&);
    ~Watch();

    String display();
private:
    Watch(pts_watch_t *inner);

    pts_watch_t *inner;
};
}

#endif // PTS_WATCH_H
