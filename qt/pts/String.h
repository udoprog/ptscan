#ifndef PTS_STRING_H
#define PTS_STRING_H

#include <string>

#include <QByteArray>
#include <QString>
#include <ptscan.h>

namespace pts {
class String {
public:
    // Construct a default empty string.
    String();
    String(const String &) = delete;
    String(String &&other);
    ~String();

    // Convert to a QByteArray.
    // This is zero-copy, the QByteArray does not take ownership of the string.
    QByteArray toQByteArray();

    // Convert into a QString.
    QString toQString();

    // Copies the string into a std::string.
    std::string string();

    // Access the raw underlying pointer to interface with the C-api.
    pts_string_t *ptr();
private:
    pts_string_t inner;
};
};

#endif // PTS_STRING_H
