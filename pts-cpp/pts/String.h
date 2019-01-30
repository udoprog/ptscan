#ifndef PTS_STRING_H
#define PTS_STRING_H

#include <string>

#ifdef PTS_QT
#include <QByteArray>
#include <QString>
#endif // PTS_QT

#include <ptscan.h>

namespace pts {
class String {
public:
    // Construct a default empty string.
    String();
    String(const String &) = delete;
    String(String &&other);
    ~String();

#ifdef PTS_QT
    // Convert to a QByteArray.
    // This is zero-copy, the QByteArray does not take ownership of the string.
    QByteArray toQByteArray();

    // Convert into a QString.
    QString toQString();
#endif // PTS_QT

    // Copies the string into a std::string.
    std::string string();

    // Access the raw underlying pointer to interface with the C-api.
    pts_string_t *ptr();
private:
    pts_string_t inner;
};
};

#endif // PTS_STRING_H
