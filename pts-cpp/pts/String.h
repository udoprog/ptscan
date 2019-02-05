#ifndef PTS_STRING_H
#define PTS_STRING_H

#include <string>

#ifdef PTS_QT
#include <QByteArray>
#include <QString>
#endif // PTS_QT

#include <ptscan.h>

namespace pts {
class Filter;
class Pointer;
class ProcessHandle;
class Value;
class ScanResult;
class Watch;
class Address;
class Type;

class String {
    friend class Filter;
    friend class Pointer;
    friend class ProcessHandle;
    friend class Value;
    friend class ScanResult;
    friend class Watch;
    friend class Address;
    friend class Type;

    friend std::exception last_exception();

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
private:
    pts_string_t inner;
};
};

#endif // PTS_STRING_H
