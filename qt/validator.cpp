#include "validator.h"
#include <pts/Pointer.h>
#include <pts/Filter.h>

PointerValidator::PointerValidator(QObject *parent) :
    QValidator(parent)
{

}

QValidator::State PointerValidator::validate(QString &input, int&) const
{
    auto d = input.toUtf8().constData();

    try {
        pts::Pointer::parse(d);
        return QValidator::Acceptable;
    } catch(const std::exception &e) {
        return QValidator::Invalid;
    }
}

FilterValidator::FilterValidator(QObject *parent) :
    QValidator(parent)
{

}

QValidator::State FilterValidator::validate(QString &input, int&) const
{
    auto d = input.toUtf8().constData();

    try {
        pts::Filter::parse(d);
        return QValidator::Acceptable;
    } catch(const std::exception &e) {
        return QValidator::Invalid;
    }
}
