#ifndef POINTERVALIDATOR_H
#define POINTERVALIDATOR_H

#include <QValidator>

class PointerValidator : public QValidator
{
    Q_OBJECT

public:
    explicit PointerValidator(QObject *parent = nullptr);
    QValidator::State validate(QString &input, int &) const;
};

class FilterValidator : public QValidator
{
    Q_OBJECT

public:
    explicit FilterValidator(QObject *parent = nullptr);
    QValidator::State validate(QString &input, int &) const;
};

#endif // POINTERVALIDATOR_H
