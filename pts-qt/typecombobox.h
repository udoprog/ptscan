#ifndef TYPECOMBOBOX_H
#define TYPECOMBOBOX_H

#include <vector>

#include <QComboBox>
#include <QStandardItemModel>
#include <pts/Value.h>

// A specialized combobox implementation for selecting a type.
class TypeComboBox : public QComboBox
{
    Q_OBJECT

public:
    explicit TypeComboBox(QWidget *parent = nullptr);

    // Currently selected type.
    pts::Type currentType();

    static std::vector<pts::Type> allTypes();
signals:
    // Signal that a new type has been selected.
    void typeSelected(pts::Type type);

public slots:
    // Set the type for this box.
    void setType(pts::Type type);

private:
    std::vector<pts::Type> all;
    // Model for types.
    QStandardItemModel model;
};

#endif // TYPECOMBOBOX_H
