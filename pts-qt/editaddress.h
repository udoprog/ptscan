#ifndef EDITADDRESS_H
#define EDITADDRESS_H

#include <QDialog>
#include <QModelIndex>
#include <pts/Value.h>

namespace pts {
class Watch;
class Pointer;
}

namespace Ui {
class EditAddress;
}

class TypeComboBox;

class EditAddress : public QDialog
{
    Q_OBJECT

public:
    explicit EditAddress(QWidget *parent = nullptr);
    ~EditAddress();

    // Set a watch.
    void setWatch(std::shared_ptr<pts::Watch> watch);
    // Set the index being edited (if any).
    void setIndex(QModelIndex index);
    // Take the index being edited.
    QModelIndex takeIndex();
    // Take the updated pointer.
    std::shared_ptr<pts::Pointer> takePointer();
    // Take type that has been set.
    pts::Type takeType();

signals:
    // Update view to reflect internal model.
    void update();

private:
    Ui::EditAddress *ui;
    TypeComboBox *type;
    QModelIndex index;
    std::shared_ptr<pts::Pointer> pointer;
    QString error;
};

#endif // EDITADDRESS_H
