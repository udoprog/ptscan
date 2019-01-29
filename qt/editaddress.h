#ifndef EDITADDRESS_H
#define EDITADDRESS_H

#include <QDialog>
#include <QModelIndex>

namespace pts {
class Watch;
class Pointer;
}

namespace Ui {
class EditAddress;
}

class EditAddress : public QDialog
{
    Q_OBJECT

public:
    explicit EditAddress(QWidget *parent = nullptr);
    ~EditAddress();

    static std::vector<uintptr_t> widths();
    void setWatch(std::shared_ptr<pts::Watch> watch);
    // Set the index being edited (if any).
    void setIndex(QModelIndex index);
    // Take the index being edited.
    QModelIndex takeIndex();
    // Take the updated pointer.
    std::shared_ptr<pts::Pointer> takePointer();
private:
    Ui::EditAddress *ui;
    QModelIndex index;
    std::shared_ptr<pts::Pointer> pointer;
    QString error;
    // Update view to reflect internal model.
    void updateView();
};

#endif // EDITADDRESS_H
