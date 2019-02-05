#ifndef ADDRESSLIST_H
#define ADDRESSLIST_H

#include <pts/Values.h>
#include <pts/Addresses.h>
#include <QStandardItemModel>
#include <QWidget>

class QMenu;

class EditAddress;

namespace pts
{
class Watch;
class ProcessHandle;
}

struct AddressInfo
{
    std::shared_ptr<pts::Values> existing;
    std::shared_ptr<pts::Values> output;
    std::shared_ptr<pts::Addresses> addresses;
    std::vector<uintptr_t> indexes;
};

namespace Ui {
class AddressList;
}

class AddressList : public QWidget
{
    Q_OBJECT

public:
    explicit AddressList(QWidget *parent = nullptr);
    ~AddressList();

    // Add the given watch to the address list.
    void addWatch(const std::shared_ptr<pts::ProcessHandle> &handle, std::shared_ptr<pts::Watch> watch);

    // Get a collection of values corresponding to all watches.
    AddressInfo info(const std::shared_ptr<pts::ProcessHandle> &handle) const;

    // Update the current values in address list.
    void updateCurrent(const AddressInfo &info);
private:
    Ui::AddressList *ui;
    QStandardItemModel model;
    std::vector<std::shared_ptr<pts::Watch>> watches;
    EditAddress *editAddress;
    QMenu *menu;
};

#endif // ADDRESSLIST_H
