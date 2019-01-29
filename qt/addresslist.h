#ifndef ADDRESSLIST_H
#define ADDRESSLIST_H

#include <QStandardItemModel>
#include <QWidget>

namespace pts
{
class Watch;
}

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
    void addWatch(std::shared_ptr<pts::Watch> watch);

private:
    Ui::AddressList *ui;
    QStandardItemModel model;
    std::vector<std::shared_ptr<pts::Watch>> watches;
};

#endif // ADDRESSLIST_H
