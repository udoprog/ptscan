#ifndef ADDRESSLIST_H
#define ADDRESSLIST_H

#include <QWidget>

namespace Ui {
class AddressList;
}

class AddressList : public QWidget
{
    Q_OBJECT

public:
    explicit AddressList(QWidget *parent = nullptr);
    ~AddressList();

private:
    Ui::AddressList *ui;
};

#endif // ADDRESSLIST_H
