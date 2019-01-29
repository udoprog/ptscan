#include "addresslist.h"
#include "ui_addresslist.h"
#include <pts/Watch.h>

AddressList::AddressList(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::AddressList)
{
    ui->setupUi(this);
    ui->list->setModel(&model);
}

AddressList::~AddressList()
{
    delete ui;
}

void AddressList::addWatch(std::shared_ptr<pts::Watch> watch)
{
    QList<QStandardItem *> row;
    row.push_back(new QStandardItem(watch->display().toQString()));
    model.appendRow(row);
}
