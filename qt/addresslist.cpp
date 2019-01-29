#include "addresslist.h"
#include "ui_addresslist.h"

AddressList::AddressList(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::AddressList)
{
    ui->setupUi(this);
}

AddressList::~AddressList()
{
    delete ui;
}
