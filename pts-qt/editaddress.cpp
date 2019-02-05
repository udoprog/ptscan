#include "editaddress.h"
#include "typecombobox.h"
#include "ui_editaddress.h"
#include <pts/Watch.h>
#include <pts/Pointer.h>

EditAddress::EditAddress(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::EditAddress),
    type(new TypeComboBox(this))
{
    ui->setupUi(this);
    ui->error->hide();
    ui->typeLayout->addWidget(type);

    connect(type, &TypeComboBox::typeSelected, this, &EditAddress::update);

    connect(ui->pointer, &QLineEdit::textChanged, this, [this](const QString& input) {
        std::string s = input.toUtf8().constData();

        try {
            pointer = pts::Pointer::parse(s);
            error = "";
        } catch(const std::exception &e) {
            pointer = {};
            error = e.what();
        }

        emit update();
    });

    connect(this, &EditAddress::update, this, [this]() {
        ui->error->setText(error);
        ui->error->setVisible(!error.isEmpty());
    });
}

void EditAddress::setIndex(QModelIndex index)
{
    this->index = index;
}

QModelIndex EditAddress::takeIndex()
{
    auto index = this->index;
    this->index = {};
    return index;
}

void EditAddress::setWatch(std::shared_ptr<pts::Watch> watch)
{
    error = "";
    ui->pointer->setText(watch->pointer()->display().toQString());
    type->setType(watch->type());
}

std::shared_ptr<pts::Pointer> EditAddress::takePointer()
{
    auto pointer = this->pointer;
    this->pointer.reset();
    return pointer;
}

pts::Type EditAddress::takeType()
{
    return type->currentType();
}

EditAddress::~EditAddress()
{
    delete ui;
    delete type;
}
