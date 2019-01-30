#include "editaddress.h"
#include "pointervalidator.h"
#include "ui_editaddress.h"
#include <pts/Watch.h>
#include <pts/Pointer.h>

EditAddress::EditAddress(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::EditAddress),
    index(),
    error()
{
    ui->setupUi(this);
    ui->error->hide();
    ui->pointer->setValidator(new PointerValidator(this));

    auto widths = EditAddress::widths();
    auto index = 0;

    for (auto const &width: widths) {
        ui->type->setItemText(index++, QString("%1-bit number").arg(width));
    }

    connect(ui->pointer, &QLineEdit::textChanged, this, [this](const QString& input) {
        std::string s = input.toUtf8().constData();

        try {
            pointer = pts::Pointer::parse(s);
            error = "";
            updateView();
        } catch(const std::exception &e) {
            pointer = {};
            error = e.what();
            updateView();
        }
    });
}

void EditAddress::updateView()
{
    ui->error->setText(error);
    ui->error->setVisible(!error.isEmpty());
}

std::vector<uintptr_t> EditAddress::widths()
{
    std::vector<uintptr_t> widths;
    widths.push_back(128);
    widths.push_back(64);
    widths.push_back(32);
    widths.push_back(16);
    widths.push_back(8);
    return widths;
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
}

std::shared_ptr<pts::Pointer> EditAddress::takePointer()
{
    auto pointer = this->pointer;
    this->pointer.reset();
    return pointer;
}

EditAddress::~EditAddress()
{
    delete ui;
}
