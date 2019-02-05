#include <iostream>

#include <QComboBox>
#include <QDebug>
#include <QPushButton>

#include <pts/Filter.h>
#include <pts/Value.h>
#include "editfilter.h"
#include "typecombobox.h"
#include "ui_editfilter.h"

EditFilter::EditFilter(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::EditFilter),
    type(new TypeComboBox(this))
{
    ui->setupUi(this);
    ui->error->hide();
    ui->buttonBox->button(QDialogButtonBox::Ok)->setEnabled(false);
    ui->typeLayout->addWidget(type);

    connect(type, &TypeComboBox::typeSelected, this, &EditFilter::update);
    connect(ui->input, &QLineEdit::textChanged, this, &EditFilter::update);
}

std::shared_ptr<pts::Filter> EditFilter::takeFilter()
{
    auto filter = this->filter;
    this->filter.reset();
    return filter;
}

QModelIndex EditFilter::takeIndex()
{
    auto index = this->index;
    this->index = QModelIndex();
    return index;
}

void EditFilter::addFilter()
{
    filter = {};
    index = {};
    ui->input->setText("");
    ui->label->setText("Add Filter");
}

void EditFilter::editFilter(std::shared_ptr<pts::Filter> filter, QModelIndex index)
{
    this->filter = filter;
    this->index = index;
    ui->input->setText(filter->display().toQString());
    ui->label->setText("Edit Filter");
}

void EditFilter::update()
{
    std::string s = ui->input->text().toUtf8().constData();
    bool error = false;

    try {
        filter = pts::Filter::parse(s, type->currentType());
    } catch(const std::exception &e) {
        this->ui->error->setText(e.what());
        error = true;
    }

    ui->buttonBox->button(QDialogButtonBox::Ok)->setEnabled(!error);
    ui->error->setVisible(!!error);
}

EditFilter::~EditFilter()
{
    delete ui;
}
