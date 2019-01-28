#include <iostream>

#include <QDebug>
#include <QPushButton>

#include <pts/Filter.h>
#include "addfilter.h"
#include "ui_addfilter.h"

AddFilter::AddFilter(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::AddFilter)
{
    ui->setupUi(this);
    ui->error->hide();
    ui->buttonBox->button(QDialogButtonBox::Ok)->setEnabled(false);
    ui->addFilterLabel->hide();
    ui->editFilterLabel->hide();

    connect(ui->input, &QLineEdit::textChanged, this, [this](const QString& input) {
        std::string s = input.toUtf8().constData();

        try {
            this->filter = pts::Filter::parse(s);
            this->error = {};
            this->stateChanged();
        } catch(const std::exception &e) {
            this->error = std::make_optional(e.what());
            this->stateChanged();
        }
    });
}

std::shared_ptr<pts::Filter> AddFilter::takeFilter()
{
    auto filter = this->filter;
    this->filter.reset();
    return filter;
}

QModelIndex AddFilter::takeIndex()
{
    auto index = this->index;
    this->index = QModelIndex();
    return index;
}

void AddFilter::addFilter()
{
    this->filter = {};
    this->index = {};
    this->ui->input->setText("");
    this->ui->addFilterLabel->show();
    this->ui->editFilterLabel->hide();
}

void AddFilter::editFilter(std::shared_ptr<pts::Filter> filter, QModelIndex index)
{
    this->filter = filter;
    this->index = index;
    this->ui->input->setText(filter->display().toQString());
    this->ui->addFilterLabel->hide();
    this->ui->editFilterLabel->show();
}

void AddFilter::stateChanged()
{
    if (auto error = this->error) {
        this->ui->error->setText(QString::fromStdString(*error));
        this->ui->error->show();
        ui->buttonBox->button(QDialogButtonBox::Ok)->setEnabled(false);
    } else {
        this->ui->error->hide();
        ui->buttonBox->button(QDialogButtonBox::Ok)->setEnabled(true);
    }
}

AddFilter::~AddFilter()
{
    delete ui;
}
