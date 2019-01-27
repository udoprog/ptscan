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

    connect(ui->input, &QLineEdit::textChanged, this, [this](const QString& input) {
        std::string s = input.toUtf8().constData();

        try {
            this->predicate = std::make_optional(pts::Filter::parse(s));
            this->error = {};
            this->stateChanged();
        } catch(const std::exception &e) {
            this->error = std::make_optional(e.what());
            this->stateChanged();
        }
    });
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
