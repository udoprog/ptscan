#include "scanresults.h"
#include "ui_scanresults.h"
#include <pts/Scan.h>
#include <pts/Values.h>
#include <pts/Address.h>
#include <QDebug>

ScanResults::ScanResults(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::ScanResults)
{
    ui->setupUi(this);
    ui->list->setModel(&model);

    QStringList headers;
    headers.push_back("Address");
    headers.push_back("Type");
    headers.push_back("Last Scan");
    headers.push_back("Current");
    model.setHorizontalHeaderLabels(headers);

    connect(ui->list, &QTreeView::doubleClicked, this, [this](auto index) {
        emit addWatch(index);
    });
}

ScanResults::~ScanResults()
{
    delete ui;
}

void ScanResults::setIntermediateCount(uint64_t count)
{
    ui->count->setText(QString("%1 (in progress)").arg(count));
}

void ScanResults::setCount(std::optional<uintptr_t> count)
{
    if (count) {
        auto c = *count;

        if (c == 0) {
            ui->count->setText("no results");
        } else if (c <= 1) {
            ui->count->setText(QString("%1 result").arg(c));
        } else {
            ui->count->setText(QString("%1 results").arg(c));
        }
    } else {
        ui->count->setText("no scan in progress");
    }
}

void ScanResults::updateCurrent(const std::shared_ptr<pts::Values> &values)
{
    auto length = int(values->length());

    for (int index = 0; index < length; index++) {
        if (index >= model.rowCount()) {
            break;
        }

        auto value = values->at(uintptr_t(index));

        if (!value) {
            continue;
        }

        auto v = *value;

        auto current = v.display().toQString();
        auto currentItem = model.item(index, 3);

        currentItem->setText(current);
        currentItem->setEditable(false);

        auto last = model.item(index, 2)->text();

        if (current != last) {
            currentItem->setForeground(Qt::red);
        } else {
            currentItem->setForeground(Qt::black);
        }
    }
}

void ScanResults::update(const std::shared_ptr<pts::ProcessHandle> &handle, std::optional<std::vector<pts::ScanResult>> results)
{
    model.removeRows(0, model.rowCount());

    if (results) {
        for (auto const &result: *results) {
            QList<QStandardItem *> row;

            auto address = new QStandardItem(result.address().display(handle).toQString());
            address->setEditable(false);
            row.push_back(address);

            auto v = result.value();

            auto type = new QStandardItem(v.type().toQString());
            type->setEditable(false);
            row.push_back(type);

            auto value = new QStandardItem(v.display().toQString());
            value->setEditable(false);
            row.push_back(value);

            auto current = new QStandardItem(value->text());
            value->setEditable(false);
            row.push_back(current);

            model.appendRow(row);
        }
    }
}
