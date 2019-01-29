#include "scanresults.h"
#include "ui_scanresults.h"

#include <pts/Scan.h>

ScanResults::ScanResults(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::ScanResults)
{
    ui->setupUi(this);
    ui->list->setModel(&model);

    QStringList headers;
    headers.push_back("Address");
    headers.push_back("Last Scan");
    headers.push_back("Current");
    model.setHorizontalHeaderLabels(headers);
}

ScanResults::~ScanResults()
{
    delete ui;
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

void ScanResults::updateCurrent(const std::vector<pts::ScanResult> &results)
{
    int index = 0;

    for (auto const &result: results) {
        if (index >= model.rowCount()) {
            break;
        }

        auto row = index++;
        auto current = result.current().toQString();
        auto currentItem = model.item(row, 2);

        currentItem->setText(current);

        auto last = model.item(row, 1)->text();

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

            auto address = new QStandardItem(result.address(handle).toQString());
            address->setEditable(false);
            row.push_back(address);

            auto value = new QStandardItem(result.value().toQString());
            value->setEditable(false);
            row.push_back(value);

            auto current = new QStandardItem(result.current().toQString());
            value->setEditable(false);
            row.push_back(current);

            model.appendRow(row);
        }
    }
}
