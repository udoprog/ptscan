#include "addresslist.h"
#include "editaddress.h"
#include "ui_addresslist.h"
#include <pts/Watch.h>
#include <pts/Pointer.h>
#include <pts/Address.h>
#include <pts/ProcessHandle.h>
#include <QMenu>
#include <QDebug>
#include <memory>

AddressList::AddressList(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::AddressList),
    model(),
    watches(),
    editAddress(new EditAddress(this)),
    menu(new QMenu())
{
    ui->setupUi(this);
    ui->list->setModel(&model);

    QStringList headers;
    headers.push_back("Pointer");
    headers.push_back("Address");
    headers.push_back("Type");
    headers.push_back("Value");
    model.setHorizontalHeaderLabels(headers);

    ui->list->setContextMenuPolicy(Qt::CustomContextMenu);

    menu->addAction(ui->actionEdit);
    menu->addAction(ui->actionDelete);
    auto currentItem = std::make_shared<QModelIndex>(QModelIndex());

    connect(ui->list, &QAbstractItemView::doubleClicked, this,
            [this, currentItem](auto index)
    {
        *currentItem = index;
        ui->actionEdit->trigger();
        *currentItem = {};
    });

    connect(ui->list, &QAbstractItemView::customContextMenuRequested, this,
            [this, currentItem](const QPoint &pos)
    {
        auto index = ui->list->indexAt(pos);

        if (!index.isValid()) {
            return;
        }

        *currentItem = index;
        menu->exec(ui->list->viewport()->mapToGlobal(pos));
        *currentItem = {};
    });

    connect(ui->actionEdit, &QAction::triggered, this,
            [this, currentItem]()
    {
        auto index = *currentItem;

        if (!index.isValid()) {
            return;
        }

        editAddress->setIndex(index);
        editAddress->setWatch(watches.at(uintptr_t(index.row())));
        editAddress->show();
    });

    connect(ui->actionDelete, &QAction::triggered, this,
            [this, currentItem]()
    {
        auto index = *currentItem;

        if (index.isValid()) {
            model.removeRow(index.row());
            watches.erase(watches.begin() + index.row());
        }
    });

    connect(editAddress, &QDialog::accepted, this,
            [this]()
    {
        auto index = editAddress->takeIndex();

        if (!index.isValid()) {
            // TODO: support adding.
            return;
        }

        auto item = model.itemFromIndex(index);
        auto watch = watches.at(index.row());

        if (auto pointer = editAddress->takePointer()) {
            watch->setPointer(pointer);
            item->setText(pointer->display().toQString());
        }
    });
}

AddressList::~AddressList()
{
    delete ui;
    delete menu;
    delete editAddress;
}

void AddressList::addWatch(const std::shared_ptr<pts::ProcessHandle> &handle, std::shared_ptr<pts::Watch> watch)
{
    QList<QStandardItem *> row;

    auto p = watch->pointer();

    auto pointer = new QStandardItem(p->display().toQString());
    pointer->setEditable(false);
    row.push_back(pointer);

    auto address = pts::Address();

    if (handle) {
        if (auto a = handle->readPointer(p)) {
            address = *a;
        }
    }

    auto addressItem = new QStandardItem(address.display(handle).toQString());
    addressItem->setEditable(false);
    row.push_back(addressItem);

    auto type = new QStandardItem(watch->type().toQString());
    type->setEditable(false);
    row.push_back(type);

    auto value = new QStandardItem(watch->value().display().toQString());
    value->setEditable(false);
    row.push_back(value);

    model.appendRow(row);
    watches.push_back(watch);
}

AddressInfo AddressList::info(const std::shared_ptr<pts::ProcessHandle> &handle) const
{
    pts::Values values;
    pts::Addresses addresses;
    std::vector<uintptr_t> indexes;

    uintptr_t index = 0;

    if (handle) {
        for (auto const &v: watches) {
            auto p = v->pointer();

            if (auto a = handle->readPointer(p)) {
                addresses.push(*a);
                values.push(v->value());
                indexes.push_back(index);
            }

            index++;
        }
    }

    auto output = values.clone();

    return AddressInfo{
        std::make_shared<pts::Values>(std::move(values)),
        std::make_shared<pts::Values>(std::move(output)),
        std::make_shared<pts::Addresses>(std::move(addresses)),
        indexes
    };
}

void AddressList::updateCurrent(const AddressInfo &info)
{
    for (auto const &index: info.indexes) {
        if (index >= uintptr_t(model.rowCount())) {
            break;
        }

        auto value = info.output->at(index);

        if (!value) {
            continue;
        }

        auto v = *value;

        auto current = v.display().toQString();
        auto currentItem = model.item(int(index), 3);
        currentItem->setText(current);
    }
}
