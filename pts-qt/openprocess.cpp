#include <string>

#include <QStringListModel>
#include <QList>
#include <QDebug>
#include <pts/System.h>
#include <pts/ProcessHandle.h>

#include "openprocess.h"
#include "ui_openprocess.h"

OpenProcess::OpenProcess(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::OpenProcess),
    model(new QStandardItemModel())
{
    ui->setupUi(this);
    ui->list->setModel(this->model);

    QStringList headers;
    headers.push_back("PID");
    headers.push_back("Name");
    model->setHorizontalHeaderLabels(headers);

    connect(this, &QDialog::accepted, this, [this]() {
        auto index = ui->list->currentIndex();

        if (!index.isValid()) {
            return;
        }

        auto row = uintptr_t(index.row());
        selected = handles.at(row);
    });
}

OpenProcess::~OpenProcess()
{
    delete model;
    delete ui;
}

void OpenProcess::clearList()
{
    model->removeRows(0, model->rowCount());
    selected.reset();
    handles.clear();
}

void OpenProcess::refreshList()
{
    model->removeRows(0, model->rowCount());
    selected.reset();
    handles.clear();

    for (auto pid: pts::system::processes()) {
        if (auto handle = pts::ProcessHandle::open(pid)) {
            QList<QStandardItem *> row;
            row.push_back(new QStandardItem(QString::fromUtf8(handle->pid().toQByteArray())));
            row.push_back(new QStandardItem(QString::fromUtf8(handle->name().toQByteArray())));
            model->appendRow(row);
            handles.push_back(handle);
        }
    }
}

std::shared_ptr<pts::ProcessHandle> OpenProcess::takeSelected()
{
    auto selected = this->selected;
    this->selected.reset();
    return selected;
}
