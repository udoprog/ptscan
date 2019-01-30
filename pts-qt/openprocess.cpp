#include <string>

#include <QStringListModel>
#include <QList>
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

    connect(ui->list, &QAbstractItemView::clicked, this, &OpenProcess::clicked);
}

OpenProcess::~OpenProcess()
{
    delete model;
    delete ui;
}

void OpenProcess::clicked(const QModelIndex &index)
{
    selected = handles.at(index.row());
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
