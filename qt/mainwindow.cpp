#include <iostream>

#include <QDebug>
#include <QApplication>
#include <QDesktopWidget>
#include <QCheckBox>
#include <QtConcurrent>
#include <pts.h>
#include <pts/Scanner.h>

#include "mainwindow.h"
#include "openprocess.h"
#include "addfilter.h"
#include "ui_mainwindow.h"

MainWindow::MainWindow(std::shared_ptr<pts::ThreadPool> threadPool, QWidget *parent) :
    QMainWindow(parent),
    threadPool(threadPool),
    ui(new Ui::MainWindow),
    openProcess(new OpenProcess(this)),
    addFilter(new AddFilter(this)),
    filtersModel(new QStandardItemModel())
{
    ui->setupUi(this);

    ui->filtersList->addAction(ui->actionRemoveFilter);
    ui->filtersList->setModel(filtersModel);

    connect(ui->filtersList, &QAbstractItemView::doubleClicked, this, [this](auto index) {
        if (!index.isValid()) {
            return;
        }

        if (!processHandle) {
            return;
        }

        auto filter = this->filters.at(index.row());

        auto processHandle = this->processHandle;
        auto threadPool = this->threadPool;

        QtConcurrent::run([threadPool, processHandle, filter]() {
            pts::ScanReporter reporter;

            reporter.report = [](int percentage){
                std::cout << percentage << std::endl;
            };

            reporter.done = [](bool interrupted){
                std::cout << interrupted << std::endl;
            };

            auto scanner = pts::Scanner::create(threadPool);
            scanner->scan(*processHandle, *filter, reporter);
        });
    });

    ui->actionRemoveFilter->setEnabled(this->filtersModel->rowCount() > 0);

    connect(ui->actionAttach, &QAction::triggered, this, [this]() {
        openProcess->refreshList();
        openProcess->show();
    });

    connect(ui->actionDetach, &QAction::triggered, this, [this]() {
        this->processHandle = {};
        this->updateView();
    });

    addAction(ui->actionAddFilter);
    addAction(ui->actionRemoveFilter);

    connect(ui->actionAddFilter, &QAction::triggered, this, [this]() {
        addFilter->show();
    });

    connect(ui->actionRemoveFilter, &QAction::triggered, this, [this]() {
        auto index = this->ui->filtersList->currentIndex();

        if (!index.isValid()) {
            return;
        }

        this->filters.erase(this->filters.begin() + index.row());
        this->filtersModel->removeRow(index.row());

        ui->actionRemoveFilter->setEnabled(this->filtersModel->rowCount() > 0);
    });

    connect(ui->addFilterButton, &QPushButton::pressed, ui->actionAddFilter, &QAction::trigger);

    connect(openProcess, &QDialog::accepted, this, [this]() {
        if (!openProcess->selected) {
            return;
        }

        this->processHandle = openProcess->selected;
        openProcess->selected = {};
        this->updateView();
    });

    connect(addFilter, &QDialog::accepted, this, [this]() {
        if (!addFilter->predicate) {
            return;
        }

        auto p = *addFilter->predicate;

        auto item = new QStandardItem(QString::fromStdString(p->display()));
        item->setEditable(false);

        filtersModel->appendRow(item);
        filters.push_back(p);

        ui->actionRemoveFilter->setEnabled(this->filtersModel->rowCount() > 0);
    });

    this->updateView();
}

void MainWindow::updateView()
{
    this->ui->actionDetach->setEnabled(!!this->processHandle);

    if (this->processHandle) {
        auto name = this->processHandle->name().toQString();

        if (name.isEmpty()) {
            name = "*unknown*";
        }

        auto info = QString("Attached to %1 (pid: %2)")
            .arg(name)
            .arg(this->processHandle->pid().toQString());

        ui->processInfo->setText(info);
    } else {
        ui->processInfo->setText("Not attached");
    }
}

MainWindow::~MainWindow()
{
    delete openProcess;
    delete ui;
}
