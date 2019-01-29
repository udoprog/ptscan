#include <iostream>

#include <QDebug>
#include <QApplication>
#include <QDesktopWidget>
#include <QCheckBox>
#include <QtConcurrent>
#include <QTimer>
#include <pts.h>
#include <pts/Scan.h>
#include <pts/Token.h>

#include "mainwindow.h"
#include "openprocess.h"
#include "addfilter.h"
#include "ui_mainwindow.h"
#include "addresslist.h"
#include "scanresults.h"

MainWindow::MainWindow(std::shared_ptr<pts::ThreadPool> threadPool, QWidget *parent) :
    QMainWindow(parent),
    threadPool(threadPool),
    ui(new Ui::MainWindow),
    openProcess(new OpenProcess(this)),
    addFilter(new AddFilter(this)),
    wantsScan(false),
    refreshTimer(new QTimer())
{
    ui->setupUi(this);
    addressList = new AddressList(ui->addressListBox);
    ui->addressListBoxLayout->addWidget(addressList);

    scanResults = new ScanResults(ui->scanResultsBox);
    ui->scanResultsBoxLayout->addWidget(scanResults);

    refreshTimer->start(100);

    ui->filtersList->setModel(&filtersModel);
    ui->errorBox->setVisible(false);

    filtersContextMenu = new QMenu(ui->filtersList);
    filtersContextMenu->addAction(ui->actionRemoveFilter);
    ui->filtersList->setContextMenuPolicy(Qt::CustomContextMenu);

    connect(ui->filtersList, &QAbstractItemView::customContextMenuRequested, this, [this](const QPoint &pos) {
        auto index = ui->filtersList->indexAt(pos);

        if (!index.isValid()) {
            return;
        }

        filtersCurrentIndex = index;
        filtersContextMenu->exec(ui->filtersList->viewport()->mapToGlobal(pos));
        filtersCurrentIndex = {};
    });

    connect(ui->actionRemoveFilter, &QAction::triggered, this, [this]() {
        auto index = filtersCurrentIndex;

        if (!index.isValid()) {
            return;
        }

        filters.erase(filters.begin() + index.row());
        filtersModel.removeRow(index.row());
        updateView();
    });

    connect(refreshTimer, &QTimer::timeout, this, &MainWindow::updateCurrent);

    connect(ui->errorDismissButton, &QPushButton::clicked, ui->errorBox, &QGroupBox::hide);
    connect(ui->scan, &QPushButton::clicked, ui->actionScan, &QAction::trigger);

    addAction(ui->actionAddFilter);
    addAction(ui->actionRemoveFilter);
    addAction(ui->actionScanCancel);
    addAction(ui->actionScan);

    connect(ui->actionScanCancel, &QAction::triggered, this, [this]() {
        if (scanToken) {
            scanToken->set();
        }
    });

    connect(ui->scanCancel, &QAbstractButton::pressed, ui->actionScanCancel, &QAction::trigger);

    connect(ui->actionScan, &QAction::triggered, this, [this]() {
        if (scanToken) {
            wantsScan = true;
            return;
        }

        scan();
    });

    connect(ui->scanReset, &QAbstractButton::pressed, ui->actionScanReset, &QAction::trigger);

    connect(ui->actionScanReset, &QAction::triggered, this, [this]() {
        scanCurrent.reset();
        updateScanResults();
        updateView();
    });

    connect(ui->filtersList, &QListView::clicked, this, [this]() {
        updateView();
    });

    connect(ui->filtersList, &QListView::doubleClicked, this, [this](auto index) {
        if (!index.isValid()) {
            return;
        }

        addFilter->editFilter(filters.at(index.row()), index);
        addFilter->show();
    });

    connect(ui->actionAttach, &QAction::triggered, this, [this]() {
        openProcess->refreshList();
        openProcess->show();
    });

    connect(ui->actionDetach, &QAction::triggered, this, [this]() {
        if (scanToken) {
            scanToken->set();
            scanToken.reset();
        }

        processHandle = {};
        updateView();
    });

    connect(ui->actionAddFilter, &QAction::triggered, this, [this]() {
        addFilter->addFilter();
        addFilter->show();
    });

    connect(ui->addFilterButton, &QPushButton::pressed, ui->actionAddFilter, &QAction::trigger);

    connect(openProcess, &QDialog::accepted, this, [this]() {
        if (scanToken) {
            scanToken->set();
            scanToken.reset();
        }

        auto selected = openProcess->takeSelected();

        if (!selected) {
            return;
        }

        processHandle = selected;
        processHandle->refreshModules();
        processHandle->refreshThreads();
        openProcess->clearList();
        updateView();
    });

    connect(addFilter, &QDialog::accepted, this, [this]() {
        auto index = addFilter->takeIndex();
        auto filter = addFilter->takeFilter();

        if (!filter) {
            return;
        }

        if (index.isValid()) {
            filtersModel.itemFromIndex(index)->setText(filter->display().toQString());
            filters.insert(filters.begin() + index.row(), filter);
        } else {
            auto item = new QStandardItem(filter->display().toQString());
            item->setEditable(false);
            filtersModel.appendRow(item);
            filters.push_back(filter);
        }

        updateView();
    });

    updateView();
}

MainWindow::~MainWindow()
{
    delete openProcess;
    delete ui;
}

void MainWindow::scan()
{
    auto processHandle = this->processHandle;

    if (!processHandle) {
        addError("not attached to a process");
        return;
    }

    auto index = ui->filtersList->currentIndex();

    if (!index.isValid()) {
        addError("no filter selected");
        return;
    }

    ui->errorBox->hide();
    ui->errorText->clear();
    ui->scanProgress->setEnabled(true);
    ui->scanProgress->setValue(0);

    auto filter = this->filters.at(index.row());
    auto threadPool = this->threadPool;

    auto scan = scanCurrent;

    if (!scan) {
        scan = pts::Scan::create(threadPool);
    }

    scanToken = std::shared_ptr<pts::Token>(new pts::Token());

    QtConcurrent::run([this, processHandle, filter, scan]() {
        pts::ScanReporter reporter;

        reporter.report = [this](int percentage){
            QMetaObject::invokeMethod(this, "scanProgress", Qt::QueuedConnection, Q_ARG(int, percentage));
        };

        bool ok = true;

        try {
            scan->scan(*processHandle, *filter, *scanToken, reporter);
        } catch(const std::exception &e) {
            ok = false;
            QString message(e.what());
            QMetaObject::invokeMethod(this, "addError", Qt::QueuedConnection, Q_ARG(QString, message));
        }

        // set the current scan if there is none, and the new scan was successful.
        if (!scanCurrent && ok) {
            scanCurrent = scan;
        }

        QMetaObject::invokeMethod(this, "scanDone", Qt::QueuedConnection, Q_ARG(bool, ok));
    });

    updateView();
}

void MainWindow::updateView()
{
    ui->actionDetach->setEnabled(!!processHandle);

    // Can cancel if a scan is not in progress.
    ui->actionScanCancel->setEnabled(!!scanToken);
    ui->scanCancel->setEnabled(ui->actionScanCancel->isEnabled());

    // A scan is not in progress and a valid filter is selected.
    ui->actionScan->setEnabled(!scanToken && ui->filtersList->currentIndex().isValid());
    ui->scan->setEnabled(ui->actionScan->isEnabled());

    // Can only reset a scan if one is loaded.
    ui->actionScanReset->setEnabled(!!scanCurrent && !scanToken);
    ui->scanReset->setEnabled(ui->actionScanReset->isEnabled());

    // Can only remove if there are filters present.
    ui->actionRemoveFilter->setEnabled(filtersModel.rowCount() > 0);

    auto count = std::make_optional(0);

    if (scanCurrent) {
        count = std::make_optional(scanCurrent->count());
    }

    scanResults->setCount(count);

    if (processHandle) {
        auto name = processHandle->name().toQString();

        if (name.isEmpty()) {
            name = "*unknown*";
        }

        auto info = QString("Attached to %1 (pid: %2)")
            .arg(name)
            .arg(processHandle->pid().toQString());

        ui->processInfo->setText(info);
    } else {
        ui->processInfo->setText("Not attached");
    }
}

void MainWindow::updateCurrent()
{
    auto scanCurrent = this->scanCurrent;
    auto processHandle = this->processHandle;

    if (!scanCurrent) {
        return;
    }

    if (!processHandle) {
        return;
    }

    if (scanToken) {
        return;
    }

    scanToken = std::shared_ptr<pts::Token>(new pts::Token());

    QtConcurrent::run([this, scanCurrent, processHandle]() {
        pts::ScanReporter reporter;

        reporter.report = [](int percentage){
        };

        try {
            scanCurrent->refresh(*processHandle, 100, *scanToken, reporter);
        } catch(const std::exception &e) {
            qDebug() << "failed to refresh" << e.what();
        }

        QMetaObject::invokeMethod(this, "refreshDone", Qt::QueuedConnection);
    });
}

void MainWindow::addError(QString message)
{
    ui->errorText->setText(message);
    ui->errorBox->show();
}

void MainWindow::refreshDone()
{
    if (!scanToken) {
        throw std::exception("expected scan token to be set");
    }

    if (this->scanCurrent) {
        auto results = this->scanCurrent->results(100);
        scanResults->updateCurrent(results);
    }

    scanToken.reset();
    updateView();
}

void MainWindow::scanProgress(int percentage)
{
    ui->scanProgress->setValue(percentage);
}

void MainWindow::scanDone(bool ok)
{
    if (ok) {
        ui->scanProgress->setValue(100);
    } else {
        ui->scanProgress->setValue(0);
    }

    ui->scanProgress->setEnabled(false);

    updateScanResults();
    scanToken.reset();
    updateView();
}

void MainWindow::updateScanResults()
{
    std::optional<std::vector<pts::ScanResult>> results = {};

    if (scanCurrent) {
        results = std::make_optional(scanCurrent->results(100));
    }

    scanResults->update(processHandle, results);
}
