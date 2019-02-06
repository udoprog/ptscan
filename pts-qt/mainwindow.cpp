#include <iostream>

#include <QDebug>
#include <QApplication>
#include <QDesktopWidget>
#include <QCheckBox>
#include <QtConcurrent>
#include <QTimer>
#include <pts/System.h>
#include <pts/Scan.h>
#include <pts/Token.h>
#include <pts/Values.h>
#include <pts/Watch.h>
#include <pts/Addresses.h>
#include <pts/ProcessHandle.h>

#include "mainwindow.h"
#include "openprocess.h"
#include "editfilter.h"
#include "ui_mainwindow.h"
#include "addresslist.h"
#include "scanresults.h"
#include "filterlist.h"

MainWindow::MainWindow(std::shared_ptr<pts::ThreadPool> threadPool, QWidget *parent) :
    QMainWindow(parent),
    threadPool(threadPool),
    ui(new Ui::MainWindow),
    openProcess(new OpenProcess(this))
{
    ui->setupUi(this);
    addressList = new AddressList(ui->addressListBox);
    ui->addressListBoxLayout->addWidget(addressList);

    scanResults = new ScanResults(ui->scanResultsBox);
    ui->scanResultsBoxLayout->addWidget(scanResults);

    filterList = new FilterList(ui->filtersBox);
    ui->filtersBoxLayout->addWidget(filterList);

    refreshTimer.start(REFRESH_TIMER);

    ui->errorBox->setVisible(false);

    connect(&refreshTimer, &QTimer::timeout, this, &MainWindow::updateCurrent);

    // performing a scan.
    connect(this, &MainWindow::scanEnabled, ui->actionScan, &QAction::setEnabled);
    connect(this, &MainWindow::scanEnabled, filterList, &FilterList::setScanEnabled);

    // resetting a scan.
    connect(this, &MainWindow::resetEnabled, ui->actionScanReset, &QAction::setEnabled);
    connect(this, &MainWindow::resetEnabled, filterList, &FilterList::setResetEnabled);

    connect(ui->errorDismissButton, &QPushButton::clicked, ui->errorBox, &QGroupBox::hide);
    connect(filterList, &FilterList::updateView, this, &MainWindow::updateView);
    connect(filterList, &FilterList::scan, ui->actionScan, &QAction::trigger);
    connect(filterList, &FilterList::scanReset, ui->actionScanReset, &QAction::trigger);

    addAction(ui->actionAddFilter);
    addAction(ui->actionScanCancel);
    addAction(ui->actionScan);

    connect(ui->actionScanCancel, &QAction::triggered, this,
            [this]()
    {
        if (scanToken) {
            qDebug() << "cancelling";
            scanToken->set();
        }
    });

    connect(ui->scanCancel, &QAbstractButton::pressed, ui->actionScanCancel, &QAction::trigger);

    connect(ui->actionScan, &QAction::triggered, this,
            [this]()
    {
        if (scanToken) {
            return;
        }

        scan();
    });

    connect(ui->actionScanReset, &QAction::triggered, this, [this]() {
        scanCurrent.reset();
        updateScanResults();
        updateView();
    });

    connect(ui->actionAttach, &QAction::triggered, this,
            [this]()
    {
        openProcess->refreshList();
        openProcess->show();
    });

    connect(ui->actionDetach, &QAction::triggered, this,
            [this]()
    {
        if (scanToken) {
            scanToken->set();
            scanToken.reset();
        }

        handle.reset();
        updateView();
    });

    connect(openProcess, &QDialog::accepted, this,
            [this]()
    {
        if (scanToken) {
            scanToken->set();
            scanToken.reset();
        }

        auto selected = openProcess->takeSelected();

        if (!selected) {
            return;
        }

        handle.swap(selected);
        handle->refreshModules();
        handle->refreshThreads();
        openProcess->clearList();
        updateView();
    });

    connect(scanResults, &ScanResults::addWatch, this,
            [this](auto index)
    {
        auto scan = scanCurrent;

        if (!scan) {
            return;
        }

        if (auto result = scan->at(index.row())) {
            auto watch = result->asWatch(handle);
            addressList->addWatch(handle, watch);
        }
    });

    updateView();
}

MainWindow::~MainWindow()
{
    delete ui;
    delete openProcess;
}

void MainWindow::scan()
{
    auto handle = this->handle;

    if (!handle) {
        addError("not attached to a process");
        return;
    }

    auto filter = filterList->currentFilter();

    if (!filter) {
        addError("no filter selected");
        return;
    }

    ui->errorBox->hide();
    ui->errorText->clear();
    ui->scanProgress->setEnabled(true);
    ui->scanProgress->setValue(0);

    auto threadPool = this->threadPool;

    auto scan = scanCurrent;

    if (!scan) {
        scan = pts::Scan::create(threadPool);
    }

    scanToken = std::shared_ptr<pts::Token>(new pts::Token());

    QtConcurrent::run([this, handle, filter, scan]() {
        pts::ScanReporter reporter;

        reporter.report = [this](int percentage, uint64_t count){
            QMetaObject::invokeMethod(this, [this, percentage, count]() {
                scanProgress(percentage, count);
            }, Qt::QueuedConnection);
        };

        auto token = scanToken;
        bool ok = true;

        try {
            scan->scan(handle, filter, token, reporter);
        } catch(const std::exception &e) {
            ok = false;
            QString message(e.what());

            QMetaObject::invokeMethod(this, [this, message] {
                addError(message);
            }, Qt::QueuedConnection);
        }

        // set the current scan if there is none, and the new scan was successful.
        if (!scanCurrent && ok) {
            scanCurrent = scan;
        }

        QMetaObject::invokeMethod(this, [this, ok]() {
            scanDone(ok);
        }, Qt::QueuedConnection);
    });

    updateView();
}

void MainWindow::updateView()
{
    ui->actionDetach->setEnabled(!!handle);

    // Can cancel if a scan is not in progress.
    ui->actionScanCancel->setEnabled(!!scanToken);
    ui->scanCancel->setEnabled(ui->actionScanCancel->isEnabled());

    // A scan is not in progress and a valid filter is selected.
    emit scanEnabled(!scanToken && !!filterList->currentFilter());
    // Resetting the scan is possible when there is no scan in progress and there is a scan.
    emit resetEnabled(!!scanCurrent && !scanToken);

    auto count = std::make_optional(0);

    if (scanCurrent) {
        count = std::make_optional(scanCurrent->count());
    }

    scanResults->setCount(count);

    if (handle) {
        auto name = handle->name().toQString();

        if (name.isEmpty()) {
            name = "*unknown*";
        }

        auto info = QString("Attached to %1 (pid: %2)")
            .arg(name)
            .arg(handle->pid().toQString());

        ui->processInfo->setText(info);
    } else {
        ui->processInfo->setText("Not attached");
    }
}

void MainWindow::updateCurrent()
{
    auto scanCurrent = this->scanCurrent;
    auto handle = this->handle;

    if (!scanCurrent) {
        return;
    }

    if (!handle) {
        return;
    }

    if (refreshToken) {
        return;
    }

    refreshToken = std::shared_ptr<pts::Token>(new pts::Token());

    QtConcurrent::run([this, scanCurrent, handle]() {
        pts::ScanReporter reporter;

        reporter.report = [](int percentage, uint64_t count){
        };

        auto info = addressList->info(handle);

        try {
            handle->readMemory(
                threadPool,
                info.addresses,
                info.output,
                refreshToken,
                reporter
            );
        } catch(const std::exception &e) {
            qDebug() << "failed to refresh" << e.what();
        }

        auto scanValues = std::make_shared<pts::Values>(scanCurrent->values(DISPLAY_LENGTH));

        try {
            scanCurrent->refresh(handle, scanValues, refreshToken, reporter);
        } catch(const std::exception &e) {
            qDebug() << "failed to refresh" << e.what();
        }

        refreshToken.reset();

        QMetaObject::invokeMethod(this, [this, info, scanValues]() {
            addressList->updateCurrent(info);
            scanResults->updateCurrent(scanValues);
            updateView();
        }, Qt::QueuedConnection);
    });
}

void MainWindow::addError(QString message)
{
    ui->errorText->setText(message);
    ui->errorBox->show();
}

void MainWindow::scanProgress(int percentage, uint64_t count)
{
    ui->scanProgress->setValue(percentage);
    scanResults->setIntermediateCount(count);
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
        results = std::make_optional(scanCurrent->results(DISPLAY_LENGTH));
    }

    scanResults->update(handle, std::move(results));
}
