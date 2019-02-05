#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <optional>

#include <QMainWindow>
#include <QStandardItemModel>
#include <QTimer>

#include <pts/System.h>
#include <pts/ThreadPool.h>
#include <pts/Filter.h>
#include <pts/ProcessHandle.h>
#include <pts/Token.h>

class OpenProcess;
class AddressList;
class ScanResults;
class FilterList;

namespace pts {
class Values;
}

namespace Ui {
class MainWindow;
}

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit MainWindow(std::shared_ptr<pts::ThreadPool> threadPool, QWidget *parent = nullptr);
    ~MainWindow();

    // The number of items to display.
    const uintptr_t DISPLAY_LENGTH = 100;
    // Refresh rate in milliseconds for values.
    const int REFRESH_TIMER = 500;

public slots:
    // Fire off a scan.
    void scan();

    // Report an error.
    void addError(QString message);

    // Report that a scan progress has progressed.
    void scanProgress(int percentage, uint64_t count);

    // Report that a scan progress has completed.
    void scanDone(bool interrupted);

    // Update existing scan results.
    void updateScanResults();

private slots:
    // Trigger an update to currently visualized values.
    void updateCurrent();

    // Update the view because something interesting happened.
    void updateView();

signals:
    // Indicate if scan is enabled.
    void scanEnabled(bool enabled);

    // Indicate if reset is enabled.
    void resetEnabled(bool enabled);

private:
    std::shared_ptr<pts::ThreadPool> threadPool;
    Ui::MainWindow *ui;
    OpenProcess *openProcess;
    AddressList *addressList;
    ScanResults *scanResults;
    FilterList *filterList;

    // Current process we are interacting with.
    std::shared_ptr<pts::ProcessHandle> handle;

    // A scan that is in progress.
    std::shared_ptr<pts::Token> scanToken;
    std::shared_ptr<pts::Scan> scanCurrent;
    // Set if we have clicked scan, but another one is already in progress.
    std::shared_ptr<pts::Token> refreshToken;

    // A timer to refresh current values.
    QTimer refreshTimer;
};

#endif // MAINWINDOW_H
