#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <optional>

#include <QMainWindow>
#include <QStandardItemModel>
#include <QFuture>

#include <pts.h>
#include <pts/ThreadPool.h>
#include <pts/Filter.h>
#include <pts/ProcessHandle.h>
#include <pts/Token.h>

class OpenProcess;
class AddFilter;

namespace Ui {
class MainWindow;
}

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit MainWindow(std::shared_ptr<pts::ThreadPool> threadPool, QWidget *parent = nullptr);
    ~MainWindow();

    void updateScanResults();

public slots:
    // Fire off a scan.
    void scan();

    // Report an error.
    void addError(QString message);

    // Report that a scan progress has progressed.
    void scanProgress(int percentage);

    // Report that a scan progress has completed.
    void scanDone(bool interrupted);

    // Report that a refresh has completed.
    void refreshDone();

private slots:
    // Trigger an update to currently visualized values.
    void updateCurrent();

    // Update the view because something interesting happened.
    void updateView();

private:
    std::shared_ptr<pts::ThreadPool> threadPool;
    Ui::MainWindow *ui;
    OpenProcess *openProcess;
    AddFilter *addFilter;

    // Model for rendering filters.
    QStandardItemModel filtersModel;
    // List of pts filters.
    std::vector<std::shared_ptr<pts::Filter>> filters;

    // Current process we are interacting with.
    std::shared_ptr<pts::ProcessHandle> processHandle;

    // A scan that is in progress.
    std::shared_ptr<pts::Token> scanToken;
    std::shared_ptr<pts::Scan> scanCurrent;
    QStandardItemModel scanResults;
    // Set if we have clicked scan, but another one is already in progress.
    bool wantsScan;

    // A timer to refresh current values.
    QTimer *refreshTimer;
};

#endif // MAINWINDOW_H
