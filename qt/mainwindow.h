#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <optional>
#include <pts.h>
#include <pts/ThreadPool.h>
#include <pts/ProcessHandle.h>

class OpenProcess;

namespace Ui {
class MainWindow;
}

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit MainWindow(std::shared_ptr<pts::ThreadPool> threadPool, QWidget *parent = nullptr);
    ~MainWindow();

public slots:
    void attachAccepted();
    void attachTriggered();

private:
    std::shared_ptr<pts::ThreadPool> threadPool;
    Ui::MainWindow *ui;
    OpenProcess *openProcess;
    std::optional<std::shared_ptr<pts::ProcessHandle>> process_handle;
};

#endif // MAINWINDOW_H
