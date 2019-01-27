#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <optional>

#include <QMainWindow>
#include <QStandardItemModel>

#include <pts.h>
#include <pts/ThreadPool.h>
#include <pts/Filter.h>
#include <pts/ProcessHandle.h>

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

private:
    std::shared_ptr<pts::ThreadPool> threadPool;
    Ui::MainWindow *ui;
    OpenProcess *openProcess;
    AddFilter *addFilter;

    // Model for rendering filters.
    QStandardItemModel *filtersModel;
    // List of pts filters.
    std::vector<std::shared_ptr<pts::Filter>> filters;
    // Currently selected filter.
    std::optional<QModelIndex> selectedFilter;

    // Current process we are interacting with.
    std::shared_ptr<pts::ProcessHandle> processHandle;

    void filtersListContextMenu(const QPoint &pos);

    // Update the view because something interesting happened.
    void updateView();
};

#endif // MAINWINDOW_H
