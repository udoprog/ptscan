#ifndef OPENPROCESS_H
#define OPENPROCESS_H

#include <QDialog>
#include <QStandardItemModel>
#include <vector>
#include <pts.h>
#include <pts/ProcessHandle.h>

namespace Ui {
class OpenProcess;
}

class OpenProcess : public QDialog
{
    Q_OBJECT
public:
    explicit OpenProcess(QWidget *parent = nullptr);
    ~OpenProcess();

    // Clear the list of processes.
    void clearList();

    // Refresh the list of processes.
    void refreshList();

    std::shared_ptr<pts::ProcessHandle> selected;

public slots:
    void clicked(const QModelIndex &index);

private:
    Ui::OpenProcess *ui;
    QStandardItemModel *model;
    std::vector<std::shared_ptr<pts::ProcessHandle>> handles;
};

#endif // OPENPROCESS_H
