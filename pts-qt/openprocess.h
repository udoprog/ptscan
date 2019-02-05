#ifndef OPENPROCESS_H
#define OPENPROCESS_H

#include <QDialog>
#include <QStandardItemModel>
#include <vector>
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

    // Take the selected value from the prompt.
    std::shared_ptr<pts::ProcessHandle> takeSelected();

private:
    Ui::OpenProcess *ui;
    QStandardItemModel *model;
    std::vector<std::shared_ptr<pts::ProcessHandle>> handles;
    std::shared_ptr<pts::ProcessHandle> selected;
};

#endif // OPENPROCESS_H
