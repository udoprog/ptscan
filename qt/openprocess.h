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
    void refreshList();

    std::shared_ptr<pts::ProcessHandle> selected;

signals:

public slots:
    void clicked(const QModelIndex &index);

private:
    Ui::OpenProcess *ui;
    QStandardItemModel *model;
    std::vector<std::shared_ptr<pts::ProcessHandle>> handles;
};

#endif // OPENPROCESS_H
