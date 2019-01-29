#ifndef SCANRESULTS_H
#define SCANRESULTS_H

#include <optional>

#include <QStandardItemModel>
#include <QWidget>

namespace pts {
class ProcessHandle;
class ScanResult;
class Values;
}

namespace Ui {
class ScanResults;
}

class ScanResults : public QWidget
{
    Q_OBJECT

public:
    explicit ScanResults(QWidget *parent = nullptr);
    ~ScanResults();

    void update(const std::shared_ptr<pts::ProcessHandle> &handle, std::optional<std::vector<pts::ScanResult>> results);
    void updateCurrent(const pts::Values &values);
public slots:
    void setCount(std::optional<uintptr_t> count);

signals:
    // Signal indicating that we should add a watch for the given scan result.
    void addWatch(QModelIndex index);

private:
    Ui::ScanResults *ui;
    QStandardItemModel model;
};

#endif // SCANRESULTS_H
