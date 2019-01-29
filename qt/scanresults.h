#ifndef SCANRESULTS_H
#define SCANRESULTS_H

#include <optional>

#include <QStandardItemModel>
#include <QWidget>

namespace pts {
class ProcessHandle;
class ScanResult;
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
    void updateCurrent(const std::vector<pts::ScanResult> &results);
public slots:
    void setCount(std::optional<uintptr_t> count);
private:
    Ui::ScanResults *ui;
    QStandardItemModel model;
};

#endif // SCANRESULTS_H
