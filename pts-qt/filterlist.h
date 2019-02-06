#ifndef FILTERLIST_H
#define FILTERLIST_H

#include <vector>
#include <memory>

#include <QWidget>
#include <QModelIndex>

class QStandardItemModel;
class QMenu;

namespace pts {
class Filter;
}

namespace Ui {
class FilterList;
}

class EditFilter;

class FilterList : public QWidget
{
    Q_OBJECT

public:
    explicit FilterList(QWidget *parent = nullptr);
    ~FilterList();
    // Access the currently selected filter.
    std::shared_ptr<pts::Filter> currentFilter();

public slots:
    // Set if reset is enabled.
    void setResetEnabled(bool enabled);
    // Set if scanning is enabled.
    void setScanEnabled(bool enabled);

signals:
    // List was updated and rendering should be refreshed.
    void updateView();
    // Trigger a scan.
    void scan();
    // Reset a scan.
    void scanReset();

private:
    Ui::FilterList *ui;
    EditFilter *editFilter;
    QMenu *contextMenu;
    // Model for rendering filters.
    QStandardItemModel *model;
    // List of pts filters.
    std::vector<std::shared_ptr<pts::Filter>> filters;
    // Current index that has activated the context menu.
    QModelIndex currentIndex;
};

#endif // FILTERLIST_H
