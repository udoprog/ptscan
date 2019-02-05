#ifndef FILTERLIST_H
#define FILTERLIST_H

#include <vector>
#include <optional>
#include <memory>

#include <QMenu>
#include <QStandardItemModel>
#include <QWidget>

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

    // Access currently selected filter.
    std::optional<std::shared_ptr<pts::Filter>> currentFilter();

public slots:
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

    // Model for rendering filters.
    QStandardItemModel model;
    // List of pts filters.
    std::vector<std::shared_ptr<pts::Filter>> filters;
    QMenu *contextMenu;
    // Current index that has activated the context menu.
    QModelIndex currentIndex;
};

#endif // FILTERLIST_H
