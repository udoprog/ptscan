#ifndef ADDFILTER_H
#define ADDFILTER_H

#include <optional>
#include <memory>
#include <string>
#include <pts/Filter.h>

#include <QDialog>
#include <QModelIndex>

namespace Ui {
class AddFilter;
}

class AddFilter : public QDialog
{
    Q_OBJECT

public:
    explicit AddFilter(QWidget *parent = nullptr);
    ~AddFilter();

    // Take filter that was edited.
    std::shared_ptr<pts::Filter> takeFilter();
    // Take index that was edited.
    QModelIndex takeIndex();

    // Indicate that we want to add a filter through the dialog.
    void addFilter();
    // Indicate that we want to edit a filter through the dialog.
    void editFilter(std::shared_ptr<pts::Filter> filter, QModelIndex index);
private:
    Ui::AddFilter *ui;
    std::optional<std::string> error;
    std::shared_ptr<pts::Filter> filter;
    QModelIndex index;

    void stateChanged();
};

#endif // ADDFILTER_H
