#ifndef EDITFILTER_H
#define EDITFILTER_H

#include <optional>
#include <memory>
#include <string>
#include <vector>
#include <QStandardItemModel>

#include <pts/Filter.h>
#include <pts/Value.h>
#include <QDialog>
#include <QModelIndex>

namespace Ui {
class EditFilter;
}

class TypeComboBox;

class EditFilter : public QDialog
{
    Q_OBJECT

public:
    explicit EditFilter(QWidget *parent = nullptr);
    ~EditFilter();
    // Take filter that was edited.
    std::shared_ptr<pts::Filter> takeFilter();
    // Take index that was edited.
    QModelIndex takeIndex();
    // Indicate that we want to add a filter through the dialog.
    void addFilter();
    // Indicate that we want to edit a filter through the dialog.
    void editFilter(std::shared_ptr<pts::Filter> filter, QModelIndex index);
private:
    Ui::EditFilter *ui;
    TypeComboBox *type;
    std::optional<std::string> error;
    std::shared_ptr<pts::Filter> filter;
    QModelIndex index;

    void updateView();
};

#endif // EDITFILTER_H
