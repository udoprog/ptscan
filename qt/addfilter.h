#ifndef ADDFILTER_H
#define ADDFILTER_H

#include <optional>
#include <memory>
#include <string>
#include <pts/Filter.h>

#include <QDialog>

namespace Ui {
class AddFilter;
}

class AddFilter : public QDialog
{
    Q_OBJECT

public:
    explicit AddFilter(QWidget *parent = nullptr);
    ~AddFilter();

    std::optional<std::shared_ptr<pts::Filter>> predicate;

private:
    Ui::AddFilter *ui;
    std::optional<std::string> error;

    void stateChanged();
};

#endif // ADDFILTER_H
