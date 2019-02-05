#include "editfilter.h"
#include "filterlist.h"
#include "ui_filterlist.h"

FilterList::FilterList(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::FilterList),
    editFilter(new EditFilter(this))
{
    ui->setupUi(this);

    QStringList headers;
    headers.push_back("Filter");
    headers.push_back("Type");

    model.setHorizontalHeaderLabels(headers);
    ui->list->setModel(&model);

    contextMenu = new QMenu(ui->list);
    contextMenu->addAction(ui->actionRemoveFilter);
    ui->list->setContextMenuPolicy(Qt::CustomContextMenu);

    addAction(ui->actionAddFilter);
    addAction(ui->actionRemoveFilter);

    connect(ui->scan, &QAbstractButton::pressed, this, &FilterList::scan);
    connect(ui->reset, &QAbstractButton::pressed, this, &FilterList::scanReset);

    connect(this, &FilterList::updateView, this, [this]() {
        // Can only remove if there are filters present.
        ui->actionRemoveFilter->setEnabled(ui->list->currentIndex().isValid());
    });

    connect(ui->list, &QAbstractItemView::clicked, this, &FilterList::updateView);

    connect(ui->list, &QAbstractItemView::customContextMenuRequested, this, [this](const QPoint &pos) {
        auto index = ui->list->indexAt(pos);

        if (index.isValid()) {
            currentIndex = index;
            contextMenu->exec(ui->list->viewport()->mapToGlobal(pos));
            currentIndex = {};
        }
    });

    connect(ui->actionRemoveFilter, &QAction::triggered, this, [this]() {
        auto index = currentIndex;

        if (index.isValid()) {
            filters.erase(filters.begin() + index.row());
            model.removeRow(index.row());
            emit updateView();
        }
    });

    connect(ui->list, &QTreeView::doubleClicked, this, [this](auto index) {
        if (index.isValid()) {
            editFilter->editFilter(filters.at(index.row()), index);
            editFilter->show();
        }
    });

    connect(ui->add, &QPushButton::pressed, ui->actionAddFilter, &QAction::trigger);

    connect(ui->actionAddFilter, &QAction::triggered, this, [this]() {
        editFilter->addFilter();
        editFilter->show();
    });

    connect(editFilter, &QDialog::accepted, this, [this]() {
        auto index = editFilter->takeIndex();
        auto filter = editFilter->takeFilter();

        if (!filter) {
            return;
        }

        if (index.isValid()) {
            model.item(index.row(), 0)->setText(filter->display().toQString());
            model.item(index.row(), 1)->setText(filter->type().display().toQString());
            filters.insert(filters.begin() + index.row(), filter);
        } else {
            QList<QStandardItem *> row;

            auto item = new QStandardItem(filter->display().toQString());
            item->setEditable(false);
            row.push_back(item);

            auto type = new QStandardItem(filter->type().display().toQString());
            type->setEditable(false);
            row.push_back(type);

            model.appendRow(row);
            filters.push_back(filter);
        }

        ui->list->resizeColumnToContents(1);
        emit updateView();
    });

    emit updateView();
}

FilterList::~FilterList()
{
    delete ui;
    delete editFilter;
}

std::optional<std::shared_ptr<pts::Filter> > FilterList::currentFilter()
{
    auto index = ui->list->currentIndex();

    if (!index.isValid()) {
        return {};
    }

    auto row = uintptr_t(index.row());
    return std::make_optional(filters.at(row));
}

void FilterList::setResetEnabled(bool enabled)
{
    ui->reset->setEnabled(enabled);
}

void FilterList::setScanEnabled(bool enabled)
{
    ui->scan->setEnabled(enabled);
}
