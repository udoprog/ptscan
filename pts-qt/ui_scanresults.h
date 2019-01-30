/********************************************************************************
** Form generated from reading UI file 'scanresults.ui'
**
** Created by: Qt User Interface Compiler version 5.12.0
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_SCANRESULTS_H
#define UI_SCANRESULTS_H

#include <QtCore/QVariant>
#include <QtWidgets/QApplication>
#include <QtWidgets/QHBoxLayout>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QLabel>
#include <QtWidgets/QSpacerItem>
#include <QtWidgets/QTreeView>
#include <QtWidgets/QVBoxLayout>
#include <QtWidgets/QWidget>

QT_BEGIN_NAMESPACE

class Ui_ScanResults
{
public:
    QVBoxLayout *verticalLayout;
    QTreeView *list;
    QHBoxLayout *horizontalLayout;
    QSpacerItem *horizontalSpacer;
    QLabel *count;

    void setupUi(QWidget *ScanResults)
    {
        if (ScanResults->objectName().isEmpty())
            ScanResults->setObjectName(QString::fromUtf8("ScanResults"));
        ScanResults->resize(400, 300);
        verticalLayout = new QVBoxLayout(ScanResults);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        verticalLayout->setContentsMargins(0, 0, 0, 0);
        list = new QTreeView(ScanResults);
        list->setObjectName(QString::fromUtf8("list"));

        verticalLayout->addWidget(list);

        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        horizontalSpacer = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout->addItem(horizontalSpacer);

        count = new QLabel(ScanResults);
        count->setObjectName(QString::fromUtf8("count"));

        horizontalLayout->addWidget(count);


        verticalLayout->addLayout(horizontalLayout);


        retranslateUi(ScanResults);

        QMetaObject::connectSlotsByName(ScanResults);
    } // setupUi

    void retranslateUi(QWidget *ScanResults)
    {
        ScanResults->setWindowTitle(QApplication::translate("ScanResults", "Form", nullptr));
        count->setText(QApplication::translate("ScanResults", "scanResultsCount", nullptr));
    } // retranslateUi

};

namespace Ui {
    class ScanResults: public Ui_ScanResults {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_SCANRESULTS_H
