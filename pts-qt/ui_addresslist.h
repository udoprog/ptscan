/********************************************************************************
** Form generated from reading UI file 'addresslist.ui'
**
** Created by: Qt User Interface Compiler version 5.12.0
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_ADDRESSLIST_H
#define UI_ADDRESSLIST_H

#include <QtCore/QVariant>
#include <QtWidgets/QAction>
#include <QtWidgets/QApplication>
#include <QtWidgets/QHBoxLayout>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QPushButton>
#include <QtWidgets/QSpacerItem>
#include <QtWidgets/QTreeView>
#include <QtWidgets/QVBoxLayout>
#include <QtWidgets/QWidget>

QT_BEGIN_NAMESPACE

class Ui_AddressList
{
public:
    QAction *actionEdit;
    QAction *actionDelete;
    QVBoxLayout *verticalLayout;
    QTreeView *list;
    QHBoxLayout *horizontalLayout;
    QSpacerItem *horizontalSpacer;
    QPushButton *pushButton;

    void setupUi(QWidget *AddressList)
    {
        if (AddressList->objectName().isEmpty())
            AddressList->setObjectName(QString::fromUtf8("AddressList"));
        AddressList->resize(601, 448);
        actionEdit = new QAction(AddressList);
        actionEdit->setObjectName(QString::fromUtf8("actionEdit"));
        actionDelete = new QAction(AddressList);
        actionDelete->setObjectName(QString::fromUtf8("actionDelete"));
        verticalLayout = new QVBoxLayout(AddressList);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        verticalLayout->setContentsMargins(0, 0, 0, 0);
        list = new QTreeView(AddressList);
        list->setObjectName(QString::fromUtf8("list"));
        QSizePolicy sizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(list->sizePolicy().hasHeightForWidth());
        list->setSizePolicy(sizePolicy);

        verticalLayout->addWidget(list);

        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        horizontalSpacer = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout->addItem(horizontalSpacer);

        pushButton = new QPushButton(AddressList);
        pushButton->setObjectName(QString::fromUtf8("pushButton"));

        horizontalLayout->addWidget(pushButton);


        verticalLayout->addLayout(horizontalLayout);


        retranslateUi(AddressList);

        QMetaObject::connectSlotsByName(AddressList);
    } // setupUi

    void retranslateUi(QWidget *AddressList)
    {
        AddressList->setWindowTitle(QApplication::translate("AddressList", "Form", nullptr));
        actionEdit->setText(QApplication::translate("AddressList", "Edit Address", nullptr));
        actionDelete->setText(QApplication::translate("AddressList", "Delete Address", nullptr));
#ifndef QT_NO_SHORTCUT
        actionDelete->setShortcut(QApplication::translate("AddressList", "Del", nullptr));
#endif // QT_NO_SHORTCUT
        pushButton->setText(QApplication::translate("AddressList", "Add Address Manually", nullptr));
    } // retranslateUi

};

namespace Ui {
    class AddressList: public Ui_AddressList {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_ADDRESSLIST_H
