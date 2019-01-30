/********************************************************************************
** Form generated from reading UI file 'openprocess.ui'
**
** Created by: Qt User Interface Compiler version 5.12.0
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_OPENPROCESS_H
#define UI_OPENPROCESS_H

#include <QtCore/QVariant>
#include <QtWidgets/QApplication>
#include <QtWidgets/QDialog>
#include <QtWidgets/QDialogButtonBox>
#include <QtWidgets/QHeaderView>
#include <QtWidgets/QTreeView>
#include <QtWidgets/QVBoxLayout>

QT_BEGIN_NAMESPACE

class Ui_OpenProcess
{
public:
    QVBoxLayout *verticalLayout;
    QTreeView *list;
    QDialogButtonBox *buttons;

    void setupUi(QDialog *OpenProcess)
    {
        if (OpenProcess->objectName().isEmpty())
            OpenProcess->setObjectName(QString::fromUtf8("OpenProcess"));
        OpenProcess->resize(661, 416);
        verticalLayout = new QVBoxLayout(OpenProcess);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        list = new QTreeView(OpenProcess);
        list->setObjectName(QString::fromUtf8("list"));

        verticalLayout->addWidget(list);

        buttons = new QDialogButtonBox(OpenProcess);
        buttons->setObjectName(QString::fromUtf8("buttons"));
        buttons->setOrientation(Qt::Horizontal);
        buttons->setStandardButtons(QDialogButtonBox::Cancel|QDialogButtonBox::Ok);

        verticalLayout->addWidget(buttons);


        retranslateUi(OpenProcess);
        QObject::connect(buttons, SIGNAL(accepted()), OpenProcess, SLOT(accept()));
        QObject::connect(buttons, SIGNAL(rejected()), OpenProcess, SLOT(reject()));

        QMetaObject::connectSlotsByName(OpenProcess);
    } // setupUi

    void retranslateUi(QDialog *OpenProcess)
    {
        OpenProcess->setWindowTitle(QApplication::translate("OpenProcess", "Dialog", nullptr));
    } // retranslateUi

};

namespace Ui {
    class OpenProcess: public Ui_OpenProcess {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_OPENPROCESS_H
