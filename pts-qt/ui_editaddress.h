/********************************************************************************
** Form generated from reading UI file 'editaddress.ui'
**
** Created by: Qt User Interface Compiler version 5.12.0
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_EDITADDRESS_H
#define UI_EDITADDRESS_H

#include <QtCore/QVariant>
#include <QtWidgets/QApplication>
#include <QtWidgets/QCheckBox>
#include <QtWidgets/QComboBox>
#include <QtWidgets/QDialog>
#include <QtWidgets/QDialogButtonBox>
#include <QtWidgets/QFormLayout>
#include <QtWidgets/QLabel>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QSpacerItem>
#include <QtWidgets/QVBoxLayout>

QT_BEGIN_NAMESPACE

class Ui_EditAddress
{
public:
    QVBoxLayout *verticalLayout;
    QFormLayout *formLayout;
    QLabel *pointerLabel;
    QLineEdit *pointer;
    QLabel *typeLabel;
    QComboBox *type;
    QCheckBox *signedCheck;
    QLabel *error;
    QSpacerItem *verticalSpacer;
    QDialogButtonBox *buttonBox;

    void setupUi(QDialog *EditAddress)
    {
        if (EditAddress->objectName().isEmpty())
            EditAddress->setObjectName(QString::fromUtf8("EditAddress"));
        EditAddress->resize(413, 172);
        verticalLayout = new QVBoxLayout(EditAddress);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        formLayout = new QFormLayout();
        formLayout->setObjectName(QString::fromUtf8("formLayout"));
        pointerLabel = new QLabel(EditAddress);
        pointerLabel->setObjectName(QString::fromUtf8("pointerLabel"));

        formLayout->setWidget(0, QFormLayout::LabelRole, pointerLabel);

        pointer = new QLineEdit(EditAddress);
        pointer->setObjectName(QString::fromUtf8("pointer"));

        formLayout->setWidget(0, QFormLayout::FieldRole, pointer);

        typeLabel = new QLabel(EditAddress);
        typeLabel->setObjectName(QString::fromUtf8("typeLabel"));

        formLayout->setWidget(1, QFormLayout::LabelRole, typeLabel);

        type = new QComboBox(EditAddress);
        type->setObjectName(QString::fromUtf8("type"));

        formLayout->setWidget(1, QFormLayout::FieldRole, type);

        signedCheck = new QCheckBox(EditAddress);
        signedCheck->setObjectName(QString::fromUtf8("signedCheck"));

        formLayout->setWidget(2, QFormLayout::FieldRole, signedCheck);


        verticalLayout->addLayout(formLayout);

        error = new QLabel(EditAddress);
        error->setObjectName(QString::fromUtf8("error"));

        verticalLayout->addWidget(error);

        verticalSpacer = new QSpacerItem(20, 40, QSizePolicy::Minimum, QSizePolicy::Expanding);

        verticalLayout->addItem(verticalSpacer);

        buttonBox = new QDialogButtonBox(EditAddress);
        buttonBox->setObjectName(QString::fromUtf8("buttonBox"));
        buttonBox->setOrientation(Qt::Horizontal);
        buttonBox->setStandardButtons(QDialogButtonBox::Cancel|QDialogButtonBox::Ok);

        verticalLayout->addWidget(buttonBox);


        retranslateUi(EditAddress);
        QObject::connect(buttonBox, SIGNAL(accepted()), EditAddress, SLOT(accept()));
        QObject::connect(buttonBox, SIGNAL(rejected()), EditAddress, SLOT(reject()));

        QMetaObject::connectSlotsByName(EditAddress);
    } // setupUi

    void retranslateUi(QDialog *EditAddress)
    {
        EditAddress->setWindowTitle(QApplication::translate("EditAddress", "Edit Address", nullptr));
        pointerLabel->setText(QApplication::translate("EditAddress", "Pointer", nullptr));
        typeLabel->setText(QApplication::translate("EditAddress", "Type", nullptr));
        signedCheck->setText(QApplication::translate("EditAddress", "Signed", nullptr));
        error->setText(QApplication::translate("EditAddress", "error", nullptr));
    } // retranslateUi

};

namespace Ui {
    class EditAddress: public Ui_EditAddress {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_EDITADDRESS_H
