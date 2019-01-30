/********************************************************************************
** Form generated from reading UI file 'editfilter.ui'
**
** Created by: Qt User Interface Compiler version 5.12.0
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_EDITFILTER_H
#define UI_EDITFILTER_H

#include <QtCore/QVariant>
#include <QtWidgets/QApplication>
#include <QtWidgets/QDialog>
#include <QtWidgets/QDialogButtonBox>
#include <QtWidgets/QLabel>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QSpacerItem>
#include <QtWidgets/QVBoxLayout>

QT_BEGIN_NAMESPACE

class Ui_AddFilter
{
public:
    QVBoxLayout *verticalLayout;
    QLabel *addFilterLabel;
    QLabel *editFilterLabel;
    QLineEdit *input;
    QLabel *error;
    QSpacerItem *verticalSpacer;
    QDialogButtonBox *buttonBox;

    void setupUi(QDialog *AddFilter)
    {
        if (AddFilter->objectName().isEmpty())
            AddFilter->setObjectName(QString::fromUtf8("AddFilter"));
        AddFilter->resize(400, 140);
        QSizePolicy sizePolicy(QSizePolicy::Ignored, QSizePolicy::Ignored);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(AddFilter->sizePolicy().hasHeightForWidth());
        AddFilter->setSizePolicy(sizePolicy);
        verticalLayout = new QVBoxLayout(AddFilter);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        addFilterLabel = new QLabel(AddFilter);
        addFilterLabel->setObjectName(QString::fromUtf8("addFilterLabel"));

        verticalLayout->addWidget(addFilterLabel);

        editFilterLabel = new QLabel(AddFilter);
        editFilterLabel->setObjectName(QString::fromUtf8("editFilterLabel"));

        verticalLayout->addWidget(editFilterLabel);

        input = new QLineEdit(AddFilter);
        input->setObjectName(QString::fromUtf8("input"));

        verticalLayout->addWidget(input);

        error = new QLabel(AddFilter);
        error->setObjectName(QString::fromUtf8("error"));

        verticalLayout->addWidget(error);

        verticalSpacer = new QSpacerItem(20, 40, QSizePolicy::Minimum, QSizePolicy::Expanding);

        verticalLayout->addItem(verticalSpacer);

        buttonBox = new QDialogButtonBox(AddFilter);
        buttonBox->setObjectName(QString::fromUtf8("buttonBox"));
        buttonBox->setOrientation(Qt::Horizontal);
        buttonBox->setStandardButtons(QDialogButtonBox::Cancel|QDialogButtonBox::Ok);

        verticalLayout->addWidget(buttonBox);


        retranslateUi(AddFilter);
        QObject::connect(buttonBox, SIGNAL(accepted()), AddFilter, SLOT(accept()));
        QObject::connect(buttonBox, SIGNAL(rejected()), AddFilter, SLOT(reject()));

        QMetaObject::connectSlotsByName(AddFilter);
    } // setupUi

    void retranslateUi(QDialog *AddFilter)
    {
        AddFilter->setWindowTitle(QApplication::translate("AddFilter", "Edit Filter", nullptr));
        addFilterLabel->setText(QApplication::translate("AddFilter", "Adding filter", nullptr));
        editFilterLabel->setText(QApplication::translate("AddFilter", "Editing filter", nullptr));
        error->setText(QApplication::translate("AddFilter", "Error", nullptr));
    } // retranslateUi

};

namespace Ui {
    class AddFilter: public Ui_AddFilter {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_EDITFILTER_H
