/********************************************************************************
** Form generated from reading UI file 'mainwindow.ui'
**
** Created by: Qt User Interface Compiler version 5.12.0
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_MAINWINDOW_H
#define UI_MAINWINDOW_H

#include <QtCore/QVariant>
#include <QtWidgets/QAction>
#include <QtWidgets/QApplication>
#include <QtWidgets/QGridLayout>
#include <QtWidgets/QGroupBox>
#include <QtWidgets/QHBoxLayout>
#include <QtWidgets/QLabel>
#include <QtWidgets/QListView>
#include <QtWidgets/QMainWindow>
#include <QtWidgets/QMenu>
#include <QtWidgets/QMenuBar>
#include <QtWidgets/QProgressBar>
#include <QtWidgets/QPushButton>
#include <QtWidgets/QSpacerItem>
#include <QtWidgets/QStatusBar>
#include <QtWidgets/QToolBar>
#include <QtWidgets/QVBoxLayout>
#include <QtWidgets/QWidget>

QT_BEGIN_NAMESPACE

class Ui_MainWindow
{
public:
    QAction *actionScanOpen;
    QAction *actionScanSave;
    QAction *actionScanSaveAs;
    QAction *actionClose;
    QAction *actionAttach;
    QAction *actionDetach;
    QAction *actionAddFilter;
    QAction *actionRemoveFilter;
    QAction *actionScan;
    QAction *actionScanCancel;
    QAction *actionScanReset;
    QWidget *centralWidget;
    QGridLayout *gridLayout;
    QVBoxLayout *verticalLayout;
    QHBoxLayout *horizontalLayout_4;
    QGroupBox *scanResultsBox;
    QGridLayout *scanResultsBoxLayout;
    QGroupBox *options;
    QVBoxLayout *verticalLayout_2;
    QGroupBox *filtersBox;
    QVBoxLayout *verticalLayout_3;
    QListView *filtersList;
    QHBoxLayout *horizontalLayout;
    QPushButton *scanReset;
    QSpacerItem *horizontalSpacer;
    QPushButton *scan;
    QPushButton *addFilterButton;
    QGroupBox *addressListBox;
    QGridLayout *addressListBoxLayout;
    QHBoxLayout *horizontalLayout_2;
    QProgressBar *scanProgress;
    QPushButton *scanCancel;
    QLabel *processInfo;
    QGroupBox *errorBox;
    QVBoxLayout *verticalLayout_4;
    QLabel *errorText;
    QHBoxLayout *horizontalLayout_3;
    QSpacerItem *horizontalSpacer_2;
    QPushButton *errorDismissButton;
    QMenuBar *menuBar;
    QMenu *menuFile;
    QToolBar *mainToolBar;
    QStatusBar *statusBar;

    void setupUi(QMainWindow *MainWindow)
    {
        if (MainWindow->objectName().isEmpty())
            MainWindow->setObjectName(QString::fromUtf8("MainWindow"));
        MainWindow->resize(826, 851);
        actionScanOpen = new QAction(MainWindow);
        actionScanOpen->setObjectName(QString::fromUtf8("actionScanOpen"));
        actionScanSave = new QAction(MainWindow);
        actionScanSave->setObjectName(QString::fromUtf8("actionScanSave"));
        actionScanSaveAs = new QAction(MainWindow);
        actionScanSaveAs->setObjectName(QString::fromUtf8("actionScanSaveAs"));
        actionClose = new QAction(MainWindow);
        actionClose->setObjectName(QString::fromUtf8("actionClose"));
        actionAttach = new QAction(MainWindow);
        actionAttach->setObjectName(QString::fromUtf8("actionAttach"));
        actionDetach = new QAction(MainWindow);
        actionDetach->setObjectName(QString::fromUtf8("actionDetach"));
        actionAddFilter = new QAction(MainWindow);
        actionAddFilter->setObjectName(QString::fromUtf8("actionAddFilter"));
        actionRemoveFilter = new QAction(MainWindow);
        actionRemoveFilter->setObjectName(QString::fromUtf8("actionRemoveFilter"));
        actionScan = new QAction(MainWindow);
        actionScan->setObjectName(QString::fromUtf8("actionScan"));
        actionScanCancel = new QAction(MainWindow);
        actionScanCancel->setObjectName(QString::fromUtf8("actionScanCancel"));
        actionScanReset = new QAction(MainWindow);
        actionScanReset->setObjectName(QString::fromUtf8("actionScanReset"));
        centralWidget = new QWidget(MainWindow);
        centralWidget->setObjectName(QString::fromUtf8("centralWidget"));
        gridLayout = new QGridLayout(centralWidget);
        gridLayout->setSpacing(6);
        gridLayout->setContentsMargins(11, 11, 11, 11);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        verticalLayout = new QVBoxLayout();
        verticalLayout->setSpacing(6);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        horizontalLayout_4 = new QHBoxLayout();
        horizontalLayout_4->setSpacing(6);
        horizontalLayout_4->setObjectName(QString::fromUtf8("horizontalLayout_4"));
        scanResultsBox = new QGroupBox(centralWidget);
        scanResultsBox->setObjectName(QString::fromUtf8("scanResultsBox"));
        scanResultsBoxLayout = new QGridLayout(scanResultsBox);
        scanResultsBoxLayout->setSpacing(6);
        scanResultsBoxLayout->setContentsMargins(11, 11, 11, 11);
        scanResultsBoxLayout->setObjectName(QString::fromUtf8("scanResultsBoxLayout"));

        horizontalLayout_4->addWidget(scanResultsBox);

        options = new QGroupBox(centralWidget);
        options->setObjectName(QString::fromUtf8("options"));
        options->setMinimumSize(QSize(200, 0));
        verticalLayout_2 = new QVBoxLayout(options);
        verticalLayout_2->setSpacing(6);
        verticalLayout_2->setContentsMargins(11, 11, 11, 11);
        verticalLayout_2->setObjectName(QString::fromUtf8("verticalLayout_2"));
        filtersBox = new QGroupBox(options);
        filtersBox->setObjectName(QString::fromUtf8("filtersBox"));
        verticalLayout_3 = new QVBoxLayout(filtersBox);
        verticalLayout_3->setSpacing(6);
        verticalLayout_3->setContentsMargins(11, 11, 11, 11);
        verticalLayout_3->setObjectName(QString::fromUtf8("verticalLayout_3"));
        filtersList = new QListView(filtersBox);
        filtersList->setObjectName(QString::fromUtf8("filtersList"));
        filtersList->setContextMenuPolicy(Qt::ActionsContextMenu);

        verticalLayout_3->addWidget(filtersList);

        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setSpacing(6);
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        horizontalLayout->setContentsMargins(-1, 0, -1, -1);
        scanReset = new QPushButton(filtersBox);
        scanReset->setObjectName(QString::fromUtf8("scanReset"));

        horizontalLayout->addWidget(scanReset);

        horizontalSpacer = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout->addItem(horizontalSpacer);

        scan = new QPushButton(filtersBox);
        scan->setObjectName(QString::fromUtf8("scan"));

        horizontalLayout->addWidget(scan);

        addFilterButton = new QPushButton(filtersBox);
        addFilterButton->setObjectName(QString::fromUtf8("addFilterButton"));

        horizontalLayout->addWidget(addFilterButton);


        verticalLayout_3->addLayout(horizontalLayout);


        verticalLayout_2->addWidget(filtersBox);


        horizontalLayout_4->addWidget(options);


        verticalLayout->addLayout(horizontalLayout_4);

        addressListBox = new QGroupBox(centralWidget);
        addressListBox->setObjectName(QString::fromUtf8("addressListBox"));
        addressListBoxLayout = new QGridLayout(addressListBox);
        addressListBoxLayout->setSpacing(6);
        addressListBoxLayout->setContentsMargins(11, 11, 11, 11);
        addressListBoxLayout->setObjectName(QString::fromUtf8("addressListBoxLayout"));

        verticalLayout->addWidget(addressListBox);


        gridLayout->addLayout(verticalLayout, 5, 0, 1, 1);

        horizontalLayout_2 = new QHBoxLayout();
        horizontalLayout_2->setSpacing(6);
        horizontalLayout_2->setObjectName(QString::fromUtf8("horizontalLayout_2"));
        horizontalLayout_2->setContentsMargins(-1, -1, -1, 0);
        scanProgress = new QProgressBar(centralWidget);
        scanProgress->setObjectName(QString::fromUtf8("scanProgress"));
        scanProgress->setEnabled(false);
        scanProgress->setValue(0);

        horizontalLayout_2->addWidget(scanProgress);

        scanCancel = new QPushButton(centralWidget);
        scanCancel->setObjectName(QString::fromUtf8("scanCancel"));

        horizontalLayout_2->addWidget(scanCancel);


        gridLayout->addLayout(horizontalLayout_2, 3, 0, 1, 1);

        processInfo = new QLabel(centralWidget);
        processInfo->setObjectName(QString::fromUtf8("processInfo"));
        QFont font;
        font.setPointSize(12);
        processInfo->setFont(font);

        gridLayout->addWidget(processInfo, 0, 0, 1, 1);

        errorBox = new QGroupBox(centralWidget);
        errorBox->setObjectName(QString::fromUtf8("errorBox"));
        QSizePolicy sizePolicy(QSizePolicy::Preferred, QSizePolicy::Preferred);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(errorBox->sizePolicy().hasHeightForWidth());
        errorBox->setSizePolicy(sizePolicy);
        QFont font1;
        font1.setPointSize(10);
        errorBox->setFont(font1);
        verticalLayout_4 = new QVBoxLayout(errorBox);
        verticalLayout_4->setSpacing(6);
        verticalLayout_4->setContentsMargins(11, 11, 11, 11);
        verticalLayout_4->setObjectName(QString::fromUtf8("verticalLayout_4"));
        errorText = new QLabel(errorBox);
        errorText->setObjectName(QString::fromUtf8("errorText"));
        sizePolicy.setHeightForWidth(errorText->sizePolicy().hasHeightForWidth());
        errorText->setSizePolicy(sizePolicy);

        verticalLayout_4->addWidget(errorText);

        horizontalLayout_3 = new QHBoxLayout();
        horizontalLayout_3->setSpacing(6);
        horizontalLayout_3->setObjectName(QString::fromUtf8("horizontalLayout_3"));
        horizontalLayout_3->setContentsMargins(-1, -1, -1, 0);
        horizontalSpacer_2 = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_3->addItem(horizontalSpacer_2);

        errorDismissButton = new QPushButton(errorBox);
        errorDismissButton->setObjectName(QString::fromUtf8("errorDismissButton"));

        horizontalLayout_3->addWidget(errorDismissButton);


        verticalLayout_4->addLayout(horizontalLayout_3);


        gridLayout->addWidget(errorBox, 4, 0, 1, 1);

        MainWindow->setCentralWidget(centralWidget);
        menuBar = new QMenuBar(MainWindow);
        menuBar->setObjectName(QString::fromUtf8("menuBar"));
        menuBar->setGeometry(QRect(0, 0, 826, 21));
        menuFile = new QMenu(menuBar);
        menuFile->setObjectName(QString::fromUtf8("menuFile"));
        MainWindow->setMenuBar(menuBar);
        mainToolBar = new QToolBar(MainWindow);
        mainToolBar->setObjectName(QString::fromUtf8("mainToolBar"));
        MainWindow->addToolBar(Qt::TopToolBarArea, mainToolBar);
        statusBar = new QStatusBar(MainWindow);
        statusBar->setObjectName(QString::fromUtf8("statusBar"));
        MainWindow->setStatusBar(statusBar);

        menuBar->addAction(menuFile->menuAction());
        menuFile->addAction(actionScanOpen);
        menuFile->addAction(actionScanSave);
        menuFile->addAction(actionScanSaveAs);
        menuFile->addSeparator();
        menuFile->addAction(actionClose);
        mainToolBar->addAction(actionAttach);
        mainToolBar->addAction(actionDetach);
        mainToolBar->addSeparator();

        retranslateUi(MainWindow);

        QMetaObject::connectSlotsByName(MainWindow);
    } // setupUi

    void retranslateUi(QMainWindow *MainWindow)
    {
        MainWindow->setWindowTitle(QApplication::translate("MainWindow", "PtScan", nullptr));
        actionScanOpen->setText(QApplication::translate("MainWindow", "Open Scan", nullptr));
        actionScanSave->setText(QApplication::translate("MainWindow", "Save Scan", nullptr));
#ifndef QT_NO_TOOLTIP
        actionScanSave->setToolTip(QApplication::translate("MainWindow", "Save Scan (CTRL+S)", nullptr));
#endif // QT_NO_TOOLTIP
        actionScanSaveAs->setText(QApplication::translate("MainWindow", "Save Scan As ...", nullptr));
        actionClose->setText(QApplication::translate("MainWindow", "Close", nullptr));
        actionAttach->setText(QApplication::translate("MainWindow", "Attach", nullptr));
#ifndef QT_NO_TOOLTIP
        actionAttach->setToolTip(QApplication::translate("MainWindow", "Attach to an open process", nullptr));
#endif // QT_NO_TOOLTIP
        actionDetach->setText(QApplication::translate("MainWindow", "Detach", nullptr));
        actionAddFilter->setText(QApplication::translate("MainWindow", "Add Filter", nullptr));
#ifndef QT_NO_SHORTCUT
        actionAddFilter->setShortcut(QApplication::translate("MainWindow", "Ctrl+F", nullptr));
#endif // QT_NO_SHORTCUT
        actionRemoveFilter->setText(QApplication::translate("MainWindow", "Remove Filter", nullptr));
#ifndef QT_NO_TOOLTIP
        actionRemoveFilter->setToolTip(QApplication::translate("MainWindow", "Remove the selected filter", nullptr));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_SHORTCUT
        actionRemoveFilter->setShortcut(QApplication::translate("MainWindow", "Del", nullptr));
#endif // QT_NO_SHORTCUT
        actionScan->setText(QApplication::translate("MainWindow", "Scan", nullptr));
#ifndef QT_NO_SHORTCUT
        actionScan->setShortcut(QApplication::translate("MainWindow", "Return", nullptr));
#endif // QT_NO_SHORTCUT
        actionScanCancel->setText(QApplication::translate("MainWindow", "Cancel Scan", nullptr));
        actionScanReset->setText(QApplication::translate("MainWindow", "Reset Scan", nullptr));
        scanResultsBox->setTitle(QApplication::translate("MainWindow", "Scan Results", nullptr));
        options->setTitle(QApplication::translate("MainWindow", "Options", nullptr));
        filtersBox->setTitle(QApplication::translate("MainWindow", "Filters", nullptr));
        scanReset->setText(QApplication::translate("MainWindow", "Reset", nullptr));
        scan->setText(QApplication::translate("MainWindow", "Scan", nullptr));
        addFilterButton->setText(QApplication::translate("MainWindow", "Add", nullptr));
        addressListBox->setTitle(QApplication::translate("MainWindow", "Address List", nullptr));
        scanCancel->setText(QApplication::translate("MainWindow", "Cancel", nullptr));
        processInfo->setText(QApplication::translate("MainWindow", "processInfo", nullptr));
        errorBox->setTitle(QApplication::translate("MainWindow", "Error", nullptr));
        errorText->setText(QApplication::translate("MainWindow", "errorText", nullptr));
        errorDismissButton->setText(QApplication::translate("MainWindow", "Dismiss", nullptr));
        menuFile->setTitle(QApplication::translate("MainWindow", "File", nullptr));
    } // retranslateUi

};

namespace Ui {
    class MainWindow: public Ui_MainWindow {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_MAINWINDOW_H
