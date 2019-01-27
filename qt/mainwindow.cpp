#include <iostream>

#include <QApplication>
#include <QDesktopWidget>
#include <pts.h>

#include "mainwindow.h"
#include "openprocess.h"
#include "ui_mainwindow.h"

MainWindow::MainWindow(std::shared_ptr<pts::ThreadPool> threadPool, QWidget *parent) :
    QMainWindow(parent),
    threadPool(threadPool),
    ui(new Ui::MainWindow),
    openProcess(new OpenProcess(this))
{
    ui->setupUi(this);

    connect(ui->actionAttach, &QAction::triggered, this, &MainWindow::attachTriggered);
    connect(openProcess, &QDialog::accepted, this, &MainWindow::attachAccepted);
}

MainWindow::~MainWindow()
{
    delete openProcess;
    delete ui;
}

void MainWindow::attachTriggered() {
    openProcess->refreshList();

    const QRect availableGeometry = QApplication::desktop()->availableGeometry(openProcess);
    openProcess->resize(availableGeometry.width() / 3, availableGeometry.height() * 2 / 3);
    openProcess->move((availableGeometry.width() - openProcess->width()) / 2,
                (availableGeometry.height() - openProcess->height()) / 2);

    openProcess->show();
}

void MainWindow::attachAccepted() {
    std::cout << "accepted: " << openProcess->selected->name() << std::endl;
}
