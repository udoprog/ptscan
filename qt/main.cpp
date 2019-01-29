#include "mainwindow.h"
#include <QApplication>
#include <pts.h>

int main(int argc, char *argv[])
{
    qDebug() << "Loaded ptscan version:" << QString::fromLocal8Bit(pts_version());

    auto threadPool = pts::ThreadPool::create();

    QApplication a(argc, argv);
    MainWindow w(threadPool);
    w.show();

    return a.exec();
}
