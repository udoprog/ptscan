#include "mainwindow.h"
#include <QApplication>
#include <pts.h>

int main(int argc, char *argv[])
{
    pts_setup();
    qDebug() << QString("Loaded ptscan version: %1").arg(QString::fromLocal8Bit(pts_version()));

    auto threadPool = pts::ThreadPool::create();

    QApplication a(argc, argv);
    MainWindow w(threadPool);
    w.show();

    return a.exec();
}
