#include "mainwindow.h"
#include <QApplication>
#include <pts.h>

int main(int argc, char *argv[])
{
    auto threadPool = pts::ThreadPool::create();

    QApplication a(argc, argv);
    MainWindow w(threadPool);
    w.show();

    return a.exec();
}
