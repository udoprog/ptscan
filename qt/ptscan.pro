#-------------------------------------------------
#
# Project created by QtCreator 2019-01-25T06:17:15
#
#-------------------------------------------------

QT       += core gui concurrent

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

TARGET = ptscan
TEMPLATE = app

LIBS += $$PWD/../target/release/ptscan_c.lib
INCLUDEPATH += $$PWD/../c/include/
DEPENDPATH += $$PWD/../c/include/
LIBS += -lws2_32 -lpsapi -lkernel32 -ladvapi32 -luserenv -lntdll -ldbghelp

# The following define makes your compiler emit warnings if you use
# any feature of Qt which has been marked as deprecated (the exact warnings
# depend on your compiler). Please consult the documentation of the
# deprecated API in order to know how to port your code away from it.
DEFINES += QT_DEPRECATED_WARNINGS

# You can also make your code fail to compile if you use deprecated APIs.
# In order to do so, uncomment the following line.
# You can also select to disable deprecated APIs only up to a certain version of Qt.
#DEFINES += QT_DISABLE_DEPRECATED_BEFORE=0x060000    # disables all the APIs deprecated before Qt 6.0.0

CONFIG += c++17

SOURCES += \
    pts.cpp \
    pts/ThreadPool.cpp \
    pts/String.cpp \
    pts/ProcessHandle.cpp \
    pts/Filter.cpp \
    pts/Scan.cpp \
    pts/Token.cpp \
    pts/Watch.cpp \
    pts/Values.cpp \
    pts/Pointer.cpp \
    main.cpp \
    mainwindow.cpp \
    openprocess.cpp \
    addresslist.cpp \
    scanresults.cpp \
    editaddress.cpp \
    editfilter.cpp \
    validator.cpp

HEADERS += \
    pts.h \
    pts/ThreadPool.h \
    pts/String.h \
    pts/ProcessHandle.h \
    pts/Filter.h \
    pts/Scan.h \
    pts/Token.h \
    pts/Watch.h \
    pts/Values.h \
    pts/Pointer.h \
    mainwindow.h \
    openprocess.h \
    addresslist.h \
    scanresults.h \
    editaddress.h \
    editfilter.h \
    validator.h

FORMS += \
    mainwindow.ui \
    openprocess.ui \
    addresslist.ui \
    scanresults.ui \
    editaddress.ui \
    editfilter.ui

# Default rules for deployment.
qnx: target.path = /tmp/$${TARGET}/bin
else: unix:!android: target.path = /opt/$${TARGET}/bin
!isEmpty(target.path): INSTALLS += target
