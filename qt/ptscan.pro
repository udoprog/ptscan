#-------------------------------------------------
#
# Project created by QtCreator 2019-01-25T06:17:15
#
#-------------------------------------------------

QT       += core gui

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

TARGET = ptscan
TEMPLATE = app

LIBS += $$PWD/../../repo/ptscan/target/release/ptscan_c.lib
INCLUDEPATH += $$PWD/../../repo/ptscan/c/include
DEPENDPATH += $$PWD/../../repo/ptscan/c/include
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
    main.cpp \
    mainwindow.cpp \
    openprocess.cpp \
    pts.cpp \
    pts/ThreadPool.cpp \
    pts/String.cpp \
    pts/ProcessHandle.cpp \
    pts/Scanner.cpp

HEADERS += \
    mainwindow.h \
    openprocess.h \
    pts.h \
    pts/ThreadPool.h \
    pts/String.h \
    pts/ProcessHandle.h \
    pts/Scanner.h

FORMS += \
        mainwindow.ui \
    openprocess.ui

# Default rules for deployment.
qnx: target.path = /tmp/$${TARGET}/bin
else: unix:!android: target.path = /opt/$${TARGET}/bin
!isEmpty(target.path): INSTALLS += target

win32:CONFIG(release, debug|release): LIBS += -L$$PWD/../../repo/ptscan/target/release/ -lptscan_c
else:win32:CONFIG(debug, debug|release): LIBS += -L$$PWD/../../repo/ptscan/target/debug/ -lptscan_c
else:unix: LIBS += -L$$PWD/../../repo/ptscan/target/ -lptscan_c
