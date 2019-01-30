TARGET = pts-qt
TEMPLATE = app

LIBS += $$PWD/../target/release/ptscan_c.lib
LIBS += $$PWD/../pts-cpp/release/pts-cpp.lib
LIBS += -lws2_32 -lpsapi -lkernel32 -ladvapi32 -luserenv -lntdll -ldbghelp

QT += core gui concurrent
greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

INCLUDEPATH += $$PWD/../pts-cpp/
INCLUDEPATH += $$PWD/../pts-c/include/

DEFINES += QT_DEPRECATED_WARNINGS
DEFINES += PTS_QT
CONFIG += c++17

SOURCES += \
    main.cpp \
    mainwindow.cpp \
    openprocess.cpp \
    addresslist.cpp \
    scanresults.cpp \
    editaddress.cpp \
    editfilter.cpp

HEADERS += \
    mainwindow.h \
    openprocess.h \
    addresslist.h \
    scanresults.h \
    editaddress.h \
    editfilter.h

FORMS += \
    mainwindow.ui \
    openprocess.ui \
    addresslist.ui \
    scanresults.ui \
    editaddress.ui \
    editfilter.ui
