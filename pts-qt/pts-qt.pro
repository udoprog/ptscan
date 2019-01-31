TARGET = pts-qt
TEMPLATE = app

CONFIG(release, debug|release) {
  LIBS += $$PWD/../target/release/ptscan_c.lib
}

CONFIG(debug, debug|release) {
  LIBS += $$PWD/../target/debug/ptscan_c.lib
}

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

HEADERS += \
    $$PWD/../pts-cpp/pts/Exception.h \
    $$PWD/../pts-cpp/pts/Filter.h \
    $$PWD/../pts-cpp/pts/Pointer.h \
    $$PWD/../pts-cpp/pts/ProcessHandle.h \
    $$PWD/../pts-cpp/pts/Scan.h \
    $$PWD/../pts-cpp/pts/String.h \
    $$PWD/../pts-cpp/pts/System.h \
    $$PWD/../pts-cpp/pts/ThreadPool.h \
    $$PWD/../pts-cpp/pts/Token.h \
    $$PWD/../pts-cpp/pts/Values.h \
    $$PWD/../pts-cpp/pts/Watch.h \
    $$PWD/../pts-cpp/pts/Value.h \
    $$PWD/../pts-cpp/pts/Address.h

SOURCES += \
    $$PWD/../pts-cpp/pts/Exception.cpp \
    $$PWD/../pts-cpp/pts/Filter.cpp \
    $$PWD/../pts-cpp/pts/Pointer.cpp \
    $$PWD/../pts-cpp/pts/ProcessHandle.cpp \
    $$PWD/../pts-cpp/pts/Scan.cpp \
    $$PWD/../pts-cpp/pts/String.cpp \
    $$PWD/../pts-cpp/pts/System.cpp \
    $$PWD/../pts-cpp/pts/ThreadPool.cpp \
    $$PWD/../pts-cpp/pts/Token.cpp \
    $$PWD/../pts-cpp/pts/Values.cpp \
    $$PWD/../pts-cpp/pts/Watch.cpp \
    $$PWD/../pts-cpp/pts/Value.cpp \
    $$PWD/../pts-cpp/pts/Address.cpp
