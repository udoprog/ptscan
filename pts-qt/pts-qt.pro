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
    editfilter.cpp \
    typecombobox.cpp

HEADERS += \
    mainwindow.h \
    openprocess.h \
    addresslist.h \
    scanresults.h \
    editaddress.h \
    editfilter.h \
    typecombobox.h

FORMS += \
    mainwindow.ui \
    openprocess.ui \
    addresslist.ui \
    scanresults.ui \
    editaddress.ui \
    editfilter.ui

HEADERS += \
    ../pts-cpp/pts/Exception.h \
    ../pts-cpp/pts/Filter.h \
    ../pts-cpp/pts/Pointer.h \
    ../pts-cpp/pts/ProcessHandle.h \
    ../pts-cpp/pts/Scan.h \
    ../pts-cpp/pts/String.h \
    ../pts-cpp/pts/System.h \
    ../pts-cpp/pts/ThreadPool.h \
    ../pts-cpp/pts/Token.h \
    ../pts-cpp/pts/Watch.h \
    ../pts-cpp/pts/Value.h \
    ../pts-cpp/pts/Values.h \
    ../pts-cpp/pts/Address.h \
    ../pts-cpp/pts/Addresses.h

SOURCES += \
    ../pts-cpp/pts/Exception.cpp \
    ../pts-cpp/pts/Filter.cpp \
    ../pts-cpp/pts/Pointer.cpp \
    ../pts-cpp/pts/ProcessHandle.cpp \
    ../pts-cpp/pts/Scan.cpp \
    ../pts-cpp/pts/String.cpp \
    ../pts-cpp/pts/System.cpp \
    ../pts-cpp/pts/ThreadPool.cpp \
    ../pts-cpp/pts/Token.cpp \
    ../pts-cpp/pts/Values.cpp \
    ../pts-cpp/pts/Watch.cpp \
    ../pts-cpp/pts/Value.cpp \
    ../pts-cpp/pts/Address.cpp \
    ../pts-cpp/pts/Addresses.cpp
