TARGET = pts-cpp
TEMPLATE = lib
CONFIG += staticlib

LIBS += $$PWD/../target/release/ptscan_c.lib
LIBS += -lws2_32 -lpsapi -lkernel32 -ladvapi32 -luserenv -lntdll -ldbghelp

INCLUDEPATH += $$PWD/../pts-c/include/
DEPENDPATH += $$PWD/../pts-c/include/

DEFINES += PTS_QT
CONFIG += c++17

HEADERS += \
    pts/Exception.h \
    pts/Filter.h \
    pts/Pointer.h \
    pts/ProcessHandle.h \
    pts/Scan.h \
    pts/String.h \
    pts/System.h \
    pts/ThreadPool.h \
    pts/Token.h \
    pts/Values.h \
    pts/Watch.h

SOURCES += \
    pts/Exception.cpp \
    pts/Filter.cpp \
    pts/Pointer.cpp \
    pts/ProcessHandle.cpp \
    pts/Scan.cpp \
    pts/String.cpp \
    pts/System.cpp \
    pts/ThreadPool.cpp \
    pts/Token.cpp \
    pts/Values.cpp \
    pts/Watch.cpp
