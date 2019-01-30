TEMPLATE = subdirs

SUBDIRS += pts-cpp pts-qt
pts-qt.subdir = ../pts-qt
pts-qt.depends = pts-cpp
pts-cpp.subdir = ../pts-cpp