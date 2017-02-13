TEMPLATE = app
TARGET = forth
CONFIG += console
CONFIG -= app_bundle
CONFIG -= qt

HEADERS += defs.h external.h

SOURCES += external.c forth.c 

SOURCES += play.fth random.fth tests.fth utils.fth
