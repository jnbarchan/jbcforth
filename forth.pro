TEMPLATE = app
TARGET = forth
CONFIG += console
CONFIG -= app_bundle
CONFIG -= qt

QMAKE_CFLAGS_WARN_ON = -Werror -Wreturn-type -Wunused-function -Wmissing-prototypes
QMAKE_CFLAGS += -std=gnu99
QMAKE_CFLAGS += -m64
QMAKE_CFLAGS_DEBUG -= -g
QMAKE_CFLAGS_DEBUG += -g3
QMAKE_LFLAGS += -m64

LIBS += -lreadline

HEADERS += defs.h external.h

SOURCES += external.c forth.c 

DISTFILES += play.fth random.fth tests.fth utils.fth empty.fth .forthrc
