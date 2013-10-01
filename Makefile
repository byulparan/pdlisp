
ifndef PD_INCLUDE
$(error "You must set environment variable PD_INCLUDE")
else ifndef ECL_LIBS
$(error "You must set environment variable ECL_LIBS")
endif

OBJS = pdlisp.o
STATIC_LIB = pdlisp_core.a

UNAME := $(shell uname)

ifeq ($(UNAME),Darwin)
CFLAGS = -arch i386 -Wall -Wno-unused -Wno-parentheses -Wno-switch -I$(PD_INCLUDE) -I./ecl_h -std=gnu99
LDFLAGS = -bundle -undefined suppress -flat_namespace -arch i386 -lecl -leclffi -leclgmp -leclatomic -leclgc -lsockets -lasdf -L$(ECL_LIBS)
SUFFIX = pd_darwin

else ifeq ($(UNAME), Linux)
export C_INCLUDE_PATH=./ecl_h
CFLAGS = -Wall -Wno-unused -Wno-parentheses -Wno-switch -I$(PD_INCLUDE) -std=gnu99
LDFLAGS = -shared -lecl -leclffi -leclgmp -leclatomic -leclgc -lsockets -lasdf -lpthread -lm -L$(ECL_LIBS)
SUFFIX = pd_linux
endif

TARGET = pdlisp.$(SUFFIX)

all : $(TARGET)

$(TARGET): $(OBJS) $(STATIC_LIB)
	$(CC)  -o $(TARGET) $(OBJS) $(LDFLAGS) lib$(STATIC_LIB)

$(STATIC_LIB) : pdlisp_core.lisp
	ecl -norc -eval '(ext:install-c-compiler)' \
		  -eval '(compile-file "pdlisp_core" :system-p t)' \
		  -eval '(c:build-static-library "pdlisp_core" :lisp-files (list "pdlisp_core.o") :init-name "init_lib_PDLISP_CORE")' \
		  -eval '(quit)'

clean :
	rm $(TARGET) lib$(STATIC_LIB) $(OBJS) pdlisp_core.o
