# Makefile for building Ada Remote I/O example programs

# These targets are not files

.PHONY: default clean reallyclean distclean

# Find where libsimpleio is installed

ifneq ($(wildcard $(HOME)/libsimpleio),)
LIBSIMPLEIO	?= $(HOME)/libsimpleio
endif

ifneq ($(wildcard /usr/local/share/libsimpleio),)
LIBSIMPLEIO	?= /usr/local/share/libsimpleio
endif

# Define a pattern rule to build an Ada Remote I/O tutorial test program using
# a common project file tutorial.gpr.  This will override the rule in ada.mk.

%:
	$(GPRBUILD) tutorial.gpr $(GPRBUILDFLAGS) $@ -cargs $(GPRBUILDCFLAGS) -largs $(GPRBUILDLDFLAGS)

include $(LIBSIMPLEIO)/ada/include/ada.mk
include $(LIBSIMPLEIO)/ada/include/remoteio.mk

# Compile the test programs

default:
ifeq ($(OS), Windows_NT)
ifneq ($(GNAT),)
	$(MAKE) hidapi.dll
endif
endif
	for F in test*.adb ; do $(MAKE) `basename $$F .adb` ; done

# Remove working files

clean: ada_mk_clean
	for F in test*.adb ; do rm -f `basename $$F .adb` ; done

reallyclean: clean ada_mk_reallyclean
ifeq ($(OS), Windows_NT)
ifneq ($(GNAT),)
	rm -f hidapi.dll
endif
endif

distclean: reallyclean ada_mk_distclean
