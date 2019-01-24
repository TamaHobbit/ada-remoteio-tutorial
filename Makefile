# Makefile for building Ada Remote I/O example programs

ifneq ($(BOARDNAME),)
# Cross compile for MuntsOS
EMBLINUXBASE	?= $(HOME)/arm-linux-mcu
include $(EMBLINUXBASE)/include/$(BOARDNAME).mk
else
# Native compile for Linux (e.g. Raspbian)
LIBSIMPLEIO	?= /usr/local/share/libsimpleio
endif

include $(LIBSIMPLEIO)/ada/include/ada.mk
include $(LIBSIMPLEIO)/ada/include/remoteio.mk

# Compile the test programs

default:
ifneq ($(GNAT),)
ifeq ($(OS), Windows_NT)
	$(MAKE) hidapi.dll
endif
endif
	for F in *.adb ; do $(MAKE) `basename $$F .adb` ; done

# Remove working files

clean: ada_mk_clean
	for F in *.adb *.dll ; do rm -f `basename $$F .adb` ; done

reallyclean: clean ada_mk_reallyclean

distclean: reallyclean ada_mk_distclean
