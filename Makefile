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
	for F in *.adb ; do $(MAKE) `basename $$F .adb` ; done

# Remove working files

clean: ada_mk_clean
	for F in *.adb ; do rm -f `basename $$F .adb` ; done

reallyclean: clean ada_mk_reallyclean
	rm -rf *.dll

distclean: reallyclean ada_mk_distclean
