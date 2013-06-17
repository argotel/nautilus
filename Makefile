#
# Makefile for Nautilus (unix)
#
# SCCS ID: @(#)makefile.unx 1.14 96/05/25
#
MAJOR  = 1
MINOR  = 8
MMINOR = rc5
VERSION = $(MAJOR).$(MINOR)$(MMINOR)
VERSIONNAME = nautilus-$(MAJOR).$(MINOR)$(MMINOR)
RPMDIR = /tmp/$(VERSIONNAME)-build

#XCOMP=/usr/bin/i586-mingw32msvc-
# default C compiler
CC=$(XCOMP)gcc
AR=$(XCOMP)ar
LD=$(XCOMP)ld
RANLIB=$(XCOMP)ranlib
export CC
export AR
export LD
export RANLIB
#COPT= -O
COPT= -O


# default compile flags
#WARNINGS = -Wunused -Wswitch -Wformat -Wchar-subscripts
WARNINGS = -Wall
CFLAGS   = -pipe $(COPT) $(WARNINGS)

# Default install Locations
BINDIR = /usr/local/bin
MANDIR = /usr/local/man 
SHAREDIR = /usr/share/nautilus-securephone

# blank separated list of NTP class module base names
NTP_CLASSES=	ntp_udp ntp_modm

# RSAREF should point to your RSAREF 2.0 base directory
RSAREF=rsaref
LPC10=lpc10

INCLUDES+=-I$(RSAREF) -I$(LPC10)
export INCLUDES 

# default libraries
LDFLAGS=-L.
export LDFLAGS
LIBS+= -lrsaref -llpc10 -lm
export LIBS

# nautilus modules
NAUTILUS_COMM_OBJ=	bitstrm.o	\
		cli.o		\
		comm.o		\
		config.o 	\
		crc.o		\
		crypt.o		\
		des3.o		\
		dhparams.o	\
		downsmpl.o	\
		idea.o		\
		init.o		\
		lpc10.o		\
		naut_bf.o	\
		sha.o           \
		sp64.o		\
		sp85.o		\
		sp2bits.o	\
		talk.o		\
		upsample.o	\
		util.o		\
		versions.o	\
		nsp.o		\
		ntp.o		\
		ntp_tab.o	\
		${NTP_CLASSES:%=%.o}


NAUTILUS_WIN_OBJ=	\
			win32.o \
			win32ai.o \
			win32ao.o 

NAUTILUS_UNX_OBJ= unix.o		

NAUTILUS_ALL_OBJ=$(NAUTILUS_COMM_OBJ) \
		$(NAUTILUS_WIN_OBJ) \
		$(NAUTILUS_UNX_OBJ)


# nuke & unnuke modules
NUKE_OBJ=	sp64.o		\
		sp85.o		\
		sp3bits.o	\
		sp2bits.o	\
		lpc10.o		\
		bitstrm.o	\
		downsmpl.o	\
		upsample.o	\
		nuke.o

all:
	@echo ""
	@echo "To build Nautilus for your flavour of Unix, type:"
	@echo "       make linux"
	@echo ""
	@echo "To build for Windows x86 (whith mingw), set environment variable "
	@echo "		XCOMP to /usr/bin/i586-mingw32msvc-"
	@echo ""
	@echo "		make windows"
	@echo ""
	@echo "or type:"
	@echo "       make clean"
	@echo "to rebuild from scratch"
	@echo ""
	@echo "then type:"
	@echo "       make install"
	@echo "to install to /usr/local/bin and /usr/local/man/man1"
	@echo ""

# NO LONGER MAINTAINED
# Sun Sparcstation with gcc
sol1gcc: $(RSAREF)/rsaref.a
	$(MAKE) nautilus nuke unnuke \
	CFLAGS="-pipe $(COPT) $(WARNINGS) -I/usr/demo/SOUND -I$(RSAREF) \
	-DUNIX -DORDER_ABCD -DSP85 -DAUDIO_DEVICE_8KHZ_ONLY -DCLIP" \
	LIBS="$(RSAREF)/rsaref.a -Llpc10 -llpc10 -L/usr/demo/SOUND/lib \
	-laudio -lm"

# NO LONGER MAINTAINED
# Sun Sparcstation running Solaris 2.x and gcc
sol2gcc: $(RSAREF)/rsaref.a
	$(MAKE) nautilus nuke unnuke \
	NAUTILUS_OBJ="$(NAUTILUS_COMM_OBJ) $(NAUTILUS_UNX_OBJ)" \
	CFLAGS="-pipe $(COPT) $(WARNINGS) -I/usr/demo/SOUND/include \
	-I$(RSAREF) -DUNIX -DORDER_ABCD -DSOLARIS \
	-DSP124 -DAUDIO_DEVICE_8KHZ_ONLY -DCLIP" \
	LIBS="$(RSAREF)/rsaref.a -Llpc10 -llpc10 -L/usr/demo/SOUND/lib \
	-laudio -lsocket -lnsl -lm"
#	-laudio -lsocket -L/usr/ucblib -lucb -lnsl -lm"

# Linux with sound driver version 1.99.x or newer
#
# Mon Jul 10 01:15:39 CDT 1995    Andy Fingerhut (jaf@arl.wustl.edu)
#
# I know that version 2.4 or newer of the sound driver is acceptable,
# but I don't have any documentation on older versions of the sound
# driver, or examples of it to try, to see if 1.99.x will work.
#
# The option -DFIXED_POINT_ARITHMETIC could be added to the list of
# CFLAGS below after zmul.asm is converted to work under Linux.
# I've started looking at the problem in some detail, but haven't
# yet completed it.  Help is welcome.  See the file zlib-gcc.c if
# you are interested.

# This assumes you are running linux on the Intel platform.
# Anyone who gets other platforms to work under linux, please send email.
linux:  
	$(MAKE) librsaref.a RANLIB=$(RANLIB)
	$(MAKE) liblpc10.a  RANLIB=$(RANLIB)
	$(MAKE) nautilus  RANLIB=$(RANLIB) \
	NAUTILUS_OBJ="$(NAUTILUS_COMM_OBJ) $(NAUTILUS_UNX_OBJ)" \
	CFLAGS+=" -DUNIX -DORDER_DCBA -DCLIP $(INCLUDES)"

windows: bin2array $(XCOMP)dlltool 
	# runs only if XCOMP ist set to the mingw prefix!
	./bin2array "uint8_t logonSoundData "< logon_new.v >winsounds.h 	
	./bin2array "uint8_t ringSoundData "< ring_new.v >>winsounds.h 	
	$(MAKE) librsaref.a  CC=$(CC)  LD=$(LD)  AR=$(AR)  RANLIB=$(RANLIB) \
	#CFLAGS+="-D_WIN32"
	#		CFLAGS+="-DWIN32 -DDNDEBUG -DWINDOWS" 
	$(MAKE) liblpc10.a   CC=$(CC)  LD=$(LD)  AR=$(AR)  RANLIB=$(RANLIB)
	$(MAKE) nautilus  \
	NAUTILUS_OBJ="$(NAUTILUS_COMM_OBJ) $(NAUTILUS_WIN_OBJ)" \
	CFLAGS=" -DWIN32 -DDEBUG -D_CONSOLE -D_MBCS -DORDER_DCBA -DCLIP $(INCLUDES)" \
	LIBS+=-liberty LDFLAGS+=-L/usr/i586-mingw32msvc/lib \
	LIBS+="-lwsock32 -lwinmm" 
	mv nautilus nautilus.exe

# recursive Call of this Makefile with CC possibly redefined.
nautilus: $(NAUTILUS_OBJ)
	echo "submake CC= $(CC)" 
	$(CC) -o nautilus $(NAUTILUS_OBJ) $(LDFLAGS) $(LIBS)

ntp_tab.o: ntp.h Makefile
	$(CC) -DNTP_CLASSES="${NTP_CLASSES:%=X(%)}" -c ntp_tab.c

nuke: liblpc10.a $(NUKE_OBJ)
	$(CC) -o nuke $(NUKE_OBJ) $(LIBS)

unnuke:	nuke
	-rm -f unnuke
	ln nuke unnuke

librsaref.a: 
	$(MAKE) -C rsaref -f makefile.unx 
	mv rsaref/rsaref.a librsaref.a

liblpc10.a:
	$(MAKE) -C lpc10 -f makefile.unx NAUTILUS_HOME=.. 
	mv lpc10/liblpc10.a liblpc10.a

# The zip file is no longer needed as we build a standalone exe file
nautilus.zip: /usr/share/doc/mingw32-runtime/mingwm10.dll.gz nautilus
	gunzip -dc /usr/share/doc/mingw32-runtime/mingwm10.dll.gz >mingwm10.dll
	export DIR=basename $(shell pwd); \
	(export DIR=`basename $$PWD`; \
	 cd ..; zip nautilus.zip $${DIR}/mingwm10.dll $${DIR}/ring_new.v \
		$${DIR}/logon_new.v $${DIR}/nautilus.exe)

# if in a working copy: pack "nautilus" else "nautilus-something"

nautilus.linux.zip: nautilus
	(export DIR=`basename $$PWD`; \
	cd ..; rm nautilus.linux.zip; \
	zip nautilus.linux.zip $${DIR}/ring_new.v \
	$${DIR}/logon_new.v $${DIR}/nautilus )


clean:
	-rm -f core $(NAUTILUS_ALL_OBJ) $(NUKE_OBJ) 
	-rm -f liblpc10.a librsaref.a nautilus nuke unnuke nautilus.zip
	(cd rsaref; $(MAKE) -f makefile.unx clobber)
	(cd lpc10; $(MAKE) -f makefile.unx clobber)

install-binary:
	cp nautilus $(BINDIR)

install: install-binary
	cp doc/nautilus.1 $(MANDIR)/man1
	cp -p logon_new.v ring_new.v $(SHAREDIR)

release-tag:
	cvs tag -R v$(MAJOR)_$(MINOR)_$(MMINOR)

release-tag-force:
	cvs tag -R -F v$(MAJOR)_$(MINOR)_$(MMINOR)

source-release-dir:
	mkdir -p tmp
	cvs export -d tmp -r v$(MAJOR)_$(MINOR)_$(MMINOR) nautilus
	mv tmp $(VERSIONNAME)

source-release: source-release-dir
	tar -czf $(VERSIONNAME).tar.gz $(VERSIONNAME)
	rm -rf $(VERSIONNAME)

rpm-dirs:
	mkdir -p $(RPMDIR)/{SOURCES,SRPMS,RPMS,BUILD,SPECS}

debian-binary:
	fakeroot debian/rules clean
	fakeroot debian/rules binary
	
src-rpm: source-release rpm-dirs
	cp $(VERSIONNAME).tar.gz $(RPMDIR)/SOURCES
	cp RPM/nautilus.spec $(RPMDIR)/SPECS
	rpmbuild -bs RPM/nautilus.spec	

bin-rpm: src-rpm
	rpmbuild -bb RPM/nautilus.spec

# Dependencies
nautilus.h: machine.h
bitstrm.o: machine.h
cli.o: nautilus.h
comm.o: nautilus.h $(RSAREF)/rsaref.h $(RSAREF)/global.h
config.o: nautilus.h
crc.o: machine.h
crypt.o: nautilus.h blowfish.h des3.h
crc.o: machine.h
des3.o: des3.h
dhparams.o: $(RSAREF)/rsaref.h $(RSAREF)/global.h
downsmpl.o:
init.o: nautilus.h
naut_bf.o: nautilus.h blowfish.h naut_bf.h
nuke.o: machine.h
nuke2.o: nautilus.h
sp1bit.o: machine.h fixfloat.h
sp2bits.o: machine.h fixfloat.h
sp64.o: sp1bit.c machine.h fixfloat.h
sp85.o: sp1bit.c machine.h fixfloat.h
talk.o: nautilus.h
unix.o: machine.h nautilus.h
util.o: nautilus.h
versions.o: nautilus.h
nsp.o: nsp.h ntp.h
ntp.o: ntp.h
${NTP_CLASSES:%=%.o}: ntp.h
win32ai.o: win32ai.c win32a.h
win32ao.o: win32ao.c win32a.h
win32.o:  win32.c
