#
# Makefile for Nautilus (unix)
#
# SCCS ID: @(#)makefile.unx 1.14 96/05/25
#

# default C compiler
CC= gcc
#COPT= -O
COPT= -O

# default compile flags
#WARNINGS = -Wunused -Wswitch -Wformat -Wchar-subscripts
WARNINGS = -Wall
CFLAGS   = -pipe $(COPT) $(WARNINGS)

# Default install Locations
BINDIR = /usr/local/bin
MANDIR = /usr/local/man 

# blank separated list of NTP class module base names
NTP_CLASSES=	ntp_udp ntp_modm

# RSAREF should point to your RSAREF 2.0 base directory
RSAREF=rsaref

# default libraries
LIBS=$(RSAREF)/rsaref.a -Llpc10 -llpc10 -lm

# nautilus modules
NAUTILUS_OBJ=	bitstrm.o	\
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
		unix.o		\
		upsample.o	\
		util.o		\
		versions.o	\
		nsp.o		\
		ntp.o		\
		ntp_tab.o	\
		${NTP_CLASSES:%=%.o}


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
	@echo "       make <system>"
	@echo ""
	@echo "where <system> can be:"
	@echo "       sol1gcc, sol2gcc, or linux"
	@echo ""
	@echo "or type:"
	@echo "       make clean"
	@echo "to rebuild from scratch"
	@echo ""
	@echo "then type:"
	@echo "       make install"
	@echo "to install to /usr/local/bin and /usr/local/man/man1"
	@echo ""

# Sun Sparcstation with gcc
sol1gcc: $(RSAREF)/rsaref.a
	$(MAKE) nautilus nuke unnuke \
	CFLAGS="-pipe $(COPT) $(WARNINGS) -I/usr/demo/SOUND -I$(RSAREF) \
	-DUNIX -DORDER_ABCD -DSP85 -DAUDIO_DEVICE_8KHZ_ONLY -DCLIP" \
	LIBS="$(RSAREF)/rsaref.a -Llpc10 -llpc10 -L/usr/demo/SOUND/lib \
	-laudio -lm"

# Sun Sparcstation running Solaris 2.x and gcc
sol2gcc: $(RSAREF)/rsaref.a
	$(MAKE) nautilus nuke unnuke \
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
linux:  $(RSAREF)/rsaref.a
	$(MAKE) nautilus \
	CFLAGS="-pipe $(COPT) $(WARNINGS) -DUNIX -DORDER_DCBA -DCLIP \
	-I$(RSAREF)" 

nautilus: $(RSAREF)/rsaref.a liblpc10.a $(NAUTILUS_OBJ)
	$(CC) -o nautilus $(NAUTILUS_OBJ) $(LIBS)

ntp_tab.o: ntp.h Makefile
	$(CC) -DNTP_CLASSES="${NTP_CLASSES:%=X(%)}" -c ntp_tab.c

nuke: liblpc10.a $(NUKE_OBJ)
	$(CC) -o nuke $(NUKE_OBJ) $(LIBS)

unnuke:	nuke
	-rm -f unnuke
	ln nuke unnuke

$(RSAREF)/rsaref.a:
	(cd rsaref; $(MAKE) -f makefile.unx)

liblpc10.a:
	(cd lpc10; $(MAKE) -f makefile.unx NAUTILUS_HOME=..)

clean:
	-rm -f core $(NAUTILUS_OBJ) $(NUKE_OBJ) liblpc10.a nautilus nuke unnuke
	(cd rsaref; $(MAKE) -f makefile.unx clobber)

install:
	cp nautilus $(BINDIR)
	cp doc/nautilus.1 $(MANDIR)/man1

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
