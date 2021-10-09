#
#	ALE Forth Version 2.04
#
#		Copyright (c) 1991,1992 by Lutz Sammer.
#
#		Bug-reports via usenet: johns
#
#	File:	makefile (makefile.unix)
#
#		Makefile for the ALE forth kernel under UNIX.
#
################################################################################
#
#	make
#		Update all files.
#
#	make fth.new
#		Test a new version.
#
#

#	Insert your favorit C compiler
#CC=	cc
CC=	gcc

CFLAGS=	-O

#	Insert your linker
LD=	ld

LDFLAGS=

#	Insert your name of the GNU assembler
GAS=	gas

################################################################################

all:	fth.new

#	Convert block file -> line file
b2l:	b2l.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ b2l.c

#	Convert line file -> block file
l2b:	l2b.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ l2b.c

################################################################################
#	Forth Kernel
################################################################################

SOURCES= kernel.4th \
	variable.4th \
	config.4th primitiv.4th stacks.4th math.4th execute.4th mem.4th \
	unix.4th float.4th \
	dict.4th blkfile.4th compiler.4th control.4th error.4th \
	in-out.4th internal.4th intrpret.4th numeric.4th search.4th \
	strings.4th tools.4th coldbye.4th user.4th

fth.s:	ftc.4th $(SOURCES)
	fth -d 131072 -i ftc.4th -i kernel.4th > $@

fth.o:	fth.s
	$(GAS) -o $@ $*.s 

startup.s: ftc.4th startup.4th
	fth -d 131072 -i ftc.4th -i startup.4th > $@

startup.o:	startup.s
	$(GAS) -o $@ $*.s 

#	Ale forth executible
fth.new:	startup.o fth.o
	$(LD) -o $@ startup.o fth.o -lc

################################################################################

#	Block file from the text file
default.blk:	force	# default.line
	l2b <default.line >default.blk

#	Text file from the block file
default.line:	force	# default.blk
	b2l <default.blk >default.line

################################################################################

#	Files in the archives
#	$(UNIX) fth unix.4th
#	$(MSDOS) fth.exe msdos.4th
#
ARCHIVE= fth fth.exe init-4th.4th boot-4th.4th readme.fth default.blk COPYING \
	lib/*.4th default.line \
	makefile makefile.dos ftc.4th startup.4th kernel.4th $(SOURCES) msdos.4th \
	x.4th sieve.4th

#	Build zoo archive
alefth.zoo:
	-zoo ah alefth.zoo	$(ARCHIVE)

#	Build zip archive
alefth.zip:
#	-zip -9 alefth.zip	$(ARCHIVE)
	-zip -k alefth.zip	$(ARCHIVE)

################################################################################

#	Make executible from core
undump:	undump.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ undump.c

c-4th: test.o fth.o
	$(CC) -o $@ test.o fth.o -lc

clean:
	rm startup.s startup.o fth.o fth.s
