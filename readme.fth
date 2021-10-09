#############################
# ALE FORTH - Version 2.04: #
#############################

History:
========

2.03:	Fri Feb 28 03:09:37 1992
-----
	Floating point word set added.
	Memory allocation word set added.
	Many bugs removed. 

2.04:	Fri Mar 20 01:37:55 1992
-----

Startup-sequence:
=================

	1.)
		Load Block 1 of default block file "default.blk".
	2.)
		Load forth boot file "boot-4th.4th"

	Next done by boot-4th.4th:
	3.)
		Interpret the command line.
	4.)
		If the command line has no option -f or doesn't do
		something like quit, abort or bye.
		Load forth init file "init-4th.4th"
	Next done by init-4th.4th:
	6.)
		Load some libraries:
	Back in the resident kernel:
	5.)
		Finaly show startup message and execute quit.

Command-line-options:
=====================

o	fth [-d n] [-i file] [-f] [other forth words]

o	-d n	Set the dictionary size to n bytes.

		The default dictionary size is 64K.
		Example:
			fth -d 131072
		Increase dictionary size to 132K.
			fth -d 32768
		Reduce dictionary size to 32K.

o	-i file	Include file

		The file is included before the file init-4th.4th is included.
		Same as:
			fth " file" included
		Example:
			fth -i lib/double.4th
		Include file "lib/double.4th".
		
o	-f	Fast.

		Don't include init-4th.4th.
		Example:
			fth -i init-4th.4th -i myfile.4th -f

		Include init-4th.4th and your source file. 
		The file init-4th.4th isn't included twice, because of -f.

o	-h	Help.

		Display a short help for the command line options.

o	Any other text on the command line:

		All words on the command line are executed as if you
		typed them from the keyboard.
		Example:
			fth 1 2 + . bye

		Add 1 to 2, display the result and leave forth.
		Same as: ( <text> text is what you have typed. ) 

			<fth> <return>
			Loading init-4th.4th 

			Ale Forth Version 2.04, Copyright 1991,1992 by ...
			CPU: i486 FPU: i487 Operating-system: unix
			Available dictionary space: 7043 
			<1 2 + .> <return>
			<bye> <return>
			Leaving forth.

		In the above example init-4th.4th isn't included and
		no startup message is shown.

o	Some examples:

		If you want to include init-4th.4th before your files use:
			fth -i init-4th.4th -i your-file -f

		If you want to run an application and exit use:
			fth -i application bye

		If you need a calculator:
			fth 1 2 + 3 * . bye

Special files:
==============

o	default.blk
		Default file for the block word set.  ( LIST, LOAD, ...)

		Format:
			A block is 16 lines a 64 characters.

		Special blocks:
			block 0
				can only be accessed if all buffers are used.
			block 1
				are loaded automaticly by the kernel
				at startup.
			block 2 - block 3
				Not assigned.
			block 4 - block 5
				Are reserved for error messages.
			block 6 ...
				Can be used by you.

		Usage:
			You can use this file to include your block sources.

o	boot-4th.4th
		First file loaded by the kernel.

		You can put the words, that you want to have always present   
		into this file.

o	init-4th.4th
		This file is loaded by boot-4th.4th.

		Use this file to extend the kernel.

o	lib	(directory)
		Directory for the library files.

		Change the string in the file "boot-4th.4th" in the word
		"INIT-LIBRARY" to your absolute path to this directory.
		You can access the files in this directory with the word
		"LIBRARY" and the command line option "-i".
		Example: s" double" library
			or
			 fth -i double

Configuration:
==============

	ftc.4th:
	--------
		/document
			if set to 1, the target compiler generates the
			document file from the source files.
			The documentation are included in the source file.
			All lines starting with \\ are copied to the 
			document file "fth.doc".  The document file can be
			used with the word HELP <name>.

		/verbose
			if set to 1, the target compiler includes compiler
			informations in the assembler output.

			Assembler:
			/ dup
				pushl	%ebx

		/view
			if set to 1, the target compiler generates the view
			file from the source files.  All words in the kernel
			produce a line in the view file "view.fth". 
			Format of the view file:
				word	file	line
			The view file can be used with the word VIEW <name>.

		/less-code
			if set to 1, the kernel use less code words.
			Can be used to port the Ale Forth to new hardware
			or other processor.
		
		buffer-output

		/ps=sp
			if set to 1, the processor stack is used as parameter
			stack and %esi is the return stack pointer.
			Code for +:
				popl	%eax
				addl	%eax, tos
				jmp	*(%esi)
			Code for high level calls:
				movl	$ ret, (%esi)
				jmp	high_level
			    ret:
			High level word:
			    high_level:
				subl	$4, %esi	\ Advance stack pointer
				...
				addl	$4, %esi
				jmp	*(%esi)		\ Return to caller
			if set to 0, the processor stack is used as return
			stack, and %esi is the parameter stack pointer.
			Code for +:
				addl	(%esi), tos
				leal	-4(%esi), %esi
				ret
			Code for high level calls:
				call	high_level
			High level word:
				...
				ret

			/ps=sp 1 produce faster code if more high level calls
			are used then primitives.
			/ps=sp 0 produce faster code if less high level calls
			are used then primitives.

		/fast-string
			if set to 1, the kernel produce faster string literals.
			Code for s"
    				jmp   1f		\ Jump over string
			  0:
				.ascii "string"		\ String literal
			  1:
				pushl	tos		\ Save top of stack
				pushl	$0b		\ String address
				movl	$length, tos	\ String length

			if set to 0, the kernel produce following code:
				movl	$ret,(%esi)
				jmp	(s")
			  ret:
				.byte	length		\ String length
				.ascii	"string"	\ String literal

		/cache
			if set to 1, the kernel uses a cache for finding words;
			otherwise no cache is used.

		/block
			if set to 1, the kernel includes the block word set;
			otherwise no block words are compiled into the kernel.

		/user
			if set to 1, the kernel includes the user word set;
			otherwise no user words are compiled into the kernel.

		/float
			if set to 1, the kernel includes the float word set;
			otherwise no float words are compiled into the kernel.

			Following operating systems are supported:

		target-msdos
			if the word exists code for the msdos operating system
			is produced.

		target-unix
			if the word exists code for the unix operating system
			is produced.

			Following processors are supported:

		target-i386
			if the word exists code is generated and optimimized
			for the intel i386.

		target-i486
			if the word exists code is generated and optimimized
			for the intel i386.

	startup.4th:
	------------

		If /ps=sp is set to 1.

		The default return stack size:	 65536	( 64k )
			The return stack size can be increased later.

		The parameter (data) stack size: 65536	( 64k )
			The data stack size can be increased later.

	FIXME: continue writing docs. ( Want do you want to know?? )

Bug-reports:
============

	Please send the bug reports to:

Contact the author:
===================
		
	e-mail:
		johns..@....net

Improvements:
=============

	I'm very interested in improvements, libraries and porting to
	different platforms.
	Every public sources, comments and improvements are welcome.

Copyright:
==========

	Copyright of all the sources:

		Copyright 1991, 1992 by Lutz Sammer.
