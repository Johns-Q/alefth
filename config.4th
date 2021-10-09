\
\	ALE Forth Version 2.04
\
\		Copyright (c) 1991,1992 by Lutz Sammer.
\
\		Bug-reports via usenet: johns
\
\	File:	config.4th
\
\		Forth kernel configuration.
\

\ -----------------------------------------------------------------------------
\	Forth configuration.
\ -----------------------------------------------------------------------------

\\	i386			( -- )					CONFIG
\\
\\		If defined, running on a i386 CPU.
\\
exist target-i386 [if]
rom create i386
[then]

\\	i486			( -- )					CONFIG
\\
\\		If defined, running on a i486 CPU.
\\
exist target-i486 [if]
rom create i486
[then]

\\	cpu			( -- xt )				CONFIG
\\
\\		Identifies the cpu.
\\
exist target-i386 [if]
' i386 constant cpu
[then]
exist target-i486 [if]
' i486 constant cpu
[then]

\\	i387			( -- )					CONFIG
\\
\\		If defined, a i387 FPU is present.
\\
exist target-i386 [if]
rom create i387
[then]

\\	i487			( -- )					CONFIG
\\
\\		If defined, a i487 FPU is present.
\\
exist target-i486 [if]
rom create i487
[then]

\\	fpu			( -- xt )				CONFIG
\\
\\		Identifies the fpu.
\\
exist target-i386 [if]
' i387 constant fpu
[then]
exist target-i486 [if]
' i487 constant fpu
[then]
\ nil constant fpu

exist target-unix [if]

\\	svr3			( -- )					CONFIG
\\
\\		If defined, forth is running on system V release 3.
\\
rom create svr3

\\	unix			( -- xt )				CONFIG
\\
\\		If defined, forth is running on a unix like operating system.
\\		xt identifies the unix system.
\\
' svr3 constant unix

[then]

exist target-msdos [if]

\\	ms30			( -- )					CONFIG
\\
\\		If defined, forth is running on msdos Version 3.
\\
rom create ms30

\\	ms40			( -- )					CONFIG
\\
\\		If defined, forth is running on msdos Version 4.
\\
\ rom create ms40

\\	ms50			( -- )					CONFIG
\\
\\		If defined, forth is running on msdos Version 5.
\\
\ rom create ms50

\\	dr60			( -- )					CONFIG
\\
\\		If defined, forth is running on digtal research Version 6.
\\
\ rom create dr60

\\	msdos			( -- xt )				CONFIG
\\
\\		If defined, forth is running on MSDOS pseudo operating system.
\\		xt identifies the MSDOS Version.
\\
' ms30 constant msdos

[then]

\\	os			( -- xt )				CONFIG
\\
\\		Identifies the operating system.
\\
exist target-unix [if]
' unix constant os
[then]
exist target-msdos [if]
 ' msdos constant os
[then]

\\	ale-forth		( -- n )				CONFIG
\\
\\		Identifies ALE - FORTH kernel.  n is the version number.
\\		( 100 is 1.00 or 223 is 2.23 )
\\
204 constant ale-forth
