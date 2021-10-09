\
\	ALE Forth Version 2.04
\
\		Copyright (c) 1991,1992 by Lutz Sammer.
\
\		Bug-reports via usenet: johns
\
\	File:	kernel.4th
\
\		Forth kernel.
\
\	RCS:
\	$Id:$
\	$Log:$

\ -----------------------------------------------------------------------------

target-compiler				\ Start target compiler

\ =============================================================================
\	Source of the forth kernel.
\ =============================================================================

/ps=sp [if]
\\	386/486 SCO-Unix V 3.2v2.0		Forth memory map:
\\
\\		0000 0000	Code-Segment read-only
\\		    v
\\		0000 00A8		Startup code
\\		    |			Forth kernel constants/code/colon
\\		    |
\\	    etext   v
\\		0040 0000	Data-Segment read/write
\\		    |			Forth kernel variables/create
\\		    v
\\	    dp0	    |			Dictionary start
\\		    |
\\	    here    |			Next available dictionary address
\\		    v			Word buffer used by word
\\		    ^			Pictured numeric buffer
\\	    pad	    |			= here + #pnb ( 80 Bytes )
\\		    v			Scratch area upto unused - #pnb bytes
\\		    |
\\	    dp0+#dp v			Dictionary end
\\		    |
\\		    v			(can be incremented to ulimit)
\\	        ---- ----
\\		    ^
\\		    |			upto ulimit data/parameter stack
\\	    sp0     |			Data/parameter stack start
\\		    ^
\\		    |			upto 64k return stack
\\	    rp0     |			Return stack start
\\		    ^
\\		    |                   Used by unix kernel:
\\		    |			arguments, environment
\\		7fff ffff	Stack-Segment
\\
\\		e000 0000	User area
\\		    |
\\		    v
\\
[else]
\\	386/486 SCO-Unix V 3.2v2.0		Forth memory map:
\\
\\		0000 0000	Code-Segment read-only
\\		    v
\\		0000 00A8		Startup code
\\		    |			Forth kernel constants/code/colon
\\		    |
\\	    etext   v
\\		0040 0000	Data-Segment read/write
\\		    |			Forth kernel variables/create
\\		    v
\\	    dp0	    |			Dictionary start
\\		    |
\\	    here    |			Next available dictionary address
\\		    v			Word buffer used by word
\\		    ^			Pictured numeric buffer
\\	    pad	    |			= here + #pnb ( 80 Bytes )
\\		    v			Scratch area upto unused - #pnb bytes
\\		    |
\\	    dp0+#dp v			Dictionary end
\\		    |
\\		    v			(can be incremented to ulimit)
\\	        ---- ----
\\		    ^
\\		    |			upto ulimit Return stack
\\	    rp0     |			Return stack start
\\		    ^
\\		    |			upto 64k Data stack
\\	    sp0     |			Data/parameter stack start
\\		    ^
\\		    |                   Used by unix kernel:
\\		    |			arguments, environment
\\		7fff ffff	Stack-Segment
\\
\\		e000 0000	User area
\\		    |
\\		    v
\\
[then]

\ /stand-alone [if]
\ s" startup.4th"		included	\ Startup code ( Must be first )
\ [then]

\\	forth-wordlist		( -- wid )				SEARCH
\\
\\		Return  wid, the identifier of the word list that  includes
\\		all  standard  words provided by the  implementation.  This
\\		word  list  is initially the compilation word list  and  is
\\		part of the initial search order.
\\
wordlist constant forth-wordlist

\\	forth			( -- )			V	SEARCH EXT
\\
\\		Transform  the search order consisting of wid1, ... widn-1,
\\		widn  (where widn is searched first) into wid1, ... widn-1,
\\		widn, FORTH-WORDLIST.
\\
forth-wordlist build forth  forth definitions

\\	compiler		( -- )				V	ALE
\\
\\		The vocabulary COMPILER contains the forth compiler private
\\		words.
\\
vocabulary compiler

s" config.4th"		included	\ Forth configuration
s" primitiv.4th"	included	\ Forth minimal primitives
s" stacks.4th"		included	\ Stack words
s" math.4th"		included	\ Mathe words
s" mem.4th"		included	\ Memory access words
s" variable.4th"	included	\ Common variables

exist target-unix [if]
s" unix.4th"		included	\ Unix kernel support
[then]
exist target-msdos [if]
s" msdos.4th"		included	\ Msdos kernel support
[then]

s" dict.4th"		included	\ Dictionary words
s" execute.4th"		included	\ Execution words
s" strings.4th"		included	\ String words
s" numeric.4th"		included	\ Numeric words
s" in-out.4th"		included	\ Input/Output words
s" blkfile.4th"		included	\ Block/File words
s" internal.4th"	included	\ Internal words
s" search.4th"		included	\ Vocabulary/Search words
s" control.4th"		included	\ Control structures
s" compiler.4th"	included	\ Compiler words

/user [if]
s" user.4th"		included	\ User defined words
[then]

s" intrpret.4th"	included	\ Interpreter words
s" error.4th"		included	\ Error words
s" tools.4th"		included	\ Resident toolkit words

/float [if]
s" float.4th"		included	\ Floating point word set
[then]

s" coldbye.4th"		included	\ Exit/Enter words

\ 3 c-function memcpy ( n s2 s1 -- s1 )	\ Arguments are REVERSE !!!
\   c-variable errno  ( -- a-addr )	\ Size must be known

\ -----------------------------------------------------------------------------
\	Libraries that could be made resident:
\ -----------------------------------------------------------------------------

\ s" lib/double.4th"	included	\ Double word set
\ s" lib/toolkit.4th"	included	\ Toolkit word set




\ Next can't be:	 FIXME: Try to get them work.

\ s" lib/defining.4th"	included	\ New defining words
exist target-unix [if]
\ s" lib/signals.4th"	included	\ Signal handling
\ s" lib/system.4th"	included	\ Issue a shell command
[then]
\ s" lib/utility.4th"	included	\ Utility word set
\ s" lib/dasm.4th"	included	\ Disassembler
