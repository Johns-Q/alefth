\
\	ALE Forth Version 2.04
\
\		Copyright (c) 1991,1992 by Lutz Sammer.
\
\		Bug-reports via usenet: johns
\
\	File:	init-4th.4th
\
\		Loaded by kernel after initialisation.
\

.( Loading init-4th.4th ) cr

s" lib/double.4th"	included	\ Double word set
s" lib/toolkit.4th"	included	\ Toolkit word set
s" lib/defining.4th"	included	\ New defining words
[ifdef] unix
s" lib/signals.4th"	included	\ Signal handling
s" lib/system.4th"	included	\ Issue a shell command
[then]
s" lib/utility.4th"	included	\ Utility word set
s" lib/dasm.4th"	included	\ Disassembler
s" lib/block.4th"	included	\ Code block word set
s" lib/memory.4th"	included	\ Memory allocation word set

\ : dis					\ Autoload disassembler
\   s" dasm.4th" included
\   s" dis" evaluate
\ ;

[ifdef] unix
" div/trace.4th"	included	\ Tracer test
" x"			library		\ Some tests
[then]

\\	update-doc			( -- )				ALE
\\
\\		Update documentation, add documentation of library files to
\\		the kernel documentation.
\\
: update-doc
  " ' add-doc" evaluate
  0=if
    " lib/utility.4th" included		\ Load needed file
  then
  " ' add-doc" evaluate
  " boot-4th.4th"	pluck execute	\ Forth boot file
  " init-4th.4th"	pluck execute	\ Forth init file
  " lib/block.4th"	pluck execute	\ Code blocks word set
  " lib/defining.4th"	pluck execute	\ New defining words
  " lib/double.4th"	pluck execute	\ Double word set
  " lib/toolkit.4th"	pluck execute	\ Toolkit word set
  [ifdef] unix
  " lib/signals.4th"	pluck execute	\ Signal handling
  " lib/system.4th"	pluck execute	\ Issue a shell command
  [then]
  " lib/utility.4th"	pluck execute	\ Utility word set
  drop
;
