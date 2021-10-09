    \
    \	ALE Forth Version 2.04
    \
    \		Copyright (c) 1991,1992 by Lutz Sammer.
    \
    \		Bug-reports via usenet: johns
\
\	File:	coldbye.4th
\
\		Forth kernel cold start and exit.
\

\ -----------------------------------------------------------------------------
\	Forth cold start + exit
\ -----------------------------------------------------------------------------

compiler definitions

exist target-unix [if]

\\	termio
\\
\\		word	iflag		input modes
\\		word	oflag		output modes
\\		word	cflag		control modes
\\		word	lflag		line discipline modes
\\		byte	line		line discipline
\\		byte	cc	[8]	control chars
\\

 0	field	->iflag
 2	field	->oflag
 4	field	->cflag
 6	field	->lflag
 8	field	->line
 9	field	->cc
18	constant b/termio

\ FIXME:
( char T 8 << 1 or ) $5401 constant TCGETA
( char T 8 << 2 or ) $5402 constant TCSETA

create o-tio  b/termio allot

\\	init-term		( -- )					ALE
\\
\\		Initialise terminal to raw-mode.
\\
: init-term
  o-tio TCGETA 0 ioctl throw drop
  o-tio pad b/termio cmove		\ Make a copy
  pad
\	IXON	Off : No XON Output Control
\	IXOFF	Off : No XOFF Send
\	IXANY	Off : No XON Output Control with any restart
\	ISTRIP	Off : No Strip of 8th bit on input
\	INPCK	Off : No Input parity
\	ICRNL	Off : No CR -> NL Mapping
\	IGNBRK	On  : Ignore break condition
  dup ->iflag
    dup w@ ( INPCK=16|ISTRIP=32|ICRNL=128|IXON=1024||IXANY=2048IXOFF=4096 )
      7344 invert and
      ( IGNBRK=4 ) 4 or swap w!
\	ICANON	Off : No Erase/Kill processing
\	ECHO	Off : No Input Echo
\	ISIG	On  : Use Signal processing
  dup ->lflag
    dup w@ ( ICANON=2|ECHO=8 ) 10 invert and
    ( ISIG=1 ) 1 or swap w!

  dup ->cc ( VMIN ) 4 + 1 swap c!
  dup ->cc ( VTIME ) 5 + 0 swap c!
  TCSETA 0 ioctl throw drop	\ Set new terminal mode
;

\\	exit-term		( -- )					ALE
\\
\\		Restore terminal mode.
\\
: exit-term
  o-tio TCSETA 0 ioctl throw drop
;

[then]

exist msdos [if]

: init-term
;

: exit-term
;

[then]

forth definitions

\\	quit			( -- ) ( r: i*x -- )			CORE
\\
\\		Empty  the return stack, enter interpretation state, accept
\\		new  input  from  the  current input device, and begin text
\\		interpretion.  Do  not  display a message. When the current
\\		input  stream  has  been exhausted, all processing has been
\\		completed    and   no   ambious    condition   exits,   the
\\		implementation-defined  system  prompt  is  displayed.  The
\\		interpreter then waits for further input.
\\
: quit
  rp0 rp!				\ Restore return stack
  0 to handler				\ Clear error frame

  [ /float ] [if]
    reset-fp				\ Reset floating point
  [then]

  [ /block ] [if]
    blk off
  [then]
  0 to source-file			\ Input comes from keyboard

  [compile] [				\ Enter interpretion mode
  begin
    begin
      cr query space			\ Get one line
      interpret ?stack
      compiling 0=
    until
    ." Ok."
  again
;

\\	bye			( -- )				TOOLKIT EXT
\\
\\		Return control to the host operating system, if any.
\\
: bye
  warning @
  if
    cr ." Leaving forth." cr
  then
  exit-term 0 (bye)
;

\\	cold		( env argv argc -- ) ( R: -- )			ALE
\\
\\		Forth cold start.
\\		env:	Unix environment pointer.
\\		argv:	Unix commandline arguments
\\		argc:	Unix number of arguments
\\
: cold
					\ rp0 rp! sp0 sp! are done by assembler
  [ /user ] [if]
    init-user				\ Init user area
  [then]

  dp0 dup to here			\ Init dictionary pointer
  65536 dup to #dp
    + brk drop				\ Allocate memory
  decimal

  [ /float ] [if]
    init-fp				\ Init floating point
  [then]

  -1 set-order definitions		\ Init search and compilation wordlist

  79 to #cols 24 to #rows		\ Init output device
  init-term

  [ /block ] [if]
    s" default.blk" init-buffer		\ Open default block file
  [then]

  s" boot-4th.4th" r/o open-file	\ Try to execute boot.
  if
    drop warning @
    if
      ." No boot-file" cr
    then
  else
    include-file
  then

  cr ." Ale Forth Version "		\ Version number
      ale-forth 100 /mod (.) type [char] . emit 0 <# # # #> type
     ." , Copyright 1991,1992 by Lutz Sammer"
  cr ." There is ABSOLUTELY NO WARRANTY for Ale Forth." cr
  cr ." CPU: " cpu .name
     [ /ps=sp ] [if]
	."  (%esp=sp)"
     [else]
	."  (%esp=rp)"
     [then]
     ."  FPU: " fpu dup  if .name  else  drop ." not present"  then
     ."  Operating-system: " os .name
  cr ." Available dictionary space: " unused .
  1 warning !				\ turn on warnings
  quit
;
