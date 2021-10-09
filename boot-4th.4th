\
\	ALE Forth Version 2.04
\
\		Copyright (c) 1991,1992 by Lutz Sammer.
\
\		Bug-reports via usenet: johns
\
\	File:	boot-4th.4th
\
\		Forth kernel bootup file. Loaded by cold as very first file.
\

\ .( Boot file loading ... ) cr

\ -----------------------------------------------------------------------------
\	Kernel commandline processing
\ -----------------------------------------------------------------------------

\\	environ			( -- a-addr )				UNIX
\\
\\		a-addr is the address of ENVIRONMENT.  ENVIRONMENT contains
\\		the pointer to the environment array.
\\
\\	    Internal:
\\		environ ->	[ env 0 ] -> Nul terminated string
\\				[ env 1 ] -> ...
\\				[ env 2 ] -> ...
\\				[  NIL  ] ( Marks the end )
\\
variable environ

\\	argv			( -- a-addr )				UNIX
\\
\\		a-addr  is the address of ARGV.  ARGV contains the  pointer
\\		to the argument vector. The length of the argument contains
\\		the variable ARGC.
\\
\\	    Internal:
\\		argv ->		[ argv 0 ] -> Nul terminated string
\\					( Normaly the command name )
\\				[ argv 1 ] -> ...
\\				[ argv 2 ] -> ...
\\			{	[  NIL  ] ( Marks the end )	} sometimes
\\
variable argv

\\	argc			( -- a-addr )				UNIX
\\
\\		a-addr  is the address of ARVC.  ARGC contains the size  of
\\		the argument vector.  SEE ALSO:  ARGV.
\\
variable argc

\\	fast			( -- flag )			COMANDLINE
\\
\\		flag is set to true by the command line option -f.
\\		If flag is true, "init-4th.4th" isn't loaded.
\\
value fast

\	debug
false [if]
: $.	( 0-addr -- )
  0>" type
;

: .args
  ." Arguments:" argc ? ." - "
  argc @ 0
  do
    argv @ i 4 * + @ $. space
  loop
  cr
  ." Environment:"
  environ @
  begin
     dup @
  while
     dup @ $. cr
     cell +
  repeat
  drop
;
[then]

\\	-i			( "file" -- )			COMMANDLINE
\\
\\		Include file.  s" file" included.
\\
: -i
  bl word count library ( included)
; immediate

compiler

\\	-d			( "n" -- )			COMMANDLINE
\\
\\		Set the dictionary size to n.
\\
: -d
  bl word count number? 0= abort" Number expected"
  dup to #dp dp0 + brk throw		\ Set dictionary size
; immediate

forth

\\	-f			( -- )				COMMANDLINE
\\
\\		Fast.  Don't load init-4th.4th.
\\
: -f
  1 to fast
;

\\	-h			( -- )				COMMANDLINE
\\
\\		Display command line help.
\\
: -h
  ." Usage: fth [-d n] [-i file] [-f] [other forth words]" cr
  ." 	-d n	Set the dictionary size to n bytes." cr
  ." 	-i file	Include file before init-4th.4th is included." cr
  ." 	-f	Fast. Don't include init-4th.4th." cr
  ." 	-h	Display this help." cr
  ." 	other	Execute words as typed from keyboard." cr
  ." If you want to include init-4th.4th before your files use:" cr
  ." 	fth -i init-4th.4th -i your-file -f" cr
  ." If you want to run an application and exit use:" cr
  ." 	fth -i application bye" cr
  cr
  bye
;

\\	command-line		( env argv argc -- )		COMMANDLINE
\\
\\		Move env, argv, argc to variables.
\\		Interpret the command line.
\\
: command-line
  argc ! argv ! environ !
  0					\ Calculate size of arguments
  argv @ argc @ cells
  bounds cell+
  ?do
    i @ 0>"				\ Convert OS-String > Forth string
    nip + 1+
    cell
  +loop
  here swap allot			\ Allocate space for string

  dup					\ Copy arguments
  argv @ argc @ cells
  bounds cell+
  ?do
    i @
    begin
      dup c@ dup
    while				\ Copy until zero
      pluck c!
      swap 1+ swap 1+
    repeat
    2drop
    bl over c!
    1+
    cell
  +loop
  over -				\ Size of string

  ['] evaluate catch			\ Execute commandline
  if
    ." :command-line" cr bye
  then
;
[then]

\\	init-library		( -- )					ALE
\\
\\		Setup the library files path to default.
\\		Change it as desired.
\\
: init-library
  s" lib/" library-path 2!		\ Set library search path
;

\\	boot		( envp argv argc -- ) ( R: -- )			ALE
\\
\\		Forth kernel boot:
\\			Setup library path,
\\			Interpret the command line,
\\			Include the file "init-4th.4th".
\\
\\		envp:	Unix/Msdos environment pointer.
\\		argv:	Unix/Msdos commandline arguments vector.
\\		argc:	Unix/Msdos number of commandline arguments.
\\
\ : boot
  init-library

  command-line				\ Execute command line

  fast					\ Fast start
  0= [if]
    s" init-4th.4th" r/o open-file	\ Try to execute init.
    [if]
      drop warning @
      [if]
	." No init-file" cr
      [then]
    [else]
      include-file
    [then]
  [then]
\ ;

only forth definitions
