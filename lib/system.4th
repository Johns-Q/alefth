\
\	ALE Forth Version 2.04
\
\		Copyright (c) 1991,1992 by Lutz Sammer.
\
\		Bug-reports via usenet: johns
\
\	File:	system.4th
\
\		c-library: system
\

compiler definitions

\\	(0")			( -- c-addr )			C	ALE
\\
\\
: (0")
  r> count over + char+ ( aligned) >r
;

forth definitions also compiler also signals

\\	0"							I	ALE
\\
\\	    Compilation:	( "ccc<">" -- )
\\		Parse  characters ccc delimited by " (double quote). Append
\\		the  execution   semantics   given  below  to  the  current
\\		definition.
\\
\\	    Execution:		( -- c-addr0 )
\\		Return  c-addr, a zero terminated string consisting of  the
\\		characters  ccc.  A  standard  program  may  not  alter the
\\		the characters ccc.
\\
\\	    Interpretation:	( "ccc<">" -- c-addr0 )
\\		Parse  characters  ccc delimited by " (double quote). Store
\\		the  resulting  string at a temporary location described by
\\		c-addr as zero terminated string.
\\
: 0"
  string?
  compiling
  if
    postpone (0") ", 0 c,
  else
    dup >r 1+ "allot tuck r@ cmove
    0 over r> + c!
  then
; immediate

\\	fork			( -- pid flag ior )			UNIX ALE
\\
\\		The fork system call causes creation of a new process.  The
\\		new process (child process) is an exact copy of the calling
\\		process (parent process).
\\		flag  is non zero for the child process, pid is the process
\\		id of the parent;  flag is zero for the parent process, pid
\\		is the process id of the child.
\\

\\	(exec)			( c-addr u -- ior )			UNIX ALE
\\
\\		The (exec) function causes the string to be given to sh  as
\\		input,  as if the string had been typed as a command  at  a
\\		terminal.  EXEC returns only if an error occures.
\\
: (exec)
  exit-term				\ Restore orginal terminal structure
  ">0					\ Make unix like string
  nil
  swap
  0" -c"
  0" sh(fth)"
  sp@
  environ @				\ Envp
  swap					\ Argvp
  0" /bin/sh"				\ Path
  exece
;

\\	system			( c-addr u -- ior )			UNIX ALE
\\
\\		The  system function causes the string to be given to sh as
\\		input,  as if the string had been typed as a command  at  a
\\		terminal.  The  current process waits until the  shell  has
\\		completed, then returns the exit status of the shell.
\\
: system
  2>r
  sig-child default-signal throw
  sig-int   ignore-signal  throw	\ Save signals
  sig-quit  ignore-signal  throw
  fork throw
  if					\ Child
    drop 2r> (exec) (bye)
  then
					\ Parent
  2r> 2drop				\ Discard 0-addr
  begin					\ Wait for child process to terminate
    wait throw
    pluck <>
  while
    drop
  repeat
  8 >> 255 and >r drop			\ Save result and discard pid

  sig-quit  catch-signal throw drop
  sig-int   catch-signal throw drop	\ Restore signals
  sig-child catch-signal throw drop
  r>
;

\\	dir			( -- )
\\
\\		Show current directory.
\\
: dir
  cr " lf" system throw
;

\\	shell			( -- )
\\
\\		Execute a shell.
\\
: shell
  exit-term
  cr " sh" system
  init-term
  throw
;

\\	vi			( c-addr u -- )
\\
\\		Leave forth, execute vi, enter forth.
\\
: vi
  " vi " 2swap concat system throw
  " fth" (exec) throw
;

forth only definitions
