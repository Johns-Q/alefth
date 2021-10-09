\
\	ALE Forth Version 2.04
\
\		Copyright (c) 1991,1992 by Lutz Sammer.
\
\		Bug-reports via usenet: johns
\
\	File:	signals.4th
\
\		Support for unix signals handling.
\

also compiler		\ system calls signal> ...

\\	signals			( -- )				V	SIGNALS
\\
\\		Append the signal word list to the current search set.
\\
\\	    Words:
\\		( constants )
\\		#sigs		sig-abort	sig-alarm	sig-bus
\\		sig-child	sig-cont	sig-emt		sig-fpe
\\		sig-hangup	sig-ill		sig-int		sig-kill
\\		sig-phone	sig-pipe	sig-poll	sig-power
\\		sig-quit	sig-segv	sig-stop	sig-sys
\\		sig-term	sig-trap	sig-ttin	sig-ttout
\\		sig-ttstop	sig-usr1	sig-usr2	sig-winch
\\		( actions )
\\		catch-signal	default-signal	ignore-signal	hold-signal
\\		pause-signal	release-signal
\\		alarm		kill		raise		pause
\\		( displaying )
\\		signal-list	.signal		.signals
\\		( private )
\\		signal-handler	init-signals
\\
\\	    Words in the compiler vocabulary:
\\		signal>		sig-call
\\
vocabulary signals  also signals definitions

\\	sig-hangup		( -- sig )				SIGNALS
\\
\\		sig is number of the hangup signal.
\\
1 constant sig-hangup

\\	sig-int			( -- sig )				SIGNALS
\\
\\		sig is number of the interrupt signal.
\\
2 constant sig-int

\\	sig-quit		( -- sig )				SIGNALS
\\
\\		sig is number of the quit signal.
\\
3 constant sig-quit

\\	sig-ill			( -- sig )				SIGNALS
\\
\\		sig is number of the illegal instruction signal.
\\
4 constant sig-ill

\\	sig-trap		( -- sig )				SIGNALS
\\
\\		sig is number of the trace trap signal.
\\
5 constant sig-trap

\\	sig-abort		( -- sig )				SIGNALS
\\
\\		sig is number of the signal used by abort.
\\
6 constant sig-abort

\\	sig-emt			( -- sig )				SIGNALS
\\
\\		sig is number of the EMT instruction signal.
\\
7 constant sig-emt

\\	sig-fpe			( -- sig )				SIGNALS
\\
\\		sig is number of the floating point exeception signal.
\\
8 constant sig-fpe

\\	sig-kill		( -- sig )				SIGNALS
\\
\\		sig is number of the kill signal.
\\		(cannot be caught or ignored)
\\
9 constant sig-kill

\\	sig-bus			( -- sig )				SIGNALS
\\
\\		sig is number of the bus error signal.
\\
10 constant sig-bus

\\	sig-segv		( -- sig )				SIGNALS
\\
\\		sig is number of the segmentation violation signal.
\\
11 constant sig-segv

\\	sig-sys			( -- sig )				SIGNALS
\\
\\		sig is number of the bad argument to system call signal.
\\
12 constant sig-sys

\\	sig-pipe		( -- sig )				SIGNALS
\\
\\		sig is number of the write on a pipe with no one to read it
\\		signal.
\\
13 constant sig-pipe

\\	sig-alarm		( -- sig )				SIGNALS
\\
\\		sig is number of the alarm clock signal.
\\
14 constant sig-alarm

\\	sig-term		( -- sig )				SIGNALS
\\
\\		sig is number of the software termination signal from kill.
\\
15 constant sig-term

\\	sig-usr1		( -- sig )				SIGNALS
\\
\\		sig is number of the user defined signal 1.
\\
16 constant sig-usr1

\\	sig-usr2		( -- sig )				SIGNALS
\\
\\		sig is number of the user defined signal 2.
\\
17 constant sig-usr2

\\	sig-child		( -- sig )				SIGNALS
\\
\\		sig is number of the death of a child signal.
\\
18 constant sig-child

\\	sig-power		( -- sig )				SIGNALS
\\
\\		sig is number of the power-fail restart signal.
\\
19 constant sig-power

\\	sig-winch		( -- sig )				SIGNALS
\\
\\		sig is number of the window change signal.
\\
20 constant sig-winch

\\	sig-phone		( -- sig )				SIGNALS
\\
\\		sig is number of the handset, line status changed.
\\
21 constant sig-phone

\\	sig-poll		( -- sig )				SIGNALS
\\
\\		sig is number of the pollable event occured signal.
\\
22 constant sig-poll

\\	sig-stop		( -- sig )				SIGNALS
\\
\\		sig is number of the sendable stop signal not from tty.
\\
23 constant sig-stop

\\	sig-ttstop		( -- sig )				SIGNALS
\\
\\		sig is number of the stop signal from tty. ( normaly 24 )
\\
24 constant sig-ttstop

\\	sig-cont		( -- sig )				SIGNALS
\\
\\		sig is number of the continue a stopped process signal.
\\
25 constant sig-cont

\\	sig-ttin		( -- sig )				SIGNALS
\\
\\		sig  is number of the  to readers pgrp upon background  tty
\\		read signal.
\\
26 constant sig-ttin

\\	sig-ttout		( -- sig )				SIGNALS
\\
\\		sig  is number of the  to readers pgrp upon background  tty
\\		write signal.
\\
27 constant sig-ttout

\\	#sigs			( -- n )				SIGNALS
\\
\\		n is the number of available signals.  Valid signal numbers
\\		are from 1 to #SIG-1.
\\
28 constant #sigs

\\	signal-list		( -- a-addr )				SIGNALS
\\
\\		a-addr is the address of the signal message list.
\\		SIGNAL-LIST is a array of counted strings.
\\
\\	    Typical use:
\\		sig cells signal-list + @ count type
\\
create signal-list  #sigs cells allot
  signal-list #sigs cells erase

\	Build table

here ," Hangup"					sig-hangup cells signal-list + !
here ," Interrupt"				sig-int    cells signal-list + !
here ," Quit"					sig-quit   cells signal-list + !
here ," Illegal instruction"			sig-ill    cells signal-list + !
here ," Trace/BPT trap"				sig-trap   cells signal-list + !
here ," Abort/IOT instruction"			sig-abort  cells signal-list + !
here ," EMT instruction"			sig-emt    cells signal-list + !
here ," Floating point exception"		sig-fpe    cells signal-list + !
here ," Killed"					sig-kill   cells signal-list + !
here ," Bus error"				sig-bus    cells signal-list + !
here ," Segmentation violation"			sig-segv   cells signal-list + !
here ," Bad argument to system call"		sig-sys    cells signal-list + !
here ," Write on pipe with no one to read it"	sig-pipe   cells signal-list + !
here ," Alarm clock"				sig-alarm  cells signal-list + !
here ," Software termination signal from kill"	sig-term   cells signal-list + !
here ," User defined signal 1"			sig-usr1   cells signal-list + !
here ," User defined signal 2"			sig-usr2   cells signal-list + !
\ here ," Death of a child"			sig-child  cells signal-list + !
here ," Child status has changed"		sig-child  cells signal-list + !
here ," Power-fail restart"			sig-power  cells signal-list + !
here ," Window changed"				sig-winch  cells signal-list + !
here ," Handset, line status changed"		sig-phone  cells signal-list + !
here ," Pollable event occured"			sig-poll   cells signal-list + !
here ," Sendable stop signal not from tty"	sig-stop   cells signal-list + !
here ," Stop signal from tty"			sig-ttstop cells signal-list + !
here ," Continue a stopped process"		sig-cont   cells signal-list + !
here ," Backg. read attempt from control tty"	sig-ttin   cells signal-list + !
here ," Backg. write attempt from control tty"	sig-ttout  cells signal-list + !

\\	default-signal		( sig -- xt ior )			SIGNALS
\\
\\		Restore default system action for signal sig.
\\		ior is implemention-defined io-result.  xt is the execution
\\		token of the previous signal handling routine.
\\
: default-signal
  nil swap signal
;

\\	catch-signal		( xt1 sig -- xt2 ior )			SIGNALS
\\
\\		Set the execution token xt1, as handler for the signal sig.
\\		If  sig  isn't a valid signal, ior is non zero and  xt2  is
\\		undefined; Otherwise SIGNAL returns the execution token xt2
\\		of  the old signal handler and ior is zero.  If you want to
\\		restore the default signal action, use nil for xt1.
\\
\\	    Execution of xt1	( sig -- )
\\		sig is the signal raising xt1.
\\
: catch-signal
  256 or signal
;

\\	hold-signal		( sig -- ior )				SIGNALS
\\
\\		Hold  signal sig.  The signal sog is to held upon  receipt.
\\		Any  pending  signal of this type remains  held.  Only  one
\\		signal  of each type is held.  If sig isn't a valid signal,
\\		ior is non zero; Otherwise ior is zero.
\\
: hold-signal
  512 or default-signal drop
;

\\	release-signal		( sig -- ior )				SIGNALS
\\
\\		Release signal sig. Signals are hold, if the signal handler
\\		is  called.  If  the signal handler didn't  return  to  the
\\		caller it must release the signal (THROW or ABORT).  If sig
\\		isn't  a valid signal, ior is non zero;  Otherwise  ior  is
\\		zero.
\\
: release-signal
  1024 or default-signal drop
;

\\	ignore-signal		( sig -- xt ior )			SIGNALS
\\
\\		Ignore signal sig.  Any pending signal sig is discarded and
\\		the   system  signal  action  is  set  to   ignore   future
\\		occurrences  of  this signal type.  If sig  isn't  a  valid
\\		signal,  ior  is non zero and xt  is  undefined;  Otherwise
\\		returns  the  execution  token xt of  the  previous  signal
\\		handler and ior is zero.
\\
: ignore-signal
\  2048 or default-signal
  -1 swap signal
;

\\	pause-signal		( sig -- ior )				SIGNALS
\\
\\		Suspends  the  calling process until it receives a  signal,
\\		the  same  as pause.  However, if the signal sig  had  been
\\		received  and  held, it is released and  the system  signal
\\		action taken.  If sig isn't a valid signal, ior is non zero
\\		Otherwise ior is zero.
\\
: pause-signal
  4096 or default-signal drop
;

\\	kill			( sig pid -- ior )			SIGNALS
\\
\\		Send the signal sig to a process or a group of processes.
\\		ior is the implementation-defined io-result.
\\
' kill alias kill

\\	raise			( sig -- ior )				SIGNALS
\\
\\		Send signal sig to execution program.  ior is implemention-
\\		defined io-result.  See kill for ior.
\\
: raise
  getpid kill
;

\\	alarm			( n -- )				SIGNALS
\\
\\		Set  the  alarm clock of the execution progamm to send  the
\\		signal SIG-ALARM after n seconds have elapsed. If n is zero
\\		it canceled, any previous alarm requests.
\\
: alarm
  alarm 2drop
;

\\	pause			( -- )					SIGNALS
\\
\\		Suspend execution program, until it receives a signal.
\\
' pause alias pause

\\	.signal			( sig -- )				SIGNALS
\\
\\		Display description for signal sig.
\\		Displays: *** Signal( sig ) Description ***.
\\
: .signal
  base @ >r decimal			\ Save base and decimal
  ." *** Signal(" dup (.) type ." ) "
  dup 1 #sigs within
  if
    cells signal-list + @ count type space
  else
    ." Unknown signal#" .
  then
  ." ***"
  r> base !				\ Restore base
;

\\	.signals		( -- )					SIGNALS
\\
\\		Display the current signal handler setting.
\\		Press any key to stop display, press ESC to abort display.
\\
: .signals
  cr ." Sig Handler                           Description"
  cr ." --- --------------------------------- -------------"
  cr
  #sigs 1
  do
    i dup 3 .r space
    nil over catch-signal		\ Get signal action
    if
      drop ." Can't be caught"
    else
       dup
       case
	 nil of  ." Default"  endof
	 1   of  ." Ignored"  endof
	 2   of  ." Holded"   endof
	 dup of  dup .name    endof
       endcase
       over catch-signal throw drop	\ Restore signal action
    then
    37 out @ - spaces			\ Indent
    cells signal-list + @ count type cr
    ?keypause
  loop
;

\\	signal>			( -- sig ) ( R: -- sys )	C I	UNIX
\\
\\		Fetch signal number for signal handling word.
\\		Build return stack to return from signal handler.
\\
: signal>
  postpone signal>
; compilation immediate

\\	signal-handler		( -- ) ( R: -- )			SIGNALS
\\
\\		Default signal handler for signals.
\\		Display signal description and if the signal is sig-quit
\\		leave forth;  Otherwise do a THROW.
\\
\\		Value for THROW is -(signal number+256).
\\			-257
\\
: signal-handler
signal>		( -- sig ) ( R: -- sys )
  ." At Address: $" signal-addr@
  base @ hex swap u. base !		\ Display address
  dup .signal				\ Display signal
  dup release-signal throw
  dup sig-quit -
  if
    256 + negate throw
  then
  bye					\ sig-quit leave forth
;

also compiler definitions

\\	init-signals		( -- )					SIGNALS
\\
\\		Setup default signal handler for signals.
\\
: init-signals
  #sigs 1
  do
    ['] signal-handler i catch-signal 2drop
  loop
;

init-signals

only forth also definitions

false [if]
\	Some examples for signal handling

also signals

: do-int				\ A signal handling word
signal>
  .signal cr
;

' do-int sig-int catch-signal 2drop	\ Catch the signal

sig-int raise drop			\ Raise the signal

' signal-handler sig-int catch-signal	\ Restore default forth signal handler
2drop

[then]
