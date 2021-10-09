\
\	ALE Forth Version 2.04
\
\		Copyright (c) 1991,1992 by Lutz Sammer.
\
\		Bug-reports via usenet: johns
\
\	File:	error.4th
\
\		Forth kernel error handling.
\

\ -----------------------------------------------------------------------------
\	Error handling
\ -----------------------------------------------------------------------------

compiler definitions

\\	handler			( -- a-addr )			ERROR ALE
\\
\\		a-addr  is  the address of the current  error  interception
\\		frame on the return stack.  Used for CATCH and THROW.
\\
value handler

forth definitions

\\	catch								ERROR
\\				( i*x xt -- j*x 0 )	No THROW received
\\				( i*x xt -- i*x n )	THROW received
\\
\\		Pushes  an  error  interception  frame on the return stack.
\\		That  frame  remembers  the  current  stack depth, and then
\\		executes the execution token xt (as with EXECUTE) in such a
\\		way  that  control  may be transferred back to a point just
\\		after  CATCH  if  THROW is executed during the execution of
\\		xt.  If the execution of xt completes normally (i. e., this
\\		CATCH  does not receive a THROW) CATCH pops the error frame
\\		and  returns  0  above whatever stack items would have been
\\		returned by xt EXECUTE.  If this CATCH receives a throw, it
\\		returns  a  non-zero  n  above an indeterminate number i of
\\		stack  items.  The depth of the stack is now the same as it
\\		was  just  before CATCH began execution.  The values of the
\\		i*x  stack  items  could  have  been  modified  during  the
\\		execution  of xt.  In general, there is nothing useful that
\\		can  be done with those stack items.  Since the stack depth
\\		is known, the application may DROP those items.
\\
: catch
  sp@ handler 2>r
  rp@ to handler
  execute
  2r> to handler drop
  0
;

\\	throw								ERROR
\\				( k*x 0 -- k*x )	not thrown  -or-
\\				( k*x n -- i*x n )	thrown
\\
\\		If  the  top  of  the  stack  is  zero,  THROW discards it.
\\		Otherwise,  it  pops  the  topmost error interception frame
\\		(see CATCH) from the return stack, along with everything on
\\		the return stack above that error frame. THROW then adjusts
\\		the  stack depth so that the depth is the same as the depth
\\		saved  in the error frame (i is the same number as the i in
\\		the input arguments to  the corresponding CATCH), puts n on
\\		top of  the stack,  and transfers  control to  a point just
\\		after the CATCH that installed that error frame.
\\		If  there is  no error  interception  frame  on the  return
\\		stack, THROW performs the function of ABORT.
\\
\\		-1		ABORT
\\		-2		ABORT"
\\		-255..-1	Reserved for ans standard
\\		-512..-256	(Unix) Signals
\\		1..1024		(Unix) ior
\\
: throw
  dup
  if
    handler dup
    if
      rp! 2r> to handler
      swap >r sp! drop r> exit
    then
    drop
					\ No handler doing abort
    dup 0< 0= if . ." :throw" then
    sp0 sp!				\ Init parameter stack
    quit
  then
  drop
;

\\	abort			( i*x -- ) ( r: j*x -- )		CORE
\\
\\		Empty  the  data  stack  and  perform the  function of QUIT
\\		without displaying a message.
\\
: abort
  true throw
;

/fast-string 0= [if]			\ old version

compiler definitions

\\	(abort")		( i*x x1 -- ) ( r: j*x -- )	C	COMPILER
\\			-or-	( i*x 0 -- i*x ) ( r: j*x -- j*x )
\\
\\		Compilation word, output a message and abort, if x1 is true
\\		otherwise continue execution. See also CATCH and THROW.
\\		(ABORT") is compiled by ABORT".
\\
: (abort")
\  if
    space r> count type -2 throw
\  then
\  r> count + ( aligned) >r
; compilation

forth definitions

\\	abort"							I	CORE
\\
\\	    Compilation:	( "ccc<">" -- )
\\		Parse characters ccc delimited by a double quote mark.
\\
\\	    Execution:		( i*x x1 -- ) ( r: j*x -- )
\\		If  all  bits of x1 are zero, execute the sequence of words
\\		after   ccc<">.  Otherwise,  display  ccc  and  perform  an
\\		implementation-defined  error abort sequence which includes
\\		the function of ABORT.
\\
\\	    Interpretation:	( i*x x1 "ccc<">" -- ) ( r: j*x -- )
\\		Parse characters ccc delimited by " (double quote mark). If
\\		all bits of x1 are zero, continue interpretation the  words
\\		after  ccc<">.   Otherwise,  display  ccc  and  perform  an
\\		implementation-defined  error abort sequence which includes
\\		the function of ABORT. (ALE EXTENSION)
\\
: abort"
  compiling
  if
\    postpone (abort") ",
    postpone if postpone (abort") string? ", postpone then
  else
    string? type -2 throw
  then
; immediate

[else]					\ New version

compiler definitions

\\	(abort")		( i*x -- ) ( r: j*x -- )	C	COMPILER
\\
\\		Compilation word, output a message and abort.
\\		(ABORT") is compiled by ABORT".
\\
: (abort")
  type -2 throw
; compilation

forth definitions

\\	abort"							I	CORE
\\
\\	    Compilation:	( "ccc<">" -- )
\\		Parse characters ccc delimited by a double quote mark.
\\
\\	    Execution:		( i*x x1 -- ) ( r: j*x -- )
\\		If  all  bits of x1 are zero, execute the sequence of words
\\		after   ccc<">.  Otherwise,  display  ccc  and  perform  an
\\		implementation-defined  error abort sequence which includes
\\		the function of ABORT.
\\
\\	    Interpretation:	( i*x x1 "ccc<">" -- ) ( r: j*x -- )
\\		Parse characters ccc delimited by " (double quote mark). If
\\		all bits of x1 are zero, continue interpretation the  words
\\		after  ccc<">.   Otherwise,  display  ccc  and  perform  an
\\		implementation-defined  error abort sequence which includes
\\		the function of ABORT. (ALE EXTENSION)
\\
: abort"
  compiling
  if
    postpone if
      postpone s" postpone (abort")
    postpone then
  else
    string? type -2 throw
  then
; immediate

[then]
