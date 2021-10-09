\
\	ALE Forth Version 2.04
\
\		Copyright (c) 1991,1992 by Lutz Sammer.
\
\		Bug-reports via usenet: johns
\
\	File:	control.4th
\
\		Forth kernel control structures.
\

\ -----------------------------------------------------------------------------
\	Control structures
\ -----------------------------------------------------------------------------

\\	so						C I	TOOLKIT EXT
\\	  ( sys(u) sys(u-1) ... sys(0) u -- sys(u-1) ... sys(0) sys(u) )
\\
\\		Remove  u.  Rotate u+1 elements on top of the control  flow
\\		stack. An ambiguous condition exists if there are less than
\\		u+2  entries on the stack before SO is executed or if SO is
\\		executed while interpreting.
\\
: so
  roll
; compilation immediate

\\	still						C I	TOOLKIT EXT
\\	 ( sys(u) ... sys(1) sys(0) u -- sys(u) ... sys(1) sys(0) sys(u) )
\\
\\		Remove u. Copy the uth element of the control flow stack to
\\		the  top of the control flow stack.  An ambiguous condition
\\		exists if there are less than u+2 items on the control flow
\\		stack  before  STILL is executed, or if STILL  is  executed
\\		while interpreting.
\\
: still
  pick
; compilation immediate

\\	ahead						C I	TOOLKIT EXT
\\
\\	    Compilation:	( -- orig )
\\		Put the location of a new unresolved forward reference orig
\\		onto the control flow stack. Append the execution semantics
\\		given  below  to the current definition.  The semantics are
\\		incomplete until orig is resolved (e.g., by THEN).
\\
\\	    Execution:		( -- )
\\		Continue   execution  at  the  location  specified  by  the
\\		resolution of orig.
\\
: ahead
  $E9 c, here 0 ,			\ jmp	+32bit
; compilation immediate

\\	if							C I	CORE
\\
\\	    Compilation:	( -- orig )
\\		Put the location of a new unresolved forward reference orig
\\		onto the control flow stack. Append the execution semantics
\\		given  below  to the current definition.  The semantics are
\\		incomplete until orig is resolved (e.g., by THEN).
\\
\\	    Execution:		( x -- )
\\		If  all  bits  of  x  are  zero,  continue execution at the
\\		location specified by the resolution of orig.
\\
: if
  $DB09 w,				\ orl	tos, tos
  pop,
  $840F w, here 0 ,			\ jz	+32bit
; compilation immediate

\\	0=if							C I	CORE
\\
\\	    Compilation:	( -- orig )
\\		Put the location of a new unresolved forward reference orig
\\		onto the control flow stack. Append the execution semantics
\\		given  below  to the current definition.  The semantics are
\\		incomplete until orig is resolved (e.g., by THEN).
\\
\\	    Execution:		( x -- )
\\		If  not all bits of x are zero,  continue execution at  the
\\		location specified by the resolution of orig.
\\
: 0=if
  $DB09 w,				\ orl	tos, tos
  pop,
  $850F w, here 0 ,			\ jnz	+32bit
; compilation immediate

\\	then							C I	CORE
\\
\\	    Compilation:	( orig -- )
\\		Resolve  the  forward  reference orig using the location of
\\		the execution semantics.
\\
\\	    Execution:		( -- )
\\		Continue execution.
\\
: then
  here over cell+ - swap !
; compilation immediate

\\	else							C I	CORE
\\
\\	    Compilation:	( orig1 -- orig2 )
\\		Put  the  location  of  a  new unresolved forward reference
\\		orig2  onto  the  control flow stack.  Append the execution
\\		semantics  given  below  to  the  current  definition.  The
\\		semantics will be incomplete until orig2 is resolved (e.g.,
\\		by  THEN).  Resolve  the  forward reference orig1 using the
\\		location following the appended execution semantics.
\\
\\	    Execution:		( -- )
\\		Continue  execution at the location given by the resolution
\\		of orig2.
\\
: else
  postpone ahead swap postpone then
; compilation immediate

\\	begin							C I	CORE
\\
\\	    Compilation:	( -- dest )
\\		Put the next location for a transfer of control, dest, onto
\\		the control flow stack.
\\
\\	    Execution:		( -- )
\\		Continue execution.
\\
\\	    Typical use:
\\		BEGIN	( Do nothing.)
\\		  word-sequence flag
\\		UNTIL	( If flag is false, continue after BEGIN.)
\\
\\		BEGIN	( Do nothing.)
\\		  word sequence flag
\\		WHILE	( If flag is false, continue after REPEAT.)
\\		  another-word-sequence
\\		REPEAT	( Continue execution after BEGIN.)
\\
\\		BEGIN	( Do nothing.)
\\		  word sequence flag
\\		AGAIN	( Continue execution after BEGIN.)
\\
: begin
  here
; compilation immediate

\\	again							C I	CORE EXT
\\
\\	    Compilation:	( dest -- )
\\		Append  the  execution semantics given below to the current
\\		definition, resolving the backward reference dest.
\\
\\	    Execution:		( -- )
\\		Continue execution at the location specified by dest. If no
\\		other  control  flow words are used, any program code after
\\		AGAIN will not be executed.
\\
: again
  dup here 1+ - -128 127 within		\ Range -128..127
  if
    $EB c, here 1+ - c,			\ jmp	-8bit
  else
    $E9 c, here cell+ - ,		\ jmp	-32bit
  then
; compilation immediate

\\	while							C I	CORE
\\
\\	    Compilation:	( dest -- orig dest )
\\		Put the location of a new unresolved forward reference orig
\\		onto  the  control  flow  stack,  under the existing dest..
\\		Append  the  execution semantics given below to the current
\\		definition.  The  semantics  are  incomplete until orig and
\\		dest are resolved (e.g., by REPEAT).
\\
\\	    Execution:		( x -- )
\\		If  all  bits  of  x  are  zero,  continue execution at the
\\		location specified by the resolution of orig.
\\
: while
  postpone if ( 1 postpone so =) swap
; compilation immediate

\\	repeat							C I	CORE
\\
\\	    Compilation:	( orig dest -- )
\\		Append  the  execution semantics given below to the current
\\		definition, resolving the backward reference dest.  Resolve
\\		the  forward  reference  orig  using the location following
\\		the appended execution semantics.
\\
\\	    Execution:		( -- )
\\		Continue execution at the location given by dest.
\\
: repeat
  postpone again postpone then
; compilation immediate

\\	until							C I	CORE
\\
\\	    Compilation:	( dest -- )
\\		Append  the  execution semantics given below to the current
\\		definition, resolving the backward reference dest.
\\
\\	    Execution:		( x -- )
\\		If  all  bits  of  x  are  zero,  continue execution at the
\\		location specified by dest.
\\
: until
  $DB09 w,				\ orl	tos, tos
  pop,
\  $840F w, here cell+ - ,		\ jz	-32bit
  $04 ?branch,				\ jz	-32bit
; compilation immediate

compiler definitions

\\	last-leave		( -- c-addr )				COMPILER
\\
\\		c-addr  is the address of the last LEAVE compiled.  nil  if
\\		no more LEAVEs present.
\\
value last-leave

\\	(do)		( n1|u1 n2|u2 -- ) ( R: -- sys )	C	COMPILER
\\
\\		Set  up  loop control parameters with index n2|u2 and limit
\\		n1|u1.  (DO) is compiled by DO or ?DO.
\\
\\	    Internals:
\\		( R: -- n1 n2 )
\\		n1		is the previous index
\\		n2		is the correction value for I
\\		%ecx		holds the modified value for I
\\
primitive (do)
	movl	%ecx, (%esi)		\ Save loop index
	leal	-8 (%esi), %esi
	popl	%ecx			\ New limit
	addl	$ $7fffffff , %ecx
	movl	%ecx, 4 (%esi)		\ Add for i,j
	subl	tos, %ecx		\ Loop count
	popl	tos
end-primitive

forth definitions

\\	do							C I	CORE
\\
\\	    Compilation:	( -- dodest )
\\		The  next  location for a transfer of control (dodest) goes
\\		onto the control flow stack. Append the execution semantics
\\		given below to the current definition.
\\
\\	    Execution:		( n1|u1 n2|u2 -- ) ( R: -- sys )
\\		Set  up  loop control parameters with index n2|u2 and limit
\\		n1|u1. An ambiguous condition exists if n1|u1 and n2|u2 are
\\		not  both  the  same  type.  Anything already on the return
\\		stack becomes unavailable until the loop control parameters
\\		are discarded.
\\
/ps=sp [if]

: do
  postpone (do)
  last-leave nil to last-leave
  postpone begin
; immediate compilation

[else]

: do
  $810E8B51 ,				\ pushl	%ecx		Old loop index
					\ movl	(%esi), %ecx	New limit
  $FFFFFFC1 , $7F c,			\ addl	$0x7fffffff, %ecx
  $51 c,				\ pushl	%ecx		Add for i
  $5E8BD929 ,				\ subl	tos, %ecx	Count
  $08768D04 ,				\ movl	4(%esi), tos	Pop tos
					\ leal	8(%esi), %esi
  last-leave 0 to last-leave
  postpone begin
; compilation immediate

[then]

\\	?do							C I	CORE EXT
\\
\\	    Compilation:	( -- dodest )
\\		The  next  location for a transfer of control (dodest) goes
\\		onto the control-flow stack. Append the execution semantics
\\		given below to the current definition.
\\
\\	    Execution:		( n n -- ) ( R:  -- )
\\			   -or-	( n1|u1 n2|u2 -- ) ( R: -- sys )
\\		If  n1|u1  is  equal  to  n2|u2  continue  execution at the
\\		location given by the consumer of dodest.  Otherwise set up
\\		loop  control  parameters  with index n2|u2 and limit n1|u1
\\		and continue executing immediately following ?DO.  Anything
\\		already  on  the return stack becomes unavailable until the
\\		loop   control   parameters  are  discarded.  An  ambiguous
\\		condition  exists  if  n1|u1  and n2|u2 are not both of the
\\		same type.
\\
/ps=sp [if]

: ?do
  postpone (do)

  $FFFFF981 ,				\ cmpl	$0x7fffffff, %ecx
  $840F7FFF ,				\ je	+32bit
  last-leave here to last-leave
  0 ,					\ 32bit displacement
  postpone begin
; immediate compilation

[else]

: ?do
  $B80E8B51 ,				\ pushl	%ecx		Old loop index
					\ movl	(%esi), %ecx	New limit
  $7FFFFFFF ,				\ movl	$0x7fffffff, %eax
  $C101 w,				\ addl	%eax, %ecx
  $51 c,				\ pushl	%ecx		Add for i
  $5E8BD929 ,				\ subl	tos, %ecx	Count
  $08768D04 ,				\ movl	4(%esi), tos	Pop tos
					\ leal	8(%esi), %esi
  $840FC139 ,				\ cmpl	%eax, %ecx

  last-leave here to last-leave
  0 ,					\ 32bit displacement
  postpone begin
; compilation immediate

[then]

\\	leave			( -- ) ( R: sys -- )		C	CORE
\\
\\		Discard  the  current  loop  control  parameters.  The loop
\\		parameters  must  have  been available.  Continue execution
\\		immediately following the syntactically enclosing
\\			DO ... LOOP or DO ... +LOOP.
\\		Syntax:	May only be used with a
\\		  DO ... LOOP, DO ... +LOOP, ?DO ... LOOP, or ?DO ... +LOOP
\\		in the form, for example:
\\			DO ... IF ... LEAVE THEN ... LOOP
\\
: leave
  postpone ahead
  last-leave over ! to last-leave
; compilation immediate

\\	?leave							C	CORE
\\				( true -- ) ( R: sys -- )
\\			-or-	( false -- ) ( R: -- )
\\
\\		Discard  the  current loop control parameters, if  flag  is
\\		true;  Otherwise continue execution of the loop.  The  loop
\\		parameters  must  have been available.  Continue  execution
\\		immediately following the syntactically enclosing
\\			DO ... LOOP or DO ... +LOOP.
\\		Syntax:	May only be used with a
\\		  DO ... LOOP, DO ... +LOOP, ?DO ... LOOP, or ?DO ... +LOOP
\\		in the form, for example:
\\			DO ... IF ... KEY? ?LEAVE THEN ... LOOP
\\
: ?leave
  postpone 0=if
  last-leave over ! to last-leave
; compilation immediate

\\	unloop			( -- ) ( R: sys -- )		C	CORE
\\
\\		Discard  the  loop  control  parameters.  The  loop control
\\		parameters must have been available.
\\
/ps=sp [if]

primitive unloop
	leal	8 (%esi), %esi		\ Discard loop parameter
	movl	(%esi), %ecx		\ Restore count
end-primitive compilation

[else]

primitive unloop
	movl	4 (%esp), %ecx		\ Discard
	leal	8 (%esp), %esp		\ loop parameter
end-primitive compilation

[then]

\\	loop							C I	CORE
\\
\\	    Compilation:	( dodest -- )
\\		Resolve  the  destination  of all unresolved occurrences of
\\		LEAVE  between  the  location  given by dodest and the next
\\		location  for  a  transfer of control, to execute the words
\\		following  the  LOOP.  Append the execution semantics given
\\		below to the current definition.
\\
\\	    Execution:		( -- ) ( R: sys -- )   if the loop exits,
\\				( -- ) ( R: sys1 -- sys2 )      otherwise
\\		Loop  control parameters must be available.  Add one to the
\\		loop  index.  If  the  loop index is then equal to the loop
\\		limit,  discard  the loop parameters and continue execution
\\		immediately   following   the   loop.   Otherwise  continue
\\		execution at the beginning of the loop.
\\
: loop
  $49 c,				\ decl	%ecx	Decrement index
  $01 ?branch,				\ jno	-branch
  last-leave
  begin
    dup
  while
    dup @ swap				\ Leave links
    postpone then			\ Resolves leaves
  repeat
  drop to last-leave
  postpone unloop
; compilation immediate

\\	+loop							C I	CORE
\\
\\	    Compilation:	( dodest -- )
\\		Resolve  the  destination  of all unresolved occurrences of
\\		LEAVE  between  the  location  given by dodest and the next
\\		location  for  a  transfer of control, to execute the words
\\		following +LOOP. Append the execution semantics given below
\\		to the current definition.
\\
\\	    Execution:		( n -- ) ( R: sys -- )	if the loop exits,
\\				( n -- ) ( R: sys1 -- sys2 )	otherwise
\\		Add  n  to  the  loop  index.  If  the  loop  index was not
\\		incremented  across  the  boundary  between  the loop limit
\\		minus one and the loop limit then continue execution at the
\\		location  given  by  the  top  element  of the control flow
\\		stack.
\\		However,  if  the  loop was incremented across the boundary
\\		between  the  loop  limit minus one and the loop limit then
\\		discard  the  current  loop control parameters and continue
\\		execution  immediately  following  +LOOP.  The loop control
\\		parameters must have been available.
\\
: +loop
  $D929 w,				\ subl	%ebx, %ecx	Decrement index
  pop,
  $01 ?branch,				\ jno	-branch
  last-leave
  begin
    dup
  while
    dup @ swap				\ Leave links
    postpone then			\ Resolves leaves
  repeat
  drop to last-leave
  postpone unloop
; compilation immediate

\\	i			( -- n|u ) ( R:  sys -- sys )	C	CORE
\\
\\		n|u  is  a copy of the current (innermost) loop index.  The
\\		loop control parameters must be available.
\\
/ps=sp [if]

primitive i
	push-tos
	movl	4 (%esi), tos
	subl	%ecx, tos		\ sub index add
end-primitive compilation

[else]

primitive i
	push-tos
	movl	(%esp), tos
	subl	%ecx, tos		\ sub index add
end-primitive compilation

[then]

\\	j			( -- n|u ) ( R: sys -- sys )	C	CORE
\\
\\		n|u is a copy of the index of the next outer loop.
\\		Syntax:	May only be used with a nested
\\			DO...LOOP, DO...+LOOP, ?DO...LOOP, or ?DO...+LOOP
\\		in the form, for example:
\\			: X ... DO ... DO ... J ... LOOP ... +LOOP ... ;
\\		The  loop  control parameters of the next outer nested loop
\\		must be available.
\\
/ps=sp [if]

primitive j
	push-tos
	movl	12 (%esi), tos
	subl	8 (%esi), tos		\ sub index add
end-primitive compilation

[else]

( FIXME:) 16 allot

primitive j
	push-tos
	movl	8 (%esp), tos
	subl	4 (%esp), tos		\ sub index add
end-primitive compilation

[then]

\\	case							C I	CORE EXT
\\
\\	    Compilation:	( -- case-sys )
\\		Mark  the  start  of  the CASE ... OF ... ENDOF ... ENDCASE
\\		structure.
\\
\\	    Execution:		( -- )
\\		Continue execution.
\\
\\	    Typical use:
\\		CASE	( Do nothing.)
\\		  1 OF	( if case value is not equal, continue after ENDOF )
\\		    word-sequence flag
\\		  ENDOF
\\		ENDCASE	( Do nothing.)
\\
: case
  nil
; compilation immediate

\\	of							C I	CORE EXT
\\
\\	    Compilation:	( case-sys1 -- case-sys2 of-sys )
\\		The  location  of the new unresolved forward reference goes
\\		onto the control flow stack. Append the execution semantics
\\		given below to the current definition.
\\
\\	    Execution:		( x1 x1 -- )  -or-  ( x1 x2 -- x1 )
\\		If  the  two values on the stack are not equal, discard the
\\		top  value and continue execution at the location specified
\\		by the consumer of orig (e.g., following the next ENDOF).
\\		Otherwise,  discard  both  values and continue execution in
\\		line.
\\
/less-code [if]
: of
  postpone over postpone =		\ test case value
  postpone if
  postpone drop				\ discard case value if =
; compilation immediate
[else]

/ps=sp [if]

: of
  $395BD889 ,				\ movl	%ebx, %eax
					\ popl	%ebx
  $0FC3 w,				\ cmpl	%eax, %ebx
  $85 c, here 0 ,			\ jne	+disp32
  pop,
; compilation immediate

[else]

: of
  $D889 w,				\ movl	%ebx, %eax
  pop,					\ popl	%ebx
  $850FC339 ,				\ cmpl	%eax, %ebx
  here 0 ,				\ jne	+disp32
  pop,
; compilation immediate

[then]
[then]

\\	endof							C I	CORE EXT
\\
\\	    Compilation:	( case-sys1 of-sys -- case-sys2 )
\\		Mark  the  end of the ... OF ... ENDOF ... part of the CASE
\\		structure.  The  next  location  for  a transfer of control
\\		resolves  the  reference  given  by  the top element of the
\\		control  flow  stack.  The  location  of the new unresolved
\\		forward  reference is placed beneath the dest left by CASE.
\\		Append  the  execution semantics given below to the current
\\		definition.
\\
\\	    Execution:		( -- )
\\		Continue   execution  at  the  location  specified  by  the
\\		consumer of orig2.
\\
: endof
  postpone else				\ Branch after endcase
; compilation immediate

\\	endcase							C I	CORE EXT
\\
\\	    Compilation:	( case-sys1 ... case-sys2 of-sys -- )
\\		Mark the  end  of  the  CASE ...  OF ... ENDOF  ... ENDCASE
\\		structure.  Resolve  all  of the orign which were processed
\\		after the dest left by CASE. Append the execution semantics
\\		given below to the current definition.
\\
\\	    Execution:		( x -- )
\\		Discard the case selector x and continue execution.
\\
: endcase
  postpone drop				\ discard case value
  begin
    dup
  while
    postpone then			\ resolve of(s)
  repeat
  drop
; compilation immediate
