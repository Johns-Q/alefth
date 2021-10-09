\
\	ALE Forth Version 2.04
\
\		Copyright (c) 1991,1992 by Lutz Sammer.
\
\		Bug-reports via usenet: johns
\
\	File:	stacks.4th
\
\		Forth kernel stack operators.
\

\ -----------------------------------------------------------------------------
\	Parameter stack manipulation
\ -----------------------------------------------------------------------------

\\	Parameter stack managment
\\
\\		sp0   sp!  sp@
\\
\\	Single cell parameter stack
\\
\\		?dup  depth  discard  drop  dup   nip  over  pick
\\		pluck roll   rot      swap  tuck  -rot
\\
\\	Double cell parameter stack
\\
\\		2drop  2dup  2over  2rot  2swap
\\

\\	sp0			( -- psp )				ALE
\\
\\		psp is the initial parameter stack pointer.
\\
value sp0

\\	over			( x1 x2 -- x1 x2 x1 )			CORE
\\
\\		Place a copy of x1 (second stack item) on top of the stack.
\\
/less-code [if]
: over
  >r dup r> swap
;
[else]

/ps=sp [if]

primitive over
	push-tos
	movl	4 (%esp), tos
end-primitive

[else]

primitive over
	push-tos
	movl	4 (%esi), tos
end-primitive

[then]
[then]

\\	?dup			( x -- x x ) -or- ( 0 -- 0 )		CORE
\\
\\		Duplicate x if it is non-zero.
\\
/less-code [if]
: ?dup
  dup  if  dup  then
;
[else]

/ps=sp [if]

primitive ?dup
	orl	tos, tos
	jz	1 <cr>
	push-tos
end-primitive

[else]

primitive ?dup
	orl	tos, tos
	jz	5 <cr>
	push-tos
end-primitive

[then]
[then]

\\	depth			( -- +n )				CORE
\\
\\		+n is the  number of one cell values  contained in the data
\\		stack before +n was placed on the stack.
\\
/less-code [if]
: depth
  sp@ sp0 swap - cell /
;
[else]

/ps=sp [if]

primitive depth
	push-tos
	movl	Dsp0 , tos
\ FIXME: won't write:	movl	' sp0 >body , tos
	subl	%esp, tos
	sarl	$ 2 , tos
end-primitive

[else]

primitive depth
	push-tos
	movl	Dsp0 , tos
	subl	%esi, tos
	sarl	$ 2 , tos
end-primitive

[then]
[then]

\\	discard			( n*x n -- )				ALE
\\
\\		Discard n items x from the stack.
\\
/less-code [if]
: discard
  0  do  drop  loop
;
[else]

/ps=sp [if]

primitive discard
	leal	(%esp, tos, 4), %esp
	pop-tos
end-primitive

[else]

primitive discard
	leal	(%esi, tos, 4), %esi
	pop-tos
end-primitive

[then]
[then]

\\	nip			( x1 x2 -- x2 )				CORE EXT
\\
\\		Drop the first item below the top of stack.
\\
/less-code [if]
: nip
  swap drop
;
[else]

/ps=sp [if]

primitive nip
	popl	%eax
end-primitive

[else]

primitive nip
	leal	4 (%esi), %esi
end-primitive

[then]
[then]

\\	pick	( x(u) ... x(1) x(0) u -- x(u) ... x(1) x(0) x(u) )	CORE EXT
\\
\\		Remove  u.  Copy  the uth item to the top of the stack.  An
\\		ambiguous condition exists if there are less than u+2 items
\\		on the stack before PICK is executed.
\\
\\	    Typical use:
\\		0 PICK		is equivalent to DUP
\\		1 PICK		is equivalent to OVER
\\		2 PICK		is equivalent to PLUCK
\\
/less-code [if]
: pick
  1 + cells sp@ + @
;
[else]

/ps=sp [if]

primitive pick
	movl	(%esp, tos, 4), tos
end-primitive

[else]

primitive pick
	movl	(%esi, tos, 4), tos
end-primitive

[then]
[then]

\\	pluck			( x1 x2 x3 -- x1 x2 x3 x1 )		CORE
\\
\\		Place a copy of x1 (third stack item) on top of the stack.
\\
/less-code [if]
: pluck
  2 pick
;
[else]

/ps=sp [if]

primitive pluck
	push-tos
	movl	8 (%esp), tos
end-primitive

[else]

primitive pluck
	push-tos
	movl	8 (%esi), tos
end-primitive

[then]
[then]

\\	roll	( x(u) x(u-1) ... x(0) u -- x(u-1) ... x(0) x(u) )	CORE EXT
\\
\\		Remove  u.  Rotate  u+1  items on the top of the stack.  An
\\		ambiguous  condition  exists  if  there  are  less than u+2
\\		entries on the stack before ROLL is executed.
\\
\\	    Typical use:
\\		0 ROLL		is a null operation
\\		1 ROLL		is equivalent to SWAP
\\		2 ROLL		is equivalent to ROT
\\
/less-code [if]
: roll
  >r r@ pick  sp@ dup cell+  r> 1 + cells cmove> drop
;
[else]

/ps=sp [if]

primitive roll
	pushl	%ecx
	pushl	%edi
	pushl	%esi			\ Save register
	movl	tos, %ecx
	leal	12 (%esp, %ecx, 4), %edi
	movl	(%edi), tos		\ x(u)
	leal	-4 (%edi), %esi
	std
	rep
	movsl	(%esi), (%edi)
	cld
	popl	%esi			\ Restore register
	popl	%edi
	popl	%ecx
	popl	%eax			\ Remove u
end-primitive

[else]

primitive roll
	pushl	%ecx
	pushl	%edi
	pushl	%esi
	movl	tos, %ecx
	leal	(%esi, %ecx, 4), %edi
	movl	(%edi), tos		\ x(u)
	leal	-4 (%edi), %esi
	std
	rep
	movsl	(%esi), (%edi)
	cld
	popl	%esi
	popl	%edi
	popl	%ecx
	leal	4 (%esi), %esi		\ Remove u
end-primitive

[then]
[then]

\\	rot			( x1 x2 x3 -- x2 x3 x1 )		CORE
\\
\\		Rotate the top three stack entries up.
\\
/less-code [if]
: rot
  >r swap r> swap
;
[else]

/ps=sp [if]

primitive rot
	popl	%eax
	popl	%edx
	pushl	%eax
	pushl	tos
	movl	%edx, tos
end-primitive

[else]

primitive rot
	movl	(%esi), %eax		\ x2
	movl	tos, (%esi)		\ x3->[x2]
	movl	4 (%esi), tos		\ x1->[x3]
	movl	%eax, 4 (%esi)		\ x2->[x1]
end-primitive

[then]
[then]

\\	tuck			( x1 x2 -- x2 x1 x2 )			CORE EXT
\\
\\		Tuck the first (top) stack item below the second stack item.
\\
/less-code [if]
: tuck
  swap over
;
[else]

/ps=sp [if]

primitive tuck
	popl	%eax
	pushl	tos
	pushl	%eax
end-primitive

[else]

primitive tuck
	movl	(%esi), %eax
	movl	tos, (%esi)
	leal	-4 (%esi), %esi
	movl	%eax, (%esi)
end-primitive

[then]
[then]

\\	-rot			( x1 x2 x3 -- x3 x1 x2 )		CORE
\\
\\		Rotate the top three stack entries down.
\\
/less-code [if]
: -rot
  rot rot
;
[else]

/ps=sp [if]

primitive -rot
	popl	%eax			\ x2
	popl	%edx			\ x1
	pushl	tos
	pushl	%edx
	movl	%eax, tos
end-primitive

[else]

primitive -rot
	movl	4 (%esi), %eax		\ x1
	movl	tos, 4 (%esi)		\ x3->[x1]
	movl	(%esi), tos		\ x2->[x3]
	movl	%eax, (%esi)		\ x1->[x2]
end-primitive

[then]
[then]

\\	2drop			( x1 x2 -- )				CORE
\\
\\		Drop cell pair x1 x2 from the stack.
\\
/less-code [if]
: 2drop
  drop drop
;
[else]

/ps=sp [if]

primitive 2drop
	popl	tos
	popl	tos
end-primitive

[else]

primitive 2drop
	movl	4 (%esi), tos
	leal	8 (%esi), %esi
end-primitive

[then]
[then]

\\	2dup			( x1 x2 -- x1 x2 x1 x2 )		CORE
\\
\\		Duplicate cell pair x1 x2.
\\
/less-code [if]
: 2dup
  over over
;
[else]

/ps=sp [if]

primitive 2dup
	movl	(%esp), %eax
	pushl	tos
	pushl	%eax
end-primitive

[else]

primitive 2dup
	movl	(%esi), %eax		\ x1
	leal	-8 (%esi), %esi
	movl	tos, 4 (%esi)
	movl	%eax, (%esi)
end-primitive

[then]
[then]

\\	2swap			( x1 x2 x3 x4 -- x3 x4 x1 x2 )		CORE
\\
\\		Exchange the top two cell pairs.
\\
/less-code [if]
: 2swap
  >r -rot r> -rot
;
[else]

/ps=sp [if]

primitive 2swap
	popl	%eax			\ x3
	popl	%edx			\ x2
	pushl	tos
	movl	%edx, tos		\ x4 <-> x2
	movl	4 (%esp), %edx
	movl	%eax, 4 (%esp)		\ x3 <-> x1
	pushl	%edx			\ x3
end-primitive

[else]

primitive 2swap
	xchgl	4 (%esi), tos
	movl	(%esi), %eax
	xchgl	%eax, 8 (%esi)
	movl	%eax, (%esi)
end-primitive

[then]
[then]

\\	2rot	( x1 x2 x3 x4 x5 x6 -- x3 x4 x5 x6 x1 x2 )	DOUBLE EXT
\\
\\		Rotate  the top three cell pairs on the stack bringing cell
\\		pair x1 x2 to the top of the stack.
\\
/less-code [if]
: 2rot
  2>r 2swap 2r> 2swap
;
[else]

/ps=sp [if]

primitive 2rot
	popl	%edx
	movl	12 (%esp), %eax
	pushl	%eax			\ x1
	movl	8 (%esp), %eax
	movl	%eax, 16 (%esp)		\ x3
	movl	%edx, 8 (%esp)		\ x5
	movl	tos, %edx
	movl	12 (%esp), %eax
	movl	%eax, tos		\ x2
	movl	4 (%esp), %eax
	movl	%eax, 12 (%esp)		\ x4
	movl	%edx, 4 (%esp)		\ x6
end-primitive

[else]

primitive 2rot
	movl	(%esi), %edx
	movl	16 (%esi), %eax
	movl	%eax, (%esi)		\ x1
	movl	8 (%esi), %eax
	movl	%eax, 16 (%esi)		\ x3
	movl	%edx, 8 (%esi)		\ x5
	movl	tos, %edx
	movl	12 (%esi), %eax
	movl	%eax, tos		\ x2
	movl	4 (%esi), %eax
	movl	%eax, 12 (%esi)		\ x4
	movl	%edx, 4 (%esi)		\ x6
end-primitive

[then]
[then]

\\	2over		( x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 )		CORE
\\
\\		Copy cell pair x1 x2 to the top of the stack.
\\
/less-code [if]
: 2over
  2>r 2dup 2r> 2swap
;
[else]

/ps=sp [if]

primitive 2over
	pushl	tos
	movl	12 (%esp), %eax
	pushl	%eax			\ looks silly, but fixer
	movl	12 (%esp), tos
end-primitive

[else]

primitive 2over
	leal	-8 (%esi), %esi
	movl	tos, 4 (%esi)
	movl	16 (%esi), %eax
	movl	%eax, (%esi)
	movl	12 (%esi), tos
end-primitive

[then]
[then]

\ -----------------------------------------------------------------------------
\	Parameter+Return stack manipulation
\ -----------------------------------------------------------------------------

\\	Single cell parameter to/from return stack
\\
\\		>r  r>
\\
\\	Double cell parameter to/from return stack
\\
\\		2>r  2r>
\\

\\	2>r			( x1 x2 -- ) ( R: -- x1 x2 )	C	CORE EXT
\\			- or -  ( d1 -- )    ( R: -- d1 )
\\
\\		Transfer cell pair x1 x2 to the return stack.  Semantically
\\		equivalent to SWAP >R >R.
\\
/less-code [if]
: 2>r
  r> rot >r swap >r >r
; compilation
[else]

/ps=sp [if]

primitive 2>r
	movl	tos, -4 (%esi)
	popl	tos
	movl	tos, (%esi)
	leal	-8 (%esi), %esi
	pop-tos
end-primitive compilation

[else]

primitive 2>r
	movl	(%esi), %eax
	pushl	%eax
\	pushl	(%esi)			\ 2 Cycles slower on 486
	pushl	tos
	movl	4 (%esi), tos
	leal	8 (%esi), %esi
end-primitive compilation

[then]
[then]

\\	2r>			( -- x1 x2 ) ( R: x1 x2 -- )	C	CORE EXT
\\
\\		Transfer   cell   pair   x1   x2  from  the  return  stack.
\\		Semantically equivalent to R> R> SWAP.
\\
/less-code [if]
: 2r>
  r> r> r> swap rot >r
; compilation
[else]

/ps=sp [if]

primitive 2r>
	push-tos
	leal	8 (%esi), %esi
	movl	(%esi), tos
	pushl	tos
	movl	-4 (%esi), tos
end-primitive compilation

[else]

primitive 2r>
	push-tos
	leal	-4 (%esi), %esi
	popl	tos
\	popl	(%esi)			\ 3 Cycles slower on 486
	popl	%eax
	movl	%eax, (%esi)
end-primitive compilation

[then]
[then]

\ -----------------------------------------------------------------------------
\	Return stack manipulation
\ -----------------------------------------------------------------------------

\\	Return stack managment
\\
\\		rp0   rp!  rp@
\\
\\	Single cell return stack
\\
\\		r@
\\
\\	Double cell return stack
\\
\\		2r@
\\

\\	rp0			( -- rsp )				ALE
\\
\\		Initial return stack.
\\
value rp0

\\	r@			( -- x ) ( R:  x -- x )		C	CORE
\\
\\		Copy x from the return stack to the data stack.
\\
/less-code [if]
: r@
  r> r> dup >r swap >r
; compilation
[else]

/ps=sp [if]

primitive r@
	push-tos
	movl	4 (%esi), tos
end-primitive compilation

[else]

primitive r@
	push-tos
	movl	(%esp), tos
end-primitive compilation

[then]
[then]

\\	2r@		( -- x1 x2 ) ( R: x1 x2 -- x1 x2 )	C	CORE EXT
\\
\\		Copy cell pair x1 x2 from the return stack.
\\		Semantically equivalent to R> R> 2DUP >R >R SWAP.
\\
/less-code [if]
: 2r@
  r> 2r> 2dup 2>r rot >r
; compilation
[else]

/ps=sp [if]

primitive 2r@
	push-tos
	movl	8 (%esi), tos
	pushl	tos
	movl	4 (%esi), tos
end-primitive compilation

[else]

primitive 2r@
	leal	-8 (%esi), %esi
	movl	tos, 4 (%esi)
	movl	(%esp), tos
	movl	4 (%esp), %eax
	movl	%eax, (%esi)
end-primitive compilation

[then]
[then]
