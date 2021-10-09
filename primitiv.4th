\
\	ALE Forth Version 2.04
\
\		Copyright (c) 1991,1992 by Lutz Sammer.
\
\		Bug-reports via usenet: johns
\
\	File:	primitiv.4th
\
\		Forth kernel minimum primitives.
\

\ -----------------------------------------------------------------------------
\	Parameter stack manipulation
\ -----------------------------------------------------------------------------

\\	sp!			( i*x n -- n*x )			ALE
\\
\\		Set the parameter stack pointer to n.
\\		Warning different from FIG Forth.
\\
/ps=sp [if]

primitive sp!
	movl	tos, %esp
	pop-tos
end-primitive

[else]

primitive sp!
	movl	tos, %esi
	pop-tos
end-primitive

[then]

\\	sp@			( -- a-addr )				ALE
\\
\\		Return  the parameter stack pointer.  a-addr is the address
\\		of the top element on stack, before a-addr is placed on the
\\		stack.
\\
/ps=sp [if]

primitive sp@
	push-tos
	movl	%esp, tos
end-primitive

[else]

primitive sp@
	push-tos
	movl	%esi, tos
end-primitive

[then]

\\	drop			( x -- )				CORE
\\
\\		Remove x from the stack.
\\
primitive drop
	pop-tos
end-primitive

\\	dup			( x -- x x )				CORE
\\
\\		Duplicate x.
\\
primitive dup
	push-tos
end-primitive

\\	swap			( x1 x2 -- x2 x1 )			CORE
\\
\\		Exchange the top two stack items.
\\
/ps=sp [if]

primitive swap
	popl	%eax
	pushl	tos
	movl	%eax, tos
end-primitive

[else]

exist target-i386 [if]

primitive swap
	xchgl	 (%esi), tos		\ 7 Cycles 486
end-primitive

[else]

primitive swap
	movl	(%esi), %eax
	movl	tos, (%esi)
	movl	%eax, tos		\ 3 Cycles 486
end-primitive

[then]
[then]

\ -----------------------------------------------------------------------------
\	Return stack manipulation
\ -----------------------------------------------------------------------------

\\	rp!			( R: i*x n -- n*x )			ALE F79
\\
\\		Set the return stack to n.
\\
/ps=sp [if]

primitive rp!
	leal	-4 (tos), %esi
	pop-tos
end-primitive compilation

[else]

primitive rp!
	movl	tos, %esp
	pop-tos
end-primitive compilation

[then]

\\	rp@			( -- rp )				ALE F79
\\
\\		Returns current return stack pointer.
\\
/ps=sp [if]

primitive rp@
	push-tos
	leal	4 (%esi), tos
end-primitive compilation

[else]

primitive rp@
	push-tos
	movl	%esp, tos
end-primitive compilation

[then]

\ -----------------------------------------------------------------------------
\	Parameter+Return stack manipulation
\ -----------------------------------------------------------------------------

\\	>r			( x -- ) ( R:  -- x )		C	CORE
\\
\\		Move x to the return stack.
\\
/ps=sp [if]

primitive >r
	movl	tos, (%esi)
	leal	-4 (%esi), %esi
	pop-tos
end-primitive compilation

[else]

primitive >r
	pushl	tos
	pop-tos
end-primitive compilation

[then]

\\	r>			( -- x ) ( R:  x -- )		C	CORE
\\
\\		Move x from the return stack to the data stack.
\\
/ps=sp [if]

primitive r>
	push-tos
	leal	4 (%esi), %esi
	movl	(%esi), tos
end-primitive compilation

[else]

primitive r>
	push-tos
	popl	tos
end-primitive compilation

[then]

\ -----------------------------------------------------------------------------
\	Arithmetic operations
\ -----------------------------------------------------------------------------

\\	+			( n1|u1 n2|u2 -- n3|u3 )		CORE
\\
\\		Add n2|u2 to n1|u1, giving the sum n3|u3.
\\
/ps=sp [if]

primitive +
	popl	%eax
	addl	%eax, tos
end-primitive

[else]

primitive +
	addl	(%esi), tos
	inc-csp
end-primitive

[then]

\ -----------------------------------------------------------------------------
\	Logical operations
\ -----------------------------------------------------------------------------

\\	and			( x1 x2 -- x3 )				CORE
\\
\\		x3 is the bit-by-bit logical 'and' of x1 with x2.
\\
/ps=sp [if]

primitive and
	popl	%eax
	andl	%eax, tos
end-primitive

[else]

primitive and
	andl	(%esi), tos
	inc-csp
end-primitive

[then]

\\	or			( x1 x2 -- x3 )				CORE
\\
\\		x3 is the bit-by-bit inclusive-or of x1 with x2.
\\
/ps=sp [if]

primitive or
	popl	%eax
	orl	%eax, tos
end-primitive

[else]

primitive or
	orl	(%esi), tos
	inc-csp
end-primitive

[then]

\\	xor			( x1 x2 -- x3 )				CORE
\\
\\		x3 is the bit-by-bit exclusive-or of x1 with x2.
\\
/ps=sp [if]

primitive xor
	popl	%eax
	xorl	%eax, tos
end-primitive

[else]

primitive xor
	xorl	(%esi), tos
	inc-csp
end-primitive

[then]

\\	0<			( n -- flag )				CORE
\\
\\		flag is true if n is less than zero.
\\
primitive 0<
	addl	$ $80000000 , tos
	sbbl	tos, tos		\ -1 true, 0 false
end-primitive

\ -----------------------------------------------------------------------------
\	Memory operations
\ -----------------------------------------------------------------------------

\\	@			( a-addr -- x )				CORE
\\
\\		Fetch x (32bit) stored at a-addr.
\\
primitive @
	movl	(tos), tos
end-primitive

\\	!			( x a-addr -- )				CORE
\\
\\		Store x (32bit) at a-addr.
\\
/ps=sp [if]

primitive !
	popl	%eax
	movl	%eax, (tos)
	popl	tos
end-primitive

[else]

primitive !
	movl	(%esi), %eax		\ get x
	movl	%eax, (tos)
	movl	4 (%esi), tos		\ new tos
	leal	8 (%esi), %esi
end-primitive

[then]
