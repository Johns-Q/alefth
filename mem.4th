\
\	ALE Forth Version 2.04
\
\		Copyright (c) 1991,1992 by Lutz Sammer.
\
\		Bug-reports via usenet: johns
\
\	File:	mem.4th
\
\		Forth kernel memory operators.
\

\ -----------------------------------------------------------------------------
\	Memory operations
\ -----------------------------------------------------------------------------

\	Single cell memory
\
\		@	!	+!	1+!	1-!
\		w@	w!	c@	c!
\
\	Double cell memory
\
\		2@	2!

\\	on			( a-addr -- )				F83
\\
\\		Store TRUE (32bit) at a-addr.
\\
/less-code [if]
: on
  true swap !
;
[else]
primitive on
	movl	$ -1 , (tos)
	pop-tos
end-primitive
[then]

\\	off			( a-addr -- )				F83
\\
\\		Store FALSE (32bit) at a-addr.
\\
/less-code [if]
: off
  false swap !
;
[else]
primitive off
	xorl	%eax, %eax
	movl	%eax, (tos)
	pop-tos
end-primitive
[then]

\\	+!			( n|u a-addr -- )			CORE
\\
\\		Add n|u to the single-cell number (32bit) at a-addr.
\\
/less-code [if]
: +!
  swap over @ + swap !
;
[else]

/ps=sp [if]

primitive +!
	popl	%eax
	addl	%eax, (tos)
	pop-tos
end-primitive

[else]

primitive +!
	movl	(%esi), %eax		\ get x
	addl	%eax, (tos)
	movl	4 (%esi), tos		\ new tos
	leal	8 (%esi), %esi
end-primitive

[then]
[then]

\\	1+!			( a-addr -- )				ALE
\\
\\		Add one to the single-cell number (32bit) at a-addr.
\\		(Increment)
\\
/less-code [if]
: 1+!
  1 over @ + swap !
;
[else]
primitive 1+!
	incl	(tos)
	pop-tos
end-primitive
[then]

\\	1-!			( a-addr -- )				ALE
\\
\\		Sub one from the single-cell number (32bit) at a-addr.
\\		(Decrement)
\\
/less-code [if]
: 1-!
  -1 over @ + swap !
;
[else]
primitive 1-!
	decl	(tos)
	pop-tos
end-primitive
[then]

\\	w@			( addr -- word )			ALE
\\
\\		Word-fetch the word (16bit) stored at addr.
\\
primitive w@
\	movzwl	(tos), tos
	movl	tos, %eax
	xorl	tos, tos
	movw	(%eax), tos
end-primitive

\\	w!			( word addr -- )			ALE
\\
\\		Word-store the word (16bit) at addr.
\\
/ps=sp [if]

primitive w!
	popl	%eax
	movw	%ax, (tos)
	popl	tos
end-primitive

[else]

primitive w!
	movl	(%esi), %eax		\ get word
	movw	%ax, (tos)
	movl	4 (%esi), tos		\ new tos
	leal	8 (%esi), %esi
end-primitive

[then]

\\	c@			( c-addr -- char )			CORE
\\
\\		Char-fetch the character stored at c-addr.
\\
primitive c@
\	movzbl	(tos), tos			\ 4 Cycles
	movl	tos, %eax			\ 3 Cycles
	xorl	tos, tos
	movb	(%eax), tos			\ Don't ask me why this quicker
end-primitive

\\	c!			( char c-addr -- )			CORE
\\
\\		Char-store the character at c-addr.
\\
/ps=sp [if]

primitive c!
	popl	%eax
	movb	%ax, (tos)
	popl	tos
end-primitive

[else]

primitive c!
	movl	(%esi), %eax		\ get char
	movb	%al, (tos)
	movl	4 (%esi), tos		\ new tos
	leal	8 (%esi), %esi
end-primitive

[then]

\\	2@			( a-addr -- x1 x2 )			CORE
\\
\\		Fetch the cell pair x1 x2 stored at a-addr. x2 is stored at
\\		a-addr   and   x1  at  the  next  consecutive  cell. It  is
\\		equivalent to the sequence DUP CELL+ @ SWAP @.
\\
/less-code [if]
: 2@
  dup cell+ @ swap @
;
[else]

/ps=sp [if]

primitive 2@
	movl	4 (tos), %eax
	pushl	%eax			\ x1
	movl	(tos), tos		\ x2
end-primitive

[else]

primitive 2@
	leal	-4 (%esi), %esi
	movl	4 (tos), %eax
	movl	%eax, (%esi)		\ x1
	movl	(tos), tos
end-primitive

[then]
[then]

\\	2!			( x1 x2 a-addr -- )			CORE
\\
\\		Store  the cell pair x1 x2 at a-addr, with x2 at a-addr and
\\		x1  at  the  next consecutive cell. It is equivalent to the
\\		sequence SWAP OVER ! CELL+ !.
\\
/less-code [if]
: 2!
  swap over ! cell+ !
;
[else]
/ps=sp [if]

primitive 2!
	popl	%eax			\ x2
	movl	%eax, (tos)
	popl	%eax			\ x1
	movl	%eax, 4 (tos)
	pop-tos
end-primitive

[else]

primitive 2!
	movl	(%esi), %eax		\ get x2
	movl	%eax, (tos)
	movl	4 (%esi), %eax		\ get x1
	movl	%eax, 4 (tos)

	movl	8 (%esi), tos		\ new tos
	leal	12 (%esi), %esi
end-primitive

[then]
[then]
