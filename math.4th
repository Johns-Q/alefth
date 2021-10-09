\
\	ALE Forth Version 2.04
\
\		Copyright (c) 1991,1992 by Lutz Sammer.
\
\		Bug-reports via usenet: johns
\
\	File:	math.4th
\
\		Forth kernel mathematic operators.
\

\ -----------------------------------------------------------------------------

\	Single cell constants
\
\		nil
\
\	Single cell logic constants
\
\		false	true
\
\	Single cell comparison
\
\		<	<>	=	>	u<	u>
\		0<	0<>	0=	0>
\
\	Single cell arithmetik
\
\		+	-	<<	>>	u>>	shift
\		*	*/mod	*/	/mod	/	mod
\		min	max	abs	negate
\		1+	1-	2*	2/
\
\	Single cell logic
\
\		invert	and	or	xor	within
\
\	Mixed cell arithmetik
\
\		m*	um*	um/mod	sm/mod	s->
\
\	Double cell arithmetik
\
\		du<	d+	d-	d2*	d2/ dnegate
\

\ -----------------------------------------------------------------------------
\	Single cell mathematic constants
\ -----------------------------------------------------------------------------

\\	nil			( -- 0 )				ALE
\\
\\		Returns a nil pointer, all bits unset.
\\
0	constant nil

\\	false			( -- false )				CORE EXT
\\
\\		Return a false flag, all bits unset.
\\
0	constant false

\\	true			( -- true )				CORE EXT
\\
\\		Return a true flag, all bits set.
\\
-1	constant true

\ -----------------------------------------------------------------------------
\	Comparison operations
\ -----------------------------------------------------------------------------

\\	0>			( n -- flag )				CORE EXT
\\
\\		flag is true if n is greater than zero.
\\
/less-code [if]
: 0>
  negate 0<
;
[else]
primitive 0>
	negl	tos
	addl	$ $80000000 , tos
	sbbl	tos, tos		\ 0> -1, false 0
end-primitive
[then]
[then]

\\	0<>			( n|u -- flag )				CORE EXT
\\
\\		flag is true if n|u is not equal to 0.
\\
/less-code [if]
: 0<>
  if  true  else  false  then
;
[else]
primitive 0<>
	negl	tos
	sbbl	tos, tos		\ true -1, false 0
end-primitive
[then]
[then]

\\	0=			( n|u -- flag )				CORE
\\
\\		flag is true if n|u is equal to zero.
\\
/less-code [if]
: 0=
  if  false  else  true  then
;
[else]
primitive 0=
	subl	$ 1 , tos
	sbbl	tos, tos		\ true -1, false 0
end-primitive
[then]
[then]

\\	u<			( u1 u2 -- flag )			CORE EXT
\\
\\		flag is true if u1 is less than u2.
\\
/less-code [if]
: u<
  2dup xor 0<  if  nip  else  -  then  0<
;
[else]
/ps=sp [if]

primitive u<
	popl	%eax
	cmpl	tos, %eax		\ Sets the carry if u<
	sbbl	tos, tos		\ -1 true, 0 false
end-primitive

[else]

primitive u<
	cmpl	tos, (%esi)		\ Sets the carry if u<
	sbbl	tos, tos		\ -1 true, 0 false
	inc-csp
end-primitive

[then]
[then]

\\	u>			( u1 u2 -- flag )			CORE EXT
\\
\\		flag is true if u1 is greater than u2.
\\
/less-code [if]
: u>
  swap u<
;
[else]
/ps=sp [if]

primitive u>
	popl	%eax
	cmpl	%eax, tos		\ Sets the carry if u<
	sbbl	tos, tos		\ -1 true, 0 false
end-primitive

[else]

primitive u>
	cmpl	(%esi), tos		\ Sets the carry if u<
	sbbl	tos, tos		\ -1 true, 0 false
	inc-csp
end-primitive

[then]
[then]

\\	<>			( x1 x2 -- flag )			CORE EXT
\\
\\		flag is true if x1 is not bit-for-bit the same as x2.
\\
/less-code [if]
: <>
  xor 0<>
;
[else]
/ps=sp [if]

primitive <>
	popl	%eax
	xorl	%eax, tos
	negl	tos
	sbbl	tos, tos		\ true -1, false 0
end-primitive

[else]

primitive <>
	subl	(%esi), tos
	negl	tos
	sbbl	tos, tos		\ true -1, false 0
	inc-csp
end-primitive

[then]
[then]

\\	=			( x1 x2 -- flag )			CORE
\\
\\		flag is true if x1 is bit-for-bit the same as x2.
\\
/less-code [if]
: =
  xor 0=
;
[else]

/ps=sp [if]

primitive =
	popl	%eax
	subl	%eax, tos
	subl	$ 1 , tos
	sbbl	tos, tos		\ true -1, false 0
end-primitive

[else]

primitive =
	subl	(%esi), tos
	subl	$ 1 , tos
	sbbl	tos, tos		\ true -1, false 0
	inc-csp
end-primitive

[then]
[then]

\\	<			( n1 n2 -- flag )			CORE
\\
\\		flag is true if n1 is less than n2.
\\
/less-code [if]
: <
  2dup xor 0<  if  drop  else  -  then  0<
;
[else]
/ps=sp [if]

( FIXME:) 16 allot

primitive <
	popl	%eax
	subl	tos, %eax		\ Sets the carry if u<
	addl	$ $80000000 , %eax
	sbbl	tos, tos		\ -1 true, 0 false
end-primitive

[else]

primitive <
	movl	(%esi), %eax
	subl	tos, %eax		\ Sets the carry if u<
	addl	$ $80000000 , %eax
	sbbl	tos, tos		\ -1 true, 0 false
	inc-csp
end-primitive

[then]
[then]

\\	>			( n1 n2 -- flag )			CORE
\\
\\		flag is true if n1 is greater than n2.
\\
/less-code [if]
: >
  swap <
;
[else]
/ps=sp [if]

primitive >
	popl	%eax
	subl	%eax, tos		\ Sets the carry if u<
	addl	$ $80000000 , tos
	sbbl	tos, tos		\ -1 true, 0 false
end-primitive

[else]

primitive >
	subl	(%esi), tos		\ Sets the carry if u<
	addl	$ $80000000 , tos
	sbbl	tos, tos		\ -1 true, 0 false
	inc-csp
end-primitive

[then]
[then]

\ -----------------------------------------------------------------------------
\	Arithmetic operations
\ -----------------------------------------------------------------------------

\\	-			( n1|u1 n2|u2 -- n3|u3 )		CORE
\\
\\		Subtract n2|u2 from n1|u1, giving the difference n3|u3.
\\
/less-code [if]
: -
  negate +
;
[else]

/ps=sp [if]

primitive -
	popl	%eax
	negl	tos
	addl	%eax, tos		\ n1-n2
end-primitive

[else]

primitive -
	negl	tos
	addl	(%esi), tos		\ n1-n2
	inc-csp
end-primitive

[then]
[then]

\\	<<			( x1 n -- x2 )				ALE
\\
\\		Shift x1 n bits left, giving the result x2.
\\		Put zero into the places "uncovered" by the shift.
\\
/ps=sp [if]

primitive <<
	movl	%ecx, %eax		\ Preserve %ecx
	movl    tos, %ecx
	popl	tos
	shll	%cl, tos
	movl	%eax, %ecx
end-primitive

[else]

primitive <<
	movl	%ecx, %eax		\ Preserve %ecx
	movl    tos, %ecx
	movl	(%esi), tos
	shll	%cl, tos
	movl	%eax, %ecx
	inc-csp
end-primitive

[then]

\\	>>			( x1 n -- x2 )				ALE
\\
\\		Signed shift x1 n bits right, giving the result x2.
\\		Keeps the sign of x1.
\\
/ps=sp [if]

primitive >>
	movl	%ecx, %eax		\ Preserve %ecx
	movl    tos, %ecx
	popl	tos
	sarl	%cl, tos
	movl	%eax, %ecx
end-primitive

[else]

primitive >>
	movl	%ecx, %eax		\ Preserve %ecx
	movl    tos, %ecx
	movl	(%esi), tos
	shrl	%cl, tos
	movl	%eax, %ecx
	inc-csp
end-primitive

[then]

\\	u>>			( x1 n -- x2 )				ALE
\\
\\		Unsigned shift x1 n bits right, giving the result x2.
\\		Put zero into the places "uncovered" by the shift.
\\
/ps=sp [if]

primitive u>>
	movl	%ecx, %eax		\ Preserve %ecx
	movl    tos, %ecx
	popl	tos
	shrl	%cl, tos
	movl	%eax, %ecx
end-primitive

[else]

primitive u>>
	movl	%ecx, %eax		\ Preserve %ecx
	movl    tos, %ecx
	movl	(%esi), tos
	shrl	%cl, tos
	movl	%eax, %ecx
	inc-csp
end-primitive

[then]

\\	shift			( x1 n -- x2 )				CORE
\\
\\		Perform  a  logical shift of n bit-places on x1, giving x2.
\\		If  n  is positive, shift the bits n places toward the most
\\		significant  bit. If  n  is negative, shift them toward the
\\		least   significant   bits.   Put   zero  into  the  places
\\		"uncovered" by the shift.
\\
/less-code [if]
: shift
  dup 0<
  if
    negate u>>
  else
    <<
  then
;
[else]

/ps=sp [if]

code shift
	movl	%ecx, %eax		\ Preserve %ecx
	movl	tos, %ecx
	popl	tos
	orl	%ecx, %ecx
	js	0f
	shll	%cl, tos
	jmp	1f
0:
	negl	%ecx
	shrl	%cl, tos
1:
	movl	%eax, %ecx
end-code

[else]

code shift
	movl	%ecx, %eax		\ Preserve %ecx
	movl	tos, %ecx
	movl	(%esi), tos
	orl	%ecx, %ecx
	js	0f
	shll	%cl, tos
	jmp	1f
0:
	negl	%ecx
	shrl	%cl, tos
1:
	movl	%eax, %ecx
	inc-csp
end-code

[then]
[then]

\\	*			( n1|u1 n2|u2 -- n3|u3 )		CORE
\\
\\		Multiply n1|u1 by n2|u2 giving the product n3|u3.
\\
/ps=sp [if]

primitive *
	popl	%eax
	imull	%eax, tos		\ 32 Bit!
end-primitive

[else]

primitive *
	imull	(%esi), tos		\ 32 Bit!
	inc-csp
end-primitive

[then]

\\	*/mod			( n1 n2 n3 -- n4 n5 )			CORE
\\
\\		Multiply  n1  by  n2 producing the intermediate double-cell
\\		result   d.  Divide  d  by  n3  producing  the  single-cell
\\		remainder n4 and the single-cell quotient n5.
\\		An  ambiguous  condition  exists  if  n3 is zero, or if the
\\		quotient  n5 lies outside the range of a single-cell signed
\\		integer.  If  d  and  n3 differ in sign the implementation-
\\		defined  result  returned  will  be  the same as either the
\\		phrase
\\			>R M* R> FM/MOD
\\		or the phrase
\\			>R M* R> SM/MOD.
\\
/ps=sp [if]

primitive */mod
	popl	%edx			\ n2
	popl	%eax			\ n1
	imull	%edx			\ n1*n2	64BIT	EAX as tos worse
	idivl	tos			\ d/n3
	pushl	%edx			\ n4 remainder
	movl	%eax, tos		\ n5 quotient
end-primitive

[else]

primitive */mod
	movl	4 (%esi), %eax
	imull	(%esi)			\ n1*n2 = 64 Bit result
	idivl	tos
	inc-csp
	movl	%edx, (%esi)		\ n4 remainder
	movl	%eax, tos
end-primitive

[then]

\\	*/			( n1 n2 n3 -- n4 )			CORE
\\
\\		Multiply  n1  by  n2 producing the double-cell intermediate
\\		result  d.  Divide  d by n3 giving the single-cell quotient
\\		n4.
\\		An  ambiguous  condition  exists  if  n3  is zero or if the
\\		quotient n4 lies outside the range of a signed number. If d
\\		and  n3  differ  in  sign the implementation-defined result
\\		returned will be the same as either the phrase
\\			>R M* R> FM/MOD SWAP DROP
\\		or the phrase
\\			>R M* SM/MOD SWAP DROP.
\\
/less-code [if]
: */
  */mod nip
;
[else]

/ps=sp [if]

primitive */
	popl	%edx			\ n2
	popl	%eax			\ n1
	imull	%edx			\ n1*n2	64BIT	EAX as tos worse
	idivl	tos			\ d/n3
	movl	%eax, tos		\ n4 quotient
end-primitive

[else]

primitive */
	movl	4 (%esi), %eax		\ n1
	imull	(%esi)			\ 64 Bit	EAX as tos worse
	idivl	tos
	movl	%eax, tos
	leal	8 (%esi), %esi
end-primitive

[then]
[then]

\\	/mod			( n1 n2 -- n3 n4 )			CORE
\\
\\		Divide  n1  by  n2, giving the single-cell remainder n3 and
\\		the single-cell quotient n4.  An ambiguous condition exists
\\		if   n2   is  zero.  If  n1  and  n2  differ  in  sign  the
\\		implementation-defined  result returned will be the same as
\\		either the phrase
\\			>R S>D R> FM/MOD
\\		or the phrase
\\			>R S>D R> SM/MOD.
\\
/ps=sp [if]

primitive /mod
	popl	%eax
	cltd
	idivl	tos			\ EAX as tos worse
	pushl	%edx
	movl	%eax, tos
end-primitive

[else]

primitive /mod
	movl	(%esi), %eax
	cltd
	idivl	tos
	movl	%edx, (%esi)
	movl	%eax, tos
end-primitive

[then]

\\	/			( n1 n2 -- n3 )				CORE
\\
\\		Divide  n1  by n2, giving the single-cell quotient n3.  An
\\		ambiguous  condition  exists  if n2 is zero.  If n1 and n2
\\		differ in sign, the implementation-defined result returned
\\		will be the same as either the phrase
\\			>R S>D R> FM/MOD SWAP DROP
\\		or the phrase
\\			>R S>D SM/MOD SWAP DROP.
\\
/less-code [if]
: /
  /mod nip
;
[else]
/ps=sp [if]

primitive /
	popl	%eax
	cltd
	idivl	tos
	movl	%eax, tos
end-primitive

[else]

primitive /
	movl	(%esi), %eax
	cltd
	idivl	tos
	movl	%eax, tos
	inc-csp
end-primitive

[then]
[then]

\\	mod			( n1 n2 -- n3 )				CORE
\\
\\		Divide  n1  by n2, giving the single-cell remainder n3.  An
\\		ambiguous  condition  exists  if  n2 is zero.  If n1 and n2
\\		differ  in sign, the implementation-defined result returned
\\		will be the same as either the phrase
\\			>R S>D R> FM/MOD DROP
\\		or the phrase
\\			>R S>D SM/MOD DROP.
\\
/less-code [if]
: mod
  /mod drop
;
[else]
/ps=sp [if]

primitive mod
	popl	%eax
	cltd
	idivl	tos
	movl	%edx, tos
end-primitive

[else]

primitive mod
	movl	(%esi), %eax
	cltd
	idivl	tos
	movl	%edx, tos
	inc-csp
end-primitive

[then]
[then]

\\	min			( n1 n2 -- n3 )				CORE
\\
\\		n3 is the lesser of n1 and n2.
\\
/less-code [if]
: min
  2dup >
  if
    swap
  then
  drop
;
[else]
/ps=sp [if]

primitive min
	popl	%eax
	cmpl	%eax, tos
	jle	2 <cr>
	movl	%eax, tos
end-primitive

[else]

primitive min
	movl	(%esi), %eax
	inc-csp
	cmpl	%eax, tos
	jle	2 <cr>
	movl	%eax, tos
end-primitive

[then]
[then]

\\	max			( n1 n2 -- n3 )				CORE
\\
\\		n3 is the greater of n1 and n2.
\\
/less-code [if]
: max
  2dup <
  if
    swap
  then
  drop
;
[else]
/ps=sp [if]

primitive max
	popl	%eax
	cmpl	%eax, tos
	jge	2 <cr>
	movl	%eax, tos
end-primitive

[else]

primitive max
	movl	(%esi), %eax
	inc-csp
	cmpl	%eax, tos
	jge	2 <cr>
	movl	%eax, tos
end-primitive

[then]
[then]

\\	abs			( n1 -- +n2 )				CORE
\\
\\		+n2 is the absolute value of n1.
\\
primitive abs
	orl	tos, tos
	jns	2 <cr>
	negl	tos
end-primitive

\\	negate			( n1 -- n2 )				CORE
\\
\\		n2 is the negation of n1. ( n2 = -n1 )
\\
primitive negate
	negl	tos
end-primitive

\\	1+			( n1|u1 -- n2|u2 )			CORE
\\
\\		Add 1 to n1|u1 giving the sum n2|u2.
\\
primitive 1+
	incl	tos
end-primitive

\\	1-			( n1|u1 -- n2|u2 )			CORE
\\
\\		Subtract 1 from n1|u1 giving the difference n2|u2.
\\
primitive 1-
	decl	tos
end-primitive

\\	2*			( n1 -- n2 )				CORE
\\
\\		Multiply  n1 by 2 giving n2.  Use of 2* as a shift operator
\\		is   an   environmental-dependency.   For  example,  2*  is
\\		equivalent  to  a  logical left shift on a two's-complement
\\		machine,  and  a  circular left shift on a one's-complement
\\		machine.
\\
/less-code [if]
: 2*
  dup +
;
[else]
primitive 2*
	addl	tos, tos
end-primitive
[then]

\\	2/			( n1 -- n2 )				CORE
\\
\\		n2  is  the  result  of  dividing n1 by two. Use of 2/ as a
\\		shift operator is an environmental-dependency. For example,
\\		2/  is  equivalent  to  a  logical  right shift on a two's-
\\		omplement  machine,  and a circular right shift on a one's-
\\		omplement machine.
\\
primitive 2/
	sarl	$ 1 , tos
end-primitive

\ -----------------------------------------------------------------------------
\	Logical operations
\ -----------------------------------------------------------------------------

\\	invert			( x1 -- x2 )				CORE
\\
\\		Invert all bits of x1, giving x2.
\\
/less-code [if]
: invert
  true xor
;
[else]
primitive invert
	notl	tos
end-primitive
[then]

\\	within			( n1|u1 n2|u2 n3|u3 -- flag )		CORE EXT
\\
\\		flag  is true if n1|u1 is less than n3|u3 and not less than
\\		n2|u2.  All  comparisons are performed in a circular number
\\		space.  An  ambiguous condition exists if n1|u1, n2|u2, and
\\		n3|u3 are not all the same type.
\\		Equivalent to:  OVER - >R - R> U<
\\
/less-code [if]
: within
  over ( n1 n2 n3 n2 ) - ( n1 n2 n3-n2 ) >r ( n1 n2 ) - ( n1-n2 ) r> u<
;
[else]
/ps=sp [if]

primitive within
	popl	%eax			\ x2
	popl	%edx			\ x1
	subl	%eax, tos		\ n3-n2 -
	subl	%eax, %edx		\ n1-n2 -
	cmpl	tos, %edx		\ n1-n2 n3-2 u<
	sbbl	tos, tos
end-primitive

[else]

code within
	movl	4 (%esi), %edx
	xorl	%eax, %eax
	cmpl	tos, %edx		\ n3 n1
	jge	0f
	cmpl	(%esi), %edx		\ n2 n1
	jl	0f
	decl	%eax
0:
	movl	%eax, tos
	leal	8 (%esi), %esi
end-code

[then]
[then]

\ -----------------------------------------------------------------------------
\	Mixed cell arithmetik
\ -----------------------------------------------------------------------------

\\	um*			( u1 u2 -- ud )				CORE
\\
\\		Multiply  u1 by u2, giving the unsigned double-cell product
\\		ud.  All values and arithmetic are unsigned.
\\
/ps=sp [if]

primitive um*
	movl	tos, %eax
	mull	(%esp)			\ EAX as tos better
	movl	%eax, (%esp)		\ DOUBLE!
	movl	%edx, tos
end-primitive

[else]

primitive um*
	movl	tos, %eax
	mull	(%esi)
	movl	%eax, (%esi)		\ DOUBLE!
	movl	%edx, tos
end-primitive

[then]

\\	m*			( n1 n2 -- d )				CORE
\\
\\		d is the signed product of n1 times n2.
\\
/ps=sp [if]

primitive m*
	movl	tos, %eax
	imull	(%esp)			\ EAX as tos better
	movl	%eax, (%esp)		\ DOUBLE!
	movl	%edx, tos
end-primitive

[else]

primitive m*
	movl	tos, %eax
	imull	(%esi)
	movl	%eax, (%esi)		\ DOUBLE!
	movl	%edx, tos
end-primitive

[then]

\\	um/mod			( ud u1 -- u2 u3 )			CORE
\\
\\		Divide  ud  by u1, giving the quotient u3 and the remainder
\\		u2.  All  values and arithmetic are unsigned.  An ambiguous
\\		condition  exists  if  u1  is  zero or if the quotient lies
\\		outside the range of a single-cell unsigned integer.
\\
/ps=sp [if]

primitive um/mod
	popl	%edx
	movl	(%esp), %eax		\ DOUBLE@
	divl	tos
	movl	%edx, (%esp)
	movl	%eax, tos
end-primitive

[else]

primitive um/mod
	movl	(%esi), %edx
	inc-csp
	movl	(%esi), %eax		\ DOUBLE@
	divl	tos
	movl	%edx, (%esi)
	movl	%eax, tos
end-primitive

[then]

\\	sm/mod			( d1 n1 -- n2 n3 )			CORE
\\
\\		Divide  d1 by n1, giving the symmetric quotient n3 and  the
\\		remainder  n2.  Input and output stack arguments are signed
\\		An  ambiguous  condition  exists if n1 is zero  or  if  the
\\		quotient  lies  outside the range of a  single-cell  signed
\\		integer.
\\
/ps=sp [if]

primitive sm/mod
	popl	%edx
	movl	(%esp), %eax		\ DOUBLE@
	idivl	tos
	movl	%edx, (%esp)
	movl	%eax, tos
end-primitive

[else]

primitive sm/mod
	movl	(%esi), %edx
	inc-csp
	movl	(%esi), %eax		\ DOUBLE@
	idivl	tos
	movl	%edx, (%esi)
	movl	%eax, tos
end-primitive

[then]

\\	s>d			( n -- d )				DOUBLE
\\
\\		d is the double equivalent of n.
\\
primitive s>d
	push-tos
	addl	$ $80000000 , tos
	sbbl	tos, tos		\ 0< -1, false 0
end-primitive

\ -----------------------------------------------------------------------------
\	Double cell arithmetik
\ -----------------------------------------------------------------------------

\\	du<			( ud1 ud2 -- flag )		DOUBLE EXT
\\
\\		flag is true if ud1 is less than ud2.
\\
/ps=sp [if]

primitive du<
	popl	%edx			\ LOW-2
	popl	%eax			\ HIGH-1
	subl	%edx, (%esp)		\ LOW-1
	sbbl	tos, %eax
	sbbl	tos, tos
	popl	%eax
end-primitive

[else]

primitive du<
	movl	(%esi), %edx		\ 8 Low1 4 High1 0 Low2 tos
	subl	%edx, 8 (%esi)
	sbbl	tos, 4 (%esi)
	sbbl	tos, tos
	leal	12 (%esi), %esi
end-primitive

[then]

\\	d+			( d1|ud1 d2|ud2 -- d3|ud3 )		DOUBLE
\\
\\		Add d2|ud2 to d1|ud1, giving the sum d3|ud3.
\\
/ps=sp [if]

primitive d+
	popl	%edx			\ LOW-2
	popl	%eax			\ HIGH-1
	addl	%edx, (%esp)		\ LOW-1
	adcl	%eax, tos
end-primitive

[else]

primitive d+
	movl	(%esi), %edx		\ Low1
	leal	8 (%esi), %esi
	addl	%edx, (%esi)
	adcl	-4 (%esi), tos
end-primitive

[then]

\\	d-			( d1|ud1 d2|ud2 -- d3|ud3 )		DOUBLE
\\
\\		Subtract d2|ud2 from d1|ud1, giving the difference d3|ud3.
\\
/ps=sp [if]

primitive d-
	popl	%edx			\ LOW-2
	popl	%eax			\ HIGH-1
	subl	%edx, (%esi)		\ LOW-1
	sbbl	tos, %eax
	movl	%eax, tos
end-primitive

[else]

primitive d-
	movl	(%esi), %edx		\ Low1
	leal	8 (%esi), %esi
	movl	-4 (%esi), %eax
	subl	%edx, (%esi)
	sbbl	tos, %eax
	movl	%eax, tos
end-primitive

[then]

\\	d2*			( d1 -- d2 )				DOUBLE
\\
\\		Multiply d1 by 2 giving d2. Use of d2* as a shift operation
\\		is an environmental dependancy. ( Can be used. )
\\
/less-code [if]
: d2*
  2dup d+
;
[else]
/ps=sp [if]

primitive d2*
	popl	%eax
	addl	%eax, %eax
	pushl	%eax
	adcl	tos, tos
end-primitive

[else]

primitive d2*
	shll	$ 1 , (%esi)
	rcll	$ 1 , tos
end-primitive

[then]
[then]

\\	d2/			( d1 -- d2 )				DOUBLE
\\
\\		d2 is the result of dividing d1 by two.
\\		( Can't be used as shift operator. )
\\
/ps=sp [if]

primitive d2/
	sarl	$ 1 , tos
	rcrl	$ 1 , (%esp)
end-primitive

[else]

primitive d2/
	sarl	$ 1 , tos
	rcrl	$ 1 , (%esi)
end-primitive

[then]

\\	dnegate			( d1 -- d2 )				DOUBLE
\\
\\		d2 is the negation of d1. ( d2 = -d1 )
\\
/ps=sp [if]

primitive dnegate
	negl	(%esp)
	adcl	$ 0 , tos
	negl	tos
end-primitive

[else]

primitive dnegate
	negl	(%esi)
	adcl	$ 0 , tos
	negl	tos
end-primitive

[then]
