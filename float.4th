\
\	ALE Forth Version 2.04
\
\		Copyright (c) 1991,1992 by Lutz Sammer.
\
\		Bug-reports via usenet: johns
\
\	File:	float.4th
\
\		Support for floating-points.
\

\ ------------------------------------------------------------------------------
\	The Floating-Point Word Set
\ ------------------------------------------------------------------------------

\\	float			( -- )				V	FLOAT
\\
\\		Append  the floating-point word list to the current  search
\\		set.
\\
\\	    Floating point constants:
\\		f0.0		f1.0		pi		log2(10)
\\		log2(e)		log(2)		ln(2)
\\	    Floating point stack manipulation:
\\		fdepth		fdrop		fdup		fswap
\\		fnip		fover		fpluck		frot
\\		f-rot		f2dup		>f		<f
\\	    Floating point memory access:
\\		sf!		sf@		f!		f@
\\		df!		df@		bcd!		bcd@
\\	    Floating point arithmetic:
\\		f+		f-		f*		f/
\\		fnegate		fabs
\\	    Floating point trigonometric:
\\		facos		fasin		fatan		fatan2
\\		fcos		fsin		fsincos		ftan
\\	    Floating point logarithm:
\\		flog2		fpow2		fln		fexp
\\		flog		falog		f**		fsqrt
\\	    Floating point comparison:
\\		f0<		f0>		f0=
\\		f<		f>		f=		f~
\\		fmin		fmax
\\	    Floating point rounding:
\\		floor		fround
\\	    Floating point conversion input output:
\\		d>f		f>d		places		>float
\\		(e.)		e.		(f.)		f.
\\	    Floating point compiler:
\\		fliteral	fconstant	fliteral	float+
\\		floats
\\	    Floating point debug:
\\		.fs		.fr
\\
\\	    Internals:
\\		Floating point numbers are kept on a own stack.  The  stack
\\		of the floationg point processor is used.
\\		The floating point stack width is 8 items.
\\		Floating point numbers are stored in memory in  64-bit IEEE
\\		double precision format.
\\
vocabulary float  float definitions

\ ------------------------------------------------------------------------------
\	Floating point constants
\ ------------------------------------------------------------------------------

\\	f0.0			( R: -- 0.0E0 )			FLOAT ALE
\	f0.0			(  -- 0.0E0 )			FLOAT ALE
\\
\\		Place 0.0E on the floating-point stack.
\\
primitive f0.0				\ FIXME: Better name wanted ( 0e ? )
	fldz
end-primitive

\\	f1.0			( R: -- 1.0E0 )			FLOAT ALE
\	f1.0			(  -- 1.0E0 )			FLOAT ALE
\\
\\		Place 1.0E on the floating-point stack.
\\
primitive f1.0				\ FIXME: Better name wanted
	fld1
end-primitive

\\	pi			( R: -- 3.14E0 )		FLOAT ALE
\	pi			(  -- 3.14E0 )			FLOAT ALE
\\
\\		Place PI on the floating-point stack.
\\
primitive pi
	fldpi
end-primitive

\\	log2(10)		( R: -- r )			FLOAT ALE
\	log2(10)		( -- r )			FLOAT ALE
\\
\\		Place  the logarithm to base 2 of 10 on the  floating-point
\\		stack.
\\
primitive log2(10)
	fldl2t
end-primitive

\\	log2(e)			( R: -- r )			FLOAT ALE
\	log2(e)			( -- r )			FLOAT ALE
\\
\\		Place  the logarithm to base 2 of e on  the  floating-point
\\		stack.
\\
primitive log2(e)
	fldl2e
end-primitive

\\	log(2)			( R: -- r )			FLOAT ALE
\	log(2)			( -- r )			FLOAT ALE
\\
\\		Place  the logarithm to base 10 of 2 on the  floating-point
\\		stack.
\\
primitive log(2)
	fldlg2
end-primitive

\\	ln(2)			( R: -- r )			FLOAT ALE
\	ln(2)			( -- r )			FLOAT ALE
\\
\\		Place  the logarithm to base e of 2 on  the  floating-point
\\		stack.
\\
primitive ln(2)
	fldln2
end-primitive

\ ------------------------------------------------------------------------------
\	Floating point stack operations
\ ------------------------------------------------------------------------------

\\	fdepth			( -- +n )				FLOAT
\\
\\		+n  is  the  number  of values  contained  on  the  default
\\		separate floating-point stack.
\\		If  floating-point numbers are kept on the data stack +n is
\\		the   current  number  of  possible  floating-point  values
\\		contained on the data stack.
\\
primitive fdepth
	push-tos
	fnstsw
	shrl	$ 11 , %eax		\ Shift depth down
	negl	%eax			\ Invert depth
	andb	$ 7 , %al
	xorl	tos, tos
	movb	%al, tos-l
end-primitive

\\	fdrop			( F: r -- )				FLOAT
\	fdrop			( r -- )				FLOAT
\\
\\		Remove r from the floating-point stack.
\\
primitive fdrop
	fstpl	%st(0)
end-primitive

\\	fdup			( F: r -- r r )				FLOAT
\	fdup			( r -- r r )				FLOAT
\\
\\		Duplicate r.
\\
primitive fdup
	fldl	%st(0)
end-primitive

\\	fswap			( F: r1 r2 -- r2 r1 )			FLOAT
\	fswap			( r1 r2 -- r2 r1 )			FLOAT
\\
\\		Exchange the top two floating-point stack items.
\\
primitive fswap
	fxch	%st(1)
end-primitive

\\	fnip			( F: r1 r2 -- r2 )		FLOAT ALE
\	fnip			( r1 r2 -- r2 )			FLOAT ALE
\\
\\		Drop the first item below the top of floating-point stack.
\\
/less-code [if]
: fnip
  fswap fdrop
;
[else]
primitive fnip
	fstpl	%st(1)
end-primitive
[then]

\\	fover			( F: r1 r2 -- r1 r2 r1 )		FLOAT
\	fover			( r1 r2 -- r1 r2 r1 )			FLOAT
\\
\\		Place a copy of r1 on top of the floating-point stack.
\\
primitive fover
	fldl	%st(1)
end-primitive

\\	fpluck			( F: r1 r2 r3 -- r1 r2 r3 r1 )	FLOAT ALE
\	fpluck			( r1 r2 r3 -- r1 r2 r3 r1 )	FLOAT ALE
\\
\\		Place  a  copy  of  r1 (third stack item)  on  top  of  the
\\		floating-point stack.
\\
primitive fpluck
	fldl	%st(2)
end-primitive

\\	frot			( F: r1 r2 r3 -- r2 r3 r1 )		FLOAT
\	frot			( r1 r2 r3 -- r2 r3 r1 )		FLOAT
\\
\\		Rotate the top three floating-point stack entries.
\\
primitive frot
	fxch	%st(1)
	fxch	%st(2)
end-primitive

\\	f-rot			( F: r1 r2 r3 -- r3 r1 r2 )	FLOAT ALE
\	f-rot			( r1 r2 r3 -- r3 r1 r2 )	FLOAT ALE
\\
\\		Rotate the top three floating-point stack entries down.
\\
/less-code [if]
: f-rot
  frot frot
;
[else]
primitive f-rot
	fxch	%st(2)
	fxch	%st(1)
end-primitive
[then]

\\	f2dup			( F: r1 r2 -- r1 r2 r1 r2 )		FLOAT
\	f2dup			( r1 r2 -- r1 r2 r1 r2 )		FLOAT
\\
\\		Duplicate the floating-point pair r1 r2.
\\
/less-code [if]
: f2dup
  fover fover
;
[else]
primitive f2dup
	fldl	%st(1)
	fldl	%st(1)
end-primitive
[then]

\\	>f			( r -- ) ( F: -- r )		FLOAT ALE
\	>f			( r -- r )			FLOAT ALE
\\
\\		Move r to the floating-point stack.  ( 64-bit IEEE )
\\
/ps=sp [if]
primitive >f
	push-tos			\ High
	fldl	(%esp)
	leal	8 (%esp), %esp
	pop-tos
end-primitive
[else]
primitive >f
	push-tos			\ High
	fldl	(%esi)
	leal	8 (%esi), %esi
	pop-tos
end-primitive
[then]

\\	<f			( -- r ) ( F: r -- )		FLOAT ALE
\	<f			( r -- r )			FLOAT ALE
\\
\\		Move r from the floating-point stack to the data stack.
\\		( 64-bit IEEE )
\\
/ps=sp [if]
primitive <f
	push-tos
	leal	-8 (%esp), %esp
	fstpl	(%esp)
	pop-tos				\ High
end-primitive
[else]
primitive <f
	push-tos
	leal	-8 (%esi), %esi
	fstpl	(%esi)
	pop-tos				\ High
end-primitive
[then]

\ ------------------------------------------------------------------------------
\	Floating point memory operations
\ ------------------------------------------------------------------------------

\\	sf!			( a-addr -- ) ( F: r -- )	FLOAT EXT
\	sf!			( r a-addr -- )			FLOAT EXT
\\
\\		Store  the floating-point number r as a 32-bit IEEE  single
\\		precision  number  at  a-addr.  If the significand  of  the
\\		internal  representation  of r has more precision than  the
\\		IEEE  single precision format, it will be rounded using the
\\		"round to nearest" rule.
\\		An  ambiguous condition exists if the exponent of r is  too
\\		large  to  be  accommodated by  the IEEE  single  precision
\\		format.
\\
primitive sf!
	fstps	(tos)
	pop-tos
end-primitive

\\	sf@			( a-addr -- ) ( F: -- r )	FLOAT EXT
\	sf@			( a-addr -- r )			FLOAT EXT
\\
\\		Fetch  the  32-bit IEEE single precision number  stored  at
\\		a-addr  to  the floating-point stack as r in  the  internal
\\		representation.  If  the IEEE single precision  significand
\\		has more precision than the internal representation it will
\\		be rounded to the internal representation using the  "round
\\		to nearest" rule.
\\		An  ambiguous condition exists if the exponent of the  IEEE
\\		single   precision  representation  is  too  large  to   be
\\		accommodated by the internal representation.
\\
primitive sf@
	flds	(tos)
	pop-tos
end-primitive

\\	f!			( a-addr -- ) ( F: r -- )		FLOAT
\	f!			( r a-addr -- )				FLOAT
\\
\\		Store r at a-addr.  ( 64-bit IEEE )
\\
primitive f!
	fstpl	(tos)
	pop-tos
end-primitive

\\	f@			( a-addr -- ) ( F: -- r )		FLOAT
\	f@			( a-addr -- r )				FLOAT
\\
\\		r is the value stored at a-addr.  ( 64-bit IEEE )
\\
primitive f@
	fldl	(tos)
	pop-tos
end-primitive

\\	df!			( a-addr -- ) ( F: r -- )	FLOAT EXT
\	df!			( r a-addr -- )			FLOAT EXT
\\
\\		Store  the floating-point number r as a 64-bit IEEE  double
\\		precision  number  at a-addr.  If the  significand  of  the
\\		internal  representation of r has more precision  than  the
\\		IEEE  double precision format, it will be rounded using the
\\		"round to nearest"  rule.
\\		An  ambiguous condition exists if the exponent of r is  too
\\		large to be accommodated in IEEE double precision format.
\\
primitive df!
	fstpl	(tos)
	pop-tos
end-primitive

\\	df@			( a-addr -- ) ( F: -- r )	FLOAT EXT
\	df@			( r a-addr -- )			FLOAT EXT
\\
\\		Fetch  the  64-bit IEEE double precision number  stored  at
\\		a-addr  to  the floating-point stack as r in  the  internal
\\		representation.  If  the IEEE double precision  significand
\\		has more precision than the internal representation it will
\\		be  rounded to the internal representation using the "round
\\		to nearest" rule.
\\		An  ambiguous condition exists if the exponent of the  IEEE
\\		double   precision  representation  is  too  large  to   be
\\		accommodated by the internal representation.
\\
primitive df@
	fldl	(tos)
	pop-tos
end-primitive

\\	bcd!			( c-addr -- ) ( F: r -- )	FLOAT ALE
\	bcd!			( c-addr -- r )			FLOAT ALE
\\
\\		Store r at c-addr as bcd number. (10 Bytes)
\\
primitive bcd!
	fbstp	(tos)
	pop-tos
end-primitive

\\	bcd@			( c-addr -- ) ( F: -- r )	FLOAT ALE
\	bcd@			( r c-addr -- )			FLOAT ALE
\\
\\		r is the value stored at c-addr as bcd number.  (10 Bytes)
\\
primitive bcd@
	fbld	(tos)
	pop-tos
end-primitive

\ ------------------------------------------------------------------------------
\	Floating point arithmetic
\ ------------------------------------------------------------------------------

\\	f+			( F: r1 r2 -- r3 )			FLOAT
\	f+			( r1 r2 -- r3 )				FLOAT
\\
\\		Add r1 to r2 giving the sum r3.
\\
primitive f+
	faddp	%st(0), %st(1)
end-primitive

\\	f-			( F: r1 r2 -- r3 )			FLOAT
\	f-			( r1 r2 -- r3 )				FLOAT
\\
\\		Subtract r2 from r1, giving r3.
\\
primitive f-
	fsubrp	%st(0), %st(1)
end-primitive

\\	f*			( F: r1 r2 -- r3 )			FLOAT
\	f*			( r1 r2 -- r3 )				FLOAT
\\
\\		Multiply r1 by r2 giving r3.
\\
primitive f*
	fmulp	%st(0), %st(1)
end-primitive

\\	f/			( F: r1 r2 -- r3 )			FLOAT
\	f/			( r1 r2 -- r3 )				FLOAT
\\
\\		Divide  r1  by  r2, giving the quotient  r3.  An  ambiguous
\\		condition  exists  if  r2 is zero,  or  the  quotient  lies
\\		outside of the range of a floating-point number.
\\
primitive f/
	fdivrp	%st(0), %st(1)
end-primitive

\\	fnegate			( F: r1 -- r2 )				FLOAT
\	fnegate			( r1 -- r2 )				FLOAT
\\
\\		r2 is the negation of r1.
\\
primitive fnegate
	fchs
end-primitive

\\	fabs			( F: r1 -- r2 )			FLOAT EXT
\	fabs			( r1 -- r2 )			FLOAT EXT
\\
\\		r2 is the absolute value of r1.
\\
primitive fabs
	fabs
end-primitive

\ -----------------------------------------------------------------------------
\	Floating point comparison operations
\ -----------------------------------------------------------------------------

\\	f0>			( -- flag ) ( F: r -- )		FLOAT ALE
\	f0>			( r -- flag )			FLOAT ALE
\\
\\		Flag is true if r is greater than zero.
\\
primitive f0>
	push-tos
	ftst
	fnstsw				\ Get status $4100
	negl	%eax
	sahf				\ To processor flags
	sbbl	tos, tos		\ -1 TRUE, 0 FALSE
	fstpl	%st(0)			\ FDROP
end-primitive

\\	f0<			( -- flag ) ( F: r -- )			FLOAT
\	f0<			( r -- flag )				FLOAT
\\
\\		Flag is true if r is less than zero.
\\
primitive f0<
	push-tos
	ftst
	fnstsw				\ Get status $4100
	sahf				\ To processor flags
	sbbl	tos, tos		\ -1 TRUE, 0 FALSE
	fstpl	%st(0)			\ FDROP
end-primitive

\\	f0=			( -- flag ) ( F: r -- )			FLOAT
\	f0=			( r -- flag )				FLOAT
\\
\\		Flag is true if r is equal to zero.
\\
primitive f0=
	push-tos
	ftst
	fnstsw				\ Get status $4100
	shrl	$ 6 , %eax		\ Zero flag -> Carry flag
	sahf				\ To processor flags
	sbbl	tos, tos		\ -1 TRUE, 0 FALSE
	fstpl	%st(0)			\ FDROP
end-primitive

\\	f<			( -- flag ) ( F: r1 r2 -- )		FLOAT
\	f<			( r1 r2 -- flag )			FLOAT
\\
\\		Flag is true if r1 is less than r2.
\\
primitive f<
	push-tos
	fcompp
	fnstsw				\ Get status $4100
	andw	$ $4100 , %ax
	subw	$ 1 , %ax		\ = 4100 Less
	sbbl	tos, tos		\ true -1, false 0
end-primitive

\\	f>			( -- flag ) ( F: r1 r2 -- )	FLOAT ALE
\	f>			( r1 r2 -- flag )		FLOAT ALE
\\
\\		Flag is true if r1 is greater than r2.
\\
primitive f>
	push-tos
	fcompp
	fnstsw				\ Get status $4100
	andw	$ $4100 , %ax
	subw	$ $0100 , %ax		\ Greater?
	sbbl	tos, tos		\ true -1, false 0
	movl	%eax, tos
end-primitive

\\	f=			( -- flag ) ( F: r1 r2 -- )	FLOAT ALE
\	f=			( r1 r2 -- flag )		FLOAT ALE
\\
\\		Flag is true if r1 is equal to r2. ( 1.0e and 1.0e must not
\\		be equal, use F~ insteed !!)
\\
primitive f=
	push-tos
	fcompp
	fnstsw				\ Get status $4100
	shrl	$ 6 , %eax		\ Zero flag -> Carry flag
	sahf				\ To processor flags
	sbbl	tos, tos		\ -1 TRUE, 0 FALSE
end-primitive

\\	f~			( -- flag ) ( F: r1 r2 r3 -- )	FLOAT EXT
\	f~			( r1 r2 r3 -- flag )		FLOAT EXT
\\
\\		If  r3  is positive, flag is true if the absolute value  of
\\		(r1 minus r2) is less than r3.  If r3 is zero, flag is true
\\		if  the implementation-dependent encoding of r1 and r2  are
\\		exactly  identical (positive and negative zero are  unequal
\\		if they have distinct encodings)
\\		If  r3 is negative, flag is true if the absolute  value  of
\\		(r1  minus r2) is less than the absolute value of r3  times
\\		the sum of the absolute values of r1 and r2.
\\
/less-code [if]
[else]
[then]
: f~
  fdup f0<
  if
    fabs f-rot
      ( F: |r3| r1 r2 )
    f2dup f- fabs f-rot
      ( F: |r3| [r1-r2| r1 r2 )
    f+ fabs frot f*
      ( F: |r1-r2| |r1+r2|*|r3| )
    f< exit
  then
  fdup f0=
  if
    fdrop f= exit
  then
  f-rot f- fabs fswap ( |r1-r2| r3 )
  f<
;

\\	fmax			( F: r1 r2 -- r3 )			FLOAT
\	fmax			( r1 r2 -- r3 )				FLOAT
\\
\\		r3 is the greater of r1 and r2.
\\
/less-code [if]
: fmax
  f2dup f<
  if
    fnip
  else
    fdrop
  then
;
[else]
code fmax
	fcoml	%st(1)
	fnstsw				\ Get status $4100
	sahf
	jbe	4 <cr>
	fstpl	%st(1)
	jmp	2 <cr>
	fstpl	%st(0)
end-code
[then]

\\	fmin			( F: r1 r2 -- r3 )			FLOAT
\	fmin			( r1 r2 -- r3 )				FLOAT
\\
\\		r3 is the lesser of r1 and r2.
\\
/less-code [if]
: fmin
  f2dup f<
  if
    fdrop
  else
    fnip
  then
;
[else]
code fmin
	fcoml	%st(1)
	fnstsw				\ Get status $4100
	sahf
	jbe	4 <cr>
	fstpl	%st(0)
	jmp	2 <cr>
	fstpl	%st(1)
end-code
[then]

\ ------------------------------------------------------------------------------
\	Floating point trigonometric
\ ------------------------------------------------------------------------------

\ ( FIXME:) 16 allot

\\	facos			( F: r1 -- r2 )			FLOAT EXT
\	facos			( r1 -- r2 )			FLOAT EXT
\\
\\		r2  is  the principle radian angle whose cosine is  r1.  An
\\		ambiguous condition exists if |r1| is greater than 1.
\\
primitive facos
	fldl	%st(0)			\ FDUP
	fld1
	fsubp	%st(0), %st(1)		\ r1-1
	fsqrt				\ sqrt(r1-1)

	fld1
	faddp	%st(0), %st(1)		\ r1+1
	fsqrt				\ sqrt(r1+1)

	fpatan

	fldl	%st(0)			\ FDUP
	faddp	%st(0), %st(1)		\ *2
end-primitive

\\	fasin			( F: r1 -- r2 )			FLOAT EXT
\	fasin			( r1 -- r2 )			FLOAT EXT
\\
\\		r2 is the principal radian angle whose sine is r1.
\\		An ambiguous condition exists if |r1| is greater than 1.
\\
primitive fasin
	fldl	%st(0)			\ FDUP
	fldl	%st(0)			\ FDUP
	fmulp	%st(0), %st(1)		\ r1*r1
	fld1
	fsubp	%st(0), %st(1)		\ r1*r1-1
	fsqrt				\ sqrt(r1*r1-1)

	fpatan
end-primitive

\\	fatan			( F: r1 -- r2 )			FLOAT EXT
\	fatan			( r1 -- r2 )			FLOAT EXT
\\
\\		r2 is the principal radian angle whose tangent is r1.
\\
primitive fatan
	fld1
	fpatan
end-primitive

\\	fatan2			( F: r1 r2 -- r3 )		FLOAT EXT
\	fatan2			( r1 r2 -- r3 )			FLOAT EXT
\\
\\		r3 is the radian angle whose tangent is r1/r2. An ambiguous
\\		condition exists if r1 and r2 are zero.
\\
primitive fatan2
	fpatan
end-primitive

\\	fcos			( F: r1 -- r2 )			FLOAT EXT
\	fcos			( r1 -- r2 )			FLOAT EXT
\\
\\		r2 is the cosine of the radian angle r1.
\\
primitive fcos
	fcos
end-primitive

\\	fsin			( F: r1 -- r2 )			FLOAT EXT
\	fsin			( r1 -- r2 )			FLOAT EXT
\\
\\		r2 is the sine of the radian angle r1.
\\
primitive fsin
	fsin
end-primitive

\\	fsincos			( F: r1 -- r2 r3 )		FLOAT EXT
\	fsincos			( r1 -- r2 r3 )			FLOAT EXT
\\
\\		r2 is the sine of the radian angle r1.  r3 is the cosine of
\\		the radian angle r1.
\\
primitive fsincos
	fsincos
end-primitive

\\	ftan			( F: r1 -- r2 )			FLOAT EXT
\	ftan			( r1 -- r2 )			FLOAT EXT
\\
\\		r2 is the tangent of the radian angle r1.
\\		An ambiguous condition exists if cos(r1) is zero.
\\
code ftan
	fptan
	fnstsw
	fstpl	%st(0)			\ FDROP
	sahf
	jnp	0f
	fstpl	%st(0)			\ FDROP
	fld1
	fchs				\ -1.0E0
0:
end-code

\ ------------------------------------------------------------------------------
\	Floating point exponent, logarithm, power
\ ------------------------------------------------------------------------------

\\	flog2			( F: r1 -- r2 )			FLOAT ALE
\	flog2			( r1 -- r2 )			FLOAT ALE
\\
\\		r2  is the base 2 logarithm of r1.  An ambiguous  condition
\\		exists if r1 is less than or equal to zero.
\\
primitive flog2
	fld1				\ 1.0E0
	fxch	%st(1)
	fyl2x				\ Y*(LOG2(X))
end-primitive

\\	fpow2			( F: r1 -- r2 )			FLOAT ALE
\	fpow2			( r1 -- r2 )			FLOAT ALE
\\
\\		Raise 2 to the power r1, giving r2.
\\
code fpow2
	fldl	%st(0)			\ FDUP

	pushl	%eax			\ {
	fnstcw	(%esp)
	movl	(%esp), %eax
	andw	$ $F3FF , %ax		\	Mask round
	orw	$ $0400 , %ax		\	Set round to negative infinity
	pushl	%eax
	fldcw	(%esp)
	popl	%eax

	frndint

	fldcw	(%esp)
	popl	%eax			\ } floor

	fxch	%st(1)			\ floor(r1) r1 --
	fsub	%st(1), %st(0)		\ floor(r1) fract(r1) --
	f2xm1				\ (2**x)-1
	fld1				\ 1.0E0
	faddp	%st(0), %st(1)
	fscale				\ *2
	fstpl	%st(1)			\ FNIP
end-code

\\	fln			( F: r1 -- r2 )			FLOAT EXT
\	fln			( r1 -- r2 )			FLOAT EXT
\\
\\		r2 is the natural logarithm of r1.  An ambiguous  condition
\\		exists if r1 is less than or equal to zero.
\\
primitive fln
	fldln2				\ log e ( 2 )
	fxch	%st(1)
	fyl2x				\ Y*(LOG2(X))
end-primitive

\\	fexp			( F: r1 -- r2 )			FLOAT EXT
\	fexp			( r1 -- r2 )			FLOAT EXT
\\
\\		Raise e to the power r1, giving r2.
\\
: fexp
  log2(e) f* fpow2
;

\\	flog			( F: r1 -- r2 )			FLOAT EXT
\	flog			( r1 -- r2 )			FLOAT EXT
\\
\\		r2 is the base 10 logarithm of r1.  An ambiguous  condition
\\		exists if r1 is less than or equal to zero.
\\
primitive flog
	fldlg2				\ log10( 2 )
	fxch	%st(1)
	fyl2x				\ Y*(LOG2(X))
end-primitive

\\	falog			( F: r1 -- r2 )			FLOAT EXT
\	falog			( r1 -- r2 )			FLOAT EXT
\\
\\		Raise 10 to the power r1, giving r2.
\\
: falog
  log2(10) f* fpow2
;

\\	f**			( F: r1 r2 -- r3 )		FLOAT EXT
\	f**			( r1 r2 -- r3 )			FLOAT EXT
\\
\\		Raise r1 to the power r2, giving the product r3.
\\
: f**
  fdup f0=
  if
    fnip exit				\ r1**0 -> 0.0E0
  then
  fover f0=
  if
    fdrop fdrop f1.0 exit		\ 0**r2 -> 1.0E0
  then
  swap
  inline
    fyl2x				\ Y*(LOG2(X))
  end-inline
  fpow2					\ 2** ...
;

\\	fsqrt			( F: r1 -- r2 )			FLOAT EXT
\	fsqrt			( r1 -- r2 )			FLOAT EXT
\\
\\		r2 is the square root of r1.  An ambiguous condition exists
\\		if r1 is less than zero.
\\
primitive fsqrt
	fsqrt
end-primitive

\ -----------------------------------------------------------------------------

\\	floor			( F: r1 -- r2 )				FLOAT
\	floor			( r1 -- r2 )				FLOAT
\\
\\		Round  r1 using the "round toward negative infinity"  rule,
\\		giving r2.
\\
code floor
	pushl	%eax
	fnstcw	(%esp)
	movl	(%esp), %eax
	andw	$ $F3FF , %ax		\ Mask round
	orw	$ $0400 , %ax		\ Set round to negative infinity
	pushl	%eax
	fldcw	(%esp)
	popl	%eax

	frndint

	fldcw	(%esp)
	popl	%eax
end-code

\\	fround			( F: r1 -- r2 )				FLOAT
\	fround			( r1 -- r2 )				FLOAT
\\
\\		Round r1 using the "round to even" rule, giving r2.
\\
code fround
	pushl	%eax
	fnstcw	(%esp)
	movl	(%esp), %eax
	andw	$ $F3FF , %ax		\ Mask round ( to even )
	pushl	%eax
	fldcw	(%esp)
	popl	%eax

	frndint

	fldcw	(%esp)
	popl	%eax
end-code

\ ------------------------------------------------------------------------------
\	Floating point conversion output
\ ------------------------------------------------------------------------------

\\	d>f			( d -- ) ( F: -- r )			FLOAT
\	d>f			( d -- r )				FLOAT
\\
\\		r is the floating-point equivalent of d.
\\		An  ambiguous  condition  exists if d cannot  be  precisely
\\		represented as a floating-point value.
\\
/ps=sp [if]
primitive d>f
	popl	%eax
	push-tos			\ High 32 bit
	pushl	%eax
	fildq	(%esp)
	leal	8 (%esp), %esp
	pop-tos
end-primitive
[else]
primitive d>f
	movl	4 (%esi), %eax
	movl	tos, 4 (%esi)		\ High 32 bit
	fildq	(%esi)
	leal	8 (%esi), %esi
	movl	%eax, tos
end-primitive
[then]

\\	f>d			( -- d ) ( F: r -- )			FLOAT
\	f>d			( r -- d )				FLOAT
\\
\\		d  is  the  double-cell signed integer  equivalent  of  the
\\		integer  portion  of  r.  The fractional portion  of  r  is
\\		discarded.  An  ambiguous condition exists if  the  integer
\\		portion   of  r  cannot  be  precisely  represented  as   a
\\		double-cell signed integer.
\\
/ps=sp [if]
primitive f>d
	push-tos
	leal	-8 (%esp), %esp
	frndint
	fistpq	(%esp)
	popl	%eax
	pop-tos				\ High 32 bit
	pushl	%eax
end-primitive
[else]
primitive f>d
	push-tos
	leal	-8 (%esi), %esi
	frndint
	fistpq	(%esi)
	movl	(%esi), %eax		\ Swap words
	leal	4 (%esi), %esi
	movl	(%esi), tos
	movl	%eax, (%esi)
end-primitive
[then]

compiler definitions

\\	#places			( -- n )			FLOAT ALE
\\
\\		n  is the number of decimal places (digits to the right  of
\\		the radix point) displayed by E. and F..
\\
value #places  4 to #places

\\	max-places		( -- n )			FLOAT ALE
\\
\\		n  is the maximal number of decimal places (digits  to  the
\\		right of the radix point) displayed by E. and F..
\\
17 constant max-places

\\	bcd-work		( -- a-addr )			FLOAT ALE
\\
\\		a-addr  is the address of BCD-WORK.  BCD-WORK contains  the
\\		bcd number for input and output conversions.
\\
create bcd-work  10 allot

\\	rnd>+inf		( -- d ) ( F: r -- )		FLOAT ALE
\\
\\		Round the floating point number r toward positive infinity.
\\		d  is  the  double-cell signed integer  equivalent  of  the
\\		integer  portion  of  r.  The fractional portion  of  r  is
\\		discarded.
\\
/ps=sp [if]
code rnd>+inf
	push-tos
	leal	-8 (%esp), %esp
	fnstcw	(%esp)
	popl	%eax
	movl	%eax, tos
	andw	$ $F3FF , %ax		\ Mask round
	orw	$ $0800 , %ax		\ Set round to positive infinity
	pushl	%eax
	fldcw	(%esp)

	fistpq	(%esp)

	pushl	tos
	fldcw	(%esp)
	popl	%eax

	popl	%eax
	pop-tos				\ High 32 bit
	pushl	%eax
end-code
[else]
code rnd>+inf
	push-tos
	leal	-8 (%esi), %esi
	fnstcw	(%esi)
	movl	(%esi), %eax
	movl	%eax, %edx
	andw	$ $F3FF , %ax		\ Mask round
	orw	$ $0800 , %ax		\ Set round to positive infinity
	movl	%eax, (%esi)
	fldcw	(%esi)

	fistpq	(%esi)

	movl	(%esi), %eax		\ Swap words
	leal	4 (%esi), %esi
	movl	(%esi), tos
	movl	%eax, (%esi)

	pushl	%edx
	fldcw	(%esp)
	popl	%eax
end-code
[then]

\\	(fxexp)			( -- n ) ( F: r -- r )		FLOAT ALE
\\	(fxexp)			( r -- r n )			FLOAT ALE
\\
\\		Consider  the floating point number as a product of a power
\\		of  ten and a mantissa m such 1 <= m < 10.  n is the  power
\\		of ten.
\\
: (fxexp)
  fdup fabs
  fdup flog rnd>+inf			\ Get exponent
  2dup d>f drop
  falog f/
  f1.0 -16 -1 d>f falog f- ( ) f<	\ Subs a small number for rounding
  if
    1-
  then
;

\\	bcd-digit		( char n -- )			FLOAT ALE
\\
\\		FIXME: No doc
\\
: bcd-digit		( d n -- )
  swap 15 and over 1 and
  if
    4 <<
  then
  swap 2/ bcd-work + dup >r c@ or r> c!
;

\\	(integer)	( c-addr u e d -- c-addr u e d )	FLOAT ALE
\\
\\		FIXME: No doc
\\
: (integer)
  2>r
  begin
    dup
  while					\ String empty
    over c@ dup [char] 0 [char] : within
    0=if
      drop 2r> exit
    then
    r@ dup 0<
    if
      2drop
    else
      bcd-digit
    then
    r> 1- >r
    1 /string
  repeat
  2r>
;

\\	(fraction)	( c-addr u e d -- c-addr u e d )	FLOAT ALE
\\
\\		FIXME: No doc
\\
: (fraction)
  2>r
  begin
    dup
  while					\ String empty
    over c@ dup [char] 0 [char] : within
    0=if
      drop 2r> exit
    then
    r@ dup 0<
    if
      2drop
    else
      bcd-digit
    then
    r> 1- r> 1+ >r >r
    1 /string
  repeat
  2r>
;

float definitions

\\	places			( u -- )				FLOAT
\\
\\		Set  the  number of decimal places (digits to the right  of
\\		the radix point) displayed by E. and F..
\\
: places
  max-places min 1 max to #places
;

\\	(e.)			( -- c-addr u ) ( F: r -- )		FLOAT
\	(e.)			( r -- c-addr u )			FLOAT
\\
\\		Convert  the top number on the floating-point stack to  its
\\		character string representation using scientific notation;
\\			<significand><exponent>
\\		where:
\\			<significand> := [-]<digit>.<digits0>
\\			<exponent> := e[-]<digits>
\\		The  exact  number of digits to the right  of  the  decimal
\\		point in the significand is determined by PLACES.
\\		An  ambiguous  condition exists if the system base  is  not
\\		DECIMAL or if the character string exceeds the maximum size
\\		of the pictured numeric output string buffer.
\\
\\	    See also:
\\		>FLOAT
\\
: (e.)
  <#
  fdup f0=
  if					\ Zero special handled
    fdrop
    [char] 0 hold [char] e hold
    #places 0
    ?do
      [char] 0 hold
    loop
    [char] . hold [char] 0 hold
  else
    max-places dup (fxexp) dup
    dup abs 0 #s 2drop sign		\ Add exponent
    [char] e hold
    ( max-places max-places exp )
    - s>d d>f falog f*			\ Scale float
    bcd-work bcd!			\ Store as BCD
    dup #places -
    do
      i 2/ bcd-work + c@		\ Convert BCD
      i 1 and  if  4 >>  then
      15 and [char] 0 + hold
    loop
    [char] . hold
    8 bcd-work + c@ 4 >> 15 and [char] 0 + hold
    9 bcd-work + c@  if  [char] - hold  then
  then
  0 dup #>
;

\\	(f.)			( -- c-addr u ) ( F: r -- )		FLOAT
\	(f.)			( r -- c-addr u )			FLOAT
\\
\\		Convert  the top number on the floating-point stack to  its
\\		character string representation using fixed point notation:
\\			[-] <digit>.<digits0>
\\		The  number of digits after the decimal point is determined
\\		by PLACES.
\\		An  ambiguous  condition exists if the system base  is  not
\\		DECIMAL or if the character string exceeds the maximum size
\\		of the pictured numeric output string buffer.
\\
\\	    See also:
\\		>FLOAT
\\
: (f.)
  <#
  fdup f0=
  if					\ Zero special handled
    fdrop
    #places 0
    ?do
      [char] 0 hold
    loop
    [char] . hold [char] 0 hold
  else
    (fxexp)
    max-places over - s>d d>f falog f*	\ Scale float
    bcd-work bcd!			\ Store as BCD

    ( exp -- )
    max-places over - swap
    0 max 1+ #places negate		\ - #places .. exp
    do
      i
      0=if
	[char] . hold
      then
      dup i +
      dup 0 max-places 1+ within
      if
	dup 2/ bcd-work + c@		\ Convert BCD
	swap 1 and  if  4 >>  then
	15 and [char] 0 + hold
      else
	drop [char] 0 hold
      then
    loop
    drop

    bcd-work 9 + c@
    if					\ Sign
      [char] - hold
    then
  then
  0 dup #>
;

\\	e.			( r -- )				FLOAT
\	e.			( -- ) ( F: r -- )			FLOAT
\\
\\		Convert  the  top number on the floating-point stack  to  a
\\		character  string using the rules of (E.) and  display  the
\\		resulting string with a trailing space.
\\		For example, the sequence
\\			4 PLACES 123.00E0 E.
\\		will display
\\			1.2300E2
\\		An  ambiguous  condition exists if the system base  is  not
\\		DECIMAL or if the character string exceeds the maximum size
\\		of the pictured numeric output string buffer.
\\
: e.
  (e.) type space
;

\\	f.			( r -- )				FLOAT
\	f.			( -- ) ( F: r -- )			FLOAT
\\
\\		Convert  the  top number on the floating-point stack  to  a
\\		character  string using the rules of (F.) and  display  the
\\		resulting string with a trailing space.
\\		For example, the sequence
\\			4 PLACES 1.23E5 F.
\\		will display
\\			123000.0000
\\		An  ambiguous  condition exists if the system basis is  not
\\		DECIMAL or if the character representation exceeds the size
\\		of the pictured numeric output string buffer.
\\
: f.
  (f.) type space
;

\\	>float			( c-addr u -- true ) ( F: -- r )	FLOAT
\	>float			( c-addr u -- r true )			FLOAT
\\			-or-	( c-addr u -- false )
\\
\\		An  attempt  is  made to convert the  string  specified  by
\\		c-addr and u to internal floating-point representation.  If
\\		the  string represents a valid floating-point number in the
\\		syntax  below  its value r and true are  returned.  If  the
\\		string  does  not represent a valid  floating-point  number
\\		only  false  is  returned.  A string of  blanks  should  be
\\		treated as a special case representing zero.
\\	    Syntax:
\\		Convertible string := <significand>[<exponent>]
\\		<significand>	:= [<sign>]{<digits>[.<digits0>]
\\					| [<digits0>].<digits> }
\\		<sign>		:= { + | - }
\\		<exponent>	:= <marker><digits0>
\\		<digits>	:= <digit><digits0>*
\\		<digits0>	:= <digit>*
\\		<digit>		:= { 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 }
\\		<marker>	:= {<e-form> | <sign-form>}
\\		<e-form>	:= <e-char>[<sign-form>]
\\		<sign-form>	:= { + | - }
\\		<e-char>	:= { D | d | E | e }
\\
: >float				\ FIXME: I think not the best way.
  bcd-work 10 erase

  over c@ [char] + -
  if
    over c@ [char] - -
    0=if				\ Set -
      128 bcd-work 9 + c!
      1 /string
    then
  else					\ Ignore +
    1 /string
  then
  ( c-addr u )
  [char] 0 skip				\ SKIP Leading zeros
  \ ." >FLOAT1: " 2dup type cr
  0 max-places
  ( c-addr u exp dig )
  (integer)				\ Integer part.
  \ ." >FLOAT2: " 2over type .s cr
  pluck
  if					\ Any chars
    3 pick c@ [char] . =
    if
      2swap 1 /string 2swap
      (fraction)			\ Fraction part.
    then
    \ ." >FLOAT3: " 2over type .s cr
  then
  pluck					\ Any chars
  if
    3 pick c@ dup [char] d =
    over [char] D = or
    over [char] e = or
    swap [char] E = or
    if
      ( c-addr u exp dig )
      2>r 1 /string			\ Remove dDeE

      over c@ [char] + -
      if
	over c@ [char] - -
	if
	  false
	else
	  1 /string true		\ Set -
	then
      else				\ Ignore +
	1 /string false
      then
      >r
      0 0 2swap >number 2swap drop
      r>  if  negate  then		\ Apply sign
      2r> rot -
    then
  then
  \ ." >FLOAT5: " 2over type .s cr

  2swap
  if					\ More chars
     3 discard false exit
  then
  drop
  bcd-work bcd@				\ Load the bcd-number

  + negate 1-
  \ ." >FLOAT6: " .s cr
  s>d d>f falog f*			\ Scale floating point number

  true					\ Yea a float
;

\\	fliteral						C I	FLOAT
\\
\\	    Compilation:	( F: r -- )
\	    Compilation:	( r -- )
\\		Compile floating-point number r as a literal.
\\
\\	    Execution:		( F: -- r )
\	    Execution:		( -- r )
\\		Place r on the floating-point stack.
\\
: fliteral
  fdup f0=
  if				\ 0.0e0 quicker
    fdrop postpone f0.0 exit
  then
  fdup f1.0 f- f0=
  if				\ 1.0e0 quicker
    fdrop postpone f1.0 exit
  then
  fdup pi f=
  if				\ pi quicker
    fdrop postpone pi exit
  then
  <f swap			\ Get the literal
  $68 c, ,			\ pushl	r-low
  $68 c, ,			\ pushl	r-high
  $8D2404DD ,			\ fldl	(%esp,1)
  $2464 w, $08 c,		\ leal	8 (%esp), %esp
; compilation immediate

compiler definitions

\\	(fconstant)		( -- ) ( F: -- r )		C	COMPILER
\	(fconstant)		( -- r )			C	COMPILER
\\
\\		Place  the contents of the data field on the floating point
\\		stack.  Only  used for the execution of entries created  by
\\		FCONSTANT.
\\
: (fconstant)
  (create) f@
; compilation

\\	<fconstant>		( xt -- )			C	COMPILER
\\
\\		Inline   compiler  for FCONSTANT,  appends  the   execution
\\		semantics of a FCONSTANT to the current definition.
\\
: <fconstant>
  >body $05DD w, ,			\ fldl	' xxx >body
; compilation

float definitions

\\	fconstant		( "name" -- ) ( F: r -- )	D	FLOAT
\	fconstant		( r "name" -- )			D	FLOAT
\\
\\		Parse   name   delimited  by  a  space,  ignoring   leading
\\		delimiters.  Create  a  dictionary entry for name with  the
\\		execution semantics defined below.
\\		name is referred to as an "f-constant."
\\
\\	    name Execution:	( -- ) ( F: -- r )
\	    name Execution:	( -- r )
\\		Place r on the floating-point stack.
\\
: fconstant
  header
  ['] <fconstant> compiler,
  ['] (fconstant) executer,
  here ( 1 floats ) 8 allot f!		\ Contents of the data field
;

compiler definitions

\\	(fvariable)		( -- dfa )			C	COMPILER
\\
\\		Place the address of the data field on the stack.
\\		Only used for the execution of entries created by FVARIABLE.
\\
: (fvariable)
  (create)
; compilation

\\	<fvariable>		( xt -- )			C	COMPILER
\\
\\		Inline   compiler  for FVARIABLE,  appends  the   execution
\\		semantics of a FVARIABLE to the current definition.
\\
: <fvariable>
					\ push tos
  >body postpone literal		\ mov $address, tos
; compilation

float definitions

\\	fvariable		( "name" -- )			D       FLOAT
\\
\\		Parse   name  delimited  by  a  space,   ignoring   leading
\\		delimiters.  Create  a dictionary entry for name  with  the
\\		execution  semantics  defined  below.   Reserve  1   FLOATS
\\		address units of data space at an aligned address.
\\		name is referred to as an "f-variable."
\\
\\	    name Execution: ( -- a-addr )
\\		a-addr  is  the  address of the  data  space  reserved  by
\\		FVARIABLE  when  it  created  name.   The  application  is
\\		responsible  for initializing the contents of the reserved
\\		space.
\\
: fvariable
  header
  ['] <fvariable> compiler,
  ['] (fvariable) executer,
  here ( 1 floats ) 8 allot
;

\\	float+			( a-addr1 -- a-addr2 )			FLOAT
\\
\\		Add  the  size  of a floating-point  number,  specified  in
\\		address units, to a-addr1, giving a-addr2.
\\
8 field float+

\\	floats			( n1 -- n2 )				FLOAT
\\
\\		n2  is  the size, in address units,  of  n1  floating-point
\\		numbers.
\\
primitive floats
	shll	$ 3 , tos
end-primitive

\\	float?	( c-addr u -- 0 ) or ( c-addr u -- -1 )	R	FLOAT ALE
\\
\\		Look  if  the string c-addr and u, can be converted into  a
\\		float.  If  the string is no float return zero.  If it  can
\\		be  converted into a number, return -1.
\\		Recognized float point literals:
\\			<significant><exponent>
\\		<significant>	:= [<sign>]<digits>[.<digits0>]
\\		<exponent>	:= E[<sign>]<digits0>
\\		<sign>		:= { + | - }
\\		<exponent>	:= <marker><digits0>
\\		<digits>	:= <digit><digits0>*
\\		<digits0>	:= <digit>*
\\		<digit>		:= { 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 }
\\
\\		float wordlist literals recognizer.
\\
: float?
  base @ 10 -
  if					\ Base not 10
    2drop false exit
  then
  dup  0=if  nip exit  then		\ Zero string
  2dup
  over c@ dup [char] + =
  swap [char] - = or
  if					\ +-
    \ ." +-" cr
    1 /string
  then
  begin
    dup  0=if  4 discard 0 exit  then	\ Zero string
    over c@ [char] 0 [char] : within	\ 0-9
  while
    \ ." digits" cr
    1 /string
  repeat
  over c@ [char] . =
  if
    1 /string
    \ ." ." cr
    begin
      dup  0=if  4 discard 0 exit  then	\ Zero string
      over c@ [char] 0 [char] : within	\ 0-9
    while
      \ ." digits" cr
      1 /string
    repeat
  then
  over c@ dup [char] e =
  swap [char] E = or
  if
    1 /string
    \ ." e" cr
    dup
    if
      over c@ dup [char] + =
      swap [char] - = or
      if				\ +-
	\ ." +-" cr
	1 /string
      then
      begin
	dup
	if
	  over c@ [char] 0 [char] : within	\ 0-9
	else
	  false
	then
      while
	\ ." digits" cr
	1 /string
      repeat
    then
    \ ." FLOAT:" 2dup type cr
    dup
    0=if
      2drop >float drop
      compiling
      if				\ If compiling create float literal
	postpone fliteral
      then
      true exit
    then
  then
  4 discard false
; recognizer

\ ------------------------------------------------------------------------------
\	Reset + init of floating point
\ ------------------------------------------------------------------------------

compiler definitions

\\	reset-fp		( -- )				FLOAT ALE
\\
\\		Reset the floating point processor, after errors.
\\
code reset-fp
	fninit
	fnclex
	pushl	%eax			\ {
	fnstcw	(%esp)
	popl	%eax
	movw	$ $1372 , %ax		\	round to even / interrupts
	pushl	%eax
	fldcw	(%esp)
	popl	%eax			\ }
end-code

\\	init-fp			( -- )				FLOAT ALE
\\
\\		Initialize the floating point processor.
\\
: init-fp
  reset-fp
;

float definitions

\ ------------------------------------------------------------------------------
\	Tests + experiments
\ ------------------------------------------------------------------------------

\\	.fs			( -- )				FLOAT ALE
\\
\\		Copy and display the values currently on the floating point
\\		stack.
\\		The format of the display is implementation-dependent.
\\		Example:	( F: 1.0e0 2.0e0 -- 1.0e0 2.0e0 )
\		Example:	(    1.0e0 2.0e0 -- 1.0e0 2.0e0 )
\\			1 PLACES .FS [2]1.0e0\2.0e0 Ok.
\\
: .fs
  fdepth dup
  if
    dup [char] [ emit (u.) type [char] ] emit
    dup >r 0
    do					\ Move floating point values from stack.
      <f
    loop
    r@ 0
    do					\ Display floating point stack
      i 2* 1+ pick
      i 2* 1+ pick >f
      [char] \ emit (e.) type
    loop
    r> 0
    do					\ Move floating point values on stack.
      >f
    loop
  else
    drop ." Floating-point stack empty"
  then
  space
;

compiler definitions

\\	float-regs		( -- a-addr )				COMPILER
\\
\\		a-addr is the address of FLOAT-REGS.  FLOAT-REGS is used by
\\		.FR.
\\
create fr 108 allot

\\	>fregs>			( a-addr -- )				COMPILER
\\
\\		Save the floating point processor register at a-addr.
\\		( The floating point register arn't changed. )
\\
primitive >fregs>
	fnsave	(tos)
	frstor	(tos)
	pop-tos
end-primitive

forth definitions

\\	.fr			( -- )				FLOAT ALE
\\
\\		Display the floating processor registers.
: .fr
  base @ hex
  fr >fregs>
  cr ." Control  Status   Tag      %EIP     %CS      Data-ptr Data-seg" cr
  28 0
  do
    i fr + @ 0 <# # # # #  # # # # #> type space
    cell
  +loop
  8 0
  do
    cr ." %st(" i (.) type ." ) " space
    fr 36 + i 10 * + w@ $8000 and
    if
      ." - ."
    else
      ." + ."
    then
    4 0
    do
      fr 34 + j 10 * + i 2* - w@ 0 <# # # # # #> type space
    loop
    ." e"
    7 fr 36 + i 10 * + w@ $7FFF and $3FFE - (.) tuck type
    - spaces
    fr 8 + w@ 8 fdepth - i + 7 and 2* >> 3 and dup
    if
      1- dup
      if
	1- dup
	if
	  ." 3 - Empty"
	else
	  ." 2 - Special"
	then
      else
	." 1 - Zero"
      then
    else
      ." 0 - Valid"
    then
    drop
  loop
  decimal cr ." Depth:" fdepth .
  fr @ 10 >> 3 and cr ." Round:" dup .
  dup
  if
    1- dup
    if
      1- dup
      if
	." to zero"
      else
	." to positive infinite"
      then
    else
      ." to negative infinite"
    then
  else
    ." to even"
  then
  drop
  base !
;

primitive fpcw>
	push-tos
	pushl	%eax
	fnstcw	(%esp)
	popl	tos
end-primitive

primitive >fpcw
	pushl	tos
	fldcw	(%esp)
	popl	%eax
	pop-tos
end-primitive

primitive fincstp
	fincstp
end-primitive

primitive fdecstp
	fdecstp
end-primitive

code rnd>-inf
	push-tos
	leal	-8 (%esp), %esp
	fnstcw	(%esp)
	popl	%eax
	movl	%eax, tos
	andw	$ $F3FF , %ax		\ Mask round
	orw	$ $0400 , %ax		\ Set round to negative infinity
	pushl	%eax
	fldcw	(%esp)

	fistpq	(%esp)

	pushl	tos
	fldcw	(%esp)
	popl	%eax

	popl	%eax
	pop-tos				\ High 32 bit
	pushl	%eax
end-code

primitive fxtract
	fxtract
end-primitive

\\	ft@			( a-addr -- ) ( F: -- r )	FLOAT ALE
\	ft@			( a-addr -- r )			FLOAT ALE
\\
\\		Fetch the 80-bit number r stored at a-addr.
\\
primitive ft@
	fldt	(tos)
	pop-tos
end-primitive

\\	ft!			( a-addr -- ) ( F: r -- )	FLOAT ALE
\	ft!			( r a-addr -- )			FLOAT ALE
\\
\\		Store  the  floating-point  number r as  80-bit  number  at
\\		a-addr.
\\
primitive ft!
	fstpt	(tos)
	pop-tos
end-primitive

: xt
  f1.0 10000 0 d>f f/ e.
;

: error
  f1.0 1000 s>d d>f f/
;

\\	387?
\\
\\		Detect the 387.  FIXME: Didn't work!!.
\\
code 387?
	push-tos
	xorl	tos, tos

	fninit
	movw	$ -1 , %ax
	fnstsw
	cmpw	$ 0 , %ax
	jne	1f			\ No 387

	pushl	%eax
	fnstcw	(%esp)
	popl	%eax
	andw	$ $103F , %ax
	cmpw	$ $003F , %ax
	jne	1f			\ No 387

	fld1
	fldz
	fdiv	%st(0), %st(1)		\ 0/1
	fldl	%st(0)			\ FDUP
	fchs
	fcompp
	fstsw
	sahf
	je	1f			\ No 387

	fninit				\ 387 Present
	pushl	%eax
	fnstcw	(%esp)
	popl	%eax
	andw	$ $FFFA , %ax		\ Enable interrupts generation
	pushl	%eax
	fldcw	(%esp)
	popl	%eax
	movl	$ -1 , tos		\ TRUE
1:
end-code

\\	>f>			( r -- ) ( F: -- r .. rn )	FLOAT ALE
\\
\\		Move r to the floating-point stack as lowest element.
\\		( Can be used to build a bigger floating point stack.)
\\
primitive >f>
	push-tos			\ High
	fldl	(%esp)
	leal	8 (%esp), %esp
	pop-tos
	fincstp
end-primitive

\\	<f>			( -- r ) ( F: r .. rn -- )	FLOAT ALE
\\
\\		Move  the lowest element r from the floating-point stack to
\\		the data stack.
\\		( Can be used to build a bigger floating point stack.)
\\
primitive <f>
	fdecstp
	push-tos
	leal	-8 (%esp), %esp
	fstpl	(%esp)
	pop-tos				\ High
end-primitive

\	Bigger floating point stack:
\
\	8 Elements on internal stack.
\
\	less then 3 elements load 3 elements
\	more then 6 elements save 3 elements
\
\	0 [*r1 ] [*r1 ] [ l0 ]
\	1 [*r2 ] [ l0 ] [ l1 ]
\	2 [ l0 ] [ l1 ] [ l2 ]
\	3 [ l1 ] [ l2 ] [    ]
\	4 [ l2 ] [    ] [    ]
\	5 [    ] [    ] [    ]
\	6 [    ] [    ] [    ]
\	7 [    ] [    ] [    ]
\
false [if]

create fstack  128 floats allot
variable fsp

: ?fload
  ." ?fload" cr
  fdepth 3 <
  if
    fdecstp fdecstp fdecstp
    fdecstp fdecstp fdecstp
  then
;

: ?fsave
;

: f+
  ?fload f+
;

: fdup
  ?fsave fdup
;

[then]

exist environment-wordlist [if]

environment-wordlist set-current

\\	floating		( -- flag )			ENVIRONMENT
\\
\\		Flag is true if floating-point word set is present.
\\
true constant floating

\\	floating-ext		( -- flag )			ENVIRONMENT
\\
\\		Flag  is  true  if  floating-point extension  word  set  is
\\		present.
\\
true constant floating-ext

\\	floating-stack		( -- n )			ENVIRONMENT
\\
\\		If n = 0 floating-point numbers are kept on the data stack,
\\		otherwise   n  is  the  maximum  depth  of  the    separate
\\		floating-point stack.
\\
8 constant floating-stack

\\	max-float		( F: -- r )			ENVIRONMENT
\	max-float		( -- r )			ENVIRONMENT
\\
\\		r is the largest usable floating-point number.
\\
: max-float
  $7FEFFFFF $FFFFFFFF >f
;

\\	min-float		( F: -- r )			ENVIRONMENT ALE
\	min-float		( -- r )			ENVIRONMENT ALE
\\
\\		r is the smallest usable floating-point number.
\\
: min-float
  $00100000 $00000000 >f
;

[then]

forth definitions
