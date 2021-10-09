\
\	ALE Forth Version 2.04
\
\		Copyright (c) 1991,1992 by Lutz Sammer.
\
\		Bug-reports via usenet: johns
\
\	File:	numeric.4th
\
\		Forth kernel numeric operators.
\

\ -----------------------------------------------------------------------------
\	Numeric conversions
\ -----------------------------------------------------------------------------

\ -----------------------------------------------------------------------------
\	Number -> numeric string
\ -----------------------------------------------------------------------------

compiler definitions

\\	convert-tab		( -- a-addr )				ALE
\\
\\		a-addr is the address of CONVERT-TAB.  CONVERT-TAB contains
\\		the conversion table for #. ( see: BASIS 17 4.05550 )
\\
rom create convert-tab
char 0 c, char 1 c, char 2 c, char 3 c, char 4 c,
char 5 c, char 6 c, char 7 c, char 8 c, char 9 c,
char A c, char B c, char C c, char D c, char E c,
char F c, char G c, char H c, char I c, char J c,
char K c, char L c, char M c, char N c, char O c,
char P c, char Q c, char R c, char S c, char T c,
char U c, char V c, char W c, char X c, char Y c,
char Z c, char [ c, char \ c, char ] c, char ^ c, char _ c,
char @ c, char a c, char b c, char c c, char d c,
char e c, char f c, char g c, char h c, char i c,
char j c, char k c, char l c, char m c, char n c,
char o c, char p c, char q c, char r c, char s c,
char t c, char u c, char v c, char w c, char x c,
char y c, char z c, char { c, char | c, char } c, char ~ c,

forth definitions

\\	hld			( -- a-addr )				ALE
\\
\\		a-addr  is  the address of HLD.  HLD contains  the  address
\\		of the last character in the pictured numeric buffer.
\\
variable hld

\\	base			( -- a-addr )				CORE
\\
\\		a-addr  is  the address of BASE.  BASE contains the current
\\		number conversion radix {{2...72}}.
\\
variable base

\\	binary			( -- )					ALE
\\
\\		Set the numeric conversion radix to two (binary).
\\
: binary
  2 base !
;

\\	octal			( -- )					ALE
\\
\\		Set the numeric conversion radix to eight (octal).
\\
: octal
  8 base !
;

\\	decimal			( -- )					CORE
\\
\\		Set the numeric conversion radix to ten (decimal).
\\
: decimal
  10 base !
;

\\	hex			( -- )					CORE EXT
\\
\\		Set the numeric conversion radix to 16 (hexadecimal).
\\
: hex
  16 base !
;

\\	hold			( char -- )				CORE
\\
\\		Add  char  to  the beginning of the pictured numeric output
\\		string.  Typically used between <# and #>.
\\
: hold
  hld 1-! hld @ c!
;

\\	<#			( -- )					CORE
\\
\\		Initialize pictured numeric process.
\\
: <#
  pad hld !
;

\\	#			( ud1 -- ud2 )				CORE
\\
\\		Divide  ud1  by  the number in BASE giving the quotient ud2
\\		and  the  remainder n. (n is the least significant digit of
\\		ud1.)  Convert  n  to  external  form and add the resulting
\\		character  to  the beginning of the pictured numeric output
\\		string. Typically used between <# and #>.
\\
: #
  base @ >r 0 r@ um/mod r> swap >r um/mod r>
  rot convert-tab + @ hold
;

\\	#s			( ud1 -- ud2 )				CORE
\\
\\		Convert  one  digit  of  ud1  according  to the rule for #.
\\		Continue  conversion  until  the  quotient  is zero. ud2 is
\\		zero. Typically used between <# and #>.
\\
: #s
  begin
    # 2dup or 0=
  until
;

\\	sign			( n -- )				CORE
\\
\\		If  n is negative, add a minus sign to the beginning of the
\\		pictured numeric output string.
\\		Typically used between <# and #>.
\\
: sign
  0< if  [char] - hold  then
;

\\	#>			( xd -- c-addr u )			CORE
\\
\\		Drop xd.  Make the pictured numeric output string available
\\		as  a character string.  c-addr and u specify the resulting
\\		character string.
\\
: #>
  2drop hld @ pad over -
;

\ -----------------------------------------------------------------------------
\	Numeric string -> number
\ -----------------------------------------------------------------------------

compiler definitions

\\	conv-itab		( -- a-addr )				ALE
\\
\\		a-addr is the address of CONV-ITAB.  CONV-ITAB contains the
\\		conversion table for >number.
\\		( see: BASIS 17 4.05550 )
\\
rom create conv-itab
99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c,	\ @ABCDEFG
99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, \ HIJKLMNO
99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c,	\ PQRSTUVW
99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c,	\ XYZ[\]^_
99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, \  !"#$%&'
99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c,	\ ()*+,-./
 0 c,  1 c,  2 c,  3 c,  4 c,  5 c,  6 c,  7 c, \ 01234567
 8 c,  9 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c,	\ 89:;<=>?
99 c, 10 c, 11 c, 12 c, 13 c, 14 c, 15 c, 16 c, \ @ABCDEFG
17 c, 18 c, 19 c, 20 c, 21 c, 22 c, 23 c, 24 c,	\ HIJKLMNO
25 c, 26 c, 27 c, 28 c, 29 c, 30 c, 31 c, 32 c, \ PQRSTUVW
33 c, 34 c, 35 c, 36 c, 37 c, 38 c, 39 c, 40 c,	\ XYZ[\]^_
41 c, 42 c, 43 c, 44 c, 45 c, 46 c, 47 c, 48 c, \ `abcdefg
49 c, 50 c, 51 c, 52 c, 53 c, 54 c, 55 c, 56 c,	\ hijklmno
57 c, 58 c, 59 c, 60 c, 61 c, 62 c, 63 c, 64 c, \ pqrstuvw
65 c, 66 c, 67 c, 68 c, 69 c, 70 c, 71 c, 99 c,	\ xyz{|}~.
99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c,	\ 80
99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c,
99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, \ 90
99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c,
99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, \ A0
99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c,
99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, \ B0
99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c,
99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, \ C0
99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c,
99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, \ D0
99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c,
99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, \ E0
99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c,
99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, \ F0
99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c,

\ /less-code [if]
rom create conv-hitab
\ [then]

99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c,	\ @ABCDEFG
99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, \ HIJKLMNO
99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c,	\ PQRSTUVW
99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c,	\ XYZ[\]^_
99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, \  !"#$%&'
99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c,	\ ()*+,-./
 0 c,  1 c,  2 c,  3 c,  4 c,  5 c,  6 c,  7 c, \ 01234567
 8 c,  9 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c,	\ 89:;<=>?
99 c, 10 c, 11 c, 12 c, 13 c, 14 c, 15 c, 99 c, \ @ABCDEFG
99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c,	\ HIJKLMNO
99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, \ PQRSTUVW
99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c,	\ XYZ[\]^_
99 c, 10 c, 11 c, 12 c, 13 c, 14 c, 15 c, 99 c, \ `abcdefg
99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c,	\ hijklmno
99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, \ pqrstuvw
99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c,	\ xyz{|}~.
99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c,	\ 80
99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c,
99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, \ 90
99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c,
99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, \ A0
99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c,
99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, \ B0
99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c,
99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, \ C0
99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c,
99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, \ D0
99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c,
99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, \ E0
99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c,
99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, \ F0
99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c, 99 c,

forth definitions

\\	>number			( ud1 c-addr1 u1 -- ud2 c-addr2 u2 )	CORE
\\
\\		ud2  is  the result of converting the characters within the
\\		character  string specified by c-addr1 u into digits, using
\\		the  number  in  BASE,  and  adding  each  into  ud1  after
\\		multiplying ud1 by the number in BASE. Conversion continues
\\		until a character that is not convertible is encountered or
\\		the  string is entirely converted.  c-addr2 is the location
\\		of  the  first unconverted character or the first character
\\		past  the  end  of  the  string  if the string was entirely
\\		converted.  u2  is  the number of unconverted characters in
\\		the string. An ambiguous condition exists if ud2 overflows.
\\
/less-code 0= [if]
: >number
  begin					\ Convert each character
    2dup 2>r
  while
    c@ base @ 16 -
    if
      conv-itab
    else
      conv-hitab			\ I WANT LOWERCASE HEX
    then
    + c@
    dup base @ <
    0=if				\ not below base
      drop 2r> exit
    then
    swap
    \ hi-l * base -> hi-l
    base @ um* drop rot base @ um* d+	\ ud1 * base + digit
    2r>
    1 /string
  repeat
  drop 2r>
;
[else]

/ps=sp [if]

code >number				\ I think should be in assembler
					\ for more speed (3*).
	leal	-12 (%esi), %esi
	movl	%ebp, (%esi)
	movl	%edi, 4 (%esi)
	movl	%ecx, 8 (%esi)
	movl	%esi, %ecx
					\ %ebx		holds u1
	popl	%edi			\ %edi		holds c-addr1
	popl	%edx
	popl	%esi			\ %esi/%edx	holds ud1
	pushl	%ecx

	movl	Dbase , %ebp		\ %ebp		holds base
	cmpl	$ 16 , %ebp
	je	1f			\ Base 16

	xorl	%ecx, %ecx
0:	movb	(%edi), %cl		\ Get next char
	movb	Dconv2Ditab (%ecx), %cl	\ Convert it
	cmpl	%ebp, %ecx
	jge	2f			\ >= base

	movl	%edx, %eax
	mull	%ebp			\ High-long*base
	pushl	%edx

	movl	%esi, %eax
	mull	%ebp			\ Low-long*base
	addl	%ecx, %eax		\ + DIGIT
	movl	%eax, %esi
	popl	%eax
	adcl	%eax, %edx		\ + high

	incl	%edi
	decl	tos			\ Advance input
	jnz	0b
	jmp	2f

1:	movb	$ 4 , %cl		\ FIXME: Next supports all bases^2

	xorl	%eax, %eax
1:	movb	(%edi), %al		\ Get next char	BASE 16
	movb	Dconv2Dhitab (%eax), %al	\ Convert it
	cmpl	%ebp, %eax
	jge	2f			\ >= base

	shld	%cl, %esi, %edx		\ High * 2 4 8 16
	shll	%cl, %esi		\ Low  * 2 4 8 16
	addl	%eax, %esi

	incl	%edi
	decl	tos			\ Advance input
	jnz	1b

2:	popl	%esi
	pushl	%edi			\ c-addr1
	pushl	%edx
	pushl	%eax			\ %eax,%edx	ud1

	movl	8 (%esi), %ecx
	movl	4 (%esi), %edi
	movl	(%esi), %ebp
	leal	12 (%esi), %esi
end-code

[else]

code >number				\ I think should be in assembler
					\ for more speed (3*).
	pushl	%ebp
	pushl	%edi
	pushl	%ecx
	pushl	%esi
					\ %ebx		holds u1
	movl	(%esi), %edi		\ %edi		holds c-addr1
	movl	4 (%esi), %edx
	movl	8 (%esi), %esi		\ %esi/%edx	holds ud1

	movl	Dbase , %ebp		\ %ebp		holds base
	cmpl	$ 16 , %ebp
	je	1f			\ Base 16

	xorl	%ecx, %ecx
0:	movb	(%edi), %cl		\ Get next char
	movb	Dconv2Ditab (%ecx), %cl	\ Convert it
	cmpl	%ebp, %ecx
	jge	2f			\ >= base

	movl	%edx, %eax
	mull	%ebp			\ High-long*base
	pushl	%edx

	movl	%esi, %eax
	mull	%ebp			\ Low-long*base
	addl	%ecx, %eax		\ + DIGIT
	movl	%eax, %esi
	popl	%eax
	adcl	%eax, %edx		\ + high

	incl	%edi
	decl	tos			\ Advance input
	jnz	0b
	jmp	2f

1:	movb	$ 4 , %cl		\ FIXME: Next supports all bases^2

	xorl	%eax, %eax
1:	movb	(%edi), %al		\ Get next char	BASE 16
	movb	Dconv2Dhitab (%eax), %al	\ Convert it
	cmpl	%ebp, %eax
	jge	2f			\ >= base

	shld	%cl, %esi, %edx		\ High * 2 4 8 16
	shll	%cl, %esi		\ Low  * 2 4 8 16
	addl	%eax, %esi

	incl	%edi
	decl	tos			\ Advance input
	jnz	1b

2:	movl	%esi, %eax
	popl	%esi
	movl	%edi, (%esi)		\ %edi		holds c-addr1
	movl	%edx, 4 (%esi)
	movl	%eax, 8 (%esi)		\ %esi/%edx	holds ud1

	popl	%ecx
	popl	%edi
	popl	%ebp
end-code

[then]
[then]
