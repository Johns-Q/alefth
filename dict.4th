\
\	ALE Forth Version 2.04
\
\		Copyright (c) 1991,1992 by Lutz Sammer.
\
\		Bug-reports via usenet: johns
\
\	File:	dict.4th
\
\		Forth kernel dictionary operators.
\

\ -----------------------------------------------------------------------------
\	Dictionary addresses
\ -----------------------------------------------------------------------------

\	Dictionary words
\
\		cell+		cells
\		char+		chars
\

\\	cell			( -- n )				ALE
\\
\\		n is the size of a cell.
\\
4	constant cell

\\	cell+			( a-addr1 -- a-addr2 )			CORE
\\
\\		Add  the  size  of  a  cell, specified in address units, to
\\		a-addr1, giving a-addr2.
\\
/less-code [if]
: cell+
  cell +
;
[else]
4	field cell+
[then]

\\	cells			( n1 -- n2 )				CORE
\\
\\		n2 is the size, in address units, of n1 cells.
\\
/less-code [if]
: cells
  2 <<
;
[else]
primitive cells
	shll	$ 2 , tos
end-primitive
[then]

\\	char+			( c-addr1 -- c-addr2 )			CORE
\\
\\		Add  the size of one character, specified in address units,
\\		to c-addr1, giving c-addr2.
\\
/less-code [if]
: char+
  1+
;
[else]
primitive char+
	incl	tos
end-primitive
[then]

\\	chars			( n1 -- n2 )				CORE
\\
\\		n2 is the size, in address units, of n1 characters.
\\
: chars
; immediate

\ ------------------------------------------------------------------------------
\	Dictionary addresses.
\ ------------------------------------------------------------------------------

\\	etext			( -- addr )				UNIX
\\
\\		addr is the address of the end of the readonly dictionary
\\		area.
\\
value etext

\\	dp0			( -- addr )				ALE
\\
\\		addr is the address of the start of the dictionary area.
\\
value dp0

\\	here			( -- addr )				CORE
\\
\\		addr  is  the  address  of  the  next  available data space
\\		location.
\\
value here

\\	#dp			( -- u )				ALE
\\
\\		u is the size of the dictionary area.
\\
value #dp

\\	#pnb			( -- n )				ALE
\\
\\		n  is the maximum number of characters in pictured  numeric
\\		buffer.
\\
96	constant #pnb

\\	pad			( -- c-addr )				CORE EXT
\\
\\		c-addr  is  the  address  of a transient region that can be
\\		used to hold data for intermediate processing.
\\		(It isn't used by any word of the forth kernel.)
\\
: pad
  here #pnb +
;

\\	allot			( n -- )				CORE
\\
\\		Reserve n address units (bytes) of data space.
\\
: allot
  here +
  \ FIXME: Under unix and msdos we can expand the dictionary here
  dup 256 + dp0 #dp +			\ Keep 256 bytes for working
  > abort" End of dictionary reached"
  to here
;

\\	aligned			( addr -- a-addr )			CORE
\\
\\		a-addr  is  the first aligned address greater than or equal
\\		to addr.
\\
: aligned
  ( dup 1 and + )
  ( dup negate 3 and + )
; immediate

\\	align			( -- )					CORE
\\
\\		If the address of the next available data space location is
\\		not aligned, reserve enough space to align it.
\\
false [if]
: align					\ Does nothing
; immediate
[else]					\ For 386 only necessary for speed
: align
  \ here 1 and allot
  here negate 3 and allot		\ Align too long boundary
;
[then]

\\	,			( x -- )				CORE
\\
\\		Reserve  one cell of data space and store x in the cell. An
\\		ambiguous  condition  exists  if  the  address  of the next
\\		available data space location is not aligned.
\\
: ,
  here cell allot !
;

\\	w,			( word -- )				CORE
\\
\\		Reserve  space  for one word (16bit) in the data space  and
\\		store word in the space.
\\		An  ambiguous  condition exists if the address of the nexts
\\		available data space location is not word-aligned.
\\
: w,
  here 2 allot w!
;

\\	c,			( char -- )				CORE
\\
\\		Reserve space for one character in the data space and store
\\		char in the space.
\\		An  ambiguous  condition exists if the address of the nexts
\\		available data space location is not character-aligned.
\\
: c,
  here 1 chars allot c!
;

\\	",			( c-addr u -- )				ALE
\\
\\		Reserve  space  for u+1 characters in the  data  space  and
\\		place  the  string c-addr and u as counted  string  in  the
\\		space.  u  is placed as first character at here followed by
\\		the string c-addr.
\\
: ",
  here over 1+ chars allot place ( align)
;

\\	,"			( ccc<"> -- )			I	ALE
\\
\\		Parse  characters ccc delimited by " (double quote).  Place
\\		the string ccc as counted string in the dictionary.
\\
: ,"
  string? ",
; immediate

\\	unused			( -- u )				CORE EXT
\\
\\		u  is the amount of space remaining in the region addressed
\\		by HERE, in address units.
\\
: unused
  dp0 #dp + here -
;

compiler definitions

\\	jmp,			( xt -- )			C	COMPILER
\\
\\		Append  the  code to jmp to the execution token xt  to  the
\		current definition.
\\
: jmp,
  $E8 c, here cell+ - ,			\ JMP	displacement
; compilation

\\	call,			( xt -- )			C	COMPILER
\\
\\		Append  the code to call the word of the execution token xt
\\		to the current definition.
\\
/ps=sp [if]

: call,
  dup here 8 + -
  -128 128 within
  if
    $06C7 w, here 6 + ,			\ movl	$ .+8 , (%esi)
    $EB c, here 1+ - c,			\ JMP	8bit-displacement
  else
    $06C7 w, here 9 + ,			\ movl	$ .+11 , (%esi)
    $E9 c, here cell+ - ,		\ JMP	32bit-displacement
  then
; compilation

[else]

: call,
  $E8 c, here cell+ - ,			\ CALL	displacement
; compilation

[then]

\\	nest,			( -- )				C	COMPILER
\\
\\		Append  the code to nest a colon definition to the  current
\\		definition.
\\
/ps=sp [if]

: nest,
  $EE83 w, 4 c,				\ subl	$4, %esi
\  $768D w, -4 c,			\ leal	-4(esi), %esi
; compilation

[else]

: nest,
; immediate compilation

[then]

\\	unnest,			( -- )				C	COMPILER
\\
\\		Append the code to unnest a colon definition to the current
\\		definition.
\\
/ps=sp [if]

: unnest,
  $C683 w, 4 c,				\ addl	$4, %esi
\  $768D w, 4 c,				\ leal	4(esi), %esi
; compilation

[else]

: unnest,
; immediate compilation

[then]

\\	ret,			( -- )				C	COMPILER
\\
\\		Append  the  code  to  return from a word  to  the  current
\\		definition.
\\
/ps=sp [if]

: ret,
  $26FF w,				\ jmp	*(%esi)
; compilation

[else]

: ret,
  $C3 c,				\ ret
; compilation

[then]

\\	push,			( -- )				C	COMPILER
\\
\\		Append the code to push top element of stack hold in %ebx
\\		to the current definition.
\\		POSTPONE dup
\\
/ps=sp [if]

: push,
  $53 c,				\ push	%ebx
; compilation

[else]

: push,
  $89FC768D ,				\ leal	-4(%esi), %esi
  $1E c,				\ movl	%ebx, (%esi)
; compilation

[then]

\\	pop,			( -- )				C	COMPILER
\\
\\		Append the code to pop top element of stack hold in %ebx
\\		to the current definition.
\\		POSTPONE drop
\\
/ps=sp [if]

: pop,
  $5B c,				\ popl	%ebx
; compilation

[else]

: pop,
  $768D1E8B ,				\ movl	(%esi), %ebx
  $04 c,				\ leal	4(%esi), %esi
; compilation

[then]

\\	?branch,		( n cc -- )			C	COMPILER
\\
\\		Append the code to branch ( Jcc n ) to the current definition.
\\
: ?branch,
  over here 2 + - -128 127 within	\ Range
  if
           $70 or c, here 1+ - c,	\ 8 Bit
  else
    $0F c, $80 or c, here cell+ - ,	\ 32 Bit
  then
; compilation

forth definitions
