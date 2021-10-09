\
\	ALE Forth Version 2.04
\
\		Copyright (c) 1991,1992 by Lutz Sammer.
\
\		Bug-reports via usenet: johns
\
\	File:	double.4th
\
\		Forth kernel double-number word set.
\

compiler definitions

\\	(2constant)		( dfa x -- x x1 x2 )		C	COMPILER
\\
\\		Place the contents of the data field on the stack.
\\		Only used for the execution of entries created by 2constant.
\\
: (2constant)
  (create) 2@
; compilation

\\	<2constant>		( xt -- )			C	COMPILER
\\
\\		Inline   compiler  for 2CONSTANT,  appends  the   execution
\\		semantics of a 2CONSTANT to the current definition.
\\
: <2constant>
					\ push tos
					\ pushl	(address+4)
  >body 2@ 2 postpone literal,		\ mov (address), tos
; compilation

\\	(2variable)		( dfa x -- x dfa )		C	COMPILER
\\
\\		Place the contents of the data field on the stack.
\\		Only used for the execution of entries created by 2variable.
\\
: (2variable)
  (create)
; compilation

\\	<2variable>		( xt -- )			C	COMPILER
\\
\\		Inline   compiler  for 2VARIABLE,  appends  the   execution
\\		semantics of a 2VARIABLE to the current definition.
\\
: <2variable>
  <create>
; compilation

compiler also forth definitions

\\	2constant		( x1 x2 "name" -- )		D	DOUBLE
\\
\\		Parse   name   delimited   by  a  space,  ignoring  leading
\\		delimiters.  Create  a dictionary  entry  for name with the
\\		execution semantics defined below. name is referred to as a
\\		"two-constant."
\\
\\	    name Execution:	( -- x1 x2 )
\\		Place cell pair x1 x2 on the stack.
\\
false [if]
: 2constant				\ Traditional
  create , ,				\ Contents of data field
does>
  2@		( x1 x2 )
;
[else]
: 2constant
  header
  ['] <2constant> compiler,
  ['] (2constant) executer,
  , ,					\ Contents of the data field
;
[then]

\\	2variable		( "name" -- )			D	DOUBLE
\\
\\		Parse   name   delimited   by  a  space,  ignoring  leading
\\		delimiters.  Create  a  dictionary  entry for name with the
\\		execution semantics defined below.  Reserve two consecutive
\\		cells  of  data  space  at  an  aligned  address.  name  is
\\		referred to as a "two-variable."
\\
\\	    name Execution:	( -- a-addr )
\\		a-addr is the address of the first (lowest address) cell of
\\		two  consecutive cells in data space reserved by  2VARIABLE
\\		when  it defined name.  The application is responsible  for
\\		initializing the contents of the reserved cell.
\\
false [if]
: 2variable				\ Traditional
  create 0 , 0 ,			\ Init data field
;
[else]
: 2variable
  header
  ['] <2variable> compiler,
  ['] (2variable) executer,
  0 , 0 ,				\ Contents of the data field
;
[then]

[ifndef] 2literal			\ Not in the kernel

\\	2literal						C I	DOUBLE
\\
\\	    Compilation:	( x1 x2 -- )
\\		Compile cell pair x1 x2 as a literal.
\\
\\	    Execution:		( -- x1 x2 )
\\		Place cell pair x1 x2 on the stack.
\\
false [if]
: 2literal
  swap postpone literal postpone literal
; compilation immediate
[then]
: 2literal
  2 postpone literal,
;
[then]

\\	m+			( d1|ud1 n -- d2|ud2 )			DOUBLE
\\
\\		Add n to d1|ud1, giving the sum d2|ud2.
\\
: m+
  s>d d+
;

\\	d-			( d1|ud1 d2|ud2 -- d3|ud3 )		DOUBLE
\\
\\		Subtract d2|ud2 from d1|ud1, giving the difference d3|ud3.
\\
[ifndef] d-				\ Not in kernel
  : d-
    dnegate d+
  ;
[then]

\\	d<			( d1 d2 -- flag )			DOUBLE
\\
\\		Flag is true if d1 is less than d2.
\\
: d<
  2 pick over =
  if
    du<
  else
    nip rot drop <
  then
;

\\	d>			( d1 d2 -- flag )		DOUBLE ALE
\\
\\		Flag is true if d1 is greater than d2.
\\
: d>
  2swap d<
;

\\	d0<			( d -- flag )				DOUBLE
\\
\\		Flag is true if d is less then zero.
\\
: d0<
  nip 0<
;

\\	d0=			( d|ud -- flag )			DOUBLE
\\
\\		Flag is true if d|ud is equal to zero.
\\
: d0=
  or 0=
;

\\	d=			( xd1 xd2 -- flag )			DOUBLE
\\
\\		Flag is true if xd1 is bit-for-bit same as xd2.
\\
: d=
  d- d0=
;

\\	dabs			( d1 -- +d2 )				DOUBLE
\\
\\		+d2 is the absolute value of d1.
\\
: dabs
  dup 0<
  if
    dnegate
  then
;

\\	fm/mod			( d1 n1 -- n2 n3 )			CORE
\\
\\		Divide  d1  by n1, giving the floored quotient n3  and  the
\\		remainder  n2.  Input and output stack arguments are signed
\\		An  ambiguous  condition  exists if n1 is zero  or  if  the
\\		quotient  lies  outside the range of a  single-cell  signed
\\		integer.
\\
: fm/mod
  dup >r 2dup xor >r dup >r abs >r dabs r> um/mod
  swap r> 0<
  if
    negate
  then
  swap r> 0<
  if
    negate
    over
    if
      r@ rot - swap 1-
    then
  then
  r> drop
;

\\	m*/			( d1 n1 +n2 -- d2 )			DOUBLE
\\
\\		Multiply  d1 by n1 producing the  triple-cell  intermediate
\\		result t.  Divide t by +n2 giving the double-cell  quotient
\\		d2.  An  ambiguous condition exits if +n2 is zero,  or  the
\\		quotient  lies  outside of the range of a  double-precision
\\		signed integer.
\\
: m*/
  >r
  2dup xor >r abs >r dabs		\ t*
  r> 2>r r@ um* 0 2r> um* d+
  r> 0<
  if
    rot invert 0 1 0 d+ >r		\ tnegate
    rot invert 0 r> 0 d+ >r
    rot invert r> +
  then
  r>
  over >r >r dup 0<
  if
    rot invert 0 1 0 d+ >r		\ tnegate
    rot invert 0 r> 0 d+ >r
    rot invert r> +
  then
  r@ um/mod -rot r> um/mod -rot drop
  r> 0<
  if
    dnegate
  then
;

\\	dmin			( d1 d2 -- d3 )				DOUBLE
\\
\\		d3 is the lesser of d1 and d2.
\\
: dmin
  2over 2over d>
  if
    2swap
  then
  2drop
;

\\	dmax			( d1 d2 -- d3 )				DOUBLE
\\
\\		d3 is the greater of d1 and d2.
\\
: dmax
  2over 2over d<
  if
    2swap
  then
  2drop
;

\\	d@			( a-addr -- d )			DOUBLE EXT
\\
\\		d is the value stored at a-addr.
\\
[ifdef] 2@				\ 2@ in kernel
  \ FIXME: use ALIAS
  : d@
    \ FIXME: ?postpone 2@
    compiling
    if
      postpone 2@
    else
      2@
    then
  ; immediate
[else]
  : d@
    dup cell+ @ swap @
  ;
[then]

\\	d!			( d a-addr -- )			DOUBLE EXT
\\
\\		Store d at a-addr.
\\
[ifdef] 2!				\ 2! in kernel
  \ FIXME: use ALIAS
  : d!
    \ FIXME: ?postpone 2!
    compiling
    if
      postpone 2!
    else
      2!
    then
  ; immediate
[else]
  : d!
    swap over ! cell+ !
  ;
[then]

\\	d>s			( d -- n )				DOUBLE
\\
\\		n is the equivalent of d.  An ambiguos condition exits if d
\\		lies outside the range of a signed single-cell number.
\\
  \ FIXME: use ALIAS
: d>s
  \ FIXME: ?postpone drop
  compiling
  if
    postpone drop
  else
    drop
  then
;

\\	(ud.)			( ud -- c-addr u )		DOUBLE ALE
\\
\\		Convert  ud  into a string.  c-addr is the address  of  the
\\		string and u2 is length of the string.
\\
: (ud.)
  <# #s #>
;

\\	ud.			( ud -- )			DOUBLE ALE
\\
\\		Display ud in free field format.
\\
: ud.
  (ud.) type space
;

\\	ud.r			( ud n -- )			DOUBLE ALE
\\
\\		Display  ud right aligned in a field n characters wide.  If
\\		the  number of characters required to display ud is greater
\\		than  n, all digits are displayed with no leading spaces in
\\		a field as wide as necessary.
\\
: ud.r
  >r (ud.) r> over - spaces type
;

\\	(d.)			( d -- c-addr u )			ALE
\\
\\		Convert  d  into  a string.  c-addr is the address  of  the
\\		string and u is length of the string.
\\
: (d.)
  tuck dabs <# #s rot sign #>
;

\\	d.			( d -- )				DOUBLE
\\
\\		Display d in free field format.
\\
: d.
  (d.) type space
;

\\	d.r			( d n -- )				DOUBLE
\\
\\		Display  d right aligned in a field n characters  wide.  If
\\		the  number of characters required to display d is  greater
\\		than n, all digits are displayed with no leading spaces  in
\\		a field as wide as necessary.
\\
: d.r
  >r (d.) r> over - spaces type
;

only forth definitions
