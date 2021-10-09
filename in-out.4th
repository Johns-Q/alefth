\
\	ALE Forth Version 2.04
\
\		Copyright (c) 1991,1992 by Lutz Sammer.
\
\		Bug-reports via usenet: johns
\
\	File:	in-out.4th
\
\		Forth kernel input and output.
\

\ -----------------------------------------------------------------------------
\	Input/Output
\ -----------------------------------------------------------------------------

\\	bsp#			( -- char )				ALE
\\
\\		char is the character value for a backspace.
\\
8	constant bsp#

\\	nl#			( -- char )				ALE
\\
\\		char is the character value for a newline.
\\
10	constant nl#

\\	cr#			( -- char )				ALE
\\
\\		char is the character value for a carriage return.
\\
13	constant cr#

\\	ret#			( -- char )				ALE
\\
\\		char is the character value for the return key.
\\
exist target-msdos  [if]  cr#  [then]
exist target-unix   [if]  nl#  [then]
	constant ret#

\\	ff#			( -- char )				ALE
\\
\\		char is the character value for a form feed.
\\
12	constant ff#

\\	esc#			( -- char )				ALE
\\
\\		char is the character value for a escape.
\\
27	constant esc#

\\	#cols			( -- n )				ALE
\\
\\		Constant n is the number of columns on the current display.
\\
value #cols

\\	#rows			( -- n )				ALE
\\
\\		Constant n is the number of rows on the current display.
\\
value #rows

\\	out			( -- a-addr )				ALE
\\
\\		n  is the current column number of the most recent type  or
\\		emit. Incremented by EMIT and TYPE. Reset by CR.
\\
variable out

/buffered-output [if]

compiler definitions

\\	>line			( -- a-addr )				ALE
\\
\\		a-addr is the address of >LINE.  >LINE contains the  offset
\\		in characters from the start of the current output line.
\\
variable >line

\\	#line			( -- n )				ALE
\\
\\		n is the number of bytes available in the output line
\\		buffer.
\\
256	constant #line

\\	line-buffer		( -- a-addr )				ALE
\\
\\		a-addr is the address of the output line buffer.
\\
create line-buffer  #line 1+ allot

\\	flush-line		( -- )					ALE
\\
\\		Output the contents of the output line buffer.
\\
: flush-line
  line-buffer >line @ (type) >line off
;

forth definitions

[then]

\\	emit			( x -- )				CORE
\\
\\		If  x  is  a  displayable  character in the implementation-
\\		defined  character  set, display x.  The effect of EMIT for
\\		all other values of x is implementation- defined.
\\		When  passed a character whose character-defining bits have
\\		a  value between hex 20 and 7E inclusive, the corresponding
\\		Standard  character is displayed.  Because of the potential
\\		non-transportable  action  by  terminal  devices of control
\\		characters,   the   use   of   control   characters  is  an
\\		environmental  dependency.  Each  EMIT  deals with only one
\\		character   and   the   character  is  displayed  with  the
\\		attributes   specified  by  the   attribute  bits.  Use  of
\\		attribute bits is an environmental dependency.
\\
/buffered-output [if]
: emit					\ buffered
  >line @ #line >  if  flush-line  then	\ Buffer full
  line-buffer >line @ + c! >line 1+!
  out 1+!
;
[else]
: emit					\ unbuffered
  (emit) out 1+!
;
[then]

\\	type			( c-addr u -- )				CORE
\\
\\		If  u  is  greater  than zero, display the character string
\\		specified  by  c-addr  and u.  When passed a character in a
\\		character string whose character-defining bits have a value
\\		between hex 20 and 7E inclusive, the corresponding Standard
\\		character  is  displayed.  Because  of  the  potential non-
\\		ransportable   action   by   terminal  devices  of  control
\\		characters,   the   use   of   control   characters  is  an
\\		environmental dependency.  Each character is displayed with
\\		the  attributes  specified  by  the attribute bits.  Use of
\\		attribute bits is an environmental dependency.
\\
/buffered-output [if]
: type					\ Buffered
  dup out +!
  >line @ over + #line >
  if					\ Buffer overflows
    flush-line
  then
  dup #line >				\ Didn't fit into buffer
  if
    (type)
  else					\ Copy into buffer
    >r line-buffer >line @ + r@ cmove
    r> >line +!
  then
;
[else]
: type					\ Unbuffered
  dup out +! (type)
;
[then]

\\	cr			( -- )					CORE
\\
\\		Cause  subsequent  output to appear at the beginning of the
\\		next line.
\\
exist target-msdos [if]
/buffered-output [if]
: cr					\ Buffered
  out on flush-line cr# (emit) nl# (emit)
;
[else]
: cr					\ Unbuffered
  out on cr# (emit) nl# (emit)
;
[then]
[else]
/buffered-output [if]
: cr					\ Buffered
  out on flush-line nl# (emit)
;
[else]
: cr					\ Unbuffered
  out on nl# (emit)
;
[then]
[then]

\\	space			( -- )					CORE
\\
\\		Display one space.
\\
: space
  bl emit
;

\\	blanks			( -- c-addr )				ALE
\\
\\		c-addr is the address of 32 continues spaces.
\\
rom create blanks
bl c, bl c, bl c, bl c, bl c, bl c, bl c, bl c,
bl c, bl c, bl c, bl c, bl c, bl c, bl c, bl c,
bl c, bl c, bl c, bl c, bl c, bl c, bl c, bl c,
bl c, bl c, bl c, bl c, bl c, bl c, bl c, bl c,

\\	spaces			( n -- )				CORE
\\
\\		If n is greater than zero, display n spaces.
\\
: spaces
  dup 0<  if  drop exit  then
  blanks over 5 >> 0
  ?do
    dup 32 type
  loop
  swap 31 and type
;

\\	#tub			( -- n )				ALE
\\
\\		n is the size of the terminal user input buffer.
\\
80	constant #tub

\\	tub			( -- c-addr )				ALE
\\
\\		c-addr is the address of the terminal user input buffer.
\\
create tub  #tub chars allot

\\	tib			( -- c-addr )				CORE
\\
\\		c-addr is the address of the text input buffer.
\\
value tib

\\	>in			( -- a-addr )				CORE
\\
\\		a-addr  is  the address of >IN.  >IN contains the offset in
\\		characters  from  the  start of the current input stream to
\\		the next character to be parsed.
\\
variable >in

\\	#tib			( -- a-addr )				CORE
\\
\\		a-addr is the address of #TIB.  #TIB contains the number of
\\		characters in the text input buffer.
\\
variable #tib

\\	key			( -- char )				CORE
\\
\\		Receive one character char, a member of the implementation-
\\		defined   character  set.   Keyboard  events  that  do  not
\\		correspond  to  such characters are discarded until a valid
\\		character  is  received,  and those events are subsequently
\\		unavailable.
\\		All   standard  characters  can  be  received.   Characters
\\		received by KEY are not displayed.
\\		Any standard returned by KEY has the numeric value specified
\\		in  6.3  Standard  Character  Set.  The  ability  to receive
\\		control characters is an environmental dependency.
\\
: key
  [ /buffered-output ] [if]
    flush-line
  [then]
  (key)
;

\\	key?			( -- flag )			FACILITY EXT
\\
\\		If a character is available, return true. Otherwise, return
\\		false.  If  non-character  keyboard  events  are  available
\\		before  the  first valid character, they are discarded, and
\\		are subsequently unavailable.
\\		After  KEY?  returns  with  a  value  of  true,  subsequent
\\		executions  of  KEY?  prior to the execution of KEY or EKEY
\\		also  return true, without discarding keyboard events.  The
\\		next  execution  of  KEY  will return the character without
\\		indefinite delay.
\\
: key?
  (key?)
;

\\	accept			( c-addr +n1 -- +n2 )			CORE
\\
\\		Receive  a string  of at most +n1 characters. An  ambiguous
\\		condition  exits  if +n1 is  zero or  greater than  32,767.
\\		Display  graphic  characters  as  they  are received. Input
\\		terminates  when  "return"  is  received.  When "return" is
\\		received, nothing is appended to the string.
\\		+n2 is the length of the string stored at c-addr.
\\
: accept
  >r 0				\ Count
  begin
    dup r@ <
  while
    key dup bsp# =		\ Backspace
    if
      drop dup
      if
	1- bsp# emit space bsp# emit
      then
    else
      dup esc# =		\ Escape
      if
	drop
	begin
	  dup
	while
	  bsp# emit space bsp# emit 1-
	repeat
      else
	dup >r ret# =		\ Return
	if
	  ( c-addr offset key )
	  nip 2r> 2drop		\ Cleanup
	  exit
	then
	2dup + r> over c!	\ Store key
	1 type			\ Display key
	1+
      then
    then
  repeat
  nip r> drop
;

\\	query			( -- )					CORE EXT
\\
\\		Receive  input into the text input buffer whose address  is
\\		qiven  by  TIB replacing its previous contents if any,  and
\\		make the result the current input stream.
\\
: query
  tub dup to tib #tub accept #tib ! >in off
;

\ -----------------------------------------------------------------------------
\	Pictured number output
\ -----------------------------------------------------------------------------

\\	(u.)			( u1 -- c-addr2 u2 )			ALE
\\
\\		Convert  u  into a string.  c-addr2 is the address  of  the
\\		string and u2 is length of the string.
\\
: (u.)
  0 <# #s #>
;

\\	u.			( u -- )				CORE
\\
\\		Display u in free field format.
\\
: u.
  (u.) type space
;

\\	u.r			( u n -- )				CORE EXT
\\
\\		Display  u  right aligned in a field n characters wide.  If
\\		the  number  of characters required to display u is greater
\\		than  n, all digits are displayed with no leading spaces in
\\		a field as wide as necessary.
\\
: u.r
  >r (u.) r> over - spaces type
;

\\	(.)			( n -- c-addr u )			ALE
\\
\\		Convert  n  into  a string.  c-addr is the address  of  the
\\		string and u is length of the string.
\\
: (.)
  dup abs 0 <# #s rot sign #>
;

\\	.			( n -- )				CORE
\\
\\		Display n in free field format.
\\
: .
  (.) type space
;

\\	.r			( n1 n2 -- )				CORE EXT
\\
\\		Display n1 right aligned in a field n2 characters wide.  If
\\		the  number of characters required to display n1 is greater
\\		than n2,  all  digits  are displayed with no leading spaces
\\		in a field as wide as necessary.
\\
: .r
  >r (.) r> over - spaces type
;

\\	?			( a-addr -- )			TOOLKIT EXT
\\
\\		Display the value stored at a-addr.
\\
: ?
  @ .
;

