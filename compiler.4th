\
\	ALE Forth Version 2.04
\
\		Copyright (c) 1991,1992 by Lutz Sammer.
\
\		Bug-reports via usenet: johns
\
\	File:	compiler.4th
\
\		Forth kernel compiler.
\

\ -----------------------------------------------------------------------------
\	Parser
\ -----------------------------------------------------------------------------

\\	source			( -- c-addr u )				ALE
\\
\\		Return the address c-addr and count u of the current input
\\		stream.
\\
: source
  [ /block ] [if]			\ No Block word set present
    blk @ dup
    if
      block b/buf exit
    then
    drop 
  [then]
  tib #tib @
;

\\	parse			( char "ccc<char>" -- c-addr u )	CORE EXT
\\
\\		Where char is a single char delimiter. Parse characters ccc
\\		delimited by char, ( including leading delimiters ). c-addr
\\		is  the address within the input stream and u is the length
\\		of  the  parsed  string.  If  the  current input stream was
\\		empty, the resulting string has a zero length.
\\
: parse
  >r source >in @ /string		\ Adjust input
  over swap				\ Save start of input
  r> dup bl -
  if
    scan				\ Find ending delimiter
  else					\ Delimiter is blank
    <scan				\ Find < ending delimiter
  then
  >r  over - dup  r> 0<> - >in +!	\ New input pointer
;

\\	word			( char "ccc<char>" -- c-addr )		CORE
\\
\\		Where char is a single char delimiter. Parse characters ccc
\\		delimited by char, ignoring leading delimiters. An amiguous
\\		condition  exists  if  the  length  of the parsed string is
\\		greater than the implementation-defined length of a counted
\\		string.  c-addr  is  the  address  of  a  transient  region
\\		containing the parsed word as a counted string.
\\		If  the  current  input  stream  was  empty or contained no
\\		characters  other  than the delimiter, the resulting string
\\		has a zero count.
\\		A space, not included in the count, follows the string.
\\
: word
  source over >r rot >r
  >in @ /string				\ Adjust input
  r@ dup bl -
  if
    skip				\ Skip leading delimiter
    over swap
    r> scan				\ Find ending delimiter
  else					\ Delimiter is blank
    <skip				\ Skip less leading delimiter
    over swap
    r> <scan				\ Find less ending delimiter
  then
  0<> negate over r> - + >in !		\ New input pointer
  over -

  here dup >r place			\ Place string
  bl r@ count + c! r>			\ Terminate string with blank
;

\ -----------------------------------------------------------------------------
\	Compiler - Interpreter
\ -----------------------------------------------------------------------------

\\	warning			( -- a-addr )				ALE
\\
\\		a-addr is the address of WARNING.  WARNING contains a flag.
\\		If WARNING is true issue a warning if you re-define a word.
\\
variable warning

\\	[			( -- )				C I	CORE
\\
\\		Enter interpretation state.  [ is an immediate word.
\\		Use of this word in interpret state is an error.
\\	        Typical use     : X ... [ 4321 ] LITERAL ... ;
\\
: [
  state off
; compilation immediate

\\	]			( -- )					CORE
\\
\\		Enter compilation state.
\\
: ]
  1 state !
;

\\	literal,		( x1 ... xn n -- )		C I	ALE
\\
\\	    Compilation:	( x1 ... xn n -- )
\\		Compile n cells x1 ... xn as literals.
\\
\\	    Execution:		( -- x1 ... xn )
\\		Place n cells x1 ... xn on the stack.
\\
/ps=sp [if]

: literal,
  dup
  if					\ n!=0
    push,
    dup 1- 0
    ?do					\ x1 .. xn-1
      dup i - pick dup -128 128 within
      if				\ Optimize values -128 .. 127
	$6A c, c,			\ pushl	x?
      else
	$68 c, ,			\ pushl	x?
      then
    loop
    over dup				\ xn
    if
      $BB c, ,				\ movl $x, %ebx
    else				\ Optimize zero
      drop $DB31 w,			\ xorl %ebx, %ebx
    then
  then
  discard
; compilation immediate

[else]

: literal,
  dup 0
  do
    push,
    dup i - pick dup
    if
      $BB c, ,				\ movl $x, %ebx
    else
      drop $DB31 w,			\ xorl %ebx, %ebx
    then
  loop
  discard
; compilation immediate

[then]

\\	literal							C I	CORE
\\
\\	    Compilation:	( x -- )
\\		Compile x as a literal.
\\
\\	    Execution:		( -- x )
\\		Place x on the stack.
\\
: literal
  push, dup				\ same as: 1 literal,
  if
    $BB c, ,				\ movl $x, %ebx
  else
    drop $DB31 w,			\ xorl %ebx, %ebx
  then
; compilation immediate

\\	compile,		( xt -- )			C	CORE EXT
\\
\\		Append   the   execution   semantics   of   the  definition
\\		represented by xt to the execution semantics of the current
\\		word definition.  An ambiguous condition exists if COMPILE,
\\		is executed while interpreting.
\\
: compile,
  dup >compiler perform
;

/cache [if]

compiler definitions

\\	#cache			( -- n )			SEARCH ALE
\\
\\		n is the size of the lookup cache.  Must be power of 2.
\\
256 constant #cache

\\	cache-table		( -- a-addr )			SEARCH ALE
\\
\\		a-addr  is  the  address of  the  CACHE-TABLE.  CACHE-TABLE
\\		contains the lookup cache.
\\		The first #CACHE elements contains the link-field of a word
\\		in execution state.
\\		The  second  #CACHE elements contains the link-field  of  a
\\		word in compilation state.
\\
create cache-table  #cache cells 2* allot
\ cache-table #cache cells 2* erase

\\	>cache			( n -- a-addr )			SEARCH ALE
\\
\\		a-addr is the n'th cache entry.
\\
: >cache
  cells cache-table +
;

\\	cache-hits		( -- a-addr )			SEARCH ALE
\\
variable cache-hits

\\	cache-miss		( -- a-addr )			SEARCH ALE
\\
variable cache-miss

\\	cache-coll		( -- a-addr )			SEARCH ALE
\\
variable cache-coll

\\	hash			( c-addr -- u )			SEARCH ALE
\\
\\		n is the hash value of the counted string c-addr.
\\
/less-code [if]

: hash
  \ dup c@ swap char+ c@ 2 << xor	\ First a very primitive
  dup c@
  over char+ c@ 4 << over xor >r	\ len^byte[0]
  + c@ 1 << r> xor			\ len^byte[0]^byte[len]

  [ #cache 1- ] literal and
;

[else]

code hash
	xorl	%eax, %eax
	movw	(tos), %dx
	movb	%dl, %al
	movzbl	(tos, %eax), tos	\ Last byte
	addl	tos, tos
	addl	tos, tos
	xorb 	%dh, tos-l		\ First byte
	addl	tos, tos
	addl	tos, tos
	xorb	%dl, tos-l		\ Length
	andl	$ 255 , tos
end-code

[then]

\\	update-cache		( xt -- )			SEARCH ALE
\\
\\		Place the execution token xt in the lookup-cache.
\\
: update-cache
  >link dup ->name hash state @  if  #cache +  then  >cache !
;

\\	?update-cache		( xt -- )			SEARCH ALE
\\
\\		Place the execution token xt only in the lookup-cache if it
\\		save.
\\
: ?update-cache
\  get-current context @ -		\ Current != Context flush
\  if
\    \ ." Clear cache" dup .name cr
\    >link ->name hash
\    cells 2* cache-table + 0 0 rot ( 0 0 addr ) 2!
\  else
\    \ ." Update cache" dup .name .s cr
\    update-cache
\  then
   >link ->name hash >cache
   dup off
   [ #cache cells ] literal + off
;

\\	lookup-cache	( c-addr -- c-addr ) ( R: -- )		SEARCH ALE
\\			-or- ( c-addr -- xt 1 ) ( R: sys -- )
\\			-or- ( c-addr -- xt -1 ) ( R: sys -- )
\\
\\		Lookup the counted string c-addr in the cache.  If the word
\\		is not found, return c-addr.  If the word is found,  return
\\		xt  and  return  control to the caller  of  the  definition
\\		containing  LOOKUP-CACHE.  If the word is  immediate,  also
\\		return 1, otherwise also return -1.
\\
: lookup-cache
  dup >r hash
  state @  if  #cache +  then
  >cache @				\ Look into table 
  dup
  if					\ Cache used.
    dup ->flag c@			\ Check if word is visibile
					\ hidden: execution && compilation
    dup COMPILATION and 0<>
      state @ 0= and			\ compilation: state = 1
    over EXECUTION and 0<>
      state @ 0<> and or		\ execution: state = 0
    swap PRIVATE and 0<> or		\ private: current = wordlist
    0=if
      dup dup ->noff c@ - count ( lfa c-addr' len' )
      r@ count compare
      0=if
	dup ->code
	swap ->flag c@ IMMEDIATE and
	if -1  else  1  then
	cache-hits 1+!
	2r> 2drop			\ c-addr + return
	exit
      then
      cache-coll 1+!
    then
  then
  drop r>
;

\\	empty-cache			( -- )			SEARCH ALE
\\
\\		Empty the lookup cache.  Must be done after search order is
\\		changed.
\\
: empty-cache
  cache-table [ #cache 2* cells ] literal erase
;

forth definitions

[then]

\\	find			( c-addr -- c-addr 0 )			CORE
\\				-or- ( c-addr -- xt 1 )
\\				-or- ( c-addr -- xt -1 )
\\
\\		Find  the Forth word named in the counted string at c-addr.
\\		If  the word is not found, return c-addr and 0. If the word
\\		is  found, return xt. If the word is immediate, also return
\\		1, otherwise also return -1.
\\
: find
  [ /cache ] [if]
    lookup-cache			\ Look if already in cache
  [then]
  0
  context #order cells bounds
  ?do					\ Lookup each wordlist in context
    over count i @ search-wordlist dup
    if					\ found
      [ /cache ] [if]
	over update-cache		\ Place word into cache
	cache-miss 1+!
      [then]
      2swap 2drop
      unloop exit
    then
    drop cell
  +loop
;

\\	?missing	( c-addr 0 | xt 1 | xt -1 -- c-addr | xt )	ALE
\\
\\		Emits a error message if the word isn't found.
\\
: ?missing
  0=if
    count type true abort"  ?"
  then
;

\\	defined			( "name" -- c-addr 0 )			ALE
\\				-or- ( "name" -- xt 1 )
\\				-or- ( "name" -- xt -1 )
\\
\\		Parse   name   delimited   by  a  space,  ignoring  leading
\\		delimiters.  If the word is not found, return c-addr and 0.
\\		If the word is  found, return xt. If the word is immediate,
\\		also return 1, otherwise also return -1.
\\		An ambiguous condition exists if name is a zero length.
\\		Does the same as the phrase:
\\			BL WORD FIND.
\\
: defined
  bl word dup c@ ?missing find
;

\\	'			( "name" -- xt )			CORE
\\
\\		Parse   name   delimited   by  a  space,  ignoring  leading
\\		delimiters.  Find  name  and return xt, the execution token
\\		for  name.  An  ambiguous  condition  exists if name is not
\\		found  or  if name is a standard word with the C attribute.
\\		When interpreting, ' name EXECUTE is equivalent to name.
\\
: '
  defined ?missing
;

\\	[compile]						C I	CORE EXT
\\
\\	    Compilation:	( "name" -- )
\\		Parse   name   delimited   by  a  space,  ignoring  leading
\\		delimiters.  If  name  has  default  compilation semantics,
\\		append  the  execution  semantics  of  name  to the current
\\		definition;  otherwise  append the compilation semantics of
\\		name.  An  ambiguous condition exists if name is not found.
\\
: [compile]
  ' compile,
; compilation immediate

\\	postpone						C I	CORE
\\
\\	    Compilation:	( "name" -- )
\\		Parse   name   delimited   by  a  space,  ignoring  leading
\\		delimiters. Append the compilation semantics of name to the
\\		current  definition.  An ambiguous condition exists if name
\\		is not found.
\\
\\	    Execution:		( -- )
\\		Perform the compilation semantics of name.
\\		If name is not immediate, its compilation semantics are to
\\		add its execution semantics to the current definition.
\\
\\		: endif postpone then ; immediate	( execute then )
\\		: +drop postpone drop ; immediate	( added drop called)
\\
: postpone
  defined dup ?missing 0<
  if					\ Immediate: add it now
    compile,
  else					\ Normal: add it at runtime
    postpone literal postpone compile,
  then
; compilation immediate

\\	[']							C I	CORE
\\
\\	    Compilation:	( "name" -- )
\\		Parse   name   delimited   by  a  space,  ignoring  leading
\\		delimiters.  Find name. Compile name's execution token as a
\\		literal.
\\		An  ambiguous  condition exists if name is not found in the
\\		search  order  or  if  name  is  a standard word with the C
\\		attribute.
\\
\\	    Execution:		( -- xt )
\\		Place name's execution token xt on the stack.
\\
: [']
  ' postpone literal
; compilation immediate

\\	char			( "ccc< >" -- char )			CORE
\\
\\		Parse characters ccc delimited by a space, ignoring leading
\\		delimiters.  Put  the  integer value of its first character
\\		onto the stack.
\\
: char
  bl word count 0= abort" ?" c@
;

\\	[char]							C I	CORE
\\
\\	    Compilation:	( "ccc< >" -- )
\\		Parse characters ccc delimited by a space, ignoring leading
\\		delimiters.  Compile  char,  the integer value of the first
\\		character of ccc, as a literal.
\\
\\	    Execution:		( -- char )
\\		Place char on the stack.
\\
: [char]
  char postpone literal
; compilation immediate

compiler definitions

\\	string?			( "ccc<">" -- c-addr u )		COMPILER
\\
\\		Parse  characters ccc delimited by " (double quote).  Issue
\\		an error message, if the resulting string has zero  length.
\\		c-addr and u is the parsed string.
\\		Used by the words C", S", ." and ABORT".
\\
: string?
  [char] " parse dup 0= abort" :Empty string!"
;

forth definitions

/fast-string 0= [if]			\ Old Version

compiler definitions

\\	(c")			( -- c-addr )			C	COMPILER
\\
\\		Compilation  word,  push the address of a counted string on
\\		the stack.  (C") is compiled by C".
\\
/less-code [if]
: (c")
  r> dup count + ( aligned) >r
;
[else]

/ps=sp [if]

code (c")
	push-tos
	movl	(%esi), tos		\ get return address
	xorl	%eax, %eax
	movb	(tos), %eax
	leal	1 (tos, %eax), %eax	\ Length + count
	jmp	%eax			\ Continue
end-code compilation

[else]

code (c")
	push-tos
	movl	(%esp), tos		\ get return address
	xorl	%eax, %eax
	movb	(tos), %eax
	leal	1 (tos, %eax), %eax	\ Length + count
	movl	%eax, (%esp)
end-code compilation

[then]
[then]

forth definitions

\\	c"							I	CORE EXT
\\
\\	    Compilation:	( "ccc<">" -- )
\\		Parse  characters  ccc  delimited  by  " (double-quote) and
\\		append  the  execution semantics given below to the current
\\		definition.
\\
\\	    Execution:		( -- c-addr )
\\		Return   c-addr,   a   counted  string  consisting  of  the
\\		characters  ccc.  A  standard  program  may  not  alter the
\\		returned string.
\\
\\	    Interpretation:	( "ccc<">" -- c-addr )
\\		Parse  characters  ccc delimited by " (double quote). Store
\\		the  resulting  string at a temporary location described by
\\		c-addr  as  counted  string.  The  maximum  length  of  the
\\		temporary  buffer is implementation-dependent but shall  be
\\		no  less than 80 characters.  Subsequent uses of S"  or  C"
\\		may  overwrite  the temporary buffer.  At  least  one  such
\\		buffer shall be provided.
\\
: c"
  string? compiling
  if
    postpone (c") ",
  else
    "place				\ temporary string
  then
; immediate

[else]					\ New version

\\	c"							I	CORE EXT
\\
\\	    Compilation:	( "ccc<">" -- )
\\		Parse  characters  ccc  delimited  by  " (double-quote) and
\\		append  the  execution semantics given below to the current
\\		definition.
\\
\\	    Execution:		( -- c-addr )
\\		Return   c-addr,   a   counted  string  consisting  of  the
\\		characters  ccc.  A  standard  program  may  not  alter the
\\		returned string.
\\
\\	    Interpretation:	( "ccc<">" -- c-addr )
\\		Parse  characters  ccc delimited by " (double quote). Store
\\		the  resulting  string at a temporary location described by
\\		c-addr  as  counted  string.  The  maximum  length  of  the
\\		temporary  buffer is implementation-dependent but shall  be
\\		no  less than 80 characters.  Subsequent uses of S"  or  C"
\\		may  overwrite  the temporary buffer.  At  least  one  such
\\		buffer shall be provided.
\\
: c"
  compiling
  if
    postpone ahead			\ Jump over string
    here swap				\ Address of string
    postpone ,"
    postpone then
    postpone literal			\ c-addr as literal
  else
    string? "place			\ temporary string
  then
; immediate

[then]

/fast-string 0= [if]			\ Old version

compiler definitions

\\	(s")			( -- c-addr u )			C	COMPILER
\\
\\		Compilation word,  push the address and length of string on
\\		the stack following the (S").  (S") is compiled by S".
\\
/less-code [if]
: (s")
  r> count 2dup + ( aligned) >r
;
[else]
/ps=sp [if]

code (s")
	push-tos

	movl	(%esi), %eax		\ Get return address
	xorl	tos, tos
	movb	(%eax), tos		\ Get length
	incl	%eax
	pushl	%eax			\ c-addr
	addl	tos, %eax
	jmp	%eax			\ Continue
end-code compilation

[else]

code (s")
	movl	(%esp), %eax		\ get return address

	leal	-8 (%esi), %esi		\ Push tos
	movl	tos, 4 (%esi)

	xorl	tos, tos
	movb	(%eax), tos		\ get length
	incl	%eax			\ to string
	movl	%eax, (%esi)

	addl	tos, %eax
	movl	%eax, (%esp)		\ New return address
end-code compilation

[then]
[then]

forth definitions

\\	s"							I	CORE
\\
\\	    Compilation:	( "ccc<">" -- )
\\		Parse  characters ccc delimited by " (double quote). Append
\\		the  execution   semantics   given  below  to  the  current
\\		definition.
\\
\\	    Execution:		( -- c-addr u )
\\		Return  c-addr  and  u that describe a string consisting of
\\		the characters ccc.
\\
\\	    Interpretation:	( "ccc<">" -- c-addr u )
\\		Parse  characters  ccc delimited by " (double quote). Store
\\		the  resulting  string at a temporary location described by
\\		c-addr and u. The maximum length of the temporary buffer is
\\		implementation-dependent  but  shall  be  no  less  than 80
\\		characters.  Subsequent uses of S" or C" may overwrite  the
\\		temporary  buffer.  At  least  one  such  buffer  shall  be
\\		provided.
\\
: s"
  string? compiling
  if
    postpone (s") ",
  else
    "place count			\ temporary string
  then
; immediate

[else]					\ New version

\\	s"							I	CORE
\\
\\	    Compilation:	( "ccc<">" -- )
\\		Parse  characters ccc delimited by " (double quote). Append
\\		the  execution   semantics   given  below  to  the  current
\\		definition.
\\
\\	    Execution:		( -- c-addr u )
\\		Return  c-addr  and  u that describe a string consisting of
\\		the characters ccc.
\\
\\	    Interpretation:	( "ccc<">" -- c-addr u )
\\		Parse  characters  ccc delimited by " (double quote). Store
\\		the  resulting  string at a temporary location described by
\\		c-addr and u. The maximum length of the temporary buffer is
\\		implementation-dependent  but  shall  be  no  less  than 80
\\		characters.  Subsequent uses of S" or C" may overwrite  the
\\		temporary  buffer.  At  least  one  such  buffer  shall  be
\\		provided.
\\
: s"
  compiling
  if
    postpone ahead
    here
    string? >r over r@ dup allot cmove
    swap
    postpone then
    r> 2 postpone literal,
  else
    string? "place count		\ temporary string
  then
; immediate

[then]

\\	"							I	ALE
\\
\\	    Compilation:	( "ccc<">" -- )
\\		See s".
\\
\\	    Execution:		( -- c-addr u )
\\		See s".
\\
\\	    Interpretation:	( "ccc<">" -- c-addr u )
\\		See s".
\\
\\		Its the default for strings.  Same as s".
\\
: " postpone s" ; immediate

\\	."							I	CORE
\\
\\	    Compilation:	( "ccc<">" -- )
\\		Parse characters ccc delimited by " (double-quote).  Append
\\		the  execution  semantics  specified  below  to the current
\\		definition.
\\
\\	    Execution:		( -- )
\\		Display ccc.
\\
\\	    Interpretation:	( "ccc<"> -- )
\\		Parse characters ccc delimited by " (double-quote). Display
\\		ccc.
\\
: ."
  compiling
  if
    postpone s" postpone type
  else
    string? type
  then
; immediate
