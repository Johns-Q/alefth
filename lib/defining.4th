\
\	ALE Forth Version 2.04
\
\		Copyright (c) 1991,1992 by Lutz Sammer.
\
\		Bug-reports via usenet: johns
\
\	File:	defining.4th
\
\		New defining words.
\

also compiler also hex

compiler definitions

\\	?immediate		( xt -- flag )				ALE
\\
\\		Flag  is  true if the execution xt is marked  as  immediate
\\		word.  Otherwise false.
\\
: ?immediate
  >flag c@ IMMEDIATE and 0<>
;

\\	(alias)			( -- i*x )				ALE
\\
\\		Executer of words created by ALIAS.
\\		Execute xt.
\\
: (alias)
  (constant) execute
; compilation

\\	<alias>			( xt -- )			C	ALE
\\
\\		Compiler of words created by ALIAS.
\\		Compile xt.
\\
: <alias>
  >body @
  dup ?immediate			\ Immediate
  if
    execute exit
  then
  compile,
; compilation

forth definitions

\\	alias			( xt "name" -- )		D	ALE
\\
\\		Parse   name   delimited   by  a  space,  ignoring  leading
\\		delimiters.  Find name.  Make name does what xt does.
\\		An  ambiguous  condition exists if name is not found in the
\\		current search order.
\\
\\	    name Execution:		( -- )
\\		Does same as xt.
\\
: alias
  header
  ['] <alias> compiler,
  ['] (alias) executer,
  ,					\ Store xt
;

compiler definitions

\\	(defer)			( -- i*x )				ALE
\\
\\		Executer of words created by DEFER.
\\		Execute defered word.
\\
: (defer)
  (constant) execute
; compilation

\\	<defer>			( xt -- )			C	ALE
\\
\\		Compiler of words created by DEFER.
\\		Compile execution of a defered word.
\\
: <defer>
  >body
  [ ' dup c@ 53 = ] [if]		\ Processor stack = Parameter stack
    06C7 w, here A + ,			\ movl	$ .+12 , (%esi)
    25FF w, ,				\ JMP * xt >body
  [else]
    15FF w, ,				\ CALL * xt >body
  [then]
; compilation

forth definitions

\\	defer			( "name" -- )			D	ALE
\\
\\		Parse   name   delimited   by  a  space,  ignoring  leading
\\		delimiters.  Create  a  dictionary  entry for name with the
\\		execution semantics defined below. name is referred to as a
\\		"execution vector".
\\
\\	    name Execution:	( i*x -- j*x ) ( R: -- sys )
\\		Save  implementation-dependent  information (sys) about the
\\		definition  that executes name and perform the contents  of
\\		name.  The  action of name is unspecified until the  phrase
\\		xt  IS  name is executed, causing xt to be associated  with
\\		name.
\\
: defer
  header
  ['] <defer> compiler,
  ['] (defer) executer,
  nil ,					\ Reset vector
;

\\	is							I	ALE
\\
\\	    Compilation:	( "name" -- )
\\		Parse   name   delimited   by  a  space,  ignoring  leading
\\		delimiters.  An  ambiguous condition exists if name was not
\\		defined  by  DEFER.  Append the execution  semantics  given
\\		below to the current definition.
\\
\\	    Execution:		( xt -- )
\\		Store xt in name.
\\
\\	    Interpretation:	( xt "name" -- )
\\		Parse   name   delimited   by  a  space,  ignoring  leading
\\		delimiters and store xt in name.
\\
: is
  ' dup c@ dup E9 <> swap E8 <> and	\ JMP or CALL
  over 1+ dup @ + cell+			\ convert to real address
  ['] (defer) <> or
  if
    .name true abort" not defered"
  then
  >body compiling
  if
    1D89 w, ,				\ movl	%ebx, ' name >body
    pop,				\ pop	tos
  else
    !
  then
; immediate

\\	array			( n "name" -- )			D	ALE
\\
\\		Parse   name   delimited   by  a  space,  ignoring  leading
\\		delimiters.  Create  a  dictionary  entry for name with the
\\		execution semantics defined below.  Reserve n cells of data
\\		space  at  an  aligned  address.  name  is referred to as a
\\		"array".
\\
\\	    name Execution:	( -- a-addr )
\\		a-addr  is  the  address of the first  reserved  cell.  The
\\		application is responsible for initializing the contents of
\\		the reserved cells.
\\
: array
  create cells allot			\ Allocate array space
;

only forth decimal
