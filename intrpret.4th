\
\	ALE Forth Version 2.04
\
\		Copyright (c) 1991,1992 by Lutz Sammer.
\
\		Bug-reports via usenet: johns
\
\	File:	intrpret.4th
\
\		Forth kernel interpreter.
\

\ -----------------------------------------------------------------------------
\	Defining words
\ -----------------------------------------------------------------------------

compiler definitions

\\	(header)		( c-addr -- )				ALE
\\
\\		Create rest of the dictionary entry after the name field. 
\\
\	FIXME:
\		For better use of the cache lines the code field should
\		start at 16 byte (Paragraph) boundary.
\
: (header)
  warning @
  if					\ Warn about double entries
    find
    if
      drop here dup count type ."  isn't unique" cr
    then
  then

  align					\ Align dictionary entry
  here
  get-current @ ,			\ Link field
  nil ,					\ Compiler field
  NORMAL c,				\ Flag field
  dup rot - c,				\ Name field offset
  0 c,					\ Primitive length

  dup get-current !			\ Connect to vocabulary

  ->code
  [ /cache ] [if]
    dup ?update-cache			\ Update cache for new created word
  [then]
  last !				\ Save as last created word
;

\\	"header			( c-addr u -- )				ALE
\\
\\		Create  a  dictionary entry for the name c-addr and u  with
\\		the  execution  and  compilation semantics defined  by  the
\\		caller.  If  the variable WARNING is non-zero a warning  is
\\		given for redefined words.
\\
\\	    Typical use:
\\		s" paul" "header 	\ Create dictionary header
\\		' <paul> compiler,	\ Defines compilation semantics
\\		' (paul) executer,	\ Defines execution semantics
\\					\	Generated code field.
\\
: "header
  here dup >r over 1+ chars allot place	\ Name field
  (header)
;

\\	compiler,		( xt -- )			C	COMPILER
\\
\\		Modify  the most recently created dictionary entry to  call
\\		the execution token xt at compile time.
\\
: compiler,
\  $E9 c, here cell+ - ,		\ jmp	displacement
  last @ >compiler !
; compilation

\\	executer,		( xt -- )			C	COMPILER
\\
\\		Modify  the most recently created dictionary entry to  call
\\		the execution token xt at execution time.
\\
/ps=sp [if]

: executer,
  $E9 c, here cell+ - ,			\ jmp	displacement
; compilation

[else]

: executer,
  $E8 c, here cell+ - ,			\ call	displacement
; compilation

[then]

\\	header			( "name" -- )				ALE
\\
\\		Parse   name   delimited   by  a  space,  ignoring  leading
\\		delimiters.  Create  a dictionary  entry  for name with the
\\		execution and compilation semantics defined by caller.
\\
\\	    Typical use:
\\		header otto	 	\ Create dictionary header
\\		' <otto> compiler,	\ Defines compilation semantics
\\		' (otto) executer,	\ Defines execution semantics
\\					\	Generated code field.
\\
: header
  bl word dup c@ 0= abort" name required"
  dup c@ char+ allot			\ Name field
  (header)
;

\\	<create>		( xt -- )			C	ALE
\\
\\		Inline compiler for CREATE, appends the execution semantics
\\		of a CREATEd word to the current definition.
\\
: <create>
					\ push tos
  >body postpone literal		\ mov $address, tos
; compilation

/ps=sp [if]

\\	(create)		( -- dfa )			C	COMPILER
\\
\\		Place the address of the data field on the stack.
\\		Only used for the execution of entries created by CREATE.
\\
primitive (create)
	push-tos
	leal	5 (%eax), tos
end-primitive compilation

[else]

\\	(create)		( -- dfa ) ( R: dfa -- )	C	COMPILER
\\
\\		Place the address of the data field on the stack.
\\		Only used for the execution of entries created by CREATE.
\\
primitive (create)
	push-tos
	popl	tos
end-primitive compilation

[then]

/ps=sp [if]

\\	<does>>			( xt -- )			C	ALE
\\
\\		Inline  compiler  for DOES> modified CREATED words,  append
\\		the  execution  semantics of a CREATEd DOES>  word  to  the
\\		current definition.
\\
: <does>>
  dup					\ push tos
  >body postpone literal		\ mov $address, tos
					\ jmp does-code
  1+ dup @ + cell+			\ -> convert to real address
  cell+					\ over swap code
  call,					\ call the does-code
; compilation

\\	(does>)			( -- ) ( R: sys -- )		C	COMPILER
\\
\\		Compilation word,  modify  the  execution  semantics of the
\\		most  recently  defined  word  as  shown  by DOES>.  Return
\\		control  to  the caller of the definition containing DOES>.
\\		(DOES>) is compiled by DOES>.
\\
: (does>)
  ['] <does>> compiler,			\ Modify compiler-field

  r>					\ Address of does code
  last @				\ Modify executer-field to
  $E8 over c!				\ CALL
  1+ dup >r cell+ - r> !		\ call	displacement
; compilation

[else]

\\	<does>>			( xt -- )			C	ALE
\\
\\		Inline  compiler  for DOES> modified CREATED words,  append
\\		the  execution  semantics of a CREATEd DOES>  word  to  the
\\		current definition.
\\
: <does>>
  dup					\ push tos
  >body postpone literal		\ mov $address, tos
					\ jmp does-code
  1+ dup @ + cell+			\ -> convert to real address
  6 +					\ over interpret code
  call,					\ call the does-code
; compilation

\\	(does>)			( -- ) ( R: sys -- )		C	COMPILER
\\
\\		Compilation word,  modify  the  execution  semantics of the
\\		most  recently  defined  word  as  shown  by DOES>.  Return
\\		control  to  the caller of the definition containing DOES>.
\\		(DOES>) is compiled by DOES>.
\\
code (does>)
	popl	%eax			\ Code after does> + unnest
	movl	Dlast , %edx
	movl	$ X3Cdoes3E3E , -7 (%edx)	\ Modify compiler field
	subl	%edx, %eax
	leal	-5 (%eax), %eax
	movl	%eax, 1 (%edx)		\ Modify code field
end-code compilation

[then]

\\	<:>			( xt -- )			C	ALE
\\
\\		Inline compiler for :, appends the execution semantics of a
\\		colon definition to the current definition.
\\
: <:>
  call,					\ CALL $code
; compilation

/ps=sp [if]

\\	(constant)		( dfa x -- x (dfa) )		C	COMPILER
\\
\\		Place the contents of the data field on the stack.
\\		Only used for the execution of entries created by constant.
\\
primitive (constant)
	pushl	tos
	movl	5 (%eax), tos
end-primitive compilation

[else]

\\	(constant)		( -- (dfa) ) ( R: dfa -- )	C	COMPILER
\\
\\		Place the contents of the data field on the stack.
\\		Only used for the execution of entries created by constant.
\\
primitive (constant)
	push-tos
	popl	%eax
	movl	(%eax), tos
end-primitive compilation

[then]

\\	<constant>		( xt -- )			C	COMPILER
\\
\\		Inline   compiler  for  CONSTANT,  appends  the   execution
\\		semantics of a CONSTANT to the current definition.
\\
: <constant>
					\ push tos
  >body @ postpone literal		\ mov (address), tos
; compilation

\\	(variable)		( -- dfa ) ( R: dfa -- )	C	COMPILER
\\
\\		Place the address of the data field on the stack.
\\		Only used for the execution of entries created by variable.
\\
/ps=sp [if]

code (variable)
	pushl	tos
	leal	5 (%eax), tos
end-code compilation

[else]

code (variable)
	push-tos
	popl	tos
end-code compilation

[then]

\\	(value)			( -- (dfa) ) ( R: dfa -- )	C	COMPILER
\\
\\		Place the contents of the data field on the stack.
\\		Only used for the execution of entries created by VALUE.
\\
/ps=sp [if]

code (value)
	pushl	tos
	movl	5 (%eax), tos
end-code compilation

[else]

code (value)
	push-tos
	popl	%eax
	movl	(%eax), tos
end-code compilation

[then]

\\	(field)			( n -- n+(dfa) ) ( R: dfa -- )	C	COMPILER
\\
\\		Add the contents of the data field to n.
\\		Only used for the execution of entries created by field.
\\
/ps=sp [if]

code (field)
	addl	5 (%eax), tos
end-code compilation

[else]

code (field)
	popl	%eax
	addl	(%eax), tos
end-code compilation

[then]

\\	<variable>		( xt -- )			C	COMPILER
\\
\\		Inline   compiler  for  VARIABLE,  appends  the   execution
\\		semantics of a VARIABLE to the current definition.
\\
: <variable>
					\ push tos
  >body postpone literal		\ mov $address, tos
; compilation

\\	<field>			( xt -- )			C	COMPILER
\\
\\		Inline  compiler for FIELD, appends the execution semantics
\\		of a FIELD to the current definition.
\\
: <field>
  >body @ dup
  if					\ Something to add?
    dup -128 128 within
    if					\ Optimize add of -128...127
      $C383 w, c,			\ addl $field, %ebx
      exit
    then
    $C381 w, ,				\ addl $field, %ebx
    exit
  then
  drop
; compilation

\\	<value>			( xt -- )			C	COMPILER
\\
\\		Inline  compiler for VALUE, appends the execution semantics
\\		of a VALUE to the current definition.
\\
: <value>
  >body
  push,					\ push tos
  $1D8B w, ,				\ movl address, %ebx
; compilation

\\	<primtive>		( xt -- )			C	COMPILER
\\
\\		Inline   compiler  for  PRIMITIVE,  appends  the  execution
\\		semantics of a PRIMITVE to the current definition.
\\
: <primitive>
  here over >link ->size c@
  dup allot cmove			\ Copy primitive
; compilation

\\	<code>			( xt -- )			C	COMPILER
\\
\\		Inline  compiler for CODE, appends the execution  semantics
\\		of a CODE to the current definition.
\\
: <code>
  call,					\ CALL $code
; compilation

forth definitions

\\	create			( "name" -- )			D	CORE
\\
\\		Parse   name   delimited   by  a  space,  ignoring  leading
\\		delimiters.  Create  a  dictionary  entry for name with the
\\		execution  semantics  defined below.  If the address of the
\\		next  available data space location is not aligned, reserve
\\		enough data space to align it.  This address defines name's
\\		data  field.  CREATE does not allocate space in name's data
\\		field.
\\
\\	    name Execution:	( -- a-addr )
\\		a-addr  is the address of name's data field.  An additional
\\		set of operations may be defined using DOES>.
\\
: create
  header
  ['] <create> compiler,
  ['] (create) executer,
;

\\	does>							C I	CORE
\\
\\	    Compilation:	( colon-sys1 -- colon-sys2 )
\\		Append   the  execution  semantics  below  to  the  current
\\		definition.
\\
\\	    Execution:		( -- ) ( R: sys1 -- )
\\		Modify the execution semantics of the most recently defined
\\		word  as  shown below.  Return control to the caller of the
\\		definition containing DOES>. The most recently defined word
\\		must  have  been defined with CREATE or a user-defined word
\\		that calls CREATE.
\\
\\	    name  Execution:	( -- a-addr ) ( R: -- sys2 )
\\		where name is the word modified by DOES>
\\		Save   implementation-dependent   information   about   the
\\		definition  that  called  name, and place name's data field
\\		address on the stack.  Execute the code following the DOES>
\\		that modified name.
\\
/ps=sp [if]

: does>
  postpone (does>)
  postpone swap				\ Access data field address
  nest,					\ Build stack frame
; immediate compilation

[else]

: does>
  postpone (does>)
  push,
  $5B c,				\ popl	%ebx
; immediate compilation

[then]

\\	constant		( x "name" -- )			D	CORE
\\
\\		Parse   name   delimited   by  a  space,  ignoring  leading
\\		delimiters.  Create  a dictionary  entry  for name with the
\\		execution semantics defined below. name is referred to as a
\\		"constant."
\\
\\	    name Execution:	( -- x )
\\		Place x on the stack.
\\
: constant
  header
  ['] <constant> compiler,
  ['] (constant) executer,
  ,					\ Contents of the data field
;

\\	field			( n1 "name" -- )		D	ALE
\\
\\		Parse   name   delimited   by  a  space,  ignoring  leading
\\		delimiters.  Create  a  dictionary  entry for name with the
\\		execution semantics defined below.
\\
\\	    name Execution:	( n2 -- n1+n2 )
\\		Add n1 to n2.
\\
: field
  header
  ['] <field> compiler,
  ['] (field) executer,
  ,					\ Contents of the data field
;

\\	value			( "name" -- )			D	CORE EXT
\\
\\		Parse   name   delimited   by  a  space,  ignoring  leading
\\		delimiters.  Create  a  dictionary  entry for name with the
\\		execution semantics defined below. name is referred to as a
\\		"value".
\\
\\	    name Execution:	( -- x )
\\		Place  x on the stack.  The value of x is unspecified until
\\		the  phrase  x  TO  name  is  executed,  causing  x  to  be
\\		associated with name.
\\
: value
  header
  ['] <value> compiler,
  ['] (value) executer,
  0 ,					\ Data field initialized to zero
;

\\	to							I	CORE EXT
\\
\\	    Compilation:	( "name" -- )
\\		Parse   name   delimited   by  a  space,  ignoring  leading
\\		delimiters.  An  ambiguous condition exists if name was not
\\		defined  by  either VALUE or (LOCAL).  Append the execution
\\		semantics given below to the current definition.
\\
\\	    Execution:		( x -- )
\\		Store x in name.
\\
\\	    Interpretation:	( x "name" -- )
\\		Parse   name   delimited   by  a  space,  ignoring  leading
\\		delimiters and store x in name.
\\
: to
  [ /ps=sp ] [if]
  ' dup c@ $E9 =			\ jmp
  [else]
  ' dup c@ $E8 =			\ call
  [then]
  if
    dup 1+ dup @ + cell+		\ convert to real address
    ['] (value) =
    if
      >body compiling
      if
	$1D89 w, ,			\ movl	%ebx, data(name)
	pop,				\ pop	tos
      else
	!
      then
      exit
    then
    [ /user ] [if]			\ User words support
    dup 1+ dup @ + cell+		\ convert to real address
    ['] (user-value) =
    if
      >body @ compiling
      if
	dup -128 128 within
	if				\ -128..127 Byte offset
	  $5F89 w, c,			\ movl	%ebx, offset (%edi)
	else
	  $9F89 w, ,			\ movl	%ebx, offset (%edi)
	then
	pop,				\ pop	tos
      else
	up@ + !
      then
      exit
    then
    [then]
  then
  .name true abort" not a value"
; immediate

\\	variable		( "name" -- )			D	CORE
\\
\\		Parse   name   delimited   by  a  space,  ignoring  leading
\\		delimiters.  Create  a  dictionary  entry for name with the
\\		execution semantics defined below. Reserve one cell of data
\\		space  at  an  aligned  address.  name  is referred to as a
\\		"variable."
\\
\\	    name Execution:	( -- a-addr )
\\		a-addr is the address of the reserved cell. The application
\\		is  responsible  for   initializing  the  contents  of  the
\\		reserved cell.
\\
: variable
  header
  ['] <variable> compiler,
  ['] (variable) executer,
  0 ,					\ Data field initialized to zero
;

\\	csp			( -- a-addr )			SECURE ALE
\\
\\		a-addr is the address of CSP.  CSP contains the  address of
\\		the  parameter  stack  pointer.  Used  for  compiler  error
\\		checking.
\\
variable csp

\\	!csp			( -- )				SECURE ALE
\\
\\		Store  parameter  stack pointer in CSP.  Used for  compiler
\\		error checking.  Checked by ?CSP.
\\
: !csp
  sp@ csp !
;

\\	?csp			( -- )				SECURE ALE
\\
\\		Issue a error message if paramter stack pointer has changed
\\		since the most recent !CSP.
\\
: ?csp
  sp@ csp @ <> abort" Stack changed"
;

\\	:			( "name" -- colon-sys )		D	CORE
\\
\\	    Usage:		: name  <words>  ;
\\		Parse   name   delimited   by   a space,  ignoring  leading
\\		delimiters.  Create  a  dictionary  entry  for  name. Enter
\\		compilation  state. The execution semantics of name will be
\\		determined  by  the  words  compiled  into  the body of the
\\		definition  following  execution  of  :  (colon)  until the
\\		execution   of  ;  (semi-colon).  The  newly  created  word
\\		definition for name cannot be found in the dictionary until
\\		the definition is completed.
\\		name is called a "colon definition".
\\		colon-sys is balanced by the corresponding ;.
\\
\\	    name Execution:	( i*x -- j*x ) ( R:  -- sys )
\\		Save  implementation-dependent  information (sys) about the
\\		definition  that  called  name  and perform the body of the
\\		definition.
\\
: :
  !csp				\ Compiler security
  header
  ['] <:> compiler,
  nest,
  HIDDEN last @ >flag c!	\ Flag field: hidden for definition
  ]				\ Start compiler
;

\\	:noname			( -- xt colon-sys )			CORE EXT
\\
\\		Create  an  execution  token  and  enter compilation state.
\\		Information  is  added to the end of the dictionary so that
\\		code  compiled  at  the  next  dictionary  location will be
\\		associated  with  xt.  This  code  can be executed later by
\\		using xt EXECUTE. sys is balanced by the corresponding ; or
\\		;CODE. An ambiguous condition exists if :NONAME is executed
\\		while in compilation state.
\\
\\	    xt	Execution:	( i*x -- j*x ) ( R:  -- sys )
\\		Save   implementation-dependent   information   about   the
\\		definition that executed xt.
\\		Typically,  the execution  semantics  of xt are expanded by
\\		compiling additional words into the definition.
\\
: :noname
  here				\ = xt
  !csp				\ Compiler security
  nest,
  0 last !			\ Turns off reveal of ;
  ]				\ Start compiler
; execution

\\	recurse			( -- )				C I	CORE
\\
\\		Append the execution semantics of the current definition to
\\		the current definition.
\\
: recurse
  last @ dup
  if
    compile, exit
  then
				\ FIXME: recurse in :noname
  drop true abort" use of recurse in :noname"
; compilation immediate

\\	exit			( -- ) ( R: sys -- )		C I	CORE
\\
\\		Return  control  to the caller of the definition containing
\\		EXIT.
\\
\ FIXME: primitive exit
\ end-primitve compilation
: exit
  unnest,
  ret,
; immediate compilation

\ variable 'optimizer

\\	;							C I	CORE
\\
\\	    Compilation:	( colon-sys -- )
\\		Compile  EXIT  (or  an  implementation-dependent  word that
\\		performs an equivalent function) in the current definition.
\\		End the current word definition and allow it to be found in
\\		the  dictionary.  Enter interpretation state.  colon-sys is
\\		balanced by the corresponding : or :NONAME.
\\
\\	    Execution:		( -- ) ( R:  sys -- )
\\		Return  control to the caller of the definition  containing
\\		;.  sys is balanced by the corresponding : or :NONAME.
\\
: ;
  postpone exit
\   'optimizer perform		\ Start optional optimizer
  [compile] [			\ Restart interpreter
  last @ dup			\ Check zero for :NONAME
  if
    [ /cache ] [if]
      dup ?update-cache		\ Update cache for revealed word
    [then]
    >flag NORMAL swap c!	\ Reveal last definition
  else
    drop
  then
  ?csp				\ Compiler security
; compilation immediate

\\	compilation		( -- )					ALE
\\
\\		Mark  the  most  recently  created  dictionary  entry as an
\\		compilation word.  Only visibile in compilation state.
\\
: compilation
  last @ >flag dup c@ COMPILATION or swap c!
;

\\	immediate		( -- )					CORE
\\
\\		Mark  the  most  recently  created  dictionary  entry as an
\\		immediate word.
\\
: immediate
  last @ >flag dup c@ IMMEDIATE or swap c!
;

\\	execution		( -- )					ALE
\\
\\		Mark  the  most  recently  created  dictionary  entry as an
\\		execution word.  Only visibile in execution state.
\\
: execution
  last @ >flag dup c@ EXECUTION or swap c!
;

\\	private			( -- )					ALE
\\
\\		Mark  the  most  recently  created  dictionary  entry as an
\\		private word.  Only visibile in the definition vocabulary.
\\
: private
  last @ >flag dup c@ PRIVATE or swap c!
;

\\	hidden			( -- )					ALE
\\
\\		Mark  the  most  recently  created  dictionary  entry as an
\\		hidden word.  Never visibile.
\\
: hidden
  last @ >flag dup c@ HIDDEN or swap c!
;

\\	recognizer		( -- )					ALE
\\
\\		Mark   the  most  recently  created  dictionary  entry   as
\\		recognizer of the current vocabulary.
\\
: recognizer
  last @ get-current cell+ !
;

\\	number?							R	ALE
\\				( c-addr u -- 0 )
\\			-or-	( c-addr u -- n 1 )
\\			-or-	( c-addr u -- d 2 )
\\
\\		Look  if  the string c-addr and u, can be converted  into a
\\		number.  If the string is no number return zero.  If it can
\\		be  converted into a number, return its number n and 1.  if
\\		it can be converted into a double number, return its double
\\		d and 2.
\\
\\		Forth wordlist literals recognizer.
\\
: number?
  over c@ [char] - = dup >r		\ Negative
  if
    1 /string
  then
  ?dup					\ More chars
  if
    0 0 2swap >number dup
    if					\ Input not complete converted
      1 =				\ One char left
      if
	c@ [char] . =
	if				\ Double cell number
	  r>  if  dnegate  then  2
	  exit
	then
      else
	drop
      then
    else
      3 discard r>  if  negate  then  1	\ Single cell number
      exit
    then
  then
  r> 3 discard 0			\ Not recognized
; recognizer

\ -----------------------------------------------------------------------------
\	Compiler and interpreter words
\ -----------------------------------------------------------------------------

\\	(			( "ccc<)>" -- )			I	CORE
\\
\\		Parse  characters  ccc  delimited  by a closing parenthesis
\\		")".  Ignore  the  resulting  text. ( is an immediate word.
\\		When  parsing,  the number of characters in ccc may be zero
\\		to  the number of characters remaining in the input stream.
\\		When  parsing from a text file, the number of characters in
\\		ccc  may  be 0 to the number of characters remaining in the
\\		file.
\\
: (
  source-file
  if					\ Source is text file
    begin
      source >in @ /string		\ Adjust input
      [char] ) scan dup
      if				\ Found )
	#tib @ swap - 1+ >in ! drop exit
      then
      2drop refill 0=
    until
    exit
  then
  [char] ) parse 2drop
; immediate

\\	.(			( "ccc<)>" -- )			I	CORE EXT
\\
\\		Parse  and  display  characters ccc delimited by ) (closing
\\		parenthesis).
\\
: .(
  [char] ) parse type
; immediate

\\	\			( "ccc<eol>" -- )		I	CORE EXT
\\
\\		If  SOURCE-FILE and BLK contains zero, ignore the remainder
\\		of the current input stream; otherwise ignore the remainder
\\		of the current line.
\\
: \
  [ /block ] [if]			\ Block word set present
    blk @
    if					\ BLOCK
      c/l >in @ c/l mod - >in +! exit
    then
  [then]
  #tib @ >in !
; immediate

\\	\\			( "ccc<eol>" -- )		I	ALE
\\
\\		For forth ignore the remainder of the current line.
\\		For document generation copy the remainder of the current
\\		line into the document file.
\\
: \\ [compile] \ ; immediate

\\	recognize		( c-addr -- c-addr 0 )			ALE
\\			-or-	( c-addr -- x1 ... xn n )
\\			-or-	( c-addr -- -1 )
\\
\\		Recognize  the literal in the counted string at c-addr.  If
\\		the literal is not recognized, return c-addr and 0.  If the
\\		literal  is  recognized, return the literals x1 ... xn  and
\\		the number of literals n.  If the literal is recognized and
\\		the literals consumed -1.
\\
: recognize
  count
  context #order cells bounds
  ?do					\ Execute recognizer of each wordlist
    i @ cell+ @ dup
    if
      ( c-addr u xt )
      pluck pluck 2>r execute dup	\ Recognized?
      if
	2r> 2drop unloop exit
      then
      drop 2r>
    else
      drop
    then
    cell
  +loop
  drop 1- 0
;

false [if]
\\	interpret		( -- )					ALE
\\
\\		The  outer text interpreter which sequentially executes  or
\\		compiles  text  from the input stream  (terminal  or  disk)
\\		depending on STATE.  If the word name cannot be found after
\\		a  search  of  CONTEXT, then it is converted  to  a  number
\\		according to the current base.  That also failing, an error
\\		message echoing the name with a "?" will be given.
\\
\\		FIXME: DOC of recognize
\\
: interpret
  begin
    bl word dup c@
  while
    find dup
    if					\ word found
      compiling =			\ IMMEDIATE
      if
	compile,			\ compiler
      else
	execute				\ executer
      then
    else				\ word not found
      drop recognize dup
      if				\ literal recognized
	dup 0> compiling or
	if
	  postpone literal,		\ compile number as literal
	else
	  drop				\ n
	then
      else
	?missing
      then
    then
  repeat
  drop
;
[else]

compiler definitions

\\	execute-literal		( c-addr -- i*n )			ALE
\\
\\		Literals  executed if not compiling.  i*n are the  literals
\\		represented by c-addr.
\\
: execute-literal
  recognize dup
  if					\ literal recognized
    drop exit				\ Remove number
  then
  ?missing				\ Issue an error
;

\\	compile-literal		( c-addr -- )				ALE
\\
\\		Literals compiled if compiling.
\\
: compile-literal	( c-addr -- )
  recognize dup
  if					\ literal recognized
    dup 0>
    if
      postpone literal, exit
    then
    drop exit
  then
  ?missing				\ Issue an error
;

\\	run-table		( -- a-addr )				ALE
\\
\\		Interpret run-table.
\\
\\	    Internals:
\\			cell	xt for executer of immediate words
\\			cell	xt for recognizer of literals if interpreting
\\			cell	xt for executer of words if interpreting
\\			cell	xt for compiler of words if compiling
\\
create run-table
  ' execute ,				\ immediate	word	s=0 f=-4
  ' execute-literal ,			\ execute	literal	s=0 f= 0
  ' execute ,				\ execute	word	s=0 f= 4
  ' compile-literal ,			\ compile	literal	s=8 f= 0
  ' compile, ,				\ compile	word	s=8 f= 4

forth definitions

\\	interpret		( -- )					ALE
\\
\\		The  outer text interpreter which sequentially executes  or
\\		compiles  text  from the input stream  (terminal  or  disk)
\\		depending on STATE.  If the word name cannot be found after
\\		a  search  of  CONTEXT, then it is converted  to  a  number
\\		according to the current base.  That also failing, an error
\\		message echoing the name with a "?" will be given.
\\
\\		FIXME: DOC of recognize
\\
: interpret
  begin
    bl word dup c@			\ Scan input get next word
  while
    find
    inline ( xt|c-addr|xt -1|0|1 )
	movl	Dstate , %eax
	leal	(tos, %eax, 2), %eax
	pop-tos
	[ /ps=sp ] [if]
	  movl	$ .+13 , (%esi)
	  jmp	Drun2Dtable+4 (, %eax, 4)
	[else]
	  call	Drun2Dtable+4 (, %eax, 4)
	[then]
    end-inline
  repeat
  drop
;

[then]

\\	evaluate		( i*x c-addr u -- j*x )			CORE
\\
\\		Save  the  current  input  stream  specification.  Make the
\\		string  described  by c-addr and u the current input stream
\\		and  interpret  its  contents.  When  the  input  stream is
\\		exhausted, restore the prior input stream specification.
\\
: evaluate
  >in @ #tib @ 2>r			\ Save current input stream
  tib source-file 2>r
  [ /block ] [if]
    blk @ >r

    blk off				\ Setup new input stream
  [then]

  -1 to source-file #tib ! to tib
  >in off

  ['] interpret catch dup
  if					\ If error, display it
    ." :evaluate"
  then

  [ /block ] [if]			\ Restore prior input stream
    r> blk !
  [then]
  2r> to source-file to tib
  2r> #tib ! >in !

  throw					\ If error throw to next frame
;

compiler definitions

\\	?stack			( -- )					ALE
\\
\\		Check  the parameter stack for underflows or overflows  and
\\		issue appropriate error message if detected.
\\
\\		UNIX and MSDOS with GO32:
\\			The parameter stack can grow dynamic.
\\
: ?stack
  depth 0< abort" Stack underflow"
;

forth definitions
