\
\	ALE Forth Version 2.04
\
\		Copyright (c) 1991,1992 by Lutz Sammer.
\
\		Bug-reports via usenet: johns
\
\	File:	user.4th
\
\		Forth kernel user defined words support.
\

\ -----------------------------------------------------------------------------
\	User vocabulary
\ -----------------------------------------------------------------------------

\\	user			( -- )				V	USER
\\
\\		Append the user word list to the current search set.
\\
\\	    Words:
\\		up@	up!		>user	#user	allot
\\		create	variable	value
\\
vocabulary user  user definitions

\\	up!			( a-addr -- )				USER
\\
\\		Set the user pointer to a-addr.
\\
primitive up!
	movl	tos, %edi
	pop-tos
end-primitive

\\	up@			( -- a-addr )				USER
\\
\\		Return  the user pointer.  a-addr is the start of the  user
\\		area.
\\
primitive up@
	push-tos
	movl	%edi, tos
end-primitive

\\	>user			( -- a-addr )				USER
\\
\\		a-addr  is the address of >USER.  >USER contains the offset
\\		in  characters from the start of the start of the user area
\\		to the next character to be allocated.
\\
variable >user  \ Set by the target compiler 0 >user !

\\	#user			( -- n )				USER
\\
\\		n is the size of the user area.
\\
1024 constant #user

\\	allot			( n -- )				USER
\\
\\		Reserve n address units (bytes) of user data space.
\\
: user-allot
  >user +!
;

compiler definitions

\\	(user-create)		( -- a-addr )				USER
\\
\\		Place the address of the data field in the user area on the
\\		stack.  Only  used for the execution of entries created  by
\\		USER CREATE.
\\
/ps=sp [if]

/less-code [if]

: (user-create)
  (constant) up@ +
; compilation

[else]

code (user-create)
	push-tos
	movl	5 (%eax), tos
	addl	%edi, tos
end-code compilation

[then]
[else]

/less-code [if]

: (user-create)
  r> @ up@ +
; compilation

[else]

code (user-create)
	push-tos
	popl	tos
	addl	%edi, tos
end-code compilation

[then]
[then]

\\	<user-create>		( xt -- )			C	ALE
\\
\\		Inline  compiler  for  USER CREATE, appends  the  execution
\\		semantics of a USER CREATEd word to the current definition.
\\
: <user-create>
  >body @
  push,
  dup -128 128 within
  if						\ -128..127 Byte offset
    $5F8D w, c,					\ leal  offset (%edi), %ebx
  else
    $9F8D w, ,					\ leal	offset (%edi), %ebx
  then
; compilation

\\	(user-variable)		( -- a-addr )				USER
\\
\\		Place the address of the data field in the user area on the
\\		stack.  Only  used for the execution of entries created  by
\\		USER VARIABLE.
\\
/ps=sp [if]

/less-code [if]

: (user-variable)
  (constant) up@ +
; compilation

[else]

code (user-variable)
	push-tos
	movl	5 (%eax), tos
	addl	%edi, tos
end-code compilation

[then]
[else]

/less-code [if]

: (user-variable)
  r> @ up@ +
; compilation

[else]

code (user-variable)
	push-tos
	popl	tos
	addl	%edi, tos
end-code compilation

[then]
[then]

\\	<user-variable>		( xt -- )			C	ALE
\\
\\		Inline  compiler for USER VARIABLE, appends the  execution
\\		semantics   of  a  USER  VARIABLE  word  to  the   current
\\		definition.
\\
: <user-variable>
  >body @
  push,
  dup -128 128 within
  if						\ -128..127 Byte offset
    $5F8D w, c,					\ leal  offset (%edi), %ebx
  else
    $9F8D w, ,					\ leal	offset (%edi), %ebx
  then
; compilation

\\	(user-value)		( -- n )				USER
\\
\\		Place  the  contents of the data field in the user area  on
\\		the  stack.  Only used for the execution of entries created
\\		by USER VALUE.
\\
/ps=sp [if]

/less-code [if]

: (user-value)
  (constant) up@ + @
; compilation

[else]

code (user-value)
	push-tos
	movl	5 (%eax), tos
	movl	(%edi, tos), tos
end-code compilation

[then]
[else]

/less-code [if]

: (user-value)
  >r @ up@ + @
; compilation

[else]

code (user-value)
	push-tos
	popl	tos
	movl	(%edi, tos), tos
end-code compilation

[then]
[then]

\\	<user-value>		( xt -- )			C	ALE
\\
\\		Inline  compiler  for  USER VALUE,  appends  the  execution
\\		semantics of a USER VALUE to the current definition.
\\
: <user-value>
  >body @
  push,
  dup -128 128 within
  if						\ -128..127 Byte offset
    $5F8B w, c,					\ movl  offset (%edi), %ebx
  else
    $9F8B w, ,					\ movl	offset (%edi), %ebx
  then
; compilation

\\	(user-defer)		( i*x -- j*x )				USER
\\
\\		Execute the contents of the data field in the user area.
\\		Only used for the execution of entries created by USER DEFER.
\\
/ps=sp [if]

/less-code [if]

: (user-defer)
  (constant) up@ + perform
; compilation

[else]

code (user-defer)
	movl	5 (%eax), %eax
	jmp	(%edi, %eax)
end-code compilation

[then]
[else]

/less-code [if]

: (user-defer)
  >r @ up@ + perform
; compilation

[else]

code (user-defer)
	popl	%eax
	jmp	(%edi, %eax)
end-code compilation

[then]
[then]

\\	<user-defer>		( xt -- )			C	ALE
\\
\\		Inline  compiler  for  USER DEFER,  appends  the  execution
\\		semantics of a USER DEFER to the current definition.
\\
: <user-defer>
  >body @
  dup -128 128 within
  if						\ -128..127 Byte offset
    $06C7 w, here 7 + ,				\ movl	$ .+9 , (%esi)
    $67FF w, c,					\ jmp * offset (%edi)
  else
    $06C7 w, here 10 + ,			\ movl	$ .+12 , (%esi)
    $A7FF w, ,					\ jmp * offset (%edi)
  then
; compilation

user definitions

\\	create			( "name" -- )			D	USER
\\
\\		Parse   name   delimited   by  a  space,  ignoring  leading
\\		delimiters.  Create  a  dictionary  entry for name with the
\\		execution  semantics  defined below.  If the address of the
\\		next  available  user data space location is  not  aligned,
\\		reserve  enough user data space to align it.  This address
\\		is return by name execution.
\\		CREATE does allocate space in the user data space.
\\
\\	    name Execution:	( -- a-addr )
\\		a-addr is the address of user data space.
\\
: user-create
  header
  ['] <user-create> compiler,
  ['] (user-create) executer,
  >user @ ,
;

\\	variable		( "name" -- )			D	USER
\\
\\		Parse   name   delimited   by  a  space,  ignoring  leading
\\		delimiters.  Create  a  dictionary  entry for name with the
\\		execution semantics defined below.  Reserve one cell of the
\\		user data space space at an aligned address.
\\		name  is referred to as a "user variable."
\\
\\	    name Execution:	( -- a-addr )
\\		a-addr is the address of the reserved cell in the user data
\\		space.  The application is responsible for initializing the
\\		contents of the reserved cell.
\\
: user-variable
  header
  ['] <user-variable> compiler,
  ['] (user-variable) executer,
  >user @ , cell user-allot
;

\\	value			( "name" -- )			D	USER
\\
\\		Parse   name   delimited   by  a  space,  ignoring  leading
\\		delimiters.  Create  a  dictionary  entry for name with the
\\		execution  semantics defined below.  name is referred to as
\\		a "user value."
\\
\\	    name Execution:	( -- x )
\\		Place  x on the stack.  The value of x is unspecified until
\\		the  phrase  x  TO  name  is  executed,  causing  x  to  be
\\		associated with name.
\\
: user-value
  header
  ['] <user-value> compiler,
  ['] (user-value) executer,
  >user @ , cell user-allot
;

\\	defer			( "name" -- )			D	USER
\\
\\		Parse   name   delimited   by  a  space,  ignoring  leading
\\		delimiters.  Create  a  dictionary  entry for name with the
\\		execution semantics defined below. name is referred to as a
\\		"user execution vector".
\\
\\	    name Execution:	( i*x -- j*x ) ( R: -- sys )
\\		Save  implementation-dependent  information (sys) about the
\\		definition  that executes name and perform the contents  of
\\		name.  The  action of name is unspecified until the  phrase
\\		xt  IS  name is executed, causing xt to be associated  with
\\		name.
\\
: user-defer
  header
  ['] <user-defer> compiler,
  ['] (user-defer) executer,
  >user @ , cell user-allot
;

\\	is							I	CORE EXT
\\
\\	    Compilation:	( "name" -- )
\\		Parse   name   delimited   by  a  space,  ignoring  leading
\\		delimiters.  An  ambiguous condition exists if name was not
\\		defined by either DEFER or USER DEFER. Append the execution
\\		semantics given below to the current definition.
\\
\\	    Execution:		( xt -- )
\\		Store xt in name.
\\
\\	    Interpretation:	( xt "name" -- )
\\		Parse   name   delimited   by  a  space,  ignoring  leading
\\		delimiters and store xt in name.
\\
: is
  [ /ps=sp ] [if]
  ' dup c@ $E9 =			\ jmp
  [else]
  ' dup c@ $E8 =			\ call
  [then]
  if
    dup 1+ dup @ + cell+		\ convert to real address
    ['] (user-defer) =
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
  then
  .name true abort" not defered"
; immediate

\\	init-user		( -- )					USER
\\
\\	FIXME: DOCO
: init-user
\ FIXME: where to place the user area.
  sp0 cell+ up!				\ At the end of the return stack.
\ FIXME: target->user to >user
;

forth definitions
