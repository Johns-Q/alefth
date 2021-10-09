\
\	ALE Forth Version 2.04
\
\		Copyright (c) 1991,1992 by Lutz Sammer.
\
\		Bug-reports via usenet: johns
\
\	File:	search.4th
\
\		Forth kernel vocabulary wordlists and search.
\

\ -----------------------------------------------------------------------------
\	Search order word set.
\ -----------------------------------------------------------------------------

\\	Search order word set:
\\
\\		#vocs		#order		context		current
\\		last		wid-link	voc-link
\\		get-current	set-current	get-order	set-order
\\		search-wordlist	definitions	also		only
\\		previous	seal		wordlist	vocabulary
\\		marker		.vocabulary	order		(set-first)
\\		(vocabulary)	<vocabulary>
\\

compiler definitions

\\	#vocs			( -- n )			SEARCH ALE
\\
\\		n  is  the  maximum number of vocabularies  in  the  search
\\		order.
\\
32	constant #vocs

\\	#order			( -- a-addr )			SEARCH ALE
\\
\\		a-addr is the address of #ORDER. #ORDER contains the number
\\		of wordlists in context.
\\
value #order

\\	context			( -- a-addr )			SEARCH ALE
\\
\\		a-addr  is  the address of CONTEXT.  CONTEXT  contains  the
\\		vocabularies that are searched for a name.
\\
create context  #vocs cells allot
\ #vocs cell erase

\\	current			( -- a-addr )			SEARCH ALE
\\
\\		a-addr  is  the address of CURRENT.  CURRENT  contains  the
\\		vocabulary where new definitions are placed.
\\
variable current

\\	last			( -- a-addr )			SEARCH ALE
\\
\\		a-addr is the address of the LAST, LAST contains the address
\\		of the last defined word.
\\
variable last

\\	wid-link		( -- a-addr )			SEARCH ALE
\\
\\		a-addr  is the address of WID-LINK.  WID-LINK contains  the
\\		execution token of the most recently defined wordlist.
\\		(Needed for FORGET).
\\
variable wid-link

\\	voc-link		( -- a-addr )			SEARCH ALE
\\
\\		a-addr  is the address of VOC-LINK.  VOC-LINK contains  the
\\		execution token of the most recently defined vocabulary.
\\		(Needed for ORDER).
\\
variable voc-link

forth definitions

\\	get-current		( -- wid )				SEARCH
\\
\\		Return wid, the identifier of the compilation word list.
\\
: get-current
  current @
;

\\	set-current		( wid -- )				SEARCH
\\
\\		Set  the compilation word list to the word list  identified
\\		by wid.
\\
: set-current
  current !
;

\\	get-order		( -- wid1 ... widn n )			SEARCH
\\
\\		Returns the number of word lists n in the search order and
\\		the word list identifiers wid1 ... widn identifying these
\\		word lists.  widn identifies the word list that is searched
\\		first, and wid1 the word list that is searched last.
\\		The search order is unaffected.
\\
: get-order
  #order dup
  if
    1- cells context tuck +
    do
      i @
      [ cell negate ] literal
    +loop
    #order
  then
;

\\	set-order		( wid1 ... widn n -- )			SEARCH
\\
\\		Set  the search order to the word lists identified by  wid1
\\		... widn.  Subsequently,  word list widn will  be  searched
\\		first,  followed by word list widn-1, and so on, with  word
\\		list  wid1  searched last.  If n is zero, empty the  search
\\		order.  If  n  is minus one, set the search  order  to  the
\\		implementation-defined  minimum  search order.  The minimum
\\		search  order  must include the ability  to  interpret  the
\\		words FORTH-WORDLIST and SET-ORDER.  A standard system must
\\		allow n to be at least 8.
\\
: set-order
  dup -1 =
  if					\ minimum search order
    drop forth-wordlist forth-wordlist 2
  then
  dup to #order
  0
  ?do
    i cells context + !
  loop
  [ /cache ] [if]
    empty-cache
  [then]
;

\\	search-wordlist		( c-addr u wid -- 0 | xt 1 | xt -1 )	SEARCH
\\
\\		Find  the  Forth word identified by the string c-addr and u
\\		in  the  word  list  identified  by wid. If the word is not
\\		found,  return  zero.  If  the  word  is  found, return its
\\		execution  token  xt  and  1  if  the word is immediate, -1
\\		otherwise.
\\
/less-code [if]

: search-wordlist
\ Mask can be calculated by [ ] and definitions
  state @				\ Build Mask
  if
    EXECUTION				\ execution: not visibile compiling
  else
    COMPILATION				\ compilation: not visibile interpret
  then
					\ hidden: execution && compilation
  over current @ <>
  if
    PRIVATE or				\ private: not visibile private
  then
  >r

  @					\ Get start of list
  begin
    dup
  while
    dup ->flag c@			\ Check if word is visibile
    r@ and
    0=if
      dup >r dup ->noff c@ - count
      pluck over -
      0=if				\ count is same
	2over compare
	0=if
	  2drop r@ ->code r> ->flag c@ IMMEDIATE and
	  if
	    -1
	  else
	    1
	  then
	  r> drop exit
	then
      else
	2drop
      then
      r>
    then
    @					\ Next element
  repeat
  r> 2drop 2drop 0			\ Not found
;

[else]

/ps=sp [if]

code search-wordlist
	popl	%eax			\ u
	popl	%edx			\ c-addr
	pushl	%esi
	pushl	%edi
	pushl	%ecx			\ Save registers
	xorl	%ecx, %ecx		\ %ecx=0!

	movb	$ EXECUTION .u , %ah
	cmpl	%ecx, Dstate <cr>
	jne	0f
	movb	$ COMPILATION .u , %ah	\ EXECUTION/COMPILATION Mask
0:	cmpl	Dcurrent , tos
	je	0f
	orb	$ PRIVATE .u , %ah	\ PRIVATE
0:
	movl	(tos), tos		\ Link
	orl	tos, tos
	jz	1f			\ End of list

	testb	8 (tos), %ah		\ Flag&Mask
	jnz	0b			\ Not visibile

	movb	9 (tos), %cl		\ Name offset BUG ALERT!
	movl	tos, %edi
	subl	%ecx, %edi		\ Name
	movb	(%edi), %cl		\ Length
	cmpb	%cl, %al
	jne	0b			\ Different name length

	incl	%edi
	movl	%edx, %esi		\ addr  c-addr

	repe
	cmpsb	(%esi), (%edi)
	jne	0b			\ Names different

	popl	%ecx			\ Restore registers
	popl	%edi
	popl	%esi

	leal	11 (tos), %eax		\ BUG ALERT -> Code field
	pushl	%eax
	testb	$ IMMEDIATE .u , -3 (%eax)	\ Look if immediate
	jz	0f
	movl	$ -1 , tos		\ Found immediate
	jmp	*(%esi)
0:
	movl	$ 1 , tos		\ Found not immediate
	jmp	*(%esi)
1:
	popl	%ecx			\ Restore registers
	popl	%edi
	popl	%esi
end-code

[else]

code search-wordlist
	pushl	%esi
	pushl	%edi
	pushl	%ecx			\ Save registers

	movl	(%esi), %eax		\ u
	movl	4 (%esi), %edx		\ c-addr
	xorl	%ecx, %ecx		\ %ecx=0!

	movb	$ EXECUTION .u , %ah
	cmpl	%ecx, Dstate <cr>
	jne	0f
	movb	$ COMPILATION .u , %ah	\ EXECUTION/COMPILATION Mask
0:	cmpl	Dcurrent , tos
	je	0f
	orb	$ PRIVATE .u , %ah	\ PRIVATE
0:
	movl	(tos), tos		\ Link
	orl	tos, tos
	jz	1f			\ End of list

	testb	8 (tos), %ah		\ Flag&Mask
	jnz	0b			\ Not visibile

	movb	9 (tos), %cl		\ Name offset BUG ALERT!
	movl	tos, %edi
	subl	%ecx, %edi		\ Name
	movb	(%edi), %cl		\ Length
	cmpb	%cl, %al
	jne	0b			\ Different name length

	incl	%edi
	movl	%edx, %esi		\ addr  c-addr

	repe
	cmpsb	(%esi), (%edi)
	jne	0b			\ Names different

	popl	%ecx			\ Restore registers
	popl	%edi
	popl	%esi
	leal	4 (%esi), %esi

	leal	11 (tos), %eax		\ BUG ALERT -> Code field
	movl	%eax, (%esi)
	testb	$ IMMEDIATE .u , -3 (%eax)	\ Look if immediate
	jz	0f
	movl	$ -1 , tos		\ Found immediate
	ret
0:
	movl	$ 1 , tos		\ Found not immediate
	ret
1:
	popl	%ecx			\ Restore registers
	popl	%edi
	popl	%esi
	leal	8 (%esi), %esi
end-code

[then]
[then]

\\	definitions		( -- )					SEARCH
\\
\\		Make  the  compilation word list the same as the first word
\\		list  in  the  search  order.   Specifies that the names of
\\		subsequent  definitions  will  be placed in the compilation
\\		word list.  Subsequent changes in the search order will not
\\		affect the compilation word list.
\\
: definitions
  context @ current !
\  get-order over set-current
;

\\	also			( -- )				SEARCH EXT
\\
\\		Transform  the search order consisting of wid1, ... widn-1,
\\		widn  (where widn is searched first) into wid1, ... widn-1,
\\		widn, widn.  An ambiguous condition exists if there are too
\\		many word lists in the search order.
\\		( If 
\\
: also
\  context [ context cell+ ] literal #order cmove>
\  #order 1+ #vocs min to #order
\  [ /cache ] [if]
\    empty-cache
\  [then]
  get-order over swap 1+ set-order
;

\\	only			( -- )				SEARCH EXT
\\
\\		Set  the search order to the implementation-defined minimum
\\		search  order.  The minimum search order must  include  the
\\		ability to interpret the words FORTH-WORDLIST and SET-ORDER.
\\
: only
  -1 set-order
;

\\	previous		( -- )				SEARCH EXT
\\
\\		Transform  the search order consisting of wid1, ... widn-1,
\\		widn  (where widn is searched first) into wid1, ... widn-1.
\\		An ambiguous condition exists if the search order was empty
\\		before PREVIOUS was executed.
\\
: previous
  get-order nip 1- set-order
;

\\	seal			( -- )				SEARCH ALE
\\
\\		Transform  the search order consisting of wid1, ... widn-1,
\\		widn  (where  widn  is  searched  first)  into  widn.  This
\\		prevents  access to any other vocabularies unless an escape
\\		mechanism is included in the widn word list.
\\
: seal
  get-order over 1 set-order discard
;

\\	wordlist		( -- wid )			D	SEARCH
\\
\\		Creates  a  new  empty word list, returning its  word  list
\\		identifier wid.  The new word list might be returned from a
\\		pool  of  preallocated  word lists or  may  be  dynamically
\\		allocated in data space.  A Standard System must allow  the
\\		creation  of  at least 8 new word lists in addition  to any
\\		provided as part of the system.
\\
: wordlist
  here
  0 ,					\ Last defined word in wordlist
  0 ,					\ Literal recognizer of wordlist
  wid-link @ ,				\ Wordlist link
  dup wid-link !			\ Link wordlists together
;

compiler definitions

\\	(set-first)		( wid -- )				COMPILER
\\
\\		Set the first searched wordlist.
\\		Transform  the search order consisting of wid1, ... widn-1,
\\		widn  (where widn is searched first) into wid1, ... widn-1,
\\		wid.
\\
: (set-first)
  context !
  [ /cache ] [if]
    empty-cache
  [then]
\  >r get-order nip r> swap set-order	\ Portable
;

/ps=sp [if]

\\	(vocabulary)		( 'wid -- )			SEARCH ALE
\\
\\		Add the contents of 'wid to the current search order.
\\		Only   used  for  the  execution  of  entries  created   by
\\		VOCABULARY.
\\
code (vocabulary)	( R: 'wid -- )
	push-tos
	movl	5 (%eax), tos
	jmp	X28set2Dfirst29 <cr>
end-code compilation

[else]

\\	(vocabulary)		( R: 'wid -- )			SEARCH ALE
\\
\\		Add the contents of 'wid to the current search order.
\\		Only   used  for  the  execution  of  entries  created   by
\\		VOCABULARY.
\\
code (vocabulary)
	popl	%eax
	push-tos
	movl	(%eax), tos
	jmp	X28set2Dfirst29 <cr>
end-code compilation

[then]

\\	<vocabulary>		( xt -- )			C	COMPILER
\\
\\		Inline  compiler  for  VOCABULARY,  appends  the  execution
\\		semantics of a VOCABULARY to the current definition.
\\
: <vocabulary>
  <constant> postpone (set-first)
; compilation

\\	(forget)		( xt -- )				COMPILER
\\
\\		FIXME: DOCO
\\
: (forget)
  ." FIXME: forget"
\	Goto through wordlist, restore last links
\	Goto through voc-link remove forgotten vocabularies.
\	Goto through wid-link remove forgotten wordlists.
\	What should we do with defered words.
;

forth definitions

\\	vocabulary		( "name" -- )			D	ALE
\\
\\		Parse   name   delimited   by  a  space,  ignoring  leading
\\		delimiters.  Create  a dictionary  entry  for name with the
\\		execution semantics defined below. name is referred to as a
\\		"vocabulary."
\\
\\	    name Execution:	( -- )
\\		Transform  the search order consisting of wid1, ... widn-1,
\\		widn  (where widn is searched first) into wid1, ... widn-1,
\\		widn, name-wordlist.
\\
\\	    Internals:
\\		create-header modified by does>
\\		cell	wid	Word list identifier
\\		cell	link	Vocabularies linked together
\\				link points to the body!
\\
: vocabulary
  wordlist header
  ['] (vocabulary) executer,
  ['] <vocabulary> compiler,
  here swap , voc-link @ , voc-link !	\ Link vocabularies together
;

\\	marker			( "name" -- )			D	CORE EXT
\\
\\		Parse   name   delimited   by  a  space,  ignoring  leading
\\		delimiters.  Create  a  dictionary  entry for name with the
\\		execution semantics defined below.
\\
\\	    name Execution:	( -- )
\\		Restore all dictionary allocation and search order pointers
\\		to the state they had just prior to the definition of name.
\\		Remove name and all subsequent word definitions.
\\		Restoration of any structures still existing that may refer
\\		to  deleted  definitions  or  deallocated data space is not
\\		necessarily provided.  No other contextual information such
\\		as numeric base is affected.
\\
: marker
  create
does>	( mark -- )
  body> (forget)
;

compiler definitions

\\	.vocabulary		( wid -- )			SEARCH ALE
\\
\\		Prints  the  name  of  a vocabulary identified  by  wid  if
\\		possible;  Otherwise print the wordlist address.
\\
: .vocabulary
  voc-link @
  begin
    2dup @ =
    if
      body> .name drop exit		\ print vocabularies name
    then
    cell+ @ ?dup 0=
  until
  .					\ address
;

forth definitions

\\	order			( -- )				SEARCH EXT
\\
\\		Identify the word lists in the search order in their search
\\		order sequence, from first searched to last searched.  Also
\\		identify  the  word list into which new definitions will be
\\		placed.  The  form of the identification is implementation-
\\		dependent.
\\	    Format:
\\		Context: <... wids in context>
\\		Current: <wid of current>
\\		Recognizer: <... name of recognizer>
\\
: order
  cr ." Context:"
  get-order 0
  ?do
    space .vocabulary
  loop
  cr ." Current:"
  get-current space .vocabulary
  cr ." Recognizer:"
  get-order 0
  ?do
    space
    cell+ @ ?dup
    if
      .name
    else
      ." <none>"
    then
  loop
;
