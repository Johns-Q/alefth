\
\	ALE Forth Version 2.04
\
\		Copyright (c) 1991,1992 by Lutz Sammer.
\
\		Bug-reports via usenet: johns
\
\	File:	ftc.4th
\
\		Forth target compiler.
\
\	RCS:
\	$Id: ftc.4th,v 1.8 1992/03/20 00:02:45 johns Exp johns $
\	$Log: ftc.4th,v $
\ Revision 1.8  1992/03/20  00:02:45  johns
\ Now use addl and subl insteed of leal for nest and unnest,
\ quicker on a 486.
\
\ Revision 1.7  1992/03/07  16:31:59  johns
\ Many changes, literals now supports more dictionary references.
\
\ Revision 1.6  1992/02/19  02:05:11  johns
\ Floating point support, internals documented, hex constants in code,
\ primitives,...
\

vocabulary target			\ Where the target words are placed
: 'compiler compiler ;
vocabulary compiler			\ Where the compiler words are placed
vocabulary hidden			\ Where the hidden words are placed
vocabulary assembler			\ Assembler vocabulary

only 'compiler
  also assembler also compiler definitions also hidden
also forth

\ -----------------------------------------------------------------------------
\	Compiler configuration
\ -----------------------------------------------------------------------------

		\	Set to 1, if you want to include compiler information.
value /verbose  1 to /verbose

		\	Set to 1, if you want to generate documentation.
value /document  0 to /document

		\	Set to 1, if you want to generate a view file.
value /view  0 to /view

		\	Set to 1, if you want minimal code.
value /less-code  0 to /less-code

		\	Set to 1, if you want to have emit, type, cr buffered.
value /buffered-output  1 to /buffered-output

		\	Set to 1, for processor stack = data stack.
value /ps=sp  1 to /ps=sp

		\	Set to 1, to use faster string literals
value /fast-string  1 to /fast-string

		\	Set to 1, to include the lookup cache
value /cache  1 to /cache

		\	Set to 1, to include block word set
value /block  1 to /block

		\	Set to 1, to include user words support
value /user  1 to /user

		\	Set to 1, to include floating point words
value /float  0 to /float

		\	Set to 1, to make the kernel stand alone
\ value /stand-alone  1 to /stand-alone

\ create target-msdos	\	Target DOMESDOS
create target-unix	\	Target unix
\ create target-i386	\	Target i386
create target-i486	\	Target i486

\ -----------------------------------------------------------------------------
\	More configurations:
\		#places	in float.4th
\		return stack size in startup.4th
\		dictionary size in coldbye.4th
\		#user in user.4th
\ -----------------------------------------------------------------------------

\ -----------------------------------------------------------------------------
\	Forth dependend
\ -----------------------------------------------------------------------------

' hidden >body @ set-current

\	>current		( "name" -- )
\
\		Make "name" the compilation word list.
: >current
  ' >body @ set-current
;

\	?number								ALE
\				( c-addr u -- c-addr u 0 )
\			-or-	( c-addr u -- n true )
\
\		Look  if  the string c-addr and u, can be converted  into a
\		number.  Returns  true and the number n, if it is a number;
\		otherwise false and the string c-addr and u.
\
: ?number
  0 0 2over
  over c@ [char] - = dup >r		\ Negative
  if
    1 /string
  then
  \ c-addr u ud0 c-addr' u'
  >number
  if					\ Input not complete converted
    r> 4 discard false exit
  then
  drop 2swap 2drop
  r>
  if
    dnegate
  then
  drop
  true
;

\	@name			( xt -- c-addr u )
\
\		Return the address and length of the name of xt.
\
: @name
  >name count
;

\	last@			( -- xt )
\
\		Same as: LAST @
: last@
  last @
;

>current compiler

\	exist			( "name" -- flag )
\
\		Returns true if name found in search order.
\
: exist
  bl word find nip 0<>
;

\ -----------------------------------------------------------------------------
\	Target tools
\ -----------------------------------------------------------------------------

>current hidden

: .:			( -- )		\ Emits :
  [char] : emit
;

: .,			( -- )		\ Emits ,
  ."  , "
;

: .-			( -- )		\ Emits -
  ."  - "
;

: .$			( -- )		\ Emits $
  ." $ "
;

: [compiler]		( "name" -- )	\ Searches next word in compiler
  compiler defined forth
  dup
  if
    0<
    if					\ Immediate: execute
      execute
    else				\ Normal: compile it
      compile,
    then
  else
    count type true abort" ?"		\ Not found
  then
; immediate

\ -----------------------------------------------------------------------------
\	Pseudo assembler ( sometime a real )
\ -----------------------------------------------------------------------------

\ Register usage:
\
\	%eax		Scratch register
\	%ebx		Top element of stack.
\	%ecx		Current loop index.
\	%edx		Scratch register
/ps=sp [if]
\	%esi		Return    stack pointer.
[else]
\	%esi		Parameter stack pointer.
[then]
/user [if]
\	%edi		User area pointer.
[else]
\	%edi		Not used ( Always preserved ).
[then]
\	%ebp		Not used ( Should become frame register ).
/ps=sp [if]
\	%esp		Parameter stack pointer.
[else]
\	%esp		Return    stack pointer.
[then]
\	cs: ds: es:	Same and used.
\	fs: gs:		Not used.

9	constant tab#

: tab		( -- )			\ Emit tab
  tab# emit
;

: .mnemonic	( "mne" -- )		\ Emit mnemonic
  create last@ ,
does>
  tab @ .name tab
;

: .mnemonic1	( "mne" -- )		\ Emit menmonic without operands
  create last@ ,
does>
  tab @ .name cr
;

: .1		( "op1" -- )		\ Emit first operand
  create last@ ,
does>
  @ .name space
;

: .2		( "op2" -- )		\ Emit second operand
  create last@ ,
does>
  @ .name cr
;

>current assembler

.mnemonic	.align			\ Pseudo opcodes
.mnemonic	.ascii
.mnemonic	.byte
.mnemonic	.globl
.mnemonic	.long
.mnemonic	.set
.mnemonic	.space
.mnemonic	.word

.mnemonic1	cld
.mnemonic1	cltd
.mnemonic1	cmc
.mnemonic1	cwtl
.mnemonic1	lahf
.mnemonic1	hlt
.mnemonic1	pushf
.mnemonic1	rep
.mnemonic1	repe
.mnemonic1	repne
.mnemonic1	ret
.mnemonic1	sahf
.mnemonic1	std

.mnemonic	adcl
.mnemonic	addb
.mnemonic	addl
.mnemonic	andb
.mnemonic	andw
.mnemonic	andl
.mnemonic	call
.mnemonic	cmpb
.mnemonic	cmpw
.mnemonic	cmpl
.mnemonic	cmpsb
.mnemonic	decl
.mnemonic	divl
.mnemonic	idivl
.mnemonic	imull
.mnemonic	incl
.mnemonic	int
.mnemonic	ja
.mnemonic	jb			\ Unsigned
.mnemonic	jbe			\ Unsigned
.mnemonic	jc
.mnemonic	je
.mnemonic	jl			\ Signed
.mnemonic	jle			\ Signed
.mnemonic	jg			\ Signed
.mnemonic	jge			\ Signed
.mnemonic	jmp
.mnemonic	jna
.mnemonic	jnb			\ Unsigned
.mnemonic	jnc
.mnemonic	jne
.mnemonic	jnl
.mnemonic	jng
.mnemonic	jno
.mnemonic	jnp
.mnemonic	jnz
.mnemonic	jns
.mnemonic	jo
.mnemonic	jp
.mnemonic	js
.mnemonic	jz
.mnemonic	lcall
.mnemonic	leal
.mnemonic	lodsb
.mnemonic	lodsl
.mnemonic	movb
.mnemonic	movw
.mnemonic	movl
.mnemonic	movsb
.mnemonic	movsbl
.mnemonic	movswl
.mnemonic	movsl
.mnemonic	movzbl
.mnemonic	movzwl
.mnemonic	mull
.mnemonic	negl
.mnemonic	notl
.mnemonic	orb
.mnemonic	orw
.mnemonic	orl
.mnemonic	popl
.mnemonic	pushl
.mnemonic	rcll
.mnemonic	rcrl
.mnemonic	sbbl
.mnemonic	sard
.mnemonic	sarl
.mnemonic	scasb
.mnemonic	seta			\ Unsigned
.mnemonic	setb			\ Unsigned
.mnemonic	sete
.mnemonic	setg			\ Signed
.mnemonic	setl			\ Signed
.mnemonic	setna			\ Unsigned
.mnemonic	setnb			\ Unsigned
.mnemonic	setne
.mnemonic	setng			\ Signed
.mnemonic	setnl			\ Signed
.mnemonic	setnz
.mnemonic	seto			\ Overflow
.mnemonic	sets			\ Sign/Negative
.mnemonic	setz
.mnemonic	shld
.mnemonic	shll
.mnemonic	shrd
.mnemonic	shrl
.mnemonic	stosb
.mnemonic	stosw
.mnemonic	stosl
.mnemonic	subb
.mnemonic	subw
.mnemonic	subl
.mnemonic	testb
.mnemonic	testl
.mnemonic	xchgl
.mnemonic	xlatb
.mnemonic	xorb
.mnemonic	xorl

.1		$			\ immediate
.1		,			\ next operand
.1		.			\ current address
.1		.+8
.1		.+13
.1		buffer
.1		sel_buf
.1		Dsp0
.1		Dbase
.1		Dstate
.1		Dcurrent
.1		Dlast
.1		Dconv2Ditab
.1		Dbrk2Dval
.1		Dconv2Dhitab
.1		X28set2Dfirst29
.1		X3Cdoes3E3E
.1		Drun2Dtable+4

.1		%al,
.1		%cl,
.1		%dl,
.1		%ah,
.1		%ch,
.1		%dh,

.1		%ax,
.1		%cx,
.1		%dx,
.1		%di,
.1		%si,
.1		%bp,
.1		%sp,

.1		%eax,
.1		%ecx,
.1		%edx,
.1		%edi,
.1		%esi,
.1		%ebp,
.1		%esp,

.1		(%eax,
.1		(%ecx,
.1		(%edx,
.1		(%edi,
.1		(%esi,
.1		(%ebp,
.1		(%esp,

.1		%eax),
.1		%ecx),
.1		%edx),
.1		%edi),
.1		%esi),
.1		%ebp),
.1		%esp),

.1		(%eax),
.1		(%ecx),
.1		(%edx),
.1		(%edi),
.1		(%esi),
.1		(%ebp),
.1		(%esp),

.1		(,

.1		1),
.1		2),
.1		4),
.1		8),

.2		1)
.2		2)
.2		4)
.2		8)

.2		%al
.2		%cl
.2		%dl
.2		%ah
.2		%ch
.2		%dh

.2		%ax
.2		%cx
.2		%dx
.2		%di
.2		%si
.2		%bp
.2		%sp

.2		%eax
.2		%ecx
.2		%edx
.2		%edi
.2		%esi
.2		%ebp
.2		%esp

.2		%eax)
.2		%ecx)
.2		%edx)
.2		%edi)
.2		%esi)
.2		%ebp)
.2		%esp)

.2		(%eax)
.2		(%ecx)
.2		(%edx)
.2		(%edi)
.2		(%esi)
.2		(%ebp)
.2		(%esp)

.2		*(%esi)

.2		0b
.2		1b
.2		2b
.2		0f
.2		1f
.2		2f
.2		0:
.2		1:
.2		2:

\ -----------------------------------------------------------------------------
\	Floating point
\ -----------------------------------------------------------------------------

/float [if]

.mnemonic1	fdecstp
.mnemonic1	finit
.mnemonic1	fclex
.mnemonic1	fstsw
.mnemonic1	fninit
.mnemonic1	fnclex
.mnemonic1	fnstsw
.mnemonic1	fincstp

.mnemonic1	fldz
.mnemonic1	fld1
.mnemonic1	fldpi
.mnemonic1	fldl2e
.mnemonic1	fldl2t
.mnemonic1	fldlg2
.mnemonic1	fldln2

.mnemonic1	fabs
.mnemonic1	fchs
.mnemonic1	fscale
.mnemonic1	frndint
.mnemonic1	fyl2x
.mnemonic1	fyl2xp1
.mnemonic1	f2xm1
.mnemonic1	fxam
.mnemonic1	fxtract
.mnemonic1	ftst
.mnemonic1	fsqrt
.mnemonic1	fcos
.mnemonic1	fptan
.mnemonic1	fpatan
.mnemonic1	fsin
.mnemonic1	fsincos
.mnemonic1	fcompp

.mnemonic	fldcw
.mnemonic	fstcw
.mnemonic	fnstcw

.mnemonic	filds
.mnemonic	fildl
.mnemonic	fildq
.mnemonic	fldl
.mnemonic	flds
.mnemonic	fldt
.mnemonic	fbld
.mnemonic	fbstp
.mnemonic	fistps
.mnemonic	fistpl
.mnemonic	fistpll
.mnemonic	fistpq
.mnemonic	fstpl
.mnemonic	fstpt
.mnemonic	fstps
.mnemonic	fsave
.mnemonic	fnsave
.mnemonic	frstor

.mnemonic	ffree
.mnemonic	fxch

.mnemonic	fadd
.mnemonic	faddp
.mnemonic	fcoml
.mnemonic	fcomp
.mnemonic	fdiv
.mnemonic	fdivp
.mnemonic	fdivr
.mnemonic	fdivrp
.mnemonic	fmul
.mnemonic	fmulp
.mnemonic	fsub
.mnemonic	fsubl
.mnemonic	fsubp
.mnemonic	fsubr
.mnemonic	fsubrp

.1		%st(0),
.1		%st(1),
.1		%st(2),
.1		%st(3),
.1		%st(4),
.1		%st(5),
.1		%st(6),
.1		%st(7),

.2		%st(0)
.2		%st(1)
.2		%st(2)
.2		%st(3)
.2		%st(4)
.2		%st(5)
.2		%st(6)
.2		%st(7)

[then]

\ -----------------------------------------------------------------------------
\	Assembler macros
\ -----------------------------------------------------------------------------

: tos ." %ebx" cr ;
: tos, ." %ebx, " ;
: tos-w ." %bx" cr ;
: tos-l ." %bl" cr ;
: tos-l, ." %bl," ;
: tos-h ." %bh" cr ;
: tos-h, ." %bh," cr ;
: (tos, ." (%ebx," ;
: tos), ." %ebx)," ;
: (tos) ." (%ebx)" cr ;
: (tos), ." (%ebx), " ;

/ps=sp [if]

: push-tos
  pushl	tos
;

: pop-tos
  popl	tos
;

[else]

: dec-csp
  tab ." leal	-4 (%esi), %esi" cr
;

: inc-csp
  tab ." leal	4 (%esi), %esi" cr
;

: push-tos
  dec-csp
  tab ." movl	%ebx, (%esi)" cr
;

: pop-tos
  tab ." movl	(%esi), %ebx" cr
  inc-csp
;

[then]

: <cr>					\ To force a newline
  cr
;

: .u			( u -- )	\ Emits number without space
  (u.) type
;

\ -----------------------------------------------------------------------------
\	Assembler pseudo opcodes.
\ -----------------------------------------------------------------------------

>current hidden

0 constant DATA				\ Last section was data segment
1 constant TEXT				\ Last section was text segment
value section  -1 to section		\ Current output section

>current assembler

: .data		( -- )			\ Data section
  section DATA -
  if
    tab ." .data" cr
    DATA to section
  then
;

: .text		( -- )			\ Text section
  section TEXT -
  if
    tab ." .text" cr
    TEXT to section
  then
;

>current hidden

: .byte-val	( u -- )
  .byte .u cr
;

: .word-val	( u -- )
  .word .u cr
;

: .long-val	( u -- )
  .long .u cr
;

: ascii-type	( c-addr u -- )
  bounds
  do
    i c@ dup [char] " =
    if
      [char] \ emit emit		\ Quote "
    else
      dup [char] \ =
      if
	[char] \ emit emit		\ Quote \
      else
	emit				\ Character
      then
    then
  loop
;

: .ascii-val	( c-addr u -- )
  .ascii [char] " emit ascii-type [char] " emit cr
;

\ -----------------------------------------------------------------------------
\	Assembler labels
\ -----------------------------------------------------------------------------

\	.label-char		( char -- )
\
\		Emit a char of the label.
\
: .label-char
  dup  [char] a [char] { within 0=
  over [char] A [char] [ within 0= and
  over [char] 0 [char] : within 0= and
  if					\ Emit it as hex
    0 <# # # #> type
  else
    emit				\ a-z A-Z 0-9
  then
;

\	.label			( c-addr u -- )
\
\		Emit string as label.
\
: .label
  base @ >r hex
  bounds
  do
    i c@ .label-char
  loop
  r> base !
;

\	.@label			( xt -- )
\
\		Emit name of xt as label.
\
: .@label
  @name .label
;

\	.Cname			( xt -- )
\
\		Emit name of xt as Cname.
\		Cxx are forward references of the length of primitives.
\
: .Cname
  [char] C emit .@label
;

\	.Dname			( xt -- )
\
\		Emit name of xt as Dname.
\		Dxx are labels of the data field.
\
: .Dname
  [char] D emit .@label
;

\	.Ename			( xt -- )
\
\		Emit name of xt as Ename.
\		Exx are labels of the link field.
\
: .Ename	( xt -- )
  [char] E emit .@label
;

\	.Xname			( xt -- )
\
\		Emit name of xt as Fname.
\		Fxx are labels of the code field.
\
: .Xname	( xt -- )
  [char] X emit .@label
;

\	.Nname			( xt -- )
\
\		Emit name of xt as Nname.
\		Nxx are labels of the name field.
\
: .Nname	( xt -- )
  [char] N emit .@label
;

\	.Pname			( xt -- )
\
\		Emit name of xt as Pname.
\		Pxx are labels used for the primitives length calculation.
\
: .Pname	( xt -- )
  [char] P emit .@label
;

\	.Tname			( xt -- )
\
\		Emit name of xt as Tname.
\		Txx are labels used as forward reference of the flag field.
\
: .Tname	( xt -- )
  [char] T emit .@label
;

\	.Vname			( xt -- )
\
\		Emit name of xt as Vname.
\		Vxx are labels used as forward reference of the data field.
\
: .Vname	( xt -- )
  [char] V emit .@label
;

/ps=sp [if]

: target-executer,
  jmp [char] X emit .label cr
;

: "call,	( c-addr u )
\  ." 	movl	$ .+11 , (%esi)" cr
\  ." 	jmp	X" .label cr
  ." 	movl	$ 0f , (%esi)" cr
  ." 	jmp	X" .label cr
  ." 0:" cr
;

: target-nest,
\  leal	-4 . (%esi), %esi
  subl	." $4, " %esi
;

: target-unnest,
\  leal	4 . (%esi), %esi
  addl	." $4, " %esi
;

: target-ret,
  jmp	*(%esi)
;

[else]

: target-executer,
  call [char] X emit .label cr
;

: "call,	( c-addr u )
  call [char] X emit .label cr
;

: target-nest,
  ( done by call/ret )
; immediate

: target-unnest,
  ( done by call/ret )
; immediate

: target-ret,
  ret
;

[then]

: target-call,	( xt -- )
  @name "call,
;

\ -----------------------------------------------------------------------------
\	Startup generation
\ -----------------------------------------------------------------------------

>current compiler

\	startup-code:		( "startup-code;" -- )			I
\
\		Starts startup-code generation.
\		Ends with STARTUP-CODE;
\
: startup-code:
\ ASSEMBLER-FIX: If we have a own assembler we must change this
\ assembler
  begin
    refill 0= abort" unexpected end-of-file in 'startup-code:'"
    bl word count dup
    if
      s" startup-code;" compare		\ Look for end of startup code
      0=if
	exit
      then
    else
      2drop
    then
    tib #tib @ type cr			\ Emits lines between s-c: and s-c;
  again
; immediate

\ -----------------------------------------------------------------------------
\	Documentation generation
\ -----------------------------------------------------------------------------

>current hidden

/document [if]				\ Documentation wanted

value doc-fid		( -- fileid )	\ Documentation file-id

: open-document		( -- )		\ Open documentation file
  s" fth.doc" r/w create-file abort" Can't open 'fth.doc'"
  to doc-fid
;

: close-document	( -- )		\ Close documentation file
  doc-fid close-file abort" close-file of 'fth.doc'"
;

>current compiler

: \\			( -- )		\ Documentation line
  tib #tib @ >in @ /string
  doc-fid write-line abort" write-line to 'fth.doc'"
  #tib @ >in !
; immediate


[else]					\ No doc wanted

: open-document ; immediate
: close-document ; immediate

>current compiler

: \\ [compile] \ ; immediate

[then]

\ -----------------------------------------------------------------------------
\	View file generation
\ -----------------------------------------------------------------------------

>current hidden

/view [if]				\ View file wanted

value view-fid		( -- fileid )	\ View file file-id

: open-view		( -- )		\ Open view file
  s" view.fth" r/w create-file abort" Can't open 'view.fth'"
  to view-fid
;

: close-view		( -- )		\ Close view file
  view-fid close-file abort" close-file of 'view.fth'"
;

: ident-view		( len to -- )	\ Indent view field
  swap - 1 max
  blanks over 5 >>
  0
  ?do
    dup 32 view-fid write-file throw
  loop
  swap 31 and view-fid write-file throw
;

: create-view		( "name" -- )	\ Record creation
  last@ @name tuck
    view-fid write-file throw
  16 ident-view
  source-file >file ->file-name count tuck
    view-fid write-file throw
  14 ident-view
  source-file >file ->file-line @ (.)
    view-fid write-file throw
  0 0 view-fid write-line throw
;

[else]

: open-view ; immediate
: close-view ; immediate
: create-view ; immediate

[then]

\ -----------------------------------------------------------------------------
\	Dictionary structure
\ -----------------------------------------------------------------------------

\	Internals:
\
\	Structure of a dictionary entry:
\
\	Eyyy:
\
\		.text			\ Readonly kernel entry
\	-or-
\		.data			\ Writeable kernel entry
\
\	Nxxx:	.byte	length		\ Name - field
\		.ascii	"xxx"
\		.align	2		\ Long word aligned
\	Exxx:	.long	Eyyy		\ Link - field
\		.long	Xccc		\ Compiler field
\		.byte	Txxx		\ Flag - field
\		.byte	Exxx-Nxxx	\ Offset to name
\		.byte	Cxxx		\ Length of a primitive
\	Fxxx:
\		....			\ Code - field
\	Pxxx:	ret			\ End of a primitive
\	Dxxx:
\		....			\ Data - field
\
\		.set	Cxxx,Pxxx-Fxxx	\ Resolve length of a primitive
\		.set	Txxx,0		\ Resolve flags of the entry
\
\	Flag - field:
\	        xxxxPECI
\			I	Forth immediate word
\			C	Compilation word only visibile if compiling
\			E	Execution word only visibile if interpreting
\			P	Privat word only visibile in definition vocab.
\			CE	Hidden word never visibile
\
\	LAST points to last created word.
\	LAST is zero if last definitions was :NONAME.
\

value comp-csp				\ Compiler security
value comp-last				\ Last compiled entry
value comp-current			\ Generator current
create temp  0 , temp to comp-current	\ Temp (first) current
variable comp-context			\ Generator context
value comp-voc-link			\ Links vocabularies together

value rom-flag  0 to rom-flag		\ 1 if next create should be in rom

variable wid  0 wid !			\ Number of wordlist created
32 constant #wid			\ Compiler supports 32 wordlists
create wid-table #wid 2* cells allot	\ Compiler wordlist table
  wid-table #wid 2* cells erase

\	Resolve assembler header flag-field
\
: resolve-flag
  comp-last ?dup
  if
    dup comp-current @ 2* cells wid-table + !
    dup .set .Tname ., >body cell+ @ .u cr
  then
;

\	Emit assembler header start
\
: start-header	( <xt> -- xt )
  resolve-flag cr

  create-view				\ Add Entry to view file

  rom-flag
  if
    .text 0 to rom-flag
  else
    .data
  then
  last@

  dup .Nname .:				\ Name - field
  dup @name dup .byte-val .ascii-val
  \ .align 2 .u cr
  .align 4 .u cr

  dup .Ename .:				\ Link - field
  comp-last dup
  .long  if  .Ename  else  .u  then  cr

  swap .long .Xname cr			\ Compiler - field

  dup .byte .Tname cr			\ Flag - field

  dup .byte .Ename .- dup .Nname cr	\ Offset to name
;

\	Emit assembler code-field label
\
: code-field		( -- )
  last@ dup .globl .Xname cr
  .Xname .: cr
;

\	Emit assembler data-field label
\
: data-field
  last@ dup .globl .Dname cr
  .Dname .: cr
;

\ -----------------------------------------------------------------------------
\	Target Compiler compile, literal
\ -----------------------------------------------------------------------------

\	target-compile,
\
\		Target compiler compile,
\
: target-compile,	( xt -- )
  /verbose
  if
  ." / " dup .name cr
  then
  dup >body @ execute
;

\	target-literal
\
\		Target compiler literal
\
: target-literal
  /verbose
  if
  ." / " dup . cr
  then
  push-tos
  ?dup
  if
    movl .$ . ." , " tos
  else
    xorl tos, tos			\ Optimize 0
  then
;

\ -----------------------------------------------------------------------------
\	Compiler defining words
\ -----------------------------------------------------------------------------

0	constant T_NORMAL		\ Normal word
1	constant T_IMMEDIATE		\ Immediate word
2	constant T_COMPILATION		\ Compilation only word
4	constant T_EXECUTION		\ Execution only word
6	constant T_HIDDEN		\ Hidden for definition

value use-wordlist  0 to use-wordlist	\ Modifies the behavior of constant
					\ for wordlists

value use-reference  0 to use-reference	\ Modifies the behavior of constant
					\ for assembler labels

\	.reference	( n -- )
\
\		Handle code references.
\
: .reference
  use-reference				\ On stack reference
  if
    use-reference .Xname		\ Code reference
    nil to use-reference
    dup
    if
      ." +" . exit			\ Add the constant
    then
    drop
  else					\ Normal constant
    u.
  then
;

\	literal-reference	( n -- )
\
\		Compile a code reference literal.
\
: literal-reference
  push-tos
  movl .$ .reference ., tos
;

\	reference,		( n -- )
\
\		.long	reference
\
: reference,
  .long .reference cr
;

\	Inline compiler of :
\
: <:>
  target-call,
;

\	Compiler forward	( "name" -- )				D
\
\		Generates a forward reference.
\
: forward
  create ['] <:> , T_NORMAL , compilation
does>
  ." unresolved forward reference"
;

>current compiler

\	Compiler directive to force next word too be created into rom.
\
: rom		( -- )
  1 to rom-flag
;

\	Compiler header
\
: target-header	( <xt> -- )
  start-header
  0 .byte-val				\ Rest of Flag - field
  to comp-last
  code-field
;

\	Inline compiler of create
\
: target-<create>	( xt -- )
  push-tos
  movl .$ .Dname ., tos
;

forward <create>

\	Compiler create
\
: create	( "name" -- )
  create ['] target-<create> , T_NORMAL ,
  ['] <create> target-header
  s" (create)" target-executer,
  data-field
does>
  body> to use-reference 0 >body exit
; execution

\	Inline compiler of variable
\
: target-<variable>	( xt -- )
  push-tos
  movl .$ .Dname ., tos
;

\	Compiler variable
\
: variable	( "name" -- )
  create ['] target-<variable> , T_NORMAL , 0 ,
  ['] <variable> target-header
  s" (variable)" target-executer,
  data-field
  last@ @name s" voc-link" compare	\ Special voc-link handling
  if
    last@ @name s" wid-link" compare	\ Special wid-link handling
    if
      0 .long-val
    else
      .long ." WIDLINK" cr
    then
  else
    .long ." VOCLINK" cr
  then
does>
  true abort" variable"
; execution

\	Inline compiler of constant
\
: <constant>	( xt -- )
  >body 2 cells + 2@
  dup to use-reference
  if					\ Code reference
    literal-reference
  else
    target-literal
  then
;

\	Inline compiler of constant of wids
\
: <wid-constant>	( xt -- )
  >body 2 cells + @
  push-tos
  movl ." $w" .u ., tos
;

\	Compiler constant
\
\		Modified to handle wordlists,
\		Modified to handle dictionary references,
\
: constant	( val "name" -- )
  create
  rom ['] <constant> target-header
  s" (constant)" target-executer,
  data-field

  dup					\ val
  use-wordlist
  if					\ On stack wid
    ['] <wid-constant> , T_NORMAL , 0 , ,
    .long [char] w emit .u cr		\ Wordlist reference
    0 to use-wordlist
  else
    ['] <constant> , T_NORMAL ,
    use-reference , ,			\ Label if any and constant
    reference,
  then
does>
  2 cells + 2@ to use-reference		\ Fetch the constant
; execution

\	Inline compiler of field
\
: <field>	( xt -- )
  >body 2 cells + @
  addl	.$ .u ., tos
;

\	Compiler field
\
: field	( n "name" -- )
  create ['] <field> , T_NORMAL , dup ,
  rom ['] <field> target-header
  s" (field)" target-executer,
  data-field
  .long-val
does>
  2 cells + @ +				\ Fetch the constant and add it.
; execution

\	Inline compiler of value
\
: <value>	( xt -- )
  push-tos
  movl .Dname ., tos
;

\	Compiler value
\
: value	( "name" -- )
  create ['] <value> , T_NORMAL ,
  ['] <value> target-header
  s" (value)" target-executer,
  data-field
  last@ dup
  .long .Vname cr			\ Contents of the value
  .set	.Vname ., ." 0" cr		\ Set default contents
does>
  true abort" value"
; execution

\	Compiler :
\
: :	( "name" -- )
  depth to comp-csp
  create ['] <:> , T_NORMAL ,
  rom ['] <:> target-header
  target-nest,
  1 state !			\ Start compiler
does>
  body> .name true abort" colon"
; execution

\	Compiler exit
\
: exit
  /verbose
  if
    ." / exit" cr
  then
  target-unnest, target-ret,
; immediate

\	Compiler ;
\
: ;
  ( postpone exit ->) target-unnest, target-ret,
  depth comp-csp <> abort" Security"
  0 state !			\ Stop compiler
; immediate

\ -----------------------------------------------------------------------------
\	Code definitions
\ -----------------------------------------------------------------------------

\	Compiler expand a primitive
\
: <primitive>	( xt -- )
  >body cell+
  begin
    cell+ dup @ ?dup
  while
    execute
  repeat
  drop
;

\	Compiler primitive header
\
: primitive-header
  ['] <primitive> start-header
  .byte	dup .Cname cr			\ Rest of Flag - field
  to comp-last
  code-field
;

\	Compiler primitive end
\
: primitive-end	( -- )
  comp-last dup .Pname .: cr
  target-ret,
  .set dup .Cname ., dup .Pname .- .Xname cr
;

\	Primitive number
\
: (p.)		( addr1 -- addr2 )
  cell+ dup ?
; compilation

\	Compile primitive
\
: compile-primitive
  begin
    bl word dup c@			\ Scan input get next word
    if
      find dup
      if
	1 =				\ Non immediate
	if
	  dup ,				\ Lay down the token
	then
	execute				\ execute it
      else
	drop count ?number
	0=if
	  over c@ [char] $ =
	  if
	    base @ >r hex 1 /string ?number r> base !
	    0=if
	      -1 /string type true abort" ?$:cp"
	    then
	  else
	    type true abort" ?:cp"
	  then
	then
	dup . ['] (p.) , ,
      then
    else
      drop
      refill 0= abort" EOF unexpected"	\ Get one line from input
    then
  again
;

\	Compiler primitive
\
: primitive	( "name" -- )
  create ['] <primitive> , T_NORMAL ,
  rom primitive-header
  assembler
  compile-primitive
  compiler
does>
  true abort" use of primitve"
;

\	End a primitive word
\
: end-primitive	( -- )
  primitive-end
  0 ,					\ Terminate primitive
  r> drop				\ Exits compile-primitive
; immediate

\	Compile code
\
: compile-code
  begin
    bl word dup c@			\ Scan input get next word
    if
      find
      if
	execute				\ execute it
      else
	count ?number
	0=if
	  over c@ [char] $ =
	  if
	    base @ >r hex 1 /string ?number r> base !
	    0=if
	      -1 /string type true abort" ?$:cc"
	    then
	  else
	    type true abort" ?:cc"
	  then
	then
	.
      then
    else
      drop
      refill 0= abort" EOF unexpected"	\ Get one line from input
    then
  again
;

\	Inline expansion of code
\
: <code>	( xt -- )
  target-call,
;

forward <execute>

\	Compiler code
\
: code	( "name" -- )
  create ['] <code> , T_NORMAL ,
  last@ @name s" execute" compare	\ Special execute handling
  if
    rom ['] <code> target-header
  else
    rom ['] <execute> target-header
  then
  assembler compile-code compiler
;

\	Compiler end-code
\
: end-code	( -- )
  target-ret,
  r> drop				\ Exits compile-code
; immediate

\	Compiler inline code
\
: inline
  [compile] [
  assembler compile-code compiler
; compilation immediate

\	Compiler end-inline
\
: end-inline
  r> drop ]				\ Exits compile-code reenter compiler
; immediate

: |type
  >r last@ >body cell+ dup @ r> or swap !
;

: immediate	T_IMMEDIATE	|type ;
: compilation	T_COMPILATION	|type ;
: execution	T_EXECUTION	|type ;
: hidden	T_HIDDEN	|type ;

\ -----------------------------------------------------------------------------
\	Compiler support for user words
\ -----------------------------------------------------------------------------

/user [if]
\ FIXME:
\	user definitions
\	create paul
\	defer emit  ' $emit is emit
\	forth definitions

\	Target compiler user vocabulary
\
\ vocabulary user  user definitions also forth

variable target->user			\ Current user offset
1000 target->user !

\	Allocate space in user-area.
\
: user-allot		( n -- )
  target->user +!
; execution

\	Inline compiler of user create.
\
: <user-create>	( xt -- )
  >body 2 cells + @
  push-tos
  leal . (%edi), tos			\ Load address
;

\	Compiler user create.
\
: user-create		( "name" -- )
  target->user @
  create ['] <user-create> , T_NORMAL , dup ,
  rom ['] <user-create> target-header
  s" (user-create)" target-executer,
  data-field
  .long-val
  cell target->user +!
does>
  true abort" user-create"
; execution

\	Inline compiler of user variable
\
: <user-variable>	( xt -- )
  >body 2 cells + @
  push-tos
  leal . (%edi), tos			\ Load address
;

\	Compiler user variable
\
: user-variable		( "name" -- )
  target->user @
  create ['] <user-variable> , T_NORMAL , dup ,
  rom ['] <user-variable> target-header
  s" (user-variable)" target-executer,
  data-field
  .long-val
  cell target->user +!
does>
  true abort" user-variable"
; execution

\	Inline compiler of user value
\
: <user-value>
  >body 2 cells + @
  push-tos
  movl . (%edi), tos			\ Load value
;

\	Compiler user value
\
: user-value		( "name" -- )
  target->user @
  create ['] <user-value> , T_NORMAL , dup ,
  rom ['] <user-value> target-header
  s" (user-value)" target-executer,
  data-field
  .long-val
  cell target->user +!
does>
  true abort" user-variable"
; execution

\	Inline compiler of user defer
\
: <user-defer>	( xt -- )
  >body 2 cells + @
  ."	movl	$ 0f , (%esi)" cr
  ."	jmp	* " . (%edi)
  0:
;

\	Compiler user defer
\
: user-defer	( "name" -- )
  target->user @
  create ['] <user-defer> , T_NORMAL , dup ,
  rom ['] <user-defer> target-header
  s" (user-defer)" target-executer,
  data-field
  .long-val
  cell target->user +!
does>
  true abort" user-defer"
; execution

false [if]
: is		( "name" -- )
  defined 0= abort" ?"
  /verbose
  if
    ." / is " dup .name cr
  then
  dup >body @ ['] <user-defer> <>
  abort" not defered"

  >body 2 cells + @
  movl	tos, . (%edi)
  pop-tos

; immediate
[then]

\ compiler definitions previous forth

[then]

\ -----------------------------------------------------------------------------
\	Additional definitions
\ -----------------------------------------------------------------------------

\	Inline compiler of defer
\
: target-<defer>	( xt -- )
  movl	.$ ." .+12" ., (%esi)
  jmp	." * " .Dname cr
; compilation

\	Compiler defer
\
: defer
  create ['] target-<defer> , T_NORMAL ,
  \ FIXME:
  \ ['] <defer> target-header
  \ s" (defer)" target-executer,
  ['] <create> target-header
  s" (create)" target-executer,
  data-field
  last@ dup
  .long .Vname cr			\ Execution vector
  .set	.Vname ., ." 0" cr		\ Set default execution vector
does>
  true abort" defer"
; execution

\	Compiler is
\
: is
  defined 0= abort" ?"
  /verbose
  if
    ." / is " dup .name cr
  then
  dup >body @ ['] target-<defer> =
  if
    state @
    if
      movl	tos, .Dname cr
      pop-tos
    else
      .set	.Vname ., .reference cr
    then
    exit
  then
  [ /user ] [if]
  dup >body @ ['] <user-defer> =
  if					\ USER VALUE
    state @
    if
      >body 2 cells + @
      movl	tos, . (%edi)
      pop-tos
    else
      .name true abort" to on user defer while interpreting"
      \ .set	.Vname ., .reference cr
    then
    exit
  then
  [then]
  .name true abort" not defered"
; immediate

\ -----------------------------------------------------------------------------
\	Compiler support for calls to c-library.
\ -----------------------------------------------------------------------------

/ps=sp [if]

\	Inline expansion of c-function.
\
: <c>	( xt -- )
  target-call,
;

\	Declare c-function	( n "name" -- )
\
: c-function
  create ['] <c> , T_NORMAL ,
  rom ['] <:> target-header
  push-tos
  movl	%ecx, tos			\ C scratch register: %eax %ecx %edx
  call	last@ @name type cr
  dup
  if					\ Has arguments
    leal cells . (%esp), %esp
  else
    drop
  then
  movl	tos, %ecx
					\ C return register: %eax      1 cell
					\		     %eax/%edx 2 cells
					\		     %st(0)    float
  movl	%eax, tos			\ return value
  target-ret,
;

\	Inline compiler of C - variable
\
: <c-variable>	( xt -- )
  push-tos
  movl .$ .name ., tos
;

\	Declare c-variable	( n "name" -- )
\
: c-variable
  create ['] <c-variable> , T_NORMAL ,
  rom primitive-header
  push-tos
  movl .$ last@ .name ., tos		\ Fetch contents
  primitive-end
;

[then]

\ -----------------------------------------------------------------------------
\	Compiler Vocabulary/Wordlist
\ -----------------------------------------------------------------------------

\	Internals:
\
\	Structure of a wordlist:
\
\		cell	Pointer to link-field of last created word
\		cell	Recognizer of the wordlist executed if a word
\			not found (See INTERPRET)
\		cell	Link to previous created wordlist
\			WID-LINK points to last created wordlist.
\
\	Structure of a vocabulary:
\
\		cell	Wordlist identifier
\		cell	Link to previous created vocabulary.
\			VOC-LINK points to last created vocabulary.
\

\	Compiler wordlist
\
: .wordlist	( -- wid )
  /verbose
  if
    ." / wordlist" cr
  then
  .data
  wid @ dup #wid = abort" Too many wordlists"
  wid 1+!

  dup [char] w emit .u .: cr
  dup .long [char] l emit .u cr		\ Last defined word in wordlist
  dup .long [char] r emit .u cr		\ Literal recognizer of wordlist
  .long
  dup
  if					\ Not first wordlist
    [char] w emit dup 1- .u		\ Make wordlist link
  else
    [char] 0 emit
  then
  cr
;

: resolve-wid	( -- )
  comp-voc-link if
    cr cr
					  \ resolve voc-link
    .set ." VOCLINK, " comp-voc-link .Dname cr

    wid @				\ resolve wordlists
    dup 1- .set ." WIDLINK, w" .u cr
    0
    ?do
      .set [char] l emit i .u .,
      i 2* cells wid-table + @ dup
      if  .Ename  else  .  then		\ Last entry of wordlist
      cr
      .set [char] r emit i .u .,
      i 2* cells cell+ wid-table + @ dup
      if  .Xname  else  .  then		\ Recognizer of wordlist
      cr
    loop
  then
;

\	Compiler wordlist for execution mode.
\
: wordlist
  .wordlist				\ Create wordlist
  1 to use-wordlist			\ Modify constants ...
; execution

\	Inline compiler of vocabulary
\
: <vocabulary>	( xt -- )
  push-tos
  movl .Dname ., tos
  s" (set-first)" "call,
;

\	Build a vocabulary
\
: build		( wid -- )
  create ['] <vocabulary> , T_NORMAL , dup , comp-voc-link ,
  ['] <vocabulary> target-header
  s" (vocabulary)" target-executer,

  data-field
  .long [char] w emit .u cr		\ wordlist
  .long comp-voc-link			\ voc-link
  dup  if  .Dname else .u  then  cr
  last@ to comp-voc-link
does>
  2 cells + comp-context !		\ To wordlist
;

\	Compiler vocabulary
\
: vocabulary	( -- )
  .wordlist build
;

\	Compiler definitions
\
: definitions	( -- )
  resolve-flag
  comp-context @ dup to comp-current
  @ 2* cells wid-table + @ to comp-last
; execution

\	Compiler also
\
\	FIXME: Not supported
: also		( -- )
; execution

\	Compiler previous
\
\	FIXME: Not supported
: previous	( -- )
; execution

\	Compiler only
\
\	FIXME: Not supported
: only		( -- )
; execution

\		Compiler recognizer
\
: recognizer	( -- )
  comp-last comp-current @ 2* cells cell+ wid-table + !
; execution

\ -----------------------------------------------------------------------------
\	Compile conditionals
\ -----------------------------------------------------------------------------

  1	constant ELSE
  2	constant THEN

  4	constant AGAIN
  8	constant UNTIL
 16	constant WHILE
 32	constant REPEAT

  64	constant LOOP
 128	constant +LOOP

 256	constant OF
 512	constant ENDOF
1024	constant ENDCASE

value labels	   0 to labels
value leave-label  0 to leave-label

: .Llabel:
  base @ swap decimal
  [char] L emit .u .: cr
  base !
;

: .Llabel
  base @ swap decimal
  [char] L emit .u cr
  base !
;

: new-label
  labels dup 1+ to labels
;

\	Init code for do - [+]loop
\
/ps=sp [if]

: -do-code
	movl	%ecx, (%esi)		\ Save loop index
	movl	tos, %ecx
	popl	%eax
	movl	%eax, -4 . (%esi)	\ >r limit
	popl	tos
	leal	-8 . (%esi), %esi
;

: do-code
  movl	%ecx, (%esi)			\ Old loop index
  leal	-8 . (%esi), %esi
  popl	%ecx				\ New limit
  addl	." $0x7fffffff" ., %ecx
  movl	%ecx, 4 . (%esi)		\ Add for i
  subl	tos, %ecx			\ Count
  popl	tos
;

: ?do-code	( label -- )
  do-code
  cmpl	." $0x7fffffff" ., %ecx
  je	.Llabel				\ Branch to exit label
;

: unloop-code
  leal	8 . (%esi), %esi		\ Discard loop parameter
  movl	(%esi), %ecx			\ Old loop index
;

[else]

: -do-code
  pushl	%ecx				\ Old loop index
  movl	tos, %ecx
  movl	(%esi), %eax			\ New limit
  pushl	%eax				\ >r limit
  movl	4 . (%esi), tos			\ Pop tos
  leal	8 . (%esi), %esi
;

: do-code
  pushl	%ecx				\ Old loop index
  movl	(%esi), %ecx			\ New limit
  addl	." $0x7fffffff" ., %ecx
  pushl	%ecx				\ Add for i
  subl	tos, %ecx			\ Count
  movl	4 . (%esi), tos			\ Pop tos
  leal	8 . (%esi), %esi
;

: ?do-code	( label -- )
  do-code
  cmpl	." $0x7fffffff" ., %ecx
  je	.Llabel				\ Branch to exit label
;

: unloop-code
  movl	4 . (%esp), %ecx		\ Old loop index
  leal	8 . (%esp), %esp		\ Discard loop parameter
;

[then]

\	Compiler do	( -- old-leave loop exit )
\
: do
  /verbose
  if
    ." / do" cr
  then

  leave-label				\ Save old loop label
  new-label				\ Loop label
  new-label dup to leave-label		\ Exit+Leave label
  over

    do-code
  .Llabel:

  [ LOOP +LOOP or ] literal
; immediate

\	Compiler do	( -- old-leave loop exit )
\
: -do
  /verbose
  if
    ." / do" cr
  then

  leave-label				\ Save old loop label
  new-label				\ Loop label
  new-label dup to leave-label		\ Exit+Leave label
  over

    -do-code
  .Llabel:

  [ LOOP +LOOP or ] literal
; immediate

: ?do
  /verbose
  if
    ." / ?do" cr
  then

  leave-label				\ Save old loop label
  new-label				\ Loop label
  new-label dup to leave-label		\ Exit+Leave label
  2dup

  ?do-code
  .Llabel:				\ Loop label

  [ LOOP +LOOP or ] literal
; immediate

/ps=sp [if]

: i
  /verbose
  if
    ." / i" cr
  then
  leave-label 0= abort" 'i' not within a 'do-loop'"

  push-tos
  movl	4 . (%esi), tos			\ Index sub
  subl	%ecx, tos			\ Calculate real index
; immediate

: -i
  /verbose
  if
    ." / i" cr
  then
  leave-label 0= abort" 'i' not within a 'do-loop'"

  push-tos
  movl	%ecx, tos			\ Index

; immediate

[else]

: i
  /verbose
  if
    ." / i" cr
  then
  leave-label 0= abort" 'i' not within a 'do-loop'"

  push-tos
  movl	(%esp), tos			\ Index sub
  subl	%ecx, tos			\ Calculate real index
; immediate

[then]


/ps=sp [if]

: -j
  /verbose
  if
    ." / j" cr
  then
  leave-label 0= abort" 'j' not within a 'do-loop'"

  push-tos
  movl	8 . (%esi), tos			\ Get index
; immediate

: j
  /verbose
  if
    ." / j" cr
  then
  leave-label 0= abort" 'j' not within a 'do-loop'"

  push-tos
  movl	12 . (%esi), tos		\ Index sub
  subl	8 . (%esi), tos			\ Calculate real index
; immediate

[else]

: j
  /verbose
  if
    ." / j" cr
  then
  leave-label 0= abort" 'j' not within a 'do-loop'"

  push-tos
  movl	8 . (%esp), tos			\ Index sub
  subl	4 . (%esp), tos			\ Calculate real index
; immediate

[then]

: leave
  /verbose
  if
    ." / leave" cr
  then
  leave-label ?dup 0= abort" 'leave' not within a 'do-loop'"
  jmp	.Llabel
; immediate

: ?leave
  /verbose
  if
    ." / ?leave" cr
  then
  leave-label ?dup 0= abort" '?leave' not within a 'do-loop'"

  orl	tos, tos
  pop-tos
  jnz	.Llabel

; immediate

: unloop
  /verbose
  if
    ." / unloop" cr
  then
  unloop-code
; immediate

: -loop
  /verbose
  if
    ." / loop" cr
  then
  LOOP and 0= abort" 'loop' does not match a 'do'"
  swap

  incl	%ecx				\ Decrement index
  cmpl	4 . (%esi), %ecx
  js	.Llabel
  .Llabel:				\ Resolve exit label
  unloop-code

  to leave-label
; immediate

: loop
  /verbose
  if
    ." / loop" cr
  then
  LOOP and 0= abort" 'loop' does not match a 'do'"
  swap

  decl	%ecx				\ Decrement index
  jno	.Llabel
  .Llabel:				\ Resolve exit label
  unloop-code

  to leave-label
; immediate

: -+loop
  /verbose
  if
    ." / +loop" cr
  then
  +LOOP and 0= abort" '+loop' does not match a 'do'"
  swap

  addl	tos, %ecx			\ Decrement index
  pop-tos
  cmpl	4 . (%esi), %ecx
  js	.Llabel
  .Llabel:				\ Resolve exit label
  unloop-code

  to leave-label
; immediate

: +loop
  /verbose
  if
    ." / +loop" cr
  then
  +LOOP and 0= abort" '+loop' does not match a 'do'"
  swap

  subl	tos, %ecx			\ Decrement index
  pop-tos
  jno	.Llabel
  .Llabel:				\ Resolve exit label
  unloop-code

  to leave-label
; immediate

: begin
  /verbose
  if
    ." / begin" cr
  then
  new-label dup .Llabel:		\ Loop label
  new-label				\ Exit label
  [ AGAIN UNTIL WHILE or or ] literal
; immediate

: while
  /verbose
  if
    ." / while" cr
  then
  WHILE and 0= abort" 'while' does not match a 'begin'"
  dup

  orl	tos, tos
  pop-tos
  jz	.Llabel

  REPEAT
; immediate

: repeat
  /verbose
  if
    ." / repeat" cr
  then
  REPEAT and 0= abort" 'repeat' does not match a 'while'"
  swap
  jmp	.Llabel
  .Llabel:				\ Resolve exit label
; immediate

: until
  /verbose
  if
    ." / until" cr
  then
  UNTIL and 0= abort" 'until' does not match a 'begin'"
  swap

  orl	tos, tos
  pop-tos
  jz	.Llabel
  .Llabel:				\ Resolve exit label

; immediate

: again
  /verbose
  if
    ." / again" cr
  then
  AGAIN and 0= abort" 'again' does not match a 'begin'"
  swap
  jmp	.Llabel
  .Llabel:				\ Resolve exit label
; immediate

: ahead
  /verbose
  if
    ." / ahead" cr
  then
  new-label dup				\ then label

  jmp	.Llabel

  THEN
; immediate

: if
  /verbose
  if
    ." / if" cr
  then
  new-label dup				\ else/then label

  orl	tos, tos
  pop-tos
  jz	.Llabel

  [ ELSE THEN or ] literal
; immediate

: 0=if
  /verbose
  if
    ." / 0=if" cr
  then
  new-label dup				\ else/then label

  orl	tos, tos
  pop-tos
  jnz	.Llabel

  [ ELSE THEN or ] literal
; immediate

: else
  /verbose
  if
    ." / else" cr
  then
  ELSE and 0= abort" 'else' does not match a 'if'"
  new-label dup				\ else/then label

  jmp	.Llabel
  swap
  .Llabel:				\ Resolve else/then label
  THEN
; immediate

: then
  /verbose
  if
    ." / then" cr
  then
  THEN and 0= abort" 'then' does not match a 'if' or 'ahead'"
  .Llabel:				\ Resolve else/then label
; immediate

\ -----------------------------------------------------------------------------
\	Modified compiler words
\ -----------------------------------------------------------------------------

\	Compiler literal
\
: literal
  \ FIXME: Check modified stack entry
  target-literal
; immediate

\	Search word in target vocabulary
\
: target-'
\  target ' compiler to use-reference 0
  bl word count ['] target >body @ search-wordlist
  if
    to use-reference 0 exit
  then
  here count ['] compiler >body @ search-wordlist
  if
    to use-reference 0 exit
  then
  here count type true abort"  :not found"
;

\	Compiler postpone
\
: postpone
  /verbose
  if
    ." / postpone" cr
  then
  bl word count ['] target >body @ search-wordlist
  0=if
    here count ['] compiler >body @ search-wordlist
    0=if
      here count type true abort" :postpone?"
    then
  then
\  target defined compiler 0= abort" :postpone?"
  dup >body cell+ @ T_IMMEDIATE and
  if					\ Immediate
    target-compile,
  else					\ Normal
    push-tos
    movl .$ .Xname ., tos		\ Literal
    s" compile," "call,			\ call compile,
  then
; immediate compilation

\	Compiler [compile]
\
: [compile]
\  target ' compiler target-compile,
  bl word count ['] target >body @ search-wordlist
  0=if
    here count ['] compiler >body @ search-wordlist
    0=if
      here count type true abort" :[compile]?"
    then
  then
  target-compile,
; immediate

forward (does>)

\	Compiler does>
\
/ps=sp [if]

: does>
  /verbose
  if
    ." / does>" cr
  then
  ['] (does>) target-call,
  popl	%eax				\ swap
  push-tos
  movl	%eax, tos
  target-nest,
; immediate

[else]

: does>
  ['] (does>) target-call,
  push-tos
  popl tos
; immediate

[then]

: to		( "name" -- )
  defined 0= abort" ?"
  /verbose
  if
    ." / to " dup .name cr
  then
  dup >body @ ['] <value> =
  if					\ VALUE
    state @
    if
      movl	tos, .Dname cr
      pop-tos
    else
      .set	.Vname ., .reference cr
    then
    exit
  then
  [ /user ] [if]
  dup >body @ ['] <user-value> =
  if					\ USER VALUE
    state @
    if
      >body 2 cells + @
      movl	tos, . (%edi)
      pop-tos
    else
      .name true abort" to on user value while interpreting"
      \ .set	.Vname ., .reference cr
    then
    exit
  then
  [then]
  .name true abort" not a value"
; immediate

\	Compiler leave compilation mode
\
: [
  0 state !
; immediate

\	Compiler enter compilation mode
\
: ]
  state @
  if					\ compiling
    /verbose if
      ." / ]" cr
    then
    ['] ] target-call,
  else
    1 state !
  then
; immediate

/ps=sp [if]

: execute
  /verbose if
    ." / execute" cr
  then
  movl	tos, %eax
  pop-tos
  movl	.$ .+8 ., (%esi)
  jmp	%eax
; immediate

: perform
  /verbose if
    ." / perform" cr
  then
  movl	(tos), %eax
  pop-tos
  orl	%eax, %eax
  jz	8 . cr
  movl	.$ .+8 ., (%esi)
  jmp	%eax
; immediate

[then]

\	Compiler '
\
\		Search next word in target wordlist.
\		Set use-reference for constants.
\
: '
  target-'
; execution

\	Compiler [']
\
\		Code reference.
\
: [']
  target-'
  literal-reference
; immediate

\	Compiler [char]
\
\		Char constant.
\
: [char]
  char target-literal
; immediate

\ -----------------------------------------------------------------------------
\	String literal operators
\ -----------------------------------------------------------------------------

/fast-string [if]			\ Faster string literals

\	Compiler counted string literal
\
: c"
  state @
  if
    /verbose
    if
      ." / c" [char] " emit cr
    then
    [char] " parse

      jmp   1f				\ Jump over literal
    0:
      dup .byte-val			\ Length of counted string
      .ascii-val			\ Counted string
    1:
      push-tos
      movl  ." $ 0b , " tos		\ String address

  else
    [compile] c"
  then
; immediate

\	Build a forth string literal
\
/ps=sp [if]
: do-s"
  [char] " parse

    jmp   1f				\ Jump over literal
  0:
    tuck .ascii-val			\ Forth string
  1:
    push-tos
    pushl .$ 0b				\ String address
    movl  .$ .u ., tos			\ String length
;
[else]
: do-s"
  [char] " parse

    jmp   1f				\ Jump over literal
  0:
    tuck .ascii-val			\ Forth string
  1:
    leal  ." -8 (%esi), %esi" cr
    movl  tos, 4 .u (%esi)		\ Save tos
    movl  ." $ 0b, (%esi)" cr		\ String address
    movl  .$ .u ., tos			\ String length
;
[then]

\	Compiler forth string literal
\
: s"
  state @
  if
    /verbose
    if
      ." / s" [char] " emit cr
    then
    do-s"
  else
    [compile] s"
  then
; immediate

\	Compiler type a string literal
\
: ."
  /verbose
  if
    ." / ." [char] " emit cr
  then
  do-s"
  s" type" "call,
; immediate

forward (abort")

: abort"
  /verbose
  if
    ." / abort" cr
  then
  new-label dup				\ else/then label

  orl	tos, tos
  pop-tos
  jz	.Llabel

  do-s"
  ['] (abort") target-compile,

  .Llabel:				\ Resolve else/then label
; immediate

[else]					\ Another version.

forward (c")

: c"
  state @
  if
    /verbose
    if
      ." / c" [char] " emit cr
    then
    ['] (c") target-compile,
    [char] " parse
    dup .byte-val .ascii-val
  else
    [compile] c"
  then
; immediate

forward (s")

: s"
  state @
  if
    /verbose
    if
      ." / s" [char] " emit cr
    then
    ['] (s") target-compile,
    [char] " parse
    dup .byte-val .ascii-val
  else
    [compile] s"
  then
; immediate

: ."
  ['] (s") target-compile,
  [char] " parse
  dup .byte-val .ascii-val
  s" type" "call,
; immediate

forward (abort")

: abort"
  /verbose
  if
    ." / abort" cr
  then
  new-label dup				\ else/then label

  orl	tos, tos
  pop-tos
  jz	.Llabel

  ['] (abort") target-compile,
  [char] " parse
  dup .byte-val .ascii-val

  .Llabel:				\ Resolve else/then label
; immediate

[then]

\ -----------------------------------------------------------------------------

\	Chars are 1 byte no conversion needed
: chars ; immediate

\	Target cells are 4 bytes
: cell
  4 state @  if  target-literal  then
; immediate

: cells
  4 *
; execution

cell	field cell+  execution

: here abort" use of here" ; execution
: false 0 ; execution
: true -1 ; execution
: decimal decimal ; execution
: octal octal ; execution
: hex hex ; execution
: negate negate ; execution
: * * ; execution
: - - ; execution
: + + ; execution
: = = ; execution
: 0= 0= ; execution
: 1+ 1+ ; execution
: 1- 1- ; execution
: 2* 2* ; execution
: 2/ 2/ ; execution
exist [ifdef] [if]
: [ifdef] [compile] [ifdef] ; immediate
[then]
exist [ifndef] [if]
: [ifndef] [compile] [ifndef] ; immediate
[then]
: [if] [compile] [if] ; immediate
: [else] [compile] [else] ; immediate
: [then] [compile] [then] ; immediate
: \ [compile] \ ; immediate
: ( [compile] ( ; immediate

: allot
  .space .u cr
; execution

: ,
  use-wordlist
  if					\ On stack wid
    .long [char] w emit .u cr		\ Wordlist reference
    0 to use-wordlist
  else
    reference,
  then
; execution

: w,
  .word-val
; execution

: c,
  .byte-val
; execution

: char
  bl word count 0= abort" character expected" c@
; execution

\ -----------------------------------------------------------------------------
\	Kernel forward references
\ -----------------------------------------------------------------------------

forward .s

forward header
forward create
forward empty-cache
forward place
forward cmove
forward string?

forward interpret
forward quit
forward abort
forward catch
forward throw

/less-code [if]

forward -
forward /
forward r@
forward 2r>
forward 2>r
forward negate
forward cells
forward cell+
forward cmove>
/ps=sp [if]
forward (constant)
[then]

[then]

forward <constant>
forward compiler,
forward executer,

\ -----------------------------------------------------------------------------

>current hidden

\	ex-literal
\
\		Parse literals in execution mode.
\
: ex-literal	( c-addr -- n )
  count ?number
  0=if
    over c@ [char] $ =
    if
      base @ >r hex 1 /string ?number r> base !
      0=if
	-1 /string type true abort" $?"
      then
    else
      order type true abort" ?"
    then
  then
;

\	co-literal
\
\		Compile literals in compilation mode.
\
: co-literal	( c-addr -- )
  ex-literal target-literal
;

\	Compilation/Interpretation table
\
\		state @ 2* find +
\
create target-run-table
  ' execute ,				\ immediate-word	s=0 f=-4
  ' ex-literal ,			\ execute-literal	s=0 f= 0
  ' execute ,				\ execute-word		s=0 f= 4
  ' co-literal ,			\ compile-literal	s=8 f= 0
  ' target-compile, ,			\ compile-word		s=8 f= 4

' target-run-table >body cell+ field +run-table

\	compiler-run
\
\		Compile current input until exhausted.
\
: compiler-run
  begin
    refill				\ Get one line from input
  while
    \ [char] / emit tib #tib @ type cr
    \ ." STACK:" .s cr
    begin
      \ depth 0< abort" Stack underflow"
      bl word dup c@			\ Scan input get next word
    while
      find state @ 2* + cells +run-table perform
    repeat
    drop
  repeat
;

>current compiler

\	included	( c-addr u -- )
\
\		Include a kernel source file.
\
: included
  2dup r/o open-file
  if
    drop type true abort" :Can't open file"
  then
  >r 2drop r>

  >in @ #tib @ 2>r			\ Save current input stream
  tib source-file 2>r
  [ exist blk ] [if]
    blk @ >r

    0 blk !
  [then]

  dup to source-file
  >file ->file-buffer to tib

  ['] compiler-run catch ?dup		\ Compile input
  if
    [char] : emit source-file dup .file
    [char] ( emit >file ->file-line @ (.) type [char] ) emit

    [ exist blk ] [if]			\ Restore prior input stream
      r> blk !
    [then]
    2r> to source-file to tib
    2r> #tib ! >in !

    throw				\ throw to next frame
  then
  source-file close-file throw

  [ exist blk ] [if]			\ Restore prior input stream
    r> blk !
  [then]
  2r> to source-file to tib
  2r> #tib ! >in !
; execution

>current hidden

\	target-compiler		( -- )
\
\		Target compiler.  Compile current input stream.
\
: target-compiler
					\ Initialisation stuff
  [ exist msdos ] [if]
    " out.s" r/w create-file throw
    dup >file ->fildes @		\ Handle
    1 swap dup2				\ redirect output
  [then]

  ['] target >body @ dup set-current	\ Words are placed in target
  ['] compiler >body @ dup 3 set-order	\ Search compiler then target

  open-document
  open-view

  compiler-run				\ Compile input

					\ Cleanup stuff
  resolve-flag				\ resolve last word
  resolve-wid				\ resolve wordlists
  close-view				\ close view file
  close-document			\ close document file
  [ exist msdos ] [if]
    close-file throw
  [then]
  bye
;
