\
\	ALE Forth Version 2.04
\
\		Copyright (c) 1991,1992 by Lutz Sammer.
\
\		Bug-reports via usenet: johns
\
\	File:	dasm.4th
\
\		Disassembler for 80[34]86.
\
\	RCS:
\	$Id: dasm.4th,v 1.2 1992/03/07 17:03:29 johns Exp johns $
\	$Log: dasm.4th,v $
\ Revision 1.2  1992/03/07  17:03:29  johns
\ Disassemble more completed, 99% Ready.
\

vocabulary dasm
also compiler also dasm definitions
hex

\ -----------------------------------------------------------------------------

\	Too avoid name conflicts:
\
\		Use {forth} and , xor , or , loop
\
: {forth}
  also forth defined previous		\ Find the word
  dup ?missing 0<
  if					\ immediate execute it
    execute exit
  then
  compile,				\ compile the word
; immediate

\	Missing features:

: (exec:)
  cells r> + perform
; compilation

: exec:					\ ( n -- ) Execute n' word
  postpone (exec:)
  begin
    begin
      bl word dup c@
    while
      find ?missing
      dup ['] \ =
      if
	drop [compile] \
      else
	dup ['] ; =
	if
	  drop postpone ; exit
	then
	,				\ Lay down address
      then
    repeat
    drop refill 0=			\ Get one line from input
  until
  true abort" EOF unexpected"
; immediate compilation

\ -----------------------------------------------------------------------------
\	Disassembler variables
\ -----------------------------------------------------------------------------

value symbolic  1 to symbolic		\ Try to show symbols
value last-addr				\ Last displayed address

variable cp				\ Code pointer
value osize				\ Operand size 0=32 1=16 bit
value asize				\ Address size 0=32 1=16 bit

\ -----------------------------------------------------------------------------

false [if]				\ { ! Defered fetch - functions

: fetchb	( -- byte )		\ Get instruction byte
  cp @ c@
;

: nextb		( -- byte)		\ Get next instruction byte
  fetchb cp 1+!
;

: fetchw	( -- word )		\ Get instruction word
  cp @ w@
;

: nextw		( -- word )		\ Get next instuction word
  fetchw 2 cp +!
;

: fetchl	( -- long )		\ Get instruction long
  cp @ @
;

: nextl		( -- long )		\ Get next instruction long
  fetchl 4 cp +!
;

[else]					\ }{

\	I need them defered for ptrace

defer c@dis	' c@ is c@dis
defer w@dis	' w@ is w@dis
defer @dis	'  @ is  @dis

: fetchb	( -- byte )		\ Get instruction byte
  cp @ c@dis
;

: nextb		( -- byte)		\ Get next instruction byte
  fetchb cp 1+!
;

: fetchw	( -- word )		\ Get instruction word
  cp @ w@dis
;

: nextw		( -- word )		\ Get next instuction word
  fetchw 2 cp +!
;

: fetchl	( -- long )		\ Get instruction long
  cp @ @dis
;

: nextl		( -- long )		\ Get next instruction long
  fetchl 4 cp +!
;

[then]					\ }

: null		( -- )			\ Does nothing
;

\ -----------------------------------------------------------------------------
\	Output
\ -----------------------------------------------------------------------------

: >col		( col -- )		\ Move to column
  out @ 2dup <
  if
    cr drop 0
  then
  - spaces
;

: .self		( -- )			\ Prints it self
  create
does>
  body> .name
;

: .self.l	( -- )			\ Prints it self left justified
  create
does>
  body> .name 9 >col
;

: (.h2)		( n -- )		\ Print n as 2 hex digits
  0 <# # # #> type
;

: .h2		( n -- )		\ Print n as 2 hex digits with space
  (.h2) space
;

: .h4		( n -- )		\ Print n as 4 hex digits with space
  0 <# # # # # #> type space
;

: .h8		( n -- )		\ Print n as 8 hex digits with space
  0 <# # # # #  # # # # #> type space
;

: .:  ." : "  ;				\ Emit :
: .,  ." , "  ;				\ Emit ,
: .(  [char] ( emit  ;			\ Emit (
: .)  ." ) "  ;				\ Emit )
: .$  ." $ "  ;				\ Emit $

: name?		( c-addr -- c-addr true | false )
  valid?				\ Address valid
  if
    dup -2 + valid?			\ Offset to name valid
    if
      c@ ->code - valid?		\ Name address valid
      if
	dup c@ 1 bl within
	0=if				\ Length of name 1-31
	  drop false exit
	then
	dup count bounds
	do
	  i c@ bl 80 within		\ All chars must within bl - del
	  0=if
	    unloop drop
	    false exit
	  then
	{forth} loop
	true exit
      then
    else
      drop
    then
  then
  drop false
;

: symbol	( addr -- )		\ Print symbol
  dup to last-addr
  symbolic
  if
    dup name?				\ Look if dictionary name
    if
      ." ' " count type drop exit
    then
    dup body> name?			\ Look if dictionary name
    if
      ." ' " count type drop ."  >body " exit
    then
  then
  .h8					\ No name display it
;

\ -----------------------------------------------------------------------------
\	Segmente
\ -----------------------------------------------------------------------------

.self %es	.self %cs	.self %ss	.self %ds
.self %fs	.self %gs	.self <s6>	.self <s7>

: .seg		( seg# -- )		\ Display segment register
  7 {forth} and exec:
    %es %cs %ss %ds  %fs %gs <s6> <s7>
;

: seg		( op -- )		\ Extract and display segment register
  3 >> .seg
;

\ -----------------------------------------------------------------------------
\	Special Register
\ -----------------------------------------------------------------------------

.self	%cr0	.self	%cr1	.self	%cr2	.self	%cr3
.self	%cr4?	.self	%cr5?	.self	%cr6?	.self	%cr7?

: creg		( op -- )		\ Extract and display control register
  3 >> 7 {forth} and exec:
     %cr0 %cr1 %cr2 %cr3 %cr4? %cr5? %cr6? %cr7?
;

.self	%db0	.self	%db1	.self	%db2	.self	%db3
.self	%db4	.self	%db5	.self	%db6	.self	%db7

: dreg		( op -- )		\ Extract and display debug register
  3 >> 7 {forth} and exec:
     %db0 %db1 %db2 %db3 %db4 %db5 %db6 %db7
;

.self	%tr0?	.self	%tr1?	.self	%tr2?	.self	%tr3?
.self	%tr4?	.self	%tr5?	.self	%tr6	.self	%tr7

: treg		( op -- )		\ Extract and display test register
  3 >> 7 {forth} and exec:
     %tr0? %tr1? %tr2? %tr3? %tr4? %tr5? %tr6 %tr7
;

\ -----------------------------------------------------------------------------
\	Register
\ -----------------------------------------------------------------------------

\	8 Bit register
.self %al	.self %cl	.self %dl	.self %bl
.self %ah	.self %ch	.self %dh	.self %bh

: reg8		( reg# -- )		\ Display 8 Bit register
  7 {forth} and exec:
    %al %cl %dl %bl %ah %ch %dh %bh
;

\	16 Bit register
.self %ax	.self %cx	.self %dx	.self %bx
.self %sp	.self %bp	.self %si	.self %di

: reg16		( reg# -- )		\ Display 16 Bit register
  7 {forth} and exec:
    %ax %cx %dx %bx %sp %bp %si %di
;

\	32 Bit register
.self %eax	.self %ecx	.self %edx	.self %ebx
.self %esp	.self %ebp	.self %esi	.self %edi

: reg32		( reg# -- )		\ Display 32 Bit register
  7 {forth} and exec:
    %eax %ecx %edx %ebx %esp %ebp %esi %edi
;

: reg16/32	( reg# -- )		\ Display 16/32 Bit register
  osize exec:
    reg32 reg16
;

: .reg		( size reg# -- )	\ Display register
  swap 1 {forth} and exec:
    reg8 reg16/32
;

: reg		( size ext -- )		\ Extract and display register
  3 >> .reg
;

: ax/eax	( -- )			\ Display %ax or %eax
  osize exec:
    %eax %ax
;

: accu		( size -- )		\ Display %al %ax %eax
  1 {forth} and exec:
    %al ax/eax
;

\	Indirect 16 Bit register
.self (%ax)	.self (%cx)	.self (%dx)	.self (%bx)
.self (%sp)	.self (%bp)	.self (%si)	.self (%di)

\	32 Bit indirect register
.self (%eax)	.self (%ecx)	.self (%edx)	.self (%ebx)
.self (%esp)	.self (%ebp)	.self (%esi)	.self (%edi)

: .ind32	( reg# -- )		\ Display 32 Bit indirect register
  7 {forth} and exec:
    (%eax) (%ecx) (%edx) (%ebx) (%esp) (%ebp) (%esi) (%edi)
;

: disp8		( -- )			\ Display 8 bit displacement
  nextb .h2
;

: disp16	( -- )			\ Display 16 bit displacement
  nextw .h4
;

: disp32	( -- )			\ Display 32 bit displacement
  nextl symbol
;

: disp		( -- )			\ Display 16/32 bit displacement
  asize exec:
    disp32 disp16
;

: 16disp	( op ext -- op ext )	\ Show displacement for 16 bit mode
  dup 6 >> 3 {forth} and exec:
    null disp8 disp16 null
;

\	16 Bit address register:
\
.self (%bx,%si)	.self (%bx,%di)	.self (%bp,%si)	.self (%bp,%di)
.self (%si)	.self (%di)	.self (%bp)	.self (%bx)

: reg/mem16	( op ext -- )		\ 16 Bit addessing modes
  nip
  dup c7 {forth} and 6 =
  if					\ MOD 00 R/M 110
    drop disp16 exit
  then
  16disp
  7 {forth} and exec:
    (%bx,%si) (%bx,%di) (%bp,%si) (%bp,%di) (%si) (%di) (%bp) (%bx)
;

: mod0		( ext -- )		\ Indirect
  7 {forth} and exec:
    (%eax) (%ecx) (%edx) (%ebx) null disp32 (%esi) (%edi)
;

: mod1		( ext -- )		\ Disp8 Indirect
  disp8 .ind32
;

: mod2		( ext -- )		\ Disp32 Indirect
  disp32 .ind32
;

\	32 Bit indirect register
.self (%eax,	.self (%ecx,	.self (%edx,	.self (%ebx,
.self (%esp,	.self (%ebp,	.self (%esi,	.self (%edi,

: .base		( sib -- sib )		\ Base register
  dup 7 {forth} and exec:
    (%eax, (%ecx, (%edx, (%ebx, (%esp, (%ebp, (%esi, (%edi,
;

\	32 Bit index register
.self %eax,	.self %ecx,	.self %edx,	.self %ebx,
.self %esp,	.self %ebp,	.self %esi,	.self %edi,

: .index	( sib -- sib )		\ Index register
  dup 3 >> 7 {forth} and exec:
    %eax, %ecx, %edx, %ebx, null %ebp, %esi, %edi,
;

\	Scaleing factors
.self 1)	.self 2)	.self	4)	.self	8)

: scale		( sib -- )		\ Scale factor
  space 6 >> exec:
    1) 2) 4) 8)
;

: s-mod0	( sib -- )		\ Indirect
  dup 7 {forth} and 5 =
  if					\ Displacement 32 Bit
    disp32 .( .,
  else
    .base space
  then
  .index scale
;

: s-mod1	( sib -- )		\ Disp8 Indirect
  disp8 .base space .index scale
;

: s-mod2	( sib -- )		\ Disp32 Indirect
  disp32 .base space .index scale
;

: s-i-b		( ext -- )		\ S-I-B Field
  nextb swap 6 >> exec:			\ Mod field
    s-mod0 s-mod1 s-mod2 null
;

: reg/mem32	( op ext -- )		\ Register/Memory 32 Bit Addressing
  nip
  dup 7 {forth} and 4 =			\ S-I-B
  if
    s-i-b
    exit
  then
  dup 6 >> exec:			\ Mod field
    mod0 mod1 mod2 null
;

: reg/mem	( op ext -- op ext )	\ Display reg/mem
  2dup
  dup 6 >> 3 =				\ Mod-field = 3
  if
    .reg				\ Always register
    exit
  then
  asize exec:
    reg/mem32 reg/mem16
;

: ?disp	( op ext -- op ext disp )	\ Immediate displacement
  asize
  if					\ 16 Bit
    dup 6 >> ?dup
    0=if				\ mod=00
      dup 7 {forth} and
      6 = 2 {forth} and			\ +2 d16 bytes
    else
      dup 3 =
      if				\ mod=11
	drop 0
      then
					\ mod=01 +1 d8 bytes
					\ mod=10 +2 d16 bytes
    then
  else					\ 32 Bit
    dup c0 {forth} and c0 <>
    over 7 {forth} and 4 = {forth} and	\ r/m=100
    if					\ s-i-b byte
      dup 6 >> ?dup
      0=if				\ mod=00
	fetchb 7 {forth} and
	5 = 4 {forth} and		\ +4 d32 bytes
      else
	dup 2 =
	if				\ mod=10
	  drop 4			\ +4 d32 bytes
	else
	  dup 3 =
	  if				\ mod=11
	    drop 0
	  then
					\ mod=01 +1 d8 bytes
	then
      then
      1+				\ + 1 s-i-b byte
    else
      dup 6 >> ?dup
      0=if				\ mod=00
	dup 7 {forth} and
	5 = 4 {forth} and		\ +4 d32 bytes
      else
	dup 2 =
	if				\ mod=10
	  drop 4			\ +4 d32 bytes
	else
	  dup 3 =
	  if				\ mod=11
	    drop 0
	  then
					\ mod=01 +1 d8 bytes
	then
      then
    then
  then
;

\ -----------------------------------------------------------------------------

.self	addr16	.self	data16	.self	lock

.self.l	aaa	.self.l aad	.self.l aam	.self.l aas
.self   adc	.self   add	.self   and	.self.l arpl
.self   bound	.self	bsf	.self	bsr	.self	bt
.self	btc	.self	btr	.self	bts	.self.l call
.self.l	cbtw	.self.l	clc	.self.l	cld	.self.l cli
.self.l	cltd	.self.l	clts	.self.l cmc	.self	cmp
.self	cmps	.self.l cwtd	.self.l cwtl	.self.l	daa
.self.l	das	.self	dec	.self.l decl	.self	div
.self.l	enter	.self.l	hlt	.self	idiv	.self	imul
.self	in	.self	inc	.self.l	incl	.self	ins
.self.l	int	.self.l	into	.self.l	iret
.self.l	jo	.self.l	jno	.self.l	jb	.self.l	jae
.self.l	je	.self.l	jne	.self.l	jbe	.self.l	ja
.self.l	js	.self.l	jns	.self.l	jpe	.self.l	jpo
.self.l	jl	.self.l	jge	.self.l	jle	.self.l	jg
.self.l	jcxz	.self.l	jecxz	.self.l	jmp	.self.l lahf
.self.l	lar	.self.l	lcall	.self	lea	.self.l leave
.self.l	ljmp	.self.l	lgdt	.self.l	lidt	.self.l	lds
.self.l	les	.self.l	lfs	.self.l	lgs	.self.l	lss
.self.l	lldt	.self.l	lmsw	.self	lods	.self.l	loop
.self.l	loop	.self.l	loope	.self.l	loopne	.self.l	lsl
.self.l	ltr	.self	mov	.self	movs	.self	movsb
.self	movzb	.self	movzw	.self	mul	.self	neg
.self	nop	.self	not	.self	or	.self	out
.self	outs
.self	pop	.self	popa	.self	push	.self	pusha
.self	rcl	.self	rcr	.self	rep	.self	repe
.self	repne	.self	ret	.self	rol	.self	ror
.self	sar	.self	sbb	.self	scas	.self	shl
.self	shr	.self	stos	.self	sub	.self	test
.self	xchg	.self	xor	.self.l	lret	.self.l	popfl
.self.l	pushfl	.self.l	pushl	.self.l	sahf
.self.l	sete	.self.l	setne	.self.l	setbe	.self.l	seta
.self.l	setl	.self.l	setge	.self.l	setle	.self.l	setg
.self.l	seto	.self.l	setno	.self.l	setb	.self.l	setae
.self.l	sets	.self.l	setns	.self.l	setpe	.self.l	setpo
.self.l	stc	.self.l	std	.self.l	sti
.self.l	verr	.self.l	verw	.self.l	xchgl
.self.l	xlat	.self.l sgdt	.self.l sidt	.self.l sldt
.self.l smsw	.self.l str

.self.l	ill0f00
.self.l	ill0f01
.self.l	ill-fe
.self.l	ill-ff
.self	ill-bit
.self	ill-shft
.self	if6/f7

\ -----------------------------------------------------------------------------

.self.l	f2xm1	.self.l	fabs	.self.l	fadd	.self.l	faddl
.self.l	faddp	.self.l	fadds	.self.l	falc	.self.l	fbld
.self.l	fbstp	.self.l	fchs	.self.l	fcom	.self.l	fcoml
.self.l	fcomp	.self.l	fcomp	.self.l	fcomp	.self.l	fcompl
.self.l	fcompp	.self.l	fcomps	.self.l	fcoms	.self.l	fdiv
.self.l	fdivl	.self.l	fdivp	.self.l	fdivr	.self.l	fdivrl
.self.l	fdivrp	.self.l	fdivrs	.self.l	fdivs	.self.l	ffree
.self.l	ffreep	.self.l	fiaddl	.self.l	fiadds	.self.l	ficoml
.self.l	ficompl	.self.l	ficomps	.self.l	ficoms	.self.l	fidivl
.self.l	fidivrl	.self.l	fidivrs	.self.l	fidivs	.self.l	fildl
.self.l	fildq	.self.l	filds	.self.l	fimull	.self.l	fimuls
.self.l	fistl	.self.l	fistpl	.self.l	fistpq	.self.l	fistps
.self.l	fists	.self.l	fisubl	.self.l	fisubrl	.self.l	fisubrs
.self.l	fisubs	.self.l	fld	.self.l	fld1	.self.l	fldcw
.self.l	fldenv	.self.l	fldl	.self.l	flds	.self.l	fldt
.self.l	fldz	.self.l	fmul	.self.l	fmull	.self.l	fmulp
.self.l	fmuls	.self.l	fnop	.self.l	fnsave	.self.l	fnstcw
.self.l	fnstenv	.self.l	fnstsw	.self.l	fprem	.self.l	frstor
.self.l	fsincos	.self.l	fst	.self.l	fstl	.self.l	fstp
.self.l	fstpl	.self.l	fstps	.self.l	fstpt	.self.l	fsts
.self.l	fstsw	.self.l	fsub	.self.l	fsubl	.self.l	fsubp
.self.l	fsubr	.self.l	fsubrl	.self.l	fsubrp	.self.l	fsubrs
.self.l	fsubs	.self.l	ftst	.self.l	fucom	.self.l	fucomp
.self.l	fucompp	.self.l	fwait	.self.l	fxam	.self.l	fxch
.self.l fcos	.self.l fdecstp	.self.l fincstp	.self.l fldl2e
.self.l fldl2t	.self.l fldlg2	.self.l fldln2	.self.l fldpi
.self.l fnclex	.self.l fndisi	.self.l fneni	.self.l fninit
.self.l fpatan	.self.l fptan	.self.l frndint	.self.l fscale
.self.l fsetpm	.self.l fsin	.self.l fsqrt	.self.l fxtract
.self.l fyl2x	.self.l fyl2xp1

\ -----------------------------------------------------------------------------
\	Operand size
\ -----------------------------------------------------------------------------

: +b  [char] b emit 9 >col ;
: +w  [char] w emit 9 >col ;
: +l  [char] l emit 9 >col ;

: +wl		( -- )			\ Add w/l
  osize exec:
    +l +w
;

: +bwl		( op ext -- op ext )	\  Add b/w/l
  over 1 {forth} and exec:
    +b +wl
;

\ -----------------------------------------------------------------------------

: sreg		( op -- )		\ Add size and display segment register
  +wl seg
;

: cr->r		( op -- )		\ Control register to register
  drop nextb +wl dup creg ., reg16/32
;

: r->cr		( op -- )		\ register to control register
  drop nextb +wl dup reg16/32 ., creg
;

: dr->r		( op -- )		\ Debug register to register
  drop nextb +wl dup dreg ., reg16/32
;

: r->dr		( op -- )		\ register to debug register
  drop nextb +wl dup reg16/32 ., dreg
;

: tr->r		( op -- )		\ Test register to register
  drop nextb +wl dup treg ., reg16/32
;

: r->tr		( op -- )		\ register to test register
  drop nextb +wl dup reg16/32 ., treg
;

\ -----------------------------------------------------------------------------

: .$8		( addr -- len )		\ Display 8 Bit immediate
  .$ c@ .h2 1
;

: imm8		( addr -- len )		\ 8 Bit immediate
  +b .$8 .,
;

: 8imm16	( addr -- len )		\ 8 Bit immediate to 16 Bit reg/mem
  +w .$8 .,
;

: 8imm32	( addr -- len )		\ 8 Bit immediate to 32 Bit reg/mem
  +l .$8 .,
;

: imm16		( addr -- len )		\ 16 Bit immediate
  +w .$ w@ .h4 ., 2
;

: imm32		( addr -- len )		\ 32 Bit immediate
  +l .$ @ symbol ., 4
;

: imm	( addr op -- len )		\ Display Immediate
  3 {forth} and
  osize 0<> 4 {forth} and		\ 32 Bit / 16 Bit
  {forth} or exec:
      imm8 imm32 imm8 8imm32
      imm8 imm16 imm8 8imm16
;

\ -----------------------------------------------------------------------------

: i$8		( -- )			\ Immediate 8 Bit
  .$ disp8
;

: $16/32	( -- )			\ Immediate 16/32 Bit
  .$ osize
  if
    disp16
  else
    disp32
  then
;

: $val		( op -- )		\ Immediate +l $8/16/32
  +l
  2 {forth} and
  if
    i$8
  else
    $16/32
  then
;

: $->a		( op -- )		\ Immediate to accu
  dup +bwl
  1 {forth} and
  if
    $16/32
  else
    i$8
  then
  ., accu
;

\ -----------------------------------------------------------------------------

: g,r		( op mod -- )		\ Register/Memory, Register
  reg/mem ., reg
;

: r,g		( op mod -- )		\ Register, Register/Memory
  2dup reg ., reg/mem 2drop
;

\ -----------------------------------------------------------------------------

: +r->g		( op -- )		\ Byte/Word Register to Register/Memory
  nextb +bwl r,g
;

: +g->r		( op -- )		\ Byte/Word Register/Memory to Register
  nextb +bwl g,r
;

: r->g		( op -- )		\ Word Register to Register/Memory
  1 {forth} or nextb +wl r,g
;

: g->r		( op -- )		\ Word Register/Memory to Register
  1 {forth} or nextb +wl g,r
;

: -r->g		( op -- )		\ Long Register to Register/Memory
  1 {forth} or nextb r,g
;

: -g->r		( op -- )		\ Long Register/Memory to Register
  1 {forth} or nextb g,r
;

: s->g		( op -- )		\ Segment register to Register/Memory
  nextb +w
  dup seg ., reg/mem 2drop
;

: g->s		( op -- )		\ Register/Memory to Segment register
  nextb +w
  reg/mem ., seg drop
;

: Wgen		( op ext -- )		\ Byte/Word Register/Memory
  reg/mem 2drop
;

: gen8		( op -- )		\ Byte Register/Memory
  fe {forth} and nextb Wgen
;

: gen		( op -- )		\ Word Register/Memory
  1 {forth} or nextb Wgen
;

: X+gen		( op ext -- )		\ + Byte/Word Register/Memory
  +bwl Wgen
;

: +gen		( op -- )		\ + Byte/Word Register/Memory
  nextb X+gen
;

: X+g->a	( op ext -- )		\ Byte/Word Register/Memory to accu
  +bwl reg/mem drop ., accu
;

: $->r		( op -- )		\ Imm, Reg
  dup 8 {forth} and			\ Size
  if
    osize
    if
      +w .$ disp16 ., reg16
    else
      +l .$ disp32 ., reg32
    then
  else
    +b i$8 ., reg8
  then
;

: +$					\ ( addr op -- )
  1 {forth} and
  osize 0<> 2 {forth} and		\ 32 Bit / 16 Bit
  {forth} or exec:
      imm8 imm32 imm8 imm16
;

: X$->rm				\ ( op ext -- ) Imm, Reg/Mem
  ?disp cp @ + pluck +$
  -rot reg/mem 2drop cp +!
;

: $->rm					\ ( op -- ) Imm, Reg/Mem
  nextb
\ FIXME:
\  dup {forth} and 38 abort" Illegal $->rm"
  X$->rm
;

: bitops	( op -- op )		\ Display Bit opcode
  fetchb 3 >> 7 {forth} and exec:
    bt  bts btr btc ill-bit ill-bit ill-bit ill-bit
;

: $8->g					\ ( op -- ) 8 Bit imm, Reg/Mem
  nextb +bwl ?disp cp @ +
  .$8 ., -rot reg/mem 2drop
  cp +!
;

: cl->g					\ ( op -- ) cl, Reg/Mem
  nextb +bwl %cl ., reg/mem 2drop
;

: alu					\ ( ext -- ) Alu opcode
  3 >> 7 {forth} and exec:
    add or adc sbb and sub xor cmp
;

: O$->rm				\ ( op -- ) 80 Opc Imm, Reg/Mem
  nextb dup alu
  ?disp cp @ + pluck
  imm -rot reg/mem 2drop cp +!
;

: Wr->rm	( op -- )		\ 16Bit Register, Register/Memory
  1 to osize				\ Force 16 Bit
  nextb 2dup reg ., reg/mem 2drop
;

: rm->R		( op -- )		\ Register/Memory, Register
  1 {forth} or				\ Force long
  nextb reg/mem ., reg
;

: a->d					\ op -- ) Accu, Disp
  0 +bwl drop
  accu ., disp
;

: d->a					\ ( op -- ) Disp, Accu
  0 +bwl drop
  disp ., accu
;

: r->a			( op -- )	\ Register, accu
  reg16/32 ., ax/eax
;

: rel8			( op -- )	\ 8 Bit Relative
  drop nextb dup 7F >  if  FFFFFF00 {forth} or  then
  cp @ + symbol
;

: rel			( op -- )	\ Relative 16/32 Bit
  drop
  osize
  if
    nextw dup 7FFF >  if  FFFF0000 {forth} or  then
  else
    nextl
  then
  cp @ + symbol
;

: inter			( op -- )	\ Intersegment call/jmp
  drop .$ disp16 ., .$ nextl .h8
;

: conv			( op -- )	\ Convert Byte/Word -> Word/Long
  drop osize exec:
    cwtl cbtw
;

: conv2			( op -- )	\ Convert Word/Long -> Long/Quad
  drop osize exec:
    cltd cwtd
;

: si->di		( op -- )	\ String (si), (di)
  0 +bwl 2drop
  asize
  if					\ 16 Bit
    (%si) ., (%di)
  else
    (%esi) ., (%edi)
  then
;

: si->ax		( op -- )	\ String (si), ax
  0 +bwl drop
  asize
  if					\ 16 Bit
    (%si)
  else
    (%esi)
  then
  ., accu
;

: ax->di		( op -- )	\ String ax, (di)
  0 +bwl drop
  accu .,
  asize exec:
    (%edi) (%di)
;

: dx->di		( op -- )	\ String (dx), (di)
  0 +bwl 2drop
  asize
  if					\ 16 Bit
    (%dx) ., (%di)
  else
    (%dx) ., (%edi)
  then
;

: si->dx		( op -- )	\ String (si), (dx)
  0 +bwl 2drop
  asize
  if					\ 16 Bit
    (%di) ., (%dx)
  else
    (%edi) ., (%dx)
  then
;

: r16/32		( op -- )	\ Register 16/32 Bit
  +wl reg16/32
;

: $16,$8		( op -- )	\ Enter: 16 Disp, 8 Disp
  drop .$ disp16 ., i$8
;

: $16			( op -- )	\ Ret:	16 Imm
  drop .$ disp16
;

: $,rm,a		( op -- )	\ Imm, Reg/Mem, accu
  nextb ?disp cp @ + pluck
  imm -rot reg/mem drop ., accu
  cp +!
;

: 8port			( op -- )	\ 8 Bit Port
  0 +bwl drop
  .$ disp8 ., accu
;

: (port)		( op -- )	\ Port (%dx)
  0 +bwl drop (%dx) ., accu
;

: int3			( op -- )	\ int $3
  drop .$ 3 .h2
;

: intX			( op -- )	\ int $x
  drop .$ disp8
;

: shifts
  fetchb 3 >> 7 {forth} and exec:
    rol ror rcl rcr shl shr ill-shft sar
;

: .f6/f7		( op ext -- )	\ Display F6/F7 Opcodes
  dup 3 >> 7 {forth} and exec:
    test if6/f7 not neg mul imul div idiv
;

: f6/f7op
  dup 3 >> 7 {forth} and exec:
    X$->rm X+gen X+gen X+gen X+g->a X+g->a X+g->a X+g->a
;

: f6/f7			( op -- )	\ F6/F7 Opcodes
  nextb .f6/f7 f6/f7op
;

: fe-opc		( op ext -- )	\ Display fe-opcodes
  3 >> 7 {forth} and exec:
    incl decl ill-fe ill-fe ill-fe ill-fe ill-fe ill-fe
;

: (reg/mem)
  [char] * emit reg/mem
;

: fe-inst		( op -- )	\ FE-Opcodes
  nextb dup fe-opc reg/mem 2drop
;

: ff-op					\ Dipslay ff-operands
  dup 3 >> 7 {forth} and exec:
    reg/mem reg/mem (reg/mem) (reg/mem) (reg/mem) (reg/mem) reg/mem reg/mem
;

: ff-opc		( op ext -- )	\ Display ff-opcodes
  3 >> 7 {forth} and exec:
    incl decl call lcall jmp ljmp pushl ill-ff
;

: ff-inst		( op -- )	\ FF-Opcodes
  nextb dup ff-opc ff-op 2drop
;

: p+wl			( op -- )	\ pusha/popa + size
  drop +wl
;

\ -----------------------------------------------------------------------------
\	387 Instructions
\ -----------------------------------------------------------------------------

: .freg		( reg# -- )		\ Display floating point register
  ." %st(" 7 {forth} and (.) type ." )"
;

: freg		( ext -- )		\ To top
  .freg ., 0 .freg
;

: rfreg		( ext -- )		\ From top
  0 .freg ., .freg
;

: freg1		( ext -- )
  .freg
;

: fgen		( ext -- )
  0 swap reg/mem 2drop
;

: fill		( ext fop -- )
  ." fill"
  2dup 4 >> 7 {forth} and d8 + .h2 .h2
;

: fopc4
  over 7 {forth} and exec:
    fneni fndisi fnclex fninit fsetpm fill fill fill
;

: fopcC
  over 7 {forth} and exec:
    fchs fabs fill fill ftst fxam fill fill
;

: fopcD
  over 7 {forth} and exec:
    fld1 fldl2t fldl2e fldpi fldlg2 fldln2 fldz fill
;

: fopcE
  over 7 {forth} and exec:
    f2xm1 fyl2x fptan fpatan fxtract fdecstp fincstp
;

: fopcF
  over 7 {forth} and exec:
    fprem fyl2xp1 fsqrt fsincos frndint fscale fsin fcos
;

: .fpu-opc	( ext fop -- ext fop )
  dup exec:
    fadds   fmuls   fcoms   fcomps  fsubs   fsubrs  fdivs   fdivrs	\ D8
    fadd    fmul    fcom    fcomp   fsub    fsubr   fdiv    fdivr
    flds    fill    fsts    fstps   fldenv  fldcw   fnstenv fnstcw	\ D9
    fld     fxch    fnop    fstp    fopcC   fopcD   fopcE   fopcF
    fiaddl  fimull  ficoml  ficompl fisubl  fisubrl fidivl  fidivrl	\ DA
    \ FIXME: E9 11 10_1 001 fucompp
    fill    fill    fill    fill    fill    fucompp fill    fill
    fildl   fill    fistl   fistpl  fill    fldt    fill    fstpt	\ DB
    \ FIXME: E0 11 10_0 XXX feni fdisi fclex finit fsetpm < E4
    fill    fill    fill    fill    fopc4   fill    fill    fill
    faddl   fmull   fcoml   fcompl  fsubl   fsubrl  fdivl   fdivrl	\ DC
    fadd    fmul    fcom    fcomp   fsub    fsubr   fdiv    fdivr
    fldl    fill    fstl    fstpl   frstor  fill    fnsave  fnstsw	\ DD
    \ FIXME: FFREE FXCH FST FSTP FUCOM FUCOMP
    ffree   fxch    fst     fstp    fucom   fucomp  fill    fill
    fiadds  fimuls  ficoms  ficomps fisubs  fisubrs fidivs  fidivrs	\ DE
    faddp   fmulp   fcomp   fcompp  fsubp   fsubrp  fdivp   fdivrp
    filds   fill    fists   fistps  fbld    fildq   fbstp   fistpq	\ DF
    ffreep  fxch    fstp    fstp    fstsw   fill    fill    fill
;

: .fpu-op	( ext fop -- ext )
  exec:
    fgen    fgen    fgen    fgen    fgen    fgen    fgen    fgen	\ D8
    freg    freg    freg    freg    freg    freg    freg    freg
    fgen    drop    fgen    fgen    fgen    fgen    fgen    fgen	\ D8
    freg1   freg1   drop    freg1   drop    drop    drop    drop
    fgen    fgen    fgen    fgen    fgen    fgen    fgen    fgen	\ DA
    drop    drop    drop    drop    drop    drop    drop    drop
    fgen    drop    fgen    fgen    drop    fgen    drop    fgen	\ DB
    drop    drop    drop    drop    drop    drop    drop    drop
    fgen    fgen    fgen    fgen    fgen    fgen    fgen    fgen	\ DC
    rfreg   rfreg   rfreg   rfreg   rfreg   rfreg   rfreg   rfreg
    fgen    drop    fgen    fgen    fgen    drop    fgen    fgen	\ DD
    freg1   freg1   freg1   freg1   freg    freg    drop    drop
    fgen    fgen    fgen    fgen    fgen    fgen    fgen    fgen	\ DE
    rfreg   rfreg   rfreg   rfreg   rfreg   rfreg   rfreg   rfreg
    fgen    drop    fgen    fgen    fgen    fgen    fgen    fgen	\ DF
    freg1   freg1   freg1   freg1   reg32   drop    drop    drop
;

: fpu		( op -- )
  7 {forth} and 4 << nextb swap
  ( mod/rm op )
  over 3 >> 7 {forth} and +
  over bf > 8 {forth} and +
  .fpu-opc .fpu-op
;

\ -----------------------------------------------------------------------------

: 2=0a					\ ( op -- ) aam aad
  drop nextb a <>  if  ." -ill"  then
;

\ -----------------------------------------------------------------------------

: ill					\ ( op -- op ) Illegal opcode
  ." Ill " dup .h2
;

: ill1					\ ( op1 -- op1 ) Illegal opcode
  ." Ill-0f " dup .h2
;

\ -----------------------------------------------------------------------------

: x00		( 00 -- 00 )		\ 0f00 Opcodes
  fetchb 3 >> 7 {forth} and exec:
    sldt str lldt ltr verr verw ill0f00 ill0f00
;

: x01		( 01 -- 01 )		\ 0f01 Opcodes
  fetchb 3 >> 7 {forth} and exec:
    sgdt sidt lgdt lidt smsw ill0f01 lmsw ill0f01
;

: .opc2		( op2 -- op2 )		\ Print Opcode
  dup exec:
    x00     x01     lar	    lsl     ill1    ill1    clts    ill1	\ 00
    ill1    ill1    ill1    ill1    ill1    ill1    ill1    ill1
    ill1    ill1    ill1    ill1    ill1    ill1    ill1    ill1	\ 10
    ill1    ill1    ill1    ill1    ill1    ill1    ill1    ill1
    mov     mov     mov     mov     mov     ill1    mov     ill1	\ 20
    ill1    ill1    ill1    ill1    ill1    ill1    ill1    ill1
    ill1    ill1    ill1    ill1    ill1    ill1    ill1    ill1	\ 30
    ill1    ill1    ill1    ill1    ill1    ill1    ill1    ill1
    ill1    ill1    ill1    ill1    ill1    ill1    ill1    ill1	\ 40
    ill1    ill1    ill1    ill1    ill1    ill1    ill1    ill1
    ill1    ill1    ill1    ill1    ill1    ill1    ill1    ill1	\ 50
    ill1    ill1    ill1    ill1    ill1    ill1    ill1    ill1
    ill1    ill1    ill1    ill1    ill1    ill1    ill1    ill1	\ 60
    ill1    ill1    ill1    ill1    ill1    ill1    ill1    ill1
    ill1    ill1    ill1    ill1    ill1    ill1    ill1    ill1	\ 70
    ill1    ill1    ill1    ill1    ill1    ill1    ill1    ill1
    jo      jno     jb      jae     je      jne     jbe     ja		\ 80
    js      jns     jpe     jpo     jl      jge     jle     jg
    seto    setno   setb    setae   sete    setne   setbe   seta	\ 90
    sets    setns   setpe   setpo   setl    setge   setle   setg
    ill1    ill1    ill1    bt      ill1    ill1    ill1    ill1	\ A0
    ill1    ill1    ill1    ill1    ill1    ill1    ill1    imul
    ill1    ill1    ill1    ill1    ill1    ill1    movzb   movzw	\ B0
    ill1    ill1    bitops  ill1    bsf     bsr     ill1    ill1
    ill1    ill1    ill1    ill1    ill1    ill1    ill1    ill1	\ C0
    ill1    ill1    ill1    ill1    ill1    ill1    ill1    ill1
    ill1    ill1    ill1    ill1    ill1    ill1    ill1    ill1	\ D0
    ill1    ill1    ill1    ill1    ill1    ill1    ill1    ill1
    ill1    ill1    ill1    ill1    ill1    ill1    ill1    ill1	\ E0
    ill1    ill1    ill1    ill1    ill1    ill1    ill1    ill1
    ill1    ill1    ill1    ill1    ill1    ill1    ill1    ill1	\ F0
    ill1    ill1    ill1    ill1    ill1    ill1    ill1    ill1
;

: .op2					\ ( op1 -- op1 ) Print Operand
  dup exec:
    gen     gen     -g->r   -g->r   drop    drop    drop    drop	\ 00
    drop    drop    drop    drop    drop    drop    drop    drop
    drop    drop    drop    drop    drop    drop    drop    drop	\ 10
    drop    drop    drop    drop    drop    drop    drop    drop
    cr->r   dr->r   r->cr   r->dr   tr->r   drop    r->tr   drop	\ 20
    drop    drop    drop    drop    drop    drop    drop    drop
    drop    drop    drop    drop    drop    drop    drop    drop	\ 30
    drop    drop    drop    drop    drop    drop    drop    drop
    drop    drop    drop    drop    drop    drop    drop    drop	\ 40
    drop    drop    drop    drop    drop    drop    drop    drop
    drop    drop    drop    drop    drop    drop    drop    drop	\ 50
    drop    drop    drop    drop    drop    drop    drop    drop
    drop    drop    drop    drop    drop    drop    drop    drop	\ 60
    drop    drop    drop    drop    drop    drop    drop    drop
    drop    drop    drop    drop    drop    drop    drop    drop	\ 70
    drop    drop    drop    drop    drop    drop    drop    drop
    rel     rel     rel     rel     rel     rel     rel     rel		\ 80
    rel     rel     rel     rel     rel     rel     rel     rel
    gen8    gen8    gen8    gen8    gen8    gen8    gen8    gen8	\ 90
    gen8    gen8    gen8    gen8    gen8    gen8    gen8    gen8
    drop    drop    drop    g->r    drop    drop    drop    drop	\ A0
    drop    drop    drop    drop    drop    drop    drop    g->r
    drop    drop    drop    drop    drop    drop    g->r    g->r	\ B0
    drop    drop    $8->g   drop    g->r    g->r    drop    drop
    drop    drop    drop    drop    drop    drop    drop    drop	\ C0
    drop    drop    drop    drop    drop    drop    drop    drop
    drop    drop    drop    drop    drop    drop    drop    drop	\ D0
    drop    drop    drop    drop    drop    drop    drop    drop
    drop    drop    drop    drop    drop    drop    drop    drop	\ E0
    drop    drop    drop    drop    drop    drop    drop    drop
    drop    drop    drop    drop    drop    drop    drop    drop	\ F0
    drop    drop    drop    drop    drop    drop    drop    drop
;

\	.inst2			( opc -- )
\
\		Display 2 Byte instruction.
\
: .inst2
  drop nextb .opc2 .op2
;

\ -----------------------------------------------------------------------------

\	.opc1			( op -- op )
\
\		Print 1 byte opcode
\
: .opc1
  dup exec:
    add    add    add    add    add    add    push   pop	\ 00
    or     or     or     or     or     or     push   null
    adc    adc    adc    adc    adc    adc    push   pop	\ 10
    sbb    sbb    sbb    sbb    sbb    sbb    push   pop
    and    and    and    and    and    and    %es    daa	\ 20
    sub    sub    sub    sub    sub    sub    %cs    das
    xor    xor    xor    xor    xor    xor    %ss    aaa	\ 30
    cmp    cmp    cmp    cmp    cmp    cmp    %ds    aas
    inc    inc    inc    inc    inc    inc    inc    inc	\ 40
    dec    dec    dec    dec    dec    dec    dec    dec
    push   push   push   push   push   push   push   push	\ 50
    pop    pop    pop    pop    pop    pop    pop    pop
    pusha  popa   bound  arpl   %fs    %gs    data16 addr16	\ 60
    push   imul   push   imul   ins    ins    outs   outs
    jo     jno    jb     jae    je     jne    jbe    ja		\ 70
    js     jns    jpe    jpo    jl     jge    jle    jg
    null   null   null   null   test   test   xchg   xchg	\ 80
    mov    mov    mov    mov    mov    lea    mov    pop
    nop    xchgl  xchgl  xchgl  xchgl  xchgl  xchgl  xchgl	\ 90
    null   null   lcall  fwait  pushfl popfl  sahf   lahf
    mov    mov    mov    mov    movs   movs   cmps   cmps	\ A0
    test   test   stos   stos   lods   lods   scas   scas
    mov    mov    mov    mov    mov    mov    mov    mov	\ B0
    mov    mov    mov    mov    mov    mov    mov    mov
    shifts shifts ret    ret    les    lds    mov    mov	\ C0
    enter  leave  lret   lret   int    int    into   iret
    shifts shifts shifts shifts aam    aad    falc   xlat	\ D0
    null   null   null   null   null   null   null   null
    loopne loope  loop   jecxz  in     in     out    out	\ E0
    call   jmp    ljmp   jmp    in     in     out    out
    lock   ill    repne  repe   hlt    cmc    null   null	\ F0
    clc    stc    cli    sti    cld    std    null   null
;

\	.op1			( op -- )
\
\		Print operand for opcode.
: .op1
  dup exec:
    +r->g  +r->g  +g->r  +g->r  $->a   $->a   sreg   sreg	\ 00
    +r->g  +r->g  +g->r  +g->r  $->a   $->a   sreg   .inst2
    +r->g  +r->g  +g->r  +g->r  $->a   $->a   sreg   sreg	\ 10
    +r->g  +r->g  +g->r  +g->r  $->a   $->a   sreg   sreg
    +r->g  +r->g  +g->r  +g->r  $->a   $->a   drop   drop	\ 20
    +r->g  +r->g  +g->r  +g->r  $->a   $->a   drop   drop
    +r->g  +r->g  +g->r  +g->r  $->a   $->a   drop   drop	\ 30
    +r->g  +r->g  +g->r  +g->r  $->a   $->a   drop   drop
    r16/32 r16/32 r16/32 r16/32 r16/32 r16/32 r16/32 r16/32	\ 40
    r16/32 r16/32 r16/32 r16/32 r16/32 r16/32 r16/32 r16/32
    r16/32 r16/32 r16/32 r16/32 r16/32 r16/32 r16/32 r16/32	\ 50
    r16/32 r16/32 r16/32 r16/32 r16/32 r16/32 r16/32 r16/32
    p+wl   p+wl   g->r   Wr->rm drop   drop   drop   drop	\ 60
    $val   $,rm,a $val   $,rm,a dx->di dx->di si->dx si->dx
    rel8   rel8   rel8   rel8   rel8   rel8   rel8   rel8	\ 70
    rel8   rel8   rel8   rel8   rel8   rel8   rel8   rel8
    O$->rm O$->rm O$->rm O$->rm +g->r  +g->r  +g->r  +g->r	\ 80
    +r->g  +r->g  +g->r  +g->r  s->g   +g->r  g->s   +gen
    drop   r->a   r->a   r->a   r->a   r->a   r->a   r->a	\ 90
    conv   conv2  inter  drop   drop   drop   drop   drop
    d->a   d->a   a->d   a->d   si->di si->di si->di si->di	\ A0
    $->a   $->a   ax->di ax->di si->ax si->ax ax->di ax->di
    $->r   $->r   $->r   $->r   $->r   $->r   $->r   $->r	\ B0
    $->r   $->r   $->r   $->r   $->r   $->r   $->r   $->r
    $8->g  $8->g  $16    drop   rm->R  rm->R  $->rm  $->rm	\ C0
    $16,$8 drop   $16    drop   int3   intX   drop   drop
    +gen   +gen   cl->g  cl->g  2=0a   2=0a   drop   drop	\ D0
    fpu    fpu    fpu    fpu    fpu    fpu    fpu    fpu
    rel8   rel8   rel8   rel8   8port  8port  8port  8port	\ E0
    rel    rel    inter  rel8   (port) (port) (port) (port)
    drop   drop   drop   drop   drop   drop   f6/f7  f6/f7	\ F0
    drop   drop   drop   drop   drop   drop   fe-inst ff-inst
;

\	.inst1			( op -- )
\
\		Display 1 Byte instruction.
\
: .inst1
  .opc1 .op1
;

\	.inst
\
: .inst
  !csp
  2 spaces
  \ .s cr
  nextb dup
  \ .s cr
  case					\ Decode prefixes
    0f  of  .inst2		endof
    26	of  %es .: drop		endof
    2e	of  %cs .: drop		endof
    36	of  %ss .: drop		endof
    3e	of  %ds .: drop		endof
    64	of  %fs .: drop		endof
    65	of  %gs .: drop		endof
    66  of  data16 1 to osize drop	endof
    67  of  addr16 1 to asize drop	endof
    f0	of  lock drop		endof
    dup of  .inst1
	    0 to osize
	    0 to asize
	endof
  endcase
  ?csp
  \ .s cr abort
;

false [if]

: tdis
  pad dup cp !
  cr .inst				\ Display instruction
  28 >col [char] \ emit			\ Ident to column
  dup .h8
  \ dup nexti over - .
  cp @ over -
  bounds
  do
    i c@ (.h2)				\ Display bytes
  {forth} loop
  \ key esc# = abort" Stopped"
  ?keypause
;

: txx
  100 0
  do
    i pad 1+ c!
    i 6 >> 3 =				\ Mod-field 3
    if
      tdis
    else
      i 7 {forth} and 4 =		\ S-I-B field
      if
	100 0
	do
	  i pad 2 + c!
	  tdis
	{forth} loop
      else
	tdis
      then
    then
  {forth} loop
;

: tDW
  dup 4 + swap
  do
    i pad c! txx
  {forth} loop
;

: t1
  pad c! tdis
;

: t1W
  dup t1 1+ t1
;

: xtxx
  pad 1+ c!
  100 0
  do
    i pad 2 + c!
    i 6 >> 3 =				\ Mod-field 3
    if
      tdis
    else
      i 7 {forth} and 4 =		\ S-I-B field
      if
	100 0
	do
	  i pad 3 + c!
	  tdis
	{forth} loop
      else
	tdis
      then
    then
  {forth} loop
;

: xt1
  pad 1+ c! tdis
;

: xtx
  pad 1+ c!
  100 0
  do
    i pad 2 + c! tdis
  {forth} loop
;

: xtest					\ 2 Byte opcodes test
  pad c!
\  0 xtxx
\  1 xtxx
\  2 xtxx
\  3 xtxx
\  6 xt1
\  20 xtx
\  21 xtx
\  22 xtx
\  23 xtx
\  24 xtx
\  26 xtx
  80 xt1 81 xt1 82 xt1 83 xt1 84 xt1 85 xt1 86 xt1 87 xt1
  88 xt1 89 xt1 8a xt1 8b xt1 8c xt1 8d xt1 8e xt1 8f xt1
  90 xtxx 91 xtxx 92 xtxx 93 xtxx 94 xtxx 95 xtxx 96 xtxx 97 xtxx
  98 xtxx 99 xtxx 9a xtxx 9b xtxx 9c xtxx 9d xtxx 9e xtxx 9f xtxx
;

: ftx
  pad c!
  100 0
  do
    i pad 1+ c! tdis
  {forth} loop
;

: ftest
  d8 ftx
  d9 ftx
  da ftx
  db ftx
  dc ftx
  dd ftx
  de ftx
  df ftx
  txx
;

: test
  hex
\  0 tDW
\  4 t1W
\  6 t1
\  7 t1
\  8 tDW
\  c t1W
\  e t1
  f xtest
;

[then]

also forth definitions

\\	dis		( addr -- )
\\
: dis
  cp !
  base @				\ Save base
  hex
  begin
    cp @
    cr .inst				\ Display instruction
    28 >col [char] \ emit		\ Ident to column
    dup .h8
    cp @ over -
    bounds 2dup
    do
      i c@ (.h2)			\ Display bytes
    {forth} loop
    40 >col space
    do
      i c@ dup bl 7f within		\ Display chars
      0=if
	drop [char] .
      then
      emit
    {forth} loop
    key dup [char] > =
    if
      last-addr cp !
    then
    esc# =				\ 1 instruction stepping
    \ keypause?				\ Pause if key pressed
  until
  base !
;

: /symbolic-on
  true to symbolic
;

: /symbolic-off
  false to symbolic
;

only forth also definitions
decimal

false [if]

D	Direction		0 Register, Register/Memory
				1 Register/Memory, Register
W	Wordsize		0 Byte size ( 8 bit )
				1 Word size ( 16 or 32 bit )
S	Size			0 Operand size
				1 Byte size
cccc	Condition		0000
				0001
				0010
				0011
				0100
				0101
				0110
				0111
				1000
				1001
				1010
				1011
				1100
				1101
				1110
				1111

1 Byte    2 Byte      3 Byte	4 Byte

0000 00DW mod-reg-r/m			add	mod-reg-r/m
0000 010W imm8/imm32			add	imm, accu
000o o110				push	%es
000o o111				pop	%es
0000 10DW mod-reg-r/m			or	mod-reg-r/m
0000 110W imm8/imm32			or	imm, accu
000o i110				push	%cs
0000 1111				= 2 Byte
0000 1111 0000 0000 mod-000-r/m		sldt	mod-r/m
		    mod-001-r/m		str	mod-r/m
		    mod-010-r/m		lldt	mod-r/m
		    mod-011-r/m		ltr	mod-r/m
		    mod-100-r/m		verr	mod-r/m
		    mod-101-r/m		verw	mod-r/m
		    mod-110-r/m		unused	mod-r/m
		    mod-111-r/m		unused	mod-r/m
0000 1111 0000 0001 mod-000-r/m		sgdt	mod-r/m
		    mod-001-r/m		sidt	mod-r/m
		    mod-010-r/m		lgdt	mod-r/m
		    mod-011-r/m		lidt	mod-r/m
		    mod-100-r/m		smsw	mod-r/m
		    mod-101-r/m		unused	mod-r/m
		    mod-110-r/m		lmsw	mod-r/m
		    mod-111-r/m		unused	mod-r/m
0000 1111 0000 0010 mod-reg-r/m		lar	gen, reg
0000 1111 0000 0011 mod-reg-r/m		lsl	gen, reg
0000 1111 0000 0100			illegal
0000 1111 0000 0101			illegal
0000 1111 0000 0110			clts
0000 1111 0000 0111			illegal
0000 1111 000X 1XXX			illegal
0000 1111 0010 0000 11-creg-reg		movl	creg,reg
0000 1111 0010 0001 11-dreg-reg		movl	dreg,reg
0000 1111 0010 0010 11-creg-reg		movl	reg,creg
0000 1111 0010 0011 11-dreg-reg		movl	reg,dreg
0000 1111 0010 0100 11-treg-reg		movl	treg,reg
0000 1111 0010 0101			illegal
0000 1111 0010 0110 11-treg-reg		movl	reg,treg
0000 1111 0010 0111			illegal
0000 1111 001X 1XXX			illegal
0000 1111 01XX XXXX			illegal
0000 1111 1000 cccc 2/4 byte		jcc	displacement
0000 1111 1001 cccc mod-000-r/m		setcc	mod-reg-r/m
	  1001 xxxx mod-???-r/m		illegal
0000 1111 10io o000			push	%fs
0000 1111 10io o001			pop	%fs
0000 1111 1010 0010			illegal
0000 1111 1010 0011 mod-reg-r/m		bt	mod-reg-r/m
0000 1111 1010 0100 mod-reg-r/m	imm8	shld	imm8,mod-reg-r/m
0000 1111 1010 0101 mod-reg-r/m		shld	%cl,mod-reg-r/m
0000 1111 1010 0110			illegal
0000 1111 1010 0111			illegal
0000 1111 10io i000			push	%gs
0000 1111 10io i001			pop	%gs
0000 1111 1010 1011 mod-reg-r/m		bts	reg,gen
0000 1111 1010 1100 mod-reg-r/m	imm8	shrd	imm8,reg,gen
0000 1111 1010 1101 mod-reg-r/m		shrd	%cl,reg,gen
0000 1111 1010 1110			illegal
0000 1111 1010 1111 mod-reg-r/m		imul	gen,reg
0000 1111 1011 0000			illegal
0000 1111 1011 0001			illegal
0000 1111 1011 0010 mod-reg-r/m		lss	gen,reg
0000 1111 1011 0011 mod-reg-r/m		btr	reg,gen
0000 1111 1011 0100 mod-reg-r/m		lfs	gen,reg
0000 1111 1011 0101 mod-reg-r/m		lgs	gen,reg
0000 1111 1011 011W mod-reg-r/m		movz[bw][wl]	gen,reg
0000 1111 1011 1000			illegal
0000 1111 1011 1001			illegal
0000 1111 1011 1010 mod-100-r/m	imm8	bt	imm,gen
	  1011 1010 mod-101-r/m	imm8	bts	imm,gen
	  1011 1010 mod-110-r/m	imm8	btr	imm,gen
	  1011 1010 mod-111-r/m	imm8	btc	imm,gen
0000 1111 1011 1011 mod-reg-r/m		btc	reg,gen
0000 1111 1011 1100 mod-reg-r/m		bsf	gen,reg
0000 1111 1011 1101 mod-reg-r/m		bsr	gen,reg
0000 1111 1011 111W mod-reg-r/m		movs[bw][wl]	gen,reg
0000 1111 11XX XXXX			illegal
0001 00DW mod-reg-r/m			adc	mod-reg-r/m
0001 010W imm8/imm32			adc	imm, accu
000i o110				push	%ss
000i o111				pop	%ss
0001 10DW mod-reg-r/m			sbb	mod-reg-r/m
0001 110W imm8/imm32			sbb	imm, accu
000i i110				push	%ds
000i i111				pop	%ds
0010 00DW mod-reg-r/m			and	mod-reg-r/m
0010 010W imm8/imm32			and	imm, accu
001o o110				es: <prefix>
0010 0111				daa
0010 00DW mod-reg-r/m			sub	mod-reg-r/m
0010 010W imm8/imm32			sub	imm, accu
001o i110				cs: <prefix>
0010 1111				das
0011 00DW mod-reg-r/m			xor	mod-reg-r/m
0011 010W imm8/imm32			xor	imm, accu
001i o110				ss: <prefix>
0011 0111				aaa
0011 10DW mod-reg-r/m			cmp	mod-reg-r/m
0011 110W imm8/imm32			cmp	imm, accu
001i i110				ds: <prefix>
0011 1111				aas
0100 0reg				inc	reg
0100 1reg				dec	reg
0101 0reg				push	reg
0101 1reg				pop	reg
0110 0000				pusha
0110 0001				popa
0110 0010 mod-reg-r/m			bound	gen,reg
0110 0011 mod-reg-r/m			arpl	reg,gen
0110 0100				fs: <prefix>
0110 0101				gs: <prefix>
0110 0110				data16 <prefix>
0110 0111				addr16 <prefix>
0110 10S0 imm8/32			push	imm
0110 10S1 mod-reg-r/m imm8/32		imul	imm,gen,reg
0110 110W				ins	(dx), (di)
0110 111W				outs	(si), (dx)
0111 cccc dis8				jcc	displacement
1000 00SW mod-000-r/m imm8/32		add	imm,gen
1000 00SW mod-001-r/m imm8/32		or	imm,gen
1000 00SW mod-010-r/m imm8/32		adc	imm,gen
1000 00SW mod-011-r/m imm8/32		sbb	imm,gen
1000 00SW mod-100-r/m imm8/32		and	imm,gen
1000 00SW mod-101-r/m imm8/32		sub	imm,gen
1000 00SW mod-110-r/m imm8/32		xor	imm,gen
1000 00SW mod-111-r/m imm8/32		cmp	imm,gen
1000 010W mod-reg-r/m			test	gen,reg
1000 011W mod-reg-r/m			xchg	gen,reg
1000 10DW mod-reg-r/m			mov	mod-reg-r/m
1000 11D0 mod-sreg-r/m			mov	mod-sreg-r/m
1000 1101 mod-reg-r/m			lea	gen,reg
1000 1111 mod-000-r/m			pop	gen
1000 1111 mod-???-r/m			illegal
1001 0reg				xchg	reg, accu
1001 100W				cwtl
1001 1001				cltd
1001 1010 seg16, disp			lcall	seg16, disp
1001 1011				fwait
1001 1100				pushfl
1001 1101				popfl
1001 1110				sahf
1001 1111				lahf
1010 000W disp				mov	accu, disp
1010 001W disp				mov	disp, accu
1010 010W				movs	(si), (di)
1010 011W				cmps	(si), (di)
1010 100W imm				test	imm, accu
1010 101W				stos	accu, (di)
1010 110W				lods	(si), accu
1010 111W				scas	accu, (di)
1011 Wreg imm				mov	imm, reg
1100 000W mod-000-r/m imm8		rol	imm8, gen
	  mod-001-r/m imm8		ror	imm8, gen
	  mod-010-r/m imm8		rcl	imm8, gen
	  mod-011-r/m imm8		rcr	imm8, gen
	  mod-100-r/m imm8		shl	imm8, gen
	  mod-101-r/m imm8		shr	imm8, gen
	  mod-110-r/m imm8		unused	imm8, gen
	  mod-111-r/m imm8		sar	imm8, gen
1100 0010 imm16				ret	imm16
1100 0011				ret
1100 0100 mod-reg-r/m			les	gen, reg
1100 0101 mod-reg-r/m			les	gen, reg
1100 011W mod-000-r/m imm		mov	imm, gen
1100 011W mod-???-r/m imm		illegal
1100 1000 disp16, level8		enter	disp16, level8
1100 1001				leave
1100 1010 imm16				lret	imm16
1100 1011				lret
1100 1100				int	$3
1100 1101 imm8				int	imm8
1100 1110				into
1100 1111				iret
1101 001W mod-000-r/m			rol	%cl,gen
	  mod-001-r/m			ror	%cl,gen
	  mod-010-r/m			rcl	%cl,gen
	  mod-011-r/m			rcr	%cl,gen
	  mod-100-r/m			shl	%cl,gen
	  mod-101-r/m			shr	%cl,gen
	  mod-110-r/m			unused	%cl,gen
	  mod-111-r/m			sar	%cl,gen
1101 0100 0000 1010			aam
1101 0101 0000 1010			aad
1101 0110				falc
1101 0111				xlat
1101 1000 387
	  fmod-000-r/m			fadds	gen
	  fmod-001-r/m			fmuls	gen
	  fmod-010-r/m			fcoms	gen
	  fmod-011-r/m			fcomps	gen
	  fmod-100-r/m			fsubs	gen
	  fmod-101-r/m			fsubrs	gen
	  fmod-110-r/m			fdivs	gen
	  fmod-110-r/m			fdivrs	gen
	  11-000-freg			fadds	freg, st
	  11-001-freg			fmuls	freg, st
	  11-010-freg			fcoms	freg, st
	  11-011-freg			fcomps	freg, st
	  11-100-freg			fsubs	freg, st
	  11-101-freg			fsubrs	freg, st
	  11-110-freg			fdivs	freg, st
	  11-110-freg			fdivrs	freg, st
1101 1001 387
	  fmod-000-r/m			flds	gen
	  fmod-001-r/m			fillegal
	  fmod-010-r/m			fsts	gen
	  fmod-011-r/m			fstps	gen
	  fmod-100-r/m			fldenv	gen
	  fmod-101-r/m			fldcw	gen
	  fmod-110-r/m			fnstenv	gen
	  fmod-111-r/m			fnstcw	gen
	  11-000-freg			fld	freg
	  11-001-freg			fxch	freg
	  11-010-freg			fnop
	  ??
	  11-011-freg			fstp	freg
	  11-100-freg			fchs
	  ??
	  11-101-freg			fld1
	  ??
	  11-110-freg			f2xm1
	  ??
	  11-110-freg			fprem
	  ??
1101 1010 387
	  fmod-000-r/m			fiaddl	gen
	  fmod-001-r/m			fimull	gen
	  fmod-010-r/m			ficoml	gen
	  fmod-011-r/m			ficompl	gen
	  fmod-100-r/m			fisubl	gen
	  fmod-101-r/m			fisubrl	gen
	  fmod-110-r/m			fidivl	gen
	  fmod-111-r/m			fidivrl	gen
1101 1011 387
	  fmod-000-r/m			fildl	gen
	  fmod-001-r/m			fillegal
	  fmod-010-r/m			fistl	gen
	  fmod-011-r/m			fistpl	gen
	  fmod-100-r/m			fillegal
	  fmod-101-r/m			fldt	gen
	  fmod-110-r/m			fillegal
	  fmod-111-r/m			fstpt	gen
1101 1100 387
	  fmod-000-r/m			faddl	gen
	  fmod-001-r/m			fmull	gen
	  fmod-010-r/m			fcoml	gen
	  fmod-011-r/m			fcompl	gen
	  fmod-100-r/m			fsubl	gen
	  fmod-101-r/m			fsubrl	gen
	  fmod-110-r/m			fdivl	gen
	  fmod-111-r/m			fdivrl	gen
1101 1101 387
	  fmod-000-r/m			fldl	gen
	  fmod-001-r/m			fillegal
	  fmod-010-r/m			fstl	gen
	  fmod-011-r/m			fstpl	gen
	  fmod-100-r/m			frstor	gen
	  fmod-101-r/m			fillegal
	  fmod-110-r/m			fnsave	gen
	  fmod-111-r/m			fnstsw	gen
1101 1110 387
	  fmod-000-r/m			fiadd	gen
	  fmod-001-r/m			fimul	gen
	  fmod-010-r/m			ficom	gen
	  fmod-011-r/m			ficomp	gen
	  fmod-100-r/m			fisub	gen
	  fmod-101-r/m			fisubr	gen
	  fmod-110-r/m			fidiv	gen
	  fmod-111-r/m			fidivr	gen
1101 1111 387
	  fmod-000-r/m			fild	gen
	  fmod-001-r/m			fillegal
	  fmod-010-r/m			fist	gen
	  fmod-011-r/m			fistp	gen
	  fmod-100-r/m			fbld	gen
	  fmod-101-r/m			fildll	gen
	  fmod-110-r/m			fbstp	gen
	  fmod-111-r/m			fistpll	gen
1110 0000 disp8				loopnz	disp8
1110 0001 disp8				loopz	disp8
1110 0010 disp8				loop	disp8
1110 0011 disp8				jcxz	disp8
1110 010w port8				in	port8,accu
1110 011w port8				out	accu,port8
1110 1000 disp				call	disp
1110 1001 disp				jmp	disp
1110 1010 seg16, disp			ljmp	seg16, disp
1110 1011 disp8				jmp	disp8
1110 110W				in	(dx), accu
1110 111W				out	accu,(dx)
1111 0000				lock	<prefix>
1111 0001				illegal
1111 0010				repnz
1111 0011				repz
1111 0100				hlt
1111 0101				cmc
1111 011W mod-000-r/m imm		test	imm,gen
	  mod-001-r/m			illegal
	  mod-010-r/m			not	gen
	  mod-011-r/m			neg	gen
	  mod-100-r/m			mul	gen , accu
	  mod-101-r/m			imul	gen , accu
	  mod-110-r/m			div	gen , accu
	  mod-111-r/m			idiv	gen , accu
1111 1000				clc
1111 1001				stc
1111 1010				cli
1111 1011				sti
1111 1100				cld
1111 1101				std
1111 1110				illegal
1111 1111 mod-000-r/m			inc	gen
	  mod-001-r/m			dec	gen
	  mod-010-r/m			call	*gen
	  mod-011-r/m			lcall	*gen
	  mod-100-r/m			jmp	*gen
	  mod-101-r/m			ljmp	*gen
	  mod-110-r/m			push	gen
	  mod-111-r/m			illegal







[then]

\	FIXME:
\	8F	pop	Illegals not decoded!
\ -----------------------------------------------------------------------------
\ Prefix 0f:
\ bt btc bts imul jcc
\ lfs lgs lss
\ movsb movzb
\ mov system registers
\
\ FIXME:
\ jcxz


