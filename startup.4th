\
\	ALE Forth Version 2.04
\
\		Copyright (c) 1991,1992 by Lutz Sammer.
\
\		Bug-reports via usenet: johns
\
\	File:	startup.4th
\
\		Forth startup code.
\
\	RCS:
\	$Id:$
\	$Log:$

\ -----------------------------------------------------------------------------

target-compiler				\ Start target compiler

\ =============================================================================
\	Startup code.
\ =============================================================================

\	Forth kernel startup code:	( -- env argv argc ) ( R: -- )
\
\		Inits stacks.  Gets environment and arguments.
\
\		Next lines are emited directly to the assembler file.
\
exist target-unix [if]
					\ / Is assembler comment !!!
/ps=sp [if]

startup-code:

	.text
.globl	_start
_start:
	movl	%esp, %esi		/ Build data/parameter stack
	movl	%esi, Drp0

	subl	$ 65536 , %esp		/ 64k return stack
	movl	%esp, Dsp0		/ Processor-stack = data stack

	movl	$ end , Ddp0		/ End of kernel data
	movl	$ etext , Detext	/ End of kernel text

	movl	(%esi), %ebx		/ argc
	leal	4 (%esi), %edx		/ argv
	leal	8 (%esi, %ebx, 4), %eax	/ env

	pushl	%eax			/ -> stack env
	pushl	%edx			/ -> stack argv

	jmp	Xcold			/ Forth-level startup code

startup-code;

[else]

startup-code:

	.text
.globl	_start
_start:
	movl	%esp, %esi		/ Build data/parameter stack
	movl	%esi, Dsp0

	subl	$ 65536 , %esp		/ 64k data stack
	movl	%esp, Drp0		/ Processor-stack = return stack

	movl	$ end , Ddp0		/ End of kernel data
	movl	$ etext , Detext	/ End of kernel text

	movl	(%esi), %ebx		/ argc
	leal	4 (%esi), %edx		/ argv
	leal	8 (%esi, %ebx, 4), %eax	/ env

	leal	-8 (%esi), %esi
	movl	%eax, 4 (%esi)		/ -> stack env
	movl	%edx, (%esi)		/ -> stack argv

	jmp	Xcold			/ Forth-level startup code

startup-code;

[then]
[then]

exist target-msdos [if]

/ps=sp [if]

startup-code:

	.data
.globl	buffer
.comm	buffer,1			/ Emit buffer

	.text
.globl	_main
_main:
	movl	%esp, %esi		/ Build data/parameter stack
	movl	%esi, Drp0

	subl	$ 65536 , %esp		/ 64k return stack
	movl	%esp, Dsp0		/ Processor-stack = data stack

	movl	$ _end , Ddp0		/ End of kernel data
	movl	$ _etext , Detext	/ End of kernel text

	movl	(%esi), %ebx		/ argc
	movl	4 (%esi), %edx		/ argv
	movl	8 (%esi), %eax		/ env

	pushl	%eax			/ -> stack env
	pushl	%edx			/ -> stack argv

	jmp	Xcold			/ Forth-level startup code
startup-code;

[else]

startup-code:

	.data
.globl	buffer
.comm	buffer,1			/ Emit buffer

	.text
.globl	_main
_main:
	movl	%esp, %esi		/ Build data/parameter stack
	movl	%esi, Dsp0

	subl	$ 65536 , %esp		/ 64k data stack
	movl	%esp, Drp0		/ Processor-stack = return stack

	movl	$ _end , Ddp0		/ End of kernel data
	movl	$ _etext , Detext	/ End of kernel text

	movl	(%esi), %ebx		/ argc
	movl	4 (%esi), %edx		/ argv
	movl	8 (%esi), %eax		/ env

	pushl	%eax			/ -> stack env
	pushl	%edx			/ -> stack argv

	jmp	Xcold			/ Forth-level startup code
startup-code;

[then]
[then]
