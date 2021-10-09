\
\	ALE Forth Version 2.04
\
\		Copyright (c) 1991,1992 by Lutz Sammer.
\
\		Bug-reports via usenet: johns
\
\	File:	msdos.4th
\
\		Forth kernel msdos operating system support.
\

\ -----------------------------------------------------------------------------
\	MSDOS system calls
\ -----------------------------------------------------------------------------

compiler definitions

\\	(bye)			( n -- )				MSDOS
\\
\\		Return control to calling process.
\\
code (bye)
	movb	tos-l, %al
	movb	$ $4C , %ah
	int	$ 33 <cr>		\ End process
	hlt				\ I hope never returns
end-code

\\	creat			( u 0-addr -- handle ior )		MSDOS
\\
\\		Create a file for reading or writing.
\\
/ps=sp [if]

code creat
					\ tos 0-addr
	popl	%eax			\ u

	pushl	%ecx
	pushl	%esi
	pushl	%edi

	movl	%eax, %ecx
	movw	$ $FF01 , %ax		\ Turbo assist: creat
	int	$ 33 <cr>

	popl	%edi
	popl	%esi
	popl	%ecx

	pushl	%eax			\ handle
	jc	1f			\ Error in %eax
	xorl	%eax, %eax
1:
	movl	%eax, tos
end-code

[else]

code creat
	pushl	%ecx
					\ tos 0-addr
	movl	(%esi), %ecx		\ u

	pushl	%esi
	pushl	%edi

	movw	$ $FF01 , %ax		\ Turbo assist: creat
	int	$ 33 <cr>

	popl	%edi
	popl	%esi
	popl	%ecx

	movl	%eax, (%esi)		\ handle
	jc	1f			\ Error in %eax
	xorl	%eax, %eax
1:
	movl	%eax, tos
end-code

[then]

\\	open			( u1 u2 0-addr -- handle ior )		MSDOS
\\
\\		Open a file for reading or writing.
\\
/ps=sp [if]

true [if]
code open
					\ tos 0-addr
	popl	%eax			\ u2
	popl	%edx			\ u1

	pushl	%ecx
	pushl	%esi
	pushl	%edi

	movl	%eax, %ecx
	\ orw	$ $4000 , %ecx		\ Text mode
	\ orw	$ $8000 , %ecx		\ Binary mode
	movw	$ $FF02 , %ax		\ Turbo assist: open
	int	$ 33 <cr>

	popl	%edi
	popl	%esi
	popl	%ecx

	pushl	%eax			\ handle
	jc	1f			\ Error in %eax
	xorl	%eax, %eax
1:
	movl	%eax, tos
end-code
[else]
code open
					\ tos 0-addr
	popl	%eax			\ u2
	popl	%edx			\ u1

	pushl	%ecx
	pushl	%esi
	pushl	%edi

	movl	tos, %edx		\ Pathname
	andw	$ $300 , %ax
	jz		1f
	movl	%eax, %ecx
	movb	$ $3C , %ah		\ Create handle
	jmp		2f
1:
	andb	$ 7 , %al		\ Access mode
	movb	$ $3D , %ah		\ Open handle
2:
	int	$ 33 <cr>

	popl	%edi
	popl	%esi
	popl	%ecx

	pushl	%eax			\ handle
	jc	1f			\ Error in %eax
	xorl	%eax, %eax
1:
	movl	%eax, tos
end-code
[then]

[else]

code open
	pushl	%ecx
					\ tos 0-addr
	movl	(%esi), %ecx		\ u2
	addl	$ 4 , %esi
	pushl	%edi
	movl	(%esi),	%edx		\ u1

	pushl	%esi

	movw	$ $FF02 , %ax		\ Turbo assist: open
	int	$ 33 <cr>

	popl	%esi
	popl	%edi
	popl	%ecx

	movl	%eax, (%esi)		\ handle
	jc	1f			\ Error in %eax
	xorl	%eax, %eax
1:
	movl	%eax, tos
end-code

[then]

\\	close			( handle -- ior )			MSDOS
\\
\\		Close a file.
\\
code close
	pushl	%ecx
	pushl	%esi
	pushl	%edi

	movb	$ $3E , %ah
	int	$ 33 <cr>		\ Close handle

	popl	%edi
	popl	%esi
	popl	%ecx

	jc	1f			\ Error in %eax
	xorl	%eax, %eax
1:
	movl	%eax, tos
end-code

\\	read			( u1 c-addr handle -- u2 ior )		MSDOS
\\
\\		Read from a file.
\\
/ps=sp [if]

code read
					\ tos = handle
	popl	%edx			\ c-addr
	popl	%eax			\ u1

	pushl	%ecx
	pushl	%esi
	pushl	%edi

	movl	%eax, %ecx
	movb	$ 63 , %ah		\ $3F Read handle bx,cx,ds:dx
	int	$ 33 <cr>

	popl	%edi
	popl	%esi
	popl	%ecx

	pushl	%eax			\ u2
	jc	1f			\ Error in %eax
	xorl	%eax, %eax
1:
	movl	%eax, tos
end-code

[else]

code read
	pushl	%ecx
					\ tos = handle
	movl	(%esi), %edx		\ c-addr
	addl	$ 4 , %esi
	pushl	%edi
	movl	(%esi), %ecx		\ u1

	pushl	%esi

	movb	$ $3F , %ah		\ Read handle bx,cx,ds:dx
	int	$ 33 <cr>

	popl	%esi
	popl	%edi
	popl	%ecx

	movl	%eax, (%esi)		\ u2
	jc	1f			\ Error in %eax
	xorl	%eax, %eax
1:
	movl	%eax, tos
end-code

[then]

\\	write			( u1 c-addr handle -- u2 ior )		MSDOS
\\
\\		Write on a file.
\\
/ps=sp [if]

code write
					\ tos = handle
	popl	%edx			\ c-addr
	popl	%eax			\ u1

	pushl	%ecx
	pushl	%esi
	pushl	%edi

	movl	%eax, %ecx
	movb	$ $40 , %ah		\ Write handle bx,cx,ds:dx
	int	$ 33 <cr>

	popl	%edi
	popl	%esi
	popl	%ecx

	pushl	%eax			\ u2
	jc	1f			\ Error in %eax
	xorl	%eax, %eax
1:
	movl	%eax, tos
end-code

[else]

code write
	pushl	%ecx
					\ tos = handle
	movl	(%esi), %edx		\ c-addr
	addl	$ 4 , %esi
	pushl	%edi
	movl	(%esi), %ecx		\ u1

	pushl	%esi

	movb	$ $40 , %ah		\ Write handle bx,cx,ds:dx
	int	$ 33 <cr>

	popl	%esi
	popl	%edi
	popl	%ecx

	movl	%eax, (%esi)		\ u2
	jc	1f			\ Error in %eax
	xorl	%eax, %eax
1:
	movl	%eax, tos
end-code

[then]

\\	SEEK_SET		( -- n )				MSDOS
\\
\\		n is the first argument to SEEK.
\\		Set the seek pointer to offset bytes.
\\
0 constant SEEK_SET

\\	SEEK_CUR		( -- n )				MSDOS
\\
\\		n is the first argument to SEEK.
\\		Set the seek pointer to its current location plus offset.
\\
1 constant SEEK_CUR

\\	SEEK_END		( -- n )				MSDOS
\\
\\		n is the first argument to SEEK.
\\		Set the seek pointer to the size of the file plus offset.
\\
2 constant SEEK_END

\\	seek		( whence offset handle -- offset ior )		MSDOS
\\
\\		Move read/write file pointer.
\\
/ps=sp [if]

code seek
					\ tos = handle
	popl	%edx			\ offset
	popl	%eax			\ whence

	pushl	%ecx
	pushl	%esi
	pushl	%edi

	movl	%edx, %ecx
	shrl	$ 16 , %ecx
	andl	$ $FFFF , %edx		\ Calculate offset cx:dx

	movb	$ 66 , %ah		\ $42 Move file pointer al,bx,cx:dx
	int	$ 33 <cr>

	popl	%edi
	popl	%esi
	popl	%ecx

	jc	1f			\ Error in %eax

	andl	$ $FFFF , %eax		\ Calculate offset from dx:ax
	shll	$ 16 , %edx
	orl	%edx, %eax
	pushl	%eax
	xorl	tos, tos
	jmp	*(%esi)
1:
	pushl	%eax			\ offset
	movl	%eax, tos
end-code

[else]

code seek
	pushl	%ecx
					\ tos = handle
	movl	(%esi), %ecx		\ offset 
	addl	$ 4 , %esi
	pushl	%edi
	movl	(%esi), %eax		\ whence 

	pushl	%esi

	xorl	%edx, %edx
	movw	%ecx, %edx		\ Calculate offset cx:dx
	shrl	$ 16 , %ecx

	movb	$ $42 , %ah		\ Move file pointer al,bx,cx:dx
	int	$ 33 <cr>

	popl	%esi
	popl	%edi
	popl	%ecx

	jc	1f			\ Error in %eax

	andl	$ $FFFF , %eax		\ Calculate offset from dx:ax
	shll	$ 16 , %edx
	orl	%edx, %eax
	movl	%eax, (%esi)		\ offset
	xorl	tos, tos
	ret
1:
					\ (%esi) undefined
	movl	%eax, tos
end-code

[then]

\\	dupf		( handle1 -- handle2 ior )			MSDOS
\\
\\		Duplicate file handle.
\\
/ps=sp [if]

code dupf
	pushl	%ecx
	pushl	%esi
	pushl	%edi

	movb	$ $45 , %ah		\ Duplicate file handle %ebx
	int	$ 33 <cr>

	popl	%edi
	popl	%esi
	popl	%ecx

	pushl	%eax			\ New handle
	jc	1f			\ Error in %eax
	xorl	%eax, %eax
1:
	movl	%eax, tos
end-code

[else]

code dupf
	pushl	%ecx
	pushl	%esi
	pushl	%edi

	movb	$ $45 , %ah		\ Duplicate file handle %ebx
	int	$ 33 <cr>

	popl	%edi
	popl	%esi
	subl	$ 4 , %esi
	popl	%ecx

	movl	%eax, (%esi)		\ New handle
	jc	1f			\ Error in %eax
	xorl	%eax, %eax
1:
	movl	%eax, tos
end-code

[then]

\\	dup2		( handle1 handle2 -- ior )			MSDOS
\\
\\		Force duplicate file handle. handle2 to handle1.
\\
/ps=sp [if]

code dup2
	popl	%eax			\ handle1

	pushl	%ecx
	pushl	%esi
	pushl	%edi

	movl	%eax, %ecx

	movb	$ $46 , %ah		\ Force duplicate file handle bx,cx
	int	$ 33 <cr>

	popl	%edi
	popl	%esi
	popl	%ecx

	jc	1f			\ Error in %eax
	xorl	%eax, %eax
1:
	movl	%eax, tos
end-code

[else]

code dup2
	pushl	%ecx

	movl	(%esi), %ecx		\ handle1
	addl	$ 4 , %esi

	pushl	%esi
	pushl	%edi

	movb	$ $46 , %ah		\ Force duplicate file handle bx,cx
	int	$ 33 <cr>

	popl	%edi
	popl	%esi
	popl	%ecx

	jc	1f			\ Error in %eax
	xorl	%eax, %eax
1:
	movl	%eax, tos
end-code

[then]

\\	brk-val			( -- n )				MSDOS
\\
\\		n is the current break value.
\\
value brk-val

\\	brk			( end -- ior )				MSDOS
\\
\\		Change data segment space allocation.
\\
code brk
	movl	tos, Dbrk2Dval <cr>	\ Save end of allocate space
	pushl	%ecx
	pushl	%esi
	pushl	%edi
	movw	$ $4A00 , %ax
	int	$ 33 <cr>		\ Set end of process
	popl	%edi
	popl	%esi
	popl	%ecx

\	subl	$ 4 , %esi
\	pushl	%eax			\ Old end not returned
\	movl	%eax, (%esi)

	jc	1f
	xorl	%eax, %eax
1:
	movl	%eax, tos
end-code

\\	time			( -- time ior )				MSDOS
\\
\\		Get time.
\\
/ps=sp [if]

code time
	push-tos
	pushl	%ecx
	pushl	%esi
	pushl	%edi

	subl	$ 8 , %esp		\ timeval
	movl	%esp, tos
	xorl	%ecx, %ecx
	movw	$ 65284 , %ax		\ $FF04 Turbo assist: gettimeofday
	int	$ 33 <cr>

	popl	%eax			\ tv_sec
	popl	%edx			\ tv_usec

	popl	%edi
	popl	%esi
	popl	%ecx

	pushl	%eax			\ seconds
	jc	1f			\ Error in %eax
	xorl	%eax, %eax
1:
	movl	%eax, tos
end-code

[else]

code time
	push-tos
	pushl	%ecx
	pushl	%esi
	pushl	%edi

	subl	$ 8 , %esp		\ timeval
	movl	%esp, tos
	xorl	%ecx, %ecx
	movw	$ $FF04 , %ax		\ Turbo assist: gettimeofday
	int	$ 33 <cr>

	popl	%eax			\ tv_sec
	popl	%edx			\ tv_usec

	popl	%edi
	popl	%esi
	subl	$ 4 , %esi
	popl	%ecx

	movl	%eax, (%esi)		\ seconds
	jc	1f			\ Error in %eax
	xorl	%eax, %eax
1:
	movl	%eax, tos
end-code

[then]

\\	chsize			( size fildes -- ior )			UNIX
\\
\\		Change size of a file.
\\
: chsize
  true abort" FIXME:"
;

\\	unlink			( 0-addr -- ior )			MSDOS
\\
\\		Remove directory entry.
\\
code unlink
	pushl	%ecx
	pushl	%esi
	pushl	%edi

	movl	tos, %edx
	movb	$ $41 , %ah
	int	$ 33 <cr>		\ Delete directory entry

	popl	%edi
	popl	%esi
	popl	%ecx

	jc	1f			\ Error in %eax
	xorl	%eax, %eax
1:
	movl	%eax, tos
end-code

\\	rename			( 0-addr1 0-addr2 -- ior )		MSDOS
\\
\\		Changes filename. ( 0-addr2 to 0-addr1 )
\\
/ps=sp [if]

code rename
	popl	%edx			\ To name
	pushl	%ecx
	pushl	%esi
	pushl	%edi

	pushl	tos
	pushl	%edx			\ INT Destorys %edx+%ebx

	movb	$ 65 , %ah
	int	$ 33 <cr>		\ $41 Delete directory entry

	popl	%edi			\ To name
	popl	%edx			\ From name

	movb	$ $56 , %ah
	int	$ 33 <cr>		\ Change directory entry
	popl	%edi
	popl	%esi
	popl	%ecx
	jc	1f			\ Error in %eax
	xorl	%eax, %eax
1:
	movl	%eax, tos
end-code

[else]

code rename
	pushl	%ecx
	pushl	%edi
	pushl	%esi
	pushl	tos			\ INT Destorys %ebx...

	movb	$ $41 , %ah
	int	$ 33 <cr>		\ Delete directory entry

	popl	%edx			\ From name
	popl	%esi
	movl	(%esi), %edi		\ To name
	addl	$ 4 , %esi
	pushl	%esi

	movb	$ $56 , %ah
	int	$ 33 <cr>		\ Change directory entry

	popl	%esi
	popl	%edi
	popl	%ecx

	jc	1f			\ Error in %eax
	xorl	%eax, %eax
1:
	movl	%eax, tos
end-code

[then]

\\	access			( mode 0-addr -- x ior )		MSDOS
\\
\\		Determine accessibility of a file.
\\
/ps=sp [if]

code access
	pushl	%ecx
	pushl	%esi
	pushl	%edi

	movl	tos, %edx		\ 0-addr
	movw	$ $4300 , %ax
	int	$ 33 <cr>		\ Get file attribute

	movl	%ecx, tos		\ Attribute
	popl	%edi
	popl	%esi
	popl	%ecx

	popl	%edx			\ Mode
	push-tos			\ Attribute
	jc	1f			\ Error in %eax
	testb	$ 2 , %dl		\ Write
	jz	0f
	testb   $ 1 , tos-l		\ Write set
	jz	0f
	movl	$ 5 , %eax		\ EACCESS
	jmp	1f
0:
	xorl	%eax, %eax		\ OK
1:
	movl	%eax, tos
end-code

[else]

code access
	pushl	%ecx
	pushl	%esi
	pushl	%edi

	movl	tos, %edx		\ 0-addr
	movw	$ $4300 , %ax
	int	$ 33 <cr>		\ Get file attribute

	movl	%ecx, tos		\ Attribute
	popl	%edi
	popl	%esi
	popl	%ecx

	jc	1f			\ Error in %eax
	movl	(%esi), %eax		\ Mode
	movl	tos, (%esi)		\ Attribute
	testb	$ 2 , %al		\ Write
	jz	0f
	testb   $ 1 , tos-l		\ Write set
	jz	0f
	movl	$ 5 , %eax		\ EACCESS
	jmp	1f
0:
	xorl	%eax, %eax		\ OK
1:
	movl	%eax, tos
end-code

[then]

\\	system			( 0-addr -- ior )			MSDOS
\\
\\		Execute an external command.
\\
code system
	pushl	%ecx			\ tos 0-addr
	pushl	%esi
	pushl	%edi

	movw	$ $FF07 , %ax		\ Turbo assist: system
	int	$ 33 <cr>

	popl	%edi
	popl	%esi
	popl	%ecx
	jc	1f			\ Error in %eax
	xorl	%eax, %eax
1:
	movl	%eax, tos
end-code

\ -----------------------------------------------------------------------------

\\	(emit)			( x -- )				ALE
\\
\\		Low level word display character x.
\\
false [if]
code (emit)
	cmpb	$ $FF , tos-l		\ 0FFH is interpreted as input
	jnz	0f			\ do NOT allow input
	movb	$ 32 , tos-l		\ change to blank
0:	movb	$ 6 , %ah		\ MS-DOS Direct Console I/O
	movb	tos-l, %dl
	pushl	%ecx
	pushl	%esi
	pushl	%edi
	int	$ 33 <cr>		\ display character
	popl	%edi
	popl	%esi
	popl	%ecx
	pop-tos
end-code
[then]

code (emit)
	pushl	%ecx
	pushl	%esi
	pushl	%edi

	pushl	tos			\ Output byte
	movl	%esp, %edx		\ buffer
	xorl	%ecx, %ecx
	movb	$ 1 , %cl		\ u
	movl	%ecx, tos		\ handle (stdout)
	movb	$ $40 , %ah		\ Write handle bx,cx,ds:dx
	int	$ 33 <cr>
	popl	%eax

	popl	%edi
	popl	%esi
	popl	%ecx

	pop-tos
end-code

\\	(type)			( c-addr u -- )				ALE
\\
\\		Low level word display character string c-addr and u.
\\
/ps=sp [if]

code (type)
	popl	%edx			\ c-addr

	pushl	%ecx
	pushl	%esi
	pushl	%edi

	movl	tos, %ecx		\ u
	movl	$ 1 , tos		\ handle 1 (stdout)
	movb	$ $40 , %ah		\ Write handle bx,cx,ds:dx
	int	$ 33 <cr>

	popl	%edi
	popl	%esi
	popl	%ecx

	pop-tos
end-code

[else]

code (type)
	movl	(%esi), %edx		\ c-addr

	pushl	%ecx
	pushl	%esi
	pushl	%edi

	movl	tos, %ecx		\ u
	movl	$ 1 , tos		\ handle 1 (stdout)
	movb	$ $40 , %ah		\ Write handle bx,cx,ds:dx
	int	$ 33 <cr>

	popl	%edi
	popl	%esi
	popl	%ecx

	movl	4 (%esi), tos		\ Pop tos
	addl	$ 8 , %esi
end-code

[then]

\\	(?key)			( -- key true | false )			MSDOS
\\
\\		Look if a key is available from the keyboard.
\\
code (?key)
	push-tos
	xorl	tos, tos		\ BX=0 setup for false flag

	movb	$ $FF , %dl		\ input command
	movb	$ 6 , %ah		\ MS-DOS Direct Console I/O

	pushl	%ecx
	pushl	%esi
	pushl	%edi
	int	$ 33 <cr>
	popl	%edi
	popl	%esi
	popl	%ecx

	jz	2f			\ ?key ready

	movb	%al, tos-l
	orb	%al, %al		\ AL=0 if extended char
	jnz	1f			\ ?extended character code

	pushl	%ecx
	pushl	%esi
	pushl	%edi
	int	$ 33 <cr>
	popl	%edi
	popl	%esi
	popl	%ecx

	jz	2f			\ ?key ready

	movb	%al, tos-h		\ extended code in msb
1:
	push-tos			\ save character
	movl	$ -1 , tos		\ true flag
2:
end-code

\\	(key-buf)		( -- a-addr )				MSDOS
\\
\\		1 Key buffer.  For (key?)
\\
variable (key-buf)

\\	(key?)			( -- flag )				ALE
\\
\\		Retruns true if a character is available; otherwise false;
\\
: (key?)
  (key-buf) @  if  true exit  then	\ Already a key buffered
  (?key)
  if
    (key-buf) ! true exit		\ A key ready.
  then
  false exit
;

\\	(key)			( -- char )				ALE
\\
\\		Receive one character char.
\\
\\		FIXME: Better code wanted!
\\
: (key)
  begin  (key?)  until			\ Wait until a key available.
  (key-buf) @ (key-buf) off
;

forth definitions
