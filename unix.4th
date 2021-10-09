\
\	ALE Forth Version 2.04
\
\		Copyright (c) 1991,1992 by Lutz Sammer.
\
\		Bug-reports via usenet: johns
\
\	File:	unix.4th
\
\		Forth kernel unix operating system support.
\

\ -----------------------------------------------------------------------------
\	Unix system calls
\ -----------------------------------------------------------------------------

\ For now in compiler

compiler definitions

\\	sys-call	( args... #args call# -- u|n ior )		UNIX
\\
\\		Unix system call.
\\		args...			Arguments to system call
\\		#args			Bytes of arguments
\\		call#			System call number.
\\
/ps=sp [if]

code sys-call
0:	movl	tos, %eax		\ Call #
	lcall	$ 7 , $ 0 <cr>
	jnc	0f			\ Ok
	cmpl	$ 4 , %eax		\ EINTR
	je	0b
	movl	%eax, tos
	jmp	1f
0:
	xorl	tos, tos
1:
	addl	(%esp), %esp		\ Remove args
	movl	%eax, (%esp)		\ Result
end-code

[else]

code sys-call
	xchgl	%esi, %esp		\ FIXME: dangerous if interrupted.
0:	movl	tos, %eax		\ Call #
	lcall	$ 7 , $ 0 <cr>
	jnb	0f			\ Ok
	cmpl	$ 4 , %eax
	je	0b			\ EINTR
	movl	%eax, tos
	jmp	1f
0:
	xorl	tos, tos
1:
	xchgl	%esi, %esp
	addl	(%esi), %esi		\ Remove args
	movl	%eax, (%esi)		\ Result
end-code

[then]

\\	edx@			( -- n )				TESTING
\\
\\		Fetch n from the processor register %edx.
\\
primitive edx@
	push-tos
	movl	%edx, tos
end-primitive

\\	edx!			( n -- )				TESTING
\\
\\		Store n in the processor register %edx.
\\
primitive edx!
	movl	tos, %edx
	pop-tos
end-primitive

\\	sys-call2	( args... #args call# %edx -- u|n ior )		UNIX
\\
\\		Unix system call, sets also the register %edx.
\\
: sys-call2
  edx! sys-call
;

/ps=sp [if]

\\	sig-call		( R: sig -- )				UNIX
\\
\\		Unix signal call, called by signal handling routines to
\\		return to unix kernel.
\\
code sig-call
	addl	$ 4 , %esi		\ Remove sig
	lcall	$ 15 , $ 0 <cr>		\ Never returns
end-code

\\	signal>		( sig sys -- sig ) ( R: -- sys )		UNIX
\\
\\		Fetch signal number for signal handling word.
\\		Build return stack to return to sig-call.
\\
primitive signal>
\ %esp: sys sig %esi: xxx
	popl	%eax			\ return address
	movl	%eax, (%esi)
	subl	$ 4 , %esi
	popl	%eax			\ swap
	pushl	tos
	movl	%eax, tos
end-primitive compilation

\\	signal-addr@		( -- addr )				UNIX
\\
\\		Fetch the code address where the signal occure.
\\
: signal-addr@
  16 pick
;

[else]

\\	sig-call		( R: sig -- )				UNIX
\\
\\		Unix signal call, called by signal handling routines to
\\		return to unix kernel.
\\
code sig-call
	popl	%eax			\ Remove sig
	lcall	$ 15 , $ 0 <cr>		\ Never returns
end-code

\\	signal>		( -- sig ) ( R: sig sys -- sig sys )	I	UNIX
\\
\\		Fetch signal number for signal handling word.
\\		Build return stack to return to sig-call.
\\
: signal>
  postpone 2r@ postpone drop
; immediate

\\	signal-addr@		( -- addr )				UNIX
\\
\\		Fetch the code address where the signal occure.
\\
: signal-addr@
  rp@ 56 + @
;

[then]

\\	(bye)			( n -- )				UNIX
\\
\\		Return control to calling process.
\\		Unix system call - exit -.
\\
: (bye)
  4 1 pluck sys-call2
;

\\	fork			( -- pid 0 ior )	for parent	UNIX
\\			-or-	( -- pid 1 ior )	for child
\\
\\		Create a new process.
\\		Unix system call - fork -.
\\
: fork
  0 2 sys-call edx@ swap
;

\\	read			( u1 c-addr fildes -- u2 ior )		UNIX
\\
\\		Read from a file.
\\		Unix system call - read -.
\\
: read
  12 3 sys-call
;

\\	write			( u1 c-addr fildes -- u2 ior )		UNIX
\\
\\		Write on a file.
\\		Unix system call - write -.
\\
: write
  12 4 sys-call
;

\\	open			( u1 u2 0-addr -- fildes ior )		UNIX
\\
\\		Open a file for reading or writing.
\\		Unix system call - open -.
\\
: open
  12 5 sys-call
;

\\	close			( fildes -- ior )			UNIX
\\
\\		Close a file.
\\		Unix system call - close -.
\\
: close
  4 6 sys-call nip
;

\\	wait			( -- status pid ior )			UNIX
\\
\\		Awaits completion of background processes.
\\		Unix system call - wait -.
: wait
  0 7 sys-call edx@ -rot
;

\\	creat			( u1 0-addr -- fildes ior )		UNIX
\\
\\		Create a new file or rewrite an existing one.
\\		Unix system call - creat -.
\\
: creat
  8 8 sys-call
;

\\	link			( 0-addr1 0-addr2 -- ior )		UNIX
\\
\\		Link  to a file.
\\		Unix system call - link -.
\\
: link
  8 9 sys-call nip
;

\\	unlink			( 0-addr -- ior )			UNIX
\\
\\		Remove directory entry.
\\		Unix system call - unlink -.
\\
: unlink
  4 10 sys-call nip
;

\\	exec			( argv file -- ior )			UNIX
\\
\\		Execute a file.
\\		Unix system call - exec -.
\\
: exec
  8 11 sys-call nip
;

\\	chdir			( 0-addr -- ior )			UNIX
\\
\\		Change working directory.
\\		Unix system call - chdir -.
\\
: chdir
  4 12 sys-call nip
;

\\	time			( -- time ior )				UNIX
\\
\\		Get time.
\\		Unix system call - time -.
\\
: time
  0 13 sys-call
;

\\	mknod			( u1 u2 0-addr -- ior )			UNIX
\\
\\		Make a directory or a special or ordinary file or a FIFO.
\\		Unix system call - mknod -.
\\
: mknod
  12 14 sys-call nip
;

\\	chmod			( u 0-addr -- ior )			UNIX
\\
\\		Change mode of a file.
\\		Unix system call - chmod -.
\\
: chmod
  8 15 sys-call nip
;

\\	chown			( grp own 0-addr -- ior )		UNIX
\\
\\		Change owner and group of a file.
\\		Unix system call - chown -.
\\
: chown
  8 16 sys-call nip
;

\\	brk-val			( -- n )				UNIX
\\
\\		n is the current break value.
\\
value brk-val

\\	brk			( end -- ior )				UNIX
\\
\\		Change data segment space allocation.
\\		Unix system call - brk -.
\\
: brk
  dup to brk-val
  4 17 sys-call nip
;

\\	stat			( buf file -- ior )			UNIX
\\
\\		Get file status.
\\		Unix system call - stat -.
\\
: stat
  8 18 sys-call nip
;

\\	SEEK_SET		( -- n )				UNIX
\\
\\		n is the first argument to SEEK.
\\		Set the seek pointer to offset bytes.
\\
0 constant SEEK_SET

\\	SEEK_CUR		( -- n )				UNIX
\\
\\		n is the first argument to SEEK.
\\		Set the seek pointer to its current location plus offset.
\\
1 constant SEEK_CUR

\\	SEEK_END		( -- n )				UNIX
\\
\\		n is the first argument to SEEK.
\\		Set the seek pointer to the size of the file plus offset.
\\
2 constant SEEK_END

\\	seek		( whence offset fildes -- offset ior )		UNIX
\\
\\		Move read/write file pointer.
\\		Unix system call - seek -.
\\
: seek
  12 19 sys-call
;

\ Rest is missing.

\\	getpid			( -- pid )				UNIX
\\
\\		Get process id.
\\		Unix system call - getpid -.
\\
: getpid
  0 20 sys-call drop
;

\\	mount			( ??? -- ??? ior )			UNIX
\\
: mount
  21 sys-call \ FIXME:
;

\\	umount			( ??? -- ??? ior )			UNIX
\\
: umount
  22 sys-call \ FIXME:
;

\\	setuid			( ??? -- ??? ior )			UNIX
\\
: setuid
  23 sys-call \ FIXME:
;

\\	getuid			( -- uid )				UNIX
\\
\\		Get user id.
\\		Unix system call - getuid -.
\\
: getuid
  0 24 sys-call drop
;

\\	stime			( ??? -- ??? ior )			UNIX
\\
: stime
  25 sys-call \ FIXME:
;

\\	ptrace		( data addr pid request -- res ior )		UNIX
\\
\\		Process trace.
\\		System call - ptrace-.
\\
: ptrace
  16 26 sys-call
;

\\	alarm			( n1 -- n2 ior )			UNIX
\\
\\		Set the alarm clock. (n2 is rest of the alarm clock)
\\		Unix system call - alarm -.
\\
: alarm
  4 27 sys-call
;

\\	fstat			( buf fildes -- ior )			UNIX
\\
\\		Get file status.
\\		Unix system call - stat -.
\\
: fstat
  8 28 sys-call nip
;

\\	pause			( -- ior )				UNIX
\\
\\		Suspend execution program, until it receives a signal.
\\		Unix system call - pause -.
\\
: pause
  0 29 sys-call drop
;

\\	utime			( ??? -- ??? ior )			UNIX
\\
: utime
  30 sys-call \ FIXME:
;

\\	stty			( ??? -- ??? ior )			UNIX
\\
: stty
  31 sys-call \ FIXME:
;

\\	gtty			( ??? -- ??? ior )			UNIX
\\
: gtty
  32 sys-call \ FIXME:
;

\\	access			( mode 0-addr -- x ior )		UNIX
\\
\\		Determine accessibility of a file.
\\		Unix system call - access -.
\\
: access
  8 33 sys-call
;

\\	nice			( incr -- nice-val ior )		UNIX
\\
\\		Change priority of a process.
\\		Unix system call - nice -.
\\
: nice
  4 34 sys-call
;

\\	statfs			( ??? -- ??? ior )			UNIX
\\
: statfs
  35 sys-call \ FIXME:
;

\\	sync			( -- )					UNIX
\\
\\		Update super block.
\\		Unix system call - sync -.
\\
: sync
  0 36 sys-call 2drop
;

\\	kill			( sig pid -- ior )			UNIX
\\
\\		Send the signal sig to a process or a group of processes.
\\		Unix system call - kill -.
\\
: kill
  8 37 sys-call
;

\\	fstatfs			( ??? -- ??? ior )			UNIX
\\
: fstatfs
  38 sys-call \ FIXME:
;

\\	setpgrp			( ??? -- ??? ior )			UNIX
\\
: setpgrp
  39 sys-call \ FIXME:
;

\\	dupf			( ??? -- ??? ior )			UNIX
\\
: dupf
  41 sys-call \ FIXME:
;

\\	pipe			( ??? -- ??? ior )			UNIX
\\
: pipe
  42 sys-call \ FIXME:
;

\\	times			( ??? -- ??? ior )			UNIX
\\
: times
  43 sys-call \ FIXME:
;

\\	prof			( ??? -- ??? ior )			UNIX
\\
: prof
  44 sys-call \ FIXME:
;

\\	lock			( ??? -- ??? ior )			UNIX
\\
: lock
  45 sys-call \ FIXME:
;

\\	setgid			( ??? -- ??? ior )			UNIX
\\
: setgid
  46 sys-call \ FIXME:
;

\\	getgid			( -- gid )				UNIX
\\
\\		Get group id.
\\		Unix system call - getgid -.
\\
: getgid
  0 47 sys-call drop
;

\\	signal			( xt1 sig -- xt2 ior )			UNIX
\\
\\		Specify what to do upon receipt of a signal.
\\		Unix system call - signal -.
\\
: signal
  8 48 ['] sig-call sys-call2
;

\ FIXME: sys-calls 49-53

\\	ioctl		( arg request fildes -- res ior )		UNIX
\\
\\		Unix system call - ioctl -.
\\
: ioctl
  12 54 sys-call
;

\ FIXME: sys-calls 55-58

\\	exece			( envp argv file -- ior )		UNIX
\\
\\		Execute a file with environment.
\\		Unix system call - exece -.
\\
: exece
  12 59 sys-call nip
;

\ FIXME: sys-calls 60-63

\ -----------------------------------------------------------------------------
\	CXENIX System calls
\ -----------------------------------------------------------------------------

\ FIXME: $0028 ... $3028

\\	chsize			( size fildes -- ior )			UNIX
\\
\\		Unix system call - chsize -.
\\
: chsize
  8 $0A28 sys-call nip
;

\\	nap			( ms1 -- ms2 ior )			UNIX
\\
\\		Suspends execution for a short interval.
\\		Unix system call - nap -.
\\
: nap
  4 $0C28 sys-call
;

\\	rename			( 0-addr1 0-addr2 -- ior )		UNIX
\\
\\		Changes filename.
\\		Unix system call - rename -.
\\
: rename
  8 $3028 sys-call nip
;

\ -----------------------------------------------------------------------------

\\	(emit)			( x -- )				ALE
\\
\\		Low level word display character x.
\\
primitive (emit)
	pushl	tos
	movl	%esp, tos		\ Address of byte to output
	xorl	%eax, %eax
	movb	$ 1 , %al

	pushl	%eax			\ 1 Byte
	pushl	tos			\ Buffer
	pushl	%eax			\ 1 Fildes
	pushl	%eax			\ Dummy
	movb	$ 4 , %al		\ system call -write-
	lcall	$ 7 , $ 0 <cr>
	addl	$ 20 , %esp
	pop-tos
end-primitive

\\	(type)			( c-addr u -- )				ALE
\\
\\		Low level word display character string c-addr and u.
\\
/ps=sp [if]

primitive (type)
	popl	%eax			\ c-addr

	pushl	tos			\ Bytes
	pushl	%eax			\ Buffer
	pushl	$ 1 <cr>		\ 1 Fildes
	pushl	%eax			\ Dummy
	xorl	%eax, %eax
	movb	$ 4 , %al		\ system call -write-
	lcall	$ 7 , $ 0 <cr>

	addl	$ 16 , %esp
	popl	tos
end-primitive

[else]

primitive (type)
	pushl	tos			\ Bytes
	movl	(%esi), %eax
	pushl	%eax			\ Buffer
	pushl	$ 1 <cr>		\ 1 Fildes
	pushl	%eax			\ Dummy
	xorl	%eax, %eax
	movb	$ 4 , %al		\ system call -write-
	lcall	$ 7 , $ 0 <cr>

	addl	$ 16 , %esp
	movl	4 (%esi), tos
	addl	$ 8 , %esi
end-primitive

[then]

\\	(key)			( -- char )				ALE
\\
\\		Receive one character char.
\\
code (key)
	push-tos
	xorl	%eax, %eax
	pushl	%eax
	movl	%esp, tos		\ Input buffer

	pushl	$ 1 <cr>		\ 1 Byte 
	pushl	tos			\ Buffer
	pushl	%eax			\ 0 Fildes
	pushl	%eax			\ Dummy
0:	movb	$ 3 , %al		\ -read-
	lcall	$ 7 , $ 0 <cr>
	jnc	1f
	cmpl	$ 4 , %eax		\ EINTR
	je	0b
1:
	addl	$ 16 , %esp
	popl	tos			\ Fetch byte ( xor above! )
end-code

\\	(key?)			( -- flag )				ALE
\\
\\		Returns true if a character is available; otherwise false;
\\
code (key?)
	push-tos			\ Save top of stack

	xorl	%eax, %eax
	pushl	%eax
	pushl	%eax			\ Timout 0 
	movl	%esp, tos

	pushl	$ 1 <cr>		\ Select on fildes 1
	movl	%esp, %edx		\ readfds buffer

	pushl	tos			\ timeout
	pushl	%eax			\ exceptfds
	pushl	%eax			\ writefds
	pushl	%edx			\ readfds
	pushl	$ 1 <cr>		\ nfds
	pushl	%eax			\ Dumpy
	movl	$ $2428 , %eax		\ -select-
	lcall	$ 7 , $ 0 <cr>

	addl	$ 36 , %esp

	movl	%eax, tos		\ fildes 1 ready ?
	negl	tos
end-code

forth definitions
