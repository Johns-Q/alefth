\
\	ALE Forth Version 2.04
\
\		Copyright (c) 1991,1992 by Lutz Sammer.
\
\		Bug-reports via usenet: johns
\
\	File:	execute.4th
\
\		Forth kernel execution operators.
\

\ -----------------------------------------------------------------------------
\	Execution
\ -----------------------------------------------------------------------------

\	Execution
\
\		execute		@execute	perform
\

\\	execute			( i*x xt -- j*x )			CORE
\\
\\		Execute the definition specified by xt.
\\
\ /less-code [if]			\ Didn't run with %esp=sp
\ : execute
\    >r
\ ;
\ [else]
\ [then]
/ps=sp [if]

compiler definitions

\\	<execute>		( xt -- )				ALE
\\
\\		Append  the  execution semantics of EXECUTE to the  current
\\		definitions.
\\
: <execute>
  drop
  $C75BD889 ,				\ movl	%ebx, %eax
					\ popl	%ebx
  $06 c, here 6 + ,			\ movl  $ .+8, (%esi)
  $E0FF w,				\ jmp	%eax
;

forth definitions

code execute
	movl	tos, %eax
	pop-tos
	jmp	%eax
end-code

[else]

primitive execute
	movl	tos, %eax
	pop-tos
	call	%eax
end-primitive

[then]

\\	perform			( i*x a-addr -- j*x )			ALE
\\			-or-	( i*x a-addr -- i*x )	a-addr @ = 0
\\
\\		Perform the definition specified by a-addr, a-addr contains
\\		the execution token xt.  If xt is zero, does nothing.
\\		Same as:  @ ?DUP  IF  EXECUTE  THEN.
\\
/less-code [if]
: perform
  @ ?dup  if  execute  then
;
[else]
/ps=sp [if]

\ FIXME: primitive

code perform
	movl	(tos), %eax
	pop-tos
	orl	%eax, %eax
	jz	2 <cr>
	jmp	%eax
end-code

[else]

primitive perform
	movl	(tos), %eax
	pop-tos
	orl	%eax, %eax
	jz	2 <cr>
	call	%eax
end-primitive

[then]
[then]
