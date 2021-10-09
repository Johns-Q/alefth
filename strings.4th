\
\	ALE Forth Version 2.04
\
\		Copyright (c) 1991,1992 by Lutz Sammer.
\
\		Bug-reports via usenet: johns
\
\	File:	strings.4th
\
\		Forth kernel string operators.
\

\ -----------------------------------------------------------------------------
\	Strings
\ -----------------------------------------------------------------------------

\\	cmove			( c-addr1 c-addr2 u -- )		STRING
\\
\\		If  u  is  greater than zero, copy u consecutive characters
\\		from  c-addr1 to c-addr2. If c-addr2 lies within the source
\\		region, memory propagation occurs. (c-addr2 lies within the
\\		source  region  if  c-addr2  is  not  less than c-addr1 and
\\		c-addr2 is less than the quantity c-addr1 u CHARS +).
\\
/less-code [if]
: cmove
  0
  ?do
    over c@ over c!
    1+ swap 1+ swap
  loop
  2drop
;
[else]

/ps=sp [if]

code cmove
	movl	%esi, %eax
	movl	%edi, %edx

	popl	%edi			\ c-addr2
	popl	%esi			\ c-addr1
	pushl	%ecx

	movl	tos, %ecx
	shrl	$ 2 , %ecx
	rep
	movsl	(%esi), (%edi)		\ Move longs
	movl	tos, %ecx
	andl	$ 3 , %ecx
	rep
	movsb	(%esi), (%edi)		\ Move bytes

	popl	%ecx
	movl	%edx, %edi
	movl	%eax, %esi
	pop-tos
end-code

[else]

code cmove
	pushl	%ecx
	pushl	%edi
	movl	%esi, %eax
	movl	(%esi), %edi
	movl	4 (%esi), %esi
	movl	tos, %ecx
	shrl	$ 2 , %ecx
	rep
	movsl	(%esi), (%edi)		\ Move longs
	movl	tos, %ecx
	andl	$ 3 , %ecx
	rep
	movsb	(%esi), (%edi)		\ Move bytes
	popl	%edi
	popl	%ecx
	movl	8 (%eax), tos
	leal	12 (%eax), %esi
end-code

[then]
[then]

\\	cmove>			( c-addr1 c-addr2 u -- )		STRING
\\
\\		If  u  is  greater than zero, copy u consecutive characters
\\		from   c-addr1  to  c-addr2.  If c-addr1  lies  within  the
\\		destination  region,  memory  propagation  occurs. (c-addr1
\\		lies  within  the  destination region if c-addr1 is greater
\\		than  or  equal  to c-addr2 and if c-addr2 is less than the
\\		quantity c-addr1 u CHARS +.)
\\
/less-code [if]
: cmove>
  >r swap r@ + swap r@ + r>
  0
  ?do
    1- swap 1- swap
    over c@ over c!
  loop
  2drop
;
[else]

/ps=sp [if]

code cmove>
	movl	%esi, %eax
	movl	%edi, %edx

	popl	%edi			\ c-addr2
	popl	%esi			\ c-addr1
	pushl	%ecx
					\ FIXME: use movsl
\	leal	-4 (%edi, tos), %edi
\	leal	-4 (%esi, tos), %esi	\ Start at high address
	std
\	shrl	$ 2 , %ecx
\	rep
\	movsl	(%esi), (%edi)
\	movl	tos, %ecx
\	andl	$ 3 , %ecx
\	leal	3 (%edi), %edi
\	leal	3 (%esi), %esi
\	movl	tos, %ecx
\	andl	$ 3 , %ecx

	leal	-1 (%edi, tos), %edi
	leal	-1 (%esi, tos), %esi	\ Start at high address
	movl	tos, %ecx

	rep
	movsb	(%esi), (%edi)
	cld

	popl	%ecx
	movl	%edx, %edi
	movl	%eax, %esi
	pop-tos
end-code
[else]

code cmove>
	pushl	%ecx
	pushl	%edi
	movl	%esi, %eax
	decl	tos
	movl	(%esi), %edi		\ Dest
	addl	tos, %edi
	movl	4 (%esi), %esi
	addl	tos, %esi		\ Start at high address
	incl	tos
	movl	tos, %ecx
					\ FIXME: movsl
	std
	rep
	movsb	(%esi), (%edi)
	cld
	popl	%edi
	popl	%ecx
	movl	8 (%eax), tos
	leal	12 (%eax), %esi
end-code

[then]
[then]

\\	move			( addr1 addr2 u -- )			CORE
\\
\\		If  u  is  greater  than  zero,  copy  the  contents  of  u
\\		consecutive  address  units at addr1 to the  u  consecutive
\\		address  units  at  addr2.  After  MOVE  completes,  the  u
\\		consecutive address units at addr2 contain exactly what the
\\		u  consecutive address units at addr1 contained before  the
\\		move.
\\
: move
  >r 2dup u<
  if
    r> cmove>
  else
    r> cmove
  then
;

\\	fill			( c-addr u char -- )			CORE
\\
\\		If  u  is  greater  than  zero,  store  char  in  each of u
\\		consecutive characters of memory beginning at c-addr.
\\
/less-code [if]
: fill
  -rot
  0
  ?do
    2dup c! 1+
  loop
  2drop
;
[else]

/ps=sp [if]

code fill
	movb	tos-l, tos-h		\ 8 -> 16
	movl	tos, %eax
	shll	$ 16 , %eax
	movw	tos, %eax		\ 16 -> 32

	movl	%ecx, tos
	movl	%edi, %edx		\ Save %ecx, %edi

	popl	%ecx			\ u
	popl	%edi			\ c-addr
	pushl	%ecx
	shrl	$ 2 , %ecx
	rep
	stosl	%eax, (%edi)		\ Store longs
	popl	%ecx
	andl	$ 3 , %ecx
	rep
	stosb	%al, (%edi)		\ Store bytes

	movl	%edx, %edi
	movl	tos, %ecx		\ Restore %ecx, %edi
	pop-tos
end-code

[else]

code fill
	pushl	%ecx
	pushl	%edi
	movl	(%esi), %ecx		\ u
	movl	4 (%esi), %edi
	movb	tos-l, tos-h		\ 8 -> 16
	movl	tos, %eax
	shll	$ 16 , %eax
	movw	tos, %eax		\ 16 -> 32
	movl	%ecx, %edx
	shrl	$ 2 , %ecx
	rep
	stosl	%eax, (%edi)		\ Store longs
	movl	%edx, %ecx
	andl	$ 3 , %ecx
	rep
	stosb	%al, (%edi)		\ Store bytes
	popl	%edi
	popl	%ecx
	leal	8 (%esi), %esi
	pop-tos
end-code

[then]
[then]

\\	erase			( addr u -- )				CORE EXT
\\
\\		If  u  is  greater  than  zero, clear all bits in each of u
\\		consecutive address units of memory beginning at addr .
\\
: erase
  0 fill
;

\\	blank			( c-addr u -- )				STRING
\\
\\		If  u  is  greater than zero, store the character value for
\\		space  in  u  consecutive  character positions beginning at
\\		c-addr.
\\
: blank
  ( bl) 32 fill
;

false [if]
code -text			( c-addr1 u1 c-addr2 -- flag )
	movl	tos, %edi
	movl	%ecx, tos
	movl	%esi, %eax
	movl	%edi, %edx

	popl	%ecx			\ u1
	popl	%esi			\ c-addr1

	repe
	cmpsb	(%esi), (%edi)
	movl	%edx, %edi
	movl	%eax, %esi
	movl	tos, %ecx
	je	0f

	movl	$ 1 , %ecx
	jnc	0f			\ >
	negl	%ecx

0:	movl	%ecx, tos
end-code
[then]

\\	compare			( c-addr1 u1 c-addr2 u2 -- n )		STRING
\\
\\		Compare  the  string  specified by c-addr1 and  u1  to  the
\\		string  specified  by  c-addr2  and  u2.  The  strings  are
\\		compared,  beginning at the given addresses,  character  by
\\		character  up to the length of the shorter string, or until
\\		a  difference is found.  If both strings are the same up to
\\		the length of the shorter string, then the longer string is
\\		greater  than  the shorter string.  n is -1 if  the  string
\\		specified  by  c-addr1  and u1  is  less  than  the  string
\\		specified  by c-addr2 and u2.  n is zero if the strings are
\\		equal.  n is 1 if the string specified by c-addr1 and u1 is
\\		greater than the string specified by c-addr2 and u2.
\\
/less-code [if]

: -text		( c-addr1 u1 c-addr2 - -1|0|1 )
   swap 0
   ?do
     over c@ over c@ - ( these chars <> ?)
     if
       unloop
       c@ swap c@ > 2* 1+ exit
     then
     1 1 d+
  loop
  2drop 0
;

: compare
  rot 2dup ( lengths ) 2>r min swap -text dup
  if
    2r> 2drop
  else
    drop  2r> 2dup = ( lengths = ?)
    if
      2drop 0
    else
      > 2* 1+
    then
  then
;

[else]

/ps=sp [if]

code compare
	leal	-8 (%esi), %eax		\ esi = ret
	movl	%edi, (%eax)
	movl	%ecx, 4 (%eax)
					\ u2	tos
	popl	%edi			\ c-addr2
	popl	%ecx			\ u1
	popl	%esi			\ c-addr1

	movl	%ecx, %edx
	subl	tos, %edx
	jb	0f
	movl	tos, %ecx		\ Shorter count
0:
	repe
	cmpsb	(%esi), (%edi)
	movl	4 (%eax), %ecx
	movl	(%eax), %edi
	leal	8 (%eax), %esi
	jne	0f

	movl	%edx, tos		\ Length must be same
	jmp	1f
0:
	movl	$ 1 , tos
	jnc	1f			\ >
	negl	tos
1:
end-code

[else]

code compare
	pushl	%edi
					\ u2	tos
	movl	(%esi), %edi		\ c-addr2
	leal	4 (%esi), %esi
	pushl	%esi
	pushl	%ecx
	movl	(%esi), %ecx		\ u1
	movl	4 (%esi), %esi		\ c-addr1
	movl	%ecx, %edx
	subl	tos, %edx
	jb	0f
	movl	tos, %ecx		\ Shorter count
0:
	repe
	cmpsb	(%esi), (%edi)
	popl	%ecx
	popl	%esi
	popl	%edi
	jne	0f

	movl	%edx, tos		\ Length must be same
	leal	8 (%esi), %esi
	ret
0:
	movl	$ 1 , tos
	jnc	0f			\ >
	negl	tos
0:
	leal	8 (%esi), %esi
end-code

[then]
[then]

\\	/string			( c-addr1 u1 n -- c-addr2 u2 )		STRING
\\
\\		Adjust the character string at c-addr1 by n characters. The
\\		resulting  character  string  specified  by  c-addr2 and u2
\\		begins  at  a-addr1  plus  n  characters  and is u1 minus n
\\		characters long.
\\
/less-code [if]
: /string
  rot over + -rot -
;
[else]

/ps=sp [if]

primitive /string
	popl	%eax
	addl	tos, (%esp)
	subl	tos, %eax
	movl	%eax, tos
end-primitive

[else]

primitive /string
	addl	tos, 4 (%esi)
	subl	tos, (%esi)
	pop-tos
end-primitive

[then]
[then]

\\	bounds		( c-addr1 u1 -- c-addr2 c-addr1 )	STRING ALE
\\
\\		Return the limits of a c-addr1 and u1. c-addr1 is unchanged
\\		c-addr2 is the sum of c-addr1 and u.  Often used in do-loop
\\		to calculate the limits.
\\
/less-code [if]
: bounds
  over + swap
;
[else]

/ps=sp [if]

primitive bounds
	movl	tos, %eax
	popl	tos			\ c-addr1
	addl	tos, %eax		\ c-addr1+u1 = c-addr2
	pushl	%eax
end-primitive

[else]

primitive bounds
	movl	(%esi), %eax
	addl	tos, (%esi)
	movl	%eax, tos
end-primitive

[then]
[then]

\\	scan		( c-addr1 u1 char -- c-addr2 u2 )	STRING ALE
\\
\\		Scan  the string c-addr1 and u1 from left to right for  the
\\		character  char.  The string c-addr2 and u2 is  the  string
\\		after  the  delimiter  char,  if found.  Otherwise  a  zero
\\		string.
\\
/less-code [if]
: scan
  >r
  begin					\ Find ending delimiter
    over c@ r@ <> over and
  while
    1 /string
  repeat
  r> drop
;
[else]

/ps=sp [if]

code scan
	movl	tos, %eax		\ char
	movl	%ecx, %edx

	popl	%ecx			\ u1
	orl	%ecx, %ecx
	jz	0f			\ Empty string

	movl	%edi, tos
	popl	%edi			\ c-addr1

	repne
	scasb	%al, (%edi)
	jnz	2 <cr>
	decl	%edi
	incl	%ecx

	pushl	%edi			\ c-addr2
	movl	tos, %edi
0:
	movl	%ecx, tos		\ u2
	movl	%edx, %ecx
end-code

[else]

code scan
	pushl	%ecx
	pushl	%edi
	movl	tos, %eax		\ char
	movl	(%esi), %ecx		\ length
	inc-csp
	movl	(%esi), %edi		\ addr
	repne
	scasb	%al, (%edi)
	jnz	0f
	decl	%edi
	incl	%ecx
0:	movl	%edi, (%esi)		\ c-addr2
	movl	%ecx, tos		\ u2
	popl	%edi
	popl	%ecx
end-code

[then]
[then]

\\	skip		( c-addr1 u1 char -- c-addr2 u2 )	STRING ALE
\\
\\		Skip  all leading characters in string c-addr1 and u1.  The
\\		string c-addr2 and u2 is the string after the characters or
\\		the  entire  string if there is no leading character  or  a
\\		zero string if there are only characters char.
\\
/less-code [if]
: skip
  >r
  begin					\ Skip leading delimiter
    over c@ r@ = over and
  while
    1 /string
  repeat
  r> drop
;
[else]

/ps=sp [if]

code skip
	movl	tos, %eax		\ char
	movl	%ecx, %edx

	popl	%ecx			\ u1
	orl	%ecx, %ecx
	jz	0f

	movl	%edi, tos
	popl	%edi			\ c-addr1

	repe
	scasb	%al, (%edi)
	jz	2 <cr>
	decl	%edi
	incl	%ecx

	pushl	%edi			\ c-addr2
	movl	tos, %edi
0:
	movl	%ecx, tos		\ u2
	movl	%edx, %ecx
end-code

[else]

code skip
	pushl	%ecx
	pushl	%edi
	movl	tos, %eax		\ char
	movl	(%esi), %ecx		\ length
	leal	4 (%esi), %esi
	movl	(%esi), %edi		\ addr
	repe
	scasb	%al, (%edi)
	jz	0f
	decl	%edi
	incl	%ecx
0:	movl	%edi, (%esi)		\ c-addr2
	movl	%ecx, tos		\ u2
	popl	%edi
	popl	%ecx
end-code

[then]
[then]

\\	<scan		( c-addr1 u1 char -- c-addr2 u2 )	STRING ALE
\\
\\		Scan  the  string c-addr1 and u1 from left to right  for  a
\\		character less equal to char.  The string c-addr2 and u2 is
\\		the string after the delimiter char, if found.  Otherwise a
\\		zero length string.
\\
/less-code [if]
: <scan
  >r
  begin				\ Find ending delimiter
    over c@ r@ > over and
  while
    1 /string
  repeat
  r> drop
;
[else]

/ps=sp [if]

code <scan
	movl	tos, %eax		\ char
	popl	tos			\ u1
	orl	tos, tos		\ ZERO LENGTH
	jz	1f
	popl	%edx
0:
	cmpb	(%edx), %al		\ not repeated scasb slower on 486
	jge	0f
	incl	%edx
	decl	tos
	jz	0f
	cmpb	(%edx), %al		\ Double faster
	jge	0f
	incl	%edx
	decl	tos
	jnz	0b			\ loop slower on 486

0:	pushl	%edx			\ c-addr2
1:
end-code

[else]

code <scan
	movl	tos, %eax		\ char
	movl	(%esi), tos		\ length
	inc-csp
	orl	tos, tos		\ ZERO LENGTH
	jz	1f
	movl	(%esi), %edx		\ addr
0:
	cmpb	(%edx), %al		\ not repeated scasb slower on 486
	jge	0f
	incl	%edx
	decl	tos
	jz	0f
	cmpb	(%edx), %al		\ Double faster
	jge	0f
	incl	%edx
	decl	tos
	jnz	0b			\ loop slower on 486

0:	movl	%edx, (%esi)		\ c-addr2
1:
end-code

[then]
[then]

\\	<skip		( c-addr1 u1 char -- c-addr2 u2 )	STRING ALE
\\
\\		Skip all leading characters less or equal to char in string
\\		c-addr1  and u1.  The string c-addr2 and u2 is  the  string
\\		after  the  characters or the entire string if there is  no
\\		leading  characters  or a zero length string if  there  are
\\		only characters less or equal to char.
\\
/less-code [if]
: <skip
  1+ >r
  begin				\ Skip leading delimiter
    over c@ r@ < over and
  while
    1 /string
  repeat
  r> drop
;
[else]

/ps=sp [if]

code <skip
	movl	tos, %eax		\ char
	popl	tos			\ u1
	orl	tos, tos		\ ZERO LENGTH
	jz	1f
	popl	%edx			\ c-addr1
0:
	cmpb	(%edx), %al		\ not repeated scasb slower on 486
	jl	0f
	incl	%edx
	decl	tos
	jz	0f
	cmpb	(%edx), %al		\ Double faster
	jl	0f
	incl	%edx
	decl	tos
	jnz	0b			\ loop slower on 486

0:	pushl	%edx			\ c-addr2
1:
end-code

[else]

code <skip
	movl	tos, %eax		\ char
	movl	(%esi), tos		\ length
	inc-csp
	orl	tos, tos		\ ZERO LENGTH
	jz	1f
	movl	(%esi), %edx		\ addr
0:
	cmpb	(%edx), %al		\ not repeated scasb slower on 486
	jl	0f
	incl	%edx
	decl	tos
	jz	0f
	cmpb	(%edx), %al		\ Double faster
	jl	0f
	incl	%edx
	decl	tos
	jnz	0b			\ loop slower on 486

0:	movl	%edx, (%esi)		\ c-addr2
1:
end-code

[then]
[then]

\\	scan>		( c-addr1 u1 char -- c-addr2 u2 )	STRING ALE
\\
\\		Scan  the string c-addr1 and u1 from right to left for  the
\\		character  char.  The string c-addr2 and u2 is  the  string
\\		before  the  delimiter  char, if found.  Otherwise  a  zero
\\		string.
\\
/less-code [if]
: scan>
  >r
  begin
    dup
  while
    1- 2dup + c@ r@ <>
    if
      r> drop 1+ exit
    then
  repeat
  r> drop
;
[else]

/ps=sp [if]

code scan>
	movl	tos, %eax		\ char
	movl	%ecx, %edx
	popl	%ecx			\ u1

	orl	%ecx, %ecx
	jz	1f

	movl	%edi, tos
	movl	(%esp), %edi		\ c-addr1
	leal	-1 (%ecx, %edi), %edi	\ To end
	std
	repne
	scasb	%al, (%edi)
	cld
	jnz	1 <cr>
	incl	%ecx

	movl	tos, %edi
1:
	movl	%ecx, tos		\ u2
	movl	%edx, %ecx
end-code

[else]

code scan>
	pushl	%ecx
	pushl	%edi
	movl	tos, %eax		\ char
	movl	(%esi), %ecx		\ length
	inc-csp
	movl	(%esi), %edi		\ addr
	leal	-1 (%ecx, %edi), %edi
	std
	repne
	scasb	%al, (%edi)
	cld
	jnz	0f
	incl	%ecx
0:
	movl	%ecx, tos		\ u2
	popl	%edi
	popl	%ecx
end-code

[then]
[then]

\\	skip>		( c-addr1 u1 char -- c-addr2 u2 )	STRING ALE
\\
\\		Skip all trailing characters in string c-addr1 and u1.  The
\\		string c-addr2 and u2 is the string without the  characters
\\		or the entire string if there is no trailing character or a
\\		zero string if there are only characters char.
\\
/less-code [if]
: skip>
  >r
  begin
    dup
  while
    1- 2dup + c@ r@ -
    if
      r> drop 1+ exit
    then
  repeat
  r> drop
;
[else]

/ps=sp [if]

code skip>
	movl	tos, %eax		\ char
	movl	%ecx, %edx

	popl	%ecx			\ u1
	orl	%ecx, %ecx
	jz	1f

	movl	%edi, tos
	movl	(%esp), %edi		\ c-addr1
	leal	-1 (%edi, %ecx), %edi

	std
	repe
	scasb	%al, (%edi)
	cld
	jz	1 <cr>
	incl	%ecx

	movl	tos, %edi
1:
	movl	%ecx, tos		\ u2
	movl	%edx, %ecx
end-code

[else]

code skip>
	pushl	%ecx
	pushl	%edi
	movl	tos, %eax		\ char
	movl	(%esi), %ecx		\ length
	leal	4 (%esi), %esi
	movl	(%esi), %edi		\ addr
	leal	-1 (%edi, %ecx), %edi
	std
	repe
	scasb	%al, (%edi)
	cld
	jnz	0f
	incl	%ecx
0:	movl	%ecx, tos		\ u2
	popl	%edi
	popl	%ecx
end-code

[then]
[then]

\\	-trailing		( c-addr u1 -- c-addr u2 )		STRING
\\
\\		If  u1  is  greater  than  zero, u2 is equal to u1 less the
\\		number  of  spaces  at  the  end  of  the  character string
\\		specified  by  c-addr  and u1.  If u1 is zero or the entire
\\		string consists of spaces, u2 is zero.
\\
false [if]
: -trailing
  begin
    dup
  while
    1- 2dup + c@ ( bl) 32 -
    if
      1+ exit
    then
  repeat
;
[then]
: -trailing
  bl skip>
;

\\	lex							STRING EXT
\\
\\	( c-addr1 u1 c-addr2 u2 -- c-addr3 u3 c-addr1 u4 char true ) if found
\\	( c-addr1 u1 c-addr2 u2 -- c-addr1 u1 false )		 if not found
\\
\\		Parse  the string at c-addr1 and u1 for the first match  of
\\		any  character in the string at c-addr2 and u2.  If  found,
\\		return  the substring following the delimiter  (c-addr3 and
\\		u3),  the  substring preceding the delimiter  (c-addr1  and
\\		u4),   the  delimiting  character   char  and  true.  If  a
\\		delimiting  character  is  not found  return  the  original
\\		string and false.
\ FIXME:

\\	search		( c-addr1 u1 c-addr2 u2 -- c-addr3 u3 flag )	STRING
\\
\\		Search the string specified by caddr1 and u1 for the string
\\		specified by c-addr2 and u2.  If flag is true, a match  was
\\		found  at c-addr3 with u3 characters remaining.  If flag is
\\		false  there was no match and c-addr3 is c-addr1 and u3  is
\\		u1.
\\
\ FIXME:

\ -----------------------------------------------------------------------------
\	Counted strings
\ -----------------------------------------------------------------------------

\\	count			( c-addr1 -- c-addr2 u )		CORE
\\
\\		Return  the  character string specification for the counted
\\		string  stored  at  c-addr1.  c-addr2 is the address of the
\\		first  character  after  c-addr1.  u is the contents of the
\\		character  at c-addr1, which is the length in characters of
\\		the string at c-addr2.
\\
/less-code [if]
: count
  dup 1+ swap c@
;
[else]

/ps=sp [if]

primitive count
	movl	tos, %eax
	incl	tos
	pushl	tos
	xorl	tos, tos
	movb	(%eax), tos
end-primitive

[else]

primitive count
	incl	tos
	leal	-4 (%esi), %esi
	movl	tos, (%esi)
	movzbl	-1 (tos), tos
end-primitive

[then]
[then]

\\	place			( c-addr1 u1 c-addr2 -- )	STRING ALE
\\
\\		Place  string c-addr1 and u1 at address c-addr2 as  counted
\\		string. u1 is placed as first character at c-addr2 followed
\\		by the string c-addr1.
\\
: place
  2dup c! 1+ swap cmove
;

compiler definitions

\\	#"buf			( -- n )				COMPILER
\\
\\		n is the size of the rotating string buffer in characters.
\\
1024 chars constant #"buf		\ 3 full-length strings.

\\	"buf			( -- c-addr )				COMPILER
\\
\\		c-addr is the address of the rotating string buffer.
\\
create "buf  #"buf allot

\\	>"buf			( -- a-addr )				COMPILER
\\
\\		a-addr  is the address of >"BUF.  >"BUF contains the offset
\\		in characters from the start of the rotating string  buffer
\\		to the next free address.
\\
variable >"buf

\\	"allot			( n -- c-addr )				COMPILER
\\
\\		Allocate n characters in the rotating string buffer. c-addr
\\		is the address of the free area.
\\
: "allot
  #"buf over >"buf @ + u<		\ Enough free to end
  if  0 >"buf !  then			\ rewind
  >"buf @ "buf +			\ address
  swap >"buf +!				\ advance
;

\\	"place			( c-addr u -- c-addr' )			COMPILER
\\
\\		Place string c-addr and u as counted string in the rotating
\\		string buffer. u is placed as first character in the buffer
\\		followed  by the string c-addr.  c-addr' is the address  of
\\		the temporary counted string.
\\
: "place
  dup char+ "allot dup >r place r>
;

forth definitions

\\	concat	( c-addr1 u1 c-addr2 u2 -- c-addr3 u3 )		STRING ALE
\\
\\		Append the strings c-addr2 u2 to c-addr1 u1 and returns the
\\		concatenated  string c-addr3 u3.  c-addr3 u3 is  placed  in
\\		the rotating string buffer.
\\
: concat
  2>r dup r@ + dup "allot 2>r r@ swap move 2r> swap
  2r> 2over rot over swap - /string move
;

\ -----------------------------------------------------------------------------
\	Null terminated strings.
\ -----------------------------------------------------------------------------

\\	0"place			( c-addr1 u1 c-addr2 -- )	STRING/UNIX ALE
\\
\\		Place  string  c-addr1  and u1 at address  c-addr2  as  nul
\\		terminated string.
\\
: 0"place
  2dup + char+ 0 swap c! swap cmove
;

\\	">0		( c-addr u -- 0-addr )			NUL STRING
\\
\\		Converts  a forth string c-addr and u to a null  terminated
\\		string 0-addr.  ( 0-addr is in rotating string buffer )
\\
: ">0
  dup >r char+ "allot
  swap over r@ cmove
  0 over r> + c!
;

\\	0>"		( 0-addr -- c-addr u )			NUL STRING
\\
\\		Converts  a null terminated string 0-addr to a forth string
\\		c-addr and u.
\\
/less-code [if]
: 0>"
  dup
  begin
    dup c@
  while
    1+
  repeat
  over -
;
[else]
code 0>"
	push-tos
	pushl	%ecx
	pushl	%edi

	movl	tos, %edi		\ 0-addr
	xorl	%eax, %eax
	leal	-1 (%eax), %ecx		\ -1 very much

	repne
	scasb	%al, (%edi)

	subl	tos, %edi
	movl	%edi, tos

	popl	%edi
	popl	%ecx
end-code
[then]
