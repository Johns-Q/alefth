\
\	ALE Forth Version 2.04
\
\		Copyright (c) 1991,1992 by Lutz Sammer.
\
\		Bug-reports via usenet: johns
\
\	File:	internal.4th
\
\		Forth kernel internals.
\

\ -----------------------------------------------------------------------------
\	Compiler - internals
\ -----------------------------------------------------------------------------

compiler definitions

0	constant NORMAL			\ Normal word
1	constant IMMEDIATE		\ Immediate word
2	constant COMPILATION		\ Compilation-only word
4	constant EXECUTION		\ Execution-only word
6	constant HIDDEN			\ Hidden for definition
8	constant PRIVATE		\ Private word only,
\					  only visibile in definition vocabulary

\\	Dictionary entry:
\\
\\		cell	->link		Link field
\\		cell	->comp		Compiler field
\\		byte	->flag		Flag field
\\		byte	->noff		Name offset
 0	field ->link	( lfa -- lfa )
 4	field ->comp	( lfa -- cfa )
 8	field ->flag	( lfa -- ffa )
 9	field ->noff	( lfa -- ofa )
10	field ->size	( lfa -- sfa )
11	field ->code	( lfa -- xfa )
16	field ->data	( lfa -- dfa )

\\	->name			( lfa -- nfa )				ALE
\\
\\		Move  from the link field address of a execution token  to
\\		the name field.
\\
: ->name
  dup ->noff c@ -
;

forth definitions

\\	>body			( xt -- a-addr )			CORE
\\
\\		a-addr  is the data field address corresponding to xt for a
\\		word defined via CREATE.
\\
5	field >body

\\	body>			( a-addr -- xt )			ALE F83
\\
\\		xt is the execution token corresponding to a-addr.
\\		a-addr is the data field address.
\\
-5	 field	body>

\\	>link			( xt -- a-addr )			ALE F83
\\
\\		a-addr is link field address corresponding to xt.
\\
-11	field	>link

\\	>name			( xt -- c-addr )			ALE F83
\\
\\		c-addr is the name field address corresponding to xt.
\\
: >name
\			    v noff
  dup ( xt -> name-length ) -2 + c@ ->code -
;

\\	>flag			( xt -- c-addr )			ALE
\\
\\		c-addr is the flag field address corresponding to xt.
\\
-3	field	>flag

\\	>compiler		( xt - a-addr )				ALE
\\
\\		a-addr is the compiler field address corresponding to xt.
\\
-7	field	>compiler

\\	.name			( xt -- )				ALE
\\
\\		Print the name of the execution token xt.
\\
: .name
  >name count type
;

compiler definitions

forth definitions
