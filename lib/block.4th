\
\	ALE Forth Version 2.04
\
\		Copyright (c) 1991,1992 by Lutz Sammer.
\
\		Bug-reports via usenet: johns
\
\	File:	block.4th
\
\		Forth library: code blocks.
\

\\	blocks			( -- )				V	BLOCKS
\\
\\		Append the code block word list to the current search set.
\\
\\	    Words:
\\		:[	];	call
\\
\\	    Typical use:
\\		: test   ... :[ ... ]; use-it ... ;
\\
\\		Can  be  used  as an alternative to  passing  functions  as
\\		parameters.
\\
vocabulary blocks  compiler also blocks definitions

\\	:[			( -- block )			I	BLOCKS
\\
\\	    Compilation:	( -- block block-sys )
\\		Compile  a  branch  over  the code  block  in  the  current
\\		definition.  Create an execution token for the code  block.
\\		Information  is  added to the end of the dictionary so that
\\		code  compiled  at  the  next  dictionary  location will be
\\		associated  with block.  This code can be executed later by
\\		using 'block CALL'.
\\		block-sys is balanced by the corresponding ];
\\
\\	    Execution:		( -- block )
\\		Place the execution token block on the stack.
\\
\\	    Interpretation:	( -- block block-sys )
\\		Create  an  execution  token  and  enter compilation state.
\\		Information  is  added to the end of the dictionary so that
\\		code  compiled  at  the  next  dictionary  location will be
\\		associated  with block.  This code can be executed later by
\\		using 'block CALL'.
\\		block-sys is balanced by the corresponding ];
\\
\\	    block Execution:	( i*x -- j*x ) ( R:  -- sys )
\\		Save   implementation-dependent   information   about   the
\\		definition that executed block.
\\		Typically, the execution semantics of block are expanded by
\\		compiling additional words into the definition.
\\
: :[
  compiling				\ If compiling
  if
    postpone ahead			\ Create a branch over block
    here swap				\ address of block code
    true
  else
    here				\ address of block code
    [compile] ]				\ Start compilation of block code
    false
  then
  [ifdef] ale-forth
  nest,
  [then]
; immediate

\\	];							C I	BLOCKS
\\
\\	    Compilation:	( block-sys -- )
\\		Compile  EXIT  (or  an  implementation-dependent  word that
\\		performs an equivalent function) in the current definition.
\\		Return to state before the execution of :[.
\\		block-sys is balanced by the corresponding :[.
\\
\\	    Execution:		( -- ) ( R:  sys -- )
\\		Return  control to the caller of the definition  containing
\\		];.
\\		sys is balanced by the corresponding :[.
\\
: ];
  postpone exit				\ Compile exit from code-block
  if					\ If were are compiling
    postpone then			\ Resolve branch over block
    postpone literal			\ Create a literal for block address
  else
    [compile] [				\ Leave compilation mode
  then
; compilation immediate

\\	call			( i*x block -- j*x )			BLOCKS
\\
\\		Call the block code specified by block.
\\
[ifdef] ale-forth
\ FIXME: use ALIAS
: call
  \ FIXME: ?postpone execute
  compiling
  if
    postpone execute
  else
    execute
  then
; immediate
[else]
: call
  >r					\ Perform the block definition
;
[then]

only forth definitions

false [if]				\ Block examples

:[ over over ]; constant '2dup		\ Generates a unnamed colon definition

1 2 '2dup call .s 4 discard		\ Execute block

: test:[
  :[ 1 ];				\ Generate a block
  call .				\ Perform it
;

: show	( block -- )
  call					\ Perform block
;

: what	( flag -- )			\ Conditional execute different blocks
  if
    :[ ." true" ];
  else
    :[ ." false" ];
  show
;

[then]
