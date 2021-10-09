\
\	ALE Forth Version 2.04
\
\		Copyright (c) 1991,1992 by Lutz Sammer.
\
\		Bug-reports via usenet: johns
\
\	File:	x.4th
\
\		Forth experiments.
\

\\	reserve			( n -- n*x a-addr )			ALE
\\
\\		Reserve n cells on the parameter stack.  a-addr is the  low
\\		address of the reserved space.
\\
: reserve
  >r sp@ r> cells - sp! sp@
;

hex

: sc@
  c@ dup 7f >  if  ffffff00 or  then
;

decimal

\\	.rs			( -- )				TOOLKIT ALE
\\
\\		Copy  and display the values currently on the return stack.
\\		The format of the display is implementation-dependent.
\\
\\	    Example:	( R: xt1 2 xt2 -- xt1 2 xt2 )
\\			.s [3]\number\{2}\interpret Ok.
\\			with xt1 is execution token of NUMBER.
\\			with {2} is a number on the return stack.
\\			with xt2 is execution token of INTERPRET.
\\
: .rs
  rp0 rp@ 2dup - 2 >>
  [char] [ emit (.) type [char] ] emit
  ?do
    [char] \ emit
    i @ dup -5 + valid?
    if
      c@ 233 =
    else
      drop false
    then
    if					\ JMP 32bit
      dup cell - @ + .name
    else
      dup -2 + valid?
      if
	c@ 235 =
      else
	drop false
      then
      if				\ JMP 8bit
	dup 1- sc@ + .name
      else
	[char] { emit
	base @ 10 -
	if  (u.)  else  (.)  then  type
	[char] } emit
      then
    then
    cell
  +loop
  space
;

0 [if]

signals

: sig-int-handler
signal>
  .signal cr
  .rs
;

' sig-int-handler sig-int catch-signal 2drop
sig-segv ignore-signal 2drop

: x .rs ;
: y x ;
: z y ;

[then]

true [if]

\\	?postpone						C I	CORE
\\
\\	    Compilation:	( "name" -- )
\\
\\	    Execution:		( -- )
\\
: ?postpone
  defined dup ?missing 0<
  if					\ Immediate: add it now
    compile,
  else					\ Normal: add it at runtime
    postpone literal			\ Store execution token
    postpone compiling
    postpone if				\ In compilation mode
    postpone   compile,			\ compile xt
    postpone else
    postpone   execute			\ execute xt
    postpone then
  then
; compilation immediate

\ : call
\   ?postpone execute
\ ; immediate

\ : 1+!
\  1 over @ + swap !
\ ; macro

\ Relocate:	 jmp	displacement32
\		 jmp	displacement8

[then]

false [if]

: include-file
  >in @ #tib @ 2>r			\ Save current input stream
  tib source-file 2>r
  [ifdef] blk
    blk @ >r

    blk off				\ Setup new input stream
  [then]
  dup to source-file
  file-size throw drop
  dup >r 2 >> 1+ reserve to tib		\ Allocate memory
  tib r@ source-file read-file throw
  #tib ! 0 >in !
  interpret
  r> discard
  source-file close-file throw		\ Close current input stream

  [ifdef] blk				\ Restore prior input stream
    r> blk !
  [then]
  2r> to source-file to tib
  2r> #tib ! >in !
;

: included
  2dup r/o open-file
  if
    drop type true abort" :Can't open file"
  then
  nip nip
  include-file
;

: \
  blk @
  if					\ BLOCK
    >in @ negate c/l mod >in +!
  else
    tib #tib @ >in @ /string
    nl# scan drop tib - >in !
  then
; immediate

' \ alias \\

[then]

compiler

: complete	( c-addr u -- c-addrn un ... c-addr0 u0 n )
  0 -rot
  #order 0
  do
    2>r
    i cells context + @
    begin
      @ dup
    while
      dup ->name count ( lfa c-addr1 u1 )
      r@ over >				\ Words >=
      if
	2drop
      else
	over r@ 2r@ compare		\ Matched c-addr u
	if
	  2drop
	else
	  2swap swap 1+ swap		\ Place on stack
	then
      then
    repeat
    drop
    2r>
  loop
  2drop
;

\	FIXME: Remove double entries

: complete>one	( c-addrn un ... c-addr0 u0 n -- c-addr u | 0 )
  dup
  if					\ Some found
    dup 1-
    0=if				\ 1 found
      drop exit
    then
					\ Find longest match
    2* discard 0
  then
;

\	included
\	include-file -> include

: .complete
  0
  do
    type space
  loop
;

: split	( c-addr1 u1 -- c-addr2 u2 )
  begin
    bl skip 2dup
    bl scan dup
  while
    2swap 2drop
  repeat
  2drop
;

: -accept
  >r 0					\ Count
  begin
    dup r@ <
  while
    key case
      bsp# of				\ Backspace
	dup if
	  1- bsp# emit space bsp# emit
	then
      endof
      esc# of				\ Escape
	begin
	  dup
	while
	  bsp# emit space bsp# emit 1-
	repeat
      endof
      9 of				\ Tabulator
	2dup split cr
	complete .complete
	cr 2dup type
      endof
      ret# of				\ Return
	( c-addr offset )
	nip r> drop			\ Cleanup
	exit
      endof
      \ default
	( c-addr offset key )
	>r 2dup + r> over c!		\ Store key
	1 type 1+			\ Display key
      endof
    endcase
  repeat
  nip r> drop
;

false [if]

compiler also signals

: save-cursor
  esc# (emit) " [s" (type)
;

: restore-cursor
  esc# (emit) " [u" (type)
;

: tick
signal>
  save-cursor
  signal-addr@
  0 0 at-xy ." Forth:" . .date space .time space cr
  restore-cursor
  drop
  1 alarm
;

' tick sig-alarm catch-signal throw drop
1 alarm

only forth

[then]

[ifdef] unix

$hex also compiler

: sysi86
  $32 sys-call
;

100 constant SI86SHFIL

\ struct mmf
\	char*	mf_filename;		Filename of file to map
\	long	mf_filesz;
\	long	mf_regsz;
\	short	mf_flags;
\ end-struct
0	field	mf_filename
4	field	mf_filesz
8	field	mf_regsz
12	field	mf_flags
14	constant	sizeof(mmf)

101 constant SI86SHPCHRGN

\ struct cmf
\	char*	cf_srcva
\	char*	cf_dstva
\	long	cf_count
\ end-struct
0	field	cf_srcva
4	field	cf_dstva
8	field	cf_count
12	constant	sizeof(cmf)

create mmf sizeof(mmf) allot
create cmf sizeof(cmf) allot

: map-file	( 0-addr flags -- reg-addr )
  mmf mf_flags w!
  -1 mmf mf_regsz !
  -1 mmf mf_filesz !
  mmf mf_filename !
  mmf SI86SHFIL 8 sysi86 throw
;

: mod-file	( src dst n -- )
  cmf cf_count !
  cmf cf_dstva !
  cmf cf_srcva !
  cmf SI86SHPCHRGN 8 sysi86 throw
;

4 constant RG_NOSHARE

value map-addr

: map
  0" test" 0 map-file
  dup to map-addr
  ." Addr  :" . cr
  ." File  :" mmf mf_filesz ? cr
  ." Region:" mmf mf_regsz  ? cr
  ." Flags :" mmf mf_flags  w@ . cr
;

: unmap
  0" test" mmf mf_filename !
  0 mmf mf_regsz !
  0 mmf mf_filesz !
  4 mmf mf_flags w!
  mmf SI86SHFIL 8 sysi86 throw
  ." Addr  :" . cr
  ." File  :" mmf mf_filesz ? cr
  ." Region:" mmf mf_regsz  ? cr
  ." Flags :" mmf mf_flags  w@ . cr
;

[then]

\\	Library:
\\		Random numbers:
\\			seed	srand	rand	random
\\

\\	seed			( -- n )				LIBRARY
\\
\\		n is the random-number generator seed.
\\		The generator is initially seeded with a value of 1.
\\
value seed  1 to seed

\\	srand			( n -- )				LIBRARY
\\
\\		Reset  the  random-number  generator to a  random  starting
\\		point.
\\
: srand
  to seed
;

\\	rand			( -- n )				LIBRARY
\\
\\		n  is a generated pseudo-random number in the range from  0
\\		to (2^15)-1.
\\
: rand
  1103515245 seed * 12345 + dup to seed 16 >> 32767 and
;

\\	random			( n -- n1 )				LIBRARY
\\
\\		n1 is a generated pseudo-random number in the range from  0
\\		to n.  ( 0<=n1<=n )
\\
: random
  rand 32767 */
;

\\	+dp			( n -- ior )				LIBRARY
\\
\\		Change the dictionary size.
\\
: +dp
  #dp dup dp0 + brk-val -
  if					\ Break value isn't end of dictionary
    drop true exit
  then
  + dup to #dp dp0 + brk
;

\\	getenv	( c-addr1 u1 -- c-addr2 u2 ) or ( c-addr1 u1 -- 0 )	LIBRARY
\\
\\		The  getenv  function searches the environment list  for  a
\\		string  of  the form name = value and returns a  the  value
\\		c-addr2  and u2 in the current environment if such a string
\\		is present, otherwise 0.
\\
: getenv
  2>r
  environ @
  begin
    dup @ dup
  while
    0>"
    2dup [char] = scan nip -
    2r@ compare
    0=if				\ Found
      2r> 2drop @ 0>"
      [char] = scan 1 /string exit
    then
    cell+
  repeat
  2r> 4 discard
  0
;

: view-fields	( c-addr u -- c-addr1 u1 c-addr2 u2 )
  bl <skip				\ Skip less leading delimiter
  over swap
  bl <scan				\ Find less ending delimiter
  >r dup >r over - r> r>
;

\	(view)	( "name" -- c-addr1 u1 c-addr2 u2 c-addr3 u3 )		UTILITY
\
\		c-addr1	u1	Entry name
\		c-addr2 u2	File name
\		c-addr3 u3	Line number
\
: (view)
  bl word
  s" view.fth" r/o open-file throw
  begin
    pad 256 pluck read-line throw
    0=if
      close-file throw
      drop count type true abort" :no entry found"
    then
    \ word fileid len
    pluck dup count pad over compare
    \ 0
    swap c@ pad + c@ bl > or
  while
    drop
  repeat
  \ word fileid len
  >r close-file throw drop
  \ len
  pad r>
  view-fields				\ Name
  view-fields				\ File
  view-fields				\ Line
  2drop
;

: view
  (view) ( entry file n )
  s" +" 2swap concat s"  " concat 2swap concat
  2swap 2drop vi
;

also blocks

\\	for-each-voc		( xt -- )			SEARCH ALE
\\
: for-each-voc
  voc-link @
  begin
    dup
  while
    dup pluck call
    cell+ @
  repeat
  2drop
;

\\	vocs			( -- )				SEARCH ALE
\\
\\		List all of the vocabularies that have been defined so far,
\\		in the order of their definition.
\\
: vocs
  cr ." Vocabularies:"
  :[	( dfa -- )
    body> .name space
  ];					\ print vocabularies name
  for-each-voc
;

: all-primitives
  cr ." Primitives:"
  :[	( dfa -- )
    cr dup body> .name [char] : emit cr
    @ @
    begin
      dup
    while
      dup ->size c@ dup
      if
	2 spaces
	over ->name count tuck type
	32 swap - spaces . cr
	?keypause
      then
      drop @
    repeat
    drop
  ];
  for-each-voc
;

: all-code
  cr ." Code words:"
  :[	( dfa -- )
    cr dup body> .name [char] : emit cr
    @ @
    begin
      dup
    while
      dup ->comp @ ['] <code> =
      if
	2 spaces dup ->name count type
	?keypause
      then
      @
    repeat
    drop
  ];
  for-each-voc
;

: all-words
  cr ." Words:"
  :[	( dfa -- )
    cr ." Vocabulary:"
    dup body> .name [char] : emit cr	\ Vocabulary name
    @ @
    begin
      dup
    while
      2 spaces dup ->name count tuck type
      32 swap - spaces
      dup ->comp @ .name cr
      ?keypause
      @
    repeat
    drop
  ];
  for-each-voc
;

\ 60 7F
\ 0110_0000  0111_1111 
\

[ifdef] #cache

: Xhash
  dup c@
  over char+ c@ 4 << over xor >r	\ len^byte[0]
  + c@ 1 << r> xor			\ len^byte[0]^byte[len]
  [ #cache 1- ] literal and
;

create cache-stat #cache cells allot

: cache-test
  cache-stat #cache cells erase
  :[	( dfa -- )
    @ @
    begin
      dup
    while	( lfa )
      \ dup 2 spaces ->name count type cr
      dup ->name hash cells cache-stat + 1+!
      @
    repeat
    drop
  ];
  for-each-voc
  #cache 0
  do
    cr i 3 .r space cache-stat i cells + ? 
  loop
  cr
;

: cache-test-forth
  cache-stat #cache cells erase
  context
  @ @
  begin
    dup
  while	( lfa )
    dup 2 spaces ->name count type cr
    dup ->name hash cells cache-stat + 1+!
    @
  repeat
  drop
  #cache 0
  do
    cr i 3 .r space cache-stat i cells + ? 
  loop
  cr
;

[then]

only forth definitions
