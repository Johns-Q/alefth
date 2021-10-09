\
\	ALE Forth Version 2.04
\
\		Copyright (c) 1991,1992 by Lutz Sammer.
\
\		Bug-reports via usenet: johns
\
\	File:	utility.4th
\
\		Utilities.
\

also compiler

\\	table			( x1 ... xn n -- )		D	UTILITY
\\
\\		Parse   name   delimited   by  a  space,  ignoring  leading
\\		delimiters.  Create  a dictionary  entry  for name with the
\\		execution semantics defined below. name is referred to as a
\\		"table."
\\
\\	    name Execution:	( n -- xn )
\\		Place the n'th table element xn on the stack.
\\
: table
  create 0  do  ,  loop			\ compile n elements
does>	( n table -- )
  swap cells + @			\ execute table lookup
;

also compiler definitions

\\	sintable		( n -- xn )				UTILITY
\\
\\		Sinus lookup table.
\\
10000 9998 9994 9986 9976 9962 9945 9925 9903 9877
 9848 9816 9781 9744 9703 9659 9613 9563 9511 9455
 9397 9336 9272 9205 9135 9063 8988 8910 8829 8746
 8660 8572 8480 8387 8290 8192 8090 7986 7880 7771
 7660 7547 7431 7314 7193 7071 6947 6820 6691 6561
 6428 6293 6157 6018 5878 5736 5592 5446 5299 5150
 5000 4848 4695 4540 4384 4226 4067 3907 3746 3584
 3420 3256 3090 2924 2756 2588 2419 2250 2079 1908
 1736 1564 1391 1219 1045  872  698  523  349  175
    0
\ 91 elements of table placed on stack
 91 table sintable

\\	s180			( n1 -- n2 )				UTILITY
\\
\\		Returns sinus from 0-180 degrees.
\\
: s180
  dup 90 >				\ if greater than 90 degrees
  if
    180 swap -
  then					\ subtract from 180
  sintable				\ then take sine
;

forth definitions

\\	sin			( n1 - n2 )				ALE
\\
\\		Returns sinus of any no. of degrees.
\\
: sin
  360 mod				\ bring within + or - 360
  dup 0<
  if
    360 +
  then					\ if negative, add 360
  dup 180 >				\ test if greater than 180
  if
    180 - s180 negate			\ if so, subtract 180, negate sine
  else
    s180				\ otherwise, straightforward
  then
;

\\	cos			( n1 -- n2 )				ALE
\\
\\		Returns cosinus of any no. of degrees.
\\
: cos  ( n -> cosine )
  360 mod				\ bring within + or - 360
  90 + sin				\ cos is sin with 90 deg phase shift
;

[ifdef] cache-table
\\	.cache				( -- )				ALE
\\
\\		Display contents of the cache and cache statistik.
\\
: .cache
  0 0
  cr ." Execution cache: " cr
  #cache 0
  do
    cache-table i cells + @ dup
    if
      ->name count type space
      1+
    else
      drop swap 1+ swap
    then
  loop

  cr ." Compilation cache: " cr
  #cache 0
  do
    cache-table #cache i + cells + @ dup
    if
      ->name count type space
      1+
    else
      drop swap 1+ swap
    then
  loop

  cr . ." Cache slots used, " . ." Cache slots free"
  [ifdef] cache-hits
    cr cache-hits ? ." Cache hits, "
       cache-miss ? ." Cache miss, "
       cache-coll ? ." Cache collisions"
  [then]
  cr
;
[then]

\\	help			( "name" -- )			UTILITY
\\
\\		Parse   name   delimited   by  a  space,  ignoring  leading
\\		delimiters.  Show the contents of the help-file for name.
\\
: help
  bl word
  s" fth.doc" r/o open-file throw
  begin					\ Find word
    pad 256 pluck read-line throw
    0=if
      drop close-file throw
      count type true abort" :no entry found"
    then
    \ word fileid len
    \ ." SKIP:" pad over type cr
    pluck dup count pad over compare
    \ 0
    swap c@ pad + c@ bl > or
  while
    drop
  repeat
  \ word fileid len
  cr 0 -rot
  begin					\ Show help
    pad swap type cr
    swap 1+ dup #rows =
    if
      key 2drop 0
    then
    swap
    pad 256 pluck read-line throw
    pad c@ 33 < and 0=
  until
  drop close-file throw 2drop
;

\\	add-doc			( c-addr u -- )				UTILITY
\\
\\		Add file specified by c-addr and u to the doc-file.
\\
: add-doc
  r/o open-file throw
  s" fth.doc" w/o open-file throw
  dup file-size throw
  pluck reposition-file throw
  ( fid-read fid-write)
  begin
     pad 256 3 pick read-line throw
  while
    dup 1 >
    if
      pad dup c@ [char] \ =
      swap 1+ c@ [char] \ = and
      if
	 ( fid-read fid-write len pad len)
	 pad over dup 2 = 3 + /string 3 pick
	 write-line throw
      then
    then
    drop
  repeat
  drop close-file throw close-file throw
;

\\	primitives		( -- )				UTILITY
\\
\\		Display all primitives with it lengths.
\\		(For debuging written)
\\
: primitives
  context @
  cr dup .vocabulary [char] : emit cr
  @
  begin
    ?dup
  while
    dup ->size c@ ?dup
    if
      2 spaces
      over ->name count tuck type
      32 swap - spaces . cr
      ?keypause
    then
    @
  repeat
;

\\	.entry			( xt -- )			UTILITY
\\
\\		Print a entry.
\\
: .entry
  cr
  ." entry#" dup . cr			\ Print entry address
  ." name: " dup .name cr		\ Print name
  dup >link
  ." link: " dup ? cr			\ Print link-field
  ." comp: " dup ->comp @
    ?dup  if  .name  then  cr		\ Print compiler-field
  ." flag: " dup ->flag c@ . cr		\ Print flag-field
  ." size: " ->size c@ . cr		\ Print primitive-size
  ." code: "				\ Print code-field
\  dup c@ ( call) 232 =
  dup c@ ( jmp) 233 =
  if
    1+ dup @ + cell+ .name
  else
    ." Unkown" drop
  then
  cr
;

\\	.base			( -- )					UTILITY
\\
\\		Display the current base in decimal.
\\
: .base
  cr ." Current base: "
  base @ dup decimal . base !
;

compiler definitions

\\	day-table		( -- a-addr )				ALE
\\
\\		a- addr is the address of the DAY-TABLE.  DAY-TABLE is used
\\		by GET-DATE.
\\
create day-table
  31 c, 28 c, 31 c, 30 c, 31 c, 30 c,
  31 c, 31 c, 30 c, 31 c, 30 c, 31 c,

forth definitions

\\	get-date		( -- +n1 +n2 +n3 +n4 +n5 +n6 )	FACILITY EXT
\\
\\		Return the current time and date.
\\		+n1 is the second	(0-59),
\\		+n2 is the minute	(0-59),
\\		+n3 is the hour		(0-23),
\\		+n4 is the day		(1-31),
\\		+n5 is the month	(1-12),
\\		and +n6 is the year	(e.g., 1991).
\\
: get-date
  time throw
  \ time
  86400 /mod				\ Break into days.
  \ hms day
  dup 0<
  if
    1- swap 86400 + swap
  then
  >r
  60 /mod				\ Seconds
  \ sec hms
  60 /mod				\ Minutes
  \ sec min hms
  r>
  dup 0<
  if					\ Before 70 Begin of time
    true abort" FIXME: Not written"
  else
    70
    \ day year
    begin
      swap over 4 mod
      if				\ 0 4 8
	365
      else
	366
      then
      over <
    while
      over 4 mod
      if				\ 0 4 8
	365
      else
	366
      then
      - swap 1+
    repeat
  then
  \ sec min hms year day
  12 0					\ Month
  do
    i 2dup day-table + c@ <
    if
      1+ leave
    then
    day-table + c@ -
  loop
  \ year day month
  swap 1+ swap rot 1900 +
;

false [if]
    if( day>=0 ) {
	for( i=BOTIME; day>=((i%4) ? 365 : 366); ++i )
	    day-=(i%4) ? 365 : 366;
    } else {
	for( i=BOTIME; day<0; --i )
	    day+=((i-1)%4) ? 365 : 366;
    }
[then]

\\	.date				( -- )				UTILITY
\\
\\		Display the current date.
\\
: .date
  get-date -rot
  0 <# # # #> type [char] / emit
  0 <# # # #> type [char] / emit
  0 <# # # # # #> type
  2drop drop
;

\\	.time				( -- )				UTILITY
\\
\\		Display the current time.
\\
: .time
  get-date 2drop drop
  0 <# # # #> type [char] : emit
  0 <# # # #> type [char] : emit
  0 <# # # #> type
;

\\	at-xy			( u1 u2 -- )			FACILITY EXT
\\
\\		Perform  implementation-dependent  steps so that  the  next
\\		character  output  will appear in column u1, row u2 of  the
\\		current output device.  The upper left corner of the output
\\		device  is  row zero, column zero.  An ambiguous  condition
\\		exists  if the operation cannot be performed on the current
\\		output device with the specified parameters.
\\
: at-xy
  \ FIXME: only ansi
  ." [" (.) type [char] ; emit (.) type [char] H emit
;

[ifdef] unix
\\	ms			( u -- )			FACILITY EXT
\\
\\		Wait at least u milliseconds.
\\		Note:	The  actual  length  and variability  of  the  time
\\			period  depends  upon   the  implementation-defined
\\			resolution  of  the  system  clock,  and upon other
\\			system  and  computer  characteristics  beyond  the
\\			scope of this standard.
\\		Ticks:	tens of milliseconds.
\\
\\		FIXME: MSDOS
\\
: ms
  nap 2drop
;
[then]

[ifdef] blk				\ { Block word set present

\\	.buffers		( -- )				BLOCK ALE
\\
\\		Display contents of the block buffers.
\\
: .buffers
  cr
  ." buffer address update block# fileid file" cr
  ." ------ ------- ------ ------ ------ ----" cr
  #buffers 0
  do
    i dup 3 u.r
    >buffer
    dup ->buffer @ 11 u.r
    dup ->update @
    if
      ."   Dirty"
    else
      ."   Clean"
    then
    dup ->fileid 2@ or
    if
      dup ->block# @ 7 u.r
      ->fileid @ dup 7 u.r
      space .file
    else
      ."  Unused buffer"
      drop
    then
    cr
  loop
;

[then]					\ }

\\	.files			( -- )				FILE ALE
\\
\\		Display contents of the file table.
\\
: .files
  cr ."  id des     size     seek     line file"
  cr ." --- --- -------- -------- -------- ----"
  cr
  #files 0
  do
    i dup 3 u.r
    >file dup ->fildes @ dup
    if
      4 .r
      dup ->file-size @ 9 u.r
      dup ->file-seek @ 9 u.r
      dup ->file-line @ 9 u.r
      dup
      space ->file-name count type
    else
      drop ."  Unused file slot"
    then
    drop
    cr
  loop
;

\\	.run-table		( -- )				DEBUG ALE
\\
\\		Display contents of the current run table.
\\
: .run-table
  cr ." Interpret run-table:"
  5 0
  do
    i cells run-table + @ space .name
  loop
;

\\	vocs			( -- )				SEARCH ALE
\\
\\		List all of the vocabularies that have been defined so far,
\\		in the order of their definition.
\\
: vocs
  ." Vocabularies:" voc-link @
  begin
    ?dup
  while
    dup body> .name space		\ print vocabularies name
    cell+ @
  repeat
;

\\	wids			( -- )				SEARCH ALE
\\
\\		List  all of the wordlist the have been created so far,  in
\\		the order of their creation.
\\
: wids
  ." Wordlists:" wid-link @
  begin
    ?dup
  while
    dup . cell+ cell+ @
  repeat
;

\\	ascii							I	F83
\\
\\	    Compilation:	( "ccc< >" -- )
\\		Parse characters ccc delimited by a space, ignoring leading
\\		delimiters.  Compile  char,  the integer value of the first
\\		character of ccc, as a literal.
\\
\\	    Execution:		( -- char )
\\		Place char on the stack.
\\
\\	    Interpretation:	( "ccc< >" -- char )
\\		Parse characters ccc delimited by a space, ignoring leading
\\		delimiters.  Put  the  integer value of its first character
\\		onto the stack.
\\
: ascii
  bl word char+ c@
  compiling
  if
    postpone literal
  then
; immediate

hex

[ifdef] unix
\\	valid?			( addr -- addr )			UNIX
\\
\\		Prove addr is a valid forth dictionary address.
\\
: valid?
  dup 000000 [ etext 1+ ] literal within
  if
    true exit
  then
  dup 400000 here 1+ within
;
[then]

[ifdef] msdos
\\	valid?			( addr -- addr )			MSDOS
\\
\\		Prove addr is a valid forth dictionary address.
\\
: valid?
  dup 001000 [ etext 1+ ] literal within
  if
    true exit
  then
  dup 400000 here 1+ within
;
[then]

decimal

[ifdef] unix

also signals

\\	dump-me			( -- )					ALE
\\
\\		Raise the abort signal, to dump the kernel.
\\
: dump-me
  ." Dumping..." cr
  sig-abort default-signal
  sig-abort raise
;

[then]

hex

\\	write-dump		( c-addr u -- )
\\
\\		Write the dictionary to the file c-addr and u.
\\
: write-dump
  r/w create-file throw
  400000 dp0 #dp + over - 1- pluck write-file throw
  close-file throw
;

\\	read-dump		( c-addr u -- )
\\
\\		Read the dictionary from the file c-addr and u.
\\		(The kernel version may not have been changed!)
\\
: read-dump
  r/o open-file throw
  dup file-size throw drop
  400000 swap pluck read-file throw drop
  close-file throw
;

decimal

only forth definitions

\\	$hex			( -- )				V
\\
\\		Vocabulary for hex literals support.
\\
vocabulary $hex  also $hex definitions

\\	$hex?		( c-addr u -- 0 )			R
\\		-or-	( c-addr u -- x 1 )
\\		-or-	( c-addr u -- d 2 )
\\
\\		Vocabulary $hex literal recognizer.
\\
\\	    Syntax:
\\		Convertible string := { <single-cell> | <double-cell> }
\\		<single-cell>	   := $<hex-digit><hex-digit>*
\\		<double-cell>	   := $<hex-digit><hex-digit>*.
\\		<hex-digit>	   := { 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
\\					| A | B | C | D | E | F }
\\
: $hex?
  over c@ [char] $ =
  if
    1 /string				\ Remove '$'
    base @ >r hex
      0 0 2swap >number
    r> base !
    ?dup
    0=if				\ Input complete converted
      2drop 1 exit
    then
    1 =
    if
      c@ [char] . =
      if				\ Double number
	2 exit
      then
    else
      drop
    then
  then
  2drop 0
; recognizer

previous definitions

\\	'char			( -- )					V
\\
\\		Vocabulary for character literal support.
\\
vocabulary 'char  also 'char definitions

\\	'char?			( c-addr u -- n 1 )			R
\\			-or-	( c-addr u -- 0 )
\\
\\		Vocabulary 'char literal recognizer.
\\
\\	    Syntax:
\\		Convertible string := '<character>'
\\		<character>	   := { hex 20 - 7F }
\\
: 'char?
  3 =
  if
    dup c@ [char] ' =
    if
      dup 2 + c@ [char] ' =
      if				\ 3 Chars first and last char is '
	1+ c@ 1 exit
      then
    then
  then
  drop 0
; recognizer

previous definitions
