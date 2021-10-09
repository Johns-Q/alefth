\
\	ALE Forth Version 2.04
\
\		Copyright (c) 1991,1992 by Lutz Sammer.
\
\		Bug-reports via usenet: johns
\
\	File:	toolkit.4th
\
\		Toolkit words.
\

also compiler

\\	keypause?		( -- flag )			TOOLKIT ALE
\\
\\		Wait if any key is pressed, until the next key is presssed.
\\		If  one of the keys is ESCape return true; otherwise false.
\\
: keypause?
  key? dup
  if
    key esc# <>
    if
      drop key esc# =
    then
  then
;

\\	?keypause		( -- )				TOOLKIT ALE
\\
\\		Wait if any key is pressed, until the next key is presssed.
\\		If  one  of the keys is ESCape abort  execution;  otherwise
\\		continue execution.
\\
: ?keypause
  keypause? abort" Stopped"
;

\\	words			( -- )				TOOLKIT EXT
\\
\\		List  the  word  names in the first word list of the search
\\		order.   The  format  of  the  display  is  implementation-
\\		dependent.
\\
: words
  cr
  context @ dup .vocabulary [char] : emit cr
  begin
    @ ?dup
  while
    dup ->name count
    dup out @ + #cols >
    if  cr ?keypause  then		\ Didn't fit on the line
    tuck type				\ Show name
    15 and 16 - negate
    dup out @ + #cols <
    if					\ Fit on the line
      spaces				\ Indent
    else
      drop cr ?keypause
    then
  repeat
;

\\	.s			( -- )				TOOLKIT EXT
\\
\\		Copy  and  display  the values currently on the data stack.
\\		The format of the display is implementation-dependent.
\\		Example:	( 1 2 3 -- 1 2 3 )
\\			.s [3]1\2\3 Ok.
\\
: .s
  ?stack depth ?dup
  if
    dup [char] [ emit (u.) type [char] ] emit
    0 swap 1-
    do
      [char] \ emit i pick
      base @ 10 -
      if  (u.)  else  (.)  then  type
      -1
      keypause? ?leave
    +loop
  else
    ." Stack empty"
  then
  space
;

\\	dump			( addr u -- )			TOOLKIT EXT
\\
\\		Display the contents of u consecutive addresses starting at
\\		addr.  Format of the display is implementation-dependent.
\\		Format:
\\			addr  x1 x2 x3 .. x15 x16 c1c2c3 .. c15c16
\\			8 Hex 2Hex ..             Chars
\\
: dump
  base @ -rot hex
  cr ." address "
  over 16 bounds
  do
    i 15 and 3 .r
  loop
  over 16 bounds
  2 spaces
  do
    i 15 and 1 .r
  loop
  cr bounds
  do
    i dup 0 <# # # # # # # # # #> type space
    16 0
    do
      dup c@ 0 <# # # #> type space
      char+
    loop
    drop space
    i 16 0
    do
      dup c@ dup bl 128 within
      0=if
	drop [char] .
      then
      emit
      char+
    loop
    drop cr
    keypause? ?leave
    16
  +loop
  base !
;

\\	du			( c-addr1 -- c-addr2 )		TOOLKIT ALE
\\
\\		Display  the contents of 64 consecutive addresses  starting
\\		at addr.  c-addr2 is c-addr1 plus 64.
\\		Format:
\\			addr  x1 x2 x3 .. x15 x16 c1c2c3 .. c15c16
\\			8 Hex 2Hex ..             Chars
\\
: du
  dup 64 dump 64 +
;

previous
