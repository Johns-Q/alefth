\
\	ALE Forth Version 2.04
\
\		Copyright (c) 1991,1992 by Lutz Sammer.
\
\		Bug-reports via usenet: johns
\
\	File:	tools.4th
\
\		Forth kernel resident tools.
\

\ -----------------------------------------------------------------------------
\	Facility Words
\ -----------------------------------------------------------------------------

\\	page			( -- )				FACILITY EXT
\\
\\		Move to another page for output. Actual function depends on
\\		the  output device.  On a terminal, PAGE clears the  screen
\\		and resets the cursor position to the upper left corner. On
\\		a printer, PAGE performs a form feed.
\\
: page
  ff# emit
;

\ -----------------------------------------------------------------------------
\	Resident Tools
\ -----------------------------------------------------------------------------

\\	[then]			( -- )			I	TOOLKIT EXT
\\
\\		Does nothing.  This word is immediate.
\\
: [then]
; immediate

\\	[else]			( -- )			I	TOOLKIT EXT
\\
\\		Parse  and  discard  blank-delimited  words  from the input
\\		stream,  ignoring leading delimiters, until the word [THEN]
\\		has  been  parsed  and  discarded.  If  the input stream is
\\		exhausted  while  parsing  words,  it  is  refilled as with
\\		REFILL.  Nested  occurrences of [IF] ... [THEN] or [IF] ...
\\		[ELSE] ... [THEN] are discarded.  This word is immediate.
\\
: [else]
  1
  begin
    begin
      bl word count dup
    while
      2dup s" [if]" compare
      0=if
	  2drop 1+
      else
	2dup s" [ifdef]" compare
	0=if
	    2drop 1+
	else
	  2dup s" [ifndef]" compare
	  0=if
	    2drop 1+
	  else
	    2dup s" [else]" compare
	    0=if
	      2drop 1- dup
	      if
		1+
	      then
	    else
	      s" [then]" compare
	      0=if
		1-
	      then
	    then
	  then
	then
      then
      ?dup
      0=if
	exit
      then
    repeat
    2drop
    refill 0= abort" unmatched [if]...[else] at end of file"
  again
; immediate

\\	[if]			( flag -- )		I	TOOLKIT EXT
\\
\\		If  the  flag  is  true,  do  nothing.  Otherwise parse and
\\		discard   blank-delimited   words  from  the  input stream,
\\		ignoring  leading  delimiters, until either the word [ELSE]
\\		or  the  word  [THEN] has been parsed and discarded. If the
\\		input  stream  is  exhausted  while  parsing  words,  it is
\\		refilled  as  with  REFILL.  Nested occurrences of [IF] ...
\\		[THEN] or [IF] ... [ELSE] ... [THEN] are discarded.
\\		This word is immediate.
\\		An ambiguous condition exists when [IF] is POSTPONEd, or if
\\		the  end  of  the  input  stream  is  reached and cannot be
\\		refilled before the terminating [ELSE] or [THEN] is parsed.
\\
: [if]
  0=if [compile] [else] then
; immediate

\\	[ifdef]			( "name" -- )		I	TOOLKIT ALE
\\
\\		Parse   name   delimited   by  a  space,  ignoring  leading
\\		delimiters.  Find name in the current search order.
\\		If name is defined, do nothing. Otherwise parse and discard
\\		blank-delimited  words  from  the  input  stream,  ignoring
\\		leading  delimiters,  until either the word [ELSE]  or  the
\\		word  [THEN]  has been parsed and discarded.  If the  input
\\		stream is exhausted while parsing words, it is refilled  as
\\		with  REFILL.  Nested occurrences of [IF..] ...
\\		[THEN] or [IF..] ... [ELSE] ... [THEN] are discarded.
\\		This word is immediate.
\\		An ambiguous condition exists when [IFDEF] is POSTPONEd, or
\\		if  the  end of the input stream is reached and  cannot  be
\\		refilled before the terminating [ELSE] or [THEN] is parsed.
\\
: [ifdef]
  defined nip [compile] [if]
; immediate

\\	[ifndef]		( "name" -- )		I	TOOLKIT ALE
\\
\\		Parse   name   delimited   by  a  space,  ignoring  leading
\\		delimiters.  Find name in the current search order.
\\		If  name  is NOT defined, do nothing.  Otherwise parse  and
\\		discard   blank-delimited   words  from  the  input stream,
\\		ignoring  leading  delimiters, until either the word [ELSE]
\\		or  the  word  [THEN] has been parsed and discarded. If the
\\		input  stream  is  exhausted  while  parsing  words,  it is
\\		refilled  as with REFILL.  Nested occurrences of [IF..] ...
\\		[THEN] or [IF..] ... [ELSE] ... [THEN] are discarded.
\\		This word is immediate.
\\		An  ambiguous condition exists when [IFNDEF] is  POSTPONEd,
\\		or if the end of the input stream is reached and cannot  be
\\		refilled before the terminating [ELSE] or [THEN] is parsed.
\\
: [ifndef]
  defined nip if [compile] [else] then
; immediate

\\	forget			( "name" -- )			TOOLKIT EXT
\\
\\		Parse   name   delimited   by  a  space,  ignoring  leading
\\		delimiters. Find name, then delete name from the dictionary
\\		along with all words added to the dictionary after name. An
\\		ambiguous condition exists if name cannot be found. If word
\\		lists  are  present,  FORGET  searches the compilation word
\\		list. An ambiguous condition exists if the compilation word
\\		list is deleted.
\\
: forget
  ' (forget)
;
