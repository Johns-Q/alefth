\
\	ALE Forth Version 2.04
\
\		Copyright (c) 1991,1992 by Lutz Sammer.
\
\		Bug-reports via usenet: johns
\
\	File:	variable.4th
\
\		Common variables and constants.
\

\\	state			( -- a-addr )				CORE
\\
\\		a-addr   is  the  address  of  STATE.  STATE  contains  the
\\		compilation  state  flag. STATE is true when in compilation
\\		state,  false  otherwise.  The  true value in STATE is non-
\\		zero,  but  is  otherwise implementation-defined.  Only the
\\		following standard words alter the value in STATE:
\\			: (colon), ; (semicolon), ABORT, QUIT,
\\			:NONAME, [ (left-bracket), and ] (right-bracket).
\\
variable state

\\	compiling		( -- flag )				ALE
\\
\\		Place the contents of state on the stack.
\\
: compiling
  state @
;

\\	bl			( -- char )				CORE
\\
\\		char is the character value for a space.
\\
32	constant bl
