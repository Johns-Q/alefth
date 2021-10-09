    \
\	ALE Forth Version 2.04
\
\		Copyright (c) 1991,1992 by Lutz Sammer.
\
\		Bug-reports via usenet: johns
\
\	File:	blkfile.4th
\
\		Forth kernel block and file words.
\

\ -----------------------------------------------------------------------------

/block [if]

\\	blk			( -- a-addr )				BLOCK
\\
\\		a-addr  is  the  address of BLK. BLK contains the number of
\\		the  mass  storage  block  being  interpreted  as the input
\\		stream. If BLK contains zero, the input is being taken from
\\		the text input buffer specified by TIB.
\\
variable blk

\\	block-fid		( -- a-addr )				FILE
\\
\\		Return  the  address  of  a  fileid.  The  identified  file
\\		contains blocks;  block requests by words in the Block word
\\		set  use  the  blocks  in  this file. if BLOCK-FID contains
\\		zero,  block  requests  made by words in the Block word set
\\		use an implementation-defined default block space.
\\
variable block-fid

[then]

\ -----------------------------------------------------------------------------
\	File-Access Word Set
\ -----------------------------------------------------------------------------

\\	source-file		( -- 0 | -1 | fileid )			FILE
\\
\\		Identifies  the source of the non-block input streams (i.e,
\\		when BLK is zero) as follows:
\\			source-file	Input stream source
\\			0		Keyboard
\\			-1		String (via EVALUATE)
\\			fileid		Text file "fileid"
\\
value source-file

compiler definitions

\\	file structure							FILE ALE
\\
\\		cell	fildes		File descriptor
\\		cell	file-size	Size of file
\\		cell	file-seek	Seek position in file
\\		cell	file-line	File line number
\\	256	bytes	file-name	File name
\\	240	bytes	file-buffer	Line input buffer
\\
240	constant #fib
  0	field ->fildes
  4	field ->file-size
  8	field ->file-seek
 12	field ->file-line
 16	field ->file-name
272	field ->file-buffer

\\	b/file			( -- n )				FILE ALE
\\
\\		n is the size of the file structure.
\\
512	constant b/file

\\	#files			( -- n )				FILE ALE
\\
\\		n is the maximal number of open files.
\\
16	constant #files

\\	file-table		( -- a-addr )				FILE ALE
\\
\\		returns the address of FILE-TABLE.
\\
create file-table  #files b/file * allot
\ file-table #files b/file * erase

\\	>file			( fileid -- a-addr )			FILE ALE
\\
\\		a-addr is the address of the file structure for fileid.
\\
: >file
  b/file * file-table +
;

\\	.file			( fileid -- )			FILE ALE
\\
\\		Print the the name of file identified by fileid.
\\
: .file
  >file ->file-name count type
;

forth definitions

\\	r/o			( -- x )				FILE
\\
\\		x  is  the implementation dependent value for selecting the
\\		"read only" file access mode.
\\
exist target-msdos [if]
1 constant r/o
[else]
0 constant r/o
[then]

\\	w/o			( -- x )				FILE
\\
\\		x  is  the implementation dependent value for selecting the
\\		"write only" file access mode.
\\
exist target-msdos [if]
2 constant w/o
[else]
1 constant w/o
[then]

\\	r/w			( -- x )				FILE
\\
\\		x  is  the implementation dependent value for selecting the
\\		"read/write" file access mode.
\\
exist target-msdos [if]
4 constant r/w
[else]
2 constant r/w
[then]

\\	open-file		( c-addr u x1 -- x2 ior )		FILE
\\
\\		Open  the  file  named in the character string specified by
\\		c-addr  and u, and open it open it with file access methode
\\		x1.  The meaning of values of x1 is implementation-defined.
\\		If  the file is successfully opened, ior is zero, x2 is the
\\		fileid,  and  the  file  is  positioned to the start of the
\\		file.  Otherwise  ior  is  the  implementation-defined  I/O
\\		result code and x2 is an unspecified value.
\\
: open-file
  [ /block ] [if]
    #files 0
  [else]
    #files 1
  [then]
  do					\ Find free slot
    i >file dup ->fildes @
    0=if				\ Free slot
      dup ->file-line off
      dup ->file-seek off
      >r
      -rot r@ ->file-name place
      r@ ->file-name
      count over + 0 swap c!		\ Build 0 terminated string
      ( 0666) 438 -rot open dup
      0=if
	swap r@ ->fildes !
	SEEK_END 0 r@ ->fildes @ seek throw
	r@ ->file-size !
	SEEK_SET 0 r> ->fildes @ seek throw drop
	i swap
      else
	r> drop
      then
      unloop exit
    then
    drop
  loop
  true abort" Too many open files"
;

\\	create-file		( c-addr u x1 -- x2 ior )		FILE
\\
\\		Create the file named in the character string specified by
\\		c-addr and u, and open it open it with file access methode
\\		x1. The meaning of values of x1 is implementation-defined.
\\		If  a file by the same name already exists, recreate it as
\\		an  empty  file.  If the file was successfully created and
\\		opened,  ior  is  zero, x2 is the fileid, and the file has
\\		been  positioned  to the start of the file. Otherwise, ior
\\		is the implementation-defined I/O result code and x2 is an
\\		unspecified value.
\\
: create-file
  ( O_CREAT O_TRUNC 01400 = ) 768 or open-file
;

\\	close-file		( fileid -- ior )			FILE
\\
\\		Close   the   file   identified   by  fileid.  ior  is  the
\\		implementation-defined I/O result code.
\\		All buffered informations are writen to the file.
\\
: close-file
  >file ->fildes dup @ close swap off
;

\\	file-position		( fileid -- ud ior )			FILE
\\
\\		ud  is the current file position for the file identified by
\\		fileid.  ior is the implementation-defined I/O result code.
\\
: file-position
  >file ->fildes @ SEEK_CUR 0 rot seek 0 swap
;

\\	reposition-file		( ud fileid -- ior )			FILE
\\
\\		Reposition the file identified by fileid to ud.  ior is the
\\		implementation-defined   I/O  result  code.   An  ambiguous
\\		condition exists if the file is positioned outside the file
\\		boundaries.   At   the   conclusion   of   the   operation,
\\		FILE-POSITION returns the value ud.
\\
: reposition-file
  nip ( High word not used )
  >file ->fildes @ SEEK_SET -rot seek nip
;

\\	file-size		( fileid -- ud ior )			FILE
\\
\\		ud  is  the  size, in characters, of the file identified by
\\		fileid.  ior is the implementation-defined I/O result code.
\\		This  operation  does  not  affect  the  value  returned by
\\		FILE-POSITION.
\\
: file-size
\  >file ->file-size @ 0
  >file ->fildes @ >r
  SEEK_CUR 0 r@ seek dup		\ Get current postion
  if  r> 2drop exit  then
  drop
  SEEK_END 0 r@ seek dup		\ Seek to end
  if  r> 2drop exit  then
  drop
  SEEK_SET rot r> seek nip		\ Set position
  0 swap
;

\\	resize-file		( ud fileid -- ior )			FILE
\\
\\		Set the size of the file identified by fileid to ud. ior is
\\		the   implementation-defined   I/O  result  code.   If  the
\\		resultant   file   is  larger  than  the  file  before  the
\\		operation, the portion of the file added as a result of the
\\		operation  may  not have been written. At the conclusion of
\\		the   operation,   FILE-SIZE   returns  the  value  ud  and
\\		FILE-POSITION returns an unspecified value.
\\
: resize-file
  >file ->fildes @ chsize nip
;

\\	read-file		( c-addr u1 fileid -- u2 ior )		FILE
\\
\\		Read  u1  consecutive characters to c-addr from the current
\\		position of the file identified by fileid.
\\		If u1 characters are read without error, ior is zero and u2
\\		is equal to u1.
\\		If  the end of the file is reached before u1 characters are
\\		read,  ior  is  zero  and  u2  is  the number of characters
\\		actually read.
\\		If  the  operation  is initiated when the value returned by
\\		FILE-POSITION  is  equal to the value returned by FILE-SIZE
\\		for  the  file identified  by fileid, ior is zero and u2 is
\\		zero.
\\		If  an  error occurs, ior is the implementation-defined I/O
\\		result  code and u2 is the number of characters transferred
\\		to c-addr without error.
\\		An ambiguous condition exists if the operation is initiated
\\		when  the  value  returned by FILE-POSITION is greater than
\\		the  value returned by FILE-SIZE for the file identified by
\\		fileid,  or  if  the  requested  operation attempts to read
\\		portions of the file not written.  At the conclusion of the
\\		operation,   FILE-POSITION   returns   a   value  past  the
\\		characters consumed by the operation.
\\
: read-file
  >file ->fildes @ >r swap r> read
;

\\	write-file		( c-addr u1 fileid -- ior )		FILE
\\
\\		Write  u1  characters from c-addr to the file identified by
\\		fileid  starting  at  its  current  position.  ior  is  the
\\		implementation-defined  I/O  result code. At the conclusion
\\		of  the  operation,  FILE-POSITION returns a value past the
\\		characters  written  to  the  file  and FILE-SIZE returns a
\\		value  greater  than  or  equal  to  the  value returned by
\\		FILE-POSITION.
\\
: write-file
  >file ->fildes @ >r swap r> write nip
;

\\	read-line		( c-addr u1 fileid -- u2 flag ior )	FILE
\\
\\		Read  the  next  line  from  file  specified by fileid into
\\		memory  at  the  address c-addr. At most, u1 characters are
\\		read. The implementation-dependent line terminator, if any,
\\		may  be  read  into memory at the end  of the line, but its
\\		length  is  not included  in the count u2.  The line buffer
\\		provided by c-addr should be at least u1+2 characters long.
\\		If  the  operation succedded, flag is true and ior is zero.
\\		If a line terminator was received before u1 characters were
\\		read, then  u2  is  the number of characters, not including
\\		the line terminator, actually read (0 <= u2 <= u1). When u1
\\		= u2  the  line  terminator  has  yet to be reached. If the
\\		operation   is   initiated   when  the  value  returned  by
\\		FILE-POSITION  is  equal to the value returned by FILE-SIZE
\\		for  the  file  identified by fileid, flag is false, ior is
\\		zero, and u2 is zero. If ior is non-zero, an error occurred
\\		during  the operation and ior is the implementation-defined
\\		I/O  result code.  An  ambiguous  condition  exists  if the
\\		operation   is   initiated   when  the  value  returned  by
\\		FILE-POSITION   is  greater  than  the  value  returned  by
\\		FILE-SIZE  for  the  file  identified  by fileid, or if the
\\		requested  operation  attempts to read portions of the file
\\		not written.
\\		At the conclusion of the operation, FILE-POSITION returns a
\\		value past the characters consumed by the operation.
\\
: read-line
  >r 2dup 1+ r@ read-file dup		\ Read requested bytes
  if					\ read error
    nip r> drop exit
  then
  drop tuck
  0=if					\ eof
    r> 4 discard 0 dup dup exit
  then
  -rot swap over
  nl# scan nip				\ find end of line
  dup
  if					\ End of line found
    >r nip r@ - r> 1-
  else
    drop 2dup u<
    if					\ More read
      drop 1
    else
      nip 0
    then
  then
  dup
  if					\ seek back
    negate SEEK_CUR swap
    r@ >file ->fildes @ seek drop
  then
  r> 2drop true 0
;

compiler definitions

\\	eol-seq			( -- c-addr )				ALE
\\
\\		Sequence for end of line.  Used by write-line.
\\
rom create eol-seq  nl# c,

forth definitions

\\	write-line		( c-addr u1 fileid -- ior )		FILE
\\
\\		Write   u1   characters   from   c-addr   followed  by  the
\\		implementation-dependent   line   terminator  to  the  file
\\		identified by fileid starting at its current position.  ior
\\		is  the  implementation-defined  I/O  result  code.  At the
\\		conclusion  of the operation, FILE-POSITION returns a value
\\		past  the  characters  written  to  the  file and FILE-SIZE
\\		returns a value greater than or equal to the value returned
\\		by FILE-POSITION.
\\
: write-line
  dup >r write-file dup
  if
    r> drop exit
  then
  drop eol-seq 1 r> write-file
;

\\	save-input		( -- x1 x2 x3 x4 )			FILE EXT
\\
\\		x1,  x2,  x3  and  x4  describe the current position in the
\\		input   stream   or  text  input  file  for  later  use  by
\\		RESTORE-INPUT.
\\
\\				x1	x2	x3		x4
\\		Block:		>in @	blk @	0		0
\\		File-Block:	>in @	blk @	block-fid	0
\\		Evaluate:	>in @	0	0		-1
\\		Keyboard:	>in @	0	0		0
\\		Text-File:	>in @	lo_pos	hi_pos		source-file
\\
: save-input
  >in @
  [ /block ] [if]
    blk @ dup
    if					\ Block
      block-fid @ 0 exit
    then
    drop
  [then]
  source-file 0>
  if
    source-file file-position throw
    #tib @ 1+ 0 d-			\ To start of line
  else
    0 0
  then
  source-file
;

\\	restore-input		( x1 x2 x3 x4 -- flag )			FILE EXT
\\
\\		Attempt to restore the current input stream or current text
\\		input  file to the position described by x1, x2, x3 and x4.
\\		flag  is  true  if the input stream cannot be positioned to
\\		the place described by x1, x2, x3 and x4.
\\		See save-input for description of x1 .. x4.
\\
: restore-input
  dup to source-file 0>			\ file
  if
    source-file reposition-file throw
    tib #fib 2 - source-file read-line throw drop
    #tib !
  else
    [ /block ] [if]
      block-fid ! blk !
    [else]
      2drop
    [then]
  then
  >in ! true
;

\\	include-file		( i*x fileid -- j*x )			FILE
\\
\\		Save  the  specification of the input stream, including the
\\		current  value  of  SOURCE-FILE.  Set the value returned by
\\		SOURCE-FILE  to fileid. Store 0 in BLK.
\\		Repeat until end of file:
\\			Read  a  line  from the file, fill the input stream
\\			from  the  contents of that line, and interpret the
\\			input stream.
\\		Interpretation  begins  at the file position where the next
\\		file read would occur. When the end of the file is reached,
\\		close  the  file and restore the specification of the input
\\		stream  to  its  saved  value.  After  an error occurs, the
\\		status  (open  or  closed)  of  any  files  that were being
\\		interpreted   is   implementation-defined.   An   ambiguous
\\		condition  exists if  fileid is invalid, if there is an I/O
\\		error  reading  fileid, or if there is an I/O error closing
\\		fileid.
\\
: include-file
  >in @ #tib @ 2>r			\ Save current input stream
  tib source-file 2>r
  [ /block ] [if]
    blk @ >r

    blk off				\ Setup new input stream
  [then]
  dup to source-file
  >file ->file-buffer to tib

  begin
    tib #fib 2 - source-file read-line throw
  while
    #tib ! >in off
    source-file >file ->file-line 1+!	\ Increment line number
    ['] interpret catch dup
    if
      base @ decimal			\ Display error:file(line)
      [char] : emit source-file dup .file
      [char] ( emit >file ->file-line @ (.) type [char] ) emit
      base !

      [ /block ] [if]			\ Restore prior input stream
	r> blk !
      [then]
      2r> to source-file to tib
      2r> #tib ! >in !

      throw				\ throw to next frame
    then
    drop
  repeat
  drop
  source-file close-file throw		\ Close current input stream

  [ /block ] [if]			\ Restore prior input stream
    r> blk !
  [then]
  2r> to source-file to tib
  2r> #tib ! >in !
;

\\	included		( i*x c-addr u -- j*x )			FILE
\\
\\		Save  the  specification of the input stream, including the
\\		current  value  of SOURCE-FILE.  Open the file specified by
\\		c-addr  and  u  and  change the value of SOURCE-FILE to the
\\		file's fileid. Store zero in BLK.
\\		Repeat until end of file:
\\			Read  a  line  from the file, fill the input stream
\\			from  the  contents of that line, and interpret the
\\			input stream.
\\		Interpretation begins at the file position where the next
\\		file read would occur. When the end of the file is reached,
\\		close  the  file and restore the specification of the input
\\		stream to its saved value. An ambiguous condition exists if
\\		the  named  file  can not be opened, if an I/O error occurs
\\		reading  the  file, or if an I/O error occurs while closing
\\		the  file.  When  an ambiguous condition exists, the status
\\		(open  or  closed) of any files that were being interpreted
\\		is implementation-defined.
\\		During the interpretation of each line, the contents of >IN
\\		and  #TIB and the value returned by TIB are as described in
\\		4.0480 input stream with zero in BLK.
\\		File  input  (as  with  INCLUDE-FILE); block input (as with
\\		LOAD), and string input (as with EVALUATE) may be nested in
\\		any order.
\\
: included
  2dup r/o open-file
  if
    drop type true abort" :Can't open file"
  then
  nip nip include-file
;

\\	library-path		( -- a-addr )				FILE ALE
\\
\\		a-addr   is  the  address  of  LIBRARY-PATH.   LIBRARY-PATH
\\		contains the directory path for library files.
\\		Path in LIBRARY-PATH searched by LIBRARY.
\\	    Typical use:
\\		s" /usr/lib/forth" library-path 2!
\\	    FIXME:
\\		-or- should i use set-library-path, get-library-path.
\\	    FIXME:
\\		Want to set more directories.
\\
create library-path  2 cells allot

\\	.library-path		( -- )					FILE ALE
\\
\\		Display the current library path.
\\
: .library-path
  cr ." Library path: "
  library-path 2@ type cr
;

\\	library			( i*x c-addr u -- j*x )			FILE ALE
\\
\\		Like INCLUDED, but searchs in LIBRARY-PATH for the file and
\\		appends .4th to file if no . present.
\\
: library
  2dup r/o open-file			\ Try original file
  if
    drop
    2dup [char] . scan nip
    0=if				\ Contains no .
      s" .4th" concat			\ Append .4th
      2dup r/o open-file		\ Try file with postfix: .4th
    else
      true
    then
    if
      drop
      2dup [char] / scan nip
      0=if				\ Contains no /
	library-path 2@ 2swap concat	\ Prefix: library-path
      then
      2dup r/o open-file		\ Try
      if
	drop type
	true abort" :Can't open library file"
      then
    then
  then
  nip nip				\ Remove string
  include-file
;

compiler definitions

\\	fil-buf1		( -- c-addr )				FILE ALE
\\
\\		c-addr is a 256 byte buffer for forth -> unix filename
\\		conversion.
\\
create fil-buf1  256 allot

\\	fil-buf2		( -- c-addr )				FILE ALE
\\
\\		c-addr is a 256 byte buffer for forth -> unix filename
\\		conversion.
\\
create fil-buf2  256 allot

forth definitions

\\	delete-file		( c-addr u -- ior )			FILE
\\
\\		Delete  the file named in the character string specified by
\\		c-addr and u.  ior is the implementation-defined I/O result
\\		code.
\\
: delete-file
\  ">0 unlink
  fil-buf1 0"place
  fil-buf1 unlink
;

\\	rename-file		( c-addr1 u1 c-addr2 u2 -- ior )	FILE EXT
\\
\\		Rename  the  file  named  by  the  first  character  string
\\		specified  by  c-addr1  and  u1  to  the name in the second
\\		character  string.  ior  is  the implementation-defined I/O
\\		result code.
\\
: rename-file
\  2>r ">0 2r> ">0 rename
  fil-buf2 0"place
  fil-buf1 0"place
  fil-buf2 fil-buf1 rename
;

\\	file-status		( c-addr u -- x ior )			FILE EXT
\\
\\		Return  the  status of the file identified by the character
\\		string c-addr u. If the file exists, ior is zero; otherwise
\\		ior  is  the  implementation-defined  I/O  result  code.  x
\\		contains implementation-defined information about the file.
\\
: file-status
\  ">0 access
  fil-buf1 0"place
  0 fil-buf1 access
;

\\	close-all-files		( -- )					FILE ALE
\\
\\		Close all opened files, without writing any data.
\\
: close-all-files
  #files 1				\ Don't close default block file
  do					\ Find used slot
    i >file ->fildes @ dup 
    if  close-file  then		\ Close used file slot
    drop
  loop
;

\ -----------------------------------------------------------------------------
\	Block Word Set
\ -----------------------------------------------------------------------------

/block [if]				\ { Block word set not wanted

\\	scr			( -- a-addr )			BLOCK EXT
\\
\\		a-addr is the address of SCR. SCR contains the block number
\\		of the block most recently LISTed. (SCR stands for screen.)
\\
variable scr

compiler definitions

\\	#buffers		( -- n )			FILE+BLOCK ALE
\\
\\		n is the number of buffers available.
\\
4	constant #buffers

\\	b/buf			( -- n )			FILE+BLOCK ALE
\\
\\		n is the size of a block buffer.
\\
1024	constant b/buf

\\	buffers			( -- a-addr )			FILE+BLOCK ALE
\\
\\		a-addr is the address of the start of the block buffers.
\\
create buffers  #buffers b/buf * allot

\\	buffer-table
\\
\\		cell	fileid		File id for buffer
\\		cell	block#		Block nr of buffer
\\		cell	update		Update flag of buffer
\\		cell	buffer		Address of buffer
\\
0	field ->fileid
4	field ->block#
8	field ->update
12	field ->buffer
16	constant b/buffer
create buffer-table  #buffers b/buffer * allot
\ buffer-table #buffers b/buffer * erase

\\	>buffer		( n -- a-addr )
\\
\\		Goto header of buffer n.
\\
: >buffer
  b/buffer * buffer-table +
;

\\	read-block		( buffer -- )			BLOCK ALE
\\
\\		Low level word reads block into buffer.
\\
: read-block
  dup ->block# @ b/buf *		\ Seek-position
  over ->fileid @ >file ->fildes @	\ File-desc
  dup >r
  SEEK_SET -rot seek throw drop		\ Seek to block
  b/buf swap ->buffer @ r> read throw	\ Read block

  b/buf <> abort" Block out of range"
;

\\	write-block		( buffer -- )			BLOCK ALE
\\
\\		Low level word writes a block from buffer.
\\
: write-block
  dup ->block# @ b/buf *		\ Seek-position
  over ->fileid @ >file ->fildes @	\ File-desc
  dup >r
  SEEK_SET -rot seek throw drop		\ Seek to block
  b/buf swap ->buffer @ r> write throw	\ Write block

  b/buf <> abort" Block out of range"
;

\\	establish		( a-addr1 -- a-addr2 )		 BLOCK ALE
\\
\\		Make buffer a-addr1 the last accessed one.
\\		a-addr2 is the buffer new address.
\\
: establish
  dup >r 2@ r@ 8 + 2@			\ Save contents

  buffer-table
  dup dup b/buffer +
  r> pluck - cmove>			\ Move up

  dup >r 8 + 2! r@ 2!			\ Restore contents
  r>
;

\\	absent?		( u fileid -- a-addr flag )		BLOCK ALE
\\
\\		Look for block u from file fileid in the buffer table.
\\		Flag is true if the block isn't a buffer, false otherwise.
\\
: absent?
  #buffers 0
  do					\ Look if block is in buffer
    i >buffer
    dup >r ->fileid @ over =
    r@ ->block# @ 3 pick =
    and
    if					\ block in buffer
      2drop r> false unloop exit
    then
    r> drop
  loop
  #buffers 1- >buffer
  dup ->update @
  if					\ oldest block is modified
    dup write-block
    dup ->update off
  then
  dup >r ->fileid 2! r>
  true
;

forth definitions

\\	file-buffer		( u fileid -- a-addr )			FILE EXT
\\
\\		a-addr  is  the address of the first character of the block
\\		buffer assigned to block u of fileid.  If fileid is zero it
\\		refers  to  the implementation-defined default block space.
\\		The  contents  of  the block are unspecified.  An ambiguous
\\		condition  exists  if u is not an available block number of
\\		fileid.
\\		If  block  u of fileid is already in a block buffer, a-addr
\\		is the address of that block buffer.
\\		If  block u of fileid is not already in memory and there is
\\		an  unassigned  block buffer, a-addr is the address of that
\\		block buffer.
\\		If  block  u  of fileid is not already in memory, and there
\\		are no unassigned block buffers, unassign a block buffer.
\\		If  the block in that buffer has been UPDATEd, transfer the
\\		block to mass storage.  a-addr is the address of that block
\\		buffer.
\\		An ambiguous condition exists if fileid is invalid or if an
\\		I/O error occurred during the block transfer.
\\		At  the  conclusion  of  the  operation,  the  block buffer
\\		pointed  to  by  a-addr  is the current block buffer and is
\\		assigned to u of fileid.
\\
: file-buffer
  absent? drop establish ->buffer @
;

\\	file-block		( u fileid -- a-addr )			FILE EXT
\\
\\		a-addr  is  the address of the first character of the block
\\		buffer assigned to block u of fileid.  If fileid is zero it
\\		refers  to  the implementation-defined default block space.
\\		An  ambiguous  condition  exists  if  u is not an available
\\		block number of fileid.  If block u of fileid is already in
\\		a  block buffer a-addr is the address of that block buffer.
\\		If  block u of fileid is not already in memory and there is
\\		an unassigned block buffer, transfer block u of fileid from
\\		mass  storage to an unassigned block buffer.  a-addr is the
\\		address  of  that block buffer. If block u of fileid is not
\\		already  in  memory,  and  there  are  no  unassigned block
\\		buffers,  unassign  a  block  buffer.  If the block in that
\\		buffer has been UPDATEd, transfer the block to mass storage
\\		and  transfer block u of fileid from mass storage into that
\\		buffer. a-addr is the address of that buffer.  An ambiguous
\\		condition  exists  if  fileid is invalid or if an I/O error
\\		occurred  during  the  block  transfer.
\\		At  the  conclusion  of  the  operation,  the  block buffer
\\		pointed  to  by  a-addr  is the current block buffer and is
\\		assigned to u of fileid.
\\
: file-block
  absent?
  if
    dup read-block
  then
  establish ->buffer @
;

\\	load-file		( i*x u fileid -- j*x )			FILE EXT
\\
\\		Save   the  current  input  stream  specification  and  the
\\		contents  of  BLOCK-FID.  Change  BLOCK-FID to fileid, make
\\		block  u  of  fileid  (implementation-defined default block
\\		space  if  fileid  is  zero)  the  current input stream and
\\		interpret its contents. When the input stream is exhausted,
\\		restore  the  prior input stream specification, restore the
\\		contents of BLOCK-FID, and close the file.
\\		An  ambiguous  condition  exists  if  u is zero or is not a
\\		valid block number in the file. When an ambiguous condition
\\		exists,  the status (open or closed) of any files that were
\\		being interpreted is implementation-defined.
\\
: load-file
  block-fid @ blk @ 2>r >in @ >r	\ Save input stream

  block-fid ! blk ! >in off		\ Setup input
  ['] interpret catch throw		\ FIXME: Error not handled

  r> >in ! 2r> blk ! block-fid !	\ Restore input stream
;

\\	buffer			( u -- a-addr )				BLOCK
\\
\\		a-addr  is  the address of the first character of the block
\\		buffer  assigned to block u.  The contents of the block are
\\		unspecified.  An  ambiguous condition exists if u is not an
\\		available block number.
\\		If  block  u  is  already  in a block buffer: a-addr is the
\\		address of that block buffer.
\\		If  block  u  is  not  already  in  memory  and there is an
\\		unassigned  buffer:  a-addr  is  the  address of that block
\\		buffer.
\\		If  block  u  is not already in memory, and  there  are  no
\\		unassigned  block buffers: unassign a block buffer.  If the
\\		block  in  that buffer has been UPDATEd, transfer the block
\\		to  mass  storage.  a-addr  is  the  address  of that block
\\		buffer.
\\		At  the  conclusion  of  the  operation,  the  block buffer
\\		pointed  to  by  a-addr  is the current block buffer and is
\\		assigned to u.
\\		An  UPDATEd block tha came  from a file must be transferred
\\		back  to the same file when the block buffer is needed  for
\\		another block.
\\		If  BLOCK-FID  is  not  equal  to  zero, the block number u
\\		references  block u  of  the  file identified by the fileid
\\		stored in BLOCK-FID.
\\
: buffer
  block-fid @ file-buffer
;

\\	block			( u -- a-addr )				BLOCK
\\
\\		a-addr  is  the address of the first character of the block
\\		buffer assigned to mass storage block u.
\\
: block
  block-fid @ file-block
;

\\	load			( i*x u -- j*x )		BLOCK EXT
\\
\\		Save  the current input stream specification.  Make block u
\\		the  current input stream and interpret the contents.  When
\\		the  input  stream  is  exhausted,  restore the prior input
\\		stream specification.
\\		An  ambiguous  condition  exists  if u is zero, or is not a
\\		valid block number.
\\
: load
  block-fid @ load-file
;

\\	thru			( u1 u2 -- )			BLOCK EXT
\\
\\		Sequentially  LOAD  the  mass  storage  blocks  numbered u1
\\		through u2.
\\
: thru
  ( cr) 1+ swap
  ?do
    ( [char] . emit ) i load
  loop
;

\\	update			( -- )					BLOCK
\\
\\		Mark  the  current block buffer as modified.  An  ambiguous
\\		condition  exists  if  there  is  no  current block buffer.
\\		UPDATE does not immediately cause I/O.
\\
: update
  buffer-table ->update on
;

\\	l/scr			( -- n )				ALE
\\
\\		Constant n is the number of lines in a screen.
\\
16	constant l/scr

\\	c/l			( -- n )				ALE
\\
\\		Constant n is the number of columns in a screen.
\\
64	constant c/l

\\	list			( u -- )			BLOCK EXT
\\
\\		Display block u in an implementation-defined format.  Store
\\		u in SCR.  The  block is displayed as 16 lines of 64 chars.
\\
: list
  cr dup scr !
  ." Screen #" scr @ 5 .r 8 spaces block-fid @ .file
  l/scr 0
  do
    cr i 3 .r space
    dup block i c/l * + c/l -trailing type
    key? ?leave
  loop
  drop
  cr
;

\\	empty-buffers		( -- )				BLOCK EXT
\\
\\		Unassign all block buffers. Do not transfer the contents of
\\		any UPDATEd block buffer to mass storage.
\\		This includes file buffers.
\\
: empty-buffers
  #buffers 0
  do
    i >buffer 12 erase
  loop
;

\\	save-buffers		( -- )					BLOCK
\\
\\		Transfer  the contents of each UPDATEd block buffer to mass
\\		storage. Mark all buffers as unmodified.
\\
: save-buffers
  #buffers 0
  do
    i >buffer ->update @
    if
      i >buffer dup write-block
      ->update off
    then
  loop
;

\\	flush-file		( fileid -- ior )			FILE EXT
\\
\\		Attempt  to  force  any buffered information written to the
\\		file  referred  to by fileid to be written to mass storage,
\\		and the size information for the file to be recorded in the
\\		storage directory if changed.
\\		If the operation is successful, ior is zero.  Otherwise, it
\\		is an implementation-defined I/O result code.
\\
: flush-file
  postpone false			\ We currently use no buffering
\  true abort" FIXME:"
\  #buffers 0
\  do
\    i >buffer dup ->fileid pluck =
\    if
\      dup ->update @
\      if
\	dup write-block
\	dup ->update 0 swap !
\      then
\    then
\    drop
\  loop
; immediate

compiler definitions

\\	init-buffer		( c-addr u -- )			BLOCK ALE
\\
\\		Init buffers.  Open default block file c-addr and u.
\\		Load screen 1 of default block file.
\\
: init-buffer	( -- )
  buffers
  #buffers 0
  do
    dup i >buffer ->buffer !
    b/buf +
  loop
  drop
  2dup r/w open-file nip
  if
    ." Can't open default block file: " type cr
    0 >file ->fildes on			\ Don't use slot 0 for files
    exit
  then
  2drop 1 load				\ Load block 1 of default block file
;

forth definitions

[then]					\ } /BLOCK

\\	refill			( -- flag )				CORE EXT
\\
\\		Attempt  to fill the current input stream, returning a true
\\		flag if successful. The action depends on the source of the
\\		current input stream.
\\		If  the  input-stream  source  is  a  string from EVALUATE,
\\		REFILL returns false and performs no other action.
\\		Otherwise,  REFILL  attempts  to  receive  input  into  the
\\		text-input buffer whose address is given by TIB, making the
\\		result  the  current  input  stream  and  returning  a true
\\		flag  if  successful.  Receipt  of  a  line  containing  no
\\		characters  is  considered  successful.  A  false  flag  is
\\		returned  only  when  there  is no input available from the
\\		current input-stream source.
\\		If  the  input-stream  source  is a block, REFILL makes the
\\		next  block  the  current input stream by adding one to the
\\		value  of BLK and setting >IN to zero.  True is returned if
\\		the  new  value  of  BLK  is  a  valid  block number, false
\\		otherwise.
\\		If  the input-stream source is a text file, REFILL attempts
\\		to  read the next line from the text-input file, making the
\\		result  the  current input stream and returning true if the
\\		read succeeded, and returning false otherwise.
\\
: refill
  >in off
  [ /block ] [if]
    blk @
    if					\ Input=Block
      blk 1+!				\ advance block
      blk @ 1+ b/buf um* ( 1.) 1 0 d-
      block-fid @ file-size throw du<	\ test if block available
      exit
    then
  [then]
  source-file dup
  if
    1+					\ -1 evaluate +1 = 0
    if					\ Input=File
      tib #fib 2 - source-file
      read-line throw
      if				\ no eof
	#tib !
	source-file >file ->file-line 1+!
	true exit
      then
      drop
    then
    false exit				\ Input=String
  then
  drop query cr true			\ Input=Keyboard
;
