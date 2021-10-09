\
\	ALE Forth Version 2.04
\
\		Copyright (c) 1991,1992 by Lutz Sammer.
\
\		Bug-reports via usenet: johns
\
\	File:	memory.4th
\
\		Memory allocation word set.
\

\ ==============================================================================
\ 	Vocabulary:	Memory
\ ============================================================================*/

compiler definitions

\\	sbrk			( n -- addr )				LIBRARY
\\
\\		Change data segment space allocation.
\\		If n is positive increment the dictionary space by n bytes.
\\		If n is negative decrement the dictionary space by n bytes.
\\		addr is the old end of the space allocation.
\\
: sbrk
  brk-val tuck				\ Old break value
  + brk throw
;

create alloc-static  2 cells allot	\ Initial static arenea
value alloc-search			\ Search pointer
value alloc-top				\ Arena top
value alloc-save			\ For resize
value alloc-end				\ Last block if it is free or alloc-top

false [if]

value single-step  0 to single-step

: (debug")
  ." : " 32 out @ - spaces .s cr
  single-step if key drop then
;

: debug"	( "ccc<">" -- )
  postpone ." postpone (debug")
; immediate

[then]

: test-busy	( c-addr -- flag )
  \ 1 postpone literal postpone and
  1 and
;

: set-busy	( c-addr -- c-addr )
  \ 1 postpone literal postpone or
  1 or
;

: clear-busy	( c-addr -- c-addr )
  \ -2 postpone literal postpone and
  -2 and
;

\ Initialize:
alloc-static cell+
  dup to alloc-top dup to alloc-end
  set-busy alloc-static !
alloc-static dup set-busy alloc-static cell+ !
  to alloc-search

: find-free	( size -- )
  alloc-search				\ Search start
  begin
    \ debug" Test"
    ( block )
    dup @ dup test-busy
    0=if				\ Block wasn't used
      \ debug" Found free block"
      ( block next )
      begin				\ Join following free blocks to one
	( block next ) dup @ dup test-busy 0=
      while
	( block next next-next)
	\ debug" Join block"
	swap alloc-end =
	if
	  \ debug" End changed"
	  over to alloc-end
	then
	( block next-next )
	dup pluck !			\ Join blocks
	\ over to alloc-search		\ Start search 
	( block next )
	\ debug" Joined"
      repeat
      drop
      over to alloc-search		\ Start search 
      \ debug" After join"
      ( size block next )
      dup pluck - 3 pick
      ( size block next next-block size )
      \ debug" Big enough"
      <
      0=if
	\ debug" Found big enough block"
	exit
      then
    then
    ( block next )
    \ clear-busy swap over >
    nip clear-busy dup alloc-search =
  until
  drop					\ No free block found
  ( size -- )
  \ debug" No free block found"
  0 sbrk				\ Get current break value
  dup alloc-top cell+ <>
  if					\ The user has changed the break value
    ( size current-brk )
    \ debug" Break value changed"
    over 4095 + -4096 and		\ Align to block boundary
    \ debug" sbrk value now"
    over [ cell 1- ] literal and dup
    if					\ break value not on cell boundary
      \ debug" Break not on cell boundary"
      ( size brk sbrk x )
      negate cell+ dup			\ Align break value to cell boundary
      >r +
      ( size brk sbrk+x )
      swap r@ + swap r>
      ( size brk+x sbrk+x x )
    then
    ( size brk sbrk x )
    \ debug" Move break value: "
  else					\ The break value hasn't be changed
    ( size brk )
    \ debug" Break value keeps same"
    over alloc-top alloc-end - - 0	\ Bytes break value must be moved
    ( size brk sbrk 0 )
    \ debug" Keep break value"
  then
  \ debug" Get current break value"
  ( size brk sbrk incr )
  over sbrk 				\ Move break value
  0=if					\ Error ??
    2drop brk drop nil exit		\ Move break value back
  then
  \ debug" After sbrk"
  cell+ - over +			\ Calculate end of block
					\ Reserve one cell for sbrk changed
  ( size brk next )
  \ debug" block-end"
  dup pluck !				\ Set new block end
  ( size brk next )
  over dup to alloc-end			\ New allocated end
  dup alloc-top cell+ <>
  if					\ New block not continous
    \ debug" Not continous"
    set-busy				\ Mark last block as busy
  then
  \ debug" Link to new link"
  alloc-top !				\ Add link to new block
  dup to alloc-top			\ New arena top
  ( size brk next )
  [ alloc-static set-busy ] literal over !
  \ debug" End find-free"
  2drop recurse
;

\ 1)
\	alloc-search:	[alloc-static]	Search first
\	alloc-end:	[alloc-static+cell]
\	alloc-top:	[alloc-static+cell]
\
\	alloc-static:	[alloc-static+cell]
\			[alloc-static]
\ 2)
\	alloc-search:	<alloc-static>	Search first
\	alloc-end:	[block1-1]
\	alloc-top:	[block1-2]
\
\	alloc-static:	<alloc-static+cell>
\			[block1-1]
\
\	block1-1:	[block1-2]
\			....
\	block1-2:	[alloc-static]



forth definitions

\\	memory			( -- )				V	MEMORY
\\
\\		Append the memory allocation word set to the current search
\\		set.
\\
\\	wordlist:
\\
\\		allocate	free		resize		available
\\
vocabulary memory  memory definitions also compiler

\\	allocate		( u -- a-addr ior )			MEMORY
\\
\\		Allocate  u  address  units  of contiguous data space.  The
\\		address  of  the  next  available  data  space  location is
\\		unaffected  by  this operation.  The initial content of the
\\		allocated  space is undefined.  If the allocation succeeds,
\\		a-addr  is  the  aligned  starting address of the allocated
\\		space and ior is 0.
\\		If  the  operation fails, a-addr does not represent a valid
\\		address  and  ior  is the implementation-defined I/O result
\\		code.
\\
: allocate
  [ 2 cells 1- ] literal + 		\ 1 extra cell and cell aligned
  [ cell negate ] literal and		\ Number of bytes to allocate
  \ debug" allocate"
  find-free
  ( size block next )
  rot 
  ( block next size )
  pluck +
  dup to alloc-search				\ Start of search 
  ( block next new-next )
  2dup >
  if
    \ debug" next block above"
    dup @ to alloc-save			\ Save destroyed cell 
    pluck @
    \ debug" next->"
    over !				\ Free block -> Next block
  then
  ( block next new-next )
  nip
  ( block next )
  set-busy over !			\ Block -> Free block
  ( block )
  dup alloc-end =
  if
     alloc-search to alloc-end		\ Move last block
  then
  ( block )
  cell+ 0
  \ debug" end-allocate"
;

\\	free			( a-addr -- ior )			MEMORY
\\
\\		Return  the  contiguous  region  of data space indicated by
\\		a-addr  to  the  system  for later allocation.  a-addr must
\\		indicate a region of data pace that was previously obtained
\\		by  ALLOCATE  or RESIZE.  The address of the next available
\\		data space location is unaffected by this operation. If the
\\		operation  succeeds, ior is 0.  If the operation fails, ior
\\		is the implementation-defined I/O result code.
\\
: free
  \ debug" free"
  cell - dup to alloc-search		\ Start search with just freed block
  dup @ clear-busy dup pluck !		\ Mark block unused
  alloc-top =
  if					\ Just freed block last block
    to alloc-end
  else
    drop
  then
  0
;

\\	resize			( a-addr1 u -- a-addr2 ior )		MEMORY
\\
\\		Change the allocation of the contiguous data space starting
\\		at the address a-addr1 allocated by ALLOCATE or RESIZE to u
\\		address  units.  u may be either larger or smaller than the
\\		current  size  of  the  region.  The  address   of the next
\\		available   data  space  location  is  unaffected  by  this
\\		operation.  If  the  operation  succeeds,  a-addr2  is  the
\\		aligned  starting  address  of u address units of allocated
\\		memory and ior is zero.  a-addr2 may or may not be the same
\\		as a-addr1.  If they are not the same, the values contained
\\		in  the region at a- addr1 are copied to a-addr2, up to the
\\		minimum size of either of the two regions.  If they are the
\\		same,  the  values contained in the region are preserved to
\\		the  minimum  of u or the original size.  If a-addr2 is not
\\		the  same  as  a-addr1,  the region of memory at a-addr1 is
\\		returned  to the system according to the operation of FREE.
\\		If  the operation fails, a-addr2 does not represent a valid
\\		address  and  ior  is the implementation-defined I/O result
\\		code.
\\
: resize
  over cell - @ dup test-busy
  if					\ Free block if allocated
    pluck 
    \ debug" Freeing"
    free abort" resize(free)"
  then
  clear-busy pluck - 
  \ debug" old size"
  ( addr new old )
  over allocate abort" resize(allocate)"			\ Allocate new memory 
  ( o-addr new old n-addr )
  \ debug" allocated"
  dup 4 pick =				\ Same memory address
  if
    >r 3 discard r> 0 exit
  then
  ( o-addr new old n-addr )
  >r pluck r@ 2over min
  \ debug" Moveing"
  ( o-addr new old n-addr o-addr n-addr min )
  cmove					\ Copy old area
					\ Repair destroyed area
  \ debug" Repair"
  pluck r@ >				\ n-addr < o-addr
  if
    pluck pluck r@ + <			\ n-addr+new >= o-addr
    0=if
      \ debug" Destroyed"
      ( o-addr new old )
      ( naddr+naddr+new-oaddr )
      pluck negate over + r@ dup + +
      alloc-save 
      \ debug" Address"
      \ alloc-save swap !
    then
  then
  3 discard r> 0
  \ debug" resize-end"
;

\\	available		( -- u )			MEMORY EXT
\\
\\		Return the number of address units contained in the largest
\\		contiguous  region  of  data space that may be allocated by
\\		ALLOCATE or RESIZE.
\\
: available
  \ Largest block in list
  \ or sbrk to ulimit
  0
;

\\	.alloc			( -- )				MEMORY ALE
\\
\\		Display the internal memory allocation structures.
\\
: .alloc
  alloc-static
  begin
    cr ?keypause
    dup @ test-busy
    if  ." Busy"  else  ." Free"  then
    ."  Block:" dup u.
    ." Size:" dup @ clear-busy over - .
    dup alloc-search =
    if
      ." <-- Search first"
    then
    dup alloc-top =
    if
      ." <-- Arena top"
    then
    dup alloc-end =
    if
      ." <-- ?Last free"
    then
    @ clear-busy dup alloc-static =
  until
  drop
  cr
;

false [if]				\ Tests of the memory allocation

value x1 value x2 value x3 value x4

hex
.alloc
4 allocate throw ." allocate:" dup . cr to x1
.alloc
4 allocate throw ." allocate:" dup . cr to x2
.alloc
FE8 allocate throw ." allocate:" dup . cr to x3
.alloc x3 free throw
x2 free throw
x1 free throw
.alloc
4 allocate throw ." allocate:" dup . cr to x1
.alloc
1000 allocate throw ." allocate:" dup . cr to x2
.alloc
1000 allocate throw ." allocate:" dup . cr to x3
.alloc
x2 free throw x1 free throw
\ true to single-step
x3 1000 char X fill
414f40 @ .
x3 1000 resize throw to x3
.alloc
x3 free throw

100 constant TEST-LOOPS

: Test
  ." Allocate " TEST-LOOPS . ." Hunks" cr
  nil
  TEST-LOOPS 0
  do
    cell allocate throw
    swap over
    ( n-addr last n-addr )
    !
  loop
  \ .alloc
  .s cr
  0 over
  begin
    dup
  while
    swap 1+ swap @
  repeat
  drop
  .s cr
  TEST-LOOPS <> abort" Wrong number"
  ." Resizeing hunks" cr
  nil swap
  begin
    dup
  while
    ( prev old )
    2 cells resize throw
    dup cell+ on
    ( prev new )
    dup dup @ 2>r
    ! 2r>
  repeat
  drop
  \ .alloc
  .s cr
  0 over
  begin
    dup
  while
    dup cell+ @ -1 <> abort" Destroyed"
    swap 1+ swap @
  repeat
  drop
  .s cr
  TEST-LOOPS <> abort" Wrong number"
  begin
    dup
  while
    dup @ >r free throw r>
  repeat
  drop
  .s cr
;

Test

[then]

only forth also definitions
