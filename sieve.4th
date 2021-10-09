
.( Loading sieve ... ) cr

unused 20000 < [if]
    .( Dictionary expanded) cr
    compiler
    #dp 65536 + dup to #dp
    dp0 + brk throw				\ Expand the kernel
    forth
[then]

decimal
\ 8192 2* constant limit
16384 2* constant limit
limit 1- 2/ constant size
create flags  size allot

: prime	( -- )
  flags size 1 fill
  8 0						( c p )
  limit 9
  do						( R: s n )
     dup flags + c@
     if
       dup 2* 3 +				( c p p*2+3 )
       [ size flags + ] literal
       pluck dup 2* swap 3 + *
       [ 3 flags + ] literal +			( c p p+p+3 p*2*{p+3}+3+flags)
       do
	 0 i c! dup
       +loop
       drop
     then
     1+
     swap 8 + swap				( c p -- c+8 p )
     over
  +loop
  2drop

  0
  [ size flags + ] literal
  flags
  do
     i c@ +
  loop
  [ 16384 2* limit = ] [if]
    3511 <> abort" Wrong number of prims"
  [else]
    1899 <> abort" Wrong number of prims"
  [then]
;

: sieve	( -- )
  1000 0 do prime loop
;

.( Starting sieve ) cr
get-date 2drop drop 60 * + 60 * +

sieve

get-date 2drop drop 60 * + 60 * + swap -
.( Needed ) .  .( Seconds ) cr
