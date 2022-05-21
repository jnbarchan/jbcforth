\ utils.fth

\ Utilities

FORTH DEFINITIONS

: CLS		( ... )
    \ clear the screen
    OS' tput clear'
  ;

: MACRO		( addr ... )
    \ Use as: " ..." MACRO CCCC
    <BUILDS
      $,
      IMMEDIATE
    DOES>
      $INTERPRET
  ;

: :INLINE	( ... )
    \ Use as: :INLINE ... ;INLINE
    :NONAME
  ;

: ;INLINE	( xt ... )
    \ Use as: :INLINE ... ;INLINE
    [COMPILE] ;
    DUP >R EXECUTE R> (FORGET)
  ; IMMEDIATE

: MEMSWAP	( addr1\addr2\count ... )
    \ swap count bytes addr2<->addr2
    >R
    DUP HERE R@ CMOVE
    2DUP R@ CMOVE
    DROP HERE SWAP R> CMOVE
  ;

: MICRO-TIME-DIFF	( mtsecsd1\mtusecsd1\mtsecsd2\mtusecsd2 ... usecsd )
    \ calculate difference (as DOUBLE micro-seconds) between 2 MICRO-TIMEs
    \ note that for convenience this does mt2 - mt1, i.e. expecting mt2 > mt1
    D>R 2SWAP D>R		( mtsecsd1\mtsecsd2 )
    2SWAP D- 1000000. D*	( [mtsecsd2 - mtsecsd1] * 1000000. )
    R>D D- R>D D+		( ... usecsd )
  ;

\
\ 1ARRAY stuff
\
: 1ARRAY	( count\size ... )
    \ Use as: count size 1ARRAY CCCC
    \ Create a 1-dimensional Array CCCC of count elements of size size
    \ Layout of array is [count, size, content...]
    \ Access to array is index CCCC
    <BUILDS
      OVER 0< 5 ?ERROR			( count < 0 )
      DUP 1- 0< 5 ?ERROR		( size <= 0 )
      2DUP *				( count * size )
      DUP 0x1000 U> 5 ?ERROR		( count * size > 0x1000 )
      ROT , SWAP ,			( store [count, size] )
      ALLOT				( allocate for content )
    DOES>	( index\addr ... addr )
      SWAP				( addr\index )
      DUP 0< 5 ?ERROR			( index < 0 )
      OVER @ OVER 1+ SWAP > 5 ?ERROR	( index+1 > count )
      OVER 2+ @				( addr\index\size )
      * + 4 +				( addr + 4 + index * size )
  ;
: 1ARRAY-COUNT	( &array[0] ... count )
    4 - @
  ;
: 1ARRAY-SIZE	( &array[0] ... size )
    2- @
  ;
: 1ARRAY-ERASE	( &array[0] ... )
    DUP 1ARRAY-COUNT OVER 1ARRAY-SIZE * ERASE
  ;
: 1ARRAY-IN-RANGE	( index\&array[0] ... f )
    OVER 0< NOT >R
    1ARRAY-COUNT < R> AND
  ;

: 1ARRAY-BUBBLE-SORT		( array\cmp ... )
    \ See https://en.wikipedia.org/wiki/Bubble_sort
    (LOCAL) cmp (LOCAL) array
    \ MICRO-TIME ." 1ARRAY-BUBBLE-SORT: "
    0 array @EXECUTE DUP 1ARRAY-COUNT (LOCAL) _count 1ARRAY-SIZE (LOCAL) size
    0 (LOCAL) newcount
    BEGIN
      0 newcount !			( newcount = 0 )
      _count @ 1 DO			( for I = 1 to _count - 1 )
        I array @EXECUTE DUP size @ -	( &array[I]\&array[I - 1] )
        2DUP cmp @EXECUTE 0< IF		( array[I] < array[I - 1] )
          size @ MEMSWAP		( array[I] <-> array[I - 1] )
          I newcount !			( newcount = I )
        ELSE
          2DROP
        THEN
      LOOP
      newcount @ DUP _count !		( count = newcount )
    2 < UNTIL				( count <= 1 )
    \ MICRO-TIME MICRO-TIME-DIFF 1000. D/ D. ." milliseconds" CR
;

: 1ARRAY-SELECTION-SORT		( array\cmp ... )
    \ See https://en.wikipedia.org/wiki/Selection_sort
    (LOCAL) cmp (LOCAL) array
    \ MICRO-TIME ." 1ARRAY-SELECTION-SORT: "
    0 array @EXECUTE DUP 1ARRAY-COUNT (LOCAL) _count 1ARRAY-SIZE (LOCAL) size
    0 (LOCAL) minI
    _count @ 1- 0 DO			( for outer = 0 to _count - 2 )
      I minI !				( minI = outer )
      _count @ I 1+ DO			( for inner = outer + 1 to _count - 1 )
        I array @EXECUTE minI @ array @EXECUTE	( &array[inner]\&array[minI] )
        cmp @EXECUTE 0< IF		( array[inner] < array[minI] )
          I minI !			( minI = inner )
        THEN
      LOOP
      minI @ I = NOT IF			( minI != outer )
        I array @EXECUTE minI @ array @EXECUTE	( &array[outer]\&array[minI] )
        size @ MEMSWAP			( array[outer] <-> array[minI] )
      THEN
    LOOP
    \ MICRO-TIME MICRO-TIME-DIFF 1000. D/ D. ." milliseconds" CR
;

: 1ARRAY-BINARY-SEARCH	( array\cmp\val ... &array[n]/0 )
    (LOCAL) val (LOCAL) cmp (LOCAL) array	( array\cmp\val ... )
    0 0 array @EXECUTE 1ARRAY-COUNT 1-	( 0\count-1 )
    BEGIN
      2DUP 1+ <				( low\high\f )
    WHILE				( low < high )
      2DUP + 2/				( low\high\mid )
      DUP array @EXECUTE val @ SWAP cmp @EXECUTE	( val\&array[mid] ... <0/0/>0 )
                                        ( low\high\mid\f )
      DUP 0> IF				( val > array[mid] )
        DROP ROT DROP 1+ SWAP		( mid+1\high )
      ELSE
        0< IF				( val < array[mid] )
          SWAP DROP 1-			( low\mid-1 )
        ELSE				( val = array[mid] )
          SWAP DROP SWAP DROP array @EXECUTE EXIT	( &array[mid] )
        THEN
      THEN
    REPEAT				( low\high )
    2DROP 0				( 0 )
;

\
\ VLIST with output sorted alphabetically
\
: IS-BLANK-DICT-ID	( addr ... f )
    \ is the dictionary id/name at addr a "dummy, blank" one (first word in a Vocabulary)
    COUNT 0x1F AND 1 = SWAP C@ BL = AND
  ;
: VLIST-SORTED	( ... )
    " " (LOCAL) lastdone 0 (LOCAL) nextfound 0 (LOCAL) found
    BEGIN
      " ~~~" nextfound ! 0 found !
      LAST
      BEGIN
      ?DUP WHILE
        DUP IS-BLANK-DICT-ID NOT IF
          DUP lastdone @ $CMPID 0>
          OVER nextfound @ $CMPID 0<
          AND IF DUP nextfound ! 1 found ! THEN
        THEN
        (FINDPREV)
      REPEAT
      found @ IF nextfound @ DUP lastdone ! ID. THEN
    found @ WHILE
    REPEAT
    CR
  ;
\
\ VLIST to show IMMEDIATE words
\
: VLIST-IMMEDIATES	( ... )
    LAST
    BEGIN
    ?DUP WHILE
      DUP IS-BLANK-DICT-ID NOT IF
        DUP COUNT SWAP DROP 0x40 AND IF DUP ID. THEN
      THEN
      (FINDPREV)
    REPEAT
    CR
  ;

\
\ Tests
\
: RUN-TESTS	( ... )
    \ Load and run the tests
    [COMPILE] TRACEOFF
    INTERPRET" ASSIGN TRACE-EXECUTE TO-DO (TRACE-EXECUTE)"
    [COMPILE] FORTH
    " tests" CURRENT @ @ (FIND)
    IF DROP DUP -FORGET ." Reloading"
    ELSE ." Loading"
    THEN ."  tests.fth" CR
    LOAD" tests.fth"
  ;

