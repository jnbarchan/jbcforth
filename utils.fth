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
: 1ARRAY-COUNT	( addr ... count )
    4 - @
  ;
: 1ARRAY-SIZE	( addr ... size )
    2- @
  ;
: 1ARRAY-BINARY-SEARCH	( array\cmp\val ... &array[n]/0 )
    (LOCAL) val (LOCAL) cmp (LOCAL) array	( array\cmp\val ... )
    0 0 array @EXECUTE 1ARRAY-COUNT 1-	( 0\count )
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

