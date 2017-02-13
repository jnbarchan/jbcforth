\ tests.fth

\ FORTH tests

FORTH DEFINITIONS
VOCABULARY tests IMMEDIATE tests DEFINITIONS

\ Start of a Test
R: T{
    ' T{			( push marker for T{ )
  R;

\ End of a Test
: }T
    ' T{ ?PAIRS			( verify marker for T{ )
  ;

\
\ Reset tests Vocabulary to this point
\
FORTH tests DEFINITIONS
: RESET-TESTS
    [COMPILE] TRACEOFF
    INTERPRET" ASSIGN TRACE-EXECUTE TO-DO (TRACE-EXECUTE)"
    INTERPRET" FORGET RESET-TESTS-POINT"
    INTERPRET" : RESET-TESTS-POINT ;"
  ;
: RESET-TESTS-POINT ;

\
\ Test Coverage stuff
\
: INIT-TC-ARRAY		( ... )
    \ The array for counting how many times each word is executed
    \ each element is 2 singles: first single is address of Dictionary Entry [xt], second single is how many times executed
    \ array must currently have 0 elements and be at HERE on entry
    \ this initialises the array, allotting in the Dictionary and incrementing array's element count
    HERE 4 - >R		( save address of array's count )
    CONTEXT @ >R	( save current CONTEXT Vocabulary )
    [COMPILE] FORTH	( start in FORTH Vocabulary )
    CONTEXT @ @		( top word in Dictionary )
    R> CONTEXT !	( restore CONTEXT Vocabulary )
    BEGIN
      ?DUP		( while not end of Dictionary )
    WHILE
      1 R> DUP >R +!	( increment array count )
      DUP , 0 ,		( Dictionary Entry's address and value 0 )
      (FINDPREV)	( move to previous Dictionary Entry [does *not* skip a SMUDGEd entry] )
    REPEAT
    R> DROP		( drop saved address of array's count )
  ;
0 4 1ARRAY TC-ARRAY
INIT-TC-ARRAY

: TC-SHOW		( f ... )
    \ Show the words in TC-ARRAY
    \ f=0 => show words where the count = 0
    \ f=1 => show words where count <> 0
    \ f=-1 => show words and their count where count <> 0
    (LOCAL) flag			( )
    0 TC-ARRAY 1ARRAY-COUNT 0 DO
      I TC-ARRAY		( array[I] )
      DUP @ SWAP 2+ @			( xt\count )
      flag @ 0= IF
        0= IF ID. ELSE DROP THEN
      THEN
      flag @ 0> IF
        IF ID. ELSE DROP THEN
      THEN
      flag @ 0< IF
        ?DUP IF SWAP ID. ." : " 1 .R ." , " ELSE DROP THEN
      THEN
    LOOP
  ;

: TC-ARRAY-FIND-SEQUENTIAL	( xt ... &array[n]/0 )
    \ Find a word [xt] in TC-ARRAY
    \ Return the address of the element
    \ 0 => not found
    0 (LOCAL) result
    0 TC-ARRAY 1ARRAY-COUNT 0 DO
      I TC-ARRAY		( xt\&array[I] )
      2DUP @ = IF DUP TO result LEAVE THEN
      DROP				( xt )
    LOOP
    DROP result @			( &array[n]/0 )
  ;

: TC-ARRAY-CMP		( xt\array[n] ... <0/0/>0 )
    \ Compare element in TC-ARRAY against xt sought
    \ Called from 1ARRAY-BINARY-SEARCH
    \ Return <0/0/>0 according as xt is </=/> element
    @				( xt\array[n].xt )
    -				( <0/0/>0 )
    NEGATE			( array elements are in *descending* address order )
  ;

: TC-ARRAY-FIND-BINARY	( xt ... &array[n]/0 )
    \ Find a word [xt] in TC-ARRAY
    \ Return the address of the element
    \ 0 => not found
    ' TC-ARRAY NFA ' TC-ARRAY-CMP NFA ROT	( array\cmp\val )
    1ARRAY-BINARY-SEARCH			( &array[n]/0 )
  ;

: TC-EXECUTE		( xt\f ... )
    \ The word called during execution once ASSIGN TRACE-EXECUTE TO-DO TC-EXECUTE has been executed )
    \ xt is the word being executed
    \ This is called twice:
    \ f=0 => pre-execution of word
    \ f=1 => post-execution of word
    IF DROP EXIT THEN			( do nothing if flag is "post-execute" )
    \ TC-ARRAY-FIND-SEQUENTIAL		( find the execution token in TC-ARRAY )
    TC-ARRAY-FIND-BINARY		( find the execution token in TC-ARRAY )
    ?DUP IF 2+ 1+! THEN			( increment number of times called )
  ;

ASSIGN TRACE-EXECUTE TO-DO TC-EXECUTE
TRACEON
TRACE-ALL

\
\ Test T{ & }T
\
T{ }T
T{ ( }T ) }T
T{ \ }T
}T

\
\ Test Vocabulary stuff
\
FORTH tests DEFINITIONS
VOCABULARY voc IMMEDIATE
FORTH tests voc DEFINITIONS
: voc1 ;
FORTH DEFINITIONS
: forth1 ;
FORTH tests DEFINITIONS
: tests1 ;
FORTH tests voc DEFINITIONS
: voc2 ;
FORTH DEFINITIONS
: forth2 ;
FORTH tests voc DEFINITIONS

T{ FORTH ' forth1 NFA FIND-VOCABULARY ( -> ) ' FORTH NFA = ASSERT tests }T
T{ FORTH ' forth2 NFA FIND-VOCABULARY ( -> ) ' FORTH NFA = ASSERT tests }T
T{ FORTH ' tests NFA FIND-VOCABULARY ( -> ) ' FORTH NFA = ASSERT tests }T
T{ tests ' tests1 NFA FIND-VOCABULARY ( -> ) ' tests NFA = ASSERT tests }T
T{ tests ' voc NFA FIND-VOCABULARY ( -> ) ' tests NFA = ASSERT tests }T
T{ voc ' voc1 NFA FIND-VOCABULARY ( -> ) ' voc NFA = ASSERT tests }T
T{ voc ' voc2 NFA FIND-VOCABULARY ( -> ) ' voc NFA = ASSERT tests }T
T{ FORTH FIND voc ( -> ) 0= ASSERT tests }T
T{ FORTH FIND tests1 ( -> ) 0= ASSERT tests }T
T{ tests FIND voc1 ( -> ) 0= ASSERT tests }T
T{ voc FIND tests1 ( -> ) 0= ASSERT tests }T

T{ tests FORGET voc }T
T{ tests FIND voc ( -> ) 0= ASSERT tests }T
T{ tests FIND tests1 ( -> ) 0= ASSERT tests }T
T{ FORTH FIND voc ( -> ) 0= ASSERT tests }T
T{ FORTH FIND forth1 ( -> ) 0= ASSERT tests }T

FORTH tests

\
\ Test :NONAME
\
VARIABLE nn1
VARIABLE nn2
T{ :NONAME 1234 ; nn1 ! ( -> ) }T
T{ :NONAME 9876 ; nn2 ! ( -> ) }T
T{ nn1 @ EXECUTE ( -> ) 1234 = ASSERT }T
T{ nn2 @EXECUTE ( -> ) 9876 = ASSERT }T

\
\ Test arithmetic stuff
\
T{ 0 -2 -1 * - 1 * 2 * ( -> ) -4 = ASSERT }T
T{ 0 1- 2- 2* 1+ 2+ BL + ( -> ) 29 = ASSERT }T
T{ PI ( -> ) F->SF SF->F 3.14159265359 F->SF SF->F F= ASSERT }T



