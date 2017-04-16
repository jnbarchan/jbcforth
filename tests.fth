\ tests.fth

\ FORTH tests

FORTH DEFINITIONS
VOCABULARY tests IMMEDIATE tests DEFINITIONS

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

\ Start of a Test
R: T{
    ' T{			( push marker for T{ )
    INTERPRET" ASSIGN TRACE-EXECUTE TO-DO TC-EXECUTE"
    [COMPILE] TRACEON
    [COMPILE] TRACE-ALL
  R;

\ End of a Test
: }T
    [COMPILE] TRACEOFF
    ' T{ ?PAIRS			( verify marker for T{ )
  ;

\ Effective value to use for DEPTH during T{ to exclude the T{ effect
: T{DEPTH DEPTH 1- ;

\
\ Start Tests
\
SP! RP!
FORTH tests DEFINITIONS

\
\ Test T{ & }T
\
T{ }T
T{ ( }T ) }T
T{ \ }T
}T

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
\ Test :INLINE
\
T{ DP @ :INLINE DUP DP @ < ASSERT ;INLINE ( -> ) DP @ = ASSERT }T

\
\ Test literals stuff
\
T{ -4 ( -> ) -4 = ASSERT }T
T{ -4. ( -> ) -4. D= ASSERT }T
T{ -4.321 ( -> ) -4.321 F= ASSERT }T
T{ 0x1234 ( -> ) 0x1234 = ASSERT }T
T{ :INLINE [ -4 ] LITERAL ;INLINE ( -> ) -4 = ASSERT }T
T{ :INLINE [ -4. ] DLITERAL ;INLINE ( -> ) -4. D= ASSERT }T
T{ :INLINE [ -4.321 ] FLITERAL ;INLINE ( -> ) -4.321 F= ASSERT }T
T{ :INLINE [ 65 ] CLITERAL ;INLINE ( -> ) 65 = ASSERT }T
T{ :INLINE [ 0x1234 ] ALITERAL ;INLINE ( -> ) 0x1234 = ASSERT }T

\
\ Test stack stuff
\
T{ ( -> ) T{DEPTH 0= ASSERT DEPTH 1 = ASSERT }T
T{ >R SP! ( -> ) SP@ S0 @ = ASSERT R> }T
T{ 1 2 3 ( -> ) T{DEPTH 3 = ASSERT 3 = ASSERT 2 = ASSERT 1 = ASSERT }T
T{ 1 2 3 DUP ROT SWAP OVER 5 PICK DROP 5 ROLL ?DUP DROP 0 ?DUP DROP ( -> ) T{DEPTH 5 = ASSERT 1 = ASSERT 2 = ASSERT 3 = ASSERT 2 = ASSERT 3 = ASSERT }T
T{ 1. 2. 3. 2SWAP 2OVER 2DUP 2DROP ( -> ) T{DEPTH 4 2* = ASSERT + 3 = ASSERT + 2 = ASSERT + 3 = ASSERT + 1 = ASSERT }T
T{ 7 >R 6 >R ( -> ) R@ 6 = ASSERT R> 6 = ASSERT RP@ R0 @ < ASSERT RP! RP@ R0 @ = ASSERT }T
T{ 9. D>R ( -> ) RP@ R0 @ < ASSERT R>D RP@ R0 @ = ASSERT 9. D= ASSERT }T
T{ 9.1 F>R ( -> ) RP@ R0 @ < ASSERT R>F RP@ R0 @ = ASSERT 9.1 F= ASSERT }T

\
\ Test arithmetic stuff
\
T{ 0 0 1 -1 ( -> ) 0< ASSERT 0> ASSERT 0= ASSERT NOT ASSERT }T
T{ 1 2 1 1 ( -> ) = ASSERT NOT= ASSERT }T
T{ -1 1 -1 1 ( -> ) < ASSERT U> ASSERT }T
T{ 1 -1 1 -1 ( -> ) > ASSERT U< ASSERT }T
T{ 0 -2 -1 * - 1 * 2 * ( -> ) -4 = ASSERT }T
T{ 0 1- 2- 2* 1+ 2+ BL + 1+ 2/ ( -> ) 15 = ASSERT }T
T{ 16 3 ( -> ) 2DUP / 5 = ASSERT 2DUP MOD 1 = ASSERT /MOD 5 = ASSERT 1 = ASSERT }T
T{ 5 -2 -7 3 ( -> ) NEGATE -3 = ASSERT ABS 7 = ASSERT +- -5 = ASSERT }T
T{ -2 2 2DUP ( -> ) MAX 2 = ASSERT MIN -2 = ASSERT }T
T{ 30000 DUP DUP DUP DUP DUP ( -> ) * / -5 = ASSERT */ 30000 = ASSERT }T
T{ 30000 DUP 70 ( -> ) */MOD 12086 = ASSERT 60 = ASSERT }T
T{ 5 6 2DUP 2DUP ( -> ) AND 4 = ASSERT OR 7 = ASSERT XOR 3 = ASSERT }T
CREATE temp1 9 C, T{ ( -> ) temp1 7 TOGGLE temp1 C@ 14 = ASSERT }T FORGET temp1
VARIABLE temp1 T{ 9 temp1 ! ( -> ) temp1 1+! 5 temp1 +! temp1 @ 15 = ASSERT }T FORGET temp1
T{ PI ( -> ) F->SF SF->F 3.14159265359 F->SF SF->F F= ASSERT }T
T{ PI FDUP F->D 2DUP D->S ( -> ) 3 = ASSERT 3. D= ASSERT PI F= ASSERT }T
T{ 7 DUP S->D 2DUP D->F ( -> ) 7.0 F= ASSERT 7. D= ASSERT 7 = ASSERT }T
T{ -1 S->D -1 U->D ( -> ) 65535. D= ASSERT -1. D= ASSERT }T
T{ -1 -1 U* 10 U/MOD ( -> ) 26214 = ASSERT 5 = ASSERT }T
T{ -1 -1 U* 10 U/ ( -> ) 26214 = ASSERT 5 = ASSERT }T

\
\ Test Vocabulary stuff
\
T{
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
}T

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

FORTH tests DEFINITIONS

\
\ Test User Variables etc.
\
T{
WDSZ WBFR PAD CSP BASE STATE CURRENT CONTEXT >IN BLK VOC-LINK DP FENCE TIB R0 S0
( -> )
T{DEPTH 16 = ASSERT
:INLINE
    T{DEPTH 0 DO 0> ASSERT LOOP
  ;INLINE
}T

T{ NOOP }T

T{
FIRST LIMIT 0 +ORIGIN
( -> )
T{DEPTH 3 = ASSERT
:INLINE
    T{DEPTH 0 DO ASSERT LOOP
  ;INLINE
}T

\
\ Message/Error stuff
\
T{ ( -> ) ?EXEC }T
: temp1 ?COMP DROP 77 ; IMMEDIATE T{ ( -> ) 88 : temp2 temp1 ; 77 = ASSERT }T FORGET temp1
T{ 7 7 ( -> ) ?PAIRS }T
T{ : temp1 ?CSP ; ( -> ) temp1 }T FORGET temp1
T{ ( -> ) 10 MESSAGE }T
T{ ( -> ) 0 10 ?ERROR }T

\
\ Test variables/constants stuff
\
T{ VARIABLE temp1 ( -> ) temp1 @ 0= ASSERT 9 temp1 ! temp1 @ 9 = ASSERT FORGET temp1 }T
T{ CREATE temp1 9 , ( -> ) temp1 @ 9 = ASSERT FORGET temp1 }T
T{ DVARIABLE temp1 ( -> ) temp1 D@ 0. D= ASSERT 9. temp1 D! temp1 D@ 9. D= ASSERT FORGET temp1 }T
T{ CREATE temp1 9. D, ( -> ) temp1 D@ 9. D= ASSERT FORGET temp1 }T
T{ FVARIABLE temp1 ( -> ) temp1 F@ 0.0 F= ASSERT 9.0 temp1 F! temp1 F@ 9.0 F= ASSERT FORGET temp1 }T
T{ CREATE temp1 9.0 F, ( -> ) temp1 F@ 9.0 F= ASSERT FORGET temp1 }T
T{ CREATE temp1 0 C, ( -> ) temp1 C@ 0= ASSERT 9 temp1 C! temp1 C@ 9 = ASSERT FORGET temp1 }T
T{ CREATE temp1 0.0 F->SF SF, ( -> ) temp1 SF@ SF->F 0.0 F= ASSERT 9.0 F->SF temp1 SF! temp1 SF@ SF->F 9.0 F= ASSERT FORGET temp1 }T
T{ 9 CONSTANT temp1 ( -> ) temp1 9 = ASSERT FORGET temp1 }T
T{ 9. DCONSTANT temp1 ( -> ) temp1 9. D= ASSERT FORGET temp1 }T
T{ 9.0 FCONSTANT temp1 ( -> ) temp1 9.0 F= ASSERT FORGET temp1 }T
T{ : temp1 [ 9 ] CLITERAL ; ( -> ) temp1 9 = ASSERT FORGET temp1 }T

\
\ Test conditionals/loops stuff
\
T{ 9 DUP ( -> ) [IF] 9 = ASSERT [ELSE] 0 ASSERT [THEN] }T
T{ 9 DUP ( -> ) NOT [IF] 0 ASSERT [ELSE] 9 = ASSERT [THEN] }T
T{ 9 DUP ( -> ) :INLINE IF 9 = ASSERT ELSE 0 ASSERT THEN ;INLINE }T
T{ 9 DUP ( -> ) :INLINE NOT IF 0 ASSERT ELSE 9 = ASSERT THEN ;INLINE }T
T{ :INLINE 3 0 DO I LOOP ;INLINE ( -> ) 2 = ASSERT 1 = ASSERT 0 = ASSERT }T
T{ :INLINE 0xFFFF 0xFFFC DO I ULOOP ;INLINE ( -> ) 0xFFFE = ASSERT 0xFFFD = ASSERT 0xFFFC = ASSERT }T
T{ :INLINE 6 0 DO I 2 +LOOP ;INLINE ( -> ) 4 = ASSERT 2 = ASSERT 0 = ASSERT }T
T{ :INLINE -6 0 DO I -2 +LOOP ;INLINE ( -> ) -6 = ASSERT -4 = ASSERT -2 = ASSERT 0 = ASSERT }T
T{ :INLINE 4 2 DO 2 0 DO J 10 * I + LOOP LOOP ;INLINE ( -> ) 31 = ASSERT 30 = ASSERT 21 = ASSERT 20 = ASSERT }T
T{ :INLINE 3 0 DO I DUP 1 = IF LEAVE I THEN LOOP ;INLINE ( -> ) 1 = ASSERT 1 = ASSERT 0 = ASSERT }T
T{ :INLINE 3 BEGIN 1- ?DUP WHILE DUP REPEAT ;INLINE ( -> ) 1 = ASSERT 2 = ASSERT }T
T{ :INLINE 3 BEGIN 1- DUP ?DUP 0= UNTIL ;INLINE ( -> ) 0 = ASSERT 1 = ASSERT 2 = ASSERT }T
T{ :INLINE 3 BEGIN 1- DUP ?DUP 0= IF EXIT THEN AGAIN ;INLINE ( -> ) 0 = ASSERT 1 = ASSERT 2 = ASSERT }T

\
\ All done!
\
." Tests run to completion" CR
