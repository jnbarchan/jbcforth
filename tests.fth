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
    \ stored in *descending* Dictionary address order
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
    \ The word called during execution once ASSIGN TRACE-EXECUTE TO-DO TC-EXECUTE has been executed
    \ xt is the word being executed
    \ This is called twice:
    \ f=0 => pre-execution of word
    \ f=1 => post-execution of word
    IF DROP EXIT THEN			( do nothing if flag is "post-execute" )
    \ TC-ARRAY-FIND-SEQUENTIAL		( find the execution token in TC-ARRAY )
    TC-ARRAY-FIND-BINARY		( find the execution token in TC-ARRAY )
    ?DUP IF 2+ 1+! THEN			( increment number of times called )
  ;

: TC-PRETEND-EXECUTE	( xt ... )
    \ Pretend to TC-EXECUTE the xt word
    \ Used to mark certain words as executed when we cannot really execute them/don't want to see them in 0 TC-SHOW
    0 TC-EXECUTE
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
\ Test CLS and start nice and fesh!
\
T{ CLS }T
T{ NOOP }T

\
\ Test case sensitivity
\
T{ CASE-INSENSITIVE }T
T{ Case-Sensitive }T
T{ CASE-INSENSITIVE }T

\
\ Test understanding of layout of dictionary entries
\
T{ ' ' ( -> ) DUP NFA PFA = ASSERT }T
T{ ' ' ( -> ) DUP NFA 36 + = ASSERT }T
T{ ' ' ( -> ) DUP LFA 4 + = ASSERT }T
T{ ' ' ( -> ) DUP CFA 2 + = ASSERT }T

\
\ Test INITVECS initial vectored words
\
T{ ' ABORT ( -> ) @ ' (ABORT) NFA = ASSERT }T
T{ ' CREATE ( -> ) @ ' (CREATE) NFA = ASSERT }T
T{ ' EMIT ( -> ) @ ' (EMIT) NFA = ASSERT }T
T{ ' EMITS ( -> ) @ ' (EMITS) NFA = ASSERT }T
T{ ' KEY ( -> ) @ ' (KEY) NFA = ASSERT }T
T{ ' MESSAGE ( -> ) @ ' MSG# NFA = ASSERT }T
T{ ' NUM ( -> ) @ ' (NUM) NFA = ASSERT }T
\ ' TRACE-EXECUTE ( -> ) @ ' (TRACE-EXECUTE) NFA = ASSERT
T{ ' TRACE-EXECUTE ( -> ) @ ' TC-EXECUTE NFA = ASSERT }T

\
\ Test dictionary allocation primitives
\
T{ DP @ HERE ( -> ) = ASSERT }T
T{ HERE CREATE temp1 FORGET temp1 HERE ( -> ) = ASSERT }T
T{ HERE CREATE temp1 HERE FORGET temp1 ( -> ) = NOT ASSERT }T
T{ CREATE temp1 HERE 5 ALLOT HERE ( -> ) SWAP 5 + = ASSERT FORGET temp1 }T
T{ CREATE temp1 ' temp1 NFA LAST ( -> ) = ASSERT FORGET temp1 }T

\
\ Test finding dictionary entries
\
T{ FIND temp1 ( -> ) NOT ASSERT }T
T{ CREATE temp1 FIND temp1 ( -> ) ASSERT }T
T{ FORGET temp1 FIND temp1 ( -> ) NOT ASSERT }T
T{ CREATE temp1 CREATE temp2 FORGET temp1 FIND temp2 FIND temp1 ( -> ) NOT ASSERT NOT ASSERT }T
T{ ( -> ) ." Expect a message on next line..." CR }T
T{ CREATE temp1 CREATE temp1 FIND temp1 ( -> ) ASSERT }T
T{ FORGET temp1 FIND temp1 ( -> ) ASSERT }T
T{ FORGET temp1 FIND temp1 ( -> ) NOT ASSERT }T
T{ ' NFA NFA ( -> ) DUP C@ 3 = ASSERT COUNT 3 = ASSERT DROP }T
T{ ' ' NFA ( -> ) DUP C@ 1 0x40 OR = ASSERT COUNT 1 0x40 OR = ASSERT DROP }T
T{ ' ' NFA ( -> ) C@ 0x1F AND 1 = ASSERT }T
T{ " (FIND)" CONTEXT @ @ (FIND) ( -> ) ASSERT 6 = ASSERT ' (FIND) NFA = ASSERT }T
T{ " -FIND" CONTEXT @ @ (FIND) ( -> ) ASSERT 5 = ASSERT ' -FIND NFA = ASSERT }T
T{ CONTEXT @ @ -FIND -FIND ( -> ) ASSERT 5 = ASSERT ' -FIND NFA = ASSERT }T
T{ FIND FIND ( -> ) ' FIND NFA = ASSERT }T
T{ CREATE temp1 CREATE temp2 ' temp2 NFA (FINDPREV) ( -> ) ' temp1 NFA = ASSERT }T FORGET temp1
T{ CREATE temp1 ' temp1 NFA (FORGET) FIND temp1 ( -> ) NOT ASSERT }T
T{ CREATE temp1 " temp1" CONTEXT @ @ -FORGET FIND temp1 ( -> ) NOT ASSERT }T

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
\ Test: MACRO
\
T{ " 1234 2*" MACRO temp1 ( -> ) temp1 2468 = ASSERT }T FORGET temp1

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
T{ 5. 16. 3. -1. ( -> ) D- D+ D* 2DUP 100. D= ASSERT 4. D/ 25. D= ASSERT }T
T{ 5 -2 -7 -3 3 ( -> ) NEGATE -3 = ASSERT NEGATE 3 = ASSERT ABS 7 = ASSERT +- -5 = ASSERT }T
T{ 5. -2 -7. -3. 3. ( -> ) DNEGATE -3. D= ASSERT DNEGATE 3. D= ASSERT DABS 7. D= ASSERT D+- -5. D= ASSERT }T
T{ -2 2 2DUP ( -> ) MAX 2 = ASSERT MIN -2 = ASSERT }T
T{ 30000 DUP DUP DUP DUP DUP ( -> ) * / -5 = ASSERT */ 30000 = ASSERT }T
T{ 30000 DUP 70 ( -> ) */MOD 12086 = ASSERT 60 = ASSERT }T
T{ 60000. -70. ( -> ) D/MOD -857. D= ASSERT 10. D= ASSERT }T
T{ 9. 8. 9. 10. ( -> ) D< ASSERT D< NOT ASSERT }T
T{ 5 6 2DUP 2DUP ( -> ) AND 4 = ASSERT OR 7 = ASSERT XOR 3 = ASSERT }T
CREATE temp1 9 C, T{ ( -> ) temp1 7 TOGGLE temp1 C@ 14 = ASSERT }T FORGET temp1
VARIABLE temp1 T{ 9 temp1 ! ( -> ) temp1 1+! 5 temp1 +! temp1 @ 15 = ASSERT }T FORGET temp1
T{ PI ( -> ) F->SF SF->F 3.14159265359 F->SF SF->F F= ASSERT }T
T{ PI FDUP F->D 2DUP D->S ( -> ) 3 = ASSERT 3. D= ASSERT PI F= ASSERT }T
T{ 1.1 2.2 F+ -3.3 F- 2.0 F* 0.5 F/ FDUP FDROP ( -> ) 26.4 F= ASSERT }T
T{ 26.4 FDUP ( -> ) 26.40001 F< ASSERT 26.39999 F< NOT ASSERT }T
T{ 7 DUP S->D 2DUP D->F ( -> ) 7.0 F= ASSERT 7. D= ASSERT 7 = ASSERT }T
T{ -1 S->D -1 U->D ( -> ) 65535. D= ASSERT -1. D= ASSERT }T
T{ -1 -1 U* 10 U/MOD ( -> ) 26214 = ASSERT 5 = ASSERT }T
T{ -1 -1 U* 10 U/ ( -> ) 26214 = ASSERT 5 = ASSERT }T
T{ 50 10 DIGIT 66 10 DIGIT ( -> ) NOT ASSERT ASSERT 2 = ASSERT }T
T{ 50 16 DIGIT 66 16 DIGIT ( -> ) ASSERT 11 = ASSERT ASSERT 2 = ASSERT }T
T{ " -123" NUM " -123" NUMBER ( -> ) -123 = ASSERT -123 = ASSERT }T
T{ " -12345." NUM " -12345." NUMBER ( -> ) -12345. D= ASSERT -12345. D= ASSERT }T
T{ " -12345.6" NUM " -12345.6" NUMBER ( -> ) -12345.6 F= ASSERT -12345.6 F= ASSERT }T
T{ " 0xAB" NUM " 0xAB" NUMBER ( -> ) 171 = ASSERT 171 = ASSERT }T
T{ " 0xABZ" NUM " 12A34" NUM " 1234 " NUM ( -> ) T{DEPTH 0= ASSERT }T
T{ DECIMAL 30 ( -> ) HEX 1E DECIMAL = ASSERT }T
T{ HEX 1E ( -> ) DECIMAL 30 = ASSERT }T

\
\ Test defining dictionary entries
\
T{ : temp1 [ LAST C@ 5 0x80 OR = ASSERT ] ; LAST C@ 5 = ASSERT ' temp1 ( -> ) ASSERT }T FORGET temp1
T{ : temp1 1234 ; temp1 ( -> ) 1234 = ASSERT }T FORGET temp1
T{ R: temp1 [ LAST C@ 5 = ASSERT ] R; LAST C@ 5 = ASSERT ' temp1 ( -> ) ASSERT }T FORGET temp1
T{ R: temp1 DUP IF 1- temp1 THEN R; 4 temp1 ( -> ) 0= ASSERT }T FORGET temp1
T{ : temp1 [ SMUDGE ] DUP IF 1- temp1 THEN [ SMUDGE ] ; 4 temp1 ( -> ) 0= ASSERT }T FORGET temp1

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

T{
FIRST LIMIT 0 +ORIGIN
( -> )
T{DEPTH 3 = ASSERT
:INLINE
    T{DEPTH 0 DO ASSERT LOOP
  ;INLINE
}T

T{ 0x30 USER temp1 temp1 ( -> ) ASSERT }T FORGET temp1

\
\ Test EXVEC: execution-vectored words
\
T{
EXVEC: temp1
' temp1 ( -> ) @ ' NOVEC NFA = ASSERT
ASSIGN temp1 TO-DO NOOP
' temp1 ( -> ) @ ' NOOP NFA = ASSERT
temp1
' temp1 NFA ' NOVEC NFA DOVEC ' temp1 ( -> ) @ ' NOVEC NFA = ASSERT
FORGET temp1
}T

\
\ Test Message/Error stuff
\
T{ ( -> ) ?EXEC }T
: temp1 ?COMP DROP 77 ; IMMEDIATE T{ ( -> ) 88 : temp2 temp1 ; 77 = ASSERT }T FORGET temp1
T{ 7 7 ( -> ) ?PAIRS }T
T{ : temp1 ?CSP ; ( -> ) temp1 }T FORGET temp1
T{ ( -> ) ." Expect a message on next line..." CR }T
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
T{ CREATE temp1 65 C, ( -> ) temp1 C1+! temp1 C@ 66 = ASSERT 10 temp1 C+! temp1 C@ 76 = ASSERT FORGET temp1 }T
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
T{ : temp1 [ HERE ] [ 16 ALLOT ] [ HERE (JUMP) ] [ 2 ALLOT ] ; ( -> ) ' temp1 16 + @ -16 2/ = ASSERT  }T FORGET temp1
T{ : temp1 [ HERE ] [ 16 ALLOT ] [ HERE >R BACK HERE R> - 2 = ASSERT ] ; ( -> ) ' temp1 16 + @ -16 2/ = ASSERT  }T FORGET temp1
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
\ Test memory fill stuff
\
CREATE temp1 5 ALLOT
CREATE temp2 5 ALLOT
T{ temp1 5 123 FILL ( -> ) :INLINE 5 0 DO temp1 I + C@ 123 = ASSERT LOOP ;INLINE }T
T{ temp1 5 123 FILL temp2 5 123 FILL ( -> ) temp1 temp2 5 MEMCMP 0= ASSERT }T
T{ temp1 5 ERASE temp2 5 0 FILL ( -> ) temp1 temp2 5 MEMCMP 0= ASSERT }T
T{ temp1 5 BLANKS temp2 5 BL FILL ( -> ) temp1 temp2 5 MEMCMP 0= ASSERT }T
T{ temp1 5 123 FILL temp2 5 ERASE temp1 temp2 5 CMOVE ( -> ) temp1 temp2 5 MEMCMP 0= ASSERT temp2 C@ 123 = ASSERT }T
T{ temp1 5 123 FILL temp2 5 ERASE temp1 temp2 2 MOVE ( -> ) temp1 temp2 4 MEMCMP 0= ASSERT temp2 5 + C@ 0 = ASSERT }T
FORGET temp2
FORGET temp1

\
\ Test Text strings stuff
\
T{ " Hello" ( -> ) DUP COUNT 5 = ASSERT DROP C@ 5 = ASSERT }T
T{ " Hello" ( -> ) COUNT 5 = ASSERT DUP C@ 72 = ASSERT 4 + C@ 111 = ASSERT }T
T{ : temp1 " Hello" " Hello" ; ( -> ) temp1 6 MEMCMP 0= ASSERT temp1 >R COUNT DROP R> COUNT MEMCMP 0= ASSERT }T FORGET temp1
T{ : temp1 " HelloX" " HelloYY" ; ( -> ) temp1 >R COUNT DROP R> COUNT DROP 5 MEMCMP 0= ASSERT }T FORGET temp1
T{ : temp1 " HelloX" " HelloYY" ; ( -> ) temp1 >R COUNT DROP R> COUNT DROP 6 MEMCMP 0= NOT ASSERT }T FORGET temp1
T{ : temp1 " Hello world   " ; ( -> ) temp1 COUNT DUP 14 = ASSERT -TRAILING 11 = ASSERT DROP }T FORGET temp1
T{ : temp1 " Hello world" ; ( -> ) temp1 COUNT TYPE CR }T FORGET temp1
T{ : temp1 ." Hello world" ; ( -> ) temp1 CR }T FORGET temp1
\ T{ : temp1 [COMPILE] (.") [ BL (WORD) Hello TEXT ] ; ( -> ) temp1 CR }T FORGET temp1
T{ " Hello" COUNT $CONSTANT temp1 ( -> ) " Hello" COUNT DROP temp1 COUNT MEMCMP 0= ASSERT }T FORGET temp1
T{ 10 $VARIABLE temp1 ( -> ) temp1 COUNT 0= ASSERT DROP }T FORGET temp1
T{ 10 $VARIABLE temp1 ( -> ) " Hello" COUNT temp1 $! temp1 COUNT 5 = ASSERT DROP }T FORGET temp1
T{ 10 $VARIABLE temp1 ( -> ) " Hello" COUNT temp1 $! " Bye" COUNT temp1 $+ temp1 COUNT 8 = ASSERT DROP }T FORGET temp1
T{ 10 $VARIABLE temp1 ( -> ) " Hello" COUNT temp1 ($!) " Bye" COUNT temp1 ($+) temp1 COUNT 8 = ASSERT DROP }T FORGET temp1
T{
6 $VARIABLE temp1 ( -> ) " Hello" COUNT temp1 $! ( -> )
temp1 " Hello" $CMP 0= ASSERT
temp1 " Hell" $CMP 0> ASSERT
temp1 " Helloz" $CMP 0< ASSERT
temp1 " HellZ" $CMP 0> ASSERT
temp1 " Hellz" $CMP 0< ASSERT
}T FORGET temp1
T{ CREATE temp1 HERE " Hello" COUNT TEXT 6 ALLOT ( -> ) " Hello" 6 MEMCMP 0= ASSERT }T FORGET temp1
T{ 32 4HEX ( -> ) 4 = ASSERT " 0020" COUNT MEMCMP 0= ASSERT }T

\
\ Test COMPILE/INTERPRET stuff
\
T{
: temp1 STATE @ IF COMPILE LIT , THEN ; IMMEDIATE
temp1 ( -> )
: temp2 [ 1234 ] temp1 ; temp2 ( -> ) 1234 = ASSERT FORGET temp2
: temp2 STATE @ IF
    SWAP [COMPILE] temp1
    [COMPILE] temp1
  THEN ; IMMEDIATE
: temp3 [ 123456. ] temp2 ; temp3 ( -> ) 123456. D= ASSERT
}T FORGET temp3 FORGET temp2 FORGET temp1
T{ INTERPRET 1234 2* ( -> ) 2468 = ASSERT }T
T{ INTERPRET" 1234 2*" ( -> ) 2468 = ASSERT }T
T{ : temp1 INTERPRET" 1234 2*" ; temp1 ( -> ) 2468 = ASSERT }T FORGET temp1

\
\ Test LOCAL stuff
\
T{ : temp1 (LOCAL) param1 4567 (LOCAL) local1 param1 @ local1 @ ; 1234 temp1 ( -> ) 4567 = ASSERT 1234 = ASSERT }T FORGET temp1
T{
VARIABLE temp1
1234 TO temp1 temp1 @ ( -> ) 1234 = ASSERT
: temp2 4567 TO temp1 temp1 @ ; temp2 ( -> ) 4567 = ASSERT FORGET temp2
}T FORGET temp1
T{ : temp1 0 (LOCAL) local1 0 (LOCAL) local2 1234 TO local1 local1 @ 4567 TO local2 local2 @ ; temp1 ( -> ) 4567 = ASSERT 1234 = ASSERT }T FORGET temp1

\
\ Test OS stuff
\
T{ : temp1 " ls >/dev/null" COUNT 2DUP + 0 SWAP C! DROP OSCLI ; ( -> ) temp1 }T FORGET temp1
T{ : temp1 [ " ls  >/dev/null" COUNT 2DUP + 0 SWAP C! ] >CLI ; ( -> ) temp1 }T FORGET temp1
T{ OS' ls >/dev/null' }T
T{ : temp1 OS' ls >/dev/null ' ; ( -> ) temp1 }T FORGET temp1
T{ OS-PID ( -> ) OR ASSERT }T

\
\ Test LOAD file stuff
\
T{ ?LOAD" non-existent.fth" ( -> ) NOT ASSERT }T
T{ ?LOAD" non-existent.fth" ( -> ) NOT ASSERT OSERRNO 2 = ASSERT }T
\ T{ ?LOAD" non-existent.fth" ( -> ) NOT ASSERT OSERROR }T
T{ ?LOAD" empty.fth" ( -> ) ASSERT }T
T{ : temp1 ?LOAD" empty.fth" ; ( -> ) temp1 ASSERT }T FORGET temp1
T{ : temp1 ?LOAD" non-existent.fth" ; ( -> ) temp1 NOT ASSERT }T FORGET temp1
T{ LOAD" empty.fth" ( -> ) }T
T{ : temp1 LOAD" empty.fth" ; ( -> ) temp1 }T FORGET temp1

\
\ Test output stuff
\
T{ ( -> ) ." Expect some output on next lines..." CR }T
T{ 16 >R H.RS .RS R> DROP ( -> ) }T
T{ 16 H.S .S DROP ( -> ) }T
T{ 16 4 .R 166661. 10 D.R ( -> ) }T CR
T{ 65 C. 123456. DH. 123.456 F->SF SF. 123456.789 F. -123456. UD. 123456. D. ' A. A. 21 B. 1234 H. 1234 DEC. -1234 U. 1234 . ( -> ) }T CR
CREATE temp1 8 ALLOT
T{ 65 temp1 C! temp1 C? ( -> ) }T
T{ 123456. temp1 D! temp1 DH? temp1 D? ( -> ) }T
T{ 123456.78 temp1 F! temp1 F? ( -> ) }T
T{ ' A? temp1 ! temp1 A? ( -> ) }T
T{ 0x1234 temp1 ! temp1 H? temp1 ? ( -> ) }T
FORGET temp1 CR
T{ ' ID. NFA ID. ( -> ) }T
T{ SPACE 8 SPACES }T CR
T{ 65 EMIT }T
T{ " Hello" COUNT EMITS }T CR

\
\ Test sundry stuff
\
T{ TIME ( -> ) TIME D- -1 D+- 2DUP 0. D= >R 1. D= R> OR ASSERT  }T
T{ MICRO-TIME ( -> ) 2DROP }T
T{ 0 SLEEP ( -> ) }T

\
\ "Pretend" to execute those words which we cannot actually test/execute
\ so that they do not show as not tested in 0 TC-SHOW
\

' RUN-TESTS NFA TC-PRETEND-EXECUTE

' OS-FORK NFA TC-PRETEND-EXECUTE

' DECOMPILE NFA TC-PRETEND-EXECUTE
' DEBUGOFF NFA TC-PRETEND-EXECUTE
' DEBUGON NFA TC-PRETEND-EXECUTE
' UNBREAK NFA TC-PRETEND-EXECUTE
' BREAK NFA TC-PRETEND-EXECUTE
' LIST-BREAK NFA TC-PRETEND-EXECUTE
' TRACE-ALL NFA TC-PRETEND-EXECUTE
' TRACEOFF NFA TC-PRETEND-EXECUTE
' TRACEON NFA TC-PRETEND-EXECUTE
' TRACE-COLONS NFA TC-PRETEND-EXECUTE
' UNTRACE NFA TC-PRETEND-EXECUTE
' TRACE NFA TC-PRETEND-EXECUTE
' TRACE-EXECUTE NFA TC-PRETEND-EXECUTE
' (TRACE-EXECUTE-STACK) NFA TC-PRETEND-EXECUTE
' (TRACE-EXECUTE) NFA TC-PRETEND-EXECUTE

\
\ Test 1ARRAY stuff
\
T{ 50 2 1ARRAY temp1 ( -> ) 0 temp1 1ARRAY-COUNT 50 = ASSERT 0 temp1 1ARRAY-SIZE 2 = ASSERT }T FORGET temp1
T{
50 2 1ARRAY temp1
:INLINE 50 0 DO I 100 + I temp1 ! LOOP ;INLINE
:INLINE 50 0 DO I temp1 @ I 100 + = ASSERT LOOP ;INLINE
: temp2 @ - ;
' temp1 NFA ' temp2 NFA 500 1ARRAY-BINARY-SEARCH ( -> ) 0= ASSERT
' temp1 NFA ' temp2 NFA 123 1ARRAY-BINARY-SEARCH ( -> ) 23 temp1 = ASSERT
}T FORGET temp2 FORGET temp1
T{
26 1 1ARRAY temp1
:INLINE 26 0 DO I 65 + I temp1 C! LOOP ;INLINE
: temp2 C@ - ;
' temp1 NFA ' temp2 NFA 91 1ARRAY-BINARY-SEARCH ( -> ) 0= ASSERT
' temp1 NFA ' temp2 NFA 90 1ARRAY-BINARY-SEARCH ( -> ) 25 temp1 = ASSERT
}T FORGET temp2 FORGET temp1
T{
26 7 1ARRAY temp1
:INLINE 26 0 DO " ABCDEF" I temp1 7 CMOVE I temp1 4 + I 3 - SWAP C+! LOOP ;INLINE
: temp2 $CMP ;
' temp1 NFA ' temp2 NFA " ABCYEZ" 1ARRAY-BINARY-SEARCH ( -> ) 0= ASSERT
' temp1 NFA ' temp2 NFA " ABCYEF" 1ARRAY-BINARY-SEARCH ( -> ) 24 temp1 = ASSERT
FORGET temp2
: temp2 $CMP NEGATE ;
' temp1 NFA ' temp2 NFA " ABCYEF" 1ARRAY-BINARY-SEARCH ( -> ) 0= ASSERT
' temp1 NFA ' temp2 NFA 1ARRAY-BUBBLE-SORT
' temp1 NFA ' temp2 NFA " ABCYEF" 1ARRAY-BINARY-SEARCH ( -> ) 1 temp1 = ASSERT
}T FORGET temp2 FORGET temp1

\
\ Test random.fth stuff
\
T{ 1234 (SRND) ( -> ) (RND-SEED) @ 1234 = ASSERT }T
T{ 100 RND ( -> ) 75 = ASSERT (RND-SEED) @ 24697 = ASSERT }T
T{ :INLINE 100 0 DO 6 RND DUP -1 > SWAP 6 < AND ASSERT LOOP ;INLINE ( -> ) }T
T{ TIME D->S (SRND) }T
T{
25 2 1ARRAY temp1
:INLINE 25 0 DO (RND) I temp1 ! LOOP ;INLINE
: temp2 @ SWAP @ SWAP - ;
' temp1 NFA ' temp2 NFA 1ARRAY-BUBBLE-SORT
:INLINE 25 1- 0 DO I temp1 DUP @ SWAP 2+ @ ( -> ) > NOT ASSERT LOOP ;INLINE
}T FORGET temp2 FORGET temp1
T{ 1000 CALC-PI ( -> ) FDUP F>R 3.0 F< NOT R>F 3.3 F< AND ASSERT }T

' MEMW-REPEAT NFA TC-PRETEND-EXECUTE
' (MEMW-REPEAT) NFA TC-PRETEND-EXECUTE
' MEMW-MATCH NFA TC-PRETEND-EXECUTE
' RND-DISTRIB NFA TC-PRETEND-EXECUTE
' RND-ARRAY NFA TC-PRETEND-EXECUTE

\
\ play.fth stuff
\
FIND play ?DUP [IF] TC-PRETEND-EXECUTE [THEN]

\
\ All done!
\
." Tests run to completion" CR
