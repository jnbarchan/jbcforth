\ play.fth

\ FORTH play things

FORTH DEFINITIONS
VOCABULARY play IMMEDIATE play DEFINITIONS

: 1000-TIMES
    1000 0 DO I DROP J DROP LOOP
  ;

: 1MILLION-TIMES
    1000 0 DO 1000-TIMES LOOP
  ;

: 1BILLION-TIMES
    1000 0 DO 1MILLION-TIMES LOOP
  ;

R: (FACT)	( nd1\n2 ... nd3 )
    ?DUP IF
      DUP 1-
      >R S->D D* R>
      (FACT)
    THEN
  R;

: FACT		( n ... factnd )
    DUP 0< OVER 12 > OR 5 ?ERROR
    1. ROT (FACT)
  ;

R: (BIGFACT)	( nf1\n2 ... nf3 )
    ?DUP IF
      DUP 1-
      >R S->D D->F F* R>
      (BIGFACT)
    THEN
  R;

: BIGFACT	( n ... factnf )
    DUP 0< OVER 170 > OR 5 ?ERROR
    >R 1.0 R> (BIGFACT)
  ;

: DOES
    <BUILDS 99 ,
    DOES> ." I am DOES> "
      ." My first data item is: " ?
      CR
  ;
