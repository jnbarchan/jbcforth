\ random.fth

\ RANDOM NUMBER GENERATOR

FORTH DEFINITIONS
VARIABLE (RND-SEED)

: (SRND)	( seed ... )
    (RND-SEED) !
  ;

: (RND)		( ... rand )
    (RND-SEED) @   259 * 3 +
    32767 AND   DUP (RND-SEED) !
  ;

: RND		( range ... random )
    (RND) 32767 */
  ;

\ Seed with low-word of TIME
TIME D->S (SRND)

: RND-ARRAY	( num ... )
    <BUILDS 0 DO (RND) , LOOP
    DOES>
  ;

\ Memory match up to length1 *words* at addr1 against addr2
\ Leave length2 *words* which match
: MEMW-MATCH	( addr1\addr2\length1 ... length2 )
    0				( addr1\addr2\length1\length2 )
    BEGIN
      2DUP >				( addr1\addr2\length1\length2\[length1>length2]? )
      5 PICK 5 PICK 4 PICK 1+ 2* MEMCMP 0=  ( addr1\addr2\length1\length2\[length1>length2]?\[memcmp[addr1,addr2,length2+1]=0]? )
      AND			( addr1\addr2\length1\length2 )
    WHILE
      1+			( addr1\addr2\length1\length2++ )
    REPEAT
    >R DROP DROP DROP R>	( length2 )
  ;

\ Memory to search is addr1 with size *words* for length1 *words* repeated
\ Longest so far is addr2 of length2 *words*
\ Longest afterward is addr3 of length3 *words*
\ and we have reached reached
: (MEMW-REPEAT)	( addr1\size\length1\addr2\length2 ... addr1\size\length1\addr3\length3\reached )
    OVER	( reached=addr2 )
    BEGIN
      >R 5 PICK 5 PICK 2* + 4 PICK 2* - R>	( addr1+size-length\reached )
      DUP ROT ROT U>				( reached\[addr1+size-length>reached]? )
    WHILE
      DUP 7 PICK 6 PICK				( reached\reached\addr1\length1 )
      MEMW-MATCH				( reached\length_match )
      DUP 4 PICK >				( addr2\length2\reached\length_match\[length_match>length2]?
      IF					( [length_match > length2] )
        ROT DROP ROT DROP OVER			( addr3\length3\reached )
      ELSE					( [length_match <= length2] )
	DROP					( addr2\length2\reached )
      THEN
      1+					( reached++ )
    REPEAT
    DROP					( drop reached )
  ;
 
\ Discover "repeated" memory area
\ Memory area to search starts at addr1 and continues for size *words*
\ length1 is the length in *words* we are looking for to repeat
\ The search leaves area starting at addr2 of length length2 *words*
\ which is the longest area found with matching words
: MEMW-REPEAT	( addr1\size\length1 ... addr2\length2 )
    DUP 0< 5 ?ERROR		( length1 must be > 0 )
    2DUP 2* < 5 ?ERROR		( size must be >= 2 * length1 )
    3 PICK OVER 2* + 0		( pushes addr2=addr1+size*2\length2=0 ) 
    (MEMW-REPEAT)		( addr1\size\length1\addr3\length3 )
    >R >R DROP DROP DROP R> R>	( addr3\length3 )
  ;

\ 20000 rnd-array rnds
\ rnds 20000 5 memw-repeat

\ See what the random number distribution looks like
: RND-DISTRIB	( count\range ... )
    CREATE HERE >R DUP 2* ALLOT DUP 2* R@ SWAP ERASE R> ROT ROT	( addr\count\range )
      SWAP 0 DO			( addr\range )
        DUP RND			( addr\range\random )
	2* 3 PICK + 1 SWAP +!	( addr[random]++ )
				( addr\range )
      LOOP
      DROP DROP			( )
  ;

\ Calculate PI using random numbers )
\ Pick pairs of random numbers between 0 & 1 [float]
\ Compute whether x^2 + y^2 <= 1 or not
\ and count how many are <=1 versus how many are >1
\ the ratio <=1 / total == PI / 4 !
: CALC-PI	( iterations ... PI )
    >R
    0						( count )
    R@ 0 DO
      (RND) S->D D->F 32768.0 F/ FDUP F*	( count\x^2 )
      (RND) S->D D->F 32768.0 F/ FDUP F*	( count\x^2\y^2 )
      F+ 1.0 F<					( count\[x^2 + y^2 < 1.0]? )
      IF 1+ THEN				( count++ )
    LOOP
    S->D D->F R> S->D D->F F/ 4.0 F*		( PI=count/total*4 )
  ;

