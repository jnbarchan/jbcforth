// JBC FORTH!!

#include <setjmp.h>
#define __USE_XOPEN_EXTENDED
#include <signal.h>

#include <stdint.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>

#include "defs.h"
#include "external.h"

// ***********************************
// ********** <BASIC TYPES> **********
// ***********************************

typedef void *PVOID;

typedef void (*VOIDFUNC)(void);

typedef byte BYTE, *PBYTE;

typedef int16_t single_t;
typedef single_t SINGLE, *PSINGLE;
typedef uint16_t usingle_t;
typedef usingle_t USINGLE, *PUSINGLE;

typedef int32_t double_t;
typedef double_t DOUBLE, *PDOUBLE;
typedef uint32_t udouble_t;
typedef udouble_t UDOUBLE, *PUDOUBLE;
typedef uint16_t addr_t;
typedef addr_t ADDR, *PADDR;
#define ADDR_0 ((ADDR)0)
typedef int16_t offset_t;
typedef offset_t OFFSET, *POFFSET;

typedef double float_t;
typedef float_t FLOAT, *PFLOAT;
typedef float small_float_t;
typedef small_float_t SMALLFLOAT, *PSMALLFLOAT;

#define ELEMENTS_COUNT(array) (sizeof(array) / sizeof((array)[0]))
#define ELEMENT_INDEX(array, element) ((element) - (array))

#define BUILD_BUG_ON(condition) extern char _BUILD_BUG_ON_ [(sizeof(char[1 - 2*!!(condition)]))]

// ************************************
// ********** </BASIC TYPES> **********
// ************************************


// ***********************************
// ********** <FORTH TYPES> **********
// ***********************************

typedef struct forthstring
{
  byte len;
  char chars[0];
} __attribute__((packed)) FORTHSTRING, *PFORTHSTRING;

typedef struct forthmaxlenstring
{
  byte maxlen;
  FORTHSTRING string;
} __attribute__((packed)) FORTHMAXLENSTRING, *PFORTHMAXLENSTRING;

// ************************************
// ********** </FORTH TYPES> **********
// ************************************


// ****************************************
// ********** <FORTH DICTIONARY> **********
// ****************************************

// structure
//   address: top-dictent  \ ptr to top-most word defined in Vocabulary
//   address: voc-link     \ ptr to previous Vocabulary
// end-structure vocab
typedef struct forthvocab
{
  ADDR top_dictent;
  ADDR voc_link;
} __attribute__((packed)) FORTHVOCAB, *PFORTHVOCAB;

// structure
//   byte:       flag           \ 3bit flags + length of word's name
//   char-array: name           \ name's runtime length isn't known at compile time
//   address:    previous       \ link field, backward ptr to previous word
//   address:    codeword       \ ptr to the code to execute this word
//   any-array:  parameterfield \ unknown length of data, words, or opcodes
// end-structure forthword

typedef struct forthdictent FORTHDICTENT, *PFORTHDICTENT;

typedef struct forthdictflag
{
  byte len       : 5;	// name length: 1--30
  byte unused    : 1;	// "unused" bit
  byte immediate : 1;	// "immediate" bit: 0 => compile/execute, 1 => execute always
  byte smudge    : 1;	// "smudge" bit: 0 => definition complete, 1 => definition incomplete
} __attribute__((packed)) FORTHDICTFLAG;
#define DICT_SMUDGE	0x80
#define DICT_IMMEDIATE	0x40
#define DICT_UNUSED	0x20
#define DICT_FLAGS	(DICT_SMUDGE | DICT_IMMEDIATE | DICT_UNUSED)
#define MAX_DICT_NAME_LEN 30
typedef char FORTHDICTNAME[MAX_DICT_NAME_LEN + 1], *PFORTHDICTNAME;
typedef int (*FORTHDICTNAMECMP)(const char *s1, const char *s2, size_t n);

typedef struct nfa
{
  FORTHDICTFLAG flag;
  FORTHDICTNAME name;
} __attribute__((packed)) NFA, *PNFA;			// name field address

typedef addr_t LFA, *PLFA;	// link field address
#define INVALID_LFA ((LFA)(0))

typedef enum cfe		// code field enum
{
  cfe_primitive,	// native primitive
  cfe_create,		// CREATE
  cfe_constant,		// CONSTANT
  cfe_double_constant,	// DCONSTANT
  cfe_float_constant,	// FCONSTANT
  cfe_string_constant,	// $CONSTANT
  cfe_variable,		// VARIABLE
  cfe_double_variable,	// DVARIABLE
  cfe_float_variable,	// FVARIABLE
  cfe_string_variable,	// $VARIABLE
  cfe_user_variable,	// USER
  cfe_local_variable,	// (LOCAL)
  cfe_colon,		// :
  cfe_does_gt,		// DOES>
  cfe_exvec,		// EXVEC:
} CFE, *PCFE;
typedef uint16_t CFA, *PCFA;	// code field address

static const char *cfe_Names[] =	// code field enum names (for forthDumpDictEnt)
{
  "primitive",		// cfe_primitive
  "create",		// cfe_create
  "constant",		// cfe_constant
  "double_constant",	// cfe_double_constant
  "float_constant",	// cfe_float_constant
  "string_constant",	// cfe_string_constant
  "variable",		// cfe_variable
  "double_variable",	// cfe_double_variable
  "float_variable",	// cfe_float_variable
  "string_variable",	// cfe_string_variable
  "user_variable",	// cfe_user_variable
  "local_variable",	// cfe_local_variable
  "colon",		// cfe_colon
  "does_gt",		// cfe_does_gt
  "exvec",		// cfe_exvec
};

//STEP#1
typedef enum primitive
{
  primitive_STORE,		// !
  primitive_QUOTE,		// "
  primitive_DOLLAR_STORE,	// $!
  primitive_DOLLAR_PLUS,	// $+
  primitive_DOLLAR_COMMA,	// $,
  primitive_DOLLAR_CMP,		// $CMP
  primitive_DOLLAR_CMPID,		// $CMPID
  primitive_DOLLAR_CONSTANT,	// $CONSTANT
  primitive_DOLLAR_INTERPRET,	// $INTERPRET
  primitive_DOLLAR_VARIABLE,	// $VARIABLE
  primitive_TICK,		// '
  primitive_PAREN,		// (
  primitive_BRACKET_QUOTE,	// (")
  primitive_BRACKET_DOLLAR_STORE,// ($!)
  primitive_BRACKET_DOLLAR_PLUS,// ($+)
  primitive_BRACKET_PLUS_LOOP,	// (+LOOP)
  primitive_BRACKET_DOT_QUOTE,	// (.")
  primitive_BRACKET_QUERY_LOAD,	// (?LOAD)
  primitive_BRACKET_ABORT,	// (ABORT)
  primitive_BRACKET_ASSERT,	// (ASSERT)
  primitive_BRACKET_CLI,	// (CLI)
  primitive_BRACKET_CREATE,	// (CREATE)
  primitive_BRACKET_DECLARE_LOCALVAR,// (DECLARE-LOCALVAR)
  primitive_BRACKET_EMIT,	// (EMIT)
  primitive_BRACKET_EMITS,	// (EMITS)
  primitive_BRACKET_EXIT,	// (EXIT)
  primitive_BRACKET_FIND,	// (FIND)
  primitive_BRACKET_FIND_PREV,	// (FINDPREV)
  primitive_BRACKET_DO,		// (DO)
  primitive_BRACKET_FORGET,	// (FORGET)
  primitive_BRACKET_INTERPRET_QUOTE,// (INTERPRET")
  primitive_BRACKET_JUMP,	// (JUMP)
  primitive_BRACKET_KEY,	// (KEY)
  primitive_BRACKET_LOAD,	// (LOAD)
  primitive_BRACKET_LOCAL,	// (LOCAL)
  primitive_BRACKET_LOCALVAR,	// (LOCALVAR)
  primitive_BRACKET_LOOP,	// (LOOP)
  primitive_BRACKET_NUM,	// (NUM)
  primitive_BRACKET_TO,		// (TO)
  primitive_BRACKET_TO_LOCALVAR,// (TO-LOCALVAR)
  primitive_BRACKET_TRACE_EXECUTE,// (TRACE-EXECUTE)
  primitive_BRACKET_TRACE_EXECUTE_STACK,// (TRACE-EXECUTE-STACK)
  primitive_BRACKET_U_LOOP,	// (ULOOP)
  primitive_BRACKET_VOCABULARY,	// (VOCABULARY)
  primitive_BRACKET_WORD,	// (WORD)
  primitive_TIMES,		// *
  primitive_TIMES_DIVIDE,	// */
  primitive_TIMES_DIVIDE_MOD,	// */MOD
  primitive_PLUS,		// +
  primitive_PLUS_STORE,		// +!
  primitive_PLUS_MINUS,		// +-
  primitive_PLUS_LOOP,		// +LOOP
  primitive_PLUS_ORIGIN,	// +ORIGIN
  primitive_COMMA,		// ,
  primitive_SUBTRACT,		// -
  primitive_DASH_FIND,		// -FIND
  primitive_DASH_FORGET,	// -FORGET
  primitive_DASH_TRAILING,	// -TRAILING
  primitive_DOT,		// .
  primitive_DOT_QUOTE,		// ."
  primitive_DOT_R,		// .R
  primitive_DOT_RS,		// .RS
  primitive_DOT_S,		// .S
  primitive_DIVIDE,		// /
  primitive_DIVIDE_MOD,		// /MOD
  primitive_ZERO_LESS,		// 0<
  primitive_ZERO_EQUALS,	// 0=
  primitive_ZERO_GREATER,	// 0>
  primitive_ZERO_BRANCH,	// 0BRANCH
  primitive_1_PLUS,		// 1+
  primitive_1_PLUS_STORE,	// 1+!
  primitive_1_MINUS,		// 1-
  primitive_1_MINUS_STORE,	// 1-!
  primitive_2_PLUS,		// 2+
  primitive_2_MINUS,		// 2-
  primitive_2_TIMES,		// 2*
  primitive_2_DIVIDE,		// 2/
  primitive_2_DROP,		// 2DROP
  primitive_2_DUP,		// 2DUP
  primitive_2_OVER,		// 2OVER
  primitive_2_SWAP,		// 2SWAP
  primitive_4_HEX,		// 4HEX
  primitive_COLON,		// :
  primitive_COLON_NONAME,	// :NONAME
  primitive_SEMI_COLON,		// ;
  primitive_LESS_THAN,		// <
  primitive_LESS_BUILDS,	// <BUILDS
  primitive_EQUALS,		// =
  primitive_GREATER_THAN,	// >
  primitive_TO_CLI,		// >CLI
  primitive_TO_R,		// >R
  primitive_QUESTION_MARK,	// ?
  primitive_QUERY_COMP,		// ?COMP
  primitive_QUERY_CSP,		// ?CSP
  primitive_QUERY_DUP,		// ?DUP
  primitive_QUERY_ERROR,	// ?ERROR
  primitive_QUERY_EXEC,		// ?EXEC
  primitive_QUERY_LOAD_QUOTE,	// ?LOAD"
  primitive_QUERY_PAIRS,	// ?PAIRS
  primitive_QUERY_STACK,	// ?STACK
  primitive_FETCH,		// @
  primitive_FETCH_EXECUTE,	// @EXECUTE
  primitive_A_DOT,		// A.
  primitive_A_QUESTION_MARK,	// A?
  vector_ABORT,			// ABORT
  primitive_ABS,		// ABS
  primitive_AGAIN,		// AGAIN
  primitive_A_LIT,		// ALIT
  primitive_A_LITERAL,		// ALITERAL
  primitive_ALLOT,		// ALLOT
  primitive_AND,		// AND
  primitive_ASSIGN,		// ASSIGN
  primitive_ASSERT,		// ASSERT
  primitive_B_DOT,		// B.
  primitive_BACK,		// BACK
  primitive_BEGIN,		// BEGIN
  primitive_BLANKS,		// BLANKS
  primitive_BRANCH,		// BRANCH
  primitive_BREAK,		// BREAK
  primitive_C_STORE,		// C!
  primitive_C_PLUS_STORE,	// C+!
  primitive_C_COMMA,		// C,
  primitive_C_DOT,		// C.
  primitive_C_QUESTION_MARK,	// C?
  primitive_C_FETCH,		// C@
  primitive_C_1_PLUS_STORE,	// C1+!
  primitive_C_1_MINUS_STORE,	// C1-!
  primitive_CASE_INSENSITIVE,	// CASE-INSENSITIVE
  primitive_CASE_SENSITIVE,	// CASE-SENSITIVE
  primitive_CFA,		// CFA
  primitive_C_LIT,		// CLIT
  primitive_C_LITERAL,		// CLITERAL
  primitive_C_MOVE,		// CMOVE
  primitive_COLD,		// COLD
  primitive_COMPILE,		// COMPILE
  primitive_CONSTANT,		// CONSTANT
  primitive_COUNT,		// COUNT
  vector_CREATE,		// CREATE
  primitive_CR,			// CR
  primitive_D_STORE,		// D!
  primitive_D_TIMES,		// D*
  primitive_D_PLUS,		// D+
  primitive_D_PLUS_MINUS,	// D+-
  primitive_D_SUBTRACT,		// D-
  primitive_D_TO_F,		// D->F
  primitive_D_TO_S,		// D->S
  primitive_D_DIVIDE,		// D/
  primitive_D_DIVIDE_MOD,	// D/MOD
  primitive_D_COMMA,		// D,
  primitive_D_DOT,		// D.
  primitive_D_DOT_R,		// D.R
  primitive_D_LESS_THAN,	// D<
  primitive_D_EQUALS,		// D=
  primitive_D_TO_R,		// D>R
  primitive_D_QUESTION_MARK,	// D?
  primitive_D_FETCH,		// D@
  primitive_D_ABS,		// DABS
  primitive_D_CONSTANT,		// DCONSTANT
  primitive_DEBUG_OFF,		// DEBUGOFF
  primitive_DEBUG_ON,		// DEBUGON
  primitive_DEC_DOT,		// DEC.
  primitive_DECIMAL,		// DECIMAL
  primitive_DECOMPILE,		// DECOMPILE
  primitive_DEFINITIONS,	// DEFINITIONS
  primitive_DEPTH,		// DEPTH
  primitive_DICTDUMP,		// DICTDUMP
  primitive_DIGIT,		// DIGIT
  primitive_DO,			// DO
  primitive_DOVEC,		// DOVEC
  primitive_D_H_DOT,		// DH.
  primitive_D_H_QUESTION_MARK,	// DH?
  primitive_D_LIT,		// DLIT
  primitive_D_LITERAL,		// DLITERAL
  primitive_D_NEGATE,		// DNEGATE
  primitive_DOES_GREATER,	// DOES>
  primitive_DROP,		// DROP
  primitive_DUP,		// DUP
  primitive_D_VARIABLE,		// DVARIABLE
  primitive_ELSE,		// ELSE
  vector_EMIT,			// EMIT
  vector_EMITS,			// EMITS
  primitive_ERASE,		// ERASE
  primitive_ERROR,		// ERROR
  primitive_EXECUTE,		// EXECUTE
  primitive_EXIT,		// EXIT
  primitive_EXVEC_COLON,	// EXVEC:
  primitive_F_STORE,		// F!
  primitive_F_TIMES,		// F*
  primitive_F_PLUS,		// F+
  primitive_F_SUBTRACT,		// F-
  primitive_F_TO_D,		// F->D
  primitive_F_TO_SF,		// F->SF
  primitive_F_DIVIDE,		// F/
  primitive_F_COMMA,		// F,
  primitive_F_DOT,		// F.
  primitive_F_LESS_THAN,	// F<
  primitive_F_EQUALS,		// F=
  primitive_F_TO_R,		// F>R
  primitive_F_QUESTION_MARK,	// F?
  primitive_F_FETCH,		// F@
  primitive_F_CONSTANT,		// FCONSTANT
  primitive_F_DROP,		// FDROP
  primitive_F_DUP,		// FDUP
  primitive_FILL,		// FILL
  primitive_FIND,		// FIND
  primitive_FIND_VOCABULARY,	// FIND-VOCABULARY
  primitive_FIRST,		// FIRST
  primitive_F_LIT,		// FLIT
  primitive_F_LITERAL,		// FLITERAL
  primitive_FORGET,		// FORGET
  primitive_FORTH,		// FORTH
  primitive_F_VARIABLE,		// FVARIABLE
  primitive_H_DOT,		// H.
  primitive_H_DOT_RS,		// H.RS
  primitive_H_DOT_S,		// H.S
  primitive_H_QUESTION_MARK,	// H?
  primitive_HERE,		// HERE
  primitive_HEX,		// HEX
  primitive_I,			// I
  primitive_ID_DOT,		// ID.
  primitive_IF,			// IF
  primitive_IMMEDIATE,		// IMMEDIATE
  primitive_INITVECS,		// INITVECS
  primitive_INTERPRET,		// INTERPRET
  primitive_INTERPRET_QUOTE,	// INTERPRET"
  primitive_J,			// J
  vector_KEY,			// KEY
  primitive_LAST,		// LAST
  primitive_LEAVE,		// LEAVE
  primitive_LFA,		// LFA
  primitive_LIMIT,		// LIMIT
  primitive_LIST_BREAK,		// LIST-BREAK
  primitive_LIT,		// LIT
  primitive_LITERAL,		// LITERAL
  primitive_LOAD_QUOTE,		// LOAD"
  primitive_LOOP,		// LOOP
  primitive_MAX,		// MAX
  primitive_MEMCMP,		// MEMCMP
  primitive_MEMDUMP,		// MEMDUMP
  primitive_MEMDUMP_W,		// MEMDUMPW
  vector_MESSAGE,		// MESSAGE
  primitive_MIN,		// MIN
  primitive_MICRO_TIME,		// MICRO-TIME
  primitive_MOD,		// MOD
  primitive_MOVE,		// MOVE
  primitive_MSG_HASH,		// MSG#
  primitive_NEGATE,		// NEGATE
  primitive_NFA,		// NFA
  primitive_NOOP,		// NOOP
  primitive_NOT,		// NOT
  primitive_NOT_EQUALS,		// NOT=
  primitive_NOVEC,		// NOVEC
  vector_NUM,			// NUM
  primitive_NUMBER,		// NUMBER
  primitive_OR,			// OR
  primitive_OS_QUOTE,		// OS'
  primitive_OS_FORK,		// OS-FORK
  primitive_OS_PID,		// OS-PID
  primitive_OS_CLI,		// OSCLI
  primitive_OS_ERRNO,		// OSERRNO
  primitive_OS_ERROR,		// OSERROR
  primitive_OVER,		// OVER
  primitive_PAD,		// PAD
  primitive_PFA,		// PFA
  primitive_PICK,		// PICK
  primitive_PRUNE,		// PRUNE
  primitive_QUERY,		// QUERY
  primitive_QUIT,		// QUIT
  primitive_R_COLON,		// R:
  primitive_R_SEMI_COLON,	// R;
  primitive_R_FROM,		// R>
  primitive_R_FROM_D,		// R>D
  primitive_R_FROM_F,		// R>F
  primitive_R_FETCH,		// R@
  primitive_REPEAT,		// REPEAT
  primitive_ROLL,		// ROLL
  primitive_ROT,		// ROT
  primitive_RP_STORE,		// RP!
  primitive_RP_FETCH,		// RP@
  primitive_S_TO_D,		// S->D
  primitive_SF_STORE,		// SF!
  primitive_SF_TO_F,		// SF->F
  primitive_SF_COMMA,		// SF,
  primitive_SF_DOT,		// SF.
  primitive_SF_FETCH,		// SF@
  primitive_SLEEP,		// SLEEP
  primitive_SMUDGE,		// SMUDGE
  primitive_SP_STORE,		// SP!
  primitive_SP_FETCH,		// SP@
  primitive_SPACE,		// SPACE
  primitive_SPACES,		// SPACES
  primitive_STDOUT_FLUSH,	// STDOUT-FLUSH
  primitive_STDOUT_TO,		// STDOUT>
  primitive_STDOUT_TO_TO,	// STDOUT>>
  primitive_STRING,		// STRING
  primitive_SWAP,		// SWAP
  primitive_TEXT,		// TEXT
  primitive_THEN,		// THEN
  primitive_TIME,		// TIME
  primitive_TO,			// TO
  primitive_TO_DO,		// TO-DO
  primitive_TOGGLE,		// TOGGLE
  primitive_TRACE,		// TRACE
  primitive_TRACE_ALL,		// TRACE-ALL
  primitive_TRACE_COLONS,	// TRACE-COLONS
  vector_TRACE_EXECUTE,		// TRACE-EXECUTE
  primitive_TRACE_OFF,		// TRACEOFF
  primitive_TRACE_ON,		// TRACEON
  primitive_TYPE,		// TYPE
  primitive_U_LOOP,		// ULOOP
  primitive_UNTIL,		// UNTIL
  primitive_U_TIMES,		// U*
  primitive_U_TO_D,		// U->D
  primitive_U_DOT,		// U.
  primitive_U_DIVIDE,		// U/
  primitive_U_DIVIDE_MOD,	// U/MOD
  primitive_U_LESS_THAN,	// U<
  primitive_U_GREATER_THAN,	// U>
  primitive_U_D_DOT,		// UD.
  primitive_UNBREAK,		// UNBREAK
  primitive_UNTRACE,		// UNTRACE
  primitive_USER,		// USER
  primitive_VARIABLE,		// VARIABLE
  primitive_VDUMP,		// VDUMP
  primitive_VLIST,		// VLIST
  primitive_VOCABULARY,		// VOCABULARY
  primitive_WARM,		// WARM
  primitive_WBFR,		// WBFR
  primitive_WDSZ,		// WDSZ
  primitive_WHILE,		// WHILE
  primitive_WORD,		// WORD
  primitive_X_OR,		// XOR
  primitive_LEFT_BRACKET,	// [
  primitive_BRACKET_COMPILE,	// [COMPILE]
  primitive_BRACKET_ELSE,	// [ELSE]
  primitive_BRACKET_IF,		// [IF]
  primitive_BRACKET_THEN,	// [THEN]
  primitive_BACKSLASH,		// \ (backslash)
  primitive_RIGHT_BRACKET,	// ]
} __attribute__((packed)) primitive_t;

typedef union pfa		// parameter field address
{
  primitive_t primitives[0];	// used by cfe_primitive
  byte bytes[0];		// used by cfe_user_variable
  SINGLE singles[0];		// used by cfe_constant, cfe_variable
  DOUBLE doubles[0];		// used by cfe_double_constant, cfe_double_variable
  FLOAT floats[0];		// used by cfe_float_constant, cfe_float_variable
  FORTHSTRING conststring[0];	// used by cfe_string_constant
  FORTHMAXLENSTRING varstring[0];// used by cfe_string_variable
  ADDR addrs[0];		// used by cfe_colon, cfe_does_gt, cfe_exvec
}  __attribute__((packed)) __attribute__ ((aligned (2))) PFA, *PPFA;
// in a cfe_colon, the special ADDR which causes EXIT from the sequence in PFA
#define PFA_ADDR_EXIT ADDR_0

struct forthdictent	// FORTH dictionary entry
{
  NFA nfa;		// name field address (32 bytes)
  LFA lfa;		// link field address (2 bytes)
  CFA cfa;		// code field address (2 bytes)
  PFA pfa;		// parameter field address (0..?? bytes)
} __attribute__((packed)) __attribute__ ((aligned (2)));
// compile-time error if struct forthdictent is not size expected
BUILD_BUG_ON(sizeof(struct forthdictent) != 36);
// convert DP to LFA
#define DP_TO_LFA(dp) (((dp) == NULL) ? INVALID_LFA : ((PBYTE)(dp) - g_pFM))
// convert LFA to DP
#define LFA_TO_DP(lfa) (((lfa) == INVALID_LFA) ? NULL : (PFORTHDICTENT)(g_pFM + (lfa)))
// convert DP to previous DP (via LFA)
#define DP_TO_PREV_DP(dp) (LFA_TO_DP((dp)->lfa))
// convert PFA to CFA
#define PFA_TO_CFA(pfa) (ADDR)((pfa) - sizeof(CFA))
// convert PPFA to PCFA
#define PPFA_TO_PCFA(pPFA) (PCFA)PFA_TO_CFA((PBYTE)pPFA)
// convert PFA to LFA
#define PFA_TO_LFA(pfa) (ADDR)((pfa) - sizeof(CFA) - sizeof(LFA))
// convert PPFA to PLFA
#define PPFA_TO_PLFA(pPFA) (PNFA)PFA_TO_NFA((PBYTE)pPFA)
// convert PFA to NFA
#define PFA_TO_NFA(pfa) (ADDR)((pfa) - sizeof(CFA) - sizeof(LFA) - sizeof(NFA))
// convert PPFA to PNFA
#define PPFA_TO_PNFA(pPFA) (PNFA)PFA_TO_NFA((PBYTE)pPFA)
// convert NFA to PFA
#define NFA_TO_PFA(nfa) (ADDR)((nfa) + sizeof(NFA) + sizeof(LFA) + sizeof(CFA))
// convert PNFA to PPFA
#define PNFA_TO_PPFA(pNFA) (PPFA)NFA_TO_PFA((PBYTE)pNFA)

// *****************************************
// ********** </FORTH DICTIONARY> **********
// *****************************************


// ****************************************
// ********** <GLOBAL VARIABLES> **********
// ****************************************

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-variable"

#define FORTH_MACHINE_SIZE (64 * 1024)		// 64K!
static byte *FORTH_MACHINE;
static byte *g_pFM, *g_pFM_HIGH;

#define COMPUTATION_STACK_SIZE 256
static byte *g_pCS, *g_pCS_LOW;			// COMPUTATION_STACK, grows *downward*

#define RETURN_STACK_SIZE 256
static byte *g_pRS, *g_pRS_LOW;			// RETURN_STACK, grows *downward*

#define BACKTRACE_STACK_SIZE 256
static byte *g_pBTS, *g_pBTS_LOW;		// BACKTRACE_STACK, grows *downward*

#define USER_VARIABLES_SIZE 512
static byte *g_pUV, *g_pUV_HIGH;		// USER VARIABLES

#define WORD_BUFFER_SIZE 256
static byte *g_pWB, *g_pWB_HIGH;		// WORD_BUFFER

#define TERMINAL_INPUT_BUFFER_SIZE 256
static byte *g_pTIB, *g_pTIB_HIGH;		// TERMINAL_INPUT_BUFFER

#define SCRATCH_PAD_SIZE 256
static byte *g_pSCP, *g_pSCP_HIGH;		// SCRATCH_PAD

#define LOCALS_DICT_SIZE 512
static byte *g_pLOCALSD, *g_pLOCALSD_HIGH;	// LOCALS_DICT

#define FORTH_DICTIONARY_SIZE (FORTH_MACHINE_SIZE - 6 * 1024)
static byte *g_pFD, *g_pFD_HIGH;		// FORTH_DICTIONARY

#define MASS_STORAGE_BLOCK_SIZE 1024
#define MASS_STORAGE_BUFFERS_SIZE (2 * MASS_STORAGE_BLOCK_SIZE)
static byte *g_pMSB, *g_pMSB_HIGH;		// MASS_STORAGE_BUFFERS

static PBYTE
   pSP			// SP:		Top of Computation Stack (grows *downward*)
  ,pRP			// RP:		Top of Return Stack (grows *downward*)
  ,pUP			// UP:		Bottom of User Variables
  ,pMSIB		// MSIB:	Bottom of Mass Storage Buffer
  ,pIBUF		// IBUF:	Bottom of Input Buffer (Terminal or Mass Storage)
  ,p0_plus_ORIGIN	// 0 +ORIGIN:	Bottom of boot-up Parameter Area
  ,pDP0			// DP0:		Bottom of Applications Dictionary
  ,pLOCALSDP0		// LOCALSDP0:	Bottom of Locals Dictionary
  ,pFIRST		// FIRST:	Bottom of Mass Storage Buffers
  ,pLIMIT		// LIMIT:	Top of Mass Storage Buffers
  ;

static PFORTHSTRING
  pPAD			// PAD:		Middle of Scratch Pad, as a FORTHSTRING
  ,pWBFR		// WBFR:	Bottom of Word Buffer, as a FORTHSTRING
  ;

static PFORTHDICTENT
  pLAST			// LAST:	Dictionary Entry of most recently defined word in CURRENT
  ,pLOCALSLAST		// LOCALSLAST:	Dictionary Entry of most recently defined word LOCALs Dictionary
  ;

static void
  *g_fspIN		// FSPIN:	Input Stream File/String Pointer (depending on value in user_var_BLK)
  ;

static FILE
  *g_fpOUT		// FPOUT:	Output Stream File Pointer
  ;

static PSINGLE
  pRPLOCALS;		// RPLOCALS:	(start of) local variables pointer

static SINGLE
  g_LOCALSINDEX;	// LOCALSINDEX:	index of last LOCAL variable (during Compilation)

static PADDR
  pBTP			// BTP:		Top of Backtrace Stack (grows *downward*), only used during Debugging
  ,pW			// W:		code field pointer
  ,pIP			// IP:		interpretive pointer
  ;

#pragma GCC diagnostic pop

static FORTHDICTNAMECMP dictcmp;	// Function for looking up names in the Dictionary (strncmp() or strncasecmp())

// array of FORTHDICTENTs which are currently being traced
#define MAX_TRACE_EXECUTE_DICTENTS 20
typedef struct tracevars
{
  bool debug;
  bool debug_suspend;
  USINGLE count_debug_dictents;
  ADDR debug_dictents[MAX_TRACE_EXECUTE_DICTENTS];
  bool trace;
  bool trace_suspend;
  bool trace_all;
  bool trace_colons;
  USINGLE count_trace_dictents;
  ADDR trace_dictents[MAX_TRACE_EXECUTE_DICTENTS];
} TRACEVARS, *PTRACEVARS;
PTRACEVARS g_pTRACE_VARS;

// *****************************************
// ********** </GLOBAL VARIABLES> **********
// *****************************************


// **************************************
// ********** <USER VARIABLES> **********
// **************************************

#define user_var_S0		(PADDR)(pUP + 0x00)
#define user_var_R0		(PADDR)(pUP + 0x02)
#define user_var_BT0		(PADDR)(pUP + 0x04)
#define user_var_TIB		(PADDR)(pUP + 0x06)
#define user_var_FENCE		(PADDR)(pUP + 0xA0)
#define user_var_DP		(PADDR)(pUP + 0xC0)
#define user_var_VOC_LINK	(PADDR)(pUP + 0xE0)
#define user_var_BLK		(PBYTE)(pUP + 0x10)
#define user_var_TO_IN		(POFFSET)(pUP + 0x12)
#define user_var_CONTEXT	(PADDR)(pUP + 0x1A)
#define user_var_CURRENT	(PADDR)(pUP + 0x1C)
#define user_var_STATE		(bool *)(pUP + 0x1E)
#define user_var_BASE		(PBYTE)(pUP + 0x20)
#define user_var_CSP		(PADDR)(pUP + 0x24)
#define user_var_LOCALSDP	(PADDR)(pUP + 0x26)
#define user_var_ERRNUM		(PADDR)(pUP + 0x28)
#define user_var_ERR_WARN	(PADDR)(pUP + 0x30)

#define USER_VAR_OFFSET(user_var) ((PBYTE)(user_var) - pUP)

// S0:		Bottom of Computation Stack
#define pS0 ((PBYTE)ADDR_TO_PTR(*user_var_S0))
// R0:		Bottom of Return Stack
#define pR0 ((PBYTE)ADDR_TO_PTR(*user_var_R0))
// BT0:		Bottom of Backtrace Stack
#define pBT0 ((PADDR)ADDR_TO_PTR(*user_var_BT0))
// TIB:		Bottom of Terminal Input Buffer
#define pTIB ((PBYTE)ADDR_TO_PTR(*user_var_TIB))
// FENCE:	Dictionary Entry below which not allowed to FORGET
#define pFENCE ((PFORTHDICTENT)ADDR_TO_PTR(*user_var_FENCE))
// DP:		First free byte at Top of Dictionary
#define pDP ((PBYTE)ADDR_TO_PTR(*user_var_DP))
// VOC-LINK:	Pointer to FORTHVOCAB inside Dictionary Entry of most recently defined Vocabulary
#define pVOC_LINK ((PFORTHVOCAB)ADDR_TO_PTR(*user_var_VOC_LINK))
// BLK:		Input Stream: 0 => keyboard, 255 => string, else => mass storage block
#define g_bBLK (*user_var_BLK)
// >IN:		Byte offset to present position in Input Buffer
#define g_ofstTO_IN (*user_var_TO_IN)
// CONTEXT:	Pointer-to-pointer-to (via VOCABULARY) Top-most Dictionary Entry for Searches
#define pCONTEXT ((PFORTHVOCAB)ADDR_TO_PTR(*user_var_CONTEXT))
// CURRENT:	Pointer-to-pointer-to (via VOCABULARY) Top-most Dictionary Entry for new Definitions
#define pCURRENT ((PFORTHVOCAB)ADDR_TO_PTR(*user_var_CURRENT))
// STATE:	0 => execution, 1 => compilation
#define g_bSTATE (*user_var_STATE)
// BASE:	2--16 numeric base for input/output
#define g_bBASE (*user_var_BASE)
// CSP:		Compiler Security pointer (holds pSP)
#define pCSP ((PBYTE)ADDR_TO_PTR(*user_var_CSP))
// LOCALSDP:	First free byte at Top of Locals Dictionary
#define pLOCALSDP ((PBYTE)ADDR_TO_PTR(*user_var_LOCALSDP))
// ERRNUM:	Last forth_ERROR() error number
#define g_bERRNUM (*user_var_ERRNUM)
// ERR_WARN:	0 => errors are errors, 1 => errors are warnings
#define g_bERR_WARN (*user_var_ERR_WARN)

// ***************************************
// ********** </USER VARIABLES> **********
// ***************************************


// *************************************
// ********** <ERROR NUMBERS> **********
// *************************************

typedef enum forth_err_num
{
  err_0,			/* 0 */
  err_STACK_EMPTY,		/* 1 */
  err_DICT_FULL,		/* 2 */
  err_BAD_ADDR_MODE,		/* 3 */
  err_NOT_UNIQUE,		/* 4 */
  err_BAD_PARAM,		/* 5 */
  err_BAD_SCR_NUM,		/* 6 */
  err_STACK_FULL,		/* 7 */
  err_BAD_FILE,			/* 8 */
  err_READ_WRITE,		/* 9 */
  err_REDEFINE_EOL,		/* 10 */
  err_DIV_0,			/* 11 */
  err_BAD_EXVEC,		/* 12 */
  err_BAD_BRANCH,		/* 13 */
  err_BAD_CURRENT_VOCAB,	/* 14 */
  err_15,			/* 15 */
  err_16,			/* 16 */
  err_COMPILATION_ONLY,		/* 17 */
  err_EXECUTION_ONLY,		/* 18 */
  err_COND_NOT_PAIRED,		/* 19 */
  err_DEF_INCOMPLETE,		/* 20 */
  err_PROTECTED_DICT,		/* 21 */
  err_LOAD_ONLY,		/* 22 */
  err_OFF_EDIT_SCR,		/* 23 */
  err_NOT_IN_CURRENT_VOCAB,	/* 24 */
  err_SYS_MEM,			/* 25 */
  err_NOT_IMPLEMENTED,		/* 26 */
  err_INTERRUPT,		/* 27 */
  err_DICT_NAME_EMPTY,		/* 28 */
  err_DICT_NAME_LEN,		/* 29 */
  err_ASSERT_FAIL,		/* 30 */
} FORTH_ERR_NUM;

const char *FORTH_ERR_MSG[] =
{
  "",						/* err_0, */
  "Stack Empty",				/* err_STACK_EMPTY, */
  "Dictionary Full",				/* err_DICT_FULL, */
  "Has Incorrect Address Mode Assembler!",	/* err_BAD_ADDR_MODE, */
  "Isn't unique",				/* err_NOT_UNIQUE, */
  "Parameter Outside Valid Range",		/* err_BAD_PARAM, */
  "Screen Number Out of Range",			/* err_BAD_SCR_NUM, */
  "Stack Full",					/* err_STACK_FULL, */
  "Can't Open or Extend File",			/* err_BAD_FILE, */
  "Read/Write not Completed",			/* err_READ_WRITE, */
  "Can't Redefine End-of-Line",			/* err_REDEFINE_EOL, */
  "Can't Divide by Zero",			/* err_DIV_0, */
  "Undefined Execution Vector",			/* err_BAD_EXVEC, */
  "Branch Too Long",				/* err_BAD_BRANCH, */
  "Incorrect CURRENT Vocabulary",		/* err_BAD_CURRENT_VOCAB, */
  "",						/* err_15, */
  "",						/* err_16, */
  "Compilation Only",				/* err_COMPILATION_ONLY, */
  "Execution Only",				/* err_EXECUTION_ONLY, */
  "Conditionals not Paired",			/* err_COND_NOT_PAIRED, */
  "Definition not Finished",			/* err_DEF_INCOMPLETE, */
  "In Protected Dictionary",			/* err_PROTECTED_DICT, */
  "Use Only When LOADing",			/* err_LOAD_ONLY, */
  "Off Current Editing Screen",			/* err_OFF_EDIT_SCR, */
  "Not in CURRENT Vocabulary",			/* err_NOT_IN_CURRENT_VOCAB, */
  "System Memory Clash",			/* err_SYS_MEM, */
  "Not Implemented",				/* err_NOT_IMPLEMENTED */
  "Interrupt",					/* err_INTERRUPT, */
  "Empty Definition Name",			/* err_DICT_NAME_EMPTY, */
  "Definition Name Too Long",			/* err_DICT_NAME_LEN, */
  "Assertion Failure",				/* err_ASSERT_FAIL, */
};

static inline const char *forth_ERR_MSG(FORTH_ERR_NUM err)
  { return (err >= 0 && err < ELEMENTS_COUNT(FORTH_ERR_MSG)) ? FORTH_ERR_MSG[err] : "?"; }
static const char *forth_ERR_FILE;
static SINGLE forth_ERR_LINE;
static void forth_SHOW_ERR_FILE_LINE(void)
{
  if (forth_ERR_FILE != NULL)
    fprintf(stderr, " [%s:%d]", forth_ERR_FILE, forth_ERR_LINE);
}

// **************************************
// ********** </ERROR NUMBERS> **********
// **************************************


// ********************************************
// ********** <FORWARD DECLARATIONS> **********
// ********************************************

static byte forthDictStoreForthStringSize(byte len);
static bool g_bExitOnError;
static void forth_QUIT(void);
static void forth_MSG_HASH(FORTH_ERR_NUM err);
static void forth_OS_ERRNO(void);
static void forth_OS_ERROR(void);
static void forth_ERROR(FORTH_ERR_NUM err);
static void forth_MESSAGE_WORD_NOT_FOUND(PFORTHSTRING pWord);
static void forth_ERROR_WORD_NOT_FOUND(PFORTHSTRING pWord);
static void forth_BRACKET_ABORT(void);
static void forth_WARM(void);
static void forth_INITVECS(void);
static void forth_COLD(void);
static void forth_EXECUTE_EXVEC(ADDR addr);
static void forth_DEBUG_EXECUTE_DICTENT(const PADDR pADDR, bool *pDebug_suspend_till_out);
static void forth_EXECUTE_DICTENT(const PFORTHDICTENT pDE);
static void forth_INTERPRET(void);
static void forth_STRING_INTERPRET(PFORTHSTRING pStr);
static bool forth_QUERY_LOAD_INTERPRET(const char *pFname);
static void forth_LOAD_INTERPRET(const char *pFname);

// *********************************************
// ********** </FORWARD DECLARATIONS> **********
// *********************************************


// ***************************************
// ********** <ERROR CHECKERS> **********
// ***************************************

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-function"

static inline bool VALID_PTR(PVOID p) { return ((PBYTE)p >= g_pFM && (PBYTE)p < g_pFM_HIGH); }
static inline bool VALID_ADDR(ADDR addr) { return (addr > ADDR_0 && addr < g_pFM_HIGH - g_pFM); }
static inline bool VALID_DICT_PTR(PVOID p) { return ((PBYTE)p >= g_pFD && (PBYTE)p < g_pFD_HIGH); }

static inline bool VALID_SP(void) { return (pSP > g_pCS_LOW && pSP <= g_pCS); }
static inline bool IS_EMPTY_SP(void) { return (pSP == g_pCS); }

static inline bool VALID_RP(void) { return (pRP > g_pRS_LOW && pRP <= g_pRS); }
static inline bool IS_EMPTY_RP(void) { return (pRP == g_pRS); }

static inline bool VALID_BTP(void) { return ((PBYTE)pBTP > g_pBTS_LOW && (PBYTE)pBTP <= g_pBTS); }
static inline bool IS_EMPTY_BTP(void) { return ((PBYTE)pBTP == g_pBTS); }

static inline void CHECK_PTR(PVOID p) { if (!VALID_PTR(p)) forth_ERROR(err_SYS_MEM); }
static inline void CHECK_ADDR(ADDR addr) { if (!VALID_ADDR(addr)) forth_ERROR(err_SYS_MEM); }
static inline void CHECK_DICT_PTR(PVOID p) { if (!VALID_DICT_PTR(p)) forth_ERROR(err_SYS_MEM); }
static inline void CHECK_CALLER(void) { if (!VALID_DICT_PTR(pIP)) forth_ERROR(err_SYS_MEM); }

static inline void CHECK_pSP_FULL(PBYTE pSP) { if (pSP <= g_pCS_LOW) forth_ERROR(err_STACK_FULL); }
static inline void CHECK_pSP_EMPTY(PBYTE pSP) { if (pSP > g_pCS) forth_ERROR(err_STACK_EMPTY); }
static inline void CHECK_SP(void) { CHECK_pSP_FULL(pSP); CHECK_pSP_EMPTY(pSP); }
static inline void CHECK_SP_HAS_1(void) { CHECK_pSP_EMPTY(pSP + sizeof(SINGLE)); }
static inline void CHECK_SP_HAS_2(void) { CHECK_pSP_EMPTY(pSP + sizeof(SINGLE) + sizeof(SINGLE)); }
static inline void CHECK_SP_HAS_3(void) { CHECK_pSP_EMPTY(pSP + sizeof(SINGLE) + sizeof(SINGLE) + sizeof(SINGLE)); }
static inline void CHECK_SP_HAS_N(byte n) { CHECK_pSP_EMPTY(pSP + n * sizeof(SINGLE)); }
static inline void CHECK_SP_HAS_DOUBLE(void) { CHECK_pSP_EMPTY(pSP + sizeof(DOUBLE)); }
static inline void CHECK_SP_HAS_FLOAT(void) { CHECK_pSP_EMPTY(pSP + sizeof(FLOAT)); }

static inline void CHECK_pRP_FULL(PBYTE pRP) { if (pRP <= g_pRS_LOW) forth_ERROR(err_STACK_FULL); }
static inline void CHECK_pRP_EMPTY(PBYTE pRP) { if (pRP > g_pRS) forth_ERROR(err_STACK_EMPTY); }
static inline void CHECK_RP(void) { CHECK_pRP_FULL(pRP); CHECK_pRP_EMPTY(pRP); }
static inline void CHECK_RP_HAS_1(void) { CHECK_pRP_EMPTY(pRP + sizeof(SINGLE)); }
static inline void CHECK_RP_HAS_2(void) { CHECK_pRP_EMPTY(pRP + sizeof(SINGLE) + sizeof(SINGLE)); }
static inline void CHECK_RP_HAS_N(byte n) { CHECK_pRP_EMPTY(pRP + n * sizeof(SINGLE)); }

static inline void CHECK_pRP_ALLOT_LOCAL(PBYTE pRP) { if (pRP > (PBYTE)pRPLOCALS) forth_ERROR(err_SYS_MEM); CHECK_pRP_FULL(pRP); }

static inline void CHECK_pBTP_FULL(PADDR pBTP) { if ((PBYTE)pBTP <= g_pBTS_LOW) forth_ERROR(err_STACK_FULL); }
static inline void CHECK_pBTP_EMPTY(PADDR pBTP) { if ((PBYTE)pBTP > g_pBTS) forth_ERROR(err_STACK_EMPTY); }
static inline void CHECK_BTP(void) { CHECK_pBTP_FULL(pBTP); CHECK_pBTP_EMPTY(pBTP); }
static inline void CHECK_BTP_HAS_1(void) { CHECK_pBTP_EMPTY(pBTP + 1); }

#pragma GCC diagnostic pop

#ifdef	FAST
//#define VALID_PTR(p)		((p) != NULL)
//#define VALID_ADDR(addr)	((addr) != 0)
//#define VALID_DICT_PTR(p)	((p) != NULL)
#define CHECK_PTR(p)
#define CHECK_ADDR(addr)
#define CHECK_DICT_PTR(p)
#define CHECK_CALLER()
#define CHECK_pSP_FULL(pSP)
#define CHECK_pSP_EMPTY(pSP)
#define CHECK_SP()
#define CHECK_SP_HAS_1()
#define CHECK_SP_HAS_2()
#define CHECK_SP_HAS_3()
#define CHECK_SP_HAS_N(n)
#define CHECK_SP_HAS_DOUBLE()
#define CHECK_SP_HAS_FLOAT()
#define CHECK_pRP_FULL(pRP)
#define CHECK_pRP_EMPTY(pRP)
#define CHECK_RP()
#define CHECK_RP_HAS_1()
#define CHECK_RP_HAS_2()
#define CHECK_RP_HAS_N(n)
#define CHECK_pRP_ALLOT_LOCAL(pRP)
#define CHECK_pBTP_FULL(pBTP)
#define CHECK_pBTP_EMPTY(pBTP)
#define CHECK_BTP()
#define CHECK_BTP_HAS_1()
#endif

// ****************************************
// ********** </ERROR CHECKERS> **********
// ****************************************


// ****************************************
// ********** <INLINE FUNCTIONS> **********
// ****************************************

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-function"

// convert a C pointer (any type) to an ADDR
static inline ADDR PTR_TO_ADDR(PVOID p) { if (p == NULL) return ADDR_0; CHECK_PTR(p); return (PBYTE)p - g_pFM; }
// convert an ADDR to a C pointer (any type)
static inline PVOID ADDR_TO_PTR(ADDR addr) { if (addr == ADDR_0) return NULL; CHECK_ADDR(addr); return g_pFM + addr; }

// clear the Return Stack (RS)
static inline void CLEAR_RETURN_STACK(void) { pRP = pR0; }
// clear the Computation Stack (SP)
static inline void CLEAR_COMPUTATION_STACK(void) { pSP = pS0; }
// clear the Backtrace Stack (BTP)
static inline void CLEAR_BACKTRACE_STACK(void) { pBTP = pBT0; }

// pointer to top-most SINGLE on Computation Stack (SP)
static inline PSINGLE SP_0(void) { return ((PSINGLE)pSP); }
// pointer to next-top-most SINGLE on Computation Stack (SP)
static inline PSINGLE SP_1(void) { return (((PSINGLE)pSP) + 1); }
// pointer to next-next-top-most SINGLE on Computation Stack (SP)
static inline PSINGLE SP_2(void) { return (((PSINGLE)pSP) + 2); }
// pointer to nth-top-most SINGLE on Computation Stack (SP)
static inline PSINGLE SP_N(BYTE n) { return (((PSINGLE)pSP) + (n - 1)); }
// pointer to top-most DOUBLE on Computation Stack (SP)
static inline PDOUBLE SP_DOUBLE_0(void) { return ((PDOUBLE)pSP); }
// pointer to top-most FLOAT on Computation Stack (SP)
static inline PFLOAT SP_FLOAT_0(void) { return ((PFLOAT)pSP); }

// pointer to top-most SINGLE on Return Stack (RP)
static inline PSINGLE RP_0(void) { return ((PSINGLE)pRP); }
// pointer to next-top-most SINGLE on Return Stack (RP)
static inline PSINGLE RP_1(void) { return (((PSINGLE)pRP) + 1); }
// pointer to nth-top-most SINGLE on Return Stack (RP)
static inline PSINGLE RP_N(BYTE n) { return (((PSINGLE)pRP) + (n - 1)); }

// pointer to top-most ADDR on Backtrace Stack (RP)
static inline PADDR BTP_0(void) { return pBTP; }
// pointer to next-top-most ADDR on Backtrace Stack (RP)
static inline PADDR BTP_1(void) { return pBTP + 1; }
// pointer to nth-top-most ADDR on Backtrace Stack (BTP)
static inline PADDR BTP_N(BYTE n) { return (pBTP + (n - 1)); }

// push a single precision to the Computation Stack (SP)
static inline void SP_PUSH(SINGLE sngl) { pSP -= sizeof(SINGLE); CHECK_pSP_FULL(pSP); *(PSINGLE)pSP = sngl; }
// pop a single precision from the Computation Stack (SP)
static inline SINGLE SP_POP(void) { pSP += sizeof(SINGLE); CHECK_pSP_EMPTY(pSP); return *(((PSINGLE)pSP) - 1); }
// replace a single precision at the Computation Stack (SP)
static inline void SP_REPLACE(SINGLE sngl) { CHECK_SP_HAS_1(); *(PSINGLE)pSP = sngl; }
// push a double precision to the Computation Stack (SP)
static inline void SP_PUSH_DOUBLE(DOUBLE dbl) { pSP -= sizeof(DOUBLE); CHECK_pSP_FULL(pSP); *(PDOUBLE)pSP = dbl; }
// pop a double precision from the Computation Stack (SP)
static inline DOUBLE SP_POP_DOUBLE(void) { pSP += sizeof(DOUBLE); CHECK_pSP_EMPTY(pSP); return *(((PDOUBLE)pSP) - 1); }
// replace a double precision at the Computation Stack (SP)
static inline void SP_REPLACE_DOUBLE(DOUBLE dbl) { CHECK_SP_HAS_2(); *(PDOUBLE)pSP = dbl; }
// push an addr to the Computation Stack (SP)
static inline void SP_PUSH_ADDR(ADDR addr) { pSP -= sizeof(ADDR); CHECK_pSP_FULL(pSP); *(PADDR)pSP = addr; }
// pop an addr from the Computation Stack (SP)
static inline ADDR SP_POP_ADDR(void) { pSP += sizeof(ADDR); CHECK_pSP_EMPTY(pSP); return *(((PADDR)pSP) - 1); }
// replace an addr at the Computation Stack (SP)
static inline void SP_REPLACE_ADDR(ADDR addr) { CHECK_SP_HAS_1(); *(PADDR)pSP = addr; }
// push a C pointer to the Computation Stack (SP)
static inline void SP_PUSH_PTR(PVOID p) { SP_PUSH_ADDR(PTR_TO_ADDR(p)); }
// pop a C pointer from the Computation Stack (SP)
static inline PVOID SP_POP_PTR(void) { return ADDR_TO_PTR(SP_POP_ADDR()); }
// replace a C pointer at the Computation Stack (SP)
static inline void SP_REPLACE_PTR(PVOID p) { SP_REPLACE_ADDR(PTR_TO_ADDR(p)); }
// push a float to the Computation Stack (SP)
static inline void SP_PUSH_FLOAT(FLOAT flt) { pSP -= sizeof(FLOAT); CHECK_pSP_FULL(pSP); *(PFLOAT)pSP = flt; }
// pop a float from the Computation Stack (SP)
static inline FLOAT SP_POP_FLOAT(void) { pSP += sizeof(FLOAT); CHECK_pSP_EMPTY(pSP); return *(((PFLOAT)pSP) - 1); }
// replace a float at the Computation Stack (SP)
static inline void SP_REPLACE_FLOAT(FLOAT flt) { CHECK_SP_HAS_2(); *(PFLOAT)pSP = flt; }
// push a float to the Computation Stack (SP)
static inline void SP_PUSH_SMALLFLOAT(SMALLFLOAT flt) { pSP -= sizeof(SMALLFLOAT); CHECK_pSP_FULL(pSP); *(PSMALLFLOAT)pSP = flt; }
// pop a float from the Computation Stack (SP)
static inline SMALLFLOAT SP_POP_SMALLFLOAT(void) { pSP += sizeof(SMALLFLOAT); CHECK_pSP_EMPTY(pSP); return *(((PSMALLFLOAT)pSP) - 1); }
// replace a float at the Computation Stack (SP)
static inline void SP_REPLACE_SMALLFLOAT(SMALLFLOAT smlflt) { CHECK_SP_HAS_2(); *(PSMALLFLOAT)pSP = smlflt; }

// push a single precision to the Return Stack (RP)
static inline void RP_PUSH(SINGLE sngl) { pRP -= sizeof(SINGLE); CHECK_RP(); *(PSINGLE)pRP = sngl; }
// pop a single precision from the Return Stack (RP)
static inline SINGLE RP_POP(void) { pRP += sizeof(SINGLE); CHECK_RP(); return *(((PSINGLE)pRP) - 1); }
// replace a single precision at the Return Stack (RP)
static inline void RP_REPLACE(SINGLE sngl) { CHECK_RP_HAS_1(); *(PSINGLE)pRP = sngl; }
// push a double precision to the Return Stack (RP)
static inline void RP_PUSH_DOUBLE(DOUBLE dbl) { pRP -= sizeof(DOUBLE); CHECK_RP(); *(PDOUBLE)pRP = dbl; }
// pop a double precision from the Return Stack (RP)
static inline DOUBLE RP_POP_DOUBLE(void) { pRP += sizeof(DOUBLE); CHECK_RP(); return *(((PDOUBLE)pRP) - 1); }
// replace a double precision at the Return Stack (RP)
static inline void RP_REPLACE_DOUBLE(DOUBLE dbl) { CHECK_RP_HAS_1(); *(PDOUBLE)pRP = dbl; }
// push a float precision to the Return Stack (RP)
static inline void RP_PUSH_FLOAT(FLOAT flt) { pRP -= sizeof(FLOAT); CHECK_RP(); *(PFLOAT)pRP = flt; }
// pop a float precision from the Return Stack (RP)
static inline FLOAT RP_POP_FLOAT(void) { pRP += sizeof(FLOAT); CHECK_RP(); return *(((PFLOAT)pRP) - 1); }
// replace a float precision at the Return Stack (RP)
static inline void RP_REPLACE_FLOAT(FLOAT flt) { CHECK_RP_HAS_1(); *(PFLOAT)pRP = flt; }

// allocate LOCAL variable storage on Return Stack (RP)
static inline void RP_ALLOT_LOCAL(PBYTE pRPNew) { CHECK_pRP_ALLOT_LOCAL(pRPNew); if (pRPNew < pRP) pRP = pRPNew; }
// allocate a single precision LOCAL variable at index on Return Stack (RP)
static inline void RP_ALLOT_LOCAL_VAR(SINGLE index) { RP_ALLOT_LOCAL((PBYTE)(pRPLOCALS - (index + 1))); }
// pointer to single precision LOCAL variable at index on Return Stack (RP)
static inline PSINGLE RP_LOCAL_VAR(SINGLE index) { CHECK_pRP_ALLOT_LOCAL(((PBYTE)(pRPLOCALS - (index + 1)))); return pRPLOCALS - (index + 1); }

// push an ADDR to the Backtrace Stack (BTP)
static inline void BTP_PUSH(ADDR addr) { pBTP--; CHECK_BTP(); *pBTP = addr; }
// pop an ADDR from the Backtrace Stack (BTP)
static inline ADDR BTP_POP(void) { pBTP++; CHECK_BTP(); return *(pBTP - 1); }
// depth (number of ADDRs) of the Backtrace Stack (BTP)
static inline ADDR BTP_DEPTH(void) { CHECK_BTP(); return (PADDR)g_pBTS - pBTP; }

// the FORTHDICTENT currently executing, i.e. "CALLER", pointed to by IP[-1]
static inline PFORTHDICTENT CALLER_DICTENT(void) { return (pIP == NULL) ? NULL : (PFORTHDICTENT)ADDR_TO_PTR(pIP[-1]); }
// pointer to the in-line ADDRs immediately following the current CALLER
static inline PADDR CALLER_INLINE_ADDRS(void) { return pIP; }
// advance current CALLER by n ADDRs
static inline void CALLER_ADVANCE(SINGLE n) { pIP += n; }
// advance current CALLER over an in-line FORTHSTRING
static inline void CALLER_ADVANCE_FORTHSTRING(PFORTHSTRING pStr) { pIP += forthDictStoreForthStringSize(pStr->len) / sizeof(ADDR); }
// move current CALLER such that it will next execute EXIT
static void CALLER_ADVANCE_TO_EXIT(void);

// find an ADDR value in an area of memory, cf. memchr()
static inline PADDR memafind(register PADDR pSrc, ADDR a, size_t n)
{
  while (n-- > 0)
  {
    if (*pSrc == a)
      return pSrc;
    pSrc++;
  }
  return NULL;
}

#pragma GCC diagnostic pop

// *****************************************
// ********** </INLINE FUNCTIONS> **********
// *****************************************


// ******************************
// ********** <MACROS> **********
// ******************************

// in Execution Mode
#define EXECUTION_MODE() (g_bSTATE == 0)
// in Compilation Mode
#define COMPILATION_MODE() (g_bSTATE != 0)
// set Execution Mode
#define SET_EXECUTION_MODE() (*user_var_STATE = 0)
// set Compilation Mode
#define SET_COMPILATION_MODE() (*user_var_STATE = 1)

// *******************************
// ********** </MACROS> **********
// *******************************


// *************************************
// ********** <LOW-LEVEL I/O> **********
// *************************************

static inline int forth_putc(char c)
{
  if (fputc(c, g_fpOUT) == EOF)
    return -1;
  return 1;
}

static inline int forth_putcount(byte count, const char *pChars)
{
  return fwrite(pChars, sizeof(char), count, g_fpOUT);
}

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-function"
static inline int forth_puts(const char *pChars)
{
  return forth_putcount(strlen(pChars), pChars);
}
#pragma GCC diagnostic pop

static int forth_oscommand(const char *pChars)
{
  int return_status = external_system(pChars);
  if (return_status == -1)
    forth_OS_ERROR();
  return return_status;
}

static int32_t forth_getpid(void)
{
  return external_getpid();
}

static bool g_bHaveForked = FALSE;
static int32_t forth_fork(void)
{
  g_bHaveForked = TRUE;
  return external_fork();
}

#ifdef	SEMAPHORE
static void forth_seminit(void)
{
  // create a single semaphore, initialise its value to 0
  if (external_seminit() == -1)
    forth_OS_ERROR();
}

static void forth_semacquire(void)
{
  if (external_semacquire() == -1)
    forth_OS_ERROR();
}

static void forth_semrelease(void)
{
  if (external_semrelease() == -1)
    forth_OS_ERROR();
}
#endif	// SEMAPHORE

// **************************************
// ********** </LOW-LEVEL I/O> **********
// **************************************


// ***********************************
// ********** <NUMERIC I/O> **********
// ***********************************

// CR
static void forth_CR(void)
{
  forth_putc('\n');
}

// NEXT_DIGIT
static char forth_NEXT_DIGIT(BYTE base, DOUBLE num, bool asUnsigned)
{
  // return the the least-significant "digit" of a number in the specified base
  if (base < 2 || base > 16)
  {
    forth_ERROR(err_BAD_PARAM);
    return '\0';
  }
  byte digit = asUnsigned ? ((UDOUBLE)num) % base : (num < 0 ? -num : num) % base;
  char c = (digit <= 9) ? '0' + digit : 'A' + digit - 10;
  return c;
}

// NUM_FORMAT
static PFORTHSTRING forth_NUM_FORMAT(DOUBLE num, bool asUnsigned, byte base, byte width, char padchar, bool trailingBlank)
{
  // format a number in the specified base to PAD
  // optionally padded with leading char to width, optionally with a trailing blank
  // precede it by a byte count, and return that address as a PFORTHSTRING
  if (pPAD == NULL)
    forth_ERROR(err_SYS_MEM);
  char *p = (char *)pPAD;
  *(--p) = '\0';
  if (trailingBlank)
    *(--p) = ' ';
  UDOUBLE unum = (UDOUBLE)num;
  bool minus = FALSE;
  if (!asUnsigned && num < 0)
    minus = TRUE;
  byte count = 0;
  do
  {
    char c = forth_NEXT_DIGIT(base, (asUnsigned ? unum : num), asUnsigned);
    *(--p) = c;
    if (asUnsigned)
      unum /= base;
    else
      num /= base;
    count++;
  } while ((asUnsigned ? unum : num) != 0);
  if (minus)
  {
    *(--p) = '-';
    count++;
  }
  while (count < width)
  {
    *(--p) = padchar;
    count++;
  }
  count = (char *)pPAD - 1 - p;
  PFORTHSTRING pStr = (PFORTHSTRING)(p - 1);
  pStr->len = count;
  return pStr;
}

// NUM_OUTPUT
static int forth_NUM_OUTPUT(DOUBLE num, bool asUnsigned, byte base, byte width, char padchar, bool trailingBlank)
{
  // output a number in the specified base, optionally with a trailing blank
  // return number of characters output
  PFORTHSTRING pStr = forth_NUM_FORMAT(num, asUnsigned, base, width, padchar, trailingBlank);
  return forth_putcount(pStr->len, pStr->chars);
}

// ADDR_OUTPUT
static int forth_ADDR_OUTPUT(ADDR addr, bool trailingBlank)
{
  // output an address, optionally with a trailing blank
  // return number of characters output
  int count = fprintf(g_fpOUT, "0x%04X", addr);
  if (trailingBlank)
  {
    forth_putc(' ');
    count++;
  }
  return count;
}

// FLOAT_OUTPUT
static int forth_FLOAT_OUTPUT(FLOAT num, bool trailingBlank)
{
  // output a floating point, optionally with a trailing blank
  // return number of characters output
  int count = fprintf(g_fpOUT, "%g", num);
  if (trailingBlank)
  {
    forth_putc(' ');
    count++;
  }
  return count;
}

// DIGIT
static int forth_DIGIT(BYTE base, register char c)
{
  // try to convert an input character "digit" in the specified base
  // return the digit as a number
  // -1 => invalid
  if (base < 2 || base > 16)
  {
    forth_ERROR(err_BAD_PARAM);
    return -1;
  }
  register int num = -1;
  if (c >= '0' && c <= '9')
    num = c - '0';
  else if ((c = toupper(c)) >= 'A' && c <= 'F')
    num = c - 'A' + 10;
  if (num < 0 || num >= base)
    return -1;
  return num;
}

// NUM
static bool forth_NUM(const PFORTHSTRING pWord)
{
  // try to convert an input word to a number, using the current BASE
  // leave the resulting number on the Computation Stack
  // * optional leading "-" (or "+")
  // * optional leading "0x" => hex (ignores BASE)
  // * float iff it has a trailing '.ddd' (always decimal, ignores BASE)
  // * double precision iff it has a trailing '.' with no further characters
  // * else single precision
  // return TRUE on success (*all* characters must be used up)
  // return FALSE on failure
  SINGLE singlePrec = 0;
  DOUBLE doublePrec = 0;
  FLOAT floatPrec = 0.0;
  
  byte len = pWord->len;
  register const char *chars = pWord->chars;
  register byte reached = 0;
  bool minus = FALSE, dot = FALSE, decimal = FALSE;
  BYTE base = g_bBASE;

  register char c = '\0';
  // skip leading whitespace
  while (reached < len && (isspace(c = chars[reached])))
    reached++;

  // note leading "-" (or "+") sign
  if (c == '-')
  {
    minus = TRUE;
    c = chars[++reached];
  }
  else if (c == '+')
    c = chars[++reached];

  if (c == '0' && reached < len && chars[reached + 1] == 'x')
  {
    // "0x" => hex
    base = 16;
    reached += 2;
    c = chars[reached];
  }
  else
  {
    // look ahead to see if it's a floating point, in which case we set base=10
    for (int reached2 = reached; reached2 + 1 < len && chars[reached2] != '\0'; reached2++)
      if (chars[reached2] == '.')
      {
        if (chars[reached2 + 1] >= '0' && chars[reached2 + 1] <= '9')
          base = 10;
        break;
      }
  }

  // need digits at this point
  if (reached >= len)
    return FALSE;

  // read digits
  int digit;
  while (reached < len && ((digit = forth_DIGIT(base, c)) >= 0))
  {
    doublePrec *= base;
    doublePrec += digit;
    if (doublePrec < 0) // signed overflow
      return FALSE;
    c = chars[++reached];
  }

  // deal with trailing "."
  if (reached < len && c == '.')
  {
    dot = TRUE;
    c = chars[++reached];
    if (reached < len && c >= '0' && c <= '9')
    {
      decimal = TRUE;
      int dpl = 1;
      floatPrec = doublePrec;
      while (reached < len && c >= '0' && c <= '9')
      {
	dpl *= 10;
	if (dpl < 0) // signed overflow
	  return FALSE;
	floatPrec += (c - '0') / (FLOAT)dpl;
	c = chars[++reached];
      }
    }
  }

  // check used up all characters
  if (reached != len)
    return FALSE;

  // deal with earlier leading "-" sign
  if (minus)
  {
    doublePrec = -doublePrec;
    floatPrec = -floatPrec;
  }

  if (decimal) // floating point
  {
    // push it to the stack
    SP_PUSH_FLOAT(floatPrec);
  }
  else if (dot) // double precision
  {
    // push it to the stack
    SP_PUSH_DOUBLE(doublePrec);
  }
  else // single precision
  {
    singlePrec = doublePrec;
    if (singlePrec != doublePrec && (doublePrec & 0xFFFF) != doublePrec) // overflow
      return FALSE;
    // push it to the stack
    SP_PUSH(singlePrec);
  }
  return TRUE;
}

// NUMBER
static void forth_NUMBER(const PFORTHSTRING pWord)
{
  // convert an input word to a number, using the current BASE
  // leave the resulting number on the Computation Stack
  // error if valid numeric conversion not possible
  if (!forth_NUM(pWord))
  {
    forth_ERROR(err_BAD_PARAM);
    return;
  }
}

// ************************************
// ********** </NUMERIC I/O> **********
// ************************************


// ******************************************
// ********** <DICTIONARY ENTRIES> **********
// ******************************************

// dump a memory area, in bytes
static void forthDumpMemBytes(ADDR addr, SINGLE num)
{
  PBYTE pMem = g_pFM + addr;
  while (num > 0)
  {
    fprintf(stdout, "0x%04X  ", PTR_TO_ADDR(pMem));
    for (int i = 0; i < 16 && i < num; i++)
      fprintf(stdout, "%02X ", pMem[i]);
    fputc(' ', stdout);
    for (int i = 0; i < 16 && i < num; i++)
    {
      char c = pMem[i];
      if (c <= ' ' || (unsigned char)c >= 0x80)
        c = '.';
      fprintf(stdout, "%c ", c);
    }
    fputc('\n', stdout);
    pMem += 16;
    num -= 16;
  }
}

// dump a memory area, in words
static void forthDumpMemWords(ADDR addr, SINGLE num)
{
  uint16_t *pMem = (uint16_t *)(g_pFM + addr);
  while (num > 0)
  {
    fprintf(stdout, "0x%04X  ", PTR_TO_ADDR(pMem));
    for (int i = 0; i < 16 && i < num; i++)
      fprintf(stdout, "%04X ", pMem[i]);
    fputc('\n', stdout);
    pMem += 16;
    num -= 16;
  }
}

// get a "safe" PNFA from a FORTHDICTENT
static PNFA SAFE_PNFA(const PFORTHDICTENT pDE)
{
  // this makes various checks to try to ensure the FORTHDICTENT has a valid NFA
  // else it return NULL
  // (so that at least it can be used in error messages)
  // make sure it is a valid pointer into the Dictionary
  if (!VALID_DICT_PTR(pDE))
    return NULL;
  PNFA pNFA = &pDE->nfa;
  // name cannot be 0 length
  if (pNFA->flag.len == 0)
    return NULL;
  // must be no '\0' inside name
  int i;
  for (i = 0; i < pNFA->flag.len; i++)
    if (pNFA->name[i] == '\0')
      return NULL;
  // must be extra '\0' at end
  if (pNFA->name[i] != '\0')
    return NULL;
  // it's safe!
  return pNFA;
}

// dump the content of a FORTHDICTENT
static void forthDumpDictEnt(const PFORTHDICTENT pDE)
{
  PNFA pNFA = SAFE_PNFA(pDE);
  if (pNFA != NULL)
  {
    fprintf(stderr, "NFA.name=%s", pNFA->name);
    if (pNFA->flag.smudge)
      fprintf(stderr,  "<INCOMPLETE>");
    if (pNFA->flag.immediate)
      fprintf(stderr, " <IMMEDIATE>");
    if (pNFA->flag.unused)
      fprintf(stderr, " <UNUSED BIT>");
  }
  else
    fprintf(stderr, "<BAD NFA>=%p,0x%02X", &pDE->nfa, *(PBYTE)&pDE->nfa.flag);
  fprintf(stderr, "\tLFA=0x%04X", pDE->lfa);
  fprintf(stderr, "\tCFA=");
  if (pDE->cfa >= 0 && pDE->cfa < sizeof(cfe_Names) / sizeof(cfe_Names[0]))
    fprintf(stderr, "%s", cfe_Names[pDE->cfa]);
  else
    fprintf(stderr, "%hd", pDE->cfa);
  fprintf(stderr, "\tPFA=0x%04X", PTR_TO_ADDR(&pDE->pfa));
  fprintf(stderr, "\n");
}


// return the FORTHDICTENT from where searches (of the *permanent* Dictionary) start (CONTEXT)
static PFORTHDICTENT forthContextDictEnt(void)
{
  if (pCONTEXT != NULL)
  {
    PFORTHDICTENT pDE = ADDR_TO_PTR(pCONTEXT->top_dictent);
    if (pDE != NULL)
    {
      CHECK_DICT_PTR(pDE);
      return pDE;
    }
  }
  // if no CONTEXT return LAST
  return pLAST;
}

// return the FORTHDICTENT from where searches (of the *compiler* Dictionary) start (LOCALS)
static PFORTHDICTENT forthStartSearchDictEnt(void)
{
  if (COMPILATION_MODE())
  {
    // if there are any LOCALs return the top LOCALs Dictionary Entry
    if (pLOCALSLAST != NULL)
      return pLOCALSLAST;
  }
  return forthContextDictEnt();
}

// return the FORTHDICTENT of the most recent definition in the CURRENT Vocabulary
static PFORTHDICTENT forthCurrentDictEnt(void)
{
  if (pCURRENT != NULL)
  {
    PFORTHDICTENT pDE = ADDR_TO_PTR(pCURRENT->top_dictent);
    if (pDE != NULL)
    {
      CHECK_DICT_PTR(pDE);
      return pDE;
    }
  }
  // if no CURRENT return LAST
  return pLAST;
}

// dump the contents of FORTH_DICTIONARY
static void forthDumpDictionary(void)
{
  // current top-most Dictionary Entry for Searches
  PFORTHDICTENT pDE = forthStartSearchDictEnt();
  // walk down FORTH_DICTIONARY
  while (pDE != NULL)
  {
    forthDumpDictEnt(pDE);
    pDE = DP_TO_PREV_DP(pDE);
  }
  fprintf(stderr, "\n");
}

// find starting from some FORTHDICTENT the FORTHDICTENT with NFA.name = pDictName
static PFORTHDICTENT forthFindDictEntFrom(PFORTHDICTENT pDE, const char *pDictName, int len)
{
  // walk down FORTH_DICTIONARY from pDE
  if (len < 0)
    len = strlen(pDictName);
  while (pDE != NULL)
  {
    if (!pDE->nfa.flag.smudge)	// if "smudge" bit is set, this is an incomplete definition, and is skipped
      if (pDE->nfa.flag.len == len &&
	dictcmp(pDE->nfa.name, pDictName, len) == 0)
      return pDE;
    pDE = DP_TO_PREV_DP(pDE);
  }
  return NULL;
}

// find the FORTHDICTENT with NFA.name = pDictName
static PFORTHDICTENT forthFindDictEnt(const PFORTHDICTNAME pDictName)
{
  // current top-most Dictionary Entry for Compiler Searches
  PFORTHDICTENT pDE = forthStartSearchDictEnt();
  // walk down FORTH_DICTIONARY
  return forthFindDictEntFrom(pDE, pDictName, -1);
}

// find the FORTHDICTENT with NFA.name = pWord
// raise error if not found
static PFORTHDICTENT forthMustFindDictEntForWord(PFORTHSTRING pWord)
{
  PFORTHDICTENT pDE = forthFindDictEnt(pWord->chars);
  if (pDE == NULL)
    forth_ERROR_WORD_NOT_FOUND(pWord);
  return pDE;
}

// return whether a FORTHDICTNAME is the dummy "blank" entry (first word in a Vocabulary)
static bool forthDictEntIsBlankName(const PFORTHDICTNAME pDictName)
{
  return (pDictName[0] == ' ' && pDictName[1] == '\0');
}

// return whether a FORTHDICTNAME is a Vocabulary
static bool forthDictEntIsVocabulary(PFORTHDICTENT pDE)
{
  // is it "FORTH"
  if (pDE->cfa == cfe_primitive && pDE->pfa.primitives[0] == primitive_FORTH)
    return TRUE;
  // is it a colon definition with "(VOCABULARY)" as its first word
  if (pDE->cfa == cfe_colon)
    if (pDE->pfa.addrs[0] != PFA_ADDR_EXIT)
    {
      PFORTHDICTENT pDEFirstWord = (PFORTHDICTENT)ADDR_TO_PTR(pDE->pfa.addrs[0]);
      CHECK_DICT_PTR(pDEFirstWord);
      if (pDEFirstWord->cfa == cfe_primitive && pDEFirstWord->pfa.primitives[0] == primitive_BRACKET_VOCABULARY)
        return TRUE;
    }
  return FALSE;
}

// find the FORTHDICTENT of the Vocabulary which a FORTHDICTENT is in
static PFORTHDICTENT forthFindDictEntVocabulary(PFORTHDICTENT pDE)
{
  CHECK_DICT_PTR(pDE);
  // walk down FORTH_DICTIONARY
  do
  {
    PFORTHDICTENT pDEPrev = pDE;
    pDE = DP_TO_PREV_DP(pDE);
    CHECK_DICT_PTR(pDE);
    // check if reached "FORTH"/a colon definition with "(VOCABULARY)" as its first word
    // where the previously-visited Dictionary Entry is the dummy "blank" first word in the Vocabulary
    if (forthDictEntIsVocabulary(pDE))
      if (forthDictEntIsBlankName(pDEPrev->nfa.name))
        return pDE;

  } while (TRUE);
}

// find the FORTHDICTENT for a primitive
static PFORTHDICTENT forthFindDictEntPrimitive(primitive_t prim)
{
  // current top-most Dictionary Entry for Searches
  PFORTHDICTENT pDE = forthCurrentDictEnt();
  // walk down FORTH_DICTIONARY
  while (pDE != NULL)
  {
    if (pDE->cfa == cfe_primitive && pDE->pfa.primitives[0] == prim)
      return pDE;
    pDE = DP_TO_PREV_DP(pDE);
  }
  // error if not found
  forth_ERROR(err_NOT_IMPLEMENTED);
  return NULL;
}

// set so that a FORTHDICTENT is the new top-most Dictionary Entry
// when *increasing* Dictionary (i.e. during a CREATE)
// sets pCONTEXT->top_dictent = pCURRENT->top_dictent = LAST = pDETop
static void forthIncreaseTopDictEnt(PFORTHDICTENT pDETop)
{
  CHECK_DICT_PTR(pDETop);
  if (pDETop <= pLAST)
  {
    forth_ERROR(err_BAD_PARAM);
    return;
  }
  // set pLAST
  pLAST = pDETop;
  // set pCURRENT
  if (pCURRENT != NULL)
    pCURRENT->top_dictent = PTR_TO_ADDR(pDETop);
  // set pCONTEXT
  if (pCONTEXT != NULL)
    pCONTEXT->top_dictent = PTR_TO_ADDR(pDETop);
}

// cut off any Vocabulary links which extend beyond the Forget point
// may set pCONTEXT = pCURRENT = pVOC_LINK = pVOC_LINK->voc_link
static void forthPruneDictEnt(PFORTHDICTENT pDEForget)
{
  // ensure it's a valid Dictionary Entry
  CHECK_DICT_PTR(pDEForget);
  // cannot PRUNE below FENCE
  if (pDEForget < pFENCE)
  {
    forth_ERROR(err_PROTECTED_DICT);
    return;
  }
  // walk down VOC-LINK while it is on or above pDEForget
  while ((PBYTE)pVOC_LINK >= (PBYTE)pDEForget)
    *user_var_VOC_LINK = pVOC_LINK->voc_link;
  // if CURRENT beyond VOC_LINK set CURRENT = VOC_LINK
  if (pCURRENT > pVOC_LINK)
    *user_var_CURRENT = PTR_TO_ADDR(pVOC_LINK);
  // if CONTEXT beyond VOC_LINK set CONTEXT = VOC_LINK
  if (pCONTEXT > pVOC_LINK)
    *user_var_CONTEXT = PTR_TO_ADDR(pVOC_LINK);
}

// set so that a FORTHDICTENT is the Forget-point
// when *reducing* Dictionary (i.e. during a FORGET)
// sets LAST = highest Dictionary Entry below pDEForget
// may set pCONTEXT->top_dictent = pCURRENT->top_dictent
static void forthDecreaseTopDictEnt(PFORTHDICTENT pDEForget)
{
  // ensure it's a valid Dictionary Entry
  CHECK_DICT_PTR(pDEForget);
  if (pDEForget > pLAST)
  {
    forth_ERROR(err_BAD_PARAM);
    return;
  }
  // walk down all Vocabularies via VOC-LINK
  // as any of them may have Dictionary Entries on or above pDEForget
  // while doing so, set pLAST to highest remaining Dictionary Entry encountered
  pLAST = NULL;
  for (PFORTHVOCAB pVocab = pVOC_LINK; pVocab != NULL; pVocab = ADDR_TO_PTR(pVocab->voc_link))
  {
    // walk down all Dictionary Entries in the Vocabulary
    // move past all those which are on or above pDEForget
    PFORTHDICTENT pDE = ADDR_TO_PTR(pVocab->top_dictent);
    while (pDE >= pDEForget)
      pDE = DP_TO_PREV_DP(pDE);
    // set Vocabulary's top-most Dictionary Entry
    pVocab->top_dictent = PTR_TO_ADDR(pDE);
    // set pLAST if this is the highest remaining Dictionary Entry so far encountered
    if (pDE > pLAST)
      pLAST = pDE;
  }
}

// FORGET everything above a FORTHDICTENT
static void forthForgetDictEnt(PFORTHDICTENT pDE)
{
  // ensure it's a valid Dictionary Entry
  CHECK_DICT_PTR(pDE);
  // cannot FORGET below FENCE
  if (pDE < pFENCE)
  {
    forth_ERROR(err_PROTECTED_DICT);
    return;
  }
  // first cut off any Vocabulary links which extend beyond this
  // may set pCONTEXT = pCURRENT = pVOC_LINK = pVOC_LINK->voc_link
  forthPruneDictEnt(pDE);
  // then scan remaining Vocabularies to move any top_dictents down to below this
  // also sets pLAST to highest remaining Dictionary Entry encountered
  forthDecreaseTopDictEnt(pDE);
  // move pDP to start of this FORTHDICTENT
  *user_var_DP = PTR_TO_ADDR(pDE);
  // set CURRENT = CONTEXT
  *user_var_CURRENT = *user_var_CONTEXT;
}

// drop the LAST FORTHDICTENT
static void forthForgetLastDictEnt(void)
{
  if (pLAST == NULL)
    forth_ERROR(err_SYS_MEM);
  // FORGET LAST Dictionary Entry
  forthForgetDictEnt(pLAST);
}

// raise an error if the LAST FORTHDICTENT *is* incomplete
static bool forthCheckCompleteLastDictEnt(void)
{
  if (pLAST == NULL)
    return TRUE;
  if (pLAST->nfa.flag.smudge)
  {
    forth_ERROR(err_DEF_INCOMPLETE);
    return FALSE;
  }
  return TRUE;
}

// raise an error if the LAST FORTHDICTENT is *not* incomplete
static bool forthCheckIncompleteLastDictEnt(void)
{
  if (pLAST == NULL)
  {
    forth_ERROR(err_SYS_MEM);
    return FALSE;
  }
  if (!pLAST->nfa.flag.smudge)
  {
    forth_ERROR(err_COMPILATION_ONLY);
    return FALSE;
  }
  return TRUE;
}

// allocate bytes at pLOCALSDP
static void forthLocalsDictAllot(SINGLE numBytes)
{
  // pLOCALSDP is current first free byte at top of LOCALs Dictionary
  PBYTE pLOCALSDPNew = pLOCALSDP + numBytes;
  if (pLOCALSDPNew >= g_pLOCALSD_HIGH)
  {
    forth_ERROR(err_DICT_FULL);
    return;
  }
  else if (pLOCALSDPNew < pLOCALSDP0)
  {
    forth_ERROR(err_SYS_MEM);
    return;
  }
  *user_var_LOCALSDP = PTR_TO_ADDR(pLOCALSDPNew);
}

// allocate bytes at pDP
static void forthDictAllot(SINGLE numBytes)
{
  // pDP is current first free byte at top of Dictionary
  PBYTE pDPNew = pDP + numBytes;
  if (pDPNew >= pFIRST)
  {
    forth_ERROR(err_DICT_FULL);
    return;
  }
  else if (pDPNew < pDP0)
  {
    forth_ERROR(err_SYS_MEM);
    return;
  }
  *user_var_DP = PTR_TO_ADDR(pDPNew);
}

// store an ADDR at pDP, allocating for it
static void forthDictStoreAddr(ADDR addr)
{
  // pDP is current first free byte at top of Dictionary
  PADDR pAddr = (PADDR)pDP;
  forthDictAllot(sizeof(addr));
  *pAddr = addr;
}

// store a PFORTHDICTENT (converted to an ADDR) at pDP, allocating for it
static void forthDictStoreDictEnt(PFORTHDICTENT pDictEnt)
{
  forthDictStoreAddr(PTR_TO_ADDR(pDictEnt));
}

// store the PFORTHDICTENT for a primitive at pDP, allocating for it
static void forthDictStoreFindPrimitive(primitive_t prim)
{
  // this has the effect of Compiling the primitive into the Dictionary
  PFORTHDICTENT pDE = forthFindDictEntPrimitive(prim);
  forthDictStoreDictEnt(pDE);
}

// store a SINGLE at pDP, allocating for it
static void forthDictStoreSingle(SINGLE value)
{
  // pDP is current first free byte at top of Dictionary
  PSINGLE pValue = (PSINGLE)pDP;
  forthDictAllot(sizeof(value));
  *pValue = value;
}

// store a BYTE at pDP, allocating for it
static void forthDictStoreByte(BYTE value)
{
  // pDP is current first free byte at top of Dictionary
  PSINGLE pValue = (PSINGLE)pDP;
  forthDictAllot(sizeof(value));
  *pValue = value;
}

// store a DOUBLE at pDP, allocating for it
static void forthDictStoreDouble(DOUBLE value)
{
  // pDP is current first free byte at top of Dictionary
  PDOUBLE pValue = (PDOUBLE)pDP;
  forthDictAllot(sizeof(value));
  *pValue = value;
}

// store a FLOAT at pDP, allocating for it
static void forthDictStoreFloat(FLOAT value)
{
  // pDP is current first free byte at top of Dictionary
  PFLOAT pValue = (PFLOAT)pDP;
  forthDictAllot(sizeof(value));
  *pValue = value;
}

// store a SMALLFLOAT at pDP, allocating for it
static void forthDictStoreSmallFloat(SMALLFLOAT value)
{
  // pDP is current first free byte at top of Dictionary
  PSMALLFLOAT pValue = (PSMALLFLOAT)pDP;
  forthDictAllot(sizeof(value));
  *pValue = value;
}

// calculate the size to allocate for storing a FORTHSTRING of length len in the Dictionary
static byte forthDictStoreForthStringSize(byte len)
{
  // when we store a FORTHSTRING in the Dictionary, we store it as:
  // <len><chars>'\0', followed by '\0's padding to align to to ADDR boundary (see definiton of IP and CALLER_...()) 
  byte bytes = sizeof(byte) + len + sizeof(char);
  bytes = (bytes + sizeof(ADDR) - 1) / sizeof(ADDR) * sizeof(ADDR);
  return bytes;
}

// store a FORTHSTRING at pDP, allocating for it
static void forthDictStoreForthString(byte len, const char *pChars)
{
  // pDP is current first free byte at top of Dictionary
  PFORTHSTRING pStr = (PFORTHSTRING)pDP;
  // calculate how many bytes to allocate for storing the string
  byte dictLen = forthDictStoreForthStringSize(len);
  forthDictAllot(dictLen);
  // store the FORTHSTRING
  pStr->len = len;
  memcpy(pStr->chars, pChars, len);
  // pad with trailing '\0's (there will be 1 at minimum) to align to ADDR boundary (see forthDictStoreForthStringSize())
  byte pad0s = dictLen - (sizeof(pStr->len) + len);
  memset(pStr->chars + len, '\0', pad0s);
}

// store a FORTHMAXLENSTRING at pDP, allocating for it
static void forthDictStoreForthMaxLenString(byte maxlen, byte len, const char *pChars)
{
  // pDP is current first free byte at top of Dictionary
  PFORTHMAXLENSTRING pMLStr = (PFORTHMAXLENSTRING)pDP;
  // calculate how many bytes to allocate for storing the maxlen string
  byte dictLen = forthDictStoreForthStringSize(sizeof(pMLStr->maxlen) + maxlen);
  forthDictAllot(dictLen);
  // store maxlen
  pMLStr->maxlen = maxlen;
  // store the FORTHSTRING
  PFORTHSTRING pStr = &pMLStr->string;
  pStr->len = len;
  memcpy(pStr->chars, pChars, len);
  // pad with trailing '\0's (there will be 1 at minimum) to align to ADDR boundary (see forthDictStoreForthStringSize())
  byte pad0s = dictLen - (sizeof(pStr->len) + len);
  memset(pStr->chars + len, '\0', pad0s);
}

// create new FORTHDICTENT at pDP
static void forthCreateDictEnt(const PFORTHDICTNAME pDictName, CFE cfe)
{
  // set CONTEXT = CURRENT as soon as a new definition is made
  *user_var_CONTEXT = *user_var_CURRENT;

  // current top-most Dictionary Entry for Definitions
  PFORTHDICTENT pDETop = forthCurrentDictEnt();
  // error if there is presently an incomplete Dictionary Entry
  if (!forthCheckCompleteLastDictEnt())
    return;

  // look at the name to be defined
  byte len = strlen(pDictName);
  if (len == 0)
  {
    forth_ERROR(err_DICT_NAME_EMPTY);
    return;
  }
  else if (len > MAX_DICT_NAME_LEN)
  {
    forth_ERROR(err_DICT_NAME_LEN);
    return;
  }

  // issue *warning* message if this redefines an existing word
  if (!forthDictEntIsBlankName(pDictName))
    if (forthFindDictEnt(pDictName) != NULL)
      forth_ERROR(err_NOT_UNIQUE);

  // pDP is current first free byte at top of Dictionary
  if (pDP + 256 >= pFIRST)
  {
    forth_ERROR(err_DICT_FULL);
    return;
  }
  else if (pDP < pDP0)
  {
    forth_ERROR(err_SYS_MEM);
    return;
  }
  // we like Dictionary Entry aligned to an ADDR boundary
  ADDR addr = PTR_TO_ADDR(pDP);
  OFFSET pad = ((addr + sizeof(ADDR) - 1) / sizeof(ADDR) * sizeof(ADDR)) - addr;
  if (pad != 0)
    forthDictAllot(pad);
  PFORTHDICTENT pDE = (PFORTHDICTENT)pDP;
  // move pDP up by sizeof(FORTHDICTENT)
  forthDictAllot(sizeof(FORTHDICTENT));

  // set NFA from pDictName
  pDE->nfa.flag.smudge = TRUE;		// set "smudge" => definition is incomplete
  pDE->nfa.flag.immediate = FALSE;	// clear "immediate" => compiles/executes depending on compilation state
  pDE->nfa.flag.unused = FALSE;		// clear "unused"
  pDE->nfa.flag.len = len;
  memcpy(pDE->nfa.name, pDictName, len);
  memset(pDE->nfa.name + len, '\0', sizeof(FORTHDICTNAME) - len);

  // set LFA to FORTHDICTENT's offset from g_pFM
  pDE->lfa = DP_TO_LFA(pDETop);

  // set CFA to CFE
  pDE->cfa = cfe;

  // set this as the top-most Dictionary Entry
  forthIncreaseTopDictEnt(pDE);
}

// create new FORTHDICTENT cfe_primitive
static void forthCreateDictEnt_primitive(const PFORTHDICTNAME pDictName, primitive_t prim, int flag)
{
  // create new Dictionary Entry, move pDP up to end
  forthCreateDictEnt(pDictName, cfe_primitive);
  // set IMMEDIATE Mode?
  if ((flag & DICT_IMMEDIATE))
    pLAST->nfa.flag.immediate = TRUE;

  // move pDP up by sizeof(primitive)
  forthDictAllot(sizeof(pLAST->pfa.primitives[0]));
  // store primitive_t at pDP->PFA
  pLAST->pfa.primitives[0] = prim;

  // clear "smudge" bit to mark definition as complete
  pLAST->nfa.flag.smudge = FALSE;
}

// create new FORTHDICTENT cfe_create
static void forthCreateDictEnt_create(const PFORTHDICTNAME pDictName)
{
  // create new Dictionary Entry, move pDP up to end
  forthCreateDictEnt(pDictName, cfe_create);

  // clear "smudge" bit to mark definition as complete
  pLAST->nfa.flag.smudge = FALSE;
}

// create new FORTHDICTENT cfe_constant
static void forthCreateDictEnt_constant(const PFORTHDICTNAME pDictName, SINGLE value)
{
  // create new Dictionary Entry, move pDP up to end
  forthCreateDictEnt(pDictName, cfe_constant);

  // store value into the Dictionary Entry
  forthDictStoreSingle(value);

  // clear "smudge" bit to mark definition as complete
  pLAST->nfa.flag.smudge = FALSE;
}

// create new FORTHDICTENT cfe_double_constant
static void forthCreateDictEnt_double_constant(const PFORTHDICTNAME pDictName, DOUBLE value)
{
  // create new Dictionary Entry, move pDP up to end
  forthCreateDictEnt(pDictName, cfe_double_constant);

  // store value into the Dictionary Entry
  forthDictStoreDouble(value);

  // clear "smudge" bit to mark definition as complete
  pLAST->nfa.flag.smudge = FALSE;
}

// create new FORTHDICTENT cfe_float_constant
static void forthCreateDictEnt_float_constant(const PFORTHDICTNAME pDictName, FLOAT value)
{
  // create new Dictionary Entry, move pDP up to end
  forthCreateDictEnt(pDictName, cfe_float_constant);

  // store value into the Dictionary Entry
  forthDictStoreFloat(value);

  // clear "smudge" bit to mark definition as complete
  pLAST->nfa.flag.smudge = FALSE;
}

// create new FORTHDICTENT cfe_string_constant
static void forthCreateDictEnt_string_constant(const PFORTHDICTNAME pDictName, byte len, const char *pChars)
{
  // create new Dictionary Entry, move pDP up to end
  forthCreateDictEnt(pDictName, cfe_string_constant);

  // store value into the Dictionary Entry
  forthDictStoreForthString(len, pChars);

  // clear "smudge" bit to mark definition as complete
  pLAST->nfa.flag.smudge = FALSE;
}

// create new FORTHDICTENT cfe_variable
static void forthCreateDictEnt_variable(const PFORTHDICTNAME pDictName, SINGLE value)
{
  // create new Dictionary Entry, move pDP up to end
  forthCreateDictEnt(pDictName, cfe_variable);

  // store value into the Dictionary Entry
  forthDictStoreSingle(value);

  // clear "smudge" bit to mark definition as complete
  pLAST->nfa.flag.smudge = FALSE;
}

// create new FORTHDICTENT cfe_double_variable
static void forthCreateDictEnt_double_variable(const PFORTHDICTNAME pDictName, DOUBLE value)
{
  // create new Dictionary Entry, move pDP up to end
  forthCreateDictEnt(pDictName, cfe_double_variable);

  // store value into the Dictionary Entry
  forthDictStoreDouble(value);

  // clear "smudge" bit to mark definition as complete
  pLAST->nfa.flag.smudge = FALSE;
}

// create new FORTHDICTENT cfe_float_variable
static void forthCreateDictEnt_float_variable(const PFORTHDICTNAME pDictName, FLOAT value)
{
  // create new Dictionary Entry, move pDP up to end
  forthCreateDictEnt(pDictName, cfe_float_variable);

  // store value into the Dictionary Entry
  forthDictStoreFloat(value);

  // clear "smudge" bit to mark definition as complete
  pLAST->nfa.flag.smudge = FALSE;
}

// create new FORTHDICTENT cfe_string_variable
static void forthCreateDictEnt_string_variable(const PFORTHDICTNAME pDictName, byte maxlen, byte len, const char *pChars)
{
  // create new Dictionary Entry, move pDP up to end
  forthCreateDictEnt(pDictName, cfe_string_variable);

  // store value into the Dictionary Entry
  forthDictStoreForthMaxLenString(maxlen, len, pChars);

  // clear "smudge" bit to mark definition as complete
  pLAST->nfa.flag.smudge = FALSE;
}

// create new FORTHDICTENT cfe_user_variable
static void forthCreateDictEnt_user_variable(const PFORTHDICTNAME pDictName, byte offset)
{
  // create new Dictionary Entry, move pDP up to end
  forthCreateDictEnt(pDictName, cfe_user_variable);

  // store offset into the Dictionary Entry
  forthDictStoreByte(offset);

  // clear "smudge" bit to mark definition as complete
  pLAST->nfa.flag.smudge = FALSE;
}

// create new FORTHDICTENT cfe_local_variable
static void forthCreateDictEnt_local_variable(const PFORTHDICTNAME pDictName)
{
  // Compilation mode only
  if (!COMPILATION_MODE())
  {
    forth_ERROR(err_COMPILATION_ONLY);
    return;
  }

  // create new LOCALs Dictionary Entry, move pLOCALSDP up to end, increment g_LOCALSINDEX

  // look at the name to be defined
  byte len = strlen(pDictName);
  if (len == 0)
  {
    forth_ERROR(err_DICT_NAME_EMPTY);
    return;
  }
  else if (len > MAX_DICT_NAME_LEN)
  {
    forth_ERROR(err_DICT_NAME_LEN);
    return;
  }

  // issue *warning* message if this redefines an existing word
  if (forthFindDictEnt(pDictName) != NULL)
    forth_ERROR(err_NOT_UNIQUE);

  // pLOCALSDP is current first free byte at top of LOCALs Dictionary
  if (pLOCALSDP + 50 >= g_pLOCALSD_HIGH)
  {
    forth_ERROR(err_DICT_FULL);
    return;
  }
  else if (pLOCALSDP < pLOCALSDP0)
  {
    forth_ERROR(err_SYS_MEM);
    return;
  }
  // we like Dictionary Entry aligned to an ADDR boundary
  ADDR addr = PTR_TO_ADDR(pLOCALSDP);
  OFFSET pad = ((addr + sizeof(ADDR) - 1) / sizeof(ADDR) * sizeof(ADDR)) - addr;
  if (pad != 0)
    forthLocalsDictAllot(pad);
  PFORTHDICTENT pDE = (PFORTHDICTENT)pLOCALSDP;
  // move pLOCALSDP up by sizeof(FORTHDICTENT)
  forthLocalsDictAllot(sizeof(FORTHDICTENT));

  // set NFA from pDictName
  pDE->nfa.flag.smudge = FALSE;		// clear "smudge" => definition is complete
  pDE->nfa.flag.immediate = TRUE;	// set "immediate" => always executes (only used in Compilation mode)
  pDE->nfa.flag.unused = FALSE;		// clear "unused"
  pDE->nfa.flag.len = len;
  memcpy(pDE->nfa.name, pDictName, len);
  memset(pDE->nfa.name + len, '\0', sizeof(FORTHDICTNAME) - len);

  // set LFA to FORTHDICTENT's offset from g_pFM
  pDE->lfa = DP_TO_LFA((pLOCALSLAST != NULL) ? pLOCALSLAST : pLAST);

  // set CFA to cfe_local_variable
  pDE->cfa = cfe_local_variable;

  // store index of local variable into Dictionary
  forthLocalsDictAllot(sizeof(SINGLE));
  pDE->pfa.singles[0] = g_LOCALSINDEX;
  // increment LOCALINDEX for variable created
  g_LOCALSINDEX += sizeof(SINGLE) / sizeof(SINGLE);

  // set this as the top-most LOCALs Dictionary Entry
  pLOCALSLAST = pDE;
}

// create new FORTHDICTENT cfe_colon
static void forthCreateDictEnt_colon(const PFORTHDICTNAME pDictName)
{
  // create new Dictionary Entry, move pDP up to end
  forthCreateDictEnt(pDictName, cfe_colon);

  // leave "smudge" bit set to mark definition as incomplete
}

// complete the new FORTHDICTENT cfe_colon
static void forthCompleteDictEnt_colon()
{
  // check that the LAST definition is indeed incomplete
  if (!forthCheckIncompleteLastDictEnt())
    return;
  // and that it is indeed a Colon or Does> Definition
  if (pLAST->cfa != cfe_colon && pLAST->cfa != cfe_does_gt)
  {
    forth_ERROR(err_COMPILATION_ONLY);
    return;
  }

  // compile into the definition the end-of-colon-list EXIT marker
  forthDictStoreAddr(PFA_ADDR_EXIT);

  // clear "smudge" bit to mark definition as complete
  pLAST->nfa.flag.smudge = FALSE;
}

// store a FORTHVOCAB inside pLAST, allocating for it
// (also create the dummy "blank" Dictionary Entry as the first word in the Vocabulary)
static void forthMakeDictEntNewVocab(void)
{
  if (pLAST == NULL)
    forth_ERROR(err_SYS_MEM);
  forthDictAllot(sizeof(FORTHVOCAB));
  PFORTHVOCAB pVOCAB = (PFORTHVOCAB)&pLAST->pfa.addrs[1];
  // store pointer to top-entry in Vocabulary (nothing at this point)
  pVOCAB->top_dictent = ADDR_0;
  // store pointer to the previous Vocabulary (via VOC-LINK)
  pVOCAB->voc_link = (pVOC_LINK != NULL) ? PTR_TO_ADDR(pVOC_LINK) : ADDR_0;
  // set VOC-LINK to the most recently defined Vocabulary
  *user_var_VOC_LINK = PTR_TO_ADDR(pVOCAB);

  // complete the FORTHDICTENT
  if (pLAST->cfa == cfe_colon)
    forthCompleteDictEnt_colon();

  // now create the dummy "blank" Dictionary Entry as the first word *in the new Vocabulary*
  ADDR savedContext = *user_var_CONTEXT;
  ADDR savedCurrent = *user_var_CURRENT;
  *user_var_CURRENT = PTR_TO_ADDR(pVOCAB);
  forthCreateDictEnt_colon(" ");
  forthCompleteDictEnt_colon();
  // store pointer to top-entry in Vocabulary (the dummy "blank" now)
  pVOCAB->top_dictent = PTR_TO_ADDR(pLAST);
  *user_var_CONTEXT = savedContext;
  *user_var_CURRENT = savedCurrent;
}

// create new FORTHDICTENT for an EXVEC: to do a primitive
static void forthCreateDictEnt_execvec(const PFORTHDICTNAME pDictName, primitive_t prim)
{
  // create new Dictionary Entry, move pDP up to end
  forthCreateDictEnt(pDictName, cfe_exvec);

  // store the primitive as the to-do into the Dictionary Entry
  forthDictStoreFindPrimitive(prim);

  // clear "smudge" bit to mark definition as complete
  pLAST->nfa.flag.smudge = FALSE;
}

// initialise a FORTHDICTENT for an EXVEC: to do a primitive
static void forth_INITVEC(const PFORTHDICTNAME pDictName, primitive_t prim)
{
  // find Dictionary Entry to ASSIGN
  PFORTHDICTENT pDEAssign = forthFindDictEnt(pDictName);
  // check it was created via EXVEC:
  if (!VALID_DICT_PTR(pDEAssign) || pDEAssign->cfa != cfe_exvec)
  {
    forth_ERROR(err_BAD_EXVEC);
    return;
  }
  // find Dictionary Entry TO-DO
  PFORTHDICTENT pDEToDo = forthFindDictEntPrimitive(prim);

  // set Dictionary Entry TO-DO Dictionary Entry
  pDEAssign->pfa.addrs[0] = PTR_TO_ADDR(pDEToDo);
}

// *******************************************
// ********** </DICTIONARY ENTRIES> **********
// *******************************************


// *********************************
// ********** <DEBUGGING> **********
// *********************************

// list Breakpoints
static void forthListBreakpoints(void)
{
  for (USINGLE count = 0; count < g_pTRACE_VARS->count_debug_dictents; count++)
  {
    PFORTHDICTENT pDE = ADDR_TO_PTR(g_pTRACE_VARS->debug_dictents[count]);
    PNFA pNFA = SAFE_PNFA(pDE);
    fprintf(stderr, "%hu: ", count);
    if (pNFA != NULL)
      fprintf(stderr, "%s", pNFA->name);
    else
      fprintf(stderr, "%04hX", PTR_TO_ADDR(pDE));
    fprintf(stderr, "\n");
  }
}

// return pointer to where an ADDR is among the Breakpoints
static inline PADDR forthFindBreakpoint(ADDR addr)
{
  return memafind(g_pTRACE_VARS->debug_dictents, addr, g_pTRACE_VARS->count_debug_dictents);
}

// return pointer to where an ADDR is among the Tracepoints
static inline PADDR forthFindTracepoint(ADDR addr)
{
  return memafind(g_pTRACE_VARS->trace_dictents, addr, g_pTRACE_VARS->count_trace_dictents);
}

// add an ADDR to the Breakpoints
static PADDR forthAddBreakpoint(ADDR addr)
{
  PADDR pADDRs = g_pTRACE_VARS->debug_dictents;
  USINGLE count = g_pTRACE_VARS->count_debug_dictents;
  PADDR pBreak = forthFindBreakpoint(addr);
  if (pBreak != NULL)
    return pBreak;
  if (count >= MAX_TRACE_EXECUTE_DICTENTS)
  {
    fprintf(stderr, "No more breakpoints");
    external_fflush(stderr);
    return NULL;
  }
  pADDRs[g_pTRACE_VARS->count_debug_dictents++] = addr;
  return pADDRs + count;
}

// add an ADDR to the Tracepoints
static PADDR forthAddTracepoint(ADDR addr)
{
  PADDR pADDRs = g_pTRACE_VARS->trace_dictents;
  USINGLE count = g_pTRACE_VARS->count_trace_dictents;
  PADDR pTrace = forthFindTracepoint(addr);
  if (pTrace != NULL)
    return pTrace;
  if (count >= MAX_TRACE_EXECUTE_DICTENTS)
  {
    fprintf(stderr, "No more tracepoints");
    external_fflush(stderr);
    return NULL;
  }
  pADDRs[g_pTRACE_VARS->count_trace_dictents++] = addr;
  return pADDRs + count;
}

// delete an ADDR from the Breakpoints
static void forthDeleteBreakpoint(ADDR addr)
{
  PADDR pADDRs = g_pTRACE_VARS->debug_dictents;
  USINGLE count = g_pTRACE_VARS->count_debug_dictents;
  PADDR pBreak = forthFindBreakpoint(addr);
  if (pBreak == NULL)
  {
    fprintf(stderr, "Breakpoint not found");
    external_fflush(stderr);
    return;
  }
  memmove(pBreak, pBreak + 1, (count - ELEMENT_INDEX(pADDRs, pBreak + 1)) * sizeof(FORTHDICTENT));
  pADDRs[--g_pTRACE_VARS->count_debug_dictents] = ADDR_0;
}

// delete an ADDR from the Tracepoints
static void forthDeleteTracepoint(ADDR addr)
{
  PADDR pADDRs = g_pTRACE_VARS->trace_dictents;
  USINGLE count = g_pTRACE_VARS->count_trace_dictents;
  PADDR pTrace = forthFindTracepoint(addr);
  if (pTrace == NULL)
  {
    fprintf(stderr, "Tracepoint not found");
    external_fflush(stderr);
    return;
  }
  memmove(pTrace, pTrace + 1, (count - ELEMENT_INDEX(pADDRs, pTrace + 1)) * sizeof(FORTHDICTENT));
  pADDRs[--g_pTRACE_VARS->count_trace_dictents] = ADDR_0;
}

// decompile one "instruction" in a cfe_colon/cfe_does_gt FORTHDICTENT
// move *ppADDR on over the instruction (including any extra inline data)
static void forthDecompileColonShowInstruction(PADDR *ppADDR, SINGLE colour)
{
  PADDR pADDR = *ppADDR;
  PFORTHDICTENT pDEWord = ADDR_TO_PTR(*pADDR);
  // check that we are pointing into the active Dictionary, at least
  CHECK_DICT_PTR(pDEWord);

#ifdef READLINE
  // if word to be highlighted, start highlighting now
  if (colour >= 0)
    readline_output_color(stdout, colour);
#endif

  // output that word's name
  PNFA pNFA = SAFE_PNFA(pDEWord);
  fprintf(stdout, "%s ", (pNFA != NULL) ? pNFA->name : "(?)");

  // move on to the next word to execute
  pADDR++;
  // special cases which have in-line data to (output and) skip over
  if (pDEWord->cfa == cfe_primitive)
    switch (pDEWord->pfa.primitives[0])
    {
    case primitive_LIT:
      // a SINGLE
      fprintf(stdout, "%hd ", *(PSINGLE)pADDR);
      pADDR += sizeof(SINGLE) / sizeof(ADDR);
      break;
    case primitive_D_LIT:
      // a DOUBLE
      fprintf(stdout, "%d ", *(PDOUBLE)pADDR);
      pADDR +=  sizeof(DOUBLE) / sizeof(ADDR);
      break;
    case primitive_F_LIT:
      // a FLOAT
      fprintf(stdout, "%g ", *(PFLOAT)pADDR);
      pADDR +=  sizeof(FLOAT) / sizeof(ADDR);
      break;
    case primitive_A_LIT:
    case primitive_BRACKET_TO:
      // an ADDR
      fprintf(stdout, "0x%04hX ", *pADDR);
      pADDR += sizeof(SINGLE) / sizeof(ADDR);
      break;
    case primitive_BRACKET_ASSERT:
    case primitive_BRACKET_LOCALVAR:
    case primitive_BRACKET_DECLARE_LOCALVAR:
      // a SINGLE which is the ASSERT line number
      // a SINGLE which is the local variable number
      fprintf(stdout, "#%hd ", *(PSINGLE)pADDR);
      pADDR += sizeof(SINGLE) / sizeof(ADDR);
      break;
    case primitive_BRACKET_QUOTE:
    case primitive_BRACKET_DOT_QUOTE:
    case primitive_BRACKET_QUERY_LOAD:
    case primitive_BRACKET_LOAD:
    case primitive_BRACKET_CLI:
    case primitive_BRACKET_INTERPRET_QUOTE:
      // a FORTHSTRING
    {
      PFORTHSTRING pStr = (PFORTHSTRING)pADDR;
      fwrite(pStr->chars, sizeof(char), pStr->len, stdout);
      pADDR += forthDictStoreForthStringSize(pStr->len) / sizeof(ADDR);
      fprintf(stdout, "\" ");
    }
      break;
    case primitive_COMPILE:
      // COMPILE is followed by the ADDR of a FORTHDICTENT
    {
      PFORTHDICTENT pDENext = ADDR_TO_PTR(*pADDR);
      // check that we are pointing into the active Dictionary, at least
      CHECK_DICT_PTR(pDENext);
      // output that word's name
      fprintf(stdout, "%s ", pDENext->nfa.name);
      pADDR++;
    }
      break;
    case primitive_BRANCH:
    case primitive_ZERO_BRANCH:
    case primitive_BRACKET_PLUS_LOOP:
    case primitive_BRACKET_LOOP:
    case primitive_BRACKET_U_LOOP:
      // BRANCH/0BRANCH/(+LOOP)/(LOOP)/(ULOOP) is followed by an OFFSET
      fprintf(stdout, "%hd ", *(POFFSET)pADDR);
      pADDR++;
      break;
    case primitive_BRACKET_VOCABULARY:
      // (VOCABULARY) is followed by a FORTHVOCAB
    {
      PFORTHVOCAB pVocab = (PFORTHVOCAB)pADDR;
      PFORTHDICTENT pDE = (PFORTHDICTENT)ADDR_TO_PTR(pVocab->top_dictent);
      if (pDE != NULL)
        CHECK_DICT_PTR(pDE);
      fprintf(stdout, "<top_dictent=0x%04hX (%s)", pVocab->top_dictent, (pDE != NULL) ? pDE->nfa.name : "nil");
      PFORTHVOCAB pPrevVocab = (PFORTHVOCAB)ADDR_TO_PTR(pVocab->voc_link);
      PFORTHDICTENT pPrevDE = (pPrevVocab != NULL) ? (PFORTHDICTENT)ADDR_TO_PTR(pPrevVocab->top_dictent) : NULL;
      if (pPrevDE != NULL)
        CHECK_DICT_PTR(pPrevDE);
      fprintf(stdout, ", voc_link=0x%04hX (%s)> ", pVocab->voc_link, (pPrevDE != NULL) ? pPrevDE->nfa.name : "nil");
      pADDR += sizeof(FORTHVOCAB) / sizeof(ADDR);
    }
      break;
    default:
      break;
    }
  *ppADDR = pADDR;

#ifdef READLINE
  // if word to be highlighted, end highlighting now
  if (colour >= 0)
    readline_output_color_white(stdout);
#endif

  external_fflush(stdout);
}

// decompile a FORTHDICTENT
static void forthDecompileDictEnt(PFORTHDICTENT pDE, PADDR pCurrentInstruction)
{
  // output the FORTH code to create (kind of!) the Dictonary Entry
  // if pCurrentInstruction != NULL it is the current instruction during Debugging
  // in which case want colour highlighting of instructions

  switch (pDE->cfa)
  {
  case cfe_primitive:
    fprintf(stdout, "%s: <primitive: %hd> ", pDE->nfa.name, pDE->pfa.primitives[0]);
    break;
  case cfe_create:
    fprintf(stdout, "CREATE %s", pDE->nfa.name);
    break;
  case cfe_constant:
    fprintf(stdout, "%hd CONSTANT %s", pDE->pfa.singles[0], pDE->nfa.name);
    break;
  case cfe_double_constant:
    fprintf(stdout, "%d DCONSTANT %s", pDE->pfa.doubles[0], pDE->nfa.name);
    break;
  case cfe_float_constant:
    fprintf(stdout, "%g FCONSTANT %s", pDE->pfa.floats[0], pDE->nfa.name);
    break;
  case cfe_string_constant:
    fprintf(stdout, "\" %s\" $CONSTANT %s", pDE->pfa.conststring[0].chars, pDE->nfa.name);
    break;
  case cfe_variable:
    fprintf(stdout, "VARIABLE %s %hd %s !", pDE->nfa.name, pDE->pfa.singles[0], pDE->nfa.name);
    break;
  case cfe_double_variable:
    fprintf(stdout, "DVARIABLE %s %d %s D!", pDE->nfa.name, pDE->pfa.doubles[0], pDE->nfa.name);
    break;
  case cfe_float_variable:
    fprintf(stdout, "FVARIABLE %s %g %s F!", pDE->nfa.name, pDE->pfa.floats[0], pDE->nfa.name);
    break;
  case cfe_string_variable:
    fprintf(stdout, "%d $VARIABLE %s \" %s\" %s $!", pDE->pfa.varstring[0].maxlen, pDE->nfa.name, pDE->pfa.varstring[0].string.chars, pDE->nfa.name);
    break;
  case cfe_user_variable:
    fprintf(stdout, "0x%02X USER %s", pDE->pfa.bytes[0], pDE->nfa.name);
    break;
  case cfe_local_variable:
    fprintf(stdout, "(LOCAL) %s ( #%hd )", pDE->nfa.name, pDE->pfa.singles[0]);
    break;
  case cfe_colon:
  case cfe_does_gt:
  {
    // the PFA.addrs[] array holds the series of words to execute, terminated by a PFA_ADDR_EXIT
    PADDR pADDR;
    if (pDE->cfa == cfe_colon)
    {
      fprintf(stdout, ": ");
      fprintf(stdout, "%s ", pDE->nfa.name);
      pADDR = &pDE->pfa.addrs[0];
    }
    else if (pDE->cfa == cfe_does_gt)
    {
      fprintf(stdout, "(DOES>) ");
      pADDR = ADDR_TO_PTR(pDE->pfa.addrs[0]);
    }
    else
    {
        forth_ERROR(err_NOT_IMPLEMENTED);
        return;
    }
    while (*pADDR != PFA_ADDR_EXIT)
    {
      SINGLE colour = -1;
      if (pCurrentInstruction != NULL)			// want instruction highlighting
      {
        if (pADDR == pCurrentInstruction)		// current instruction
          colour = 2;					// highlight in green
        else if (forthFindBreakpoint(*pADDR) != NULL)	// word has Breakpoint
          colour = 1;					// highlight in red
      }
      forthDecompileColonShowInstruction(&pADDR, colour);
    }
    fprintf(stdout, ";");
  }
    break;
  case cfe_exvec:
    fprintf(stdout, "EXVEC: %s ASSIGN %s TO-DO ", pDE->nfa.name, pDE->nfa.name);
  {
    PFORTHDICTENT pDEToDo = ADDR_TO_PTR(pDE->pfa.addrs[0]);
    // check that we are pointing into the active Dictionary, at least
    CHECK_DICT_PTR(pDEToDo);
    // output that word's name
    fprintf(stdout, "%s ", pDEToDo->nfa.name);
  }
    break;
  default:
    fprintf(stdout, "Unrecognised CFA: %hd", pDE->cfa);
    break;
  }
  if (pDE->nfa.flag.immediate)
    fprintf(stdout, " IMMEDIATE");
  fprintf(stdout, "\n");
}

#ifdef READLINE
// display the Backtrace Stack
static void forthDebugBacktrace(void)
{
  for (USINGLE level = BTP_DEPTH() / 2; level > 0; level--)
  {
    PFORTHDICTENT pDE = ADDR_TO_PTR(*BTP_N(level * 2));
    PADDR pCurrentInstruction = ADDR_TO_PTR(*BTP_N(level * 2 - 1));
#ifdef READLINE
    readline_output_color(stdout, 6);
#endif
    fprintf(stdout, "[%u] ", level - 1);
#ifdef READLINE
    readline_output_color_white(stdout);
#endif
    forthDecompileDictEnt(pDE, pCurrentInstruction);
  }
}
#endif

// **********************************
// ********** </DEBUGGING> **********
// **********************************


// ************************************
// ********** <INPUT BUFFER> **********
// ************************************

typedef struct forthfileinsaveposition
{
  BYTE bBLK;		// Input Stream: 0 => keyboard, 255 => string, else => mass storage block
  void *fsp;		// the FILE/string* (including its input buffer)
  const char *pFname;	// the file name
  int line;		// the line number reached
  PBYTE pBuf;		// the Input Buffer (allocated for file input)
  OFFSET ofstTO_IN;	// >IN, byte offset to present position in Input Buffer
} FORTHFILEINSAVEPOSITION, *PFORTHFILEINSAVEPOSITION;

static inline bool forth_FILE_IN_IS_STRING(void) { return (g_bBLK == 255); }
static inline bool forth_FILE_IN_IS_KEYBOARD(void) { return (g_bBLK == 0); }
static inline bool forth_FILE_IN_IS_FILE(void) { return (g_bBLK != 255 && g_bBLK != 0); }
static inline void forth_FILE_IN_SET_STRING(void) { *user_var_BLK = 255; }
static inline void forth_FILE_IN_SET_KEYBOARD(void) { *user_var_BLK = 0; }
static inline void forth_FILE_IN_SET_FILE(void) { *user_var_BLK = 1; }

// set so that input comes from string
static void forthSetStringInput(PFORTHSTRING pStr)
{
  if (pStr == NULL)
    forth_ERROR(err_SYS_MEM);
  // Input Stream is coming from a string
  forth_FILE_IN_SET_STRING();
  g_fspIN = pStr;
  // set Input Stream to read from g_fspIN
  pIBUF = (PBYTE)pStr->chars;
}

// set so that input comes from file
static void forthSetFileInput(FILE *pFile, PBYTE pBuf)
{
  if (pFile == NULL)
    forth_ERROR(err_SYS_MEM);
  // Input Stream is coming from a file
  forth_FILE_IN_SET_FILE();
  g_fspIN = pFile;
  // set Input Stream to read into pBuf
  if (pBuf == NULL)
    forth_ERROR(err_SYS_MEM);
  pIBUF = pBuf;
}

// set so that input comes from keyboard
static void forthSetKeyboardInput(void)
{
  if (stdin == NULL)
    forth_ERROR(err_SYS_MEM);
  // Input Stream is keyboard/Terminal Input Buffer
  forth_FILE_IN_SET_KEYBOARD();
  g_fspIN = stdin;
  // set Input Stream to read into pTIB
  if (pTIB == NULL)
    forth_ERROR(err_SYS_MEM);
  pIBUF = pTIB;
}

// update the current forth_ERR_FILE & forth_ERR_LINE
static inline void forth_SET_ERR_FILE_LINE(const char *pFname, int line)
{
  forth_ERR_FILE = pFname;
  forth_ERR_LINE = line;
}

// save the current Input Stream position
static void forth_FILE_IN_SAVE_POSITION(PFORTHFILEINSAVEPOSITION pSavePos)
{
  pSavePos->bBLK = g_bBLK;
  pSavePos->fsp = g_fspIN;
  pSavePos->pFname = forth_ERR_FILE;
  pSavePos->line = forth_ERR_LINE;
  pSavePos->pBuf = pIBUF;
  pSavePos->ofstTO_IN = g_ofstTO_IN;
}

// restore the saved Input Stream position
static void forth_FILE_IN_RESTORE_POSITION(PFORTHFILEINSAVEPOSITION pSavePos)
{
  *user_var_BLK = pSavePos->bBLK;
  if (forth_FILE_IN_IS_STRING())
    forthSetStringInput(pSavePos->fsp);
  else if (forth_FILE_IN_IS_FILE())
    forthSetFileInput(pSavePos->fsp, pSavePos->pBuf);
  else if (forth_FILE_IN_IS_KEYBOARD())
    forthSetKeyboardInput();
  forth_SET_ERR_FILE_LINE(pSavePos->pFname, pSavePos->line);
  *user_var_TO_IN = pSavePos->ofstTO_IN;
}

// FILE_IN_STRING
static void forth_FILE_IN_STRING(PFORTHSTRING pStr)
{
  // Input Stream is coming from a string
  forthSetStringInput(pStr);
  forth_SET_ERR_FILE_LINE(NULL, 0);
}

// FILE_IN_STDIN
static void forth_FILE_IN_STDIN(void)
{
  if (external_isatty(external_fileno(stdin)))
  {
    // Input Stream is keyboard/Terminal Input Buffer
    forthSetKeyboardInput();
  }
  else
  {
    // Input Stream is coming from a file
    forthSetFileInput(stdin, g_pMSB);
  }
  forth_SET_ERR_FILE_LINE(NULL, 0);
}

// FILE_IN_OPEN
static bool forth_FILE_IN_OPEN(const char *pFilePath)
{
  // open the Input Stream file
  // if successful, leaves the open File Pointer in g_fpIN and returns TRUE
  // else returns FALSE
  if (pFilePath == NULL)
    forth_ERROR(err_SYS_MEM);
  external_fflush(stdout);
  FILE *fp = external_fopen(pFilePath, "r");
  if (fp == NULL)
    return FALSE;
  // Input Stream is coming from a file
  PBYTE pBuf = external_malloc(MASS_STORAGE_BLOCK_SIZE);
  forthSetFileInput(fp, pBuf);
  forth_SET_ERR_FILE_LINE(external_strdup(pFilePath), 0);
  return TRUE;
}

// FILE_IN_CLOSE
static void forth_FILE_IN_CLOSE(void)
{
  // close the Input Stream file from file/string
  if (g_fspIN != NULL && g_fspIN != stdin)
  {
    external_fflush(stdout);
    if (forth_FILE_IN_IS_FILE())
    {
      external_fclose(g_fspIN);
      external_free(pIBUF);
      pIBUF = NULL;
    }
    g_fspIN = NULL;
    external_free((void *)forth_ERR_FILE);
    forth_SET_ERR_FILE_LINE(NULL, 0);
  }
  // restore Input Stream to stdin
  forth_FILE_IN_STDIN();
}

// chopNewline
static USINGLE chopNewline(char *buf)
{
  // chop off any trailing '\n' at the end of a buffer
  // return the length of the string
  int len = strlen(buf); 
  // chop off a terminating '\n'
  if (len > 0 && buf[len - 1] == '\n')
    buf[--len] = '\0';
  return len;
}

// STRING_QUERY
static SINGLE forth_STRING_QUERY(void)
{
  // input characters from string at/to pIBUF
  // return number of characters in string

  // initialise byte offset into Input Buffer from where next text will be accepted
  *user_var_TO_IN = 0;

  // input directly from g_fspIN
  if (g_fspIN == NULL || !forth_FILE_IN_IS_STRING())
    forth_ERROR(err_SYS_MEM);
  PFORTHSTRING pStr = (PFORTHSTRING)g_fspIN;
  if (pIBUF != (PBYTE)pStr->chars)
    forth_ERROR(err_SYS_MEM);

  // return number of characters in string
  return pStr->len;
}

// FILE_QUERY
static SINGLE forth_FILE_QUERY(void)
{
  // input characters from g_fpIN to Mass Storage Input Buffer/FORTHFILEINSAVEPOSITION.pBuf == pIBUF
  // return number of characters read (excluding terminating newline)
  // -1 => EOF

  // initialise byte offset into Input Buffer from where next text will be accepted
  *user_var_TO_IN = 0;

  // input from g_fspIN to pMSIB
  if (g_fspIN == NULL || !forth_FILE_IN_IS_FILE())
    forth_ERROR(err_SYS_MEM);
  if (pIBUF == NULL)
    forth_ERROR(err_SYS_MEM);
  // get one line into pIBUF
  char *in = fgets((char *)pIBUF, MASS_STORAGE_BLOCK_SIZE, g_fspIN);
  if (in == NULL) // EOF
    return -1;
  forth_ERR_LINE++;

  // chop off a terminating '\n', return number of characters read
  return chopNewline(in); 
}

#ifdef	READLINE
/* Generator function for Forth word completion.  STATE lets us know whether
to start from scratch; without any state (i.e. STATE == 0), then we
start at the top of the list. */
extern char *forth_word_generator(const char *text, int state)
{
  static PFORTHDICTENT *forth_dictents = NULL;
  static PFORTHDICTENT pDEPrevStart = NULL;
  static int list_index, len;
  /* If this is a new word to complete, initialize now.  This includes
  saving the length of TEXT for efficiency, and initializing the index
  variable to 0. */
  if (!state)
  {
    // current top-most Dictionary Entry for Searches
    PFORTHDICTENT pDE = forthStartSearchDictEnt();
    // if CURRENT differs from what it was last time, (re-)create the word list
    if (pDE != pDEPrevStart)
    {
      // note where we started the list from, for next time around
      pDEPrevStart = pDE;
      // (re-) create the list
      forth_dictents = readline_realloc(forth_dictents, 1 * sizeof(PFORTHDICTENT));
      // walk down FORTH_DICTIONARY
      int i = 0;
      while (pDE != NULL)
      {
        if (pDE->nfa.flag.smudge)	// if "smudge" bit is set, this is an incomplete definition, and is skipped
          continue;
        i++;
        forth_dictents = readline_realloc(forth_dictents, i * sizeof(PFORTHDICTENT));
        forth_dictents[i - 1] = pDE;
        pDE = DP_TO_PREV_DP(pDE);
      }
      // terminate list with a NULL
      forth_dictents = readline_realloc(forth_dictents, (i + 1) * sizeof(PFORTHDICTENT));
      forth_dictents[i] = NULL;
    }

    list_index = 0;
    len = strlen(text);
  }
  /* Return the next name which partially matches from the command list. */
  PFORTHDICTENT pDE;
  while ((pDE = forth_dictents[list_index]) != NULL)
  {
    list_index++;
    const char *name = pDE->nfa.name;
    if (dictcmp(name, text, len) == 0)
      return readline_word_generator_result(name);
  }
  /* If no names matched, then return NULL. */
  return NULL;
}
#endif	//READLINE

// KEYBOARD_QUERY
static SINGLE forth_KEYBOARD_QUERY(const char *prompt, bool addHistory)
{
  // input characters from keyboard to Terminal Input Buffer/pTIB == pIBUF
  // return number of characters read (excluding terminating newline)
  // -1 => EOF

  // initialise byte offset into Input Buffer from where next text will be accepted
  *user_var_TO_IN = 0;

  // set Input Stream to read into pTIB
  if (g_fspIN == NULL || !forth_FILE_IN_IS_KEYBOARD())
    forth_ERROR(err_SYS_MEM);
  if (pTIB == NULL || pIBUF != pTIB)
    forth_ERROR(err_SYS_MEM);

  external_fflush(stdout);
  external_fflush(stderr);

#ifdef	SEMAPHORE
  forth_semacquire();
#endif

  // get one line into TIB
  char *in = NULL;
#ifdef	READLINE
  initialise_readline();

  in = readline_getline(prompt, addHistory);
  if (in != NULL)
  {
    strncpy((char *)pTIB, in, TERMINAL_INPUT_BUFFER_SIZE - 1);
    pTIB[TERMINAL_INPUT_BUFFER_SIZE - 1] = '\0';
    in = (char *)pTIB;
  }
#else	// !READLINE
  if (prompt != NULL)
    fputs(prompt, stdout);
  external_fflush(stdout);
  in = fgets((char *)pTIB, TERMINAL_INPUT_BUFFER_SIZE, stdin);
#endif // READLINE

#ifdef	SEMAPHORE
  //TODO
  // Big problem here: if interrupted above (e.g. Ctrl+C => EINTR)
  // this code will not be reached and the forth_semacquire() will not be released
  forth_semrelease();
#endif

  if (in == NULL) // EOF
    return -1;

  // chop off a terminating '\n', return number of characters read
  return chopNewline(in); 
}

// QUERY
static SINGLE forth_QUERY(const char *prompt)
{
  // input characters from Input Stream to Input Buffer (pIBUF)
  // return number of characters read (excluding terminating newline)
  // -1 => EOF

  if (forth_FILE_IN_IS_STRING())
  {
    // input directly from string
    // gets just one line; should not be called again on same string
    return forth_STRING_QUERY();
  }
  else if (forth_FILE_IN_IS_FILE())
  {
    // input from Mass Storage Buffer
    return forth_FILE_QUERY();
  }
  else if (forth_FILE_IN_IS_KEYBOARD())
  {
    // input from Terminal Input Buffer
    // get just one line (so will later error if line is "incomplete")
    return forth_KEYBOARD_QUERY(prompt, TRUE);
  }
  else
    forth_ERROR(err_NOT_IMPLEMENTED);
  return -1;
}

// (SCAN)
static void forth_BRACKET_SCAN(char delimiter, POFFSET pOffset, PBYTE pCount)
{
  // scan the current line in the Input Stream for a "token"
  // if isspace(delimiter), skip leading whitespace
  // starts from pIBUF + g_ofstTO_IN, and *does* move g_ofstTO_IN on
  // set *offset to offset from pIBUF of first non-lead-delimiter
  // continue scanning till reach first trail-delimiter (pass '\0' to prevent this)
  // and set *count to length up to trail-delimiter (not included in count)
  // move over the trail-delimiter (just one)
  // if Input Stream is keyboard/Terminal Input Buffer this will *not* move onto the next line
  // if Input Stream is file/Mass Storage Buffer this will get next line till it has something to return

  bool isSpace = (isspace(delimiter) != 0);
  char c;
  do
  {
    // (re-)start from IBUF + >IN
    // skip leading delimiters if whitespace
    while ((c = pIBUF[g_ofstTO_IN]) != '\0' && isSpace && isspace(c))
      (*user_var_TO_IN)++;
    // if exhausted input and Input Stream is file/Mass Storage Buffer
    // get the next line into Input Buffer
    if (c == '\0' && forth_FILE_IN_IS_FILE())
    {
      // input characters from file to Mass Storage Input Buffer
      if (forth_FILE_QUERY() < 0) // EOF
      {
	// set as 0-length word, break out of loop
	pIBUF[g_ofstTO_IN] = '\0';
        break;
      }
      c = pIBUF[g_ofstTO_IN];
    }
    // repeat if we got nothing and Input Stream is file/Mass Storage Buffer
  } while ((c == '\0' || (isSpace ? isspace(c) : FALSE)) && forth_FILE_IN_IS_FILE());

  // *pOffset = offset from pIBUF of start of word
  *pOffset = g_ofstTO_IN;

  // find length till trailing delimiter
  byte len = 0;
  while ((c = pIBUF[g_ofstTO_IN]) != '\0' && !(isSpace ? isspace(c) : (c == delimiter)))
  {
    len++;
    (*user_var_TO_IN)++;
  }
  // *pCount = count of characters in word
  *pCount = len;
  // move over the trail-delimiter
  if (c != '\0')
    (*user_var_TO_IN)++;
}

// (WORD)
static void forth_BRACKET_WORD(char delimiter, POFFSET pOffset, PBYTE pCount)
{
  // scan the current line in the Input Stream for next word terminated by delimiter
  // if isspace(delimiter), skip leading delimiter character
  // starts from pIBUF + g_ofstTO_IN, and *does* move g_ofstTO_IN on
  // set *offset to offset from pIBUF of word and *count to length up to terminator

  forth_BRACKET_SCAN(delimiter, pOffset, pCount);
}

// >WBFR
static void forth_TO_WBFR(OFFSET offset, BYTE len)
{
  // copy a word of len characters from pIBUF + offset to pWBFR
  // store characters with leading count byte (FORTHSTRING)
  // and terminate it with a '\0'

  // set pWBFR->len to character count
  pWBFR->len = len;
  // copy word to pWBFR->chars, with terminator at end (not included in count)
  // the terminator will either be the delimiter, or '\0'
  memcpy(pWBFR->chars, pIBUF + offset, len + 1);
  // '\0' terminate string (which actually overwrites the terminator which was copied above...)
  pWBFR->chars[len] = '\0';
}

// WORD
static void forth_WORD(char delimiter)
{
  // accept characters from the Input Stream up to delimiter character
  // skip leading delimiter characters
  // store characters at pWBFR, with leading count byte (FORTHSTRING)

  // find the next word
  OFFSET offset;
  BYTE len;
  forth_BRACKET_WORD(delimiter, &offset, &len);

  // copy it to pWBFR as a FORTHSTRING, terminating with a '\0'
  forth_TO_WBFR(offset, len);
}

// *************************************
// ********** </INPUT BUFFER> **********
// *************************************

// ******************************************
// ********** <OUTPUT REDIRECTION> **********
// ******************************************

// set so that output goes to file
static void forthSetFileOutput(FILE *pFile)
{
  if (pFile == NULL)
    forth_ERROR(err_SYS_MEM);
  if (g_fpOUT != NULL && g_fpOUT != pFile)
  {
      if (g_fpOUT == stdout)
        external_fflush(g_fpOUT);
      else
        external_fclose(g_fpOUT);
  }
  // Output Stream is going to a file
  g_fpOUT = pFile;
}

// set so that output goes to console
static void forthSetConsoleOutput(void)
{
  if (stdout == NULL)
    forth_ERROR(err_SYS_MEM);
  if (g_fpOUT != NULL && g_fpOUT != stdout)
      external_fclose(g_fpOUT);
  // Output Stream is console
  g_fpOUT = stdout;
}

// flush any output going to file
static void forthFlushFileOutput()
{
  if (g_fpOUT != NULL)
    external_fflush(g_fpOUT);
}

// *******************************************
// ********** </OUTPUT REDIRECTION> **********
// *******************************************


// *****************************************
// ********** <BOOTUP DICTIONARY> **********
// *****************************************

// create the Bootup Dictionary
static void forthBootupDict(void)
{
  // Boot Up Parameters region (start of Forth Dictionary region: 0 +ORIGIN)
  p0_plus_ORIGIN = g_pFD;
  // start of Applications Dictionary region
  pDP0 = p0_plus_ORIGIN;
  // Dictionary Pointer (top of Applications Dictonary)
  *user_var_DP = PTR_TO_ADDR(pDP0);
  // Dictionary Entry of most recently defined word in CURRENT
  pLAST = NULL;
  // start of LOCALs Dictionary region
  pLOCALSDP0 = g_pLOCALSD;
  // LOCALs Dictionary Pointer (top of LOCALs Dictonary)
  *user_var_LOCALSDP = PTR_TO_ADDR(pLOCALSDP0);
  // VOC-LINK Pointer to Dictionary Entry of most recently defined Vocabulary
  *user_var_VOC_LINK = ADDR_0;
  // CONTEXT & CURRENT Dictionary Pointer-to-pointer (via a VOCABULARY) of Top entry in VOCABULARY
  *user_var_CONTEXT = *user_var_CURRENT = ADDR_0;
  // No FENCE => allowed to FORGET anything
  *user_var_FENCE = ADDR_0;

  // create bootup literals/nucleus dictionary

//STEP#2

  // create the FORTH VOCABULARY as first word in Dictionary
  forthCreateDictEnt_primitive("FORTH", primitive_FORTH, (DICT_IMMEDIATE));
  // set it as a new VOCABULARY (sets pVOC_LINK to the VOCAB inside the VOCABULARY word)
  // (also creates the dummy "blank" Dictionary Entry as the first word in the Vocabulary)
  forthMakeDictEntNewVocab();
  // do a FORTH (i.e. set CONTEXT)
  *user_var_CONTEXT = PTR_TO_ADDR(pVOC_LINK);
  // do a FORTH DEFINITIONS (i.e. set CURRENT)
  *user_var_CURRENT = *user_var_CONTEXT;

  forthCreateDictEnt_primitive("NOOP", primitive_NOOP, 0);

  // ++++++++++ Constant definitions
  forthCreateDictEnt_constant("0", 0);
  forthCreateDictEnt_constant("1", 1);
  forthCreateDictEnt_constant("2", 2);
  forthCreateDictEnt_constant("-1", -1);
  forthCreateDictEnt_constant("-2", -2);
  forthCreateDictEnt_constant("BL", ' ');
  forthCreateDictEnt_float_constant("PI", 3.14159265359);
  // "use" the remaining CreateDictEnt functions that have been defined
  VOIDFUNC func;
  func = (VOIDFUNC)forthCreateDictEnt_double_constant;
  func = (VOIDFUNC)forthCreateDictEnt_string_constant;
  func = (VOIDFUNC)forthCreateDictEnt_variable;
  func = (VOIDFUNC)forthCreateDictEnt_double_variable;
  func = (VOIDFUNC)forthCreateDictEnt_float_variable;
  func = (VOIDFUNC)forthCreateDictEnt_string_variable;
  VOIDFUNC UNUSED(func2) = func;
  // ---------- Constant definitions

  // ++++++++++ User Variables
  forthCreateDictEnt_user_variable("S0", USER_VAR_OFFSET(user_var_S0));
  forthCreateDictEnt_user_variable("R0", USER_VAR_OFFSET(user_var_R0));
  forthCreateDictEnt_user_variable("TIB", USER_VAR_OFFSET(user_var_TIB));
  forthCreateDictEnt_user_variable("FENCE", USER_VAR_OFFSET(user_var_FENCE));
  forthCreateDictEnt_user_variable("DP", USER_VAR_OFFSET(user_var_DP));
  forthCreateDictEnt_user_variable("VOC-LINK", USER_VAR_OFFSET(user_var_VOC_LINK));
  forthCreateDictEnt_user_variable("BLK", USER_VAR_OFFSET(user_var_BLK));
  forthCreateDictEnt_user_variable(">IN", USER_VAR_OFFSET(user_var_TO_IN));
  forthCreateDictEnt_user_variable("CONTEXT", USER_VAR_OFFSET(user_var_CONTEXT));
  forthCreateDictEnt_user_variable("CURRENT", USER_VAR_OFFSET(user_var_CURRENT));
  forthCreateDictEnt_user_variable("STATE", USER_VAR_OFFSET(user_var_STATE));
  forthCreateDictEnt_user_variable("BASE", USER_VAR_OFFSET(user_var_BASE));
  forthCreateDictEnt_user_variable("CSP", USER_VAR_OFFSET(user_var_CSP));
  forthCreateDictEnt_user_variable("ERR-WARN", USER_VAR_OFFSET(user_var_ERR_WARN));
  forthCreateDictEnt_user_variable("ERRNUM", USER_VAR_OFFSET(user_var_ERRNUM));
  // ---------- User Variables

  // ++++++++++ System Constants
  forthCreateDictEnt_primitive("FIRST", primitive_FIRST, 0);
  forthCreateDictEnt_primitive("LIMIT", primitive_LIMIT, 0);
  forthCreateDictEnt_primitive("PAD", primitive_PAD, 0);
  forthCreateDictEnt_primitive("WBFR", primitive_WBFR, 0);
  forthCreateDictEnt_primitive("WDSZ", primitive_WDSZ, 0);
  forthCreateDictEnt_primitive("+ORIGIN", primitive_PLUS_ORIGIN, 0);
  // ---------- System Constants

  // ++++++++++ Error definitions
  forthCreateDictEnt_primitive("QUIT", primitive_QUIT, 0);
  forthCreateDictEnt_primitive("MSG#", primitive_MSG_HASH, 0);
  forthCreateDictEnt_execvec("MESSAGE", primitive_MSG_HASH);
  forthCreateDictEnt_primitive("OSERRNO", primitive_OS_ERRNO, 0);
  forthCreateDictEnt_primitive("OSERROR", primitive_OS_ERROR, 0);
  forthCreateDictEnt_primitive("ERROR", primitive_ERROR, 0);
  forthCreateDictEnt_primitive("?ERROR", primitive_QUERY_ERROR, 0);
  forthCreateDictEnt_primitive("?STACK", primitive_QUERY_STACK, 0);
  forthCreateDictEnt_primitive("(ABORT)", primitive_BRACKET_ABORT, 0);
  forthCreateDictEnt_execvec("ABORT", primitive_BRACKET_ABORT);
  forthCreateDictEnt_primitive("WARM", primitive_WARM, 0);
  forthCreateDictEnt_primitive("INITVECS", primitive_INITVECS, 0);
  forthCreateDictEnt_primitive("COLD", primitive_COLD, 0);
  // ---------- Error definitions

  // ++++++++++ Compilation State definitions
  forthCreateDictEnt_primitive("?PAIRS", primitive_QUERY_PAIRS, 0);
  forthCreateDictEnt_primitive("?CSP", primitive_QUERY_CSP, 0);
  forthCreateDictEnt_primitive("?COMP", primitive_QUERY_COMP, 0);
  forthCreateDictEnt_primitive("?EXEC", primitive_QUERY_EXEC, 0);
  forthCreateDictEnt_primitive("(ASSERT)", primitive_BRACKET_ASSERT, 0);
  forthCreateDictEnt_primitive("ASSERT", primitive_ASSERT, (DICT_IMMEDIATE));
  // ---------- Compilation State definitions

  // ++++++++++ Computation Stack definitions
  forthCreateDictEnt_primitive("SP!", primitive_SP_STORE, 0);
  forthCreateDictEnt_primitive("SP@", primitive_SP_FETCH, 0);
  forthCreateDictEnt_primitive("DEPTH", primitive_DEPTH, 0);
  forthCreateDictEnt_primitive("DROP", primitive_DROP, 0);
  forthCreateDictEnt_primitive("DUP", primitive_DUP, 0);
  forthCreateDictEnt_primitive("?DUP", primitive_QUERY_DUP, 0);
  forthCreateDictEnt_primitive("OVER", primitive_OVER, 0);
  forthCreateDictEnt_primitive("ROT", primitive_ROT, 0);
  forthCreateDictEnt_primitive("SWAP", primitive_SWAP, 0);
  forthCreateDictEnt_primitive("PICK", primitive_PICK, 0);
  forthCreateDictEnt_primitive("ROLL", primitive_ROLL, 0);
  forthCreateDictEnt_primitive("2DROP", primitive_2_DROP, 0);
  forthCreateDictEnt_primitive("2DUP", primitive_2_DUP, 0);
  forthCreateDictEnt_primitive("2OVER", primitive_2_OVER, 0);
  forthCreateDictEnt_primitive("2SWAP", primitive_2_SWAP, 0);
  forthCreateDictEnt_primitive("FDROP", primitive_F_DROP, 0);
  forthCreateDictEnt_primitive("FDUP", primitive_F_DUP, 0);
  // ---------- Computation Stack definitions

  // ++++++++++ Return Stack definitions
  forthCreateDictEnt_primitive("RP!", primitive_RP_STORE, 0);
  forthCreateDictEnt_primitive("RP@", primitive_RP_FETCH, 0);
  forthCreateDictEnt_primitive(">R", primitive_TO_R, 0);
  forthCreateDictEnt_primitive("R>", primitive_R_FROM, 0);
  forthCreateDictEnt_primitive("R@", primitive_R_FETCH, 0);
  forthCreateDictEnt_primitive("D>R", primitive_D_TO_R, 0);
  forthCreateDictEnt_primitive("F>R", primitive_F_TO_R, 0);
  forthCreateDictEnt_primitive("R>D", primitive_R_FROM_D, 0);
  forthCreateDictEnt_primitive("R>F", primitive_R_FROM_F, 0);
  // ---------- Return Stack definitions

  // ++++++++++ Arithmetic definitions
  forthCreateDictEnt_primitive("*", primitive_TIMES, 0);
  forthCreateDictEnt_primitive("+", primitive_PLUS, 0);
  forthCreateDictEnt_primitive("+!", primitive_PLUS_STORE, 0);
  forthCreateDictEnt_primitive("1+!", primitive_1_PLUS_STORE, 0);
  forthCreateDictEnt_primitive("1-!", primitive_1_MINUS_STORE, 0);
  forthCreateDictEnt_primitive("-", primitive_SUBTRACT, 0);
  forthCreateDictEnt_primitive("/", primitive_DIVIDE, 0);
  forthCreateDictEnt_primitive("MOD", primitive_MOD, 0);
  forthCreateDictEnt_primitive("/MOD", primitive_DIVIDE_MOD, 0);
  forthCreateDictEnt_primitive("*/", primitive_TIMES_DIVIDE, 0);
  forthCreateDictEnt_primitive("*/MOD", primitive_TIMES_DIVIDE_MOD, 0);
  forthCreateDictEnt_primitive("NEGATE", primitive_NEGATE, 0);
  forthCreateDictEnt_primitive("ABS", primitive_ABS, 0);
  forthCreateDictEnt_primitive("+-", primitive_PLUS_MINUS, 0);
  forthCreateDictEnt_primitive("1+", primitive_1_PLUS, 0);
  forthCreateDictEnt_primitive("1-", primitive_1_MINUS, 0);
  forthCreateDictEnt_primitive("2+", primitive_2_PLUS, 0);
  forthCreateDictEnt_primitive("2-", primitive_2_MINUS, 0);
  forthCreateDictEnt_primitive("2*", primitive_2_TIMES, 0);
  forthCreateDictEnt_primitive("2/", primitive_2_DIVIDE, 0);
  forthCreateDictEnt_primitive("AND", primitive_AND, 0);
  forthCreateDictEnt_primitive("OR", primitive_OR, 0);
  forthCreateDictEnt_primitive("XOR", primitive_X_OR, 0);
  forthCreateDictEnt_primitive("TOGGLE", primitive_TOGGLE, 0);
  forthCreateDictEnt_primitive("0<", primitive_ZERO_LESS, 0);
  forthCreateDictEnt_primitive("0=", primitive_ZERO_EQUALS, 0);
  forthCreateDictEnt_primitive("NOT", primitive_NOT, 0);
  forthCreateDictEnt_primitive("NOT=", primitive_NOT_EQUALS, 0);
  forthCreateDictEnt_primitive("0>", primitive_ZERO_GREATER, 0);
  forthCreateDictEnt_primitive("<", primitive_LESS_THAN, 0);
  forthCreateDictEnt_primitive("=", primitive_EQUALS, 0);
  forthCreateDictEnt_primitive(">", primitive_GREATER_THAN, 0);
  forthCreateDictEnt_primitive("U<", primitive_U_LESS_THAN, 0);
  forthCreateDictEnt_primitive("U>", primitive_U_GREATER_THAN, 0);
  forthCreateDictEnt_primitive("MAX", primitive_MAX, 0);
  forthCreateDictEnt_primitive("MIN", primitive_MIN, 0);
  forthCreateDictEnt_primitive("U*", primitive_U_TIMES, 0);
  forthCreateDictEnt_primitive("S->D", primitive_S_TO_D, 0);
  forthCreateDictEnt_primitive("U->D", primitive_U_TO_D, 0);
  forthCreateDictEnt_primitive("D->S", primitive_D_TO_S, 0);
  forthCreateDictEnt_primitive("D->F", primitive_D_TO_F, 0);
  forthCreateDictEnt_primitive("D*", primitive_D_TIMES, 0);
  forthCreateDictEnt_primitive("D+", primitive_D_PLUS, 0);
  forthCreateDictEnt_primitive("D-", primitive_D_SUBTRACT, 0);
  forthCreateDictEnt_primitive("D/", primitive_D_DIVIDE, 0);
  forthCreateDictEnt_primitive("D/MOD", primitive_D_DIVIDE_MOD, 0);
  forthCreateDictEnt_primitive("DNEGATE", primitive_D_NEGATE, 0);
  forthCreateDictEnt_primitive("DABS", primitive_D_ABS, 0);
  forthCreateDictEnt_primitive("D+-", primitive_D_PLUS_MINUS, 0);
  forthCreateDictEnt_primitive("D<", primitive_D_LESS_THAN, 0);
  forthCreateDictEnt_primitive("D=", primitive_D_EQUALS, 0);
  forthCreateDictEnt_primitive("U/", primitive_U_DIVIDE, 0);
  forthCreateDictEnt_primitive("U/MOD", primitive_U_DIVIDE_MOD, 0);
  forthCreateDictEnt_primitive("F*", primitive_F_TIMES, 0);
  forthCreateDictEnt_primitive("F+", primitive_F_PLUS, 0);
  forthCreateDictEnt_primitive("F-", primitive_F_SUBTRACT, 0);
  forthCreateDictEnt_primitive("F/", primitive_F_DIVIDE, 0);
  forthCreateDictEnt_primitive("F->D", primitive_F_TO_D, 0);
  forthCreateDictEnt_primitive("F->SF", primitive_F_TO_SF, 0);
  forthCreateDictEnt_primitive("SF->F", primitive_SF_TO_F, 0);
  forthCreateDictEnt_primitive("F<", primitive_F_LESS_THAN, 0);
  forthCreateDictEnt_primitive("F=", primitive_F_EQUALS, 0);
  // ---------- Arithmetic definitions

  // ++++++++++ Literal definitions
  forthCreateDictEnt_primitive("LIT", primitive_LIT, 0);
  forthCreateDictEnt_primitive("LITERAL", primitive_LITERAL, (DICT_IMMEDIATE));
  forthCreateDictEnt_primitive("DLIT", primitive_D_LIT, 0);
  forthCreateDictEnt_primitive("DLITERAL", primitive_D_LITERAL, (DICT_IMMEDIATE));
  forthCreateDictEnt_primitive("FLIT", primitive_F_LIT, 0);
  forthCreateDictEnt_primitive("FLITERAL", primitive_F_LITERAL, (DICT_IMMEDIATE));
  forthCreateDictEnt_primitive("CLIT", primitive_C_LIT, 0);
  forthCreateDictEnt_primitive("CLITERAL", primitive_C_LITERAL, (DICT_IMMEDIATE));
  forthCreateDictEnt_primitive("ALIT", primitive_A_LIT, 0);
  forthCreateDictEnt_primitive("ALITERAL", primitive_A_LITERAL, (DICT_IMMEDIATE));
  forthCreateDictEnt_primitive("(\")", primitive_BRACKET_QUOTE, 0);
  forthCreateDictEnt_primitive("\"", primitive_QUOTE, (DICT_IMMEDIATE));
  forthCreateDictEnt_primitive("(.\")", primitive_BRACKET_DOT_QUOTE, 0);
  forthCreateDictEnt_primitive(".\"", primitive_DOT_QUOTE, (DICT_IMMEDIATE));
  forthCreateDictEnt_primitive("(", primitive_PAREN, (DICT_IMMEDIATE));
  forthCreateDictEnt_primitive("\\", primitive_BACKSLASH, (DICT_IMMEDIATE));
  // ---------- Literal definitions

  // ++++++++++ Store/Fetch definitions
  forthCreateDictEnt_primitive("!", primitive_STORE, 0);
  forthCreateDictEnt_primitive("D!", primitive_D_STORE, 0);
  forthCreateDictEnt_primitive("F!", primitive_F_STORE, 0);
  forthCreateDictEnt_primitive("SF!", primitive_SF_STORE, 0);
  forthCreateDictEnt_primitive("C!", primitive_C_STORE, 0);
  forthCreateDictEnt_primitive("C+!", primitive_C_PLUS_STORE, 0);
  forthCreateDictEnt_primitive("C1+!", primitive_C_1_PLUS_STORE, 0);
  forthCreateDictEnt_primitive("C1-!", primitive_C_1_MINUS_STORE, 0);
  forthCreateDictEnt_primitive("@", primitive_FETCH, 0);
  forthCreateDictEnt_primitive("D@", primitive_D_FETCH, 0);
  forthCreateDictEnt_primitive("F@", primitive_F_FETCH, 0);
  forthCreateDictEnt_primitive("SF@", primitive_SF_FETCH, 0);
  forthCreateDictEnt_primitive("C@", primitive_C_FETCH, 0);
  forthCreateDictEnt_primitive("CMOVE", primitive_C_MOVE, 0);
  forthCreateDictEnt_primitive("MOVE", primitive_MOVE, 0);
  forthCreateDictEnt_primitive("FILL", primitive_FILL, 0);
  forthCreateDictEnt_primitive("ERASE", primitive_ERASE, 0);
  forthCreateDictEnt_primitive("BLANKS", primitive_BLANKS, 0);
  forthCreateDictEnt_primitive("MEMCMP", primitive_MEMCMP, 0);
  forthCreateDictEnt_primitive("$CMP", primitive_DOLLAR_CMP, 0);
  forthCreateDictEnt_primitive("$CMPID", primitive_DOLLAR_CMPID, 0);
  forthCreateDictEnt_primitive("MEMDUMP", primitive_MEMDUMP, 0);
  forthCreateDictEnt_primitive("MEMDUMPW", primitive_MEMDUMP_W, 0);
  // ---------- Store/Fetch definitions

  // ++++++++++ String definitions
  forthCreateDictEnt_primitive("STRING", primitive_STRING, 0);
  forthCreateDictEnt_primitive("TEXT", primitive_TEXT, 0);
  forthCreateDictEnt_primitive("COUNT", primitive_COUNT, 0);
  forthCreateDictEnt_primitive("-TRAILING", primitive_DASH_TRAILING, 0);
  forthCreateDictEnt_primitive("TYPE", primitive_TYPE, 0);
  forthCreateDictEnt_primitive("($!)", primitive_BRACKET_DOLLAR_STORE, 0);
  forthCreateDictEnt_primitive("($+)", primitive_BRACKET_DOLLAR_PLUS, 0);
  forthCreateDictEnt_primitive("$!", primitive_DOLLAR_STORE, 0);
  forthCreateDictEnt_primitive("$+", primitive_DOLLAR_PLUS, 0);
  forthCreateDictEnt_primitive("$,", primitive_DOLLAR_COMMA, 0);
  // ---------- String definitions

  // ++++++++++ Output definitions
  forthCreateDictEnt_primitive("(EMIT)", primitive_BRACKET_EMIT, 0);
  forthCreateDictEnt_execvec("EMIT", primitive_BRACKET_EMIT);
  forthCreateDictEnt_primitive("(EMITS)", primitive_BRACKET_EMITS, 0);
  forthCreateDictEnt_execvec("EMITS", primitive_BRACKET_EMITS);
  forthCreateDictEnt_primitive("CR", primitive_CR, 0);
  forthCreateDictEnt_primitive("SPACE", primitive_SPACE, 0);
  forthCreateDictEnt_primitive("SPACES", primitive_SPACES, 0);
  forthCreateDictEnt_primitive("DECIMAL", primitive_DECIMAL, 0);
  forthCreateDictEnt_primitive("HEX", primitive_HEX, 0);
  forthCreateDictEnt_primitive("4HEX", primitive_4_HEX, 0);
  forthCreateDictEnt_primitive(".", primitive_DOT, 0);
  forthCreateDictEnt_primitive("U.", primitive_U_DOT, 0);
  forthCreateDictEnt_primitive("DEC.", primitive_DEC_DOT, 0);
  forthCreateDictEnt_primitive("H.", primitive_H_DOT, 0);
  forthCreateDictEnt_primitive("B.", primitive_B_DOT, 0);
  forthCreateDictEnt_primitive("A.", primitive_A_DOT, 0);
  forthCreateDictEnt_primitive("D.", primitive_D_DOT, 0);
  forthCreateDictEnt_primitive("UD.", primitive_U_D_DOT, 0);
  forthCreateDictEnt_primitive("F.", primitive_F_DOT, 0);
  forthCreateDictEnt_primitive("SF.", primitive_SF_DOT, 0);
  forthCreateDictEnt_primitive("DH.", primitive_D_H_DOT, 0);
  forthCreateDictEnt_primitive("C.", primitive_C_DOT, 0);
  forthCreateDictEnt_primitive("?", primitive_QUESTION_MARK, 0);
  forthCreateDictEnt_primitive("H?", primitive_H_QUESTION_MARK, 0);
  forthCreateDictEnt_primitive("A?", primitive_A_QUESTION_MARK, 0);
  forthCreateDictEnt_primitive("D?", primitive_D_QUESTION_MARK, 0);
  forthCreateDictEnt_primitive("F?", primitive_F_QUESTION_MARK, 0);
  forthCreateDictEnt_primitive("DH?", primitive_D_H_QUESTION_MARK, 0);
  forthCreateDictEnt_primitive("C?", primitive_C_QUESTION_MARK, 0);
  forthCreateDictEnt_primitive(".R", primitive_DOT_R, 0);
  forthCreateDictEnt_primitive("D.R", primitive_D_DOT_R, 0);
  forthCreateDictEnt_primitive(".S", primitive_DOT_S, 0);
  forthCreateDictEnt_primitive("H.S", primitive_H_DOT_S, 0);
  forthCreateDictEnt_primitive(".RS", primitive_DOT_RS, 0);
  forthCreateDictEnt_primitive("H.RS", primitive_H_DOT_RS, 0);
  // ---------- Output definitions

  // ++++++++++ Input definitions
  forthCreateDictEnt_primitive("(NUM)", primitive_BRACKET_NUM, 0);
  forthCreateDictEnt_execvec("NUM", primitive_BRACKET_NUM);
  forthCreateDictEnt_primitive("DIGIT", primitive_DIGIT, 0);
  forthCreateDictEnt_primitive("NUMBER", primitive_NUMBER, 0);
  forthCreateDictEnt_primitive("(KEY)", primitive_BRACKET_KEY, 0);
  forthCreateDictEnt_execvec("KEY", primitive_BRACKET_KEY);
  forthCreateDictEnt_primitive("(WORD)", primitive_BRACKET_WORD, 0);
  forthCreateDictEnt_primitive("WORD", primitive_WORD, 0);
  forthCreateDictEnt_primitive("QUERY", primitive_QUERY, 0);
  // ---------- Input definitions

  // ++++++++++ Conditional/Loop definitions
  forthCreateDictEnt_primitive("[THEN]", primitive_BRACKET_THEN, (DICT_IMMEDIATE));
  forthCreateDictEnt_primitive("[ELSE]", primitive_BRACKET_ELSE, (DICT_IMMEDIATE));
  forthCreateDictEnt_primitive("[IF]", primitive_BRACKET_IF, (DICT_IMMEDIATE));
  forthCreateDictEnt_primitive("(JUMP)", primitive_BRACKET_JUMP, 0);
  forthCreateDictEnt_primitive("BACK", primitive_BACK, 0);
  forthCreateDictEnt_primitive("BRANCH", primitive_BRANCH, 0);
  forthCreateDictEnt_primitive("0BRANCH", primitive_ZERO_BRANCH, 0);
  forthCreateDictEnt_primitive("IF", primitive_IF, (DICT_IMMEDIATE));
  forthCreateDictEnt_primitive("ELSE", primitive_ELSE, (DICT_IMMEDIATE));
  forthCreateDictEnt_primitive("THEN", primitive_THEN, (DICT_IMMEDIATE));
  forthCreateDictEnt_primitive("BEGIN", primitive_BEGIN, (DICT_IMMEDIATE));
  forthCreateDictEnt_primitive("AGAIN", primitive_AGAIN, (DICT_IMMEDIATE));
  forthCreateDictEnt_primitive("UNTIL", primitive_UNTIL, (DICT_IMMEDIATE));
  forthCreateDictEnt_primitive("WHILE", primitive_WHILE, (DICT_IMMEDIATE));
  forthCreateDictEnt_primitive("REPEAT", primitive_REPEAT, (DICT_IMMEDIATE));
  forthCreateDictEnt_primitive("I", primitive_I, 0);
  forthCreateDictEnt_primitive("J", primitive_J, 0);
  forthCreateDictEnt_primitive("(DO)", primitive_BRACKET_DO, 0);
  forthCreateDictEnt_primitive("(+LOOP)", primitive_BRACKET_PLUS_LOOP, 0);
  forthCreateDictEnt_primitive("(LOOP)", primitive_BRACKET_LOOP, 0);
  forthCreateDictEnt_primitive("(ULOOP)", primitive_BRACKET_U_LOOP, 0);
  forthCreateDictEnt_primitive("DO", primitive_DO, (DICT_IMMEDIATE));
  forthCreateDictEnt_primitive("LEAVE", primitive_LEAVE, 0);
  forthCreateDictEnt_primitive("+LOOP", primitive_PLUS_LOOP, (DICT_IMMEDIATE));
  forthCreateDictEnt_primitive("LOOP", primitive_LOOP, (DICT_IMMEDIATE));
  forthCreateDictEnt_primitive("ULOOP", primitive_U_LOOP, (DICT_IMMEDIATE));
  // ---------- Conditional/Loop definitions

  // ++++++++++ Dictionary definitions
  forthCreateDictEnt_primitive("CFA", primitive_CFA, 0);
  forthCreateDictEnt_primitive("LFA", primitive_LFA, 0);
  forthCreateDictEnt_primitive("NFA", primitive_NFA, 0);
  forthCreateDictEnt_primitive("PFA", primitive_PFA, 0);
  forthCreateDictEnt_primitive("SMUDGE", primitive_SMUDGE, 0);
  forthCreateDictEnt_primitive("LAST", primitive_LAST, 0);
  forthCreateDictEnt_primitive("DEFINITIONS", primitive_DEFINITIONS, 0);
  forthCreateDictEnt_primitive("IMMEDIATE", primitive_IMMEDIATE, 0);
  forthCreateDictEnt_primitive("(VOCABULARY)", primitive_BRACKET_VOCABULARY, 0);
  forthCreateDictEnt_primitive("VOCABULARY", primitive_VOCABULARY, 0);
  forthCreateDictEnt_primitive(",", primitive_COMMA, 0);
  forthCreateDictEnt_primitive("D,", primitive_D_COMMA, 0);
  forthCreateDictEnt_primitive("F,", primitive_F_COMMA, 0);
  forthCreateDictEnt_primitive("SF,", primitive_SF_COMMA, 0);
  forthCreateDictEnt_primitive("C,", primitive_C_COMMA, 0);
  forthCreateDictEnt_primitive("'", primitive_TICK, (DICT_IMMEDIATE));
  forthCreateDictEnt_primitive("DICTDUMP", primitive_DICTDUMP, 0);
  forthCreateDictEnt_primitive("VDUMP", primitive_VDUMP, 0);
  forthCreateDictEnt_primitive("ID.", primitive_ID_DOT, 0);
  forthCreateDictEnt_primitive("VLIST", primitive_VLIST, 0);
  forthCreateDictEnt_primitive("(FINDPREV)", primitive_BRACKET_FIND_PREV, 0);
  forthCreateDictEnt_primitive("(FIND)", primitive_BRACKET_FIND, 0);
  forthCreateDictEnt_primitive("-FIND", primitive_DASH_FIND, 0);
  forthCreateDictEnt_primitive("FIND", primitive_FIND, 0);
  forthCreateDictEnt_primitive("FIND-VOCABULARY", primitive_FIND_VOCABULARY, 0);
  forthCreateDictEnt_primitive("PRUNE", primitive_PRUNE, 0);
  forthCreateDictEnt_primitive("(FORGET)", primitive_BRACKET_FORGET, 0);
  forthCreateDictEnt_primitive("-FORGET", primitive_DASH_FORGET, 0);
  forthCreateDictEnt_primitive("FORGET", primitive_FORGET, 0);
  forthCreateDictEnt_primitive("HERE", primitive_HERE, 0);
  forthCreateDictEnt_primitive("ALLOT", primitive_ALLOT, 0);
  forthCreateDictEnt_primitive("COMPILE", primitive_COMPILE, 0);
  forthCreateDictEnt_primitive("(CREATE)", primitive_BRACKET_CREATE, 0);
  forthCreateDictEnt_execvec("CREATE", primitive_BRACKET_CREATE);
  forthCreateDictEnt_primitive("<BUILDS", primitive_LESS_BUILDS, 0);
  forthCreateDictEnt_primitive("DOES>", primitive_DOES_GREATER, 0);
  forthCreateDictEnt_primitive("NOVEC", primitive_NOVEC, 0);
  forthCreateDictEnt_primitive("EXVEC:", primitive_EXVEC_COLON, 0);
  forthCreateDictEnt_primitive("DOVEC", primitive_DOVEC, 0);
  forthCreateDictEnt_primitive("ASSIGN", primitive_ASSIGN, 0);
  forthCreateDictEnt_primitive("TO-DO", primitive_TO_DO, (DICT_IMMEDIATE));
  forthCreateDictEnt_primitive("CONSTANT", primitive_CONSTANT, 0);
  forthCreateDictEnt_primitive("DCONSTANT", primitive_D_CONSTANT, 0);
  forthCreateDictEnt_primitive("FCONSTANT", primitive_F_CONSTANT, 0);
  forthCreateDictEnt_primitive("$CONSTANT", primitive_DOLLAR_CONSTANT, 0);
  forthCreateDictEnt_primitive("USER", primitive_USER, 0);
  forthCreateDictEnt_primitive("VARIABLE", primitive_VARIABLE, 0);
  forthCreateDictEnt_primitive("DVARIABLE", primitive_D_VARIABLE, 0);
  forthCreateDictEnt_primitive("FVARIABLE", primitive_F_VARIABLE, 0);
  forthCreateDictEnt_primitive("$VARIABLE", primitive_DOLLAR_VARIABLE, 0);
  forthCreateDictEnt_primitive("(LOCALVAR)", primitive_BRACKET_LOCALVAR, 0);
  forthCreateDictEnt_primitive("(DECLARE-LOCALVAR)", primitive_BRACKET_DECLARE_LOCALVAR, 0);
  forthCreateDictEnt_primitive("(LOCAL)", primitive_BRACKET_LOCAL, (DICT_IMMEDIATE));
  forthCreateDictEnt_primitive("(TO-LOCALVAR)", primitive_BRACKET_TO_LOCALVAR, 0);
  forthCreateDictEnt_primitive("(TO)", primitive_BRACKET_TO, 0);
  forthCreateDictEnt_primitive("TO", primitive_TO, (DICT_IMMEDIATE));
  forthCreateDictEnt_primitive(":", primitive_COLON, 0);
  forthCreateDictEnt_primitive(":NONAME", primitive_COLON_NONAME, 0);
  forthCreateDictEnt_primitive("R:", primitive_R_COLON, 0);
  forthCreateDictEnt_primitive("[", primitive_LEFT_BRACKET, (DICT_IMMEDIATE));
  forthCreateDictEnt_primitive("]", primitive_RIGHT_BRACKET, 0);
  forthCreateDictEnt_primitive("[COMPILE]", primitive_BRACKET_COMPILE, (DICT_IMMEDIATE));
  forthCreateDictEnt_primitive("(EXIT)", primitive_BRACKET_EXIT, 0);
  forthCreateDictEnt_primitive("EXIT", primitive_EXIT, (DICT_IMMEDIATE));
  forthCreateDictEnt_primitive(";", primitive_SEMI_COLON, (DICT_IMMEDIATE));
  forthCreateDictEnt_primitive("R;", primitive_R_SEMI_COLON, (DICT_IMMEDIATE));
  forthCreateDictEnt_primitive("EXECUTE", primitive_EXECUTE, 0);
  forthCreateDictEnt_primitive("@EXECUTE", primitive_FETCH_EXECUTE, 0);
  forthCreateDictEnt_primitive("(TRACE-EXECUTE)", primitive_BRACKET_TRACE_EXECUTE, 0);
  forthCreateDictEnt_primitive("(TRACE-EXECUTE-STACK)", primitive_BRACKET_TRACE_EXECUTE_STACK, 0);
  forthCreateDictEnt_execvec("TRACE-EXECUTE", primitive_BRACKET_TRACE_EXECUTE);
  forthCreateDictEnt_primitive("TRACE", primitive_TRACE, (DICT_IMMEDIATE));
  forthCreateDictEnt_primitive("UNTRACE", primitive_UNTRACE, (DICT_IMMEDIATE));
  forthCreateDictEnt_primitive("TRACE-ALL", primitive_TRACE_ALL, (DICT_IMMEDIATE));
  forthCreateDictEnt_primitive("TRACE-COLONS", primitive_TRACE_COLONS, (DICT_IMMEDIATE));
  forthCreateDictEnt_primitive("TRACEON", primitive_TRACE_ON, (DICT_IMMEDIATE));
  forthCreateDictEnt_primitive("TRACEOFF", primitive_TRACE_OFF, (DICT_IMMEDIATE));
  forthCreateDictEnt_primitive("LIST-BREAK", primitive_LIST_BREAK, (DICT_IMMEDIATE));
  forthCreateDictEnt_primitive("BREAK", primitive_BREAK, (DICT_IMMEDIATE));
  forthCreateDictEnt_primitive("UNBREAK", primitive_UNBREAK, (DICT_IMMEDIATE));
  forthCreateDictEnt_primitive("DEBUGON", primitive_DEBUG_ON, (DICT_IMMEDIATE));
  forthCreateDictEnt_primitive("DEBUGOFF", primitive_DEBUG_OFF, (DICT_IMMEDIATE));
  forthCreateDictEnt_primitive("DECOMPILE", primitive_DECOMPILE, 0);
  forthCreateDictEnt_primitive("CASE-INSENSITIVE", primitive_CASE_INSENSITIVE, 0);
  forthCreateDictEnt_primitive("CASE-SENSITIVE", primitive_CASE_SENSITIVE, 0);
  // ---------- Dictionary definitions

  // ++++++++++ Miscellaneous definitions
  forthCreateDictEnt_primitive("INTERPRET", primitive_INTERPRET, 0);
  forthCreateDictEnt_primitive("$INTERPRET", primitive_DOLLAR_INTERPRET, 0);
  forthCreateDictEnt_primitive("(INTERPRET\")", primitive_BRACKET_INTERPRET_QUOTE, 0);
  forthCreateDictEnt_primitive("INTERPRET\"", primitive_INTERPRET_QUOTE, (DICT_IMMEDIATE));
  forthCreateDictEnt_primitive("(?LOAD)", primitive_BRACKET_QUERY_LOAD, 0);
  forthCreateDictEnt_primitive("(LOAD)", primitive_BRACKET_LOAD, 0);
  forthCreateDictEnt_primitive("?LOAD\"", primitive_QUERY_LOAD_QUOTE, (DICT_IMMEDIATE));
  forthCreateDictEnt_primitive("LOAD\"", primitive_LOAD_QUOTE, (DICT_IMMEDIATE));
  forthCreateDictEnt_primitive("STDOUT-FLUSH", primitive_STDOUT_FLUSH, 0);
  forthCreateDictEnt_primitive("STDOUT>", primitive_STDOUT_TO, 0);
  forthCreateDictEnt_primitive("STDOUT>>", primitive_STDOUT_TO_TO, 0);
  forthCreateDictEnt_primitive("TIME", primitive_TIME, 0);
  forthCreateDictEnt_primitive("MICRO-TIME", primitive_MICRO_TIME, 0);
  forthCreateDictEnt_primitive("SLEEP", primitive_SLEEP, 0);
  forthCreateDictEnt_primitive("OSCLI", primitive_OS_CLI, 0);
  forthCreateDictEnt_primitive("(CLI)", primitive_BRACKET_CLI, 0);
  forthCreateDictEnt_primitive(">CLI", primitive_TO_CLI, (DICT_IMMEDIATE));
  forthCreateDictEnt_primitive("OS'", primitive_OS_QUOTE, (DICT_IMMEDIATE));
  forthCreateDictEnt_primitive("OS-PID", primitive_OS_PID, 0);
  forthCreateDictEnt_primitive("OS-FORK", primitive_OS_FORK, 0);
  // ---------- Miscellaneous definitions

  // Now set FENCE to Top of Dictionary => not allowed to FORGET anything below HERE
  *user_var_FENCE = PTR_TO_ADDR(pDP);
}

// ******************************************
// ********** </BOOTUP DICTIONARY> **********
// ******************************************


// **********************************************
// ********** <PRIMITIVE DEFINITIONS> ***********
// **********************************************

//STEP#3

// NOOP
static void prim_NOOP(void)
{
}

// ++++++++++ System Constants

// FIRST
static void prim_FIRST(void)
{
  SP_PUSH_PTR(pFIRST);
}

// LIMIT
static void prim_LIMIT(void)
{
  SP_PUSH_PTR(pLIMIT);
}

// PAD
static void prim_PAD(void)
{
  SP_PUSH_PTR(pPAD);
}

// WBFR
static void prim_WBFR(void)
{
  SP_PUSH_PTR(pWBFR);
}

// WDSZ
static void prim_WDSZ(void)
{
  SP_PUSH(g_pWB_HIGH - g_pWB);
}

// +ORIGIN
static void prim_PLUS_ORIGIN(void)
{
  SP_PUSH_PTR(p0_plus_ORIGIN + SP_POP());
}

// ---------- System Constants

// ++++++++++ Error definitions

// QUIT
static void prim_QUIT(void)
{
  forth_QUIT();
}

// MSG#
static void prim_MSG_HASH(void)
{
  SINGLE num = SP_POP();
  forth_MSG_HASH(num);
}

// MESSAGE
static void vect_MESSAGE(void)
{
  // first time: find MESSAGE in the Dictionary
  static PFORTHDICTENT s_pDEMessage = NULL;
  if (s_pDEMessage == NULL)
    s_pDEMessage = forthFindDictEnt("MESSAGE");

  // execute the FORTHDICTENT which the vector is assigned to do
  forth_EXECUTE_EXVEC(s_pDEMessage->pfa.addrs[0]);
}

// OSERRNO
static void prim_OS_ERRNO(void)
{
  forth_OS_ERRNO();
}

// OSERROR
static void prim_OS_ERROR(void)
{
  forth_OS_ERROR();
}

// ERROR
static void prim_ERROR(void)
{
  SINGLE num = SP_POP();
  forth_ERROR(num);
}

// ?ERROR
static void prim_QUERY_ERROR(void)
{
  SINGLE num = SP_POP();
  bool flag = SP_POP();
  if (flag)
    forth_ERROR(num);
}

// ?STACK
static void prim_QUERY_STACK(void)
{
  // must not use CHECK_SP() in case compiled FAST
  if (pSP <= g_pCS_LOW)
    forth_ERROR(err_STACK_FULL);
  if (pSP > g_pCS)
    forth_ERROR(err_STACK_EMPTY);
}

// WARM
static void prim_WARM(void)
{
  // longjmp into forthMAIN(), restart at COLD
  forth_WARM();
}

// (ABORT)
static void prim_BRACKET_ABORT(void)
{
  forth_BRACKET_ABORT();
}

// ABORT
static void vect_ABORT(void)
{
  // first time: find ABORT in the Dictionary
  static PFORTHDICTENT s_pDEAbort = NULL;
  if (s_pDEAbort == NULL)
    s_pDEAbort = forthFindDictEnt("ABORT");

  // execute the FORTHDICTENT which the vector is assigned to do
  forth_EXECUTE_EXVEC(s_pDEAbort->pfa.addrs[0]);
}

// INITVECS
static void prim_INITVECS(void)
{
  forth_INITVECS();
}

// COLD
static void prim_COLD(void)
{
  // longjmp into forthMAIN(), restart at COLD
  forth_COLD();
}

// ---------- Error definitions

// ++++++++++ Compilation State definitions

// ?PAIRS
static void prim_QUERY_PAIRS(void)
{
  if (SP_POP() != SP_POP())
  {
    forth_ERROR(err_COND_NOT_PAIRED);
    return;
  }
}

// ?CSP
static void prim_QUERY_CSP(void)
{
  // verify that pSP == pCSP, for Compiler Security
  if (pSP != pCSP)
  {
    forth_ERROR(err_COND_NOT_PAIRED);
    return;
  }
}

// ?COMP
static void prim_QUERY_COMP(void)
{
  // error if not in Compilation Mode
  if (!COMPILATION_MODE())
  {
    forth_ERROR(err_COMPILATION_ONLY);
    return;
  }
}

// ?EXEC
static void prim_QUERY_EXEC(void)
{
  // error if not in Execution Mode
  if (!EXECUTION_MODE())
  {
    forth_ERROR(err_EXECUTION_ONLY);
    return;
  }
}

// (ASSERT)
static void prim_BRACKET_ASSERT(void)
{
  // fetch the inline SINGLE from the CALLER
  CHECK_CALLER();
  SINGLE line = *(PSINGLE)CALLER_INLINE_ADDRS();
  // advance the CALLER over the SINGLE
  CALLER_ADVANCE(1);
  if (SP_POP() != 0)
    return;
  external_fflush(stdout);
  external_fflush(g_fpOUT);
  fprintf(stderr, "Assert line #%hd: ", line);
  forth_ERROR(err_ASSERT_FAIL);
}

// ASSERT
static void prim_ASSERT(void)
{
  if (COMPILATION_MODE())
  {
    // Compile (ASSERT) into Dictionary
    forthDictStoreFindPrimitive(primitive_BRACKET_ASSERT);
    // Compile the current line number into Dictionary
    forthDictStoreSingle(forth_ERR_LINE);
  }
  else
  {
    if (SP_POP() != 0)
      return;
    external_fflush(stdout);
    external_fflush(g_fpOUT);
    fprintf(stderr, "Assert line #%hd: ", forth_ERR_LINE);
    forth_ERROR(err_ASSERT_FAIL);
  }
}

// ---------- Compilation State definitions

// ++++++++++ Computation Stack definitions

// SP!
static void prim_SP_STORE(void)
{
  CLEAR_COMPUTATION_STACK();
}

// SP@
static void prim_SP_FETCH(void)
{
  SP_PUSH_PTR(pSP);
}

// DEPTH
static void prim_DEPTH(void)
{
  SP_PUSH((pS0 - pSP) / sizeof(SINGLE));
}

// DROP
static void prim_DROP(void)
{
  (void)SP_POP();
}

// DUP
static void prim_DUP(void)
{
  CHECK_SP_HAS_1();
  SP_PUSH(*SP_0());
}

// ?DUP
static void prim_QUERY_DUP(void)
{
  CHECK_SP_HAS_1();
  if (*SP_0() != 0)
    SP_PUSH(*SP_0());
}

// OVER
static void prim_OVER(void)
{
  CHECK_SP_HAS_2();
  SP_PUSH(*SP_1());
}

// ROT
static void prim_ROT(void)
{
  CHECK_SP_HAS_3();
  SINGLE temp = *SP_2();
  *SP_2() = *SP_1();
  *SP_1() = *SP_0();
  *SP_0() = temp;
}

// SWAP
static void prim_SWAP(void)
{
  CHECK_SP_HAS_2();
  SINGLE temp = *SP_1();
  *SP_1() = *SP_0();
  *SP_0() = temp;
}

// PICK
static void prim_PICK(void)
{
  SINGLE num = SP_POP();
  if (num < 1)
    return;
  CHECK_SP_HAS_N(num);
  SP_PUSH(*SP_N(num));
}

// ROLL
static void prim_ROLL(void)
{
  SINGLE num = SP_POP();
  if (num < 1)
    return;
  CHECK_SP_HAS_N(num);
  SINGLE temp = *SP_N(num);
  memmove(SP_1(), SP_0(), (num - 1) * sizeof(SINGLE));
  *SP_0() = temp;
}

// 2DROP
static void prim_2_DROP(void)
{
  (void)SP_POP_DOUBLE();
}

// 2DUP
static void prim_2_DUP(void)
{
  CHECK_SP_HAS_2();
  SP_PUSH_DOUBLE(*(PDOUBLE)SP_0());
}

// 2OVER
static void prim_2_OVER(void)
{
  CHECK_SP_HAS_N(4);
  SP_PUSH_DOUBLE(*(PDOUBLE)SP_2());
}

// 2SWAP
static void prim_2_SWAP(void)
{
  CHECK_SP_HAS_N(4);
  DOUBLE temp = *(PDOUBLE)SP_N(3);
  *(PDOUBLE)SP_N(3) = *(PDOUBLE)SP_N(1);
  *(PDOUBLE)SP_N(1) = temp;
}

// FDROP
static void prim_F_DROP(void)
{
  (void)SP_POP_FLOAT();
}

// FDUP
static void prim_F_DUP(void)
{
  CHECK_SP_HAS_FLOAT();
  SP_PUSH_FLOAT(*SP_FLOAT_0());
}

// ---------- Computation Stack definitions

// ++++++++++ Return Stack definitions

// RP!
static void prim_RP_STORE(void)
{
  CLEAR_RETURN_STACK();
}

// RP@
static void prim_RP_FETCH(void)
{
  SP_PUSH_PTR(pRP);
}

// >R
static void prim_TO_R(void)
{
  RP_PUSH(SP_POP());
}

// R>
static void prim_R_FROM(void)
{
  SP_PUSH(RP_POP());
}

// R@
static void prim_R_FETCH(void)
{
  CHECK_RP_HAS_1();
  SP_PUSH(*RP_0());
}

// D>R
static void prim_D_TO_R(void)
{
  RP_PUSH_DOUBLE(SP_POP_DOUBLE());
}

// F>R
static void prim_F_TO_R(void)
{
  RP_PUSH_FLOAT(SP_POP_FLOAT());
}

// R>D
static void prim_R_FROM_D(void)
{
  SP_PUSH_DOUBLE(RP_POP_DOUBLE());
}

// R>F
static void prim_R_FROM_F(void)
{
  SP_PUSH_FLOAT(RP_POP_FLOAT());
}


// ---------- Return Stack definitions

// ++++++++++ Arithmetic definitions

// *
static void prim_TIMES(void)
{
  SINGLE num2 = SP_POP();
  SP_REPLACE(*SP_0() * num2);
}

// +
static void prim_PLUS(void)
{
  SINGLE num2 = SP_POP();
  SP_REPLACE(*SP_0() + num2);
}

// +!
static void prim_PLUS_STORE(void)
{
  PSINGLE pSingle = SP_POP_PTR();
  *pSingle += SP_POP();
}

// 1+!
static void prim_1_PLUS_STORE(void)
{
  PSINGLE pSingle = SP_POP_PTR();
  (*pSingle)++;
}

// 1-!
static void prim_1_MINUS_STORE(void)
{
  PSINGLE pSingle = SP_POP_PTR();
  (*pSingle)--;
}

// -
static void prim_SUBTRACT(void)
{
  SINGLE num2 = SP_POP();
  SP_REPLACE(*SP_0() - num2);
}

// /
static void prim_DIVIDE(void)
{
  SINGLE num2 = SP_POP();
  SINGLE num1 = SP_POP();
  if (num2 == 0)
  {
    forth_ERROR(err_DIV_0);
    return;
  }
  SP_PUSH(num1 / num2);
}

// MOD
static void prim_MOD(void)
{
  SINGLE num2 = SP_POP();
  SINGLE num1 = SP_POP();
  if (num2 == 0)
  {
    forth_ERROR(err_DIV_0);
    return;
  }
  SP_PUSH(num1 % num2);
}

// /MOD
static void prim_DIVIDE_MOD(void)
{
  SINGLE num2 = SP_POP();
  SINGLE num1 = SP_POP();
  if (num2 == 0)
  {
    forth_ERROR(err_DIV_0);
    return;
  }
  SP_PUSH(num1 % num2);
  SP_PUSH(num1 / num2);
}

// */
static void prim_TIMES_DIVIDE(void)
{
  SINGLE num3 = SP_POP();
  SINGLE num2 = SP_POP();
  SINGLE num1 = SP_POP();
  if (num3 == 0)
  {
    forth_ERROR(err_DIV_0);
    return;
  }
  SP_PUSH((DOUBLE)num1 * num2 / num3);
}

// */MOD
static void prim_TIMES_DIVIDE_MOD(void)
{
  SINGLE num3 = SP_POP();
  SINGLE num2 = SP_POP();
  SINGLE num1 = SP_POP();
  if (num3 == 0)
  {
    forth_ERROR(err_DIV_0);
    return;
  }
  SP_PUSH((DOUBLE)num1 * num2 % num3);
  SP_PUSH((DOUBLE)num1 * num2 / num3);
}

// NEGATE
static void prim_NEGATE(void)
{
  SP_REPLACE(-(*SP_0()));
}

// ABS
static void prim_ABS(void)
{
  if (*SP_0() < 0)
    SP_REPLACE(-(*SP_0()));
}

// +-
static void prim_PLUS_MINUS(void)
{
  SINGLE n2 = SP_POP();
  if (n2 < 0)
    SP_REPLACE(-(*SP_0()));
}

// 1+
static void prim_1_PLUS(void)
{
  SP_REPLACE((*SP_0()) + 1);
}

// 1-
static void prim_1_MINUS(void)
{
  SP_REPLACE((*SP_0()) - 1);
}

// 2+
static void prim_2_PLUS(void)
{
  SP_REPLACE((*SP_0()) + 2);
}

// 2-
static void prim_2_MINUS(void)
{
  SP_REPLACE((*SP_0()) - 2);
}

// 2*
static void prim_2_TIMES(void)
{
  SP_REPLACE((*SP_0()) * 2);
}

// 2/
static void prim_2_DIVIDE(void)
{
  SP_REPLACE((*SP_0()) / 2);
}

// AND
static void prim_AND(void)
{
  SINGLE num2 = SP_POP();
  SP_REPLACE(num2 & *SP_0());
}

// OR
static void prim_OR(void)
{
  SINGLE num2 = SP_POP();
  SP_REPLACE(num2 | *SP_0());
}

// XOR
static void prim_X_OR(void)
{
  SINGLE num2 = SP_POP();
  SP_REPLACE(num2 ^ *SP_0());
}

// TOGGLE
static void prim_TOGGLE(void)
{
  byte b = SP_POP();
  PBYTE pBYTE = SP_POP_PTR();
  *pBYTE ^= b;
}

// 0<
static void prim_ZERO_LESS(void)
{
  SP_REPLACE(*SP_0() < 0);
}

// 0=
static void prim_ZERO_EQUALS(void)
{
  SP_REPLACE(*SP_0() == 0);
}

// NOT
static void prim_NOT(void)
{
  SP_REPLACE(*SP_0() == 0);
}

// 0>
static void prim_ZERO_GREATER(void)
{
  SP_REPLACE(*SP_0() > 0);
}

// <
static void prim_LESS_THAN(void)
{
  SINGLE num2 = SP_POP();
  SP_REPLACE(*SP_0() < num2);
}

// =
static void prim_EQUALS(void)
{
  SINGLE num2 = SP_POP();
  SP_REPLACE(*SP_0() == num2);
}

// NOT=
static void prim_NOT_EQUALS(void)
{
  SINGLE num2 = SP_POP();
  SP_REPLACE(*SP_0() != num2);
}

// >
static void prim_GREATER_THAN(void)
{
  SINGLE num2 = SP_POP();
  SP_REPLACE(*SP_0() > num2);
}

// U<
static void prim_U_LESS_THAN(void)
{
  SINGLE num2 = SP_POP();
  SP_REPLACE(*(PUSINGLE)SP_0() < (USINGLE)num2);
}

// U>
static void prim_U_GREATER_THAN(void)
{
  SINGLE num2 = SP_POP();
  SP_REPLACE(*(PUSINGLE)SP_0() > (USINGLE)num2);
}

// MAX
static void prim_MAX(void)
{
  SINGLE num2 = SP_POP();
  if (num2 > *SP_0())
    SP_REPLACE(num2);
}

// MIN
static void prim_MIN(void)
{
  SINGLE num2 = SP_POP();
  if (num2 < *SP_0())
    SP_REPLACE(num2);
}

// S->D
static void prim_S_TO_D(void)
{
  // convert Single to Double
  // note how this is equivalent to 0/-1 SWAP, whereas BBC would be 0/-1
  SINGLE num = SP_POP();
  SP_PUSH_DOUBLE((DOUBLE)num);
}

// U->D
static void prim_U_TO_D(void)
{
  // convert unsigned Single to Double
  // note how this is equivalent to 0 SWAP, whereas BBC would be 0
  USINGLE num = SP_POP();
  SP_PUSH_DOUBLE((DOUBLE)num);
}

// D->S
static void prim_D_TO_S(void)
{
  // convert Double to Single
  // note how this is equivalent to SWAP DROP, whereas BBC would be DROP
  DOUBLE num = SP_POP_DOUBLE();
  SP_PUSH((SINGLE)num);
}

// D->F
static void prim_D_TO_F(void)
{
  // convert Double to Float
  DOUBLE num = SP_POP_DOUBLE();
  SP_PUSH_FLOAT((FLOAT)num);
}

// D*
static void prim_D_TIMES(void)
{
  DOUBLE num2 = SP_POP_DOUBLE();
  SP_REPLACE_DOUBLE(*SP_DOUBLE_0() * num2);
}

// D+
static void prim_D_PLUS(void)
{
  DOUBLE num2 = SP_POP_DOUBLE();
  SP_REPLACE_DOUBLE(*SP_DOUBLE_0() + num2);
}

// D/
static void prim_D_DIVIDE(void)
{
  DOUBLE num2 = SP_POP_DOUBLE();
  DOUBLE num1 = SP_POP_DOUBLE();
  if (num2 == 0)
  {
    forth_ERROR(err_DIV_0);
    return;
  }
  SP_PUSH_DOUBLE(num1 / num2);
}

// D/MOD
static void prim_D_DIVIDE_MOD(void)
{
  DOUBLE num2 = SP_POP_DOUBLE();
  DOUBLE num1 = SP_POP_DOUBLE();
  if (num2 == 0)
    forth_ERROR(err_DIV_0);
  SP_PUSH_DOUBLE(num1 % num2);
  SP_PUSH_DOUBLE(num1 / num2);
}

// U*
static void prim_U_TIMES(void)
{
  SP_REPLACE_DOUBLE(*(PUSINGLE)SP_1() * *(PUSINGLE)SP_0());
}

// DNEGATE
static void prim_D_NEGATE(void)
{
  SP_REPLACE_DOUBLE(-(*(PDOUBLE)SP_0()));
}

// DABS
static void prim_D_ABS(void)
{
  if (*(PDOUBLE)SP_0() < 0)
    SP_REPLACE_DOUBLE(-(*(PDOUBLE)SP_0()));
}

// D+-
static void prim_D_PLUS_MINUS(void)
{
  SINGLE n2 = SP_POP();
  if (n2 < 0)
    SP_REPLACE_DOUBLE(-(*(PDOUBLE)SP_0()));
}

// D-
static void prim_D_SUBTRACT(void)
{
  DOUBLE num2 = SP_POP_DOUBLE();
  SP_REPLACE_DOUBLE(*SP_DOUBLE_0() - num2);
}

// D<
static void prim_D_LESS_THAN(void)
{
  DOUBLE num2 = SP_POP_DOUBLE();
  DOUBLE num1 = SP_POP_DOUBLE();
  SP_PUSH(num1 < num2);
}

// D=
static void prim_D_EQUALS(void)
{
  DOUBLE num2 = SP_POP_DOUBLE();
  DOUBLE num1 = SP_POP_DOUBLE();
  SP_PUSH(num1 == num2);
}

// U/
static void prim_U_DIVIDE(void)
{
  USINGLE num2 = SP_POP();
  UDOUBLE num1 = SP_POP_DOUBLE();
  SP_PUSH((USINGLE)(num1 % num2));
  SP_PUSH((USINGLE)(num1 / num2));
}

// U/MOD
static void prim_U_DIVIDE_MOD(void)
{
  USINGLE num2 = SP_POP();
  UDOUBLE num1 = SP_POP_DOUBLE();
  if (num2 == 0)
  {
    forth_ERROR(err_DIV_0);
    return;
  }
  SP_PUSH((USINGLE)(num1 % num2));
  SP_PUSH((USINGLE)(num1 / num2));
}

// F*
static void prim_F_TIMES(void)
{
  FLOAT num2 = SP_POP_FLOAT();
  SP_REPLACE_FLOAT(*SP_FLOAT_0() * num2);
}

// F+
static void prim_F_PLUS(void)
{
  FLOAT num2 = SP_POP_FLOAT();
  SP_REPLACE_FLOAT(*SP_FLOAT_0() + num2);
}

// F-
static void prim_F_SUBTRACT(void)
{
  FLOAT num2 = SP_POP_FLOAT();
  SP_REPLACE_FLOAT(*SP_FLOAT_0() - num2);
}

// F/
static void prim_F_DIVIDE(void)
{
  FLOAT num2 = SP_POP_FLOAT();
  FLOAT num1 = SP_POP_FLOAT();
  if (num2 == 0.0)
  {
    forth_ERROR(err_DIV_0);
    return;
  }
  SP_PUSH_FLOAT(num1 / num2);
}

// F->D
static void prim_F_TO_D(void)
{
  // convert Float to Double
  FLOAT num = SP_POP_FLOAT();
  SP_PUSH_DOUBLE((DOUBLE)num);
}

// F->SF
static void prim_F_TO_SF(void)
{
  // convert Float to Small Float
  FLOAT num = SP_POP_FLOAT();
  SP_PUSH_SMALLFLOAT((SMALLFLOAT)num);
}

// SF->F
static void prim_SF_TO_F(void)
{
  // convert Small Float to Float
  SMALLFLOAT num = SP_POP_SMALLFLOAT();
  SP_PUSH_FLOAT((FLOAT)num);
}

// F<
static void prim_F_LESS_THAN(void)
{
  FLOAT num2 = SP_POP_FLOAT();
  FLOAT num1 = SP_POP_FLOAT();
  SP_PUSH(num1 < num2);
}

// F=
static void prim_F_EQUALS(void)
{
  FLOAT num2 = SP_POP_FLOAT();
  FLOAT num1 = SP_POP_FLOAT();
  SP_PUSH(num1 == num2);
}

// ---------- Arithmetic definitions

// ++++++++++ Literal definitions

// LIT
static void prim_LIT(void)
{
  // fetch the inline SINGLE from the CALLER
  CHECK_CALLER();
  SINGLE n = *(PSINGLE)CALLER_INLINE_ADDRS();
  // advance the CALLER over the SINGLE
  CALLER_ADVANCE(1);
  // push the value to the Stack
  SP_PUSH(n);
}

// LITERAL
static void prim_LITERAL(void)
{
  // Compilation Mode only
  if (!COMPILATION_MODE())
    return;
  // Compile LIT into Dictionary
  forthDictStoreFindPrimitive(primitive_LIT);
  // store SINGLE into Dictionary
  SINGLE n = SP_POP();
  forthDictStoreSingle(n);
}

// DLIT
static void prim_D_LIT(void)
{
  // fetch the inline DOUBLE from the CALLER
  CHECK_CALLER();
  DOUBLE n = *(PDOUBLE)CALLER_INLINE_ADDRS();
  // advance the CALLER over the DOUBLE
  CALLER_ADVANCE(2);
  // push the value to the Stack
  SP_PUSH_DOUBLE(n);
}

// DLITERAL
static void prim_D_LITERAL(void)
{
  // Compilation Mode only
  if (!COMPILATION_MODE())
    return;
  // Compile DLIT into Dictionary
  forthDictStoreFindPrimitive(primitive_D_LIT);
  // store DOUBLE into Dictionary
  DOUBLE n = SP_POP_DOUBLE();
  forthDictStoreDouble(n);
}

// FLIT
static void prim_F_LIT(void)
{
  // fetch the inline DOUBLE from the CALLER
  CHECK_CALLER();
  FLOAT n = *(PFLOAT)CALLER_INLINE_ADDRS();
  // advance the CALLER over the FLOAT
  CALLER_ADVANCE(sizeof(FLOAT) / sizeof(ADDR));
  // push the value to the Stack
  SP_PUSH_FLOAT(n);
}

// FLITERAL
static void prim_F_LITERAL(void)
{
  // Compilation Mode only
  if (!COMPILATION_MODE())
    return;
  // Compile FLIT into Dictionary
  forthDictStoreFindPrimitive(primitive_F_LIT);
  // store FLOAT into Dictionary
  FLOAT n = SP_POP_FLOAT();
  forthDictStoreFloat(n);
}

// CLIT
static void prim_C_LIT(void)
{
  // fetch the inline BYTE from the CALLER
  CHECK_CALLER();
  BYTE c = *(PBYTE)CALLER_INLINE_ADDRS();
  // advance the CALLER over the BYTE (it occupies a SINGLE)
  CALLER_ADVANCE(1);
  // push the value to the Stack
  SP_PUSH(c);
}

// CLITERAL
static void prim_C_LITERAL(void)
{
  // Compilation Mode only
  if (!COMPILATION_MODE())
    return;
  // Compile CLIT into Dictionary
  forthDictStoreFindPrimitive(primitive_C_LIT);
  // store BYTE into Dictionary (it occupies a SINGLE)
  BYTE c = SP_POP();
  forthDictStoreSingle(c);
}

// ALIT
static void prim_A_LIT(void)
{
  // fetch the inline ADDR from the CALLER
  CHECK_CALLER();
  ADDR addr = *(PADDR)CALLER_INLINE_ADDRS();
  // advance the CALLER over the ADDR
  CALLER_ADVANCE(1);
  // push the value to the Stack
  SP_PUSH(addr);
}

// ALITERAL
static void prim_A_LITERAL(void)
{
  // Compilation Mode only
  if (!COMPILATION_MODE())
    return;
  // Compile ALIT into Dictionary
  forthDictStoreFindPrimitive(primitive_A_LIT);
  // store ADDR into Dictionary
  ADDR addr = SP_POP();
  forthDictStoreAddr(addr);
}

// (.")
static void prim_BRACKET_DOT_QUOTE(void)
{
  // fetch the inline FORTHSTRING from the CALLER
  CHECK_CALLER();
  PFORTHSTRING pStr = (PFORTHSTRING)CALLER_INLINE_ADDRS();
  // advance the CALLER over the FORTHSTRING
  CALLER_ADVANCE_FORTHSTRING(pStr);
  // output the value
  forth_putcount(pStr->len, pStr->chars);
}

// ."
static void prim_DOT_QUOTE(void)
{
  // start of literal string, move up to "
  forth_WORD('"');
  if (COMPILATION_MODE())
  {
    // Compile (.") into Dictionary
    forthDictStoreFindPrimitive(primitive_BRACKET_DOT_QUOTE);
    // store FORTHSTRING into Dictionary
    forthDictStoreForthString(pWBFR->len, pWBFR->chars);
  }
  else
  {
    forth_putcount(pWBFR->len, pWBFR->chars);
  }
}

// (
static void prim_PAREN(void)
{
  // start of comment, skip to )
  forth_WORD(')');
}

// \ (backslash)
static void prim_BACKSLASH(void)
{
  // start of comment, skip to end of line
  while (pIBUF[g_ofstTO_IN] != '\0')
    (*user_var_TO_IN)++;
}

// ---------- Literal definitions

// ++++++++++ Store/Fetch definitions

// !
static void prim_STORE(void)
{
  ADDR addr = SP_POP_ADDR();
  SINGLE n = SP_POP();
  *(PSINGLE)ADDR_TO_PTR(addr) = n;
}

// D!
static void prim_D_STORE(void)
{
  ADDR addr = SP_POP_ADDR();
  DOUBLE n = SP_POP_DOUBLE();
  *(PDOUBLE)ADDR_TO_PTR(addr) = n;
}

// F!
static void prim_F_STORE(void)
{
  ADDR addr = SP_POP_ADDR();
  FLOAT n = SP_POP_FLOAT();
  *(PFLOAT)ADDR_TO_PTR(addr) = n;
}

// SF!
static void prim_SF_STORE(void)
{
  ADDR addr = SP_POP_ADDR();
  SMALLFLOAT n = SP_POP_SMALLFLOAT();
  *(PSMALLFLOAT)ADDR_TO_PTR(addr) = n;
}

// C!
static void prim_C_STORE(void)
{
  ADDR addr = SP_POP_ADDR();
  byte n = SP_POP();
  *(PBYTE)ADDR_TO_PTR(addr) = n;
}

// C+!
static void prim_C_PLUS_STORE(void)
{
  PBYTE pByte = SP_POP_PTR();
  *pByte += SP_POP();
}

// C1+!
static void prim_C_1_PLUS_STORE(void)
{
  PBYTE pByte = SP_POP_PTR();
  (*pByte)++;
}

// C1-!
static void prim_C_1_MINUS_STORE(void)
{
  PBYTE pByte = SP_POP_PTR();
  (*pByte)--;
}

// @
static void prim_FETCH(void)
{
  ADDR addr = SP_POP_ADDR();
  SINGLE n = *(PSINGLE)ADDR_TO_PTR(addr);
  SP_PUSH(n);
}

// D@
static void prim_D_FETCH(void)
{
  ADDR addr = SP_POP_ADDR();
  DOUBLE n = *(PDOUBLE)ADDR_TO_PTR(addr);
  SP_PUSH_DOUBLE(n);
}

// F@
static void prim_F_FETCH(void)
{
  ADDR addr = SP_POP_ADDR();
  FLOAT n = *(PFLOAT)ADDR_TO_PTR(addr);
  SP_PUSH_FLOAT(n);
}

// SF@
static void prim_SF_FETCH(void)
{
  ADDR addr = SP_POP_ADDR();
  SMALLFLOAT n = *(PSMALLFLOAT)ADDR_TO_PTR(addr);
  SP_PUSH_SMALLFLOAT(n);
}

// C@
static void prim_C_FETCH(void)
{
  ADDR addr = SP_POP_ADDR();
  BYTE n = *(PBYTE)ADDR_TO_PTR(addr);
  SP_PUSH(n);
}

// CMOVE
static void prim_C_MOVE(void)
{
  SINGLE count = SP_POP();
  PBYTE pTo = SP_POP_PTR();
  PBYTE pFrom = SP_POP_PTR();
  if (count <= 0)
    return;
  memmove(pTo, pFrom, count * sizeof(BYTE));
}

// MOVE
static void prim_MOVE(void)
{
  SINGLE count = SP_POP();
  PBYTE pTo = SP_POP_PTR();
  PBYTE pFrom = SP_POP_PTR();
  if (count <= 0)
    return;
  memmove(pTo, pFrom, count * sizeof(SINGLE));
}

// FILL
static void prim_FILL(void)
{
  BYTE b = SP_POP();
  SINGLE num = SP_POP();
  PBYTE pADDR = SP_POP_PTR();
  if (num <= 0)
    return;
  memset(pADDR, b, num);
}

// ERASE
static void prim_ERASE(void)
{
  SP_PUSH('\0');
  prim_FILL();
}

// BLANKS
static void prim_BLANKS(void)
{
  SP_PUSH(' ');
  prim_FILL();
}

// MEMCMP
static void prim_MEMCMP(void)
{
  USINGLE num = SP_POP();
  PBYTE pMem2 = SP_POP_PTR();
  PBYTE pMem1 = SP_POP_PTR();
  SP_PUSH(memcmp(pMem1, pMem2, num));
}

// MEMDUMP
static void prim_MEMDUMP(void)
{
  SINGLE num = SP_POP();
  ADDR addr = SP_POP_ADDR();
  forthDumpMemBytes(addr, num);
}

// MEMDUMPW
static void prim_MEMDUMP_W(void)
{
  SINGLE num = SP_POP();
  ADDR addr = SP_POP_ADDR();
  forthDumpMemWords(addr, num);
}

// ---------- Store/Fetch definitions

// ++++++++++ String definitions

// STRING
static void prim_STRING(void)
{
  char c = SP_POP();
  if (c == '\0' || isspace(c))
  {
    forth_ERROR(err_BAD_PARAM);
    return;
  }
  forth_WORD(c);
  SP_PUSH_PTR(pWBFR->chars);
  SP_PUSH(pWBFR->len);
}

// TEXT
static void prim_TEXT(void)
{
  SINGLE num = SP_POP();
  const char *pChars = SP_POP_PTR();
  forthDictStoreForthString(num, pChars);
}

// (")
static void prim_BRACKET_QUOTE(void)
{
  // fetch the inline FORTHSTRING from the CALLER
  CHECK_CALLER();
  PFORTHSTRING pStr = (PFORTHSTRING)CALLER_INLINE_ADDRS();
  // advance the CALLER over the FORTHSTRING
  CALLER_ADVANCE_FORTHSTRING(pStr);
  SP_PUSH_PTR(pStr);
}

// "
static void prim_QUOTE(void)
{
  // start of literal string, move up to "
  SP_PUSH('\"');
  prim_STRING();
  if (COMPILATION_MODE())
  {
    // Compile (") into Dictionary
    forthDictStoreFindPrimitive(primitive_BRACKET_QUOTE);
    // store FORTHSTRING into Dictionary
    prim_TEXT();
  }
  else
  {
    // move string to Scratch Pad
    BYTE len = SP_POP();
    const char *pChars = SP_POP_PTR();
    pPAD->len = len;
    memcpy(pPAD->chars, pChars, len);
    // '\0' terminate string
    pPAD->chars[len] = '\0';
    // and leave addr
    SP_PUSH_PTR(pPAD);
  }
}

// COUNT
static void prim_COUNT(void)
{
  CHECK_SP_HAS_1();
  PFORTHSTRING pStr = ADDR_TO_PTR(*SP_0());
  SP_REPLACE(*SP_0() + 1);
  SP_PUSH(pStr->len);
}

// -TRAILING
static void prim_DASH_TRAILING(void)
{
  CHECK_SP_HAS_2();
  SINGLE num = *SP_0();
  if (num < 0)
  {
    forth_ERROR(err_BAD_PARAM);
    return;
  }
  const char *pChars = ADDR_TO_PTR(*SP_1());
  while (num > 0 && isspace(pChars[num - 1]))
    num--;
  SP_REPLACE(num);
}

// TYPE
static void prim_TYPE(void)
{
  SINGLE num = SP_POP();
  const char *pChars = SP_POP_PTR();
  forth_putcount(num, pChars);
}

// ($+)
static void prim_BRACKET_DOLLAR_PLUS(void)
{
  PFORTHSTRING pStr = SP_POP_PTR();
  BYTE count = SP_POP();
  char *pChars = SP_POP_PTR();
  if (pStr->len + count > 255)
  {
    forth_ERROR(err_BAD_PARAM);
    return;
  }
  memcpy(pStr->chars + pStr->len, pChars, count);
  pStr->len += count;
  pStr->chars[pStr->len] = '\0';
}

// ($!)
static void prim_BRACKET_DOLLAR_STORE(void)
{
  PFORTHSTRING pStr = SP_POP_PTR();
  BYTE count = SP_POP();
  char *pChars = SP_POP_PTR();
  memcpy(pStr->chars, pChars, count);
  pStr->len = count;
  // '\0' terminate string
  pStr->chars[pStr->len] = '\0';
}

// $+
static void prim_DOLLAR_PLUS(void)
{
  CHECK_SP_HAS_2();
  PFORTHSTRING pStr = ADDR_TO_PTR(*SP_0());
  PFORTHMAXLENSTRING pMLStr = (PFORTHMAXLENSTRING)(((PBYTE)pStr) - 1);
  BYTE count = *SP_1();
  if (pStr->len + count > pMLStr->maxlen)
  {
    forth_ERROR(err_BAD_PARAM);
    return;
  }
  prim_BRACKET_DOLLAR_PLUS();
}

// $,
static void prim_DOLLAR_COMMA(void)
{
  PFORTHSTRING pStr = SP_POP_PTR();
  forthDictStoreForthString(pStr->len, pStr->chars);
}

// $!
static void prim_DOLLAR_STORE(void)
{
  CHECK_SP_HAS_2();
  PFORTHSTRING pStr = ADDR_TO_PTR(*SP_0());
  PFORTHMAXLENSTRING pMLStr = (PFORTHMAXLENSTRING)(((PBYTE)pStr) - 1);
  BYTE count = *SP_1();
  if (count > pMLStr->maxlen)
  {
    forth_ERROR(err_BAD_PARAM);
    return;
  }
  prim_BRACKET_DOLLAR_STORE();
}

// $CMP
static void prim_DOLLAR_CMP(void)
{
  PFORTHSTRING pStr2 = SP_POP_PTR();
  PFORTHSTRING pStr1 = SP_POP_PTR();
  int len1 = pStr1->len, len2 = pStr2->len;
  byte minlen = (len1 < len2) ? len1 : len2;
  SINGLE cmp = memcmp(pStr1->chars, pStr2->chars, minlen);
  if (cmp == 0)
  {
    if (len1 > len2)
      cmp = (unsigned char)pStr1->chars[minlen];
    else if (len2 > len1)
      cmp = -(unsigned char)pStr2->chars[minlen];
  }
  SP_PUSH(cmp);
}

// $CMPID
static void prim_DOLLAR_CMPID(void)
{
  PFORTHSTRING pStr2 = SP_POP_PTR();
  PFORTHSTRING pStr1 = SP_POP_PTR();
  int len1 = (pStr1->len & 0x1F), len2 = (pStr2->len & 0x1F);
  byte minlen = (len1 < len2) ? len1 : len2;
  SINGLE cmp = memcmp(pStr1->chars, pStr2->chars, minlen);
  if (cmp == 0)
  {
    if (len1 > len2)
      cmp = (unsigned char)pStr1->chars[minlen];
    else if (len2 > len1)
      cmp = -(unsigned char)pStr2->chars[minlen];
  }
  SP_PUSH(cmp);
}

// ---------- String definitions

// ++++++++++ Output definitions

// (EMIT)
static void prim_BRACKET_EMIT(void)
{
  forth_putc((char)SP_POP());
}

// EMIT
static void vect_EMIT(void)
{
  // first time: find EMIT in the Dictionary
  static PFORTHDICTENT s_pDEEmit = NULL;
  if (s_pDEEmit == NULL)
    s_pDEEmit = forthFindDictEnt("EMIT");

  // execute the FORTHDICTENT which the vector is assigned to do
  forth_EXECUTE_EXVEC(s_pDEEmit->pfa.addrs[0]);
}

// (EMITS)
static void prim_BRACKET_EMITS(void)
{
  BYTE count = SP_POP();
  const char *pChars = SP_POP_PTR();
  forth_putcount(count, pChars);
}

// EMITS
static void vect_EMITS(void)
{
  // first time: find EMITS in the Dictionary
  static PFORTHDICTENT s_pDEEmits = NULL;
  if (s_pDEEmits == NULL)
    s_pDEEmits = forthFindDictEnt("EMITS");

  // execute the FORTHDICTENT which the vector is assigned to do
  forth_EXECUTE_EXVEC(s_pDEEmits->pfa.addrs[0]);
}

// CR
static void prim_CR(void)
{
  forth_putc('\n');
}

// SPACE
static void prim_SPACE(void)
{
  forth_putc(' ');
}

// SPACES
static void prim_SPACES(void)
{
  SINGLE num = SP_POP();
  while (num-- > 0)
    prim_SPACE();
}

// DECIMAL
static void prim_DECIMAL(void)
{
  *user_var_BASE = 10;
}

// HEX
static void prim_HEX(void)
{
  *user_var_BASE = 16;
}

// 4HEX
static void prim_4_HEX(void)
{
  USINGLE num = SP_POP();
  PFORTHSTRING pStr = forth_NUM_FORMAT(num, TRUE, 16, 4, '0', FALSE);
  SP_PUSH_PTR(pStr->chars);
  SP_PUSH(pStr->len);
}

// .
static void prim_DOT(void)
{
  forth_NUM_OUTPUT(SP_POP(), FALSE, g_bBASE, 0, '\0', TRUE);
}

// U.
static void prim_U_DOT(void)
{
  USINGLE num = SP_POP();
  forth_NUM_OUTPUT(num, TRUE, g_bBASE, 0, '\0', TRUE);
}

// ?
static void prim_QUESTION_MARK(void)
{
  PSINGLE pNum = SP_POP_PTR();
  forth_NUM_OUTPUT(*pNum, FALSE, g_bBASE, 0, '\0', TRUE);
}

// D.
static void prim_D_DOT(void)
{
  forth_NUM_OUTPUT(SP_POP_DOUBLE(), FALSE, g_bBASE, 0, '\0', TRUE);
}

// UD.
static void prim_U_D_DOT(void)
{
  UDOUBLE num = SP_POP_DOUBLE();
  forth_NUM_OUTPUT(num, TRUE, g_bBASE, 0, '\0', TRUE);
}

// F.
static void prim_F_DOT(void)
{
  FLOAT num = SP_POP_FLOAT();
  forth_FLOAT_OUTPUT(num, TRUE);
}

// SF.
static void prim_SF_DOT(void)
{
  SMALLFLOAT num = SP_POP_SMALLFLOAT();
  forth_FLOAT_OUTPUT(num, TRUE);
}

// D?
static void prim_D_QUESTION_MARK(void)
{
  PDOUBLE pNum = SP_POP_PTR();
  forth_NUM_OUTPUT(*pNum, FALSE, g_bBASE, 0, '\0', TRUE);
}

// F?
static void prim_F_QUESTION_MARK(void)
{
  PFLOAT pNum = SP_POP_PTR();
  forth_FLOAT_OUTPUT(*pNum, TRUE);
}

// DEC.
static void prim_DEC_DOT(void)
{
  forth_NUM_OUTPUT(SP_POP(), FALSE, 10, 0, '\0', TRUE);
}

// H.
static void prim_H_DOT(void)
{
  forth_NUM_OUTPUT(SP_POP(), FALSE, 16, 0, '\0', TRUE);
}

// B.
static void prim_B_DOT(void)
{
  forth_NUM_OUTPUT(SP_POP(), FALSE, 2, 0, '\0', TRUE);
}

// A.
static void prim_A_DOT(void)
{
  forth_ADDR_OUTPUT(SP_POP_ADDR(), TRUE);
}

// C.
static void prim_C_DOT(void)
{
  char c = SP_POP();
  forth_putc(c);
  forth_putc(' ');
}

// H?
static void prim_H_QUESTION_MARK(void)
{
  PSINGLE pNum = SP_POP_PTR();
  forth_NUM_OUTPUT(*pNum, FALSE, 16, 0, '\0', TRUE);
}

// A?
static void prim_A_QUESTION_MARK(void)
{
  PADDR pADDR = SP_POP_PTR();
  forth_ADDR_OUTPUT(*pADDR, TRUE);
}

// DH.
static void prim_D_H_DOT(void)
{
  forth_NUM_OUTPUT(SP_POP_DOUBLE(), FALSE, 16, 0, '\0', TRUE);
}

// DH?
static void prim_D_H_QUESTION_MARK(void)
{
  PDOUBLE pNum = SP_POP_PTR();
  forth_NUM_OUTPUT(*pNum, FALSE, 16, 0, '\0', TRUE);
}

// C?
static void prim_C_QUESTION_MARK(void)
{
  char *pChar = SP_POP_PTR();
  forth_putc(*pChar);
  forth_putc(' ');
}

// .R
static void prim_DOT_R(void)
{
  BYTE width = SP_POP();
  SINGLE num = SP_POP();
  PFORTHSTRING pStr = forth_NUM_FORMAT(num, FALSE, g_bBASE, width, ' ', FALSE);
  forth_putcount(pStr->len, pStr->chars);
}

// D.R
static void prim_D_DOT_R(void)
{
  BYTE width = SP_POP();
  DOUBLE num = SP_POP_DOUBLE();
  PFORTHSTRING pStr = forth_NUM_FORMAT(num, FALSE, g_bBASE, width, ' ', FALSE);
  forth_putcount(pStr->len, pStr->chars);
}

// .S
static void prim_DOT_S(void)
{
  for (PUSINGLE pUSINGLE = (PUSINGLE)pS0 - 1; pUSINGLE >= (PUSINGLE)pSP; pUSINGLE--)
    forth_NUM_OUTPUT(*pUSINGLE, TRUE, g_bBASE, 0, '\0', TRUE);
}

// H.S
static void prim_H_DOT_S(void)
{
  for (PUSINGLE pUSINGLE = (PUSINGLE)pS0 - 1; pUSINGLE >= (PUSINGLE)pSP; pUSINGLE--)
    forth_NUM_OUTPUT(*pUSINGLE, TRUE, 16, 0, '\0', TRUE);
}

// .RS
static void prim_DOT_RS(void)
{
  for (PUSINGLE pUSINGLE = (PUSINGLE)pR0 - 1; pUSINGLE >= (PUSINGLE)pRP; pUSINGLE--)
    forth_NUM_OUTPUT(*pUSINGLE, TRUE, g_bBASE, 0, '\0', TRUE);
}

// H.RS
static void prim_H_DOT_RS(void)
{
  for (PUSINGLE pUSINGLE = (PUSINGLE)pR0 - 1; pUSINGLE >= (PUSINGLE)pRP; pUSINGLE--)
    forth_NUM_OUTPUT(*pUSINGLE, TRUE, 16, 0, '\0', TRUE);
}

// ---------- Output definitions

// ++++++++++ Input definitions

// DIGIT
static void prim_DIGIT(void)
{
  SINGLE base = SP_POP();
  char c = (char)SP_POP();
  SINGLE num = forth_DIGIT(base, c);
  if (num >= 0)
    SP_PUSH(num);
  SP_PUSH(num >= 0);
}

// (NUM)
static void prim_BRACKET_NUM(void)
{
  PFORTHSTRING pStr = SP_POP_PTR();
  forth_NUM(pStr);
}

// NUM
static void vect_NUM(void)
{
  // first time: find NUM in the Dictionary
  static PFORTHDICTENT s_pDENum = NULL;
  if (s_pDENum == NULL)
    s_pDENum = forthFindDictEnt("NUM");

  // execute the FORTHDICTENT which the vector is assigned to do
  forth_EXECUTE_EXVEC(s_pDENum->pfa.addrs[0]);
}

// NUMBER
static void prim_NUMBER(void)
{
  PFORTHSTRING pStr = SP_POP_PTR();
  forth_NUMBER(pStr);
}

// (KEY)
static void prim_BRACKET_KEY(void)
{
#ifdef	READLINE
  SP_PUSH(readline_get_key());
#else
  forth_ERROR(err_NOT_IMPLEMENTED);
#endif
}

// KEY
static void vect_KEY(void)
{
  // first time: find KEY in the Dictionary
  static PFORTHDICTENT s_pDEKey = NULL;
  if (s_pDEKey == NULL)
    s_pDEKey = forthFindDictEnt("KEY");

  // execute the FORTHDICTENT which the vector is assigned to do
  forth_EXECUTE_EXVEC(s_pDEKey->pfa.addrs[0]);
}

// (WORD)
static void prim_BRACKET_WORD(void)
{
  char c = SP_POP();
  OFFSET offset;
  BYTE count;
  forth_BRACKET_WORD(c, &offset, &count);
  SP_PUSH_PTR(pIBUF + offset);
  SP_PUSH(count);
}

// WORD
static void prim_WORD(void)
{
  char c = SP_POP();
  forth_WORD(c);
  SP_PUSH_PTR(pWBFR);
}

// QUERY
static void prim_QUERY(void)
{
  forth_KEYBOARD_QUERY(NULL, TRUE);
}

// ---------- Input definitions

// ++++++++++ Conditional/Loop definitions

// [THEN]
static void prim_BRACKET_THEN(void)
{
  // pop the primitive_BRACKET_IF left by [IF]/[ELSE]
  primitive_t prim = RP_POP();
  if (prim != primitive_BRACKET_IF && prim != primitive_BRACKET_ELSE)
  {
    forth_ERROR(err_COND_NOT_PAIRED);
    return;
  }
}

// [ELSE]
static void prim_BRACKET_ELSE(void)
{
  // pop the primitive_BRACKET_IF left by [IF]
  primitive_t prim = RP_POP();
  if (prim != primitive_BRACKET_IF)
  {
    forth_ERROR(err_COND_NOT_PAIRED);
    return;
  }
  // push primitive_BRACKET_ELSE, for [THEN] to find
  RP_PUSH(primitive_BRACKET_ELSE);
  // skip through the Input Stream till [THEN]
  do
  {
    forth_WORD(' ');
    PFORTHDICTENT pDE = forthFindDictEnt(pWBFR->chars);
    prim = (pDE != NULL && pDE->cfa == cfe_primitive) ? pDE->pfa.primitives[0] : (primitive_t)-1;
  } while (prim != primitive_BRACKET_THEN);
  // execute [THEN]
  prim_BRACKET_THEN();
}

// [IF]
static void prim_BRACKET_IF(void)
{
  // push primitive_BRACKET_IF, for [ELSE]/[THEN] to find
  RP_PUSH(primitive_BRACKET_IF);
  // pop the boolean flag
  bool f = SP_POP();
  // terminate if flag is true (i.e. execute the following words)
  if (f)
    return;
  // skip through the Input Stream till [ELSE]/[THEN]
  primitive_t prim;
  do
  {
    forth_WORD(' ');
    PFORTHDICTENT pDE = forthFindDictEnt(pWBFR->chars);
    prim = (pDE != NULL && pDE->cfa == cfe_primitive) ? pDE->pfa.primitives[0] : (primitive_t)-1;
  } while (prim != primitive_BRACKET_ELSE && prim != primitive_BRACKET_THEN);
  // execute [ELSE]/[THEN]
  if (prim == primitive_BRACKET_ELSE)
  {
    // push primitive_BRACKET_ELSE, for [THEN] to find
    RP_REPLACE(primitive_BRACKET_ELSE);
    // terminate (i.e. execute the following words)
    return;
  }
  else if (prim == primitive_BRACKET_THEN)
    prim_BRACKET_THEN();
}

// (JUMP)
static void prim_BRACKET_JUMP(void)
{
  // pop the From ADDR from the stack
  PADDR pFrom = SP_POP_PTR();
  CHECK_DICT_PTR(pFrom);
  // pop the To ADDR from the stack
  PADDR pTo = SP_POP_PTR();
  CHECK_DICT_PTR(pTo);
  // calculate offset from From to To (could be negative or positive)
  OFFSET offset = pTo - pFrom;
  // store it into Dictionary at From
  *pFrom = offset;
}

// BACK
static void prim_BACK(void)
{
  // pDP is current first free byte at top of Dictionary
  PADDR pFrom = (PADDR)pDP;
  // allocate for OFFSET at pDP
  forthDictAllot(sizeof(OFFSET));
  // push the From ADDR to the stack
  SP_PUSH_PTR(pFrom);
  // call (JUMP) to calculate and store the jump offset into the Dictionary at From
  prim_BRACKET_JUMP();
}

// BRANCH
static void prim_BRANCH(void)
{
  // fetch the inline OFFSET from the CALLER
  CHECK_CALLER();
  OFFSET offset = *(POFFSET)CALLER_INLINE_ADDRS();
  // advance the CALLER by OFFSET
  CALLER_ADVANCE(offset);
}

// 0BRANCH
static void prim_ZERO_BRANCH(void)
{
  // fetch the inline OFFSET from the CALLER
  CHECK_CALLER();
  OFFSET offset = *(POFFSET)CALLER_INLINE_ADDRS();
  if (!SP_POP())
  {
    // false => advance the CALLER by OFFSET (i.e. follow the branch)
    CALLER_ADVANCE(offset);
  }
  else
  {
    // advance the CALLER over the OFFSET (i.e. skip the branch)
    CALLER_ADVANCE(1);
  }
}

// IF
static void prim_IF(void)
{
  // Compilation Mode only
  if (!COMPILATION_MODE())
  {
    forth_ERROR(err_COMPILATION_ONLY);
    return;
  }
  // Compile 0BRANCH into Dictionary
  forthDictStoreFindPrimitive(primitive_ZERO_BRANCH);
  // get BACK to pop a temporary ADDR (IF's HERE) from stack and compile offset into Dictionary
  // the offset will be overwritten when ELSE/THEN is subsequently encountered
  // pDP is current first free byte at top of Dictionary
  PADDR pHere = (PADDR)pDP;
  SP_PUSH_PTR(pHere);
  prim_BACK();
  // push IF's HERE to the stack, followed by primitive_IF, for ELSE/THEN to find
  SP_PUSH_PTR(pHere);
  SP_PUSH(primitive_IF);
}

// ELSE
static void prim_ELSE(void)
{
  // Compilation Mode only
  if (!COMPILATION_MODE())
  {
    forth_ERROR(err_COMPILATION_ONLY);
    return;
  }
  // pop the primitive_IF from the stack, left by IF
  primitive_t prim = SP_POP();
  if (prim != primitive_IF)
  {
    forth_ERROR(err_COND_NOT_PAIRED);
    return;
  }
  // pop the ADDR left by IF (IF's HERE) from the stack
  // this is where the offset following IF's 0BRANCH was stored
  PADDR pIf = SP_POP_PTR();

  // First make the IF code jump over ELSE to THEN
  // Compile BRANCH into Dictionary
  forthDictStoreFindPrimitive(primitive_BRANCH);
  // get BACK to pop a temporary ADDR (ELSE's HERE) from stack and compile offset into Dictionary
  // the offset will be overwritten when THEN is subsequently encountered
  // pDP is current first free byte at top of Dictionary
  PADDR pHere = (PADDR)pDP;
  SP_PUSH_PTR(pHere);
  prim_BACK();
  // push ELSE's HERE to the stack, followed by primitive_ELSE, for THEN to find
  SP_PUSH_PTR(pHere);
  SP_PUSH(primitive_ELSE);

  // Now do IF's 0BRANCH forward to after ELSE (HERE)
  // pDP is current first free byte at top of Dictionary
  pHere = (PADDR)pDP;
  // push the To ADDR (ELSE's HERE) to the stack
  SP_PUSH_PTR(pHere);
  // push the From ADDR (IF's HERE) to the stack
  SP_PUSH_PTR(pIf);
  // call (JUMP) to calculate and store the jump offset into the Dictionary
  prim_BRACKET_JUMP();
}

// THEN
static void prim_THEN(void)
{
  // Compilation Mode only
  if (!COMPILATION_MODE())
  {
    forth_ERROR(err_COMPILATION_ONLY);
    return;
  }
  // pop the primitive_IF from the stack, left by IF/ELSE
  primitive_t prim = SP_POP();
  if (prim != primitive_IF && prim != primitive_ELSE)
  {
    forth_ERROR(err_COND_NOT_PAIRED);
    return;
  }
  // pop the ADDR left by IF/ELSE (IF/ELSE's HERE) from the stack
  // this is where the offset following IF's 0BRANCH/ELSE's BRANCH was stored
  PADDR pIfElse = SP_POP_PTR();

  // Now do IF's 0BRANCH/ELSE's BRANCH forward to after THEN (HERE)
  // pDP is current first free byte at top of Dictionary
  PADDR pHere = (PADDR)pDP;
  // push the To ADDR (THEN's HERE) to the stack
  SP_PUSH_PTR(pHere);
  // push the From ADDR (IF/ELSE's HERE) to the stack
  SP_PUSH_PTR(pIfElse);
  // call (JUMP) to calculate and store the jump offset into the Dictionary
  prim_BRACKET_JUMP();
}

// BEGIN
static void prim_BEGIN(void)
{
  // Compilation Mode only
  if (!COMPILATION_MODE())
  {
    forth_ERROR(err_COMPILATION_ONLY);
    return;
  }
  // pDP is current first free byte at top of Dictionary
  PADDR pHere = (PADDR)pDP;
  // push BEGIN's HERE to the stack, followed by primitive_BEGIN, for AGAIN/UNTIL/WHILE/REPEAT to find
  SP_PUSH_PTR(pHere);
  SP_PUSH(primitive_BEGIN);
}

// AGAIN
static void prim_AGAIN(void)
{
  // Compilation Mode only
  if (!COMPILATION_MODE())
  {
    forth_ERROR(err_COMPILATION_ONLY);
    return;
  }
  // pop the primitive_BEGIN from the stack, left by BEGIN
  primitive_t prim = SP_POP();
  if (prim != primitive_BEGIN)
  {
    forth_ERROR(err_COND_NOT_PAIRED);
    return;
  }
  // Compile BRANCH into Dictionary
  forthDictStoreFindPrimitive(primitive_BRANCH);
  // get BACK to pop BEGIN's HERE from stack and compile offset into Dictionary
  prim_BACK();
}

// UNTIL
static void prim_UNTIL(void)
{
  // Compilation Mode only
  if (!COMPILATION_MODE())
  {
    forth_ERROR(err_COMPILATION_ONLY);
    return;
  }
  // pop the primitive_BEGIN from the stack, left by BEGIN
  primitive_t prim = SP_POP();
  if (prim != primitive_BEGIN)
  {
    forth_ERROR(err_COND_NOT_PAIRED);
    return;
  }
  // Compile 0BRANCH into Dictionary
  forthDictStoreFindPrimitive(primitive_ZERO_BRANCH);
  // get BACK to pop BEGIN's HERE from stack and compile offset into Dictionary
  prim_BACK();
}

// WHILE
static void prim_WHILE(void)
{
  // Compilation Mode only
  if (!COMPILATION_MODE())
  {
    forth_ERROR(err_COMPILATION_ONLY);
    return;
  }
  // check for the primitive_BEGIN from the stack, left by BEGIN
  primitive_t prim = *SP_0();
  if (prim != primitive_BEGIN)
  {
    forth_ERROR(err_COND_NOT_PAIRED);
    return;
  }
  // Compile 0BRANCH into Dictionary
  forthDictStoreFindPrimitive(primitive_ZERO_BRANCH);
  // get BACK to pop a temporary ADDR (WHILE's HERE) from stack and compile offset into Dictionary
  // the offset will be overwritten when REPEAT is subsequently encountered
  // pDP is current first free byte at top of Dictionary
  PADDR pHere = (PADDR)pDP;
  SP_PUSH_PTR(pHere);
  prim_BACK();
  // push WHILE's HERE to the stack, followed by primitive_WHILE, for REPEAT to find
  SP_PUSH_PTR(pHere);
  SP_PUSH(primitive_WHILE);
}

// REPEAT
static void prim_REPEAT(void)
{
  // Compilation Mode only
  if (!COMPILATION_MODE())
  {
    forth_ERROR(err_COMPILATION_ONLY);
    return;
  }
  // pop the primitive_WHILE from the stack, left by WHILE
  primitive_t prim = SP_POP();
  if (prim != primitive_WHILE)
  {
    forth_ERROR(err_COND_NOT_PAIRED);
    return;
  }
  // pop the ADDR left by WHILE (WHILE's HERE) from the stack
  // this is where the offset following WHILE's 0BRANCH was stored
  // save this up for use later on
  PADDR pWhile = SP_POP_PTR();

  // First do REPEAT's BRANCH back to BEGIN
  // pop the primitive_BEGIN from the stack, left by BEGIN
  prim = SP_POP();
  if (prim != primitive_BEGIN)
  {
    forth_ERROR(err_COND_NOT_PAIRED);
    return;
  }
  // Compile BRANCH into Dictionary
  forthDictStoreFindPrimitive(primitive_BRANCH);
  // get BACK to pop BEGIN's HERE from stack and compile offset into Dictionary
  prim_BACK();

  // Now do WHILE's 0BRANCH forward to after REPEAT (HERE)
  // pDP is current first free byte at top of Dictionary
  PADDR pHere = (PADDR)pDP;
  // push the From ADDR (REPEAT's HERE) to the stack
  SP_PUSH_PTR(pHere);
  // push the To ADDR (WHILE's HERE) to the stack
  SP_PUSH_PTR(pWhile);
  // call (JUMP) to calculate and store the jump offset into the Dictionary
  prim_BRACKET_JUMP();
}

// I
static void prim_I(void)
{
  CHECK_RP_HAS_2();
  // fetch the current counter value from the Return Stack and push it to the Computation Stack
  SP_PUSH(*RP_0());
}

// J
static void prim_J(void)
{
  CHECK_RP_HAS_N(4);
  // fetch the previous counter value from the Return Stack and push it to the Computation Stack
  SP_PUSH(*RP_N(3));
}

// (DO)
static void prim_BRACKET_DO(void)
{
  // fetch the initial & limit values from Computation the Stack
  SINGLE initial = SP_POP();
  SINGLE limit = SP_POP();
  // push the limit & initial/current counter values to the Return Stack
  RP_PUSH(limit);
  RP_PUSH(initial);
}

// (+LOOP)
static void prim_BRACKET_PLUS_LOOP(void)
{
  CHECK_RP_HAS_2();
  // fetch the current & limit values from the Stack
  SINGLE current = RP_POP();
  SINGLE limit = *RP_0();
  // pop the increment from the Computation Stack and increment current by that
  SINGLE increment = SP_POP();
  current += increment;
  // fetch the inline OFFSET from the CALLER
  CHECK_CALLER();
  OFFSET offset = *(POFFSET)CALLER_INLINE_ADDRS();
  // for a positive increment the loop continues until current >= limit
  // however, for a negative increment the loop continues until current < limit
  if ((increment >= 0) ? current < limit : current >= limit)
  {
    // continue the loop
    // push new current value back to Return Stack
    RP_PUSH(current);
    // advance the CALLER by OFFSET (i.e. follow the branch)
    CALLER_ADVANCE(offset);
  }
  else
  {
    // terminate the loop
    // pop loop limit off the Return Stack
    RP_POP();
    // advance the CALLER over the OFFSET (i.e. skip the branch)
    CALLER_ADVANCE(1);
  }
}

// (LOOP)
static void prim_BRACKET_LOOP(void)
{
  CHECK_RP_HAS_2();
  // fetch the current & limit values from the Stack
  SINGLE current = RP_POP();
  SINGLE limit = *RP_0();
  // increment current by 1
  current++;
  // fetch the inline OFFSET from the CALLER
  CHECK_CALLER();
  OFFSET offset = *(POFFSET)CALLER_INLINE_ADDRS();
  if (current < limit)
  {
    // continue the loop
    // push new current value back to Return Stack
    RP_PUSH(current);
    // advance the CALLER by OFFSET (i.e. follow the branch)
    CALLER_ADVANCE(offset);
  }
  else
  {
    // terminate the loop
    // pop loop limit off the Return Stack
    RP_POP();
    // advance the CALLER over the OFFSET (i.e. skip the branch)
    CALLER_ADVANCE(1);
  }
}

// (ULOOP)
static void prim_BRACKET_U_LOOP(void)
{
  CHECK_RP_HAS_2();
  // fetch the current & limit values from the Stack
  USINGLE current = RP_POP();
  USINGLE limit = *RP_0();
  // increment current by 1
  current++;
  // fetch the inline OFFSET from the CALLER
  CHECK_CALLER();
  OFFSET offset = *(POFFSET)CALLER_INLINE_ADDRS();
  if (current < limit)
  {
    // continue the loop
    // push new current value back to Return Stack
    RP_PUSH(current);
    // advance the CALLER by OFFSET (i.e. follow the branch)
    CALLER_ADVANCE(offset);
  }
  else
  {
    // terminate the loop
    // pop loop limit off the Return Stack
    RP_POP();
    // advance the CALLER over the OFFSET (i.e. skip the branch)
    CALLER_ADVANCE(1);
  }
}

// DO
static void prim_DO(void)
{
  // Compilation Mode only
  if (!COMPILATION_MODE())
  {
    forth_ERROR(err_COMPILATION_ONLY);
    return;
  }
  // Compile (DO) into Dictionary
  forthDictStoreFindPrimitive(primitive_BRACKET_DO);
  // pDP is current first free byte at top of Dictionary
  PADDR pHere = (PADDR)pDP;
  // push DO's HERE to the stack, followed by primitive_DO, for LOOP to find
  SP_PUSH_PTR(pHere);
  SP_PUSH(primitive_DO);
}

// LEAVE
static void prim_LEAVE(void)
{
  CHECK_RP_HAS_2();
  // set the loop limit to the current counter value on the Return Stack
  *RP_1() = *RP_0();
}

// +LOOP
static void prim_PLUS_LOOP(void)
{
  // Compilation Mode only
  if (!COMPILATION_MODE())
  {
    forth_ERROR(err_COMPILATION_ONLY);
    return;
  }
  // pop the primitive_DO from the stack, left by DO
  primitive_t prim = SP_POP();
  if (prim != primitive_DO)
  {
    forth_ERROR(err_COND_NOT_PAIRED);
    return;
  }
  // Compile (LOOP) into Dictionary
  forthDictStoreFindPrimitive(primitive_BRACKET_PLUS_LOOP);
  // get BACK to pop DO's HERE from stack and compile offset into Dictionary
  prim_BACK();
}

// LOOP
static void prim_LOOP(void)
{
  // Compilation Mode only
  if (!COMPILATION_MODE())
  {
    forth_ERROR(err_COMPILATION_ONLY);
    return;
  }
  // pop the primitive_DO from the stack, left by DO
  primitive_t prim = SP_POP();
  if (prim != primitive_DO)
  {
    forth_ERROR(err_COND_NOT_PAIRED);
    return;
  }
  // Compile (LOOP) into Dictionary
  forthDictStoreFindPrimitive(primitive_BRACKET_LOOP);
  // get BACK to pop DO's HERE from stack and compile offset into Dictionary
  prim_BACK();
}

// ULOOP
static void prim_U_LOOP(void)
{
  // Compilation Mode only
  if (!COMPILATION_MODE())
  {
    forth_ERROR(err_COMPILATION_ONLY);
    return;
  }
  // pop the primitive_DO from the stack, left by DO
  primitive_t prim = SP_POP();
  if (prim != primitive_DO)
  {
    forth_ERROR(err_COND_NOT_PAIRED);
    return;
  }
  // Compile (ULOOP) into Dictionary
  forthDictStoreFindPrimitive(primitive_BRACKET_U_LOOP);
  // get BACK to pop DO's HERE from stack and compile offset into Dictionary
  prim_BACK();
}

// ---------- Conditional/Loop definitions

// ++++++++++ Dictionary definitions

// CFA
static void prim_CFA(void)
{
  SP_PUSH_ADDR(PFA_TO_CFA(SP_POP()));
}

// LFA
static void prim_LFA(void)
{
  SP_PUSH_ADDR(PFA_TO_LFA(SP_POP()));
}

// NFA
static void prim_NFA(void)
{
  SP_PUSH_ADDR(PFA_TO_NFA(SP_POP()));
}

// PFA
static void prim_PFA(void)
{
  SP_PUSH_ADDR(NFA_TO_PFA(SP_POP()));
}

// SMUDGE
static void prim_SMUDGE(void)
{
  pLAST->nfa.flag.smudge = !pLAST->nfa.flag.smudge;
}

// LAST
static void prim_LAST(void)
{
  SP_PUSH_PTR(pLAST);
}

// DEFINITIONS
static void prim_DEFINITIONS(void)
{
  CHECK_DICT_PTR(ADDR_TO_PTR(pCONTEXT->top_dictent));
  // set CURRENT Vocabulary to CONTEXT
  *user_var_CURRENT = *user_var_CONTEXT;
}

// IMMEDIATE
static void prim_IMMEDIATE(void)
{
  CHECK_DICT_PTR(pLAST);
  pLAST->nfa.flag.immediate = TRUE;
}

// (VOCABULARY)
static void prim_BRACKET_VOCABULARY(void)
{
  // fetch the inline FORTHVOCAB from the CALLER
  CHECK_CALLER();
  PFORTHVOCAB pVocab = (PFORTHVOCAB)CALLER_INLINE_ADDRS();
  // set CONTEXT
  *user_var_CONTEXT = PTR_TO_ADDR(pVocab);
  // advance the CALLER over the FORTHVOCAB
  CALLER_ADVANCE(sizeof(FORTHVOCAB) / sizeof(ADDR));
}

// VOCABULARY
static void prim_VOCABULARY(void)
{
  // verify in Execution Mode
  if (!EXECUTION_MODE())
  {
    forth_ERROR(err_EXECUTION_ONLY);
    return;
  }
  // get the next word as the Dictionary Entry name
  forth_WORD(' ');
  // create a Colon Definition
  forthCreateDictEnt_colon(pWBFR->chars);
  // Compile (VOCABULARY) into Dictionary
  forthDictStoreFindPrimitive(primitive_BRACKET_VOCABULARY);
  // set it as a new VOCABULARY (sets pVOC_LINK to the VOCAB inside the VOCABULARY word)
  // (also creates the dummy "blank" Dictionary Entry as the first word in the Vocabulary)
  forthMakeDictEntNewVocab();
}

// FORTH
static void prim_FORTH(void)
{
  // first time: find FORTH in the Dictionary
  static PFORTHDICTENT s_pDEForth = NULL;
  if (s_pDEForth == NULL)
    s_pDEForth = forthFindDictEntPrimitive(primitive_FORTH);

  PFORTHVOCAB pVocab = (PFORTHVOCAB)&s_pDEForth->pfa.addrs[1];
  // set CONTEXT
  *user_var_CONTEXT = PTR_TO_ADDR(pVocab);
}

// ,
static void prim_COMMA(void)
{
  forthDictStoreSingle(SP_POP());
}

// D,
static void prim_D_COMMA(void)
{
  forthDictStoreDouble(SP_POP_DOUBLE());
}

// F,
static void prim_F_COMMA(void)
{
  forthDictStoreFloat(SP_POP_FLOAT());
}

// SF,
static void prim_SF_COMMA(void)
{
  forthDictStoreSmallFloat(SP_POP_SMALLFLOAT());
}

// C,
static void prim_C_COMMA(void)
{
  forthDictStoreByte(SP_POP());
}

// '
static void prim_TICK(void)
{
  // get the next word as the Dictionary Entry name
  forth_WORD(' ');
  PFORTHDICTENT pDE = forthMustFindDictEntForWord(pWBFR);
  ADDR addr = PTR_TO_ADDR(&pDE->pfa);
  if (EXECUTION_MODE())
  {
    // push ADDR to stack
    SP_PUSH_ADDR(addr);
  }
  else
  {
    // Compile ALIT into Dictionary
    forthDictStoreFindPrimitive(primitive_A_LIT);
    // store ADDR into Dictionary
    forthDictStoreAddr(addr);
  }
}

// DICTDUMP
static void prim_DICTDUMP(void)
{
  PFORTHDICTENT pDE = SP_POP_PTR();
  forthDumpDictEnt(pDE);
}

// VDUMP
static void prim_VDUMP(void)
{
  forthDumpDictionary();
}

// ID.
static void prim_ID_DOT(void)
{
  PFORTHDICTENT pDE = SP_POP_PTR();
  CHECK_DICT_PTR(pDE);
  fprintf(g_fpOUT, "%s ", pDE->nfa.name);
}

// VLIST
static void prim_VLIST(void)
{
  // current top-most Dictionary Entry for Searches
  PFORTHDICTENT pDE = forthStartSearchDictEnt();
  // walk down FORTH_DICTIONARY
  while (pDE != NULL)
  {
    if (!forthDictEntIsBlankName(pDE->nfa.name))
    {
      // highlight word in green if it is a Vocabulary
      bool isVocabulary = forthDictEntIsVocabulary(pDE);
#ifdef READLINE
      if (isVocabulary)
        readline_output_color(stdout, 2);
#endif
      fprintf(stdout, "%s ", pDE->nfa.name);
#ifdef READLINE
      if (isVocabulary)
        readline_output_color_white(stdout);
#endif
    }
    pDE = DP_TO_PREV_DP(pDE);
  }
  forth_CR();
}

// (FINDPREV)
static void prim_BRACKET_FIND_PREV(void)
{
  PFORTHDICTENT pDE = SP_POP_PTR();
  pDE = DP_TO_PREV_DP(pDE);
  SP_PUSH_PTR(pDE);
}

// (FIND)
static void prim_BRACKET_FIND(void)
{
  PFORTHDICTENT pDEFrom = SP_POP_PTR();
  PFORTHSTRING pStr = SP_POP_PTR();
  PFORTHDICTENT pDE = forthFindDictEntFrom(pDEFrom, pStr->chars, pStr->len);
  if (pDE == NULL)
    SP_PUSH(FALSE);
  else
  {
    SP_PUSH_PTR(pDE);
    SP_PUSH(pDE->nfa.flag.len);
    SP_PUSH(TRUE);
  }
}

// -FIND
static void prim_DASH_FIND(void)
{
  PFORTHDICTENT pDEFrom = SP_POP_PTR();
  // get the next word as the Dictionary Entry name
  forth_WORD(' ');
  PFORTHDICTENT pDE = forthFindDictEntFrom(pDEFrom, pWBFR->chars, -1);
  if (pDE == NULL)
    SP_PUSH(FALSE);
  else
  {
    SP_PUSH_PTR(pDE);
    SP_PUSH(pDE->nfa.flag.len);
    SP_PUSH(TRUE);
  }
}

// FIND
static void prim_FIND(void)
{
  // get the next word as the Dictionary Entry name
  forth_WORD(' ');
  PFORTHDICTENT pDE = forthFindDictEnt(pWBFR->chars);
  if (pDE == NULL)
    SP_PUSH_ADDR(0);
  else
    SP_PUSH_PTR(pDE);
}

// FIND-VOCABULARY
static void prim_FIND_VOCABULARY(void)
{
  PFORTHDICTENT pDE = SP_POP_PTR();
  PFORTHDICTENT pDEVocab = forthFindDictEntVocabulary(pDE);
  SP_PUSH_PTR(pDEVocab);
}

// PRUNE
static void prim_PRUNE(void)
{
  PFORTHDICTENT pDE = SP_POP_PTR();
  forthPruneDictEnt(pDE);
}

// (FORGET)
static void prim_BRACKET_FORGET(void)
{
  PFORTHDICTENT pDE = SP_POP_PTR();
  if (pDE == NULL)
  {
    forth_ERROR(err_NOT_IN_CURRENT_VOCAB);
    return;
  }
  forthForgetDictEnt(pDE);
}

// -FORGET
static void prim_DASH_FORGET(void)
{
  PFORTHDICTENT pDEFrom = SP_POP_PTR();
  PFORTHSTRING pStr = SP_POP_PTR();
  PFORTHDICTENT pDE = forthFindDictEntFrom(pDEFrom, pStr->chars, pStr->len);
  if (pDE == NULL)
  {
    forth_ERROR(err_NOT_IN_CURRENT_VOCAB);
    return;
  }
  forthForgetDictEnt(pDE);
}

// FORGET
static void prim_FORGET(void)
{
  // get the next word as the Dictionary Entry name
  forth_WORD(' ');
  PFORTHDICTENT pDE = forthFindDictEnt(pWBFR->chars);
  if (pDE == NULL)
  {
    forth_ERROR(err_NOT_IN_CURRENT_VOCAB);
    return;
  }
  forthForgetDictEnt(pDE);
}

// HERE
static void prim_HERE(void)
{
  SP_PUSH_PTR(pDP);
}

// ALLOT
static void prim_ALLOT(void)
{
  SINGLE num = SP_POP();
  forthDictAllot(num);
}

// COMPILE
static void prim_COMPILE(void)
{
  // fetch the inline ADDR from the CALLER
  CHECK_CALLER();
  ADDR addr = *CALLER_INLINE_ADDRS();
  // advance the CALLER over the ADDR
  CALLER_ADVANCE(1);
  // store address into Dictionary
  forthDictStoreAddr(addr);
}

// (CREATE)
static void prim_BRACKET_CREATE(void)
{
  // get the next word as the Dictionary Entry name
  forth_WORD(' ');
  // create a Create Definition
  forthCreateDictEnt_create(pWBFR->chars);
}

// CREATE
static void vect_CREATE(void)
{
  // first time: find CREATE in the Dictionary
  static PFORTHDICTENT s_pDECreate = NULL;
  if (s_pDECreate == NULL)
    s_pDECreate = forthFindDictEnt("CREATE");

  // execute the FORTHDICTENT which the vector is assigned to do
  forth_EXECUTE_EXVEC(s_pDECreate->pfa.addrs[0]);
}

// NOVEC
static void prim_NOVEC(void)
{
  forth_ERROR(err_BAD_EXVEC);
  return;
}

// DOES>
static void prim_DOES_GREATER(void)
{
  // error if pLAST is presently an incomplete Dictionary Entry
  if (!forthCheckCompleteLastDictEnt())
    return;
  // make sure it is a Create Definition
  if (pLAST == NULL || pLAST->cfa != cfe_create)
  {
    forth_ERROR(err_COMPILATION_ONLY);
    return;
  }
  // change it to a Does> Definition
  pLAST->cfa = cfe_does_gt;
  // Compile address of this Dictionary Entry's DOES> into definition
  // fetch the inline ADDR from the CALLER
  CHECK_CALLER();
  ADDR addr = PTR_TO_ADDR(CALLER_INLINE_ADDRS());
  // store address into Dictionary at pfa.addrs[0] (the address reserved by <BUILDS)
  pLAST->pfa.addrs[0] = addr;
  // finally make the CALLER EXIT this definition now
  // (i.e. skip all the words after DOES> to end of definition;
  // the code after DOES> executes when the <BUILDS cfe_does_gt word executes)
  CALLER_ADVANCE_TO_EXIT();
}

// <BUILDS
static void prim_LESS_BUILDS(void)
{
  // call CREATE
  vect_CREATE();
  // reserve pfa.addrs[0] for DOES> in the Dictionary Entry
  forthDictStoreAddr(0);
}

// EXVEC:
static void prim_EXVEC_COLON(void)
{
  // call CREATE
  vect_CREATE();
  // change Dictionary Entry's CFA to cfe_exvec
  pLAST->cfa = cfe_exvec;
  // Compile NOVEC into definition
  forthDictStoreFindPrimitive(primitive_NOVEC);
}

// DOVEC
static void prim_DOVEC(void)
{
  // pop the ADDR of the Dictionary Entry left by TO-DO
  PFORTHDICTENT pDEToDo = SP_POP_PTR();
  CHECK_DICT_PTR(pDEToDo);
  // pop the ADDR of the Dictionary Entry left by ASSIGN
  PFORTHDICTENT pDEAssign = SP_POP_PTR();
  CHECK_DICT_PTR(pDEAssign);
  // check that it was created via EXVEC:
  if (pDEAssign->cfa != cfe_exvec)
  {
    forth_ERROR(err_BAD_EXVEC);
    return;
  }
  // set ASSIGNed Dictionary Entry TO-DO Dictionary Entry
  pDEAssign->pfa.addrs[0] = PTR_TO_ADDR(pDEToDo);
}

// ASSIGN
static void prim_ASSIGN(void)
{
  // get the next word as the Dictionary Entry name
  forth_WORD(' ');
  PFORTHDICTENT pDE = forthMustFindDictEntForWord(pWBFR);
  // check it was created via EXVEC:
  if (pDE->cfa != cfe_exvec)
  {
    forth_ERROR(err_BAD_EXVEC);
    return;
  }
  // push the ADDR of the Dictionary Entry for TO-DO to pick up
  SP_PUSH_PTR(pDE);
}

// TO-DO
static void prim_TO_DO(void)
{
  // get the next word as the Dictionary Entry name
  forth_WORD(' ');
  PFORTHDICTENT pDEToDo = forthMustFindDictEntForWord(pWBFR);
  SP_PUSH_PTR(pDEToDo);
  // call DOVEC
  prim_DOVEC();
}

// CONSTANT
static void prim_CONSTANT(void)
{
  // pop the value from the Computation Stack
  SINGLE num = SP_POP();
  // call CREATE
  vect_CREATE();
  // store it into the Dictionary Entry
  forthDictStoreSingle(num);
  // change Dictionary Entry's CFA to cfe_constant
  pLAST->cfa = cfe_constant;
}

// DCONSTANT
static void prim_D_CONSTANT(void)
{
  // pop the value from the Computation Stack
  DOUBLE num = SP_POP_DOUBLE();
  // call CREATE
  vect_CREATE();
  // store it into the Dictionary Entry
  forthDictStoreDouble(num);
  // change Dictionary Entry's CFA to cfe_double_constant
  pLAST->cfa = cfe_double_constant;
}

// FCONSTANT
static void prim_F_CONSTANT(void)
{
  // pop the value from the Computation Stack
  FLOAT num = SP_POP_FLOAT();
  // call CREATE
  vect_CREATE();
  // store it into the Dictionary Entry
  forthDictStoreFloat(num);
  // change Dictionary Entry's CFA to cfe_float_constant
  pLAST->cfa = cfe_float_constant;
}

// $CONSTANT
static void prim_DOLLAR_CONSTANT(void)
{
  // pop the value from the Computation Stack
  byte len = SP_POP();
  const char *pChars = SP_POP_PTR();
  // call CREATE
  vect_CREATE();
  // store it into the Dictionary Entry
  forthDictStoreForthString(len, pChars);
  // change Dictionary Entry's CFA to cfe_string_constant
  pLAST->cfa = cfe_string_constant;
}

// USER
static void prim_USER(void)
{
  // pop the value from the Computation Stack
  SINGLE num = SP_POP();
  // call CREATE
  vect_CREATE();
  // store it into the Dictionary Entry
  forthDictStoreByte(num);
  // change Dictionary Entry's CFA to cfe_user_variable
  pLAST->cfa = cfe_user_variable;
}

// VARIABLE
static void prim_VARIABLE(void)
{
  // call CREATE
  vect_CREATE();
  // store 0 into the Dictionary Entry
  forthDictStoreSingle(0);
  // change Dictionary Entry's CFA to cfe_variable
  pLAST->cfa = cfe_variable;
}

// DVARIABLE
static void prim_D_VARIABLE(void)
{
  // call CREATE
  vect_CREATE();
  // store 0 into the Dictionary Entry
  forthDictStoreDouble(0);
  // change Dictionary Entry's CFA to cfe_double_variable
  pLAST->cfa = cfe_double_variable;
}

// FVARIABLE
static void prim_F_VARIABLE(void)
{
  // call CREATE
  vect_CREATE();
  // store 0 into the Dictionary Entry
  forthDictStoreFloat(0.0);
  // change Dictionary Entry's CFA to cfe_float_variable
  pLAST->cfa = cfe_float_variable;
}

// $VARIABLE
static void prim_DOLLAR_VARIABLE(void)
{
  // pop the value from the Computation Stack
  byte maxlen = SP_POP();
  // call CREATE
  vect_CREATE();
  // store "" into the Dictionary Entry
  forthDictStoreForthMaxLenString(maxlen, 0, "");
  // change Dictionary Entry's CFA to cfe_string_variable
  pLAST->cfa = cfe_string_variable;
}

// (LOCALVAR)
static void prim_BRACKET_LOCALVAR(void)
{
  // fetch the inline SINGLE from the CALLER
  CHECK_CALLER();
  SINGLE index = *(PSINGLE)CALLER_INLINE_ADDRS();
  // advance the CALLER over the SINGLE
  CALLER_ADVANCE(1);
  // push local variable's storage location to the Computation Stack
  SP_PUSH_PTR(RP_LOCAL_VAR(index));
}

// (DECLARE-LOCALVAR)
static void prim_BRACKET_DECLARE_LOCALVAR(void)
{
  // fetch the inline SINGLE from the CALLER
  CHECK_CALLER();
  SINGLE index = *(PSINGLE)CALLER_INLINE_ADDRS();
  // advance the CALLER over the SINGLE
  CALLER_ADVANCE(1);
  // allocate local variable's storage location on the Return Stack
  RP_ALLOT_LOCAL_VAR(index);
  // pop Computation Stack and store into local variable's storage location
  SINGLE num = SP_POP();
  *RP_LOCAL_VAR(index) = num;
}

// (LOCAL)
static void prim_BRACKET_LOCAL(void)
{
  // Compilation Mode only
  if (!COMPILATION_MODE())
  {
    forth_ERROR(err_COMPILATION_ONLY);
    return;
  }
  // get the next word as the local variable's Dictionary Entry name
  forth_WORD(' ');
  SINGLE index = g_LOCALSINDEX;
  // create Compiler LOCAL variable
  forthCreateDictEnt_local_variable(pWBFR->chars);
  // Compile (DECLARE-LOCALVAR) into Dictionary
  forthDictStoreFindPrimitive(primitive_BRACKET_DECLARE_LOCALVAR);
  // store index into Dictionary
  forthDictStoreSingle(index);
}

// (TO)
static void prim_BRACKET_TO(void)
{
  // fetch the inline ADDR from the CALLER
  CHECK_CALLER();
  ADDR addr = *CALLER_INLINE_ADDRS();
  // advance the CALLER over the ADDR
  CALLER_ADVANCE(1);
  *(PSINGLE)ADDR_TO_PTR(addr) = SP_POP();
}

// (TO-LOCALVAR)
static void prim_BRACKET_TO_LOCALVAR(void)
{
  // fetch the inline SINGLE from the CALLER
  CHECK_CALLER();
  SINGLE index = *(PSINGLE)CALLER_INLINE_ADDRS();
  // advance the CALLER over the SINGLE
  CALLER_ADVANCE(1);
  *RP_LOCAL_VAR(index) = SP_POP();
}

// TO
static void prim_TO(void)
{
  // get the next word as the variable's Dictionary Entry name
  forth_WORD(' ');
  PFORTHDICTENT pDE = forthMustFindDictEntForWord(pWBFR);
  if (EXECUTION_MODE())
  {
    // store to PFA
    pDE->pfa.singles[0] = SP_POP();
  }
  else
  {
    if (pDE->cfa == cfe_local_variable)
    {
      // Compile (TO-LOCALVAR) into Dictionary
      forthDictStoreFindPrimitive(primitive_BRACKET_TO_LOCALVAR);
      // store LOCAL variable index into Dictionary
      forthDictStoreSingle(pDE->pfa.singles[0]);
    }
    else
    {
      // Compile (TO) into Dictionary
      forthDictStoreFindPrimitive(primitive_BRACKET_TO);
      // store PFA into Dictionary
      forthDictStoreAddr(PTR_TO_ADDR(&pDE->pfa));
    }
  }
}

// :
static void prim_COLON(void)
{
  // verify in Execution Mode
  if (!EXECUTION_MODE())
  {
    forth_ERROR(err_EXECUTION_ONLY);
    return;
  }
  // call CREATE
  vect_CREATE();
  // change Dictionary Entry's CFA to cfe_colon
  pLAST->cfa = cfe_colon;
  // set "smudge" => definition is incomplete
  pLAST->nfa.flag.smudge = TRUE;
  // clear the Locals Dictionary
  *user_var_LOCALSDP = PTR_TO_ADDR(pLOCALSDP0);
  pLOCALSLAST = NULL;
  g_LOCALSINDEX = 0;
  // save the current Stack Pointer for Compiler Security
  *user_var_CSP = PTR_TO_ADDR(pSP);
  // go into Compilation Mode
  SET_COMPILATION_MODE();
}

// :NONAME
static void prim_COLON_NONAME(void)
{
  // verify in Execution Mode
  if (!EXECUTION_MODE())
  {
    forth_ERROR(err_EXECUTION_ONLY);
    return;
  }
  // create a dummy/blank Colon Definition
  forthCreateDictEnt_colon(" ");
  // push the Execution Token to the Stack now
  // (otherwise would have to recognise this case when the ; is reached,
  // plus this means the token can be used for a recursive definition)
  SP_PUSH_PTR(pLAST);
  // clear the Locals Dictionary
  *user_var_LOCALSDP = PTR_TO_ADDR(pLOCALSDP0);
  pLOCALSLAST = NULL;
  g_LOCALSINDEX = 0;
  // save the current Stack Pointer for Compiler Security
  *user_var_CSP = PTR_TO_ADDR(pSP);
  // go into Compilation Mode
  SET_COMPILATION_MODE();
}

// R:
static void prim_R_COLON(void)
{
  // execute :
  prim_COLON();
  // clear the SMUDGE bit, to make the definition findable
  pLAST->nfa.flag.smudge = FALSE;
}

// [
static void prim_LEFT_BRACKET(void)
{
  // Compilation Mode only
  if (!COMPILATION_MODE())
  {
    forth_ERROR(err_COMPILATION_ONLY);
    return;
  }
  // return to Execution Mode
  SET_EXECUTION_MODE();
}

// ]
static void prim_RIGHT_BRACKET(void)
{
  // verify in Execution Mode
  if (!EXECUTION_MODE())
  {
    forth_ERROR(err_EXECUTION_ONLY);
    return;
  }
  // return to Compilation Mode
  SET_COMPILATION_MODE();
}

// [COMPILE]
static void prim_BRACKET_COMPILE(void)
{
  // Compilation Mode only
  if (!COMPILATION_MODE())
  {
    forth_ERROR(err_COMPILATION_ONLY);
    return;
  }
  // get the next word as the Dictionary Entry name
  forth_WORD(' ');
  PFORTHDICTENT pDE = forthMustFindDictEntForWord(pWBFR);
  // compile it into the Dictionary
  forthDictStoreDictEnt(pDE);
}

// (EXIT)
static void prim_BRACKET_EXIT(void)
{
  CALLER_ADVANCE_TO_EXIT();
}

// EXIT
static void prim_EXIT(void)
{
  if (COMPILATION_MODE())
  {
    // Compile (EXIT) into Dictionary
    forthDictStoreFindPrimitive(primitive_BRACKET_EXIT);
  }
  else
  {
    // terminate interpretation of current Input Stream
    external_fclose(g_fspIN);
  }
}

// ;
static void prim_SEMI_COLON(void)
{
  // Compilation Mode only
  if (!COMPILATION_MODE())
  {
    forth_ERROR(err_COMPILATION_ONLY);
    return;
  }
  // verify that pSP == pCSP, for Compiler Security
  if (pSP != pCSP)
  {
    forth_ERROR(err_COND_NOT_PAIRED);
    return;
  }
  // complete the colon definition
  forthCompleteDictEnt_colon();
  // return to Execution Mode
  SET_EXECUTION_MODE();
}

// R;
static void prim_R_SEMI_COLON(void)
{
  // set the SMUDGE bit, prior to executing ;
  pLAST->nfa.flag.smudge = TRUE;
  // execute ;
  prim_SEMI_COLON();
}

// EXECUTE
static void prim_EXECUTE(void)
{
  PFORTHDICTENT pDE = SP_POP_PTR();
  CHECK_DICT_PTR(pDE);
  forth_EXECUTE_DICTENT(pDE);
}

// @EXECUTE
static void prim_FETCH_EXECUTE(void)
{
  PADDR pADDR = SP_POP_PTR();
  if (pADDR == NULL)
    forth_ERROR(err_SYS_MEM);
  PFORTHDICTENT pDE = ADDR_TO_PTR(*pADDR);
  CHECK_DICT_PTR(pDE);
  forth_EXECUTE_DICTENT(pDE);
}

// (TRACE-EXECUTE)
static void prim_BRACKET_TRACE_EXECUTE(void)
{
  bool postPre = SP_POP();
  PFORTHDICTENT pDE = SP_POP_PTR();
  CHECK_DICT_PTR(pDE);
  if (!postPre)
    fprintf(stdout, "->%s ", pDE->nfa.name);
  else
  {
    fprintf(stdout, "%s-> ", pDE->nfa.name);
    fprintf(stdout, "\n");
  }
  external_fflush(stdout);
}

// (TRACE-EXECUTE-STACK)
static void prim_BRACKET_TRACE_EXECUTE_STACK(void)
{
  bool postPre = SP_POP();
  PFORTHDICTENT pDE = SP_POP_PTR();
  CHECK_DICT_PTR(pDE);
  if (!postPre)
  {
    prim_H_DOT_S();
    fprintf(stdout, "->%s ", pDE->nfa.name);
  }
  else
  {
    fprintf(stdout, "%s-> ", pDE->nfa.name);
    prim_H_DOT_S();
    fprintf(stdout, "\n");
  }
  external_fflush(stdout);
}

// TRACE-EXECUTE
static void vect_TRACE_EXECUTE(void)
{
  // first time: find TRACE-EXECUTE in the Dictionary
  static PFORTHDICTENT s_pDETraceExecute = NULL;
  if (s_pDETraceExecute == NULL)
    s_pDETraceExecute = forthFindDictEnt("TRACE-EXECUTE");

  // execute the FORTHDICTENT which the vector is assigned to do
  forth_EXECUTE_EXVEC(s_pDETraceExecute->pfa.addrs[0]);
}

// TRACE
static void prim_TRACE(void)
{
  // get the next word as the Dictionary Entry name
  forth_WORD(' ');
  ADDR aDE = PTR_TO_ADDR(forthMustFindDictEntForWord(pWBFR));
  forthAddTracepoint(aDE);
}

// UNTRACE
static void prim_UNTRACE(void)
{
  // get the next word as the Dictionary Entry name
  forth_WORD(' ');
  ADDR aDE = PTR_TO_ADDR(forthMustFindDictEntForWord(pWBFR));
  forthDeleteTracepoint(aDE);
}

// TRACE-ALL
static void prim_TRACE_ALL(void)
{
  g_pTRACE_VARS->trace_all = TRUE;
}

// TRACE-COLONS
static void prim_TRACE_COLONS(void)
{
  g_pTRACE_VARS->trace_colons = TRUE;
}

// TRACEON
static void prim_TRACE_ON(void)
{
  if (!g_pTRACE_VARS->trace)
  {
    g_pTRACE_VARS->trace = TRUE;
    g_pTRACE_VARS->trace_all = g_pTRACE_VARS->trace_colons = FALSE;
  }
  g_pTRACE_VARS->trace_suspend = FALSE;
}

// TRACEOFF
static void prim_TRACE_OFF(void)
{
  if (g_pTRACE_VARS->trace)
    g_pTRACE_VARS->trace = FALSE;
}

// LIST-BREAK
static void prim_LIST_BREAK(void)
{
  forthListBreakpoints();
}

// BREAK
static void prim_BREAK(void)
{
  // get the next word as the Dictionary Entry name
  forth_WORD(' ');
  PFORTHDICTENT pDE = forthFindDictEnt(pWBFR->chars);
  if (pDE == NULL)
    forth_MESSAGE_WORD_NOT_FOUND(pWBFR);
  else
    forthAddBreakpoint(PTR_TO_ADDR(pDE));
}

// UNBREAK
static void prim_UNBREAK(void)
{
  // get the next word as the Dictionary Entry name
  forth_WORD(' ');
  PFORTHDICTENT pDE = forthFindDictEnt(pWBFR->chars);
  if (pDE == NULL)
    forth_MESSAGE_WORD_NOT_FOUND(pWBFR);
  else
    forthDeleteBreakpoint(PTR_TO_ADDR(pDE));
}

// DEBUGON
static void prim_DEBUG_ON(void)
{
  if (!g_pTRACE_VARS->debug)
  {
    g_pTRACE_VARS->debug = TRUE;
    CLEAR_BACKTRACE_STACK();
  }
  g_pTRACE_VARS->debug_suspend = FALSE;
}

// DEBUGOFF
static void prim_DEBUG_OFF(void)
{
  if (g_pTRACE_VARS->debug)
  {
    g_pTRACE_VARS->debug = FALSE;
    CLEAR_BACKTRACE_STACK();
  }
}

// DECOMPILE
static void prim_DECOMPILE(void)
{
  // get the next word as the Dictionary Entry name
  forth_WORD(' ');
  PFORTHDICTENT pDE = forthMustFindDictEntForWord(pWBFR);
  forthDecompileDictEnt(pDE, NULL);
}

// CASE-INSENSITIVE
static void prim_CASE_INSENSITIVE(void)
{
  // make Dictionary lookup be case-insensitive
  dictcmp = strncasecmp;
}

// CASE-SENSITIVE
static void prim_CASE_SENSITIVE(void)
{
  // make Dictionary lookup be case-sensitive
  dictcmp = strncmp;
}

// ---------- Dictionary definitions

// ++++++++++ Miscellaneous definitions

// INTERPRET
static void prim_INTERPRET(void)
{
  forth_INTERPRET();
}

// $INTERPRET
static void prim_DOLLAR_INTERPRET(void)
{
  PFORTHSTRING pStr = SP_POP_PTR();
  // interpret the string
  forth_STRING_INTERPRET(pStr);
}

// (INTERPRET")
static void prim_BRACKET_INTERPRET_QUOTE(void)
{
  // fetch the inline FORTHSTRING from the CALLER
  CHECK_CALLER();
  PFORTHSTRING pStr = (PFORTHSTRING)CALLER_INLINE_ADDRS();
  // advance the CALLER over the FORTHSTRING
  CALLER_ADVANCE_FORTHSTRING(pStr);
  // interpret the string
  forth_STRING_INTERPRET(pStr);
}

// INTERPRET"
static void prim_INTERPRET_QUOTE(void)
{
  // start of literal string, move up to "
  forth_WORD('"');
  if (COMPILATION_MODE())
  {
    // Compile (INTERPRET") into Dictionary
    forthDictStoreFindPrimitive(primitive_BRACKET_INTERPRET_QUOTE);
    // store FORTHSTRING into Dictionary
    forthDictStoreForthString(pWBFR->len, pWBFR->chars);
  }
  else
  {
    // interpret the string
    forth_STRING_INTERPRET(pWBFR);
  }
}

// (?LOAD)
static void prim_BRACKET_QUERY_LOAD(void)
{
  // fetch the inline FORTHSTRING from the CALLER
  CHECK_CALLER();
  PFORTHSTRING pFName = (PFORTHSTRING)CALLER_INLINE_ADDRS();
  // advance the CALLER over the FORTHSTRING
  CALLER_ADVANCE_FORTHSTRING(pFName);
  // load and interpret the file if it can be opened
  bool f = forth_QUERY_LOAD_INTERPRET(pFName->chars);
  SP_PUSH(f);
}

// (LOAD)
static void prim_BRACKET_LOAD(void)
{
  // fetch the inline FORTHSTRING from the CALLER
  CHECK_CALLER();
  PFORTHSTRING pFName = (PFORTHSTRING)CALLER_INLINE_ADDRS();
  // advance the CALLER over the FORTHSTRING
  CALLER_ADVANCE_FORTHSTRING(pFName);
  // load and interpret the file
  forth_LOAD_INTERPRET(pFName->chars);
}

// ?LOAD"
static void prim_QUERY_LOAD_QUOTE(void)
{
  // start of literal file name, move up to "
  forth_WORD('"');
  if (COMPILATION_MODE())
  {
    // Compile (?LOAD") into Dictionary
    forthDictStoreFindPrimitive(primitive_BRACKET_QUERY_LOAD);
    // store FORTHSTRING into Dictionary
    forthDictStoreForthString(pWBFR->len, pWBFR->chars);
  }
  else
  {
    // load and interpret the file if it can be opened
    bool f = forth_QUERY_LOAD_INTERPRET(pWBFR->chars);
    SP_PUSH(f);
  }
}

// LOAD"
static void prim_LOAD_QUOTE(void)
{
  // start of literal file name, move up to "
  forth_WORD('"');
  if (COMPILATION_MODE())
  {
    // Compile (LOAD") into Dictionary
    forthDictStoreFindPrimitive(primitive_BRACKET_LOAD);
    // store FORTHSTRING into Dictionary
    forthDictStoreForthString(pWBFR->len, pWBFR->chars);
  }
  else
  {
    // load and interpret the file
    forth_LOAD_INTERPRET(pWBFR->chars);
  }
}

// STDOUT-FLUSH
static void prim_STDOUT_FLUSH(void)
{
  forthFlushFileOutput();
}

// STDOUT>
static void prim_STDOUT_TO(void)
{
  PFORTHSTRING pFName = SP_POP_PTR();
  if (pFName == NULL)
  {
    // redirect g_fpOUT to the file
    forthSetConsoleOutput();
  }
  else
  {
    // redirect g_fpOUT to the file
    FILE *fp = external_fopen(pFName->chars, "w");
    if (fp == NULL)
      forth_OS_ERROR();
    forthSetFileOutput(fp);
  }
}

// STDOUT>>
static void prim_STDOUT_TO_TO(void)
{
  PFORTHSTRING pFName = SP_POP_PTR();
  if (pFName == NULL)
  {
    // redirect g_fpOUT to the file
    forthSetConsoleOutput();
  }
  else
  {
    // redirect g_fpOUT to the file (for append)
    FILE *fp = external_fopen(pFName->chars, "a");
    if (fp == NULL)
      forth_OS_ERROR();
    forthSetFileOutput(fp);
  }
}

// TIME
static void prim_TIME(void)
{
  DOUBLE tim = external_time();
  SP_PUSH_DOUBLE(tim);
}

// MICRO-TIME
static void prim_MICRO_TIME(void)
{
  DOUBLE secs, usecs;
  external_getmicrotime(&secs, &usecs);
  SP_PUSH_DOUBLE(secs);
  SP_PUSH_DOUBLE(usecs);
}

// SLEEP
static void prim_SLEEP(void)
{
  SINGLE num = SP_POP();
  external_sleep(num);
}

// OSCLI
static void prim_OS_CLI(void)
{
  const char *pChars = SP_POP_PTR();
  forth_oscommand(pChars);
}

// (CLI)
static void prim_BRACKET_CLI(void)
{
  // fetch the inline FORTHSTRING from the CALLER
  CHECK_CALLER();
  PFORTHSTRING pStr = (PFORTHSTRING)CALLER_INLINE_ADDRS();
  // advance the CALLER over the FORTHSTRING
  CALLER_ADVANCE_FORTHSTRING(pStr);
  SP_PUSH_PTR(pStr->chars);
  prim_OS_CLI();
}

// >CLI
static void prim_TO_CLI(void)
{
  BYTE count = SP_POP();
  if (COMPILATION_MODE())
  {
    const char *pChars = SP_POP_PTR();
    // Compile (CLI) into Dictionary
    forthDictStoreFindPrimitive(primitive_BRACKET_CLI);
    // store FORTHSTRING into Dictionary
    forthDictStoreForthString(count, pChars);
  }
  else
  {
    prim_OS_CLI();
  }
}

// OS'
static void prim_OS_QUOTE(void)
{
  // start of literal string, move up to '
  forth_WORD('\'');
  SP_PUSH_PTR(pWBFR->chars);
  SP_PUSH(pWBFR->len);
  prim_TO_CLI();
}

// OS-PID
static void prim_OS_PID(void)
{
  pid_t pid = forth_getpid();
  SP_PUSH_DOUBLE(pid);
}

// OS-FORK
static void prim_OS_FORK(void)
{
  pid_t pid = forth_fork();
  switch (pid)
  {
  case -1:	// error
    prim_OS_ERROR();
    break;

  case 0:	// in child process
    fprintf(stderr, "FORK: In child\n");
    break;

  default:	// in parent process, pid is child's PID
    fprintf(stderr, "FORK: In parent, child PID=%d\n", pid);
    break;
  }
}

// ---------- Miscellaneous definitions

// ***********************************************
// ********** </PRIMITIVE DEFINITIONS> **********
// ***********************************************


// *****************************************
// ********** <EXECUTE PRIMITIVE> **********
// *****************************************

// EXECUTE_PRIMITIVE
static void forth_EXECUTE_PRIMITIVE(primitive_t prim)
{
  // execute a primitive

//STEP#4
  switch (prim)
  {
  case primitive_STORE:			// !
    prim_STORE(); break;

  case primitive_QUOTE:			// "
    prim_QUOTE(); break;

  case primitive_DOLLAR_STORE:		// $!
    prim_DOLLAR_STORE(); break;

  case primitive_DOLLAR_PLUS:		// $+
    prim_DOLLAR_PLUS(); break;

  case primitive_DOLLAR_COMMA:		// $,
    prim_DOLLAR_COMMA(); break;

  case primitive_DOLLAR_CMP:		// $CMP
    prim_DOLLAR_CMP(); break;

  case primitive_DOLLAR_CMPID:		// $CMPID
    prim_DOLLAR_CMPID(); break;

  case primitive_DOLLAR_CONSTANT:	// $CONSTANT
    prim_DOLLAR_CONSTANT(); break;

  case primitive_DOLLAR_INTERPRET:	// $INTERPRET
    prim_DOLLAR_INTERPRET(); break;

  case primitive_DOLLAR_VARIABLE:	// $VARIABLE
    prim_DOLLAR_VARIABLE(); break;

  case primitive_TICK:			// '
    prim_TICK(); break;

  case primitive_PAREN:			// (
    prim_PAREN(); break;

  case primitive_BRACKET_QUOTE:		// (")
    prim_BRACKET_QUOTE(); break;

  case primitive_BRACKET_DOLLAR_STORE:	// ($!)
    prim_BRACKET_DOLLAR_STORE(); break;

  case primitive_BRACKET_DOLLAR_PLUS:	// ($+)
    prim_BRACKET_DOLLAR_PLUS(); break;

  case primitive_BRACKET_PLUS_LOOP:	// (+LOOP)
    prim_BRACKET_PLUS_LOOP(); break;

  case primitive_BRACKET_DOT_QUOTE:	// (.")
    prim_BRACKET_DOT_QUOTE(); break;

  case primitive_BRACKET_QUERY_LOAD:	// (?LOAD)
    prim_BRACKET_QUERY_LOAD(); break;

  case primitive_BRACKET_ABORT:		// (ABORT)
    prim_BRACKET_ABORT(); break;

  case primitive_BRACKET_ASSERT:	// (ASSERT)
    prim_BRACKET_ASSERT(); break;

  case primitive_BRACKET_CLI:		// (CLI)
    prim_BRACKET_CLI(); break;

  case primitive_BRACKET_CREATE:	// (CREATE)
    prim_BRACKET_CREATE(); break;

  case primitive_BRACKET_DECLARE_LOCALVAR:	// (DECLARE-LOCALVAR)
    prim_BRACKET_DECLARE_LOCALVAR(); break;

  case primitive_BRACKET_DO:		// (DO)
    prim_BRACKET_DO(); break;

  case primitive_BRACKET_EMIT:		// (EMIT)
    prim_BRACKET_EMIT(); break;

  case primitive_BRACKET_EMITS:		// (EMITS)
    prim_BRACKET_EMITS(); break;

  case primitive_BRACKET_EXIT:		// (EXIT)
    prim_BRACKET_EXIT(); break;

  case primitive_BRACKET_FIND:		// (FIND)
    prim_BRACKET_FIND(); break;

  case primitive_BRACKET_FIND_PREV:	// (FINDPREV)
    prim_BRACKET_FIND_PREV(); break;

  case primitive_BRACKET_FORGET:	// (FORGET)
    prim_BRACKET_FORGET(); break;

  case primitive_BRACKET_KEY:		// (KEY)
    prim_BRACKET_KEY(); break;

  case primitive_BRACKET_INTERPRET_QUOTE:	// (INTERPRET")
    prim_BRACKET_INTERPRET_QUOTE(); break;

  case primitive_BRACKET_JUMP:		// (JUMP)
    prim_BRACKET_JUMP(); break;

  case primitive_BRACKET_LOAD:		// (LOAD)
    prim_BRACKET_LOAD(); break;

  case primitive_BRACKET_LOCAL:		// (LOCAL)
    prim_BRACKET_LOCAL(); break;

  case primitive_BRACKET_LOCALVAR:	// (LOCALVAR)
    prim_BRACKET_LOCALVAR(); break;

  case primitive_BRACKET_LOOP:		// (LOOP)
    prim_BRACKET_LOOP(); break;

  case primitive_BRACKET_NUM:		// (NUM)
    prim_BRACKET_NUM(); break;

  case primitive_BRACKET_TO:		// (TO)
    prim_BRACKET_TO(); break;

  case primitive_BRACKET_TO_LOCALVAR:	// (TO-LOCALVAR)
    prim_BRACKET_TO_LOCALVAR(); break;

  case primitive_BRACKET_TRACE_EXECUTE:	// (TRACE-EXECUTE)
    prim_BRACKET_TRACE_EXECUTE(); break;

  case primitive_BRACKET_TRACE_EXECUTE_STACK:	// (TRACE-EXECUTE-STACK)
    prim_BRACKET_TRACE_EXECUTE_STACK(); break;

  case primitive_BRACKET_U_LOOP:	// (ULOOP)
    prim_BRACKET_U_LOOP(); break;

  case primitive_BRACKET_VOCABULARY:	// (VOCABULARY)
    prim_BRACKET_VOCABULARY(); break;

  case primitive_BRACKET_WORD:		// (WORD)
    prim_BRACKET_WORD(); break;

  case primitive_TIMES:			// *
    prim_TIMES(); break;

  case primitive_TIMES_DIVIDE:		// */
    prim_TIMES_DIVIDE(); break;

  case primitive_TIMES_DIVIDE_MOD:	// */MOD
    prim_TIMES_DIVIDE_MOD(); break;

  case primitive_PLUS:			// +
    prim_PLUS(); break;

  case primitive_PLUS_STORE:		// +!
    prim_PLUS_STORE(); break;

  case primitive_PLUS_MINUS:		// +-
    prim_PLUS_MINUS(); break;

  case primitive_PLUS_LOOP:		// +LOOP
    prim_PLUS_LOOP(); break;

  case primitive_PLUS_ORIGIN:		// +ORIGIN
    prim_PLUS_ORIGIN(); break;

  case primitive_COMMA:			// ,
    prim_COMMA(); break;

  case primitive_SUBTRACT:		// -
    prim_SUBTRACT(); break;

  case primitive_DASH_FIND:		// -FIND
    prim_DASH_FIND(); break;

  case primitive_DASH_FORGET:		// -FORGET
    prim_DASH_FORGET(); break;

  case primitive_DASH_TRAILING:		// -TRAILING
    prim_DASH_TRAILING(); break;

  case primitive_DOT:			// .
    prim_DOT(); break;

  case primitive_DOT_QUOTE:		// ."
    prim_DOT_QUOTE(); break;

  case primitive_DOT_R:			// .R
    prim_DOT_R(); break;

  case primitive_DOT_RS:		// .RS
    prim_DOT_RS(); break;

  case primitive_DOT_S:			// .S
    prim_DOT_S(); break;

  case primitive_DIVIDE:		// /
    prim_DIVIDE(); break;

  case primitive_DIVIDE_MOD:		// /MOD
    prim_DIVIDE_MOD(); break;

  case primitive_ZERO_LESS:		// 0<
    prim_ZERO_LESS(); break;

  case primitive_ZERO_EQUALS:		// 0=
    prim_ZERO_EQUALS(); break;

  case primitive_ZERO_GREATER:		// 0>
    prim_ZERO_GREATER(); break;

  case primitive_ZERO_BRANCH:		// 0BRANCH
    prim_ZERO_BRANCH(); break;

  case primitive_1_PLUS:		// 1+
    prim_1_PLUS(); break;

  case primitive_1_PLUS_STORE:		// 1+!
    prim_1_PLUS_STORE(); break;

  case primitive_1_MINUS:		// 1-
    prim_1_MINUS(); break;

  case primitive_1_MINUS_STORE:		// 1-!
    prim_1_MINUS_STORE(); break;

  case primitive_2_PLUS:		// 2+
    prim_2_PLUS(); break;

  case primitive_2_MINUS:		// 2-
    prim_2_MINUS(); break;

  case primitive_2_TIMES:		// 2*
    prim_2_TIMES(); break;

  case primitive_2_DIVIDE:		// 2/
    prim_2_DIVIDE(); break;

  case primitive_2_DROP:		// 2DROP
    prim_2_DROP(); break;

  case primitive_2_DUP:			// 2DUP
    prim_2_DUP(); break;

  case primitive_2_OVER:		// 2OVER
    prim_2_OVER(); break;

  case primitive_2_SWAP:		// 2SWAP
    prim_2_SWAP(); break;

  case primitive_4_HEX:			// 4HEX
    prim_4_HEX(); break;

  case primitive_COLON:			// :
    prim_COLON(); break;

  case primitive_COLON_NONAME:		// :NONAME
    prim_COLON_NONAME(); break;

  case primitive_SEMI_COLON:		// ;
    prim_SEMI_COLON(); break;

  case primitive_LESS_THAN:		// <
    prim_LESS_THAN(); break;

  case primitive_LESS_BUILDS:		// <BUILDS
    prim_LESS_BUILDS(); break;

  case primitive_EQUALS:		// =
    prim_EQUALS(); break;

  case primitive_GREATER_THAN:		// >
    prim_GREATER_THAN(); break;

  case primitive_TO_CLI:		// >CLI
    prim_TO_CLI(); break;

  case primitive_TO_R:			// >R
    prim_TO_R(); break;

  case primitive_QUESTION_MARK:		// ?
    prim_QUESTION_MARK(); break;

  case primitive_QUERY_COMP:		// ?COMP
    prim_QUERY_COMP(); break;

  case primitive_QUERY_CSP:		// ?CSP
    prim_QUERY_CSP(); break;

  case primitive_QUERY_DUP:		// ?DUP
    prim_QUERY_DUP(); break;

  case primitive_QUERY_ERROR:		// ?ERROR
    prim_QUERY_ERROR(); break;

  case primitive_QUERY_EXEC:		// ?EXEC
    prim_QUERY_EXEC(); break;

  case primitive_QUERY_LOAD_QUOTE:	// ?LOAD"
    prim_QUERY_LOAD_QUOTE(); break;

  case primitive_QUERY_PAIRS:		// ?PAIRS
    prim_QUERY_PAIRS(); break;

  case primitive_QUERY_STACK:		// ?STACK
    prim_QUERY_STACK(); break;

  case primitive_FETCH:			// @
    prim_FETCH(); break;

  case primitive_FETCH_EXECUTE:		// @EXECUTE
    prim_FETCH_EXECUTE(); break;

  case primitive_A_DOT:			// A.
    prim_A_DOT(); break;

  case primitive_A_QUESTION_MARK:	// A?
    prim_A_QUESTION_MARK(); break;

  case primitive_AND:			// AND
    prim_AND(); break;

  case vector_ABORT:			// ABORT
    vect_ABORT(); break;

  case primitive_ABS:			// ABS
    prim_ABS(); break;

  case primitive_AGAIN:			// AGAIN
    prim_AGAIN(); break;

  case primitive_A_LIT:			// ALIT
    prim_A_LIT(); break;

  case primitive_A_LITERAL:		// ALITERAL
    prim_A_LITERAL(); break;

  case primitive_ALLOT:			// ALLOT
    prim_ALLOT(); break;

  case primitive_ASSIGN:		// ASSIGN
    prim_ASSIGN(); break;

  case primitive_ASSERT:		// ASSERT
    prim_ASSERT(); break;

  case primitive_B_DOT:			// B.
    prim_B_DOT(); break;

  case primitive_BACK:			// BACK
    prim_BACK(); break;

  case primitive_BEGIN:			// BEGIN
    prim_BEGIN(); break;

  case primitive_BLANKS:		// BLANKS
    prim_BLANKS(); break;

  case primitive_BRANCH:		// BRANCH
    prim_BRANCH(); break;

  case primitive_BREAK:			// BREAK
    prim_BREAK(); break;

  case primitive_C_STORE:		// C!
    prim_C_STORE(); break;

  case primitive_C_PLUS_STORE:	// C+!
    prim_C_PLUS_STORE(); break;

  case primitive_C_COMMA:		// C,
    prim_C_COMMA(); break;

  case primitive_C_DOT:			// C.
    prim_C_DOT(); break;

  case primitive_C_QUESTION_MARK:	// C?
    prim_C_QUESTION_MARK(); break;

  case primitive_C_FETCH:		// C@
    prim_C_FETCH(); break;

  case primitive_C_1_PLUS_STORE:// C1+!
    prim_C_1_PLUS_STORE(); break;

  case primitive_C_1_MINUS_STORE:// C1-!
    prim_C_1_MINUS_STORE(); break;

  case primitive_C_LIT:			// CLIT
    prim_C_LIT(); break;

  case primitive_C_LITERAL:		// CLITERAL
    prim_C_LITERAL(); break;

  case primitive_C_MOVE:		// CMOVE
    prim_C_MOVE(); break;

  case primitive_CASE_INSENSITIVE:	// CASE-INSENSITIVE
    prim_CASE_INSENSITIVE(); break;

  case primitive_CASE_SENSITIVE:	// CASE-SENSITIVE
    prim_CASE_SENSITIVE(); break;

  case primitive_CFA:			// CFA
    prim_CFA(); break;

  case primitive_COLD:			// COLD
    prim_COLD(); break;

  case primitive_COMPILE:		// COMPILE
    prim_COMPILE(); break;

  case primitive_CONSTANT:		// CONSTANT
    prim_CONSTANT(); break;

  case primitive_COUNT:			// COUNT
    prim_COUNT(); break;

  case primitive_CR:			// CR
    prim_CR(); break;

  case vector_CREATE:			// CREATE
    vect_CREATE(); break;

  case primitive_D_STORE:		// D!
    prim_D_STORE(); break;

  case primitive_D_TIMES:		// D*
    prim_D_TIMES(); break;

  case primitive_D_PLUS:		// D+
    prim_D_PLUS(); break;

  case primitive_D_PLUS_MINUS:		// D+-
    prim_D_PLUS_MINUS(); break;

  case primitive_D_SUBTRACT:		// D-
    prim_D_SUBTRACT(); break;

  case primitive_D_TO_F:		// D->F
    prim_D_TO_F(); break;

  case primitive_D_TO_S:		// D->S
    prim_D_TO_S(); break;

  case primitive_D_DIVIDE:		// D/
    prim_D_DIVIDE(); break;

  case primitive_D_DIVIDE_MOD:		// D/MOD
    prim_D_DIVIDE_MOD(); break;

  case primitive_D_LESS_THAN:		// D<
    prim_D_LESS_THAN(); break;

  case primitive_D_EQUALS:		// D=
    prim_D_EQUALS(); break;

  case primitive_D_TO_R:		// D>R
    prim_D_TO_R(); break;

  case primitive_D_COMMA:		// D,
    prim_D_COMMA(); break;

  case primitive_D_DOT:			// D.
    prim_D_DOT(); break;

  case primitive_D_DOT_R:		// D.R
    prim_D_DOT_R(); break;

  case primitive_D_QUESTION_MARK:	// D?
    prim_D_QUESTION_MARK(); break;

  case primitive_D_FETCH:		// D@
    prim_D_FETCH(); break;

  case primitive_D_ABS:			// DABS
    prim_D_ABS(); break;

  case primitive_D_CONSTANT:		// DCONSTANT
    prim_D_CONSTANT(); break;

  case primitive_DEBUG_OFF:		// DEBUGOFF
    prim_DEBUG_OFF(); break;

  case primitive_DEBUG_ON:		// DEBUGON
    prim_DEBUG_ON(); break;

  case primitive_DEC_DOT:		// DEC.
    prim_DEC_DOT(); break;

  case primitive_DECIMAL:		// DECIMAL
    prim_DECIMAL(); break;

  case primitive_DECOMPILE:		// DECOMPILE
    prim_DECOMPILE(); break;

  case primitive_DICTDUMP:		// DICTDUMP
    prim_DICTDUMP(); break;

  case primitive_DROP:			// DROP
    prim_DROP(); break;

  case primitive_DUP:			// DUP
    prim_DUP(); break;

  case primitive_DEFINITIONS:		// DEFINITIONS
    prim_DEFINITIONS(); break;

  case primitive_DEPTH:			// DEPTH
    prim_DEPTH(); break;

  case primitive_D_H_DOT:		// DH.
    prim_D_H_DOT(); break;

  case primitive_D_H_QUESTION_MARK:	// DH?
    prim_D_H_QUESTION_MARK(); break;

  case primitive_DIGIT:			// DIGIT
    prim_DIGIT(); break;

  case primitive_D_LIT:			// DLIT
    prim_D_LIT(); break;

  case primitive_D_LITERAL:		// DLITERAL
    prim_D_LITERAL(); break;

  case primitive_D_NEGATE:		// DNEGATE
    prim_D_NEGATE(); break;

  case primitive_DO:			// DO
    prim_DO(); break;

  case primitive_DOES_GREATER:		// DOES>
    prim_DOES_GREATER(); break;

  case primitive_DOVEC:			// DOVEC
    prim_DOVEC(); break;

  case primitive_D_VARIABLE:		// DVARIABLE
    prim_D_VARIABLE(); break;

  case primitive_EXECUTE:		// EXECUTE
    prim_EXECUTE(); break;

  case primitive_ELSE:			// ELSE
    prim_ELSE(); break;

  case vector_EMIT:			// EMIT
    vect_EMIT(); break;

  case vector_EMITS:			// EMITS
    vect_EMITS(); break;

  case primitive_ERASE:			// ERASE
    prim_ERASE(); break;

  case primitive_ERROR:			// ERROR
    prim_ERROR(); break;

  case primitive_EXIT:			// EXIT
    prim_EXIT(); break;

  case primitive_EXVEC_COLON:		// EXVEC:
    prim_EXVEC_COLON(); break;

  case primitive_F_STORE:		// F!
    prim_F_STORE(); break;

  case primitive_F_TIMES:		// F*
    prim_F_TIMES(); break;

  case primitive_F_PLUS:		// F+
    prim_F_PLUS(); break;

  case primitive_F_SUBTRACT:		// F-
    prim_F_SUBTRACT(); break;

  case primitive_F_TO_D:		// F->D
    prim_F_TO_D(); break;

  case primitive_F_TO_SF:		// F->SF
    prim_F_TO_SF(); break;

  case primitive_F_DIVIDE:		// F/
    prim_F_DIVIDE(); break;

  case primitive_F_LESS_THAN:		// F<
    prim_F_LESS_THAN(); break;

  case primitive_F_EQUALS:		// F=
    prim_F_EQUALS(); break;

  case primitive_F_TO_R:		// F>R
    prim_F_TO_R(); break;

  case primitive_F_COMMA:		// F,
    prim_F_COMMA(); break;

  case primitive_F_DOT:			// F.
    prim_F_DOT(); break;

  case primitive_F_QUESTION_MARK:	// F?
    prim_F_QUESTION_MARK(); break;

  case primitive_F_FETCH:		// F@
    prim_F_FETCH(); break;

  case primitive_F_CONSTANT:		// FCONSTANT
    prim_F_CONSTANT(); break;

  case primitive_F_DROP:		// FDROP
    prim_F_DROP(); break;

  case primitive_F_DUP:			// FDUP
    prim_F_DUP(); break;

  case primitive_FILL:			// FILL
    prim_FILL(); break;

  case primitive_FIND:			// FIND
    prim_FIND(); break;

  case primitive_FIND_VOCABULARY:	// FIND-VOCABULARY
    prim_FIND_VOCABULARY(); break;

  case primitive_FIRST:			// FIRST
    prim_FIRST(); break;

  case primitive_F_LIT:			// FLIT
    prim_F_LIT(); break;

  case primitive_F_LITERAL:		// FLITERAL
    prim_F_LITERAL(); break;

  case primitive_FORGET:		// FORGET
    prim_FORGET(); break;

  case primitive_FORTH:			// FORTH
    prim_FORTH(); break;

  case primitive_F_VARIABLE:		// FVARIABLE
    prim_F_VARIABLE(); break;

  case primitive_H_DOT:			// H.
    prim_H_DOT(); break;

  case primitive_H_DOT_RS:		// H.RS
    prim_H_DOT_RS(); break;

  case primitive_H_DOT_S:		// H.S
    prim_H_DOT_S(); break;

  case primitive_H_QUESTION_MARK:	// H?
    prim_H_QUESTION_MARK(); break;

  case primitive_HEX:			// HEX
    prim_HEX(); break;

  case primitive_HERE:			// HERE
    prim_HERE(); break;

  case primitive_I:			// I
    prim_I(); break;

  case primitive_ID_DOT:		// ID.
    prim_ID_DOT(); break;

  case primitive_IF:			// IF
    prim_IF(); break;

  case primitive_IMMEDIATE:		// IMMEDIATE
    prim_IMMEDIATE(); break;

  case primitive_INITVECS:		// INITVECS
    prim_INITVECS(); break;

  case primitive_INTERPRET:		// INTERPRET
    prim_INTERPRET(); break;

  case primitive_INTERPRET_QUOTE:	// INTERPRET"
    prim_INTERPRET_QUOTE(); break;

  case primitive_J:			// J
    prim_J(); break;

  case vector_KEY:			// KEY
    vect_KEY(); break;

  case primitive_LAST:			// LAST
    prim_LAST(); break;

  case primitive_LEAVE:			// LEAVE
    prim_LEAVE(); break;

  case primitive_LIMIT:			// LIMIT
    prim_LIMIT(); break;

  case primitive_LIST_BREAK:		// LIST-BREAK
    prim_LIST_BREAK(); break;

  case primitive_LIT:			// LIT
    prim_LIT(); break;

  case primitive_LITERAL:		// LITERAL
    prim_LITERAL(); break;

  case primitive_LFA:			// LFA
    prim_LFA(); break;

  case primitive_LOAD_QUOTE:		// LOAD"
    prim_LOAD_QUOTE(); break;

  case primitive_LOOP:			// LOOP
    prim_LOOP(); break;

  case primitive_MICRO_TIME:		// MICRO-TIME
    prim_MICRO_TIME(); break;

  case primitive_MAX:			// MAX
    prim_MAX(); break;

  case primitive_MEMCMP:		// MEMCMP
    prim_MEMCMP(); break;

  case primitive_MEMDUMP:		// MEMDUMP
    prim_MEMDUMP(); break;

  case primitive_MEMDUMP_W:		// MEMDUMPW
    prim_MEMDUMP_W(); break;

  case vector_MESSAGE:			// MESSAGE
    vect_MESSAGE(); break;

  case primitive_MIN:			// MIN
    prim_MIN(); break;

  case primitive_MSG_HASH:		// MSG#
    prim_MSG_HASH(); break;

  case primitive_MOD:			// MOD
    prim_MOD(); break;

  case primitive_MOVE:			// MOVE
    prim_MOVE(); break;

  case primitive_NEGATE:		// NEGATE
    prim_NEGATE(); break;

  case primitive_NFA:			// NFA
    prim_NFA(); break;

  case primitive_NOOP:			// NOOP
    prim_NOOP(); break;

  case primitive_NOT:			// NOT
    prim_NOT(); break;

  case primitive_NOT_EQUALS:		// NOT=
    prim_NOT_EQUALS(); break;

  case primitive_NOVEC:			// NOVEC
    prim_NOVEC(); break;

  case vector_NUM:			// NUM
    vect_NUM(); break;

  case primitive_NUMBER:		// NUMBER
    prim_NUMBER(); break;

  case primitive_OR:			// OR
    prim_OR(); break;

  case primitive_OS_QUOTE:		// OS'
    prim_OS_QUOTE(); break;

  case primitive_OS_FORK:		// OS-FORK
    prim_OS_FORK(); break;

  case primitive_OS_PID:		// OS-PID
    prim_OS_PID(); break;

  case primitive_OS_CLI:		// OSCLI
    prim_OS_CLI(); break;

  case primitive_OS_ERRNO:		// OSERRNO
    prim_OS_ERRNO(); break;

  case primitive_OS_ERROR:		// OSERROR
    prim_OS_ERROR(); break;

  case primitive_OVER:			// OVER
    prim_OVER(); break;

  case primitive_PAD:			// PAD
    prim_PAD(); break;

  case primitive_PFA:			// PFA
    prim_PFA(); break;

  case primitive_PICK:			// PICK
    prim_PICK(); break;

  case primitive_PRUNE:			// PRUNE
    prim_PRUNE(); break;

  case primitive_QUERY:			// QUERY
    prim_QUERY(); break;

  case primitive_QUIT:			// QUIT
    prim_QUIT(); break;

  case primitive_R_COLON:		// R:
    prim_R_COLON(); break;

  case primitive_R_SEMI_COLON:		// R;
    prim_R_SEMI_COLON(); break;

  case primitive_R_FROM:		// R>
    prim_R_FROM(); break;

  case primitive_R_FROM_D:		// R>D
    prim_R_FROM_D(); break;

  case primitive_R_FROM_F:		// R>F
    prim_R_FROM_F(); break;

  case primitive_R_FETCH:		// R@
    prim_R_FETCH(); break;

  case primitive_REPEAT:		// REPEAT
    prim_REPEAT(); break;

  case primitive_RP_STORE:		// RP!
    prim_RP_STORE(); break;

  case primitive_RP_FETCH:		// RP@
    prim_RP_FETCH(); break;

  case primitive_ROLL:			// ROLL
    prim_ROLL(); break;

  case primitive_ROT:			// ROT
    prim_ROT(); break;

  case primitive_S_TO_D:		// S->D
    prim_S_TO_D(); break;

  case primitive_SF_STORE:		// SF!
    prim_SF_STORE(); break;

  case primitive_SF_TO_F:		// SF->F
    prim_SF_TO_F(); break;

  case primitive_SF_COMMA:		// SF,
    prim_SF_COMMA(); break;

  case primitive_SF_DOT:		// SF.
    prim_SF_DOT(); break;

  case primitive_SF_FETCH:		// SF@
    prim_SF_FETCH(); break;

  case primitive_SLEEP:			// SLEEP
    prim_SLEEP(); break;

  case primitive_SMUDGE:		// SMUDGE
    prim_SMUDGE(); break;

  case primitive_SP_STORE:		// SP!
    prim_SP_STORE(); break;

  case primitive_SP_FETCH:		// SP@
    prim_SP_FETCH(); break;

  case primitive_SPACE:			// SPACE
    prim_SPACE(); break;

  case primitive_SPACES:		// SPACES
    prim_SPACES(); break;

  case primitive_STDOUT_FLUSH:		// STDOUT-FLUSH
    prim_STDOUT_FLUSH(); break;

  case primitive_STDOUT_TO:		// STDOUT>
    prim_STDOUT_TO(); break;

  case primitive_STDOUT_TO_TO:		// STDOUT>>
    prim_STDOUT_TO_TO(); break;

  case primitive_STRING:		// STRING
    prim_STRING(); break;

  case primitive_SWAP:			// SWAP
    prim_SWAP(); break;

  case primitive_TEXT:			// TEXT
    prim_TEXT(); break;

  case primitive_THEN:			// THEN
    prim_THEN(); break;

  case primitive_TIME:			// TIME
    prim_TIME(); break;

  case primitive_TO:			// TO
    prim_TO(); break;

  case primitive_TO_DO:			// TO-DO
    prim_TO_DO(); break;

  case primitive_TOGGLE:		// TOGGLE
    prim_TOGGLE(); break;

  case primitive_TRACE:			// TRACE
    prim_TRACE(); break;

  case primitive_TRACE_ALL:		// TRACE-ALL
    prim_TRACE_ALL(); break;

  case primitive_TRACE_COLONS:		// TRACE-COLONS
    prim_TRACE_COLONS(); break;

  case vector_TRACE_EXECUTE:		// TRACE-EXECUTE
    vect_TRACE_EXECUTE(); break;

  case primitive_TRACE_OFF:		// TRACEOFF
    prim_TRACE_OFF(); break;

  case primitive_TRACE_ON:		// TRACEON
    prim_TRACE_ON(); break;

  case primitive_TYPE:			// TYPE
    prim_TYPE(); break;

  case primitive_U_TIMES:		// U*
    prim_U_TIMES(); break;

  case primitive_U_TO_D:		// U->D
    prim_U_TO_D(); break;

  case primitive_U_DOT:			// U.
    prim_U_DOT(); break;

  case primitive_U_DIVIDE:		// U/
    prim_U_DIVIDE(); break;

  case primitive_U_DIVIDE_MOD:		// U/MOD
    prim_U_DIVIDE_MOD(); break;

  case primitive_U_LESS_THAN:		// U<
    prim_U_LESS_THAN(); break;

  case primitive_U_GREATER_THAN:	// U>
    prim_U_GREATER_THAN(); break;

  case primitive_U_D_DOT:		// UD.
    prim_U_D_DOT(); break;

  case primitive_U_LOOP:		// ULOOP
    prim_U_LOOP(); break;

  case primitive_UNBREAK:		// UNBREAK
    prim_UNBREAK(); break;

  case primitive_UNTIL:			// UNTIL
    prim_UNTIL(); break;

  case primitive_UNTRACE:		// UNTRACE
    prim_UNTRACE(); break;

  case primitive_USER:			// USER
    prim_USER(); break;

  case primitive_VARIABLE:		// VARIABLE
    prim_VARIABLE(); break;

  case primitive_VDUMP:			// VDUMP
    prim_VDUMP(); break;

  case primitive_VLIST:			// VLIST
    prim_VLIST(); break;

  case primitive_VOCABULARY:		// VOCABULARY
    prim_VOCABULARY(); break;

  case primitive_WARM:			// WARM
    prim_WARM(); break;

  case primitive_WBFR:			// WBFR
    prim_WBFR(); break;

  case primitive_WDSZ:			// WDSZ
    prim_WDSZ(); break;

  case primitive_WHILE:			// WHILE
    prim_WHILE(); break;

  case primitive_WORD:			// WORD
    prim_WORD(); break;

  case primitive_X_OR:			// XOR
    prim_X_OR(); break;

  case primitive_LEFT_BRACKET:		// [
    prim_LEFT_BRACKET(); break;

  case primitive_BRACKET_COMPILE:	// [COMPILE]
    prim_BRACKET_COMPILE(); break;

  case primitive_BRACKET_ELSE:		// [ELSE]
    prim_BRACKET_ELSE(); break;

  case primitive_BRACKET_IF:		// [IF]
    prim_BRACKET_IF(); break;

  case primitive_BRACKET_THEN:		// [THEN]
    prim_BRACKET_THEN(); break;

  case primitive_BACKSLASH:		// \ (backslash)
    prim_BACKSLASH(); break;

  case primitive_RIGHT_BRACKET:		// ]
    prim_RIGHT_BRACKET(); break;

  default:
    fprintf(stderr, "Unrecognised \"primitive_t\": %d\n", prim);
    forth_ERROR(err_0);
    break;
  }
}

// ******************************************
// ********** </EXECUTE PRIMITIVE> **********
// ******************************************


// *****************************************
// ********** <EXECUTE LOCAL_VAR> **********
// *****************************************

// EXECUTE_LOCAL_VAR
static void forth_EXECUTE_LOCAL_VAR(SINGLE index)
{
  // Compilation Mode only
  if (!COMPILATION_MODE())
  {
    forth_ERROR(err_COMPILATION_ONLY);
    return;
  }
  // Compile (LOCALVAR) into Dictionary
  forthDictStoreFindPrimitive(primitive_BRACKET_LOCALVAR);
  // store index into Dictionary
  forthDictStoreSingle(index);
}

// ******************************************
// ********** </EXECUTE LOCAL_VAR> **********
// ******************************************


// *************************************
// ********** <EXECUTE COLON> **********
// *************************************

// CALLER_ADVANCE_TO_EXIT
static void CALLER_ADVANCE_TO_EXIT(void)
{
  // while executing a colon definition's words
  // move pIP on to its terminating PFA_ADDR_EXIT, i.e. skip the remaining words in the colon and EXIT
  // this is called by DOES> and (EXIT)
  // note that this could alight on a data item happening to have value PFA_ADDR_EXIT
  // but that is OK as it will still cause exit if IP points to it
  while (*pIP != PFA_ADDR_EXIT)
    pIP++;
}

// EXECUTE_COLON
static void forth_EXECUTE_COLON(const PFORTHDICTENT pDEColon, register PADDR pADDR)
{
  // execute a colon definition's words
  // pDEColon is the Colon Definition being executed
  // the PFA.addrs[] array holds the series of words to execute, terminated by a PFA_ADDR_EXIT
  // on entry pADDR (BBC's "W") points to the word *before* the next word to be executed
  // * from a cfe_colon => pDE->pfa.addrs - 1
  // * from a cfe_does_gt => pDE->pfa.addrs + n - 1
  // and pIP (BBC's "IP") points to (the next) word to be executed in the CALLER

  // variable for Debug "Out" instruction
  bool debug_suspend_till_out = FALSE;

  // save the current value of IP
  PADDR pIPOld = pIP;
  // save the current value of RP
  PBYTE pRPOld = pRP;
  // save the current value of RPLOCALS
  PSINGLE pRPLOCALSOld = pRPLOCALS;
  // set pRPLOCALS to the start of local variables
  pRPLOCALS = (PSINGLE)pRP;

  // move pADDR on to first word to execute
  pADDR++;
  while (*pADDR != PFA_ADDR_EXIT)
  {
    PFORTHDICTENT pDE = ADDR_TO_PTR(*pADDR);
    // check that we are pointing into the active Dictionary, at least
    CHECK_DICT_PTR(pDE);

    // set IP to the *next* word to be executed
    // (this means that callees can access position in CALLER via IP:
    //   pADDR[0..] are words after currently executing word (CALLER_INLINE_ADDRS())
    //   pADDR[-1] is word being executed (CALLER_DICTENT())
    // )
    pIP = pADDR + 1;

    // execute the word
    if (g_pTRACE_VARS->debug)
    {
      // push the current Colon Definition/Instruction Address to the Backtrace Stack
      BTP_PUSH(PTR_TO_ADDR(pDEColon));
      BTP_PUSH(PTR_TO_ADDR(pADDR));
      forth_DEBUG_EXECUTE_DICTENT(pADDR, &debug_suspend_till_out);
      // pop the current Colon Definition/Instruction Address from the Backtrace Stack
      BTP_POP();
      BTP_POP();
    }
    else
      forth_EXECUTE_DICTENT(pDE);

    // move on to the next word to execute, normally the word after the one just executed i.e. pIP above
    // (note that this means that callees can alter IP to change where CALLER executes next)
    pADDR = pIP;
  }

  // restore the previous value of pRPLOCALS
  pRPLOCALS = pRPLOCALSOld;
  // restore the previous value of RP
  pRP = pRPOld;
  // restore the previous value of IP
  pIP = pIPOld;

  // if did a Debug "Out", now resume Debugging on coming out of the definition
  if (g_pTRACE_VARS->debug)
    if (debug_suspend_till_out)
      g_pTRACE_VARS->debug_suspend = FALSE;
}

// **************************************
// ********** </EXECUTE COLON> **********
// **************************************


// ***************************************
// ********** <EXECUTE DOES_GT> **********
// ***************************************

// EXECUTE_DOES_GT
static void forth_EXECUTE_DOES_GT(const PFORTHDICTENT pDEColon, PPFA pPFA)
{
  // get the pointer to the DOES> code stored in addrs[0]
  // pDEColon is the Colon Definition being executed
  PADDR pDoesGt = ADDR_TO_PTR(pPFA->addrs[0]);
  CHECK_DICT_PTR(pDoesGt);
  // push ADDR of start of usable PFA to the stack
  // this is immediately after the address of the DOES> code stored in addrs[0]
  SP_PUSH_PTR(pPFA->addrs + 1);
  // execute colon, starting at the address of DOES> code
  forth_EXECUTE_COLON(pDEColon, pDoesGt - 1);
}

// ****************************************
// ********** </EXECUTE DOES_GT> **********
// ****************************************


// *************************************
// ********** <EXECUTE EXVEC> **********
// *************************************

// EXECUTE_EXVEC
static void forth_EXECUTE_EXVEC(ADDR addr)
{
  // execute the FORTHDICTENT which the vector is assigned to do
  PFORTHDICTENT pDE = ADDR_TO_PTR(addr);
  CHECK_DICT_PTR(pDE);
  forth_EXECUTE_DICTENT(pDE);
}

// **************************************
// ********** </EXECUTE EXVEC> **********
// **************************************


// ***************************************
// ********** <EXECUTE DICTENT> **********
// ***************************************

// DEBUG_EXECUTE_DICTENT
static void forth_DEBUG_EXECUTE_DICTENT(const PADDR pADDR, bool *pDebug_suspend_till_out)
{
  // execute a Dictionary Entry with Debugging
  // pADDR is the instruction to be executed in the Colon Definition
  // set *pDebug_suspend_till_out = TRUE if want to resume Debugging after completing this Dictionary Entry

#ifndef	READLINE
  forth_ERROR(err_NOT_IMPLEMENTED);
#else
  PFORTHDICTENT pDEColon = ADDR_TO_PTR(*BTP_1());	// Colon Definition being executed
  PADDR UNUSED(pCurrentInstruction) = ADDR_TO_PTR(*BTP_0());	// current Instruction
  PFORTHDICTENT pDE = ADDR_TO_PTR(*pADDR);		// current word to execute
  // if Debugging suspended, resume Debugging if Dictionary Entry has breakpoint
  if (g_pTRACE_VARS->debug_suspend)
  {
    // determine whether this Dictionary Entry has breakpoint
    if (g_pTRACE_VARS->count_debug_dictents > 0)
    {
      PADDR pDEBreak = forthFindBreakpoint(*pADDR);
      // if it does, resume Debugging
      if (pDEBreak != NULL)
        g_pTRACE_VARS->debug_suspend = FALSE;
    }
    // if Debugging is suspended, return to normal execution
    if (g_pTRACE_VARS->debug_suspend)
    {
      forth_EXECUTE_DICTENT(pDE);
      return;
    }
  }
  // verify input/output is Terminal
  if (!external_isatty(external_fileno(stdin)) || !external_isatty(external_fileno(stdout)))
  {
    forth_ERROR(err_BAD_PARAM);
    return;
  }
  // output the Colon Definition, highlighting the execution point
  forthDecompileDictEnt(pDEColon, pADDR);
  external_fflush(stdout);

  do
  {
    static char last_key = 'n';	// initially <Return> => Next
    // get a key, without echoing
    char key = readline_get_key();
    if (key == '\004')		// <^D>/EOF
      key = 'q';
    if (key == '\r')		// <Return>
      key = last_key;
    last_key = key;
    // suspend Debugging
    g_pTRACE_VARS->debug_suspend = TRUE;
    switch (key)
    {
    case '.':			// Print Stack
      prim_DOT_S();
      break;
    case 'b':			// Break
    case 'u':			// Unbreak
      // get word to Break/Unbreak on
      forth_KEYBOARD_QUERY((key == 'b') ? "BREAK " : "UNBREAK ", FALSE);
      if (key == 'b')
        prim_BREAK();
      else
        prim_UNBREAK();
      break;
    case 'c':			// Continue
      // Continue without Debug
      forth_EXECUTE_DICTENT(pDE);
      return;
    case 'l':			// List
      // list Breakpoints
      forthListBreakpoints();
      external_fflush(stderr);
      break;
    case 'n':			// Next
      // Step Over without Debug
      forth_EXECUTE_DICTENT(pDE);
      // resume Debugging
      g_pTRACE_VARS->debug_suspend = FALSE;
      return;
    case 'o':			// Out
      // let forth_EXECUTE_COLON know to resume Debugging on coming out of the definition
      *pDebug_suspend_till_out = TRUE;
      // return to normal execution
      forth_EXECUTE_DICTENT(pDE);
      return;
    case 'q':			// Quit
      // disable Debugging, return to normal execution
      g_pTRACE_VARS->debug = FALSE;
      CLEAR_BACKTRACE_STACK();
      // return to top-level
      forth_ERROR(err_INTERRUPT);
      return;
    case 's':			// Step
      // resume Debugging
      g_pTRACE_VARS->debug_suspend = FALSE;
      // Step In with Debug
      forth_EXECUTE_DICTENT(pDE);
      return;
    case 't':			// backTrace
      // display the Backtrace Stack
      forthDebugBacktrace();
      break;
    default:
      fprintf(stderr, "? [b,c,l,n,o,q,s,t,u]");
      external_fflush(stderr);
      break;
    }
  } while (TRUE);
#endif
}

// TRACE_EXECUTE_DICTENT
static void forth_TRACE_EXECUTE_DICTENT(const PFORTHDICTENT pDE, bool prePost)
{
  // do Tracing prior to or after executing a Dictionary Entry

  // determine whether this Dictionary Entry's execution is being traced
  bool trace = FALSE;
  if (g_pTRACE_VARS->trace_all)
    trace = TRUE;
  else if (g_pTRACE_VARS->trace_colons && pDE->cfa == cfe_colon)
    trace = TRUE;
  else if (forthFindTracepoint(PTR_TO_ADDR(pDE)) != NULL)
    trace = TRUE;
  if (!trace)
    return;

  // push the Dictionary Entry followed by the pre/post flag to the stack
  // and call TRACE_EXECUTE
  SP_PUSH_PTR(pDE);
  SP_PUSH(prePost);
  // set trace_suspend while calling TRACE_EXECUTE so that it does not trace itself/what it calls
  g_pTRACE_VARS->trace_suspend = TRUE;
  vect_TRACE_EXECUTE();
  g_pTRACE_VARS->trace_suspend = FALSE;
}

// EXECUTE_DICTENT
static void forth_EXECUTE_DICTENT(const PFORTHDICTENT pDE)
{
  // execute a Dictionary Entry

  // determine whether this Dictionary Entry's execution is being traced
  if (g_pTRACE_VARS->trace && !g_pTRACE_VARS->trace_suspend)
    forth_TRACE_EXECUTE_DICTENT(pDE, FALSE);

  switch (pDE->cfa)
  {
  case cfe_primitive:		// PRIMITIVES
    // execute primitive
    forth_EXECUTE_PRIMITIVE(pDE->pfa.primitives[0]);
    break;

  case cfe_create:		// CREATE
    // push ADDR of start of PFA to the stack
    SP_PUSH_PTR(&pDE->pfa);
    break;

  case cfe_constant:		// CONSTANT
    // push SINGLE value at PFA to the stack
    SP_PUSH(pDE->pfa.singles[0]);
    break;

  case cfe_double_constant:	// DCONSTANT
    // push DOUBLE value at PFA to the stack
    SP_PUSH_DOUBLE(pDE->pfa.doubles[0]);
    break;

  case cfe_float_constant:	// FCONSTANT
    // push FLOAT value at PFA to the stack
    SP_PUSH_FLOAT(pDE->pfa.floats[0]);
    break;

  case cfe_string_constant:	// $CONSTANT
    // push ADDR of FORTHSTRING value at PFA to the stack
    SP_PUSH_PTR(&pDE->pfa.conststring[0]);
    break;

  case cfe_variable:		// VARIABLE
    // push ADDR of SINGLE value at PFA to the stack
    SP_PUSH_PTR(&pDE->pfa.singles[0]);
    break;

  case cfe_double_variable:	// DVARIABLE
    // push ADDR of DOUBLE value at PFA to the stack
    SP_PUSH_PTR(&pDE->pfa.doubles[0]);
    break;

  case cfe_float_variable:	// FVARIABLE
    // push ADDR of FLOAT value at PFA to the stack
    SP_PUSH_PTR(&pDE->pfa.floats[0]);
    break;

  case cfe_string_variable:	// $VARIABLE
    // push ADDR of FORTHMAXLENSTRING *string* value at PFA to the stack
    SP_PUSH_PTR(&pDE->pfa.varstring[0].string);
    break;

  case cfe_user_variable:	// USER VARIABLE
    // push ADDR of SINGLE value at (UP + byte at PFA) to the stack
    SP_PUSH_PTR(pUP + pDE->pfa.bytes[0]);
    break;

  case cfe_local_variable:	// LOCAL VARIABLE (executes in Compilation mode)
    // execute localvar
    forth_EXECUTE_LOCAL_VAR(pDE->pfa.singles[0]);
    break;

  case cfe_colon:		// COLON DEFINITION
    // execute colon, starting at PFA
    forth_EXECUTE_COLON(pDE, pDE->pfa.addrs - 1);
    break;

  case cfe_does_gt:		// DOES> DEFINITION
    // execute does>
    forth_EXECUTE_DOES_GT(pDE, &pDE->pfa);
    break;

  case cfe_exvec:		// EXECUTION VECTOR
    // execute exvec
    forth_EXECUTE_EXVEC(pDE->pfa.addrs[0]);
    break;

  default:
    fprintf(stderr, "Unrecognised \"cfe\": %d\n", pDE->cfa);
    forth_ERROR(err_0);
    break;
  }

  // determine whether this Dictionary Entry's execution is being traced
  if (g_pTRACE_VARS->trace && !g_pTRACE_VARS->trace_suspend)
    forth_TRACE_EXECUTE_DICTENT(pDE, TRUE);
}

// ****************************************
// ********** </EXECUTE DICTENT> **********
// ****************************************


// ***********************************
// ********** <INTERPRETER> **********
// ***********************************

// INTERPRET
static void forth_INTERPRET(void)
{
  // outer text interpreter
  do
  {
    // get characters from the Input Stream
    // up to delimiting ' ' character, skipping leading ' 's
    // copying to pWBFR with leading length byte
    forth_WORD(' ');
    if (pWBFR->len == 0) // Input Stream exhausted, exit
      break;

    // (try to) find the word
    PFORTHDICTENT pDE = forthFindDictEnt(pWBFR->chars);
    if (pDE != NULL)
    {
      // execute/compile the word
      if (EXECUTION_MODE() || pDE->nfa.flag.immediate)
	forth_EXECUTE_DICTENT(pDE);
      else
      	forthDictStoreDictEnt(pDE);
    }
    else
    {
      // (try to) convert the word to a number
      // leaves the number on the stack
      PBYTE pSPOld = pSP;
      SP_PUSH_PTR(pWBFR);
      vect_NUM();
      bool isNum = (pSP < pSPOld);

      if (!isNum)
      {
	// if not found, output the word and a "?", and ERROR
	forth_ERROR_WORD_NOT_FOUND(pWBFR);
      }

      // compile the number? (else leave it on the stack)
      if (COMPILATION_MODE())
      {
	if (pSPOld - pSP == sizeof(SINGLE))
	  prim_LITERAL();
	else if (pSPOld - pSP == sizeof(DOUBLE))
	  prim_D_LITERAL();
	else if (pSPOld - pSP == sizeof(FLOAT))
	  prim_F_LITERAL();
	else
    {
	  forth_ERROR(err_BAD_PARAM);
      return;
    }
      }
    }

    // repeat
  } while (TRUE);
}

// TOP_LEVEL_INTERPRETER
static void forth_TOP_LEVEL_INTERPRETER(void)
{
  // run the top-level interpreter

  // close any open file Input Stream
  forth_FILE_IN_CLOSE();
  // set Input Stream to come from keyboard/Terminal Input Buffer or from file if stdin is not a tty
  forth_FILE_IN_STDIN();

  do
  {
    // prompt is "OK> " if in Execution Mode, ":> " if in Compilation Mode (i.e. definition not finished)
    const char *exec_prompt = EXECUTION_MODE() ? "OK> " : ":> ";
    char prompt[10];
#ifdef	SEMAPHORE
    if (g_bHaveForked)
      sprintf(prompt, "[%d] %s", forth_getpid(), exec_prompt);
    else
      sprintf(prompt, "%s", exec_prompt);
#else
    sprintf(prompt, "%s", exec_prompt);
#endif
    // input characters from Input Stream
    if (forth_QUERY(prompt) < 0) // EOF
      break;

    // outer text interpreter
    forth_INTERPRET();

    // repeat
  } while (TRUE);
}

// STRING_INTERPRET
static void forth_STRING_INTERPRET(PFORTHSTRING pStr)
{
  if (pStr == NULL)
    forth_ERROR(err_SYS_MEM);

  // save the current Input Stream position
  FORTHFILEINSAVEPOSITION savePos;
  forth_FILE_IN_SAVE_POSITION(&savePos);

  // open the string as Input Stream
  forth_FILE_IN_STRING(pStr);

  // run the interpreter to read the string
  // unlike other interpreters, no loop, only call for string input once
  // input characters from string
  forth_STRING_QUERY();

  // outer text interpreter
  forth_INTERPRET();

  // close the file Input Stream
  forth_FILE_IN_CLOSE();

  // restore the saved Input Stream position
  forth_FILE_IN_RESTORE_POSITION(&savePos);
}

// QUERY_LOAD_INTERPRET
static bool forth_QUERY_LOAD_INTERPRET(const char *pFname)
{
  if (pFname == NULL)
    forth_ERROR(err_SYS_MEM);

  // save the current Input Stream position
  FORTHFILEINSAVEPOSITION savePos;
  forth_FILE_IN_SAVE_POSITION(&savePos);

  // try to open the file as Input Stream
  if (! forth_FILE_IN_OPEN(pFname))
    return FALSE; // failed to open file

  // run the interpreter to read the file
  do
  {
    // input characters from file to Mass Storage Input Buffer
    if (forth_FILE_QUERY() < 0) // EOF
      break;

    // outer text interpreter
    forth_INTERPRET();

    // repeat
  } while (TRUE);

  // close the file Input Stream
  forth_FILE_IN_CLOSE();

  // restore the saved Input Stream position
  forth_FILE_IN_RESTORE_POSITION(&savePos);

  return TRUE; // succeeded in opening file
}

// LOAD_INTERPRET
static void forth_LOAD_INTERPRET(const char *pFname)
{
  // must open the file as Input Stream
  if (!forth_QUERY_LOAD_INTERPRET(pFname))
    forth_ERROR(err_BAD_FILE);
}

// FORTHRC_INTERPRETER
static void forth_FORTHRC_INTERPRETER(void)
{
  // try to open as file Input Stream a ".forthrc" file in the current directory
  forth_QUERY_LOAD_INTERPRET("./.forthrc");
}

// ************************************
// ********** </INTERPRETER> **********
// ************************************


// *******************************
// ********** <STARTUP> **********
// *******************************

// top-level setjmp/longjmp
static jmp_buf forthMAIN_jmp_buf_env;
typedef enum forthMAIN_longjmp_val
{
  longjmp_0 = 0,
  longjmp_COLD,
  longjmp_WARM,
  longjmp_ABORT,
  longjmp_ERROR,
  longjmp_OS_ERROR,
  longjmp_QUIT,
  longjmp_TOP_LEVEL_INTERPRETER,
} forthMAIN_longjmp_val_t;

static void forthMAIN_longjmp(forthMAIN_longjmp_val_t val)
{
  // longjmp into forthMAIN(), restart at <val>
  // this clears the Return Stack
  longjmp(forthMAIN_jmp_buf_env, val);
}

// QUIT
static void forth_QUIT(void)
{
  // clear the Return & Backtrace Stacks
  CLEAR_RETURN_STACK();
  CLEAR_BACKTRACE_STACK();

  // longjmp into forthMAIN(), restart at TOP_LEVEL_INTERPRETER
  forthMAIN_longjmp(longjmp_TOP_LEVEL_INTERPRETER);
}

// MSG#
static void forth_MSG_HASH(FORTH_ERR_NUM err)
{
  // print the standard error message
  // (except when err == 0, which is used by "word not found")
  if (err != err_0)
  {
    PFORTHDICTENT pDE = CALLER_DICTENT();
    PNFA pNFA = SAFE_PNFA(pDE);
    if (pNFA != NULL)
      fprintf(stderr, "? %s ", pNFA->name);
    fprintf(stderr, "MSG # %d", err);
    fprintf(stderr, " (%s)", forth_ERR_MSG(err));
    forth_SHOW_ERR_FILE_LINE();
    fprintf(stderr, "\n");
  }
}

// MESSAGE
static void forth_MESSAGE(FORTH_ERR_NUM err)
{
  PFORTHDICTENT pDEMessage = forthFindDictEnt("MESSAGE");
  if (pDEMessage != NULL)
  {
    SP_PUSH(err);
    // execute the FORTHDICTENT which the vector is assigned to do
    forth_EXECUTE_DICTENT(pDEMessage);
  }
  else
    forth_MSG_HASH(err);
}

// OSERRNO
static void forth_OS_ERRNO(void)
{
    SP_PUSH(external_errno());
}

// OSERROR
static void forth_OS_ERROR(void)
{
  // is this error only a warning?
  bool isWarning = FALSE;
  // are all errors warnings (safe check)
  if (!g_bExitOnError)
    if (pUP)
      if (g_bERR_WARN)
        isWarning = TRUE;

  // flush any pending output, before error message
  external_fflush(stdout);
  external_fflush(g_fpOUT);

  if (g_bExitOnError)
    fprintf(stderr, "\nEXIT ");
  else
  {
    PFORTHDICTENT pDE = CALLER_DICTENT();
    PNFA pNFA = SAFE_PNFA(pDE);
    if (pNFA != NULL)
      fprintf(stderr, "? %s ", pNFA->name);
  }
  fprintf(stderr, "OS ERROR # %d", external_errno());
  fprintf(stderr, " (%s)", external_strerror(external_errno()));
  forth_SHOW_ERR_FILE_LINE();
  fprintf(stderr, "\n");
  if (g_bExitOnError)
  {
    // exit with error
    external_exit(1);
  }

  // set the flag now, so that any error inside forth_ERROR causes exit
  g_bExitOnError = TRUE;

  if (!isWarning)
  {
    // clear the Return & Backtrace Stacks
    CLEAR_RETURN_STACK();
    CLEAR_BACKTRACE_STACK();
  }

  // execution continues if it's only a warning
  if (isWarning)
  {
    g_bExitOnError = FALSE;
    return;
  }

  // longjmp into forthMAIN(), restart at OS_ERROR
  forthMAIN_longjmp(longjmp_OS_ERROR);
}

// ERROR
static void forth_ERROR(FORTH_ERR_NUM err)
{
  // is this error only a warning?
  bool isWarning = (err == err_NOT_UNIQUE);
  // set last error number
  if (pUP)
    *user_var_ERRNUM = err;
  // are all errors warnings (safe check)
  if (!g_bExitOnError)
    if (pUP)
    {
      // ASSERTs always cause all errors to be errors
      // and err_SYS_MEMs are too hard to handle so also unconditional error
      if (err == err_ASSERT_FAIL || err == err_SYS_MEM)
        *user_var_ERR_WARN = 0;
      else if (g_bERR_WARN)
        isWarning = TRUE;
    }

  // flush any pending output, before error message
  external_fflush(stdout);
  external_fflush(g_fpOUT);

  // if the flag g_bExitOnError is set
  // --- which is set during COLD and during forth_ERROR, and cleared when the top-level interpreter is reached ---
  // exit FORTH now
  if (g_bExitOnError && !isWarning)
  {
    fprintf(stderr, "\nEXIT ERROR # %d", err);
    fprintf(stderr, " (%s)", forth_ERR_MSG(err));
    forth_SHOW_ERR_FILE_LINE();
    fprintf(stderr, "\n");
    // exit with error
    external_exit(1);
  }
  // set the flag now, so that any error inside forth_ERROR causes exit
  g_bExitOnError = TRUE;

  if (!isWarning)
  {
    // clear the Computation Stack
    CLEAR_COMPUTATION_STACK();
    // clear the Return & Backtrace Stacks
    CLEAR_RETURN_STACK();
    CLEAR_BACKTRACE_STACK();
  }
  else
  {
    // have to clear the Computation Stack at least for forth_MESSAGE()
    if (err == err_STACK_EMPTY || err == err_STACK_FULL)
      CLEAR_COMPUTATION_STACK();
  }

  // call MESSAGE to print the standard error message
  forth_MESSAGE(err);

  // execution continues if it's only a warning
  if (isWarning)
  {
    g_bExitOnError = FALSE;
    return;
  }

  // longjmp into forthMAIN(), restart at ERROR
  forthMAIN_longjmp(longjmp_ERROR);
}

// MESSAGE_WORD_NOT_FOUND
static void forth_MESSAGE_WORD_NOT_FOUND(PFORTHSTRING pWord)
{
  // when a word is not found, output the word and a "?"
  external_fflush(stdout);
  external_fflush(g_fpOUT);
  fprintf(stderr, "%.*s ?\n", pWord->len, pWord->chars);
  external_fflush(stderr);
}

// ERROR_WORD_NOT_FOUND
static void forth_ERROR_WORD_NOT_FOUND(PFORTHSTRING pWord)
{
  // when a word is not found, output the word and a "?", and ERROR
  forth_MESSAGE_WORD_NOT_FOUND(pWord);
  forth_ERROR(err_0);
}

// (ABORT)
static void forth_BRACKET_ABORT(void)
{
  // clear the Computation Stack
  CLEAR_COMPUTATION_STACK();
  // clear the Return & Backtrace Stacks
  CLEAR_RETURN_STACK();
  CLEAR_BACKTRACE_STACK();

  // if the LAST Dictionary Entry is incomplete
  // (e.g. because there was an error during Compilation)
  // delete it now
  if (pLAST != NULL && pLAST->nfa.flag.smudge)
    forthForgetLastDictEnt();
  // set Execution Mode
  SET_EXECUTION_MODE();

  // longjmp into forthMAIN(), restart at QUIT
  forthMAIN_longjmp(longjmp_QUIT);
}

// ABORT
static void forth_ABORT(void)
{
  PFORTHDICTENT pDEAbort = forthFindDictEnt("ABORT");
  if (pDEAbort != NULL)
  {
    // execute the FORTHDICTENT which the vector is assigned to do
    forth_EXECUTE_DICTENT(pDEAbort);
  }
  else
    forth_BRACKET_ABORT();
}

// (WARM)
static void forth_BRACKET_WARM(void)
{
  // clear the Return & Backtrace Stacks
  CLEAR_RETURN_STACK();
  CLEAR_BACKTRACE_STACK();

  // Decimal numeric Base
  *user_var_BASE = 10;

  // longjmp into forthMAIN(), restart at ABORT
  forthMAIN_longjmp(longjmp_ABORT);
}

// WARM
static void forth_WARM(void)
{
  fprintf(stdout, "WARM\n");
  forth_BRACKET_WARM();
}

// INITVECS
static void forth_INITVECS(void)
{
  forth_INITVEC("ABORT", primitive_BRACKET_ABORT);
  forth_INITVEC("CREATE", primitive_BRACKET_CREATE);
  forth_INITVEC("EMIT", primitive_BRACKET_EMIT);
  forth_INITVEC("EMITS", primitive_BRACKET_EMITS);
  forth_INITVEC("KEY", primitive_BRACKET_KEY);
  forth_INITVEC("MESSAGE", primitive_MSG_HASH);
  forth_INITVEC("NUM", primitive_BRACKET_NUM);
  forth_INITVEC("TRACE-EXECUTE", primitive_BRACKET_TRACE_EXECUTE);
}

// (COLD)
static void forth_BRACKET_COLD(void)
{
  fprintf(stdout, "COLD\n");

  // the whole of the Forth Machine!
  g_pFM = FORTH_MACHINE; g_pFM_HIGH = g_pFM + FORTH_MACHINE_SIZE;

  // regions/stacks
  g_pCS_LOW = g_pFM + 0x0000; g_pCS = g_pCS_LOW + COMPUTATION_STACK_SIZE;
  g_pRS_LOW = g_pFM + 0x0200; g_pRS = g_pRS_LOW + RETURN_STACK_SIZE;
  g_pBTS_LOW = g_pFM + 0x0400; g_pBTS = g_pBTS_LOW + BACKTRACE_STACK_SIZE;
  g_pUV = g_pFM + 0x0600; g_pUV_HIGH = g_pUV + USER_VARIABLES_SIZE;
  g_pWB = g_pFM + 0x0800; g_pWB_HIGH = g_pWB + WORD_BUFFER_SIZE;
  g_pTIB = g_pFM + 0x0A00; g_pTIB_HIGH = g_pTIB + TERMINAL_INPUT_BUFFER_SIZE;
  g_pSCP = g_pFM + 0x0C00; g_pSCP_HIGH = g_pSCP + SCRATCH_PAD_SIZE;
  g_pLOCALSD = g_pFM + 0x0E00; g_pLOCALSD_HIGH = g_pLOCALSD + LOCALS_DICT_SIZE;
  g_pFD = g_pFM + 0x1000; g_pFD_HIGH = g_pFD + FORTH_DICTIONARY_SIZE;
  g_pMSB_HIGH = g_pFM_HIGH - 0x0400; g_pMSB = g_pFM_HIGH - MASS_STORAGE_BUFFERS_SIZE;

  // USER Variables region
  pUP = g_pUV;
  // set so that forth_ERROR()s are errors
  *user_var_ERR_WARN = 0;
  // clear last error number
  *user_var_ERRNUM = 0;

  if (g_pMSB < g_pFD_HIGH)
    forth_ERROR(err_SYS_MEM);

  g_pTRACE_VARS = (PTRACEVARS)(g_pUV + 0x0100);
  if (((PBYTE)g_pTRACE_VARS) + sizeof(TRACEVARS) >= g_pUV_HIGH)
    forth_ERROR(err_SYS_MEM);

  // Computation Stack
  *user_var_S0 = PTR_TO_ADDR(g_pCS);
  pSP = pS0;

  // Return Stack
  *user_var_R0 = PTR_TO_ADDR(g_pRS);
  pRP = pR0;

  // Backtrace Stack
  *user_var_BT0 = PTR_TO_ADDR(g_pBTS);
  pBTP = pBT0;

  // Decimal numeric Base
  *user_var_BASE = 10;

  // WORD Buffer region
  pWBFR = (PFORTHSTRING)g_pWB;
  // Terminal Input Buffer region
  *user_var_TIB = PTR_TO_ADDR(g_pTIB);
  // Pad in middle of Scratch Pad region
  pPAD = (PFORTHSTRING)(g_pSCP + 50);

  // mass storage buffers constants
  pFIRST = g_pMSB;
  pLIMIT = g_pMSB_HIGH;

  // set Input Stream to stdin
  forth_FILE_IN_STDIN();

  // clear any tracing
  memset(g_pTRACE_VARS, 0, sizeof(TRACEVARS));

  // set Dictionary lookup to be case-sensitive
  dictcmp = strncmp;
  // create bootup dictionary
  forthBootupDict();

  // initialise all vectored words to defaults
  forth_INITVECS();

  // read in any ".forthrc" file
  forth_FORTHRC_INTERPRETER();

  // longjmp into forthMAIN(), restart at (WARM)
  forthMAIN_longjmp(longjmp_WARM);
}

// COLD
static void forth_COLD(void)
{
  // longjmp into forthMAIN(), restart at COLD
  forthMAIN_longjmp(longjmp_COLD);
}

// ESCAPE
static void forthSIGINT_HANDLER(int signum)
{
  // restore signal handler to default
  signal(signum, SIG_DFL);
  // and allow it to be generated again
//  sigrelse(signum);
  sigset_t sigset;
  sigemptyset(&sigset);
  sigaddset(&sigset, signum);
  sigprocmask(SIG_UNBLOCK, &sigset, NULL);

  // raise Interrupt error
  forth_ERROR(err_INTERRUPT);
}

// MAIN
static void forthMAIN(void)
{
  forthMAIN_longjmp_val_t setjmp_return;

  // top-level setjmp(), all longjmp()s return here
  setjmp_return = setjmp(forthMAIN_jmp_buf_env);

  // set so that any error exits FORTH
  g_bExitOnError = TRUE;
  if (pUP)
  {
    // set that errors are errors
    *user_var_ERR_WARN = 0;
    // clear last error number
    *user_var_ERRNUM = 0;
  }
  // set SIGINT (Ctrl+C) to do raise a FORTH error
  signal(SIGINT, forthSIGINT_HANDLER);

  // clear IP from whatever it might be pointing to
  pIP = NULL;

  // ensure output to console
  forthSetConsoleOutput();

  switch (setjmp_return)
  {
  case longjmp_0:			// "returning directly"/first time
    fprintf(stdout, "\nJBC FORTH (C) 2016\n");

  case longjmp_COLD:			// COLD
    forth_BRACKET_COLD();

  case longjmp_WARM:			// (WARM)
    forth_BRACKET_WARM();

  case longjmp_ABORT:			// ABORT
  case longjmp_ERROR:			// ERROR
    forth_ABORT();

  case longjmp_OS_ERROR:		// OSERROR
  case longjmp_QUIT:			// QUIT
    forth_QUIT();

  case longjmp_TOP_LEVEL_INTERPRETER:	// run the top-level interpreter
  default:
    // now set so that any error stays in FORTH
    g_bExitOnError = FALSE;

    // start the top-level interpreter
    forth_TOP_LEVEL_INTERPRETER();
    // EOF from top-level interpreter

    // set so that any error exits FORTH
    g_bExitOnError = TRUE;

    // check for an incomplete Dictionary Definition at this point
    forthCheckCompleteLastDictEnt();
  }

  // close any open file Input Stream
  forth_FILE_IN_CLOSE();

  // close any open file Output Stream
  forthSetConsoleOutput();

  fprintf(stdout, "BYE\n");
}

// ********************************
// ********** </STARTUP> **********
// ********************************


// ****************************
// ********** <MAIN> **********
// ****************************

// main
int main(int UNUSED(argc), char* UNUSED(argv[]))
{
  forth_ERR_FILE = NULL;
  g_bExitOnError = TRUE;

  g_fpOUT = stdout;

#ifdef	SEMAPHORE
  // initialise the global semaphore
  forth_seminit();
#endif	// SEMAPHORE

  // allocate the FORTH Machine!
  FORTH_MACHINE = external_malloc(FORTH_MACHINE_SIZE);
  if (FORTH_MACHINE == NULL)
  {
    external_perror("malloc");
    external_exit(1);
  }

  // run the Forth MAIN loop
  forthMAIN();

  // exit with success
  external_exit(0);
}

// *****************************
// ********** </MAIN> **********
// *****************************
