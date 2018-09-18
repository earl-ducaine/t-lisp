#define S0 r0
#define S1 r1
#define S2 r2
#define S3 r3
#define NARGS r4
#define P r5
#define A1 r6
#define A2 r7
#define A3 r8
#define A4 r9
#define T r10
#define NIL r11
#define REST ap

#define FIX_TAG 0
#define IMM_TAG 1
#define STORED_TAG 2
#define LIST_TAG 3

#define STEMPLATE (8+IMM_TAG)
#define HTEMPLATE (0x80+STEMPLATE)
