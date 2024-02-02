;
; Formatting:
;_______________I___________O_______________________________C
;               17          29                              61
;               Instr.      Operand                         Comment
;

TRAP_SVC       equ      0
TRAP_JOB       equ      1
TRAP_IO        equ      2
TRAP_STRM      equ      3
;
; JOB(#1) functions
;
MT_CJOB         equ     1
MT_IPCOM        equ     $11
MT_BAUD         equ     $12
;
; IO(#2) functions
;
IO_PEND         equ     0
IO_OPEN         equ     1
IO_CLOSE        equ     2
IO_FORMT        equ     3
IO_DELET        equ     4
;
; STRM(#3) functions
;
IO_FBYTE        equ     1
IO_FSTRG        equ     3
IO_SBYTE        equ     5
IO_SSTRG        equ     7
SD_CHENQ        equ     $0b
SD_CURENA       equ     $0e
SD_CURS         equ     $0f
SD_POS          equ     $10
SD_TAB          equ     $11
SD_PCOL         equ     $13
SD_NCOL         equ     $14
SD_PROW         equ     $15
SD_NROW         equ     $16
SD_SCROL        equ     $18
SD_SCRTP        equ     $19
SD_SCRBT        equ     $1a
SD_CLEAR        equ     $20
SD_CLRBT        equ     $22
SD_CLRLN        equ     $23
SD_CLRRT        equ     $24
SD_SETPA        equ     $27
SD_SETST        equ     $28
SD_SETIN        equ     $29
SD_SETUL        equ     $2b
FS_POSAB        equ     $42
IO_FBYTE        equ     1
IO_SSTRG        equ     7
;
; Fixed vectors
;
IO_QTEST        equ     $00de
IO_QOUT         equ     $00e0
;
; System variables
;
SV_KEYQ        equ      $2804c
SV_CQCH        equ      $28092
