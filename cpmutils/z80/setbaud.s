bdos    equ     5

        org     0x100

        LD      HL,0x82
skip_spaces:
        LD      A,(HL)
        INC     HL
        CP      0x20
        JR      Z,skip_spaces
l_109:
        SUB     '0'
        JP      M,invalid_baud_rate
        CP      10
        JP      P,invalid_baud_rate
        LD      E,A
        LD      D,0x0
find_value:
        LD      A,(HL)
        SUB     '0'
        JP      M,end_of_value
        CP      10
        JP      P,end_of_value
        INC     HL
        PUSH    HL
        EX      DE,HL                       ; DE=DE*10
        ADD     HL,HL
        PUSH    HL
        ADD     HL,HL
        ADD     HL,HL
        POP     DE
        ADD     HL,DE
        EX      DE,HL
        POP     HL
        ADD     A,E
        LD      E,A
        LD      A,D
        ADC     A,0                         ; Check for overflow
        JR      C,invalid_baud_rate
        LD      D,A
        JR      find_value
end_of_value:
        EX      DE,HL
        LD      A,0x5                       ; SETBAUD
        OUT     (11h),A                     ; QDOS CALL
        AND     A
        RET     Z
invalid_baud_rate:
        LD      DE,invalid_baud_rate
        LD      C,0x9
        JP      bdos
invalid_baud_rate:
        ds      "ERROR: Missing or invalid baud rate$"
