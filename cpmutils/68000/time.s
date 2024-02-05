

                dc.b        $01,$fb,$4a                     ; ld bc,$4afb
                dc.b        $21,$16,$01                     ; ld hl,$0116
                dc.b        $d3,$13                         ; out (19),a
                dc.b        $d3,$01                         ; out (1),a
                dc.b        $c9                             ; ret
                ds         "<68K CODE>"
        ;
        ; This is at address $0116, when loaded on CP/M
                movem.l     d0-d7/a0-a6,-(sp)
                suba.l      a6,a6
                movea.l     a0,a5
                moveq       #MT_RCLCK,D0
                trap        #0x1
                lea         date_string(PC),A1
                ; Convert time to ASCII
                ; D1 - Time in seconds
                ; A1 - Output buffer
                movea.w     CN_DATE.w,A2
                jsr         (A2)
                movea.l     A5,A0
                lea         print_buffer(PC),A1
                addq.w      #0x1,(A1)
                movea.w     UT_MTEXT.w,A2
                jsr         (A2)
                movem.l     (SP)+,d0-d7/a0-a6
                rts
; At 0x40
text_bufer:
                dc.b        0,0,0,0,0,0,0,0
                dc.b        0,0,0,0,0,0,0,0
                dc.b        0,0,0,0,0,0
; At 0x56
date_string:
                dc.b        $0a,$00
                dc.b        $1a,$1a,$1a,$1a,$1a,$1a,$1a,$1a,$1a,$1a
                dc.b        $1a,$1a,$1a,$1a,$1a,$1a,$1a,$1a,$1a,$1a       
                dc.b        $1a,$1a,$1a,$1a,$1a,$1a,$1a,$1a,$1a,$1a       
                dc.b        $1a,$1a,$1a,$1a,$1a,$1a,$1a,$1a,$1a,$1a       
