        include "qdos.i"
        include "success.i"


EXIT            equ         0
SETPOS          equ         1
SETINV          equ         2
SETPAPER        equ         3
CLRSCR          equ         4
SETFLASH        equ         5
SETUNDER        equ         6
SETSIZE         equ         7
SETCURS         equ         8
SETBORD         equ         9

output_handle   equ         0
file_handle     equ         4
buffer_index    equ         8
base_of_z80     equ         10

buffer_size     equ         $1a0
                             //
                             // ram 
                             // ram:00000000-ram:000009ff
                             //
                dc.b        $21,$00,$02     ; ld bc,$200
                dc.b        $01,$fb,$4a     ; ld hl,$4afb
                dc.b        $d3,$13         ; out (19),a
                dc.b        $c7             ; ret
                ds.b        $100-*
    lea        workspace(pc),a6
                move.l                 a5,base_of_z80(a6)
                move.l                 a0,output_handle(a6)
                movea.l    flp_handle(A4),a0
    clr.l      flp_handle(A4)
                moveq      #IO_CLOSE,d0
                trap       #TRAP_IO
                movea.l    output_handle(a6),a0
                bsr.w      inline_string                                                                               undefined FUN_0000049c()
                dc.b       CLRSCR,'SUCCESS Create CPMFILES utility.',10
                dc.b       'By B Watson (C) 1987 Digital Precision',10
                dc.b       'This  program will create a QDOS  file',10
                dc.b       'called CPMFILES which is essential for',10
                dc.b       'the  operation  of SUCCESS. The  drive',10
                dc.b       'required MUST have been formatted from',10
                dc.b       'the SuperBASIC level.',10,10
                dc.b       'A - FLP1_                        b - FLP2_',10
                dc.b       'C - MDV1_            D - MDV2_',10
                dc.b       'E - RAM1_            F - RAM2_',10
                dc.b       'Q - Quit program.\n',10
                dc.b       'Which drive to format ?',SETCURS,1,EXIT
wait_key:
                moveq       #IO_FBYTE,d0
                moveq       #-1,d3
                trap        #TRAP_STRM
                bsr.w       to_upper
                cmpi.b      #'Q',d1
                beq.w       exit_abort
                subi.b      #'A',d1
                bmi.b       wait_key
                cmpi.b      #0x5,d1
                bhi.b       wait_key
                andi.w      #0xff,d1
                move.b      d1,d4
                mulu.w      #0x6,d1
                lea         device_table(pc),a1
                move.l      0(a1,d1),d2
                move.w      4(a1,d1),buffer_index(a6)

                lea         image_filename(pc),a1
                move.l      d2,(a1)
                move.b      d4,d1
                addi.b      #'1',d1
                moveq       #IO_SBYTE,d0
                trap        #TRAP_STRM
                moveq       #IO_FBYTE,d0
                moveq       #0x32,d3
                trap        #TRAP_STRM
                bsr.w       inline_string
                dc.b        SETCURS,0,CLRSCR,'Creating CPMFILES...',10,EXIT,0
try_again:  
                moveq       #IO_OPEN,d0
                lea         filename(pc),a0
                moveq       #0x2,d3
                moveq       #-1,d1
                trap        #TRAP_IO
                move.l      a0,file_handle(a6)
                movea.l     output_handle(a6),a0
                tst.l       d0
                beq.w       open_ok
                cmpi.b      #-0x8,d0
                beq.b       confirm_overwrite
                move.l      d0,-(sp)
                bsr.w       inline_string
                dc.b        'Create CPMFILES failed - reason : ',EXIT,0
                move.l      (sp)+,d0
                bra.w       print_error_message
confirm_overwrite:
                bsr.w      inline_string
                dc.b       'CPMFILES exists on drive, overwrite ?',SETCURS,1,0
wait_confirmation:
                moveq       #IO_FBYTE
                moveq       #-1,d3
                trap        #TRAP_STRM
                bsr.w       to_upper
                cmpi.b      #'N',d1
                beq.w       abort
                cmpi.b      #'Y',d1
                bne.s       wait_confirmation
                bsr.w       inline_string
                dc.b        SETCURS,0,'Yes',10,EXIT,0
                lea         filename(pc),a0
                moveq       #-1,d1
                moveq       #IO_DELET,d0
                trap        #TRAP_IO
                bra.w       try_again
open_ok:
                move.w      buffer_index(a6),d5
                moveq       #0x0,D4
                cmpi.w      #buffer_size-1,d5
                bcs.s       buffer_not_full
                movea.l     file_handle(a6),a0
                movea.l     base_of_z80(a6),a1
                adda.l      #0xd400,a1

                move.w      #0x1e00,d2
                moveq       #IO_SSTRG,d0
                moveq       #-1,d3
                trap        #TRAP_STRM
                move.w      buffer_index(a6),d5
                subq.w      #0x7,d5
                moveq       #0x7,D4
                bra.s       check_for_eof
buffer_not_full:
                movea.l     file_handle(a6),a0
                lea         file_buffer(pc),a1
                move.w      #0x400,d2
                moveq       #IO_SSTRG,d0
                moveq       #-1,d3
                trap        #TRAP_STRM
                tst.l       d0
                bne.s       exit_close
                addq.w      #0x1,d4
                movea.l     output_handle(a6),a0
                bsr.w       inline_string
                dc.b        SETPOS,5,0,'Amount written : ',0,0
                move.w      d4,d1
                bsr.w       print_number
check_for_eof:  
                dbf         d5,buffer_not_full
exit_close: 
                movea.l     file_handle(a6),a0
                moveq       #IO_CLOSE,d0
                trap        #TRAP_IO
                movea.l     output_handle(a6),a0
                bsr.w       inline_string
                dc.b        10,'Create CPMFILES completed.',10,EXIT,0
                rts 
abort:  
                bsr.w       inline_string
                dc.b        'No',10,EXIT
                rts 
print_number:   
                movea.w     UT_MINT,a2
                jmp         (a2)
exit_abort: 
                movea.l     output_handle(a6),a0
                bsr.w       inline_string
                dc.b        10,'Quitted...',10,0,0
                rts
; Print error in d0
print_error_message:
                movea.w     UT_ERR,a1
                jmp         (a1)
; d1 to uppercase
to_upper:
                cmpi.b      #'a',d1
                bcs.s       not_lc
                cmpi.b      #'z',d1
                bhi.b       not_lc
                subi.b      #0x20,d1
not_lc:         rts
inline_string:
                movea.l     (sp)+,a5
next_char:
                clr.w       d1
                move.b      (a5)+,d1
                cmpi.b      #9,d1
                bls.b       handle_control
                moveq       #IO_SBYTE,d0
exec_control:
                clr.b       SV_SCRST
                moveq       #-1,d3
                trap        #TRAP_STRM
                bra.s       next_char
handle_control:
                add.w       d1,d1
                lea         dispatch_table(pc),a1
                move.w      0(a1,d1.w),d2
                adda.w      d2,a1
                clr.l       d1
                clr.l       d2
                jmp         (a1)
command_exit:
                move.l      a5,d0
                btst.l      0x0,d0
                beq.b       is_already_even
                addq.w      #0x1,a5
is_already_even:
                clr.l       d0
                jmp         (a5)                                    ; return to original caller, now updated to be after the inline string
set_position:
                moveq       #SD_POS,d0
                move.b      (a5)+,d2
                move.b      (a5)+,d1
                bra.s       exec_control
set_inverse:    
                moveq       #SD_SETIN,d0
get_one_param:  
                move.b      (a5)+,d1
                bra.s       exec_control
set_paper:  
                moveq       #SD_SETPA,d0
                move.b      (a5),d1
                moveq       #-1,d3
                trap        #TRAP_STRM
                moveq       #SD_SETST,d0
                bra.s       get_one_param
clear_screen:   
                moveq       #SD_CLEAR,d0
                bra.s       exec_control
set_flashing:   
                moveq       #SD_SETFL,d0
                bra.s       get_one_param
set_underline:  
                moveq       #SD_SETUL,d0
                bra.s       get_one_param
set_size:   
                moveq       #SD_SETSZ,d0
                move.b      (a5)+,d1
                move.b      (a5)+,d2
                bra.s       exec_control
set_cursor: 
                moveq       #SD_CURS,d0
                sub.b       (a5)+,d0
                bra.s       exec_control
set_border:
                moveq       #SD_BORDR,d0
                            move.b     (a5)+,d2
                bra.s       get_one_param
dispatch_table:
                dc.w        command_exit-dispatch_table
                dc.w        set_position-dispatch_table
                dc.w        set_inverse-dispatch_table
                dc.w        set_paper-dispatch_table
                dc.w        clear_screen-dispatch_table
                dc.w        set_flashing-dispatch_table
                dc.w        set_underline-dispatch_table
                dc.w        set_size-dispatch_table
                dc.w        set_cursor-dispatch_table
                dc.w        set_border-dispatch_table
workspace:
                dc.w        0,0,0,0
                dc.w        0,0,0,0
device_table:
                dc.b        'FLP1'
                dw          $208
                dc.b        'FLP2'
                dw          $08
                dc.b        'MDV1'
                dw          $64
                dc.b        'MDV2'
                dw          $64
                dc.b        'RAM1'
                dw          $C8
                dc.b        'RAM2'
                dw          $C8
filename:
                dc.w        13
                dc.b        'FLP1_CPMFILES'
file_buffer:
                ds.b        buffer_size
    