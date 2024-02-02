;
; Formatting:
;_______________I___________O_______________________________C
;               17          29                              61
;               Instr.      Operand                         Comment
;
                include     'qdos.i'
                include     'success.i'
; Register allocation are as follows:
;
; D1 - 1 if last op was add,-1 if last op was sub
; D2 - Backup copy of last accumulator used
; D3 - DE register
; D4 - BC register
; D5 - HL register
; D6 - F register
; D7 - A register
; A0 - Channel ID of main display
; A1 - Pointer to z80 execution loop
; A2 - Pointer to internal data
; A3 - Z80 stack address
; A4 - Base of workspace
; A5 - Base of Z80 memory
; A6 - Program counter address
; 
;
; This is position independent code
;
; On Entry:
;       d0  - bios call index.
;           - This comes from 3 sources, initial boot and in/out instruction
;           - d0 = 0 on cold boot
;       d1  - 0 for out instruction or cold boot
;             1 for in instruction,
                org         $0
                bra.s       start
                dc.w        $0af2,$0cf6
start:
                tst.w       d0
                bmi.s       screen_rw
                ext.w       d0
                cmpi.b      #$14,d0
                bhi.s       out_of_range
                movem.l     a1-a2,-(sp)
                lsl.w       #1,d0
                lea         bios_call_dispatch(pc),a2
                adda.w      0(a2,d0.w),a2
                jsr         (a2)
                movem.l     (sp)+,a1-a2
out_of_range:
                rts
screen_rw:
                andi.w      #$7fff,d0
                move.l      #$00020000,a0
                tst.w       d1
                bne.s       screen_read
screen_write:
                move.b      d7,0(a0,d0.l)
                rts
screen_read:
                move.b      0(a0,d0.l),d7
                rts
syscall_boot:
                move.w      #$00a3,SV_CQCH          
                move.l      #$00020000,a0
                move.w      #$1fff,d0
clr_scr:        clr.l       (a0)+
                dbf         d0,clr_scr
                move.b      #0,4(a5)
                tst.l       con_handle(a4)          ;; 0x14 offset
                bne.s       console_is_open
                lea         console_name(pc),a0
                moveq.l     #IO_OPEN,d0
                moveq.l     #-1,d1
                moveq.l     #0,d3
                trap        #TRAP_IO
                tst.l       d0
                bne.w       console_open_failed
                move.l      a0,con_handle(a4)
console_is_open:
                tst.l       ser_handle(a4)   ;; 0x1c offset
                bne.s       clear_console
                ;
                ; Try and open serial console
                lea         serial_device_name(pc),a0
                moveq.l     #IO_OPEN,d0
                moveq.l     #-1,d1
                moveq.l     #$0,d3
                trap        #TRAP_IO
                move.l      a0,ser_handle(a4)
clear_console:
                move.l      con_handle(a4),a0
                moveq.l     #SD_CLEAR,d0
                moveq.l     #-1,d3
                trap        #TRAP_STRM
                moveq.l     #SD_SETIN,d0                    ; Set ink color to white
                moveq.l     #$7,d1
                trap        #TRAP_STRM
                lea         signon_message(pc),a1
                move.w      (a1)+,d2
                moveq.l     #IO_SSTRG,d0
                trap        #TRAP_STRM
syscall_wboot:
                clr.b       term_inp_mode(a4)
                lea         z80_bios(pc),a1
                move.l      a5,a2
                add.l       #$ea00,a2
                move.w      #z80_bios_end-z80_bios-1,d0  ; Should be 219
copy_loop1:     move.b      (a1)+,(a2)+
                dbf         d0,copy_loop1
                move.l      $28(a4),a0
                cmp.l       #0,a0
                beq.s       not_open
                moveq.l     #IO_CLOSE,d0
                trap        #TRAP_IO
not_open:
                clr.l       $28(a4)
                move.b      4(a5),d0
                cmpi.b      #$7,d0
                bcs.s       b_e8
                clr.b       4(a5)
b_e8:
                move.b      4(a5),(a4)
                move.w      #128,dma_address(a4)
                moveq.l     #$0,d4
                bsr         open_system_disk
                tst.l       d5
                beq.w       not_system_disk
                move.l      flp_handle(a4),a0           ; offset 0x18
                move.l      a5,a1
                add.l       #$d400,a1
                move.l      a1,a6
                moveq.l     #IO_FSTRG,d0
                moveq.l     #-1,d3
                move.w      #$1600,d2                      ; Loading the first 44 sectors from the disk
                trap        #TRAP_STRM
                ; Set up a new bios & bdos jump vector now that CP/M disk
                ; bootstrap has been loaded.
                move.b      #$c3,0(a5)                     ; jp 0xea03
                move.b      #$03,1(a5)
                move.b      #$ea,2(a5)
                move.b      #$c3,5(a5)                     ; jp 0xdc06
                move.b      #$06,6(a5)
                move.b      #$dc,7(a5)
                lea         128(a5),a3
                clr.l       d4
                move.b      (a4),d4
                bsr         open_system_disk
                tst.w       d5
                beq.s       open_fail
                move.w      #$0000,selected_track(a4)       ; Read sector 0
                bsr.w       syscall_read
                tst.w       d7
                beq.s       dont_reset
open_fail:
                clr.b       (a4)
dont_reset:
                move.b      (a4),d4
                move.b      d4,reg_de(a5)
console_open_failed:
                rts
syscall_const:
                movem.l     a2-a3,-(sp)
                clr.l       d7
                move.l      SV_KEYQ,a2
                move.w      IO_QTEST,a1
                jsr         (a1)
                movem.l     (sp)+,a2-a3
                tst.l       d0
                bne.s       failed_0
                subq.b      #1,d7
failed_0:       rts
syscall_listst:
                move.l      ser_handle(a4),a0
                clr.l       d7
                move.l      #IO_PEND.l,d0
                moveq.l     #$0,d3
                trap        #TRAP_STRM
                tst.l       d0
                bne.s       failed_0
                subq.b      #1,d7
                rts
;; read console
syscall_conin:
                move.l      con_handle(a4),a0
                moveq.l     #SD_CURENA,d0
                moveq.l     #-1,d3
                trap        #TRAP_STRM
                moveq.l     #IO_FBYTE,d0
                trap        #TRAP_STRM
                clr.l       d7
                move.b      d1,d7
                moveq.l     #SD_CURS,d0
                trap        #TRAP_STRM
                move.b      ascii_translate_table(pc,d7.w),d7
                cmpi.b      #$e0,d7
                bcc.w       function_key 
translate_control_code:
                cmpi.b      #$e0,d7                 ;; CAPSLOCK
                bne.s       not_c3
                moveq.l     #$13,d7                 ;; C3,CTRL-S
not_c3:         cmpi.b      #$e1,d7                 ;; ALT CAPSLOCK
                bne.s       not_eot
                moveq.l     #$4,d7                  ;; EOT,CTRL-D
not_eot:        cmpi.b      #$e2,d7                 ;; CTRL CAPSLOCK
                bne.s       not_enq
                moveq.l     #$5,d7                  ;; ENQ,CTRL-E
not_enq:        cmpi.b      #$e3,d7                 ;; ALT-CTRL CAPSLOCK
                bne.s       not_can
                moveq.l     #24,d7                  ;; CAN, CTRL-X
not_can:    
                rts
ascii_translate_table:
                dc.b        0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 13,11,12,10,14,15
                dc.b        16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31
                dc.b        32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47
                dc.b        48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63
                dc.b        64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79
                dc.b        80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95
                dc.b        96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111
                dc.b        112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127
                dc.b        128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143
                dc.b        144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159
                dc.b        160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175
                dc.b        176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191
                dc.b        224,229,127,195,196,197,198,199,225,230,202,203,204,205,206,207
                dc.b        226,231,210,211,212,213,214,215,227,232,218,219,220,221,222,223
                dc.b        224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239
                dc.b        240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255
function_key:
                clr.l       d0
                move.b      d7,d0
                subi.b      #224,d0
                add.b       d0,d0
                move.w      key_definitions(a4,d0.w),d0
                beq.w       translate_control_code
                lea         0(a5,d0.l),a1
                move.l      SV_KEYQ,d0
                beq.w       translate_control_code
                movem.l     a2-a3,-(sp)
                move.l      d0,a2
                move.b      (a1)+,d7
                move.w      IO_QOUT,a0
next_char:
                move.b      (a1)+,d1
                beq.s       end_of_string
                jsr         (a0)
                tst.l       d0
                beq.s       next_char
end_of_string:
                movem.l     (sp)+,a2-a3
                bra.w       translate_control_code
syscall_conout:
                move.l      con_handle(a4),a0
                moveq.l     #-1,d3
                clr.l       d2
                clr.l       d1
                move.b      d4,d1
                move.b      term_inp_mode(a4),d2
                bne.w       escape_mode
                tst.b       d1
                beq.w       print_exit
                cmpi.b      #' ',d1
                bcc.w       print_char
                cmpi.b      #$4,d1              ;; eot
                beq.w       term_next_row
                cmpi.b      #$5,d1              ;; enq
                beq.w       term_prev_col
                cmpi.b      #$12,d1             ;; dc2
                beq.w       term_set_underline
                cmpi.b      #$13,d1             ;; dc3
                beq.w       term_reset_underline
                cmpi.b      #$18,d1             ;; can
                beq.w       term_next_col
                cmpi.b      #$7,d1              ;; bel
                beq.w       term_bell
                cmpi.b      #$a,d1              ;; nl
                beq.w       term_next_row
                cmpi.b      #$d,d1              ;; cr
                beq.w       term_carriage_ret
                cmpi.b      #$8,d1              ;; bs
                beq.w       term_backspace
                cmpi.b      #$1a,d1             ;; sub
                beq.w       term_clear
                cmpi.b      #$1d,d1             ;; gs
                beq.w       b_4b0               
                cmpi.b      #$1e,d1             ;; rs
                beq.w       b_4a0               
                cmpi.b      #$1b,d1             ;; esc
                beq.w       term_escape
                cmpi.b      #$1c,d1             ;; fs
                beq.w       term_home
                cmpi.b      #$7f,d1             ;; del
                beq.w       term_backspace
print_char_ctrl_check:
                cmpi.b      #' ',d1
                bcs.s       print_exit
print_char:
                moveq.l     #IO_SBYTE,d0
                trap        #TRAP_STRM
                clr.b       term_inp_mode(a4)
print_exit:
                rts
escape_mode:
                cmpi.b      #$ff,d2
                beq.s       check_escape_code
                subq.b      #1,term_inp_mode(a4)
                beq.s       input_complete
                move.b      d1,term_saved_char(a4)
                rts
input_complete:
                cmpi.b      #'Y',term_last_char(a4)
                beq.s       move_curs_done
                rts
move_curs_done:
                move.b      term_saved_char(a4),d2
                subi.b      #$20,d1
                subi.b      #$20,d2
set_cursor_pos:
                moveq.l     #SD_POS,d0
                trap        #TRAP_STRM
                rts
term_home_2:
                moveq.l     #$0,d1
                moveq.l     #$0,d2
                bra.s       set_cursor_pos
check_escape_code:
                move.b      d1,term_last_char(a4)
                clr.b       term_inp_mode(a4)
                cmpi.b      #'p',d1
                beq.s       term_inverse_video
                cmpi.b      #'q',d1
                beq.w       term_norm_video
                cmpi.b      #'A',d1
                beq.w       term_prev_row
                cmpi.b      #'B',d1
                beq.w       term_next_row
                cmpi.b      #'C',d1
                beq.w       term_next_col
                cmpi.b      #'D',d1
                beq.w       term_prev_col
                cmpi.b      #'E',d1
                beq.w       term_clear
                cmpi.b      #'H',d1
                beq.w       term_home_2
                cmpi.b      #'Y',d1
                beq.s       term_move_curs
                cmpi.b      #'J',d1
                beq.s       term_erase_eos
                cmpi.b      #'K',d1
                bne.s       unknown_control_char
                moveq.l     #SD_CLRRT,d0
                trap        #TRAP_STRM
                rts
unknown_control_char:
                move.w      d1,-(sp)
                moveq.l     #'^',d1
                bsr.w       print_char
                moveq.l     #'[',d1
                bsr.w       print_char
                move.w      (sp)+,d1
                bra.w       print_char_ctrl_check
term_erase_eos:
                moveq.l     #SD_CLRLN,d0
                trap        #TRAP_STRM
                moveq.l     #SD_CLRBT,d0
                trap        #TRAP_STRM
                rts
; This puts input stream parser in to coordinate input mode. Waiting for
; new characters to appear.
term_move_curs:
                move.b      #$02,term_inp_mode(a4)
                rts
term_inverse_video:
                moveq.l     #$7,d1
norm_video:
                moveq.l     #SD_SETST,d0
                trap        #TRAP_STRM
                moveq.l     #SD_SETPA,d0
                trap        #TRAP_STRM
                eor.b       #$7,d1
                moveq.l     #SD_SETIN,d0
                trap        #TRAP_STRM
                rts
term_set_underline:
                moveq.l     #$1,d1
do_underline:   moveq.l     #SD_SETUL,d0
                trap        #TRAP_STRM
                rts
term_reset_underline:
                moveq.l     #$0,d1
                bra.s       do_underline
term_norm_video:
                moveq.l     #$0,d1
                bra.s       norm_video
b_4a0:
                moveq.l     #SD_PROW,d0
b_4a2:
                trap        #TRAP_STRM
                moveq.l     #SD_SCRBT,d0
                moveq.l     #$a,d1
                trap        #TRAP_STRM
                moveq.l     #SD_NROW,d0
                trap        #TRAP_STRM
                rts
b_4b0:
                moveq.l     #SD_CLRLN,d0
                trap        #TRAP_STRM
                moveq.l     #SD_PROW,d0
                trap        #TRAP_STRM
b_4b8:
                moveq.l     #SD_SCRBT,d0
                moveq.l     #$f6,d1
                trap        #TRAP_STRM
                moveq.l     #SD_NROW,d0
                trap        #TRAP_STRM
                rts
term_backspace:
                bsr.s       term_prev_col
                moveq.l     #$20,d1
                moveq.l     #IO_SBYTE,d0
b_4ca:
                trap        #TRAP_STRM
term_prev_col:
                moveq.l     #SD_PCOL,d0
                trap        #TRAP_STRM
                tst.l       d0
                beq.w       print_exit
                moveq.l     #SD_PROW,d0
                trap        #TRAP_STRM
                moveq.l     #SD_TAB,d0
b_4dc:
                moveq.l     #$4f,d1
                trap        #TRAP_STRM
                rts
term_next_col:
                moveq.l     #SD_NCOL,d0
                trap        #TRAP_STRM
                tst.l       d0
b_4e8:
                beq.w       print_exit
                moveq.l     #$a,d1
                moveq.l     #IO_SBYTE,d0
                trap        #TRAP_STRM
                rts
term_next_row:                
                moveq.l     #SD_NROW,d0
                trap        #TRAP_STRM
                tst.l       d0
                beq.w       print_exit
                moveq.l     #SD_SCROL,d0
                moveq.l     #$f6,d1
                trap        #TRAP_STRM
                rts
term_prev_row:
                moveq.l     #SD_PROW,d0
                trap        #TRAP_STRM
                rts
term_clear:     moveq.l     #SD_CLEAR,d0
                trap        #TRAP_STRM
                rts
term_carriage_ret:
                bsr.s       get_terminal_position
                move.w      #$0000,d1
                bra.s       set_pos
term_escape:
                move.b      #$ff,term_inp_mode(a4)
                rts
term_home:
                clr.l       d1
                clr.l       d2
set_pos:
                moveq.l     #SD_POS,d0
                trap        #TRAP_STRM
                bra.w       print_exit
get_terminal_position:
                moveq.l     #SD_CHENQ,d0
                lea         temp_buffer(a4),a1
                trap        #TRAP_STRM
                move.w      temp_buffer+4(a4),d1
                move.w      temp_buffer+6(a4),d2
                rts 
term_bell:
                movem.l     d5/d7/a3,-(sp)
                moveq.l     #MT_IPCOM,d0
                lea         beep_sound_ipc(pc),a3
                trap        #TRAP_JOB
                movem.l     (sp)+,d5/d7/a3
                rts 
beep_sound_ipc:
                dc.b        $0a,$08,$00,$00,$aa,$aa
                dc.b        $04,$00,$00,$00,$30,$10
                dc.b        $00,$00,$01,$00
syscall_punch:
                rts 
syscall_list:
                move.b      d4,d1
                moveq.l     #IO_SBYTE,d0
                moveq.l     #-1,d3
                move.l      ser_handle(a4),a0
                trap        #TRAP_STRM
                rts 
syscall_reader:
                moveq.l     #26,d7
                rts 
syscall_home:
                move.b      #$00,selected_track(a4)
                rts
syscall_seldsk:
open_system_disk:
                bsr         b_8a2
                move.w      #-1,cached_track(a4)
                clr.w       d5
                lea         name_buffer(a4),a0
                lea         temp_buffer(a4),a1
                move.w      (a0),(a1)+
                move.w      #$000d,(a0)+
                move.l      a0,-(sp)
                move.l      #$464c5031,(a0)+            ; 'FLP1'
                move.l      (a0),(a1)+
                move.l      #$5f43504d,(a0)+            ; '_CPM'
                move.w      (a0),(a1)+
                move.l      #$46494c45,(a0)+            ; 'FILE'
                move.b      #$53,(a0)                   ; 'S'
                move.b      cache_flags(a4),(a1)
                move.l      (sp)+,a0
                bclr        #0,cache_flags(a4)
                bclr        #7,cache_flags(a4)
                lea         device_name_table(pc),a1
                move.l      (a1)+,(a0)
                move.b      d4,d1
                cmpi.b      #$2,d1
                bcs.l       b_65c
                move.l      (a1)+,(a0)
                ;; Uses the short form, original assembler used long-form. sub.b       #$2,d1
                dc.w        $0401,$0002         ; This is long-form subi.b #$0002,d1
                cmpi.b      #$2,d1
                bcs.w       b_65c
                move.l      (a1)+,(a0)
                ;; Uses the short form, original assembler used long-form. sub.b       #$2,d1
                dc.w        $0401,$0002         ; This is long-form subi.b #$0002,d1
                cmpi.b      #$2,d1
                bcs.l       b_65c
                ;; Uses the short form, original assembler used long-form. sub.b       #$2,d1
                dc.w        $0401,$0002         ; This is long-form subi.b #$0002,d1
                cmpi.b      #$1,d1
                bcc.w       copy_name_and_open
                ; Special filename that will allow us to access a disk raw.

                move.w      #$0009,-2(a0)
                move.l      #$464c5032,(a0)         ; 'FLP2'
                move.l      #$5f2a4432,4(a0)        ; '_*D2'
                move.b      #$44,8(a0)              ; 'D'
                clr.l       d0
                move.l      a5,a1
                move.w      active_dpb(a4),d0
                tst.l       d0
                beq.w       copy_name_and_open
                add.l       d0,a1 
                move.b      0(a1),d0
                add.b       #$30,d0
                move.b      d0,3(a0)                    ; Replace '2' above with proper drive index
                move.b      1(a1),d0                    ; Disk number?
                add.b       #$30,d0
                move.b      d0,7(a0)
                move.b      #'S',d0
                btst        #0,5(a1)
                beq.s       single_density
                move.b      #'D',d0
single_density:
                move.b      d0,8(a0)
                bset        #0,cache_flags(a4)
                bra.s       activate_disk
;
; I don't think this is right.
b_65c:
                add.b       #'1',d1
                move.b      d1,3(a0)
activate_disk:
                bsr         close_user_disk
                bsr         open_user_disk
                tst.l       d0
                bne.s       copy_name_and_open
                move.w      #$ea3c,d0
                clr.l       d1
                move.b      d4,d1
                lsl.w       #4,d1
                add.w       d1,d0
                move.w      d0,d5
                move.b      d4,4(a5)
                rts
copy_name_and_open:
                lea         temp_buffer(a4),a0
                lea         name_buffer(a4),a1
                move.l      (a0)+,(a1)+
                move.l      (a0)+,(a1)+
                move.l      (a0)+,(a1)+
                move.b      (a0),cache_flags(a4)
                bsr         open_user_disk
                rts 
syscall_settrk:
                move.b    d4,selected_track(a4)
                rts 
seek_sector:
                clr.l       d2
                clr.l       d1
                move.b      selected_sector(a4),d2
                move.b      selected_track(a4),d1
                mulu        #SEC_PER_TRACK,d1
                ; This is some opcode that got mis-disassembled with mulu
                ;;dc.w        $d282
                add.l       d2,d1
                lsl.l       #7,d1
                move.l      flp_handle(a4),a0
                moveq.l     #FS_POSAB,d0
                moveq.l     #-1,d3
                trap        #TRAP_STRM
                rts 
syscall_setsec:
                move.b      d4,selected_sector(a4)
                rts 
syscall_setdma:
                move.w      d4,dma_address(a4)
                rts 
syscall_read:
                btst        #0,cache_flags(a4)
                bne.s       cache_valid
                bsr.s       seek_sector
                tst.l       d0
                bne.s       seek_failed
                move.l      a5,a1
                move.w      dma_address(a4),d0
                add.l       d0,a1 
                move.w      #SECTOR_SIZE,d2
                moveq.l     #IO_FSTRG,d0
                moveq.l     #-1,d3
                trap        #TRAP_STRM
seek_failed:
                clr.b       d7
                tst.l       d0
                beq.s       no_read_error
                move.b      #$01,d7
no_read_error:  rts 
cache_valid:
                clr.l       d0
                move.l      a5,a0
                move.w      active_dpb(a4),d0
                beq.w       b_7de
                add.l       d0,a0 
                clr.l       d1
                clr.l       d2
                clr.l       d7
                move.b      6(a0),-(a4)         ; Endian swap.
                move.b      7(a0),-(a4)
                move.w      (a4)+,d0
                lea         0(a5,d0.l),a1
                move.b      1(a0),d7
                bsr         b_8a2
                tst.l       d0
                bne.w       seek_failed
                move.w      selected_track(a4),d0
                move.w      cached_track(a4),d1
                lsr.w       d7,d1
                lsr.w       d7,d0
                cmp.w       d1,d0
                beq.s       cache_hit
                move.l      a1,-(sp)
                move.w      selected_track(a4),d3
                bsr.s       b_784
                moveq.l     #IO_FSTRG,d0
                clr.l       d2
                move.w      #SECTOR_SIZE,d2
                lsl.w       d7,d2
                move.l      (a7),a1
                moveq.l     #-1,d3
                trap        #TRAP_STRM
                move.l      (sp)+,a1
                tst.l       d0
                bne.w       seek_failed
cache_hit:
                moveq.l     #$1,d0
                lsl.w       d7,d0
                subq.w      #1,d0
                and.b       selected_sector(a4),d0
                lsl.w       #7,d0
                add.l       d0,a1 
                clr.l       d2
                move.w      dma_address(a4),d2
                lea         0(a5,d2.l),a2
                moveq.l     #SECTOR_SIZE-1,d2
b_774:
                move.b      (a1)+,(a2)+
                dbf         d2,b_774
                clr.l       d7
                move.w      selected_track(a4),cached_track(a4)
                rts 
b_784:
                clr.l       d1
                clr.l       d0
                clr.l       d2
                move.w      d3,-(sp)
                move.b      (a7),d1
                cmp.b       3(a0),d1
                bcs.s       b_7ae
                sub.b       3(a0),d1
                btst        #2,5(a0)
                beq.s       b_7aa
                move.w      d1,-(sp)
                move.b      3(a0),d1
                subq.w      #1,d1
                sub.w       (sp)+,d1
b_7aa:
                move.w      #$0100,d2
b_7ae:
                move.b      4(a0),d0
                lsl.b       d0,d1
                swap        d1
                move.w      d2,d1
                move.w      (sp)+,d0
                andi.w      #$ff,d0
                lsr.b       d7,d0
                move.l      a1,-(sp)
                move.b      8(a0,d0.w),d1
                bclr        #7,d1
                beq.s       b_7d0
                bset        #8,d1
b_7d0:
                move.l      flp_handle(a4),a0
                moveq.l     #FS_POSAB,d0
                moveq.l     #-1,d3
                trap        #TRAP_STRM
                move.l      (sp)+,a1
                rts 
b_7de:
                moveq.l     #$1,d7
                move.w      selected_track(a4),cached_track(a4)
                rts 
syscall_write:
                btst        #0,cache_flags(a4)
                bne.s       b_814
                bsr         seek_sector
                tst.l       d0
                bne.w       seek_failed
                move.w      dma_address(a4),d0
                move.l      a5,a1
                add.l       d0,a1 
                move.w      #SECTOR_SIZE,d2
                moveq.l     #IO_SSTRG,d0
                moveq.l     #-1,d3
                trap        #TRAP_STRM
                bra         seek_failed
syscall_sectran:
                move.w      d4,d5
                rts 
b_814:
                clr.l       d0
                move.l      a5,a0
                move.w      active_dpb(a4),d0
                beq.w       b_7de
                add.l       d0,a0 
                clr.l       d1
                clr.l       d2
                clr.l       d7
                move.b      6(a0),-(a4)
                move.b      7(a0),-(a4)
                move.w      (a4)+,d0
                lea         0(a5,d0.l),a1
                move.w      selected_track(a4),d0
                move.w      cached_track(a4),d1
                move.b      1(a0),d7
                lsr.w       d7,d0
                lsr.w       d7,d1
                cmp.w       d1,d0
                beq.s       b_872
                bsr.s       b_8a2
                tst.l       d0
                bne.w       seek_failed
                move.w      selected_track(a4),d3
                bsr         b_784
                move.l      a1,-(sp)
                moveq.l     #IO_FSTRG,d0
                clr.l       d2
                move.w      #SECTOR_SIZE,d2
                lsl.w       d7,d2
                moveq.l     #-1,d3
                trap        #TRAP_STRM
                move.l      (sp)+,a1
                tst.l       d0
                bne.w       seek_failed
b_872:
                moveq.l     #$1,d0
                lsl.w       d7,d0
                subq.w      #1,d0
                and.b       selected_sector(a4),d0
                lsl.w       #7,d0
                add.l       d0,a1 
                clr.l       d2
                move.w      dma_address(a4),d2
                lea         0(a5,d2.l),a2
                moveq.l     #SECTOR_SIZE-1,d2
copyloop:
                move.b      (a2)+,(a1)+
                dbf         d2,copyloop
                bset        #7,cache_flags(a4)
                clr.l       d7
                move.w      selected_track(a4),cached_track(a4)
                rts 
b_8a2:      
                clr.l       d0
                bclr        #7,cache_flags(a4)
                beq.s       cache_check_exit
                btst        #0,cache_flags(a4)
                beq.s       cache_check_exit
                move.l      a0,-(sp)
                move.l      a1,-(sp)
                clr.l       d7
                move.l      a5,a0
                clr.l       d1
                move.w      active_dpb(a4),d1
                beq.w       b_8f0
                adda.l      d1,a0 
                move.b      6(a0),-(a4)
                move.b      7(a0),-(a4)
                move.w      (a4)+,d0
                lea         0(a5,d0.l),a1
                move.b      1(a0),d7
                move.w      cached_track(a4),d3
                bsr         b_784
                moveq.l     #IO_SSTRG,d0
                clr.l       d2
                move.w      #SECTOR_SIZE,d2
                lsl.w       d7,d2
                moveq.l     #-1,d3
                trap        #TRAP_STRM
b_8f0:
                move.l      (sp)+,a1
                move.l      (sp)+,a0
                move.w      #-1,cached_track(a4)
cache_check_exit:
                rts 
close_user_disk:
                move.l      flp_handle(a4),a0
                tst.l       flp_handle(a4)
                beq.s       already_closed
                clr.l       flp_handle(a4)
                moveq.l     #IO_CLOSE,d0
                trap        #TRAP_IO
already_closed:
                rts 
open_user_disk:
                move.l      flp_handle(a4),a0
                tst.l       flp_handle(a4)
                bne.s       floppy_already_open
                lea         name_buffer(a4),a0
                moveq.l     #IO_OPEN,d0
                moveq.l     #$0,d3
                moveq.l     #-1,d1
                trap        #TRAP_IO
                tst.l       d0
                bne.s       floppy_already_open
                move.l      a0,flp_handle(a4)
floppy_already_open:
                rts
address_not_even:
                lea         not_even_error(pc),a1
                move.l      con_handle(a4),a0
                move.w      (a1)+,d2
                moveq.l     #IO_SSTRG,d0
                moveq.l     #-1,d3
                trap        #TRAP_STRM
                bra         syscall_wboot
                bsr.w       close_user_disk
                clr.b       4(a5)
not_system_disk:
                lea         not_system_disk_error(pc),a1
                move.l      con_handle(a4),a0
                move.w      (a1)+,d2
                moveq.l     #IO_SSTRG,d0
                moveq.l     #-1,d3
                trap        #TRAP_STRM
                bsr         syscall_conin
                bra         syscall_wboot
syscall_custom:
                moveq.l     #$f1,d0
                cmpi.b      #$6,d7
                bcc.s       set_error_return
                add.b       d7,d7
                ext.w       d7
                lea         bios_extra_dispatch(pc),a0
                add.w       0(a0,d7.w),a0
                jsr         (a0)
set_error_return:
                move.b      d0,d7
                rts 
syscall_qdos_close:
                tst.l       sys_disk_handle(a4)
                beq.s       sys_disk_closed
                move.l      sys_disk_handle(a4),a0
                moveq.l     #IO_CLOSE,d0
                trap        #TRAP_IO
                clr.l       sys_disk_handle(a4)
                rts 
sys_disk_closed:
                clr.l       d0
                rts 
filename_not_even:
                moveq.l     #-1,d7
                rts 
syscall_qdos_open:
                moveq.l     #$0,d3
user_file_operation:
                lea         0(a5,d5.l),a0
                btst        #0,d5
                bne.s       filename_not_even
                moveq.l     #IO_OPEN,d0
                moveq.l     #-1,d1
                trap        #TRAP_IO
                tst.l       d0
                bne.s       set_error_return
                move.l      a0,sys_disk_handle(a4)
                rts 
syscall_qdos_opennew:
                moveq.l     #$2,d3
                bra.s       user_file_operation
syscall_qdos_opendir:
                moveq.l     #$4,d3
                bra.s       user_file_operation
syscall_qdos_delete:
                lea         0(a5,d5.l),a0
                btst        #0,d5
                bne.s       filename_not_even
                moveq.l     #IO_DELET,d0
                moveq.l     #-1,d1
                trap        #TRAP_IO
                rts 
syscall_qdos_setbaud:
                move.w      d5,d1
                moveq.l     #MT_BAUD,d0
                trap        #TRAP_JOB
                rts 
syscall_unknown_0:
syscall_qdos_io:  
                moveq.l     #$f1,d0
                cmpi.b      #$5,d7
                bcc         set_error_return
                moveq.l     #$fa,d0
                tst.l       sys_disk_handle(a4)
                beq         set_error_return
                move.l      sys_disk_handle(a4),a0
                add.b       d7,d7
                ext.w       d7
                lea         bios_user_call(pc),a1
                add.w       0(a1,d7.w),a1
                jsr         (a1)
                bra         set_error_return
syscall_qdos_rdbyte:
                moveq.l     #IO_FBYTE,d0
                moveq.l     #-1,d3
                trap        #TRAP_STRM
                move.b      d1,d4
                rts 
syscall_qdos_read:
                moveq.l     #IO_FSTRG,d0
                lea         0(a5,d5.l),a1
                move.w      d4,d2
                moveq.l     #-1,d3
                trap        #TRAP_STRM
                move.w      d1,d4
                rts 
syscall_qdos_wrbyte:
                moveq.l     #IO_SBYTE,d0
                move.b      d4,d1
                moveq.l     #-1,d3
                trap        #TRAP_STRM
                rts 
syscall_qdos_write:
                moveq.l     #IO_SSTRG,d0
                move.w      d4,d2
                lea         0(a5,d5.l),a1
                moveq.l     #-1,d3
                trap        #TRAP_STRM
                move.w      d1,d4
                rts 
;
; hl (d5) - routine to execute (must be even)
; bc (d4) - identifier $4afb
syscall_exec_68k:
                clr.l       d0
                move.w      d5,d0
                btst        #0,d0
                bne.w       address_not_even
                lea         0(a5,d0.l),a1
                cmpi.w      #$4afb,d4                       ; Check signature
                bne.w       address_not_even                ; Bail if need to.
                movem.l     a0-a6,-(sp)
                move.l      con_handle(a4),a0
                jsr         (a1)
                movem.l     (sp)+,a0-a6
out_of_range2:
                rts 
;
; This is the entrypoint, where out (n),a or in (n) etc, will come through.
; Not sure what this is doing.
; a (d7) - key code, $ff to define a drive.
; hl (d5) - ptr to key definition
syscall_defkey:
                cmpi.b      #$ff,d7
                beq.s       define_drive
                subi.b      #$e0,d7
                bcs         out_of_range2
                cmpi.b      #$1b,d7
                bhi         out_of_range2

                add.b       d7,d7
                move.w      d5,key_definitions(a4,d7.w)
                rts 
define_drive:
                move.w      d5,active_dpb(a4)
                rts
;; Table should be 0x4a bytes long. This jump table is relative to the start of it. Most of the other
;; tables have been address relative. 
bios_call_dispatch:
bcdt:
                dc.w        syscall_boot-bcdt           ; #-1
                dc.w        syscall_wboot-bcdt          ; #0
                dc.w        syscall_const-bcdt          ; #1
                dc.w        syscall_conin-bcdt          ; #2
                dc.w        syscall_conout-bcdt         ; #3
                dc.w        syscall_list-bcdt           ; #4
                dc.w        syscall_punch-bcdt          ; #5
                dc.w        syscall_reader-bcdt         ; #6
                dc.w        syscall_home-bcdt           ; #7
                dc.w        syscall_seldsk-bcdt         ; #8
                dc.w        syscall_settrk-bcdt         ; #9
                dc.w        syscall_setsec-bcdt         ; #10
                dc.w        syscall_setdma-bcdt         ; #11
                dc.w        syscall_read-bcdt           ; #12
                dc.w        syscall_write-bcdt          ; #13
                dc.w        syscall_listst-bcdt         ; #14
                dc.w        syscall_sectran-bcdt        ; #15
                dc.w        syscall_custom-bcdt         ; #16       QDOS ALLOC
                dc.w        syscall_qdos_io-bcdt        ; #17       QDOS I/O
                dc.w        syscall_exec_68k-bcdt       ; #18       EXEC 68K
                dc.w        syscall_defkey-bcdt         ; #19       Define function keys
                ;; None of the following calls make much sense. So it's probably
                ;; another unused table :)
                dc.w        b_4a2-bcdt                  ; $FA28
                dc.w        b_4b8-bcdt                  ; $FA3E
                dc.w        b_4ca-bcdt                  ; $FA50
                dc.w        b_4dc-bcdt                  ; $FA62
                dc.w        term_next_col-bcdt          ; b_4e2-bcdt                  ; $FA68
                dc.w        b_4e8-bcdt                  ; $FA6E
bios_extra_dispatch:
bed:
                dc.w        syscall_qdos_close-bed      ; IO$CLOSE
                dc.w        syscall_qdos_open-bed       ; IO$OPEN
                dc.w        syscall_qdos_opennew-bed    ; IO$OPENNEW
                dc.w        syscall_qdos_opendir-bed    ; IO$OPENDIR
                dc.w        syscall_qdos_delete-bed     ; IO$DELETE
                dc.w        syscall_qdos_setbaud-bed    ; SETBAUD
bios_user_call:
buc:
                dc.w        syscall_qdos_rdbyte-buc
                dc.w        syscall_qdos_read-buc
                dc.w        syscall_qdos_wrbyte-buc
                dc.w        syscall_qdos_write-buc
console_name:   dc.w        name_end-name_start
name_start:     dc.b        'con_480x250a16x3'
name_end:
                dc.w        $0028,$000a,$01d0,$0000
                 
not_even_error: dc.w        nee_end-nee_start
nee_start:
                dc.b        'ADDRESS NOT EVEN',10,0
nee_end:

device_name_table:
l_af2:          dc.b        'FLP0'
l_af6:          dc.b        'MDV0'
l_afa:          dc.b        'RAM0'
serial_device_name:
                dc.w        sdn_end-sdn_start       ;; Should be 6
sdn_start:      dc.b        'SER1HR'
sdn_end:        dc.b        '    '                  ;; Extra padding for slightly longer name.
not_system_disk_error:
                dc.w        nsde_end-nsde_start     ;; should be 0x50
nsde_start:
                dc.b        10,'Not a SUCCESS system disk in drive A:',10
                dc.b        'Insert a system disk, then press any key.'
nsde_end:
signon_message:
                dc.w        signon_end-signon_start                         ;; Should be 0x5b
signon_start:
                dc.b        'SUCCESS (C) 1987 Digital Precision',10
                dc.b        'QDOS BIOS version 1.0 by B. Watson',10
                dc.b        '64k CP/M version 2.2',10
signon_end:
                dc.b        0
; It's not clear what this is used for. Nothing points to it.
login_message:
                dc.b        'login',13,0
                dc.b        0,0,0


; It doesn't look like this is used either.
file_open_error:
                dc.b        'Cannot open file, reason : ',0

; It doesn't look like this is used either.
file_write_prompt:
                dc.w        fwp_end-fwp_start                   ;; Should be 0x1b
fwp_start:      dc.b        'Which QDOS file to write : '
fwp_end:
                dc.b        0

; It doesn't look like this is used either.
file_read_prompt:
                dc.w        frp_end-frp_start                   ;; Should be 0x1a
frp_start:      dc.b        'Which QDOS file to read : '
frp_end:

z80_bios:
                dc.b        $d3,$00,$c9           ; BOOT;     out (0),a; ret
                dc.b        $d3,$01,$c9           ; WBOOT;    out (1),a; ret
                dc.b        $d3,$02,$c9           ; CONST;    out (2),a; ret
                dc.b        $d3,$03,$c9           ; CONIN;    out (3),a; ret
                dc.b        $d3,$04,$c9           ; CONOUT;   out (4),a; ret
                dc.b        $d3,$05,$c9           ; LIST;     out (5),a; ret
                dc.b        $d3,$06,$c9           ; PUNCH;    out (6),a; ret
                dc.b        $d3,$07,$c9           ; READER;   out (7),a; ret
                dc.b        $d3,$08,$c9           ; HOME;     out (8),a; ret
                dc.b        $d3,$09,$c9           ; SELDSK;   out (9),a; ret
                dc.b        $d3,$0a,$c9           ; SETTRK;   out (10),a; ret
                dc.b        $d3,$0b,$c9           ; SETSEC;   out (11),a; ret
                dc.b        $d3,$0c,$c9           ; SETDMA;   out (12),a; ret
                dc.b        $d3,$0d,$c9           ; READ;     out (13),a; ret
                dc.b        $d3,$0e,$c9           ; WRITE;    out (14),a; ret
                dc.b        $d3,$0f,$c9           ; LISTST;   out (15),a; ret
                dc.b        $d3,$10,$c9           ; SECTRAN;  out (16),a; ret
;; CP/M 3.3?
                dc.b        $d3,$11,$c9           ; CONOST;   out (17),a; ret
                dc.b        $d3,$12,$c9           ; AUXIST;   out (18),a; ret
                dc.b        $d3,$13,$c9           ; AUXOST;   out (19),a; ret
                dc.b        $00,$00,$00,$00,$00,$00,$00,$00
                dc.b        $00,$eb,$ac,$ea,$80,$eb,$00,$ec,$00,$00,$00,$00,$00,$00,$00,$00
                dc.b        $00,$eb,$ac,$ea,$90,$eb,$20,$ec,$00,$00,$00,$00,$00,$00,$00,$00
                dc.b        $00,$eb,$bc,$ea,$a0,$eb,$40,$ec,$00,$00,$00,$00,$00,$00,$00,$00
                dc.b        $00,$eb,$bc,$ea,$b0,$eb,$50,$ec,$00,$00,$00,$00,$00,$00,$00,$00
                dc.b        $00,$eb,$cc,$ea,$c0,$eb,$60,$ec,$00,$00,$00,$00,$00,$00,$00,$00
                dc.b        $00,$eb,$cc,$ea,$d0,$eb,$80,$ec,$00,$00,$00,$00,$00,$00,$00,$00
                dc.b        $00,$eb,$dc,$ea,$90,$eb,$20,$ec,$14,$00,$04,$0f,$01,$ff,$00,$7f
                dc.b        $00,$c0,$00,$01,$00,$03,$00,$00,$14,$00,$03,$07,$00,$63,$00,$3f
                dc.b        $00,$c0,$00,$01,$00,$00,$00,$00,$14,$00,$03,$07,$00,$c7,$00,$5f
                dc.b        $00,$e0,$00,$00,$00,$00,$00,$00

z80_bios_end:
;;#### End of Z80 bios at 0xcf5
