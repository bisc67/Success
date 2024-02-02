;
; Formatting:
;_______________I___________O_______________________________C
;               17          29                              61
;               Instr.      Operand                         Comment
; Tabs should be set to 4 characters, preferably set to insert spaces
;
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
                include     "qdos.i"
                include     "success.i"
                org         $0
start:          dc.l        $4afb0001
                dc.w        procdefs-start
                dc.w        init-start
signature:      dc.w        sigend-sigstart
sigstart:       dc.b        'SUCCESS ',$7f,' 1988 Digital Precision V1.0',$0a
sigend:
procdefs:
                dc.w        $0001
                dc.w        gocpm-*
                dc.b        cmdend-cmdstart
cmdstart:       dc.b        'GOCPM'
cmdend:
                dc.b        0
                dc.w        0
                dc.w        0
init:           rts
gocpm:      
                lea         bios_filename(pc),a0
                moveq.l     #IO_OPEN,d0
                moveq.l     #0,d3
                moveq.l     #-1,d1
                trap        #TRAP_IO
                tst.l       d0
                beq.s       bios_open_ok
                lea         bios_not_found(pc),a0
                move.l      a0,d0
                bset        #31,d0
                rts  
bios_open_ok:    
                move.l      a0,d5
                moveq.l     #0,d0
                trap        #TRAP_SVC               ; Switch to supervisor mode.
                moveq.l     #MT_CJOB,d0
                move.l      #$00011000,d2          ; Code Space needed
                moveq.l     #0,d3               ; Data space needed
                suba.l      a1,a1
                trap        #TRAP_JOB
                tst.l       d0
                beq.s       job_create_ok
                move.l      d0,-(sp)
                move.l      d5,a0
                moveq.l     #IO_CLOSE,d0
                trap        #TRAP_IO
                move.l      (sp)+,d0
                rts  
job_create_ok:   
                lea         z80_workspace+3(pc),a4
                clr.l       (a4)
loop:           tst.l       (a4)
                bne.s       running_from_rom
                subq.l      #1,(a4)
                tst.l       (a4)
                bmi.s       running_from_ram
running_from_rom:   
                lea          $f00(a0),a4               ;; $f00 is the workspace offset.
running_from_ram:
                moveq.l      #workspace_size/4,d0   ;; Clear 360 bytes               
                move.l       a4,a1
clear1:         clr.l        (a1)+
                dbf          d0, clear1
                lea          $1000(a0),a5           ;; Z80 RAM base, from workspace+4k
                move.l       d5,a0
                ;
                ; read the BIOS image
                ;
                lea         -$1000(a5),a1
                moveq.l     #IO_FSTRG,d0
                move.w      #$1000,d2
                moveq.l     #-1,d3
                trap        #TRAP_STRM
                moveq.l     #IO_CLOSE,d0
                trap        #TRAP_IO
                move.l      a5,a0
                ;; Clear Z80 RAM
                move.w      #$3fff,d0
clear3:         clr.l       (a0)+
                dbf         d0, clear3
                ; Setting up initial values for registers.
                clr.l       d0
                clr.l       d1
                clr.l       d5               ; HL
                clr.l       d4               ; BC
                clr.l       d7               ; A
                clr.l       d6               ; F
                lea         flag_table(pc),a2               ; flag translation table
                jsr         -$1000(a5)                      ; Initialize bios
                clr.l       d3
                lea         z80_exec_loop(pc),a1
z80_exec_loop: 
                moveq.l     #0,d0
                move.b      (a6)+,d0
                add.w       d0,d0
                lea         dispatch_group_0(pc,d0.w),a0
dispatch_instr:
                add.w       (a0),a0
                jmp         (a0)
; IX
ix_prefix_instr:
                move.w      d5,-(sp)                           ; Preserve HL
                move.w      reg_ix(a4),d5                      ; get IX register
                lea         ix_return(pc),a1
exec_prefix:    clr.l       d0
                move.b      (a6)+,d0
                lsl.w       #1,d0
                lea         dispatch_group_0(pc,d0.w),a0
                add.w       (a0),a0
                clr.l        d0
                jmp         (a0)
ix_return:      move.w      d5, reg_ix(a4)
                move.w      (sp)+,d5
                lea         z80_exec_loop(pc),a1
                jmp         (a1)
; IY    
iy_prefix_instr:    
                move.w      d5,-(sp)                           ; Preserve HL
                move.w      reg_iy(a4),d5                      ; get iy register
                lea         iy_return(pc),a1
                bra.s       exec_prefix
iy_return:      move.w      d5, reg_iy(a4)
                move.w      (sp)+,d5
                lea         z80_exec_loop(pc),a1
                jmp         (a1)
dispatch_group_0:
                ; Miscellaneous instructions
                dc.w        nop_instr-*,ld_bc_imm_instr-*,ld_bc_a_instr-*,inc_bc_instr-*,inc_b_instr-*,dec_b_instr-*,ld_b_imm_instr-*,rlca_instr-*                      ; $00-$07
                dc.w        exaf_af_instr-*,add_hl_bc_instr-*,ld_a_bc_instr-*,dec_bc_instr-*,inc_c_instr-*,dec_c_instr-*,ld_c_imm_instr-*,rrca_instr-*                  ; $08-$0f
                dc.w        djnz_instr-*,ld_de_imm_instr-*,ld_de_a_instr-*,inc_de_instr-*,inc_d_instr-*,dec_d_instr-*,ld_d_imm_instr-*,rla_instr-*                      ; $10-$17
                dc.w        jr_d_instr-*,add_hl_de_instr-*,ld_a_de_instr-*,dec_de_instr-*,inc_e_instr-*,dec_e_instr-*,ld_e_imm_instr-*,rra_instr-*                      ; $18-$1f
                dc.w        jr_nz_instr-*,ld_hl_imm_instr-*,ld_imm_hl_instr-*,inc_hl_instr-*,inc_h_instr-*,dec_h_instr-*,ld_h_imm_instr-*,daa_instr-*                   ; $20-$27
                dc.w        jr_z_instr-*,add_hl_hl_instr-*,ld_hl_ind_instr-*,dec_hl_instr-*,inc_l_instr-*,dec_l_instr-*,ld_l_imm_instr-*,cpl_instr-*                    ; $28-$2f
                dc.w        jr_nc_instr-*,ld_sp_imm_instr-*,ld_ind_a_instr-*,inc_sp_instr-*,inc_m_instr-*,dec_m_instr-*,ld_m_imm_instr-*,scf_instr-*                  ; $30-$37
                dc.w        jr_c_instr-*,add_hl_sp_instr-*,ld_a_ind_instr-*,dec_sp_instr-*,inc_a_instr-*,dec_a_instr-*,ld_a_imm_instr-*,ccf_instr-*                     ; $38-$3f
                ; Register to register move
                dc.w        ld_b_b_instr-*,ld_b_c_instr-*,ld_b_d_instr-*,ld_b_e_instr-*,ld_b_h_instr-*,ld_b_l_instr-*,ld_b_m_instr-*,ld_b_a_instr-*                    ; $40-$47
                dc.w        ld_c_b_instr-*,ld_c_c_instr-*,ld_c_d_instr-*,ld_c_e_instr-*,ld_c_h_instr-*,ld_c_l_instr-*,ld_c_m_instr-*,ld_c_a_instr-*                    ; $48-$4f
                dc.w        ld_d_b_instr-*,ld_d_c_instr-*,ld_d_d_instr-*,ld_d_e_instr-*,ld_d_h_instr-*,ld_d_l_instr-*,ld_d_m_instr-*,ld_d_a_instr-*                    ; $50-$57
                dc.w        ld_e_b_instr-*,ld_e_c_instr-*,ld_e_d_instr-*,ld_e_e_instr-*,ld_e_h_instr-*,ld_e_l_instr-*,ld_e_m_instr-*,ld_e_a_instr-*                    ; $58-$5f
                dc.w        ld_h_b_instr-*,ld_h_c_instr-*,ld_h_d_instr-*,ld_h_e_instr-*,ld_h_h_instr-*,ld_h_l_instr-*,ld_h_m_instr-*,ld_h_a_instr-*                    ; $60-$67
                dc.w        ld_l_b_instr-*,ld_l_c_instr-*,ld_l_d_instr-*,ld_l_e_instr-*,ld_l_h_instr-*,ld_l_l_instr-*,ld_l_m_instr-*,ld_l_a_instr-*                    ; $68-$6f
                dc.w        ld_m_b_instr-*,ld_m_c_instr-*,ld_m_d_instr-*,ld_m_e_instr-*,ld_m_h_instr-*,ld_m_l_instr-*,halt_instr-*,ld_m_a_instr-*                ; $70-$77
                dc.w        ld_a_b_instr-*,ld_a_c_instr-*,ld_a_d_instr-*,ld_a_e_instr-*,ld_a_h_instr-*,ld_a_l_instr-*,ld_a_m_instr-*,ld_a_a_instr-*                    ; $78-$7f
                ; Arithmetic and logic operations
                dc.w        add_a_b_instr-*, add_a_c_instr-*,add_a_d_instr-*,add_a_e_instr-*,add_a_h_instr-*,add_a_l_instr-*,add_a_m_instr-*,add_a_a_instr-*           ; $80-$87
                dc.w        adc_a_b_instr-*, adc_a_c_instr-*,adc_a_d_instr-*,adc_a_e_instr-*,adc_a_h_instr-*,adc_a_l_instr-*,adc_a_m_instr-*,adc_a_a_instr-*           ; $88-$8f
                dc.w        sub_a_b_instr-*, sub_a_c_instr-*,sub_a_d_instr-*,sub_a_e_instr-*,sub_a_h_instr-*,sub_a_l_instr-*,sub_a_m_instr-*,sub_a_a_instr-*           ; $90-$97
                dc.w        sbc_a_b_instr-*, sbc_a_c_instr-*,sbc_a_d_instr-*,sbc_a_e_instr-*,sbc_a_h_instr-*,sbc_a_l_instr-*,sbc_a_m_instr-*,sbc_a_a_instr-*           ; $98-$9f
                dc.w        and_b_instr-*, and_c_instr-*,and_d_instr-*,and_e_instr-*,and_h_instr-*,and_l_instr-*,and_m_instr-*,and_a_instr-*                           ; $a0-$a7
                dc.w        xor_b_instr-*, xor_c_instr-*,xor_d_instr-*,xor_e_instr-*,xor_h_instr-*,xor_l_instr-*,xor_m_instr-*,xor_a_instr-*                           ; $a8-$af
                dc.w        or_b_instr-*, or_c_instr-*,or_d_instr-*,or_e_instr-*,or_h_instr-*,or_l_instr-*,or_m_instr-*,or_a_instr-*                                   ; $b0-$b7
                dc.w        cp_b_instr-*, cp_c_instr-*,cp_d_instr-*,cp_e_instr-*,cp_h_instr-*,cp_l_instr-*,cp_m_instr-*,cp_a_instr-*                                   ; $b8-$bf
                ; Miscellaneous instructions
                dc.w        ret_nz_instr-*,pop_bc_instr-*,jp_nz_instr-*,jp_instr-*,call_nz_instr-*,push_bc_instr-*,add_a_imm_instr-*,rst_00_instr-*                     ; $c0-$c7
                dc.w        ret_z_instr-*,ret_instr-*,jp_z_instr-*,cb_prefix_instr-*,call_z_instr-*,call_instr-*,adc_imm_instr-*,rst_08_instr-*                        ; $c8-$cf
                dc.w        ret_nc_instr-*,pop_de_instr-*,jp_nc_instr-*,out_imm_instr-*,call_nc_instr-*,push_de_instr-*,sub_imm_instr-*,rst_10_instr-*                ; $d0-$d7
                dc.w        ret_c_instr-*,exx_instr-*,jp_c_instr-*,in_imm_instr-*,call_c_instr-*,ix_prefix_instr-*,sbc_imm_instr-*,rst_18_instr-*                 ; $d8-$df
                dc.w        ret_po_instr-*,pop_hl_instr-*,jp_po_instr-*,ex_ind_sp_hl_instr-*,call_po_instr-*,push_hl_instr-*,and_imm_instr-*,rst_20_instr-*             ; $e0-$e7
                dc.w        ret_pe_instr-*,jp_hl_instr-*,jp_pe_instr-*,ex_de_hl_instr-*,call_pe_instr-*,ed_group_instr-*,xor_imm_instr-*,rst_28_instr-*                 ; $e8-$ef
                dc.w        ret_p_instr-*,pop_af_instr-*,jp_p_instr-*,di_instr-*,call_p_instr-*,push_af_instr-*,or_imm_instr-*,rst_30_instr-*                       ; $f0-$f7
                dc.w        ret_m_instr-*,ld_sp_hl_instr-*,jp_m_instr-*,ei_instr-*,call_m_instr-*,iy_prefix_instr-*,cp_imm_instr-*,rst_38_instr-*                       ; $f8-$ff
im0_instr:
im1_instr:
im2_instr:
reti_instr:
retn_instr:
ld_i_a_instr:
ld_r_a_instr:
in_c_instr:
nop_instr:
                jmp         (a1)
ld_bc_imm_instr:
                move.b       (a6)+,-(a4)
                move.b       (a6)+,-(a4)
                move.w       (a4)+,d4
                jmp          (a1)
ld_bc_a_instr:  
                move.b       d7, 0(a5,d4.l)
                jmp          (a1)
inc_bc_instr:   
                addq.w       #1,d4
                jmp          (a1)
inc_b_instr:    
                move.w       d4,(a4)
                addq.b       #1,(a4)
                move         sr,d0
                move.w       (a4),d4
                and.b        (a2),d6
                andi.b       #$fe,d0            ; This should be 0x200,0x00fe but is 0xc03c 0x00fe
                or.b         d0,d6
                jmp          (a1)
dec_b_instr:    
                move.w       d4,(a4)
                subq.b       #1,(a4)
                move         sr,d0
                move.w       (a4),d4
                and.b        (a2),d6
                andi.b       #$fe,d0
                or.b         d0,d6
                jmp          (a1)
ld_b_imm_instr:     
                move.b       d4,-(a4)           ; save c
                move.b       (a6)+,-(a4)         ; load a new b
                move.w       (a4)+,d4               ; restore bc
                jmp          (a1)
rlca_instr:     
                lsr.b        #1,d6
                rol.b        #1,d7
                move         sr,d0
                lsr.b        #1,d0
                roxl.b       #1,d6
                jmp          (a1)
exaf_af_instr:  
                move.b       reg_af_dash(a4),d0
                move.b       d7, reg_af_dash(a4)
                move.b       d0,d7
                move.b       reg_af_dash+1(a4),d0
                move.b       d6, reg_af_dash+1(a4)
                move.b       d0,d6
                jmp          (a1)
add_hl_bc_instr:    
                lsr.b        #1,d6               ; Shift out carry
                add.w        d4,d5               ; add bc to hl
                roxl.b       #1,d6               ; shift back carry bit
                jmp          (a1)
ld_a_bc_instr:  
                move.b       0(a5,d4.l),d7
                jmp          (a1)
dec_bc_instr:   
                subq.w       #1,d4
                jmp          (a1)
inc_c_instr:    
                addq.b       #1,d4
                move         sr,d0
                and.b        (a2),d6
                andi.b       #$fe,d0
                or.b         d0,d6
                jmp          (a1)
dec_c_instr:    
                subq.b       #1,d4
                move         sr,d0
                and.b        (a2),d6
                andi.b       #$fe,d0
                or.b         d0,d6
                jmp          (a1)
ld_c_imm_instr:     
                move.b       (a6)+,d4
                jmp          (a1)
rrca_instr:     
                lsr.b          #1,d6
                ror.b          #1,d7
                move           sr,d0
                lsr.b          #1,d0
                roxl.b         #1,d6
                jmp            (a1)
djnz_instr:     
                move.b       (a6)+,d0
                sub.w        #$100,d4
                move.w       d4,(a4)
                tst.b        (a4)
                bne.s        jr_offset
                jmp          (a1)
ld_de_imm_instr:    
                move.b       (a6)+,-(a4)        ; Fetch e
                move.b       (a6)+,-(a4)        ; Fetch d
                move.w       (a4)+,d3           ; Byte swapped
                jmp          (a1)
ld_de_a_instr:  
                move.b       d7, 0(a5,d3.l)
                jmp          (a1)
inc_de_instr:   
                addq.w       #1,d3
                jmp          (a1)
inc_d_instr:    
                move.w       d3,(a4)
                addq.b       #1,(a4)
                move         sr,d0
                move.w       (a4),d3
                and.b        (a2),d6
                andi.b       #$fe,d0
                or.b         d0,d6
                jmp          (a1)
dec_d_instr:    
                move.w       d3,(a4)
                subq.b       #1,(a4)
                move         sr,d0
                move.w       (a4),d3
                and.b        (a2),d6
                andi.b       #$fe,d0
                or.b         d0,d6
                jmp          (a1)
ld_d_imm_instr:     
                move.b       d3,-(a4)
                move.b       (a6)+,-(a4)
                move.w       (a4)+,d3
                jmp          (a1)
rla_instr:  
                roxr.b       #1,d6
                roxl.b       #1,d7
                roxl.b       #1,d6
                jmp          (a1)
jr_d_instr:     
                move.b       (a6)+,d0
jr_offset:  
                ext.w        d0
                adda.w       d0,a6
                jmp          (a1)
jr_nz_instr:    
                move.b       (a6)+,d0
                move         d6,ccr
                bne.s        jr_offset
                jmp          (a1)
jr_z_instr:     
                move.b       (a6)+,d0
                move         d6, ccr
                beq.s        jr_offset
                jmp          (a1)
jr_nc_instr:    
                move.b       (a6)+,d0
                move         d6, ccr
                bcc.s        jr_offset
                jmp          (a1)
jr_c_instr:     
                move.b       (a6)+,d0
                move         d6, ccr
                bcs.s        jr_offset
                jmp          (a1)
add_hl_de_instr:    
                lsr.b        #1,d6
                add.w        d3,d5
                roxl.b       #1,d6
                jmp          (a1)
ld_a_de_instr:  
                move.b       0(a5,d3.l),d7
                jmp          (a1)
dec_de_instr:   
                subq.w       #1,d3
                jmp          (a1)
inc_e_instr:    
                addq.b       #1,d3
                move         sr,d0
                and.b        (a2),d6
                andi.b       #$fe,d0
                or.b         d0,d6
                jmp          (a1)
dec_e_instr:    
                subq.b       #1,d3
                move         sr,d0
                and.b        (a2),d6
                andi.b       #$fe,d0
                or.b         d0,d6
                jmp          (a1)
ld_e_imm_instr:     
                move.b       (a6)+,d3
                jmp          (a1)
rra_instr:  
                lsr.b        #1,d6
                roxr.b       #1,d7
                roxl.b       #1,d6
                jmp          (a1)
ld_hl_imm_instr:    
                move.b       (a6)+,-(a4)
                move.b       (a6)+,-(a4)
                move.w       (a4)+,d5
                jmp          (a1)
ld_imm_hl_instr:    
                move.b       (a6)+,-(a4)
                move.b       (a6)+,-(a4)
                move.w       (a4)+,d0
                lea          0(a5,d0.l),a0
                move.w       d5,(a4)
                move.b       d5,(a0)+
                move.b       (a4),(a0)
                jmp          (a1)
inc_hl_instr:   
                addq.w       #1,d5
                jmp          (a1)
inc_h_instr:    
                move.w       d5,(a4)
                addq.b       #1,(a4)
                move         sr,d0
                move.w       (a4),d5
                and.b        (a2),d6
                andi.b       #$fe,d0
                or.b         d0,d6
                jmp          (a1)
dec_h_instr:    
                move.w       d5,(a4)
                subq.b       #1,(a4)
                move         sr,d0
                move.w       (a4),d5
                and.b        (a2),d6
                andi.b       #$fe,d0
                or.b         d0,d6
                jmp          (a1)
ld_h_imm_instr:
                move.b      d5,-(a4)
                move.b      (a6)+,-(a4)
                move.w      (a4)+,d5
                jmp         (a1)
daa_instr:
                move.b      d7,d0
                andi.b      #$f,d0
                cmpi.b      #$9,d0
                bhi.s       lower_carry
                andi.b      #$f,d2
                andi.b      #$f,d1
                ori.b       #$f0,d1
                add.b       d2,d1
                bcc.s       did_carry
lower_carry:
; Although this addi.b should be the long-form addi, it is forcing it to be the short form. So we just
; force it for now.
; It should be  addi.b      #$6,d7

                dc.w        $0607,$0006
                bcs.s       upper_carry
did_carry:
                btst        #0,d6
                bne.s       upper_carry
                move.b      d7,d0
                andi.b      #$f0,d0
                cmpi.b      #$90,d0
                bls.s       no_upper_carry
upper_carry:
; Although this addi.b should be the long-form addi, it is forcing it to be the short form
                addi.b      #$60,d7
                or          #01, ccr
daa_done:       move        sr,d6
                clr.l       d1
                clr.l       d2
                jmp         (a1)
no_upper_carry: 
                tst.b       d7
                bra.s       daa_done
add_hl_hl_instr:
                lsr.b       #1,d6
                add.w       d5,d5
                roxl.b      #1,d6
                jmp         (a1)
ld_hl_ind_instr:
                move.b      (a6)+,-(a4)
                move.b      (a6)+,-(a4)
                move.w      (a4)+,d0
                lea         0(a5,d0.l),a0
                move.b      (a0)+,-(a4)
                move.b      (a0),-(a4)
                move.w      (a4)+,d5
                jmp         (a1)
dec_hl_instr:   
                subq.w      #1,d5
                jmp         (a1)
inc_l_instr:    
                addq.b      #1,d5
                move        sr,d0
                and.b       (a2),d6
                andi.b      #$fe,d0
                or.b        d0,d6
                jmp         (a1)
dec_l_instr:    
                subq.b      #1,d5
                move        sr,d0
                and.b       (a2),d6
                andi.b      #$fe,d0
                or.b        d0,d6
                jmp         (a1)
ld_l_imm_instr:
                move.b       (a6)+,d5
                jmp      (a1)
cpl_instr:
               not.b       d7
                jmp         (a1)
ld_sp_imm_instr:
                move.b      (a6)+,-(a4)
                move.b      (a6)+,-(a4)
                move.w      (a4)+,d0
                lea         0(a5,d0.l),a3
                jmp         (a1)
ld_ind_a_instr:
                move.b      (a6)+,-(a4)
                move.b      (a6)+,-(a4)
                move.w      (a4)+,d0
                move.b      d7,0(a5,d0.l)
                jmp         (a1)
inc_sp_instr:
                addq.l      #1,a3
                jmp         (a1)
inc_m_instr:
                beq.s     inc_idx_with_offset
                addq.b      #1, 0(a5,d5.l)
                move        sr,d0
                and.b       (a2),d6
                andi.b      #$fe,d0
                or.b      d0,d6
                jmp         (a1)
inc_idx_with_offset:
                move.b      (a6)+,d0
                ext.w       d0
                add.w       d5,d0
                addq.b      #1, 0(a5,d0.l)
                move        sr,d0
                and.b       (a2),d6
                andi.b      #$fe,d0
                or.b      d0,d6
                jmp         (a1)
dec_m_instr:
                beq.s       dec_idx_with_offset
                subq.b      #1, 0(a5,d5.l)
                move        sr,d0
                and.b       (a2),d6
                andi.b      #$fe,d0
                or.b      d0,d6
                jmp         (a1)
dec_idx_with_offset:
                move.b      (a6)+,d0
                ext.w       d0
                add.w       d5,d0
                subq.b      #1, 0(a5,d0.l)
                move        sr,d0
                and.b       (a2),d6
                andi.b      #$fe,d0
                or.b      d0,d6
                jmp         (a1)
ld_m_imm_instr:
                beq.s       ld_m_imm_with_offset
                move.b      (a6)+, 0(a5,d5.l)
                jmp         (a1)
ld_m_imm_with_offset
                move.b      (a6)+,d0
                ext.w       d0
                add.w       d5,d0
                move.b      (a6)+, 0(a5,d0.l)
                jmp         (a1)
scf_instr:
                bset      #0,d6
                jmp         (a1)
add_hl_sp_instr:
                move.l      a3,d0
                sub.l       a5,d0
                lsr.b       #1,d6
                add.w       d0,d5
                roxl.b      #1,d6
                jmp         (a1)
ld_a_ind_instr:
                move.b      (a6)+,-(a4)
                move.b      (a6)+,-(a4)
                move.w      (a4)+,d0
                move.b      0(a5,d0.l),d7
                jmp         (a1)
dec_sp_instr:
                subq.l      #1,a3
                jmp         (a1)
inc_a_instr:
                move.b      d7,d2
                moveq.l     #$1,d1
                addq.b      #1,d7
                move        sr,d0
                and.b       (a2),d6
                andi.b      #$fe,d0
                or.b      d0,d6
                jmp         (a1)
dec_a_instr:
                move.b      d7,d2
                moveq.l     #-1,d1
                subq.b      #1,d7
                move        sr,d0
                and.b       (a2),d6
                andi.b      #$fe,d0
                or.b      d0,d6
                jmp         (a1)
ld_a_imm_instr:
                move.b      (a6)+,d7
                jmp         (a1)
ccf_instr:
                bchg      #0,d6
                jmp         (a1)
ld_b_b_instr:
                jmp         (a1)
ld_b_c_instr:
                move.b      d4,-(a4)
                move.b      d4,-(a4)
                move.w      (a4)+,d4
                jmp         (a1)
ld_b_d_instr:
                move.b      d4,d0
                move.w      d3,d4
                move.b      d0,d4
                jmp         (a1)
ld_b_e_instr:
                move.b      d4,-(a4)
                move.b      d3,-(a4)
                move.w      (a4)+,d4
                jmp         (a1)
ld_b_h_instr:
                move.b      d4,d0
                move.w      d5,d4
                move.b      d0,d4
                jmp         (a1)
ld_b_l_instr:
                move.b      d4,-(a4)
                move.b      d5,-(a4)
                move.w      (a4)+,d4
                jmp         (a1)
ld_b_m_instr:
                beq.s       ld_b_ind_ixy
                move.b      d4,-(a4)
                move.b      0(a5,d5.l),-(a4)
                move.w      (a4)+,d4
                jmp         (a1)
ld_b_ind_ixy:
                move.b      (a6)+,d0
                ext.w       d0
                add.w       d5,d0
                move.b      d4,-(a4)
                move.b      0(a5,d0.l),-(a4)
                move.w      (a4)+,d4
                jmp         (a1)
ld_b_a_instr:
                move.b      d4,-(a4)
                move.b      d7,-(a4)
                move.w      (a4)+,d4
                jmp         (a1)
ld_c_b_instr:
                move.w      d4,(a4)
                move.b      (a4),d4
                jmp         (a1)
ld_c_c_instr:
                jmp         (a1)
ld_c_d_instr:
                move.w      d3,(a4)
                move.b      (a4),d4
                jmp         (a1)
ld_c_e_instr:
                move.b      d3,d4
                jmp         (a1)
ld_c_h_instr:
                move.w      d5,(a4)
                move.b      (a4),d4
                jmp         (a1)
ld_c_l_instr:
                move.b      d5,d4
                jmp         (a1)
ld_c_m_instr:
                beq.s       ld_c_ind_ixy
                move.b      0(a5,d5.l),d4
                jmp         (a1)
ld_c_ind_ixy:
                move.b      (a6)+,d0
                ext.w       d0
                add.w       d5,d0
                move.b      0(a5,d0.l),d4
                jmp         (a1)
ld_c_a_instr:
                move.b      d7,d4
                jmp         (a1)
ld_d_b_instr:
                move.b      d3,d0
                move.w      d4,d3
                move.b      d0,d3
                jmp         (a1)
ld_d_c_instr:
                move.b      d3,-(a4)
                move.b      d4,-(a4)
                move.w      (a4)+,d3
                jmp         (a1)
ld_d_d_instr:
                jmp         (a1)
ld_d_e_instr:
                move.b      d3,-(a4)
                move.b      d3,-(a4)
                move.w      (a4)+,d3
                jmp         (a1)
ld_d_h_instr:
                move.b      d3,d0
                move.w      d5,d3
                move.b      d0,d3
                jmp         (a1)
ld_d_l_instr:
                move.b      d3,-(a4)
                move.b      d5,-(a4)
                move.w      (a4)+,d3
                jmp         (a1)
ld_d_m_instr:
                beq.s       ld_d_idx_instr
                move.b      d3,-(a4)
                move.b      0(a5,d5.l),-(a4)
                move.w      (a4)+,d3
                jmp         (a1)
ld_d_idx_instr:
                move.b      (a6)+,d0
                ext.w       d0
                add.w       d5,d0
                move.b      d3,-(a4)
                move.b      0(a5,d0.l),-(a4)
                move.w      (a4)+,d3
                jmp         (a1)
ld_d_a_instr:
                move.b      d3,-(a4)
                move.b      d7,-(a4)
                move.w      (a4)+,d3
                jmp         (a1)
ld_e_b_instr:
                move.w      d4,(a4)
                move.b      (a4),d3
                jmp         (a1)
ld_e_c_instr:
                move.b      d4,d3
                jmp         (a1)
ld_e_d_instr:
                move.w      d3,(a4)
                move.b      (a4),d3
                jmp         (a1)
ld_e_e_instr:
                jmp         (a1)
ld_e_h_instr:
                move.w      d5,(a4)
                move.b      (a4),d3
                jmp         (a1)
ld_e_l_instr:
                move.b      d5,d3
                jmp         (a1)
ld_e_m_instr:
                beq.s       ld_e_idx_instr
                move.b      0(a5,d5.l),d3
                jmp         (a1)
ld_e_idx_instr:
                move.b      (a6)+,d0
                ext.w       d0
                add.w       d5,d0
                move.b      0(a5,d0.l),d3
                jmp         (a1)s
ld_e_a_instr:
                move.b      d7,d3
                jmp         (a1)
ld_h_b_instr:
                move.b      d5,d0
                move.w      d4,d5
                move.b      d0,d5
                jmp         (a1)
ld_h_c_instr:
                move.b      d5,-(a4)
                move.b      d4,-(a4)
                move.w      (a4)+,d5
                jmp         (a1)
ld_h_d_instr:
                move.b      d5,d0
                move.w      d3,d5
                move.b      d0,d5
                jmp         (a1)
ld_h_e_instr:
                move.b      d5,-(a4)
                move.b      d3,-(a4)
                move.w      (a4)+,d5
                jmp         (a1)
ld_h_h_instr:
                jmp         (a1)
ld_h_l_instr:
                move.b      d5,-(a4)
                move.b      d5,-(a4)
                move.w      (a4)+,d5
                jmp         (a1)
ld_h_m_instr:
                beq.s       ld_h_idx_instr
                move.b      d5,-(a4)
                move.b      0(a5,d5.l),-(a4)
                move.w      (a4)+,d5
                jmp         (a1)
ld_h_idx_instr:
                move.b      (a6)+,d0
                ext.w       d0
                add.w       d5,d0
                move.b      0(a5,d0.l),(a7)
                jmp         (a1)
ld_h_a_instr:
                move.b      d5,-(a4)
                move.b      d7,-(a4)
                move.w      (a4)+,d5
                jmp         (a1)
ld_l_b_instr:
                move.w      d4,(a4)
                move.b      (a4),d5
                jmp         (a1)
ld_l_c_instr:
                move.b      d4,d5
                jmp         (a1)
ld_l_d_instr:
                move.w      d3,(a4)
                move.b      (a4),d5
                jmp         (a1)
ld_l_e_instr:
                move.b      d3,d5
                jmp         (a1)
ld_l_h_instr:
                move.w      d5,(a4)
                move.b      (a4),d5
                jmp         (a1)
ld_l_l_instr:
                jmp         (a1)
ld_l_m_instr:
                beq.s       ld_l_idx_instr
                move.b      0(a5,d5.l),d5
                jmp         (a1)
ld_l_idx_instr:
                move.b      (a6)+,d0
                ext.w       d0
                add.w       d5,d0
                move.b      0(a5,d0.l),1(sp)
                jmp         (a1)
ld_l_a_instr:
                move.b      d7,d5
                jmp         (a1)
ld_m_b_instr:
                beq.s       ld_idx_b_instr
                move.w      d4,(a4)
                move.b      (a4),0(a5,d5.l)
                jmp         (a1)
ld_idx_b_instr:
                move.w      d4,(a4)
                move.b      (a6)+,d0
                ext.w       d0
                add.w       d5,d0
                move.b      (a4),0(a5,d0.l)
                jmp         (a1)
ld_m_c_instr:
                beq.s       ld_idx_c_instr
                move.b      d4,0(a5,d5.l)
                jmp         (a1)
ld_idx_c_instr:
                move.b      (a6)+,d0
                ext.w       d0
                add.w       d5,d0
                move.b      d4,0(a5,d0.l)
                jmp         (a1)
ld_m_d_instr:
                beq.s       ld_idx_d_instr
                move.w      d3,(a4)
                move.b      (a4),0(a5,d5.l)
                jmp         (a1)
ld_idx_d_instr:
                move.w      d3,(a4)
                move.b      (a6)+,d0
                ext.w       d0
                add.w       d5,d0
                move.b      (a4),0(a5,d0.l)
                jmp         (a1)
ld_m_e_instr:
                beq.s       ld_idx_e_instr
                move.b      d3,0(a5,d5.l)
                jmp         (a1)
ld_idx_e_instr:
                move.b      (a6)+,d0
                ext.w       d0
                add.w       d5,d0
                move.b      d3,0(a5,d0.l)
                jmp         (a1)
ld_m_h_instr:
                beq.s       ld_idx_h_instr
                move.w      d5,(a4)
                move.b      (a4),0(a5,d5.l)
                jmp         (a1)
ld_idx_h_instr:
                move.b      (a6)+,d0
                ext.w       d0
                add.w       d5,d0
                move.b      (a7),0(a5,d0.l)
                jmp         (a1)
ld_m_l_instr:
                beq.s       ld_idx_l_instr
                move.b      d5,0(a5,d5.l)
                jmp         (a1)
ld_idx_l_instr:
                move.b      (a6)+,d0
                ext.w       d0
                add.w       d5,d0
                move.b    1(sp),0(a5,d0.l)
                jmp         (a1)
halt_instr:
                bra       not_implemented
ld_m_a_instr:
                beq.s       ld_idx_a_instr
                move.b      d7,0(a5,d5.l)
                jmp         (a1)
ld_idx_a_instr:
                move.b      (a6)+,d0
                ext.w       d0
                add.w       d5,d0
                move.b      d7,0(a5,d0.l)
                jmp         (a1)
ld_a_b_instr:
                move.w      d4,(a4)
                move.b      (a4),d7
                jmp         (a1)
ld_a_c_instr:
                move.b      d4,d7
                jmp         (a1)
ld_a_d_instr:
                move.w      d3,(a4)
                move.b      (a4),d7
                jmp         (a1)
ld_a_e_instr:
                move.b      d3,d7
                jmp         (a1)
ld_a_h_instr:
                move.w      d5,(a4)
                move.b      (a4),d7
                jmp         (a1)
ld_a_l_instr:
                move.b      d5,d7
                jmp         (a1)
ld_a_m_instr:
                beq.s       ld_a_idx_instr
                move.b      0(a5,d5.l),d7
                jmp         (a1)
ld_a_idx_instr:
                move.b      (a6)+,d0
                ext.w       d0
                add.w       d5,d0
                move.b      0(a5,d0.l),d7
                jmp         (a1)
ld_a_a_instr:
                jmp         (a1)
add_a_b_instr:
                move.b      d7,d1
                move.w      d4,(a4)
                move.b      (a4),d2
                add.b       d2,d7
                move        sr,d6
                jmp         (a1)
add_a_c_instr:
                move.b      d7,d1
                move.b      d4,d2
                add.b       d2,d7
                move        sr,d6
                jmp         (a1)
add_a_d_instr:
                move.b      d7,d1
                move.w      d3,(a4)
                move.b      (a4),d2
                add.b       d2,d7
                move        sr,d6
                jmp         (a1)
add_a_e_instr:
                move.b      d7,d1
                move.b      d3,d2
                add.b       d2,d7
                move        sr,d6
                jmp         (a1)
add_a_h_instr:
                move.b      d7,d1
                move.w      d5,(a4)
                move.b      (a4),d2
                add.b       d2,d7
                move        sr,d6
                jmp         (a1)
add_a_l_instr:
                move.b      d7,d1
                move.b      d5,d2
                add.b       d2,d7
                move        sr,d6
                jmp         (a1)
add_a_m_instr:
                beq.s     add_a_idx_instr
                move.b      d7,d1
                move.b      0(a5,d5.l),d2
                add.b       d2,d7
                move        sr,d6
                jmp         (a1)
add_a_idx_instr:
                move.b      (a6)+,d0
                ext.w       d0
                add.w       d5,d0
                move.b      d7,d1
                move.b      0(a5,d0.l),d2
                add.b       d2,d7
                move        sr,d6
                jmp         (a1)
add_a_a_instr:
                move.b      d7,d1
                move.b      d7,d2
                add.b       d7,d7
                move        sr,d6
                jmp         (a1)
adc_a_b_instr:
                move.w      d4,(a4)
                move.b      (a4),d2
                move.b      d7,d1
                and.b       (a2),d6
                lsr.b       #1,d6
                addx.b      d2,d7
                move        sr,d6
                jmp         (a1)
adc_a_c_instr:
                move.b      d4,d2
                move.b      d7,d1
                and.b       (a2),d6
                lsr.b       #1,d6
                addx.b      d2,d7
                move        sr,d6
                jmp         (a1)
adc_a_d_instr:
                move.w      d3,(a4)
                move.b      (a4),d2
                move.b      d7,d1
                and.b       (a2),d6
                lsr.b       #1,d6
                addx.b      d2,d7
                move        sr,d6
                jmp         (a1)
adc_a_e_instr:
                move.b      d3,d2
                move.b      d7,d1
                and.b       (a2),d6
                lsr.b       #1,d6
                addx.b      d2,d7
                move        sr,d6
                jmp         (a1)
adc_a_h_instr:
                move.w      d5,(a4)
                move.b      (a4),d2
                move.b      d7,d1
                and.b       (a2),d6
                lsr.b       #1,d6
                addx.b      d2,d7
                move        sr,d6
                jmp         (a1)
adc_a_l_instr:
                move.b      d5,d2
                move.b      d7,d1
                and.b       (a2),d6
                lsr.b       #1,d6
                addx.b      d2,d7
                move        sr,d6
                jmp         (a1)
adc_a_m_instr:
                beq.s     adc_a_idx_instr
                move.b      d7,d1
                move.b      0(a5,d5.l),d2
                and.b       (a2),d6
                lsr.b       #1,d6
                addx.b      d2,d7
                move        sr,d6
                jmp         (a1)
adc_a_idx_instr:
                move.b      (a6)+,d0
                ext.w       d0
                add.w       d5,d0
                move.b      d7,d1
                move.b      0(a5,d0.l),d2
                and.b       (a2),d6
                lsr.b       #1,d6
                addx.b      d2,d7
                move        sr,d6
                jmp         (a1)
adc_a_a_instr:
                move.b      d7,d1
                move.b      d1,d2
                and.b       (a2),d6
                lsr.b       #1,d6
                addx.b      d7,d7
                move        sr,d6
                jmp         (a1)
sub_a_b_instr:
                move.w      d4,(a4)
                move.b      d7,d2
                move.b      (a4),d1
                sub.b       d1,d7
                move        sr,d6
                jmp         (a1)
sub_a_c_instr:
                move.b      d7,d2
                move.b      d4,d1
                sub.b       d1,d7
                move        sr,d6
                jmp         (a1)
sub_a_d_instr:
                move.w      d3,(a4)
                move.b      d7,d2
                move.b      (a4),d1
                sub.b       d1,d7
                move        sr,d6
                jmp         (a1)
sub_a_e_instr:
                move.b      d7,d2
                move.b      d3,d1
                sub.b       d1,d7
                move        sr,d6
                jmp         (a1)
sub_a_h_instr:
                move.w      d5,(a4)
                move.b      d7,d2
                move.b      (a4),d1
                sub.b       (a4),d7
                move        sr,d6
                jmp         (a1)
sub_a_l_instr:
                move.b      d7,d2
                move.b      d5,d1
                sub.b       d1,d7
                move        sr,d6
                jmp         (a1)
sub_a_m_instr:
                beq.s     sub_idx_instr
                move.b      d7,d2
                move.b      0(a5,d5.l),d1
                sub.b       d1,d7
                move        sr,d6
                jmp         (a1)
sub_idx_instr:
                move.b      (a6)+,d0
                ext.w       d0
                add.w       d5,d0
                move.b      d7,d2
                move.b      0(a5,d0.l),d1
                sub.b       d1,d7
                move        sr,d6
                jmp         (a1)
sub_a_a_instr:
                move.b      d7,d2
                move.b      d7,d1
                sub.b       d7,d7
                move        sr,d6
                jmp         (a1)
sbc_a_b_instr:
                move.w      d4,(a4)
                move.b      d7,d2
                move.b      (a4),d1
                and.b       (a2),d6
                lsr.b       #1,d6
                subx.b      d1,d7
                move        sr,d6
                jmp         (a1)
sbc_a_c_instr:
                move.b      d7,d2
                move.b      d4,d1
                and.b       (a2),d6
                lsr.b       #1,d6
                subx.b      d1,d7
                move        sr,d6
                jmp         (a1)
sbc_a_d_instr:
                move.w      d3,(a4)
                move.b      d7,d2
                move.b      (a4),d1
                and.b       (a2),d6
                lsr.b       #1,d6
                subx.b      d1,d7
                move        sr,d6
                jmp         (a1)
sbc_a_e_instr:
                move.b      d7,d2
                move.b      d3,d1
                and.b       (a2),d6
                lsr.b       #1,d6
                subx.b      d1,d7
                move        sr,d6
                jmp         (a1)
sbc_a_h_instr:
                move.w      d5,(a4)
                move.b      d7,d2
                move.b      (a4),d1
                and.b       (a2),d6
                lsr.b       #1,d6
                subx.b      d1,d7
                move        sr,d6
                jmp         (a1)
sbc_a_l_instr:
                move.b      d7,d2
                move.b      d5,d1
                and.b       (a2),d6
                lsr.b       #1,d6
                subx.b      d1,d7
                move        sr,d6
                jmp         (a1)
sbc_a_m_instr:
                beq.s     sbc_idx_instr
                move.b      0(a5,d5.l),d1
                move.b      d7,d2
                and.b       (a2),d6
                lsr.b       #1,d6
                subx.b      d1,d7
                move        sr,d6
                jmp         (a1)
sbc_idx_instr:
                move.b      (a6)+,d0
                ext.w       d0
                add.w       d5,d0
                move.b      0(a5,d0.l),d1
                move.b      d7,d2
                and.b       (a2),d6
                lsr.b       #1,d6
                subx.b      d1,d7
                move        sr,d6
                jmp         (a1)
sbc_a_a_instr:
                move.b      d7,d2
                move.b      d7,d1
                and.b       (a2),d6
                lsr.b       #1,d6
                subx.b      d7,d7
                move        sr,d6
                jmp         (a1)
and_b_instr:
                move.w      d4,(a4)
                and.b       (a4),d7
                move.b    2(a2,d7.w),d6
                jmp         (a1)
and_c_instr:
                and.b       d4,d7
                move.b    2(a2,d7.w),d6
                jmp         (a1)
and_d_instr:
                move.w      d3,(a4)
                and.b       (a4),d7
                move.b    2(a2,d7.w),d6
                jmp         (a1)
and_e_instr:
                and.b       d3,d7
                move.b    2(a2,d7.w),d6
                jmp         (a1)
and_h_instr:
                move.w      d5,(a4)
                and.b       (a4),d7
                move.b    2(a2,d7.w),d6
                jmp         (a1)
and_l_instr:
                and.b       d5,d7
                move.b    2(a2,d7.w),d6
                jmp         (a1)
and_m_instr:
                beq.s     and_idx_instr
                and.b     0(a5,d5.l),d7
                move.b    2(a2,d7.w),d6
                jmp         (a1)
and_idx_instr:
                move.b      (a6)+,d0
                ext.w       d0
                add.w       d5,d0
                and.b     0(a5,d0.l),d7
                move.b    2(a2,d7.w),d6
                jmp         (a1)
and_a_instr:
                and.b       d7,d7
                move.b    2(a2,d7.w),d6
                jmp         (a1)
xor_b_instr:
                move.w      d4,(a4)
                move.b      (a4),d0
                eor.b       d0,d7
                move.b    2(a2,d7.w),d6
                jmp         (a1)
xor_c_instr:
                eor.b       d4,d7
                move.b    2(a2,d7.w),d6
                jmp         (a1)
xor_d_instr:
                move.w      d3,(a4)
                move.b      (a4),d0
                eor.b       d0,d7
                move.b    2(a2,d7.w),d6
                jmp         (a1)
xor_e_instr:
                eor.b       d3,d7
                move.b    2(a2,d7.w),d6
                jmp         (a1)
xor_h_instr:
                move.w      d5,(a4)
                move.b      (a4),d0
                eor.b       d0,d7
                move.b    2(a2,d7.w),d6
                jmp         (a1)
xor_l_instr:
                eor.b       d5,d7
                move.b    2(a2,d7.w),d6
                jmp         (a1)
xor_m_instr:
                beq.s     xor_idx_instr
                move.b      0(a5,d5.l),d0
                eor.b       d0,d7
                move.b    2(a2,d7.w),d6
                jmp         (a1)
xor_idx_instr:
                move.b      (a6)+,d0
                ext.w       d0
                add.w       d5,d0
                move.b      0(a5,d0.l),d0
                eor.b       d0,d7
                move.b    2(a2,d7.w),d6
                jmp         (a1)
xor_a_instr:
                eor.b       d7,d7
                move.b    2(a2,d7.w),d6
                jmp         (a1)
or_b_instr:
                move.w      d4,(a4)
                or.b      (a4),d7
                move.b    2(a2,d7.w),d6
                jmp         (a1)
or_c_instr:
                or.b      d4,d7
                move.b    2(a2,d7.w),d6
                jmp         (a1)
or_d_instr:
                move.w      d3,(a4)
                or.b      (a4),d7
                move.b    2(a2,d7.w),d6
                jmp         (a1)
or_e_instr:
                or.b      d3,d7
                move.b    2(a2,d7.w),d6
                jmp         (a1)
or_h_instr:
                move.w      d5,(a4)
                or.b      (a4),d7
                move.b    2(a2,d7.w),d6
                jmp         (a1)
or_l_instr:
                or.b      d5,d7
                move.b    2(a2,d7.w),d6
                jmp         (a1)
or_m_instr:
                beq.s     or_idx_instr
                or.b      0(a5,d5.l),d7
                move.b    2(a2,d7.w),d6
                jmp         (a1)
or_idx_instr:
                move.b      (a6)+,d0
                ext.w       d0
                add.w       d5,d0
                or.b      0(a5,d0.l),d7
                move.b    2(a2,d7.w),d6
                jmp         (a1)
or_a_instr:
                or.b      d7,d7
                move.b    2(a2,d7.w),d6
                jmp         (a1)
cp_b_instr:
                move.w      d4,(a4)
                cmp.b       (a4),d7
                move        sr,d6
                jmp         (a1)
cp_c_instr:
                cmp.b       d4,d7
                move        sr,d6
                jmp         (a1)
cp_d_instr:
                move.w      d3,(a4)
                cmp.b       (a4),d7
                move        sr,d6
                jmp         (a1)
cp_e_instr:
                cmp.b       d3,d7
                move        sr,d6
                jmp         (a1)
cp_h_instr:
                move.w      d5,(a4)
                cmp.b       (a4),d7
                move        sr,d6
                jmp         (a1)
cp_l_instr:
                cmp.b       d5,d7
                move        sr,d6
                jmp         (a1)
cp_m_instr:
                beq.s       cp_idx_instr
                cmp.b     0(a5,d5.l),d7
                move        sr,d6
                jmp         (a1)
cp_idx_instr:
                move.b      (a6)+,d0
                ext.w       d0
                add.w       d5,d0
                cmp.b     0(a5,d0.l),d7
                move        sr,d6
                jmp         (a1)
cp_a_instr:
                cmp.b       d7,d7
                move        sr,d6
                jmp         (a1)
pop_bc_instr:
                move.b      (a3)+,-(a4)
                move.b      (a3)+,-(a4)
                move.w      (a4)+,d4
                jmp         (a1)
pop_de_instr:
                move.b      (a3)+,-(a4)
                move.b      (a3)+,-(a4)
                move.w      (a4)+,d3
                jmp         (a1)
pop_hl_instr:
                move.b      (a3)+,-(a4)
                move.b      (a3)+,-(a4)
                move.w      (a4)+,d5
                jmp         (a1)
pop_af_instr:
                move.b      (a3)+,d0
                lsr.b       #1,d0
                roxr.b      #1,d6
                lsr.b       #2,d0
                roxr.b      #1,d6
                lsr.b       #4,d0
                roxr.b      #1,d6
                lsr.b       #1,d0
                roxr.b      #5,d6
                move.b      (a3)+,d7
                jmp         (a1)
jp_nz_instr:
                move        d6,ccr
                bne.s       jp_instr
                addq.l      #2,a6
                jmp         (a1)
jp_z_instr:
                move        d6,ccr
                beq.s       jp_instr
                addq.l      #2,a6
                jmp         (a1)
jp_nc_instr:
                move        d6,ccr
                bcc.s       jp_instr
                addq.l      #2,a6
                jmp         (a1)
jp_c_instr:
                move        d6,ccr
                bcs.s       jp_instr
                addq.l      #2,a6
                jmp         (a1)
jp_po_instr:
                move        d6,ccr
                bvc.s       jp_instr
                addq.l      #2,a6
                jmp         (a1)
jp_pe_instr:
                move        d6,ccr
                bvs.s       jp_instr
                addq.l      #2,a6
                jmp         (a1)
jp_p_instr:
                move        d6,ccr
                bpl.s       jp_instr
skip_and_return:
                addq.l      #2,a6
                jmp         (a1)
jp_m_instr:
                move        d6,ccr
                bpl.s     skip_and_return
jp_instr:
                move.b      (a6)+,-(a4)
                move.b      (a6),-(a4)
                move.w      (a4)+,d0
                lea         0(a5,d0.l),a6
                jmp         (a1)
; call nz,nn
call_nz_instr:
                move        d6,ccr
                beq.s       drop_instr
;; call nn
call_instr:
                move.b      (a6)+,-(a4)
                move.b      (a6)+,-(a4)
                move.w      (a4)+,d0
                suba.l      a5,a6
                move.w      a6,-(a4)
                move.b      (a4)+,-(a3)
                move.b      (a4)+,-(a3)
                lea         0(a5,d0.l),a6
                jmp         (a1)
call_z_instr:
                move        d6,ccr
                beq.s       call_instr
drop_instr:
                addq.l      #2,a6
                jmp         (a1)
call_nc_instr:
                move        d6,ccr
                bcc.s       call_instr
                addq.l      #2,a6
                jmp         (a1)
call_c_instr:
                move        d6,ccr
                bcs.s       call_instr
                addq.l      #2,a6
                jmp         (a1)
call_po_instr:
                move        d6,ccr
                bvc.s       call_instr
                addq.l      #2,a6
                jmp         (a1)
call_pe_instr:
                move        d6,ccr
                bvs.s       call_instr
                addq.l      #2,a6
                jmp         (a1)
call_p_instr:
                move        d6,ccr
                bpl.s       call_instr
                addq.l      #2,a6
                jmp         (a1)
call_m_instr:
                move        d6,ccr
                bmi       call_instr
                addq.l      #2,a6
                jmp         (a1)
push_bc_instr:
                move.w      d4,(a4)
                move.b      (a4),-(a3)
                move.b      d4,-(a3)
                jmp         (a1)
push_de_instr:
                move.w      d3,(a4)
                move.b      (a4),-(a3)
                move.b      d3,-(a3)
                jmp         (a1)
push_hl_instr:
                move.w      d5,(a4)
                move.b      (a4),-(a3)
                move.b      d5,-(a3)
                jmp         (a1)
push_af_instr:
                move.b      d7,-(a3)
                andi.w      #$f,d6
                move.b      -16(a2,d6.w),-(a3)
                jmp         (a1)
add_a_imm_instr:
                move.b      d7,d1
                move.b      (a6)+,d2
                add.b       d2,d7
                move        sr,d6
                jmp         (a1)
rst_00_instr:
                clr.l       d0
                bra.s       do_restart
rst_08_instr:
                moveq.l     #$8,d0
                bra.s       do_restart
rst_10_instr:
                moveq.l     #$10,d0
                bra.s       do_restart
rst_18_instr:
                moveq.l     #$18,d0
                bra.s       do_restart
rst_20_instr:
                moveq.l     #$20,d0
                bra.s       do_restart
rst_28_instr:
                moveq.l     #$28,d0
                bra.s       do_restart
rst_30_instr:
                moveq.l     #$30,d0
                bra.s       do_restart
rst_38_instr:
                moveq.l     #$38,d0
do_restart:
                move.w      d0,-(sp)
                move.l      a6,d0
                sub.l       a5,d0
                move.w      d0,(a4)
                move.b      (a4),-(a3)
                move.b      d0,-(a3)
                move.w      (sp)+,d0
                lea         0(a5,d0.w),a6
                jmp         (a1)
ret_nz_instr:
                move        d6,ccr
                bne.s       ret_instr
                jmp         (a1)
ret_z_instr:
                move        d6,ccr
                beq.s       ret_instr
                jmp         (a1)
ret_nc_instr:
                move        d6,ccr
                bcc.s       ret_instr
                jmp         (a1)
ret_c_instr:
                move        d6,ccr
                bcs.s       ret_instr
                jmp         (a1)
ret_po_instr:
                move        d6,ccr
                bvc.s       ret_instr
                jmp         (a1)
ret_pe_instr:
                move        d6,ccr
                bvs.s       ret_instr
                jmp         (a1)
ret_p_instr:
                move        d6,ccr
                bpl.s       ret_instr
                jmp         (a1)
ret_m_instr:
                move        d6,ccr
                bpl.s     no_ret
ret_instr:
                move.b      (a3)+,-(a4)
                move.b      (a3)+,-(a4)
                move.w      (a4)+,d0
                lea         0(a5,d0.l),a6
no_ret:
                jmp         (a1)
cb_prefix_instr:
                beq.s       cb_group_idx
                moveq.l     #$0,d0
                move.b      (a6)+,d0
                add.w       d0,d0
                lea         dispatch_cb_group(pc),a0
                adda.w      0(a0,d0.w),a0
                move.w      d5,d0
                jmp         (a0)
cb_group_idx:
                moveq.l     #$0,d0
                move.b      (a6)+,d0
                ext.w       d0
                add.w       d5,d0
                moveq.l     #$0,d1
                move.b      (a6)+,d1
                add.w       d1,d1
                lea         dispatch_cb_group(pc),a0
                adda.w      0(a0,d1.w),a0
                jmp         (a0)
adc_imm_instr:
                move.b      (a6)+,d2
                move.b      d7,d1
                and.b       (a2),d6
                lsr.b       #1,d6
                addx.b      d2,d7
                move        sr,d6
                jmp         (a1)
out_imm_instr:
                move.w      d3,reg_de(a4)
                move.b      (a6)+,d0
                clr.l       d1
                jsr         -$1000(a5)
                clr.l       d3
                move.w      reg_de(a4),d3
                jmp         (a1)
sub_imm_instr:
                move.b      d7,d2
                move.b      (a6)+,d1
                sub.b       d1,d7
                move        sr,d6
                jmp         (a1)
exx_instr:
                move.w      d4,d0
                move.w      reg_bc_dash(a4),d4
                move.w      d0,reg_bc_dash(a4)
                move.w      d3,d0
                move.w      reg_de_dash(a4),d3
                move.w      d0,reg_de_dash(a4)
                move.w      d5,d0
                move.w      reg_hl_dash(a4),d5
                move.w      d0,reg_hl_dash(a4)
                jmp         (a1)
in_imm_instr:
                move.w      d3,reg_de(a4)
                move.b      (a6)+,d0
                moveq.l     #$1,d1
                jsr         -$1000(a5)
                clr.l       d3
                move.w      reg_de(a4),d3
                jmp         (a1)
sbc_imm_instr:
                move.b      (a6)+,d1
                move.b      d7,d2
                and.b       (a2),d6
                lsr.b       #1,d6
                subx.b      d1,d7
                move        sr,d6
                jmp         (a1)
ex_ind_sp_hl_instr:
                move.b      (a3)+,-(a4)
                move.b      (a3)+,-(a4)
                move.w      (a4)+,d0
                move.w      d5,-(a4)
                move.b      (a4)+,-(a3)
                move.b      (a4)+,-(a3)
                move.w      d0,d5
                jmp         (a1)
and_imm_instr:
                and.b       (a6)+,d7
                move.b      2(a2,d7.w),d6
                jmp         (a1)
jp_hl_instr:
                lea         0(a5,d5.l),a6
                jmp         (a1)
ex_de_hl_instr:
                exg         d3,d5
                jmp         (a1)
ed_group_instr:
                clr.l       d0
                move.b      (a6)+,d0
                lea         dispatch_ed_group(pc),a0
                add.w       d0,d0
                move.w      0(a0,d0.w),d0
                tst.w       d0
                beq.s       illegal
                jsr         0(a0,d0.w)
                jmp         (a1)
illegal:
                subq.w      #1,a6
                bra         not_implemented
xor_imm_instr:
                move.b      (a6)+,d0
                eor.b       d0,d7
                move.b      2(a2,d7.w),d6
                jmp         (a1)
di_instr:               
                jmp         (a1)
or_imm_instr:
                or.b        (a6)+,d7
                move.b      2(a2,d7.w),d6
                jmp         (a1)
ld_sp_hl_instr:
                lea         0(a5,d5.l),a3
                jmp         (a1)
ei_instr:
                jmp         (a1)
cp_imm_instr:
                cmp.b       (a6)+,d7
                move        sr,d6
                jmp         (a1)
in_b_c_instr:
in_c_c_instr:
in_d_c_instr:
in_e_c_instr:
in_h_c_instr:
in_l_c_instr:
in_a_c_instr:
                move.w      d4,d0
                moveq.l     #$1,d1
                move.w      d3,reg_de(a4)
                jsr         -$1000(a5)
                clr.l       d3
                move.w      reg_de(a4),d3
                rts
out_c_b_instr:
out_c_c_instr:
out_c_d_instr:
out_c_e_instr:
out_c_h_instr:
out_c_l_instr:
out_c_a_instr:
                move.w      d4,d0
                clr.l       d1
                move.w      d3,reg_de(a4)
                jsr         -$1000(a5)
                clr.l       d3
                move.w      reg_de(a4),d3
                rts
sbc_hl_bc_instr:
                and.b       (a2),d6
                lsr.b       #1,d6
                subx.w      d4,d5
                move        sr,d6
                rts
ld_ind_imm_bc_instr:
                move.b      (a6)+,-(a4)
                move.b      (a6)+,-(a4)
                move.w      (a4)+,d0
                lea         0(a5,d0.l),a0
                move.w      d4,(a4)
                move.b      d4,(a0)+
                move.b      (a4),(a0)
                rts
neg_instr:
                neg.b       d7
                move        sr,d6
                rts
adc_hl_bc_instr:
                and.b       (a2),d6
                lsr.b       #1,d6
                addx.w      d4,d5
                move        sr,d6
                rts
ld_bc_ind_imm_instr:
                move.b      (a6)+,-(a4)
                move.b      (a6)+,-(a4)
                move.w      (a4)+,d0
                lea         0(a5,d0.l),a0
                move.b      (a0)+,-(a4)
                move.b      (a0),-(a4)
                move.w      (a4)+,d4
                rts
sbc_hl_de_instr:
                and.b       (a2),d6
                lsr.b       #1,d6
                subx.w      d3,d5
                move        sr,d6
                rts
ld_ind_imm_de_instr:
                move.b      (a6)+,-(a4)
                move.b      (a6)+,-(a4)
                move.w      (a4)+,d0
                lea         0(a5,d0.l),a0
                move.w      d3,(a4)
                move.b      d3,(a0)+
                move.b      (a4),(a0)
                rts
adc_hl_de_instr:
                and.b       (a2),d6
                lsr.b       #1,d6
                addx.w      d3,d5
                move        sr,d6
                rts
ld_de_ind_imm_instr:
                move.b      (a6)+,-(a4)
                move.b      (a6)+,-(a4)
                move.w      (a4)+,d0
                lea         0(a5,d0.l),a0
                move.b      (a0)+,-(a4)
                move.b      (a0)+,-(a4)
                move.w      (a4)+,d3
                rts
ld_a_i_instr:
ld_a_r_instr:
                not.b       d6
                rts
sbc_hl_hl_instr:
                and.b       (a2),d6
                lsr.b       #1,d6
                subx.w      d5,d5
                move        sr,d6
                rts
ld_ind_imm_hl_instr:
                move.b      (a6)+,-(a4)
                move.b      (a6)+,-(a4)
                move.w      (a4)+,d0
                lea         0(a5,d0.l),a0
                move.w      d5,(a4)
                move.b      d5,(a0)+
                move.b      (a4),(a0)
                rts

; this looks like a nibble flip. But must be something to do with flags (d6) and A (d7)
rrd_instr:
                move.b      d7,d0                    
                andi.b      #$f,d0                             ; Lower 4 bits of A
                lsl.b       #4,d0                              ; Now in upper 4 bits
                andi.b      #$f0,d7                            ; this has upper 4 bits of A
                lea         0(a5,d5.l),a0                      ; Absolute address of HL
                move.b      (a0),d1                            ; Get contents of (hl)
                move.b      d1,d2                              ; copy of memory
                lsr.b       #4,d1                              ; lower 4 bits
                or.b        d0,d1
                move.b      d1,(a0)                            ; Store back in (hl)
                andi.b      #$f,d2
                or.b        d2,d7
                and.b       (a2),d6                            ; Clear some flag bits, others are sticky.
                or.b        2(a2,d7.w),d6                      ; Set flags depending on value of A
                rts
adc_hl_hl_instr:
                and.b       (a2),d6
                lsr.b       #1,d6
                addx.w      d5,d5
                move        sr,d6
                rts
ld_hl_ind_imm_instr:
                move.b      (a6)+,-(a4)
                move.b      (a6)+,-(a4)
                move.w      (a4)+,d0
                lea         0(a5,d0.l),a0
                move.b      (a0)+,-(a4)
                move.b      (a0),-(a4)
                move.w      (a4)+,d5
                rts
rld_instr:
                move.b      d7,d0
                andi.b      #$f,d0
                andi.b      #$f0,d7
                lea         0(a5,d5.l),a0
                move.b      (a0),d1
                move.b      d1,d2
                lsl.b       #4,d1
                or.b        d0,d1
                move.b      d1,(a0)
                lsr.b       #4,d2
                or.b        d2,d7
                and.b       (a2),d6
                or.b        2(a2,d7.w),d6
                rts
sbc_hl_sp_instr:
                move.l      a3,d0
                sub.l       a5,d0
                and.b       (a2),d6
                lsr.b       #1,d6
                subx.w      d0,d5
                move        sr,d6
                rts
ld_ind_imm_sp_instr:
                move.b      (a6)+,-(a4)
                move.b      (a6)+,-(a4)
                move.w      (a4)+,d0
                lea         0(a5,d0.l),a0
                move.l      a3,d0
                sub.l       a5,d0
                move.b      d0,(a0)+
                ror.w       #8,d0
                move.b      d0,(a0)
                rts
adc_hl_sp_instr:
                move.l       a3,d0
                sub.l        a5,d0
                and.b        (a2),d6
                lsr.b        #1,d6
                addx.w       d0,d5
                move         sr,d6
                rts
ld_sp_ind_imm_instr:
                move.b       (a6)+,-(a4)
                move.b       (a6)+,-(a4)
                move.w       (a4)+,d0
                lea          0(a5,d0.l),a0
                move.b       (a0)+,-(a4)
                move.b       (a0),-(a4)
                move.w       (a4)+,d0
                lea          0(a5,d0.l),a3
                rts
ldi_instr:
                bclr         #1,d6
                move.b       0(a5,d5.l),0(a5,d3.l)
                addq.w       #1,d3
                addq.w       #1,d5
                subq.w       #1,d4
                beq.s        ldi_done
                bset         #1,d6
ldi_done:      rts
cpi_instr:
                cmp.b       0(a5,d5.l),d7
                move        sr,d0
                and.b       (a2),d6
                andi.b      #$0c,d0
                or.b        d0,d6
                addq.w      #1,d5
                subq.w      #1,d4
                beq.s       cpi_done
                bset        #1,d6
cpi_done:       rts
ini_instr:
                moveq.l     #$1,d1
ioid_common:   move.b       d4,d0
                move.w      d3,reg_de(a4)
                jsr         -$1000(a5)
                clr.l       d3
                move.w      reg_de(a4),d3
                sub.w       #$100,d4
                addq.w      #1,d5
                rts
outi_instr:     moveq.l     #$0,d1
                bra.w       ioid_common
ldd_instr:
                bclr        #1,d6
                move.b      0(a5,d5.l),0(a5,d3.l)
                subq.w      #1,d3
                subq.w      #1,d5
                subq.w      #1,d4
                beq.s       ldd_done
                bset        #1,d6
ldd_done:       rts
cpd_instr:
                cmp.b     0(a5,d5.l),d7
                move        sr,d0
                and.b       (a2),d6
                andi.b      #$c,d0
                or.b      d0,d6
                subq.w      #1,d5
                subq.w      #1,d4
                beq.s       cpd_done
                bset      #1,d6
cpd_done:       rts
ind_instr:
                moveq.l     #$1,d1
inout_d:       move.b      d4,d0
                move.w      d3,reg_de(a4)
                jsr         -$1000(a5)
                clr.l       d3
                move.w      reg_de(a4),d3
                sub.w     #$100,d4
                subq.w      #1,d5
                rts
outd_instr:
                moveq.l     #$0,d1
                bra.w     inout_d
ldir_instr:
                lea         0(a5,d5.l),a0
                lea         0(a5,d3.l),a1
                add.w       d4,d3
                add.w       d4,d5
ldir_next:     move.b      (a0)+,(a1)+
                subq.w      #1,d4
                bne.s     ldir_next
                bclr      #1,d6
                clr.l       d4
                lea       z80_exec_loop(pc),a1
                rts
cpir_instr:
                bsr.w     cpi_instr
                move        d6,ccr
                beq.s       cpir_done
                btst      #1,d6
                bne.s       cpir_instr
cpir_done:     rts
inir_instr:
                bsr.w     ini_instr
                move.w      d4,(a4)
                tst.b       (a4)
                bne.s     inir_instr
                rts
otir_instr:
                bsr.w     outi_instr
                move.w      d4,(a4)
                tst.b       (a4)
                bne.s     otir_instr
                rts
lddr_instr:
                lea       1(a5,d5.l),a0
                lea       1(a5,d3.l),a1
                sub.w       d4,d3
                sub.w       d4,d5
lddr_next:
                move.b      -(a0),-(a1)
                subq.w      #1,d4
                bne.w     lddr_next
                bclr      #1,d6
                clr.l       d4
                lea       z80_exec_loop(pc),a1
                rts
cpdr_instr:
                bsr.w     cpd_instr
                move        d6,ccr
                beq.s       cpdr_done
                btst      #1,d6
                bne.s       cpdr_instr
cpdr_done:     rts
indr_instr:    bsr.w     ind_instr
                move.w      d4,(a4)
                tst.b       (a4)
                bne.s     indr_instr
                rts
otdr_instr:
                bsr.w     outd_instr
                move.w      d4,(a4)
                tst.b       (a4)
                bne.s     otdr_instr
                rts
rlc_b_instr:
                move.w      d4,(a4)
                move.b      (a4),d0
                rol.b       #1,d0
                move        sr,d6
                move.b      d0,(a4)
                move.w      (a4),d4
                jmp         (a1)
rlc_c_instr:
                rol.b       #1,d4
                move        sr,d6
                jmp         (a1)
rlc_d_instr:
                move.w      d3,(a4)
                move.b      (a4),d0
                rol.b       #1,d0
                move        sr,d6
                move.b      d0,(a4)
                move.w      (a4),d3
                jmp         (a1)
rlc_e_instr:
                rol.b       #1,d3
                move        sr,d6
                jmp         (a1)
rlc_h_instr:
                move.w      d5,(a4)
                move.b      (a4),d0
                rol.b       #1,d0
                move        sr,d6
                move.b      d0,(a4)
                move.w      (a4),d5
                jmp         (a1)
rlc_l_instr:
                rol.b       #1,d5
                move        sr,d6
                jmp         (a1)
rlc_m_instr:
                move.b      0(a5,d0.l),d1
                rol.b       #1,d1
                move        sr,d6
                move.b      d1,0(a5,d0.l)
                jmp         (a1)
rlc_a_instr:
                rol.b       #1,d7
                move        sr,d6
                jmp         (a1)
rrc_b_instr:
                move.w      d4,(a4)
                move.b      (a4),d0
                ror.b       #1,d0
                move        sr,d6
                move.b      d0,(a4)
                move.w      (a4),d4
                jmp         (a1)
rrc_c_instr:
                ror.b       #1,d4
                move        sr,d6
                jmp         (a1)
rrc_d_instr:
                move.w      d3,(a4)
                move.b      (a4),d0
                ror.b       #1,d0
                move        sr,d6
                move.b      d0,(a4)
                move.w      (a4),d3
                jmp         (a1)
rrc_e_instr:
                ror.b       #1,d3
                move        sr,d6
                jmp         (a1)
rrc_h_instr:
                move.w      d5,(a4)
                move.b      (a4),d0
                ror.b       #1,d0
                move        sr,d6
                move.b      d0,(a4)
                move.w      (a4),d5
                jmp         (a1)
rrc_l_instr:
                ror.b       #1,d5
                move        sr,d6
                jmp         (a1)
rrc_m_instr:
                move.b      0(a5,d0.l),d1
                ror.b       #1,d1
                move        sr,d6
                move.b      d1,0(a5,d0.l)
                jmp         (a1)
rrc_a_instr:
                ror.b       #1,d7
                move        sr,d6
                jmp         (a1)
rl_b_instr:
                move.w      d4,(a4)
                move.b      (a4),d0
                lsr.b       #1,d6
                roxl.b      #1,d0
                move        sr,d6
                move.b      d0,(a4)
                move.w      (a4),d4
                jmp         (a1)
rl_c_instr:
                lsr.b       #1,d6
                roxl.b      #1,d4
                move        sr,d6
                jmp         (a1)
rl_d_instr:
                move.w      d3,(a4)
                move.b      (a4),d0
                lsr.b       #1,d6
                roxl.b      #1,d0
                move        sr,d6
                move.b      d0,(a4)
                move.w      (a4),d3
                jmp         (a1)
rl_e_instr:
                lsr.b       #1,d6
                roxl.b      #1,d3
                move        sr,d6
                jmp         (a1)
rl_h_instr:
                move.w      d5,(a4)
                move.b      (a4),d0
                lsr.b       #1,d6
                roxl.b      #1,d0
                move        sr,d6
                move.b      d0,(a4)
                move.w      (a4),d5
                jmp         (a1)
rl_l_instr:
                lsr.b       #1,d6
                roxl.b      #1,d5
                move        sr,d6
                jmp         (a1)
rl_m_instr:
                move.b      0(a5,d0.l),d1
                lsr.b       #1,d6
                roxl.b      #1,d1
                move        sr,d6
                move.b      d1,0(a5,d0.l)
                jmp         (a1)
rl_a_instr:
                lsr.b       #1,d6
                roxl.b      #1,d7
                move        sr,d6
                jmp         (a1)
rr_b_instr:
                move.w      d4,(a4)
                move.b      (a4),d0
                lsr.b       #1,d6
                roxr.b      #1,d0
                move        sr,d6
                move.b      d0,(a4)
                move.w      (a4),d4
                jmp         (a1)
rr_c_instr:
                lsr.b       #1,d6
                roxr.b      #1,d4
                move        sr,d6
                jmp         (a1)
rr_d_instr:
                move.w      d3,(a4)
                move.b      (a4),d0
                lsr.b       #1,d6
                roxr.b      #1,d0
                move        sr,d6
                move.b      d0,(a4)
                move.w      (a4),d3
                jmp         (a1)
rr_e_instr:
                lsr.b       #1,d6
                roxr.b      #1,d3
                move        sr,d6
                jmp         (a1)
rr_h_instr:
                move.w      d5,(a4)
                move.b      (a4),d0
                lsr.b       #1,d6
                roxr.b      #1,d0
                move        sr,d6
                move.b      d0,(a4)
                move.w      (a4),d5
                jmp         (a1)
rr_l_instr:
                lsr.b       #1,d6
                roxr.b      #1,d5
                move        sr,d6
                jmp         (a1)
rr_m_instr:
                move.b      0(a5,d0.l),d1
                lsr.b       #1,d6
                roxr.b      #1,d1
                move        sr,d6
                move.b      d1,0(a5,d0.l)
                jmp         (a1)
rr_a_instr:
                lsr.b       #1,d6
                roxr.b      #1,d7
                move        sr,d6
                jmp         (a1)
sll_b_instr:
                move.w      d4,(a4)
                move.b      (a4),d0
                lsl.b       #1,d0
                move        sr,d6
                move.b      d0,(a4)
                move.w      (a4),d4
                jmp         (a1)
sll_c_instr:
                lsl.b       #1,d4
                move        sr,d6
                jmp         (a1)
sll_d_instr:
                move.w      d3,(a4)
                move.b      (a4),d0
                lsl.b       #1,d0
                move        sr,d6
                move.b      d0,(a4)
                move.w      (a4),d3
                jmp         (a1)
sll_e_instr:
                lsl.b       #1,d3
                move        sr,d6
                jmp         (a1)
sll_h_instr:
                move.w      d5,(a4)
                move.b      (a4),d0
                lsl.b       #1,d0
                move        sr,d6
                move.b      d0,(a4)
                move.w      (a4),d5
                jmp         (a1)
sll_l_instr:
                lsl.b       #1,d5
                move        sr,d6
                jmp         (a1)
sll_m_instr:
                move.b      0(a5,d0.l),d1
                lsl.b       #1,d1
                move        sr,d6
                move.b      d1,0(a5,d0.l)
                jmp         (a1)
sll_a_instr:
                lsl.b       #1,d7
                move        sr,d6
                jmp         (a1)
sra_b_instr:
                move.w      d4,(a4)
                move.b      (a4),d0
                asr.b       #1,d0
                move        sr,d6
                move.b      d0,(a4)
                move.w      (a4),d4
                jmp         (a1)
sra_c_instr:
                asr.b       #1,d4
                move        sr,d6
                jmp         (a1)
sra_d_instr:
                move.w      d3,(a4)
                move.b      (a4),d0
                asr.b       #1,d0
                move        sr,d6
                move.b      d0,(a4)
                move.w      (a4),d3
                jmp         (a1)
sra_e_instr:
                asr.b       #1,d3
                move        sr,d6
                jmp         (a1)
sra_h_instr:
                move.w      d5,(a4)
                move.b      (a4),d0
                asr.b       #1,d0
                move        sr,d6
                move.b      d0,(a4)
                move.w      (a4),d5
                jmp         (a1)
sra_l_instr:
                asr.b       #1,d5
                move        sr,d6
                jmp         (a1)
;; Unknown, what would d0 be, Ah. An offset
sra_m_instr:
                move.b      0(a5,d0.l),d1
                asr.b       #1,d1
                move        sr,d6
                move.b      d1,0(a5,d0.l)
                jmp         (a1)
sra_a_instr:
                asr.b       #1,d7
                move        sr,d6
                jmp         (a1)
sla_b_instr:
                move.w      d4,(a4)
                move.b      (a4),d0
                asl.b       #1,d0
                move        sr,d6
                move.b      d0,(a4)
                move.w      (a4),d4
                jmp         (a1)
sla_c_instr:
                asl.b       #1,d4
                move        sr,d6
                jmp         (a1)
sla_d_instr:
                move.w      d3,(a4)
                move.b      (a4),d0
                asl.b       #1,d0
                move        sr,d6
                move.b      d0,(a4)
                move.w      (a4),d3
                jmp         (a1)
sla_e_instr:
                asl.b       #1,d3
                move        sr,d6
                jmp         (a1)
sla_h_instr:
                move.w      d5,(a4)
                move.b      (a4),d0
                asl.b       #1,d0
                move        sr,d6
                move.b      d0,(a4)
                move.w      (a4),d5
                jmp         (a1)
sla_l_instr
                asl.b       #1,d5
                move        sr,d6
                jmp         (a1)
sla_m_instr:
                move.b      0(a5,d0.l),d1
                asl.b       #1,d1
                move        sr,d6
                move.b      d1,0(a5,d0.l)
                jmp         (a1)
sla_a_instr:
                asl.b       #1,d7
                move        sr,d6
                jmp         (a1)
srl_b_instr:
                move.w      d4,(a4)
                move.b      (a4),d0
                lsr.b       #1,d0
                move        sr,d6
                move.b      d0,(a4)
                move.w      (a4),d4
                jmp         (a1)
srl_c_instr:
                lsr.b       #1,d4
                move        sr,d6
                jmp         (a1)
srl_d_instr:
                move.w      d3,(a4)
                move.b      (a4),d0
                lsr.b       #1,d0
                move        sr,d6
                move.b      d0,(a4)
                move.w      (a4),d3
                jmp         (a1)
srl_e_instr:
                lsr.b       #1,d3
                move        sr,d6
                jmp         (a1)
srl_h_instr:
                move.w      d5,(a4)
                move.b      (a4),d0
                lsr.b       #1,d0
                move        sr,d6
                move.b      d0,(a4)
                move.w      (a4),d5
                jmp         (a1)
srl_l_instr:
                lsr.b       #1,d5
                move        sr,d6
                jmp         (a1)
srl_m_instr:
                move.b      0(a5,d0.l),d1
                lsr.b       #1,d1
                move        sr,d6
                move.b      d1,0(a5,d0.l)
                jmp         (a1)
srl_a_instr:
                lsr.b       #1,d7
                move        sr,d6
                jmp         (a1)
bit_0_b_instr:
                btst      #8,d4
                move        sr,d6
                jmp         (a1)
bit_0_c_instr:
                btst      #0,d4
                move        sr,d6
                jmp         (a1)
bit_0_d_instr:
                btst      #8,d3
                move        sr,d6
                jmp         (a1)
bit_0_e_instr:
                btst      #0,d3
                move        sr,d6
                jmp         (a1)
bit_0_h_instr:
                btst      #8,d5
                move        sr,d6
                jmp         (a1)
bit_0_l_instr:
                btst      #0,d5
                move        sr,d6
                jmp         (a1)
bit_0_m_instr:
                btst      #0,0(a5,d0.l)
                move        sr,d6
                jmp         (a1)
bit_0_a_instr:
                btst      #0,d7
                move        sr,d6
                jmp         (a1)
bit_1_b_instr:               
                btst      #9,d4
                move        sr,d6
                jmp         (a1)
bit_1_c_instr:
                btst      #1,d4
                move        sr,d6
                jmp         (a1)
bit_1_d_instr:
                btst      #9,d3
                move        sr,d6
                jmp         (a1)
bit_1_e_instr:
                btst      #1,d3
                move        sr,d6
                jmp         (a1)
bit_1_h_instr:
                btst      #9,d5
                move        sr,d6
                jmp         (a1)
bit_1_l_instr:
                btst      #1,d5
                move        sr,d6
                jmp         (a1)
bit_1_m_instr:
                btst      #1,0(a5,d0.l)
                move        sr,d6
                jmp         (a1)
bit_1_a_instr:
                btst      #1,d7
                move        sr,d6
                jmp         (a1)
bit_2_b_instr:
                btst      #10,d4
                move        sr,d6
                jmp         (a1)
bit_2_c_instr:
                btst      #2,d4
                move        sr,d6
                jmp         (a1)
bit_2_d_instr:
                btst      #10,d3
                move        sr,d6
                jmp         (a1)
bit_2_e_instr:
                btst      #2,d3
                move        sr,d6
                jmp         (a1)
bit_2_h_instr:
                btst      #10,d5
                move        sr,d6
                jmp         (a1)
bit_2_l_instr:
                btst      #2,d5
                move        sr,d6
                jmp         (a1)
bit_2_m_instr:
                btst      #2,0(a5,d0.l)
                move        sr,d6
                jmp         (a1)
bit_2_a_instr:
                btst      #2,d7
                move        sr,d6
                jmp         (a1)
bit_3_b_instr:
                btst      #11,d4
                move        sr,d6
                jmp         (a1)
bit_3_c_instr:
                btst      #3,d4
                move        sr,d6
                jmp         (a1)
bit_3_d_instr:
                btst      #11,d3
                move        sr,d6
                jmp         (a1)
bit_3_e_instr:
                btst      #3,d3
                move        sr,d6
                jmp         (a1)
bit_3_h_instr:
                btst      #11,d5
                move        sr,d6
                jmp         (a1)
bit_3_l_instr:
                btst      #3,d5
                move        sr,d6
                jmp         (a1)
bit_3_m_instr:
                btst      #3,0(a5,d0.l)
                move        sr,d6
                jmp         (a1)
bit_3_a_instr:
                btst      #3,d7
                move        sr,d6
                jmp         (a1)
bit_4_b_instr:
                btst      #12,d4
                move        sr,d6
                jmp         (a1)
bit_4_c_instr:
                btst      #4,d4
                move        sr,d6
                jmp         (a1)
bit_4_d_instr:
                btst      #12,d3
                move        sr,d6
                jmp         (a1)
bit_4_e_instr:
                btst      #4,d3
                move        sr,d6
                jmp         (a1)
bit_4_h_instr:
                btst      #12,d5
                move        sr,d6
                jmp         (a1)
bit_4_l_instr:
                btst      #4,d5
                move        sr,d6
                jmp         (a1)
bit_4_m_instr:
                btst      #4,0(a5,d0.l)
                move        sr,d6
                jmp         (a1)
bit_4_a_instr:
                btst      #4,d7
                move        sr,d6
                jmp         (a1)
bit_5_b_instr:
                btst      #13,d4
                move        sr,d6
                jmp         (a1)
bit_5_c_instr:
                btst      #5,d4
                move        sr,d6
                jmp         (a1)
bit_5_d_instr:
                btst      #13,d3
                move        sr,d6
                jmp         (a1)
bit_5_e_instr:
                btst      #5,d3
                move        sr,d6
                jmp         (a1)
bit_5_h_instr:
                btst      #13,d5
                move        sr,d6
                jmp         (a1)
bit_5_l_instr:
                btst      #5,d5
                move        sr,d6
                jmp         (a1)
bit_5_m_instr:
                btst      #5,0(a5,d0.l)
                move        sr,d6
                jmp         (a1)
bit_5_a_instr:
                btst      #5,d7
                move        sr,d6
                jmp         (a1)
bit_6_b_instr:
                btst      #14,d4
                move        sr,d6
                jmp         (a1)
bit_6_c_instr:
                btst      #6,d4
                move        sr,d6
                jmp         (a1)
bit_6_d_instr:
                btst      #14,d3
                move        sr,d6
                jmp         (a1)
bit_6_e_instr:
                btst      #6,d3
                move        sr,d6
                jmp         (a1)
bit_6_h_instr:
                btst      #14,d5
                move        sr,d6
                jmp         (a1)
bit_6_l_instr:
                btst      #6,d5
                move        sr,d6
                jmp         (a1)
bit_6_m_instr:
                btst      #6,0(a5,d0.l)
                move        sr,d6
                jmp         (a1)
bit_6_a_instr:
                btst      #6,d7
                move        sr,d6
                jmp         (a1)
bit_7_b_instr:
                btst      #15,d4
                move        sr,d6
                jmp         (a1)
bit_7_c_instr:
                btst      #7,d4
                move        sr,d6
                jmp         (a1)
bit_7_d_instr:
                btst      #15,d3
                move        sr,d6
                jmp         (a1)
bit_7_e_instr:
                btst      #7,d3
                move        sr,d6
                jmp         (a1)
bit_7_h_instr:
                btst      #15,d5
                move        sr,d6
                jmp         (a1)
bit_7_l_instr:
                btst      #7,d5
                move        sr,d6
                jmp         (a1)
bit_7_m_instr:
                btst      #7,0(a5,d0.l)
                move        sr,d6
                jmp         (a1)
bit_7_a_instr:
                btst      #7,d7
                move        sr,d6
                jmp         (a1)
res_0_b_instr:
                bclr      #8,d4
                jmp         (a1)
res_0_c_instr:
                bclr      #0,d4
                jmp         (a1)
res_0_d_instr:
                bclr      #8,d3
                jmp         (a1)
res_0_e_instr:
                bclr      #0,d3
                jmp         (a1)
res_0_h_instr:
                bclr      #8,d5
                jmp         (a1)
res_0_l_instr:
                bclr      #0,d5
                jmp         (a1)
res_0_m_instr:
                bclr      #0,0(a5,d0.l)
                jmp         (a1)
res_0_a_instr:
                bclr      #0,d7
                jmp         (a1)
res_1_b_instr:
                bclr      #9,d4
                jmp         (a1)
res_1_c_instr:
                bclr      #1,d4
                jmp         (a1)
res_1_d_instr:
                bclr      #9,d3
                jmp         (a1)
res_1_e_instr:
                bclr      #1,d3
                jmp         (a1)
res_1_h_instr:
                bclr      #9,d5
                jmp         (a1)
res_1_l_instr:
                bclr      #1,d5
                jmp         (a1)
res_1_m_instr:
                bclr      #1,0(a5,d0.l)
                jmp         (a1)
res_1_a_instr:
                bclr      #1,d7
                jmp         (a1)
res_2_b_instr:
                bclr      #10,d4
                jmp         (a1)
res_2_c_instr:
                bclr      #2,d4
                jmp         (a1)
res_2_d_instr:
                bclr      #10,d3
                jmp         (a1)
res_2_e_instr:
                bclr      #2,d3
                jmp         (a1)
res_2_h_instr:
                bclr      #10,d5
                jmp         (a1)
res_2_l_instr:
                bclr      #2,d5
                jmp         (a1)
res_2_m_instr:
                bclr      #2,0(a5,d0.l)
                jmp         (a1)
res_2_a_instr:
                bclr      #2,d7
                jmp         (a1)
res_3_b_instr:
                bclr      #11,d4
                jmp         (a1)
res_3_c_instr:
                bclr      #3,d4
                jmp         (a1)
res_3_d_instr:
                bclr      #11,d3
                jmp         (a1)
res_3_e_instr:
                bclr      #3,d3
                jmp         (a1)
res_3_h_instr:
                bclr      #11,d5
                jmp         (a1)
res_3_l_instr:
                bclr      #3,d5
                jmp         (a1)
res_3_m_instr:
                bclr      #3,0(a5,d0.l)
                jmp         (a1)
res_3_a_instr:
                bclr      #3,d7
                jmp         (a1)
res_4_b_instr:
                bclr      #12,d4
                jmp         (a1)
res_4_c_instr:
                bclr      #4,d4
                jmp         (a1)
res_4_d_instr:
                bclr      #12,d3
                jmp         (a1)
res_4_e_instr:
                bclr      #4,d3
                jmp         (a1)
res_4_h_instr:
                bclr      #12,d5
                jmp         (a1)
res_4_l_instr:
                bclr      #4,d5
                jmp         (a1)
res_4_m_instr:
                bclr      #4,0(a5,d0.l)
                jmp         (a1)
res_4_a_instr:
                bclr      #4,d7
                jmp         (a1)
res_5_b_instr:
                bclr      #13,d4
                jmp         (a1)
res_5_c_instr:
                bclr      #5,d4
                jmp         (a1)
res_5_d_instr:
                bclr      #13,d3
                jmp         (a1)
res_5_e_instr:
                bclr      #5,d3
                jmp         (a1)
res_5_h_instr:
                bclr      #13,d5
                jmp         (a1)
res_5_l_instr:
                bclr      #5,d5
                jmp         (a1)
res_5_m_instr:
                bclr      #5,0(a5,d0.l)
                jmp         (a1)
res_5_a_instr:
                bclr      #5,d7
                jmp         (a1)
res_6_b_instr:
                bclr      #14,d4
                jmp         (a1)
res_6_c_instr:
                bclr      #6,d4
                jmp         (a1)
res_6_d_instr:
                bclr      #14,d3
                jmp         (a1)
res_6_e_instr:
                bclr      #6,d3
                jmp         (a1)
res_6_h_instr:
                bclr      #14,d5
                jmp         (a1)
res_6_l_instr:
                bclr      #6,d5
                jmp         (a1)
res_6_m_instr:
                bclr      #6,0(a5,d0.l)
                jmp         (a1)
res_6_a_instr:
                bclr      #6,d7
                jmp         (a1)
res_7_b_instr:
                bclr      #15,d4
                jmp         (a1)
res_7_c_instr:
                bclr      #7,d4
                jmp         (a1)
res_7_d_instr:
                bclr      #15,d3
                jmp         (a1)
res_7_e_instr:
                bclr      #7,d3
                jmp         (a1)
res_7_h_instr:
                bclr      #15,d5
                jmp         (a1)
res_7_l_instr:
                bclr      #7,d5
                jmp         (a1)
res_7_m_instr:
                bclr      #7,0(a5,d0.l)
                jmp         (a1)
res_7_a_instr:
                bclr      #7,d7
                jmp         (a1)
set_0_b_instr:
                bset      #8,d4
                jmp         (a1)
set_0_c_instr:
                bset      #0,d4
                jmp         (a1)
set_0_d_instr:
                bset      #8,d3
                jmp         (a1)
set_0_e_instr:
                bset      #0,d3
                jmp         (a1)
set_0_h_instr:
                bset      #8,d5
                jmp         (a1)
set_0_l_instr:
                bset      #0,d5
                jmp         (a1)
set_0_m_instr:
                bset      #0,0(a5,d0.l)
                jmp         (a1)
set_0_a_instr:
                bset      #0,d7
                jmp         (a1)
set_1_b_instr:
                bset      #9,d4
                jmp         (a1)
set_1_c_instr:
                bset      #1,d4
                jmp         (a1)
set_1_d_instr:
                bset      #9,d3
                jmp         (a1)
set_1_e_instr:
              bset      #1,d3
                jmp         (a1)
set_1_h_instr:
                bset      #9,d5
                jmp         (a1)
set_1_l_instr:
                bset      #1,d5
                jmp         (a1)
set_1_m_instr:
                bset      #1,0(a5,d0.l)
                jmp         (a1)
set_1_a_instr:
                bset      #1,d7
                jmp         (a1)
set_2_b_instr:
                bset      #10,d4
                jmp         (a1)
set_2_c_instr:
                bset      #2,d4
                jmp         (a1)
set_2_d_instr:
                bset      #10,d3
                jmp         (a1)
set_2_e_instr:
                bset      #2,d3
                jmp         (a1)
set_2_h_instr:
                bset      #10,d5
                jmp         (a1)
set_2_l_instr:
                bset      #2,d5
                jmp         (a1)
set_2_m_instr:
                bset      #2,0(a5,d0.l)
                jmp         (a1)
set_2_a_instr:
                bset      #2,d7
                jmp         (a1)
set_3_b_instr:
                bset      #11,d4
                jmp         (a1)
set_3_c_instr:
                bset      #3,d4
                jmp         (a1)
set_3_d_instr:
                bset      #11,d3
                jmp         (a1)
set_3_e_instr:
                bset      #3,d3
                jmp         (a1)
set_3_h_instr:
                bset      #11,d5
                jmp         (a1)
set_3_l_instr:
                bset      #3,d5
                jmp         (a1)
set_3_m_instr:
                bset      #3,0(a5,d0.l)
                jmp         (a1)
set_3_a_instr:
                bset      #3,d7
                jmp         (a1)
set_4_b_instr:
                bset      #12,d4
                jmp         (a1)
set_4_c_instr:
                bset      #4,d4
                jmp         (a1)
set_4_d_instr:
                bset      #12,d3
                jmp         (a1)
set_4_e_instr:
                bset      #4,d3
                jmp         (a1)
set_4_h_instr:
                bset      #12,d5
                jmp         (a1)
set_4_l_instr:
                bset      #4,d5
                jmp         (a1)
set_4_m_instr:
                bset      #4,0(a5,d0.l)
                jmp         (a1)
set_4_a_instr:
                bset      #4,d7
                jmp         (a1)
set_5_b_instr:
                bset      #13,d4
                jmp         (a1)
set_5_c_instr:
                bset      #5,d4
                jmp         (a1)
set_5_d_instr:
                bset      #13,d3
                jmp         (a1)
set_5_e_instr:
                bset      #5,d3
                jmp         (a1)
set_5_h_instr:
                bset      #13,d5
                jmp         (a1)
set_5_l_instr:
                bset      #5,d5
                jmp         (a1)
set_5_m_instr:
                bset      #5,0(a5,d0.l)
                jmp         (a1)
set_5_a_instr:
                bset      #5,d7
                jmp         (a1)
set_6_b_instr:
                bset      #14,d4
                jmp         (a1)
set_6_c_instr:
                bset      #6,d4
                jmp         (a1)
set_6_d_instr:
                bset      #14,d3
                jmp         (a1)
set_6_e_instr:
                bset      #6,d3
                jmp         (a1)
set_6_h_instr:
                bset      #14,d5
                jmp         (a1)
set_6_l_instr:
                bset      #6,d5
                jmp         (a1)
set_6_m_instr:
                bset      #6,0(a5,d0.l)
                jmp         (a1)
set_6_a_instr:
                bset      #6,d7
                jmp         (a1)
set_7_b_instr:
                bset      #15,d4
                jmp         (a1)
set_7_c_instr:
                bset      #7,d4
                jmp         (a1)
set_7_d_instr:
                bset      #15,d3
                jmp         (a1)
set_7_e_instr:
                bset      #7,d3
                jmp         (a1)
set_7_h_instr:
                bset      #15,d5
                jmp         (a1)
set_7_l_instr:
                bset      #7,d5
                jmp         (a1)
set_7_m_instr:
                bset      #7,0(a5,d0.l)
                jmp         (a1)
set_7_a_instr:
                bset      #7,d7
                jmp         (a1)
not_implemented:
                movem.l   d0-d3/a0-a1,-(sp)
                lea       not_implemented_msg(pc),a1
                moveq.l     #IO_SSTRG,d0
                move.w      (a1)+,d2
                move.l    con_handle(a4),a0
                trap      #TRAP_STRM
                movem.l   (sp)+,d0-d3/a0-a1
                bsr.s       dump_registers
                moveq.l     #$1,d0
                clr.l       d1
                jsr         -$1000(a5)
                move.l      a5,a6
                jmp         (a1)
dump_registers:
                movem.l       d3-d7/a1-a6,-(sp)
                subq.l      #1,a6
                move.l      con_handle(a4),a0
                lea         register_dump_msg(pc),a1
                move.w      (a1)+,d2
                moveq.l     #IO_SSTRG,d0
                trap        #TRAP_STRM
                move.l      a6,d0               ;; pc
                sub.l       a5,d0
                bsr.s       print_hex_word
                move.b      0(a6),(a4)             ;; 1st byte of opcode
                move.w      (a4),d0
                move.b      1(a6),d0               ;; 2nd byte of opcode
                bsr.s       print_hex_word
                move.l      a3,d0               ;; sp
                sub.l       a5,d0
                bsr.s       print_hex_word
                move.b      d7,(a4)               ;; a
                move.w      (a4),d0
                move.b      d6,d0               ;; f
                bsr.s       print_hex_word
                move.w      d4,d0               ;; bc
                bsr.s       print_hex_word
                move.w      d3,d0               ;; de
                bsr.s       print_hex_word
                move.w      d5,d0               ;; hl
                bsr.s       print_hex_word
                move.w      reg_ix(a4),d0
                bsr.s       print_hex_word
                move.w      reg_iy(a4),d0
                bsr.s       print_hex_word
                move.w      reg_af_dash(a4),d0
                bsr.s       print_hex_word
                move.w      reg_bc_dash(a4),d0
                bsr.s       print_hex_word
                move.w      reg_de_dash(a4),d0
                bsr.s       print_hex_word
                move.w      reg_hl_dash(a4),d0
                bsr.s       print_hex_word
                moveq.l     #IO_SBYTE,d0
                moveq.l     #$a,d1
                trap        #TRAP_STRM
                movem.l     (sp)+,d3-d7/a1-a6
                rts
print_hex_word:
                moveq.l     #$1,d2
                move.w      d0,(a4)
                move.l      a4,a2
next_digit: 
                clr.w       d1
                move.b      (a2),d1
                lsr.b       #4,d1
                bsr.s       print_hex_digit
                move.b      (a2)+,d1
                bsr.s       print_hex_digit
                dbf         d2,next_digit
                moveq.l     #IO_SBYTE,d0
                moveq.l     #' ',d1
                trap        #TRAP_STRM
                rts
print_hex_digit:
                andi.b      #$f,d1
                add.b       #'0',d1
                cmpi.b      #'9',d1
                bls.s       no_adjust
                addq.w      #7,d1
no_adjust:
                movem.l       d0-d3/a1-a3,-(sp)
                moveq.l     #IO_SBYTE,d0
                moveq.l     #-1,d3
                trap        #TRAP_STRM
                movem.l     (sp)+,d0-d3/a1-a3
                rts
not_implemented_msg:
                dc.w        oni_end-oni_start                             ;; Should be $18
oni_start:      dc.b        10,'OPCODE NOT IMPLEMENTED',10
oni_end:
register_dump_msg:
                dc.w        rd_end-rd_start ;; should be $42
rd_start:       dc.b        ' PC   OP   SP   AF   BC   DE   HL   IX   IY  A',39,'F',39,' B',39
                dc.b        'C',39,' D',39,'E',39,' H',39,'L',39,' ',10
rd_end:
pushed_flags_translate:
                dc.b        $00,$01,$04,$05,$40,$41,$44,$45,$80,$81,$84,$85,$c0,$c1,$c4,$c5
;
; 2 masking bytes, followed by the flags table.
flag_table:
                dc.b        $01,$fe
                dc.b        $06,$00,$00,$02,$00,$02,$02,$00,$00,$02,$02,$00,$02,$00,$00,$02
                dc.b        $00,$02,$02,$00,$02,$00,$00,$02,$02,$00,$00,$02,$00,$02,$02,$00
                dc.b        $00,$02,$02,$00,$02,$00,$00,$02,$02,$00,$00,$02,$00,$02,$02,$00
                dc.b        $02,$00,$00,$02,$00,$02,$02,$00,$00,$02,$02,$00,$02,$00,$00,$02
                dc.b        $00,$02,$02,$00,$02,$00,$00,$02,$02,$00,$00,$02,$00,$02,$02,$00
                dc.b        $02,$00,$00,$02,$00,$02,$02,$00,$00,$02,$02,$00,$02,$00,$00,$02
                dc.b        $02,$00,$00,$02,$00,$02,$02,$00,$00,$02,$02,$00,$02,$00,$00,$02
                dc.b        $00,$02,$02,$00,$02,$00,$00,$02,$02,$00,$00,$02,$00,$02,$02,$00
                dc.b        $08,$0a,$0a,$08,$0a,$08,$08,$0a,$0a,$08,$08,$0a,$08,$0a,$0a,$08
                dc.b        $0a,$08,$08,$0a,$08,$0a,$0a,$08,$08,$0a,$0a,$08,$0a,$08,$08,$0a
                dc.b        $0a,$08,$08,$0a,$08,$0a,$0a,$08,$08,$0a,$0a,$08,$0a,$08,$08,$0a
                dc.b        $08,$0a,$0a,$08,$0a,$08,$08,$0a,$0a,$08,$08,$0a,$08,$0a,$0a,$08
                dc.b        $0a,$08,$08,$0a,$08,$0a,$0a,$08,$08,$0a,$0a,$08,$0a,$08,$08,$0a
                dc.b        $08,$0a,$0a,$08,$0a,$08,$08,$0a,$0a,$08,$08,$0a,$08,$0a,$0a,$08
                dc.b        $08,$0a,$0a,$08,$0a,$08,$08,$0a,$0a,$08,$08,$0a,$08,$0a,$0a,$08
                dc.b        $0a,$08,$08,$0a,$08,$0a,$0a,$08,$08,$0a,$0a,$08,$0a,$08,$08,$0a               
;; Each entry is offset from dispatch_cb_group. This is different to how the main z80 instructions 
;; are handled. I have no idea why this is different.
dispatch_cb_group:
cbg:
                ; Shift operations
                ; I've got a bad feeling sra and srl were flipped in the original implementation. This is
                ; correct below, as per https://clrhome.org/table/
                ; sla and sla behavior, as described is opposite of expected.
                dc.w        rlc_b_instr-cbg,rlc_c_instr-cbg,rlc_d_instr-cbg,rlc_e_instr-cbg,rlc_h_instr-cbg,rlc_l_instr-cbg,rlc_m_instr-cbg,rlc_a_instr-cbg
                dc.w        rrc_b_instr-cbg,rrc_c_instr-cbg,rrc_d_instr-cbg,rrc_e_instr-cbg,rrc_h_instr-cbg,rrc_l_instr-cbg,rrc_m_instr-cbg,rrc_a_instr-cbg
                dc.w        rl_b_instr-cbg,rl_c_instr-cbg,rl_d_instr-cbg,rl_e_instr-cbg,rl_h_instr-cbg,rl_l_instr-cbg,rl_m_instr-cbg,rl_a_instr-cbg
                dc.w        rr_b_instr-cbg,rr_c_instr-cbg,rr_d_instr-cbg,rr_e_instr-cbg,rr_h_instr-cbg,rr_l_instr-cbg,rr_m_instr-cbg,rr_a_instr-cbg
                dc.w        sll_b_instr-cbg,sll_c_instr-cbg,sll_d_instr-cbg,sll_e_instr-cbg,sll_h_instr-cbg,sll_l_instr-cbg,sll_m_instr-cbg,sll_a_instr-cbg
                dc.w        sra_b_instr-cbg,sra_c_instr-cbg,sra_d_instr-cbg,sra_e_instr-cbg,sra_h_instr-cbg,sra_l_instr-cbg,sra_m_instr-cbg,sra_a_instr-cbg
                dc.w        sla_b_instr-cbg,sla_c_instr-cbg,sla_d_instr-cbg,sla_e_instr-cbg,sla_h_instr-cbg,sla_l_instr-cbg,sla_m_instr-cbg,sla_a_instr-cbg
                dc.w        srl_b_instr-cbg,srl_c_instr-cbg,srl_d_instr-cbg,srl_e_instr-cbg,srl_h_instr-cbg,srl_l_instr-cbg,srl_m_instr-cbg,srl_a_instr-cbg
                ; Bit test operations
                dc.w        bit_0_b_instr-cbg,bit_0_c_instr-cbg,bit_0_d_instr-cbg,bit_0_e_instr-cbg,bit_0_h_instr-cbg,bit_0_l_instr-cbg,bit_0_m_instr-cbg,bit_0_a_instr-cbg
                dc.w        bit_1_b_instr-cbg,bit_1_c_instr-cbg,bit_1_d_instr-cbg,bit_1_e_instr-cbg,bit_1_h_instr-cbg,bit_1_l_instr-cbg,bit_1_m_instr-cbg,bit_1_a_instr-cbg
                dc.w        bit_2_b_instr-cbg,bit_2_c_instr-cbg,bit_2_d_instr-cbg,bit_2_e_instr-cbg,bit_2_h_instr-cbg,bit_2_l_instr-cbg,bit_2_m_instr-cbg,bit_2_a_instr-cbg
                dc.w        bit_3_b_instr-cbg,bit_3_c_instr-cbg,bit_3_d_instr-cbg,bit_3_e_instr-cbg,bit_3_h_instr-cbg,bit_3_l_instr-cbg,bit_3_m_instr-cbg,bit_3_a_instr-cbg
                dc.w        bit_4_b_instr-cbg,bit_4_c_instr-cbg,bit_4_d_instr-cbg,bit_4_e_instr-cbg,bit_4_h_instr-cbg,bit_4_l_instr-cbg,bit_4_m_instr-cbg,bit_4_a_instr-cbg
                dc.w        bit_5_b_instr-cbg,bit_5_c_instr-cbg,bit_5_d_instr-cbg,bit_5_e_instr-cbg,bit_5_h_instr-cbg,bit_5_l_instr-cbg,bit_5_m_instr-cbg,bit_5_a_instr-cbg
                dc.w        bit_6_b_instr-cbg,bit_6_c_instr-cbg,bit_6_d_instr-cbg,bit_6_e_instr-cbg,bit_6_h_instr-cbg,bit_6_l_instr-cbg,bit_6_m_instr-cbg,bit_6_a_instr-cbg
                dc.w        bit_7_b_instr-cbg,bit_7_c_instr-cbg,bit_7_d_instr-cbg,bit_7_e_instr-cbg,bit_7_h_instr-cbg,bit_7_l_instr-cbg,bit_7_m_instr-cbg,bit_7_a_instr-cbg
                ; Bit reset operations
                dc.w        res_0_b_instr-cbg,res_0_c_instr-cbg,res_0_d_instr-cbg,res_0_e_instr-cbg,res_0_h_instr-cbg,res_0_l_instr-cbg,res_0_m_instr-cbg,res_0_a_instr-cbg
                dc.w        res_1_b_instr-cbg,res_1_c_instr-cbg,res_1_d_instr-cbg,res_1_e_instr-cbg,res_1_h_instr-cbg,res_1_l_instr-cbg,res_1_m_instr-cbg,res_1_a_instr-cbg
                dc.w        res_2_b_instr-cbg,res_2_c_instr-cbg,res_2_d_instr-cbg,res_2_e_instr-cbg,res_2_h_instr-cbg,res_2_l_instr-cbg,res_2_m_instr-cbg,res_2_a_instr-cbg
                dc.w        res_3_b_instr-cbg,res_3_c_instr-cbg,res_3_d_instr-cbg,res_3_e_instr-cbg,res_3_h_instr-cbg,res_3_l_instr-cbg,res_3_m_instr-cbg,res_3_a_instr-cbg
                dc.w        res_4_b_instr-cbg,res_4_c_instr-cbg,res_4_d_instr-cbg,res_4_e_instr-cbg,res_4_h_instr-cbg,res_4_l_instr-cbg,res_4_m_instr-cbg,res_4_a_instr-cbg
                dc.w        res_5_b_instr-cbg,res_5_c_instr-cbg,res_5_d_instr-cbg,res_5_e_instr-cbg,res_5_h_instr-cbg,res_5_l_instr-cbg,res_5_m_instr-cbg,res_5_a_instr-cbg
                dc.w        res_6_b_instr-cbg,res_6_c_instr-cbg,res_6_d_instr-cbg,res_6_e_instr-cbg,res_6_h_instr-cbg,res_6_l_instr-cbg,res_6_m_instr-cbg,res_6_a_instr-cbg
                dc.w        res_7_b_instr-cbg,res_7_c_instr-cbg,res_7_d_instr-cbg,res_7_e_instr-cbg,res_7_h_instr-cbg,res_7_l_instr-cbg,res_7_m_instr-cbg,res_7_a_instr-cbg
                ; Bit set operations
                dc.w        set_0_b_instr-cbg,set_0_c_instr-cbg,set_0_d_instr-cbg,set_0_e_instr-cbg,set_0_h_instr-cbg,set_0_l_instr-cbg,set_0_m_instr-cbg,set_0_a_instr-cbg
                dc.w        set_1_b_instr-cbg,set_1_c_instr-cbg,set_1_d_instr-cbg,set_1_e_instr-cbg,set_1_h_instr-cbg,set_1_l_instr-cbg,set_1_m_instr-cbg,set_1_a_instr-cbg
                dc.w        set_2_b_instr-cbg,set_2_c_instr-cbg,set_2_d_instr-cbg,set_2_e_instr-cbg,set_2_h_instr-cbg,set_2_l_instr-cbg,set_2_m_instr-cbg,set_2_a_instr-cbg
                dc.w        set_3_b_instr-cbg,set_3_c_instr-cbg,set_3_d_instr-cbg,set_3_e_instr-cbg,set_3_h_instr-cbg,set_3_l_instr-cbg,set_3_m_instr-cbg,set_3_a_instr-cbg
                dc.w        set_4_b_instr-cbg,set_4_c_instr-cbg,set_4_d_instr-cbg,set_4_e_instr-cbg,set_4_h_instr-cbg,set_4_l_instr-cbg,set_4_m_instr-cbg,set_4_a_instr-cbg
                dc.w        set_5_b_instr-cbg,set_5_c_instr-cbg,set_5_d_instr-cbg,set_5_e_instr-cbg,set_5_h_instr-cbg,set_5_l_instr-cbg,set_5_m_instr-cbg,set_5_a_instr-cbg
                dc.w        set_6_b_instr-cbg,set_6_c_instr-cbg,set_6_d_instr-cbg,set_6_e_instr-cbg,set_6_h_instr-cbg,set_6_l_instr-cbg,set_6_m_instr-cbg,set_6_a_instr-cbg
                dc.w        set_7_b_instr-cbg,set_7_c_instr-cbg,set_7_d_instr-cbg,set_7_e_instr-cbg,set_7_h_instr-cbg,set_7_l_instr-cbg,set_7_m_instr-cbg,set_7_a_instr-cbg
;; Each entry is offset from dispatch_ed_group. This is different to how the main z80 instructions 
;; are handled. I have no idea why this is different. If the entry is 0, this is considered an
;; illegal instruction.
dispatch_ed_group:
edg:
out_c_0_instr:
;
; This is just a parking place for undefined opcodes. These symbols will evaluate to 0 when
; put in the table. 
                dc.w        $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000                                                                                         ; $00-$07
                dc.w        $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000                                                                                         ; $08-$0f
                dc.w        $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000                                                                                         ; $10-$17
                dc.w        $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000                                                                                         ; $18-$1f
                dc.w        $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000                                                                                         ; $20-$27
                dc.w        $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000                                                                                         ; $28-$2f
                dc.w        $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000                                                                                         ; $30-$37
                dc.w        $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000                                                                                         ; $38-$3f

                dc.w        in_b_c_instr-edg,out_c_b_instr-edg,sbc_hl_bc_instr-edg,ld_ind_imm_bc_instr-edg,neg_instr-edg,retn_instr-edg,im0_instr-edg,ld_i_a_instr-edg  ; $40-$47
                dc.w        in_c_c_instr-edg,out_c_c_instr-edg,adc_hl_bc_instr-edg,ld_bc_ind_imm_instr-edg,$0000,reti_instr-edg,$0000,ld_r_a_instr-edg              ; $48-$4f
                dc.w        in_d_c_instr-edg,out_c_d_instr-edg,sbc_hl_de_instr-edg,ld_ind_imm_de_instr-edg,$0000,$0000,im1_instr-edg,ld_a_i_instr-edg                   ; $50-$57
                dc.w        in_e_c_instr-edg,out_c_e_instr-edg,adc_hl_de_instr-edg,ld_de_ind_imm_instr-edg,$0000,$0000,im2_instr-edg,ld_a_r_instr-edg                             ; $58-$5f
                dc.w        in_h_c_instr-edg,out_c_h_instr-edg,sbc_hl_hl_instr-edg,ld_ind_imm_hl_instr-edg,$0000,$0000,$0000,rrd_instr-edg                          ; $60-$67
                dc.w        in_l_c_instr-edg,out_c_l_instr-edg,adc_hl_hl_instr-edg,ld_hl_ind_imm_instr-edg,$0000,$0000,$0000,rld_instr-edg                                ; $68-$6f
                dc.w        $0000,$0000,sbc_hl_sp_instr-edg,ld_ind_imm_sp_instr-edg,$0000,$0000,$0000,$0000                                    ; $70-$77
                dc.w        in_a_c_instr-edg,out_c_a_instr-edg,adc_hl_sp_instr-edg,ld_sp_ind_imm_instr-edg,$0000,$0000,$0000,$0000                                        ; $78-$7f

                dc.w        $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000                                                                                         ; $80-$87
                dc.w        $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000                                                                                         ; $88-$8f
                dc.w        $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000                                                                                         ; $90-$97
                dc.w        $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000                                                                                         ; $98-$9f
                dc.w        ldi_instr-edg,cpi_instr-edg,ini_instr-edg,outi_instr-edg,$0000,$0000,$0000,$0000                                                        ; $a0-$a7
                dc.w        ldd_instr-edg,cpd_instr-edg,ind_instr-edg,outd_instr-edg,$0000,$0000,$0000,$0000                                                        ; $a8-$af
                dc.w        ldir_instr-edg,cpir_instr-edg,inir_instr-edg,otir_instr-edg,$0000,$0000,$0000,$0000                                                     ; $b0-$b7
                dc.w        lddr_instr-edg,cpdr_instr-edg,indr_instr-edg,otdr_instr-edg,$0000,$0000,$0000,$0000                                                     ; $b8-$bf

                dc.w        $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000                                                                                         ; $c0-$c7
                dc.w        $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000                                                                                         ; $c8-$cf
                dc.w        $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000                                                                                         ; $d0-$d7
                dc.w        $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000                                                                                         ; $d8-$df
                dc.w        $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000                                                                                         ; $e0-$e7
                dc.w        $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000                                                                                         ; $e8-$ef
                dc.w        $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000                                                                                         ; $f0-$f7
                dc.w        $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000                                                                                         ; $f8-$ff
bios_filename:
                dc.w        nameend-namestart
namestart:      dc.b        'flp1_bios_cde'
nameend:
                dc.b        0
bios_not_found:
                dc.w        failend-failstart
failstart:      dc.b        'file FLP1_BIOS_CDE not found',10
failend:        dc.b        0,10
z80_workspace:
                ds.b        workspace_size
               