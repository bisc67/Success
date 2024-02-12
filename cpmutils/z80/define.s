dpb_spt             equ $eadc           ; eadc
dpb_bsh             equ dpb_spt+2       ; eade
dpb_blm             equ dpb_bsh+1       ; eadf
dpb_exm             equ dpb_blm+1       ; eae0
dpb_dsm             equ dpb_exm+1       ; eae1
dpb_drm             equ dpb_dsm+2       ; eae3
dpb_al0             equ dpb_drm+2       ; eae5
dpb_al1             equ dpb_al0+1       ; eae6
dpb_cks             equ dpb_al1+1       ; eae7
dpb_off             equ dpb_cks+2       ; eae9
dpb_end             equ dpb_off+2       ; eaeb

cust_raw_drv        equ $ed00           ; $ed00
cust_raw_pss        equ cust_raw_drv+1  ; $ed01
cust_raw_nps        equ cust_raw_pss+1  ; $ed02
cust_raw_npt        equ cust_raw_nps+1  ; $ed03
cust_raw_dsf        equ cust_raw_npt+1  ; $ed04
cust_raw_flg        equ cust_raw_dsf+1  ; $ed05
cust_raw_wkspc      equ cust_raw_flg+1  ; $ed06
cust_raw_lpsmap     equ cust_raw_wkspc+2; $ed08

        include "cpm.i"

        org         $100
        LD          DE,signon_message
        LD          C,c_writestr
        CALL        bdos
        LD          HL,0x65
        LD          A,(HL)
        CP          0x20
        JP          NZ,LAB_ram_0119
        LD          (HL),0x44
        INC         HL
        LD          (HL),0x52
        INC         HL
        LD          (HL),0x56
LAB_ram_0119:
        LD          DE,fcb
        LD          A,0x0
        LD          (007ch),A
        LD          C,f_open
        CALL        bdos
        INC         A
        JP          Z,file_not_found
        LD          HL,line_buffer
        LD          (line_ptr),HL
read_record:
        PUSH        HL
        EX          DE,HL
        LD          C,f_dmaoff
        CALL        bdos
        LD          C,f_read
        LD          DE,fcb
        CALL        bdos
        POP         HL
        LD          BC,0x80
        ADD         HL,BC
        AND         A
        JP          Z,read_record
        LD          C,f_close
        LD          DE,fcb
        CALL        bdos
        CALL        parse_word_value
        LD          (dpb_spt),HL
        CALL        parse_byte_value
        CP          0x0
        JP          Z,out_of_range
        CP          0x6
        JP          NC,out_of_range
        LD          HL,default_definitions
        LD          D,0x0
        LD          E,A
        ADD         A,E
        ADD         A,E
        LD          E,A
        ADD         HL,DE
        LD          A,(HL)
        LD          (dpb_bsh),A
        INC         HL
        LD          A,(HL)
        LD          (dpb_blm),A
        INC         HL
        LD          A,(HL)
        LD          (dpb_exm),A
        LD          HL,0xff
        LD          (dpb_dsm),HL
        CALL        parse_word_value                        
        LD          (dpb_drm),HL
        LD          HL,0x4
        LD          (dpb_cks),HL
        CALL        parse_word_value                        
        LD          (dpb_al0),HL
        CALL        parse_word_value                        
        LD          (dpb_off),HL
        CALL        parse_byte_value                        
        CP          0x4
        JP          NC,out_of_range
        CP          0x0
        JP          Z,out_of_range
        LD          (cust_raw_drv),A
        CALL        parse_byte_value                        
        CP          0x4
        JP          NC,out_of_range
        LD          (cust_raw_pss),A
        CALL        parse_byte_value                        
        LD          (cust_raw_nps),A
        CALL        parse_byte_value                        
        LD          (cust_raw_npt),A
        CALL        parse_byte_value                        
        LD          (cust_raw_dsf),A
        CALL        parse_byte_value                        
        LD          (cust_raw_flg),A
        LD          A,(cust_raw_nps)
        LD          DE,cust_raw_lpsmap
LAB_ram_01d2:
        PUSH        AF
        PUSH        DE
        CALL        parse_byte_value
        POP         DE
        LD          (DE),A
        INC         DE
        POP         AF
        DEC         A
        JP          NZ,LAB_ram_01d2
        EX          DE,HL
        LD          (cust_raw_wkspc),HL
        ; Define custom drive entry
        LD          A,0xff
        LD          HL,0xed00
        OUT         ($14),A
        LD          DE,success_message
        LD          C,c_writestr
        JP          bdos
;************************************************************************************************
; Return result in HL
parse_word_value:
        CALL        parse_number                            ;undefined parse_number()
        PUSH        DE
        POP         HL
        RET
;************************************************************************************************
; Return result in A
parse_byte_value:
        CALL        parse_number
        LD          A,D
        AND         A
        JP          NZ,out_of_range
        LD          A,E
        RET
;************************************************************************************************
; Return result in DE
parse_number:
        LD          HL,(line_ptr)
        LD          A,(HL)
        INC         HL
        CP          0x1a                                    ; CTRL-Z?
        JP          Z,end_of_file
        CP          0x21                                    ; '!'?
        JP          NZ,must_be_number
        ; Handle comment
        CALL        read_to_eol
        JP          parse_number
;
; First character must be a digit.
; From then on, we don't care. Any non digit character cancels parsing.
;
must_be_number:
        SUB         '0'
        JP          C,invalid_number
        CP          10
        JP          NC,invalid_number
        LD          E,A
        LD          D,0x0
parse_next_digit:
        LD          A,(HL)                                  ; Get next digit
        SUB         '0'                                     ; Only accept '0'..'9'
        JP          C,read_to_eol
        CP          10
        JP          NC,read_to_eol
        INC         HL
        EX          DE,HL
        ADD         HL,HL
        LD          B,H                                     ; Multiply by 10
        LD          C,L
        ADD         HL,HL
        ADD         HL,HL
        ADD         HL,BC
        LD          B,0x0
        LD          C,A
        ADD         HL,BC                                   ; Add on new digit
        EX          DE,HL
        JP          C,out_of_range
        JP          parse_next_digit
;************************************************************************************************
read_to_eol:
        LD          A,(HL)
        INC         HL
        CP          0x1a                    ; CTRL-Z
        JP          Z,end_of_file
        CP          10                      ; EOL
        JP          NZ,read_to_eol
        LD          (line_ptr),HL
        RET
file_not_found:
        LD          DE,err_filenotfound
print_and_exit:
        LD          C,c_writestr
        CALL        bdos
        RST         0
invalid_number:
        LD          DE,err_invalidnumber
        JP          print_and_exit
out_of_range:
        LD          DE,err_outofrange
        JP          print_and_exit
end_of_file:
        LD          DE,err_endoffile
        JP          print_and_exit
err_filenotfound:
        db         "ERROR: File not found, aborted.",10,13,"$"
err_invalidnumber:
        db         "ERROR: Line contains invalid number",10,13,"$"
err_outofrange:
        db         "ERROR: Number is out of range",10,13,"$"
err_endoffile:
        db         "ERROR: Unexpected end of input file",10,13,"$"
success_message:
        db         "Drive G: has successfully been defined.",10,13,"$"
signon_message:
        db         "Raw disk read definer V1.0 by B. Watson",10,13
        db         "(C) 1987 Digital Precision",10,13,"$"

default_definitions:
        db          00h,00h,00h
        db          03h,07h,00h
        db          04h,0Fh,01h
        db          05h,1Fh,03h
        db          06h,3Fh,07h
        db          07h,7Fh,0Fh
line_ptr:
        dw          0
line_buffer:
        dw          00h
