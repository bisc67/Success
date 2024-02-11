fcb             equ 005ch

c_writestr      equ 09h
f_open          equ 0fh
f_close         equ 10h
f_read          equ 14h
f_dmaoff        equ 1ah

filebuff        equ 80h

bdos    equ     5
        org     $100
entry:
        LD         DE,banner_msg
        LD         C,c_writestr
        CALL       bdos
        LD         HL,0x0
        ADD        HL,SP
        LD         (entry_stack_ptr),HL
        AND        0x0
        LD         (filebuf),A
        LD         A,(filebuf+2)
        CP         ':'
        JP         P,load_file_and_parse
        LD         HL,filebuf+2
        LD         (file_read_ptr),HL
 parse_line:
        LD         HL,(file_read_ptr)
 prune_space_lp:
        LD         A,(HL)
        INC        HL
        CP         ' '
        JP         Z,prune_space_lp
        SUB        '0'
        JP         M,bad_number
        CP         '0'+10
        JP         P,bad_number
        LD         E,A
 parse_key_num:
        LD         A,(HL)
        SUB        '0'
        JP         M,end_of_keynum
        CP         10
        JP         P,end_of_keynum
        INC        HL
        PUSH       AF
        LD         A,E
        ADD        A,A
        JP         C,key_out_of_range
        LD         E,A
        ADD        A,A
        JP         C,key_out_of_range
        ADD        A,A
        JP         C,key_out_of_range
        ADD        A,E
        JP         C,key_out_of_range
        LD         E,A
        POP        AF
        ADD        A,E
        JP         C,key_out_of_range
        LD         E,A
        JP         parse_buffer_key_num
 end_of_keynum:
        LD         A,E
        LD         (key_number),A 
        SUB        224
        JP         C,key_out_of_range
        CP         32
        JP         NC,key_out_of_range
        LD         (file_read_ptr),HL
        LD         (definition_ptr),HL
        LD         L,A
        LD         H,0x0
        ADD        HL,HL
        ADD        HL,HL
        ADD        HL,HL
        ADD        HL,HL
        LD         DE,0xf600
        ADD        HL,DE
        LD         (def_buffer_ptr),HL
        EX         DE,HL
        LD         HL,(file_read_ptr)
        LD         B,0xff
find_open_quote:
        LD         A,(HL)
        AND        A
        JP         Z,opening_quote_missing
        CP         0x27                             ;; Single quote mark
        INC        HL
        JP         NZ,find_open_quote
 next_char:
        LD         A,(HL)
        INC        HL
        INC        B
        AND        A
        JP         Z,end_quote_missing
        CP         0x27                            ;; Quote mark
        JP         Z,end_of_definition
        CP         0x5c                            ;; backslash
        JP         NZ,is_upper_def
        LD         A,(HL)
        INC        HL
        SUB        64                               ;; Lowercase to uppercase
 is_upper_def:
        LD         (DE),A
        INC        DE
        JP         next_char
 end_of_definition:
        LD         (file_read_ptr),HL
        LD         HL,(def_buffer_ptr)
        XOR        A
        LD         (DE),A
        LD         A,B
        AND        A
        JP         NZ,dont_clear_definition
        LD         HL,(definition_ptr)
        LD         (HL),0x0
        LD         HL,0x0
 dont_clear_definition:
        LD         A,(key_number)
        ; Defkey bios call
        OUT        (20),A
        RET
 load_file_and_parse:
        LD         DE,reading_msg
        LD         C,c_writestr
        CALL       bdos
        LD         HL,0x65
        LD         A,(HL)
        CP         0x20
        JP         NZ,open_file
        LD         (HL),'K'
        INC        HL
        LD         (HL),'E'
        INC        HL
        LD         (HL),'Y'
 open_file:
        LD         C,f_open
        LD         DE,fcb
        CALL       bdos
        INC        A
        JP         Z,file_open_failed
        XOR        A
        LD         (DAT_ram_007c),A
        LD         HL,parse_buffer
        LD         (file_read_ptr),HL
 read_next_line:
        PUSH       HL
        EX         DE,HL
        LD         C,f_dmaoff
        CALL       bdos
        LD         C,f_read
        LD         DE,fcb
        CALL       bdos
        POP        HL
        LD         BC,0x80
        ADD        HL,BC
        AND        A
        JP         Z,read_next_line
 next_line:
        CALL       parse_line
        LD         HL,(file_read_ptr)
 skip_to_eol:
        INC        HL
        LD         A,(HL)
        AND        A
        JP         Z,bad_keyboard_definition
        CP         10
        JP         NZ,skip_to_eol
        INC        HL
        LD         (file_read_ptr),HL 
        LD         A,(HL)
        CP         26                  ; CTRL-Z, eof?
        JP         NZ,next_line
        RET
 end_quote_missing:
        LD         DE,end_quote_missing_msg
        JP         print_msg_and_exit
 file_open_failed:
        LD         DE,file_open_failed_msg
        JP         print_msg_and_exit
 bad_keyboard_definition:
        LD         DE,bad_keyboard_definition_msg
        JP         print_msg_and_exit
opening_quote_missing:
        LD         DE,opening_quote_missing_msg
 print_msg_and_exit:
        LD         C,c_writestr
        CALL       bdos
        LD         HL,(entry_stack_ptr)
        LD         SP,HL
        RET
bad_number:
        LD         DE,bad_number_msg
        JP         print_msg_and_exit
key_out_of_range:
        LD         DE,key_out_of_range_msg
        JP         print_msg_and_exit
bad_number_msg
        ds         "\r\nError: missing or bad numeric characters$"
key_out_of_range_msg:
        ds         "\r\nError: key number is out of range$"
end_quote_missing_msg:
        ds         "\r\nError: end of string quote is missing$"
opening_quote_missing_msg:
        ds         "\r\nError: string or opening quote is missing$"
file_open_failed_msg:
        ds         "\r\nError: cannot open file or filename ambiguous$"
bad_keyboard_definition_msg:
        ds         "\r\nError: bad keyboard definition file$"
banner_msg:
        ds         "Key definer V1.0 by B. Watson,"
        ds         " (C) 1987 Digital Precision$"
reading_msg:
        ds         "\r\nReading key definitions from file...$"
key_number:
        defb    0
file_read_ptr: ;; @3b1,3b2
    db          0, 0
definition_ptr:
        defb        0, 0
 entry_stack_ptr:
        defb        0, 0
 def_buffer_ptr:
        db      0,0
