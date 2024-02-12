        include    "cpm.i"

        org        $100

entry:
        LD         C,c_writestr
        LD         DE,signon
        CALL       bdos
        LD         A,(filebuff)
        AND        A
        JP         Z,invalid_param
        DEC        A
        LD         B,A
        LD         HL,filebuff+1
        JP         Z,invalid_param
find_next_param:
        INC        HL
        LD         A,(HL)
        DEC        B
        JP         Z,invalid_param
        CP         0x20
        JP         NZ,find_next_param
 remove_leading_spaces:
        INC        HL
        LD         A,(HL)
        DEC        B
        JP         Z,invalid_param
        CP         0x20
        JP         Z,remove_leading_spaces
        INC        B
        LD         A,B
        LD         (qdos_name_buffer+1),A
        LD         DE,qdos_name_buffer+2
why_not_ldir:
        LD         A,(HL)
        LD         (DE),A
        INC        DE
        INC        HL
        DEC        B
        JP         NZ,why_not_ldir
        LD         DE,fcb
        LD         C,f_open
        XOR        A
        LD         (fcb+rec_in_extent),A
        CALL       bdos
        INC        A
        JP         Z,file_not_found
        LD         HL,qdos_name_buffer
        LD         A,0x2                            ; QDOS open new
        OUT        (17),A
        AND        A
        JP         NZ,cannot_open_file
write_next_buffer:
        CALL       read_buffer
        RRA
        LD         B,A
        LD         A,0x0
        RRA
        LD         C,A
        OR         B
        JP         Z,copy_complete
        LD         HL,file_buffer
        LD         A,0x3                            ; QDOS write
        OUT        (18),A
        AND        A
        JP         NZ,write_error
        JP         write_next_buffer
copy_complete:
        CALL       find_next_file
        CALL       qdos_close
        LD         DE,copy_complete_msg
write_message_and_exit:
        LD         C,c_writestr
        CALL       bdos
        JP         0
read_buffer:
        LD         DE,file_buffer
        XOR        A
read_next_sector:
        CP         0x40
        RET        Z
        INC        A
        PUSH       AF
        PUSH       DE
        LD         C,f_dmaoff
        CALL       bdos
        LD         DE,fcb
        LD         C,f_read
        CALL       bdos
        POP        HL
        LD         DE,0x80
        ADD        HL,DE
        EX         DE,HL
        POP        BC
        AND        A
        LD         A,B
        JP         Z,read_next_sector
        DEC        A
        RET
qdos_close:
        LD         A,0x0
        OUT        (17),A
        RET
find_next_file:
        LD         DE,fcb
        LD         C,f_sfirst
        JP         bdos
invalid_param:
        LD         DE,invalid_param_msg
        JP         write_message_and_exit
cannot_open_file:
        LD         DE,cannot_open_file_msg
        JP         write_message_and_exit
file_not_found:
        CALL       qdos_close
        LD         DE,file_not_found_msg
        JP         write_message_and_exit
write_error:
        CALL       qdos_close
        CALL       find_next_file
        LD         DE,error_while_writing_msg
        JP         write_message_and_exit
invalid_param_msg:
        db         "Incorrect command format, WRQDOS <cpmname> <qdosname>$"
cannot_open_file_msg:
        db         "Cannot open QDOS file, check name with a CAT$"
error_while_writing_msg:
        db         "Error during writing the QDOS file$"
file_not_found_msg:
        db         "CP/M format file does not exist$"
copy_complete_msg:
        db         "Single file copy complete.$"
directory_full_msg:
        db         "CP/M disk or directory is full$"
signon:
        db         "CP/M to QDOS file copier vers 1.0",10,13
        db         "By B. Watson, (C) 1987 Digital Precision",10,13,"$"
        db         0

        db         0
        db         0
qdos_name_buffer:
        ds         39,0
file_buffer:

