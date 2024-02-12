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
        LD         HL,qdos_name_buffer
        LD         A,0x1
        OUT        (17),A
        AND        A
        JP         NZ,cannot_open_qdos
        LD         DE,fcb
        LD         C,f_sfirst
        CALL       bdos
        INC        A
        JP         NZ,file_exists
        LD         DE,fcb
        LD         C,f_creat
        CALL       bdos
        INC        A
        JP         Z,cannot_create_file
read_next_buffer:
        LD         HL,file_buffer+1
        LD         BC,0x2000
fill_with_eof:
        LD         (HL),0x1a
        INC        HL
        DEC        BC
        LD         A,B
        OR         C
        JP         NZ,fill_with_eof
        LD         A,0x1
        LD         HL,file_buffer+1
        LD         BC,0x2000
        OUT        (18),A
        PUSH       AF
        LD         HL,file_buffer
        LD         A,C
        RLCA
        LD         A,B
        RLA
        LD         (HL),A
        LD         A,C
        AND        0x7f
        JP         Z,read_next_buffer
        INC        (HL)
write_next_buffer:
        CALL       write_buffer
        AND        A
        POP        BC
        JP         NZ,disk_full
        LD         A,B
        AND        A
        JP         Z,write_next_buffer
        LD         DE,fcb
        LD         C,f_close
        CALL       bdos
        CALL       qdos_close
        LD         DE,copy_complete_msg
write_message_and_exit:
        LD         C,c_writestr
        CALL       bdos
        JP         0
write_buffer:
        LD         DE,file_buffer+1
        LD         A,(file_buffer)
next_sector:
        AND        A
        RET        Z
        DEC        A
        PUSH       AF
        PUSH       DE
        LD         C,f_dmaoff
        CALL       bdos
        LD         DE,fcb
        LD         C,f_write
        CALL       bdos
        AND        A
        JP         NZ,write_done
        POP        HL
        LD         DE,0x80
        ADD        HL,DE
        EX         DE,HL
        POP        AF
        JP         next_sector
write_done:
        POP        DE
        POP        DE
        RET
qdos_close:
        LD         A,0x0
        OUT        (17),A
        RET
invalid_param:
        LD         DE,invalid_param_msg
        JP         write_message_and_exit
cannot_open_qdos:
        LD         DE,cannot_open_qdos_msg
        JP         write_message_and_exit
cannot_create_file:
        CALL       qdos_close
        LD         DE,cannot_create_cpm_msg
        JP         write_message_and_exit
file_exists:
        CALL       qdos_close
        LD         DE,file_exists_msg
        JP         write_message_and_exit
disk_full:
        CALL       qdos_close
        LD         DE,disk_full_msg
        JP         write_message_and_exit
invalid_param_msg:
        db         "Incorrect command format, RDQDOS <cpmname> <qdosname>$"
cannot_open_qdos_msg:
        db         "Cannot open QDOS file, check name with a CAT$"
file_exists_msg:
        db         "CP/M file already exists, or name ambiguous$"
cannot_create_cpm_msg:
        db         "Cannot open CP/M file, check disk for R/O or space$"
copy_complete_msg:
        db         "Single file copy complete.$"
disk_full_msg:
        db         "CP/M disk or directory is full$"
signon:
        db         "QDOS to CP/M file copier vers 1.0",10,13
        db         "By B. Watson (C) 1987 Digital Precision",10,13,10,13,"$"
qdos_name_buffer:
        ds         38,0
file_buffer:
