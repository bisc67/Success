        include    "cpm.i"

        org        $100

entry:
        LD         DE,name_buffer+2s
        LD         C,0x0
        LD         HL,filebuff
remove_leading_spaces:
        INC        HL
        LD         A,(HL)
        CP         0x20
        JP         Z,remove_leading_spaces
copy_name:
        LD         (DE),A
        INC        DE
        INC        C
        INC        HL
        LD         A,(HL)
        CP         0x20
        JP         P,copy_name
        LD         A,C
        LD         (name_buffer+1),A                                                                         = ""
        LD         A,0x4
        LD         HL,name_buffer
        OUT        (17),A
        AND        A
        RET        Z
        LD         DE,cannot_delete_file_msg
        LD         C,c_writestr
        JP         bdos
annot_delete_file_msg:                                
        ds         "ERROR: Cannot delete QDOS file$"
name_buffer:
        defw        0 ;; length
        defb        0,0,0,0,0
