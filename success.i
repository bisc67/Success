;
; Formatting:
;_______________I___________O_______________________________C
;               17          29                              61
;               Instr.      Operand                         Comment
;
reg_af          equ     $00
reg_bc          equ     $02
reg_de          equ     $04
reg_hl          equ     $06
reg_ix          equ     $08
reg_iy          equ     $0a
reg_bc_dash     equ     $0c
reg_de_dash     equ     $0e
reg_hl_dash     equ     $10
reg_af_dash     equ     $12
con_handle      equ     $14
flp_handle      equ     $18
ser_handle      equ     $1c
sys_disk_handle equ     $28
dma_address     equ     $2c
selected_track  equ     $2e
selected_sector equ     $2f
cache_flags     equ     $30
cached_track    equ     $32
active_dpb      equ     $34         ; Not really sure about this.
temp_buffer     equ     $36
term_saved_char equ     $3d
term_last_char  equ     $3e
term_inp_mode   equ     $3f
name_buffer     equ     $42
key_definitions equ     $6a
workspace_size  equ     360         ; Total size, should be 360

SECTOR_SIZE     equ     128
SEC_PER_TRACK   equ     20