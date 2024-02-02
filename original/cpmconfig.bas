100 base=RESPR(RESPR(0)-(262144-4*1024))
110 LBYTES flp1_bios_cde,base
120 MODE 4:OPEN#3;con_:PAPER#3;2,0:INK#3;7
130 CLS#3
140 CSIZE#3;2,1:PRINT#3;'   SUCCESS Configuration program.'
150 CSIZE#3;1,0
160 AT#3;2,13:PRINT#3;'(C) 1987 Digital Precision'
170 FOR drv=0 TO 5 STEP 2
180 AT#3;drv+5,0
190 PRINT#3;'Current ';CHR$(65+drv);': and ';CHR$(66+drv);': is ';dev$(drv*2,3);', new is ';
200 INPUT#3;newdev$
210 IF newdev$='' OR LEN(newdev$)<>3 THEN newdev$=dev$(drv*2,3)
220 AT#3;drv+5,33:PRINT#3;newdev$
230 FOR n=0 TO 2:POKE base+PEEK_W(base+2)+n+drv*2,CODE(newdev$(n+1))
240 END FOR drv
250 lstlen=CODE(dev$(13,1))
260 AT#3;11,0:PRINT#3;'Current LST: is ';dev$(14,lstlen);', new is ';
270 INPUT#3;newdev$
280 IF newdev$='' OR LEN(newdev$)>9 THEN newdev$=dev$(14,lstlen)
290 FOR n=0 TO LEN(newdev$)-1
300 POKE base+PEEK_W(base+2)+n+14,CODE(newdev$(n+1))
310 END FOR n
320 POKE base+PEEK_W(base+2)+13,LEN(newdev$)
330 PRINT#3;'SUCCESS will boot from drive ';dev$(0,3);'1_'
340 PRINT#3;'Save configured program?'
350 key$=INKEY$(-1):IF key$<>'y' AND key$<>'Y' THEN GO TO 130
360 PRINT#3;'Renaming flp1_bios_cde to flp1_bios_bak'
370 DELETE flp1_bios_bak
380 COPY flp1_bios_cde,flp1_bios_bak
390 DELETE flp1_bios_cde
400 PRINT#3;'Saving newly configured flp1_bios_cde'
410 SBYTES flp1_bios_cde,base,PEEK_W(base+4)
420 DEFine FuNction dev$(offset,length)
430 LOCal loop,temp$
440 temp$=''
450 FOR loop=0 TO length-1:temp$=temp$&CHR$(PEEK(base+PEEK_W(base+2)+loop+offset))
460 RETurn temp$
470 END DEFine 
9000 DEFine PROCedure sve
9010 DELETE flp1_cpmconfig
9020 SAVE flp1_cpmconfig
9030 END DEFine 
