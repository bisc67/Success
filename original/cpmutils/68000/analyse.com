! �J��                                                                                                                                                                                                                                                       "NM��-I ,-M (-H   l B� ��    gpNB n  a �SUCCESS  Analyse disk  format utility by B Watson (C) 1987
Digital  Precision. This program  will try to  analyse the
format of a disk so  that it may be  read by the  RAW disk
routines. Some options,  if not known should be guessed.


Which drive to read from (1-4)?  *<  a R 0A �C���a � 	
Reading from   C�ptpNCa h *** Please wait, Analysing *** E��r/a  R"J�g  �Q��� n  a & 	Sorry, the disk controller cannot read this disk.

 ` �A��-R  �(/
aJ�fra*/ pNB $_J�Nu?pr�v A��NBJ�f-H 8J�Nu? n pBv�NCC��pv�B�4NCNua ��"<  a ��J�g|  "<  a ��J�W� pNB n  a X 	Disk is   . S 
f�   a 8single  `�   a $double  a  density with   B�. 	  0�HC����ptNCa � bytes per sector and   J. fa �single  `a �double  a � sided.
 There are    sector(s) per track
  z | a ��"8< �a ��J�gRFF "a ��J�f RERF n  H� a L
 L� `"a �`�F  n  a 0 Now searching for start of directory...
 B�. B�. RGHG"a �HJ�f 
�C�?pB�. 	 0�hS@r1   e 
�1  b 
�Q�����  Q���HFF  n  a � 	
Found possible directory start on track  B�. a J. g  a `	 This disk has two formatted sides. Are they:-
1: Numbered 0-79 side 1, 0-79 side 2
2: Numbered 0-79 side 1, 79-0 side 2
3: Both sides used as one logical track
4: Disk is definitely single sided.
Enter choice > *<  B. a 
2SAgSAf�  `SAW� a 
b 	
What size is each logical CP/M block?
0: 1024 bytes  1: 2048 bytes  2: 4096 bytes
3: 8192 bytes  4:16384 bytes. Enter choice > *<   a 	�C�
hR@@ �A=q  a 	� 	
There are  2. a 	<a 	� bytes per logical block.
  a 	�	 	
How many directory entries are there?
0: 64  entries, 1:  96 entries, 2: 128 entries
3: 256 entries, 4: 320 entries. Enter choice >  *<   a ��C�	�=q  a �	 	
There are  2. a \a � directory entries.
  2. H���  4. H� H@J@gRA?H�SAB�� �b�Z=B a vThere is  2a �a b sector(s) allocated for the directory.
What is the sector interleave format (ie; number of
sectors before the next physical sector on disk)
[usually 1-9] >  *< 	 a p=A p(C�	
2���Q���B�C��B�J. gzB�. �. ( S@B�. J. g
Jf�L� 1 �  g
R�e�`��  ��e�RCQ���Q���a H 	
Now creating  a file in the  QDOS  directory called
FLP1_DISKFMT_DRV.  This should be copied to SUCCESS
format  using RDQDOS filename.DRV FLP1_DISKFMT_DRV.
NOTE : This format  may not be correct: if it isn't
please try again with different option values.
   n pNBA��pNBpA��r�vNBJ�f �a ! This file contains the raw disk definition for a disk.
! The file was created using the ANALYSE utility program.
! All the parameters needed to read the disk are included.
! Neither Brian Watson nor Digital Precision can accept any
! responsibility whatsoever for damage caused to the disk
! by an inappropriate selection of parameters.
 B�B�. �. J. g�	?. 	  0�ia 6a �        ! sectors per track (logical)
 B�. a  a d         ! block size,(1=1024,2=2048,3=4096,4=8192,5=16384)
 2. SAa �a        ! max directory entries -1
 2. a �a �       ! initial allocation vector (used in WRITE only)
 B�. J. g�a 4a �         ! number of reserved tracks
!
! Now, the physical characteristics of the disk
!
 C�7pNCa ,         ! RAW$DRV, drive to read from
  . 	pNCa �         ! RAW$PSS, physical sector size
  2a \a �         ! RAW$NPS, number of sectors per track
80        ! RAW$NPT, assume 80 physical tracks
 B�. a �a P         ! RAW$DSF, =0, 80 track disk in 80 drive,=1 40 track disk
  B�. a �a �       ! RAW$FLG, bit 0= density (0=single,1=double)
! Now the logical -> physical sector translate table
  8SDK��H�a a x
  L� Q���pNB`  n  a \CANNOT OPEN OUTPUT FILE ON DISK. ABORTED!!  `  �"a �C�}R�. f  ܇. Nu܇HFF (b
HF. ` �
 n  a  � Cannot find start of directory. The directory is 
assumed to start on track 0. Manually alter this if necessary.
  B. ` � n pNB`  4x �N� n  a  \
Bye.
  ,n ,B�Nu/a  F  *pv�NC 0�e� H@� b�?A 0pNCa     2Nu ae
 zb  Nu*_BA 	cpB9 �3v�NC`��AC� T41 ��B�B�N�    gRMB�N�p`�p)`�p'v�NCp(`�p `�p*`�p+`�p-`�p�`�p"`��������������������� flp1_DISKFMT_DRV     @  @ ` � @128 256 512 1024D2D   �D2S   �D1D    �D1S    �D0D    D0S    D3D   �D3S   � 	FLP1_*D2D                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   