# Success
Sinclair QL Z80 emulator, used for CP/M 2.2

# History
This was originally released in 1988. I had lost all the source code associated with it, so this project includes a disassembly of the production code. This has been commented, and appropriate labels added. The resulting source files in the 'original' folder will assemble to byte-exact versions of the released code. Use Devpac QL to assemble, alternatively, use Easy68K for assembly (until I find a better one).

There is no guarantee that the Z80 emulation is correct.

There appears to be some dead-code and dead-data.

# The Utilities

There are 2 folders of utility files. The '68000' are written in 68K assembly, where there is a short Z80 trampoline at the start of the program to execute some 68k code, using a custom syscall.

The 'z80' utilities are written completely in z80 assembly. They tend to be the simpler, smaller, utilities - or where it makes sense to do it within the CP/M environment.


