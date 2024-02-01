# Success
Sinclair QL Z80 emulator, used for CP/M 2.2

# History
This was originally released in 1988. I had lost all the source code associated with it, so this project includes a disassembly of the production code. This has been commented, and appropriate labels added. The resulting source files in the 'original' folder will assemble to byte-exact versions of the released code. Use Devpac QL to assemble, alternatively, use Easy68K for assembly (until I find a better one).

There is no guarantee that the Z80 emulation is correct. During disassembly, I noticed some inconsistencies between labels and what functions actually do, specifically for rl/rlc/rr/rrc. As documentation online indicates rl is really a rotate with carry, and rlc is a rotate without carry.
