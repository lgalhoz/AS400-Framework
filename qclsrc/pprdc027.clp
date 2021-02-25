             PGM
             CRTPF      FILE(QTEMP/OPLSEGT) SRCFILE(SOURCE/QDDSSRC) +
                          OPTION(*NOSRC)
             CRTLF      FILE(QTEMP/OPLSEGT1) SRCFILE(SOURCE/QDDSSRC) +
                          OPTION(*NOSRC)
             CRTBNDRPG  PGM(SOURCE/AP6000R) SRCFILE(SOURCE/QRPGLESRC) +
                          DBGVIEW(*LIST) OUTPUT(*NONE) REPLACE(*YES)
             CRTBNDRPG  PGM(SOURCE/AP6051R) SRCFILE(SOURCE/QRPGLESRC) +
                          DBGVIEW(*LIST) OUTPUT(*NONE) REPLACE(*YES)
             ENDPGM
