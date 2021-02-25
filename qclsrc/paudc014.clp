             PGM
             DLTF       FILE(QS36F/SD1CCTMP)
             MONMSG     MSGID(CPF2105)
             CRTPF      FILE(QS36F/SD1CCTMP) SRCFILE(LIB.SD/QDDSSRC) +
                          SIZE(*NOMAX)
             CPYF       FROMFILE(QS36F/SD.1.CC) +
                          TOFILE(QS36F/SD1CCTMP) MBROPT(*REPLACE) +
                          FMTOPT(*NOCHK)
             RUNSQLSTM  SRCFILE(#JMPRDSRC/QSQLSRC) SRCMBR(SCOB001) +
                          COMMIT(*NONE)

             ENDPGM
