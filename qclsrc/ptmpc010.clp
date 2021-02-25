             PGM
             DLTF       FILE(QTEMP/FTMPP020)
             CRTPF      FILE(QTEMP/FTMPP020) +
                          SRCFILE(#JMPRDSRC/QDDSSRC) +
                          SRCMBR(FTMPP020) SIZE(*NOMAX)
             CPYF       FROMFILE(QS36F/GC88410) +
                          TOFILE(QTEMP/FTMPP020) MBROPT(*REPLACE) +
                          FMTOPT(*NOCHK)
             WRKQRY
             ENDPGM
