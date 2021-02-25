             PGM
             DLTF       FILE(QTEMP/RECIBOS)
             MONMSG     MSGID(CPF2105)
             CRTPF      FILE(QTEMP/RECIBOS) SRCFILE(#JMPRDSRC/QS36DDS) +
                          SRCMBR(GUIAS) SIZE(*NOMAX)
             CPYF       FROMFILE(QS36F/GGUI1601) TOFILE(QTEMP/RECIBOS) +
                          MBROPT(*REPLACE) FMTOPT(*NOCHK)
             CPYF       FROMFILE(QS36F/GGUIA01A) TOFILE(QTEMP/RECIBOS) +
                          MBROPT(*ADD) FMTOPT(*NOCHK)
             CPYF       FROMFILE(QS36F/GGUIA01B) TOFILE(QTEMP/RECIBOS) +
                          MBROPT(*ADD) FMTOPT(*NOCHK)
             CPYF       FROMFILE(QS36F/GGUIA01C) TOFILE(QTEMP/RECIBOS) +
                          MBROPT(*ADD) FMTOPT(*NOCHK)
             CPYF       FROMFILE(QS36F/GGUIA01D) TOFILE(QTEMP/RECIBOS) +
                          MBROPT(*ADD) FMTOPT(*NOCHK)

             CPYF       FROMFILE(QS36F/GGUIA01F) TOFILE(QTEMP/RECIBOS) +
                          MBROPT(*ADD) FMTOPT(*NOCHK)
             CPYF       FROMFILE(QS36F/GGUIA012) TOFILE(QTEMP/RECIBOS) +
                          MBROPT(*ADD) FMTOPT(*NOCHK)
             CPYF       FROMFILE(QS36F/GGUIA014) TOFILE(QTEMP/RECIBOS) +
                          MBROPT(*ADD) FMTOPT(*NOCHK)
             CPYF       FROMFILE(QS36F/GGUIA016) TOFILE(QTEMP/RECIBOS) +
                          MBROPT(*ADD) FMTOPT(*NOCHK)
             CPYF       FROMFILE(QS36F/GGUIA017) TOFILE(QTEMP/RECIBOS) +
                          MBROPT(*ADD) FMTOPT(*NOCHK)
             CPYF       FROMFILE(QS36F/GGUIA019) TOFILE(QTEMP/RECIBOS) +
                          MBROPT(*ADD) FMTOPT(*NOCHK)

             ENDPGM
