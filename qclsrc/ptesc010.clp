             PGM
             CPYF       FROMFILE(QS36F/GBASEDIO) +
                          TOFILE(QTEMP/GBASEDIO) MBROPT(*REPLACE) +
                          CRTFILE(*YES)
             MONMSG     MSGID(CPF2869 CPF2817)

             CPYF       FROMFILE(QS36F/GREGAPOL) +
                          TOFILE(QTEMP/GREGAPOL) MBROPT(*REPLACE) +
                          CRTFILE(*YES)
             MONMSG     MSGID(CPF2869 CPF2817)

             CPYF       FROMFILE(QS36F/GREGAPO2) +
                          TOFILE(QTEMP/GREGAPO2) MBROPT(*REPLACE) +
                          CRTFILE(*YES)
             MONMSG     MSGID(CPF2869 CPF2817)

             CPYF       FROMFILE(QS36F/FALTER) +
                          TOFILE(QTEMP/FALTER) MBROPT(*REPLACE) +
                          CRTFILE(*YES)
             MONMSG     MSGID(CPF2869 CPF2817)

             CPYF       FROMFILE(#JMPRDDBS/FPRDT001) +
                          TOFILE(QTEMP/FPRDT001) MBROPT(*REPLACE) +
                          CRTFILE(*YES)
             MONMSG     MSGID(CPF2869 CPF2817)
             CALL       PGM(PPRDO010) PARM('QTEMP')
             ENDPGM
