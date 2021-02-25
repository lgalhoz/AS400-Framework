             PGM
             CPYF       FROMFILE(QS36F/SD.1.PE) +
                          TOFILE(QTEMP/SD.1.PE) MBROPT(*REPLACE) +
                          CRTFILE(*YES)
             CPYF       FROMFILE(QS36F/GFSEG) +
                          TOFILE(QTEMP/GFSEG) MBROPT(*REPLACE) +
                          CRTFILE(*YES)
             CPYF       FROMFILE(QS36F/CL.NUM) +
                          TOFILE(QTEMP/CL.NUM) MBROPT(*REPLACE) +
                          CRTFILE(*YES)
             CPYF       FROMFILE(#JMPRDDBS/FSINP114) +
                          TOFILE(QTEMP/FSINP114) MBROPT(*REPLACE) +
                          CRTFILE(*YES)
             STRSEU     SRCFILE(#JMPRDSRC/QRPGLESRC) +
                          SRCMBR(PSINO102) OPTION(2)
             ENDDBG
             MONMSG     MSGID(CPD0039 CPF0001 CPF9999)
             STRDBG     PGM(#JMPRDOBJ/PSINO102)
             CALL       PGM(#JMPRDOBJ/PSINO102) PARM('20130310')
             ENDPGM
