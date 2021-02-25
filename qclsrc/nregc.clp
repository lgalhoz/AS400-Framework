             PGM        PARM(&MES)
             DCL        VAR(&MES) TYPE(*CHAR) LEN(2)
             DCL        VAR(&NOME) TYPE(*CHAR) LEN(10)
             CHGVAR     VAR(&NOME) VALUE('NREG' *CAT &MES)
             DLTF       FILE(QTEMP/&NOME)
             MONMSG     MSGID(CPF2105)

             CRTPF      FILE(QTEMP/&NOME) SRCFILE(SOAPERV/QS36DDS) +
                          SRCMBR(&NOME)

             CPYF       FROMFILE(QS36F/&NOME) TOFILE(QTEMP/&NOME) +
                          MBROPT(*ADD) FMTOPT(*NOCHK)

             ENDPGM
