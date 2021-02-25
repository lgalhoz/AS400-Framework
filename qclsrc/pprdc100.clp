             PGM        PARM(&APOLI)
             DCL        VAR(&APOLI) TYPE(*CHAR) LEN(7)
             DCL        VAR(&APOLO) TYPE(*CHAR) LEN(10)

             CHGVAR     VAR(&APOLO) VALUE('000' *CAT &APOLI)

             CRTPF      FILE(QTEMP/EUROPINT) SRCFILE(SOURCE/QDDSSRC) +
                          SRCMBR(EUROPINT) OPTION(*NOLIST *NOSRC) +
                          SIZE(*NOMAX)
             MONMSG     MSGID(CPF5813 CPF7302)

             CPYF       FROMFILE(QS36F/ERPH) TOFILE(QTEMP/EUROPINT) +
                          MBROPT(*REPLACE) INCCHAR(K00001 1 *EQ +
                          &APOLO) FMTOPT(*NOCHK)
             DSPPFM     QTEMP/EUROPINT

             ENDPGM
