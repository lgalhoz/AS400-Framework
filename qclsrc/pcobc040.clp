             PGM        PARM(&NUMORDEM)
             DCL        VAR(&NUMORDEM) TYPE(*CHAR) LEN(5)
             CPYF       FROMFILE(QS36F/SD.1.REC) +
                          TOFILE(QTEMP/SD.1.REC) MBROPT(*REPLACE) +
                          CRTFILE(*YES) INCREL((*IF NORD2 *EQ +
                          &NUMORDEM))
             UPDDTA     FILE(QTEMP/QS36F)
             CPYF       FROMFILE(QTEMP/SD.1.REC) +
                          TOFILE(QTEMP/SD.1.RCA) MBROPT(*REPLACE) +
                          INCREL((*IF K00001 *EQ &NUMORDEM)) +
                          FMTOPT(*NOCHK)
             ENDPGM
