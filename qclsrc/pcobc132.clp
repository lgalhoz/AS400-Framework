             PGM        PARM(&FILE &MODO &CODERRO)

             DCL        VAR(&FILE) TYPE(*CHAR) LEN(10)
             DCL        VAR(&MODO) TYPE(*CHAR) LEN(1)
             DCL        VAR(&CODERRO) TYPE(*CHAR) LEN(7)

             OVRDBF     FILE(SDRCA) TOFILE(QS36F/SD.1.RCA) +
                          SHARE(*NO)
             OVRDBF     FILE(SD1CG2) TOFILE(QS36F/SD.1.CG2) +
                          SHARE(*NO)
             OVRDBF     FILE(SD1PE) TOFILE(SD.1.PE)

             OVRPRTF    FILE(TB2301M) HOLD(*YES)
             OVRPRTF    FILE(TB2302M) PAGESIZE(*N 90) CPI(12) +
                          DRAWER(2) PAGRTT(0) DUPLEX(*YES) HOLD(*YES)
             OVRPRTF    FILE(TB2303M) PAGESIZE(*N 90) CPI(12) +
                          DRAWER(2) PAGRTT(0) DUPLEX(*NO) HOLD(*YES)

             CALL       PGM(TB2300RR) PARM(&FILE &MODO &CODERRO)

             DLTOVR     FILE(SDRCA)
             DLTOVR     FILE(SD1CG2)

             ENDPGM
