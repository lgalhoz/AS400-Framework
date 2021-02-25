             PGM  PARM(&SERVER)
             DCL        VAR(&SERVER) TYPE(*CHAR) LEN(15)
             OVRDBF     FILE(INPUT) TOFILE(QTEMP/INPUT)
             OVRDBF     FILE(OUTPUT) TOFILE(QTEMP/OUTPUT)
             FTP        RMTSYS(*INTNETADR) INTNETADR(&SERVER)
             DLTOVR     FILE(INPUT OUTPUT)
             ENDPGM
