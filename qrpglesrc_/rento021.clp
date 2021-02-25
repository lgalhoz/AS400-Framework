             PGM
             STRSEU     SRCFILE(SOAPSERV/QRPGLESRC) SRCMBR(RENTO020) +
                          OPTION(2)
             CLRPFM     FILE(SOAPSERV/FENTP001)
             CLRPFM     FILE(SOAPSERV/FENTP002)
             CLRPFM     FILE(SOAPSERV/FENTP003)
             CLRPFM     FILE(SOAPSERV/FENTP004)
             CLRPFM     FILE(SOAPSERV/FENTP005)
             CLRPFM     FILE(SOAPSERV/FENTP006)
             CRTBNDRPG  PGM(SOAPSERV/RENTO020) +
                          SRCFILE(SOAPSERV/QRPGLESRC) +
                          SRCMBR(RENTO020) DBGVIEW(*LIST) +
                          OUTPUT(*NONE) REPLACE(*YES)
             ENDDBG
             MONMSG     MSGID(CPA0701 CPF0001 CPF9999)
             STRDBG     PGM(SOAPSERV/RENTO020)
             CALL       PGM(SOAPSERV/RENTO020)
             ENDPGM
