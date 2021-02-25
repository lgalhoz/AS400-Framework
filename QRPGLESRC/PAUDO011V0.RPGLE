     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*=====================================================================
     H* Programa...: PAUDO013
     H* Objectivo..: Validar que informação seguiu no Intrerface.
     H* Observação.:
     H*   CRTPF FILE(QTEMP/FAUDP022) SRCFILE(#JMPRDSRC/QDDSSRC) SIZE(*NOMAX)
     H*   CRTPF FILE(QTEMP/FAUDP023) SRCFILE(#JMPRDSRC/QDDSSRC) SIZE(*NOMAX)
     H*=====================================================================
     FFAUDP022  UF A E           K DISK    PREFIX(OUT_)
     FFAUDP023  UF A E           K DISK    PREFIX(OUT_) USROPN
     FFINTT010  IF   E           K DISK    PREFIX(INT_)
     F*=====================================================================
     D DS#RECIBO       DS
     D   WRec#Recibo           1      9S 0
     D   WRec#Last             5      9S 0
     D
     D*==> Lista de Parametros
     D shell           PR                  EXTPGM('QCMDEXC')
     D   StdCmd                     200A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5
     D
     D*==> Campos de Trabalho
     D WStdCmd         S            200A
     D WStdLen         S             15P 5
     D WCdRamo         S              2A
     D WNuApolice      S              7A
     D WNumRecibo      S              6A
     D WNumSequ        S              5S 0
     D WQtdeEnvios     S              3S 0
     D
     D WTotRecibo      S             13S 2
     D WTotComiss      S             13S 2
     D*=====================================================================
      /FREE
        EXSR PrepAudit;
        READ RAUDP022;
        DOW (NOT %EOF());
           WQtdeEnvios = *ZEROS;
           WTotRecibo  = *ZEROS;
           WTotComiss  = *ZEROS;
           OUT_QTDEREG = WQtdeEnvios;
           OUT_INTTREC = WTotRecibo;
           OUT_INTTCOM = WTotComiss;
           IF (OUT_NUMREC <> OUT_LSNREC);
              WNumSequ   = *ZEROS;
              WCdRamo    = %TRIM(%EDITW(OUT_LSRAMO:'0  '));
              WNuApolice = %CHAR(OUT_LSAPOL);
              WNumRecibo = %CHAR(OUT_LSNREC);
              SETLL (WCdRamo:WNuApolice:WNumRecibo) RINTT010;
              READE (WCdRamo:WNuApolice:WNumRecibo) RINTT010;
              DOW (NOT %EOF(FINTT010));
                 WQtdeEnvios += 1;
                 WNumSequ    += 1;
                 OUT_NUSEQU   = WNumSequ;
                 OUT_CODIGO   = INT_LCDMOV;
                 OUT_DATOPERA = %DEC(INT_LDTMOV:8:0);
                 OUT_DATMOVIM = %DEC(INT_LDTANU:8:0);
                 OUT_TOTREC   = %DEC(INT_LTREC:11:0) / 100;
                 OUT_TOTCOM   = %DEC(INT_LCOM1:11:0) / 100;
                 WRITE RAUDP023;
                 IF (INT_LCDMOV <> '1');
                    WTotRecibo   = %DEC(INT_LTREC:11:0) / 100;
                    WTotComiss   = %DEC(INT_LCOM1:11:0) / 100;
                 ENDIF;
                 READE (WCdRamo:WNuApolice:WNumRecibo) RINTT010;
              ENDDO;
              OUT_QTDEREG = WQtdeEnvios;
              OUT_INTTREC = WTotRecibo;
              OUT_INTTCOM = WTotComiss;
           ENDIF;
           UPDATE RAUDP022 %FIELDS(OUT_QTDEREG:OUT_INTTREC:OUT_INTTCOM);
           READ RAUDP022;
        ENDDO;
        CLOSE FAUDP023;

        *INLR = *ON;

        //===================================================================
        // Subrotina.: PrepAudit
        // Objectivo.: Preparar Auditoria aos movimentos
        //===================================================================
        BEGSR PrepAudit;
           //------------------------------------------------------
           // Controlo de Registos
           //------------------------------------------------------
           MONITOR;
              WStdCmd = 'CLRPFM FILE(QTEMP/FAUDP023)';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);
           ON-ERROR;
           ENDMON;
           OPEN FAUDP023;
        ENDSR;
      /END-FREE
