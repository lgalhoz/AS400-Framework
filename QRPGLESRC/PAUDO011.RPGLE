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
     D
     D WRamo           S              2S 0
     D WApolice        S              7S 0
     D WDatMovim       S              8S 0
     D WDatOpera       S              8S 0
     D WQtdeEnvios     S              3S 0
     D WTotRecibo      S             13S 2
     D WTotComiss      S             13S 2
     D WRecibo         S              9S 0
     D*=====================================================================
      /FREE
        EXSR PrepAudit;
        READ RAUDP022;
        DOW (NOT %EOF());
           WRamo       = *ZEROS;
           WApolice    = *ZEROS;
           WQtdeEnvios = *ZEROS;
           WTotRecibo  = *ZEROS;
           WTotComiss  = *ZEROS;
           WRecibo     = *ZEROS;
           WDatMovim   = *ZEROS;
           OUT_DTMOVI  = WDatMovim;
           OUT_NUMREC  = WRecibo;
           OUT_QTDEREG = WQtdeEnvios;
           OUT_INTRAMO = WRamo;
           OUT_INTAPOL = WApolice;
           OUT_INTTREC = WTotRecibo;
           OUT_INTTCOM = WTotComiss;

           WNumSequ   = *ZEROS;
           WCdRamo    = %TRIM(%EDITW(OUT_LSRAMO:'0  '));
           WNuApolice = %CHAR(OUT_LSAPOL);
           WNumRecibo = %CHAR(OUT_LSNREC);
           SETLL (WCdRamo:WNuApolice:WNumRecibo) RINTT010;
           READE (WCdRamo:WNuApolice:WNumRecibo) RINTT010;
           DOW (NOT %EOF(FINTT010));
              WDatOpera    = %DEC(INT_LDTMOV:8:0);
              IF (WDatopera <= OUT_DTOPER);
                 WDatMovim    = %DEC(INT_LDTANU:8:0);
                 WQtdeEnvios += 1;
                 WRecibo      = INT_KRECIBO;
                 WNumSequ    += 1;
                 WRamo        = INT_KRAMO;
                 WApolice     = INT_KAPOL;
                 OUT_NUSEQU   = WNumSequ;
                 OUT_CODIGO   = INT_LCDMOV;
                 OUT_NUMREC   = WRecibo;
                 OUT_RAMO     = WRamo;
                 OUT_APOL     = WApolice;
                 OUT_DATOPERA = WDatOpera;
                 OUT_DATMOVIM = WDatMovim;
                 OUT_TOTREC   = %DEC(INT_LTREC:11:0) / 100;
                 OUT_TOTCOM   = %DEC(INT_LCOM1:11:0) / 100;
                 WRITE RAUDP023;
                 IF (INT_LCDMOV <> '1');
                    WTotRecibo   = %DEC(INT_LTREC:11:0) / 100;
                    WTotComiss   = %DEC(INT_LCOM1:11:0) / 100;
                 ENDIF;
              ENDIF;
              READE (WCdRamo:WNuApolice:WNumRecibo) RINTT010;
           ENDDO;
           OUT_NUMREC  = WRecibo;
           OUT_DTMOVI  = WDatMovim;
           OUT_QTDEREG = WQtdeEnvios;
           OUT_INTRAMO = WRamo;
           OUT_INTAPOL = WApolice;
           OUT_INTTREC = WTotRecibo;
           OUT_INTTCOM = WTotComiss;
           UPDATE RAUDP022 %FIELDS(OUT_NUMREC:OUT_QTDEREG:OUT_INTTREC:
                       OUT_INTTCOM:OUT_DTMOVI:OUT_INTRAMO:OUT_INTAPOL);
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
