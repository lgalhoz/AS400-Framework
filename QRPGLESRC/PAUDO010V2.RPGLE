     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*=====================================================================
     H* Programa...: PAUDO010
     H* Objectivo..: Converter Extracto de Movimentos da Lusitânia.
     H* Observação.:
     H*   CRTPF FILE(QTEMP/FAUDP020) SRCFILE(#JMPRDSRC/QDDSSRC) SIZE(*NOMAX)
     H*   CRTPF FILE(QTEMP/FAUDP021) SRCFILE(#JMPRDSRC/QDDSSRC) SIZE(*NOMAX)
     H*   CRTPF FILE(QTEMP/FAUDP022) SRCFILE(#JMPRDSRC/QDDSSRC) SIZE(*NOMAX)
     H*   CRTPF FILE(QTEMP/FINTT010) SRCFILE(#JMPRDSRC/QDDSSRC) SIZE(*NOMAX)
     H*=====================================================================
     FFAUDP020  UF   E           K DISK    PREFIX(INT_)
     FFAUDP021  UF A E           K DISK    PREFIX(CTL_) USROPN
     FFAUDP022  UF A E           K DISK    PREFIX(OUT_) USROPN
     FFAUDP010  IF   E           K DISK    PREFIX(RUL_)
     F*FINT010   IF   E           K DISK    PREFIX(ENV_)
     FGBASEGRL  IF   E           K DISK    PREFIX(BAS_)
     FFPRDT001  IF   E           K DISK    PREFIX(BAS_)
     FGCOBGRH4  IF   E           K DISK    PREFIX(COB_)
     FFAUDI010  O    E             PRINTER
     F*=====================================================================
     D*==> Decompor Códigos
     D DS#REFERENC     DS
     D   WRef#Comp             1     50A
     D   WRef#Ramo             2      3S 0
     D   WRef#Apol             5     11S 0
     D   WRef#Recibo          13     18S 0
     D   WRef#Last            14     18S 0
     D
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
     D
     D WCodigo         S              4A
     D WNumRecibo      S              9S 0
     D WNuApolice      S              7S 0
     D WCdRamo         S              2S 0
     D WNumSegura      S              6S 0
     D
     D WDatValor       S              8S 0
     D WDataRegisto    S              8S 0
     D WTipMovim       S              2S 0
     D WSitMovim       S              2S 0
     D WTotValor       S             11S 2
     D WTotValAlfa     S             11A
     D
     D WIstoSelect     S               N
     D WIcAchou        S               N
     D WIcExiste       S               N
     D WMsgErro        S              7A
     D WIdx            S              3S 0
     D*=====================================================================
      /FREE
        EXSR PrepAudit;

        WRITE RAUDI01000;
        READ RAUDP020;
        DOW (NOT %EOF());
           //--------------------------------------
           // Validar se existe alguma regra para o
           // Código de movimento
           //--------------------------------------
           WIsToSelect = *OFF;
           WCodigo = '  ' + %TRIM(INT_CODIGO);
           SETLL (WCodigo) RAUDP010;
           READE (WCodigo) RAUDP010;
           DOW (NOT %EOF(FAUDP010));
              IF (INT_DSCRITIVO = RUL_DSCRITIVO);
                 WIsToSelect = *ON;
                 LEAVE;
              ENDIF;
              READE (WCodigo) RAUDP010;
           ENDDO;

           IF (WIsToSelect) AND (INT_NUMREC = *ZEROS);
              WIcAchou   = *OFF;
              WRef#Comp  = INT_REFERENC;
              WNuApolice = WRef#Apol;
              WCdRamo    = *ZEROS;
              DOW (WCdRamo < 99);
                 WCdRamo += 1;
                 CHAIN (WCdRamo:WNuApolice) RPRDP001;
                 IF (%FOUND(FPRDT001));
                    SETLL (WCdRamo:WNuApolice) GCOBGRH4;
                    READE (WCdRamo:WNuApolice) GCOBGRH4;
                    DOW (NOT %EOF(GCOBGRH4));
                       WRec#Recibo   = COB_GCNREC;
                       IF (WRec#Last = WRef#Last);
                          WIcAchou   = *ON;
                          WNumRecibo = COB_GCNREC;
                          WNumSegura = COB_GCNSEG;
                          WDatValor  = 20000000 + COB_GCDPAG;
                          EXSR GetValores;
                          IF ((OUT_TOTREC=COB_GCTREC) AND (OUT_TOTREC>*ZEROS))
                           OR ((OUT_TOTCOM=COB_GCCMED) AND (OUT_TOTCOM>*ZEROS));
                             OUT_GCTIPR = COB_GCTIPR;
                             OUT_GCSITR = COB_GCSITR;
                             OUT_GCTREC = COB_GCTREC;
                             OUT_GCCMED = COB_GCCMED;
                             EXSR Processar;
                             LEAVE;
                          ENDIF;
                       ENDIF;
                       READE (WCdRamo:WNuApolice) GCOBGRH4;
                    ENDDO;
                 ENDIF;
                 IF (WIcAchou);
                    LEAVE;
                 ENDIF;
              ENDDO;
           ENDIF;
           READ RAUDP020;
        ENDDO;
        CLOSE FAUDP021;
        CLOSE FAUDP022;

        *INLR = *ON;

        //===================================================================
        // Subrotina.: PrepAudit
        // Objectivo.: Preparar Auditoria aos movimentos
        //===================================================================
        BEGSR PrepAudit;
           MONITOR;
              WStdCmd = 'CLRPFM FILE(QTEMP/FAUDP022)';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);
           ON-ERROR;
           ENDMON;
           MONITOR;
              WStdCmd = 'CLRPFM FILE(QTEMP/FAUDP021)';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);
           ON-ERROR;
           ENDMON;
           OPEN FAUDP021;
           OPEN FAUDP022;
        ENDSR;

        //===================================================================
        // Subrotina.: GetValores
        // Objectivo.: Formatar os Valores das Comissões
        //===================================================================
        BEGSR GetValores;
           WIcExiste  = *OFF;
           CHAIN (WNumRecibo) RAUDP022;
           IF (%FOUND(FAUDP022));
              IF ((OUT_REFTREC <> *BLANKS) AND (OUT_REFTREC = INT_REFERENC)) OR
                ((OUT_REFTCOM <> *BLANKS) AND (OUT_REFTCOM = INT_REFERENC));
                 WIcExiste = *ON;
              ENDIF;
           ENDIF;
           IF (NOT WIcExiste);
              OUT_NUMREC  = WNumRecibo;
              OUT_TIPMOV  = RUL_TIPRECIBO;
              OUT_SITMOV  = RUL_SITRECIBO;
              OUT_NUAPOL  = WNuApolice;
              OUT_CDRAMO  = WCdRamo;
              OUT_NUSEGU  = WNumSegura;
              OUT_DTOPER  = WDataRegisto;
              OUT_DTMOVI  = WDatValor;
              OUT_TOTREC  = *ZEROS;
              OUT_TOTCOM  = *ZEROS;
              OUT_OTHERS  = *ZEROS;
              OUT_MSGERRO = *BLANKS;
           ENDIF;

           //-----------------------------------------
           // Processar os Movimentos
           //-----------------------------------------
           IF (RUL_TIPO = 'D');
              WTotValAlfa = INT_DEBITO;
           ELSE;
              WTotValAlfa = INT_CREDITO;
           ENDIF;
           EXSR FixString;
           WTotValor = %DEC(WTotValAlfa:11:2);

           IF (RUL_INCIDENC = 'R');
              OUT_TOTREC  += WTotValor;
              OUT_REFTREC  = INT_REFERENC;
           ELSEIF (RUL_INCIDENC = 'C');
              OUT_TOTCOM  += WTotValor;
              OUT_REFTCOM  = INT_REFERENC;
           ELSEIF (RUL_INCIDENC = 'O');
              OUT_OTHERS  += WTotValor;
           ENDIF;
        ENDSR;

        //===================================================================
        // Subrotina.: Processar
        // Objectivo.: Processar os registos
        //===================================================================
        BEGSR Processar;
           //--------------------------------------
           // Actualizar equivalência com datas da
           // prestação de contas
           //--------------------------------------
           WCodigo = '  ' + %TRIM(INT_CODIGO);
           WDataRegisto = %DEC(INT_DATA:8:0);
           CHAIN (WdataRegisto:WCodigo) RAUDP021;
           IF (NOT %FOUND(FAUDP021));
              CTL_DATALUSIT = WDataRegisto;
              CTL_CODIGO    = WCodigo;
              CTL_DATAPREST = WDatValor;
              CTL_QTDE      = 1;
              WRITE RAUDP021;
           ELSE;
              CTL_QTDE  += 1;
              UPDATE RAUDP021 %FIELDS(CTL_QTDE);
           ENDIF;

           IF (WIcExiste);
              UPDATE RAUDP022 %FIELDS(OUT_TOTREC:OUT_TOTCOM:OUT_OTHERS:
                                      OUT_MSGERRO:OUT_REFTREC:OUT_REFTCOM);
           ELSE;
              WRITE RAUDP022;
           ENDIF;

           INT_NUMREC = WNumRecibo;
           INT_CDRAMO = WCdRamo;
           INT_NUAPOL = WNuApolice;
           INT_ICCONV = 'S';
           UPDATE RAUDP020 %FIELDS(INT_NUMREC:INT_CDRAMO:INT_NUAPOL:INT_ICCONV);
        ENDSR;

        //===================================================================
        // Subrotina.: FixString
        // Objectivo.: Corrigir a String, eliminando caracteres
        //===================================================================
        BEGSR FixString;
           WIdx = *ZEROS;
           DOW (WIdx < 10);
              WIdx += 1;
              IF (%SUBST(WTotValAlfa:WIdx:1) = '.');
                 WTotValAlfa = %SUBST(WTotValAlfa:1:(WIdx - 1)) +
                            %SUBST(WTotValAlfa:(WIdx + 1));
              ENDIF;
           ENDDO;
        ENDSR;
      /END-FREE
