     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*=====================================================================
     H* Programa...: PAUDO010
     H* Objectivo..: Converter Extracto de Movimentos da Lusitânia.
     H*=====================================================================
     FFINTT030  UP   E           K DISK    PREFIX(INT_)
     FFINTT031  UF A E           K DISK    PREFIX(OUT_)
     FFINTT032  IF   E           K DISK    PREFIX(RUL_)
     FGBASEGRL  IF   E           K DISK    PREFIX(BAS_)
     FFPRDT001  IF   E           K DISK    PREFIX(BAS_)
     FGCOBGRH4  IF   E           K DISK    PREFIX(COB_)
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
     D*==> Campos de Trabalho
     D WCodigo         S              4A
     D WNumRecibo      S              9S 0
     D WNuApolice      S              7S 0
     D WCdRamo         S              2S 0
     D WNumSegura      S              6S 0
     D
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
        WIsToSelect = *OFF;
        WCodigo = '  ' + %TRIM(INT_CODIGO);
        SETLL (WCodigo) RINTT032;
        READE (WCodigo) RINTT032;
        DOW (NOT %EOF(FINTT032));
           IF (INT_DSCRITIVO = RUL_DSCRITIVO);
              WIsToSelect = *ON;
           ENDIF;
           READE (WCodigo) RINTT032;
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
                    WRec#Recibo = COB_GCNREC;
                    IF (WRec#Last = WRef#Last);
                       WIcAchou  = *ON;
                       WNumRecibo  = COB_GCNREC;
                       WNumSegura  = COB_GCNSEG;
                       EXSR Processar;
                       LEAVE;
                    ENDIF;
                    READE (WCdRamo:WNuApolice) GCOBGRH4;
                 ENDDO;
              ENDIF;
              IF (WIcAchou);
                 LEAVE;
              ENDIF;
           ENDDO;
        ENDIF;

        //===================================================================
        // Subrotina.: Processar
        // Objectivo.: Processar os registos
        //===================================================================
        BEGSR Processar;
           WIcExiste  = *OFF;
           CHAIN (WNumRecibo) RINTT031;
           IF (%FOUND(FINTT031));
              WIcExiste = *ON;
           ELSE;
              OUT_NUMREC  = WNumRecibo;
              OUT_TIPMOV  = RUL_TIPRECIBO;
              OUT_SITMOV  = RUL_SITRECIBO;
              OUT_NUAPOL  = WNuApolice;
              OUT_CDRAMO  = WCdRamo;
              OUT_NUSEGU  = WNumSegura;
              OUT_DTMOVI  = %DEC(INT_DATA:8:0);
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
              WTotValAlfa = INT_DEBITO;
           ENDIF;
           EXSR FixString;
           WTotValor = %DEC(WTotValAlfa:11:2);

           IF (RUL_INCIDENC = 'R');
              OUT_TOTREC  += WTotValor;
           ELSEIF (RUL_INCIDENC = 'C');
              OUT_TOTCOM  += WTotValor;
           ELSEIF (RUL_INCIDENC = 'O');
              OUT_OTHERS  += WTotValor;
           ENDIF;

           IF (WIcExiste);
              UPDATE RINTT031 %FIELDS(OUT_TOTREC:OUT_TOTCOM:OUT_OTHERS:
                                      OUT_MSGERRO);
           ELSE;
              WRITE RINTT031;
           ENDIF;

           INT_NUMREC = WNumRecibo;
           INT_CDRAMO = WCdRamo;
           INT_NUAPOL = WNuApolice;
           INT_ICCONV = 'S';
           UPDATE RINTT030 %FIELDS(INT_NUMREC:INT_CDRAMO:INT_NUAPOL:INT_ICCONV);
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
