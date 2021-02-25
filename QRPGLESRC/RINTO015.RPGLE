     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*===========================================================
     H* Programa..: RINTO015
     H* Descrição.: Completar a informação dos recibos, com o que
     H*             já está carregado em Demo Comp, Lda (Corretor)
     H*             Obter o respectivo número de Ordem.
     H*
     H*===========================================================
     FSD1RE2    IF   E           K DISK    PREFIX(REC_)
     FFINTP028  UF A E           K DISK    PREFIX(INT_)
     FFINTP029  UF A E           K DISK    PREFIX(INT_)
     FFINTP021  UF A E           K DISK    PREFIX(REL_)
     F*===========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010

     D*==> Campos de Trabalho
     D WIcLigou        S               N
     D WDataYMD        S              8S 0
     D WCdRegis        S              9S 0
     D WCodErro        S              7A
     D WConvRamo       S              3S 0
     D WNuApolice      S             10A
     D WCodCompanhia   S              3S 0
     D WNumRecibo      S              9A
     D WEspacos        S              9A   INZ(*BLANKS)
     D WDim            S              2S 0
     D
     D*==> Campos de Trabalho
     D REC#Indemniza   C                   '7'
     D*===========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCdRegis
     C                   PARM                    WCodErro
     C
      /FREE
        //-----------------------------------------------------------------
        // Validar se existem registos sem número de Ordem
        //-----------------------------------------------------------------
        WIcLigou = *OFF;
        WCodErro = *BLANKS;
        CHAIN (WCdRegis) RINTP028;
        IF (%FOUND(FINTP028));
           IF (INT_CDESTAD = '2') OR (INT_CDESTAD = '3');
              WConvRamo     = INT_CDRAMO;
              WCodCompanhia = INT_CCOMP;
              WNuApolice    = INT_NUAPOL;
              CHAIN(N) (INT_CCOMP:INT_CDRMORG:INT_NUAPOLOR) RINTP021;
              IF (%FOUND(FINTP021));
                 WConvRamo     = REL_CDRAMO;
                 WCodCompanhia = REL_CDCOMP;
                 WNuApolice    = REL_NUAPOLJM;
              ENDIF;

              WNumRecibo = INT_NUMREC;
              EXSR Pesquisar;

              IF (INT_TPRECIB = REC#Indemniza);
                 WNumRecibo = %TRIM(INT_NUMRECOR);
                 EXSR Pesquisar;
              ENDIF;

              //------------------------------------------------
              // Procurar Chouriçadas
              //------------------------------------------------

              IF (NOT WIcLigou);
                 WNumRecibo = '0' + %TRIM(WNumRecibo);
                 EXSR Pesquisar;
              ENDIF;
              IF (NOT WIcLigou);
                 WNumRecibo = '0' + %TRIM(WNumRecibo);
                 EXSR Pesquisar;
              ENDIF;
              IF (NOT WIcLigou);
                 WNumRecibo = '0' + %TRIM(WNumRecibo);
                 EXSR Pesquisar;
              ENDIF;

              IF (NOT WIcLigou) AND (INT_CDESTAD = '2');
                 INT_CDESTAD = '3';
                 UPDATE RINTP028 %FIELDS(INT_CDESTAD);
                 INT_NUOPERA  = $GetNextKey('FINTP029':'NUOPERA':
                             '(CDREGIS = '''+%CHAR(INT_CDREGIS)+''')');
                 INT_CDERRO  = WCodErro;
                 INT_DTOPERA = %DATE();
                 INT_HROPERA = %TIME();
                 INT_USOPERA = S#USR;
                 WRITE RINTP029;
              ENDIF;
           ENDIF;
        ENDIF;

        *INLR = *ON;
        //==============================================================
        // Subrotina..: Pesquisar
        // Objectivo..: Pesquisar a existência do Recibo
        // Observação.: Para prever situações da mesma numeração de
        //              recibo na mesma companhia, passou-se a validar
        //              pelo Ramo, Apólice e valor do recibo.
        //==============================================================
        BEGSR Pesquisar;
           WDim = %LEN(%TRIM(WNumRecibo));
           WNumRecibo = %SUBST(WEspacos:1:(9-WDim)) + %TRIM(WNumRecibo);
           SETLL (WCodCompanhia:WNumRecibo) SD1RE2;
           READE (WCodCompanhia:WNumRecibo) SD1RE2;
           DOW (NOT %EOF(SD1RE2));
              IF (REC_APOLIC = INT_NUAPOL) AND (REC_CRAMO = INT_CDRAMO)
                AND (REC_VTREC = INT_IMTOTREC);
                 EXSR Connection;
                 IF (REC_APOLIC = WNuApolice);
                    LEAVE;
                 ENDIF;
              ENDIF;
              READE (WCodCompanhia:WNumRecibo) SD1RE2;
           ENDDO;
        ENDSR;

        //==============================================================
        // Subrotina.: Connection
        // Objectivo.: Regiar Informação da Ligação
        //==============================================================
        BEGSR Connection;
           INT_NUMORDRC = REC_NORD2;
           IF (REC_DTCOB = *BLANKS);
              REC_DTCOB = *ZEROS;
           ENDIF;
           IF (REC_DTDEV = *BLANKS);
              REC_DTDEV = *ZEROS;
           ENDIF;

           //---------------------------------------
           // Validar o estado do Recibo
           //---------------------------------------
           IF (REC_CODSIT = 2);
              INT_CDESTAD = '7'; // Cobrado
              WDataYMD    = %DEC(REC_DTCOB:6:0);
              WDataYMD    = $CvtData(WDataYMD:'DMY':'YYMD');
           ELSEIF (REC_CODSIT = 1) OR (REC_CODSIT = 3);
              INT_CDESTAD = '9'; // Anulado
              WDataYMD    = %DEC(REC_DTDEV:6:0);
              WDataYMD    = $CvtData(WDataYMD:'DMY':'YYMD');
           ELSE;
              WDataYMD = *ZEROS;
              INT_CDESTAD = '5'; // Pendente
           ENDIF;
           INT_DTPAGAAN = $IntToDate(WDataYMD);

           UPDATE RINTP028 %FIELDS(INT_NUMORDRC:INT_CDESTAD:
                                   INT_DTPAGAAN);
           WIcLigou = *ON;
        ENDSR;
      /END-FREE
