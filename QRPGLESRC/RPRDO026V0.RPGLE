     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*===========================================================
     H* Programa..: RPRDO026
     H* Objectivo.: Validar se o Segurado é mau pagador
     H* Descrição.: Pesquisar em José Mata (Corretor), se o Segurado
     H*             tem recibos e apólices anuladas por falta de
     H*             pagamento, ou recibos pagos fora do prazo de
     H*             pagamento.
     H*===========================================================
     FSD1RE1    IF   F  326    13AIDISK    KEYLOC(32) EXTFILE('SD.1.RE1')
     FGFSEG     IF   E           K DISK    PREFIX(SEG_)
     FGFSEG3    IF   E           K DISK    PREFIX(SEG_) RENAME(FSEGF1:FSEGF3)
     FGBASEGRL  IF   E           K DISK    PREFIX(PRD_)
     F*===========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010

     D*==> Formatos de Registo
     D R#SD1RE1      E DS                  EXTNAME('SD.1.RE1') PREFIX('REC_')
     D K#SD1RE1        DS
     D   K#COMP                1     13S 0
     D   K#CSEGUR              1      6S 0
     D   K#NORD2               7     13S 0
     D
     D*==> Lista de Registos onde é Segurado
     D Lst#Segurado    DS                  QUALIFIED DIM(25)
     D   CodSeg                       8S 0
     D   IsEmpty                       N
     D WIdxSeg         S              3S 0
     D
     D*==> Lista de Recibos com anomalias
     D Lst#Recibo      DS                  QUALIFIED DIM(100)
     D   NumRecibo                    9S 0
     D   DatLimite                    8S 0
     D   DatPagame                    8S 0
     D   DifDias                      3S 0
     D   CodRamo                      2S 0
     D   NuApol                       7S 0
     D   SitApolic                    1S 0
     D   CodMotivo                    3S 0
     D   IsEmpty                       N
     D WIdxRec         S              3S 0
     D
     D*==> Campos de Trabalho
     D WCodSegurado    S              8S 0
     D WNumNIF         S              9S 0
     D WTipSegurado    S              2A
     D WCodErro        S              7A
     D WDataLimite     S              8S 0
     D WDataPagamento  S              8S 0
     D
     D*==> Definição de Constantes
     D TIP#MauPagador  C                   'MP'
     D TIP#BomPagador  C                   'BP'
     D*===========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCodSegurado
     C                   PARM                    WTipSegurado
     C                   PARM                    WCodErro
     C
      /FREE
        //-----------------------------------------------------------------
        // Tratar Parametros Recebidos
        //-----------------------------------------------------------------
        WCodErro = *BLANKS;
        EXSR ValParametros;
        IF (WCodErro <> *BLANKS);
           RETURN;
        ENDIF;

        //-----------------------------------------------------------------
        // Obter informação dos Recibos
        //-----------------------------------------------------------------
        EXSR GetSegurado;
        EXSR ValRecibos;

        RETURN;
        *INLR = *ON;

        //=======================================================================
        // Subrotina.: ValParametros
        // Objectivo.: Validar os Parameteros Recebidos
        //=======================================================================
        BEGSR ValParametros;
           //----------------------------------------------------
           // Validar Código do Segurado
           //----------------------------------------------------
           MONITOR;
              IF (WCodSegurado <= *ZEROS);
                 WCodErro = 'PRDXXX1';
              ELSE;
                 CHAIN (WCodSegurado) FSEGF1;
                 IF (%FOUND(GFSEG));
                    WNumNIF = SEG_NIF;
                    IF (WNumNIF = *ZEROS);
                       WCodErro = 'PRDXXX5';
                    ELSE;
                       WIdxSeg = 1;
                       LST#Segurado(WIdxSeg).CodSeg = WCodSegurado;
                       LST#Segurado(WIdxSeg).IsEmpty = *OFF;
                       WIdxSeg += 1;
                       LST#Segurado(WIdxSeg).IsEmpty = *ON;
                    ENDIF;
                 ENDIF;
              ENDIF;
           ON-ERROR;
              WCodErro = 'PRDXXX2';
           ENDMON;

           //----------------------------------------------------
           // Inicializar tipo de Segurado
           //----------------------------------------------------
           MONITOR;
              WTipSegurado = *BLANKS;
           ON-ERROR;
              WCodErro = 'PRDXXX3';
           ENDMON;
        ENDSR;

        //=======================================================================
        // Subrotina.: GetSegurado
        // Objectivo.: Obter outros registos para o mesmo NIF
        //=======================================================================
        BEGSR GetSegurado;
           WIdxSeg = 2;
           SETLL (WNumNIF) FSEGF3;
           READE (WNumNIF) FSEGF3;
           DOW (NOT %EOF(GFSEG3));
              IF (SEG_NSEG <> WCodSegurado);
                 LST#Segurado(WIdxSeg).CodSeg = SEG_NSEG;
                 LST#Segurado(WIdxSeg).IsEmpty = *OFF;
                 WIdxSeg += 1;
                 LST#Segurado(WIdxSeg).IsEmpty = *ON;
              ENDIF;
              READE (WNumNIF) FSEGF3;
           ENDDO;
        ENDSR;

        //=======================================================================
        // Subrotina.: ValRecibos
        // Objectivo.: Pesquisar informação sobre os Recibos
        //=======================================================================
        BEGSR ValRecibos;
           WIdxSeg = 1;
           WIdxRec = 1;
           DOW (NOT LST#Segurado(WidxSeg).IsEmpty);
              K#CSEGUR = LST#Segurado(WidxSeg).CodSeg;
              K#NORD2  = *ZEROS;
              SETLL K#COMP SD1RE1;
              READP SD1RE1 R#SD1RE1;
              DOW (NOT %EOF(SD1RE1));
                 IF (REC_CSEGUR <> K#CSEGUR);
                    LEAVE;
                 ENDIF;

                 MONITOR;
                    WDataLimite = %DEC(REC_DTLIM:6:0);
                    WDataLimite = $CvtData(WDataLimite:'DMY':'YYMD');
                 ON-ERROR;
                    WDataLimite = *ZEROS;
                 ENDMON;

                 MONITOR;
                    WDataPagamento = %DEC(REC_DTCOB:6:0);
                    WDataPagamento = $CvtData(WDataPagamento:'DMY':'YYMD');
                 ON-ERROR;
                    WDataPagamento = *ZEROS;
                 ENDMON;

                 IF (WDataPagamento > WDataLimite) AND (WDatalimite > *ZEROS);
                    Lst#Recibo(WIdxRec).NumRecibo = %DEC(REC_NREC2:9:0);
                    Lst#Recibo(WIdxRec).DatLimite = WDataLimite;
                    Lst#Recibo(WIdxRec).DatPagame = WDataPagamento;
                    Lst#Recibo(WIdxRec).DifDias   = %DIFF(%DATE(WDataPagamento):
                                                    %DATE(WDataLimite):*DAYS);
                    Lst#Recibo(WIdxRec).CodRamo   = *ZEROS;
                    Lst#Recibo(WIdxRec).NuApol    = *ZEROS;
                    Lst#Recibo(WIdxRec).SitApolic = *ZEROS;
                    Lst#Recibo(WIdxRec).CodMotivo = *ZEROS;
                    Lst#Recibo(WIdxRec).IsEmpty   = *OFF;
                    WIdxRec += 1;
                    Lst#Recibo(WIdxRec).IsEmpty   = *ON;
                 ENDIF;
                 READP SD1RE1 R#SD1RE1;
              ENDDO;
              WIdxSeg += 1;
           ENDDO;
        ENDSR;
      /END-FREE
