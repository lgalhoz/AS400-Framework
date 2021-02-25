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
     FGCOBGR    IF   E           K DISK    PREFIX(RLS_)
     FGBASEGRL  IF   E           K DISK    PREFIX(PRD_)
     FFENTP01001IF   E           K DISK    PREFIX(ENT_)
     F                                     RENAME(RENTP010:RENTP01001)
     F*===========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SDBSO010

     D*==> Formatos de Registo
     D R#SD1RE1      E DS                  EXTNAME('SD.1.RE1') PREFIX('REC_')
     D K#SD1RE1        DS
     D   K#COMP                1     13S 0
     D   K#CSEGUR              1      6S 0
     D   K#NORD2               7     13S 0
     D
     D*==> Lista de Registos onde é Segurado
     D Lst#Segurado    DS                  QUALIFIED DIM(LMT#Segur)
     D   CodSeg                       8S 0
     D   IsEmpty                       N
     D WIdxSeg         S              3S 0
     D
     D*==> Lista de Recibos com anomalias
     D Lst#Recibo      DS                  QUALIFIED DIM(LMT#Recibo)
     D   NumRecibo                    9S 0
     D   DatRecibo                    8S 0
     D   DatLimite                    8S 0
     D   DatPagame                    8S 0
     D   DatDevol                     8S 0
     D   SitRec                       1A
     D   DifDias                      3S 0
     D   CodRamo                      2S 0
     D   NuApol                       7S 0
     D   PrevSeq                      3S 0
     D   NextSeq                      3S 0
     D   IsEmpty                       N
     D WIdxRec         S              3S 0
     D WActRec         S              3S 0
     D WFstRec         S              3S 0
     D
     D*==> Campos de Trabalho
     D WCodSegurado    S              8S 0
     D WNumNIF         S              9S 0
     D WTipSegurado    S              2A
     D WResposta       S              1A
     D WCodErro        S              7A
     D WDataRecibo     S              8S 0
     D WDataLimite     S              8S 0
     D WDataPagamento  S              8S 0
     D WDataDevol      S              8S 0
     D WCodRamo        S              2S 0
     D WNuApol         S              7S 0
     D WMensagem       S            100A
     D
     D WNumRecAct      S              9S 0
     D WNumRecIdx      S              9S 0
     D WNumRecInf      S              9S 0
     D WNumRecSup      S              9S 0
     D
     D*==> Definição de Constantes
     D TIP#Recibo      C                   15
     D TIP#MauPagador  C                   'MP'
     D TIP#BomPagador  C                   'BP'
     D STS#Pendente    C                   'P'
     D STS#Anulado     C                   'A'
     D STS#Pago        C                   'L'
     D STS#Contenc     C                   'C'
     D LMT#Segur       C                   25
     D LMT#Recibo      C                   500
     D*===========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCodSegurado
     C                   PARM                    WResposta
     C                   PARM                    WCodErro
     C
      /FREE
        //-----------------------------------------------------------------
        // Tratar Parametros Recebidos
        //-----------------------------------------------------------------
        WNumNIF   = *ZEROS;
        WResposta = 'S';
        WCodErro  = *BLANKS;
        RETURN;    // Desactivar esta instrução

        EXSR ValParametros;
        IF (WCodErro <> *BLANKS);
           RETURN;
        ENDIF;

        //-----------------------------------------------------------------
        // Extrair Informação dos Recibos e Apólices Associadas
        //-----------------------------------------------------------------
        IF (WNumNIF > *ZEROS);
           SETLL (WNumNIF) RENTP01001;
           READE (WNumNIF) RENTP01001;
           DOW (NOT %FOUND(FENTP01001));
              // Se registo activo //
              //  Mostrar mensagem PRD0215
              //  Sair com o valor 'N'
              READE (WNumNIF) RENTP01001;
           ENDDO;
           EXSR GetSegurado;
        ENDIF;
        EXSR GetRecibos;
        // EXSR SortRecibos;

        //-----------------------------------------------------------------
        // Processar a Informação
        //-----------------------------------------------------------------
        WIdxRec = 1;
        DOW (NOT LST#Recibo(WIdxRec).IsEmpty AND (WIdxRec < LMT#Recibo));
           WCodRamo = LST#Recibo(WIdxRec).CodRamo;
           WNuApol  = LST#Recibo(WIdxRec).NuApol;
           CHAIN (WCodRamo:WNuApol) GBASEGRL;
           IF (%FOUND(GBASEGRL));
              WMensagem = %EDITW(LST#Recibo(WIdxRec).CodRamo:'0  ') + '-' +
                          %CHAR(LST#Recibo(WIdxRec).NuApol);
              IF (PRD_SIT = 4);
                 WResposta = $ShowDialog('PRD0214':WMensagem:'NS');
                 RETURN;
              ELSEIF (PRD_SIT = 3) AND ((PRD_MALT = 62) OR (PRD_MALT = 81)
                 OR (PRD_MALT = 89) OR (PRD_MALT = 91));
                 WMensagem = %TRIM(WMensagem) + ', motivo:'+
                 $getDescricao(27:%TRIM(%EDITW(PRD_MALT:'0  ')));
                 WResposta = $ShowDialog('PRD0215':WMensagem:'NS');
                 RETURN;
              ENDIF;
           ENDIF;
           WIdxRec += 1;
        ENDDO;

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
                    WIdxSeg = 1;
                    LST#Segurado(WIdxSeg).CodSeg = WCodSegurado;
                    LST#Segurado(WIdxSeg).IsEmpty = *OFF;
                    WIdxSeg += 1;
                    LST#Segurado(WIdxSeg).IsEmpty = *ON;
                    WNumNIF = SEG_NIF;
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
        // Subrotina.: GetRecibos
        // Objectivo.: Pesquisar informação sobre os Recibos
        //=======================================================================
        BEGSR GetRecibos;
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
                    WDataRecibo = $CvtData(REC_DTREC:'DMY':'YYMD');
                 ON-ERROR;
                    WDataRecibo = *ZEROS;
                 ENDMON;
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
                 MONITOR;
                    WDataDevol = %DEC(REC_DTDEV:6:0);
                    WDataDevol = $CvtData(WDataPagamento:'DMY':'YYMD');
                 ON-ERROR;
                    WDataDevol = *ZEROS;
                 ENDMON;

                 Lst#Recibo(WIdxRec).NumRecibo = %DEC(REC_NREC2:9:0);
                 Lst#Recibo(WIdxRec).DatRecibo = WDataRecibo;
                 Lst#Recibo(WIdxRec).DatLimite = WDataLimite;
                 Lst#Recibo(WIdxRec).DatPagame = WDataPagamento;
                 Lst#Recibo(WIdxRec).DatDevol  = WDataDevol;
                 Lst#Recibo(WIdxRec).PrevSeq   = WIdxRec;
                 Lst#Recibo(WIdxRec).NextSeq   = WIDxRec;

                 IF (REC_CODSIT = 2) AND
                    (WDataPagamento>WDataLimite) AND (WDatalimite > *ZEROS);
                    Lst#Recibo(WIdxRec).SitRec = STS#Pendente;
                    Lst#Recibo(WIdxRec).DifDias= %DIFF(%DATE(WDataPagamento):
                                                  %DATE(WDataLimite):*DAYS);
                 ELSEIF (REC_CODSIT = 1) OR (REC_CODSIT = 3);
                    Lst#Recibo(WIdxRec).SitRec = STS#Anulado;
                 ELSE;
                    Lst#Recibo(WIdxRec).SitRec = STS#Pendente;
                 ENDIF;

                 CHAIN (Lst#Recibo(WIdxRec).NumRecibo) GCOBGR;
                 IF (%FOUND(GCOBGR));
                    Lst#Recibo(WIdxRec).CodRamo   = RLS_GCRAMO;
                    Lst#Recibo(WIdxRec).NuApol    = RLS_GCAPOL;
                    IF (Lst#Recibo(WIdxRec).SitRec = STS#Pendente) AND
                       (RLS_GCSITR = 4);
                       Lst#Recibo(WIdxRec).SitRec = STS#Contenc;
                    ENDIF;
                 ENDIF;
                 Lst#Recibo(WIdxRec).IsEmpty   = *OFF;
                 WIdxRec += 1;
                 Lst#Recibo(WIdxRec).IsEmpty   = *ON;
                 READP SD1RE1 R#SD1RE1;
              ENDDO;
              WIdxSeg += 1;
           ENDDO;
        ENDSR;

        //=======================================================================
        // Subrotina.: SortRecibos
        // Objectivo.: Ordenar os recibos descendentemente
        //=======================================================================
        BEGSR SortRecibos;
           WActRec = 1;
           DOW (NOT LST#Recibo(WActRec).IsEmpty AND (WActRec<LMT#Recibo));
              WNumRecAct = LST#Recibo(WActRec).NumRecibo;
              WNumRecSup = *ZEROS;
              WIdxRec    = WActRec + 1;
              DOW (NOT LST#Recibo(WIdxRec).IsEmpty AND (WIDxRec < LMT#Recibo));
                 WNumRecIDX = LST#Recibo(WIdxRec).NumRecibo;
                 IF (WNumRecAct <= WNumRecIdx) OR ((WNumRecSup > WNumRecIdx)
                    AND (WNumRecSup > *ZEROS));
                    WNumRecSup = LST#Recibo(WIdxRec).NumRecibo;
                    LST#Recibo(WActRec).NextSeq = WIdxRec;
                 ENDIF;
                 WIdxRec += 1;
              ENDDO;
              LST#Recibo(LST#Recibo(WactRec).NextSeq).PrevSeq = WActRec;
              WActRec += 1;
           ENDDO;
        ENDSR;
      /END-FREE
