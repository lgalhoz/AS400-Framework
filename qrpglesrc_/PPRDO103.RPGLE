     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PRDO103
     H* Descrição.: Parameterizar Limites e Franquias para as Apólices
     H*             pode trabalhar a diferentes níveis, conforme o que
     H*             estiver parameterizado no Catálogo.
     H*==================================================================
     FFCATP102  IF   E           K DISK    PREFIX(CAT_)
     FFCATP103  IF   E           K DISK    PREFIX(CAT_)
     FFCATP10301IF   E           K DISK    PREFIX(CAT_)
     F                                     RENAME(RCATP103:RCATP10301)
     FFPRDP01401IF   E           K DISK    PREFIX(PRD_)
     F                                     RENAME(RPRDP014:RPRDP01401)
     FFPRDP014  UF A E           K DISK    PREFIX(PRD_)
     FFPRDE103  CF   E             WORKSTN SFILE(RPRDE10302:REC#)
     F                                     PREFIX(ECR_)
     F                                     INFDS(INFD#KEYS)
     F*==========================================================
     F* Indicadores  Descrição
     F* -----------  --------------------------------------------
     F*      03      Tecla F3
     F*      06      Tecla F6
     F*      12      Tecla F12
     F*      45      IO Condicional - Capital
     F*      46      IO Condicional - Texto
     F*      50      Tecla PagDn
     F*      51      Tecla PagUp
     F*      52      DSP
     F*      53      CLR
     F*      54      END
     D*==========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SCATO010
      /Copy QRPGLESRC,SSECO010

     D*==> Estrutura de dados do Registo
     D RBASEDIO      E DS                  ExtName(GBASEDIO)
     D                                     Prefix(OR_)
     D                                     Based(ptrOR)
     D
     D*==> Gerir Limites e Franquias
     D MntLimites      PR             5S 0
     D    CdProduto                   5S 0 CONST
     D    CdRamo                      2S 0 CONST
     D    NuApolice                   7S 0 CONST
     D    CdSuper                     5S 0 CONST
     D
     D*==> Gerir o Detalhe
     D MntDetalhe      PR             5S 0
     D    PRM_CdProd                  5S 0 CONST
     D    PRM_CdRamo                  2S 0 CONST
     D    PRM_Apolice                 7S 0 CONST
     D    PRM_CdItem                  5S 0 CONST
     D    PRM_NuSequ                  5S 0 CONST
     D
     D*==> Pré-visualização das Actas
     D  PrintPreview   PR                  EXTPGM('RPRDO005')
     D   ptrOR                         *   CONST
     D   Cdfunction                   5S 0 CONST
     D
     D*==> Tem valores do Utilizador
     D IsUserDefined   PR             5S 0
     D   PRM_CdProd                   5S 0 CONST
     D   PRM_CdRamo                   2S 0 CONST
     D   PRM_Apolice                  7S 0 CONST
     D   PRM_CdLimite                 5S 0 CONST
     D
     D*==> Validar se é para seleccionar
     D IsToSelect      PR              N
     D   PRM_CdProd                   2S 0 CONST
     D   PRM_CdCobert                 5S 0 CONST
     D   PRM_CdRisco                  5S 0 CONST
     D
     D*==> Parametros recebidos
     D WNome           S             40A
     D WCodProduto     S              5S 0
     D WpLstRisco      S               *
     D
     D WIcErro         S               N   INZ(*OFF)
     D WIcOrigem       S              1A
     D REC#            S              4S 0
     D
     D #LstLimites     DS                  QUALIFIED DIM(50)
     D   CdLimite                          LIKE(CAT_CDLIMITE)
     D   CdSuper                           LIKE(CAT_CDSUPER)
     D   CdCobert                          LIKE(CAT_CDCOBERT)
     D   CdRisco                           LIKE(CAT_CDRISCO)
     D   DsLimite                          LIKE(CAT_DSLIMITE)
     D   CdIncInd                          LIKE(CAT_CDINCIND)
     D   VlPerInd                          LIKE(CAT_VLPERIND)
     D   VlMaxInd                          LIKE(CAT_VLMAXIND)
     D   CdIncFrq                          LIKE(CAT_CDINCFRQ)
     D   VlPerFrq                          LIKE(CAT_VLPERFRQ)
     D   VlMinFrq                          LIKE(CAT_VLMINFRQ)
     D   DtInicio                          LIKE(CAT_CAT10301)
     D   DtTermo                           LIKE(CAT_CAT10302)
     D   IcEmpty                       N   INZ(*ON)
     D
     D WIdxRisco       S              5S 0
     D WIdx            S              5S 0
     D WIcExtract      S               N
     D WIcDisplay      S               N
     D WModo           S               N
     D WExceptKeys     S             24A   INZ(*BLANKS)
     D DynamicCall     S             10A   INZ(*BLANKS)
     D WMoreText       S              4A   INZ(*BLANKS)
     D WDatInicio      S              8S 0
     D WptrOr          S               *
     C*========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCodProduto
     C                   PARM                    WDatInicio
     C                   PARM                    WPtrOR
     C                   PARM                    WModo
      /FREE

       //------------------------------------
       // Se não estiver autorizado então sai
       //------------------------------------
       IF NOT $ValAccess(S#PGM:'':99999);
          *INLR = *ON;
          RETURN;
       ENDIF;

       ptrOr       = WptrOR;
       ECR_NOMSEG  = OR_SEG;
       ECR_RAMO    = OR_RAMO;
       ECR_APOLICE = OR_Apol;
       WIcDisplay = WModo; // *ON
       MntLimites(WCodProduto:ECR_RAMO:ECR_APOLICE:*ZEROS);

       *INLR = *ON;
       RETURN;

      /END-FREE
     P*=======================================================
     P* Processo..: MntLimites
     P* Objectivo.: Realizar a Manutenção dos Limites e Franquias
     P*=======================================================
     P MntLimites      B                   EXPORT
     D MntLimites      PI             5S 0
     D    PRM_CdProd                  5S 0 CONST
     D    PRM_CdRamo                  2S 0 CONST
     D    PRM_Apolice                 7S 0 CONST
     D    PRM_CdSuper                 5S 0 CONST
     D
     D    WResult      S              5S 0
     D    WIcLoad      S               N   INZ(*OFF)
     D    WSelRec      S              4S 0
     D    WIcDetail    S              1A
     D    WCdLimite    S              5S 0
     D    WNuSqSup     S              5S 0
     D    WIcChanged   S              1A
     D    WNoFunction  S             10A
     D    WNoApp       S             10A
      /FREE
       WIcLoad  = *ON;
       DOW (NOT *IN03 AND NOT *IN12);
          EXSR ShowCapRisco;
          //-----------------------------------------------
          // Obter registos em função do Nível
          //-----------------------------------------------
          IF (WIcLoad);
             EXSR InzRecTxt; // Limpar a Lista de Ecrã
             EXSR  GetRecTexto; // Carregar Lista com Registos
             WIcLoad = *OFF;
          ENDIF;

          //-----------------------------------------------
          // Condicionar apresentação em função do Nível
          //-----------------------------------------------
          IF (PRM_CdSuper = *ZEROS);
             WNoFunction = 'RPRDE10303'; // Manutenção de Textos
             IF (WIcDisplay);
                WExceptKeys = *BLANKS;
             ELSE;
                WExceptKeys = '000000000000000000001'; // F21 Off
             ENDIF;
          ELSE;
             WNoFunction = 'RPRDE10304'; // Detalhe
             WExceptKeys = *BLANKS;
          ENDIF;
          DS#HEADER    = $getHeader(S#PGM:WNoFunction);
          DS#FOOTER    = $getFooter(S#PGM:WNoFunction:WExceptKeys);
          ECR_S#PGM = S#PGM;
          WRITE     RPRDE10300;
          WRITE     RPRDE10399;
          EXFMT RPRDE10303;

          IF ($ValidKey(Func#Key:KEY#F21));
             //-------------------------------------------------
             // Mudar Modo IO, Para alteração
             //-------------------------------------------------
             //IF (PRM_CdSuper = *ZEROS);
             //   WIcDisplay = *OFF;
             //   WIcLoad = *ON;
             //ENDIF;
          ELSEIF (Func#Key = KEY#F14);
             MONITOR;
                PrintPreview(WPtrOr:24);
             ON-ERROR;
             ENDMON;
          ELSEIF (Func#Key = KEY#Enter);
             //-----------------------------------------------
             // Validar se foi Seleccionado algum registo
             //-----------------------------------------------
             EXSR SelRec;
          ELSEIF (Func#Key = KEY#F12);
             IF (PRM_CdSuper > *ZEROS);
                *IN12 = *OFF;
                RETURN WResult;
             ENDIF;
          ENDIF;
       ENDDO;
       RETURN WResult;

       //============================================================
       // Subrotina..: ShowCapRisco
       // Objectivo..: repor Capitais de Risco
       //============================================================
       BEGSR ShowCapRisco;
          IF (OR_CR1 > *ZEROS);
             ECR_DSRISC001 = $getDescRisco(PRM_CdProd:OR_CR1);
             ECR_CPRISC001 = OR_CAP1;
          ELSE;
            ECR_DSRISC001 = *BLANKS;
            ECR_CPRISC001 = *ZEROS;
          ENDIF;
          IF (OR_CR2 > *ZEROS);
             ECR_DSRISC002 = $getDescRisco(PRM_CdProd:OR_CR2);
             ECR_CPRISC002 = OR_CAP2;
          ELSE;
            ECR_DSRISC002 = *BLANKS;
            ECR_CPRISC002 = *ZEROS;
          ENDIF;
          IF (OR_CR3 > *ZEROS);
             ECR_DSRISC003 = $getDescRisco(PRM_CdProd:OR_CR3);
             ECR_CPRISC003 = OR_CAP3;
          ELSE;
            ECR_DSRISC003 = *BLANKS;
            ECR_CPRISC003 = *ZEROS;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: InzRecTxt
       // Objectivo.: Inicializar a lista de registos
       //========================================================
       BEGSR inzRecTxt;
          WCdLimite = *ZEROS;   // Código do Limite
          WNuSqSup  = *ZEROS;   // Elemento Superior
          *IN46     = *OFF;
          REC#      = 0;        // Controlo de Registos da Lista
          *In53     = *On;      // Limpar Lista de Ecrã
          *In52     = *On;      // Mostrar a Lista de Ecrã
          WRITE RPRDE10303; // Limpar Lista
          *In53     = *Off;     // Repor Indicador
          *In54     = *ON;     // Repor Indicador
          WIdx = 1;
          DOW (WIdx < 50);
             #LstLimites(Widx).IcEmpty  = *ON;
             WIdx += 1;
          ENDDO;
       ENDSR;

       //========================================================
       // Subrotina.: GetRecTexto
       // Objectivo.: Obter todos os registos de tabelas disponív
       //========================================================
       BEGSR getRecTexto;
          WNoApp      = *BLANKS;
          WIdx = *ZEROS;

          //------------------------------------------------------
          // Obter o elemento superior se não for a raiz
          //------------------------------------------------------
          IF (PRM_CdSuper > *ZEROS);
             CHAIN (PRM_CdProd:PRM_CdSuper) RCATP103;
             IF (%FOUND());
                IF IsToSelect(PRM_CdProd:CAT_CDCOBERT:CAT_CDRISCO);
                   WIdx += 1;
                   #LstLimites(Widx).CdLimite = CAT_CdLimite;
                   #LstLimites(Widx).CdSuper  = CAT_CdSuper;
                   #LstLimites(Widx).CdCobert = CAT_CdCobert;
                   #LstLimites(Widx).CdRisco  = CAT_CdRisco;
                   #LstLimites(Widx).DsLimite = CAT_DsLimite;
                   #LstLimites(Widx).CdIncInd = CAT_CdIncInd;
                   #LstLimites(Widx).VlPerInd = CAT_VlPerInd;
                   #LstLimites(Widx).VlMaxInd = CAT_VlMaxInd;
                   #LstLimites(Widx).CdIncFrq = CAT_CdIncFrq;
                   #LstLimites(Widx).VlPerFrq = CAT_VlPerFrq;
                   #LstLimites(Widx).VlMinFrq = CAT_VlMinFrq;
                   #LstLimites(Widx).DtInicio = CAT_CAT10301;
                   #LstLimites(Widx).DtTermo  = CAT_CAT10302;
                   #LstLimites(Widx).IcEmpty  = *OFF;
                ENDIF;
             ENDIF;
          ENDIF;

          //------------------------------------------------------
          // Obter os dependentes directos
          //------------------------------------------------------
          IF (PRM_CDSUPER > *ZEROS);
             SETLL (PRM_CdProd:PRM_CdSuper) RCATP10301;
             READE (PRM_CdProd:PRM_CdSuper) RCATP10301;
          ELSE;
             SETLL (PRM_CdProd) RCATP10301;
             READE (PRM_CdProd) RCATP10301;
          ENDIF;
          DOW (NOT %EOF());
             IF (PRM_CDSUPER=*ZEROS AND (CAT_CDLIMITE = CAT_CDSUPER)) OR
                (PRM_CDSUPER <> *ZEROS AND (CAT_CDLIMITE<> CAT_CDSUPER));
                IF IsToSelect(PRM_CdProd:CAT_CDCOBERT:CAT_CDRISCO);
                   WIdx += 1;
                   #LstLimites(Widx).CdLimite = CAT_CdLimite;
                   #LstLimites(Widx).CdSuper  = CAT_CdSuper;
                   #LstLimites(Widx).CdCobert = CAT_CdCobert;
                   #LstLimites(Widx).CdRisco  = CAT_CdRisco;
                   #LstLimites(Widx).DsLimite = CAT_DsLimite;
                   #LstLimites(Widx).CdIncInd = CAT_CdIncInd;
                   #LstLimites(Widx).VlPerInd = CAT_VlPerInd;
                   #LstLimites(Widx).VlMaxInd = CAT_VlMaxInd;
                   #LstLimites(Widx).CdIncFrq = CAT_CdIncFrq;
                   #LstLimites(Widx).VlPerFrq = CAT_VlPerFrq;
                   #LstLimites(Widx).VlMinFrq = CAT_VlMinFrq;
                   #LstLimites(Widx).DtInicio = CAT_CAT10301;
                   #LstLimites(Widx).DtTermo  = CAT_CAT10302;
                   #LstLimites(Widx).IcEmpty  = *OFF;
                ENDIF;
             ENDIF;
             IF (PRM_CDSUPER > *ZEROS);
                READE (PRM_CdProd:PRM_CdSuper) RCATP10301;
             ELSE;
                READE (PRM_CdProd) RCATP10301;
             ENDIF;
          ENDDO;

          WIdx = 1;
          DOW (NOT #LstLimites(Widx).IcEmpty);
             //----------------------------------------------
             // Validar se existem dependentes
             //----------------------------------------------
             WMoreText = *BLANKS;
             ECR_ICDETAIL = 'N';
             IF (PRM_CDSUPER=*ZEROS) OR (PRM_CDSUPER>*ZEROS AND WIDX > 1);
                SETLL (PRM_CdProd:#LstLimites(Widx).CdLimite) RCATP10301;
                READE (PRM_CdProd:#LstLimites(Widx).CdLimite) RCATP10301;
                DOW (NOT %EOF());
                   IF (CAT_CDSUPER <> CAT_CDLIMITE);
                      WMoreText = '...';
                      ECR_ICDETAIL = 'S';
                      LEAVE;
                   ENDIF;
                   READE (PRM_CdProd:#LstLimites(Widx).CdLimite) RCATP10301;
                ENDDO;
             ENDIF;

             //---------------------------------------------------
             // Formatar a descrição
             //---------------------------------------------------
             CHAIN (PRM_CdProd:#LstLimites(Widx).CdCobert) RCATP102;
             IF (%FOUND());
             ECR_OPTION   = *BLANKS;
                IF (CAT_CDNATURA = 'B');
                   ECR_DSTIPO   = 'Base';
                ELSE;
                   ECR_DSTIPO   = 'Facultativa';
                ENDIF;
                ECR_DSLIMITE = %TRIM(WMoreText) + CAT_DSCOBERT;
             ENDIF;
             IF (#LstLimites(Widx).DsLimite <> *BLANKS);
                ECR_DSLIMITE = %TRIM(WMoreText)+#LstLimites(Widx).DsLimite;
             ENDIF;

             //---------------------------------------------------
             //
             //---------------------------------------------------
             ECR_NUSEQU = IsUserDefined(PRM_CDPROD:PRM_CdRamo:PRM_APOLICE:
                                            #LstLimites(Widx).CdLimite);
             IF (ECR_NUSEQU > *ZEROS);
                ECR_DSVALOR = '<utilizador>';
                *IN45 = WIcDisplay;
                CHAIN (PRM_CdRamo:PRM_APOLICE:
                         #LstLimites(Widx).CdLimite) RPRDP01401;
                IF (%FOUND());
                   ECR_NUSEQU = PRD_NUSEQU;
                ELSE;
                   ECR_NUSEQU = *ZEROS;
                   IF (#LstLimites(Widx).CdLimite = PRM_CDSUPER);
                      ECR_DSVALOR  = '<produto>';
                      *IN45 = *ON;
                   ENDIF;
                ENDIF;
             ELSE;
                ECR_DSVALOR  = '<produto>';
                *IN45 = *ON;
             ENDIF;

             ECR_CDPROD   = PRD_CDRAMO;
             ECR_CDLIMITE = #LstLimites(Widx).CdLimite;
             ECR_CDSUPER  = #LstLimites(Widx).CdSuper;
             REC#     += 1;
             WRITE RPRDE10302;
             WIdx += 1;
          ENDDO;
       ENDSR;

       //========================================================
       // Subrotina.: SelRec
       // Objectivo.: Seleccionar os registos
       //========================================================
       BEGSR selRec;
          WSelRec    = *ZEROS;
          WIcChanged = 'N';
          READC RPRDE10302;
          DOW (NOT %EOF());
             READC RPRDE10302;
             IF (ECR_OPTION <> *BLANKS);
                IF (ECR_IcDetail = 'S');
                   MntLimites(PRM_CdProd:PRM_CdRamo:PRM_APOLICE:
                              ECR_CDLIMITE);
                   WIcLoad = *ON;
                ELSE;
                   MntDetalhe(PRM_CdProd:PRM_CdRamo:PRM_APOLICE:
                              ECR_CDLIMITE:ECR_NUSEQU);
                   WIcLoad = *ON;
                   *IN12 = *OFF;
                   LEAVE;
                ENDIF;
             ENDIF;
          ENDDO;
       ENDSR;
      /END-FREE
     P MntLimites      E
     P*=======================================================
     P* Processo..: MntDetalhe
     P* Objectivo.: Realizar a Manutenção dos Detalhe
     P*=======================================================
     P MntDetalhe      B                   EXPORT
     D MntDetalhe      PI             5S 0
     D    PRM_CdProd                  5S 0 CONST
     D    PRM_CdRamo                  2S 0 CONST
     D    PRM_Apolice                 7S 0 CONST
     D    PRM_CdItem                  5S 0 CONST
     D    PRM_NuSequ                  5S 0 CONST
     D WCallStatus     S              1A
     D WRow            S              3P 0
     D WCol            S              3P 0
     D WResult         S              5S 0
     D WIcUser         S               N
     D WIcErro         S               N
      /FREE

       WIcUser = *OFF;  // Valores do Utilizador
       EXSR GetData;
       DOW (NOT *IN03 AND NOT *IN12);
          DS#HEADER    = $getHeader(S#PGM:'RPRDE10305');
          IF (WIcDisplay);
             *IN46 = *OFF;
             WExceptKeys = '0001000000000000000001'; // F22 Off
          ELSE;
             *IN46 = *ON;
             IF (NOT WIcUser);
                WExceptKeys = '0000000000000000000001'; // F22 Off
             ELSE;
                WExceptKeys = *BLANKS;
             ENDIF;
          ENDIF;
          ECR_DSINCIND = $getDescricao(23:ECR_CDINCIND);
          ECR_DSINCFRQ = $getDescricao(24:ECR_CDINCFRQ);
          DS#FOOTER = $getFooter(S#PGM:'RPRDE10305':WExceptKeys);
          WRITE RPRDE10300;
          WRITE RPRDE10399;
          EXFMT RPRDE10305;
          IF (Func#Key = KEY#F04) AND (NOT WIcDisplay);
             WRow    = ECR_CM#CROW;
             WCol    = ECR_CM#CCOL;
             EXSR PopUp;
             ECR_CM#ROW  = WRow;
             ECR_CM#COL  = WCol;
          ELSEIF ($ValidKey(Func#Key:KEY#F22)) AND (NOT WIcDisplay)
                      AND (WIcUser);
             ECR_OPTION = 'N';
             EXFMT RPRDE10322;
             IF (ECR_OPTION = 'S');
                DELETE (OR_Ramo:OR_Apol:PRM_NuSequ) RPRDP014;
             ENDIF;
             *IN12 = *ON;
          ELSEIF (Func#Key = KEY#Enter) AND (NOT WIcDisplay);
             EXSR UpdRecord;
          ENDIF;
       ENDDO;
       RETURN WResult;

       //========================================================
       // Subrotina.: getData
       // Objectivo.: Obter dados para Consulta
       //========================================================
       BEGSR getData;
         CHAIN (PRM_CdProd:PRM_CdItem) RCATP103;
         IF (%FOUND());
            ECR_CDINCIND  = CAT_CDINCIND;
            ECR_VLPERIND  = CAT_VLPERIND;
            ECR_VLMAXIND  = CAT_VLMAXIND;
            ECR_CDINCFRQ  = CAT_CDINCFRQ;
            ECR_VLMINFRQ  = CAT_VLMINFRQ;
            ECR_VLPERFRQ  = CAT_VLPERFRQ;
            ECR_DSLIMITE_ = CAT_DSLIMITE;
            IF (CAT_DSLIMITE = *BLANKS);
               CHAIN (PRM_CdProd:CAT_CDCOBERT) RCATP102;
               IF (%FOUND());
                  ECR_DSLIMITE_ = CAT_DSCOBERT;
               ELSE;
                  *IN12 = *OFF; // Dados Inconsistentes
               ENDIF;
            ENDIF;
         ELSE;
            *IN12 = *OFF; // Dados Inconsistentes
         ENDIF;

         IF (PRM_NuSequ > *ZEROS);
            CHAIN(N) (PRM_CdRamo:PRM_Apolice:PRM_NuSequ) RPRDP014;
            IF (%FOUND());
               WIcUser       = *ON;
               ECR_CDINCIND  = PRD_CDINCIND;
               ECR_VLPERIND  = PRD_VLPERIND;
               ECR_VLMAXIND  = PRD_VLMAXIND;
               ECR_CDINCFRQ  = PRD_CDINCFRQ;
               ECR_VLMINFRQ  = PRD_VLMINFRQ;
               ECR_VLPERFRQ  = PRD_VLPERFRQ;
            ELSE;
               *IN12 = *OFF; // Dados Inconsistentes
            ENDIF;
         ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: UpdRecord
       // Objectivo.: Actualizar o registo
       //========================================================
       BEGSR UpdRecord;
          IF (WIcUser);  // Actualizar ou alterar o registo
             CHAIN (PRM_CdRamo:PRM_Apolice:PRM_NuSequ) RPRDP014;
             PRD_CDINCIND = ECR_CDINCIND;
             PRD_VLPERIND = ECR_VLPERIND;
             PRD_VLMAXIND = ECR_VLMAXIND;
             PRD_CDINCFRQ = ECR_CDINCFRQ;
             PRD_VLPERFRQ = ECR_VLPERFRQ;
             PRD_VLMINFRQ = ECR_VLMINFRQ;
             UPDATE RPRDP014 %FIELDS(PRD_VLPERIND:PRD_VLMAXIND:PRD_CDINCIND:
                                     PRD_VLPERFRQ:PRD_VLMINFRQ:PRD_CDINCFRQ);
          ELSE;
             SETGT (PRM_CdRamo:PRM_APOLICE) RPRDP014;
             READP(N) RPRDP014;
             PRD_CDRAMO   = PRM_CdRAMO;
             PRD_NUAPOL   = PRM_Apolice;
             PRD_NUSEQU   = PRD_NUSEQU + 1;
             PRD_CDLIMITE = PRM_CDITEM;
             PRD_CDINCIND = ECR_CDINCIND;
             PRD_VLPERIND = ECR_VLPERIND;
             PRD_VLMAXIND = ECR_VLMAXIND;
             PRD_CDINCFRQ = ECR_CDINCFRQ;
             PRD_VLPERFRQ = ECR_VLPERFRQ;
             PRD_VLMINFRQ = ECR_VLMINFRQ;
             WRITE RPRDP014;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: popup
       // Objectivo.: Afixar uma lista de valoers possíveis
       //========================================================
       BEGSR popup;
          //----------------------------------------------
          // Validar qual o campo onde foi seleccionado
          //----------------------------------------------
          IF (ECR_CM#NMFLD = 'CDINCIND');
             ECR_CDINCIND = $runPopUp(23:ECR_CDINCIND);
          ELSEIF (ECR_CM#NMFLD = 'CDINCFRQ');
             ECR_CDINCFRQ = $runPopUp(24:ECR_CDINCFRQ);
          ENDIF;
       ENDSR;
      /END-FREE
     P MntDetalhe      E
     P*=======================================================
     P* Processo..: IsUserDefined
     P* Objectivo.: Valores definidos pelo Utilizador
     P*=======================================================
     P IsUserDefined   B                   EXPORT
     D IsUserDefined   PI             5S 0
     D   PRM_CdProd                   5S 0 CONST
     D   PRM_CdRamo                   2S 0 CONST
     D   PRM_Apolice                  7S 0 CONST
     D   PRM_CdLimite                 5S 0 CONST
     D
     D #Lista          DS                  QUALIFIED DIM(50)
     D   CdLimite                          LIKE(CAT_CDLIMITE)
     D   CdSuper                           LIKE(CAT_CDSUPER)
     D   IsEmpty                       N   INZ(*ON)
     D WIdx            S              5S 0
     D WResult         S              5S 0
      /FREE
         WResult = *ZEROS;
         CHAIN (PRM_CDRamo:PRM_APOLICE:PRM_CdLimite) RPRDP01401;
         IF (%FOUND());
            WResult = PRD_NUSEQU;
         ELSE;
            WIdx = *ZEROS;
            SETLL (PRM_CDPROD:PRM_CdLimite) RCATP10301;
            READE (PRM_CDPROD:PRM_CdLimite) RCATP10301;
            DOW (NOT %EOF());
               IF (CAT_CDLIMITE <> CAT_CDSUPER);
                  WIdx += 1;
                  #Lista(WIdx).CdLimite = CAT_CDLIMITE;
                  #Lista(WIdx).CdSuper  = CAT_CDSUPER;
                  #Lista(WIdx).IsEmpty  = *OFF;
               ENDIF;
               READE (PRM_CDPROD:PRM_CdLimite) RCATP10301;
            ENDDO;

            WIdx = 1;
            DOW (NOT #Lista(WIdx).IsEmpty);
               WResult = IsUserDefined(PRM_CDPROD:PRM_CdRamo:PRM_APOLICE
                               :#Lista(Widx).CdLimite);
               IF (WResult > *ZEROS);
                  LEAVE;
               ENDIF;
               WIdx += 1;
            ENDDO;
         ENDIF;
         RETURN WResult;
      /END-FREE
     P IsUserDefined   E
     P*=======================================================
     P* Processo..: IsToSelect
     P* Objectivo.: Seleccionar o regisot de limite e Franquia
     P*             caso esteja contratado.
     P* Observação: Código Isolado para prever a inclusão dos
     P*             novos Módulos de Gestão
     P*=======================================================
     P IsToSelect      B                   EXPORT
     D IsToSelect      PI              N
     D   PRM_CdProd                   2S 0 CONST
     D   PRM_CdCobert                 5S 0 CONST
     D   PRM_CdRisco                  5S 0 CONST
     D
     D WOrigem         S              1A   INZ('D')
     D WResult         S               N
      /FREE
         WResult = *OFF;

         //-----------------------------------------------
         // Validar se o Risco está contratado
         //-----------------------------------------------
         IF (PRM_CdRisco <> *ZEROS);
            IF (OR_CR1 = PRM_CdRisco) OR (OR_CR2 = PRM_CdRisco) OR
               (OR_CR3 = PRM_CdRisco);
               WResult = *ON;
            ENDIF;
         ELSE;
            WResult = *ON;
         ENDIF;

         //-----------------------------------------------
         // Validar se a Cobertura faz parte do Contrato
         //-----------------------------------------------
         IF (WResult = *ON);
            CHAIN (PRM_CdProd:PRM_CdCobert) RCATP102;
            IF (%FOUND());
               IF (CAT_CDNATURA <> 'B');
                  IF (CAT_ALIAS = '01') AND (OR_CF01 <> 'S');
                     WResult = *OFF;
                  ELSEIF (CAT_ALIAS = '02') AND (OR_CF02 <> 'S');
                     WResult = *OFF;
                  ELSEIF (CAT_ALIAS = '03') AND (OR_CF03 <> 'S');
                     WResult = *OFF;
                  ELSEIF (CAT_ALIAS = '04') AND (OR_CF04 <> 'S');
                     WResult = *OFF;
                  ELSEIF (CAT_ALIAS = '05') AND (OR_CF05 <> 'S');
                     WResult = *OFF;
                  ELSEIF (CAT_ALIAS = '06') AND (OR_CF06 <> 'S');
                     WResult = *OFF;
                  ELSEIF (CAT_ALIAS = '07') AND (OR_CF07 <> 'S');
                     WResult = *OFF;
                  ELSEIF (CAT_ALIAS = '08') AND (OR_CF08 <> 'S');
                     WResult = *OFF;
                  ELSEIF (CAT_ALIAS = '09') AND (OR_CF09 <> 'S');
                     WResult = *OFF;
                  ELSEIF (CAT_ALIAS = '10') AND (OR_CF10 <> 'S');
                     WResult = *OFF;
                  ELSEIF (CAT_ALIAS = '11') AND (OR_CF11 <> 'S');
                     WResult = *OFF;
                  ELSEIF (CAT_ALIAS = '12') AND (OR_CF12 <> 'S');
                     WResult = *OFF;
                  ELSEIF (CAT_ALIAS = '13') AND (OR_CF13 <> 'S');
                     WResult = *OFF;
                  ENDIF;
               ENDIF;
            ENDIF;
         ENDIF;
         RETURN WResult;
      /END-FREE
     P IsToSelect      E
