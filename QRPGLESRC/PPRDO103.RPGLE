     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PRDO103
     H* Descrição.: Parameterizar Limites e Franquias para as Apólices
     H*             pode trabalhar a diferentes níveis, conforme o que
     H*             estiver parameterizado no Catálogo.
     H*==================================================================
     FFPRDT001  IF   E           K DISK    PREFIX(PRD_)
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
      /Copy QRPGLESRC,SGRLO013
      /Copy QRPGLESRC,SPRDO002
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SCATO010
      /Copy QRPGLESRC,SSECO010
      /Copy QRPGLESRC,SDBSO010

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
     D    PRM_IdxLst                  5S 0 CONST
     D
     D*==> Gerir Limites e Franquias
     D FillLista       PR             3S 0
     D    CdProduto                   5S 0 CONST
     D    CdRamo                      2S 0 CONST
     D    NuApolice                   7S 0 CONST
     D    CdSuper                     5S 0 CONST
     D    Identation                 12A   CONST
     D
     D*==> Pré-visualização das Actas
     D  PrintPreview   PR                  EXTPGM('RPRDO005')
     D   ptrOR                         *   CONST
     D   Cdfunction                   2A   CONST
     D   Data                         8S 0 CONST
     D
     D*==> Validar se é para seleccionar
     D IsToSelect      PR              N
     D   PRM_CdProd                   2S 0 CONST
     D   PRM_CdRamo                   2S 0 CONST
     D   PRM_NuApol                   7S 0 CONST
     D   PRM_CdCobert                 5S 0 CONST
     D   PRM_CdRisco                  5S 0 CONST
     D   PRM_CdNatura                 1A   CONST
     D
     D*==> Riscos da Apólice
     D LST#Risco       DS                  QUALIFIED DIM(10)
     D   CodRisco                     5S 0 INZ(*ZEROS)
     D   CapRisco                    11S 2 INZ(*ZEROS)
     D   IsEmpty                       N   INZ(*ON)
     D
     D*==> Lista de Limites
     D LST#Limites     DS                  QUALIFIED DIM(50)
     D   CdLimite                     5S 0
     D   CdSuper                      5S 0
     D   CdCobert                     5S 0
     D   CdRisco                      5S 0
     D   IcSelect                     1A
     D   DsLimite                   100A
     D   CdIncInd                     2A
     D   VlPerInd                     5S 2
     D   VlMaxInd                    11S 2
     D   CdIncFrq                     2A
     D   VlPerFrq                     5S 2
     D   VlMinFrq                    11S 2
     D   IcDetail                     1A
     D   CdNatura                     1A
     D   VlIndemniza                 15A
     D   VlFranquia                  15A
     D   NuSequ                       5S 0
     D   IcEmpty                       N   INZ(*ON)
     D
     D*==> Campos de Trabalho
     D REC#            S              4S 0
     D WRecAct         S              5S 0 INZ(*ZEROS)
     D WCdRisco        S              5S 0 INZ(*ZEROS)
     D WCapRisco       S             11S 2 INZ(*ZEROS)
     D WCodProduto     S              5S 0
     D WIcDisplay      S               N
     D WModo           S               N
     D WptrOr          S               *
     D WCdCobert       S              5S 0 INZ(*ZEROS)
     D WCdSuper        S              5S 0 INZ(*ZEROS)
     D WExceptKeys     S             24A   INZ(*BLANKS)
     D WIdx            S              5S 0
     D WNoApp          S             10A
     D WSessScr        S              5S 0
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
          $ShowDialog('SEC0001');
          RETURN;
       ENDIF;

       //------------------------------------
       // Preparar o ecrã
       //------------------------------------
       WSessScr = $AddScreen('FPRDE103':'RPRDE10305');
       $AddScPFld(%ADDR(ECR_F#00):'VLMAXIND');// Valor maximo de Indemnização
       $AddScPFld(%ADDR(ECR_F#01):'VLPERIND');// Percentagem de Indemnização
       $AddScPFld(%ADDR(ECR_F#02):'CDINCIND');// Código de indemnização
       $AddScPFld(%ADDR(ECR_F#03):'VLMINFRQ');// Valor minimo de Franquia
       $AddScPFld(%ADDR(ECR_F#04):'VLPERFRQ');// Percentagem da Franquia
       $AddScPFld(%ADDR(ECR_F#05):'CDINCFRQ');// Código de Franquia
       $AddScCoord(%ADDR(ECR_CM#ROW):%ADDR(ECR_CM#COL));
       $AddScMsg(%ADDR(ECR_ERR#MSG));

       ptrOr       = WptrOR;
       ECR_NOMSEG  = OR_SEG;
       ECR_RAMO    = OR_RAMO;
       ECR_APOLICE = OR_Apol;
       WIcDisplay = WModo; // *ON

       //---------------------------------------------
       // Validar se os dados estão convertidos
       //---------------------------------------------
       CHAIN (OR_RAMO:OR_APOL) RPRDP001;
       IF (%FOUND());
          IF (PRD_TPACTA = 'A');
             $ShowDialog('PRD0160');
             RETURN;
          ENDIF;
       ELSE;
          $ShowDialog('PRD0161');
          RETURN;
       ENDIF;


       LST#Risco   = $GetCapRisco(WCodProduto:OR_RAMO:OR_Apol:
                                  *ZEROS:*OFF:WDatInicio);
       EXSR ShowCapRisco;

       //----------------------------------------------
       // Invocar Função
       //----------------------------------------------
       MntLimites(WCodProduto:ECR_RAMO:ECR_APOLICE:*ZEROS);
       *IN12 = *OFF;
       *IN03 = *OFF;
       $RmvScreen(WSessScr);

       RETURN;

       //============================================================
       // Subrotina..: ShowCapRisco
       // Objectivo..: repor Capitais de Risco
       //============================================================
       BEGSR ShowCapRisco;
          ECR_DSRISC001 = *BLANKS;
          ECR_CPRISC001 = *ZEROS;
          ECR_DSRISC002 = *BLANKS;
          ECR_CPRISC002 = *ZEROS;
          ECR_DSRISC003 = *BLANKS;
          ECR_CPRISC003 = *ZEROS;
          ECR_DSRISC004 = *BLANKS;
          ECR_CPRISC004 = *ZEROS;
          WRecAct = 1;
          DOW (NOT Lst#Risco(WRecAct).IsEmpty);
             WCdRisco  = Lst#Risco(WRecAct).CodRisco;
             WCapRisco = Lst#Risco(WRecAct).CapRisco;
             IF (WCdRisco > *ZEROS);
                IF (ECR_DSRISC001 = *BLANKS);
                   ECR_DSRISC001 = $getDescRisco(WCodProduto:WCdRisco);
                   ECR_CPRISC001 = WCapRisco;
                ELSEIF (ECR_DSRISC002 = *BLANKS);
                   ECR_DSRISC002 = $getDescRisco(WCodProduto:WCdRisco);
                   ECR_CPRISC002 = WCapRisco;
                ELSEIF (ECR_DSRISC003 = *BLANKS);
                   ECR_DSRISC003 = $getDescRisco(WCodProduto:WCdRisco);
                   ECR_CPRISC003 = WCapRisco;
                ELSEIF (ECR_DSRISC004 = *BLANKS);
                   ECR_DSRISC004 = $getDescRisco(WCodProduto:WCdRisco);
                   ECR_CPRISC004 = WCapRisco;
                   LEAVE;
                ENDIF;
             ENDIF;
             WRecAct += 1;
          ENDDO;
       ENDSR;

      /END-FREE
     P*===========================================================
     P* Processo..: MntLimites
     P* Objectivo.: Realizar a Manutenção dos Limites e Franquias
     P*===========================================================
     P MntLimites      B                   EXPORT
     D MntLimites      PI             5S 0
     D    PRM_CdProd                  5S 0 CONST
     D    PRM_CdRamo                  2S 0 CONST
     D    PRM_Apolice                 7S 0 CONST
     D    PRM_CdSuper                 5S 0 CONST
     D
     D*==> Campos de Trabalho
     D WResult         S              5S 0
     D WIcLoad         S               N   INZ(*OFF)
     D WSelRec         S              4S 0
     D WIcDetail       S              1A
     D WNuSqSup        S              5S 0
     D WIcChanged      S              1A
     D WNoFunction     S             10A
     D
     D
     D WRRN            S              4S 0 INZ(*ZEROS)
     D WCdNatura       S              1A
     D WDsTipo         S             20A
     D WDsLimite       S            100A
     D WDsValor        S             15A
     D
     D WIcErro         S               N   INZ(*OFF)
     D WIcOrigem       S              1A
     D
     D WIdxRisco       S              5S 0
     D WIcExtract      S               N
     D DynamicCall     S             10A   INZ(*BLANKS)
     D*===========================================================
      /FREE

       WIcLoad  = *ON;
       DOW (NOT *IN03 AND NOT *IN12);
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
          IF (WRRN <> *ZEROS);
             IF (WRRN <= REC#);
                WRRN = (%INT(WRRN / 12)*12+1);
                ECR_RRN = WRRN;
             ELSE;
                ECR_RRN = 1;
             ENDIF;
          ELSE;
             ECR_RRN = 1;
          ENDIF;
          WRRN = *ZEROS;
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
                PrintPreview(WPtrOr:'LF':WDatInicio);
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
                LEAVE;
             ENDIF;
          ENDIF;
       ENDDO;
       RETURN WResult;

       //========================================================
       // Subrotina.: InzRecTxt
       // Objectivo.: Inicializar a lista de registos
       //========================================================
       BEGSR inzRecTxt;
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
             LST#Limites(Widx).IcEmpty  = *ON;
             WIdx += 1;
          ENDDO;
       ENDSR;

       //========================================================
       // Subrotina.: GetRecTexto
       // Objectivo.: Obter todos os registos de tabelas disponív
       //========================================================
       BEGSR getRecTexto;
          WIdx = *ZEROS;
          FillLista(PRM_CdProd:PRM_CdRamo:PRM_Apolice:PRM_CdSuper:*BLANKS);

          //---------------------------------------------------
          // Escrever registos no ecrã
          //---------------------------------------------------
          WIdx = 1;
          %SUBST(ECR_LSTHDR:1)  = 'Coberturas';
          %SUBST(ECR_LSTHDR:48) = 'Indemnização';
          %SUBST(ECR_LSTHDR:69) = 'Franquia';
          DOW (NOT LST#Limites(Widx).IcEmpty);
             //---------------------------------------------------
             // Escrever o Titulo do Grupo
             //---------------------------------------------------
             IF (WCdNatura <> Lst#Limites(WIdx).CdNatura)
                OR ($IsMultiplo(REC#:12));
                *IN49 = *ON;
                *IN45 = *OFF;
                ECR_LSTDET = $getDescricao(57:Lst#Limites(WIdx).CdNatura);
                REC#      += 1;
                WRITE RPRDE10302;
                WCdNatura  = Lst#Limites(WIdx).CdNatura;
                *IN49 = *OFF;
             ENDIF;

             //---------------------------------------------------
             // Escrever o Detalhe do Grupo
             //---------------------------------------------------
             ECR_NUSEQU = LST#Limites(Widx).NUSEQU;
             ECR_IDXLST = Widx;
             IF (ECR_NUSEQU > *ZEROS);
                *IN45 = WIcDisplay;
             ELSE;
                *IN45 = *ON;
             ENDIF;

             %SUBST(ECR_LSTDET:1) = LST#Limites(Widx).DsLimite;
             %SUBST(ECR_LSTDET:44) = *BLANKS;
             %SUBST(ECR_LSTDET:45) = LST#Limites(Widx).VlIndemniza;
             %SUBST(ECR_LSTDET:62) = LST#Limites(Widx).VlFranquia;
             ECR_CDLIMITE = LST#Limites(Widx).CdLimite;
             ECR_CDSUPER  = LST#Limites(Widx).CdSuper;
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
                WRRN = REC#;
                MntDetalhe(PRM_CdProd:PRM_CdRamo:PRM_APOLICE:
                           ECR_CDLIMITE:ECR_NUSEQU:ECR_IDXLST);
                WIcLoad = *ON;
                *IN12 = *OFF;
                LEAVE;
             ENDIF;
          ENDDO;
       ENDSR;
      /END-FREE
     P MntLimites      E

     P*=======================================================
     P* Processo..: FillLista
     P* Objectivo.: Actualizar Lista de Trabalho
     P*=======================================================
     P FillLista       B                   EXPORT
     D FillLista       PI             3S 0
     D    PRM_CdProd                  5S 0 CONST
     D    PRM_CdRamo                  2S 0 CONST
     D    PRM_Apolice                 7S 0 CONST
     D    PRM_CdSuper                 5S 0 CONST
     D    PRM_Identa                 12A   CONST
     D
     D*==> Campos de Trabalho
     D WCdLimite       S              5S 0 INZ(*ZEROS)
     D WSessCatalogo   S              3S 0 INZ(*ZEROS)
     D WSessProducao   S              3S 0 INZ(*ZEROS)
     D WResult         S              3S 0 INZ(*ZEROS)
     D WIdenta         S             12A
     D WRegIni         S              5S 0 INZ(*ZEROS)
     D*---> WIdx            S              3S 0 INZ(*ZEROS)
     D*=======================================================
      /FREE
        WNoApp  = *BLANKS;
        //----------------------------------------------
        // Parameterizar Tabelas de Limites e Franquias
        //----------------------------------------------
        WSessCatalogo = $SetFiles('FCATP103 A INNER JOIN FCATP102 B +
                                 ON (A.CDCOBERT = B.CDCOBERT) AND +
                                 (A.CDPROD = B.CDPROD)');
        $AddDBDate($IntToDate(WDatInicio));
        //$SetFields('FCATP103':'A');
        //$SetFields('FCATP102':'B':*OFF);
        $SetFields('A.CDRISCO,A.CDCOBERT,B.CDNATURA,A.CDLIMITE,A.CDSUPER,+
                    A.CDCOBERT,A.CDRISCO,A.ICSELECT,A.DSLIMITE,A.CDINCIND,+
                    A.VLPERIND,A.VLMAXIND,A.CDINCFRQ,A.VLPERFRQ,A.VLMINFRQ,+
                    A.CDLIMITE,B.DSCOBERT,B.ALIAS');

        //------------------------------------------------------
        // Obter os dependentes directos
        //------------------------------------------------------
        WRegIni = 1;
        $SetDBSession(WSessCatalogo);
        IF (PRM_CDSUPER > *ZEROS);
           $SetCriteria('(A.CDPROD  = '''+%CHAR(PRM_CdProd)+''') AND +
                         (A.CDSUPER = '''+%CHAR(PRM_CdSuper)+''') AND +
                         (A.CDSUPER <> A.CDLIMITE)');
           //$SetSort('A.CDSUPER');
        ELSE;
           $SetCriteria('(A.CDPROD  = '''+%CHAR(PRM_CdProd)+''') AND +
                         (A.CDSUPER = A.CDLIMITE)');
           $SetSort(*BLANKS);
        ENDIF;
        DOW ($GetRecord(WRegIni) > *ZEROS);
           WCdRisco  = $GetFieldNum('A.CDRISCO');
           WCdCobert = $GetFieldNum('A.CDCOBERT');
           IF IsToSelect(PRM_CdProd:PRM_CdRamo:PRM_Apolice:WCdCobert:
                         WCdRisco:$GetField('B.CDNATURA'));
              WIdx += 1;
              LST#Limites(Widx).CdLimite = $GetFieldNum('A.CDLIMITE');
              LST#Limites(Widx).CdSuper  = $GetFieldNum('A.CDSUPER');
              LST#Limites(Widx).CdCobert = $GetFieldNum('A.CDCOBERT');
              LST#Limites(Widx).CdRisco  = $GetFieldNum('A.CDRISCO');
              LST#Limites(Widx).IcSelect = $GetField('A.ICSELECT');
              LST#Limites(Widx).DsLimite = $GetField('A.DSLIMITE');
              LST#Limites(Widx).CdIncInd = $GetField('A.CDINCIND');
              LST#Limites(Widx).VlPerInd = $GetFieldNum('A.VLPERIND');
              LST#Limites(Widx).VlMaxInd = $GetFieldNum('A.VLMAXIND');
              LST#Limites(Widx).CdIncFrq = $GetField('A.CDINCFRQ');
              LST#Limites(Widx).VlPerFrq = $GetFieldNum('A.VLPERFRQ');
              LST#Limites(Widx).VlMinFrq = $GetFieldNum('A.VLMINFRQ');
              LST#Limites(Widx).CDNATURA = $GetField('B.CDNATURA');
              LST#Limites(Widx).Nusequ   = *ZEROS;
              LST#Limites(Widx).IcDetail = 'N';
              LST#Limites(Widx).IcEmpty  = *OFF;
              WCdLimite = $GetFieldNum('A.CDLIMITE');

              //---------------------------------------------------
              // Formatar a descrição
              //---------------------------------------------------
              IF ($GetField('A.DSLIMITE') = *BLANKS);
                 LST#Limites(Widx).DsLimite = $GetField('B.DSCOBERT');
              ENDIF;
              IF (%TRIM(PRM_Identa) <> *BLANKS);
                 LST#Limites(Widx).DsLimite = %TRIM(PRM_Identa) +
                                              %TRIM(LST#Limites(WIdx).DsLimite);
              ELSE;
                 LST#Limites(Widx).DsLimite = %TRIM($GetField('B.ALIAS'))+'.'+
                                              %TRIM(LST#Limites(WIdx).DsLimite);
              ENDIF;

              //---------------------------------------------------
              // Validar se Existem valores de Utilizador
              //---------------------------------------------------
              WSessProducao = $SetFiles('FPRDP014');
              $AddDBDate($IntToDate(WDatInicio));
              $SetCriteria('(CDRAMO = '''+%CHAR(PRM_CdRamo)+''') AND +
                            (NUAPOL = '''+%CHAR(PRM_Apolice)+''') AND +
                            (CDLIMITE = '''+%CHAR(WCdLimite)+''')');
              IF ($GetRecord() > *ZEROS);
                 LST#Limites(Widx).Nusequ   = $GetFieldNum('NUSEQU');
                 LST#Limites(Widx).CdIncInd = $GetField('CDINCIND');
                 LST#Limites(Widx).VlPerInd = $GetFieldNum('VLPERIND');
                 LST#Limites(Widx).VlMaxInd = $GetFieldNum('VLMAXIND');
                 LST#Limites(Widx).CdIncFrq = $GetField('CDINCFRQ');
                 LST#Limites(Widx).VlPerFrq = $GetFieldNum('VLPERFRQ');
                 LST#Limites(Widx).VlMinFrq = $GetFieldNum('VLMINFRQ');
              ENDIF;
              $RmvDBSession(WSessProducao);

              //---------------------------------------------------
              // Calcular os Limites e franquia
              //---------------------------------------------------
              LST#Limites(Widx).VlIndemniza = $GetCapitalAlf(WCodProduto:
                     OR_RAMO:OR_APOL:'L':
                     LST#Limites(WIdx).CdCobert:
                     LST#Limites(WIdx).CdLimite:*OFF:WDatInicio);
              LST#Limites(Widx).VlFranquia = $GetCapitalAlf(WCodProduto:
                     OR_RAMO:OR_APOL:'F':
                     LST#Limites(WIdx).CdCobert:
                     LST#Limites(WIdx).CdLimite:*OFF:WDatInicio);

              //---------------------------------------------------
              // Obter os Dependentes
              //---------------------------------------------------
              IF (WCdLimite <> PRM_CDSUPER);
                 WIdenta = %TRIM(PRM_Identa)+'...';
                 FillLista(PRM_CdProd:PRM_CdRamo:PRM_Apolice:WCdLimite:WIdenta);
              ENDIF;
           ENDIF;
           $SetDBSession(WSessCatalogo);
           WRegIni += 1;
        ENDDO;
        $rmvDBSession(WSessCatalogo);
        RETURN *ZEROS;
      /END-FREE
     P FillLista       E

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
     D    PRM_IdxLst                  5S 0 CONST
     D
     D*==> Campos de Trabalho
     D WCallStatus     S              1A
     D WIcAlterar      S              1A
     D WRow            S              3P 0
     D WCol            S              3P 0
     D WResult         S              5S 0
     D WIcUser         S               N
     D WIcErro         S               N
     D WCdCobert       S              5S 0
     D WRegIni         S              5S 0 INZ(*ZEROS)
     D*=========================================================
      /FREE

       WIcUser = *OFF;  // Valores do Utilizador
       WIcAlterar    = LST#Limites(PRM_IDXLST).IcSelect;
       ECR_CDINCIND  = LST#Limites(PRM_IDXLST).CDINCIND;
       ECR_VLPERIND  = LST#Limites(PRM_IDXLST).VLPERIND;
       ECR_VLMAXIND  = LST#Limites(PRM_IDXLST).VLMAXIND;
       ECR_CDINCFRQ  = LST#Limites(PRM_IDXLST).CDINCFRQ;
       ECR_VLMINFRQ  = LST#Limites(PRM_IDXLST).VLMINFRQ;
       ECR_VLPERFRQ  = LST#Limites(PRM_IDXLST).VLPERFRQ;
       ECR_DSLIMITE_ = LST#Limites(PRM_IDXLST).DSLIMITE;
       WCdCobert     = LST#Limites(PRM_IDXLST).CdCobert;
       IF (LST#Limites(PRM_IDXLST).NuSequ > *ZEROS);
          WIcUser       = *ON;
       ENDIF;

       DOW (NOT *IN03 AND NOT *IN12);
          DS#HEADER    = $getHeader(S#PGM:'RPRDE10305');
          IF (WIcDisplay);
             *IN46 = *OFF;
             WExceptKeys = '0001000000000000000001'; // F22 Off
          ELSE;
             IF (WIcAlterar = 'N');
                *IN46 = *OFF;
                WExceptKeys = *BLANKS;
             ELSE;
                *IN46 = *ON;
                IF (NOT WIcUser);
                   WExceptKeys = '0000000000000000000001'; // F22 Off
                ELSE;
                   WExceptKeys = *BLANKS;
                ENDIF;
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
             IF ($ShowDialog('PRD0191':*BLANKS:'NS') = 'S');
                DELETE (OR_Ramo:OR_Apol:PRM_NuSequ) RPRDP014;
             ENDIF;
             *IN12 = *ON;
          ELSEIF (Func#Key = KEY#Enter) AND (NOT WIcDisplay);
             IF (WIcAlterar = 'S');
                EXSR Validar;
                IF ($GetScStatus = 'O');
                   EXSR UpdRecord;
                   *IN12 = *ON;
                ENDIF;
             ELSE;
                *IN12 = *ON;
             ENDIF;
          ENDIF;
       ENDDO;
       RETURN WResult;

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
             PRD_CDRAMO   = PRM_CdRAMO;
             PRD_NUAPOL   = PRM_Apolice;
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
       // Subrotina.: PopUp
       // Objectivo.: Afixar uma lista de valoers possíveis
       //========================================================
       BEGSR PopUp;
          //----------------------------------------------
          // Validar qual o campo onde foi seleccionado
          //----------------------------------------------
          IF (ECR_CM#NMFLD = 'CDINCIND');
             ECR_CDINCIND = $runPopUp(23:ECR_CDINCIND);
          ELSEIF (ECR_CM#NMFLD = 'CDINCFRQ');
             ECR_CDINCFRQ = $runPopUp(24:ECR_CDINCFRQ);
          ENDIF;
       ENDSR;

       //============================================================
       // Subrotina..: Validar
       // Objectivo..: Validar se o registo pode ser gravado
       //============================================================
       BEGSR Validar;
          $InzScStatus();
          //-----------------------------------------
          // Validar Limites de Indemnização
          //-----------------------------------------
          IF (NOT $ValCodigo(23:ECR_CDINCIND));
             $SetScError('CDINCIND':'PRD0200');
          ELSEIF (ECR_CDINCIND = '04');
             IF (ECR_VLMAXIND <> *ZEROS);
                $SetScError('VLMAXIND':'PRD0192');
             ENDIF;
             IF (ECR_VLPERIND <> *ZEROS);
                $SetScError('VLPERIND':'PRD0201');
             ENDIF;
          ELSEIF (ECR_CDINCIND <> '04');
             IF (ECR_VLMAXIND <= *ZEROS) AND (ECR_VLPERIND <= *ZEROS);
                $SetScError('VLMAXIND':'PRD0192');
             ENDIF;
             IF (ECR_VLPERIND < *ZEROS) OR (ECR_VLPERIND > 100);
                $SetScError('VLPERIND':'PRD0201');
             ENDIF;
          ENDIF;

          //-----------------------------------------
          // Validar Franquia
          //-----------------------------------------
          IF (NOT $ValCodigo(24:ECR_CDINCFRQ));
             $SetScError('CDINCFRQ':'PRD0200');
          ELSEIF (ECR_CDINCFRQ = '04');
             IF (ECR_VLMINFRQ <> *ZEROS);
                $SetScError('VLMINFRQ':'PRD0202');
             ENDIF;
             IF (ECR_VLPERFRQ <> *ZEROS);
                $SetScError('VLPERFRQ':'PRD0201');
             ENDIF;
          ELSEIF (ECR_CDINCFRQ <> '04');
             IF (ECR_VLMINFRQ <= *ZEROS) AND (ECR_VLPERFRQ <= *ZEROS);
                $SetScError('VLMINFRQ':'PRD0202');
             ENDIF;
             IF (ECR_VLPERFRQ < *ZEROS) OR (ECR_VLPERFRQ > 100);
                $SetScError('VLPERFRQ':'PRD0201');
             ENDIF;
          ENDIF;
       ENDSR;
      /END-FREE
     P MntDetalhe      E

     P*=======================================================
     P* Processo..: IsToSelect
     P* Objectivo.: Seleccionar o registo de limite e Franquia
     P*             caso esteja contratado.
     P* Observação: Código Isolado para prever a inclusão dos
     P*             novos Módulos de Gestão
     P*=======================================================
     P IsToSelect      B                   EXPORT
     D IsToSelect      PI              N
     D   PRM_CdProd                   2S 0 CONST
     D   PRM_CdRamo                   2S 0 CONST
     D   PRM_NuApol                   7S 0 CONST
     D   PRM_CdCobert                 5S 0 CONST
     D   PRM_CdRisco                  5S 0 CONST
     D   PRM_CdNatura                 1A   CONST
     D
     D*==> Campos de Trabalho
     D WIsToSelect     S               N
     D WSessCobertur   S              5S 0 INZ(*ZEROS)
     D WIdx            S              5S 0 INZ(*ZEROS)
     D*=========================================================
      /FREE
         //-----------------------------------------------
         // Validar se o Risco está contratado
         //-----------------------------------------------
         WIsToSelect = *ON;
         IF (Prm_CdRisco > *ZEROS);
            WIsToSelect = *OFF;
            WIdx = 1;
            DOW (NOT LST#Risco(WIdx).IsEmpty);
               IF (LST#Risco(WIdx).CodRisco = Prm_CdRisco);
                  WIsToSelect = *ON;
                  LEAVE;
               ENDIF;
               WIdx += 1;
            ENDDO;
         ENDIF;

         //-----------------------------------------------
         // Validar se a Cobertura faz parte do Contrato
         //-----------------------------------------------
         IF (PRM_CdNatura <> 'B') AND WIsToSelect;
            WSessCobertur = $SetFiles('FPRDP008');
            $AddDBDate($IntToDate(WDatInicio));
            $SetCriteria('(CDRAMO='''+%CHAR(PRM_CdRamo)+''') +
                           AND (NUAPOL='''+%CHAR(PRM_NuApol)+''') +
                           AND (CDCOBERT='''+%CHAR(PRM_CdCobert)+''')');
            IF ($GetRecord() > *ZEROS);
               WIsToSelect = *ON;
            ELSE;
               WIsToSelect = *OFF;
            ENDIF;
            $RmvDBSession(WSessCobertur);
         ENDIF;
         RETURN WIsToSelect;
      /END-FREE
     P IsToSelect      E
