     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PPRDO104
     H* Descrição.: Parameterizar Coberturas para uma Apólices.
     H*==================================================================
     FFPRDP008  UF A E           K DISK    PREFIX(PRD_) COMMIT
     FFPRDE104  CF   E             WORKSTN SFILE(RPRDE10402:REC#)
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
      /Copy QRPGLESRC,SPRDO002
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SCATO010
      /Copy QRPGLESRC,SSECO010

     D*==> Estrutura de dados do Registo
     D RBASEDIO      E DS                  ExtName(GBASEDIO)
     D                                     Prefix(OR_)
     D                                     Based(ptrOR)
     D
     D*==> Obter os valores da Cobertura
     D GetCobValue     PR            11S 2
     D   PCdProd                      5S 0 CONST
     D   PCdRamo                      5S 0 CONST
     D   PNuApol                      7S 0 CONST
     D   PCdCobert                    5S 0 CONST
     D   PData                        8S 0 CONST
     D   PTipo                        2A   CONST
     D
     D*==> Manutenção de Coberturas
     D MntCoberturas   PR
     D   PCdProd                      5S 0 CONST
     D   PDsProd                     40A   CONST
     D   PCdSuper                     5S 0 CONST
     D   PDsSuper                    40A   CONST
     D
     D*==> Actualizar Lista de Coberturas
     D  FillLstCobert  PR
     D   CdProd                       5S 0 CONST
     D   CdRamo                       2S 0 CONST
     D   NuApol                       7S 0 CONST
     D   CdCobert                     5S 0 CONST
     D
     D*==> Validar Existência da Cobertura
     D ChkCobertura    PR             5S 0
     D   CdProd                       5S 0 CONST
     D   CdCobert                     5S 0 CONST
     D   CdAlias                     10A   CONST
     D   Natureza                     1A   CONST
     D
     D*==> Pré-visualização das Actas
     D  PrintPreview   PR                  EXTPGM('RPRDO005')
     D   ptrOR                         *   CONST
     D   Cdfunction                   2A   CONST
     D   PrintPreview                 8S 0 CONST
     D
     D*==> Estrutura de dados do Registo
     D WIcErro         S               N   INZ(*OFF)
     D WIcOrigem       S              1A
     D REC#            S              4S 0
     D
     D*==> Outros Parâmetros
     D WCodProduto     S              5S 0
     D WIcDisplay      S               N   INZ(*ON)
     D WModo           S               N
     D WNoFunction     S             10A   INZ(*BLANKS)
     D WCdTipo         S              1A   INZ(*BLANKS)
     D WCdCobert       S                   LIKE(PRD_CDCOBERT)
     D WValor          S              1A
     D WptrOr          S               *
     D
     D*==> Riscos da Apólice
     D LST#Risco       DS                  QUALIFIED DIM(10)
     D   CodRisco                     5S 0 INZ(*ZEROS)
     D   CapRisco                    11S 2 INZ(*ZEROS)
     D   IsEmpty                       N   INZ(*ON)
     D
     D*==> Coberturas
     D LST#Cobert      DS                  QUALIFIED DIM(100)
     D   NuSequ                       5S 0 INZ(*ZEROS)
     D   CDCOBERT                     5S 0 INZ(*ZEROS)
     D   CDSUPER                      5S 0 INZ(*ZEROS)
     D   NUORDEM                      5S 0 INZ(*ZEROS)
     D   DSCOBERT                   100A   INZ(*BLANKS)
     D   CDNATURA                     1A   INZ(*BLANKS)
     D   ICOBRIGA                     1A   INZ(*BLANKS)
     D   IsEmpty                       N   INZ(*OFF)
     D
     D WSessCapRis     S              5S 0 INZ(*ZEROS)
     D WRecAct         S              5S 0 INZ(*ZEROS)
     D WCdRisco        S              5S 0 INZ(*ZEROS)
     D WCapRisco       S             11S 2 INZ(*ZEROS)
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
          RETURN;
       ENDIF;
       ptrOr   = WptrOR;
       LST#Risco   = $GetCapRisco(OR_RAMO:OR_Apol:*ZEROS:WDatInicio);
       EXSR ShowCapRisco; // Mostrar Capitais de Risco

       MntCoberturas(WCodProduto:*BLANKS:*ZEROS:*BLANKS);
       *IN12 = *OFF;
       *IN03 = *OFF;
       RETURN;

       //============================================================
       // Subrotina..: ShowCapRisco
       // Objectivo..: repor Capitais de Risco
       //============================================================
       BEGSR ShowCapRisco;
          ECR_DSRISC001 = *BLANKS;
          ECR_DSRISC002 = *BLANKS;
          ECR_DSRISC003 = *BLANKS;
          ECR_DSRISC004 = *BLANKS;
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

     P*===================================================================
     P* Processo..: MntCoberturas
     P* Descrição.: Realizar a manutenção das Coberturas
     P*===================================================================
     P MntCoberturas   B                   EXPORT
     D MntCoberturas   PI
     D   PCdProd                      5S 0 CONST
     D   PDsProd                     40A   CONST
     D   PCdSuper                     5S 0 CONST
     D   PDsSuper                    40A   CONST
     D
     D WSessionSup     S              5S 0 INZ(*ZEROS)
     D WCapital        S             15A
     D WPremio         S             15A
     C*=================================================================
      /FREE

       WIcDisplay = WModo; // *ON
       EXSR inzRecords;
       EXSR getRecords;

       ECR_NOMSEG  = OR_Seg;
       ECR_RAMO    = OR_RAMO;
       ECR_APOLICE = OR_Apol;
       DOW (NOT *IN03 AND NOT *IN12);
          IF (WIcDisplay);
             WNoFunction = 'RPRDE10405'; // Consulta Coberturas
          ELSE;
             WNoFunction = 'RPRDE10421'; // Alterar Coberturas
          ENDIF;

          DS#HEADER    = $getHeader(S#PGM:WNoFunction);
          DS#FOOTER    = $getFooter(S#PGM:WNoFunction:*BLANKS);
          ECR_S#PGM = S#PGM;
          WRITE     RPRDE10400;
          WRITE     RPRDE10499;
          EXFMT RPRDE10403;
          IF ($ValidKey(Func#Key:KEY#F21)) AND WIcDisplay;
             //-------------------------------------------------
             // Mudar Modo IO, Para alteração
             //-------------------------------------------------
             //WIcDisplay = *OFF;
             //EXSR inzRecords;
             //EXSR getRecords;
          // ELSEIF ((Func#Key = KEY#PgDown) AND (NOT $IsLastPage())) OR
          //    ((Func#Key = KEY#PgUp) AND ($GetPageNbr() > 1));
          //    EXSR inzRecords;
          //    EXSR getRecords;
          ELSEIF (Func#Key = KEY#F14);
             MONITOR;
                PrintPreview(WPtrOr:'CB':WDatInicio);
             ON-ERROR;
             ENDMON;
          ELSEIF (Func#Key = KEY#Enter);
             EXSR SelRec;
             *IN12 = *OFF;
             EXSR inzRecords;
             EXSR getRecords;
          ENDIF;
       ENDDO;

       RETURN;

       //========================================================
       // Subrotina.: InzRecTxt
       // Objectivo.: Inicializar a lista de registos
       //========================================================
       BEGSR inzRecords;
          *IN46     = *OFF;
          *IN45     = *ON;
          *IN48     = *ON;
          REC#      = 0;        // Controlo de Registos da Lista
          *In53     = *On;      // Limpar Lista de Ecrã
          *In52     = *On;      // Mostrar a Lista de Ecrã
          WRITE RPRDE10403; // Limpar Lista
          *In53     = *Off;     // Repor Indicador
          *In54     = *Off;     // Repor Indicador
       ENDSR;

       //========================================================
       // Subrotina.: GetRecords
       // Objectivo.: Obter todos os registos de tabelas disponív
       //========================================================
       BEGSR getRecords;
          FillLstCobert(Produto:Ramo:Apolice:*ZEROS);
          WCdTipo  = *BLANKS;
          DOW (NOT LST#Cobert(WIdx).IsEmpty);
                //---------------------------------------------
                // Escreve os Cabeçalhos da Listagem
                //---------------------------------------------
                IF (REC# < 12); // BRK001
                   IF (WCdTipo <> $GetField('CDNATURA'));
                      REC# += 1;
                      WCdTipo = $GetField('CDNATURA');
                      *IN48 = *OFF;
                      *IN49 = *ON;
                      ECR_CDSELECT = *BLANKS;
                      ECR_DSLINHA  = $getDescricao(57:WCdTipo);
                      WRITE RPRDE10402;
                   ENDIF;
                ENDIF;

                //---------------------------------------------
                // Escreve os Registo da Listagem
                //---------------------------------------------
                IF (REC# < 12); // BRK001
                   REC# += 1;
                   *IN49 = *OFF;
                   *IN45 = *ON;
                   *IN48 = *ON;
                   MONITOR;
                      WCdCobert = $GetFieldNum('CDCOBERT');
                   ON-ERROR;
                      WcdCobert = *ZEROS;
                   ENDMON;
                   ECR_ICMORE   = 'N';
                   ECR_CDCOBERT = WCdCobert;
                   ECR_DSLINHA  = $GetField('DSCOBERT');
                   ECR_DSLINHA  = %TRIM($GetField('ALIAS'))+'.'+
                                    %TRIM(ECR_DSLINHA);
                   EXSR IsSuper;  // Validar se é uma Coberura Superior
                   ECR_CDNATURA = $GetField('CDNATURA');
                   ECR_ALIAS    = $GetField('ALIAS');
                   ECR_NUSEQU   = *ZEROS;
                   ECR_CDSELECT = 'N';
                   IF (WCdTipo = 'B') OR (ECR_ICMORE = 'S');
                      *IN48 = *OFF;
                      IF (WCdTipo = 'B');
                         ECR_CDSELECT = 'S';
                      ELSE;
                         ECR_NUSEQU=ChkCobertura(PCdProd:WCdCobert:
                                $GetField('ALIAS'):WCdTipo);
                         IF (ECR_NUSEQU > *ZEROS);
                            *IN48        = *OFF;
                            ECR_CDSELECT = 'S';
                         ENDIF;
                      ENDIF;
                   ELSE;
                      ECR_NUSEQU=ChkCobertura(PCdProd:WCdCobert:
                              $GetField('ALIAS'):WCdTipo);
                      IF (ECR_NUSEQU > *ZEROS);
                         *IN48        = *OFF;
                         ECR_CDSELECT = 'S';
                      ENDIF;

                      IF (NOT WIcDisplay);
                         *IN45  = *OFF;
                      ENDIF;
                   ENDIF;
                   //###########################################
                   WPremio  = '000.000.000,00¤';
                   WCapital = '000.000.000,00¤';
                   IF (WCdTipo = 'B') OR (ECR_CDSELECT <> 'S');
                      WPremio  = *BLANKS;
                      IF (ECR_CDSELECT <> 'S');
                         WCapital = *BLANKS;
                      ENDIF;
                   ENDIF;
                   %SUBST(ECR_DSLINHA:40) = *BLANKS;
                   %SUBST(ECR_DSLINHA:41) = WPremio;
                   %SUBST(ECR_DSLINHA:58) = WCapital;
                   WRITE RPRDE10402;
                ENDIF;

                IF (REC# = 11);
                   LEAVE;
                ENDIF;
                $getNext();
             ENDDO;
          ENDIF;

          //---------------------------------------
          // Validar se ainda existem mais registos
          //---------------------------------------
          IF (NOT $IsLastPage());
             *IN54    = *OFF;
          ELSE;
             *IN54    = *ON;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: isSuper
       // Objectivo.: Validar se a Cobertura tem dependentes.
       //========================================================
       BEGSR isSuper;
          WSessionSup = $SetFiles('FCATP102');
          $AddDBDate($IntToDate(WDatInicio));
          $SetCriteria('(CDPROD='''+%CHAR(PCdProd)+''') +
                  AND (CDSUPER='''+%CHAR(ECR_CDCOBERT)+ ''') +
                  AND (CDSUPER<>CDCOBERT)');
          IF ($getRecord() > *ZEROS);
             ECR_DSLINHA = '...'+%TRIM(ECR_DSLINHA);
             ECR_ICMORE   = 'S';
          ENDIF;
          $RmvDBSession(WSessionSup);
       ENDSR;

       //========================================================
       // Subrotina.: SelRec
       // Objectivo.: Seleccionar os registos
       //========================================================
       BEGSR SelRec;
          READC RPRDE10402;
          DOW (NOT %EOF());
             IF (ECR_OPTION <> *BLANKS);
                IF (ECR_ICMORE = 'N');
                   ExSr ShowText;
                ELSE;
                   MntCoberturas(WCodProduto:*BLANKS:
                                 ECR_CdCobert:*BLANKS);
                ENDIF;
             ELSE;
                IF (ECR_CDNATURA <> 'B');
                   WCdCobert    = ECR_CDCOBERT;
                   EXSR UpdCoberturas;
                ENDIF;
             ENDIF;
             READC RPRDE10402;
          ENDDO;
       ENDSR;

       //========================================================
       // Subrotina.: ShowText
       // Objectivo.: Mostrar os textos associados à Cobertura
       //========================================================
       BEGSR ShowText;
          //---------------------------------------------------
          // ForCompr o texto associado à Cobertura
          //---------------------------------------------------
       // EXFMT RPRDE10407;
       ENDSR;

       //========================================================
       // Subrotina.: UpdCoberturas
       // Objectivo.: Actualizar Coberturas
       //========================================================
       BEGSR UpdCoberturas;
          IF ((ECR_NUSEQU > *ZEROS) AND (ECR_CDSELECT = 'N'));
             DELETE (OR_Ramo:OR_Apol:ECR_NUSEQU) RPRDP008;
             WValor = *BLANKS;
             EXSR UpdOldData;
          ELSEIF ((ECR_NUSEQU = *ZEROS) AND (ECR_CDSELECT = 'S'));
             PRD_CDRAMO   = OR_Ramo;
             PRD_NUAPOL   = OR_Apol;
             PRD_CDCOBERT = WCdCobert;
             WRITE RPRDP008;
             WValor = 'S';
             EXSR UpdOldData;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: UpdOldData
       // Objectivo.: Actualizar Tabelas Antigas
       //========================================================
       BEGSR UpdOldData;
          IF (PCdProd = 5) OR (PCdProd = 11);
             EXSR UpdOld01;
          ELSEIF (PCdProd = 50) OR (PCdProd = 51);
             EXSR UpdOld03;
          ELSE;
             EXSR UpdOld02;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: UpdOld01
       // Objectivo.: Actualizar Tabelas Antigas
       //========================================================
       BEGSR UpdOld01;
          IF (%TRIM(ECR_ALIAS) = '01');
             OR_CF01 = WValor;
          ELSEIF (%TRIM(ECR_ALIAS) = '02');
             OR_CF02 = WValor;
          ELSEIF (%TRIM(ECR_ALIAS) = '03');
             OR_CF03 = WValor;
          ELSEIF (%TRIM(ECR_ALIAS) = '04');
             OR_CF04 = WValor;
          ELSEIF (%TRIM(ECR_ALIAS) = '05');
             OR_CF05 = WValor;
          ELSEIF (%TRIM(ECR_ALIAS) = '06');
             OR_CF06 = WValor;
          // ELSEIF (%TRIM(ECR_ALIAS) = '07');
          //    OR_CF07 = WValor;
          ELSEIF (%TRIM(ECR_ALIAS) = '08');
             OR_CF08 = WValor;
          ELSEIF (%TRIM(ECR_ALIAS) = '09');
             OR_CF09 = WValor;
          ELSEIF (%TRIM(ECR_ALIAS) = '10');
             OR_CF10 = WValor;
          ELSEIF (%TRIM(ECR_ALIAS) = '11');
             OR_CF11 = WValor;
          ELSEIF (%TRIM(ECR_ALIAS) = '12');
             OR_CF12 = WValor;
          ELSEIF (%TRIM(ECR_ALIAS) = '13');
             OR_CF13 = WValor;
          ELSEIF (%TRIM(ECR_ALIAS) = '0800');
             OR_CF0800 = WValor;
             OR_CF07   = WValor;
          ELSEIF (%TRIM(ECR_ALIAS) = '0830');
             OR_CF0830 = WValor;
             OR_CF07   = WValor;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: UpdOld02
       // Objectivo.: Actualizar Tabelas Antigas
       //========================================================
       BEGSR UpdOld02;
          IF (WValor = *BLANKS) AND (ECR_ALIAS <> *BLANKS);
             IF (OR_CL01 = %DEC(ECR_ALIAS:2:0));
                OR_CL01 = 00;
             ELSEIF (OR_CL02 = %DEC(ECR_ALIAS:2:0));
                OR_CL02 = 00;
             ELSEIF (OR_CL03 = %DEC(ECR_ALIAS:2:0));
                OR_CL03 = 00;
             ELSEIF (OR_CL04 = %DEC(ECR_ALIAS:2:0));
                OR_CL04 = 00;
             ELSEIF (OR_CL05 = %DEC(ECR_ALIAS:2:0));
                OR_CL05 = 00;
             ELSEIF (OR_CL06 = %DEC(ECR_ALIAS:2:0));
                OR_CL06 = 00;
             ENDIF;
          ELSEIF (ECR_ALIAS <> *BLANKS);
             IF (OR_CL01 = 00);
                OR_CL01 = %DEC(ECR_ALIAS:2:0);
             ELSEIF (OR_CL02 = 00);
                OR_CL02 = %DEC(ECR_ALIAS:2:0);
             ELSEIF (OR_CL03 = 00);
                OR_CL03 = %DEC(ECR_ALIAS:2:0);
             ELSEIF (OR_CL04 = 00);
                OR_CL04 = %DEC(ECR_ALIAS:2:0);
             ELSEIF (OR_CL05 = 00);
                OR_CL05 = %DEC(ECR_ALIAS:2:0);
             ELSEIF (OR_CL06 = 00);
                OR_CL06 = %DEC(ECR_ALIAS:2:0);
             ENDIF;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: UpdOld03
       // Objectivo.: Actualizar Tabelas Antigas
       //========================================================
       BEGSR UpdOld03;
       ENDSR;
      /END-FREE
     PMntCoberturas    E

     P*=======================================================
     P* Processo..: FillLstCobert
     P* Objectivo.: Actualizar a lista de Coberturas.
     P*=======================================================
     P FilLstCobert    B                   EXPORT
     D                 PI
     D   CdProd                       5S 0 CONST
     D   CdRamo                       2S 0 CONST
     D   NuApol                       7S 0 CONST
     D   CdCobert                     5S 0 CONST
     D
     D*==> Campos de Trabalho
     D WSessionID      S              5S 0
     D*=======================================================
      /FREE
       WSessionID = $SetFiles('FCATP102');
       $AddDBDate($IntToDate(WDatInicio));
       $SetSort('CDNATURA #DESC, NUORDEM');
       $SetCriteria('CDPROD = ' + %CHAR(WCodProduto));
       IF (PCdSuper = *ZEROS);
          $SetCriteria('CDPROD = ''' + %CHAR(WCodProduto) + ''' +
                        AND (CDCOBERT = CDSUPER)');
       ELSE;
          $SetCriteria('CDPROD = ''' + %CHAR(WCodProduto) + ''' +
                        AND CDSUPER = ''' + %CHAR(PCdSuper) + ''' +
                        AND (CDSUPER <> CDCOBERT)');
       ENDIF;
       $RmvDBSession(WSessionID);
      /END-FREE
     P FilLstCobert    E

     P*=======================================================
     P* Processo..: ChkCobertura
     P* Objectivo.: Validar se a Cobertura está contratada para
     P*             a Apólices (trabalha com as duas versões das
     P*             Tabelas). Actualizar nova Tabela, partindo
     P*             do que encontrar na antiga.
     P*=======================================================
     P ChkCobertura    B                   EXPORT
     D                 PI             5S 0
     D   PRM_CdProd                   5S 0 CONST
     D   PRM_CdCobert                 5S 0 CONST
     D   PRM_Alias                   10A   CONST
     D   PRM_Natureza                 1A   CONST
     D
     D WSessCobert     S              5S 0 INZ(*ZEROS)
     D WNuSequ         S              5S 0 INZ(*ZEROS)
     D*=======================================================
      /FREE
        WNuSequ = *ZEROS;
        IF (PRM_CdProd = 5) OR (PRM_CdProd = 11);
           EXSR GetOldSet1;
        ELSEIF (PRM_CdProd = 50) OR (PRM_CdProd = 51);
           EXSR GetOldSet3;
        ELSE;
           EXSR GetOldSet2;
        ENDIF;
        EXSR GetRecord;
        RETURN WNuSequ;

       //========================================================
       // Subrotina.: GetRecord
       // Objectivo.: Obter o registo existente na nova estrutura
       //             de dados.
       //========================================================
       BEGSR GetRecord;
          //----------------------------------------------------------------
          // Obter o Registo para a Cobertura activo na data
          //----------------------------------------------------------------
          WSessCobert = $SetFiles('FPRDP008');
          $AddDBDate($IntToDate(WDatInicio));
          $SetCriteria('(CDRAMO='''+%CHAR(OR_RAMO)+''') +
                    AND (NUAPOL='''+%CHAR(OR_Apol)+''') +
                  AND (CDCOBERT='''+%CHAR(PRM_CdCobert)+ ''')');
          IF ($getRecord() = *ZEROS);
             IF (WNuSequ = 9999) AND (NOT WIcDisplay);
                PRD_CDRAMO   = OR_Ramo;
                PRD_NUAPOL   = OR_Apol;
                PRD_CDCOBERT = PRM_CdCobert;
                WRITE RPRDP008;
                $getRecord();
             ENDIF;
          ENDIF;

          MONITOR;
             WNuSequ = $GetFieldNum('NUSEQU');
          ON-ERROR;
             WNuSequ = *ZEROS;
          ENDMON;
          $RmvDBSession(WSessCobert);
       ENDSR;

       //========================================================
       // Subrotina.: GetOldSet1
       // Objectivo.: Aplicar configuração partindo das antigas
       //             tabelas. Descontinuar, esta rotina depois
       //             de modificar os Sistemas.
       //========================================================
       BEGSR GetOldSet1;
          WNuSequ = *ZEROS;
          IF (OR_CF01 = 'S') AND (%TRIM(PRM_ALIAS) = '01');
             WNuSequ = 9999;
          ELSEIF (OR_CF02 = 'S') AND (%TRIM(PRM_ALIAS) = '02');
             WNuSequ = 9999;
          ELSEIF (OR_CF03 = 'S') AND (%TRIM(PRM_ALIAS) = '03');
             WNuSequ = 9999;
          ELSEIF (OR_CF04 = 'S') AND (%TRIM(PRM_ALIAS) = '04');
             WNuSequ = 9999;
          ELSEIF (OR_CF05 = 'S') AND (%TRIM(PRM_ALIAS) = '05');
             WNuSequ = 9999;
          ELSEIF (OR_CF06 = 'S') AND (%TRIM(PRM_ALIAS) = '06');
             WNuSequ = 9999;
          ELSEIF (OR_CF08 = 'S') AND (%TRIM(PRM_ALIAS) = '08');
             WNuSequ = 9999;
          ELSEIF (OR_CF09 = 'S') AND (%TRIM(PRM_ALIAS) = '09');
             WNuSequ = 9999;
          ELSEIF (OR_CF10 = 'S') AND (%TRIM(PRM_ALIAS) = '10');
             WNuSequ = 9999;
          ELSEIF (OR_CF11 = 'S') AND (%TRIM(PRM_ALIAS) = '11');
             WNuSequ = 9999;
          ELSEIF (OR_CF12 = 'S') AND (%TRIM(PRM_ALIAS) = '12');
             WNuSequ = 9999;
          ELSEIF (OR_CF13 = 'S') AND (%TRIM(PRM_ALIAS) = '13');
             WNuSequ = 9999;
          ELSEIF (OR_CF07 = 'S') AND (%TRIM(PRM_ALIAS) = '07');
             IF (OR_CF0800<>'S') AND (OR_CF0830<>'S');
                OR_CF0800 = 'S';
             ENDIF;
             WNuSequ = 9999;
          ELSEIF (OR_CF0800 = 'S') AND (%TRIM(PRM_ALIAS) = '0800');
             WNuSequ = 9999;
          ELSEIF (OR_CF0830 = 'S') AND (%TRIM(PRM_ALIAS) = '0830');
             WNuSequ = 9999;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: GetOldSet2
       // Objectivo.: Aplicar configuração partindo das antigas
       //             tabelas. Descontinuar, esta rotina depois
       //             de modificar os Sistemas.
       //========================================================
       BEGSR GetOldSet2;
          WNuSequ = *ZEROS;
          IF (PRM_ALIAS <> *BLANKS);
             IF (OR_CL01 = %DEC(PRM_ALIAS:2:0));
                WNuSequ = 9999;
             ELSEIF (OR_CL02 = %DEC(PRM_ALIAS:2:0));
                WNuSequ = 9999;
             ELSEIF (OR_CL03 = %DEC(PRM_ALIAS:2:0));
                WNuSequ = 9999;
             ELSEIF (OR_CL04 = %DEC(PRM_ALIAS:2:0));
                WNuSequ = 9999;
             ELSEIF (OR_CL05 = %DEC(PRM_ALIAS:2:0));
                WNuSequ = 9999;
             ELSEIF (OR_CL06 = %DEC(PRM_ALIAS:2:0));
                WNuSequ = 9999;
             ENDIF;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: GetOldSet3
       // Objectivo.: Obter as Coberturas existentes para o Seguro
       //             Automóvel.
       //========================================================
       BEGSR GetOldSet3;
       ENDSR;
      /END-FREE
     P ChkCobertura    E
     P*=======================================================
     P* Processo..: GetCobValue
     P* Objectivo.: Obter o Valor previsto para uma cobertura
     P*  Tipo  Desrição
     P*  ----  -----------------------------------------------
     P*   CP   Capital Seguro
     P*   PR   Prémio
     P*   FR   Franquia
     P*   LI   Limite de Indemnização
     P*=======================================================
     P GetCobValue     B                   EXPORT
     D GetCobValue     PI            11S 2
     D   PCdProd                      5S 0 CONST
     D   PCdRamo                      5S 0 CONST
     D   PNuApol                      7S 0 CONST
     D   PCdCobert                    5S 0 CONST
     D   PData                        8S 0 CONST
     D   PTipo                        2A   CONST
     D
     D*==> Campos de Trabalho
     D  WSession       S              8S 0
     D  WResult        S             11S 2
     D*=======================================================
      /FREE
       IF (PTipo = 'CP');
          EXSR CalcCapital;
       ENDIF;
       RETURN WResult;

       //=====================================================
       // Subrotina.: CalcCapital
       // Descrição.: Devolver o valor previsto para o Capital
       //=====================================================
       BEGSR CalcCapital;
          WSession = $SETFILES('FCATP102 A INNER JOIN +
                                FCATP103 B ON (A.CDPROD = +
                                B.CDPROD) AND (A.CDCOBERT = +
                                B.CDCOBERT)');
          $SetFields('FCATP102':'A');
          $SetFields('FCATP103':'B':*OFF);
          $AddDBDate($IntToDate(PData));
          $SetCriteria('(CDPROD ='''+%CHAR(PCdProd)+''') +
                    AND (CDCOBERT ='''+%CHAR(PCdCobert)+''')');
          IF ($GetRecord() > *ZEROS);
          ENDIF;
          $RmvDBSession(WSession);
       ENDSR;
      /END-FREE
     P GetCobValue     E
