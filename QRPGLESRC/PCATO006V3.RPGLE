     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PCATO006
     H* Descrição.: Manutenção de Limites e Franquias
     H*==================================================================
     FFCATP10301IF   E           K DISK    PREFIX(CAT_)
     F                                     RENAME(RCATP103:RCATP10301)
     FFCATP103  UF A E           K DISK    PREFIX(CAT_)
     FFCATE006  CF   E             WORKSTN SFILE(RCATE00602:REC#)
     F                                     PREFIX(ECR_)
     F                                     INFDS(INFD#KEYS)
     F*==========================================================
     F* Indicadores  Descrição
     F* -----------  --------------------------------------------
     F*      45      Condicionar Campos com Input/Output
     F*      52      DSP
     F*      53      CLR
     F*      54      END
     D*==========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SSECO010
      /Copy QRPGLESRC,SCATO010

     D*==> Carregar lista de Trabalho com Coberturas
     D FillCoberturas  PR
     D   PCdProd                      5S 0 CONST
     D   PCdSuper                     5S 0 CONST
     D   PCdNivel                     5S 0 CONST
     D
     D LST#Cobert      DS                  QUALIFIED DIM(200)
     D   CdProd                       5S 0 INZ(*ZEROS)
     D   CdLimite                     5S 0 INZ(*ZEROS)
     D   DsLimite                    50A   INZ(*BLANKS)
     D   CdCobert                     5S 0 INZ(*ZEROS)
     D   CdSuper                      5S 0 INZ(*ZEROS)
     D   DsCobert                    50A   INZ(*BLANKS)
     D   Codigo                      10A   INZ(*BLANKS)
     D   CdNatura                     1A   INZ(*BLANKS)
     D   Header                       1A   INZ('N')
     D   TpHeader                     1A   INZ(*BLANKS)
     D   CdNivel                      3S 0 INZ(*ZEROS)
     D   IsEmpty                       N   INZ(*ON)
     D WIdxCobert      S              3S 0 INZ(*ZEROS)
     D WIsCobHeader    S               N   INZ(*OFF)
     D
     D*==> Estrutura da Manutenção Limites
     D MntLimites      PR
     D   PCdProd                      5S 0 CONST
     D   PDsProd                     50A   CONST
     D   PCdSuper                     5S 0 CONST
     D   PDsSuper                    50A   CONST
     D
     D*==> Estrutura de dados do Registo
     D WIcErro         S               N   INZ(*OFF)
     D WIcOrigem       S              1A
     D REC#            S              4S 0
     D
     D WCdProd         S              5S 0
     D WDsprod         S             50A
     D PCdProd         S              5S 0
     D PDsprod         S             50A
     D WCdCobert       S              5S 0
     D WDsCobert       S             50A
     C*========================================================
     C     *ENTRY        PLIST
     C                   PARM                    PCDPROD
     C                   PARM                    PDSPROD
      /FREE

       MONITOR;
          WCdProd   = PCdProd;
          WDsProd   = PDsProd;
       ON-ERROR;
          *INLR = *ON;
          RETURN;
       ENDMON;

       //------------------------------------
       // Se não estiver autorizado então sai
       //------------------------------------
       IF NOT $ValAccess(S#PGM:'':99999);
          *INLR = *ON;
          RETURN;
       ENDIF;
       MntLimites(WCdProd:WDsProd:*ZEROS:*BLANKS);

       *INLR = *ON;
       RETURN;
      /END-FREE

     P*===================================================================
     P* Processo..: MntLimites
     P* Descrição.: Realizar a manutenção dos Limites a diferentes níveis
     P*===================================================================
     P MntLimites      B                   EXPORT
     D MntLimites      PI
     D   PCdProd                      5S 0 CONST
     D   PDsProd                     50A   CONST
     D   PCdSuper                     5S 0 CONST
     D   PDsSuper                    50A   CONST
     D
     D WIcDisplay      S               N   INZ(*ON)
     D WModo           S               N
     D WNoFunction     S             10A   INZ(*BLANKS)
     D WSelected       S               N
     D
     D WCdSuper        S              5S 0 INZ(*ZEROS)
     D WDsSuper        S             50A   INZ(*ZEROS)
     D*======================================================================
      /FREE

       WIcDisplay =*ON;
       FillCoberturas(PCdProd:*ZEROS:*ZEROS);
       EXSR inzRecords;
       EXSR getRecords;

       ECR_DSPROD   = PDsProd;
       ECR_CDPROD   = PCdProd;
       ECR_CDSUPER  = PCdSuper;
       ECR_DSSUPER  = PDsSuper;
       DOW (NOT *IN03 AND NOT *IN12);
          WNoFunction = 'RCATE00603'; // Listar Componentes
          DS#HEADER    = $getHeader(S#PGM:WNoFunction);
          DS#FOOTER    = $getFooter(S#PGM:WNoFunction:*BLANKS);
          DS#OPTIONS   = $getOption(S#PGM:WNoFunction:*BLANKS);
          ECR_S#PGM    = S#PGM;
          ECR_DS#FNC   = $AlignText('('+%CHAR(ECR_CDPROD)+') '+ECR_DSPROD:
                         57:'C');
          WRITE     RCATE00600;
          WRITE     RCATE00699;
          WRITE     RCATE00698;
          EXFMT RCATE00603;
          IF (Func#Key = KEY#F05);
             $ChgCriteria();
             EXSR inzRecords;
             EXSR getRecords;
          ELSEIF ($ValidKey(Func#Key:KEY#F06) AND (PCdSuper > *ZEROS));
             EXSR addComponente;
             EXSR inzRecords;
             EXSR getRecords;
          ELSEIF ((Func#Key = KEY#PgDown) AND (NOT $IsLastPage())) OR
             ((Func#Key = KEY#PgUp) AND ($GetPageNbr() > 1));
             EXSR inzRecords;
             EXSR getRecords;
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
          REC#      = *ZEROS;   // Controlo de Registos da Lista
          *In53     = *On;      // Limpar Lista de Ecrã
          *In52     = *On;      // Mostrar a Lista de Ecrã
          WRITE RCATE00603;     // Limpar Lista
          *In53     = *Off;     // Repor Indicador
       ENDSR;

       //========================================================
       // Subrotina.: GetRecords
       // Objectivo.: Obter todos os registos de tabelas disponív
       //========================================================
       BEGSR getRecords;
          WIdxCobert = 1;
          DOW (NOT LST#Cobert(WIdxCobert).IsEmpty);
             //------------------------------------------------
             // Escreve os Registo da Listagem
             //------------------------------------------------
             ECR_OPTION   = *BLANKS;
             *IN49 = *OFF;
             *IN48 = *ON;
             *IN47 = *OFF;
             IF (LST#Cobert(WIdxcobert).Header = 'S');
                *IN49 = *ON;
                *IN48 = *OFF;
                *IN47 = *ON;
                ECR_TEXTABRV = LST#Cobert(WIdxCobert).DsCobert;
                REC# += 1;
                WRITE RCATE00602;
             ELSE;
                //------------------------------------------------
                // Escreve os Registo da Listagem
                //------------------------------------------------
                IF (LST#Cobert(WIdxcobert).TpHeader = 'S');
                   *IN49 = *ON;
                   *IN47 = *ON;
                ENDIF;
                ECR_CDPROD   = LST#Cobert(WIdxCobert).CdProd;
                ECR_CDSUPER  = LST#Cobert(WIdxCobert).CdSuper;
                ECR_CDLIMITE = LST#Cobert(WIdxCobert).CdLimite;
                ECR_TEXTABRV = LST#Cobert(WIdxCobert).DsLimite;
                ECR_CDCOBERT = LST#Cobert(WIdxCobert).CdCobert;
                ECR_DSCOBERT = LST#Cobert(WIdxCobert).DsCobert;
                REC# += 1;
                WRITE RCATE00602;
             ENDIF;
             WIdxCobert += 1;
          ENDDO;

          //---------------------------------------
          // Validar se ainda existem mais registos
          //---------------------------------------
          IF (REC# = *ZEROS);
             *IN52 = *OFF;
          ELSE;
             IF (NOT $IsLastPage());
                *IN54    = *OFF;
             ELSE;
                *IN54    = *ON;
             ENDIF;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: SelRec
       // Objectivo.: Seleccionar os registos
       //========================================================
       BEGSR SelRec;
          IF (REC# = *ZEROS);
             LEAVESR;
          ENDIF;
          WSelected = *OFF;
          READC RCATE00602;
          DOW (NOT %EOF());
             IF ($ValidOption(ECR_OPTION:'5'));
                WSelected = *ON;
                EXSR DspLimiteF;
             ELSEIF ($ValidOption(ECR_OPTION:'7'));
                WCdCobert = ECR_CDCOBERT;
                WDsCobert = ECR_DSCOBERT;
                MntLimites(WCdProd:WDsProd:ECR_CDLIMITE:ECR_DSLIMITE);
                *IN12 = *OFF;
             ENDIF;
             READC RCATE00602;
          ENDDO;
       ENDSR;

       //========================================================
       // Subrotina.: DspLimiteF
       // Objectivo.: Apresentar os dados do Limite e Franquia
       //========================================================
       BEGSR DspLimiteF;
          ECR_CDPROD    = PCdProd;
          ECR_DSPROD    = PDsProd;
          ECR_CDSUPER   = PCdSuper;
          ECR_DSSUPER   = PDsSuper;
          CHAIN(N) (WCDProd:ECR_CDLIMITE) RCATP103;
          IF (CAT_DSLIMITE = *BLANKS);
             *IN48 = *OFF;
             ECR_DSLIMITE = ECR_DSCOBERT;
          ELSE;
             *IN48 = *ON;
             IF (CAT_DSLIMITE <> *BLANKS);
                ECR_DSLIMITE = CAT_DSLIMITE;
             ELSE;
                ECR_DSLIMITE = ECR_DSCOBERT;
             ENDIF;
          ENDIF;

          ECR_CDRISCO   = CAT_CDRISCO;
          ECR_CDINCIND  = CAT_CDINCIND;
          ECR_DSRISCO   = $GetDescRisco(PCdProd:CAT_CDRISCO);
          ECR_DSINCIND  = $getDescricao(23:CAT_CDINCIND);
          ECR_VLPERIND  = CAT_VLPERIND;
          ECR_VLMAXIND  = CAT_VLMAXIND;
          ECR_ICSELECT  = CAT_ICSELECT;
          ECR_ICESCIND  = CAT_ICESCIND;
          ECR_CDINCFRQ  = CAT_CDINCFRQ;
          ECR_DSINCFRQ  = $getDescricao(24:CAT_CDINCFRQ);
          ECR_VLPERFRQ  = CAT_VLPERFRQ;
          ECR_VLMINFRQ  = CAT_VLMINFRQ;
          ECR_ICESCFRQ  = CAT_ICESCFRQ;
          DOW (NOT *IN03) AND (NOT *IN12);
             IF (WIcDisplay);
                WNoFunction = 'RCATE00605'; // Listar Componentes
                *IN45 = *OFF;
             ELSE;
                WNoFunction = 'RCATE00621'; // Alterar Componentes
                *IN45 = *ON;
             ENDIF;
             DS#HEADER    = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER    = $getFooter(S#PGM:WNoFunction:*BLANKS);
             ECR_S#PGM    = S#PGM;
             WRITE     RCATE00600;
             WRITE     RCATE00699;
             EXFMT RCATE00605;
             IF ($ValidKey(Func#Key:KEY#F21)) AND (WIcDisplay);
                WIcDisplay = *OFF;
             ELSEIF ($ValidKey(Func#Key:KEY#F08)) AND (WIcDisplay);
                WCdCobert = ECR_CDCOBERT;
                WDsCobert = ECR_DSCOBERT;
                MntLimites(WCdProd:WDsProd:ECR_CDLIMITE:ECR_DSLIMITE);
                *IN12 = *OFF;
             ELSEIF ($ValidKey(Func#Key:KEY#F22)) AND (WIcDisplay) AND
                    (PCdSuper > *ZEROS);
                ECR_OPTION = 'N';
                EXFMT RCATE00622;
                IF (Func#Key = KEY#Enter) AND (%TRIM(ECR_OPTION) = 'S');
                   DELETE (PCdProd:ECR_CdLimite) RCATP103;
                   LEAVESR;
                ENDIF;
                *IN12 = *OFF;
             ELSEIF (Func#Key = KEY#F12) AND (NOT WIcDisplay);
                WIcDisplay = *ON;
                *IN12 = *OFF;
             ELSEIF (Func#Key = KEY#F04) AND (NOT WIcDisplay);
                EXSR RunPopUp;
             ELSEIF (Func#Key = KEY#Enter);
                IF (NOT WIcDisplay);
                   CHAIN (PCdProd:ECR_CDLIMITE) RCATP103;
                   IF (%FOUND());
                      CAT_CDRISCO  = ECR_CDRISCO;
                      CAT_ICSELECT = ECR_ICSELECT;
                      CAT_DSLIMITE = ECR_DSLIMITE;
                      CAT_CDINCIND = ECR_CDINCIND;
                      CAT_VLPERIND = ECR_VLPERIND;
                      CAT_VLMAXIND = ECR_VLMAXIND;
                      CAT_ICESCIND = ECR_ICESCIND;
                      CAT_CDINCFRQ = ECR_CDINCFRQ;
                      CAT_VLPERFRQ = ECR_VLPERFRQ;
                      CAT_VLMINFRQ = ECR_VLMINFRQ;
                      CAT_ICESCFRQ = ECR_ICESCFRQ;
                      UPDATE RCATP103 %FIELDS(CAT_DSLIMITE:CAT_CDINCIND:
                                              CAT_VLPERIND:CAT_VLMAXIND:
                                              CAT_ICESCIND:CAT_CDINCFRQ:
                                              CAT_VLPERFRQ:CAT_VLMINFRQ:
                                              CAT_ICESCFRQ:CAT_CDRISCO:
                                              CAT_ICSELECT);
                   ENDIF;
                   WIcDisplay = *ON;
                   ECR_DSRISCO   = $GetDescRisco(PCdProd:CAT_CDRISCO);
                   ECR_DSINCIND  = $getDescricao(23:CAT_CDINCIND);
                   *IN45 = *OFF;
                   *IN12 = *OFF;
                ELSE;
                   LEAVESR;
                ENDIF;
             ENDIF;
          ENDDO;
          *IN12 = *OFF;
          WIcDisplay = *ON;
       ENDSR;

       //========================================================
       // Subrotina.: AddComponente;
       // Objectivo.: Adicionar um novo Programa
       //========================================================
       BEGSR AddComponente;
          ECR_CDLIMITE  = $getNextKey('FCATP103':'CDLIMITE':
                          'CDPROD = ''' + %CHAR(PCdProd) + '''');
          ECR_CDPROD   = WCdProd;
          ECR_CDCOBERT = WCdCobert;
          ECR_CDRISCO  = *ZEROS;
          ECR_DSLIMITE = *BLANKS;
          ECR_CDINCIND = *BLANKS;
          ECR_VLPERIND = *ZEROS;
          ECR_VLMAXIND = *ZEROS;
          ECR_ICESCIND = *BLANKS;
          ECR_CDINCFRQ = *BLANKS;
          ECR_VLPERFRQ = *ZEROS;
          ECR_VLMINFRQ = *ZEROS;
          ECR_ICESCFRQ = *BLANKS;
          DOW (NOT *IN03) AND (NOT *IN12);
             WNoFunction = 'RCATE00606';
             WIcDisplay = *OFF;
             *IN45 = *ON;
             DS#HEADER    = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER    = $getFooter(S#PGM:WNoFunction:*BLANKS);
             ECR_S#PGM    = S#PGM;
             WRITE     RCATE00600;
             WRITE     RCATE00699;
             EXFMT RCATE00605;
             IF (Func#Key = KEY#F04) AND (NOT WIcDisplay);
                EXSR RunPopUp;
             ELSEIF (Func#Key = KEY#Enter);
                CAT_CDPROD   = PCdProd;
                CAT_CDSUPER  = PCdSuper;
                CAT_CDLIMITE = ECR_CDLIMITE;
                CAT_CDCOBERT = ECR_CDCOBERT;
                CAT_CDRISCO  = ECR_CDRISCO;
                CAT_DSLIMITE = ECR_DSLIMITE;
                CAT_ICSELECT = ECR_ICSELECT;
                CAT_CDINCIND = ECR_CDINCIND;
                CAT_VLPERIND = ECR_VLPERIND;
                CAT_VLMAXIND = ECR_VLMAXIND;
                CAT_ICESCIND = ECR_ICESCIND;
                CAT_CDINCFRQ = ECR_CDINCFRQ;
                CAT_VLPERFRQ = ECR_VLPERFRQ;
                CAT_VLMINFRQ = ECR_VLMINFRQ;
                CAT_ICESCFRQ = ECR_ICESCFRQ;
                CAT_CAT10301 = $IntTodate(20000101);
                CAT_CAT10302 = $IntTodate(99991231);
                WRITE RCATP103;
                WIcDisplay = *ON;
                LEAVESR;
             ENDIF;
          ENDDO;
          *IN12 = *OFF;
          WIcDisplay = *ON;
       ENDSR;

       //==================================================================
       // Subrotina.: RunPopUp
       // Objectivo.: Adicionar um novo registo de uma Apólice
       //==================================================================
+----  BEGSR RunPopUp;
|+---     IF (ECR_CM#NMFLD = 'CDINCIND');
             ECR_CDINCIND = $runPopUp(23:ECR_CDINCIND);
          ENDIF;
|+---     IF (ECR_CM#NMFLD = 'CDINCFRQ');
             ECR_CDINCFRQ = $runPopUp(24:ECR_CDINCFRQ);
          ENDIF;
|+---     IF (ECR_CM#NMFLD = 'CDRISCO');
             ECR_CDRISCO = $runRiskPopUp(PCdProd:ECR_CDRISCO);
          ENDIF;
|         ECR_CM#ROW = CM#CROW;
|         ECR_CM#COL = CM#CCOL;
+----  ENDSR;
      /END-FREE
     PMntLimites       E

     P*===================================================================
     P* Processo..: FillCoberturas
     P* Descrição.: Actualizar lista de Coberturas
     P*===================================================================
     P FillCoberturas  B                   EXPORT
     D FillCoberturas  PI
     D   PCdProd                      5S 0 CONST
     D   PCdSuper                     5S 0 CONST
     D   PCdNivel                     5S 0 CONST
     D
     D*==> Campos de Trabalho
     D WSessionID      S              5S 0 INZ(*ZEROS)
     D WRec            S              5S 0 INZ(*ZEROS)
     D WLstIdx         S              5S 0 INZ(*ZEROS)
     D WCdNatura       S              1A   INZ(*BLANKS)
     D WBullet         S              9A   INZ(*BLANKS)
     D WCdNivel        S              3S 0 INZ(*ZEROS)
     D*=========================================================================
      /FREE
       //--------------------------------------------------
       // Declaração dos ficheiros
       //--------------------------------------------------
       IF (PCdSuper = *ZEROS);
          WSessionID = $SETFILES('FCATP103 A LEFT JOIN FCATP102 B ON +
                     (A.CDPROD = B.CDPROD) AND (A.CDCOBERT = B.CDCOBERT)');
          $SetFields('FCATP103':'A');
          $SetFields('FCATP102':'B':*OFF);
          $SetSort('CDNATURA, CDLIMITE');
          $SetCriteria('A.CDPROD = ''' + %CHAR(PCdProd) + ''' +
                        AND A.CDSUPER = A.CDLIMITE');
       ELSE;
          WSessionID = $SETFILES('FCATP103 A LEFT JOIN FCATP102 B ON +
                     (A.CDPROD = B.CDPROD) AND (A.CDCOBERT = B.CDCOBERT)+
                      AND (A.CDSUPER <> A.CDLIMITE)');
          $SetFields('FCATP103':'A');
          $SetFields('FCATP102':'B':*OFF);
          $SetSort('CDLIMITE');
          $SetCriteria('A.CDPROD = ''' + %CHAR(PCdProd) + ''' +
                        AND A.CDSUPER = ''' + %CHAR(PCdSuper) + '''');
          WCdNivel = *ZEROS;
          DOW (WCdNivel < PCdNivel);
             WBullet = %TRIM(WBullet)+'...';
             WCdNivel += 1;
          ENDDO;
       ENDIF;

       //--------------------------------------------------
       // Actualização da lista de Trabalho
       //--------------------------------------------------
       WIdxCobert = 1;
       DOW (NOT LST#Cobert(WIdxCobert).IsEmpty);
          WIdxCobert += 1;
       ENDDO;

       WRec =1;
       DOW ($GetRecord(WRec) > *ZEROS);
          //--------------------------------------------
          // Formatar o Cabeçalho
          //--------------------------------------------
          IF ((WCdNatura <> $GetField('B.CDNATURA')) AND (PCdsuper = *ZEROS));
             WCdNatura = $GetField('B.CDNATURA');
             LST#Cobert(WIdxCobert).Header   = 'S';
             LST#Cobert(WIdxCobert).CdNatura = $GetField('B.CDNATURA');
             LST#Cobert(WIdxCobert).DsCobert = $GetDescricao(57:WCdNatura);
             LST#Cobert(WIdxCobert).IsEmpty  = *OFF;
             WIdxCobert += 1;
          ENDIF;

          //--------------------------------------------
          // Formatar o Registo
          //--------------------------------------------
          LST#Cobert(WIdxCobert).Header   = 'N';
          LST#Cobert(WIdxCobert).CdProd   = $GetFieldNum('A.CDPROD');
          LST#Cobert(WIdxCobert).CdNatura = $GetField('B.CDNATURA');
          LST#Cobert(WIdxCobert).CdCobert = $GetFieldNum('A.CDCOBERT');
          LST#Cobert(WIdxCobert).CdLimite = $GetFieldNum('A.CDLIMITE');
          LST#Cobert(WIdxCobert).CdSuper  = $GetFieldNum('A.CDSUPER');
          LST#Cobert(WIdxCobert).DsCobert = $GetField('B.DSCOBERT');
          IF (PCdNivel = *ZEROS);
             LST#Cobert(WIdxCobert).DsLimite = %TRIM($GetField('B.ALIAS'))+
                                               '.'+$GetField('B.DSCOBERT');
          ELSE;
             LST#Cobert(WIdxCobert).DsLimite = $GetField('B.DsCobert');
             IF ($GetField('A.DSLIMITE') <> *BLANKS);
                LST#Cobert(WIdxCobert).DsLimite = %TRIM(WBullet) +
                                                  $GetField('A.DSLIMITE');
             ENDIF;
          ENDIF;
          LST#Cobert(WIdxCobert).DsLimite = %SUBST(LST#Cobert(WIdxCobert)
                                               .DsLimite:1:43);
          LST#Cobert(WIdxCobert).CdNivel  = PCdNivel;
          LST#Cobert(WIdxCobert).TpHeader = 'N';
          LST#Cobert(WIdxCobert).IsEmpty  = *OFF;
          LST#Cobert(WIdxCobert+1).IsEmpty = *ON;

          //--------------------------------------------
          // Formatar os Dependentes
          //--------------------------------------------
          WLstIdx = WIdxCobert;
          WIsCobHeader = *OFF;
          FillCoberturas(PCdProd:$GetFieldNum('A.CDLIMITE'):(PCdNivel+1));
          IF (WIsCobHeader);
             LST#Cobert(WLstIdx).TpHeader = 'S';
          ENDIF;
          IF ($GetFieldNum('B.CDCOBERT') <> $GetfieldNum('B.CDSUPER'))
             AND ($GetfieldNum('B.CDCOBERT') <> *ZEROS);
             WIsCobHeader = *ON;
          ENDIF;
          WRec += 1;
       ENDDO;

       $RmvDBSession(WSessionID);
      /END-FREE
     PFillCoberturas   E
