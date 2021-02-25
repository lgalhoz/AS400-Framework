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
     (*      45      Condicionar Campos com Input/Output
     F*      52      DSP
     F*      53      CLR
     F*      54      END
     D*==========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SSECO010
      /Copy QRPGLESRC,SCATO010

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
     D WCdNatura       S              1A
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
     D
     D WSessionID      S              5S 0 INZ(*ZEROS)
      /FREE

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
                     (A.CDPROD = B.CDPROD) AND (A.CDCOBERT = B.CDCOBERT)');
          $SetFields('FCATP103':'A');
          $SetFields('FCATP102':'B':*OFF);
          $SetSort('CDLIMITE');
          $SetCriteria('A.CDPROD = ''' + %CHAR(PCdProd) + ''' +
                        AND A.CDSUPER = ''' + %CHAR(PCdSuper) + '''');
       ENDIF;
       $SetPage(12);
       WIcDisplay =*ON;
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
             $SetDBSession(WSessionID);
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

       $RmvDBSession(WSessionID);
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
          WCdNatura = *BLANKS;
          IF ($GetPage(Func#Key) > *ZEROS);
             DOW (NOT $IsEOF());
                //------------------------------------------------
                // Escreve os Registo da Listagem
                //------------------------------------------------
                ECR_OPTION   = *BLANKS;
                IF (WCdNatura<>%TRIM($GetField('B.CDNATURA')));
                   *IN45 = *OFF;
                   WCdNatura = %TRIM($GEtField('B.CDNATURA'));
                   ECR_TEXTABRV = $GetDescricao(57:WCdNatura);
                   REC# += 1;
                   WRITE RCATE00602;
                   *IN45 = *ON;
                ENDIF;
                MONITOR;
                   ECR_CDPROD   = %DEC($GetField('A.CDPROD'):5:0);
                   ECR_CDSUPER  = PCdSuper;
                   ECR_CDLIMITE = %DEC($GetField('A.CDLIMITE'):5:0);
                ON-ERROR;
                ENDMON;
                ECR_TEXTABRV = $GetField('A.DSLIMITE');
                IF (ECR_TEXTABRV = *BLANKS);
                   ECR_TEXTABRV = '('+%TRIM($GetField('B.ALIAS'))+') '+
                                  $GetField('B.DSCOBERT');

                ENDIF;
                ECR_CDCOBERT = %DEC($GetField('A.CDCOBERT'):5:0);
                ECR_DSCOBERT = $GetField('B.DSCOBERT');
                IF (ECR_CDLIMITE <> ECR_CDSUPER);
                   SETLL (WCdProd:ECR_CDLIMITE) RCATP10301;
                   READE (WCdProd:ECR_CDLIMITE) RCATP10301;
                   DOW (NOT %EOF());
                      IF (CAT_CDSUPER <> CAT_CDLIMITE);
                         ECR_TEXTABRV = '...' + ECR_TEXTABRV;
                         LEAVE;
                      ENDIF;
                      READE (WCdProd:ECR_CDLIMITE) RCATP10301;
                   ENDDO;
                   REC# += 1;
                   WRITE RCATE00602;
                ENDIF;
                IF (REC#=12);
                   LEAVE;
                ENDIF;
                $getNext();
             ENDDO;
          ENDIF;

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
                $SetDBSession(WSessionID);
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
                $SetDBSession(WSessionID);
             ELSEIF ($ValidKey(Func#Key:KEY#F08)) AND (WIcDisplay);
                WCdCobert = ECR_CDCOBERT;
                WDsCobert = ECR_DSCOBERT;
                MntLimites(WCdProd:WDsProd:ECR_CDLIMITE:ECR_DSLIMITE);
                $SetDBSession(WSessionID);
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
                $SetDBSession(WSessionID);
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
                   $SetDBSession(WsessionID);
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
                $SetDBSession(WSessionID);
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
