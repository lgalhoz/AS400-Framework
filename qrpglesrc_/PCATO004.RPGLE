     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PCATO006
     H* Descrição.: Manutenção de Limites e Franquias
     H*==================================================================
     FFCATP10601IF   E           K DISK    PREFIX(CAT_)
     F                                     RENAME(RCATP106:RCATP10601)
     FFCATP106  UF A E           K DISK    PREFIX(CAT_)
     FFCATP105  UF A E           K DISK    PREFIX(CAT_)
     FFCATE004  CF   E             WORKSTN SFILE(RCATE00402:REC#)
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
     D MntTextos       PR
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
     D PCdRisc         S              5S 0
     D PDsprod         S             50A
     D WCdRisco        S              5S 0
     D WDsRisco        S             50A
     C*========================================================
     C     *ENTRY        PLIST
     C                   PARM                    PCDPROD
     C                   PARM                    PDSPROD
     C                   PARM                    PCDRISC
      /FREE

       MONITOR;
          WCdProd   = PCdProd;
          WDsProd   = PDsProd;
          MONITOR;
             WCdRisco  = PCdRisc;
          ON-ERROR;
             WCdRisco  = *ZEROS;
          ENDMON;
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
       MntTextos(WCdProd:WDsProd:*ZEROS:*BLANKS);

       *INLR = *ON;
       RETURN;
      /END-FREE

     P*===================================================================
     P* Processo..: MntTextos
     P* Descrição.: Realizar a manutenção dos Textos associados aos Riscos
     P*===================================================================
     P MntTextos       B                   EXPORT
     D MntTextos       PI
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

       WSessionID = $AddDBSession();
       IF (PCdSuper = *ZEROS);
          $SETFILES('FCATP106 A INNER JOIN +
                     FCATP105 B ON (A.CDPROD = B.CDPROD AND A.CDTEXTO +
                     = B.CDTEXTO)');
          $SETFIELDS('A.CDPROD, B.CDRISCO, A.CDTEXTO, A.DSTEXTO, +
                      A.CDSUPER');
          $SetSort('CDRISCO');
          IF (WCdRisco = *ZEROS);
             $SetCriteria('A.CDPROD = ''' + %CHAR(PCdProd) + ''' +
                        AND A.CDSUPER = A.CDTEXTO');
          ELSE;
             $SetCriteria('A.CDPROD = ''' + %CHAR(PCdProd) + ''' +
                        AND B.CDRISCO = ''' + %CHAR(WCdRisco) + ''' +
                        AND A.CDSUPER = A.CDTEXTO');
          ENDIF;
       ELSE;
          $SETFILES('FCATP106 A');
          $SETFIELDS('A.CDPROD, A.CDTEXTO, A.DSTEXTO, A.CDSUPER');
          $SetSort('CDTEXTO');
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
          WNoFunction = 'RCATE00403'; // Listar Componentes
          DS#HEADER    = $getHeader(S#PGM:WNoFunction);
          DS#FOOTER    = $getFooter(S#PGM:WNoFunction:*BLANKS);
          DS#OPTIONS   = $getOption(S#PGM:WNoFunction:*BLANKS);
          ECR_S#PGM    = S#PGM;
          ECR_DS#FNC   = $AlignText('('+%CHAR(ECR_CDPROD)+') '+ECR_DSPROD:
                         57:'C');
          WRITE     RCATE00400;
          WRITE     RCATE00499;
          WRITE     RCATE00498;
          EXFMT RCATE00403;
          IF (Func#Key = KEY#F05);
             $ChgCriteria();
             EXSR inzRecords;
             EXSR getRecords;
          ELSEIF ($ValidKey(Func#Key:KEY#F06));
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
          WRITE RCATE00403;     // Limpar Lista
          *In53     = *Off;     // Repor Indicador
       ENDSR;

       //========================================================
       // Subrotina.: GetRecords
       // Objectivo.: Obter todos os registos de tabelas disponív
       //========================================================
       BEGSR getRecords;
          ECR_CDRISCO = WCdRisco;
          IF ($GetPage(Func#Key) > *ZEROS);
             DOW (NOT $IsEOF());
                //------------------------------------------------
                // Escreve os Registo da Listagem
                //------------------------------------------------
                ECR_OPTION   = *BLANKS;
                MONITOR;
                   ECR_CDPROD   = %DEC($GetField('A.CDPROD'):5:0);
                   ECR_CDSUPER  = PCdSuper;
                   ECR_CDTEXTO  = %DEC($GetField('A.CDTEXTO'):5:0);
                   ECR_CDRISCO  = %DEC($GetField('B.CDRISCO'):5:0);
                ON-ERROR;
                ENDMON;
                IF (ECR_CDRISCO = *ZEROS);
                   ECR_CDRISCO  = WCdRisco;
                ELSE;
                   WCdRisco = ECR_CDRISCO;
                ENDIF;
                ECR_DSRISCO   = $GetDescRisco(PCdProd:WCdRisco);
                ECR_TEXTABRV = '('+%TRIM(ECR_DSRISCO)+') '+
                                 $GetField('A.DSTEXTO');
                IF (ECR_CDSUPER <> ECR_CDTEXTO);
                   SETLL (WCdProd:ECR_CDTEXTO ) RCATP10601;
                   READE (WCdProd:ECR_CDTEXTO ) RCATP10601;
                   DOW (NOT %EOF());
                      IF (CAT_CDSUPER <> CAT_CDTEXTO);
                         ECR_TEXTABRV = '...' + ECR_TEXTABRV;
                         LEAVE;
                      ENDIF;
                      READE (WCdProd:ECR_CDTEXTO ) RCATP10601;
                   ENDDO;
                   REC# += 1;
                   WRITE RCATE00402;
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
          READC RCATE00402;
          DOW (NOT %EOF());
             IF ($ValidOption(ECR_OPTION:'5'));
                WSelected = *ON;
                EXSR DspLimiteF;
             ELSEIF ($ValidOption(ECR_OPTION:'7'));
                WCdRisco = ECR_CDRISCO;
                WDsRisco = $GetDescRisco(PCdProd:WCdRisco);
                MntTextos(WCdProd:WDsProd:ECR_CDTEXTO :ECR_DSTEXTO);
                $SetDBSession(WSessionID);
                *IN12 = *OFF;
             ENDIF;
             READC RCATE00402;
          ENDDO;
       ENDSR;

       //========================================================
       // Subrotina.: DspLimiteF
       // Objectivo.: Apresentar os dados
       //========================================================
       BEGSR DspLimiteF;
          ECR_CDPROD    = PCdProd;
          ECR_DSPROD    = PDsProd;
          ECR_DSRISCO   = $GetDescRisco(PCdProd:ECR_CDRISCO);
          ECR_CDSUPER   = PCdSuper;
          ECR_DSSUPER   = PDsSuper;
          CHAIN(N) (WCDProd:ECR_CDTEXTO ) RCATP106;
          ECR_DSTEXTO = CAT_DSTEXTO;
          ECR_NUORDEM = CAT_NUORDEM;
          ECR_ICOBRIGA = CAT_ICOBRIGA;
          ECR_ICRESERV = CAT_ICRESERV;
          ECR_ICESCREV = CAT_ICESCREV;
          ECR_CDFUNCAO = CAT_CDFUNCT;
          ECR_DSFUNCAO = $GetDescFunct(CAT_CDFUNCT);
          ECR_ICDETAIL = CAT_ICDETAIL;
          ECR_CAT10601 = $DateToInt(CAT_CAT10601);
          ECR_CAT10602 = $DateToInt(CAT_CAT10602);
          ECR_ALIAS    = CAT_ALIAS;
          ECR_CONVERSAO= CAT_CONVERSAO;
          DOW (NOT *IN03) AND (NOT *IN12);
             IF (WIcDisplay);
                WNoFunction = 'RCATE00405'; // Listar Componentes
                *IN45 = *OFF;
                *IN48 = *OFF;
             ELSE;
                WNoFunction = 'RCATE00421'; // Alterar Componentes
                *IN45 = *ON;
             ENDIF;
             DS#HEADER    = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER    = $getFooter(S#PGM:WNoFunction:*BLANKS);
             ECR_S#PGM    = S#PGM;
             WRITE     RCATE00400;
             WRITE     RCATE00499;
             EXFMT RCATE00405;
             IF ($ValidKey(Func#Key:KEY#F21)) AND (WIcDisplay);
                WIcDisplay = *OFF;
                $SetDBSession(WSessionID);
             ELSEIF ($ValidKey(Func#Key:KEY#F08)) AND (WIcDisplay);
                WCdRisco = ECR_CDRISCO;
                WDsRisco = $GetDescRisco(PCdProd:WCdRisco);
                MntTextos(WCdProd:WDsProd:ECR_CDTEXTO :ECR_DSTEXTO);
                $SetDBSession(WSessionID);
                *IN12 = *OFF;
             ELSEIF ($ValidKey(Func#Key:KEY#F22)) AND (WIcDisplay);
                ECR_OPTION = 'N';
                EXFMT RCATE00422;
                IF (Func#Key = KEY#Enter) AND (%TRIM(ECR_OPTION) = 'S');
                   DELETE (PCdProd:ECR_CDTEXTO ) RCATP106;
                   LEAVESR;
                ENDIF;
                *IN12 = *OFF;
             ELSEIF (Func#Key = KEY#F12) AND (NOT WIcDisplay);
                WIcDisplay = *ON;
                *IN12 = *OFF;
             ELSEIF (Func#Key = KEY#F04) AND (NOT WIcDisplay);
                EXSR RunPopUp;
                $SetDBSession(WSessionID);
                ITER;
             ELSEIF (Func#Key = KEY#Enter);
                IF (NOT WIcDisplay);
                   CHAIN (PCdProd:ECR_CDTEXTO ) RCATP106;
                   IF (%FOUND());
                      CAT_DSTEXTO   = ECR_DSTEXTO;
                      CAT_NUORDEM   = ECR_NUORDEM;
                      CAT_ICOBRIGA  = ECR_ICOBRIGA;
                      CAT_ICRESERV  = ECR_ICRESERV;
                      CAT_ICESCREV  = ECR_ICESCREV;
                      CAT_CDFUNCT   = ECR_CDFUNCAO;
                      CAT_ICDETAIL  = ECR_ICDETAIL;
                      CAT_CAT10601  = $IntToDate(ECR_CAT10601);
                      CAT_CAT10602  = $IntToDate(ECR_CAT10602);
                      CAT_ALIAS     = ECR_ALIAS;
                      CAT_CONVERSAO = ECR_CONVERSAO;
                      UPDATE RCATP106 %FIELDS(CAT_DSTEXTO:CAT_NUORDEM:
                                              CAT_CDGRUPO:CAT_ICOBRIGA:
                                              CAT_ICRESERV:CAT_CDFUNCT:
                                              CAT_ICDETAIL:CAT_CAT10601:
                                              CAT_CAT10602:CAT_ALIAS:
                                              CAT_CONVERSAO:CAT_ICESCREV);
                      ECR_DSFUNCAO = $GetDescFunct(ECR_CDFUNCAO);
                   ENDIF;
                   $SetDBSession(WSessionID);
                   WIcDisplay = *ON;
                   *IN45 = *OFF;
                   *IN12 = *OFF;
                ELSE;
                   LEAVESR;
                ENDIF;
             ENDIF;
          ENDDO;
          *IN12 = *OFF;
       ENDSR;

       //========================================================
       // Subrotina.: AddComponente;
       // Objectivo.: Adicionar um novo Programa
       //========================================================
       BEGSR AddComponente;
          ECR_CDPROD    = PCdProd;
          ECR_DSPROD    = PDsProd;
          ECR_DSRISCO   = $GetDescRisco(PCdProd:ECR_CDRISCO);
          ECR_CDSUPER   = PCdSuper;
          ECR_DSSUPER   = PDsSuper;
          ECR_CDTEXTO   = $getNextKey('FCATP106':'CDTEXTO':
                          'CDPROD = ''' + %CHAR(PCdProd) + '''');
          ECR_DSTEXTO   = *BLANKS;
          ECR_NUORDEM   = *ZEROS;
          ECR_ICOBRIGA  = 'N';
          ECR_ICRESERV  = 'N';
          ECR_CDFUNCAO  = 17;
          ECR_ICDETAIL  = 'N';
          ECR_CAT10601  = *ZEROS;
          ECR_CAT10602  = *ZEROS;
          ECR_ALIAS     = *BLANKS;
          ECR_CONVERSAO = *BLANKS;
          *IN48 = *OFF;
          // IF (PCdSuper = *ZEROS) AND (WCdRisco = *ZEROS);
          //     *IN48 = *ON;
          // ENDIF;
          DOW (NOT *IN03) AND (NOT *IN12);
             WNoFunction = 'RCATE00406';
             WIcDisplay = *OFF;
             *IN45 = *ON;
             DS#HEADER    = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER    = $getFooter(S#PGM:WNoFunction:*BLANKS);
             ECR_S#PGM    = S#PGM;
             WRITE     RCATE00400;
             WRITE     RCATE00499;
             EXFMT RCATE00405;
             IF (Func#Key = KEY#F04) AND (NOT WIcDisplay);
                EXSR RunPopUp;
                $SetDBSession(WSessionID);
                ITER;
             ELSEIF (Func#Key = KEY#Enter);
                CAT_CDPROD    = PCdProd;
                CAT_CDTEXTO   = ECR_CDTEXTO;
                IF (PCdSuper = *ZEROS);
                   CAT_CDSUPER = CAT_CDTEXTO;
                ELSE;
                   CAT_CDSUPER   = PCdSuper;
                ENDIF;
                CAT_DSTEXTO   = ECR_DSTEXTO;
                CAT_NUORDEM   = ECR_NUORDEM;
                CAT_ICOBRIGA  = ECR_ICOBRIGA;
                CAT_ICRESERV  = ECR_ICRESERV;
                CAT_ICESCREV  = ECR_ICESCREV;
                CAT_CDFUNCT   = ECR_CDFUNCAO;
                CAT_ICDETAIL  = ECR_ICDETAIL;
                CAT_CAT10601  = $IntToDate(ECR_CAT10601);
                CAT_CAT10602  = $IntToDate(ECR_CAT10602);
                CAT_ALIAS     = ECR_ALIAS;
                CAT_CONVERSAO = ECR_CONVERSAO;
                WRITE RCATP106;
                IF (PCDSuper = *ZEROS);
                   CAT_CDRISCO   = ECR_CDRISCO;
                   WRITE RCATP105;
                ENDIF;
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
          // ECR_CDINCIND = $runPopUp(23:ECR_CDINCIND);
|+---     ELSEIF (ECR_CM#NMFLD = 'CDFUNCAO');
             ECR_CDFUNCAO = $runPopUpTempl(13:ECR_CDFUNCAO);
             ECR_DSFUNCAO = $GetDescFunct(ECR_CDFUNCAO);
|+---     ELSEIF (ECR_CM#NMFLD = 'CDRISCO');
             ECR_CDRISCO = $runRiskPopUp(PCdProd:ECR_CDRISCO);
             ECR_DSRISCO   = $GetDescRisco(PCdProd:ECR_CDRISCO);
          ENDIF;
|         ECR_CM#ROW = CM#CROW;
|         ECR_CM#COL = CM#CCOL;
+----  ENDSR;
      /END-FREE
     PMntTextos        E
