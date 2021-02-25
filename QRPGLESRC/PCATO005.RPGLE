     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PCATO001
     H* Descrição.: Manutenção de Produtos
     H*==================================================================
     FFCATP108  UF A E           K DISK    PREFIX(CAT_)
     FFCATE005  CF   E             WORKSTN SFILE(RCATE00502:REC#)
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

     D DspTextos       PR                  EXTPGM('PCATO009')
     D   WCdProd                      5S 0 CONST
     D   WDsProd                     40A   CONST
     D   WCdCobert                    5S 0 CONST
     D   WDsCobert                   40A   CONST
     D
     D*==> Estrutura de dados do Registo
     D WIcErro         S               N   INZ(*OFF)
     D WIcOrigem       S              1A
     D REC#            S              4S 0
     D
     D WIcDisplay      S               N   INZ(*ON)
     D WModo           S               N
     D WNoFunction     S             10A   INZ(*BLANKS)
     D WSelected       S               N
     D WTpItem         S              1A
     D
     D WCdProd         S              5S 0
     D WDsprod         S             50A
     D PCdProd         S              5S 0
     D PDsprod         S             50A
     D
     D WSessionID      S              5S 0
     C*========================================================
     C     *ENTRY        PLIST
     C                   PARM                    PCDPROD
     C                   PARM                    PDSPROD
      /FREE

       MONITOR;
          WCdProd = PCdProd;
          WDsProd = PDsProd;
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

       WSessionID = $SETFILES('FCATP108');
       $SetPage(10);
       $SetCriteria('CDPROD = ''' + %CHAR(WCdProd) + '''');
       $SetSort('TPITEM, NUORDEM');
       WIcDisplay =*ON;
       EXSR inzRecords;
       EXSR getRecords;

       ECR_DSPROD = WDsProd;
       ECR_CDPROD = WCdProd;
       DOW (NOT *IN03 AND NOT *IN12);
          WNoFunction = 'RCATE00503'; // Listar Componentes
          DS#HEADER    = $getHeader(S#PGM:WNoFunction);
          DS#FOOTER    = $getFooter(S#PGM:WNoFunction:*BLANKS);
          DS#OPTIONS   = $getOption(S#PGM:WNoFunction:*BLANKS);
          ECR_S#PGM    = S#PGM;
          ECR_DS#FNC   = $AlignText('('+%CHAR(ECR_CDPROD)+') '+ECR_DSPROD:
                         57:'C');
          WRITE     RCATE00500;
          WRITE     RCATE00599;
          WRITE     RCATE00598;
          EXFMT RCATE00503;
          IF (Func#Key = KEY#F05);
             $ChgCriteria();
             // ECR_DSCRITER = $getCritText();
             EXSR inzRecords;
             EXSR getRecords;
          ELSEIF ($ValidKey(Func#Key:KEY#F06));
             EXSR addComponente;
             $SetDBSession(WsessionID);
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
       *INLR = *ON;
       RETURN;

       //========================================================
       // Subrotina.: InzRecTxt
       // Objectivo.: Inicializar a lista de registos
       //========================================================
       BEGSR inzRecords;
          REC#      = *ZEROS;   // Controlo de Registos da Lista
          *In53     = *On;      // Limpar Lista de Ecrã
          *In52     = *On;      // Mostrar a Lista de Ecrã
          WRITE RCATE00503;     // Limpar Lista
          *In53     = *Off;     // Repor Indicador
       ENDSR;

       //========================================================
       // Subrotina.: GetRecords
       // Objectivo.: Obter todos os registos de tabelas disponív
       //========================================================
       BEGSR getRecords;
          WTpItem = *BLANKS;
          IF ($GetPage(Func#Key) > *ZEROS);
             DOW (NOT $IsEOF());
                //------------------------------------------------
                // Escreve os Registo da Listagem
                //------------------------------------------------
                IF (REC# < 11) AND (WTpItem <> $getField('TPITEM'));
                   ECR_OPTION   = *BLANKS;
                   WTpItem = $getField('TPITEM');
                   ECR_DSITEMABRV = $GetDescricao(25:$GetField('TPITEM'));
                   ECR_ICOBRIGA   = *BLANKS;
                   *IN49 = *ON;
                   REC# += 1;
                   WRITE RCATE00502;
                ENDIF;
                REC# += 1;
                *IN49 = *OFF;
                ECR_CDPROD   = WCdProd;
                MONITOR;
                   ECR_NUSEQU = %DEC($GetField('NUSEQU'):5:0);
                   ECR_NUORDEM = %DEC($GetField('NUORDEM'):5:0);
                ON-ERROR;
                ENDMON;
                ECR_DSITEMABRV= %TRIM($GetField('CDITEM'))+' '+
                                $GetField('DSITEM');
                ECR_ICOBRIGA  = $GetField('ICOBRIGA');
                WRITE RCATE00502;
                IF (REC# = 11);
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
            *IN54 = $IsLastPage();
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
          READC RCATE00502;
          DOW (NOT %EOF());
             IF ($ValidOption(ECR_OPTION:'5'));
                WSelected = *ON;
                EXSR DspClausula;
             ELSEIF ($ValidOption(ECR_OPTION:'7'));
                MONITOR;
                 DspTextos(ECR_CDPROD:ECR_DSPROD:ECR_NUSEQU:
                     ECR_CDITEM+' '+ECR_DSITEM1);
                ON-ERROR;
                ENDMON;
                $SetDBSession(WSessionID);
             ENDIF;
             READC RCATE00502;
          ENDDO;
       ENDSR;

       //========================================================
       // Subrotina.: DspClausula
       // Objectivo.: Apresentar os dados da Clausula
       //========================================================
       BEGSR DspClausula;
          CHAIN(N) (WCDProd:ECR_NUSEQU) RCATP108;
          ECR_CDPROD    = WCdProd;
          ECR_DSPROD    = WDsProd;
          ECR_NUSEQU    = CAT_NUSEQU;
          ECR_TPITEM    = CAT_TPITEM;
          ECR_CDVAR     = CAT_CDVAR;
          ECR_DSCDVAR   = $GetDescFunct(ECR_CDVAR);
          ECR_NUORDEM   = CAT_NUORDEM;
          ECR_CDITEM    = CAT_CDITEM;
          ECR_CDRISCO   = CAT_CDRISCO;
          ECR_DSRISCO   = $GetDescRisco(PCdProd:ECR_CDRISCO);
          ECR_DSITEM1   = %SUBST(CAT_DSITEM:1:50);
          ECR_DSITEM2   = %SUBST(CAT_DSITEM:51:50);
          ECR_ICOBRIGA  = CAT_ICOBRIGA;
          ECR_ICDEFEITO = CAT_ICDEFEITO;
          ECR_ALIAS     = CAT_ALIAS;
          ECR_CONVERSAO = CAT_CONVERSAO;
          DOW (NOT *IN03) AND (NOT *IN12);
             IF (WIcDisplay);
                WNoFunction = 'RCATE00505'; // Listar Componentes
                *IN45 = *OFF;
             ELSE;
                WNoFunction = 'RCATE00521'; // Alterar Componentes
                *IN45 = *ON;
             ENDIF;
             DS#HEADER    = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER    = $getFooter(S#PGM:WNoFunction:*BLANKS);
             ECR_S#PGM    = S#PGM;
             WRITE     RCATE00500;
             WRITE     RCATE00599;
             EXFMT RCATE00505;
             IF ($ValidKey(Func#Key:KEY#F21)) AND (WIcDisplay);
                WIcDisplay = *OFF;
                $SetDBSession(WSessionID);
             ELSEIF ($ValidKey(Func#Key:KEY#F22)) AND (WIcDisplay);
                ECR_OPTION = 'N';
                EXFMT RCATE00522;
                IF (Func#Key = KEY#Enter) AND (%TRIM(ECR_OPTION) = 'S');
                   DELETE (ECR_CDPROD:ECR_NUSEQU) RCATP108;
                   LEAVESR;
                ENDIF;
                *IN12 = *OFF;
             ELSEIF ($ValidKey(Func#Key:KEY#F08));
                MONITOR;
                 DspTextos(ECR_CDPROD:ECR_DSPROD:ECR_NUSEQU:
                     ECR_CDITEM+' '+ECR_DSITEM1);
                ON-ERROR;
                ENDMON;
                $SetDBSession(WSessionID);
             ELSEIF (Func#Key = KEY#F12) AND (NOT WIcDisplay);
                WIcDisplay = *ON;
                *IN12 = *OFF;
             ELSEIF (Func#Key = KEY#F04) AND (NOT WIcDisplay);
                EXSR RunPopUp;
                $SetDBSession(WSessionID);
             ELSEIF (Func#Key = KEY#Enter);
                IF (NOT WIcDisplay);
                   CHAIN (ECR_CDPROD:ECR_NUSEQU) RCATP108;
                   IF (%FOUND());
                      CAT_CDPROD    = WCdProd;
                      CAT_NUORDEM   = ECR_NUORDEM;
                      CAT_CDITEM    = ECR_CDITEM;
                      CAT_CDRISCO   = ECR_CDRISCO;
                      CAT_DSITEM    = ECR_DSITEM1 + ECR_DSITEM2;
                      CAT_ICOBRIGA  = ECR_ICOBRIGA;
                      CAT_ICDEFEITO = ECR_ICDEFEITO;
                      CAT_ALIAS     = ECR_ALIAS;
                      CAT_CDVAR     = ECR_CDVAR;
                      CAT_CONVERSAO = ECR_CONVERSAO;
                      UPDATE RCATP108 %FIELDS(CAT_NUORDEM:CAT_CDITEM:
                                              CAT_DSITEM:CAT_ICOBRIGA:
                                              CAT_ALIAS:CAT_CONVERSAO:
                                              CAT_CDVAR:CAT_CDRISCO:
                                              CAT_ICDEFEITO);
                   ENDIF;
                   $SetDBSession(WsessionID);
                   WIcDisplay = *ON;
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
          ECR_NUSEQU = $GetNextKey('FCATP108':'NUSEQU':
                       'CDPROD = ''' + %CHAR(WCdProd) + '''');
          ECR_TPITEM    = *BLANKS;
          ECR_NUORDEM   = *ZEROS;
          ECR_CDVAR     = *ZEROS;
          ECR_CDITEM    = *BLANKS;
          ECR_CDRISCO   = *ZEROS;
          ECR_DSRISCO   = *BLANKS;
          ECR_DSITEM1   = *BLANKS;
          ECR_DSITEM2   = *BLANKS;
          ECR_ICOBRIGA  = 'N';
          ECR_ICDEFEITO = 'N';
          ECR_ALIAS     = *BLANKS;
          ECR_CONVERSAO = *BLANKS;
          DOW (NOT *IN03) AND (NOT *IN12);
             WNoFunction = 'RCATE00506';
             WIcDisplay = *OFF;
             *IN45 = *ON;
             DS#HEADER    = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER    = $getFooter(S#PGM:WNoFunction:*BLANKS);
             ECR_S#PGM    = S#PGM;
             WRITE     RCATE00500;
             WRITE     RCATE00599;
             EXFMT RCATE00505;
             IF (Func#Key = KEY#F04) AND (NOT WIcDisplay);
                EXSR RunPopUp;
                $SetDBSession(WSessionID);
             ELSEIF (Func#Key = KEY#Enter);
                CAT_CDPROD    = WCdProd;
                CAT_NUSEQU    = ECR_NUSEQU;
                CAT_TPITEM    = ECR_TPITEM;
                CAT_CDVAR     = ECR_CDVAR;
                CAT_NUORDEM   = ECR_NUORDEM;
                CAT_CDITEM    = ECR_CDITEM;
                CAT_CDRISCO   = ECR_CDRISCO;
                CAT_DSITEM    = ECR_DSITEM1 + ECR_DSITEM2;
                CAT_ICOBRIGA  = ECR_ICOBRIGA;
                CAT_ICDEFEITO = ECR_ICDEFEITO;
                CAT_ALIAS     = ECR_ALIAS;
                CAT_CONVERSAO = ECR_CONVERSAO;
                CAT_CAT10801 = $IntTodate(20000101);
                CAT_CAT10802 = $IntTodate(99991231);
                WRITE RCATP108;
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
|+---     IF (ECR_CM#NMFLD = 'TPITEM');
             ECR_TPITEM = $runPopUp(25:ECR_TPITEM);
|+---     ELSEIF (ECR_CM#NMFLD = 'CDVAR');
             ECR_CDVAR   = $RunPopUpTempl(125:ECR_CDVAR);
             ECR_DSCDVAR = $GetDescFunct(ECR_CDVAR);
|+---     ELSEIF (ECR_CM#NMFLD = 'CDRISCO');
             ECR_CDRISCO = $runRiskPopUp(PCdProd:ECR_CDRISCO);
             ECR_DSRISCO   = $GetDescRisco(PCdProd:ECR_CDRISCO);
          ENDIF;
|         ECR_CM#ROW = CM#CROW;
|         ECR_CM#COL = CM#CCOL;
+----  ENDSR;
      /END-FREE
