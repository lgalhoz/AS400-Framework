     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PCATO001
     H* Descrição.: Manutenção de Produtos
     H*==================================================================
     FFCATP101  UF A E           K DISK    PREFIX(CAT_)
     FFCATE003  CF   E             WORKSTN SFILE(RCATE00302:REC#)
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

     D DspNotasDesc    PR                  EXTPGM('PCATO004')
     D   WCdProd                      5S 0 CONST
     D   WDsprod                     50A   CONST
     D   WCdRisc                      5S 0 CONST
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

       WSessionID = $SETFILES('FCATP101');
       $SetPage(12);
       $SetCriteria('CDPROD = ''' + %CHAR(WCdProd) + '''');
       $SetSort('CDRISCO');
       WIcDisplay =*ON;
       EXSR inzRecords;
       EXSR getRecords;

       ECR_DSPROD = WDsProd;
       ECR_CDPROD = WCdProd;
       DOW (NOT *IN03 AND NOT *IN12);
          WNoFunction = 'RCATE00303'; // Listar Componentes
          DS#HEADER    = $getHeader(S#PGM:WNoFunction);
          DS#FOOTER    = $getFooter(S#PGM:WNoFunction:*BLANKS);
          DS#OPTIONS   = $getOption(S#PGM:WNoFunction:*BLANKS);
          ECR_S#PGM    = S#PGM;
          ECR_DS#FNC   = $AlignText('('+%CHAR(ECR_CDPROD)+') '+ECR_DSPROD:
                         57:'C');
          WRITE     RCATE00300;
          WRITE     RCATE00399;
          WRITE     RCATE00398;
          EXFMT RCATE00303;
          IF (Func#Key = KEY#F05);
             $ChgCriteria();
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
          WRITE RCATE00303;     // Limpar Lista
          *In53     = *Off;     // Repor Indicador
       ENDSR;

       //========================================================
       // Subrotina.: GetRecords
       // Objectivo.: Obter todos os registos de tabelas disponív
       //========================================================
       BEGSR getRecords;
          IF ($GetPage(Func#Key) > *ZEROS);
             DOW (NOT $IsEOF());

                //------------------------------------------------
                // Escreve os Registo da Listagem
                //------------------------------------------------
                REC# += 1;
                ECR_OPTION   = *BLANKS;
                ECR_CDPROD   = WCdProd;
                ECR_CDRISCO   = $GetFieldNum('CDRISCO');
                ECR_CDSUPER   = $GetFieldNum('CDSUPER');
                ECR_DSRISCO   = $GetField('DSRISCO');
                ECR_ICINDEX   = $GetField('ICINDEX');
                ECR_ICCAPTOT  = $GetField('ICCAPTOT');
                ECR_ICESCACT  = $GetField('ICESCACT');
                ECR_ALIAS     = $GetField('ALIAS');
                ECR_CONVERSAO = $GetField('CONVERSAO');
                WRITE RCATE00302;
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
          READC RCATE00302;
          DOW (NOT %EOF());
             IF ($ValidOption(ECR_OPTION:'5'));
                WSelected = *ON;
                EXSR DspCoberta;
             ELSEIF ($ValidOption(ECR_OPTION:'7'));
                DspNotasDesc(WCdProd:WDsProd:ECR_CDRISCO);
                $SetDBSession(WSessionID);
             ENDIF;
             READC RCATE00302;
          ENDDO;
       ENDSR;

       //========================================================
       // Subrotina.: DspCoberta
       // Objectivo.: Apresentar os dados da Cobertura
       //========================================================
       BEGSR DspCoberta;
          CHAIN(N) (WCDProd:ECR_CDRISCO) RCATP101;
          ECR_CDPROD    = WCdProd;
          ECR_DSPROD    = WDsProd;
          ECR_DSRISCO   = CAT_DSRISCO;
          ECR_ICINDEX   = CAT_ICINDEX;
          ECR_ICCAPTOT  = CAT_ICCAPTOT;
          ECR_ICESCACT  = CAT_ICESCACT;
          ECR_CDRISCO   = CAT_CDRISCO;
          ECR_CDSUPER   = CAT_CDSUPER;
          ECR_DSSUPER   = $GetDescRisco(WCDProd:ECR_CDSUPER);
          ECR_ALIAS     = CAT_ALIAS;
          ECR_CONVERSAO = CAT_CONVERSAO;
          ECR_CAT10101  = 20000101;
          ECR_CAT10102  = 99991231;
          DOW (NOT *IN03) AND (NOT *IN12);
             IF (WIcDisplay);
                WNoFunction = 'RCATE00305'; // Listar Componentes
                *IN45 = *OFF;
             ELSE;
                WNoFunction = 'RCATE00321'; // Alterar Componentes
                *IN45 = *ON;
             ENDIF;
             DS#HEADER    = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER    = $getFooter(S#PGM:WNoFunction:*BLANKS);
             ECR_S#PGM    = S#PGM;
             WRITE     RCATE00300;
             WRITE     RCATE00399;
             EXFMT RCATE00305;
             IF ($ValidKey(Func#Key:KEY#F21)) AND (WIcDisplay);
                WIcDisplay = *OFF;
                $SetDBSession(WSessionID);
             ELSEIF ($ValidKey(Func#Key:KEY#F08)) AND (WIcDisplay);
                DspNotasDesc(WCdProd:WDsProd:ECR_CDRISCO);
                $SetDBSession(WSessionID);
             ELSEIF ($ValidKey(Func#Key:KEY#F22)) AND (WIcDisplay);
                ECR_OPTION = 'N';
                EXFMT RCATE00322;
                IF (Func#Key = KEY#Enter) AND (%TRIM(ECR_OPTION) = 'S');
                   DELETE (ECR_CDPROD:ECR_CDRISCO) RCATP101;
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
                   CHAIN (ECR_CDPROD:ECR_CDRISCO) RCATP101;
                   IF (%FOUND());
                      CAT_CDPROD    = WCdProd;
                      CAT_CDRISCO   = ECR_CDRISCO;
                      CAT_CDSUPER   = ECR_CDSUPER;
                      CAT_DSRISCO   = ECR_DSRISCO;
                      CAT_ICINDEX   = ECR_ICINDEX;
                      CAT_ICCAPTOT  = ECR_ICCAPTOT;
                      CAT_ICESCACT  = ECR_ICESCACT;
                      CAT_ALIAS     = ECR_ALIAS;
                      CAT_CONVERSAO = ECR_CONVERSAO;
                      CAT_CAT10101 = $IntToDate(20000101);
                      CAT_CAT10102 = $IntToDate(99991231);
                      UPDATE RCATP101 %FIELDS(CAT_DSRISCO:CAT_ICINDEX:
                                              CAT_ALIAS:CAT_CONVERSAO:
                                              CAT_ICESCACT:CAT_ICCAPTOT:
                                              CAT_CDSUPER:
                                              CAT_CAT10101:CAT_CAT10102);
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
          ECR_CDRISCO   = $GetNextKey('FCATP101':'CDRISCO':
                          'CDPROD = ''' + %CHAR(WCdProd) + '''');
          ECR_CDSUPER   = *ZEROS;
          ECR_DSSUPER   = *BLANKS;
          ECR_DSRISCO   = *BLANKS;
          ECR_CDPROD    = WCdProd;
          ECR_DSRISCO   = *BLANKS;
          ECR_ICINDEX   = 'S';
          CAT_ICCAPTOT  = 'S';
          CAT_ICESCACT  = 'S';
          ECR_ALIAS     = *BLANKS;
          ECR_CONVERSAO = *BLANKS;
          DOW (NOT *IN03) AND (NOT *IN12);
             WNoFunction = 'RCATE00306';
             WIcDisplay = *OFF;
             *IN45 = *ON;
             DS#HEADER    = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER    = $getFooter(S#PGM:WNoFunction:*BLANKS);
             ECR_S#PGM    = S#PGM;
             WRITE     RCATE00300;
             WRITE     RCATE00399;
             EXFMT RCATE00305;
             IF (Func#Key = KEY#F04) AND (NOT WIcDisplay);
                EXSR RunPopUp;
                $SetDBSession(WSessionID);
             ELSEIF (Func#Key = KEY#Enter);
                CAT_CDPROD    = ECR_CDPROD;
                CAT_CDRISCO   = ECR_CDRISCO;
                CAT_CDSUPER   = ECR_CDSUPER;
                CAT_DSRISCO   = ECR_DSRISCO;
                CAT_ICINDEX   = ECR_ICINDEX;
                CAT_ICCAPTOT  = ECR_ICCAPTOT;
                CAT_ICESCACT  = ECR_ICESCACT;
                CAT_ALIAS     = ECR_ALIAS;
                CAT_CONVERSAO = ECR_CONVERSAO;
                CAT_CAT10101 = $IntToDate(20000101);
                CAT_CAT10102 = $IntToDate(99991231);
                WRITE RCATP101;
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
|+---     IF (ECR_CM#NMFLD = 'CDSEGURA');
          // ECR_CDSEGURA = %DEC($runPopUp(33:%CHAR(ECR_CDSEGURA)):5:0);
|+---     ELSEIF (ECR_CM#NMFLD = 'CDSUPER');
             ECR_CDSUPER = $RunRiskPopUp(WCdProd:ECR_CDSUPER);
             ECR_DSSUPER = $GetDescRisco(WCDProd:ECR_CDSUPER);
          ENDIF;
|         ECR_CM#ROW = CM#CROW;
|         ECR_CM#COL = CM#CCOL;
+----  ENDSR;
      /END-FREE
