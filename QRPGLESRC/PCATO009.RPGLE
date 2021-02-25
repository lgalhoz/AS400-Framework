     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PCATO001
     H* Descrição.: Manutenção de Produtos
     H*==================================================================
     FFCATP109  UF A E           K DISK    PREFIX(CAT_)
     FFCATE009  CF   E             WORKSTN SFILE(RCATE00902:REC#)
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
     D WNuSequ         S              5S 0
     D WDSCLAUS        S             50A
     D PCdProd         S              5S 0
     D PDsprod         S             50A
     D PNuSequ         S              5S 0
     D PDSCLAUS        S             50A
     D
     D WSessionID      S              5S 0
     C*========================================================
     C     *ENTRY        PLIST
     C                   PARM                    PCDPROD
     C                   PARM                    PDSPROD
     C                   PARM                    PNuSequ
     C                   PARM                    PDSCLAUS
      /FREE

       MONITOR;
          WCdProd   = PCdProd;
          WDsProd   = PDsProd;
          WNuSequ   = PNuSequ  ;
          WDSCLAUS  = PDSCLAUS ;
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

       WSessionID = $SETFILES('FCATP109');
       $SetPage(12);
       $SetCriteria('CDPROD = ''' + %CHAR(WCdProd) + ''' +
                     AND NUSEQU = ''' + %CHAR(WNuSequ  ) + '''');
       $SetSort('NUSEQU');
       WIcDisplay =*ON;
       EXSR inzRecords;
       EXSR getRecords;

       ECR_DSPROD   = WDsProd;
       ECR_CDPROD   = WCdProd;
       ECR_DSCLAUS  = WDsClaus;
       ECR_NUSEQU   = WNuSequ;
       DOW (NOT *IN03 AND NOT *IN12);
          WNoFunction = 'RCATE00903'; // Listar Componentes
          DS#HEADER    = $getHeader(S#PGM:WNoFunction);
          DS#FOOTER    = $getFooter(S#PGM:WNoFunction:*BLANKS);
          ECR_S#PGM    = S#PGM;
          WRITE     RCATE00900;
          WRITE     RCATE00999;
          WRITE     RCATE00998;
          EXFMT RCATE00903;
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
          WRITE RCATE00903;     // Limpar Lista
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
                MONITOR;
                   ECR_NUSEQU  = %DEC($GetField('NUSEQU'):5:0);
                   ECR_NULINE  = %DEC($GetField('NULINE'):5:0);
                ON-ERROR;
                ENDMON;
                ECR_NULINE    = %DEC($GetField('NULINE'):5:0);
                ECR_TEXTABRV   = $GetField('TEXTO');
                WRITE RCATE00902;
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
          READC RCATE00902;
          DOW (NOT %EOF());
             IF (ECR_OPTION <> *BLANKS);
                WSelected = *ON;
                EXSR DspCoberta;
             ENDIF;
             READC RCATE00902;
          ENDDO;
       ENDSR;

       //========================================================
       // Subrotina.: DspCoberta
       // Objectivo.: Apresentar os dados da Cobertura
       //========================================================
       BEGSR DspCoberta;
          CHAIN(N) (WCDProd:WNuSequ:ECR_NuLine) RCATP109;
          ECR_CDPROD    = WCdProd;
          ECR_DSPROD    = WDsProd;
          ECR_NUSEQU    = WNuSequ  ;
          ECR_DSCLAUS   = WDSCLAUS ;
          ECR_NUSEQU    = CAT_NUSEQU;
          ECR_NULINE    = CAT_NULINE ;
          ECR_TEXTO     = CAT_TEXTO;
          DOW (NOT *IN03) AND (NOT *IN12);
             IF (WIcDisplay);
                WNoFunction = 'RCATE00905'; // Listar Componentes
                *IN45 = *OFF;
             ELSE;
                WNoFunction = 'RCATE00921'; // Alterar Componentes
                *IN45 = *ON;
             ENDIF;
             DS#HEADER    = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER    = $getFooter(S#PGM:WNoFunction:*BLANKS);
             ECR_S#PGM    = S#PGM;
             WRITE     RCATE00900;
             WRITE     RCATE00999;
             EXFMT RCATE00905;
             IF ($ValidKey(Func#Key:KEY#F21)) AND (WIcDisplay);
                WIcDisplay = *OFF;
                $SetDBSession(WSessionID);
             ELSEIF ($ValidKey(Func#Key:KEY#F22)) AND (WIcDisplay);
                ECR_OPTION = 'N';
                EXFMT RCATE00922;
                IF (Func#Key = KEY#Enter) AND (ECR_OPTION = 'S');
                   DELETE (WCdProd:WNuSequ :ECR_NULINE) RCATP109;
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
                   CHAIN (WCdProd:WNuSequ :ECR_NULINE) RCATP109;
                   IF (%FOUND());
                      CAT_CDPROD    = WCdProd;
                      CAT_NUSEQU    = WNuSequ;
                      CAT_NULINE    = ECR_NULINE ;
                      CAT_TEXTO     = ECR_TEXTO;
                      UPDATE RCATP109 %FIELDS(CAT_NULINE :CAT_TEXTO);
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
          ECR_NULINE    = $getNextKey('FCATP109':'NULINE':
                          'CDPROD = ''' + %CHAR(WCdProd) + ''' +
                           AND NUSEQU = ''' + %CHAR(WNuSequ  ) + '''');
          ECR_NUSEQU    =WNuSequ;
          ECR_TEXTO     = *BLANKS;
          DOW (NOT *IN03) AND (NOT *IN12);
             WNoFunction = 'RCATE00906';
             WIcDisplay = *OFF;
             *IN45 = *ON;
             DS#HEADER    = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER    = $getFooter(S#PGM:WNoFunction:*BLANKS);
             ECR_S#PGM    = S#PGM;
             WRITE     RCATE00900;
             WRITE     RCATE00999;
             EXFMT RCATE00905;
             IF (Func#Key = KEY#F04) AND (NOT WIcDisplay);
                EXSR RunPopUp;
                $SetDBSession(WSessionID);
             ELSEIF (Func#Key = KEY#Enter);
                CAT_CDPROD    = WCdProd;
                CAT_NUSEQU    = WNuSequ;
                CAT_NULINE    = ECR_NULINE ;
                CAT_TEXTO     = ECR_TEXTO;
                CAT_CAT10901 = $IntTodate(20000101);
                CAT_CAT10902 = $IntTodate(99991231);
                WRITE RCATP109;
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
|+---     IF (ECR_CM#NMFLD = 'CDIDENT');
             ECR_NULINE   = %DEC($runPopUp(33:%CHAR(ECR_NULINE)):5:0);
          ENDIF;
|         ECR_CM#ROW = CM#CROW;
|         ECR_CM#COL = CM#CCOL;
+----  ENDSR;
      /END-FREE
