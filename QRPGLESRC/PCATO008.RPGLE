     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PCATO001
     H* Descrição.: Manutenção de Produtos
     H*==================================================================
     FFCATP107  UF A E           K DISK    PREFIX(CAT_)
     FFCATP117  UF A E           K DISK    PREFIX(CAT_)
     FFCATE008  CF   E             WORKSTN SFILE(RCATE00802:REC#)
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

     D VerTexto        PR                  EXTPGM('RCATO009')
     D   WCodPagina                   5S 0 CONST
     D   WTipItem                     2A   CONST
     D   WCodProd                     5S 0 CONST
     D   WCodItem1                    9S 0 CONST
     D   WCodItem2                    9S 0 CONST
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
     D WCodPagina      S              5S 0
     D WCdProd         S              5S 0
     D WDsprod         S             50A
     D WCdCobert       S              5S 0
     D WDsCobert       S             50A
     D PCdProd         S              5S 0
     D PDsprod         S             50A
     D PCdCobert       S              5S 0
     D PDsCobert       S             50A
     D
     D WSesPagina      S              5S 0
     D WSesTexto       S              5S 0
     C*========================================================
     C     *ENTRY        PLIST
     C                   PARM                    PCDPROD
     C                   PARM                    PDSPROD
     C                   PARM                    PCDCOBERT
     C                   PARM                    PDSCOBERT
      /FREE

       MONITOR;
          WCdProd   = PCdProd;
          WDsProd   = PDsProd;
          WCdCobert = PCdCobert;
          WDsCobert = PDsCobert;
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

       //----------------------------------------------
       // Definições dos Documentos
       //----------------------------------------------
       WSesPagina = $SETFILES('FCATP117');
       $SetPage(12);
       $SetCriteria('CDPROD = ''' + %CHAR(WCdProd) + ''' +
                     AND CDCOBERT = ''' + %CHAR(WCdCobert) + '''');
       $SetLstFld('CDDOCUM':'Docum':5:'D');
       $SetLstFld('TPDOCUM':'Tipo.':5:'C');
       $SetLstFld('DSDOCUM':'Descrição':63:'E');
       $SetSort('CDDOCUM');

       //----------------------------------------------
       // Definições do Texto
       //----------------------------------------------
       WSesTexto = $SETFILES('FCATP107');
       $SetPage(12);
       $SetCriteria('CDPROD = ''' + %CHAR(WCdProd) + ''' +
                     AND CDCOBERT = ''' + %CHAR(WCdCobert) + '''');
       $SetLstFld('NUORDEM':'Ordem':5:'D');
       $SetLstFld('CDIDENT':'Ident.':5:'C');
       $SetLstFld('TEXTO':'Texto':63:'E');
       $SetSort('NUORDEM');

       $SetDBSession(WSesPagina);

       WIcDisplay =*ON;
       EXSR inzRecords;
       EXSR getRecords;


       ECR_DSPROD   = WDsProd;
       ECR_CDPROD   = WCdProd;
       ECR_DSCOBERT = WDsCobert;
       ECR_CDCOBERT = WCdCobert;
       DOW (NOT *IN03 AND NOT *IN12);
          IF ($GetDBSession = WSesPagina);
             WNoFunction = 'RCATE00803'; // Listar Páginas
          ELSE;
             WNoFunction = 'RCATE00804'; // Listar Parágrafos
          ENDIF;
          DS#HEADER    = $getHeader(S#PGM:WNoFunction);
          ECR_DS#FNC   = $AlignText('('+%CHAR(WCdProd)+') '+ECR_DSPROD:
                         57:'C');
          DS#FOOTER    = $getFooter(S#PGM:WNoFunction:*BLANKS);
          DS#OPTIONS   = $getOption(S#PGM:WNoFunction:*BLANKS);
          ECR_S#PGM    = S#PGM;
          WRITE     RCATE00800;
          WRITE     RCATE00899;
          WRITE     RCATE00898;
          EXFMT RCATE00803;
          IF (Func#Key = KEY#F05);
             $ChgCriteria();
             // ECR_DSCRITER = $getCritText();
             EXSR inzRecords;
             EXSR getRecords;
          ELSEIF ($ValidKey(Func#Key:KEY#F06));
             IF ($GetDbSession = WSesTexto);
                EXSR addTexto;
                $SetDBSession(WSesTexto);
             ELSE;
                EXSR addPagina;
                $SetDBSession(WSesPagina);
             ENDIF;
             EXSR inzRecords;
             EXSR getRecords;
          ELSEIF ($ValidKey(Func#Key:KEY#F12));
             IF ($GetDBSession = WSesTexto);
                $SetDBSession(WSesPagina);
                *IN12 = *OFF;
                EXSR inzRecords;
                EXSR getRecords;
             ENDIF;
          ELSEIF ($ValidKey(Func#Key:KEY#F14));
             VerTexto(WCodPagina:'CB':WCdProd:WCdCobert:ECR_CDDOCUM);
             $SetDBSession(WSesTexto);
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

       $RmvDBSession(WSesPagina);
       $RmvDBSession(WSesTexto);
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
          WRITE RCATE00803;     // Limpar Lista
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
                ECR_OPTION   = *BLANKS;
                ECR_LSTHDR = $GetLstHdr();
                ECR_LSTDET = $GetLstLine();

                MONITOR;
                   ECR_CDDOCUM = %DEC($GetField('CDDOCUM'):5:0);
                   IF ($GetDBSession() = WSesPagina);
                      ECR_CDPAGINA= %DEC($GetField('CDPAGINA'):5:0);
                      ECR_NUORDEM = *ZEROS;
                      ECR_NUSEQU  = *ZEROS;
                   ELSE;
                      ECR_NUSEQU  = %DEC($GetField('NUSEQU'):5:0);
                   // ECR_NUORDEM = %DEC($GetField('NUORDEM'):5:0);
                   // ECR_CDPAGINA = *ZEROS;
                   ENDIF;
                ON-ERROR;
                ENDMON;

                REC# += 1;
                WRITE RCATE00802;
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
          READC RCATE00802;
          DOW (NOT %EOF());
             IF ($ValidOption(ECR_OPTION:'5'));
                WSelected = *ON;
                IF ($GetDBSession=WSesTexto);
                   EXSR DspTexto;
                ELSE;
                   WCodPagina = ECR_CDPAGINA;
                   EXSR DspPagina;
                ENDIF;
             ELSEIF ($ValidOption(ECR_OPTION:'7'));
                $SetDBSession(WSesTexto);
                $SetCriteria('CDPROD = ''' + %CHAR(WCdProd) + ''' +
                     AND CDCOBERT = ''' + %CHAR(WCdCobert) + '''+
                     AND CDDOCUM = ''' + %CHAR(ECR_CDDOCUM) + '''');
                WCodPagina = ECR_CDPAGINA;
                WSelected = *ON;
             ENDIF;
             READC RCATE00802;
          ENDDO;
       ENDSR;

       //========================================================
       // Subrotina.: DspTexto
       // Objectivo.: Apresentar o Texto do Documento da Cobertura
       //========================================================
       BEGSR DspTexto;
          CHAIN(N) (WCDProd:WCdCobert:ECR_CDDOCUM:ECR_NuSequ) RCATP107;
          ECR_CDPROD    = WCdProd;
          ECR_DSPROD    = WDsProd;
          ECR_CDCOBERT  = WCdCobert;
          ECR_DSCOBERT  = WDsCobert;
          ECR_NUSEQU    = CAT_NUSEQU;
          ECR_NUORDEM   = CAT_NUORDEM;
          ECR_CDIDENT   = CAT_CDIDENT;
          ECR_DSIDENT   = $getStyleDesc(WCdProd:ECR_CDIDENT);
          ECR_TEXTO     = CAT_TEXTO;
          DOW (NOT *IN03) AND (NOT *IN12);
             IF (WIcDisplay);
                WNoFunction = 'RCATE00805'; // Listar Componentes
                *IN45 = *OFF;
             ELSE;
                WNoFunction = 'RCATE00821'; // Alterar Componentes
                *IN45 = *ON;
             ENDIF;
             DS#HEADER    = $getHeader(S#PGM:WNoFunction);
             ECR_DS#FNC   = $AlignText('('+%CHAR(WCdProd)+') '+ECR_DSPROD:
                              57:'C');
             DS#FOOTER    = $getFooter(S#PGM:WNoFunction:*BLANKS);
             ECR_S#PGM    = S#PGM;
             WRITE     RCATE00800;
             WRITE     RCATE00899;
             EXFMT RCATE00805;
             IF ($ValidKey(Func#Key:KEY#F21)) AND (WIcDisplay);
                WIcDisplay = *OFF;
                $SetDBSession(WSesTexto);
             ELSEIF ($ValidKey(Func#Key:KEY#F22)) AND (WIcDisplay);
                ECR_OPTION = 'N';
                EXFMT RCATE00822;
                IF (Func#Key = KEY#Enter) AND (%TRIM(ECR_OPTION) = 'S');
                   DELETE (WCdProd:WCdCobert:ECR_CDDOCUM:
                             ECR_NUSEQU) RCATP107;
                   LEAVESR;
                ENDIF;
                *IN12 = *OFF;
             ELSEIF (Func#Key = KEY#F12) AND (NOT WIcDisplay);
                WIcDisplay = *ON;
                *IN12 = *OFF;
             ELSEIF (Func#Key = KEY#F04) AND (NOT WIcDisplay);
                EXSR RunPopUp;
                $SetDBSession(WSesTexto);
             ELSEIF (Func#Key = KEY#Enter);
                IF (NOT WIcDisplay);
                   CHAIN (WCdProd:WCdCobert:ECR_CDDOCUM:
                                 ECR_NUSEQU) RCATP107;
                   IF (%FOUND());
                      CAT_CDPROD    = WCdProd;
                      CAT_CDCOBERT  = WCdCobert;
                      CAT_CDDOCUM   = ECR_CDDOCUM;
                      CAT_NUSEQU    = ECR_NUSEQU;
                      CAT_NUORDEM   = ECR_NUORDEM;
                      CAT_CDIDENT   = ECR_CDIDENT;
                      CAT_TEXTO     = ECR_TEXTO;
                      UPDATE RCATP107 %FIELDS(CAT_CDIDENT:CAT_TEXTO:
                                              CAT_NUORDEM);
                   ENDIF;
                   $SetDBSession(WSesTexto);
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
       // Subrotina.: AddTexto
       // Objectivo.: Adicionar um novo prágrafo
       //========================================================
       BEGSR AddTexto;
          ECR_NUSEQU    = $getNextKey('FCATP107':'NUSEQU':
                          'CDPROD = ''' + %CHAR(WCdProd) + ''' +
                           AND CDCOBERT = ''' + %CHAR(WCdCobert) + ''' +
                           AND CDDOCUM = ''' + %CHAR(ECR_CDDOCUM) + '''');
          ECR_CDIDENT   = *BLANKS;
          ECR_NUORDEM   = *ZEROS;
          ECR_TEXTO     = *BLANKS;
          DOW (NOT *IN03) AND (NOT *IN12);
             WNoFunction = 'RCATE00806';
             WIcDisplay = *OFF;
             *IN45 = *ON;
             DS#HEADER    = $getHeader(S#PGM:WNoFunction);
             ECR_DS#FNC   = $AlignText('('+%CHAR(WCdProd)+') '+ECR_DSPROD:
                            57:'C');
             DS#FOOTER    = $getFooter(S#PGM:WNoFunction:*BLANKS);
             ECR_S#PGM    = S#PGM;
             WRITE     RCATE00800;
             WRITE     RCATE00899;
             EXFMT RCATE00805;
             IF (Func#Key = KEY#F04) AND (NOT WIcDisplay);
                EXSR RunPopUp;
                $SetDBSession(WSesTexto);
             ELSEIF (Func#Key = KEY#Enter);
                CAT_CDPROD    = WCdProd;
                CAT_CDCOBERT  = WCdCobert;
                CAT_CDDOCUM   = ECR_CDDOCUM;
                CAT_NUSEQU    = ECR_NUSEQU;
                CAT_NUORDEM   = ECR_NUORDEM;
                CAT_CDIDENT   = ECR_CDIDENT;
                CAT_TEXTO     = ECR_TEXTO;
                CAT_CAT10701 = $IntTodate(20000101);
                CAT_CAT10702 = $IntTodate(99991231);
                WRITE RCATP107;
                WIcDisplay = *ON;
                LEAVESR;
             ENDIF;
          ENDDO;
          *IN12 = *OFF;
          WIcDisplay = *ON;
       ENDSR;

       //========================================================
       // Subrotina.: DspPagina
       // Objectivo.: Apresentar a Página do Documento da Cobertura
       //========================================================
       BEGSR DspPagina;
          CHAIN(N) (WCDProd:WCdCobert:ECR_CDDOCUM) RCATP117;
          ECR_CDPROD    = WCdProd;
          ECR_DSPROD    = WDsProd;
          ECR_CDCOBERT  = WCdCobert;
          ECR_DSCOBERT  = WDsCobert;
          ECR_CDDOCUM   = CAT_CDDOCUM;
          ECR_DSDOCUM   = CAT_DSDOCUM;
          ECR_ICESCREVE = CAT_ICESCREVE;
          ECR_TPDOCUM   = CAT_TPDOCUM;
          ECR_CDPAGINA  = CAT_CDPAGINA;
          DOW (NOT *IN03) AND (NOT *IN12);
             IF (WIcDisplay);
                WNoFunction = 'RCATE00807'; // Listar Componentes
                *IN45 = *OFF;
             ELSE;
                WNoFunction = 'RCATE00823'; // Alterar Componentes
                *IN45 = *ON;
             ENDIF;
             DS#HEADER    = $getHeader(S#PGM:WNoFunction);
             ECR_DS#FNC   = $AlignText('('+%CHAR(WCdProd)+') '+ECR_DSPROD:
                            57:'C');
             DS#FOOTER    = $getFooter(S#PGM:WNoFunction:*BLANKS);
             ECR_S#PGM    = S#PGM;
             WRITE     RCATE00800;
             WRITE     RCATE00899;
             EXFMT RCATE00807;
             IF ($ValidKey(Func#Key:KEY#F21)) AND (WIcDisplay);
                WIcDisplay = *OFF;
                $SetDBSession(WSesPagina);
             ELSEIF ($ValidKey(Func#Key:KEY#F22)) AND (WIcDisplay);
                ECR_OPTION = 'N';
                EXFMT RCATE00822;
                IF (Func#Key = KEY#Enter) AND (%TRIM(ECR_OPTION) = 'S');
                   DELETE (WCdProd:WCdCobert:ECR_CDDOCUM) RCATP117;
                   LEAVESR;
                ENDIF;
                *IN12 = *OFF;
             ELSEIF (Func#Key = KEY#F12) AND (NOT WIcDisplay);
                WIcDisplay = *ON;
                *IN12 = *OFF;
             ELSEIF (Func#Key = KEY#F07) AND (WIcDisplay);
                VerTexto(WCodPagina:'CB':WCdProd:WCdCobert:ECR_CDDOCUM);
                $SetDBSession(WSesPagina);
                EXSR inzRecords;
                EXSR getRecords;
             ELSEIF (Func#Key = KEY#F04) AND (NOT WIcDisplay);
                EXSR RunPopUp;
                $SetDBSession(WSesPagina);
             ELSEIF (Func#Key = KEY#Enter);
                IF (NOT WIcDisplay);
                   CHAIN (WCDProd:WCdCobert:ECR_CDDOCUM) RCATP117;
                   IF (%FOUND());
                      CAT_CDPROD   = WCdProd;
                      CAT_CDCOBERT = WCdCobert;
                      CAT_CDDOCUM  = ECR_CDDOCUM;
                      CAT_NUSEQU   = ECR_NUSEQU;
                      CAT_NUORDEM  = ECR_NUORDEM;
                      CAT_DSDOCUM  = ECR_DSDOCUM;
                      CAT_TPDOCUM  = ECR_TPDOCUM;
                      CAT_CDPAGINA = ECR_CDPAGINA;
                      CAT_ICESCREVE= ECR_ICESCREVE;
                      UPDATE RCATP117 %FIELDS(CAT_TPDOCUM:CAT_DSDOCUM:
                                              CAT_CDPAGINA:CAT_ICESCREVE);
                   ENDIF;
                   $SetDBSession(WSesPagina);
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
       // Subrotina.: AddPagina
       // Objectivo.: Adicionar uma nova Página
       //========================================================
       BEGSR AddPagina;
          ECR_CDDOCUM   = $getNextKey('FCATP117':'CDDOCUM':
                          'CDPROD = ''' + %CHAR(WCdProd) + ''' +
                           AND CDCOBERT = ''' + %CHAR(WCdCobert) + '''');
          ECR_TPDOCUM   = *BLANKS;
          ECR_DSDOCUM   = *BLANKS;
          ECR_ICESCREVE = 'N';
          ECR_CDPAGINA  = *ZEROS;
          ECR_DSTEMPLATE= *BLANKS;
          DOW (NOT *IN03) AND (NOT *IN12);
             WNoFunction = 'RCATE00808';
             WIcDisplay = *OFF;
             *IN45 = *ON;
             DS#HEADER    = $getHeader(S#PGM:WNoFunction);
             ECR_DS#FNC   = $AlignText('('+%CHAR(WCdProd)+') '+ECR_DSPROD:
                            57:'C');
             DS#FOOTER    = $getFooter(S#PGM:WNoFunction:*BLANKS);
             ECR_S#PGM    = S#PGM;
             WRITE     RCATE00800;
             WRITE     RCATE00899;
             EXFMT RCATE00807;
             IF (Func#Key = KEY#F04) AND (NOT WIcDisplay);
                EXSR RunPopUp;
                $SetDBSession(WSesTexto);
             ELSEIF (Func#Key = KEY#Enter);
                CAT_CDPROD    = WCdProd;
                CAT_CDCOBERT  = WCdCobert;
                CAT_CDDOCUM   = ECR_CDDOCUM;
                CAT_TPDOCUM   = ECR_TPDOCUM;
                CAT_ICESCREVE = ECR_ICESCREVE;
                CAT_CDPAGINA  = ECR_CDPAGINA;
                CAT_DSDOCUM   = ECR_DSDOCUM;
                CAT_CAT11701 = $IntTodate(20000101);
                CAT_CAT11702 = $IntTodate(99991231);
                WRITE RCATP117;
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
             ECR_CDIDENT  = $RunStylPopUp(WCodPagina:ECR_CDIDENT);
          ENDIF;
|         ECR_CM#ROW = CM#CROW;
|         ECR_CM#COL = CM#CCOL;
+----  ENDSR;
      /END-FREE
