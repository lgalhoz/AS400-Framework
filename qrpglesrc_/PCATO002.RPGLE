     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PCATO001
     H* Descrição.: Manutenção de Produtos
     H*==================================================================
     FFCATP102  UF A E           K DISK    PREFIX(CAT_)
     FFCATP10202IF A E           K DISK    PREFIX(CAT_)
     F                                     RENAME(RCATP102:RCATP10202)
     FFCATP103  UF A E           K DISK    PREFIX(CAT_)
     FFCATP10302IF   E           K DISK    RENAME(RCATP103:RCATP10302)
     F                                     PREFIX(CAT_)
     FFCATE002  CF   E             WORKSTN SFILE(RCATE00202:REC#)
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

     D MntCoberturas   PR
     D   PCdProd                      5S 0 CONST
     D   PDsProd                     40A   CONST
     D   PCdSuper                     5S 0 CONST
     D   PDsSuper                    40A   CONST
     D
     D DspTextos       PR                  EXTPGM('PCATO008')
     D   WCdProd                      5S 0 CONST
     D   WDsProd                     40A   CONST
     D   WCdCobert                    5S 0 CONST
     D   WDsCobert                   40A   CONST
     D
     D REC#            S              4S 0
     D PARM_CdProd     S              5S 0
     D PARM_DsProd     S             40A
     C*========================================================
     C     *ENTRY        PLIST
     C                   PARM                    PARM_CdProd
     C                   PARM                    PARM_DsProd
      /FREE

       //------------------------------------
       // Se não estiver autorizado então sai
       //------------------------------------
       IF NOT $ValAccess(S#PGM:'':99999);
          *INLR = *ON;
          RETURN;
       ENDIF;

       MntCoberturas(PARM_CdProd:PARM_DsProd:*ZEROS:*BLANKS);

       *INLR = *ON;
       RETURN;
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
     D*==> Estrutura de dados do Registo
     D WIcErro         S               N   INZ(*OFF)
     D WIcOrigem       S              1A
     D
     D WIcDisplay      S               N   INZ(*ON)
     D WModo           S               N
     D WNoFunction     S             10A   INZ(*BLANKS)
     D WSelected       S               N
     D
     D WCdProd         S              5S 0
     D WDsprod         S             50A
     D
     D WCdNatura       S              1A
     D WSessionID      S              5S 0 INZ(*ZEROS)
     D*=============================================================
      /FREE
       WCdProd = PCdProd;
       WDsProd = PDsProd;
       WSessionID = $AddDBSession();
       $SETFILES('FCATP102');
       $SETFIELDS('CDPROD, CDCOBERT, NUORDEM, DSCOBERT, CDNATURA,  +
                   ICOBRIGA, ALIAS, CONVERSAO');
       IF (PCdSuper = *ZEROS);
          $SetCriteria('CDPROD = ''' + %CHAR(WCdProd) + ''' +
                        AND (CDCOBERT = CDSUPER)');
       ELSE;
          $SetCriteria('CDPROD = ''' + %CHAR(WCdProd) + ''' +
                        AND CDSUPER = ''' + %CHAR(PCdSuper) + ''' +
                        AND (CDSUPER <> CDCOBERT)');
       ENDIF;
       $SetPage(12);
       $SetSort('CDNATURA #DESC, NUORDEM');
       WIcDisplay =*ON;
       EXSR inzRecords;
       EXSR getRecords;

       ECR_DSPROD  = PDsProd;
       ECR_CDPROD  = PCdProd;
       // ECR_DSSUPER = PDsSuper;
       // ECR_CDSuper = PCdSuper;
       DOW (NOT *IN03 AND NOT *IN12);
          WNoFunction = 'RCATE00203'; // Listar Componentes
          DS#HEADER    = $getHeader(S#PGM:WNoFunction);
          ECR_DS#FNC   = $AlignText('('+%CHAR(ECR_CDPROD)+') '+ECR_DSPROD:
                         57:'C');
          DS#FOOTER    = $getFooter(S#PGM:WNoFunction:*BLANKS);
          DS#OPTIONS   = $getOption(S#PGM:WNoFunction:*BLANKS);
          ECR_S#PGM    = S#PGM;
          WRITE     RCATE00200;
          WRITE     RCATE00299;
          WRITE     RCATE00298;
          EXFMT RCATE00203;
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

       //========================================================
       // Subrotina.: InzRecTxt
       // Objectivo.: Inicializar a lista de registos
       //========================================================
       BEGSR inzRecords;
          REC#      = *ZEROS;   // Controlo de Registos da Lista
          *In53     = *On;      // Limpar Lista de Ecrã
          *In52     = *On;      // Mostrar a Lista de Ecrã
          WRITE RCATE00203;     // Limpar Lista
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
                ECR_OPTION   = *BLANKS;
                IF (WCdNatura<>%TRIM($GetField('CDNATURA')));
                   ECR_ICOBRIGA = *BLANKS;
                   *IN45 = *OFF;
                   WCdNatura = %TRIM($GetField('CDNATURA'));
                   ECR_DSCOBERT = $GetDescricao(57:WCdNatura);
                   REC# += 1;
                   WRITE RCATE00202;
                   *IN45 = *ON;
                ENDIF;

                //------------------------------------------------
                // Escreve os Registo da Listagem
                //------------------------------------------------
                ECR_CDPROD   = WCdProd;
                MONITOR;
                   ECR_CDCOBERT = %DEC($GetField('CDCOBERT'):5:0);
                ON-ERROR;
                ENDMON;

                ECR_DSCOBERT = '('+%TRIM($GetField('ALIAS')) + ') ' +
                               $GetField('DSCOBERT');
                ECR_ICMORE   = 'N';
                SETLL (PCdProd:ECR_CDCOBERT) RCATP10202;
                READE (PCdProd:ECR_CDCOBERT) RCATP10202;
                DOW (NOT %EOF());
                   IF (CAT_CDSUPER <> CAT_CDCOBERT);
                      ECR_DSCOBERT = '...'+ ECR_DSCOBERT;
                      ECR_ICMORE   = 'S';
                      LEAVE;
                   ENDIF;
                   READE (PCdProd:ECR_CDCOBERT) RCATP10202;
                ENDDO;
                ECR_CDNATURA = $GetField('CDNATURA');
                ECR_ICOBRIGA = $GetField('ICOBRIGA');
                REC# += 1;
                WRITE RCATE00202;
                IF (REC# >= 11);
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
          READC RCATE00202;
          DOW (NOT %EOF());
             IF ($ValidOption(ECR_OPTION:'5'));
                WSelected = *ON;
                EXSR DspCoberta;
             ELSEIF ($ValidOption(ECR_OPTION:'7'));
                MONITOR;
                   DspTextos(ECR_CDPROD:ECR_DSPROD:ECR_CDCOBERT:
                                      ECR_DSCOBERT);
                ON-ERROR;
                ENDMON;
                $SetDBSession(WSessionID);
             ELSEIF ($ValidOption(ECR_OPTION:'8'));
             ELSEIF ($ValidOption(ECR_OPTION:'9'));
                MONITOR;
                   MntCoberturas(ECR_CDPROD:ECR_DSPROD:ECR_CDCOBERT:
                                 ECR_DSCOBERT);
                ON-ERROR;
                ENDMON;
                $SetDBSession(WSessionID);
             ELSE;
               // Mensagem de Opção inválida
             ENDIF;
             READC RCATE00202;
          ENDDO;
       ENDSR;

       //========================================================
       // Subrotina.: DspCoberta
       // Objectivo.: Apresentar os dados da Cobertura
       //========================================================
       BEGSR DspCoberta;
          DOW (NOT *IN03) AND (NOT *IN12);
             CHAIN(N) (WCDProd:ECR_CDCOBERT) RCATP102;
             ECR_CDPROD    = WCdProd;
             ECR_DSPROD    = WDsProd;
             ECR_DSCOBERT1 = %SUBST(CAT_DSCOBERT:1:50);
             ECR_DSCOBERT2 = %SUBST(CAT_DSCOBERT:51:50);
             ECR_NUORDEM   = CAT_NUORDEM;
             ECR_CDNATURA  = CAT_CDNATURA;
             ECR_DSNATURA  = $GetDescricao(57:CAT_CDNATURA);
             ECR_ICOBRIGA  = CAT_ICOBRIGA;
             ECR_ALIAS     = CAT_ALIAS;
             ECR_CONVERSAO = CAT_CONVERSAO;
             ECR_CAT10201  = $DateToInt(CAT_CAT10201);
             ECR_CAT10202  = $DateToInt(CAT_CAT10202);
             IF (WIcDisplay);
                WNoFunction = 'RCATE00205'; // Listar Componentes
                *IN45 = *OFF;
             ELSE;
                WNoFunction = 'RCATE00221'; // Alterar Componentes
                *IN45 = *ON;
             ENDIF;
             DS#HEADER    = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER    = $getFooter(S#PGM:WNoFunction:*BLANKS);
             ECR_S#PGM    = S#PGM;
             WRITE     RCATE00200;
             WRITE     RCATE00299;
             EXFMT RCATE00205;
             IF ($ValidKey(Func#Key:KEY#F21)) AND (WIcDisplay);
                WIcDisplay = *OFF;
                $SetDBSession(WSessionID);
             ELSEIF ($ValidKey(Func#Key:KEY#F22)) AND (WIcDisplay);
                ECR_OPTION = 'N';
                EXFMT RCATE00222;
                IF (Func#Key = KEY#Enter) AND (%TRIM(ECR_OPTION) = 'S');
                   //==> Eliminar o Registo da Cobertura
                   DELETE (ECR_CDPROD:ECR_CDCOBERT) RCATP102;
                   //==> Eliminar o Registo de Limite e Franquia
                   SETLL (ECR_CDPROD:ECR_CDCOBERT) RCATP10302;
                   READE (ECR_CDPROD:ECR_CDCOBERT) RCATP10302;
                   DOW (NOT %EOF());
                      DELETE (ECR_CDPROD:CAT_CDLIMITE) RCATP103;
                      READE (ECR_CDPROD:ECR_CDCOBERT) RCATP10302;
                   ENDDO;
                   LEAVESR;
                ENDIF;
                *IN12 = *OFF;
             ELSEIF ($ValidKey(Func#Key:KEY#F07)) AND (WIcDisplay);
                MONITOR;
                   MntCoberturas(ECR_CDPROD:ECR_DSPROD:ECR_CDCOBERT:
                                 ECR_DSCOBERT1);
                ON-ERROR;
                ENDMON;
                $SetDBSession(WSessionID);
                ITER;
             ELSEIF ($ValidKey(Func#Key:KEY#F08));
                MONITOR;
                   DspTextos(ECR_CDPROD:ECR_DSPROD:ECR_CDCOBERT:
                                      ECR_DSCOBERT);
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
                   CHAIN (ECR_CDPROD:ECR_CDCOBERT) RCATP102;
                   IF (%FOUND());
                      CAT_CDPROD    = WCdProd;
                      CAT_CDCOBERT  = ECR_CDCOBERT;
                      CAT_NUORDEM   = ECR_NUORDEM;
                      CAT_DSCOBERT  = ECR_DSCOBERT1 + ECR_DSCOBERT2;
                      CAT_CDNATURA  = ECR_CDNATURA;
                      CAT_ICOBRIGA  = ECR_ICOBRIGA;
                      CAT_ALIAS     = ECR_ALIAS;
                      CAT_CONVERSAO = ECR_CONVERSAO;
                      CAT_CAT10201 = $IntToDate(ECR_CAT10201);
                      CAT_CAT10202 = $IntToDate(ECR_CAT10202);
                      UPDATE RCATP102 %FIELDS(CAT_NUORDEM:CAT_DSCOBERT:
                                              CAT_CDNATURA:CAT_ICOBRIGA:
                                              CAT_ALIAS:CAT_CONVERSAO:
                                              CAT_CAT10201:CAT_CAT10202);
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
          ECR_CDCOBERT = $GetNextKey('FCATP102':'CDCOBERT':
                       'CDPROD = ''' + %CHAR(WCdProd) + '''');
          ECR_CDPROD    = WCdProd;
          ECR_DSPROD    = WDsProd;
          ECR_DSCOBERT1 = *BLANKS;
          ECR_DSCOBERT2 = *BLANKS;
          ECR_NUORDEM   = *ZEROS;
          ECR_CDNATURA  = 'F';
          ECR_ICOBRIGA  = 'N';
          ECR_ALIAS     = *BLANKS;
          ECR_CONVERSAO = *BLANKS;
          ECR_CONVERSAO=*BLANKS;
          DOW (NOT *IN03) AND (NOT *IN12);
             WNoFunction = 'RCATE00206';
             WIcDisplay = *OFF;
             *IN45 = *ON;
             DS#HEADER    = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER    = $getFooter(S#PGM:WNoFunction:*BLANKS);
             ECR_S#PGM    = S#PGM;
             WRITE     RCATE00200;
             WRITE     RCATE00299;
             EXFMT RCATE00205;
             IF (Func#Key = KEY#F04) AND (NOT WIcDisplay);
                EXSR RunPopUp;
                $SetDBSession(WSessionID);
             ELSEIF (Func#Key = KEY#Enter);
                //==> Criar o registo da Cobertura
                CAT_CDPROD    = ECR_CDPROD;
                CAT_CDCOBERT  = ECR_CDCOBERT;
                IF (PCdSuper = *ZEROS);
                   CAT_CDSUPER = CAT_CDCOBERT;
                ELSE;
                   CAT_CDSUPER = PCdSUper;
                ENDIF;
                CAT_NUORDEM   = ECR_NUORDEM;
                CAT_DSCOBERT  = ECR_DSCOBERT1 + ECR_DSCOBERT2;
                CAT_CDNATURA  = ECR_CDNATURA;
                CAT_ICOBRIGA  = ECR_ICOBRIGA;
                CAT_ALIAS     = ECR_ALIAS;
                CAT_CONVERSAO = ECR_CONVERSAO;
                WRITE RCATP102;

                //==> Criar o registo do Limite e Franquia
                IF (CAT_CDSUPER <> CAT_CDCOBERT);
                   SETLL (ECR_CDPROD:ECR_CDCOBERT) RCATP10302;
                   READE (ECR_CDPROD:ECR_CDCOBERT) RCATP10302;
                   DOW (NOT %EOF());
                      IF (CAT_CDSUPER = CAT_CDLIMITE);
                         LEAVE;
                      ENDIF;
                   ENDDO;
                ELSE;
                   CAT_CDSUPER = *ZEROS;
                ENDIF;
                CAT_CDLIMITE = $GetNextKey('FCATP103':'CDLIMITE':
                                 'CDPROD = ''' + %CHAR(WCdProd) + '''');
                IF (CAT_CDSUPER = *ZEROS);
                   CAT_CDSUPER  = CAT_CDLIMITE;
                ENDIF;
                CAT_CDPROD   = ECR_CDPROD;
                CAT_CDRISCO  = *ZEROS;
                CAT_DSLIMITE = *BLANKS;
                CAT_CDCOBERT = ECR_CDCOBERT;
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
|+---     IF (ECR_CM#NMFLD = 'CDNATURA');
             ECR_CDNATURA = $runPopUp(57:ECR_CDNATURA);
          ENDIF;
|         ECR_CM#ROW = CM#CROW;
|         ECR_CM#COL = CM#CCOL;
+----  ENDSR;
      /END-FREE
     PMntCoberturas    E
