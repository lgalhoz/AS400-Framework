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

     D*==> Gerir coberturas
     D MntCoberturas   PR
     D   PCdProd                      5S 0 CONST
     D   PDsProd                     40A   CONST
     D   PCdSuper                     5S 0 CONST
     D   PDsSuper                    40A   CONST
     D
     D*==> Carregar lista de Trabalho com Coberturas
     D FillCoberturas  PR
     D   PCdProd                      5S 0 CONST
     D   PCdSuper                     5S 0 CONST
     D
     D LST#Cobert      DS                  QUALIFIED DIM(200)
     D   CdCobert                     5S 0 INZ(*ZEROS)
     D   CdSuper                      5S 0 INZ(*ZEROS)
     D   DsCobert                    50A   INZ(*BLANKS)
     D   Codigo                      10A   INZ(*BLANKS)
     D   CdNatura                     1A   INZ(*BLANKS)
     D   Header                       1A   INZ('N')
     D   TpHeader                     1A   INZ('N')
     D   IsEmpty                       N   INZ(*ON)
     D WIdxCobert      S              3S 0 INZ(*ZEROS)
     D
     D*==> Apresentar os textos pata as Coberturas
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
     D*=============================================================
      /FREE
       WCdProd = PCdProd;
       WDsProd = PDsProd;
       FillCoberturas(PCdProd:*ZEROS);
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
             EXSR inzRecords;
             EXSR getRecords;
       // ELSEIF ((Func#Key = KEY#PgDown) AND (NOT $IsLastPage())) OR
       //    ((Func#Key = KEY#PgUp) AND ($GetPageNbr() > 1));
       //    EXSR inzRecords;
       //    EXSR getRecords;
          ELSEIF (Func#Key = KEY#Enter);
             EXSR SelRec;
             *IN12 = *OFF;
             EXSR inzRecords;
             EXSR getRecords;
          ENDIF;
       ENDDO;


       //========================================================
       // Subrotina.: InzRecords
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
          WCdNatura  = *BLANKS;
          WIdxCobert = 1;
          DOW (NOT LST#Cobert(WIdxCobert).IsEmpty);
             ECR_OPTION = *BLANKS;
             *IN49 = *OFF;
             *IN48 = *ON;
             *IN47 = *OFF;
             IF (LST#Cobert(WIdxcobert).Header = 'S');
                ECR_ICOBRIGA = *BLANKS;
                *IN49 = *ON;
                *IN48 = *OFF;
                *IN47 = *ON;
                ECR_DSCOBERT = LST#Cobert(WIdxCobert).DsCobert;
                REC# += 1;
                WRITE RCATE00202;
             ELSE;
                //------------------------------------------------
                // Escreve os Registo da Listagem
                //------------------------------------------------
                IF (LST#Cobert(WIdxcobert).TpHeader = 'S');
                   *IN49        = *ON;
                ENDIF;
                ECR_CDPROD   = WCdProd;
                ECR_CDCOBERT = LST#Cobert(WIdxCobert).CdCobert;
                ECR_DSCOBERT = LST#Cobert(WIdxCobert).DsCobert;
                // ECR_ICMORE    = 'N';
                // ECR_ICESCREVE = $GetField('ICESCREVE');
                // ECR_CDNATURA  = $GetField('CDNATURA');
                // ECR_ICOBRIGA  = $GetField('ICOBRIGA');
                REC# += 1;
                WRITE RCATE00202;
             ENDIF;
             WIdxCobert += 1;
          ENDDO;

          //---------------------------------------
          // Validar se ainda existem mais registos
          //---------------------------------------
          IF (REC# = *ZEROS);
             *IN52 = *OFF;
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
             ECR_CDSUPER   = CAT_CDSUPER;
             ECR_DSCOBERT1 = %SUBST(CAT_DSCOBERT:1:50);
             ECR_DSCOBERT2 = %SUBST(CAT_DSCOBERT:51:50);
             ECR_NUORDEM   = CAT_NUORDEM;
             ECR_CDNATURA  = CAT_CDNATURA;
             ECR_ICESCREVE = CAT_ICESCREVE;
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
             ELSEIF ($ValidKey(Func#Key:KEY#F22)) AND (WIcDisplay);
                ECR_OPTION = 'N';
                EXFMT RCATE00222;
                IF (Func#Key = KEY#Enter) AND (%TRIM(ECR_OPTION) = 'S');
                   //==> Eliminar o Registo da Cobertura
                   DELETE (ECR_CDPROD:ECR_CDCOBERT) RCATP102;
                   FillCoberturas(PCdProd:*ZEROS);
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
                ITER;
             ELSEIF ($ValidKey(Func#Key:KEY#F08));
                MONITOR;
                   DspTextos(ECR_CDPROD:ECR_DSPROD:ECR_CDCOBERT:
                                      ECR_DSCOBERT);
                ON-ERROR;
                ENDMON;
             ELSEIF (Func#Key = KEY#F12) AND (NOT WIcDisplay);
                WIcDisplay = *ON;
                *IN12 = *OFF;
             ELSEIF (Func#Key = KEY#F04) AND (NOT WIcDisplay);
                EXSR RunPopUp;
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
                      CAT_ICESCREVE = ECR_ICESCREVE;
                      CAT_ALIAS     = ECR_ALIAS;
                      CAT_CONVERSAO = ECR_CONVERSAO;
                      UPDATE RCATP102 %FIELDS(CAT_NUORDEM:CAT_DSCOBERT:
                                              CAT_CDNATURA:CAT_ICOBRIGA:
                                              CAT_ALIAS:CAT_CONVERSAO:
                                              CAT_ICESCREVE);
                      FillCoberturas(PCdProd:*ZEROS);
                   ENDIF;
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
          ECR_CDSUPER   = *ZEROS;
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
             ELSEIF (Func#Key = KEY#Enter);
                //==> Criar o registo da Cobertura
                CAT_CDPROD    = ECR_CDPROD;
                CAT_CDCOBERT  = ECR_CDCOBERT;
                IF (PCdSuper = *ZEROS);
                   CAT_CDSUPER = CAT_CDCOBERT;
                ELSE;
                   CAT_CDSUPER = PCdSUper;
                ENDIF;
                CAT_CDSUPER   = ECR_CDSUPER;
                CAT_NUORDEM   = ECR_NUORDEM;
                CAT_DSCOBERT  = ECR_DSCOBERT1 + ECR_DSCOBERT2;
                CAT_CDNATURA  = ECR_CDNATURA;
                CAT_ICESCREVE = ECR_ICESCREVE;
                CAT_ICOBRIGA  = ECR_ICOBRIGA;
                CAT_ALIAS     = ECR_ALIAS;
                CAT_CONVERSAO = ECR_CONVERSAO;
                CAT_CAT10201 = $IntToDate(20000101);
                CAT_CAT10202 = $IntToDate(99991231);
                WRITE RCATP102;
                FillCoberturas(PCdProd:*ZEROS);

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
                CAT_CAT10301 = $IntToDate(20000101);
                CAT_CAT10302 = $IntToDate(99991231);
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

     P*===================================================================
     P* Processo..: FillCoberturas
     P* Descrição.: Actualizar lista de Coberturas
     P*===================================================================
     P FillCoberturas  B                   EXPORT
     D FillCoberturas  PI
     D   PCdProd                      5S 0 CONST
     D   PCdSuper                     5S 0 CONST
     D
     D*==> Campos de Trabalho
     D WSessionID      S              5S 0 INZ(*ZEROS)
     D WRec            S              5S 0 INZ(*ZEROS)
     D WLstIdx         S              5S 0 INZ(*ZEROS)
     D WCdNatura       S              1A   INZ(*BLANKS)
     D WBullet         S              3A   INZ(*BLANKS)
     D*=========================================================================
      /FREE
       //--------------------------------------------------
       // Declaração dos ficheiros
       //--------------------------------------------------
       WSessionID = $SETFILES('FCATP102');
       IF (PCdSuper = *ZEROS);
          WBullet = *BLANKS;
          LST#Cobert(1).IsEmpty = *ON;
          $SetCriteria('CDPROD = ''' + %CHAR(PCdProd) + ''' +
                        AND (CDCOBERT = CDSUPER)');
       ELSE;
          WBullet = '...';
          $SetCriteria('CDPROD = ''' + %CHAR(PCdProd) + ''' +
                        AND CDSUPER = ''' + %CHAR(PCdSuper) + ''' +
                        AND (CDSUPER <> CDCOBERT)');
       ENDIF;
       $SetPage(12);
       $SetSort('CDNATURA #DESC, NUORDEM');

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
          IF ((WCdNatura <> $GetField('CDNATURA')) AND (PCdsuper = *ZEROS));
             WCdNatura = $GetField('CDNATURA');
             LST#Cobert(WIdxCobert).Header   = 'S';
             LST#Cobert(WIdxCobert).CdNatura = $GetField('CDNATURA');
             LST#Cobert(WIdxCobert).DsCobert = $GetDescricao(57:WCdNatura);
             LST#Cobert(WIdxCobert).IsEmpty  = *OFF;
             WIdxCobert += 1;
          ENDIF;

          //--------------------------------------------
          // Formatar o Registo
          //--------------------------------------------
          LST#Cobert(WIdxCobert).Header   = 'N';
          LST#Cobert(WIdxCobert).CdNatura = $GetField('CDNATURA');
          LST#Cobert(WIdxCobert).CdCobert = $GetFieldNum('CDCOBERT');
          LST#Cobert(WIdxCobert).CdSuper  = $GetFieldNum('CDSUPER');
          LST#Cobert(WIdxCobert).DsCobert = %TRIM(WBullet) +
                                            %TRIM($GetField('ALIAS'))+'.'+
                                            $GetField('DSCOBERT');
          LST#Cobert(WIdxCobert).Codigo   = $GetField('ALIAS');
          LST#Cobert(WIdxCobert).IsEmpty  = *OFF;
          LST#Cobert(WIdxCobert+1).IsEmpty = *ON;

          //--------------------------------------------
          // Formatar os Dependentes
          //--------------------------------------------
          WLstIdx = WIdxCobert;
          FillCoberturas(PCdProd:$GetFieldNum('CDCOBERT'));
          IF (WLstIdx = (WIdxCobert-1));
             LST#Cobert(WLstIdx).TpHeader = 'N';
          ELSE;
             LST#Cobert(WLstIdx).TpHeader = 'S';
          ENDIF;
          WRec       += 1;
       ENDDO;

       $RmvDBSession(WSessionID);
      /END-FREE
     PFillCoberturas   E
