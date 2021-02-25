     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PCATO001
     H* Descrição.: Manutenção de Produtos
     H*==================================================================
     FFCATP114  UF A E           K DISK    PREFIX(CAT_)
     FFCATE013  CF   E             WORKSTN SFILE(RCATE01302:REC#)
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
     D PCdProd         S              5S 0
     D PDsProd         S             40A
     D
     D
     D WSessionID      S              5S 0
     C*========================================================
     C     *ENTRY        PLIST
     C                   PARM                    PCdProd
     C                   PARM                    PDSProd
     C
      /FREE
       //------------------------------------
       // Se não estiver autorizado então sai
       //------------------------------------
       IF NOT $ValAccess(S#PGM);
          *INLR = *ON;
          RETURN;
       ENDIF;

       MONITOR;
          ECR_CdProd = PCdProd;
          ECR_DsProd = PDsProd;
       ON-ERROR;
          DSPLY 'Parâmetros com Erros';
          *INLR = *ON;
          RETURN;
       ENDMON;

       WSessionID = $AddDBSession();
       $SETFILES('FCATP114 INNER JOIN FSECP010 ON CDFUNCT = CDPROGR');
       $SETFIELDS('CDPROD, CDSEQU, NUORDEM, CDFUNCT,+
                   NMPROGR, DSPROGR');
       $SetPage(12);
       $SetSort('NUORDEM');
       $SetCriteria('CDPROD = ' + %CHAR(ECR_CDPROD));
       WIcDisplay =*ON;
       *IN45 = *OFF;
       EXSR inzRecords;
       EXSR getRecords;

       DOW (NOT *IN03 AND NOT *IN12);
          IF (WIcDisplay);
             WNoFunction = 'RCATE01303'; // Listar Componentes
          ELSE;
             WNoFunction = 'RCATE01307'; // Alterar Componentes
          ENDIF;
          DS#HEADER    = $getHeader(S#PGM:WNoFunction);
          DS#FOOTER    = $getFooter(S#PGM:WNoFunction:*BLANKS);
          ECR_S#PGM    = S#PGM;
          WRITE     RCATE01300;
          WRITE     RCATE01399;
          WRITE     RCATE01398;
          EXFMT RCATE01303;
          IF ($ValidKey(Func#Key:KEY#F06)) AND (NOT WIcDisplay);
             IF (REC# < 10);
                REC# += 1;
                ECR_OPTION   = *BLANKS;
                ECR_CDSEQU   = *ZEROS;
                ECR_NUORDEM  = *ZEROS;
                ECR_CDFUNCT  = *ZEROS;
                ECR_DSPROGR  = *BLANKS;
                WRITE RCATE01302;
                *IN52 = *ON;
             ENDIF;
          ELSEIF ($ValidKey(Func#Key:KEY#F04)) AND (NOT WIcDisplay);
             EXSR RunPopUp;
          ELSEIF ($ValidKey(Func#Key:KEY#F12)) AND (NOT WIcDisplay);
             WIcDisplay = *ON;
             *IN45 = *OFF;
             *IN12 = *OFF;
             EXSR inzRecords;
             EXSR getRecords;
          ELSEIF ($ValidKey(Func#Key:KEY#F21)) AND (WIcDisplay);
             WIcDisplay = *OFF;
             *IN45 = *ON;
             EXSR inzRecords;
             EXSR getRecords;
          ELSEIF ((Func#Key = KEY#PgDown) AND (NOT $IsLastPage())) OR
             ((Func#Key = KEY#PgUp) AND ($GetPageNbr() > 1));
             EXSR inzRecords;
             EXSR getRecords;
          ELSEIF (Func#Key = KEY#Enter);
             EXSR SelRec;
             IF (NOT WIcDisplay);
                WIcDisplay = *ON;
                *IN45 = *OFF;
                *IN12 = *OFF;
                EXSR inzRecords;
                EXSR getRecords;
             ELSE;
                *IN12 = *ON;
             ENDIF;
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
          WRITE RCATE01303;     // Limpar Lista
          *In53     = *Off;     // Repor Indicador
          ECR_CM#PGRRN = 1;
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
                ECR_ISCHG    = *ZEROS;
                MONITOR;
                   ECR_CDSEQU   = %DEC($GetField('CDSEQU'):5:0);
                   ECR_NUORDEM  = %DEC($GetField('NUORDEM'):5:0);
                   ECR_CDFUNCT  = %DEC($GetField('CDFUNCT'):5:0);
                ON-ERROR;
                ENDMON;
                ECR_DSPROGR  = %TRIM($GetField('DSPROGR'));
                WRITE RCATE01302;
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
          IF (NOT WIcDisplay);
             REC#      = 1;
             CHAIN REC# RCATE01302;
             DOW (%FOUND());
                EXSR UpdBlocos;
                WSelected = *ON;
                REC# += 1;
                CHAIN REC# RCATE01302;
             ENDDO;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: UpdBlocos
       // Objectivo.: Actualizar os Blocos de Texto
       //========================================================
       BEGSR UpdBlocos;
          //----------------------------------------------------
          // Eliminar o registo
          //----------------------------------------------------
          IF (ECR_NUORDEM = *ZEROS) AND (ECR_CDFUNCT = *ZEROS);
             DELETE (ECR_CDPROD:ECR_CDSEQU) RCATP114;
          ELSEIF ($ValTemplate(35:ECR_CDFUNCT));
             //----------------------------------------------------
             // Alterar/Inserir um novo Registo
             //----------------------------------------------------
             CHAIN(N) (ECR_CDPROD:ECR_CDSEQU) RCATP114;
             IF (%FOUND());
                DELETE (ECR_CDPROD:ECR_CDSEQU) RCATP114;
             ENDIF;
             CAT_CDPROD  = ECR_CDPROD;
             CAT_CDSEQU  = $GetNextKey('FCATP114':'CDSEQU':
                       'CDPROD = ''' + %CHAR(CAT_CDPROD) + '''');
             ECR_CDSEQU  = CAT_CDSEQU;
             CAT_NUORDEM = ECR_NUORDEM;
             CAT_CDFUNCT = ECR_CDFUNCT;
             WRITE RCATP114;
          ENDIF;
       ENDSR;
       //==================================================================
       // Subrotina.: RunPopUp
       // Objectivo.: Adicionar um novo registo de uma Apólice
       //==================================================================
+----  BEGSR RunPopUp;
|+---     IF (ECR_CM#NMFLD = 'CDFUNCT');
             MONITOR;
                CHAIN ECR_CM#RELRCD RCATE01302;
                ECR_DSPROGR = *BLANKS;
                ECR_CDFUNCT = $RunPopUpTempl(35:ECR_CDFUNCT);
                IF (ECR_CDFUNCT) > *ZEROS;
                   ECR_DSPROGR  = $GetDescFunct(ECR_CDFUNCT);
                ENDIF;
                ECR_ISCHG   = 1;
                UPDATE RCATE01302;
             ON-ERROR;
                ECR_CDSEQU  = *ZEROS;
                ECR_CDFUNCT = *ZEROS;
                ECR_NUORDEM = *ZEROS;
                ECR_DSPROGR = *BLANKS;
                UPDATE RCATE01302;
             ENDMON;
          ENDIF;
|         ECR_CM#ROW = CM#CROW;
|         ECR_CM#COL = CM#CCOL;
+----  ENDSR;
      /END-FREE
