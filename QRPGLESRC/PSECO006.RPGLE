     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PSECO001
     H* Descrição.: Manuenção de Componentes do Sistema
     H*==================================================================
     FFSECP010  UF A E           K DISK    PREFIX(SEC_)
     FFSECP011  UF A E           K DISK    PREFIX(SEC_)
     FFSECP012  UF A E           K DISK    PREFIX(SEC_)
     FFSECP014  UF A E           K DISK    PREFIX(SEC_)
     FFSECE006  CF   E             WORKSTN SFILE(RSECE00602:REC#)
     F                                     SFILE(RSECE00608:REC#)
     F                                     PREFIX(ECR_)
     F                                     INFDS(INFD#KEYS)
     F*==========================================================
     F* Indicadores  Descrição
     F* -----------  --------------------------------------------
     F*      45      Condicionar campos com Input/Output
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
     D WIsEmpty        S               N
     D WSelected       S               N
     D WLastRec        S              4S 0 INZ(*ZEROS)
     D
     D KTE#Menu        S              5S 0
     D KTE#Opcoes      S              5S 0
     D
     C*========================================================
      /FREE

       //------------------------------------
       // Se não estiver autorizado então sai
       //------------------------------------
       IF NOT $ValAccess(S#PGM:'':99999);
          *INLR = *ON;
          RETURN;
       ENDIF;

       //----------------------------------------------
       // Sessão das Funções
       //----------------------------------------------
       KTE#Menu = $SETFILES('FSECP010');
       $SetPage(12);
       $SetCriteria('CDTIPO = ''M''');
       $addCriteria('Nome':'NMPROGR':'GE');
       $addCriteria('Descrição':'DSPROGR':'LK');
       ECR_DSCRITER = $getCritText();
       $SetSort('DSPROGR');

       //----------------------------------------------
       // Sessão das Operações
       //----------------------------------------------
       KTE#Opcoes = $SETFILES('FSECP014');

       $SetPage(12);
       $SetCriteria(*BLANKS);
       $SetSort('NUOPTION');

       $SetDBSession(KTE#Menu); // Repor Sessão das Funções
       WIcDisplay =*ON;
       EXSR inzRecords;
       EXSR getRecords;

       DOW (NOT *IN03 AND NOT *IN12);
          IF ($GetDBSession() = KTE#Menu);
             IF (WIcDisplay);
                WNoFunction = 'RSECE00603'; // Listar Componentes
             ELSE;
                WNoFunction = 'RSECE00604'; // Alterar Componentes
             ENDIF;
          ELSE;
             IF (WIcDisplay);
                WNoFunction = 'RSECE00609'; // Listar Operações
             ELSE;
                WNoFunction = 'RSECE00608'; // Alterar Operaçõs
             ENDIF;
          ENDIF;
          DS#HEADER   = $getHeader(S#PGM:WNoFunction);
          DS#FOOTER   = $getFooter(S#PGM:WNoFunction:*BLANKS);
          DS#OPTIONS   = $getOption(S#PGM:WNoFunction:*BLANKS);
          ECR_S#PGM   = S#PGM;
          WRITE     RSECE00600;
          WRITE     RSECE00699;
          WRITE     RSECE00698;
          IF ($GetDBSession() = KTE#Menu);
             EXFMT RSECE00603;
          ELSE;
             EXFMT RSECE00609;
          ENDIF;
          IF ($ValidKey(Func#Key:KEY#F06)) AND (NOT WIcDisplay);
              REC# = WLastRec + 1;
              WLastRec = REC#;
              ECR_OPTION   = *BLANKS;
              IF ($GetDBSession() = KTE#Menu);
                 ECR_CDPROGR  = *ZEROS;
                 ECR_NMPROGR  = *BLANKS;
                 ECR_DSPROGR  = *BLANKS;
                 ECR_CMITEM   = *BLANKS;
                 WRITE RSECE00602;
              ELSE;
                 IF (NOT WIcDisplay);
                    *IN45 = *ON;
                 ENDIF;
                 ECR_ISCHG    = *ZEROS;
                 ECR_NUSEQU   = *ZEROS;
                 ECR_NUOPTION = *ZEROS;
                 ECR_CDITEM   = *ZEROS;
                 ECR_DSITEM   = *BLANKS;
                 ECR_CMITEM   = *BLANKS;
                 WRITE RSECE00608;
              ENDIF;
          ELSEIF ((Func#Key = KEY#PgDown) AND (NOT $IsLastPage())) OR
             ((Func#Key = KEY#PgUp) AND ($GetPageNbr() > 1));
             EXSR inzRecords;
             EXSR getRecords;
          ELSEIF (Func#Key = KEY#F05) AND (WIcDisplay);
             $ChgCriteria();
             ECR_DSCRITER = $getCritText();
             EXSR inzRecords;
             EXSR getRecords;
          ELSEIF (Func#Key = KEY#F12) AND ((NOT WIcDisplay) OR
               ($GetDBSession() = KTE#Opcoes));
             IF ($GetDBSession() = KTE#Opcoes);
                $SetDBSession(KTE#Menu);
             ENDIF;
             WIcDisplay = *ON;
             *IN45 = *OFF;
             *IN12 = *OFF;
             EXSR inzRecords;
             EXSR getRecords;
          ELSEIF (Func#Key = KEY#F04) AND (NOT WIcDisplay);
             EXSR RunPopUp;
             ITER;
          ELSEIF ($ValidKey(Func#Key:KEY#F21)) AND (WIcDisplay);
             WIcDisplay =*OFF;
             *IN45 = *ON;
             EXSR inzRecords;
             EXSR getRecords;
          ELSEIF (Func#Key = KEY#Enter);
             EXSR SelRec;
             *IN12 = *OFF;
             EXSR inzRecords;
             EXSR getRecords;
          ENDIF;
       ENDDO;

       $RmvDBSession(KTE#Menu);
       $RmvDBSession(KTE#Opcoes);
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
          WRITE RSECE00603;     // Limpar Lista de Funções
          WRITE RSECE00609;     // Limpar Lista de Operações
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
                IF (WIcDisplay);
                   *IN45 = *OFF;
                ELSE;
                   *IN45 = *ON;
                ENDIF;
                REC# += 1;
                ECR_OPTION   = *BLANKS;
                IF ($GetDBSession() = KTE#Menu);
                   MONITOR;
                      ECR_CDPROGR  = %DEC($GetField('CDPROGR'):5:0);
                   ON-ERROR;
                   ENDMON;
                   ECR_NMPROGR  = $GetField('NMPROGR');
                   ECR_DSPROGR  = $GetField('DSPROGR');
                   ECR_CMITEM   = $GetField('CMITEM');
                   WRITE RSECE00602;
                ELSE;
                   ECR_ISCHG    = *ZEROS;
                   MONITOR;
                      ECR_NUSEQU   = %DEC($GetField('NUSEQU'):5:0);
                      ECR_NUOPTION = %DEC($GetField('NUOPTION'):5:0);
                      ECR_CDITEM   = %DEC($GetField('CDITEM'):5:0);
                      CHAIN(N) (ECR_CDITEM) RSECP010;
                      ECR_DSITEM   = SEC_DSPROGR;
                      ECR_CMITEM   = SEC_CMITEM;
                      IF (ECR_CDITEM = ECR_CDPROGR);
                         *IN45 = *OFF;
                      ENDIF;
                   ON-ERROR;
                   ENDMON;
                   WRITE RSECE00608;
                ENDIF;
                $getNext();
             ENDDO;
          ENDIF;
          WLastRec = REC#;

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
          WSelected = *OFF;
          REC# = 1;
          IF ($GetDBSession=KTE#Menu);
             CHAIN REC# RSECE00602;
          ELSE;
             CHAIN REC# RSECE00608;
          ENDIF;
          DOW (%FOUND());
             IF (ECR_OPTION <> *BLANKS) AND (WIcDisplay);
                IF ($GetDBSession=KTE#Menu);
                   WSelected = *ON;
                   $SetDBSession(KTE#Opcoes);
                   $SetCriteria('CDMENU = ''' + %CHAR(ECR_CDPROGR) + '''');
                   LEAVESR;
                ENDIF;
             ELSE;
                IF ($GetDBSession = KTE#Menu);
                   EXSR MntMenu;
                ELSE;
                   EXSR MntOpcoes;
                ENDIF;
             ENDIF;

             REC# += 1;
             IF ($GetDBSession=KTE#Menu);
                CHAIN REC# RSECE00602;
             ELSE;
                CHAIN REC# RSECE00608;
             ENDIF;
          ENDDO;
          IF (NOT WSelected);
             IF (%TRIM(ECR_CRITERIA) <> *BLANKS);
                $SetCriteria(ECR_CRITERIA:*ON);
             ELSE;
                $SetCriteria(*BLANKS:*ON);
             ENDIF;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: MntMenu
       // Objectivo.: Alterar, Criar ou eliminar um Menu
       //========================================================
       BEGSR MntMenu;
          //-------------------------------------------
          // Inserir um registo
          //-------------------------------------------
          IF (ECR_CDPROGR = *ZEROS);
             SETGT (*HIVAL) RSECP010;
             READP(N) RSECP010;
             SEC_CDPROGR = SEC_CDPROGR + 1;
             SEC_NMPROGR = ECR_NMPROGR;
             SEC_DSPROGR = ECR_DSPROGR;
             SEC_CDOMIN  ='GRL';
             SEC_CDTIPO  ='M'; // Menu
             SEC_CDTMPLT = *ZEROS;
             SEC_CMITEM  = ECR_CMITEM;
             SEC_DFACESS = 'A';
             WRITE RSECP010;
             SEC_CDFUNCT = 99999;
             SEC_NMFUNCT = ECR_NMPROGR;
             SEC_DSFUNCT = ECR_DSPROGR;
             WRITE RSECP011;
             SEC_CDOPERA = 99999;
             SEC_VLINDICA = 99;
             SEC_DSOPERA  = 'F99=Generico';
             SEC_DFACCESS = SEC_DFACESS;
             WRITE RSECP012;
             SEC_NUSEQU = 1;
             SEC_NUOPTION = *ZEROS;
             SEC_CDITEM = SEC_CDPROGR;
             SEC_CDMENU = SEC_CDPROGR;
             WRITE RSECP014;

          //-------------------------------------------
          // Alterar um registo
          //-------------------------------------------
          ELSE;
             CHAIN (ECR_CDPROGR) RSECP010;
             IF (%FOUND());
                SEC_NMPROGR = ECR_NMPROGR;
                SEC_DSPROGR = ECR_DSPROGR;
                SEC_CMITEM  = ECR_CMITEM;
                UPDATE RSECP010 %FIELDS(SEC_NMPROGR:SEC_DSPROGR:SEC_CMITEM);
             ENDIF;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: MntOpcoes
       // Objectivo.: Alterar, Criar ou eliminar uma Opção de Menu
       //========================================================
       BEGSR MntOpcoes;
          //-------------------------------------------
          // Inserir um registo
          //-------------------------------------------
          CHAIN(N) (ECR_CDPROGR:ECR_NUSEQU) RSECP014;
          IF (%FOUND());
             IF (ECR_CDITEM = *ZEROS);
                DELETE (ECR_CDPROGR:ECR_NUSEQU) RSECP014;
             ELSE;
                CHAIN (ECR_CDPROGR:ECR_NUSEQU) RSECP014;
                SEC_NUOPTION = ECR_NUOPTION;
                SEC_CDITEM   = ECR_CDITEM;
                UPDATE RSECP014 %FIELDS(SEC_NUOPTION:SEC_CDITEM);
             ENDIF;
          ELSE;
             SETGT (ECR_CDPROGR) RSECP014;
             READP(N) RSECP014;
             SEC_CDMENU   = ECR_CDPROGR;
             SEC_NUSEQU   = SEC_NUSEQU + 1;
             SEC_NUOPTION = ECR_NUOPTION;
             SEC_CDITEM   = ECR_CDITEM;
             WRITE RSECP014;
          ENDIF;
       ENDSR;

       //==================================================================
       // Subrotina.: RunPopUp
       // Objectivo.: Adicionar um novo registo de uma Apólice
       //==================================================================
+----  BEGSR RunPopUp;
|+---     IF (ECR_CM#NMFLD = 'CDITEM') AND (ECR_CM#RELRCD > 1);
             MONITOR;
                CHAIN ECR_CM#RELRCD RSECE00608;
                ECR_DSITEM = *BLANKS;
                ECR_CMITEM = *BLANKS;
                ECR_CDITEM = $RunPopUpComp(ECR_CDITEM);
                IF (ECR_CDITEM) > *ZEROS;
                   CHAIN(N) (ECR_CDITEM) RSECP010;
                   IF (%FOUND());
                      ECR_DSITEM = SEC_DSPROGR;
                      ECR_CMITEM = SEC_CMITEM;
                      ECR_ISCHG  = 1;
                   ENDIF;
                ENDIF;
                UPDATE RSECE00608;
             ON-ERROR;
                ECR_CDITEM = 32;
                UPDATE RSECE00608;
             ENDMON;
          ENDIF;
|         ECR_CM#ROW = CM#CROW;
|         ECR_CM#COL = CM#CCOL;
+----  ENDSR;
      /END-FREE
