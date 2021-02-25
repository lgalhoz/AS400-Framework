     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PSECO001
     H* Descrição.: Manuenção de Componentes do Sistema

     H*==================================================================
     FFSECP030  UF A E           K DISK    PREFIX(SEC_)
     FFSECE004  CF   E             WORKSTN SFILE(RSECE00402:REC#)
     F                                     SFILE(RSECE00408:REC#)
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
     D WCdUser         S             10A
     D WNmUser         S             40A
     D WCdProgr        S              5S 0
     D WCdFunct        S              5S 0
     D WNmProgr        S             10A
     D WDsProgr        S             40A
     D
     D KTE#Compon      S              5S 0 INZ(*ZEROS)
     D KTE#Operacao    S              5S 0 INZ(*ZEROS)
     C*========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCdUser
     C                   PARM                    WNmUser
     C
      /FREE

       //------------------------------------
       // Se não estiver autorizado então sai
       //------------------------------------
       IF NOT $ValAccess(S#PGM:'':99999);
          *INLR = *ON;
          RETURN;
       ENDIF;

       //----------------------------------------------
       // Sessão dos Componentes
       //----------------------------------------------
       KTE#Compon = $SETFILES('FSECP010');
       $SetPage(12);
       $AddCriteria('Nome':'NMPROGR':'GE');
       $AddCriteria('Descrição':'DSPROGR':'LK');
       ECR_DSCRITER = $getCritText();

       //----------------------------------------------
       // Sessão das Funções
       //----------------------------------------------
       KTE#Operacao = $SETFILES('FSECP011 A INNER JOIN FSECP012 B ON +
                  (A.CDPROGR = B.CDPROGR) AND (A.CDFUNCT = B.CDFUNCT)');
       $SetFields('FSECP011':'A');
       $SetFields('FSECP012':'B':*OFF);
       $SetPage(12);
       $SetSort('NMFUNCT, DSOPERA');

       $SetDBSession(KTE#Compon); // Repor Sessão das Funções
       WIcDisplay =*ON;
       EXSR inzRecords;
       EXSR getRecords;

       ECR_CDUSER  = WCdUser;
       ECR_NMUSER  = WNmUser;
       DOW (NOT *IN03 AND NOT *IN12);
          //ECR_CDPROGR = WCdProgr;
          IF ($GetDBSession() = KTE#Compon);
             IF (WIcDisplay);
                WNoFunction = 'RSECE00403'; // Listar Componentes
             ELSE;
                WNoFunction = 'RSECE00404'; // Alterar Componentes
             ENDIF;
          ELSE;
             IF (WIcDisplay);
                WNoFunction = 'RSECE00409'; // Listar Operações
             ELSE;
                WNoFunction = 'RSECE00408'; // Alterar Operaçõs
             ENDIF;
          ENDIF;
          DS#HEADER   = $getHeader(S#PGM:WNoFunction);
          DS#FOOTER   = $getFooter(S#PGM:WNoFunction:*BLANKS);
          ECR_S#PGM   = S#PGM;
          WRITE     RSECE00400;
          WRITE     RSECE00499;
          WRITE     RSECE00498;
          IF ($GetDBSession() = KTE#Compon);
             EXFMT RSECE00403;
          ELSE;
             EXFMT RSECE00409;
          ENDIF;
          IF (Func#Key = KEY#F05);
             $ChgCriteria();
             ECR_DSCRITER = $getCritText();
             EXSR inzRecords;
             EXSR getRecords;
          ELSEIF ((Func#Key = KEY#PgDown) AND (NOT $IsLastPage())) OR
             ((Func#Key = KEY#PgUp) AND ($GetPageNbr() > 1));
             EXSR inzRecords;
             EXSR getRecords;
          ELSEIF (Func#Key = KEY#F12);
             IF (NOT WIcDisplay);
                WIcDisplay = *ON;
                *IN45 = *OFF;
                *IN12 = *OFF;
             ELSEIF ($GetDBSession() = KTE#Operacao);
                 $SetDBSession(KTE#Compon);
                *IN12 = *OFF;
             ENDIF;
             EXSR inzRecords;
             EXSR getRecords;
          ELSEIF ($ValidKey(Func#Key:KEY#F21)) AND (WIcDisplay);
             WIcDisplay =*OFF;
             *IN45 = *ON;
             EXSR inzRecords;
             EXSR getRecords;
          ELSEIF (Func#Key = KEY#Enter);
             EXSR SelRec;
             IF (NOT WSelected);
                IF (NOT WIcDisplay);
                  WIcDisplay = *ON;
                  *IN45 = *OFF;
                  *IN12 = *OFF;
                ELSEIF ($GetDBSession() = KTE#Operacao);
                   $SetDBSession(KTE#Compon);
                ELSE;
                   *IN12 = *ON;
                ENDIF;
             ENDIF;
          ENDIF;
       ENDDO;
       $RmvDBSession(KTE#Compon);
       $RmvDBSession(KTE#Operacao);
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
          WRITE RSECE00403;     // Limpar Lista de Funções
          WRITE RSECE00409;     // Limpar Lista de Operações
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
                IF ($GetDBSession() = KTE#Compon);
                   MONITOR;
                      ECR_CDPROGR  = %DEC($GetField('CDPROGR'):5:0);
                   ON-ERROR;
                   ENDMON;
                   ECR_NMPROGR  = $GetField('NMPROGR');
                   ECR_DSPROGR  = $GetField('DSPROGR');
                   ECR_DFACESS  = $GetField('DFACESS');
                   CHAIN(N) (ECR_CDPROGR:99999:99999:WCdUser) RSECP030;
                   IF (%FOUND());
                      ECR_DFACESS = SEC_TPACCES;
                   ENDIF;
                   WRITE RSECE00402;
                ELSE;
                   MONITOR;
                      ECR_CDFUNCT  = %DEC($GetField('A.CDFUNCT'):5:0);
                   ON-ERROR;
                   ENDMON;
                   MONITOR;
                      ECR_CDOPERA  = %DEC($GetField('B.CDOPERA'):5:0);
                   ON-ERROR;
                   ENDMON;
                   ECR_DSFUNCT  = $GetField('A.DSFUNCT');
                   ECR_DSOPERA  = $GetField('B.DSOPERA');
                   ECR_DFACCESS = $GetField('B.DFACCESS');
                   CHAIN(N) (ECR_CDPROGR:ECR_CDFUNCT:ECR_CDOPERA:
                             WCdUser) RSECP030;
                   IF (%FOUND());
                      ECR_DFACCESS = SEC_TPACCES;
                   ENDIF;
                   WRITE RSECE00408;
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
          WSelected = *OFF;
          IF ($GetDBSession=KTE#Compon);
             READC RSECE00402;
          ELSE;
             READC RSECE00408;
          ENDIF;
          DOW (NOT %EOF());
             WSelected = *ON;
             IF (ECR_OPTION<>*BLANKS) AND (WIcDisplay);
                 IF ($GetDBSession=KTE#Compon);
                    $SetDBSession(KTE#Operacao);
                    WCdFunct = ECR_CDFUNCT;
                    $SetCriteria('(A.CDPROGR = ''' +
                               %CHAR(ECR_CDPROGR)+ ''')');
                    LEAVESR;
                 ENDIF;
             ELSEIF (NOT WIcDisplay);
                IF ($GetDBSession = KTE#Compon);
                   EXSR MntFuncao;
                ELSE;
                   EXSR MntOperacao;
                ENDIF;
             ENDIF;
             IF ($GetDBSession=KTE#Compon);
                READC RSECE00402;
             ELSE;
                READC RSECE00408;
             ENDIF;
          ENDDO;

          IF (NOT WSelected) AND ($GetDBSession=KTE#Compon);
             IF (%TRIM(ECR_CRITERIA) <> *BLANKS);
                $SetCriteria(ECR_CRITERIA:*ON);
             ELSE;
                $SetCriteria(*BLANKS);
             ENDIF;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: MntFuncao
       // Objectivo.: Alterar, Criar ou eliminar uma função
       //========================================================
       BEGSR MntFuncao;
          //-------------------------------------------
          // Inserir um registo
          //-------------------------------------------
          CHAIN(N) (ECR_CDPROGR:99999:99999:WCdUser) RSECP030;
          IF (%FOUND());
             IF (ECR_DFACESS = *BLANKS);
                DELETE (ECR_CDPROGR:99999:99999:ECR_CDUSER) RSECP030;
             ELSE;
                CHAIN (ECR_CDPROGR:99999:99999:WCdUser) RSECP030;
                SEC_TPACCES = ECR_DFACESS;
                UPDATE RSECP030 %FIELDS(SEC_TPACCES);
             ENDIF;
          ELSE;
             SEC_CDPROGR = ECR_CDPROGR;
             SEC_CDFUNCT = 99999;
             SEC_CDOPERA = 99999;
             SEC_CDUSER  = WCdUser;
             SEC_TPACCES = ECR_DFACESS;
             WRITE RSECP030;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: MntOperacao
       // Objectivo.: Alterar, Criar ou eliminar uma Operacao
       //========================================================
       BEGSR MntOperacao;
          CHAIN(N) (ECR_CDPROGR:ECR_CDFUNCT:ECR_CDOPERA:
                    WCdUser) RSECP030;
          IF (%FOUND());
             IF (ECR_DFACCESS = *BLANKS);
                DELETE (ECR_CDPROGR:ECR_CDFUNCT:ECR_CDOPERA:WCdUser)
                   RSECP030;
             ELSE;
                CHAIN (ECR_CDPROGR:ECR_CDFUNCT:ECR_CDOPERA:WCdUser) RSECP030;
                SEC_TPACCES = ECR_DFACCESS;
                UPDATE RSECP030 %FIELDS(SEC_TPACCES);
             ENDIF;
          ELSE;
             SEC_CDPROGR = ECR_CDPROGR;
             SEC_CDFUNCT = ECR_CDFUNCT;
             SEC_CDOPERA = ECR_CDOPERA;
             SEC_CDUSER  = WCdUser;
             SEC_TPACCES = ECR_DFACCESS;
             WRITE RSECP030;
          ENDIF;
       ENDSR;

       //==================================================================
       // Subrotina.: RunPopUp
       // Objectivo.: Adicionar um novo registo de uma Apólice
       //==================================================================
+----  BEGSR RunPopUp;
|+---     //IF (ECR_CM#NMFLD = 'CDOMIN');
          // ECR_CDOMIN = $runPopUp(4:ECR_CDOMIN);
|+---     //ELSEIF (ECR_CM#NMFLD = 'CDTIPO');
          // ECR_CDTIPO = $runPopUp(55:ECR_CDTIPO);
|+---     //ELSEIF (ECR_CM#NMFLD = 'DFACESS');
          // ECR_DFACESS = $runPopUp(56:ECR_DFACESS);
          //ENDIF;
|      // ECR_CM#ROW = CM#CROW;
|      // ECR_CM#COL = CM#CCOL;
+----  ENDSR;
      /END-FREE
