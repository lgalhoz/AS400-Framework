     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PSECO001
     H* Descrição.: Manuenção de Componentes do Sistema
     H*==================================================================
     FFSECP010  IF   E           K DISK    PREFIX(SEC_)
     FFSECP011  UF A E           K DISK    PREFIX(SEC_)
     FFSECP012  UF A E           K DISK    PREFIX(SEC_)
     FFSECE002  CF   E             WORKSTN SFILE(RSECE00202:REC#)
     F                                     SFILE(RSECE00208:REC#)
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

     D DspUsers        PR                  EXTPGM('PSECO005')
     D   WCdProgr                     5S 0 CONST
     D   WNmProgr                    10A   CONST
     D   WDsProgr                    40A   CONST
     D   WTipAcess                    1A   CONST
     D   WCdFunct                     5S 0 CONST
     D   WNmFunct                    10A   CONST
     D   WDsFunct                    40A   CONST
     D   WCdOpera                     5S 0 CONST
     D   WDsOpera                    40A   CONST
     D
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
     D WCdProgr        S              5S 0
     D WCdFunct        S              5S 0
     D WNmProgr        S             10A
     D WDsProgr        S             40A
     D WNmFunct        S             10A
     D WDsFunct        S             40A
     D
     D KTE#Funcao      S              5S 0
     D KTE#Operacao    S              5S 0
     D
     C*========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCdProgr
     C                   PARM                    WNmProgr
     C                   PARM                    WDsProgr
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
       // Sessão das Funções
       //----------------------------------------------
       KTE#Funcao = $AddDBSession();
       $SETFILES('FSECP011');
       $SETFIELDS('CDPROGR, CDFUNCT, NMFUNCT, DSFUNCT');
       $SetPage(12);
       $SetCriteria('CDPROGR = ''' + %CHAR(WCdProgr) + '''');
       $SetSort('NMFUNCT');

       //----------------------------------------------
       // Sessão das Operações
       //----------------------------------------------
       KTE#Operacao = $AddDBSession();
       $SETFILES('FSECP012');
       $SETFIELDS('CDPROGR, CDFUNCT, CDOPERA, DSOPERA, DFACCESS, +
                   CDTECLA, NUOPTION, ICSPECIAL, CDFUNCAO');
       $SetPage(12);
       $SetCriteria(*BLANKS);
       $SetSort('CDOPERA');

       $SetDBSession(KTE#Funcao); // Repor Sessão das Funções
       WIcDisplay =*ON;
       EXSR inzRecords;
       EXSR getRecords;

       ECR_CDPROGR = WCdProgr;
       ECR_NMPROGR = WNmProgr;
       ECR_DSPROGR = WDsProgr;
       DOW (NOT *IN03 AND NOT *IN12);
          ECR_CDPROGR = WCdProgr;
          IF ($GetDBSession() = KTE#Funcao);
             IF (WIcDisplay);
                WNoFunction = 'RSECE00203'; // Listar Componentes
             ELSE;
                WNoFunction = 'RSECE00204'; // Alterar Componentes
             ENDIF;
          ELSE;
             IF (WIcDisplay);
                WNoFunction = 'RSECE00209'; // Listar Operações
             ELSE;
                WNoFunction = 'RSECE00208'; // Alterar Operaçõs
             ENDIF;
          ENDIF;
          DS#HEADER   = $getHeader(S#PGM:WNoFunction);
          DS#FOOTER   = $getFooter(S#PGM:WNoFunction:*BLANKS);
          ECR_S#PGM   = S#PGM;
          WRITE     RSECE00200;
          WRITE     RSECE00299;
          WRITE     RSECE00298;
          IF ($GetDBSession() = KTE#Funcao);
             EXFMT RSECE00203;
          ELSE;
             EXFMT RSECE00209;
          ENDIF;
          IF ($ValidKey(Func#Key:KEY#F06)) AND (NOT WIcDisplay);
              REC# += 1;
              ECR_OPTION   = *BLANKS;
              IF ($GetDBSession() = KTE#Funcao);
                 ECR_CDFUNCT  = *ZEROS;
                 ECR_NMFUNCT  = *BLANKS;
                 ECR_DSFUNCT  = *BLANKS;
                 WRITE RSECE00202;
              ELSE;
                 ECR_CDOPERA  = *ZEROS;
                 ECR_NUOPTION = *ZEROS;
                 ECR_CDTECLA  = *ZEROS;
                 ECR_DSOPERA  = *BLANKS;
                 ECR_DFACCESS = *BLANKS;
                 ECR_ICSPECIAL= *BLANKS;
                 ECR_CDFUNCAO = *ZEROS;
                 WRITE RSECE00208;
              ENDIF;
              IF (REC# = 1);
                 *IN52 = *ON;
                 IF ($GetDBSession() = KTE#Funcao);
                    EXFMT RSECE00203;
                 ELSE;
                    EXFMT RSECE00209;
                 ENDIF;
              ENDIF;
              ITER;
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
                 $SetDBSession(KTE#Funcao);
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
                   $SetDBSession(KTE#Funcao);
                ELSE;
                   *IN12 = *ON;
                ENDIF;
             ENDIF;
          ENDIF;
          EXSR inzRecords;
          EXSR getRecords;
       ENDDO;

       $RmvDBSession(KTE#Funcao);
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
          WRITE RSECE00203;     // Limpar Lista de Funções
          WRITE RSECE00209;     // Limpar Lista de Operações
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
                IF ($GetDBSession() = KTE#Funcao);
                   MONITOR;
                      ECR_CDFUNCT  = %DEC($GetField('CDFUNCT'):5:0);
                      ECR_CDPROGR  = %DEC($GetField('CDPROGR'):5:0);
                   ON-ERROR;
                   ENDMON;
                   ECR_NMFUNCT  = $GetField('NMFUNCT');
                   ECR_DSFUNCT  = $GetField('DSFUNCT');
                   WRITE RSECE00202;
                ELSE;
                   MONITOR;
                      ECR_CDOPERA  = %DEC($GetField('CDOPERA'):5:0);
                   ON-ERROR;
                   ENDMON;
                   ECR_DSOPERA  = $GetField('DSOPERA');
                   ECR_DFACCESS = $GetField('DFACCESS');
                   ECR_ICSPECIAL= $GetField('ICSPECIAL');
                   MONITOR;
                      ECR_CDTECLA  = %DEC($GetField('CDTECLA'):2:0);
                      ECR_NUOPTION = %DEC($GetField('NUOPTION'):2:0);
                      ECR_CDFUNCAO = %DEC($GetField('CDFUNCAO'):5:0);
                   ON-ERROR;
                   ENDMON;
                   WRITE RSECE00208;
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
          IF ($GetDBSession=KTE#Funcao);
             READC RSECE00202;
          ELSE;
             READC RSECE00208;
          ENDIF;
          DOW (NOT %EOF());
             IF (ECR_OPTION <> *BLANKS) AND (WIcDisplay);
                IF ($GetDBSession=KTE#Funcao);
                   WSelected = *ON;
                   $SetDBSession(KTE#Operacao);
                   WCdFunct = ECR_CDFUNCT;
                   $SetCriteria('(CDPROGR = ''' + %CHAR(WCdProgr) + ''') +
                              AND (CDFUNCT = ''' + %CHAR(WCdFunct) + ''')');
                ELSE;
                   MONITOR;
                      DspUsers(ECR_CDPROGR:ECR_NMPROGR:ECR_DSPROGR:
                                ECR_DFACCESS:
                                ECR_CDFUNCT:ECR_NMFUNCT:ECR_DSFUNCT:
                                ECR_CDOPERA:ECR_DSOPERA);
                   ON-ERROR;
                   ENDMON;
                   $SetDBSession(KTE#Operacao);
                ENDIF;
                LEAVESR;
             ELSE;
                IF ($GetDBSession = KTE#Funcao);
                   EXSR MntFuncao;
                ELSE;
                   EXSR MntOperacao;
                ENDIF;
             ENDIF;
             IF ($GetDBSession=KTE#Funcao);
                READC RSECE00202;
             ELSE;
                READC RSECE00208;
             ENDIF;
          ENDDO;
       ENDSR;

       //========================================================
       // Subrotina.: MntFuncao
       // Objectivo.: Alterar, Criar ou eliminar uma função
       //========================================================
       BEGSR MntFuncao;
          //-------------------------------------------
          // Inserir um registo
          //-------------------------------------------
          IF (ECR_CDFUNCT = *ZEROS);
             SETGT (ECR_CDPROGR) RSECP011;
             READP(N) RSECP011;
             READP(N) RSECP011; // Segundo salto por causa do valor 99999
             IF (SEC_CDFUNCT = 99999);
                SEC_CDFUNCT = *ZEROS;
             ENDIF;
             SEC_CDPROGR = ECR_CDPROGR;
             SEC_CDFUNCT = SEC_CDFUNCT + 1; // ncrementar
             SEC_NMFUNCT = ECR_NMFUNCT;
             SEC_DSFUNCT = ECR_DSFUNCT;
             WRITE RSECP011;

          //-------------------------------------------
          // Eliminar um registo
          //-------------------------------------------
          ELSEIF (ECR_NMFUNCT = *BLANKS) AND (ECR_DSFUNCT = *BLANKS);
             DELETE (ECR_CDPROGR:ECR_CDFUNCT) RSECP011;

          //-------------------------------------------
          // Alterar um registo
          //-------------------------------------------
          ELSE;
             CHAIN (ECR_CDPROGR:ECR_CDFUNCT) RSECP011;
             SEC_NMFUNCT = ECR_NMFUNCT;
             SEC_DSFUNCT = ECR_DSFUNCT;
             UPDATE RSECP011 %FIELDS(SEC_NMFUNCT:SEC_DSFUNCT);
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: MntOperacao
       // Objectivo.: Alterar, Criar ou eliminar uma Operacao
       //========================================================
       BEGSR MntOperacao;
          //-------------------------------------------
          // Inserir um registo
          //-------------------------------------------
          IF (ECR_CDOPERA = *ZEROS);
             SETGT (ECR_CDPROGR:ECR_CDFUNCT) RSECP012;
             READP(N) RSECP012;
             IF ((SEC_CDPROGR<>ECR_CDPROGR) AND (SEC_CDFUNCT<>ECR_CDFUNCT));
                SEC_CDOPERA = *ZEROS;
             ELSE;
                IF (SEC_CDOPERA = 99999);
                   READP(N) RSECP012;
                   IF ((SEC_CDPROGR<>ECR_CDPROGR) AND
                       (SEC_CDFUNCT<>ECR_CDFUNCT));
                      SEC_CDOPERA = *ZEROS;
                   ENDIF;
                ENDIF;
             ENDIF;
             SEC_CDPROGR  = ECR_CDPROGR;
             SEC_CDFUNCT  = ECR_CDFUNCT;
             SEC_CDOPERA  = SEC_CDOPERA + 1; // Incrementar
             SEC_DSOPERA  = ECR_DSOPERA;
             SEC_DFACCESS = ECR_DFACCESS;
             SEC_CDTECLA  = ECR_CDTECLA;
             SEC_NUOPTION = ECR_NUOPTION;
             SEC_ICSPECIAL= ECR_ICSPECIAL;
             SEC_CDFUNCAO = ECR_CDFUNCAO;
             WRITE RSECP012;

          //-------------------------------------------
          // Eliminar um registo
          //-------------------------------------------
          ELSEIF (ECR_DSOPERA = *BLANKS) AND (ECR_DFACCESS = *BLANKS);
             DELETE (ECR_CDPROGR:ECR_CDFUNCT:ECR_CDOPERA) RSECP012;

          //-------------------------------------------
          // Alterar um registo
          //-------------------------------------------
          ELSE;
             CHAIN (ECR_CDPROGR:ECR_CDFUNCT:ECR_CDOPERA) RSECP012;
             SEC_DSOPERA  = ECR_DSOPERA;
             SEC_DFACCESS = ECR_DFACCESS;
             SEC_CDTECLA  = ECR_CDTECLA;
             SEC_NUOPTION = ECR_NUOPTION;
             SEC_ICSPECIAL= ECR_ICSPECIAL;
             SEC_CDFUNCAO = ECR_CDFUNCAO;
             UPDATE RSECP012 %FIELDS(SEC_DSOPERA:SEC_DFACCESS:
                                     SEC_CDTECLA:SEC_NUOPTION:
                                     SEC_ICSPECIAL:SEC_CDFUNCAO);
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
