     H ALWNULL(*USRCTL)  BNDDIR('JMDIR':'JMINT') DFTACTGRP(*NO)
     H AUT(*ALL) ACTGRP(*NEW)
     H*==================================================================
     H* Programa..: PINTE054
     H* Descrição.: Manutenção da Informação de Mediadores
     H*
     H*    +-------+
     H*    | +-------+    Programa adaptado para trabalhar com diferentes
     H*    +-| +-------+  níveis de ecrãs de consulta em lista.
     H*      +-| SCR01 |
     H*        +-------+
     H*==================================================================
     FFINTP055  UF   E           K DISK    PREFIX(INT_)
     FFINTP056  UF   E           K DISK    PREFIX(INT_)
     FFINTP058  UF   E           K DISK    PREFIX(INT_)
     FFINTP05801IF   E           K DISK    PREFIX(INT_)
     F                                     RENAME(RINTP058:RINTP05801)
     FFINTP059  UF   E           K DISK    PREFIX(INT_)
     FTMPFILE   UF A F  400        DISK    USROPN EXTFILE(WFileName)
     FFINTE054  CF   E             WORKSTN SFILE(RINTE05402:REC#)
     F                                     PREFIX(ECR_)
     F                                     INFDS(INFD#KEYS)
     F*==================================================================*
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SCATO010
      /Copy QRPGLESRC,SSECO010
      /Copy QRPGLESRC,SGRLO013
      /Copy QRPGLESRC,SINTO010

     D*==> Registo a inserir no ficheiro
     D R#TEMP          DS                  QUALIFIED
     D   Texto                 1    400
     D
     D*==> Gerar Interfaces
     D GerarInterface  PR                  EXTPGM('PINTO050')
     D
     D*==> Lista de Processos
     D IsScreen        PR              N
     D   TipScreen                    1A   CONST
     D GetScreen       PR            10A
     D ChgScreen       PR             7A
     D   TipScreen                    1A   CONST
     D   Criteria                   100A   CONST OPTIONS(*NOPASS)
     D StartScreen     PR             7A
     D   Default                      1A   CONST
     D EndScreen       PR             7A
     D MakeInterface   PR             7A
     D    CodMediad                   9S 0 CONST
     D    CodInterf                   5S 0 CONST
     D    CodProces                   5S 0 CONST OPTIONS(*NOPASS)
     D
     D*==> Lista de Parametros
     D shell           PR                  EXTPGM('QCMDEXC')
     D   StdCmd                     200A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5
     D WStdCmd         S            200A
     D WStdLen         S             15P 5
     D
     D*==> Listagens activas
     D LST#Screen      DS                  QUALIFIED DIM(6)
     D   CodSessao                    5S 0 INZ(*ZEROS)
     D   TipLista                     1A   INZ(*BLANKS)
     D   NoFunction                  10A   INZ(*BLANKS)
     D   IsEmpty                       N   INZ(*OFF)
     D
     D*==> Profundidade da Consulta
     D LVL#Screen      DS                  QUALIFIED DIM(6)
     D   TipLista                     1A   INZ(*BLANKS)
     D   IdxLst                       3S 0 INZ(*ZEROS)
     D   CodSessao                    3S 0 INZ(*ZEROS)
     D   IsEmpty                       N   INZ(*OFF)
     D
     D*==> Campos de Trabalho
     D WQtdeRegistos   S              3S 0
     D WMsgErro        S              7A
     D WNoFunction     S             10A
     D WSelected       S               N
     D WExtKeys        S             24A
     D
     D WFileName       S             10A
     D WCodProcess     S              5S 0
     D WCodMediador    S              9S 0
     D WCodInterf      S              5S 0
     D WCodTempor      S              5S 0
     D WCriteria       S            100A
     D
     D REC#            S              4S 0
     D
     D*==> Constantes
     D SES#Sessao      C                   'S'
     D SES#Tempor      C                   'T'
     D SES#Interface   C                   'I'
     D SES#Mediador    C                   'M'
     D SES#Registos    C                   'R'
     D STS#Aberto      C                   'OPN'
     D STS#Fechado     C                   'CLO'
     D STS#Enviado     C                   'ENV'
     D*==================================================================*
      /FREE
       //------------------------------------
       // Validar o acesso a esta função
       //------------------------------------
       IF NOT $ValAccess(S#PGM:'':99999);
          *INLR = *ON;
          RETURN;
       ENDIF;
       StartScreen(SES#Sessao);
       EXSR ListShow;
       ENDScreen();

       *INLR = *ON;

       //=========================================================
       // Subrotina.: ListShow
       // Objectivo.: Apresentar o detalhe dos registo da lista
       //=========================================================
       BEGSR ListShow;
          DOW (NOT *IN03 AND NOT *IN12);
             EXSR getRecords;
             WNoFunction = GetScreen();
             DS#HEADER   = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER   = $getFooter(S#PGM:WNoFunction:*BLANKS);
             DS#OPTIONS  = $getOption(S#PGM:WNoFunction:*BLANKS);
             ECR_S#PGM   = S#PGM;
             ECR_ERR#MSG = *BLANKS;
             WRITE RINTE05400;
             WRITE RINTE05499;
             WRITE RINTE05498;
             EXFMT RINTE05403;
             IF ($ValidKey(Func#Key:KEY#F05));
                $ChgCriteria();
                ECR_DSCRITER = $getCritText();
             ELSEIF ($ValidKey(Func#Key:KEY#F08));
             ELSEIF ($ValidKey(Func#Key:KEY#F07));
             ELSEIF ($ValidKey(Func#Key:KEY#F09));
                IF (IsScreen(SES#Sessao));
                   ChgScreen(SES#Mediador);
                ELSEIF (IsScreen(SES#Tempor));
                ELSEIF (IsScreen(SES#Interface));
                ELSEIF (IsScreen(SES#Mediador));
                   ChgScreen(SES#Sessao);
                ENDIF;
             ELSEIF ($ValidKey(Func#Key:KEY#F12));
                IF (ChgScreen(*BLANKS) = *BLANKS);
                   *IN12 = *OFF;
                ENDIF;
             ELSEIF ($ValidKey(Func#Key:KEY#F21));
                GerarInterface();
             ELSEIF (Func#Key = KEY#Enter);
                EXSR SelRec;
             ENDIF;
          ENDDO;
       ENDSR;

       //========================================================
       // Subrotina.: GetRecords
       // Objectivo.: Obter todos os registos de tabelas disponív
       //========================================================
       BEGSR getRecords;
          //-------------------------------------------------------
          // Inicializar os ReEgistos
          //-------------------------------------------------------
          REC#      = *ZEROS;   // Controlo de Registos da Lista
          *In53     = *On;      // Limpar Lista de Ecrã
          *In52     = *On;      // Mostrar a Lista de Ecrã
          WRITE RINTE05403;     // Limpar Lista de Funções
          *In53     = *Off;     // Repor Indicador
          *In52     = *Off;     // Mostrar a Lista de Ecrã
          WQtdeRegistos = *ZEROS;

          //-------------------------------------------------------
          // Obter os Registos
          //-------------------------------------------------------
          IF ($GetPage(Func#Key) > *ZEROS);
             DOW (NOT $IsEOF());
                WQtdeRegistos += 1;
                //------------------------------------------------
                // Escreve os Registo da Listagem
                //------------------------------------------------
                IF (IsScreen(SES#Sessao));
                   ECR_CDPROCE = $GetFieldNum('CDPROCE');
                ELSEIF (IsScreen(SES#Tempor));
                   ECR_NUSEQU  = $GetFieldNum('A.NUSEQU');
                ELSEIF (IsScreen(SES#Interface));
                   ECR_CDMEDIA = $GetFieldNum('A.CDMEDIA');
                   ECR_CDINTER = $GetFieldNum('B.CDINTER');
                ELSEIF (IsScreen(SES#Mediador));
                   ECR_CDMEDIA = $GetFieldNum('CDMEDIA');
                ENDIF;
                ECR_OPTION   = *BLANKS;
                ECR_LST#HDR = $GetLstHdr();
                ECR_LST#DET = $GetLstLine();
                REC# += 1;
                ECR_RRN = 1;
                WRITE RINTE05402;
                *IN52 = *ON;       // Encontrou registos
                $getNext();
             ENDDO;
             *IN54 = $IsLastPage();   // Indicação de Fim de Página
          ELSE;
             *IN54 = *OFF;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: SelRec
       // Objectivo.: Seleccionar os registos
       //========================================================
       BEGSR SelRec;
          WSelected = *OFF;
          READC RINTE05402;
          DOW (NOT %EOF());
             WSelected = *ON;
             WCodProcess  = ECR_CDPROCE;
             WCodProcess  = ECR_CDPROCE;
             IF ($ValidOption(ECR_OPTION:'2':S#PGM:WNoFunction));
                IF (IsScreen(SES#Sessao));
                   IF ($ShowDialog('INT0026':*BLANKS:'NS') = 'S');
                      EXSR CloseSession;
                      IF (WMsgErro <> *BLANKS);
                         $ShowDialog(WMsgErro);
                      ENDIF;
                   ENDIF;
                ELSEIF (IsScreen(SES#Tempor));
                ELSEIF (IsScreen(SES#Interface));
                ELSEIF (IsScreen(SES#Mediador));
                ENDIF;
             ELSEIF ($ValidOption(ECR_OPTION:'4':S#PGM:WNoFunction));
                IF (IsScreen(SES#Sessao));
                   WCriteria = '(B.CDPROCE ='''+%CHAR(ECR_CDPROCE)+''')';
                   ChgScreen(SES#Interface:WCriteria);
                ELSEIF (IsScreen(SES#Tempor));
                ELSEIF (IsScreen(SES#Interface));
                ELSEIF (IsScreen(SES#Mediador));
                ENDIF;
             ELSEIF ($ValidOption(ECR_OPTION:'5':S#PGM:WNoFunction));
                IF (IsScreen(SES#Sessao));
                   WCriteria = '(A.CDPROCE ='''+%CHAR(ECR_CDPROCE)+''')';
                   ChgScreen(SES#Tempor:WCriteria);
                ELSEIF (IsScreen(SES#Tempor));
                ELSEIF (IsScreen(SES#Interface));
                   WCriteria = '(CDMEDIA ='''+%CHAR(ECR_CDMEDIA)+''') AND +
                                (CDINTER ='''+%CHAR(ECR_CDINTER)+''')';
                   ChgScreen(SES#Registos:WCriteria);
                ELSEIF (IsScreen(SES#Mediador));
                ENDIF;
             ELSEIF ($ValidOption(ECR_OPTION:'6':S#PGM:WNoFunction));
                IF (IsScreen(SES#Sessao));
                ELSEIF (IsScreen(SES#Tempor));
                ELSEIF (IsScreen(SES#Interface));
                   WMsgErro = MakeInterface(ECR_CDMEDIA:ECR_CDINTER);
                   IF (WMsgErro <> *BLANKS);
                      $ShowDialog(WMsgErro);
                   ENDIF;
                ELSEIF (IsScreen(SES#Mediador));
                ENDIF;
             ELSEIF ($ValidOption(ECR_OPTION:'8':S#PGM:WNoFunction));
                IF (IsScreen(SES#Sessao));
                   IF ($ShowDialog('INT0027':*BLANKS:'NS') = 'S');
                      EXSR DeleteSession;
                      IF (WMsgErro <> *BLANKS);
                         $ShowDialog(WMsgErro);
                      ENDIF;
                   ENDIF;
                ELSEIF (IsScreen(SES#Tempor));
                   IF ($ShowDialog('INT0028':*BLANKS:'NS') = 'S');
                      EXSR DeleteTemporario;
                      IF (WMsgErro <> *BLANKS);
                         $ShowDialog(WMsgErro);
                      ENDIF;
                   ENDIF;
                ELSEIF (IsScreen(SES#Interface));
                ELSEIF (IsScreen(SES#Mediador));
                ENDIF;
             ENDIF;
             READC RINTE05402;
          ENDDO;
          IF (NOT WSelected);
             IF (ECR_CRITERIA <> *BLANKS);
                $SetCriteria(ECR_CRITERIA:*ON);
             ELSE;
                $SetCriteria(*BLANKS:*ON);
             ENDIF;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: CloseSession
       // Objectivo.: Fechar a sessão
       //========================================================
       BEGSR CloseSession;
          WMsgErro = *BLANKS;
          CHAIN(N) (ECR_CDPROCE) RINTP055;
          IF (%FOUND(FINTP055));
             IF (INT_CDESTADO = STS#Aberto);
                CHAIN (ECR_CDPROCE) RINTP055;
                INT_CDESTADO = STS#Fechado;
                INT_DTTERMO  = INT_DTULTPRC;
                UPDATE RINTP055 %FIELDS(INT_CDESTADO:INT_DTTERMO);

                SETLL (ECR_CDPROCE) RINTP05801;
                READE (ECR_CDPROCE) RINTP05801;
                DOW (NOT %EOF(FINTP05801));
                   CHAIN (INT_CDMEDIA:INT_CDINTER) RINTP058;
                   INT_CDSTATUS = STS#Fechado;
                   UPDATE RINTP058 %FIELDS(INT_CDSTATUS);
                   READE (ECR_CDPROCE) RINTP05801;
                ENDDO;
             ELSE;
                WMsgErro = 'INT0025';
             ENDIF;
          ELSE;
             WMsgErro = 'INT0024';
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: DeleteSession
       // Objectivo.: Eliminar acções realizadas na Sessão
       //========================================================
       BEGSR DeleteSession;
          WMsgErro = *BLANKS;
          CHAIN(N) (ECR_CDPROCE) RINTP055;
          IF (%FOUND(FINTP055));
             IF (INT_CDESTADO = STS#Aberto);
                //----------------------------------------------
                // Eliminar a Sessão
                //----------------------------------------------
                CHAIN (ECR_CDPROCE) RINTP055;
                DELETE RINTP055;

                //----------------------------------------------
                // Eliminar os Temporários associados à sessão
                //----------------------------------------------
                SETLL (ECR_CDPROCE) RINTP056;
                READE(N) (ECR_CDPROCE) RINTP056;
                DOW (NOT %EOF(FINTP056));
                   CHAIN (ECR_CDPROCE:INT_NUSEQU) RINTP056;
                   DELETE RINTP056;
                   READE(N) (ECR_CDPROCE) RINTP056;
                ENDDO;

                //----------------------------------------------
                // Eliminar os Interfaces gerados para a Sessão
                //----------------------------------------------
                SETLL (ECR_CDPROCE) RINTP05801;
                READE (ECR_CDPROCE) RINTP05801;
                DOW (NOT %EOF(FINTP05801));
                   WCodMediador = INT_CDMEDIA;
                   WCodInterf   = INT_CDINTER;

                   //----------------------------------------------
                   // Eliminar os Registos do Interfaces da Sessão
                   //----------------------------------------------
                   SETLL (WCodMediador:WCodInterf) RINTP059;
                   READE(N) (WCodMediador:WCodInterf) RINTP059;
                   DOW (NOT %EOF(FINTP059));
                      CHAIN (WCodMediador:WCodInterf:INT_NUSEQU) RINTP059;
                      DELETE FINTP059;
                      READE(N) (WCodMediador:WCodInterf) RINTP059;
                   ENDDO;

                   CHAIN (WCodMediador:WCodInterf) RINTP058;
                   DELETE RINTP058;
                   READE (ECR_CDPROCE) RINTP05801;
                ENDDO;
             ELSE;
                WMsgErro = 'INT0025';
             ENDIF;
          ELSE;
             WMsgErro = 'INT0024';
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: DeleteTemporario
       // Objectivo.: Eliminar acções realizadas na Sessão
       //========================================================
       BEGSR DeleteTemporario;
          WMsgErro = *BLANKS;
          CHAIN(N) (ECR_CDPROCE) RINTP055;
          IF (%FOUND(FINTP055));
             IF (INT_CDESTADO = STS#Aberto);
                //----------------------------------------------
                // Eliminar os registos gerados para o temporário
                //----------------------------------------------
                WCodTempor = ECR_NUSEQU;
                SETLL (ECR_CDPROCE) RINTP05801;
                READE (ECR_CDPROCE) RINTP05801;
                DOW (NOT %EOF(FINTP05801));
                   WCodMediador = INT_CDMEDIA;
                   WCodInterf   = INT_CDINTER;

                   //----------------------------------------------
                   // Eliminar os Registos do Interfaces da Sessão
                   //----------------------------------------------
                   SETLL (WCodMediador:WCodInterf) RINTP059;
                   READE(N) (WCodMediador:WCodInterf) RINTP059;
                   DOW (NOT %EOF(FINTP059));
                      IF (WCodTempor = INT_CDFILE);
                         CHAIN (WCodMediador:WCodInterf:INT_NUSEQU) RINTP059;
                         DELETE FINTP059;
                      ENDIF;
                      READE(N) (WCodMediador:WCodInterf) RINTP059;
                   ENDDO;
                   READE (ECR_CDPROCE) RINTP05801;
                ENDDO;

                //----------------------------------------------
                // Eliminar o ficheiro Temporario
                //----------------------------------------------
                CHAIN (ECR_CDPROCE:ECR_NUSEQU) RINTP056;
                DELETE RINTP056;
             ELSE;
                WMsgErro = 'INT0025';
             ENDIF;
          ELSE;
             WMsgErro = 'INT0024';
          ENDIF;
       ENDSR;
      /END-FREE

     P*==================================================================
     P* Processo..: IsScreen
     P* Descrição.: Obter a actual listagem de ecrã
     P*==================================================================
     P IsScreen        B                   EXPORT
     D IsScreen        PI              N
     D   TipScreen                    1A   CONST
     D
     D*==> Campos de Trabalho
     D WIdxLst         S              3S 0
     D WIdxLvl         S              3S 0
     D*=================================================================
      /FREE
       WIdxLvl = 1;
       DOW (NOT LVL#Screen(WIdxLvl).IsEmpty);
          WIdxLst = LVL#Screen(WIdxLvl).IdxLst;
          WIdxLvl += 1;
       ENDDO;
       IF (WIdxLvl = 1);
          RETURN *OFF;
       ELSE;
          IF (LST#Screen(WIdxLst).TipLista = TipScreen);
             RETURN *ON;
          ELSE;
             RETURN *OFF;
          ENDIF;
       ENDIF;
      /END-FREE
     P IsScreen        E

     P*==================================================================
     P* Processo..: GetScreen
     P* Descrição.: Obter a actual listagem de ecrã
     P*==================================================================
     P GetScreen       B                   EXPORT
     D GetScreen       PI            10A
     D
     D*==> Campos de Trabalho
     D WIdxLst         S              3S 0
     D WIdxLvl         S              3S 0
     D*=================================================================
      /FREE
       WIdxLvl = 1;
       DOW (NOT LVL#Screen(WIdxLvl).IsEmpty);
          WIdxLst = LVL#Screen(WIdxLvl).IdxLst;
          WIdxLvl += 1;
       ENDDO;
       IF (WIdxLvl = 1);
          RETURN *BLANKS;
       ELSE;
          RETURN LST#Screen(WIdxLst).NoFunction;
       ENDIF;
      /END-FREE
     P GetScreen       E

     P*==================================================================
     P* Processo..: ChgScreen
     P* Descrição.: Mudar a listagem seguinte
     P*             Quando o TipScreen = '', passa para o ecrã anterior
     P*==================================================================
     P ChgScreen       B                   EXPORT
     D ChgScreen       PI             7A
     D   TipScreen                    1A   CONST
     D   Criteria                   100A   CONST OPTIONS(*NOPASS)
     D
     D*==> Campos de Trabalho
     D WCodErro        S              7A
     D WCriteria       S            100A
     D WIdxLst         S              3S 0
     D WIdxLvl         S              3S 0
     D*=================================================================
      /FREE
       WCodErro  = *BLANKS;
       WCriteria = *BLANKS;
       IF (%PARMS() > 1);
          WCriteria = Criteria;
       ENDIF;

       //--------------------------------------------
       // Posicionar-se no ecrã anterior
       //--------------------------------------------
       IF (TipScreen = *BLANKS);
          WIdxLvl = 1;
          DOW (NOT LVL#Screen(WIdxLvl).IsEmpty);
             WIdxLvl += 1;
          ENDDO;

          IF (WIdxLvl > 2);
             WIdxLvl -= 1;
             LVL#Screen(WIdxLvl).IsEmpty = *ON;
             WIdxLvl -= 1;
             WIdxLst  = LVL#Screen(WIdxLvl).IdxLst;
          ELSE;
             WCodErro = 'INT0029';
          ENDIF;
       ENDIF;

       //--------------------------------------------
       // Posicionar-se no ecrã Solicitado
       //--------------------------------------------
       IF (TipScreen <> *BLANKS);
          //--------------------------------------------------
          // Validar se o ecrã já está activo noutra sessão
          //--------------------------------------------------
          WIdxLvl = 1;
          DOW (NOT LVL#Screen(WIdxLvl).IsEmpty);
             IF (LVL#Screen(WIdxLvl).TipLista = TipScreen);
                RETURN 'INT0029';
             ENDIF;
             WIdxLvl += 1;
          ENDDO;


          //--------------------------------------------------
          // Abrir o ecrã
          //--------------------------------------------------
          WIdxLst = 1;
          DOW (NOT LST#Screen(WIdxLst).IsEmpty);
             IF (LST#Screen(WIdxLst).TipLista = TipScreen);
                LEAVE;
             ENDIF;
             WIdxLst += 1;
          ENDDO;

          IF (LST#Screen(WIdxLst).IsEmpty);
             WIdxLst = 1;
          ENDIF;

          //-------------------------------------------
          // Criar uma entrada no controlo de ecrãs
          //-------------------------------------------
          WIdxLvl = 1;
          DOW (NOT LVL#Screen(WIdxLvl).IsEmpty);
             WIdxLvl += 1;
          ENDDO;

          LVL#Screen(WIdxLvl).TipLista = TipScreen;
          LVL#Screen(WIdxLvl).IdxLst    = WIdxLst;
          LVL#Screen(WIdxLvl).CodSessao = LST#Screen(WIdxLst).CodSessao;
          LVL#Screen(WIdxLvl).IsEmpty   = *OFF;
          WIdxLvl += 1;
          LVL#Screen(WIdxLvl).IsEmpty = *ON;
       ENDIF;

       IF (WCodErro = *BLANKS);
          $SetDBSession(LST#Screen(WIdxLst).CodSessao);
          IF (WCriteria <> *BLANKS);
             $SetCriteria(WCriteria);
          ENDIF;
          ECR_DSCRITER = $getCritText();
       ENDIF;
       RETURN WCodErro;
      /END-FREE
     P ChgScreen       E

     P*==================================================================
     P* Processo..: StartScreen
     P* Descrição.: Iniciar as listagens de Ecrã
     P*==================================================================
     P StartScreen     B                   EXPORT
     D StartScreen     PI             7A
     D   Default                      1A   CONST
     D
     D*==> Campos de Trabalho
     D WMsgErro        S              7A
     D WIdxLst         S              3S 0
     D*=================================================================
      /FREE
       LVL#Screen(1).IsEmpty = *ON;
       WIdxLst = 1;
       //----------------------------------------------
       // Sessões
       //----------------------------------------------
       LST#Screen(WIdxLst).TipLista   = SES#Sessao;
       LST#Screen(WIdxLst).NoFunction = 'RINTE05403';
       LST#Screen(WIdxLst).IsEmpty    = *OFF;
       EXSR AddSessao;
       WIdxLst += 1;
       LST#Screen(WIdxLst).IsEmpty    = *ON;

       //----------------------------------------------
       // Mediadores
       //----------------------------------------------
       LST#Screen(WIdxLst).Tiplista   = SES#Mediador;
       LST#Screen(WIdxLst).NoFunction = 'RINTE05413';
       LST#SCreen(WIdxLst).IsEmpty    = *OFF;
       EXSR AddMediador;
       WIdxLst += 1;
       LST#Screen(WIdxLst).IsEmpty    = *ON;

       //----------------------------------------------
       // Temporarios
       //----------------------------------------------
       LST#Screen(WIdxLst).Tiplista   = SES#Tempor;
       LST#Screen(WIdxLst).NoFunction = 'RINTE05411';
       LST#Screen(WIdxLst).IsEmpty    = *OFF;
       EXSR AddTemporario;
       WidxLst += 1;
       LST#Screen(WIdxLst).IsEmpty    = *ON;

       //----------------------------------------------
       // Interfaces
       //----------------------------------------------
       LST#Screen(WIdxLst).Tiplista   = SES#Interface;
       LST#Screen(WIdxLst).NoFunction = 'RINTE05412';
       LST#Screen(WIdxLst).IsEmpty    = *OFF;
       EXSR AddInterface;
       WidxLst += 1;
       LST#Screen(WIdxLst).IsEmpty    = *ON;

       //----------------------------------------------
       // Registos
       //----------------------------------------------
       LST#Screen(WIdxLst).Tiplista   = SES#Registos;
       LST#Screen(WIdxLst).NoFunction = 'RINTE05414';
       LST#Screen(WIdxLst).IsEmpty    = *OFF;
       EXSR AddRegistos;
       WidxLst += 1;
       LST#Screen(WIdxLst).IsEmpty    = *ON;

       //----------------------------------------------
       // Posiciona-se no ecrã por defeito
       //----------------------------------------------
       ChgScreen(Default);
       RETURN *BLANKS;

       //---------------------------------------------------------------
       // Subrotina.: AddSessao
       // Descrição.: Adicionar uma Sessão de Interfaces
       //---------------------------------------------------------------
       BEGSR AddSessao;
          LST#Screen(WIdxLst).CodSessao  = $SetFiles('FINTP055');

          $addCriteria('Data.......:':'DTINICIO':'GE':'D');
          $addCriteria('Estado.....:':'CDESTADO':'LK');

          $SetLstFld('DTINICIO':'Inicio':10:'C':'D');
          $SetLstFld('DTTERMO' :'Termo':10:'C':'D');
          $SetLstFld('CDESTADO':'Estado':40:'E');
          $SetLstFld('DTULTPRC':'Ultimo':10:'C':'D');

          $SetPage(12);
       ENDSR;

       //---------------------------------------------------------------
       // Subrotina.: AddTemporario
       // Descrição.: Adicionar uma Sessão de Interfaces
       //---------------------------------------------------------------
       BEGSR AddTemporario;
          LST#Screen(WIdxLst).CodSessao  = $SetFiles('FINTP056 A +
                                      INNER JOIN FGRLP023 B ON (A.CDBATCH = +
                                      B.CDBATCH) AND (A.CDFILE = B.NUSEQU)');
          $SetFields('A.NUSEQU,A.DTPROCES,B.NOLIBL,B.NOFILE,B.CDFILE,+
                      #B.DTCRIA');

          $addCriteria('Data.......:':'A.DTPROCES':'GE':'D');
          $addCriteria('Nome.......:':'B.NOFILE':'LK');

          $SetLstFld('A.DTPROCES':'Data':10:'C':'D');
          $SetLstFld('B.NOLIBL'  :'Biblioteca':10:'C');
          $SetLstFld('B.NOFILE'  :'Ficheiro':10:'E');
          $SetLstFld('#B.DTCRIA' :'Criação':10:'C');

          $SetPage(12);
       ENDSR;

       //---------------------------------------------------------------
       // Subrotina.: AddInterface
       // Descrição.: Adicionar uma Sessão de Interfaces
       //---------------------------------------------------------------
       BEGSR AddInterface;
          LST#Screen(WIdxLst).CodSessao  = $SetFiles('FINTP050 A +
                            INNER JOIN FINTP058 B ON (A.CDMEDIA = B.CDMEDIA)');
          $SetFields('A.CDMEDIA,A.NOMMED,A.ALIAS,B.CDINTER,B.NMINTER,B.TPINTER,+
                               B.DTPRODUC,B.DTENVIO,B.CDSTATUS,B.QTDREGIS');

          $addCriteria('Data.......:':'B.DTPRODUC':'GE':'D');
          $addCriteria('Mediador...:':'A.NOMMED'  :'LK');
          $addCriteria('Estado.....:':'B.CDSTATUS':'LK');

          $SetLstFld('B.DTPRODUC':'Inicio':10:'C':'D');
          $SetLstFld('A.NOMMED'  :'Mediador':20:'E');
          $SetLstFld('B.CDSTATUS':'Est.':4:'C');
          $SetLstFld('B.TPINTER' :'Tipo':4:'C');
          $SetLstFld('B.DTENVIO' :'Envio':10:'C':'D');
          $SetLstFld('B.QTDREGIS':'Quant.':4:'D');
          $SetLstFld('B.NMINTER' :'Interface':17:'E');

          $SetPage(12);
       ENDSR;

       //---------------------------------------------------------------
       // Subrotina.: AddMediador
       // Descrição.: Adicionar uma Sessão de Mediador
       //---------------------------------------------------------------
       BEGSR AddMediador;
          LST#Screen(WIdxLst).CodSessao  = $SetFiles('FINTP050');

          $addCriteria('Código.....:':'ALIAS':'GE':'D');
          $addCriteria('Nome.......:':'NOMMED':'LK');

          $SetLstFld('ALIAS':'Código':7:'D');
          $SetLstFld('NOMMED' :'Nome':40:'E');
          $SetLstFld('ISAPOLIC':'Apol.':5:'C');
          $SetLstFld('ISSEGURA':'Seg.':5:'C');
          $SetLstFld('ISRECPND':'Rec.':5:'C');

          $SetPage(12);
       ENDSR;

       //---------------------------------------------------------------
       // Subrotina.: AddRegistos
       // Descrição.: Adicionar uma Sessão de registos
       //---------------------------------------------------------------
       BEGSR AddRegistos;
          LST#Screen(WIdxLst).CodSessao  = $SetFiles('FINTP059');

          $addCriteria('Sequenc....:':'NUSEQU':'GE':'A');
          $addCriteria('Chave......:':'KEYFLD':'LK');
          $addCriteria('Registo....:':'DETAIL':'LK');

          $SetLstFld('NUSEQU':'Sequ.':5:'D');
          $SetLstFld('KEYFLD':'Chave':15:'E');
          $SetLstFld('DETAIL':'Registo':53:'E');

          $SetPage(12);
       ENDSR;
      /END-FREE
     P StartScreen     E

     P*==================================================================
     P* Processo..: EndScreen
     P* Descrição.: Termianar a Listagem
     P*==================================================================
     P EndScreen       B                   EXPORT
     D EndScreen       PI             7A
     D
     D*==> Campos de Trabalho
     D WMsgErro        S              7A
     D WIdxLst         S              3S 0
     D*=================================================================
      /FREE
          WIdxLst = 1;
          DOW (NOT LST#Screen(WIdxLst).IsEmpty);
             IF (LST#Screen(WIdxLst).CodSessao > *ZEROS);
                $RmvDBSession(LST#Screen(WIdxLst).CodSessao);
                LST#Screen(WIdxLst).CodSessao = *ZEROS;
                LST#Screen(WIdxLst).IsEmpty = *ON;
             ENDIF;
             WIdxLst += 1;
          ENDDO;
          RETURN *BLANKS;
      /END-FREE
     P EndScreen       E

     P*==================================================================
     P* Processo..: MakeInterface
     P* Descrição.: Termianar a Listagem
     P*==================================================================
     P MakeInterface   B                   EXPORT
     D MakeInterface   PI             7A
     D    CodMediad                   9S 0 CONST
     D    CodInterf                   5S 0 CONST
     D    CodProces                   5S 0 CONST OPTIONS(*NOPASS)
     D
     D*==> Campos de Trabalho
     D WCodErro        S              7A
     D WEtapa          S              1S 0
     D WNomInter       S             25A
     D WCodMediad      S              9S 0
     D WCodInterf      S              5S 0
     D WCodProces      S              5S 0
     D WIsToAdd        S               N
     D
     D*==> Controlo de Registos repetidos
     D LST#Chave       DS                  QUALIFIED DIM(500)
     D   Chave                       20A
     D   IsEmpty                       N
     D WActReg         S              3S 0
     D
     D*==> Definição de Constantes
     D Reserv          C                   ';|.'
     D Transf          C                   ',;,'
     D*=================================================================
      /FREE
       LST#Chave(1).IsEmpty = *ON;
       WEtapa     = *ZEROS;
       WCodErro   = *BLANKS;
       IF (%PARMS() > 2);
          WCodProces = CodProces;
          SETLL (WCodProces) RINTP05801;
          READE (WCodProces) RINTP05801;
          DOW (NOT %EOF(FINTP05801));
             WCodMediad = INT_CDMEDIA;
             WCodInterf = INT_CDINTER;
             EXSR SendThisOne;
             IF (WCodErro <> *BLANKS);
                LEAVE;
             ENDIF;
             READE (WCodProces) RINTP05801;
          ENDDO;
       ELSE;
          WCodMediad = CodMediad;
          WCodInterf = CodInterf;
          EXSR SendThisOne;
       ENDIF;
       RETURN WCodErro;

       //===================================================
       // Subrotina.: SendThisOne
       // Objectivo.: enviar este Interface
       //===================================================
       BEGSR SendThisOne;
          CHAIN(N) (WCodMediad:WCodInterf) RINTP058;
          IF (%FOUND(FINTP058));
             IF (INT_CDSTATUS = STS#Aberto);
             // WCodErro = 'INTXXX1';
             // LEAVESR;
             ENDIF;
          ELSE;
             WCodErro = 'INTXXX2';
             LEAVESR;
          ENDIF;

          WFileName = 'INT' + INT_TPINTER +
                      %CHAR($GetCodMedia(WCodMediad));
          IF (INT_CDSTATUS <> STS#enviado);
             WNomInter = %TRIM(WFileName)+'_'+
                         %CHAR(%SUBDT(%DATE():*YEARS))+
                         %CHAR(%SUBDT(%DATE():*MONTHS))+
                         %CHAR(%SUBDT(%DATE():*DAYS))+'_'+
                         %CHAR(%SUBDT(%TIMESTAMP():*SECONDS))+
                         '.csv';
          ELSE;
             WNomInter = INT_NMINTER;
          ENDIF;

          SETLL (WCodMediad:WCodInterf) RINTP059;
          READE(N) (WCodMediad:WCodInterf) RINTP059;
          DOW (NOT %EOF(FINTP059));
             IF (WEtapa = *ZEROS);
                EXSR CrtTemporario;
                IF (WCodErro <> *BLANKS);
                   LEAVE;
                ENDIF;
             ENDIF;

             //---------------------------------------------------------
             // Validar se o registo está duplicado
             //---------------------------------------------------------
             WActReg = 1;
             WIsToAdd = *ON;
             DOW (NOT LST#Chave(WActReg).IsEmpty);
                IF (%TRIM(LST#Chave(WActReg).Chave) = %TRIM(INT_KEYFLD));
                   WIsToAdd = *OFF;
                   LEAVE;
                ENDIF;
                WActReg += 1;
             ENDDO;

             //---------------------------------------------------------
             // Validar se o registo está duplicado
             //---------------------------------------------------------
             IF (WIsToAdd);
                LST#Chave(WActReg).Chave = %TRIM(INT_KEYFLD);
                LST#Chave(WActReg).IsEmpty = *OFF;
                R#TEMP.TEXTO = %XLATE(Reserv:Transf:INT_DETAIL);
                WRITE TMPFILE R#TEMP;
                WActReg += 1;
                LST#Chave(WActReg).IsEmpty = *ON;
             ENDIF;
             READE(N) (WCodMediad:WCodInterf) RINTP059;
          ENDDO;

          IF (WEtapa > *ZEROS);
             IF (INT_CDSTATUS = STS#Aberto);
                CHAIN (WCodMediad:WCodInterf) RINTP058;
                INT_CDSTATUS = STS#Enviado;
                INT_NMINTER  = WNomInter;
                UPDATE RINTP058 %FIELDS(INT_CDSTATUS:INT_NMINTER);
             ENDIF;
             EXSR EnvTemporario;
             CLOSE TMPFILE;
          ENDIF;
       ENDSR;

       //===================================================
       // Subrotina.: CrtTemporario
       // Objectivo.: Criar na QTEMP o ficheiro Temporario
       //===================================================
       BEGSR CrtTemporario;
          WCodErro = $CrtFile(WFileName:400);
          IF (WCodErro = *BLANKS);
             OPEN TMPFILE;
             WEtapa += 1;
          ENDIF;
       ENDSR;

       //===================================================
       // Subrotina.: EnvTemporario
       // Objectivo.: enviar o ficheiro Temporário P/Linux
       //===================================================
       BEGSR EnvTemporario;
          MONITOR;
             WStdCMD = 'SNDPF OUTFILE(QTEMP/'+%TRIM(WFileName)+
                       ') DESTINO('''+%TRIM(WNomInter)+''')';
             WStdLen = %LEN(%TRIM(WStdCMD));
             shell(WStdCMD:WStdLen);
          ON-ERROR;
          ENDMON;
       ENDSR;
      /END-FREE
     P MakeInterface   E
