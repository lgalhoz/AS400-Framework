     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PTBLE002
     H* Descrição.: Manutenção de Tabelas e Códigos
     H*==================================================================
     FFTBLP013  UF A E           K DISK    PREFIX(TBL_)
     FFTBLE002  CF   E             WORKSTN SFILE(RTBLE00202:REC#)
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

     D*==> Estrutura de Tabelas ou Códigos
     D MntLocais       PR
     D   PCdSuper                     9S 0 CONST
     D   PCdTipo                      1A   CONST
     D   PDsSuper                    40A   CONST
     D
     D*==> Estrutura de dados do Registo
     D WIcErro         S               N   INZ(*OFF)
     D WCdMessage      S              7A   INZ(*BLANKS)
     D WIcOrigem       S              1A
     D REC#            S              4S 0
     C*========================================================
      /FREE

       //------------------------------------
       // Se não estiver autorizado então sai
       //------------------------------------
       IF NOT $ValAccess(S#PGM:'':99999);
          *INLR = *ON;
          RETURN;
       ENDIF;

       MntLocais (*ZEROS:*BLANKS:*BLANKS);

       *INLR = *ON;
       RETURN;
      /END-FREE

     P*===================================================================
     P* Processo..: MntLocais
     P* Descrição.: Realizar a manutenção dos Concelhos, Distritos, ...
     P*===================================================================
     P MntLocais       B                   EXPORT
     D MntLocais       PI
     D   PCdSuper                     9S 0 CONST
     D   PCdTipo                      1A   CONST
     D   PDsSuper                    40A   CONST
     D
     D WSession        S              5S 0 INZ(*ZEROS)
     D WIcDisplay      S               N   INZ(*ON)
     D WIcInsert       S               N   INZ(*ON)
     D WNoFunction     S             10A   INZ(*BLANKS)
     D WIsEmpty        S               N
     D WSelected       S               N
     D
     D*===================================================================
      /FREE

       WIcDisplay =*ON;
       WIcInsert  =*OFF;

       //----------------------------------------------
       // Sessão das Funções
       //----------------------------------------------
       WSession = $AddDBSession();
       $SetFiles('FTBLP013');
       $SetFields('CDLOCAL,CDSUPER,DSLOCAL,ALIAS,NUNIVEL,CDTIPO');
       $addCriteria('Descrição..:':'DSLOCAL':'LK':*ON);
       $addCriteria('Código.....:':'CDLOCAL':'GE');
       ECR_DSCRITER = $getCritText();
       $SetLstFld('ALIAS':'Código':8:'D');
       $SetLstFld('DSLOCAL':'Descrição.':50:'E');
       $SetLstFld('CDTIPO':'Tipo':1:'C');
       IF (PCdSuper <> *ZEROS);
          $SetCriteria('(CDSUPER = ''' + %CHAR(PCdSuper) + ''')');
       ELSEIF (PcdTipo <> *BLANKS);
          $SetCriteria('(CDTIPO = ''' + PCdTipo + ''')');
       ELSE;
          $SetCriteria('(CDTIPO = ''' + 'D' + ''')');
       ENDIF;
       $SetPage(12);

       DOW (NOT *IN03 AND NOT *IN12);
          EXSR inzRecords;
          EXSR getRecords;
          WNoFunction = 'RTBLE00203'; // Listar Locais
          DS#HEADER   = $getHeader(S#PGM:WNoFunction);
          DS#FOOTER   = $getFooter(S#PGM:WNoFunction:*BLANKS);
          DS#OPTIONS   = $getOption(S#PGM:WNoFunction:*BLANKS);
          ECR_S#PGM   = S#PGM;
          ECR_ERR#MSG = *BLANKS;
          WRITE     RTBLE00200;
          WRITE     RTBLE00299;
          WRITE     RTBLE00298;
          EXFMT RTBLE00203;
          IF (Func#Key = KEY#F05);
             $ChgCriteria();
             ECR_DSCRITER = $getCritText();
          ELSEIF ($ValidKey(Func#Key:KEY#F06));
             WIcDisplay = *OFF;
             WIcInsert  = *ON;
             EXSR DspLocal;
             *IN12 = *OFF;
             WIcDisplay = *ON;
             WIcInsert  = *OFF;
          ELSEIF ((Func#Key = KEY#PgDown) AND (NOT $IsLastPage())) OR
             ((Func#Key = KEY#PgUp) AND ($GetPageNbr() > 1));
          ELSEIF (Func#Key = KEY#Enter);
             EXSR SelRec;
             *IN12 = *OFF;
          ENDIF;
       ENDDO;
       $RmvDBSession(WSession);

       //========================================================
       // Subrotina.: InzRecTxt
       // Objectivo.: Inicializar a lista de registos
       //========================================================
       BEGSR inzRecords;
          REC#      = *ZEROS;   // Controlo de Registos da Lista
          *In53     = *On;      // Limpar Lista de Ecrã
          *In52     = *On;      // Mostrar a Lista de Ecrã
          WRITE RTBLE00203;     // Limpar Lista de Funções
          *In53     = *Off;     // Repor Indicador
          *In52     = *Off;     // Mostrar a Lista de Ecrã
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

                //---------------------------------------------
                // Formatar as Chaves das Tabelas
                //---------------------------------------------
                MONITOR;
                   ECR_CDLOCAL = %DEC($GetField('CDLOCAL'):9:0);
                ON-ERROR;
                ENDMON;

                REC# += 1;
                WRITE RTBLE00202;
                *IN52 = *ON;       // Encontrou registos
                $getNext();
             ENDDO;
          ENDIF;
          *IN54 = $IsLastPage();   // Indicação de Fim de Página
       ENDSR;

       //========================================================
       // Subrotina.: SelRec
       // Objectivo.: Seleccionar os registos
       //========================================================
       BEGSR SelRec;
          WSelected = *OFF;
          READC RTBLE00202;
          DOW (NOT %EOF());
             IF ($ValidOption(ECR_OPTION:'5'));
                *IN45 = *OFF;
                WIcDisplay = *ON;
                EXSR DspLocal;
                LEAVESR;
             ELSEIF ($ValidOption(ECR_OPTION:'7'));
                CHAIN(N) (ECR_CDLOCAL) RTBLP013;
                IF (%FOUND());
                   MntLocais (TBL_CDLOCAL:TBL_CDTIPO:TBL_DSLOCAL);
                   $SetDBSession(WSession);
                ENDIF;
                LEAVESR;
             ENDIF;
             READC RTBLE00202;
          ENDDO;
          IF (NOT WSelected AND WIcDisplay);
             IF (ECR_CRITERIA <> *BLANKS);
                $SetCriteria(ECR_CRITERIA:*ON);
             ELSE;
                $SetCriteria(*BLANKS:*ON);
             ENDIF;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: DspLocal
       // Objectivo.: Apresentar os valores da Tabela seleccionada
       //========================================================
       BEGSR DspLocal;
          ECR_CDSUPER = PCdSuper;
          ECR_DSSUPER = PDsSuper;
          IF (NOT WIcInsert);
             CHAIN(N) (ECR_CDLOCAL) RTBLP013;
             IF (%FOUND());
                ECR_CDLOCAL = TBL_CDLOCAL;
                ECR_DSLOCAL = TBL_DSLOCAL;
                ECR_ALIAS   = TBL_ALIAS;
                ECR_NUNIVEL = TBL_NUNIVEL;
                ECR_CDTIPO  = TBL_CDTIPO;
                ECR_ICRESERV= TBL_ICRESERV;
             ELSE;
                LEAVESR;
             ENDIF;
          ELSE;
             *IN45 = *ON;
             ECR_CDLOCAL = *ZEROS;
             ECR_DSLOCAL = *BLANKS;
             ECR_ALIAS   = *BLANKS;
             ECR_NUNIVEL = *ZEROS;
             ECR_CDTIPO  = *BLANKS;
             ECR_ICRESERV= 'N';
          ENDIF;
          DOW NOT (*IN03 OR *IN12);
             IF ( WIcInsert);
                WNoFunction = 'RTBLE00206'; // Adicionar um registo
             ELSEIF (WIcDisplay);
                WNoFunction = 'RTBLE00205'; // Mostrar o registo
             ELSE;
                WNoFunction = 'RTBLE00221'; // Alterar o Registo
             ENDIF;
             DS#HEADER   = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER   = $getFooter(S#PGM:WNoFunction:*BLANKS);
             ECR_S#PGM   = S#PGM;
             WRITE     RTBLE00200;
             WRITE     RTBLE00299;
             EXFMT RTBLE00205;
             IF (Func#Key=KEY#F04) AND (NOT WIcDisplay);
                EXSR RunPopUp;
             ELSEIF (Func#Key=KEY#F08) AND (WIcDisplay);
                MntLocais (ECR_CDLOCAL:ECR_CDTIPO:ECR_DSLOCAL);
                $SetDBSession(WSession);
                *IN12 = *OFF;
                *IN45 = *OFF;
                WIcDisplay = *ON;
             ELSEIF (Func#Key=KEY#F12) AND (NOT WIcDisplay)
                AND (Not WIcInsert);
                WIcDisplay = *ON;
                *IN45 = *OFF;
                *IN21 = *OFF;
                *IN12 = *OFF;
             ELSEIF ($ValidKey(Func#Key:KEY#F21)) AND (WIcDisplay)
                AND (ECR_ICRESERV = 'N');
                WIcDisplay = *OFF;
                *IN45 = *ON;
             ELSEIF ($ValidKey(Func#Key:KEY#F22)) AND (WIcDisplay);
                IF (ECR_ICRESERV = 'N');
                   ECR_OPTION = 'N';
                   EXFMT RTBLE00222;
                   IF (Func#Key = KEY#Enter) AND (%TRIM(ECR_OPTION) = 'S');
                      DELETE (ECR_CDLOCAL) RTBLP013;
                      LEAVESR;
                   ENDIF;
                ENDIF;
                *IN12 = *OFF;
             ELSEIF (Func#Key = KEY#ENTER);
                IF (NOT WIcDisplay) OR (WIcInsert);
                   WIcErro = *OFF;
                   WCdMessage = *BLANKS;
                   ECR_ERR#MSG = *BLANKS;
                   EXSR ValLocal;
                   IF (WIcErro);
                      ECR_ERR#MSG = $GetMessage(WCdMessage);
                      ITER;
                   ENDIF;
                   EXSR MntLocal;
                   WIcDisplay = *ON;
                   WIcInsert = *OFF;
                   *IN45 = *OFF;
                ELSE;
                   LEAVESR;
                ENDIF;
             ENDIF;
          ENDDO;
          *IN12 = *OFF;
       ENDSR;

       //========================================================
       // Subrotina.: ValLocal
       // Objectivo.: Apresentar os valores da Tabela seleccionada
       //========================================================
       BEGSR ValLocal;
          IF (ECR_DSLOCAL = *BLANKS);
             WCdMessage = 'TBL0001';
             WIcErro = *ON;
|            ECR_CM#COL = CM#CCOL;
             $getFldLoc('FTBLE002':'RTBLE00205':'DSLOCAL':
                        ECR_CM#ROW:ECR_CM#COL);
          ENDIF;
       ENDSR;

       //==================================================================
       // Subrotina.: RunPopUp
       // Objectivo.: Adicionar um novo registo de uma Apólice
       //==================================================================
+----  BEGSR RunPopUp;
|+---     IF (ECR_CM#NMFLD = 'CDTIPO');
             ECR_CDTIPO = $runPopUp(4:ECR_CDTIPO);
          ENDIF;
|         ECR_CM#ROW = CM#CROW;
|         ECR_CM#COL = CM#CCOL;
+----  ENDSR;

       //========================================================
       // Subrotina.: MntLocal
       // Objectivo.: Alterar, Criar ou eliminar uma Tabela
       //========================================================
       BEGSR MntLocal;
          //-------------------------------------------
          // Inserir um registo
          //-------------------------------------------
          IF (ECR_CDLOCAL = *ZEROS);
             TBL_CDLOCAL  = $getNextKey('FTBLP013':'CDLOCAL');
             ECR_CDLOCAL  = TBL_CDLOCAL;
             TBL_CDSUPER  = ECR_CDSUPER;
             TBL_DSLOCAL  = ECR_DSLOCAL;
             TBL_ALIAS    = ECR_ALIAS;
             TBL_NUNIVEL  = ECR_NUNIVEL;
             TBL_CDTIPO   = ECR_CDTIPO;
             TBL_ICRESERV = ECR_ICRESERV;
             WRITE RTBLP013;

          //-------------------------------------------
          // Alterar um registo
          //-------------------------------------------
          ELSE;
             CHAIN (ECR_CDLOCAL) RTBLP013;
             TBL_DSLOCAL  = ECR_DSLOCAL;
             TBL_ALIAS    = ECR_ALIAS;
             TBL_NUNIVEL  = ECR_NUNIVEL;
             TBL_CDTIPO   = ECR_CDTIPO;
             UPDATE RTBLP013 %FIELDS(TBL_DSLOCAL:TBL_ALIAS:TBL_NUNIVEL:
                                     TBL_CDTIPO);
          ENDIF;
       ENDSR;
      /END-FREE
     PMntLocais        E
