     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PGRLE012
     H* Descrição.: Manutenção de Tabelas e Códigos
     H*==================================================================
     FFGRLP008  UF A E           K DISK    PREFIX(GRL_)
     FFGRLP010  UF A E           K DISK    PREFIX(GRL_)
     FFGRLE012  CF   E             WORKSTN SFILE(RGRLE01202:REC#)
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
     D MntPaginas      PR
     D   PCdPagina                    5S 0 CONST
     D   PDsPagina                   40A   CONST
     D
     D*==> Estrutura de dados do Registo
     D WIcErro         S               N   INZ(*OFF)
     D WCdMessage      S              7A   INZ(*BLANKS)
     D WIcOrigem       S              1A
     D REC#            S              4S 0
     D
     D KTE#Pagina      S              5S 0
     D KTE#Paragr      S              5S 0
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
       KTE#Pagina = $AddDBSession();
       $SetFiles('FGRLP008');
       $SetFields('CDPAGINA,DSPAGINA,NMPAGINA,TEMPLATE,TXTSIZE');
       $addCriteria('Nome.......:':'NMPAGINA':'GE':*ON);
       $addCriteria('Descrição..:':'DSPAGINA':'LK');
       $addCriteria('Código.....:':'CDPAGINA':'GE');
       ECR_DSCRITER = $getCritText();
       $SetLstFld('NMPAGINA':'Nome':10:'E');
       $SetLstFld('DSPAGINA':'Descrição':40:'E');
       $SetLstFld('TXTSIZE':'Largura':10:'D');
       $SetPage(12);

       //----------------------------------------------
       // Sessão das Operações
       //----------------------------------------------
       KTE#Paragr   = $AddDBSession();
       $SETFILES('FGRLP010');
       $SETFIELDS('CDPAGINA,CDTIPO,DSTIPO,ISEXCEPT,NMFONTE');
       $addCriteria('Descrição..:':'DSTIPO':'LK':*ON);
       $addCriteria('Código.....:':'CODIGO':'GE');
       ECR_DSCRITER = $getCritText();
       $SetLstFld('CDTIPO':'Código':10:'E');
       $SetLstFld('DSTIPO':'Descrição':40:'E');
       $SetLstFld('ISEXCEPT':'Excepção':10:'C');
       $SetPage(12);

       $SetDBSession(KTE#Pagina); // Repor Sessão das Funções

       MntPaginas(*ZEROS:*BLANKS);
       $RmvDBSession(KTE#Pagina);
       $RmvDBSession(KTE#Paragr);
       *INLR = *ON;
       RETURN;
      /END-FREE

     P*===================================================================
     P* Processo..: MntPaginas
     P* Descrição.: Realizar a manutenção dos Tabelas de Código e Códigos
     P*===================================================================
     P MntPaginas      B                   EXPORT
     D MntPaginas      PI
     D   PCdPagina                    5S 0 CONST
     D   PDsPagina                   40A   CONST
     D
     D WIcDisplay      S               N   INZ(*ON)
     D WIcInsert       S               N   INZ(*ON)
     D WNoFunction     S             10A   INZ(*BLANKS)
     D WIsEmpty        S               N
     D WSelected       S               N
     D
     D*===================================================================
      /FREE

       WIcDisplay   = *ON;
       WIcInsert    = *OFF;
       ECR_CDPAGINA = PCdPagina;
       ECR_DSPAGINA = PDsPagina;

       DOW (NOT *IN03 AND NOT *IN12);
          EXSR inzRecords;
          EXSR getRecords;
          IF ($GetDBSession() = KTE#Pagina);
             WNoFunction = 'RGRLE01203'; // Listar Componentes
          ELSE;
             WNoFunction = 'RGRLE01204'; // Listar Operações
          ENDIF;
          DS#HEADER   = $getHeader(S#PGM:WNoFunction);
          DS#FOOTER   = $getFooter(S#PGM:WNoFunction:*BLANKS);
          DS#OPTIONS   = $getOption(S#PGM:WNoFunction:*BLANKS);
          ECR_S#PGM   = S#PGM;
          ECR_ERR#MSG = *BLANKS;
          WRITE     RGRLE01200;
          WRITE     RGRLE01299;
          WRITE     RGRLE01298;
          EXFMT RGRLE01203;
          IF (Func#Key = KEY#F05);
             $ChgCriteria();
             ECR_DSCRITER = $getCritText();
          ELSEIF ($ValidKey(Func#Key:KEY#F06));
             WIcDisplay = *OFF;
             WIcInsert  = *ON;
             IF ($GetDBSession()=KTE#Paragr);
                EXSR DspParagrafo;
             ELSE;
                EXSR DspPagina;
             ENDIF;
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

       //========================================================
       // Subrotina.: InzRecTxt
       // Objectivo.: Inicializar a lista de registos
       //========================================================
       BEGSR inzRecords;
          REC#      = *ZEROS;   // Controlo de Registos da Lista
          *In53     = *On;      // Limpar Lista de Ecrã
          *In52     = *On;      // Mostrar a Lista de Ecrã
          WRITE RGRLE01203;     // Limpar Lista de Funções
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
                   ECR_CDPAGINA = %DEC($GetField('CDPAGINA'):5:0);
                   IF ($GetDBSession() = KTE#Pagina);
                      ECR_DSPAGINA = $GetField('DSPAGINA');
                      ECR_CDTIPO   = *BLANKS;
                   ELSE;
                      ECR_CDTIPO   = $GetField('CDTIPO');
                   ENDIF;
                ON-ERROR;
                ENDMON;

                REC# += 1;
                WRITE RGRLE01202;
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
          READC RGRLE01202;
          DOW (NOT %EOF());
             IF ($ValidOption(ECR_OPTION:'5'));
                *IN45 = *OFF;
                WIcDisplay = *ON;
                IF ($GetDBSession=KTE#Pagina);
                   EXSR DspPagina;
                ELSE;
                   EXSR DspParagrafo;
                ENDIF;
                LEAVESR;
             ELSEIF ($ValidOption(ECR_OPTION:'7'));
                CHAIN(N) (ECR_CDPAGINA) RGRLP008;
                IF (%FOUND());
                   $SetDBSession(KTE#Paragr);
                   $SetCriteria('(CDPAGINA = ''' +
                                  %CHAR(ECR_CDPAGINA) + ''')');
                   MntPaginas(ECR_CDPAGINA:ECR_DSPAGINA);
                   $SetDBSession(KTE#Pagina);
                ENDIF;
                LEAVESR;
             ENDIF;
             READC RGRLE01202;
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
       // Subrotina.: DspPagina
       // Objectivo.: Apresentar os valores da Tabela seleccionada
       //========================================================
       BEGSR DspPagina;
          IF (NOT WIcInsert);
             CHAIN(N) (ECR_CDPAGINA) RGRLP008;
             IF (%FOUND());
                ECR_CDPAGINA = GRL_CDPAGINA;
                ECR_DSPAGINA = GRL_DSPAGINA;
                ECR_NMPAGINA = GRL_NMPAGINA;
                ECR_TEMPLATE = GRL_TEMPLATE;
                ECR_TXTSIZE  = GRL_TXTSIZE;
                ECR_GRL00801 = GRL_GRL00801;
                ECR_GRL00802 = GRL_GRL00802;
             ELSE;
                LEAVESR;
             ENDIF;
          ELSE;
             *IN45 = *ON;
             ECR_CDPAGINA = *ZEROS;
             ECR_DSPAGINA = *BLANKS;
             ECR_NMPAGINA = *BLANKS;
             ECR_TEMPLATE = *ZEROS;
             ECR_TXTSIZE  = *ZEROS;
             ECR_GRL00801 = %DATE();
             ECR_GRL00802 = %DATE();
          ENDIF;
          DOW NOT (*IN03 OR *IN12);
             IF ( WIcInsert);
                WNoFunction = 'RGRLE01206'; // Adicionar um registo
             ELSEIF (WIcDisplay);
                WNoFunction = 'RGRLE01205'; // Mostrar o registo
             ELSE;
                WNoFunction = 'RGRLE01221'; // Alterar o Registo
             ENDIF;
             DS#HEADER   = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER   = $getFooter(S#PGM:WNoFunction:*BLANKS);
             ECR_S#PGM   = S#PGM;
             WRITE     RGRLE01200;
             WRITE     RGRLE01299;
             EXFMT RGRLE01205;
             IF (Func#Key=KEY#F04) AND (NOT WIcDisplay);
                EXSR RunPopUp;
             ELSEIF (Func#Key=KEY#F08) AND (WIcDisplay);
                $SetDBSession(KTE#Paragr);
                $SetCriteria('(CDPAGINA = ''' +
                               %CHAR(ECR_CDPAGINA) + ''')');
                MntPaginas(ECR_CDPAGINA:ECR_DSPAGINA);
                $SetDBSession(KTE#Pagina);
                *IN12 = *OFF;
                *IN45 = *OFF;
                WIcDisplay = *ON;
             ELSEIF (Func#Key=KEY#F12) AND (NOT WIcDisplay)
                AND (Not WIcInsert);
                WIcDisplay = *ON;
                *IN45 = *OFF;
                *IN21 = *OFF;
                *IN12 = *OFF;
             ELSEIF ($ValidKey(Func#Key:KEY#F21)) AND (WIcDisplay);
                WIcDisplay = *OFF;
                *IN45 = *ON;
             ELSEIF ($ValidKey(Func#Key:KEY#F22)) AND (WIcDisplay);
                ECR_OPTION = 'N';
                EXFMT RGRLE01222;
                IF (Func#Key = KEY#Enter) AND (%TRIM(ECR_OPTION) = 'S');
                   DELETE (ECR_CDPAGINA) RGRLP008;
                   LEAVESR;
                ENDIF;
                *IN12 = *OFF;
             ELSEIF (Func#Key = KEY#ENTER);
                IF (NOT WIcDisplay) OR (WIcInsert);
                   WIcErro = *OFF;
                   WCdMessage = *BLANKS;
                   ECR_ERR#MSG = *BLANKS;
                   EXSR ValPagina;
                   IF (WIcErro);
                      ECR_ERR#MSG = $GetMessage(WCdMessage);
                      ITER;
                   ENDIF;
                   EXSR MntPagina;
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
       // Subrotina.: ValPagina
       // Objectivo.: Apresentar os valores da Pagina seleccionada
       //========================================================
       BEGSR ValPagina;
          IF (ECR_DSPAGINA = *BLANKS);
             WCdMessage = 'TBL0001';
             WIcErro = *ON;
|            ECR_CM#COL = CM#CCOL;
             $getFldLoc('FGRLE012':'RGRLE01205':'DSPAGINA':
                        ECR_CM#ROW:ECR_CM#COL);
          ELSEIF (ECR_NMPAGINA = *BLANKS);
             WCdMessage = 'TBL0002';
             WIcErro = *ON;
             $getFldLoc('FGRLE012':'RGRLE01205':'CDMODULO':
                        ECR_CM#ROW:ECR_CM#COL);
          ENDIF;
       ENDSR;

       //==================================================================
       // Subrotina.: RunPopUp
       // Objectivo.: Adicionar um novo registo de uma Apólice
       //==================================================================
+----  BEGSR RunPopUp;
|+---     IF (ECR_CM#NMFLD = 'TEMPLATE');
             ECR_TEMPLATE = %DEC($runPopUp(4:%CHAR(ECR_TEMPLATE)):5:0);
|+---     ELSEIF (ECR_CM#NMFLD = 'DFTBULL');
             ECR_DFTBULL = $runPopUp(3:ECR_DFTBULL);
          ENDIF;
|         ECR_CM#ROW = CM#CROW;
|         ECR_CM#COL = CM#CCOL;
+----  ENDSR;

       //========================================================
       // Subrotina.: MntPagina
       // Objectivo.: Alterar, Criar ou eliminar uma Tabela
       //========================================================
       BEGSR MntPagina;
          //-------------------------------------------
          // Inserir um registo
          //-------------------------------------------
          IF (ECR_CDPAGINA = *ZEROS);
             GRL_CDPAGINA = $getNextKey('FGRLP008':'CDPAGINA');
             ECR_CDPAGINA = GRL_CDPAGINA;
             GRL_DSPAGINA = ECR_DSPAGINA;
             GRL_NMPAGINA = ECR_NMPAGINA;
             GRL_TEMPLATE = ECR_TEMPLATE;
             GRL_TXTSIZE  = ECR_TXTSIZE;
             GRL_GRL00801 = ECR_GRL00801;
             GRL_GRL00802 = ECR_GRL00802;
             WRITE RGRLP008;

          //-------------------------------------------
          // Alterar um registo
          //-------------------------------------------
          ELSE;
             CHAIN (ECR_CDPAGINA) RGRLP008;
             ECR_CDPAGINA = GRL_CDPAGINA;
             GRL_DSPAGINA = ECR_DSPAGINA;
             GRL_NMPAGINA = ECR_NMPAGINA;
             GRL_TEMPLATE = ECR_TEMPLATE;
             GRL_TXTSIZE  = ECR_TXTSIZE;
             GRL_GRL00802 = %DATE();
             UPDATE RGRLP008 %FIELDS(GRL_DSPAGINA:GRL_NMPAGINA:GRL_TEMPLATE:
                                     GRL_TXTSIZE:GRL_GRL00802);
          ENDIF;
       ENDSR;
       //========================================================
       // Subrotina.: DspParagrafo
       // Objectivo.: Apresentar os valores do Código
       //========================================================
       BEGSR DspParagrafo;
          *IN48 = *OFF;
          IF (NOT WIcDisplay);
          ENDIF;
          IF (NOT WIcInsert);
             CHAIN(N) (ECR_CDPAGINA:ECR_CDTIPO) RGRLP010;
             IF (%FOUND());
                ECR_CDPAGINA   = GRL_CDPAGINA;
                ECR_CDTIPO     = GRL_CDTIPO;
                ECR_DSTIPO     = GRL_DSTIPO;
                ECR_ISEXCEPT   = GRL_ISEXCEPT;
                ECR_NMFONTE    = GRL_NMFONTE;
                ECR_DIMFONTE   = GRL_DIMFONTE;
                ECR_ISBOLD     = GRL_ISBOLD;
                ECR_ISUNDERLIN = GRL_ISUNDERLIN;
                ECR_ISUPPER    = GRL_ISUPPER;
                ECR_SPCANTES   = GRL_SPCANTES;
                ECR_SPCDEPOIS  = GRL_SPCDEPOIS;
                ECR_DFTBULL    = GRL_DFTBULL;
                ECR_QTDEIDENT  = GRL_QTDEIDENT;
                ECR_TXTALIGN   = GRL_TXTALIGN;
             ENDIF;
          ELSE;
             *IN45 = *ON;
             *IN48 = *ON;
             ECR_CDPAGINA   = ECR_CDPAGINA;
             ECR_CDTIPO     = *BLANKS;
             ECR_DSTIPO     = *BLANKS;
             ECR_ISEXCEPT   = 'N';
             ECR_NMFONTE    = *BLANKS;
             ECR_DIMFONTE   = *ZEROS;
             ECR_ISBOLD     = 'N';
             ECR_ISUNDERLIN = 'N';
             ECR_ISUPPER    = 'N';
             ECR_SPCANTES   = *ZEROS;
             ECR_SPCDEPOIS  = *ZEROS;
             ECR_DFTBULL    = *BLANKS;
             ECR_QTDEIDENT  = *ZEROS;
             ECR_TXTALIGN   = 'E';
          ENDIF;
          DOW NOT (*IN03 OR *IN12);
             IF (WIcInsert);
                WNoFunction = 'RGRLE01209'; // Adicionar um Código
             ELSEIF (WIcDisplay);
                WNoFunction = 'RGRLE01207'; // Consultar um Código
             ELSE;
                WNoFunction = 'RGRLE01208'; // Alterar um Código
             ENDIF;
             DS#HEADER   = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER   = $getFooter(S#PGM:WNoFunction:*BLANKS);
             ECR_S#PGM   = S#PGM;
             WRITE     RGRLE01200;
             WRITE     RGRLE01299;
             EXFMT RGRLE01207;
             IF (Func#Key=KEY#F21) AND (WIcDisplay);
                *IN45 = *ON;
                WIcDisplay = *OFF;
             ELSEIF (Func#Key=KEY#F12) AND (NOT WIcDisplay) AND
                (NOT WIcInsert);
                *IN45 = *OFF;
                *IN12 = *OFF;
                WIcDisplay = *ON;
             ELSEIF (Func#Key=KEY#F22) AND (WIcDisplay);
                ECR_OPTION = 'N';
                EXFMT RGRLE01222;
                IF (Func#Key = KEY#Enter) AND (%TRIM(ECR_OPTION) = 'S');
                   DELETE (ECR_CDPAGINA:ECR_CDTIPO) RGRLP010;
                   LEAVESR;
                ENDIF;
                *IN12 = *OFF;
             ELSEIF (Func#Key=KEY#ENTER);
                IF (NOT WIcDisplay) OR (WIcInsert);
                   WIcErro = *OFF;
                   ECR_ERR#MSG = *BLANKS;
                   EXSR ValParagrafo;
                   IF (WIcErro);
                      ECR_ERR#MSG = $GetMessage(WCdMessage);
                      ITER;
                   ENDIF;
                   EXSR MntParagrafo;
                   *IN48 = *OFF;
                   *IN45 = *OFF;
                   *IN12 = *OFF;
                   WIcDisplay = *ON;
                   WIcInsert = *OFF;
                ELSE;
                   *IN12 = *ON;
                ENDIF;
             ENDIF;
          ENDDO;
          *IN12 = *OFF;
       ENDSR;

       //========================================================
       // Subrotina.: ValParagrafo
       // Objectivo.: Validar os campos da frame de códigos
       //========================================================
       BEGSR ValParagrafo;
          IF (ECR_CDTIPO = *BLANKS);
             WCdMessage = 'TBL0007';
             WIcErro = *ON;
|            ECR_CM#COL = CM#CCOL;
             $getFldLoc('FGRLE012':'RGRLE01207':'CDTIPO':
                        ECR_CM#ROW:ECR_CM#COL);
          ELSEIF (ECR_DSTIPO = *BLANKS);
             WCdMessage = 'TBL0008';
             WIcErro = *ON;
             $getFldLoc('FGRLE012':'RGRLE01207':'DSTIPO':
                        ECR_CM#ROW:ECR_CM#COL);
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: MntParagrafo
       // Objectivo.: Alterar, Criar ou eliminar um Código
       //========================================================
       BEGSR MntParagrafo;
          //-------------------------------------------
          // Inserir um registo
          //-------------------------------------------
          IF (WIcInsert);
             GRL_CDPAGINA   = ECR_CDPAGINA;
             GRL_CDTIPO     = ECR_CDTIPO;
             ECR_CDTIPO     = GRL_CDTIPO;
             GRL_DSTIPO     = ECR_DSTIPO;
             GRL_ISEXCEPT   = ECR_ISEXCEPT;
             GRL_NMFONTE    = ECR_NMFONTE;
             GRL_DIMFONTE   = ECR_DIMFONTE;
             GRL_ISBOLD     = ECR_ISBOLD;
             GRL_ISUNDERLIN = ECR_ISUNDERLIN;
             GRL_ISUPPER    = ECR_ISUPPER;
             GRL_SPCANTES   = ECR_SPCANTES;
             GRL_SPCDEPOIS  = ECR_SPCDEPOIS;
             GRL_DFTBULL    = ECR_DFTBULL;
             GRL_QTDEIDENT  = ECR_QTDEIDENT;
             GRL_TXTALIGN   = ECR_TXTALIGN;
             GRL_GRL01001   = %DATE();
             GRL_GRL01002   = %DATE();
             WRITE RGRLP010;

          //-------------------------------------------
          // Alterar um registo
          //-------------------------------------------
          ELSE;
             CHAIN (ECR_CDPAGINA:ECR_CDTIPO) RGRLP010;
             GRL_DSTIPO     = ECR_DSTIPO;
             GRL_ISEXCEPT   = ECR_ISEXCEPT;
             GRL_NMFONTE    = ECR_NMFONTE;
             GRL_DIMFONTE   = ECR_DIMFONTE;
             GRL_ISBOLD     = ECR_ISBOLD;
             GRL_ISUNDERLIN = ECR_ISUNDERLIN;
             GRL_ISUPPER    = ECR_ISUPPER;
             GRL_SPCANTES   = ECR_SPCANTES;
             GRL_SPCDEPOIS  = ECR_SPCDEPOIS;
             GRL_DFTBULL    = ECR_DFTBULL;
             GRL_QTDEIDENT  = ECR_QTDEIDENT;
             GRL_TXTALIGN   = ECR_TXTALIGN;
             GRL_GRL01002   = %DATE();
             GRL_DIMBULL    = ECR_DIMBULL;
             UPDATE RGRLP010 %FIELDS(GRL_DSTIPO:GRL_ISEXCEPT:GRL_NMFONTE:
                                     GRL_DIMFONTE:GRL_ISBOLD:GRL_ISUNDERLIN:
                                     GRL_ISUPPER:GRL_SPCANTES:GRL_SPCDEPOIS:
                                     GRL_DFTBULL:GRL_QTDEIDENT:GRL_TXTALIGN:
                                     GRL_GRL01002:GRL_DIMBULL);
          ENDIF;
       ENDSR;
      /END-FREE
     PMntPaginas       E
