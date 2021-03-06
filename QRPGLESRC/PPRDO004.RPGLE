     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PPRDE004
     H* Descri��o.: Manuten��o de Tabelas e C�digos
     H*==================================================================
     FFTBLP010  UF A E           K DISK    PREFIX(TBL_)
     FFTBLP01202IF   E           K DISK    PREFIX(TBL_)
     F                                     RENAME(RTBLP012:RTBLP01202)
     FFTBLP011  UF A E           K DISK    PREFIX(TBL_)
     FFTBLP012  UF A E           K DISK    PREFIX(TBL_)
     FFPRDE004  CF   E             WORKSTN SFILE(RPRDE00402:REC#)
     F                                     PREFIX(ECR_)
     F                                     INFDS(INFD#KEYS)
     F*==========================================================
     F* Indicadores  Descri��o
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

     D*==> Estrutura de Tabelas ou C�digos
     D MntTabelas      PR
     D   PCdTabela                    5S 0 CONST
     D   PDsTabela                   40A   CONST
     D
     D*==> Estrutura de dados do Registo
     D WIcErro         S               N   INZ(*OFF)
     D WCdMessage      S              7A   INZ(*BLANKS)
     D WIcOrigem       S              1A
     D REC#            S              4S 0
     D
     D WDimMin         S              5S 0
     D WDimMax         S              5S 0
     D WDimensao       S              5S 0
     D WDominio        S              1A
     D WIncrem         S              5S 0
     D
     D KTE#Meses       S              5S 0
     D KTE#Opera       S              5S 0
     D KTE#Apolice     S              5S 0
     C*========================================================
      /FREE

       //------------------------------------
       // Se n�o estiver autorizado ent�o sai
       //------------------------------------
       IF NOT $ValAccess(S#PGM:'':99999);
          *INLR = *ON;
          RETURN;
       ENDIF;

       //----------------------------------------------
       // Sess�o das Fun��es
       //----------------------------------------------
       KTE#Meses = $SetFiles('FPRDP120');
       $addCriteria('Ano........:':'ANOIDX':'GE');
       $addCriteria('Mes........:':'MESIDX':'GE');
       ECR_DSCRITER = $getCritText();
       $SetLstFld('ANOIDX':'Ano':6:'D');
       $SetLstFld('MESIDX':'Mes.':6:'D');
       $SetLstFld('DATE(DATEXEC)':'Data':20:'E');
       $SetOrder('ANOIDX #DESC, MESIDX #DESC');
       $SetPage(12);

       //----------------------------------------------
       // Sess�o das Opera��es
       //----------------------------------------------
       KTE#Opera  = $SETFILES('FPRDP121');
       $addCriteria('Descri��o..:':'DESCCOD':'LK');
       $addCriteria('C�digo.....:':'CODIGO':'GE');
       ECR_DSCRITER = $getCritText();
       $SetLstFld('CODIGO':'C�digo':10:'D');
       $SetLstFld('DESCCOD':'Descri��o':59:'E');
       $SetPage(12);

       $SetDBSession(KTE#Tabela); // Repor Sess�o das Fun��es

       MntTabelas(*ZEROS:*BLANKS);

       $RmvDBSession(KTE#Tabela);
       $RmvDBSession(KTE#Codigo);
       *INLR = *ON;
       RETURN;
      /END-FREE

     P*===================================================================
     P* Processo..: MntTabelas
     P* Descri��o.: Realizar a manuten��o dos Tabelas de C�digo e C�digos
     P*===================================================================
     P MntTabelas      B                   EXPORT
     D MntTabelas      PI
     D   PCdTabela                    5S 0 CONST
     D   PDsTabela                   40A   CONST
     D
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

       DOW (NOT *IN03 AND NOT *IN12);
          EXSR inzRecords;
          EXSR getRecords;
          IF ($GetDBSession() = KTE#Tabela);
             WNoFunction = 'RPRDE00403'; // Listar Componentes
          ELSE;
             WNoFunction = 'RPRDE00404'; // Listar Opera��es
          ENDIF;
          DS#HEADER   = $getHeader(S#PGM:WNoFunction);
          DS#FOOTER   = $getFooter(S#PGM:WNoFunction:*BLANKS);
          DS#OPTIONS   = $getOption(S#PGM:WNoFunction:*BLANKS);
          ECR_S#PGM   = S#PGM;
          ECR_ERR#MSG = *BLANKS;
          WRITE     RPRDE00400;
          WRITE     RPRDE00499;
          WRITE     RPRDE00498;
          EXFMT RPRDE00403;
          IF (Func#Key = KEY#F05);
             $ChgCriteria();
             ECR_DSCRITER = $getCritText();
          ELSEIF ($ValidKey(Func#Key:KEY#F06));
             WIcDisplay = *OFF;
             WIcInsert  = *ON;
             IF ($GetDBSession()=KTE#Codigo);
                EXSR DspCodigo;
             ELSE;
                EXSR DspTabela;
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
          *In53     = *On;      // Limpar Lista de Ecr�
          *In52     = *On;      // Mostrar a Lista de Ecr�
          WRITE RPRDE00403;     // Limpar Lista de Fun��es
          *In53     = *Off;     // Repor Indicador
          *In52     = *Off;     // Mostrar a Lista de Ecr�
       ENDSR;

       //========================================================
       // Subrotina.: GetRecords
       // Objectivo.: Obter todos os registos de tabelas dispon�v
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
                // ForCompr as Chaves das Tabelas
                //---------------------------------------------
                MONITOR;
                   ECR_CDTABELA = %DEC($GetField('CDINDICE'):5:0);
                   IF ($GetDBSession() = KTE#Tabela);
                      ECR_CDCODIGO = *ZEROS;
                   ELSE;
                      ECR_CDCODIGO = %DEC($GetField('CDCODIGO'):5:0);
                   ENDIF;
                ON-ERROR;
                ENDMON;

                REC# += 1;
                WRITE RPRDE00402;
                *IN52 = *ON;       // Encontrou registos
                $getNext();
             ENDDO;
          ENDIF;
          *IN54 = $IsLastPage();   // Indica��o de Fim de P�gina
       ENDSR;

       //========================================================
       // Subrotina.: SelRec
       // Objectivo.: Seleccionar os registos
       //========================================================
       BEGSR SelRec;
          WSelected = *OFF;
          READC RPRDE00402;
          DOW (NOT %EOF());
             IF ($ValidOption(ECR_OPTION:'5'));
                *IN45 = *OFF;
                WIcDisplay = *ON;
                IF ($GetDBSession=KTE#Tabela);
                   EXSR DspTabela;
                ELSE;
                   EXSR DspCodigo;
                ENDIF;
                LEAVESR;
             ELSEIF ($ValidOption(ECR_OPTION:'7'));
                CHAIN(N) (ECR_CDTABELA) RTBLP010;
                IF (%FOUND());
                   $SetDBSession(KTE#Codigo);
                   $SetCriteria('(CDINDICE = ''' +
                                  %CHAR(ECR_CDTABELA) + ''')');
                   WDimMin   = TBL_DIMMIN;
                   WDimMax   = TBL_DIMMAX;
                   WDominio  = TBL_DOMINIO;
                   WIncrem   = TBL_CDINCREM;
                   MntTabelas(ECR_CDTABELA:ECR_DSINDICE);
                   $SetDBSession(KTE#Tabela);
                ENDIF;
                LEAVESR;
             ELSEIF ($ValidOption(ECR_OPTION:'8'));
                EXSR DspTexto;
                $SetDBSession(KTE#Tabela);
                LEAVESR;
             ENDIF;
             READC RPRDE00402;
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
       // Subrotina.: DspTabela
       // Objectivo.: Apresentar os valores da Tabela seleccionada
       //========================================================
       BEGSR DspTabela;
          IF (NOT WIcInsert);
             CHAIN(N) (ECR_CDTABELA) RTBLP010;
             IF (%FOUND());
                ECR_CDINDICE = TBL_CDINDICE;
                ECR_DSINDICE = TBL_DSINDICE;
                ECR_CDMODULO = TBL_CDMODULO;
                ECR_DOMINIO  = TBL_DOMINIO;
                ECR_DIMMIN   = TBL_DIMMIN;
                ECR_DIMMAX   = TBL_DIMMAX;
                ECR_ICEMPTY  = TBL_ICEMPTY;
                ECR_CDINCREM = TBL_CDINCREM;
                ECR_ICRESERV = TBL_ICRESERV;
             ELSE;
                LEAVESR;
             ENDIF;
          ELSE;
             *IN45 = *ON;
             ECR_CDINDICE = *ZEROS;
             ECR_DSINDICE = *BLANKS;
             ECR_CDMODULO = *BLANKS;
             ECR_DOMINIO  = *BLANKS;
             ECR_DIMMIN   = *ZEROS;
             ECR_DIMMAX   = *ZEROS;
             ECR_ICEMPTY  = *BLANKS;
             ECR_CDINCREM = *ZEROS;
             ECR_ICRESERV = *BLANKS;
          ENDIF;
          DOW NOT (*IN03 OR *IN12);
             IF ( WIcInsert);
                WNoFunction = 'RPRDE00406'; // Adicionar um registo
             ELSEIF (WIcDisplay);
                WNoFunction = 'RPRDE00405'; // Mostrar o registo
             ELSE;
                WNoFunction = 'RPRDE00421'; // Alterar o Registo
             ENDIF;
             DS#HEADER   = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER   = $getFooter(S#PGM:WNoFunction:*BLANKS);
             ECR_S#PGM   = S#PGM;
             WRITE     RPRDE00400;
             WRITE     RPRDE00499;
             EXFMT RPRDE00405;
             IF (Func#Key=KEY#F04) AND (NOT WIcDisplay);
                EXSR RunPopUp;
             ELSEIF (Func#Key=KEY#F08) AND (WIcDisplay);
                $SetDBSession(KTE#Codigo);
                $SetCriteria('(CDINDICE = ''' +
                               %CHAR(ECR_CDTABELA) + ''')');
                WDimMin   = ECR_DIMMIN;
                WDimMax   = ECR_DIMMAX;
                WDominio  = ECR_DOMINIO;
                WIncrem   = ECR_CDINCREM;
                MntTabelas(ECR_CDTABELA:ECR_DSINDICE);
                $SetDBSession(KTE#Tabela);
                *IN12 = *OFF;
                *IN45 = *OFF;
                WIcDisplay = *ON;
             ELSEIF (Func#Key=KEY#F12) AND (NOT WIcDisplay)
                AND (Not WIcInsert);
                WIcDisplay = *ON;
                *IN45 = *OFF;
                *IN21 = *OFF;
                *IN12 = *OFF;
             ELSEIF (Func#Key=KEY#F14) AND (WIcDisplay);
                EXSR DspTexto;
             ELSEIF ($ValidKey(Func#Key:KEY#F21)) AND (WIcDisplay);
                WIcDisplay = *OFF;
                *IN45 = *ON;
             ELSEIF ($ValidKey(Func#Key:KEY#F22)) AND (WIcDisplay);
                ECR_OPTION = 'N';
                EXFMT RPRDE00422;
                IF (Func#Key = KEY#Enter) AND (%TRIM(ECR_OPTION) = 'S');
                   DELETE (ECR_CDTABELA) RTBLP010;
                   LEAVESR;
                ENDIF;
                *IN12 = *OFF;
             ELSEIF (Func#Key = KEY#ENTER);
                IF (NOT WIcDisplay) OR (WIcInsert);
                   WIcErro = *OFF;
                   WCdMessage = *BLANKS;
                   ECR_ERR#MSG = *BLANKS;
                   EXSR ValTabela;
                   IF (WIcErro);
                      ECR_ERR#MSG = $GetMessage(WCdMessage);
                      ITER;
                   ENDIF;
                   EXSR MntTabela;
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
       // Subrotina.: ValTabela
       // Objectivo.: Apresentar os valores da Tabela seleccionada
       //========================================================
       BEGSR ValTabela;
          IF (ECR_DSINDICE = *BLANKS);
             WCdMessage = 'TBL0001';
             WIcErro = *ON;
|            ECR_CM#COL = CM#CCOL;
             $getFldLoc('FPRDE004':'RPRDE00405':'DSINDICE':
                        ECR_CM#ROW:ECR_CM#COL);
          ELSEIF (ECR_CDMODULO = *BLANKS);
             WCdMessage = 'TBL0002';
             WIcErro = *ON;
             $getFldLoc('FPRDE004':'RPRDE00405':'CDMODULO':
                        ECR_CM#ROW:ECR_CM#COL);
          ELSEIF (ECR_DOMINIO = *BLANKS);
             WCdMessage = 'TBL0003';
             WIcErro = *ON;
             $getFldLoc('FPRDE004':'RPRDE00405':'DOMINIO':
                        ECR_CM#ROW:ECR_CM#COL);
          ELSEIF (ECR_DIMMIN = *ZEROS);
             WCdMessage = 'TBL0004';
             WIcErro = *ON;
             $getFldLoc('FPRDE004':'RPRDE00405':'DIMMIN':
                        ECR_CM#ROW:ECR_CM#COL);
          ELSEIF (ECR_DIMMAX = *ZEROS);
             WCdMessage = 'TBL0005';
             WIcErro = *ON;
             $getFldLoc('FPRDE004':'RPRDE00405':'DIMMAX':
                        ECR_CM#ROW:ECR_CM#COL);
          ELSEIF (ECR_DIMMIN > ECR_DIMMAX);
             $getFldLoc('FPRDE004':'RPRDE00405':'DIMMIN':
                        ECR_CM#ROW:ECR_CM#COL);
             WCdMessage = 'TBL0006';
             WIcErro = *ON;
          ENDIF;
       ENDSR;

       //==================================================================
       // Subrotina.: DspTexto
       // Objectivo.: Apresentar o texto de ajuda para a Tabela
       //==================================================================
+----  BEGSR DspTexto;
          CHAIN(N) (ECR_CDTABELA) RTBLP011;
          IF (%FOUND());
             ECR_DSAJUDA = TBL_TEXTO;
          ELSE;
             ECR_DSAJUDA = *BLANKS;
          ENDIF;
          DOW NOT (*IN03 OR *IN12);
             IF (WIcDisplay);
                WNoFunction = 'RPRDE00414'; // Consultar o Texto
             ELSE;
                WNoFunction = 'RPRDE00415'; // Alterar o Texto
             ENDIF;
             DS#HEADER   = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER   = $getFooter(S#PGM:WNoFunction:*BLANKS);
             ECR_S#PGM   = S#PGM;
             WRITE     RPRDE00400;
             WRITE     RPRDE00499;
             EXFMT RPRDE00414;
             IF (Func#Key = KEY#Enter);
                IF (NOT WIcDisplay);
                   CHAIN (ECR_CDTABELA) RTBLP011;
                   IF (%FOUND());
                      TBL_TEXTO = ECR_DSAJUDA;
                      UPDATE RTBLP011 %FIELDS(TBL_TEXTO);
                   ELSE;
                      TBL_TEXTO = ECR_DSAJUDA;
                      TBL_CDINDICE = ECR_CDTABELA;
                      WRITE RTBLP011;
                   ENDIF;
                   WIcDisplay = *ON;
                   *IN45 = *OFF;
                ELSE;
                   LEAVESR;
                ENDIF;
             ELSEIF (Func#Key = KEY#F12) AND (NOT WIcDisplay);
                WIcDisplay = *ON;
                *IN45     = *OFF;
                *IN12     = *OFF;
             ELSEIF (Func#Key = KEY#F21) AND (WIcDisplay);
                WIcDisplay = *OFF;
                *IN45 = *ON;
             ENDIF;
          ENDDO;
          *IN12 = *OFF;
       ENDSR;

       //==================================================================
       // Subrotina.: RunPopUp
       // Objectivo.: Adicionar um novo registo de uma Ap�lice
       //==================================================================
+----  BEGSR RunPopUp;
|+---     IF (ECR_CM#NMFLD = 'CDMODULO');
             ECR_CDMODULO = $runPopUp(4:ECR_CDMODULO);
|+---     ELSEIF (ECR_CM#NMFLD = 'DOMINIO');
             ECR_DOMINIO = $runPopUp(3:ECR_DOMINIO);
          ENDIF;
|         ECR_CM#ROW = CM#CROW;
|         ECR_CM#COL = CM#CCOL;
+----  ENDSR;

       //========================================================
       // Subrotina.: MntTabela
       // Objectivo.: Alterar, Criar ou eliminar uma Tabela
       //========================================================
       BEGSR MntTabela;
          //-------------------------------------------
          // Inserir um registo
          //-------------------------------------------
          IF (ECR_CDINDICE = *ZEROS);
             TBL_CDINDICE  = $getNextKey('FTBLP010':'CDINDICE');
             ECR_CDTABELA  = TBL_CDINDICE;
             TBL_DSINDICE = ECR_DSINDICE;
             TBL_CDMODULO = ECR_CDMODULO;
             TBL_DOMINIO  = ECR_DOMINIO;
             TBL_DIMMIN   = ECR_DIMMIN;
             TBL_DIMMAX   = ECR_DIMMAX;
             TBL_CDINCREM = ECR_CDINCREM;
             TBL_ICEMPTY  = ECR_ICEMPTY;
             TBL_ICRESERV = ECR_ICRESERV;
             WRITE RTBLP010;

          //-------------------------------------------
          // Alterar um registo
          //-------------------------------------------
          ELSE;
             CHAIN (ECR_CDINDICE) RTBLP010;
             TBL_DSINDICE = ECR_DSINDICE;
             TBL_CDMODULO = ECR_CDMODULO;
             TBL_DOMINIO  = ECR_DOMINIO;
             TBL_DIMMIN   = ECR_DIMMIN;
             TBL_DIMMAX   = ECR_DIMMAX;
             TBL_CDINCREM = ECR_CDINCREM;
             TBL_ICEMPTY  = ECR_ICEMPTY;
             TBL_ICRESERV = ECR_ICRESERV;
             UPDATE RTBLP010 %FIELDS(TBL_DSINDICE:TBL_CDMODULO:TBL_DOMINIO:
                                     TBL_DIMMIN:TBL_DIMMAX:TBL_CDINCREM:
                                     TBL_ICEMPTY:TBL_ICRESERV);
          ENDIF;
       ENDSR;
       //========================================================
       // Subrotina.: DspCodigo
       // Objectivo.: Apresentar os valores do C�digo
       //========================================================
       BEGSR DspCodigo;
          *IN48 = *OFF;
          IF (NOT WIcDisplay);
          ENDIF;
          IF (NOT WIcInsert);
             CHAIN(N) (ECR_CDTABELA:ECR_CDCODIGO) RTBLP012;
             IF (%FOUND());
                ECR_CDINDICE = TBL_CDINDICE;
                ECR_CDCODIGO = TBL_CDCODIGO;
                ECR_CODIGO   = TBL_CODIGO;
                ECR_ALIAS    = TBL_ALIAS;
                ECR_DESCABRV = TBL_DESCABRV;
                ECR_DESCCOD  = TBL_DESCCOD;
             ENDIF;
          ELSE;
             *IN45 = *ON;
             IF (WIncrem = *ZEROS);
                ECR_CODIGO   = *BLANKS;
                *IN48 = *ON;
             ELSE;
                MONITOR;
                SETGT (ECR_CDTABELA:'02199') RTBLP01202;
                READP RTBLP01202;
                IF (ECR_CDTABELA = TBL_CDINDICE);
                   ECR_Codigo='0'+%CHAR(((%DEC(TBL_CODIGO:5:0)-2100)+1)+2100);
                ENDIF;
                ON-ERROR;
                ENDMON;
             ENDIF;
             ECR_CDINDICE = ECR_CDTABELA;
             ECR_CDCODIGO = *ZEROS;
             ECR_ALIAS    = *BLANKS;
             ECR_DESCABRV = *BLANKS;
             ECR_DESCCOD  = *BLANKS;
          ENDIF;
          DOW NOT (*IN03 OR *IN12);
             IF (WIcInsert);
                WNoFunction = 'RPRDE00409'; // Adicionar um C�digo
             ELSEIF (WIcDisplay);
                WNoFunction = 'RPRDE00407'; // Consultar um C�digo
             ELSE;
                WNoFunction = 'RPRDE00408'; // Alterar um C�digo
             ENDIF;
             DS#HEADER   = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER   = $getFooter(S#PGM:WNoFunction:*BLANKS);
             ECR_S#PGM   = S#PGM;
             WRITE     RPRDE00400;
             WRITE     RPRDE00499;
             EXFMT RPRDE00407;
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
                EXFMT RPRDE00422;
                IF (Func#Key = KEY#Enter) AND (%TRIM(ECR_OPTION) = 'S');
                   DELETE (ECR_CDTABELA:ECR_CDCODIGO) RTBLP012;
                   LEAVESR;
                ENDIF;
                *IN12 = *OFF;
             ELSEIF (Func#Key=KEY#ENTER);
                IF (NOT WIcDisplay) OR (WIcInsert);
                   WIcErro = *OFF;
                   ECR_ERR#MSG = *BLANKS;
                   EXSR ValCodigo;
                   IF (WIcErro);
                      ECR_ERR#MSG = $GetMessage(WCdMessage);
                      ITER;
                   ENDIF;
                   EXSR MntCodigo;
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
       // Subrotina.: ValCodigo
       // Objectivo.: Validar os campos da frame de c�digos
       //========================================================
       BEGSR ValCodigo;
          WDimensao = %LEN(%TRIM(ECR_CODIGO));
          IF (ECR_CODIGO = *BLANKS);
             WCdMessage = 'TBL0007';
             WIcErro = *ON;
|            ECR_CM#COL = CM#CCOL;
             $getFldLoc('FPRDE004':'RPRDE00407':'CODIGO':
                        ECR_CM#ROW:ECR_CM#COL);
          ELSEIF (ECR_DESCABRV = *BLANKS);
             WCdMessage = 'TBL0008';
             WIcErro = *ON;
             $getFldLoc('FPRDE004':'RPRDE00407':'DESCABRV':
                        ECR_CM#ROW:ECR_CM#COL);
          ELSEIF (WDimensao > WDimMax);
             WCdMessage = 'TBL0009';
             WIcErro = *ON;
|            ECR_CM#COL = CM#CCOL;
             $getFldLoc('FPRDE004':'RPRDE00407':'CODIGO':
                        ECR_CM#ROW:ECR_CM#COL);
          ELSEIF (WDimensao < WDimMin);
             WCdMessage = 'TBL0010';
             WIcErro = *ON;
|            ECR_CM#COL = CM#CCOL;
             $getFldLoc('FPRDE004':'RPRDE00407':'CODIGO':
                        ECR_CM#ROW:ECR_CM#COL);
          ELSE;
             MONITOR;
                IF (%DEC(%TRIM(ECR_CODIGO):5:0) = *ZEROS);
                ENDIF;
                IF (WDominio = 'A');
                   WCdMessage = 'TBL0012';
                   WIcErro = *ON;
|                  ECR_CM#COL = CM#CCOL;
                   $getFldLoc('FPRDE004':'RPRDE00407':'CODIGO':
                              ECR_CM#ROW:ECR_CM#COL);
                ENDIF;
             ON-ERROR;
                IF (WDominio = 'N');
                   WCdMessage = 'TBL0013';
                   WIcErro = *ON;
|                  ECR_CM#COL = CM#CCOL;
                   $getFldLoc('FPRDE004':'RPRDE00407':'CODIGO':
                              ECR_CM#ROW:ECR_CM#COL);
                ENDIF;
             ENDMON;
             IF (NOT WIcErro);
                CHAIN (ECR_CDTABELA:ECR_CODIGO) RTBLP01202;
                IF (%FOUND());
                   IF (ECR_CDCODIGO <> TBL_CDCODIGO);
                      WCdMessage = 'TBL0011';
                      WIcErro = *ON;
|                     ECR_CM#COL = CM#CCOL;
                      $getFldLoc('FPRDE004':'RPRDE00407':'CODIGO':
                                 ECR_CM#ROW:ECR_CM#COL);
                   ENDIF;
                ENDIF;
             ENDIF;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: MntCodigo
       // Objectivo.: Alterar, Criar ou eliminar um C�digo
       //========================================================
       BEGSR MntCodigo;
          //-------------------------------------------
          // Inserir um registo
          //-------------------------------------------
          IF (ECR_CDCODIGO = *ZEROS);
             TBL_CDCODIGO  = $getNextKey('FTBLP012':'CDCODIGO':
                              'CDINDICE = '''+%CHAR(ECR_CDTABELA)+'''');
             TBL_CDINDICE = ECR_CDTABELA;
             ECR_CDCODIGO = TBL_CDCODIGO;
             TBL_CODIGO   = ECR_CODIGO;
             TBL_ALIAS    = ECR_CODIGO;
             TBL_DESCCOD  = ECR_DESCCOD;
             TBL_DESCABRV = ECR_DESCABRV;
             WRITE RTBLP012;

          //-------------------------------------------
          // Alterar um registo
          //-------------------------------------------
          ELSE;
             CHAIN (ECR_CDTABELA:ECR_CDCODIGO) RTBLP012;
             TBL_CODIGO   = ECR_CODIGO;
             TBL_ALIAS    = ECR_ALIAS;
             TBL_DESCCOD  = ECR_DESCCOD;
             TBL_DESCABRV = ECR_DESCABRV;
             UPDATE RTBLP012 %FIELDS(TBL_CODIGO:TBL_DESCCOD:
                                     TBL_ALIAS:TBL_DESCABRV);
          ENDIF;
       ENDSR;
      /END-FREE
     PMntTabelas       E
