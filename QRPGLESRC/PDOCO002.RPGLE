     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PDOCE002
     H* Descrição.: Manutenção de Páginas do Formulário
     H*==================================================================
     FFDOCP002  UF A E           K DISK    PREFIX(DOC_)
     FFDOCE002  CF   E             WORKSTN SFILE(RDOCE00202:REC#)
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

     D*==> Estrutura
     D MntParagrafo    PR                  EXTPGM('PDOCO003')
     D   PCdDocumento                 5S 0 CONST
     D   PDsDocumento                50A   CONST
     D   PCdPagina                    5S 0 CONST
     D   PDsPagina                   50A   CONST
     D
     D
     D*==> Estrutura
     D MntPaginas      PR
     D   PCdDocumento                 5S 0 CONST
     D   PDsDocumento                50A   CONST
     D
     D*==> Estrutura de dados do Registo
     D WIcErro         S               N   INZ(*OFF)
     D WCdMessage      S              7A   INZ(*BLANKS)
     D WIcOrigem       S              1A
     D REC#            S              4S 0
     D
     D WCdDocum        S              5S 0
     D WDsDocum        S             50A
     D
     D KTE#Tabela      S              5S 0
     C*========================================================
     C     *ENTRY        PLISt
     C                   PARM                    WCdDocum
     C                   PARM                    WDsDocum
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
       KTE#Tabela = $SETFILES('FDOCP002');
       $addCriteria('Descrição..:':'DSPAGINA':'LK');
       $addCriteria('Código.....:':'NUPAGINA':'GE');
       ECR_DSCRITER = $getCritText();
       $SetCriteria('(CDDOCUM = ''' + %CHAR(WCdDocum) + ''')');
       $SetPage(12);

       MntPaginas(WCdDocum:WDsDocum);

       $RmvDBSession(KTE#Tabela);
       *INLR = *ON;
       RETURN;
      /END-FREE

     P*===================================================================
     P* Processo..: MntPaginas
     P* Descrição.: Realizar a manutenção das Páginas.
     P*===================================================================
     P MntPaginas      B                   EXPORT
     D MntPaginas      PI
     D   PCdDocumento                 5S 0 CONST
     D   PDsDocumento                50A   CONST
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
       ECR_CdDocum = PCdDocumento;
       ECR_DsDocum = PDsDocumento;
       DOW (NOT *IN03 AND NOT *IN12);
          EXSR inzRecords;
          EXSR getRecords;
          WNoFunction = 'RDOCE00203'; // Listar Componentes
          DS#HEADER   = $getHeader(S#PGM:WNoFunction);
          DS#FOOTER   = $getFooter(S#PGM:WNoFunction:*BLANKS);
          ECR_S#PGM   = S#PGM;
          ECR_ERR#MSG = *BLANKS;
          WRITE     RDOCE00200;
          WRITE     RDOCE00299;
          WRITE     RDOCE00298;
          EXFMT RDOCE00203;
          IF (Func#Key = KEY#F05);
             $ChgCriteria();
             ECR_DSCRITER = $getCritText();
          ELSEIF ($ValidKey(Func#Key:KEY#F06));
             WIcDisplay = *OFF;
             WIcInsert  = *ON;
             EXSR DspFormulario;
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
          WRITE RDOCE00203;     // Limpar Lista de Funções
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
                ECR_OPTION   = *BLANKS;
                ECR_LSTHDR = *BLANKS;
                ECR_LSTDET = *BLANKS;
                %SUBST(ECR_LSTHDR:1:10)='Descrição';
                %SUBST(ECR_LSTDET:1:40) =$GetField('DSPAGINA');
                %SUBST(ECR_LSTHDR:42:10)='Orientação';
                %SUBST(ECR_LSTDET:42:5)=$GetField('ICORIENT');
                MONITOR;
                   %SUBST(ECR_LSTHDR:53:7)='Formato';
                   %SUBST(ECR_LSTDET:53:3)= $GetField('ICFORMAT');
                   ECR_NUPAGINA = %DEC($GetField('NUPAGINA'):5:0);
                ON-ERROR;
                ENDMON;
                REC# += 1;
                WRITE RDOCE00202;
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
          READC RDOCE00202;
          DOW (NOT %EOF());
             IF (ECR_OPTION <> *BLANKS);
                *IN45 = *OFF;
                WIcDisplay = *ON;
                EXSR DspFormulario;
                LEAVESR;
             ENDIF;
             READC RDOCE00202;
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
       // Subrotina.: DspFormulario
       // Objectivo.: Apresentar os valores do Formulário
       //========================================================
       BEGSR DspFormulario;
          IF (NOT WIcInsert);
             CHAIN(N) (ECR_CDDOCUM:ECR_NUPAGINA) RDOCP002;
             IF (%FOUND());
                ECR_NUPAGINA = DOC_NUPAGINA;
                ECR_DSPAGINA = DOC_DSPAGINA;
                ECR_ICORIENT = DOC_ICORIENT;
                ECR_DSORIENT = $GetDescricao(67:ECR_ICORIENT);
                ECR_ICFORMAT = DOC_ICFORMAT;
                ECR_DSFORMAT = $GetDescricao(68:%CHAR(ECR_ICFORMAT));
                ECR_NULINHAS = DOC_NULINHAS;
                ECR_NUCOLUNAS= DOC_NUCOLUNAS;
             ELSE;
                LEAVESR;
             ENDIF;
          ELSE;
             *IN45 = *ON;
             ECR_NUPAGINA = *ZEROS;
             ECR_DSPAGINA = *BLANKS;
             ECR_ICORIENT = *BLANKS;
             ECR_ICFORMAT = *ZEROS;
             ECR_NULINHAS = *ZEROS;
             ECR_NUCOLUNAS= *ZEROS;
          ENDIF;
          DOW NOT (*IN03 OR *IN12);
             IF ( WIcInsert);
                WNoFunction = 'RDOCE00206'; // Adicionar um registo
             ELSEIF (WIcDisplay);
                WNoFunction = 'RDOCE00205'; // Mostrar o registo
             ELSE;
                WNoFunction = 'RDOCE00221'; // Alterar o Registo
             ENDIF;
             DS#HEADER   = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER   = $getFooter(S#PGM:WNoFunction:*BLANKS);
             ECR_S#PGM   = S#PGM;
             WRITE     RDOCE00200;
             WRITE     RDOCE00299;
             EXFMT RDOCE00205;
             IF (Func#Key=KEY#F04) AND (NOT WIcDisplay);
                EXSR RunPopUp;
             ELSEIF (Func#Key=KEY#F08) AND (WIcDisplay);
                MntParagrafo(ECR_CDDOCUM:ECR_DSDOCUM:
                             ECR_NUPAGINA:ECR_DSPAGINA);
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
             // EXSR DspTexto;
             ELSEIF ($ValidKey(Func#Key:KEY#F21)) AND (WIcDisplay);
                WIcDisplay = *OFF;
                *IN45 = *ON;
             ELSEIF ($ValidKey(Func#Key:KEY#F22)) AND (WIcDisplay);
                ECR_OPTION = 'N';
                EXFMT RDOCE00222;
                IF (Func#Key = KEY#Enter) AND (ECR_OPTION = 'S');
                   DELETE (ECR_CDDOCUM:ECR_NUPAGINA) RDOCP002;
                   LEAVESR;
                ENDIF;
                *IN12 = *OFF;
             ELSEIF (Func#Key = KEY#ENTER);
                IF (NOT WIcDisplay) OR (WIcInsert);
                   WIcErro = *OFF;
                   WCdMessage = *BLANKS;
                   ECR_ERR#MSG = *BLANKS;
                   EXSR ValFormulario;
                   IF (WIcErro);
                      ECR_ERR#MSG = $GetMessage(WCdMessage);
                      ITER;
                   ENDIF;
                   EXSR MntForm;
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
       // Subrotina.: ValFormulario
       // Objectivo.: Apresentar os valores da Tabela seleccionada
       //========================================================
       BEGSR ValFormulario;
          IF (ECR_DSPAGINA = *BLANKS);
             WCdMessage = 'COR0004';
             WIcErro = *ON;
|            ECR_CM#COL = CM#CCOL;
             $getFldLoc('FDOCE002':'RDOCE00205':'DSPAGINA':
                        ECR_CM#ROW:ECR_CM#COL);
          ELSEIF (ECR_ICORIENT = *BLANKS);
             WCdMessage = 'COR0005';
             WIcErro = *ON;
             $getFldLoc('FDOCE002':'RDOCE00205':'ICORIENT':
                        ECR_CM#ROW:ECR_CM#COL);
          ELSEIF (ECR_ICFORMAT = *ZEROS);
             WCdMessage = 'COR0006';
             WIcErro = *ON;
             $getFldLoc('FDOCE002':'RDOCE00205':'ICFORMAT':
                        ECR_CM#ROW:ECR_CM#COL);
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: MntForm
       // Objectivo.: Alterar, Criar ou eliminar uma Tabela
       //========================================================
       BEGSR MntForm;
          //-------------------------------------------
          // Inserir um registo
          //-------------------------------------------
          IF (ECR_NUPAGINA = *ZEROS);
             DOC_CDDOCUM   = ECR_CDDOCUM;
             DOC_NUPAGINA  = $getNextKey('FDOCP002':'NUPAGINA':
                    '(CDDOCUM='''+%CHAR(ECR_CDDOCUM)+''')');
             ECR_NUPAGINA  = DOC_NUPAGINA;
             DOC_DSPAGINA  = ECR_DSPAGINA;
             DOC_ICORIENT  = ECR_ICORIENT;
             DOC_ICFORMAT  = ECR_ICFORMAT;
             DOC_NULINHAS  = ECR_NULINHAS;
             DOC_NUCOLUNAS = ECR_NUCOLUNAS;
             WRITE RDOCP002;

          //-------------------------------------------
          // Alterar um registo
          //-------------------------------------------
          ELSE;
             CHAIN (ECR_CDDOCUM:ECR_NUPAGINA) RDOCP002;
             DOC_DSPAGINA = ECR_DSPAGINA;
             DOC_ICORIENT  = ECR_ICORIENT;
             DOC_ICFORMAT  = ECR_ICFORMAT;
             DOC_NULINHAS  = ECR_NULINHAS;
             DOC_NUCOLUNAS = ECR_NUCOLUNAS;
             UPDATE RDOCP002 %FIELDS(DOC_DSPAGINA:DOC_ICORIENT:DOC_ICFORMAT:
                                     DOC_NULINHAS:DOC_NUCOLUNAS);
          ENDIF;
       ENDSR;

       //==================================================================
       // Subrotina.: RunPopUp
       // Objectivo.: Adicionar um novo registo de uma Apólice
       //==================================================================
+----  BEGSR RunPopUp;
|+---     IF (ECR_CM#NMFLD = 'ICORIENT');
             ECR_ICORIENT = $runPopUp(67:ECR_ICORIENT);
             ECR_DSORIENT = $GetDescricao(67:ECR_ICORIENT);
|+---     ELSEIF (ECR_CM#NMFLD = 'ICFORMAT');
             ECR_ICFORMAT = (%DEC($runPopUp(68:%CHAR(ECR_ICFORMAT)):2:0));
             ECR_DSFORMAT = $GetDescricao(68:%CHAR(ECR_ICFORMAT));
          ENDIF;
|         ECR_CM#ROW = CM#CROW;
|         ECR_CM#COL = CM#CCOL;
+----  ENDSR;
      /END-FREE
     PMntPaginas       E
