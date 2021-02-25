     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PDOCE003
     H* Descrição.: Manutenção de Páginas do Formulário
     H*==================================================================
     FFDOCP003  UF A E           K DISK    PREFIX(DOC_)
     FFDOCE003  CF   E             WORKSTN SFILE(RDOCE00302:REC#)
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
     D MntParagrafo    PR
     D   PCdDocumento                 5S 0 CONST
     D   PDsDocumento                50A   CONST
     D   PCdPagina                    5S 0 CONST
     D   PDsPagina                   50A   CONST
     D
     D*==> Estrutura de dados do Registo
     D WIcErro         S               N   INZ(*OFF)
     D WCdMessage      S              7A   INZ(*BLANKS)
     D WIcOrigem       S              1A
     D REC#            S              4S 0
     D
     D WCdDocum        S              5S 0
     D WDsDocum        S             50A
     D WCdPagina       S              5S 0
     D WDsPagina       S             50A
     D
     D KTE#Tabela      S              5S 0
     C*========================================================
     C     *ENTRY        PLISt
     C                   PARM                    WCdDocum
     C                   PARM                    WDsDocum
     C                   PARM                    WCdPagina
     C                   PARM                    WDsPagina
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
       KTE#Tabela = $SETFILES('FDOCP003');
       $addCriteria('Ordem......:':'NUORDEM':'GE');
       $addCriteria('Texto......:':'TEXTO':'LK');
       $addCriteria('Tipo.......:':'TPPARAGR':'EQ');
       ECR_DSCRITER = $getCritText();
       $SetCriteria('(CDDOCUM = ''' + %CHAR(WCdDocum) + ''') AND +
                     (NUPAGINA = ''' + %CHAR(WCdPagina) + ''')');
       $SetSort('NUORDEM');
       $SetPage(12);

       MntParagrafo(WCdDocum:WDsDocum:WCdPagina:WDsPagina);

       $RmvDBSession(KTE#Tabela);
       *INLR = *ON;
       RETURN;
      /END-FREE

     P*===================================================================
     P* Processo..: MntParagrafo
     P* Descrição.: Realizar a manutenção das Páginas.
     P*===================================================================
     P MntParagrafo    B                   EXPORT
     D MntParagrafo    PI
     D   PCdDocumento                 5S 0 CONST
     D   PDsDocumento                50A   CONST
     D   PCdPagina                    5S 0 CONST
     D   PDsPagina                   50A   CONST
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
       ECR_NuPagina= PCdPagina;
       ECR_DsPagina= PDsPagina;
       DOW (NOT *IN03 AND NOT *IN12);
          EXSR inzRecords;
          EXSR getRecords;
          WNoFunction = 'RDOCE00303'; // Listar Componentes
          DS#HEADER   = $getHeader(S#PGM:WNoFunction);
          DS#FOOTER   = $getFooter(S#PGM:WNoFunction:*BLANKS);
          ECR_S#PGM   = S#PGM;
          ECR_ERR#MSG = *BLANKS;
          WRITE     RDOCE00300;
          WRITE     RDOCE00399;
          WRITE     RDOCE00398;
          EXFMT RDOCE00303;
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
          WRITE RDOCE00303;     // Limpar Lista de Funções
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
                MONITOR;
                   %SUBST(ECR_LSTHDR:1:7)='Ordem';
                   %SUBST(ECR_LSTDET:1:5)= $GetField('NUORDEM');
                   ECR_NUSEQU = %DEC($GetField('NUSEQU'):5:0);
                ON-ERROR;
                ENDMON;
                %SUBST(ECR_LSTHDR:8:10)='Texto';
                %SUBST(ECR_LSTDET:8:65) =$GetField('TEXTO');
                REC# += 1;
                WRITE RDOCE00302;
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
          READC RDOCE00302;
          DOW (NOT %EOF());
             IF (ECR_OPTION <> *BLANKS);
                *IN45 = *OFF;
                WIcDisplay = *ON;
                EXSR DspFormulario;
                LEAVESR;
             ENDIF;
             READC RDOCE00302;
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
             CHAIN(N) (ECR_CDDOCUM:ECR_NUPAGINA:ECR_NUSEQU) RDOCP003;
             IF (%FOUND());
                ECR_NUSEQU   = DOC_NUSEQU;
                ECR_NUORDEM  = DOC_NUORDEM;
                ECR_NULINHA  = DOC_NULINHA;
                ECR_NUCOLUNA = DOC_NUCOLUNA;
                ECR_TPPARAGR = DOC_TPPARAGR;
                ECR_CDESTILO = DOC_CDESTILO;
                ECR_TEXTO    = DOC_TEXTO;
             // ECR_DSORIENT = $GetDescricao(67:ECR_ICORIENT);
             // ECR_DSFORMAT = $GetDescricao(68:%CHAR(ECR_ICFORMAT));
             ELSE;
                LEAVESR;
             ENDIF;
          ELSE;
             *IN45 = *ON;
             ECR_NUSEQU   = *ZEROS;
             ECR_NUORDEM  = $getNextKey('FDOCP003':'NUORDEM':
                            '(CDDOCUM='''+%CHAR(ECR_CDDOCUM)+
                             ''') AND (NUPAGINA='''+%CHAR(ECR_NUPAGINA)+
                             ''')');
             ECR_NULINHA  = *ZEROS;
             ECR_NUCOLUNA = *ZEROS;
             ECR_TPPARAGR = *BLANKS;
             ECR_CDESTILO = *BLANKS;
             ECR_TEXTO    = *BLANKS;
          ENDIF;
          DOW NOT (*IN03 OR *IN12);
             IF ( WIcInsert);
                WNoFunction = 'RDOCE00306'; // Adicionar um registo
             ELSEIF (WIcDisplay);
                WNoFunction = 'RDOCE00305'; // Mostrar o registo
             ELSE;
                WNoFunction = 'RDOCE00321'; // Alterar o Registo
             ENDIF;
             DS#HEADER   = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER   = $getFooter(S#PGM:WNoFunction:*BLANKS);
             ECR_S#PGM   = S#PGM;
             WRITE     RDOCE00300;
             WRITE     RDOCE00399;
             EXFMT RDOCE00305;
             IF (Func#Key=KEY#F04) AND (NOT WIcDisplay);
                EXSR RunPopUp;
             ELSEIF (Func#Key=KEY#F08) AND (WIcDisplay);
             // MntTabelas(ECR_CDTABELA:ECR_DSINDICE);
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
                EXFMT RDOCE00322;
                IF (Func#Key = KEY#Enter) AND (ECR_OPTION = 'S');
                   DELETE (ECR_CDDOCUM:ECR_NUPAGINA:ECR_NUSEQU) RDOCP003;
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
          IF (ECR_NUORDEM = *ZEROS);
             WCdMessage = 'COR0007';
             WIcErro = *ON;
|            ECR_CM#COL = CM#CCOL;
             $getFldLoc('FDOCE003':'RDOCE00305':'NUORDEM':
                        ECR_CM#ROW:ECR_CM#COL);
          ELSEIF (ECR_TPPARAGR = *BLANKS);
             WCdMessage = 'COR0008';
             WIcErro = *ON;
             $getFldLoc('FDOCE003':'RDOCE00305':'TPPARAGR':
                        ECR_CM#ROW:ECR_CM#COL);
          ELSEIF (ECR_CDESTILO = *BLANKS);
             WCdMessage = 'COR0009';
             WIcErro = *ON;
             $getFldLoc('FDOCE003':'RDOCE00305':'CDESTILO':
                        ECR_CM#ROW:ECR_CM#COL);
          ELSEIF (ECR_TEXTO = *BLANKS);
             WCdMessage = 'COR0010';
             WIcErro = *ON;
             $getFldLoc('FDOCE003':'RDOCE00305':'TEXTO':
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
          IF (ECR_NUSEQU  = *ZEROS);
             DOC_CDDOCUM  = ECR_CDDOCUM;
             DOC_NUPAGINA = ECR_NUPAGINA;
             DOC_NUSEQU   = $getNextKey('FDOCP003':'NUSEQU':
                            '(CDDOCUM='''+%CHAR(ECR_CDDOCUM)+
                             ''') AND (NUPAGINA='''+%CHAR(ECR_NUPAGINA)+
                             ''')');
             ECR_NUSEQU   = DOC_NUSEQU;
             DOC_NUORDEM  = ECR_NUORDEM;
             DOC_NULINHA  = ECR_NULINHA;
             DOC_NUCOLUNA = ECR_NUCOLUNA;
             DOC_TPPARAGR = ECR_TPPARAGR;
             DOC_CDESTILO = ECR_CDESTILO;
             DOC_TEXTO    = ECR_TEXTO;
             WRITE RDOCP003;

          //-------------------------------------------
          // Alterar um registo
          //-------------------------------------------
          ELSE;
             CHAIN (ECR_CDDOCUM:ECR_NUPAGINA:ECR_NUSEQU) RDOCP003;
             DOC_NUORDEM  = ECR_NUORDEM;
             DOC_NULINHA  = ECR_NULINHA;
             DOC_NUCOLUNA = ECR_NUCOLUNA;
             DOC_TPPARAGR = ECR_TPPARAGR;
             DOC_CDESTILO = ECR_CDESTILO;
             DOC_TEXTO    = ECR_TEXTO;
             UPDATE RDOCP003 %FIELDS(DOC_NUORDEM:DOC_NULINHA:DOC_NUCOLUNA:
                                     DOC_TPPARAGR:DOC_CDESTILO:DOC_TEXTO);
          ENDIF;
       ENDSR;

       //==================================================================
       // Subrotina.: RunPopUp
       // Objectivo.: Adicionar um novo registo de uma Apólice
       //==================================================================
+----  BEGSR RunPopUp;
|+---     IF (ECR_CM#NMFLD = 'TPPARAGR');
             ECR_TPPARAGR = $runPopUp(67:ECR_TPPARAGR);
          // ECR_DSORIENT = $GetDescricao(67:ECR_ICORIENT);
|+---     ELSEIF (ECR_CM#NMFLD = 'CDESTILO');
             ECR_CDESTILO = $runPopUp(68:ECR_CDESTILO);
          // ECR_DSFORMAT = $GetDescricao(68:%CHAR(ECR_ICFORMAT));
          ENDIF;
|         ECR_CM#ROW = CM#CROW;
|         ECR_CM#COL = CM#CCOL;
+----  ENDSR;
      /END-FREE
     PMntParagrafo     E
