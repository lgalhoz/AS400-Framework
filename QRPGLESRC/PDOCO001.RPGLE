     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PDOCE001
     H* Descrição.: Manutenção de Formulários
     H*==================================================================
     FFDOCP001  UF A E           K DISK    PREFIX(DOC_)
     FFDOCE001  CF   E             WORKSTN SFILE(RDOCE00102:REC#)
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

     D*==> Ficheiros de Spool gerados no AS/400
     D DspSpoolFile    PR                  EXTPGM('PDOCO005')
     D   PCdDocumento                 5S 0 CONST
     D   PDsDocumento                50A   CONST
     D   PNmDocumento                10A   CONST
     D
     D*==> Manutenção das Páginas
     D MntPaginas      PR                  EXTPGM('PDOCO002')
     D   PCdDocumento                 5S 0 CONST
     D   PDsDocumento                50A   CONST
     D
     D*==> Estrutura dos formulários
     D MntConfigura    PR                  EXTPGM('PDOCO004')
     D   PCdDocumento                 5S 0 CONST
     D   PDsDocumento                50A   CONST
     D
     D*==> Estrutura dos formulários
     D MntFormulario   PR
     D   PCdDocumento                 5S 0 CONST
     D   PDsDocumento                50A   CONST
     D
     D*==> Estrutura de dados do Registo
     D WIcErro         S               N   INZ(*OFF)
     D WCdMessage      S              7A   INZ(*BLANKS)
     D WIcOrigem       S              1A
     D REC#            S              4S 0
     D
     D KTE#Tabela      S              5S 0
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
       KTE#Tabela = $SETFILES('FDOCP001');
       $addCriteria('Nome.......:':'NMDOCUM':'GE');
       $addCriteria('Descrição..:':'DSDOCUM':'LK');
       $addCriteria('Tipo.......:':'TPDOCUM':'GE');
       ECR_DSCRITER = $getCritText();
       $SetPage(12);

       MntFormulario(*ZEROS:*BLANKS);

       $RmvDBSession(KTE#Tabela);
       *INLR = *ON;
       RETURN;
      /END-FREE

     P*===================================================================
     P* Processo..: MntFormulário
     P* Descrição.: Realizar a manutenção dos Formulário.
     P*===================================================================
     P MntFormulario   B                   EXPORT
     D MntFormulario   PI
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

       DOW (NOT *IN03 AND NOT *IN12);
          EXSR inzRecords;
          EXSR getRecords;
          WNoFunction = 'RDOCE00103'; // Listar Componentes
          DS#HEADER   = $getHeader(S#PGM:WNoFunction);
          DS#FOOTER   = $getFooter(S#PGM:WNoFunction:*BLANKS);
          ECR_S#PGM   = S#PGM;
          ECR_ERR#MSG = *BLANKS;
          WRITE     RDOCE00100;
          WRITE     RDOCE00199;
          WRITE     RDOCE00198;
          EXFMT RDOCE00103;
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
          WRITE RDOCE00103;     // Limpar Lista de Funções
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
                %SUBST(ECR_LSTHDR:1:10)='Nome';
                %SUBST(ECR_LSTDET:1:10) =$GetField('NMDOCUM');
                %SUBST(ECR_LSTHDR:11:10)='Descrição';
                %SUBST(ECR_LSTDET:11:40) =$GetField('DSDOCUM');
                %SUBST(ECR_LSTHDR:52:4)='Tipo';
                %SUBST(ECR_LSTDET:52:3)=$GetField('TPDOCUM');
                MONITOR;
                   ECR_CDDOCUM = %DEC($GetField('CDDOCUM'):5:0);
                ON-ERROR;
                ENDMON;
                REC# += 1;
                WRITE RDOCE00102;
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
          READC RDOCE00102;
          DOW (NOT %EOF());
             IF (ECR_OPTION <> *BLANKS);
                *IN45 = *OFF;
                WIcDisplay = *ON;
                EXSR DspFormulario;
                LEAVESR;
             ENDIF;
             READC RDOCE00102;
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
             CHAIN(N) (ECR_CDDOCUM) RDOCP001;
             IF (%FOUND());
                ECR_CDDOCUM = DOC_CDDOCUM;
                ECR_DSDOCUM = DOC_DSDOCUM;
                ECR_NMDOCUM = DOC_NMDOCUM;
                ECR_NMABREV = DOC_NMABREV;
                ECR_TPDOCUM = DOC_TPDOCUM;
                ECR_DSTPDOC = $GetDescricao(66:ECR_TPDOCUM);
                ECR_FMTDOC  = DOC_FMTDOC;
                ECR_PARSER  = DOC_PARSER;
                ECR_DESTINO = DOC_DESTINO;
                ECR_CDATRIB = DOC_CDATRIB;
                ECR_ICLOCKED= DOC_ICLOCKED;
                ECR_ICTESTES= DOC_ICTESTES;
                ECR_DOC00101= DOC_DOC00101;
                ECR_DOC00102= DOC_DOC00102;
             ELSE;
                LEAVESR;
             ENDIF;
          ELSE;
             *IN45 = *ON;
             ECR_CDDOCUM = *ZEROS;
             ECR_DSDOCUM = *BLANKS;
             ECR_NMDOCUM = *BLANKS;
             ECR_NMABREV = *BLANKS;
             ECR_TPDOCUM = *BLANKS;
             ECR_FMTDOC  = '.PDF';
             ECR_PARSER  = *BLANKS;
             ECR_DESTINO = *BLANKS;
             ECR_CDATRIB = *ZEROS;
             ECR_ICLOCKED= 'N';
             ECR_ICTESTES= 'N';
          ENDIF;
          DOW NOT (*IN03 OR *IN12);
             IF ( WIcInsert);
                WNoFunction = 'RDOCE00106'; // Adicionar um registo
             ELSEIF (WIcDisplay);
                WNoFunction = 'RDOCE00105'; // Mostrar o registo
             ELSE;
                WNoFunction = 'RDOCE00121'; // Alterar o Registo
             ENDIF;
             DS#HEADER   = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER   = $getFooter(S#PGM:WNoFunction:*BLANKS);
             ECR_S#PGM   = S#PGM;
             WRITE     RDOCE00100;
             WRITE     RDOCE00199;
             EXFMT RDOCE00105;
             IF (Func#Key=KEY#F04) AND (NOT WIcDisplay);
                EXSR RunPopUp;
             ELSEIF (Func#Key=KEY#F08) AND (WIcDisplay);
                MntPaginas(ECR_CDDOCUM:ECR_DSDOCUM);
                $SetDBSession(KTE#Tabela);
                *IN12 = *OFF;
                *IN45 = *OFF;
                WIcDisplay = *ON;
             ELSEIF (Func#Key=KEY#F10) AND (WIcDisplay);
                MntConfigura(ECR_CDDOCUM:ECR_DSDOCUM);
                $SetDBSession(KTE#Tabela);
                *IN12 = *OFF;
                *IN45 = *OFF;
                WIcDisplay = *ON;
             ELSEIF (Func#Key=KEY#F11) AND (WIcDisplay);
                DspSpoolFile(ECR_CDDOCUM:ECR_DSDOCUM:ECR_NMDOCUM);
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
                EXFMT RDOCE00122;
                IF (Func#Key = KEY#Enter) AND (ECR_OPTION = 'S');
                   DELETE (ECR_CDDOCUM) RDOCP001;
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
          IF (ECR_DSDOCUM = *BLANKS);
             WCdMessage = 'COR0001';
             WIcErro = *ON;
|            ECR_CM#COL = CM#CCOL;
             $getFldLoc('FDOCE001':'RDOCE00105':'DSDOCUM':
                        ECR_CM#ROW:ECR_CM#COL);
          ELSEIF (ECR_TPDOCUM = *BLANKS);
             WCdMessage = 'COR0003';
             WIcErro = *ON;
             $getFldLoc('FDOCE001':'RDOCE00105':'TPDOCUM':
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
          IF (ECR_CDDOCUM = *ZEROS);
             DOC_CDDOCUM  = $getNextKey('FDOCP001':'CDDOCUM');
             DOC_NMDOCUM  = ECR_NMDOCUM;
             DOC_NMABREV  = ECR_NMABREV;
             ECR_CDDOCUM  = DOC_CDDOCUM;
             DOC_DSDOCUM  = ECR_DSDOCUM;
             DOC_TPDOCUM  = ECR_TPDOCUM;
             DOC_FMTDOC   = ECR_FMTDOC;
             DOC_PARSER   = ECR_PARSER;
             DOC_DESTINO  = ECR_DESTINO;
             DOC_CDATRIB  = ECR_CDATRIB;
             DOC_ICLOCKED = ECR_ICLOCKED;
             DOC_ICTESTES = ECR_ICTESTES;
             DOC_DOC00101 = %DATE();
             DOC_DOC00102 = %DATE();
             WRITE RDOCP001;

          //-------------------------------------------
          // Alterar um registo
          //-------------------------------------------
          ELSE;
             CHAIN (ECR_CDDOCUM) RDOCP001;
             DOC_DSDOCUM = ECR_DSDOCUM;
             DOC_TPDOCUM = ECR_TPDOCUM;
             DOC_NMDOCUM = ECR_NMDOCUM;
             DOC_NMABREV = ECR_NMABREV;
             DOC_FMTDOC  = ECR_FMTDOC;
             DOC_PARSER  = ECR_PARSER;
             DOC_DESTINO = ECR_DESTINO;
             DOC_CDATRIB  = ECR_CDATRIB;
             DOC_ICLOCKED = ECR_ICLOCKED;
             DOC_ICTESTES = ECR_ICTESTES;
             DOC_DOC00102 = %DATE();
             UPDATE RDOCP001 %FIELDS(DOC_DSDOCUM:DOC_TPDOCUM:DOC_FMTDOC:
                                     DOC_NMDOCUM:DOC_NMABREV:DOC_PARSER:
                                     DOC_DESTINO:DOC_CDATRIB:DOC_ICLOCKED:
                                     DOC_ICTESTES:DOC_DOC00102);
          ENDIF;
       ENDSR;

       //==================================================================
       // Subrotina.: RunPopUp
       // Objectivo.: Adicionar um novo registo de uma Apólice
       //==================================================================
+----  BEGSR RunPopUp;
|+---     IF (ECR_CM#NMFLD = 'TPDOCUM');
             ECR_TPDOCUM = $runPopUp(66:ECR_TPDOCUM);
             ECR_DSTPDOC = $GetDescricao(66:ECR_TPDOCUM);
          ENDIF;
|         ECR_CM#ROW = CM#CROW;
|         ECR_CM#COL = CM#CCOL;
+----  ENDSR;
      /END-FREE
     PMntFormulario    E
