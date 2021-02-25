     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PDOCE005
     H* Descrição.: Manutenção de Páginas do Formulário
     H*==================================================================
     FFDOCP015  UF A E           K DISK    PREFIX(DOC_)
     FFDOCP016  UF A E           K DISK    PREFIX(DOC_)
     FFDOCP017  UF A E           K DISK    PREFIX(DOC_)
     FFDOCE005  CF   E             WORKSTN SFILE(RDOCE00502:REC#)
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
     D
     D*==> Estrutura de dados do Registo
     D WIcErro         S               N   INZ(*OFF)
     D WCdMessage      S              7A   INZ(*BLANKS)
     D WIcOrigem       S              1A
     D REC#            S              4S 0
     D
     D WCdDocum        S              5S 0
     D WDsDocum        S             50A
     D WFileName       S             10A
     D
     D KTE#Tabela      S              5S 0
     C*========================================================
     C     *ENTRY        PLISt
     C                   PARM                    WCdDocum
     C                   PARM                    WDsDocum
     C                   PARM                    WFileName
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
       KTE#Tabela = $AddDBSession();
       $SETFILES('FDOCP015');
       $SETFIELDS('DOC01501,FILENAME,JOBNAME,STATUS,PAGES,SNDSTS,+
                   CHAR(DATE),STATUS');
       $addCriteria('User.......:':'USER':'LK':*ON);
       $addCriteria('Date.......:':'DATE':'GE':*ON);
       ECR_DSCRITER = $getCritText();
       $SetCriteria('(FILENAME = ''' + %TRIM(WFileName) + ''')');
       $SetSort('DATE #DESC');
       $SetPage(12);

       MntParagrafo(WCdDocum:WDsDocum);

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
     D
     D WIcDisplay      S               N   INZ(*ON)
     D WNoFunction     S             10A   INZ(*BLANKS)
     D WIsEmpty        S               N
     D WSelected       S               N
     D
     D*===================================================================
      /FREE

       WIcDisplay =*ON;
       ECR_CdDocum = PCdDocumento;
       ECR_DsDocum = PDsDocumento;
       DOW (NOT *IN03 AND NOT *IN12);
          EXSR inzRecords;
          EXSR getRecords;
          WNoFunction = 'RDOCE00503'; // Listar Componentes
          DS#HEADER   = $getHeader(S#PGM:WNoFunction);
          DS#FOOTER   = $getFooter(S#PGM:WNoFunction:*BLANKS);
          ECR_S#PGM   = S#PGM;
          ECR_ERR#MSG = *BLANKS;
          WRITE     RDOCE00500;
          WRITE     RDOCE00599;
          WRITE     RDOCE00598;
          EXFMT RDOCE00503;
          IF (Func#Key = KEY#F05);
             $ChgCriteria();
             ECR_DSCRITER = $getCritText();
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
          WRITE RDOCE00503;     // Limpar Lista de Funções
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
                %SUBST(ECR_LSTHDR:1:10)='Sequencia';
                %SUBST(ECR_LSTDET:1:5)= $GetField('DOC01501');
                %SUBST(ECR_LSTHDR:11:10)='Trabalho';
                %SUBST(ECR_LSTDET:11:10)= $GetField('JOBNAME');
                %SUBST(ECR_LSTHDR:22:10)='Páginas';
                %SUBST(ECR_LSTDET:22:10)= $GetField('PAGES');
                %SUBST(ECR_LSTHDR:31:10)='Enviado';
                %SUBST(ECR_LSTDET:31:10)= $GetField('SNDSTS');
                %SUBST(ECR_LSTHDR:41:10)='Data';
                %SUBST(ECR_LSTDET:41:10)= $GetField('CHAR(DATE)');
                MONITOR;
                   ECR_DOC01501 = %DEC($GetField('DOC01501'):11:0);
                ON-ERROR;
                ENDMON;
                REC# += 1;
                WRITE RDOCE00502;
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
          READC RDOCE00502;
          DOW (NOT %EOF());
             IF (ECR_OPTION <> *BLANKS);
                *IN45 = *OFF;
                WIcDisplay = *ON;
                EXSR DspFormulario;
                LEAVESR;
             ENDIF;
             READC RDOCE00502;
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
          CHAIN(N) (ECR_DOC01501) RDOCP015;
          IF (%FOUND());
             ECR_FILENAME = DOC_FILENAME;
             ECR_SPOOL#   = DOC_SPOOL#;
             ECR_JOB#     = DOC_JOB#;
             ECR_TYPE     = DOC_TYPE;
             ECR_USER     = DOC_USER;
             ECR_PAGES    = DOC_PAGES;
             ECR_STATUS   = DOC_STATUS;
             ECR_DATA     = DOC_DATE;
             ECR_HORA     = DOC_HOUR;
             ECR_JOBNAME  = DOC_JOBNAME;
             ECR_HORSTS   = DOC_HORSTS;
             ECR_DATSTS   = DOC_DATSTS;
             IF (DOC_SNDSTS = 'S');
                ECR_DSSNDSTS = 'Enviado';
             ELSE;
                ECR_DSSNDSTS = 'Não enviado';
             ENDIF;
             //----------------------------------
             // Obter o nome da fila de Outq
             //----------------------------------
             CHAIN (DOC_CODFILA) RDOCP017;
             IF (%FOUND());
                ECR_OUTQ = DOC_NOMFILA;
             ELSE;
                ECR_OUTQ = 'Desconhecida';
             ENDIF;
             //----------------------------------
             // Obter o nome do servidor
             //----------------------------------
             CHAIN (DOC_CODSERVER) RDOCP016;
             IF (%FOUND());
                ECR_SRVNAME = DOC_DESSERVER;
             ELSE;
                ECR_SRVNAME = 'Desconhecida';
             ENDIF;
          ELSE;
             LEAVESR;
          ENDIF;
          DOW NOT (*IN03 OR *IN12);
             WNoFunction = 'RDOCE00505'; // Mostrar o registo
             DS#HEADER   = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER   = $getFooter(S#PGM:WNoFunction:*BLANKS);
             ECR_S#PGM   = S#PGM;
             WRITE     RDOCE00500;
             WRITE     RDOCE00599;
             EXFMT RDOCE00505;
             IF (Func#Key=KEY#F08) AND (WIcDisplay);
             // MntTabelas(ECR_CDTABELA:ECR_DSINDICE);
                $SetDBSession(KTE#Tabela);
                *IN12 = *OFF;
                *IN45 = *OFF;
                WIcDisplay = *ON;
             ELSEIF (Func#Key = KEY#F05);
               $ShellExec('DSPSPLF FILE('+%TRIM(ECR_FILENAME)+') JOB('+
                           %TRIM(ECR_JOB#)+'/'+%TRIM(ECR_USER)+'/'+
                           %TRIM(ECR_JOBNAME)+') SPLNBR('+
                           %CHAR(ECR_SPOOL#)+')');
             ELSEIF (Func#Key=KEY#F14) AND (WIcDisplay);
             // EXSR DspTexto;
             ELSEIF ($ValidKey(Func#Key:KEY#F21)) AND (WIcDisplay);
                WIcDisplay = *OFF;
                *IN45 = *ON;
             ELSEIF ($ValidKey(Func#Key:KEY#F22)) AND (WIcDisplay);
                ECR_OPTION = 'N';
                EXFMT RDOCE00522;
                IF (Func#Key = KEY#Enter) AND (ECR_OPTION = 'S');
                   LEAVESR;
                ENDIF;
                *IN12 = *OFF;
             ELSEIF (Func#Key = KEY#ENTER);
                LEAVESR;
             ENDIF;
          ENDDO;
          *IN12 = *OFF;
       ENDSR;

      /END-FREE
     PMntParagrafo     E
