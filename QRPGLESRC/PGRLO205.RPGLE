     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PGRLO205
     H* Descrição.: Exportar infiormação para Excel
     H*==================================================================
     FFGRLP200  IF   E           K DISK    PREFIX(GRL_)
     FFGRLP204  IF   E           K DISK    PREFIX(GRL_)
     FFGRLP205  IF   E           K DISK    PREFIX(GRL_)
     FFGRLP206  IF   E           K DISK    PREFIX(GRL_)
     FFGRLP207  UF A E           K DISK    PREFIX(SET_)
     FTMPFILE   UF A F  400        DISK    USROPN EXTFILE(WFileName)
     FFGRLE205  CF   E             WORKSTN SFILE(RGRLE20502:REC#)
     F                                     PREFIX(ECR_)
     F                                     INFDS(INFD#KEYS)
     F*==========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SGRLO013
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SSECO010

     D*==> Registo a inserir no ficheiro
     D DS#VALOR        DS
     D   WValor                1    400
     D
     D*==> Lista de valores por defeito
     D LST#FIELD       DS                  QUALIFIED DIM(100)
     D  NuSequ                        5S 0 INZ(*ZEROS)
     D  DesField                     50A   INZ(*BLANKS)
     D  NomField                     15A   INZ(*BLANKS)
     D  ValDef                        1A   INZ(*BLANKS)
     D  Tabela                        5S 0 INZ(*ZEROS)
     D  IsEmpty                        N   INZ(*ON)
     D WIdx            S              3S 0
     D
     D*==> Parâmetros de Entrada
     D PCodSessao      S              5S 0
     D PRecordKey      S             10A
     D WMsgErro        S              7A
     D
     D*==> Campos de Trabalho
     D WNoFunction     S             10A
     D WFileName       S             10A
     D WPCName         S            100A
     D WCodSessao      S              5S 0
     D WCodTransf      S              5S 0
     D WRecordKey      S             10A
     D WSessDBTmp      S              5S 0
     D WSessDBSet      S              5S 0
     D WCdGrupo        S              2A
     D WCriteria       S            300A
     D WFieldLst       S            300A
     D WRec#           S              4S 0
     D WLimite         S              4S 0
     D REC#            S              4S 0
     C*========================================================
     C     *ENTRY        PLIST
     C                   PARM                    PCodSessao
     C                   PARM                    PRecordKey
     C                   PARM                    WMsgErro
      /FREE
       //----------------------------------------------------
       // validar Parametros Recebidos
       //----------------------------------------------------
       //MONITOR;
       //   WMsgErro = *BLANKS;
       //ON-ERROR;
       // RETURN;
       //ENDMON;
       MONITOR;
          WCodSessao = PCodSessao;
       ON-ERROR;
       //   WCodSessao = 1;
       RETURN;
       ENDMON;
       MONITOR;
          WRecordKey = PRecordKey;
       ON-ERROR;
       //   WRecordKey = '029901847';
       RETURN;
       ENDMON;
       IF (WRecordKey = *BLANKS);
          RETURN;
       ENDIF;

       //-------------------------------------------
       // Validar Acesso à função
       //-------------------------------------------
       IF NOT $ValAccess(S#PGM);
          *INLR = *ON;
          RETURN;
       ENDIF;

       //-------------------------------------------
       // Processar pedido
       //-------------------------------------------
       EXSR SetSession;
       IF (WMsgErro <> *BLANKS);
          RETURN;
       ENDIF;
       EXSR ShowScreen;

       $RmvDBSession(WSessDBTmp);
       $RmvDBSession(WSessDBSet);
       *INLR = *ON;

       //=======================================================
       // Subrotina..: GetSession
       // Objectivo..: Obter a informação sobre a Sessão
       //=======================================================
       BEGSR SetSession;
          CHAIN (WCodSessao) RGRLP204;
          IF (NOT %FOUND());
             WMsgErro = 'PRDXXX1';
             LEAVESR;
          ENDIF;
          WCodTransf = GRL_CDTEMPL;

          //----------------------------------------------------
          // Construir o Criterio de Pesquisa
          //----------------------------------------------------
          WCriteria = GRL_CRITER;
          SETLL (WCodSessao) RGRLP205;
          READE (WCodSessao) RGRLP205;
          DOW (NOT %EOF(FGRLP205));
             MONITOR;
                WCriteria = $StrReplace(GRL_VARNAME:
                            %SUBST(WRecordKey:GRL_POSINI:GRL_QTDE):WCriteria);
             ON-ERROR;
                WMsgErro = 'PRDXXX2';
                LEAVESR;
             ENDMON;
             READE (WCodSessao) RGRLP205;
          ENDDO;

          //----------------------------------------------------
          // Construir a Query com os registos a seleccionar
          //----------------------------------------------------
          WSessDBTmp = $SetFiles(GRL_FILES);
          $SetCriteria(WCriteria);

          //----------------------------------------------------
          // Construir a Query para apresentar campos a listar
          //----------------------------------------------------
          WSessDBSet = $SetFiles('FGRLP206');
          $SetCriteria('(CDSESS='''+%CHAR(WCodSessao)+''')');
          $SetLstFld('DSFIELD':'Campo':50:'E');
          $SetSort('CDGRUPO,NUORDEM');
          $SetPage(12);
       ENDSR;

       //=======================================================
       // Subrotina..: ShowScreen
       // Objectivo..: Seleccionar registos a Exportar
       //=======================================================
       BEGSR ShowScreen;
          LST#Field(1).IsEmpty = *ON;
          DOW (NOT *IN03 AND NOT *IN12);
             EXSR getRecords;
             WNoFunction = 'RGRLE20503'; // Listar Cabeçalhos das Listas
             DS#HEADER   = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER    = $getFooter(S#PGM:WNoFunction:*BLANKS);
             DS#OPTIONS   = $getOption(S#PGM:WNoFunction:*BLANKS);
             ECR_S#PGM   = S#PGM;
             ECR_ERR#MSG = *BLANKS;
             WRITE     RGRLE20500;
             WRITE     RGRLE20599;
             WRITE     RGRLE20598;
             EXFMT RGRLE20503;
             IF (Func#Key = KEY#Enter);
             // EXSR SelRecord;
             // *IN12 = *OFF;
             ELSEIF ($ValidKey(Func#Key:KEY#F08));
                EXSR Exportar;
             ELSEIF ((Func#Key=KEY#F12) OR (Func#Key=KEY#F03));
             ELSE;
                EXSR SaveChoices;
             ENDIF;
          ENDDO;
       ENDSR;

       //=======================================================
       // Subrotina..: SaveChoices
       // Objectivo..: Guardar a selecção realizada na lista
       //=======================================================
       BEGSR SaveChoices;
          WLimite = REC#;
          WRec# = 1;
          DOW (WRec# <= WLimite);
             CHAIN WRec# RGRLE20502;
             WIdx = 1;
             DOW (NOT LST#Field(WIdx).IsEmpty);
                IF (LST#Field(WIdx).NuSequ = ECR_NUSEQU);
                   LST#Field(WIdx).ValDef = ECR_OPTION;
                   LEAVE;
                ENDIF;
                WIdx += 1;
             ENDDO;
             WRec# += 1;
          ENDDO;
       ENDSR;

       //=======================================================
       // Subrotina..: GetRecords
       // Objectivo..: Obter os registos a Exportar
       //=======================================================
       BEGSR GetRecords;
          REC#      = *ZEROS;   // Controlo de Registos da Lista
          *In53     = *On;      // Limpar Lista de Ecrã
          *In52     = *On;      // Mostrar a Lista de Ecrã
          WRITE RGRLE20503;     // Limpar Lista de Funções
          *In53     = *Off;     // Repor Indicador
          *In52     = *Off;     // Mostrar a Lista de Ecrã
          WCdGrupo  = *BLANKS;

          IF ($GetPage(Func#Key) > *ZEROS);
             DOW (NOT $IsEOF());
                ECR_OPTION   = *BLANKS;
                ECR_LST#HDR = $GetLstHdr();

                //------------------------------------------------
                // Escreve o Cabeçalho do Grupo da Listagem
                //------------------------------------------------
                IF ($GetField('CDGRUPO') <> WCdGrupo);
                   *IN45 = *ON;
                   ECR_NUSEQU  = *ZEROS;
                   ECR_NOFILE  = *BLANKS;
                   ECR_NOFIELD = *BLANKS;
                   WCdGrupo    = $GetField('CDGRUPO');
                   ECR_LST#DET = $GetDescricao(84:WCdGrupo);
                   REC# += 1;
                   WRITE RGRLE20502;
                   IF (REC# = 12);
                      LEAVE;
                   ENDIF;
                ENDIF;

                //------------------------------------------------
                // Escreve os Registo da Listagem
                //------------------------------------------------
                *IN45 = *OFF;
                ECR_NUSEQU  = $GetFieldNum('NUSEQU');
                ECR_NOFILE  = $GetField('NOFILE');
                ECR_NOFIELD = $GetField('NOFIELD');
                ECR_LST#DET = $GetLstLine();

                WIdx = 1;
                DOW (NOT LST#Field(WIdx).IsEmpty);
                   IF (LST#Field(WIdx).NuSequ = ECR_NUSEQU);
                      ECR_OPTION  = LST#Field(WIdx).ValDef;
                      LEAVE;
                   ENDIF;
                   WIdx += 1;
                ENDDO;
                IF (ECR_OPTION = *BLANKS);
                   ECR_OPTION = $GetField('ICDEFT');
                   IF (ECR_OPTION = *BLANKS);
                      ECR_OPTION  = 'N';
                   ENDIF;
                   LST#Field(WIdx).ValDef    = ECR_OPTION;
                   LST#Field(WIdx).NomField  = ECR_NOFIELD;
                   LST#Field(WIdx).DesField  = $GetField('LBFIELD');
                   LST#Field(Widx).Tabela    = $GetFieldNum('NUTABL');
                   LST#Field(WIdx).NuSequ    = ECR_NUSEQU;
                   LST#Field(WIdx).IsEmpty   = *OFF;
                   LST#Field(WIdx+1).IsEmpty = *ON;
                ENDIF;

                REC# += 1;
                WRITE RGRLE20502;
                *IN52 = *ON;       // Encontrou registos
                IF (REC# = 12);
                   LEAVE;
                ENDIF;
                $getNext();
             ENDDO;
          ENDIF;
          *IN54 = $IsLastPage();   // Indicação de Fim de Página
       ENDSR;

       //=======================================================
       // Subrotina..: Exportar
       // Objectivo..: Exportar a listagem
       //=======================================================
       BEGSR Exportar;
          EXSR SaveChoices;
          WFileName = 'TMPFILE';
          WMsgErro  = $CrtFile(WFileName:400);
          IF (WMsgErro <> *BLANKS);
             LEAVESR;
          ENDIF;

          //---------------------------------------------------
          // Construir Lista de Campos a Processar
          //---------------------------------------------------
          WFieldLst = *BLANKS;
          WIdx = 1;
          DOW (NOT LST#Field(WIdx).IsEmpty);
             IF (Lst#Field(WIdx).ValDef = 'S');
                WFieldLst = %TRIM(WFieldLst)+%TRIM(LST#Field(Widx).NomField)+
                           ',';
             ENDIF;
             Widx += 1;
          ENDDO;
          %SUBST(WFieldLst:%LEN(%TRIM(WFieldLst)):1) = *BLANKS;
          $SetDBSession(WSessDBTmp);
          $SetFields(WFieldLst);

          //---------------------------------------------------
          // Carregar listagem com os registos a enviar
          //---------------------------------------------------
          OPEN TMPFILE;
          WRec# = 1;
          DOW ($GetRecord(WRec#) > *ZEROS);
             //-----------------------------------------
             // Escreve o Cabeçalho da Listagem
             //-----------------------------------------
             IF (WRec#=1);
                WValor = *BLANKS;
                WIdx = 1;
                DOW (NOT LST#Field(WIdx).IsEmpty);
                   IF (Lst#Field(WIdx).ValDef = 'S');
                      WValor = %TRIM(WValor)+
                               %TRIM(Lst#Field(WIdx).DesField)+';';
                      IF (Lst#Field(WIdx).Tabela > *ZEROS);
                         WValor = %TRIM(WValor)+'Descrição;';
                      ENDIF;
                   ENDIF;
                   WIdx += 1;
                ENDDO;
                %SUBST(WFieldLst:%LEN(%TRIM(WValor)):1) = *BLANKS;
                WRITE TMPFILE DS#VALOR;
             ENDIF;

             //-----------------------------------------
             // Escreve o Corpo da Listagem
             //-----------------------------------------
             WValor = *BLANKS;
             WIdx   = 1;
             DOW (NOT LST#Field(WIdx).IsEmpty);
                IF (Lst#Field(WIdx).ValDef = 'S');
                   WValor = %TRIM(WValor)+
                            %TRIM($GetField(Lst#Field(WIdx).NomField))+';';
                   IF (Lst#Field(WIdx).Tabela > *ZEROS);
                      WValor = %TRIM(WValor)+$GetDescricao(
                               Lst#Field(Widx).Tabela:
                               %TRIM($GetField(Lst#Field(WIdx).NomField)))+';';
                   ENDIF;
                ENDIF;
                WIdx += 1;
             ENDDO;
             %SUBST(WFieldLst:%LEN(%TRIM(WValor)):1) = *BLANKS;
             WRITE TMPFILE DS#VALOR;
             WRec# += 1;
          ENDDO;
          CLOSE TMPFILE;

          //---------------------------------------------------
          // Enviar os ficheros para o servidor e abrir
          //---------------------------------------------------
          WPcName = %TRIM(WRecordKey)+'.csv';
          IF ($SendFTP(WCodTransf:'QTEMP':WFileName:%TRIM(WPcName)) <> *BLANKS);
             $ShowDialog('PRD0156');
          ELSE;
             MONITOR;
                Chain (WCodTransf) RGRLP200;
                WPcName = '\\'+%TRIM(GRL_IPSERVER)+'\'+
                           %TRIM($StrReplace('/':'\':%TRIM(GRL_NoPasta)))+
                           %TRIM(WPcName);
                $ShellExec('strpco');
                $ShellExec('strpccmd pccmd('''+
                                    %TRIM(WPcName)+''') pause(*NO)');
             ON-ERROR;
             ENDMON;
             $ShowDialog('PRD0157');
          ENDIF;
          $SetDBSession(WSessDBSet);
       ENDSR;
      /END-FREE
