     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: TGRLT000
     H* Descrição.: TEMPLATE - Consulta em Lista
     H*==================================================================
     FFGRLT000  CF   E             WORKSTN SFILE(RGRLE00002:REC#)
     F                                     PREFIX(ECR_)
     F                                     INFDS(INFD#KEYS)
     F*==========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SGRLO013
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SSECO010

     D*==> Guardar opções seleccionadas
     D LST#FIELD       DS                  QUALIFIED DIM(100)
     D  NuSequ                        5S 0 INZ(*ZEROS)
     D  NuSess                        5S 0 INZ(*ZEROS)
     D  ValDef                        2A   INZ(*BLANKS)
     D  IsEmpty                        N   INZ(*ON)
     D WIdx            S              3S 0
     D
     D*==> Parâmetros de Entrada
     D PCodSessao      S              5S 0
     D PRecordKey      S             10A
     D WMsgErro        S              7A
     D
     D*==> Campos de Trabalho
     D WCriteria       S            300A
     D WNoFunction     S             10A
     D WOvrDefKeys     S             24A
     D WRecordKey      S             10A
     D WSessDB001      S              5S 0
     D WSessDB002      S              5S 0
     D WBreakField     S             15A
     D WBreakValue     S             15A
     D WRec#           S              4S 0
     D WLimite         S              4S 0
     D REC#            S              4S 0
     C*========================================================
     C*--- *ENTRY        PLIST
     C*---              PARM                    PCodSessao
      /FREE
       //----------------------------------------------------
       // validar Parametros Recebidos
       //----------------------------------------------------
       MONITOR;
       ON-ERROR;
           RETURN;
       ENDMON;

       //-------------------------------------------
       // Validar Acesso à função
       //-------------------------------------------
       IF NOT $ValAccess(S#PGM);
          *INLR = *ON;
          RETURN;
       ENDIF;

       //-------------------------------------------
       // Sessão de Base de Dados
       //-------------------------------------------
       WSessDB001 = $SetFiles('<Ficheiro>');
       $addCriteria('<Label>....:':'<Campo>':'LK');
       $addCriteria('<Label>....:':'<Campo>':'GE');
       $addCriteria('<Label>....:':'<Campo>':'GE');
       $SetLstFld('<Campo>':'<Coluna>':01:'<Alinhamento>');
       $SetLstFld('<Campo>':'<Coluna>':01:'<Alinhamento>');
       $SetLstFld('<Campo>':'<Coluna>':01:'<Alinhamento>');
       $SetPage(12);

       //----------------------------------------------
       // Sessão de Base de Dados
       //----------------------------------------------
       WSessDB002 = $SETFILES('FTBLP012');
       $addCriteria('<Label>....:':'<Campo>':'LK');
       $addCriteria('<Label>....:':'<Campo>':'GE');
       $addCriteria('<Label>....:':'<Campo>':'GE');
       $SetLstFld('<Campo>':'<Coluna>':01:'<Alinhamento>');
       $SetLstFld('<Campo>':'<Coluna>':01:'<Alinhamento>');
       $SetLstFld('<Campo>':'<Coluna>':01:'<Alinhamento>');
       $SetPage(12);

       $SetDBSession(WsessDB001);
       ECR_LB#CRIT = $getCritText();
       ECR_VL#CRIT = *BLANKS;

       WBreakField = '<FieldName>';
       WSessDB001 = $SetFiles('<File>');
       $SetCriteria(WCriteria);

       //-------------------------------------------
       // Processar pedido
       //-------------------------------------------
       EXSR ShowScreen;

       $RmvDBSession(WSessDB001);
       $RmvDBSession(WSessDB002);
       *INLR = *ON;

       //=======================================================
       // Subrotina..: ShowScreen
       // Objectivo..: Seleccionar registos a Exportar
       //=======================================================
       BEGSR ShowScreen;
          LST#Field(1).IsEmpty = *ON;
          DOW (NOT *IN03 AND NOT *IN12);
             EXSR getRecords;
             WNoFunction = 'RGRLE00003'; // Listar Cabeçalhos das Listas
             WOvrDefKeys = *BLANKS;
             DS#HEADER   = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER   = $getFooter(S#PGM:WNoFunction:WOvrDefKeys);
             DS#OPTIONS  = $getOption(S#PGM:WNoFunction:WOvrDefKeys);
             ECR_S#PGM   = S#PGM;
             ECR_ERR#MSG = *BLANKS;
             WRITE     RGRLE00000;
             WRITE     RGRLE00099;
             WRITE     RGRLE00098;
             EXFMT RGRLE00003;
             IF ($ValidKey(Func#Key:KEY#F05));
             ELSEIF ($ValidKey(Func#Key:KEY#F06));
             ELSEIF ($ValidKey(Func#Key:KEY#F07));
             ELSEIF ($ValidKey(Func#Key:KEY#F08));
             ELSEIF ($ValidKey(Func#Key:KEY#F09));
             ELSEIF ($ValidKey(Func#Key:KEY#F10));
             ELSEIF ((Func#Key=KEY#F12) OR (Func#Key=KEY#F03));
             ELSEIF ((Func#Key=KEY#PgUp) OR (Func#Key=KEY#PgDown));
                EXSR SaveChoices;
             ELSEIF (Func#Key = KEY#Enter);
                EXSR SelRecord;
                *IN12 = *OFF;
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
             CHAIN WRec# RGRLE00002;
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
          WRITE RGRLE00003;     // Limpar Lista de Funções
          *In53     = *Off;     // Repor Indicador
          *In52     = *Off;     // Mostrar a Lista de Ecrã
          WBreakValue = *BLANKS;

          IF ($GetPage(Func#Key) > *ZEROS);
             DOW (NOT $IsEOF());
                ECR_OPTION   = *BLANKS;
                ECR_LST#HDR = $GetLstHdr();

                //------------------------------------------------
                // Escreve o Cabeçalho do Grupo da Listagem
                //------------------------------------------------
                IF ($GetField(WBreaKField) <> WBreakValue);
                   *IN45 = *ON;
                   ECR_NUSEQU  = *ZEROS;
                   ECR_NOFILE  = *BLANKS;
                   ECR_NOFIELD = *BLANKS;
                   WBreakValue = $GetField(WBreakField);
                   ECR_LST#DET = $GetDescricao(84:WBreakValue);
                   REC# += 1;
                   WRITE RGRLE00002;
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
                WRITE RGRLE00002;
                *IN52 = *ON;       // Encontrou registos
                IF (REC# = 12);
                   LEAVE;
                ENDIF;
                $getNext();
             ENDDO;
          ENDIF;
          *IN54 = $IsLastPage();   // Indicação de Fim de Página
       ENDSR;

       //========================================================
       // Subrotina.: SelRecord
       // Objectivo.: Seleccionar os registos
       //========================================================
       BEGSR SelRecord;
          WSelected = *OFF;
          READC RTBLE00102;
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
             READC RTBLE00102;
          ENDDO;
          IF (NOT WSelected AND WIcDisplay);
             IF (ECR_CRITERIA <> *BLANKS);
                $SetCriteria(ECR_CRITERIA:*ON);
             ELSE;
                $SetCriteria(*BLANKS:*ON);
             ENDIF;
          ENDIF;
       ENDSR;
      /END-FREE
