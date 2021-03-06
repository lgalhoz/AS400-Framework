     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PRDPO010
     H* Descri��o.: Manuen��o de Ap�lices
     H*==================================================================
     FFPRDP002  IF   E           K DISK    PREFIX(PRD_)
     FFPRDE010  CF   E             WORKSTN SFILE(RPRDE01002:REC#)
     F                                     PREFIX(ECR_)
     F                                     INFDS(INFD#KEYS)
     F*==========================================================
     F* Indicadores  Descri��o
     F* -----------  --------------------------------------------
     F*      45      Condicionar Campos com Input/Output
     F*      52      DSP
     F*      53      CLR
     F*      54      END
     D*==========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SSECO010

     D*==> Chamada de fun��o Dynamic, para trabalhar detalhe do Item
     D ShellExec       PR                  EXTPGM('QCMDEXC')
     D   stdcmd                     300A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5 CONST
     D
     D*==> Aceder aos ficheiro de Hist�rico para a Ap�lice
     D AppHistorico    PR                  EXTPGM('RPRDO004')
     D   Ramo                         2S 0 CONST
     D   Apolice                      7S 0 CONST
     D   Lib                         10A   CONST
     D   File                        10A   CONST
     D
     D*==> Aceder aos documentos gerados para a Ap�lice
     D AppDocumento    PR                  EXTPGM('RPRDO006')
     D   Ramo                         2S 0 CONST
     D   Apolice                      7S 0 CONST
     D   Lib                         10A   CONST
     D   File                        10A   CONST
     D
     D*==> Apresentar uma query com os sinistros da Ap�lice
     D AppSinistro     PR                  EXTPGM('RSINO010')
     D   Ramo                         2S 0 CONST
     D   Apolice                      7S 0 CONST
     D
     D*==> Apresentar uma query com os Recibos da Ap�lice
     D AppRecibos      PR                  EXTPGM('PPRDO016')
     D   Ramo                         2S 0 CONST
     D   Apolice                      7S 0 CONST
     D
     D*==> Criar tempor�rios de ap�lices
     D CrtTempor       PR                  EXTPGM('RPRDO001')
     D
     D*==> Marcar Ap�lices Alteradas/Novas
     D MrkApolices     PR                  EXTPGM('RPRDO003')
     D
     D*==> Chamada � Aplica��o auto
     D ShowApolice     PR                  EXTPGM(DynamicCall)
     D   Option                       1A   CONST
     D   RamApol                      9A   CONST
     D   Data                         8S 0 CONST Options(*NOPASS)
     D DynamicCall     S             10A   INZ(*BLANKS)
     D
     D*==> Base Auto (Original)
     D AppAuto         PR                  EXTPGM('GP010X')
     D   Option                       1A   CONST
     D   RamApol                      9A   CONST
     D   Data                         8S 0 CONST Options(*NOPASS)
     D
     D*==> Base Outros Ramos (Original)
     D AppOutros       PR                  EXTPGM('GP015X')
     D   Option                       1A   CONST
     D   RamApol                      9A   CONST
     D   Data                         8S 0 CONST Options(*NOPASS)
     D
     D*==> Estrutura de dados do Registo
     D WIcErro         S               N   INZ(*OFF)
     D WCodErro        S              7A
     D WIcOrigem       S              1A
     D REC#            S              4S 0
     D
     D WIcDisplay      S               N   INZ(*ON)
     D WModo           S               N
     D WNoFunction     S             10A   INZ(*BLANKS)
     D WHideTeclas     S             24A   INZ(*BLANKS)
     D WSelected       S               N
     D WRamoApolice    S              9A
     D WComando        S            300A   INZ(*BLANKS)
     D WOption         S              5S 0 INZ(*ZEROS)
     D WNoFile         S             10A
     D WNoLib          S             10A
     D WWorkLib        S             10A   INZ('QS36F')
     D WSessionID      S              5S 0
     D WIcNovas        S              1A
     C*========================================================
      /FREE

       //------------------------------------
       // Se n�o estiver autorizado ent�o sai
       //------------------------------------
       IF NOT $ValAccess(S#PGM);
          *INLR = *ON;
          RETURN;
       ENDIF;

       //------------------------------------------
       // Preparar ambiente para consultar ap�lices
       //------------------------------------------
       //EXSR CtrTempor;

       WIcNovas = 'N';
       WSessionID = $SETFILES('FPRDT001');

       //-------------------------------------
       // Defini��o dos campos da Listagem
       //-------------------------------------
       // $SetLstFld('CDRAMO':'Rm.':3:'D');
       // $SetLstFld('NUAPOL':'Ap�lice':3:'D');
       // $SetLstFld('NUSEG':'Cod.':1:'D');
       // $SetLstFld('NOSEG':'Segurado':4:'D');
       // $SetLstFld('NUNIF':'NIF':4:'D');
       // $SetLstFld('STATUS':'E.':4:'D');
       // $SetLstFld('MATRIC':'Matricula':4:'C');

       //-------------------------------------
       // Defini��o das op��es de Consulta
       //-------------------------------------
       $SetCriteria(*BLANKS);
       $addCriteria('Ap�lice....:':'NUAPOL':'LK');
       $addCriteria('C�digo Seg.:':'NUSEG':'EQ');
       $addCriteria('Local Risco:':'LCRISK':'LK');
       $addCriteria('NIF........:':'NUNIF':'GE');
       $addCriteria('Matricula..:':'MATRIC':'LK');
       $addCriteria('Apelido....:':'APELIDO':'EQ');
       $addCriteria('Nome.......:':'NOSEG':'LK');
       ECR_DSCRITER = $getCritText();

       $SetPage(12);
       WIcDisplay =*ON;
       EXSR inzRecords;
       EXSR getRecords;

       DOW (NOT *IN03 AND NOT *IN12);
          WNoFunction = 'RPRDE01003'; // Listar Componentes
          WHideTeclas = '000000000000000000011000';
          DS#HEADER    = $getHeader(S#PGM:WNoFunction);
          DS#FOOTER    = $getFooter(S#PGM:WNoFunction:*BLANKS);
          DS#OPTIONS   = $getOption(S#PGM:WNoFunction:*BLANKS);
          ECR_S#PGM    = S#PGM;
          WRITE     RPRDE01000;
          WRITE     RPRDE01099;
          WRITE     RPRDE01098;
          EXFMT RPRDE01003;
          IF (Func#Key = KEY#F05);
             $ChgCriteria();
             ECR_DSCRITER = $getCritText();
             EXSR inzRecords;
             EXSR getRecords;
          ELSEIF (Func#Key = KEY#F07); // Multicriterio de Selec��o
          // DOW (Func#Key<>KEY#Enter) AND (Func#Key<>KEY#F03)
          //    AND (Func#Key<>KEY#F12);
          //    EXFMT RPRDE01007;
|         //    ECR_CM#ROW = (CM#CROW-4);
|         //    ECR_CM#COL = (CM#CCOL-9);
          // ENDDO;
          // *IN12 = *OFF;
          // *IN03 = *OFF;
          // EXSR inzRecords;
          // EXSR getRecords;
          ELSEIF ($ValidKey(Func#Key:KEY#F06));
             EXSR AddNewApolice;
             EXSR inzRecords;
             EXSR getRecords;
          ELSEIF ($ValidKey(Func#Key:KEY#F09));
             // Marcar registos que est�o no carregamento
             Monitor;
             // MrkApolices();
             On-Error;
             EndMon;

             IF (WIcNovas = 'S');
                WIcNovas = 'N';
                $SetCriteria(*BLANKS);
             ELSE;
                WIcNovas = 'S';
                $SetCriteria('ICCARR = ''S''');
             ENDIF;
             EXSR inzRecords;
             EXSR getRecords;
          ELSEIF ((Func#Key = KEY#PgDown) AND (NOT $IsLastPage())) OR
             ((Func#Key = KEY#PgUp) AND ($GetPageNbr() > 1));
             EXSR inzRecords;
             EXSR getRecords;
          ELSEIF (Func#Key = KEY#Enter);
             EXSR SelRec;
             *IN12 = *OFF;
          // EXSR inzRecords;
          // EXSR getRecords;
          ENDIF;
       ENDDO;

       $RmvDBSession(WSessionID);
       *INLR = *ON;
       RETURN;

       //========================================================
       // Subrotina.: OpenDiario
       // Objectivo.: Abrir Ficheiro di�rio em fun��o do Base
       //========================================================
       BEGSR OpenDiario;
          //--------------------------------------------------
          // Se o base estiver fechado ent�o usar temporarios
          //--------------------------------------------------
          MONITOR;
             IF ($checkObject('GP010A':WWorkLib:'*FILE')); // GP010A
                WComando = 'OVRDBF FILE(GBASEDIA) TOFILE(SEXTRAS/JBASEDIA)';
   29           ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
             ELSE;
                WComando = 'OVRDBF FILE(GBASEDIA) +
                           TOFILE('+%TRIM(WWorkLib)+'/GBASEDIA)';
   29           ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
             ENDIF;

             IF ($checkObject('GP015A':WWorkLib:'*FILE')); // GP015A
                WComando = 'OVRDBF FILE(GBASEDIO) TOFILE(SEXTRAS/JBASEDIO)';
   29           ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
             ELSE;
                WComando = 'OVRDBF FILE(GBASEDIO) +
                            TOFILE('+%TRIM(WWorkLib)+'/GBASEDIO)';
   29           ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
             ENDIF;
          ON-ERROR;
          ENDMON;
       ENDSR;

       //========================================================
       // Subrotina.: AddNewApolice
       // Objectivo.: Adicionar uma nova Ap�lice
       //========================================================
       BEGSR AddNewApolice;
          DOW (NOT *IN03) AND (NOT *IN12);
             WNoFunction = 'RPRDE01006';
             ECR_Ramo    = *ZEROS;
             DS#HEADER   = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER   = $getFooter(S#PGM:WNoFunction:*BLANKS);
             ECR_S#PGM   = S#PGM;
             WRITE     RPRDE01000;
             WRITE     RPRDE01099;
             WRITE     RPRDE01098;
             EXFMT RPRDE01006;
             IF (Func#Key = KEY#Enter);
                IF ($ValidKey(KEY#F06:KEY#F06:%CHAR(ECR_RAMO):
                              S#PGM:'RPRDE01003'));
                   WIcErro = *OFF;
                   //----------------------------------------------
                   // Validar em Cat�lago se C�digo de Ramo valido
                   //----------------------------------------------
                   //----------------------------------------------
                   // Validar se o Base est� fechado
                   //----------------------------------------------
                   IF (ECR_RAMO = 50) OR (ECR_RAMO = 51);
                      IF ($checkObject('GP010A':WWorkLib:'*FILE')); // GP010A
                         WIcErro  = *ON;
                         WCodErro = 'PRD0093';
                      ENDIF;
                   ELSE;
                      IF ($checkObject('GP015A':WWorkLib:'*FILE')); // GP015A
                         WIcErro  = *ON;
                         WCodErro = 'PRD0094';
                      ENDIF;
                   ENDIF;

                   //------------------------------------------------
                   // Se N�o existirem Erros ent�o
                   //------------------------------------------------
                   IF (NOT WICErro);
                   MONITOR;
                      WRamoApolice = %TRIM(%EDITW(ECR_RAMO:'0  '))+'       ';
                      IF (ECR_RAMO = 50) OR (ECR_RAMO = 51);
                         AppAuto('1':WRamoApolice);
                      ELSE;
                         AppOutros('1':WRamoApolice);
                      ENDIF;
                      LEAVE;
                   ON-ERROR;
                      WIcErro  = *ON;
                      WCodErro = 'MSG0095';
                   ENDMON;
                   ENDIF;
                ELSE;
                   WIcErro  = *ON;
                   WCodErro = 'MSG0096';
                ENDIF;

                //------------------------------------------------
                // Se foram identifcados Erros ent�o
                //------------------------------------------------
                IF (WIcErro);
                   IF (WCodErro = 'PRD0093');
                      ECR_MSG001 = $AlignText('Esta op��o n�o � v�lida'
                                   :49:'C');
                      ECR_MSG002 = $AlignText('O Base autom�vel est� +
                                   fechado':49:'C');
                   ELSEIF (WCodErro = 'PRD0094');
                      ECR_MSG001 = $AlignText('Esta op��o n�o � v�lida'
                                   :49:'C');
                      ECR_MSG002 = $AlignText('O Base outros ramos est� +
                                   fechado':49:'C');
                   ELSEIF (WCodErro = 'PRD0095');
                      ECR_MSG001 = $AlignText('Forma detecados erros...'
                                   :49:'C');
                      ECR_MSG002 = $AlignText('Contacte a Inform�tica'
                                   :49:'C');
                   ELSEIF (WCodErro = 'PRD0096');
                      ECR_MSG001 = $AlignText('N�o est� autorizado'
                                   :49:'C');
                      ECR_MSG002 = $AlignText('Contacte a Inform�tica'
                                   :49:'C');
                   ENDIF;
                   EXFMT RPRDE01010;
                ENDIF;
             ENDIF;
          ENDDO;
          *IN12 = *OFF;
       ENDSR;

       //========================================================
       // Subrotina.: InzRecTxt
       // Objectivo.: Inicializar a lista de registos
       //========================================================
       BEGSR inzRecords;
          REC#      = *ZEROS;   // Controlo de Registos da Lista
          *In53     = *On;      // Limpar Lista de Ecr�
          *In52     = *On;      // Mostrar a Lista de Ecr�
          WRITE RPRDE01003;     // Limpar Lista
          *In53     = *Off;     // Repor Indicador
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
                MONITOR;
                   IF ($GetField('ICCARR') = 'S');
                      *IN45 = *ON;
                   ELSE;
                      *IN45 = *OFF;
                   ENDIF;
                   ECR_RAMO     = $GetFieldNum('CDRAMO');
                   ECR_APOLICE  = $GetFieldNum('NUAPOL');
                   ECR_NUSEG    = $GetFieldNum('NUSEG');
                   ECR_NIF      = $GetFieldNum('NUNIF');
                   ECR_STATUS   = $GetFieldNum('STATUS');
                ON-ERROR;
                ENDMON;
                CHAIN (ECR_RAMO:ECR_APOLICE) RPRDP002;
                IF (%FOUND());
                   ECR_ICMEMO = 'S';
                ELSE;
                   ECR_ICMEMO = 'N';
                ENDIF;
                ECR_SEGURADO = $GetField('NOSEG');
                IF (ECR_RAMO <> 50);
                   ECR_MATRICULA= $GetField('LCRISK');
                ELSE;
                   ECR_MATRICULA= $GetField('MATRIC');
                ENDIF;

                REC# += 1;
                WRITE RPRDE01002;
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
          READC RPRDE01002;
          DOW (NOT %EOF());
             WIcErro  = *OFF;
             WCodErro = *BLANKS;
             IF (ECR_OPTION <> *BLANKS);
             MONITOR;
                IF (ECR_OPTION <> *BLANKS);
                   WSelected = *ON;
                ENDIF;
                WRamoApolice = %TRIM(%EDITW(ECR_RAMO:'0  ')) +
                               %CHAR(ECR_APOLICE);

                //-----------------------------------------
                // Consultar os dados da Ap�lice
                //-----------------------------------------
                IF ($ValidOption(ECR_OPTION:'2'));
                   EXSR OpenDiario;
                   IF (ECR_RAMO = 50) OR (ECR_RAMO = 51);
                      DynamicCall = 'GP010X';
                      ShowApolice('2':WRamoApolice);
                   // AppAuto('2':WRamoApolice);
                   ELSE;
                      DynamicCall = 'GP015X';
                      ShowApolice('2':WRamoApolice);
                   // AppOutros('2':WRamoApolice);
                   ENDIF;

                //-----------------------------------------
                //  O Base est� aberto
                //-----------------------------------------
                ELSEIF ($ValidOption(ECR_OPTION:'3':%CHAR(ECR_Ramo)) OR
                    $ValidOption(ECR_OPTION:'4':%CHAR(ECR_Ramo)));
                    IF (ECR_OPTION = '03');
                       ECR_OPTION = '3';
                    ELSEIF (ECR_OPTION = '04');
                       ECR_OPTION = '4';
                   ENDIF;
                   IF (ECR_RAMO = 50) OR (ECR_RAMO = 51);
                      IF ($checkObject('GP010A':WWorkLib:'*FILE'))   // GP010A
                         AND (S#USR <> 'LGALHOZ');
                         WIcErro  = *ON;
                         WCodErro = 'PRD0093';
                      ELSE;
                         EXSR OpenDiario;
                         AppAuto(%TRIM(ECR_OPTION):WRamoApolice);
                      ENDIF;
                   ELSE;
                      IF ($checkObject('GP015A':WWorkLib:'*FILE'))   // GP015A
                         AND (S#USR <> 'LGALHOZ');
                         WIcErro  = *ON;
                         WCodErro = 'PRD0094';
                      ELSE;
                         EXSR OpenDiario;
                         AppOutros(%TRIM(ECR_OPTION):WRamoApolice);
                      ENDIF;
                   ENDIF;

                //-----------------------------------------
                // Consultar o documento com as Observa��es
                //-----------------------------------------
                ELSEIF ($ValidOption(ECR_OPTION:'5'));
                // AppDocumento(ECR_RAMO:ECR_APOLICE:WNoLib:WNoFile);
                   $ShellExec('strpco');
                   $ShellExec('strpccmd pccmd(''\\20.0.0.2\MEMO_APOL+
                        \criar_memo.bat ' + WRamoApolice + '.doc'') +
                        pause(*NO)');
                   $ShellExec('strpccmd pccmd(''\\20.0.0.2\MEMO_APOL+
                        \'+WRamoApolice+'.doc'') +
                        pause(*NO    )');

                //-----------------------------------------
                // Consultar os dados dos Recibos
                //-----------------------------------------
                ELSEIF ($ValidOption(ECR_OPTION:'6'));
                   AppRecibos(ECR_RAMO:ECR_APOLICE);
                   $SetDBSession(WSessionID);
                // MONITOR;
                //    WComando = 'STRQMQRY QMQRY(#JMPRDOBJ/RECDATA) +
                //                     QMFORM(#JMPRDOBJ/RECFORM) +
                //                     SETVAR((NUAPOL '+%CHAR(ECR_APOLICE) +') +
                //                      (CDRAMO '+%CHAR(ECR_RAMO)+'))';
                //    ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
                // ON-ERROR;
                // ENDMON;

                //-----------------------------------------
                // Consultar os dados dos Sinistros
                //-----------------------------------------
                ELSEIF ($ValidOption(ECR_OPTION:'7'));
                   AppSinistro(ECR_RAMO:ECR_APOLICE);

                //-----------------------------------------
                // Consultar o Hist�rico das Ap�lices
                //-----------------------------------------
                ELSEIF ($ValidOption(ECR_OPTION:'8'));
                   DOU (WNoFile = *BLANKS);
                      AppHistorico(ECR_RAMO:ECR_APOLICE:WNoLib:WNoFile);
                      // ECR_OPTION = '2';
                      // IF (WNoFile <> *BLANKS);
                      //    IF (ECR_RAMO = 50) OR (ECR_RAMO = 51);
                      //       WComando = 'OVRDBF FILE(GBASEDIA) TOFILE('+
                      //                  %TRIM(WNoLib)+'/'+%TRIM(WNoFile)+')';
   29                 //      ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
                      //       AppAuto(%TRIM(ECR_OPTION):WRamoApolice);
                      //       WComando = 'DLTOVR FILE(GBASEDIA)';
   29                 //      ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
                      //    ELSE;
                      //       WComando = 'OVRDBF FILE(GBASEDIO) TOFILE('+
                      //                  %TRIM(WNoLib)+'/'+%TRIM(WNoFile)+')';
   29                 //      ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
                      //       AppOutros(%TRIM(ECR_OPTION):WRamoApolice);
                      //       WComando = 'DLTOVR FILE(GBASEDIO)';
   29                 //      ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
                      //    ENDIF;
                      // ENDIF;
                      ECR_OPTION = *BLANKS;
                   ENDDO;
                ELSE;
                   WIcErro = *ON;
                   WCodErro = 'PPRDO0001';
                ENDIF;
             ON-ERROR;
                WIcErro = *ON;
                WCodErro = 'PRD0095';
             ENDMON;

                //------------------------------------------------
                // Apresentar a Mensagem de Erro
                //------------------------------------------------
                IF (WIcErro);
                   IF (WCodErro = 'PRD0001');
                      ECR_MSG001 = $AlignText('Esta op��o n�o � v�lida'
                                   :49:'C');
                      ECR_MSG002 = $AlignText('Usar um dos valores poss�ve+
                                   is':49:'C');
                   ELSEIF (WCodErro = 'PRD0093');
                      ECR_MSG001 = $AlignText('Esta op��o n�o � v�lida'
                                   :49:'C');
                      ECR_MSG002 = $AlignText('O Base autom�vel est� fecha+
                                   do':49:'C');
                   ELSEIF (WCodErro = 'PRD0094');
                      ECR_MSG001 = $AlignText('Esta op��o n�o � v�lida'
                                   :49:'C');
                      ECR_MSG002 = $AlignText('O Base outros ramos est� fe+
                                   chado':49:'C');
                   ELSEIF (WCodErro = 'PRD0095');
                      ECR_MSG001 = $AlignText('Forma detecados erros...'
                                   :49:'C');
                      ECR_MSG002 = $AlignText('Contacte a Inform�tica'
                                   :49:'C');
                   ENDIF;
                   EXFMT RPRDE01010;
                ENDIF;
             ENDIF;
             ECR_OPTION = *BLANKS;
             UPDATE RPRDE01002;
             READC RPRDE01002;
          ENDDO;
          IF (NOT WSelected);
             $SetCriteria(ECR_CRITERIA:*ON);
             EXSR inzRecords;
             EXSR getRecords;
          ENDIF;
       ENDSR;
      /END-FREE
