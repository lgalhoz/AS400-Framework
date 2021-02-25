     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PRDPO010
     H* Descrição.: Manuenção de Apólices
     H*==================================================================
     FFPRDP002  IF   E           K DISK    PREFIX(PRD_)
     FFPRDE010  CF   E             WORKSTN SFILE(RPRDE01002:REC#)
     F                                     PREFIX(ECR_)
     F                                     INFDS(INFD#KEYS)
     F*==========================================================
     F* Indicadores  Descrição
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
      /Copy QRPGLESRC,SCATO010

     D*==> Actualizar a nova estrutura
     D UpdNewStruct    PR                  EXTPGM('PCVSO001')
     D   CdRamo                       2S 0 CONST
     D   NuApoliceo                   7S 0 CONST
     D
     D*==> Chamada de função Dynamic, para trabalhar detalhe do Item
     D ShellExec       PR                  EXTPGM('QCMDEXC')
     D   stdcmd                     300A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5 CONST
     D
     D*==> Aceder aos registos de Histórico para a Apólice
     D AppHistorico    PR                  EXTPGM('RPRDO004')
     D   Ramo                         2S 0 CONST
     D   Apolice                      7S 0 CONST
     D   Lib                         10A   CONST
     D   File                        10A   CONST
     D
     D*==> Aceder às actas geradas para a Apólice
     D AppActa123      PR                  EXTPGM('RPRDO002')
     D   Ramo                         2S 0 CONST
     D   Apolice                      7S 0 CONST
     D   Lib                         10A   CONST
     D   File                        10A   CONST
     D
     D*==> Aceder aos ficheiro de Histórico para a Apólice
     D AppActas        PR                  EXTPGM('RPRDO007')
     D   Ramo                         2S 0 CONST
     D   Apolice                      7S 0 CONST
     D
     D*==> Aceder aos documentos gerados para a Apólice
     D AppDocumento    PR                  EXTPGM('RPRDO006')
     D   Ramo                         2S 0 CONST
     D   Apolice                      7S 0 CONST
     D   Lib                         10A   CONST
     D   File                        10A   CONST
     D
     D*==> Apresentar uma query com os sinistros da Apólice
     D AppSinistro     PR                  EXTPGM('PSINO010')
     D   Ramo                         2S 0 CONST
     D   Apolice                      7S 0 CONST
     D   DatInicio                    8S 0 CONST
     D   DatTermo                     8S 0 CONST
     D   CodErro                      7A   CONST
     D
     D
     D AppSinistro1    PR                  EXTPGM('RSINO010')
     D   Ramo                         2S 0 CONST
     D   Apolice                      7S 0 CONST
     D
     D*==> Apresentar uma query com os Recibos da Apólice
     D AppRecibos      PR                  EXTPGM('PPRDO016')
     D   Ramo                         2S 0 CONST
     D   Apolice                      7S 0 CONST
     D
     D*==> Criar temporários de apólices
     D CrtTempor       PR                  EXTPGM('RPRDO001')
     D
     D*==> Marcar Apólices Alteradas/Novas
     D MrkApolices     PR                  EXTPGM('RPRDO003')
     D
     D*==> Chamada à Aplicação auto
     D ShowApolice     PR                  EXTPGM(DynamicCall)
     D   Option                       1A   CONST
     D   RamApol                      9A   CONST
     D   Data                         8S 0 CONST Options(*NOPASS)
     D DynamicCall     S             10A   INZ(*BLANKS)
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
     D WWorkLib        S             10A
     D PWorkLib        S             10A
     D WSessionID      S              5S 0
     D WIcNovas        S              1A
     C*========================================================
     C     *ENTRY        PLIST
     C                   PARM                    PWorkLib
      /FREE

       //------------------------------------
       // Se não estiver autorizado então sai
       //------------------------------------
       IF NOT $ValAccess(S#PGM);
          *INLR = *ON;
          RETURN;
       ENDIF;

       //------------------------------------
       // Invocar o modo de Testes
       //------------------------------------
       MONITOR;
          WWorkLib = PWorkLib;
       ON-ERROR;
          WWorkLib = 'QS36F';
       ENDMON;

       //------------------------------------------
       // Preparar ambiente para consultar apólices
       //------------------------------------------
       //EXSR CtrTempor;

       WIcNovas = 'N';
       WSessionID = $SETFILES('FPRDT001');

       //-------------------------------------
       // Definição dos campos da Listagem
       //-------------------------------------
       // $SetLstFld('CDRAMO':'Rm.':3:'D');
       // $SetLstFld('NUAPOL':'Apólice':3:'D');
       // $SetLstFld('NUSEG':'Cod.':1:'D');
       // $SetLstFld('NOSEG':'Segurado':4:'D');
       // $SetLstFld('NUNIF':'NIF':4:'D');
       // $SetLstFld('STATUS':'E.':4:'D');
       // $SetLstFld('MATRIC':'Matricula':4:'C');

       //-------------------------------------
       // Definição das opções de Consulta
       //-------------------------------------
       $SetCriteria(*BLANKS);
       $addCriteria('Apólice....:':'NUAPOL':'LK');
       $addCriteria('Código Seg.:':'NUSEG':'EQ');
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
          ELSEIF (Func#Key = KEY#F07); // Multicriterio de Selecção
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
             // Marcar registos que estão no carregamento
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
          ELSEIF ($ValidKey(Func#Key:KEY#F21));
             WComando = 'CALL QCMD';
   29        ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
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

          //-------------------------------------
          // Confirmar se deseja sair da função
          //-------------------------------------
          IF (*IN03 OR *IN12) AND (S#USR='TESTE');
||+--        IF ($ShowDialog('PRD0190':*BLANKS:'NS') = 'N');
                *IN03 = *OFF;
                *IN12 = *OFF;
             ENDIF;
          ENDIF;
       ENDDO;

       $RmvDBSession(WSessionID);
       *INLR = *ON;
       RETURN;

       //========================================================
       // Subrotina.: OpenDiario
       // Objectivo.: Abrir Ficheiro diário em função do Base
       //========================================================
       BEGSR OpenDiario;
          //--------------------------------------------------
          // Se o base estiver fechado então usar temporarios
          //--------------------------------------------------
          MONITOR;
             IF ($checkObject('GP010A':WWorkLib:'*FILE'))  // GP010A
                AND (S#USR <> 'LGALHOZ');
                WComando = 'OVRDBF FILE(GBASEDIA) TOFILE(SEXTRAS/JBASEDIA)';
   29           ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
             ELSE;
             // WComando = 'OVRDBF FILE(GBASEDIA) +
             //            TOFILE('+%TRIM(WWorkLib)+'/GBASEDIA)';
                WComando = 'OVRDBF FILE(GBASEDIA) +
                           TOFILE(GBASEDIA)';
   29           ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
             ENDIF;

             IF ($checkObject('GP015A':WWorkLib:'*FILE')); // GP015A
                WComando = 'OVRDBF FILE(GBASEDIO) TOFILE(SEXTRAS/JBASEDIO)';
   29           ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
             ELSE;
             // WComando = 'OVRDBF FILE(GBASEDIO) +
             //             TOFILE('+%TRIM(WWorkLib)+'/GBASEDIO)';
                WComando = 'OVRDBF FILE(GBASEDIO) +
                            TOFILE(GBASEDIO)';
   29           ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
             ENDIF;
          ON-ERROR;
          ENDMON;
       ENDSR;

       //========================================================
       // Subrotina.: AddNewApolice
       // Objectivo.: Adicionar uma nova Apólice
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
                   // Validar em Catálago se Código de Ramo valido
                   // Validar se o Base está fechado
                   //----------------------------------------------
                   DynamicCall = *BLANKS;
                   IF ((ECR_RAMO = 50) OR (ECR_RAMO = 51)) AND
                       ($checkObject('GP010A':WWorkLib:'*FILE')); // GP010A
                      WIcErro  = *ON;
                      WCodErro = 'PRD0093';
                   ELSEIF ($checkObject('GP015A':WWorkLib:'*FILE'))
                          AND ((ECR_RAMO < 68) OR (ECR_RAMO > 70));
                      WIcErro  = *ON;
                      WCodErro = 'PRD0094';
                   ELSE;
                      DynamicCall = $GetAppApol(ECR_RAMO);
                      IF (DynamicCall = *BLANKS);
                         WIcErro = *ON;
                         WCodErro = 'PRD0097';
                      ENDIF;
                   ENDIF;

                   //------------------------------------------------
                   // Se Não existirem Erros então
                   //------------------------------------------------
                   IF (NOT WICErro);
                      MONITOR;
                         WRamoApolice = %TRIM(%EDITW(ECR_RAMO:'0  '))+'       ';
                         ShowApolice('1':WRamoApolice);
                         LEAVE;
                      ON-ERROR;
                         WIcErro  = *ON;
                         WCodErro = 'PRD0095';
                      ENDMON;
                   ENDIF;
                ELSE;
                   WIcErro  = *ON;
                   WCodErro = 'PRD0096';
                ENDIF;

                //------------------------------------------------
                // Se foram identifcados Erros então
                //------------------------------------------------
                IF (WIcErro);
                   ECR_MSG001 = *BLANKS;
                   ECR_MSG002 = *BLANKS;
                   ECR_MSG003 = *BLANKS;

                   $setTexto($GetMessage(WCodErro):49:'C');
                   ECR_MSG001 = $GetFirstLine();
                   IF (NOT $EOFText());
                      ECR_MSG002 = $GetNextLine();
                   ENDIF;
                   IF (NOT $EOFText());
                      ECR_MSG003 = $GetNextLine();
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
          *In53     = *On;      // Limpar Lista de Ecrã
          *In52     = *On;      // Mostrar a Lista de Ecrã
          WRITE RPRDE01003;     // Limpar Lista
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
                // Consultar os dados da Apólice
                //-----------------------------------------
                IF ($ValidOption(ECR_OPTION:'2'));
                   EXSR OpenDiario;
                   DynamicCall = $GetAppApol(ECR_RAMO);
                   IF (DynamicCall = *BLANKS);
                      WIcErro = *ON;
                      WCodErro = 'PRD0097';
                   ELSE;
                      ShowApolice('2':WRamoApolice);
                   ENDIF;

                //-----------------------------------------
                //  O Base está aberto
                //-----------------------------------------
                ELSEIF ($ValidOption(ECR_OPTION:'3':%CHAR(ECR_Ramo)) OR
                    $ValidOption(ECR_OPTION:'4':%CHAR(ECR_Ramo)));
                   IF (ECR_OPTION = '03');
                      ECR_OPTION = '3';
                   ELSEIF (ECR_OPTION = '04');
                      ECR_OPTION = '4';
                   ENDIF;

                   DynamicCall = *BLANKS;
                   IF ((ECR_RAMO = 50) OR (ECR_RAMO = 51)) AND
                       ($checkObject('GP010A':WWorkLib:'*FILE'))    // GP010A
                        AND (S#USR <> 'LGALHOZ');
                      WIcErro  = *ON;
                      WCodErro = 'PRD0093';
                   ELSEIF ($checkObject('GP015A':WWorkLib:'*FILE')) // GP015A
                        AND (S#USR <> 'LGALHOZ');
                      WIcErro  = *ON;
                      WCodErro = 'PRD0094';
                   ELSE;
                      DynamicCall = $GetAppApol(ECR_RAMO);
                      IF (DynamicCall = *BLANKS);
                         WIcErro = *ON;
                         WCodErro = 'PRD0097';
                      ENDIF;
                   ENDIF;

                   IF (NOT WIcErro);
                      EXSR OpenDiario;
                      ShowApolice(%TRIM(ECR_OPTION):WRamoApolice);
                   ENDIF;

                //-----------------------------------------
                // Consultar o documento com as Observações
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

                //-----------------------------------------
                // Consultar os dados dos Sinistros
                //-----------------------------------------
                ELSEIF ($ValidOption(ECR_OPTION:'7'));
                // AppSinistro1(ECR_RAMO:ECR_APOLICE);
                   WCodErro = *BLANKS;
                   AppSinistro(ECR_RAMO:ECR_APOLICE:*ZEROS:*ZEROS:WCodErro);
                   IF (WCodErro <> *BLANKS);
                      $ShowDialog(WCodErro);
                      WCodErro = *BLANKS;
                   ENDIF;

                //-----------------------------------------
                // Consultar o Histórico das Apólices
                //-----------------------------------------
                ELSEIF ($ValidOption(ECR_OPTION:'8'));
                   DOU (WNoFile = *BLANKS);
                      AppHistorico(ECR_RAMO:ECR_APOLICE:WNoLib:WNoFile);
                      ECR_OPTION = *BLANKS;
                   ENDDO;

                //-----------------------------------------
                // Consultar o Histórico das Actas
                //-----------------------------------------
                ELSEIF ($ValidOption(ECR_OPTION:'10'));
                   UpdNewStruct(ECR_RAMO:ECR_APOLICE);
                   ECR_OPTION = *BLANKS;

                //-----------------------------------------
                // Aceder ao Arquivo de documentos
                //-----------------------------------------
                ELSEIF ($ValidOption(ECR_OPTION:'11'));
                   $ShellExec('strpco');
                   $ShellExec('strpccmd pccmd(''\\20.0.0.130\share\+
                        Arquivo\Segurados\criar_folder.bat '+
                        %CHAR(ECR_NUSEG)+''') pause(*NO)');
                   $ShellExec('strpccmd pccmd(''explorer \\20.0.0.130\share\+
                        Arquivo\Segurados\'+%CHAR(ECR_NUSEG)+''') +
                        pause(*NO)');

                ELSEIF ($ValidOption(ECR_OPTION:'12'));
                // DOU (WNoFile = *BLANKS);
                      AppActa123(ECR_RAMO:ECR_APOLICE:WNoLib:WNoFile);
                      ECR_OPTION = *BLANKS;
                // ENDDO;
                ELSE;
                   WIcErro = *ON;
                   WCodErro = 'PRD0001';
                ENDIF;
             ON-ERROR;
                WIcErro = *ON;
                WCodErro = 'PRD0095';
             ENDMON;

                //------------------------------------------------
                // Apresentar a Mensagem de Erro
                //------------------------------------------------
                IF (WIcErro);
                   ECR_MSG001 = *BLANKS;
                   ECR_MSG002 = *BLANKS;
                   ECR_MSG003 = *BLANKS;

                   $setTexto($GetMessage(WCodErro):49:'C');
                   ECR_MSG001 = $GetFirstLine();
                   IF (NOT $EOFText());
                      ECR_MSG002 = $GetNextLine();
                   ENDIF;
                   IF (NOT $EOFText());
                      ECR_MSG003 = $GetNextLine();
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
