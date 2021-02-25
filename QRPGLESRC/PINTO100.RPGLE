     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL) FIXNBR(*ZONED)
     H*==================================================================
     H* Programa..: PINTO001
     H* Descrição.: Gerar o Interface de Coberturas e Apólices de uma
     H*             forma versátil, possibilitando por exemplo gerar uma
     H*             Interface Mensal, Anual, ou então uma informação
     H*             pontual sobre a carteira de um Mediador específico.
     H*
     H* Parametros e intruções de Utilização:
     H* [x] PAnoMes   = Periodo a Selecionar por Ano/Mês
     H* [x] PApolice  = Ramo e Numero de Apolice
     H* [x] PNumMedia = Código do Mediador
     H*
     H*  +------+---------+------------------------------------------------+
     H*  | Per. | PAnoMes | Descrição                                      |
     H*  +------+---------+------------------------------------------------+
     H*  |  1   | 999999  | Devolve o Interface com todas as Apólices      |
     H*  |  2   | 000000  | Devolve a última não processada                |
     H*  |  3   | 201206  | Devolve a posição da Carteira em Junho de 2012 |
     H*  |  4   | 201200  | Devolve a posição da Carteira em 2012          |
     H*  +------+---------+------------------------------------------------+
     H*
     H*  Se PNumMedia preenchido então filtra a Carteira apenas para o
     H*  Mediador Selecionado. Caso contrário selecciona para todos.
     H*
     H*==================================================================
     FFGRLP020  IF   E           K DISK    PREFIX(GRL_)
     FFINTP00201IF   E           K DISK    PREFIX(INT_)
     F                                     RENAME(RINTP002:RINTP00201)
     FFGRLP02001IF   E           K DISK    PREFIX(GRL_)
     F                                     RENAME(RGRLP020:RGRLP02001)
     FFGRLP023  IF   E           K DISK    PREFIX(GRL_)
     F*==========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SSECO010

     D*==> Estrutura dos comandos AS400
     D Exec            PR                  EXTPGM('QCMDEXC')
     D   stdcmd                     300A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5
     D
     D*=======================================================
     D*    Programa de carregamento dos Interfaces
     D*=======================================================
     D*==> Apólices Auto e Outros Ramos
     D FillAO          PR                  EXTPGM('PINTO101')
     D
     D*==> Apólices de Transporte e Viagens
     D FillTV          PR                  EXTPGM('PINTO103')
     D FillTVC         PR                  EXTPGM('PINTO104')
     D
     D*==> Apólices de Viagem
     D FillVG          PR                  EXTPGM('PINTO102')
     D
     D*==> Área de Dados
     DJMLDA            DS                  DTAARA(*LDA)
     D  LDA$FICH             100    107
     D  LDA$DDALT            108    109  0
     D  LDA$MMALT            110    111  0
     D  LDA$AAALT            112    113  0
     D  LDA$NUMMED           114    119  0
     D
     D*==> Campos de Trabalho
     D WStdCmd         S            300A
     D WStdLen         S             15P 5
     D WOvrFile        S             10A
     D WTmpFile        S             10A
     D WTmpLib         S             10A
     D WIcSelect       S               N
     D WCdBatch        S                   LIKE(INT_CDBATCH)
     D WNomPase        S             40A
     D WNomAS400       S             10A
     D WQtdFiles       S              2S 0
     D WIsToSelect     S               N
     D WDatChar        S             20A
     D WRamFile        S              1A
     D WNumMedia       S              6S 0
     D WNumApolice     S              7S 0
     D WCodRamo        S              2S 0
     D WAno            S              4S 0
     D WAnoMes         S              6S 0
     D WAnoMesAnula    S              6S 0
     D WQtdeRegistos   S              9S 0
     D WPeriodo        S              1S 0
     D WSessionID      S              5S 0
     D WTpRemessa      S              1A
     D WIcAnulacao     S               N
     D WDtAntiga       S               D   INZ(D'9999-12-31')
     D WDtRecente      S               D   INZ(D'1970-01-01')
     D WCdFuncao       S                   LIKE(INT_CDFUNCAO)
     D WIO#Error       S              5S 0
     D
     D*==> lista de Parâmetros
     D PAnoMes         S              6A
     D PApolice        S              9A
     D PNumMedia       S              6A
     D*==========================================================
     C     *ENTRY        PLIST
     C                   PARM                    PAnoMes
     C                   PARM                    PApolice
     C                   PARM                    PNumMedia
     C
      /FREE
       //--------------------------------------------------
       // Formatar o Período (Ano e mês)
       //--------------------------------------------------
       MONITOR;
          WAnoMes  = %DEC(PAnoMes:6:0);
          IF (%SUBST(PAnoMes:5:2) = '00');
             WPeriodo = 4; // Apólices do ano
          ELSEIF (%SUBST(PAnoMes:5:2) = '99');
             WPeriodo = 1; // Apólices do Mes
          ELSE;
             WPeriodo = 3; // Todas as Apólices
          ENDIF;
       ON-ERROR;
          //-----------------------------------------------------
          // Apenas executa se o processo anterior terminou
          //-----------------------------------------------------
          IF (NOT $ValBatch(S#PGM:'INTER'));
             *INLR = *ON;
             RETURN;
          ELSE;
             WAnoMes  = *ZEROS;
             WPeriodo = 2;   // As mais Recentes
          ENDIF;
       ENDMON;

       //--------------------------------------------------
       // Formatar o Ramo e Número de Apólice
       //--------------------------------------------------
       MONITOR;
           WCodRamo    = %DEC(%SUBST(PApolice:1:2):2:0);
           WNumApolice = %DEC(%SUBST(PApolice:3:7):7:0);
       ON-ERROR;
          WCodRamo    = *ZEROS;
          WNumApolice = *ZEROS;
       ENDMON;

       //--------------------------------------------------
       // Formatar o Código de Mediador
       //--------------------------------------------------
       MONITOR;
          WNumMedia = %DEC(PNumMedia:6:0);
       ON-ERROR;
          WNumMedia = *ZEROS;
       ENDMON;

       //---------------------------------------------------------
       // Processar Apólices de Viagem  (TRSF06)
       //---------------------------------------------------------
       WTpRemessa = 'D'; // Registos Diários
       WIO#Error = *ZEROS;
       WIcSelect = *OFF;
       CHAIN ('GREGA':*BLANKS:%SUBDT(%DATE():*YEARS)*100) RGRLP02001;
       IF (%FOUND());
          WRamFile = 'E'; // Apólices do Ramo Viagem
          EXSR FillTemporarios;
       ENDIF;

       //---------------------------------------------------------
       // Seleccionar ficheiros Temporários novos ou num período
       //   (TRSF05, TRSF07, TRSF07C)
       //---------------------------------------------------------
       IF (WPeriodo <> 1);
          WIcAnulacao = *OFF;
          SETLL (*LOVAL) RGRLP020;
          READ RGRLP020;
          DOW NOT %EOF();
             WRamFile = 'X';
             IF (GRL_CDGRUPO = 'GBDIX');
                WRamFile = 'A'; // Automóvel e Outros Ramos (Diario)
             ELSEIF (GRL_CDGRUPO = 'CGRX'); // Automóvel e Outros Ramos (Continuados)
                WRamFile = 'B'; // Automóvel e Outros Ramos (Continuados/Anulações)
             ELSEIF (GRL_CDGRUPO = 'GBDIT'); // Transportes e Diario
                WRamFile = 'C'; // Transportes e Viagens
             ELSEIF (GRL_CDGRUPO = 'CGRT'); // Transportes e Viagens (Continuados)
                WRamFile = 'D';
             ENDIF;

             IF (WRamFile <> 'X');
                IF (WPeriodo = 2); // Os mais recentes
                   CHAIN (GRL_CDBATCH) RINTP00201;
                   IF (NOT %FOUND());
                      EXSR FillTemporarios;
                   ENDIF;
                ELSEIF ((WPeriodo = 3) AND (WAnoMes = GRL_AnoMes)) OR
                       ((WPeriodo = 4) AND (%SUBST(%CHAR(WAnoMes):1:4) =
                          %SUBST(%CHAR(GRL_ANOMES):1:4)));
                   EXSR FillTemporarios;
                ENDIF;
                IF (WIO#Error <> *ZEROS);
                   LEAVE;
                ENDIF;
             ENDIF;
             READ RGRLP020;
          ENDDO;

          //---------------------------------------------------------
          // Se encontrou um registo mensal então processa anulações
          //---------------------------------------------------------
          IF (WIcAnulacao);
             WAnoMesAnula -= 4;
             IF (%SUBST(%CHAR(WAnoMesAnula):5:2) = '00') OR
                  (%SUBST(%CHAR(WAnoMesAnula):5:2) > '12');
                WAnoMesAnula = WAnoMesAnula - 88;
             ENDIF;
             CHAIN ('AGRX':*BLANKS:WAnoMesAnula) RGRLP02001;
             IF (%FOUND());
                WRamFile = 'B'; // Automóvel e Outros Ramos (Continuados/Anulações)
                EXSR FillTemporarios;
             ENDIF;
          ENDIF;
       ELSE; // Os mais actuais
       //---------------------------------------------------------
       //  Seleccionar com base no Actual (Todas)
       //---------------------------------------------------------
          EXSR PrepareInter;

          //----------------------------------------
          // Carregar com dados os Temporários
          //----------------------------------------
          EXSR SelectAll;
       ENDIF;

        //------------------------------------------------
        // Enviar Temporarios para a Lusitania e Arquivar
        //------------------------------------------------
        IF (WIcSelect);
           EXSR SendTemporarios;
        ENDIF;

        *INLR = *ON;

       //========================================================
       // Subrotina...: SelectAll
       // Objectivo...: Seleccionar todos os Registos
       //========================================================
       BEGSR SelectAll;
          WTmpLib  = 'QS36F';
          WOvrFile = 'GBASEDIA';
          WTmpFile = 'GBASEGRA';
          EXSR OpenFiles;

          WTmpLib  = 'QS36F';
          WOvrFile = 'GBASEDIO';
          WTmpFile = 'GBASEGRO';
          EXSR OpenFiles;

          //--------------------------------------------------
          // Carregar temporários com dados
          //--------------------------------------------------
          IF (WIO#Error = *ZEROS) AND (WQtdFiles = 2);
             FillAO();
          ELSEIF (WQtdFiles > *ZEROS);
             WIO#Error = 5;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina...: FillTemporarios
       // Objectivo...: Carregar os Ficheiros Temporários
       //========================================================
       BEGSR FillTemporarios;
          EXSR PrepareInter;
          WCdBatch      = GRL_CDBATCH;
          IF (WRamFile = 'A') OR (WRamFile='B');
             WCdFuncao = 5;   // Antigo TRSF05
          ELSEIF (WRamFile = 'C');
             WCdFuncao = 6;   // Antigo TRSF07
          ELSEIF (WRamFile = 'D');
             WCdFuncao = 7;   // Antigo TRSF07C
          ELSEIF (WRamFile = 'E');
             WCdFuncao = 8;   // Antigo TRSF06
          ENDIF;
          WQtdFiles = *ZEROS;

          //--------------------------------------------------
          // Seleccionar Ficheiros gerados nos Processo Batch
          //--------------------------------------------------
          SETLL (GRL_CDBATCH) RGRLP023;
          READE(N) (WCdBatch) RGRLP023;
          DOW (NOT %EOF());
             WIsToSelect = *OFF;
             WTmpLib  = GRL_NOLIBL;
             WTmpFile = GRL_NOFILE;
             IF (WRamFile='A') OR (WRamFile='B');
                IF (%SUBST(GRL_NOFILE:1:5)='GBDIA') OR
                    (%SUBST(GRL_NOFILE:1:4) = 'CGRA') OR
                     (%SUBST(GRL_NOFILE:1:4) = 'AGRA');
                   WOvrFile = 'GBASEDIA';
                   EXSR OpenFiles;
                   IF (%SUBST(GRL_NOFILE:1:4) = 'CGRA');
                       WIcAnulacao = *ON;
                       WAnoMesAnula = 200000+%DEC(%SUBST(GRL_NOFILE:5:4):4:0);
                   ENDIF;
                ELSEIF (%SUBST(GRL_NOFILE:1:5) = 'GBDIO') OR
                    (%SUBST(GRL_NOFILE:1:4) = 'CGRO') OR
                     (%SUBST(GRL_NOFILE:1:4) = 'AGRO');
                   WOvrFile = 'GBASEDIO';
                   EXSR OpenFiles;
                ENDIF;
             ELSEIF (WRamFile='C');
                IF (%SUBST(GRL_NOFILE:1:5)='FADIT');
                   WOvrFile = 'FAPLDIT';
                   EXSR OpenFiles;
                ELSEIF (%SUBST(GRL_NOFILE:1:5) = 'GBDIT');
                   WOvrFile = 'GBASEDIT';
                   EXSR OpenFiles;
                ENDIF;
             ELSEIF (WRamFile='D');
                WOvrFile = 'GBASEGRT';
                EXSR OpenFiles;
             ELSEIF (WRamFile='E');
                WOvrFile = 'GREGA';
                EXSR OpenFiles;
             ENDIF;

             //-------------------------------------------------
             // Actualizar Base de Dados de Controlo
             //-------------------------------------------------
             IF (WRamFile <> 'X');
                $AddBatchInp(WCdBatch:GRL_NUSEQU:WCdFuncao);

                //-----------------------------------------
                // Obter intervalo de datas dos ficheiros
                //-----------------------------------------
                IF (GRL_CDGRUPO <> 'AGRX') AND (GRL_CDGRUPO <> 'GREGA');
                   IF (WDtAntiga > GRL_DTCRIA);
                      WDtAntiga = GRL_DTCRIA;
                   ENDIF;
                   IF (WDtRecente < GRL_DTCRIA);
                      WDtRecente = GRL_DTCRIA;
                   ENDIF;
                ENDIF;
             ENDIF;
             READE(N) (GRL_CDBATCH) RGRLP023;
          ENDDO;

          //--------------------------------------------------
          // Carregar temporários com dados
          //--------------------------------------------------
          IF (WRamFile='A') AND (WQtdFiles = 2);
             FillAO(); // Antigo TRSF05
          ELSEIF (WRamFile='B') AND (WQtdFiles = 2);
             WTpRemessa = 'M'; // Registos Mensais
             FillAO(); // TRSF05 Informação Mensal
          ELSEIF (WRamFile='C') AND (WQtdFiles = 2);
             FillTV();  // Antigo TRSF07
          ELSEIF (WRamFile='D');
             FillTVC(); // Antigo TRSF07C
          ELSEIF (WRamFile='E');
             FillVG();  // Antigo TRSF06
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina...: PrepareInter
       // Objectivo...: Preparar envio de Ficheioros
       //========================================================
       BEGSR PrepareInter;
          //----------------------------------------
          // Criar Ficheiros Temporários
          //----------------------------------------
          IF (NOT WIcSelect);
             WTmpFile = 'LFAPOL';
             EXSR CrtTmpFiles;
             WTmpFile = 'LFCOBE';
             EXSR CrtTmpFiles;
             WIcSelect = *ON;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina...: CrtTmpFiles
       // Objectivo...: Criar ficheiros Temporários
       //========================================================
       BEGSR CrtTmpFiles;
          //---------------------------------------
          // Criar ficheiro Temporário
          //---------------------------------------
          IF (WTmpFile = 'LFAPOL');
             MONITOR;
                WStdCmd = 'DLTF FILE(QTEMP/LFAPOL)';
                WStdLen = %LEN(WStdCmd);
                EXEC(WStdCmd:WStdLen);
             ON-ERROR;
             ENDMON;
             MONITOR;
                WStdCmd = 'CRTPF FILE(QTEMP/LFAPOL) +
                           SRCFILE(*LIBL/QDDSSRC) +
                           OPTION(*NOLIST *NOSOURCE) +
                           SRCMBR(FINTT001) SIZE(*NOMAX)';
                WStdLen = %LEN(WStdCmd);
                EXEC(WStdCmd:WStdLen);
             ON-ERROR;
                WIO#Error = 20;
                LEAVESR;
             ENDMON;
          ELSE;
             MONITOR;
                WStdCmd = 'DLTF FILE(QTEMP/LFCOBE)';
                WStdLen = %LEN(WStdCmd);
                EXEC(WStdCmd:WStdLen);
             ON-ERROR;
             ENDMON;

             MONITOR;
                WStdCmd = 'CRTPF FILE(QTEMP/LFCOBE) +
                           SRCFILE(*LIBL/QDDSSRC) +
                           OPTION(*NOLIST *NOSOURCE) +
                           SRCMBR(FINTT002) SIZE(*NOMAX)';
                WStdLen = %LEN(WStdCmd);
                EXEC(WStdCmd:WStdLen);
             ON-ERROR;
                WIO#Error = 10;
                LEAVESR;
             ENDMON;
          ENDIF;

          //---------------------------------------
          // Override ao Ficheiro
          //---------------------------------------
          MONITOR;
             WStdCmd = 'OVRDBF FILE('+%TRIM(WTmpFile)+') +
                        TOFILE(QTEMP/'+%TRIM(WTmpFile)+')';
             WStdLen = %LEN(WStdCmd);
             EXEC(WStdCmd:WStdLen);
          ON-ERROR;
             WIO#Error = 10;
          ENDMON;
       ENDSR;

       //========================================================
       // Subrotina...: OpenFiles
       // Objectivo...: Abrir ficheiros para Processar
       //========================================================
       BEGSR OpenFiles;
          MONITOR;
             WStdCmd = 'OVRDBF FILE('+%TRIM(WOvrFile)+') TOFILE('+
                        %TRIM(WTmpLib)+'/'+%TRIM(WTmpFile)+')';
             WStdLen = %LEN(WStdCmd);
             EXEC(WStdCmd:WStdLen);
             WIsToSelect = *ON;

             IF (WIsToSelect);
                LDA$FICH   = WTmpFile;
                LDA$DDALT  = %SUBDT(GRL_DTCRIA:*Days);
                LDA$MMALT  = %SUBDT(GRL_DTCRIA:*Months);
                LDA$AAALT  = %SUBDT(GRL_DTCRIA:*Years) - 2000;
                LDA$NUMMED = WNumMedia;
                OUT JMLDA;
                WQtdFiles += 1;
             ENDIF;
          ON-ERROR;
             WIO#Error = 10;
          ENDMON;
       ENDSR;

       //========================================================
       // Subrotina...: SendTemporarios
       // Objectivo...: Enviar Ficheiros Temporários e Arquivar
       //========================================================
       BEGSR SendTemporarios;
          WNomAS400 = 'LFAPOL';
          WSessionId = $SetFiles(%TRIM(WNomAS400));
          MONITOR;
             WQtdeRegistos = $GetQtdeReg();
          ON-ERROR;
             WQtdeRegistos = *ZEROS;
          ENDMON;
          $RmvDBSession(WSessionID);

          IF (WQtdeRegistos > *ZEROS);
             $AddBatchOut(WNomAS400:WQtdeRegistos:'Y');
             WNomAS400 = 'LFCOBE';
             WSessionID = $SetFiles(%TRIM(WNomAS400));
             MONITOR;
                WQtdeRegistos = $GetQtdeReg();
             ON-ERROR;
                WQtdeRegistos = *ZEROS;
             ENDMON;
             $RmvDBSession(WSessionID);
             $AddBatchOut(WNomAS400:WQtdeRegistos:'Y');
             $chgBatchStat(WDtAntiga:WDtRecente);
          ELSE;
             $rmvBatch(); // Abortar Processo
          ENDIF;
       ENDSR;
      /END-FREE
