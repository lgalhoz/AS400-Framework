     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PGRLO001
     H* Descrição.: Actualizar lista de ficheiros temporários
     H*==================================================================
     FQAFDBASI  IF   E           K DISK    PREFIX(INV_) USROPN
     FFGRLP020  UF A E           K DISK    PREFIX(GRL_)
     FFGRLP02001IF   E           K DISK    PREFIX(GRL_)
     F                                     RENAME(RGRLP020:RGRLP02001)
     FFGRLP023  UF A E           K DISK    PREFIX(GRL_)
     FFGRLP024  IF   E           K DISK    PREFIX(TPL_)
     FFGRLP02301IF   E           K DISK    PREFIX(GRL_)
     F                                     RENAME(RGRLP023:RGRLP02301)
     FAUDFILES  O    F  120        PRINTER OFLIND(PrintOf) PRTCTL(PrintDS)
     F                                     EXTFILE('QSYSPRT')
     F                                     USROPN
     F*==========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SSECO010

     D*==> Estrura de dado para controlo da Impressora
     D PrintDS         DS
     D   $Fill                 1     12
     D   $CurLin              13     15  0
     D
     D*==> Estrutura dos comandos AS400
     D Exec            PR                  EXTPGM('QCMDEXC')
     D   stdcmd                     300A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5
     D
     D*==> Actualizar ficheiro de Dados
     D ChkLibrary      PR
     D    Library                    10A   Const
     D
     D*==> Actualizar ficheiro de Dados
     D UpdFilCtl       PR
     D    FilTipo                     1A   Const
     D
     D*==> Processo para carregar a lista de trabalho
     D FillList        PR
     D    FilTemplate                40A   Const
     D    FilNome                    10A   Const
     D    FilDate                     8S 0 Const
     D    FilTime                     6S 0 Const
     D    FilCdFile                   5S 0 Const
     D
     D*==> Lista de Ficheiros a Tratar
     D Lst#Files       DS                  QUALIFIED DIM(9999)
     D   Grupo                       10A
     D   Mes                          2S 0
     D   Ano                          4S 0
     D   Carrega                      1A
     D   NextRec                      4S 0
     D   Data                         8S 0
     D   Hora                         6S 0
     D   FILE                              LIKEDS(L#File) DIM(MaxFil)
     D WIdxLst         S              4S 0
     D WIdxFirst       S              4S 0
     D
     D L#File          DS                  QUALIFIED
     D   CdFile                       5S 0
     D   File                        10A   INZ(*BLANKS)
     D   Lib                         10A   INZ(*BLANKS)
     D   DtCria                        D
     D   HrCria                        T
     D   IsEmpty                       N   INZ(*ON)
     D
     D*==> Campos de Trabalho
     D WStdCmd         S            300A
     D WStdLen         S             15P 5
     D WFilDate        S              8S 0
     D WFilTipo        S              1A
     D WEditMask       S             10A
     D WNomLib         S             10A
     D PNomLib         S             10A
     D WPrefixo        S             10A
     D WDatHoje        S               D
     D WNuAno          S              4S 0
     D WIdxRec         S              4S 0
     D WStatus         S             20A
     D WIO#Error       S              5S 0
     D MaxFil          C                   14
     D*==========================================================
      /FREE
        //-------------------------------------------------------
        // Actualizar a Inventário de ficheiros
        //-------------------------------------------------------
        WDatHoje   = %DATE();
        WNuAno  = %SUBDT(%DATE():*YEARS);
        WIdxRec = 2006;
        DOW (WIdxRec <= WNuAno);
           WNomLib = 'GUIAS'+%CHAR(WIdxRec);
           ChkLibrary(WNomLib);
           WIdxRec += 1;
        ENDDO;
        ChkLibrary('QS36F');

        //-------------------------------------------------------
        // Desactivar ficheiros Elimiandos
        //-------------------------------------------------------
        MONITOR;
           WStdCmd = 'OVRPRTF FILE(AUDFILES) TOFILE(QSYSPRT) +
                      OUTQ(PDFARCH)';
           WStdLen = %LEN(WStdCmd);
           EXEC(WStdCmd:WStdLen);
           OPEN AUDFILES;
        ON-ERROR;
        ENDMON;
        EXCEPT  PRT_HEADER;
        SETLL *LOVAL RGRLP023;
        READ RGRLP023;
        DOW NOT %EOF();
           WStatus = *BLANKS;
           IF (GRL_DTACTUA <> %DATE());
              GRL_CDSTATUS = 'E'; // Eliminado
              WStatus = 'Ficheiro Eliminado';
           ELSE;
              IF (GRL_DTACTUA = GRL_DTREGIS);
                 WStatus = 'Ficheiro Criado';
              ENDIF;
              GRL_CDSTATUS = 'A'; // Activo
           ENDIF;
           UPDATE RGRLP023 %FIELDS(GRL_CDSTATUS);
           IF (WStatus <> *BLANKS);
              EXCEPT  PRT_DETAIL;
              IF (PrintOf);
                 EXCEPT PRT_HEADER;
                 PrintOf = *OFF;
              ENDIF;
           ENDIF;
           READ RGRLP023;
        ENDDO;

        *INLR = *ON;
      /END-FREE
     C*============================================================
     OAUDFILES  E            PRT_HEADER     1  1
     O                                            9 'PGRLO001'
     O                                           84 'Auditoria aos Temporários'
     O                                          103 'Pag.:'
     O                       PAGE          Z    114
     O          E            PRT_HEADER     2
     O                                          103 'Data:'
     O                       WDatHoje           114
     O          E            PRT_HEADER     1
     O                                           21 '---------------------'
     O                                           27 '----'
     O                                           40 '-----------'
     O                                           52 '-----------'
     O                                           64 '-----------'
     O                                           77 '------------'
     O                                           98 '--------------------'
     O          E            PRT_HEADER     1
     O                                           19 'Biblioteca/Ficheiro'
     O                                           26 'Ano'
     O                                           40 'Dat.Criação'
     O                                           52 'Hor.Criação'
     O                                           64 'Dat.Registo'
     O                                           77 'Dat.Situação'
     O                                           88 'Ocorrência'
     O          E            PRT_HEADER     1
     O                                           21 '---------------------'
     O                                           27 '----'
     O                                           40 '-----------'
     O                                           52 '-----------'
     O                                           64 '-----------'
     O                                           77 '------------'
     O                                           98 '--------------------'
     OAUDFILES  E            PRT_DETAIL
     O                       GRL_NOLIBL          10
     O                                           11 '/'
     O                       GRL_NOFILE          21
     O                       GRL_NUANO           27
     O                       GRL_DTCRIA          39
     O                       GRL_HRCRIA          49
     O                       GRL_DTREGIS         63
     O                       GRL_DTACTUA         75
     O                       WStatus             98
     P*=======================================================
     P* Processo..: ChkLibrary
     P* Descrição.: Validar o conteudo da Biblioteca.
     P*
     P*=======================================================
     P ChkLibrary      B                   EXPORT
     D                 PI
     D    PNomLib                    10A   Const
     D*=======================================================
      /FREE
       WNomLib = PNomLib;
       WIO#Error = *ZEROS;
       WIdxFirst = *ZEROS;
       WIdxLst   = *ZEROS;

       MONITOR;
          WStdCmd = 'CLRPFM FILE(QTEMP/BASATR)';
          WStdLen = %LEN(WStdCmd);
          EXEC(WStdCmd:WStdLen);
       ON-ERROR;
       ENDMON;

       //----------------------------------------------
       //  Carregamentos
       //----------------------------------------------
       SETLL (*LOVAL) RGRLP024;
       READ RGRLP024;
       DOW NOT %EOF();
          IF (TPL_CDSTATUS = '1');
             EXSR FillFile;
          ENDIF;

          IF (WIO#Error <> *ZEROS);
             LEAVE;
          ENDIF;
          READ RGRLP024;
       ENDDO;

       //----------------------------------------------
       //  Actualizar listas de Trabalho
       //----------------------------------------------
       IF (WIO#Error = *ZEROS);
          EXSR UpdLista;
       ENDIF;

       //----------------------------------------------
       //  Actualizar Ficheiro de controlo de Temporários
       //----------------------------------------------
       IF (WIO#Error = *ZEROS);
          UpdFilCtl('A');
       ENDIF;
       RETURN;

       //========================================================
       // Subrotina...: FillFile
       // Objectivo...: Listar inventário de ficheiros
       //========================================================
       BEGSR FillFile;
          MONITOR;
             WStdCmd = 'DSPFD FILE('+%TRIM(WNomLib)+
                        '/'+%TRIM(TPL_PREFIXO)+'*) TYPE(*BASATR) +
                        OUTPUT(*OUTFILE) OUTFILE(QTEMP/BASATR) +
                        OUTMBR(*FIRST *ADD)';
             WStdLen = %LEN(WStdCmd);
             EXEC(WStdCmd:WStdLen);
          ON-ERROR;
          ENDMON;
       ENDSR;

       //========================================================
       // Subrotina...: UpdLista
       // Objectivo...: Actualizar lista de trabalho
       //               INV_ATFILE  10A
       //               INV_ATLIB   10A
       //               INV_ATFCCN   1A (0=19XX, 1=20XX)
       //               INV_ATFCDT   6A (YYMMDD)
       //               INV_ATFCTM   6A (HHMMSS)
       //========================================================
       BEGSR UpdLista;
          //-----------------------------------------------------
          // Abrir Temporário com o inventário de Ficheiros
          //-----------------------------------------------------
          MONITOR;
             WStdCmd = 'OVRDBF FILE(QAFDBASI) TOFILE(QTEMP/BASATR)';
             WStdLen = %LEN(WStdCmd);
             EXEC(WStdCmd:WStdLen);
          ON-ERROR;
             WIO#Error = 10;
             LEAVESR;
          ENDMON;

          //-----------------------------------------------------
          // Actualizar controlo de Temporários
          //-----------------------------------------------------
          MONITOR;
          OPEN QAFDBASI;
          READ QAFDBASI;
          DOW NOT %EOF(QAFDBASI);
             SETLL (*LOVAL) RGRLP024;
             READ RGRLP024;
             DOW NOT %EOF();
                IF (TPL_CDSTATUS = '1');
                   IF ($ValStrStruct(INV_ATFILE:TPL_TEMPLATE));
                      WFilDate = ((%DEC(INV_ATFCCN:1:0)+19)*1000000)+
                                   %DEC(INV_ATFCDT:6:0);
                      WEditmask = %EDITW(WFilDate:'    -  -  ');
                      CHAIN (INV_ATFILE:%DATE(WEditMask:*ISO)) FGRLP02301;
                      IF (NOT %FOUND());
                         FillList(TPL_TEMPLATE:INV_ATFILE:
                                     WFilDate:%DEC(INV_ATFCTM:6:0):
                                     TPL_CDFILE);
                      ELSEIF (%TRIM(INV_ATLIB)<>%TRIM(GRL_NOLIBL));
                         CHAIN (GRL_CDBATCH:GRL_NUSEQU) RGRLP023;
                         GRL_NOLIBL = WNomLib;
                         GRL_DTACTUA = %DATE();
                         UPDATE RGRLP023 %FIELDS(GRL_NOLIBL:GRL_DTACTUA);
                      ELSE;
                         CHAIN (GRL_CDBATCH:GRL_NUSEQU) RGRLP023;
                         GRL_DTACTUA = %DATE();
                         UPDATE RGRLP023 %FIELDS(GRL_NOLIBL:GRL_DTACTUA);
                      ENDIF;
                      LEAVE;
                   ENDIF;
                ENDIF;
                READ RGRLP024;
             ENDDO;
             READ QAFDBASI;
          ENDDO;
          CLOSE QAFDBASI;
          ON-ERROR;
          ENDMON;
       ENDSR;
      /END-FREE
     P ChkLibrary      E

     P*=======================================================
     P* Processo..: FillList
     P* Descrição.: Preencher as variáveis globais com valores
     P*             e ordenar os elementos da lista
     P*=======================================================
     P FillList        B                   EXPORT
     D                 PI
     D    FilTemplate                40A   Const
     D    FilNome                    10A   Const
     D    FilDate                     8S 0 Const
     D    FilTime                     6S 0 Const
     D    FilCdFile                   5S 0 Const
     D
     D*==> Variáveis de Trabalho
     D WProduto        S              1A
     D WCarrega        S              1A
     D WMes            S              2S 0
     D WAno            S              4S 0
     D WCdGrupo        S             10A
     D
     D WIcExiste       S               N
     D WIdxAct         S              4S 0
     D WIdxNext        S              4S 0
     D WDataHoraPrm    S             14S 0
     D WDataHoraLst    S             14S 0
     D*===========================================================
      /FREE

          //----------------------------------------------
          // Formatar Variáveis de Trabalho
          //----------------------------------------------
          WDataHoraPrm = FilDate * 1000000 + FilTime;
          WProduto     = $ExtrStringVar(FilNome:FilTemplate:'P');
          WCarrega     = $ExtrStringVar(FilNome:FilTemplate:'C');
          WMes         = %DEC($ExtrStringVar(FilNome:FilTemplate:'M'):2:0);
          WAno         = %DEC($ExtrStringVar(FilNome:FilTemplate:'A'):4:0);
          IF (WAno = *ZEROS);
              WAno = %DEC(%SUBST(%CHAR(FilDate):1:4):4:0);
          ENDIF;
          WCdGrupo     = $ExtrStringVar(FilNome:FilTemplate:'G');
          IF (WProduto = 'A') OR (WProduto = 'O');
             WProduto  = 'X';
          ENDIF;
          WCdGrupo  = %TRIM(WCdGrupo)+%TRIM(WProduto);
          WCdGrupo  = TPL_CDGRUPO;

          //----------------------------------------------
          // Validar se o Processo já está registado
          //----------------------------------------------
          WIcExiste = *OFF;
          IF (WIdxFirst > *ZEROS);
             WIdxAct  = WIdxFirst;
             WIdxNext = *ZEROS;
             DOW (WIdxFirst > *ZEROS);
                WDataHoraLst = Lst#Files(WIdxAct).Data * 1000000
                               + Lst#Files(WIdxAct).Hora;
                IF ((Lst#Files(WIdxAct).Mes = WMes) AND
                    (Lst#Files(WIdxAct).Ano = WAno) AND
                    (Lst#Files(WIdxAct).Carrega = WCarrega) AND
                    (Lst#Files(WIdxAct).Grupo = WCdGrupo));
                   WIcExiste = *ON;
                   LEAVE;
                ELSE;
                   IF (WDataHoraPrm > WDataHoraLst);
                      WIdxNext = WIdxAct;
                   ENDIF;
                ENDIF;
                IF (WIdxAct = Lst#Files(WIdxAct).NextRec);
                   LEAVE;
                ENDIF;
                WIdxAct = Lst#Files(WIdxAct).NextRec;
             ENDDO;
          ELSE;
             WIdxFirst = 1;
          ENDIF;

          IF (NOT WIcExiste);
             WIdxLst += 1;
             Lst#Files(WIdxLst).Grupo   = WCdGrupo;
             Lst#Files(WIdxLst).Mes     = WMes;
             Lst#Files(WIdxLst).Ano     = WAno;
             Lst#Files(WIdxLst).Carrega = WCarrega;
             Lst#Files(WIdxLst).Data    = FilDate;
             Lst#Files(WIdxLst).Hora    = FilTime;
             IF (WIdxNext > *ZEROS);
                IF (Lst#Files(WIdxNext).NextRec = WIdxNext);
                   Lst#Files(WIdxLst).NextRec = WIdxLst;
                ELSE;
                   Lst#Files(WIdxLst).NextRec = Lst#Files(WIdxNext).NextRec;
                ENDIF;
                Lst#Files(WIdxNext).NextRec = WIdxLst;
             ELSE;
                Lst#Files(WIdxLst).NextRec = WIdxFirst;
                WIdxFirst = WIdxLst;
             ENDIF;
             WIdxNext = *ZEROS;
             DOW (WIdxNext < MaxFil);
                WIdxNext += 1;
                Lst#Files(WIdxLst).File(WIdxNext).IsEmpty = *ON;
             ENDDO;
             WIdxAct = WIdxLst;
          ENDIF;

          //-----------------------------------------------------
          // Associar ficheiros ao Batch
          //-----------------------------------------------------
          WIdxNext = 1;
          DOW (NOT Lst#Files(WIdxAct).File(WIdxNext).IsEmpty);
             WIdxNext += 1;
          ENDDO;
          Lst#Files(WIdxAct).File(WIdxNext).File   = FilNome;
          Lst#Files(WIdxAct).File(WIdxNext).Lib    = %TRIM(WNomLib);
          Lst#Files(WIdxAct).File(WIdxNext).DtCria = %DATE(FilDate:*ISO);
          Lst#Files(WIdxAct).File(WIdxNext).HrCria = %TIME(FilTime:*ISO);
          Lst#Files(WIdxAct).File(WIdxNext).CdFile = FilCdFile;
          Lst#Files(WIdxAct).File(WIdxNext).Isempty = *OFF;
      /END-FREE
     P FillList        E

     P*=======================================================
     P* Processo..: UpdfilCtl
     P* Descrição.: Actualizar ficheiros de Controlo de
     P*             Temporários gerados pelos Batch.
     P*=======================================================
     P UpdFilCtl       B                   EXPORT
     D                 PI
     D    FilType                     1A   Const
     D
     D*==> Variáveis de Trabalho
     D WCarrega        S              1A
     D WMes            S              2S 0
     D WAno            S              4S 0
     D WIdxAct         S              4S 0
     D WIdxNext        S              4S 0
     D WDataHoraPrm    S             14S 0
     D WDataHoraLst    S             14S 0
     D*===========================================================
      /FREE

          WIdxAct  = WIdxFirst;
          DOW (WIdxFirst > *ZEROS);
             WDataHoraLst = Lst#Files(WIdxAct).Data * 1000000
                            + Lst#Files(WIdxAct).Hora;
             // DSPLY %CHAR(WDataHoraLst);
             EXSR AddBatch;
             IF (WIdxAct = Lst#Files(WIdxAct).NextRec);
                LEAVE;
             ENDIF;
             WIdxAct = Lst#Files(WIdxAct).NextRec;
          ENDDO;


       //==========================================================
       // Subrotina.: AddBatch
       // Objectivo.: Actualizar controlo de Temporários gerados
       //==========================================================
       BEGSR AddBatch;
          //----------------------------------------------------------
          // Localizar existência de um registo e actualziar
          //----------------------------------------------------------
          CHAIN (Lst#Files(WIdxAct).Grupo:Lst#Files(WIdxAct).Carrega:
                 Lst#Files(WIdxAct).Ano * 100 + Lst#Files(WIdxAct).Mes)
                 RGRLP02001;
          IF (NOT %FOUND());
             GRL_CDBATCH  = $GetNextKey('FGRLP020':'CDBATCH');
             GRL_CDGRUPO  = Lst#Files(WIdxAct).Grupo;
             GRL_CDLETRA  = Lst#Files(WIdxAct).Carrega;
             GRL_ANOMES   = Lst#Files(WIdxAct).Ano*100+Lst#Files(WIdxAct).Mes;
             GRL_GRL02001 = %DATE();
             GRL_GRL02002 = %DATE();
             WRITE RGRLP020;
          ENDIF;

          //----------------------------------------------------------
          // Ligar Ficheiros
          //----------------------------------------------------------
          WIdxNext = 1;
          DOW (NOT Lst#Files(WIdxAct).File(WIdxNext).IsEmpty);
             GRL_NUSEQU   = $GetNextKey('FGRLP023':'NUSEQU':'CDBATCH = +
                            ''' + %TRIM(%CHAR(GRL_CDBATCH)) + '''');
             GRL_NOFILE   = Lst#Files(WIdxAct).File(WIdxNext).File;
             GRL_NOLIBL   = Lst#Files(WIdxAct).File(WIdxNext).Lib;
             GRL_DTCRIA   = Lst#Files(WIdxAct).File(WIdxNext).DtCria;
             GRL_HRCRIA   = Lst#Files(WIdxAct).File(WIdxNext).HrCria;
             GRL_CDFILE   = Lst#Files(WIdxAct).File(WIdxNext).CdFile;
             GRL_NUANO    = %SUBDT(GRL_DTCRIA:*YEARS);
             IF (GRL_NUANO > WNuAno);
                GRL_NUANO    = WNuAno;
             ENDIF;
             GRL_DTREGIS = %DATE();
             GRL_DTACTUA = %DATE();
             WRITE RGRLP023;
             WIdxNext += 1;
          ENDDO;
       ENDSR;
      /END-FREE
     P UpdFilCtl       E
