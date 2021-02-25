     H*NOMAIN
     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Função....: RGRLO030
     H* Descrição.: Este componente, pesquisa os ficheiros temporários
     H*             que obedecem ao critério de pesquisa. Estes ficheiros
     H*             Temporário, guardam informação de histórico das
     H*             Apólices, Recibos, Actas, Cartas Verdes entre Outras.
     H*==================================================================
     FFGRLP020  IF   E           K DISK    PREFIX(GRL_)
     FFGRLP023  IF   E           K DISK    PREFIX(GRL_)
     FFGRLP02302IF   E           K DISK    PREFIX(GRL_)
     F                                     RENAME(RGRLP023:RGRLP02302)
     FFGRLP026  IF   E           K DISK    PREFIX(GRL_)
     FFGRLP02601IF   E           K DISK    PREFIX(GRL_)
     F                                     RENAME(RGRLP026:RGRLP02601)
     FFGRLP02602IF   E           K DISK    PREFIX(GRL_)
     F                                     RENAME(RGRLP026:RGRLP02602)
     FFGRLP02603IF   E           K DISK    PREFIX(GRL_)
     F                                     RENAME(RGRLP026:RGRLP02603)
     F*==================================================================
      /Copy QRPGLESRC,SGRLO010

     D*==> Lista de Ficheiros Temporários a receber
     D LST#TmpFile     DS                  QUALIFIED DIM(9999)
     D    CodFile                     5S 0 INZ(*ZEROS)
     D    NomLibl                    10A   INZ(*BLANKS)
     D    NomFile                    10A   INZ(*BLANKS)
     D    AnoMes                      6S 0 INZ(*ZEROS)
     D    CodLetra                    1A   INZ(*BLANKS)
     D    DatCria                     8S 0 INZ(*ZEROS)
     D    HorCria                     6S 0 INZ(*ZEROS)
     D    IsEmpty                      N   INZ(*ON)
     D
     D*==> Lista de Valores a Pesquisar
     D LST#ValField    DS                  QUALIFIED DIM(10)
     D    ValField                   20A   INZ(*BLANKS)
     D    CodField                    9S 0 INZ(*ZEROS)
     D    IsEmpty                      N   INZ(*ON)
     D
     D*==> Processo de Pesquisa Metadados
     D $GetTmpFiles    PR                  LIKEDS(LST#TmpFile) DIM(9999)
     D   ValFields                         CONST LIKEDS(LST#ValField) DIM(10)
     D   Ordenar                       N   CONST OPTIONS(*NOPASS)
     D
     D WLstResult      DS                  LIKEDS(LST#TmpFile) DIM(9999)
     D WLstParms       DS                  LIKEDS(LST#ValField) DIM(10)
     D WIdxLst         S              5S 0
     D
     D*==> Parametros Recebidos
     D PValFld01       S                   LIKE(GRL_VALFIELD)
     D PValFld02       S                   LIKE(GRL_VALFIELD)
     D PValFld03       S                   LIKE(GRL_VALFIELD)
     D PValFld04       S                   LIKE(GRL_VALFIELD)
     D PValFld05       S                   LIKE(GRL_VALFIELD)
     D
     D*==> Campos de Trabalho
     D WCdBatch        S                   LIKE(GRL_CDBATCH)
     D WNuSequ         S                   LIKE(GRL_NUSEQU)
     D WNumReg         S                   LIKE(GRL_NUMREG)
     D WSeqField       S                   LIKE(GRL_SEQFLD)
     D WCodField       S                   LIKE(GRL_CDFIELD)
     D*==================================================================
     C     *ENTRY        PLIST
     C                   PARM                    PValFld01
     C                   PARM                    PValFld02
     C                   PARM                    PValFld03
     C                   PARM                    PValFld04
     C                   PARM                    PValFld05
     C
      /FREE
       //--------------------------------------------------------
       // Trabalhar Parametros Recebidos
       //--------------------------------------------------------
       WIdxLst = 1;
       WLstResult(WIdxLst).IsEmpty = *ON;
       WLstParms(WIdxLst).IsEmpty  = *ON;

       MONITOR;
          WLstParms(WIdxLst).ValField = PValFld01;
          WLstParms(WIdxLst).IsEmpty  = *OFF;
          WIdxLst += 1;
          WLstParms(WIdxLst).IsEmpty  = *ON;
          MONITOR;
             WLstParms(WIdxLst).ValField = PValFld02;
             WLstParms(WIdxLst).IsEmpty  = *OFF;
             WIdxLst += 1;
             WLstParms(WIdxLst).IsEmpty  = *ON;
             MONITOR;
                WLstParms(WIdxLst).ValField = PValFld03;
                WLstParms(WIdxLst).IsEmpty  = *OFF;
                WIdxLst += 1;
                WLstParms(WIdxLst).IsEmpty  = *ON;
                MONITOR;
                   WLstParms(WIdxLst).ValField = PValFld04;
                   WLstParms(WIdxLst).IsEmpty  = *OFF;
                   WIdxLst += 1;
                   WLstParms(WIdxLst).IsEmpty  = *ON;
                   MONITOR;
                      WLstParms(WIdxLst).ValField = PValFld05;
                      WLstParms(WIdxLst).IsEmpty  = *OFF;
                      WIdxLst += 1;
                      WLstParms(WIdxLst).IsEmpty  = *ON;
                   ON-ERROR;
                   ENDMON;
                ON-ERROR;
             ENDMON;
             ON-ERROR;
          ENDMON;
          ON-ERROR;
          ENDMON;
       ON-ERROR;
          *INLR = *ON;
          RETURN;
       ENDMON;

       WLstResult = $GetTmpFiles(WLstParms);

       *INLR = *ON;
       RETURN;
      /END-FREE

     P*========================================================================
     P* Processo..: GetTmpFiles
     P* Objectivo.: Devolve lista de Ficheiros Temporários activos onde o
     P*             Campos está em utilização.
     P*========================================================================
     P $GetTmpFiles    B                   EXPORT
     D                 PI                  LIKEDS(LST#TmpFile) DIM(9999)
     D   LstField                          CONST LIKEDS(LST#ValField) DIM(10)
     D   Ordenar                       N   CONST OPTIONS(*NOPASS)
     D
     D*==> Campos de Trabalho
     D WIcExiste       S               N
     D WOrdenar        S               N
     D WIdxLst         S              5S 0
     D WIdxTmp         S              5S 0
     D WValField       S                   LIKE(GRL_VALFIELD)
     D WValSecund      S                   LIKE(GRL_VALFIELD)
     D WTmpFiles       DS                  LIKEDS(LST#TmpFile) DIM(9999)
     D*========================================================================
      /FREE
       //--------------------------------------------------------
       // Procede à Pesquisa na Base de Dados
       //--------------------------------------------------------
       WIdxTmp = 1;
       WTmpFiles(WIdxTmp).IsEmpty = *ON;
       WOrdenar = *OFF;
       IF (%PARMS() > 1);
         WOrdenar = Ordenar;
       ENDIF;
       WValField = LstField(1).ValField;

       SETLL *HIVAL RGRLP02302;
       READ RGRLP02302;
       DOW (NOT %EOF(FGRLP02302));
          IF (GRL_CDSTATUS = 'A');
             WIcExiste = *OFF;
             WCdBatch = GRL_CDBATCH;
             WNuSequ  = GRL_NUSEQU;
             WTmpFiles(WIdxTmp).CodFile  = GRL_CDFILE;
             WTmpFiles(WIdxTmp).NomLibl  = GRL_NOLIBL;
             WTmpFiles(WIdxTmp).NomFile  = GRL_NOFILE;
             WTmpFiles(WIdxTmp).DatCria  = $DateToInt(GRL_DTCRIA);
             WTmpFiles(WIdxTmp).HorCria  = %DEC(GRL_HRCRIA);

             //---------------------------------------------
             // Obter Informação sobre os Campos
             //---------------------------------------------
             CHAIN (WCdBatch:WNuSequ:WValField) RGRLP02602;
             IF (%FOUND(FGRLP02602));
                WIcExiste = *ON;
                WNumReg  = GRL_NUMREG;
                WIdxLst = 2;
                DOW (NOT LstField(WIdxLst).IsEmpty);
                   WValSecund = LstField(WIdxLst).ValField;
                   CHAIN (WCdBatch:WNuSequ:WNumReg:WValSecund) RGRLP02601;
                   IF (NOT %FOUND(FGRLP02601));
                      WIcExiste = *OFF;
                      LEAVE;
                   ENDIF;
                   WIdxLst += 1;
                ENDDO;
             ENDIF;

             //---------------------------------------------
             // Completar Informação sobre o Batch
             //---------------------------------------------
             IF (WIcExiste);
                CHAIN (WCdBatch) RGRLP020;
                IF (%FOUND(FGRLP020));
                   WTmpFiles(WIdxTmp).AnoMes   = GRL_ANOMES;
                   WTmpFiles(WIdxTmp).CodLetra = GRL_CDLETRA;
                ENDIF;

                WTmpFiles(WIdxTmp).IsEmpty = *OFF;
                WIdxTmp += 1;
                WTmpFiles(WIdxTmp).IsEmpty = *ON;
             ENDIF;
          ENDIF;
          READ RGRLP02302;
       ENDDO;
       RETURN WTmpFiles;
      /END-FREE
     P $GetTmpFiles    E
