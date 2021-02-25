     H NOMAIN
     H*==================================================================
     H* Função....: RGRLO030
     H* Descrição.: Este componente, pesquisa os ficheiros temporários
     H*             que obedecem ao critério de pesquisa. Estes ficheiros
     H*             Temporário, guardam informação de histórico das
     H*             Apólices, Recibos, Actas, Cartas Verdes entre Outras.
     H*==================================================================
     FFGRLP020  IF   E           K DISK    USROPN PREFIX(GRL_)
     FFGRLP02302IF   E           K DISK    USROPN PREFIX(GRL_)
     F                                     RENAME(RGRLP023:RGRLP02302)
     FFGRLP02303IF   E           K DISK    USROPN PREFIX(GRL_)
     F                                     RENAME(RGRLP023:RGRLP02303)
     FFGRLP02601IF   E           K DISK    USROPN PREFIX(GRL_)
     F                                     RENAME(RGRLP026:RGRLP02601)
     FFGRLP02602IF   E           K DISK    USROPN PREFIX(GRL_)
     F                                     RENAME(RGRLP026:RGRLP02602)
     F*==================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SGRLO020

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
     D WCdBatch        S                   LIKE(GRL_CDBATCH)
     D WNuSequ         S                   LIKE(GRL_NUSEQU)
     D WNumReg         S                   LIKE(GRL_NUMREG)
     D*WSeqField       S                   LIKE(GRL_SEQFLD)
     D*WCodField       S                   LIKE(GRL_CDFIELD)
     D*========================================================================
      /FREE
       //--------------------------------------------------------
       // Procede à Pesquisa na Base de Dados
       //--------------------------------------------------------
       OPEN FGRLP020;
       OPEN FGRLP02302;
       OPEN FGRLP02303;
       OPEN FGRLP02601;
       OPEN FGRLP02602;
       WOrdenar = *ON;
       WIdxTmp = 1;
       WTmpFiles(WIdxTmp).IsEmpty = *ON;
       IF (%PARMS() > 1);
         WOrdenar = Ordenar;
       ENDIF;
       WValField = LstField(1).ValField;

       //----
       IF (WOrdenar);
       // SETLL *HIVAL RGRLP02302;
          READ RGRLP02302;
          DOW (NOT %EOF(FGRLP02302));
             EXSR ExtFields;
             READ RGRLP02302;
          ENDDO;
       ELSE;
       // SETLL *HIVAL RGRLP02303;
          READ RGRLP02303;
          DOW (NOT %EOF(FGRLP02303));
             EXSR ExtFields;
             READ RGRLP02303;
          ENDDO;
       ENDIF;
       CLOSE FGRLP020;
       CLOSE FGRLP02302;
       CLOSE FGRLP02303;
       CLOSE FGRLP02601;
       CLOSE FGRLP02602;
       RETURN WTmpFiles;

       //====================================================================
       // Subrotina..: ExtFields
       // Descrição..: Extrair os Campos que obedecem ao critério
       //====================================================================
       BEGSR ExtFields;
          IF (GRL_CDSTATUS = 'A');
             WIcExiste = *OFF;
             WCdBatch = GRL_CDBATCH;
             WNuSequ  = GRL_NUSEQU;
             WTmpFiles(WIdxTmp).CodFile  = GRL_CDFILE;
             WTmpFiles(WIdxTmp).IcHistor = GRL_ICHISTOR;
             WTmpFiles(WIdxTmp).NomLibl  = GRL_NOLIBL;
             WTmpFiles(WIdxTmp).NomFile  = GRL_NOFILE;
             WTmpFiles(WIdxTmp).DatCria  = $DateToInt(GRL_DTCRIA);
             WTmpFiles(WIdxTmp).HorCria  = %DEC(GRL_HRCRIA);

             //---------------------------------------------
             // Obter Informação sobre os Campos
             //---------------------------------------------
             SETLL (WCdBatch:WNuSequ:WValField) RGRLP02602;
             READE (WCdBatch:WNuSequ:WValField) RGRLP02602;
             DOW (NOT %EOF(FGRLP02602));
                WIcExiste = *OFF;
                WNumReg  = GRL_NUMREG;
                WIdxLst = 2;
                DOW (NOT LstField(WIdxLst).IsEmpty);
                   WValSecund = LstField(WIdxLst).ValField;
                   CHAIN (WCdBatch:WNuSequ:WNumReg:WValSecund) RGRLP02601;
                   IF (NOT %FOUND(FGRLP02601));
                      WIcExiste = *OFF;
                      LEAVE;
                   ELSE;
                      WIcExiste = *ON;
                   ENDIF;
                   WIdxLst += 1;
                ENDDO;
                IF (WIcExiste);
                   LEAVE;
                ENDIF;
                READE (WCdBatch:WNuSequ:WValField) RGRLP02602;
             ENDDO;

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
       ENDSR;
      /END-FREE
     P $GetTmpFiles    E
