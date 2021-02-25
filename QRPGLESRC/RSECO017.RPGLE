     H NOMAIN
     H*=======================================================
     H* Dat.Cria...: 2013-04-11
     H* Objectivo..: Grupo de Funções para gerir as Funções Batch
     H*=======================================================
     FFSECP01003IF   E           K DISK    PREFIX(SEC_)
     F                                     RENAME(RSECP010:RSECP01003)
     FFSECP07001IF   E           K DISK    PREFIX(SEC_)
     F                                     RENAME(RSECP070:RSECP07001)
     FFSECP071  IF   E           K DISK    PREFIX(SEC_)
     FFSECP07101IF   E           K DISK    PREFIX(SEC_)
     F                                     RENAME(RSECP071:RSECP07101)
     FFINTP001  UF A E           K DISK    PREFIX(INT_)
     FFINTP002  UF A E           K DISK    PREFIX(INT_)
     FFINTP00201IF   E           K DISK    PREFIX(INT_)
     F                                     RENAME(RINTP002:RINTP00201)
     FFINTP003  UF A E           K DISK    PREFIX(INT_)
     FFINTP004  UF A E           K DISK    PREFIX(INT_)
     F*=======================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SSECO010

     D*==> Variáveis Globais
     D  WCodBatch      S                   LIKE(SEC_CDBATCH)
     D  WCodFuncao     S                   LIKE(SEC_CDPROGR)
     D  WCdRemessa     S                   LIKE(INT_CDREMESSA)
     D*=======================================================

     C*=======================================================
     C* Processo..: $ValBatch
     C* Objectivo.: Validar Execução da função na Cadeia Batch
     C*=======================================================
     P $ValBatch       B                   EXPORT
     D                 PI              N
     D  AppName                      10A   CONST
     D  NomBatch                      5A   CONST
     D
     D  WNuSequ        S              5S 0
     D  WCodFuncAnt    S                   LIKE(SEC_CDPROGR)
     D  WResult        S               N
     D*================================================================
      /FREE
        //-----------------------------------------------------
        // Obter o Código do Processo Batch
        //-----------------------------------------------------
        CHAIN (NomBatch) RSECP07001;
        IF (NOT %FOUND);
           RETURN *OFF; // Processo Batch não registado
        ENDIF;
        WCodBatch = SEC_CDBATCH;

        //-----------------------------------------------------
        // Obter o Código de Função
        //-----------------------------------------------------
        WCodFuncao = *ZEROS;
        SETLL (AppName) RSECP01003;
        READE (AppName) RSECP01003;
        DOW (NOT %EOF());
           IF (SEC_CDTIPO = 'B');
              WCodFuncao = SEC_CDPROGR;
              LEAVE;
           ENDIF;
           READE (AppName) RSECP01003;
        ENDDO;
        IF (WCodFuncao = *ZEROS);
           RETURN *OFF; // Função não registada
        ENDIF;

        //-----------------------------------------------------
        // Obter o Código esperado para a função anterior
        //-----------------------------------------------------
        CHAIN (WCodBatch:WCodFuncao) RSECP07101;
        IF (NOT %FOUND());
           RETURN *OFF; // Função não registada no Processo
        ENDIF;
        WNuSequ = SEC_NUSEQU-1;
        IF (WNuSequ = *ZEROS);
           WCodFuncAnt = 99999;
        ELSE;
           CHAIN (WCodBatch:WNuSequ) RSECP071;
           IF (NOT %FOUND());
              RETURN *OFF;  // Erro na definição da Cadeia Batch
           ENDIF;
           WCodFuncAnt = SEC_CDPROGR;
        ENDIF;

        //-----------------------------------------------------
        // Validar se está em execução o gerador de Interfaces
        //-----------------------------------------------------
        SETLL (*HIVAL) RINTP001;
        READP(N) RINTP001;
        DOW (NOT %EOF());
           IF (INT_NOBATCH = NomBatch);
              IF (INT_CDPRCEXEC <> WCodFuncAnt);
                 RETURN *OFF; // Não está na Vez de ser executada
              ENDIF;
           ENDIF;
           READP(N) RINTP001;
        ENDDO;

        //-----------------------------------------------------
        // Criar o cabeçalho para o Iniciar o Processo
        //-----------------------------------------------------
        IF (WCodFuncAnt = 99999);
           WCdRemessa    = $GetNextKey('FINTP001':'CDREMESSA');
           INT_CDREMESSA = WCdRemessa;
           INT_CDENTIDAD = 1;
           INT_NOBATCH   = NomBatch;
           INT_CDUSER    = S#USR;
           INT_CDPRCEXEC = *ZEROS; // Operação Aberta
           INT_INT00101  = %TIMESTAMP();
           INT_INT00102  = %TIMESTAMP();
           WRITE RINTP001;
        ELSE;
           WCdRemessa = INT_CDREMESSA;
        ENDIF;
        RETURN *ON;
      /END-FREE
     P $ValBatch       E

     C*=======================================================
     C* Processo..: $AddBatchInp
     C* Objectivo.: Adicionar ficheiro Input usado pela Função
     C*=======================================================
     P $AddBatchInp    B                   EXPORT
     D                 PI              N
     D  CodBatch                      9S 0 CONST
     D  CodFile                       5S 0 CONST
     D  CodFuncao                     5S 0 CONST
     D
     D  WResult        S             10A
     D*================================================================
      /FREE
       CHAIN(N) (WCdRemessa:CodBatch) RINTP002;
       IF (NOT %FOUND());
          INT_CDREMESSA = WCdRemessa;
          INT_CDBATCH   = CodBatch;
          INT_CDFUNCAO  = CodFuncao;
          INT_CDSTATUS  = 'O'; // Operação Aberta
          INT_INT00201  = %TIMESTAMP();
          INT_INT00202  = %TIMESTAMP();
          WRITE RINTP002;
       ENDIF;

       CHAIN(N) (WCdRemessa:CodBatch:CodFile) RINTP003;
       IF (NOT %FOUND());
          INT_CDREMESSA = WCdRemessa;
          INT_CDBATCH   = CodBatch;
          INT_NUSEQU    = CodFile;
          INT_CDSTATUS  = 'O';  // Operação Aberta
          INT_INT00301  = %TIMESTAMP();
          INT_INT00302  = %TIMESTAMP();
          WRITE RINTP003;
       ENDIF;

       RETURN *ON;
      /END-FREE
     P $AddBatchInp    E

     C*=======================================================
     C* Processo..: $AddBatchOut
     C* Objectivo.: Adicionar ficheiro output gerado pela Função
     C*=======================================================
     P $AddBatchOut    B                   EXPORT
     D                 PI              N
     D  NomFile                      10A   CONST
     D  QtdeRegis                     9S 0 CONST
     D  CodAcction                    1A   CONST Options(*NOPASS)
     D
     D*==> Estrutura dos comandos AS400
     D Exec            PR                  EXTPGM('QCMDEXC')
     D   stdcmd                     300A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5
     D
     D*==> Campos de Trabalho
     D WStdCmd         S            300A
     D WStdLen         S             15P 5
     D WDatChar        S             20A
     D WIcEnviar       S               N
     D WNomPase        S                   LIKE(INT_NOMPASE)
     D*================================================================
      /FREE
       //-----------------------------------------------------
       // Validar se existe instrução para enviar
       //-----------------------------------------------------
       IF (%PARMS() > 1);
          WIcEnviar = *ON;
          WDatChar = %CHAR(%SUBDT(%DATE():*YEARS)*10000+
                           %SUBDT(%DATE():*MONTHS)*100+
                           %SUBDT(%DATE():*DAYS)) + '_' +
                     %CHAR(%SUBDT(%TIME():*HOURS)*10000+
                           %SUBDT(%TIME():*MINUTES)*100+
                           %SUBDT(%TIME():*SECONDS));
          WNomPase = %TRIM(NomFile)+'_'+%TRIM(WDatChar);
       ELSE;
          WIcEnviar = *OFF;
          WNomPase = *BLANKS;
       ENDIF;

       //-----------------------------------------------------
       // Enviar o Ficheiro
       //-----------------------------------------------------
       IF (WIcEnviar);
          MONITOR;
             //==> Esta Aberração é por causa dos caracteres portugueses....
             WStdCmd = 'SNDPF OUTFILE(QTEMP/'+%TRIM(NomFile)+') +
                          DESTINO('''+%TRIM(NomFile)+'.INT'')';
             WStdLen = %LEN(WStdCmd);
             EXEC(WStdCmd:WStdLen);

             WStdCmd = 'QSH CMD(''scp root@20.0.0.3:/share/+
                                AS400Docs/'+%TRIM(NomFile)+'.INT +
                                /interfaces/pendente/'+
                                %TRIM(WNomPase)+''')';
             WStdLen = %LEN(WStdCmd);
             EXEC(WStdCmd:WStdLen);
          ON-ERROR;
             RETURN *OFF; // Foram detectados erros
          ENDMON;
       ENDIF;

       //--------------------------------------------------------------
       // Registar informação
       //--------------------------------------------------------------
       INT_CDREMESSA  = WCdRemessa;
       INT_NUSEQU     = $GetNextKey('FINTP004':'NUSEQU':
                       'CDREMESSA = ''' + %CHAR(WCdRemessa) + '''');
       INT_NURECORD   = QtdeRegis;
       INT_NOMAS400   = NomFile;
       INT_NOMPASE    = WNomPase;
       INT_ICENVIADO  = *ON;
       INT_DTENVIO    = %DATE();
       WRITE RINTP004;

       RETURN *ON; // Operação realizada com sucesso
      /END-FREE
     P $AddBatchOut    E

     C*=======================================================
     C* Processo..: $ChgBatchStat
     C* Objectivo.: Actualizar informação do Processo Batch
     C*=======================================================
     P $ChgBatchStat   B                   EXPORT
     D                 PI              N
     D  DatAntiga                      D   CONST Options(*NOPASS)
     D  DatRecente                     D   CONST Options(*NOPASS)
     D
     D*==> Campos de Trabalho
     D  WDatAntiga     S               D
     D  WDatRecente    S               D
     D  WIcChgDate     S               N
     D  WResult        S               N
     D*================================================================
      /FREE
       //------------------------------------------------
       // Confirmar se recebeu datas como parâmetro
       //------------------------------------------------
       IF (%PARMS() > *ZEROS);
          WDatAntiga  = DatAntiga;
          WDatRecente = DatRecente;
          WIcChgDate  = *ON;
       ELSE;
          WIcChgDate =  *OFF;
       ENDIF;

       //------------------------------------------------
       // Alterar Processo
       //------------------------------------------------
       CHAIN (WCdRemessa) RINTP001;
       IF (%FOUND());
          INT_CDPRCEXEC = WCodFuncao;
          INT_DTINICIO  = WDatAntiga;
          INT_DTTERMO   = WDatRecente;
          IF (WIcChgDate);
             UPDATE RINTP001 %FIELDS(INT_CDPRCEXEC:
                                      INT_DTINICIO:INT_DTTERMO);
          ELSE;
             UPDATE RINTP001 %FIELDS(INT_CDPRCEXEC);
          ENDIF;
       ELSE;
          RETURN *OFF; // Erro na obtenção do registo
       ENDIF;

       RETURN *ON;
      /END-FREE
     P $ChgBatchStat   E

     C*=======================================================
     C* Processo..: $getBatchSett
     C* Objectivo.: Obter Informação do Processo
     C*=======================================================
     P $getBatchSett   B                   EXPORT
     D                 PI              D
     D  TipData                       1A   CONST
      /FREE
        CHAIN(N) (WCdRemessa) RINTP001;
        IF (%FOUND());
          IF (TipData = 'A'); // Mais Antiga
             RETURN INT_DTINICIO;
          ELSEIF (TipData = 'R'); // Mais Recente
             RETURN INT_DTTERMO;
          ENDIF;
        ENDIF;
        RETURN;
      /END-FREE
     P $getBatchSett   E

     C*=======================================================
     C* Processo..: $rmvBatch
     C* Objectivo.: Colocar sem efeito a operação Batch
     C*=======================================================
     P $rmvBatch       B                   EXPORT
     D                 PI              N
      /FREE
        CHAIN (WCdRemessa) RINTP001;
        IF (%FOUND());
           CHAIN(N) (WCdRemessa) RINTP004;
           IF (NOT %FOUND());
              DELETE RINTP001;
           ELSE;
              RETURN *OFF;
           ENDIF;
        ENDIF;
        RETURN *ON;
      /END-FREE
     P $rmvBatch       E
