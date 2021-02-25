     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*===========================================================
     H* Programa..: PINTO050
     H* Descrição.: Gerar interfaces para os Mediadores, para o efeito
     H*             Localizar os ficheiros temporários gerados nos
     H*             Sistema, e por cada novo que localizar, procede
     H*             à extracção dos registos.
     H*
     H* Ficheiros a Processar:
     H*   CDFILE    Descrição
     H*     21      Diário de Automóvel
     H*     22      Diário de Outros Ramos
     H*     23      Diário de Transportes
     H*     30      Guias (Recibos) Diários
     H*     30      Guias (Recibos) Continuados
     H*===========================================================
     F*==> Informação sobre os Mediadores Alvo
     FFINTP055  UF A E           K DISK    PREFIX(CTL_)
     FFINTP056  UF A E           K DISK    PREFIX(CTL_)
     FFINTP05601IF   E           K DISK    PREFIX(CTL_)
     F                                     RENAME(RINTP056:RINTP05601)
     F*===========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010

     D*==> Chamada dinâmica à criação dos registos
     D FillInterface   PR                  EXTPGM(WDynamic)
     D   CodProcess                   5S 0 CONST
     D   CodFile                      5S 0 CONST
     D   FileToProces                21A   CONST
     D   TipInterface                 3A   CONST
     D   DatFile                      8S 0 CONST
     D   CodErro                      7A   CONST
     D WDynamic        S             10A
     D
     D*==> Campos de Trabalho
     D WCdBatch        S              9S 0
     D WSeqFile        S              5S 0
     D WCdFile         S              5S 0
     D WDatInicio      S              8S 0
     D WDatTermo       S              8S 0
     D WSessTempor     S              5S 0
     D WIdx            S              3S 0
     D WLastState      S              3A
     D WCdProcess      S              5S 0
     D WNoLibl         S             10A
     D WNoFile         S             10A
     D WDatIniChr      S             10A
     D WDatFimChr      S             10A
     D WDatIniInt      S              8S 0
     D WDatFimInt      S              8S 0
     D WFileToRead     S             21A
     D WCodErro        S              7A
     D WNuSequ         S              5S 0
     D WTipFile        S              3A
     D
     D*==> Declaração de Constantes
     D STS#Aberto      C                   'OPN'
     D STS#SemEfeito   C                   'SEF'
     D STS#Enviado     C                   'ENV'
     D STS#Cancelado   C                   'CAN'
     D STS#Fechado     C                   'CLO'
     D TIP#Recibos     C                   'REC'
     D TIP#Segurado    C                   'SEG'
     D TIP#AplAuto     C                   'AAU'
     D TIP#AplOutros   C                   'AOR'
     D TIP#AplTransp   C                   'ATR'
     D*===========================================================
      /FREE
        //-----------------------------------------------------------------
        // Seleccionar a data do último processo de extracção Mediadores
        //-----------------------------------------------------------------
        WCodErro = *BLANKS;
        EXSR OpenControlo;

        IF (WCodErro = *BLANKS);
           EXSR SelTemporarios;
        ENDIF;

        IF (WCodErro = *BLANKS);
           EXSR CloseControlo;
        ENDIF;
        RETURN;

        //================================================================
        // Subrotina.: SelTemporarios
        // objectivo.: Processar ficheiros Temporários
        //================================================================
        BEGSR SelTemporarios;
           //-----------------------------------------------------------------
           // Seleccionar Ficheiros Temporários gerados depois da data
           //-----------------------------------------------------------------
           WDatFimChr  = %CHAR($IntToDate(WDatTermo));
           IF (WDatInicio > *ZEROS);
              WDatIniChr  = %CHAR($IntToDate(WDatInicio));
           ELSE;
              WDatIniChr = '1900-01-01';
           ENDIF;

           WSessTempor = $SetFiles('FGRLP020 B INNER JOIN FGRLP023 F ON +
                                    (B.CDBATCH = F.CDBATCH)');
           $SetCriteria('(F.CDFILE IN (21, 22, 23, 30, 31)) AND +
                         (F.DTCRIA >= ''' + WDatIniChr + ''') +
                          AND (F.DTCRIA <= ''' + WDatFimChr + ''')');
           $SetFields('B.CDBATCH,F.NUSEQU,F.CDFILE,F.NOFILE,F.NOLIBL,+
                         #F.DTCRIA');
           WIdx = 1;
           DOW ($GetRecord(WIdx) > *ZEROS);
              WDatIniChr = $GetField('#F.DTCRIA');
              WCdBatch   = $GetFieldNum('B.CDBATCH');
              WSeqFile   = $GetFieldNum('F.NUSEQU');
              WCdFile    = $GetFieldNum('F.CDFILE');
              WNoLibl    = $GetField('F.NOLIBL');
              WNoFile    = $GetField('F.NOFILE');
              WDatIniInt = $DateToInt(%DATE(WDatIniChr));
              EXSR ExtFiles;
              Widx += 1;
           ENDDO;
           $RmvDBSession(WSessTempor);
        ENDSR;

        //================================================================
        // Subrotina.: ExtFiles
        // objectivo.: Ler os ficheiros a Processar
        //================================================================
        BEGSR ExtFiles;
           //---------------------------------------------------
           // Validar se o ficheiro já foi processado
           //---------------------------------------------------
           SETLL (WCdBatch:WSeqFile) RINTP05601;
           READE (WCdBatch:WSeqFile) RINTP05601;
           DOW (NOT %EOF(FINTP05601));
               IF (CTL_CDERRO = *BLANKS);
                  LEAVESR; // O ficheiro já foi processado
               ENDIF;
               READE (WCdBatch:WSeqFile) RINTP05601;
           ENDDO;

           //---------------------------------------------------
           // Identificar o tipo de ficheiro
           // Extrair os registos e actualizar as Interfaces
           //---------------------------------------------------
           WFileToRead = %TRIM(WNolibl)+'/'+%TRIM(WNoFile);
           IF (WCdFile = 30) OR (WcdFile = 31);
              WTipFile = TIP#Recibos;
              WDynamic = 'RINTO051';  // Extrair Recibos
           ELSE;
              WDynamic = 'RINTO050';  // Extrair Apólices
              IF (WCdFile = 21);
                 WTipFile = TIP#AplAuto;
              ELSEIF (WCdFile = 22);
                 WTipFile = TIP#AplOutros;
              ELSEIF (WCdFile = 23);
                 WTipFile = TIP#AplTransp;
              ENDIF;
           ENDIF;

           MONITOR;
              FillInterface(WCdProcess:WNusequ:WFileToRead:
                            WTipFile:WDatIniInt:WCodErro);
           ON-ERROR;
              WCodErro = 'INTXXXX';
              LEAVESR;
           ENDMON;

           //---------------------------------------------------
           // Actualizar o ficheiro processado
           //---------------------------------------------------
           IF (WCodErro <> *BLANKS);
              ROLBK; // Desfazer as alterações
              WCodErro = *BLANKS;
           ELSE;
              COMMIT;   // Efectivar as alterações
           ENDIF;

           CTL_CDPROCE = WCdProcess;
           CTL_NUSEQU  = WNuSequ;
           CTL_CDBATCH = WCdBatch;
           CTL_CDFILE  = WSeqFile;
           CTL_NOLIBL  = WNoLibl;
           CTL_DTPROCES= $DateToInt(%DATE());
           CTL_CDERRO  = WCodErro;
           CTL_INT05601= %DATE();
           CTL_INT05602= %DATE();
           WRITE RINTP056;
           WNuSequ += 1;
        ENDSR;

        //================================================================
        // Subrotina.: OpenControlo
        // objectivo.: Abrir o controlo dos Interfaces
        //================================================================
        BEGSR OpenControlo;
           SETLL (*HIVAL) FINTP055;
           READP(N) FINTP055;
           IF (NOT %EOF(FINTP055));
              WCdProcess = CTL_CDPROCE;
              WLastState = CTL_CDESTADO;
              IF (CTL_CDESTADO = STS#Aberto);
                 WDatInicio = CTL_DTULTPRC;
                 WDatTermo  = CTL_DTTERMO;
                 WNuSequ = $GetNextKey('FINTP056':'NUSEQU':
                                       '(CDPROCE = '''+%CHAR(WCdProcess)+''')');
              ELSEIF (CTL_CDESTADO = STS#SemEfeito);
                 WDatInicio = CTL_DTINICIO;
              ELSEIF (CTL_CDESTADO = STS#Fechado);
                 WDatInicio = CTL_DTULTPRC;
                 WDatTermo  = 99999999;
              ELSE;
                 WCodErro = 'INTXXXX';
                 LEAVESR;
              ENDIF;
           ENDIF;

           IF (WLastState <> 'OPN');
              WCdProcess = $GetNextKey('FINTP055':'CDPROCE');
              WNuSequ    = 1;
              CTL_CDPROCE  = WCdProcess;
              CTL_CDESTADO = STS#Aberto;
              CTL_DTINICIO = WDatInicio;
              CTL_DTTERMO  = WDatTermo;
              CTL_DTULTPRC = WDatInicio;
              CTL_INT05501 = %DATE();
              CTL_INT05502 = %DATE('9999-12-31');
              WRITE RINTP055;
           ENDIF;
        ENDSR;

        //================================================================
        // Subrotina.: CloseControlo
        // objectivo.: Fechar o ficheiro de Controlo
        //================================================================
        BEGSR CloseControlo;
           CHAIN (WCdProcess) RINTP055;
           IF (%FOUND(FINTP055));
              CTL_INT05502 = %DATE('9999-12-31');
              CTL_DTULTPRC = WDatIniInt;
              UPDATE RINTP055 %FIELDS(CTL_INT05502:CTL_DTULTPRC);
           ENDIF;
        ENDSR;
      /END-FREE
