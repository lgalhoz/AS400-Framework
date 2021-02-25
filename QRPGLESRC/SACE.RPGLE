     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*===========================================================
     H* Programa..: PINTO050
     H* Descrição.: Gerar interfaces para os Mediadores, para o efeito
     H*             Localizar os ficheiros temporários gerados nos
     H*             Sistema, e por cada novo que localizar, procede
     H*             à extracção dos registos.
     H*
     H* Para Compilar este programa fazer:
     H*    CRTPF FILE(QTEMP/INTFILE) SRCFILE(#JMPRDSRC/QDDSSRC) SRCMBR(FINTT020)
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
     D   FileToProces                21A   CONST
     D   TipFile                      2A   CONST
     D   DatFile                      8S 0 CONST
     D   CodErro                      7A   CONST
     D WDynamic        S             10A
     D
     D*==> Campos de Trabalho
     D WCdBatch        S              9S 0
     D WSeqFile        S              5S 0
     D WCdFile         S              5S 0
     D WDatInicio      S              8S 0
     D WSessTempor     S              5S 0
     D WIdx            S              3S 0
     D WLastState      S              3A
     D WCdProcess      S              5S 0
     D WNoLibl         S             10A
     D WNoFile         S             10A
     D WDatFile        S              8S 0
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
     D TIP#Recibos     C                   'REC'
     D TIP#Apolices    C                   'APL'
     D RAM#Automovel   C                   'AU'
     D RAM#Outros      C                   'OR'
     D RAM#Transport   C                   'TR'
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
           WSessTempor = $SetFiles('FGRLP020 B INNER JOIN FGRLP023 F ON +
                                    (B.CDBATCH = F.CDBATCH)');
           $SetCriteria('(B.CDFILE IN (21,22,23,30,31)');
           $SetFields('A.CDBATCH,B.NUSEQU,B.NOFILE,B.NOLIBL');
           WIdx = 1;
           DOW ($GetRecord(WIdx) > *ZEROS);
              WCdBatch = $GetFieldNum('B.CDBATCH');
              WSeqFile = $GetFieldNum('B.NUSEQU');
              WCdFile  = $GetFieldNum('B.CDFILE');
              WNoLibl  = $GetField('B.NOLIBL');
              WNoFile  = $GetField('B.NOFILE');
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
           IF (WTipFile = TIP#Apolices);
              WDynamic = 'RINTO050';
           ELSEIF (WTipFile = TIP#Recibos);
              WDynamic = 'RINTO051';
           ENDIF;

           MONITOR;
              FillInterface(WCdProcess:WFileToRead:WCodErro:
                            WTipFile:WDatFile:WCodErro);
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
           SETLL (*LOVAL) RINTP055;
           READP(N) RINTP055;
           IF (%FOUND(FINTP055));
              WCdProcess = CTL_CDPROCE;
              WLastState = CTL_CDESTADO;
              IF (CTL_CDESTADO = STS#Aberto);
                 WDatInicio = CTL_DTULTPRC;
                 WNuSequ = $GetNextKey('FINTP056':'NUSEQU':
                                       '(CDPROCE = '''+%CHAR(WCdProcess)+'''');
              ELSEIF (CTL_CDESTADO = STS#SemEfeito);
                 WDatInicio = CTL_DTINICIO;
              ELSE;
                 WDatInicio = CTL_DTTERMO;
              ENDIF;
           ENDIF;

           IF (WLastState <> 'OPN');
              WCdProcess = $GetNextKey('FINTP055':'CDPROCE');
              WNuSequ    = 1;
              CTL_CDPROCE = WCdProcess;
              CTL_CDESTADO = STS#Aberto;
              CTL_DTINICIO = WDatInicio;
              CTL_DTTERMO = 99999999;
              CTL_DTULTPRC = $DateToInt(%DATE());
              CTL_INT05401 = %DATE();
              CTL_INT05402 = %DATE();
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
              CTL_INT05402 = %DATE();
              CTL_DTULTPRC = $DateToInt(%DATE());
              UPDATE RINTP055 %FIELDS(CTL_INT05402:CTL_DTULTPRC);
           ENDIF;
        ENDSR;
      /END-FREE
