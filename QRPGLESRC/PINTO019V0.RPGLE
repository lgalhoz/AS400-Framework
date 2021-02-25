     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*===========================================================
     H* Programa..: PINTO010
     H* Descrição.: Carregar Sinistros IDS Automóvel
     H*
     H* Para Compilar este programa fazer:
     H*    CRTPF FILE(QTEMP/FINTT030) SRCFILE(#JMPRDSRC/QDDSSRC)
     H*    CRTPF FILE(QTEMP/INTFILE) SRCFILE(#JMPRDSRC/QDDSSRC) SRCMBR(FINTT030)
     H*
     H* Para correr este programa em DEBUG fazer:
     H*    CALL PGM(#JMPRDOBJ/PGRLO015)
     H*===========================================================
     FFINTP010  UF A E           K DISK    PREFIX(INT_)
     FFINTP01001IF   E           K DISK    PREFIX(INT_)
     F                                     RENAME(RINTP010:RINTP01001)
     FFICIDS    UF A E           K DISK    PREFIX(IDS_) USROPN
     F                                     EXTFILE(WExtFile)
     FFINTT030  IF   E           K DISK    PREFIX(TMP_) USROPN
     F                                     EXTFILE('QTEMP/INTFILE')
     F*===========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010

     D*==> converter um Campo alfa num número
     D CATN            PR            15S 5
     D   ALFA                        15A   Const
     D   DFT                         15S 5 Const
     D
     D*==> Chamada de função Dynamic, para trabalhar detalhe do Item
     D ShellExec       PR                  EXTPGM('QCMDEXC')
     D   stdcmd                     300A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5 CONST
     D
     D*==> Campos de Trabalho
     D WNomFile        S             40A
     D WCdInter        S              9S 0
     D WAction         S              1A
     D WResposta       S              1A
     D WMsgErro        S              7A
     D
     D WComando        S            300A   INZ(*BLANKS)
     D WDatFile        S             10A
     D WAAMMDD         S              6A
     D WExtFile        S             21A
     D WNumProc        S              9A
     D WCdFile         S              7S 0
     D WNuSequ         S              5S 0
     D WCdRegis        S              9S 0
     D WIsFirst        S               N
     D
     D*==> Declaração de Constantes
     D ACT#Validar     C                   'V'
     D ACT#Processar   C                   'P'
     D*===========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WNomFile
     C                   PARM                    WCdInter
     C                   PARM                    WAction
     C                   PARM                    WResposta
     C                   PARM                    WMsgErro
      /FREE
        //-----------------------------------------------------------------
        // Validar se o Ficheiro ja foi Processado
        //-----------------------------------------------------------------
        CHAIN (WNomFile) RINTP01001;
        IF (%FOUND());
           WMsgErro = 'INT0001'; // O ficheiro já foi processado
           WResposta = 'N'; // Não é para processar
           RETURN;
        ELSEIF (WAction = ACT#Validar);
           WResposta = 'S'; // É para processar
           WMsgErro = *BLANKS;
           RETURN;
        ENDIF;

        WIsFirst = *ON;
        EXSR ZapFile;
        IF (WMsgErro = *BLANKS);
           EXSR ReadInterface;
        ENDIF;
        *INLR = *ON;

       //=================================================================
       // SubRotina..: ReadInterface
       // Objectivo..: Ler o ficheiro de Interface recebido
       //=================================================================
       BEGSR ReadInterface;
          OPEN FICIDS;
          OPEN FINTT030;
          SETLL *LOVAL RINTT030;
          READ FINTT030;
          DOW (NOT %EOF(FINTT030));
             //-----------------------------------
             // Registar Cabeçalho do Registo
             //-----------------------------------
             IF (WIsFirst);
                WIsFirst = *OFF;
                EXSR RegisFile;
             ENDIF;

             //-----------------------------------------
             // Carregar Interface
             //-----------------------------------------
             WNumProc    = %TRIM(TMP_NUMPROC) + %TRIM(TMP_ANOPROC);
             IF (WnumProc <> *BLANKS);
                IDS_TIPPRO  = TMP_TIPO;            // Tipo de Registo
                IDS_NUMPRO  = CATN(WNumProc:0);    // Número do Processo
                IDS_DATPRO  = CATN(TMP_DATPROC:0); // Data do Processo
                IDS_DATSIN  = CATN(TMP_DATSINO:0); // Data do Sinistro
                IDS_NUMAPO  = CATN(TMP_NUAPOL:0);  // Número da Apólice
                IDS_MATRIC  = TMP_MATRIC;          // Número da Apólice
                IDS_CUSPRO  = CATN(TMP_IMPPROC:0); // Custo do Processo
                IDS_DATENC  = CATN(TMP_DATENCE:0); // Data de Encerramento
                IDS_NUMSEG  = CATN(TMP_NUMSEGU:0); // Código do Segurado
                IDS_NOMSEG  = TMP_NOMSEGU;         // Nome do Segurado
                WRITE FICIDSF1;
             ENDIF;
             READ RINTT030;
          ENDDO;
          CLOSE FINTT030;
          CLOSE FICIDS;
       ENDSR;

       //=================================================================
       // SubRotina..: ZapFile
       // Objectivo..: Limpar ficheiro antes de carregar com dados
       //=================================================================
       BEGSR ZapFile;
          MONITOR;
             WAAMMDD  = %SUBST(WNomFile:17:6);
             WExtFile = 'QS36F/FICI' + %TRIM(WAAMMDD);
             WComando = 'CRTPF FILE(' + %TRIM(WExtFile) + ') +
                               SRCFILE(SOURCE/QDDSSRC) +
                               SRCMBR(FICIDS) SIZE(*NOMAX)';
             ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
          ON-ERROR;
             WMsgErro = 'XXXXXXX';
          ENDMON;

          MONITOR;
             WComando = 'CLRPFM FILE(' + %TRIM(WExtFile) + ')';
             ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
          ON-ERROR;
             WMsgErro = 'XXXXXXX';
          ENDMON;
       ENDSR;

       //=================================================================
       // SubRotina..: RegisFile
       // Objectivo..: Registar o Ficheiro
       //=================================================================
       BEGSR RegisFile;
          WNuSequ     = 1;
          WCdRegis    = $GetNextKey('FENTP010':'CDREGIS');
          WCdFile     = $GetNextKey('FINTP010':'CDFILE');
          INT_CDFILE  = WCdFile;
          INT_NOMFILE = WNomFile;
          MONITOR;
             WDatFile    = %SUBST(WNomFile:15:4) + '-' +
                           %SUBST(WNomFile:19:2) + '-' +
                           %SUBST(WNomFile:21:2);
             INT_DTFILE  = %DATE(WDatFile:*ISO); // ForCompr com a data do ficheiro
          ON-ERROR;
             INT_DTFILE  = %DATE();
          ENDMON;
          INT_DTPROC  = %DATE();
          WRITE RINTP010;
       ENDSR;
      /END-FREE
     P*=======================================================
     P* Processo..: CATN
     P* Descrição.: Converter campo alfa num Numero
     P*=======================================================
     P CATN            B                   EXPORT
     D                 PI            15S 5
     D   ALFA                        15A   Const
     D   DFT                         15S 5 Const
     D WResult         S             15S 0
     D*===========================================================
      /FREE
         WResult = DFT;
         IF (ALFA <> *BLANKS);
            MONITOR;
               WResult = %DEC(ALFA:15:5);
            ON-ERROR;
            ENDMON;
         ENDIF;

         RETURN WResult;
      /END-FREE
     P CATN            E
