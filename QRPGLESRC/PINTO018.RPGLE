     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*===========================================================
     H* Programa..: PINTO018
     H* Descri��o.: Carregar Interface Sinistros Acidentes Trabalho
     H*
     H* Para Compilar este programa fazer:
     H*    CRTPF FILE(QTEMP/FINTT029) SRCFILE(#JMPRDSRC/QDDSSRC)
     H*    CRTPF FILE(QTEMP/INTFILE) SRCFILE(#JMPRDSRC/QDDSSRC) SRCMBR(FINTT029)
     H*
     H* Para correr este programa em DEBUG fazer:
     H*    CALL PGM(#JMPRDOBJ/PGRLO015)
     H*===========================================================
     FFINTP010  UF A E           K DISK    PREFIX(INT_)
     FFINTP01001IF   E           K DISK    PREFIX(INT_)
     F                                     RENAME(RINTP010:RINTP01001)
     FFICAT     UF A E           K DISK    PREFIX(AT_) USROPN
     F                                     EXTFILE(WExtFile)
     FFINTT029  IF   E           K DISK    PREFIX(TMP_) USROPN
     F                                     EXTFILE('QTEMP/INTFILE')
     F*===========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010

     D*==> converter um Campo alfa num n�mero
     D CATN            PR            15S 5
     D   ALFA                        15A   Const
     D   DFT                         15S 5 Const
     D
     D*==> Chamada de fun��o Dynamic, para trabalhar detalhe do Item
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
     D*==> Declara��o de Constantes
     D ACT#Validar     C                   'V'
     D ACT#Processar   C                   'P'
     D FACTOR          C                   4
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
           WMsgErro = 'INT0001'; // O ficheiro j� foi processado
           WResposta = 'N'; // N�o � para processar
           RETURN;
        ELSEIF (WAction = ACT#Validar);
           WResposta = 'S'; // � para processar
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
          OPEN FICAT;
          OPEN FINTT029;
          SETLL *LOVAL RINTT029;
          READ FINTT029;
          DOW (NOT %EOF(FINTT029));
             //-----------------------------------
             // Registar Cabe�alho do Registo
             //-----------------------------------
             IF (WIsFirst);
                WIsFirst = *OFF;
                EXSR RegisFile;
             ENDIF;

             //-----------------------------------------
             // Carregar Interface
             //-----------------------------------------
             WNumProc   = %TRIM(TMP_NUMPROC) + %TRIM(TMP_ANOPROC);
             IF (WNumProc <> *BLANKS);
                AT_TIPO    = TMP_TIPO;            // Tipo de Registo
                AT_NUMPRO  = CATN(WNumProc:0);    // N�mero do Processo
                AT_DATPRO  = CATN(TMP_DATPROC:0); // Data do Processo
                AT_DATREA  = CATN(TMP_DATREAB:0); // Data de Reabertura
                AT_DATSIN  = CATN(TMP_DATSINO:0); // Data do Sinistro
                AT_NUMAPO  = CATN(TMP_NUAPOL:0);  // N�mero da Ap�lice
                AT_CUSPRO  = CATN(TMP_IMPPROC:0); // Custo do Processo
                AT_DATENC  = CATN(TMP_DATENCE:0); // Data de Encerramento
                AT_NUMSEG  = CATN(TMP_NUMSEGU:0); // C�digo do Segurado
                AT_NOMSEG  = TMP_NOMSEGU;         // Nome do Segurado
                WRITE FICATF1;
             ENDIF;
             READ RINTT029;
          ENDDO;
          CLOSE FINTT029;
          CLOSE FICAT;
       ENDSR;

       //=================================================================
       // SubRotina..: ZapFile
       // Objectivo..: Limpar ficheiro antes de carregar com dados
       //=================================================================
       BEGSR ZapFile;
          MONITOR;
             WAAMMDD  = %SUBST(WNomFile:16:6);
             WExtFile = 'QS36F/FICA' + %TRIM(WAAMMDD);
             WComando = 'CRTPF FILE(' + %TRIM(WExtFile) + ') +
                               SRCFILE(SOURCE/QDDSSRC) +
                               SRCMBR(FICAT) SIZE(*NOMAX)';
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
             WDatFile    = %SUBST(WNomFile:14:4) + '-' +
                           %SUBST(WNomFile:18:2) + '-' +
                           %SUBST(WNomFile:20:2);
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
     P* Descri��o.: Converter campo alfa num Numero
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
