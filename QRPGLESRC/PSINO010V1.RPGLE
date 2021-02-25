     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*===========================================================
     H* Programa..: PSINO010
     H* Descrição.: Informação de sinistralidade da Apólice
     H*
     H* CALL PGM(PSINO010) PARM('02' '9901847' '00000000' '00000000' ' ')
     H*===========================================================
     FPROSINOR  IF   E           K DISK    PREFIX(OPRC_) USROPN
     FPRVSINOR  IF   F  128    10AIDISK    EXTFILE('O.SINPRV') USROPN
     FPAGSINOR  IF   F  300    10AIDISK    EXTFILE('O.SINPAG') USROPN
     F
     FPROSINAU  IF   E           K DISK    PREFIX(APRC_) USROPN
     FPRVSINAU  IF   F  128     8AIDISK    EXTFILE('G.SINPRV') USROPN
     FPAGSINAU  IF   F  300     8AIDISK    EXTFILE('G.SINPAG') USROPN
     F
     FGBASEGRL  IF   E           K DISK    PREFIX(BAS_)
     F*===========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SSECO010

     D*==> Formatos de Registo
     D R#PRVOR       E DS                  EXTNAME('O.SINPRV') PREFIX('OPRV_')
     D K#PRVOR         DS                  QUALIFIED
     D  KEY                    1     10A
     D  Ramo                   1      2S 0
     D  Ano                    3      4S 0
     D  Sequ                   5      8S 0
     D  Linha                  9     10S 0
     D
     D R#PAGOR       E DS                  EXTNAME('O.SINPAG') PREFIX('OPAG_')
     D K#PAGOR         DS                  QUALIFIED
     D  KEY                    1     10A
     D  Ramo                   1      2S 0
     D  Ano                    3      4S 0
     D  Sequ                   5      8S 0
     D  Linha                  9     10S 0
     D
     D R#PRVAU       E DS                  EXTNAME('G.SINPRV') PREFIX('APRV_')
     D K#PRVAU         DS                  QUALIFIED
     D  KEY                    1      8A
     D  Ano                    1      2S 0
     D  Sequ                   3      6S 0
     D  Linha                  7      8S 0
     D
     D R#PAGAU       E DS                  EXTNAME('G.SINPAG') PREFIX('APAG_')
     D K#PAGAU         DS                  QUALIFIED
     D  KEY                    1      8A
     D  Ano                    1      2S 0
     D  Sequ                   3      6S 0
     D  Linha                  7      8S 0
     D
     D*==> lista de Trabalo com o Resultado
     D LST#SINSITR     DS                  QUALIFIED DIM(500)
     D   Ramo                         2S 0
     D   Apolice                      7S 0
     D   IsEmpty                       N
     D
     D
     D*==>  Campos de Trabalho
     D WCodRamo        S              2S 0
     D WNuApolice      S              7S 0
     D WDatInicio      S              8S 0
     D WDatTermo       S              8S 0
     D WCodErro        S              7A
     D
     D WTotProvis      S             13S 0
     D WTotPagam       S             13S 0
     D WTotRecup       S             13S 0
     D*===========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCodRamo
     C                   PARM                    WNuApolice
     C                   PARM                    WDatInicio
     C                   PARM                    WDatTermo
     C                   PARM                    WCodErro
      /FREE
       //------------------------------------------------------------------------
       // Validar os Parâmetros Recebidos
       //------------------------------------------------------------------------
       EXSR ValParametros;
       IF (WCodErro <> *BLANKS);
          RETURN;
       ENDIF;

       //------------------------------------------------------------------------
       // Obter Processos para a Apólice
       //------------------------------------------------------------------------
       IF (WCodRamo = 50);
          EXSR GetProcAUT;
       ELSE;
          EXSR GetProcOR;
       ENDIF;

       //------------------------------------------------------------------------
       // Obter os Prémios recebidos da Apólice
       //------------------------------------------------------------------------
       RETURN;

       //=================================================================================
       // Subrotina..: ValParametros
       // Descrição..: Validar os Parâmetros Recebidos
       //=================================================================================
       BEGSR ValParametros;
          WCodErro = *BLANKS;

          //------------------------------------------------------------------------
          // Validar a Existência da Apólice
          //------------------------------------------------------------------------
          CHAIN (WCodRamo:WNuApolice) GBASEGRL;
          IF (NOT %FOUND(GBASEGRL));
             WCodErro = 'PRDXXXX';
             LEAVESR;
          ENDIF;

          //------------------------------------------------------------------------
          // Validar o Período
          //------------------------------------------------------------------------
          IF (WDatInicio > WDatTermo);
             WCodErro = 'PRDXXXX';
             LEAVESR;
          ENDIF;
       ENDSR;

       //=================================================================================
       // Subrotina..: GetProcAUT
       //  Descrição.: Obter a Informação sobre os Processo Automóvel
       //=================================================================================
       BEGSR GetProcAUT;
          OPEN PROSINAU;
          OPEN PAGSINAU;
          OPEN PRVSINAU;
          CLOSE PROSINAU;
          CLOSE PRVSINAU;
          CLOSE PROSINAU;
       ENDSR;

       //=================================================================================
       // Subrotina..: GetProcOR
       //  Descrição.: Obter a Informação sobre os Processo Outros Ramos
       //=================================================================================
       BEGSR GetProcOR;
          OPEN PROSINOR;
          OPEN PAGSINOR;
          OPEN PRVSINOR;

          SETLL (WCodRamo:WNuApolice) PROSINOR;
          READE (WCodRamo:WNuApolice) PROSINOR;
          DOW (NOT %EOF(PROSINOR));
             IF (OPRC_IPROC = 'P');
                EXSR GetProvOR;
                EXSR GetPagaOR;
                WTotRecup = WTotPagam - WTotProvis;
             ENDIF;
             READE (WCodRamo:WNuApolice) PROSINOR;
          ENDDO;
          CLOSE PROSINOR;
          CLOSE PRVSINOR;
          CLOSE PAGSINOR;
       ENDSR;

       //=================================================================================
       // Subrotina..: GetProvOR
       //  Descrição.: Obter as provisões para o Processo
       //=================================================================================
       BEGSR GetProvOR;
          WTotProvis = *ZEROS;
          K#PRVOR.Ramo  = WCodRamo;
          K#PRVOR.Ano   = OPRC_AAPROC;
          K#PRVOR.Sequ  = OPRC_NSEQ;
          K#PRVOR.Linha = *ZEROS;
          SETLL K#PRVOR.Key PRVSINOR;
          READ PRVSINOR R#PRVOR;
          DOW (NOT %EOF(PRVSINOR));
             IF (OPRV_RM = WCodRamo) AND (OPRV_NSEQ = OPRC_NSEQ)
               AND (OPRV_AAPROC = OPRC_AAPROC);
                WTotProvis += OPRV_VALOR;
             ELSE;
                LEAVE;
             ENDIF;
             READ PRVSINOR R#PRVOR;
          ENDDO;
       ENDSR;

       //=================================================================================
       // Subrotina..: GetPagaOR
       //  Descrição.: Obter os pagamebntos para o Processo
       //=================================================================================
       BEGSR GetPagaOR;
          WTotPagam = *ZEROS;
          K#PAGOR.Ramo  = WCodRamo;
          K#PAGOR.Ano   = OPRC_AAPROC;
          K#PAGOR.Sequ  = OPRC_NSEQ;
          K#PAGOR.Linha = *ZEROS;
          SETLL K#PAGOR.Key PAGSINOR;
          READ PAGSINOR R#PAGOR;
          DOW (NOT %EOF(PAGSINOR));
             IF (OPAG_RM = WCodRamo) AND (OPAG_NSEQ = OPRC_NSEQ)
               AND (OPAG_AAPROC = OPRC_AAPROC);
                WTotPagam += OPAG_VALOR;
             ELSE;
                LEAVE;
             ENDIF;
             READ PAGSINOR R#PAGOR;
          ENDDO;
       ENDSR;
      /END-FREE
