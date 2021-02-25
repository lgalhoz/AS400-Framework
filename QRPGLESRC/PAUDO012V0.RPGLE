     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*=====================================================================
     H* Programa...: PAUDO011
     H* Objectivo..: Criar equivalências com o Sistema da José Mata, Lda
     H* Observação.:
     H*   CRTPF FILE(QTEMP/FAUDP022) SRCFILE(#JMPRDSRC/QDDSSRC) SIZE(*NOMAX)
     H*=====================================================================
     FFAUDP022  UF A E           K DISK    PREFIX(OUT_)
     FGBASEGRL  IF   E           K DISK    PREFIX(BAS_)
     FFPRDT001  IF   E           K DISK    PREFIX(BAS_)
     FGCOBGRH4  IF   E           K DISK    PREFIX(COB_)
     F*=====================================================================
     D DS#RECIBO       DS
     D   WRec#Recibo           1      9S 0
     D   WRec#Last             5      9S 0
     D
     D*==> Campos de Trabalho
     D WCodigo         S              4A
     D WNumRecibo      S              9S 0
     D WNuApolice      S              7S 0
     D WCdRamo         S              2S 0
     D WNumSegura      S              6S 0
     D
     D WDatValor       S              8S 0
     D WDataRegisto    S              8S 0
     D WTipMovim       S              2S 0
     D WSitMovim       S              2S 0
     D WTotValor       S             11S 2
     D WTotValAlfa     S             11A
     D WLastPosis01    S              5S 0
     D WLastPosis02    S              5S 0
     D
     D WIcExiste       S               N
     D WMsgErro        S              7A
     D WIdx            S              3S 0
     D*=====================================================================
      /FREE
        READ RAUDP022;
        DOW (NOT %EOF());
           WNuApolice = OUT_NUAPOL;
           WCdRamo    = *ZEROS;
           DOW (WCdRamo < 99);
              WIcExiste = *OFF;
              WCdRamo  += 1;
              CHAIN (WCdRamo:WNuApolice) RPRDP001;
              IF (%FOUND(FPRDT001));
                 SETLL (WCdRamo:WNuApolice) GCOBGRH4;
                 READE (WCdRamo:WNuApolice) GCOBGRH4;
                 DOW (NOT %EOF(GCOBGRH4));
                    WRec#Recibo   = COB_GCNREC;
                    WLastPosis01  = WRec#Last;
                    WRec#Recibo   = OUT_NUMREC;
                    WLastPosis02  = WRec#Last;
                    IF (WLastPosis01 = WLastPosis02);
                    IF (COB_GCTREC = OUT_TOTREC)
                       AND (COB_GCCMED = OUT_TOTCOM);
                       WIcExiste  = *ON;
                       WNumRecibo = COB_GCNREC;
                       WNumSegura = COB_GCNSEG;
                       EXSR Processar;
                       LEAVE;
                    ENDIF;
                    ENDIF;
                    READE (WCdRamo:WNuApolice) GCOBGRH4;
                 ENDDO;
              ENDIF;
              IF (WIcExiste);
                  LEAVE;
              ENDIF;
           ENDDO;
           READ RAUDP022;
        ENDDO;

        *INLR = *ON;

        //===================================================================
        // Subrotina.: Processar
        // Objectivo.: Processar os registos
        //===================================================================
        BEGSR Processar;
           WDatValor  = 20000000 + COB_GCDPAG;
           OUT_NUMREC  = WNumRecibo;
           OUT_CDRAMO  = WCdRamo;
           OUT_NUSEGU  = WNumSegura;
           OUT_DTMOVI  = WDatValor;
           OUT_GCTIPR  = COB_GCTIPR;
           OUT_GCSITR  = COB_GCSITR;
           OUT_GCTREC  = COB_GCTREC;
           OUT_GCCMED  = COB_GCCMED;
           OUT_MSGERRO = *BLANKS;
           UPDATE RAUDP022 %FIELDS(OUT_NUMREC:OUT_CDRAMO:OUT_NUSEGU:
                                      OUT_DTMOVI:OUT_GCTIPR:OUT_GCSITR:
                                      OUT_GCTREC:OUT_GCCMED:OUT_MSGERRO);
        ENDSR;
      /END-FREE
