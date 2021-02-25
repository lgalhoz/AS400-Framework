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
     FGCOBGRH   IF   E           K DISK    PREFIX(COB_)
     F*=====================================================================
     D*==> Campos de Trabalho
     D WDatValor       S              8S 0
     D WIcExiste       S               N
     D WMsgErro        S              7A
     D*=====================================================================
      /FREE
        READ RAUDP022;
        DOW (NOT %EOF());
           IF (OUT_NUMREC > *ZEROS);
              CHAIN (OUT_NUMREC) GCOBGRH;
              IF (%FOUND(GCOBGRH));
                 IF (OUT_INTRAMO = COB_GCRAMO) AND
                   (OUT_INTAPOL = COB_GCAPOL);
                    OUT_CDRAMO  = COB_GCRAMO;
                    OUT_NUSEGU  = COB_GCNSEG;
                    OUT_GCTIPR  = COB_GCTIPR;
                    OUT_GCSITR  = COB_GCSITR;
                    OUT_GCTREC  = COB_GCTREC;
                    OUT_GCCMED  = COB_GCCMED;
                    OUT_MSGERRO = *BLANKS;
                    UPDATE RAUDP022 %FIELDS(OUT_CDRAMO:OUT_NUSEGU:
                                            OUT_GCTIPR:OUT_GCSITR:
                                         OUT_GCTREC:OUT_GCCMED:OUT_MSGERRO);
                 ENDIF;
              ENDIF;
           ENDIF;
           READ RAUDP022;
        ENDDO;

        *INLR = *ON;
      /END-FREE
