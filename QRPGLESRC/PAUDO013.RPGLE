     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*=====================================================================
     H* Programa...: PAUDO012
     H* Objectivo..: Criar Excepções na criação dos registos.
     H* Observação.:
     H*   CRTPF FILE(QTEMP/FAUDP022) SRCFILE(#JMPRDSRC/QDDSSRC) SIZE(*NOMAX)
     H*=====================================================================
     FFAUDP022  UF A E           K DISK    PREFIX(OUT_)
     F*=====================================================================
     D*==> Campos de Trabalho
     D WMsgErro        S              7A
     D*=====================================================================
      /FREE
        READ RAUDP022;
        DOW (NOT %EOF());
           OUT_MSGERRO = '0000000';
           IF (OUT_NUMREC = *ZEROS);
              %SUBST(OUT_MSGERRO:1:1) = '1'; // Não existe correspondência
           ENDIF;
           IF (OUT_QTDEREG = 1) AND (OUT_INTTREC = 0);
              %SUBST(OUT_MSGERRO:2:1) = '1'; // Apenas enviou uma única vez
           ENDIF;
           IF (OUT_QTDEREG > 2);
              %SUBST(OUT_MSGERRO:3:1) = '1'; // Enviou mais do que uma vez
           ENDIF;
           IF (OUT_INTTREC <> OUT_GCTREC);
              %SUBST(OUT_MSGERRO:4:1) = '1'; // O Prémio do interface difere do recibo
           ENDIF;
           IF (OUT_INTTCOM <> OUT_GCCMED);
              %SUBST(OUT_MSGERRO:5:1) = '1'; // A comissão do interface difere do recibo
           ENDIF;
           IF (OUT_GCTREC <> OUT_TOTREC);
              %SUBST(OUT_MSGERRO:6:1) = '1'; // O Prémio da Lusitânia difere do Recibo
           ENDIF;
           IF (OUT_GCCMED <> OUT_TOTCOM);
              %SUBST(OUT_MSGERRO:7:1) = '1'; // Acomissão da Lusitânia difere do Recibo
           ENDIF;
           UPDATE RAUDP022 %FIELDS(OUT_MSGERRO);
           READ RAUDP022;
        ENDDO;

        *INLR = *ON;
      /END-FREE
