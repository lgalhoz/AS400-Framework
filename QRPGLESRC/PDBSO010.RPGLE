     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PRDO102
     H* Descrição.: Este programa possibilita a gestão de textos ou Notas
     H*             Descritivas a diferentes níveis (n).
     H*
     H*==================================================================
     FFPRDP008  UF A E           K DISK    PREFIX(PRD_) COMMIT
     F*==================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO012

     D*==================================================================
      /FREE
       IN *LOCK LDA#;
       IF (L#JNRENT = *ZEROS);
          L#JNRENT = 1;
          OUT LDA#;
       ELSE;
          L#JNRENT += 1;
          OUT LDA#;
       ENDIF;
       UNLOCK LDA#;
       RETURN;

       IF ($getCMTStatus);
       ENDIF;
       PRD_CDRAMO   = 5;
       PRD_NUAPOL   = 9910401;
       PRD_NUSEQU   = 100;
       PRD_CDCOBERT = 999;
       WRITE RPRDP008;

       IF ($getCMTStatus);
       ENDIF;
       ROLBK;

       IF ($getCMTStatus);
       ENDIF;

       RETURN;
      /END-FREE
