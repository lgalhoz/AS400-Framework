     H BNDDIR('JMDIR')
     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*=====================================================================
     FFSECP020  IF   E           K DISK    PREFIX(SEC_)
     D*=====================================================================
      /Copy QRPGLESRC,SGRLO010

     D WNomAppl        S             10A
     D WNomFunction    S             10A
     D WNumOption      S              2S 0
     D WOrigem         S              1A
     D WValor          S             50A
     D WResultado      S               N
     C*=====================================================================
     C     *ENTRY        PLIST
     C                   PARM                    WNomAppl
     C                   PARM                    WNomFunction
     C                   PARM                    WNumOption
     C                   PARM                    WOrigem
     C                   PARM                    WValor
     C                   PARM                    WResultado
      /FREE

        IF (WOrigem = 'O') AND (WNumOption = 2);
           WResultado = *ON;
        ELSE;
           CHAIN (S#USR) RSECP020;
           IF (%FOUND());
              IF (%TRIM(WValor) = '50') OR (%TRIM(WVAlor) = '51');
                 IF ((SEC_CDDEPART='AUT') OR (SEC_CDDEPART='INF')
                      OR (SEC_CDDEPART='SAU') OR (SEC_CDDEPART='MST'));
                    WResultado = *ON;
                 ELSE;
                    WResultado = *OFF;
                 ENDIF;
              ELSEIF (%TRIM(WValor) <> '15');
                 IF ((SEC_CDDEPART='PRD') OR (SEC_CDDEPART='INF')
                      OR (SEC_CDDEPART='MST'));
                    WResultado = *ON;
                 ELSE;
                    WResultado = *OFF;
                 ENDIF;
              ELSEIF (%TRIM(WValor) = '15');
                 IF ((SEC_CDDEPART='ACT') OR (SEC_CDDEPART='INF')
                      OR (SEC_CDDEPART='MST'));
                    WResultado = *ON;
                 ELSE;
                    WResultado = *OFF;
                 ENDIF;
              ENDIF;
           ELSE;
              WResultado = *OFF;
           ENDIF;
        ENDIF;

        RETURN;
      /END-FREE
