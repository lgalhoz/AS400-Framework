     FFPRDP008  UF A E           K DISK
      /FREE
       CHAIN (05:9900006) RPRDP008;
       IF (%FOUND());
          CDCOBERT = 10;
          UPDATE RPRDP008 %FIELDS(CDCOBERT);
       ENDIF;

       *INLR = *ON;
       RETURN;
      /END-FREE
