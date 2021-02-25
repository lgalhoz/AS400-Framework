     FFTBLP015  IF   E             DISK    PREFIX(IN_)
     FFTBLP013  UF A E             DISK    PREFIX(OUT_)
     FFTBLP01301IF   E           K DISK    RENAME(RTBLP013:RTBLP01301)
     F*================================================================
     D WActKey         S              9S 0
     D WAlias          S              4A
     D*================================================================
      /FREE
        WActKey = 30;
        READ FTBLP015;
        DOW NOT %EOF();
           WActKey += 1;
           WAlias = %SUBST(%EDITW(IN_CDDISTR:'0    '):4:2);
           CHAIN (WAlias:1) RTBLP01301;
           IF (%FOUND());
              OUT_CDLOCAL = WActKey;
              OUT_CDSUPER = CDLOCAL;
              OUT_DSLOCAL = IN_DSCONCE;
              OUT_ALIAS   = %SUBST(%EDITW(IN_CDCONCE:'0    '):4:2);
              OUT_NUNIVEL = 3;
              OUT_CDTIPO  = 'C';  // Concelho
              WRITE RTBLP013;
           ENDIF;
           READ FTBLP015;
        ENDDO;
        *INLR = *ON;
      /END-FREE
