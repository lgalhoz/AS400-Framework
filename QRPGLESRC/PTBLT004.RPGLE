     FFTBLP014  IF   E             DISK    PREFIX(IN_)
     FFTBLP013  UF A E             DISK    PREFIX(OUT_)
     F*================================================================
     D WActKey         S              9S 0
     D*================================================================
      /FREE
        WActKey = 1;
        READ FTBLP014;
        DOW NOT %EOF();
           WActKey += 1;
           OUT_CDLOCAL = WActKey;
           OUT_CDSUPER = 1;
           OUT_DSLOCAL = IN_DSDISTR;
           OUT_ALIAS   = %SUBST(%EDITW(IN_CDDISTR:'0    '):4:2);
           OUT_NUNIVEL = 2;
           OUT_CDTIPO  = 'D';  // Distrito
           WRITE RTBLP013;
           READ FTBLP014;
        ENDDO;
        *INLR = *ON;
      /END-FREE
