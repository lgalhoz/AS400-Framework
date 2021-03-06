     H DFTACTGRP(*NO)
     H*=================================================================
     FFTMPP001  IF   E             DISK    RENAME(FTMPP001:RTMPP001)
     F                                     PREFIX(IN_)
     FFTBLP013  UF A E           K DISK    PREFIX(OUT_)
     FFTBLP01301IF   E           K DISK    RENAME(RTBLP013:RTBLP01301)
     FFTBLP01302IF   E           K DISK    RENAME(RTBLP013:RTBLP01302)
     F*================================================================
     D GetSuperKey     PR             9S 0
     D   PKeyDistr                    2A   CONST
     D   PKeyConce                    2A   CONST OPTIONS(*NOPASS)
     D   PDesFregu                  120A   CONST OPTIONS(*NOPASS)
     D   PKeyLocal                    8A   CONST OPTIONS(*NOPASS)
     D
     D WKeyDistr       S              2A
     D WKeyConce       S              2A
     D WKeyFregu       S              6A
     D WKeyLocal       S              8A
     D WDesLocal       S            120A
     D WDesRua         S            120A
     D WDesFregu       S            120A
     D WKeyRua         S              8A
     D
     D WChaveSup       S              9S 0
     D WChaveAct       S              9S 0
     D
     D WPosIni         S              5S 0
     D WPosFim         S              5S 0
     D WStrLen         S              5S 0
     D WActFld         S              5S 0
     F*================================================================
      /FREE
        WChaveAct = 41696;
        READ FTMPP001;
        DOW NOT %EOF();
           WActFld      = 1;
           WKeyDistr = %SUBST(IN_FTMPP001:1:2);
           WKeyConce = %SUBST(IN_FTMPP001:4:2);
           WDesLocal = *BLANKS;
           WDesRua   = *BLANKS;
           WPosIni   = %SCAN(';':IN_FTMPP001);
           DOW (WPosIni > 0);
              WActFld += 1;
              IF (WActFld = 4);
                 WKeyLocal = %SUBST(IN_FTMPP001:1:(WPosIni-1));
              ELSEIF (WActFld = 5);
                 WDesLocal = %SUBST(IN_FTMPP001:1:(WPosIni-1));
              ELSEIF (WActFld > 6) AND (WActFld < 16);
                 WDesRua   = %TRIM(WDesRua) + ' ' +
                             %SUBST(IN_FTMPP001:1:(WPosIni-1));
              ELSEIF (WActFld = 16);
                 WKeyRua   = %SUBST(IN_FTMPP001:6:3);
              ELSEIF (WActFld = 17);
                 IF (WDesRua = *BLANKS);
                    WDesRua = WDesLocal;
                 ENDIF;
                 WDesFregu = %SUBST(IN_FTMPP001:5);
                 WChaveSup = GetSuperKey(WKeyDistr:WKeyConce:WDesFregu);
                 IF (WChaveSup > *ZEROS);
                    WChaveSup = GetSuperKey(WKeyDistr:WKeyConce:WDesFregu:
                                            WKeyLocal);
                    IF (WChaveSup > *ZEROS);
                      WChaveAct += 1;
                      OUT_CDLOCAL = WChaveAct;
                      OUT_CDSUPER=WChaveSup;
                      OUT_DSLOCAL = WDesRua;
                      OUT_ALIAS   = WKeyRua;
                      OUT_NUNIVEL = 6;
                      OUT_CDTIPO  = 'R';
                      WRITE RTBLP013;
                    ENDIF;
                 ENDIF;
              ENDIF;
              IN_FTMPP001 = %SUBST(IN_FTMPP001:WPosIni+1);
              WPosIni  = %SCAN(';':IN_FTMPP001);
           ENDDO;
           READ FTMPP001;
        ENDDO;

        *INLR = *ON;
      /END-FREE
     P*===================================================================
     P* Processo..: GetSuperKey
     P* Descri��o.: Obter a chave do n�vel Superior
     P*===================================================================
     P GetSuperKey     B                   EXPORT
     D GetSUperKey     PI             9S 0
     D   PKeyDistr                    2A   CONST
     D   PKeyConce                    2A   CONST OPTIONS(*NOPASS)
     D   PDesFregu                  120A   CONST OPTIONS(*NOPASS)
     D   PKeyLocal                    8A   CONST OPTIONS(*NOPASS)
     D
     D WCdSuper        S              9S 0
     D*=================================================================
      /FREE
        IF (%PARMS() > 3);
           WCdSuper = 1;
           CHAIN (PKeyDistr:WCdSuper) RTBLP01301;
           IF (%FOUND());
              WCdSuper = CDLOCAL;
              CHAIN (PKeyConce:WCdSuper) RTBLP01301;
              IF (%FOUND());
                 WCdSuper = CDLOCAL;
                 CHAIN (PDesFregu:WCdSuper) RTBLP01302;
                 IF (%FOUND());
                    WCdSuper = CDLOCAL;
                    CHAIN (PKeyLocal:WCdSuper) RTBLP01301;
                    IF (%FOUND());
                       RETURN CDLOCAL;
                    ENDIF;
                 ENDIF;
              ENDIF;
           ENDIF;
        ELSEIF (%PARMS() > 2);
           WCdSuper = 1;
           CHAIN (PKeyDistr:WCdSuper) RTBLP01301;
           IF (%FOUND());
              WCdSuper = CDLOCAL;
              CHAIN (PKeyConce:WCdSuper) RTBLP01301;
              IF (%FOUND());
                 WCdSuper = CDLOCAL;
                 CHAIN (PDesFregu:WCdSuper) RTBLP01302;
                 IF (%FOUND());
                    RETURN CDLOCAL;
                 ENDIF;
              ENDIF;
           ENDIF;
        ELSEIF (%PARMS() > 1);
           WCdSuper = 1;
           CHAIN (PKeyDistr:WCdSuper) RTBLP01301;
           IF (%FOUND());
              WCdSuper = CDLOCAL;
              CHAIN (PKeyConce:WCdSuper) RTBLP01301;
              IF (%FOUND());
                 RETURN CDLOCAL;
              ENDIF;
           ENDIF;
        ELSE;
        ENDIF;
        RETURN *ZEROS;
      /END-FREE
     PGetSuperKey      E
