     FFPRDP010  UF A E           K DISK
      /Copy QRPGLESRC,SGRLO012
     D
     D*-------------------------------------------------
     D* Formato de Registo
     D*-------------------------------------------------
     D OldRecord     E DS                  ExtName(FPRDP010)
     D                                     Prefix(O_)
     D                                     Based(OldRecPtr)
     D NewRecord     E DS                  ExtName(FPRDP010)
     D                                     Prefix(N_)
     D                                     Based(NewRecPtr)
     D
     D WChave01        S              5S 0
     D*===========================================================
     C     *ENTRY        PLIST
     C                   PARM                    TrgBuffer
     C                   PARM                    TrgBufferLen
      /FREE

       OldRecPtr  = %ADDR(TrgBuffer) + TOldRecOff;
       NewRecPtr  = %ADDR(TrgBuffer) + TNewRecOff;
       OldNullPtr = %ADDR(TrgBuffer) + TOldNullOff;
       NewNullPtr = %ADDR(TrgBuffer) + TNewNullOff;

       IF (O_PRD01001 < %DATE());
          IF (TTrgEvent = IO#UPD);
             CDPROD   = N_CDPROD;
             NUAPOL   = N_NUAPOL;
             NUSEQU   = WChave01;
             NUSQSUP  = N_NUSQSUP;
             CDTEXTO  = N_CDTEXTO;
             CPSEGURO = N_CPSEGURO;
             PRD01001 = %DATE();
             WRITE RPRDP010;

             N_CDPROD   = O_CDPROD;
             N_NUAPOL   = O_NUAPOL;
             N_NUSEQU   = O_NUSEQU;
             N_NUSQSUP  = O_NUSQSUP;
             N_CDTEXTO  = O_CDTEXTO;
             N_CPSEGURO = O_CPSEGURO;
             N_PRD01001 = O_PRD01001;
             N_PRD01002 = %DATE();
          ELSEIF (TTrgEvent = IO#DEL);
             CDPROD   = O_CDPROD;
             NUAPOL   = O_NUAPOL;
             NUSEQU   = O_NUSEQU;
             NUSQSUP  = O_NUSQSUP;
             CDTEXTO  = O_CDTEXTO;
             CPSEGURO = O_CPSEGURO;
             PRD01001 = O_PRD01001;
             PRD01002 = %DATE();
             WRITE RPRDP010;
          ENDIF;
       ENDIF;

       *INLR = *ON;
       //==============================================================
       // Subrotina..: GetChave
       // Objectivo..: Obter a sequenica seguinte para a Chave
       //==============================================================
       BEGSR GetChave;
          SETGT (O_CDPROD:O_NUAPOL) RPRDP010;
          READP(N) RPRDP010;
          IF (%FOUND());
             WChave01 = NUSEQU + 1;
          ELSE;
             WChave01 = 1;
          ENDIF;
       ENDSR;
      /END-FREE
