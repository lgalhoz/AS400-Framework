     H ALWNULL(*USRCTL) dftactgrp(*NO)
     H*=============================================================
     FFSINP105  IF   E           K DISK    PREFIX(D_)
     F*=============================================================
      /Copy QRPGLESRC,SGRLO012

     D*-------------------------------------------------
     D* Formato de Registo
     D*-------------------------------------------------
     D OldRecord     E DS                  ExtName('G.SININT')
     D                                     Prefix(O_)
     D                                     Based(OldRecPtr)
     D NewRecord     E DS                  ExtName('G.SININT')
     D                                     Prefix(N_)
     D                                     Based(NewRecPtr)
     D
     D*==> Actualizar Intervenientes
     D ActInterven     PR                  EXTPGM('RSINO001')
     D   WNuProcesso                  7A   CONST
     D   WModo                        1S 0 CONST
     D   WCodInterv                   9S 0 CONST
     D   WTipInterv                   1A   CONST
     D   WTipDanos                    1A   CONST
     D   WNome                       40A   CONST
     D   WMatricula                  20A   CONST
     D   WDatEncerra                   D   CONST
     D
     D WNome           S             40A
     D WNuOrdem        S              5S 0 INZ(*ZEROS)
     D WDummyDate      S               D
     D*===========================================================
     C     *ENTRY        PLIST
     C                   PARM                    TrgBuffer
     C                   PARM                    TrgBufferLen
      /FREE
       //-----------------------------------------------------
       // Salvaguardar Campos
       //-----------------------------------------------------
       MONITOR;
          OldRecPtr  = %ADDR(TrgBuffer) + TOldRecOff;
          NewRecPtr  = %ADDR(TrgBuffer) + TNewRecOff;
          OldNullPtr = %ADDR(TrgBuffer) + TOldNullOff;
          NewNullPtr = %ADDR(TrgBuffer) + TNewNullOff;
       ON-ERROR;
          RETURN;
       ENDMON;

       //-------------------------------------------------------
       // Aplicar modifica��es ao registo em fun��o da Opera��o
       //-------------------------------------------------------
       IF (TTrgEvent=IO#INS) OR (TTrgEvent = IO#UPD);
          D_NRPROC   = %TRIM(%EDITW(N_AAPROC:'0  '))+'9'+%CHAR(N_NSEQ);
          CHAIN (D_NRPROC) RSINP105;
          IF (%FOUND());
             MONITOR;
                IF (N_NOMSEG <> *BLANKS);
                   WNome = N_NOMSEG;
                ELSE;
                   WNome = N_NOMSIN;
                ENDIF;
                WDummyDate = %Date();
                ActInterven(D_NRPROC:3:N_NREG:N_CODINT:
                            N_TPDN:WNome:N_IMATR1:WDummyDate);
             ON-ERROR;
             ENDMON;
          ENDIF;
       ELSEIF (TTrgEvent = IO#DEL);  // Elimina��o
       ENDIF;

       RETURN;
       //=======================================================
      /END-FREE
