     H ALWNULL(*USRCTL)  DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL) FIXNBR(*ZONED)
     H*==================================================================
     H* Programa..: RPRDT001
     H* Descrição.: Gerir Operações IO ao Inventário de Apólices.
     H*==================================================================
     F*FSOURCES  IF   F  140        DISK    USROPN  EXTFILE(WNomFile)
     F*==================================================================
      /Copy QRPGLESRC,SGRLO012

     D*-------------------------------------------------
     D* Formato de Registo
     D*-------------------------------------------------
     D OldRecord       DS                  Based(OldRecPtr)
     D   O_SRCSEQ              1      6S 2
     D   O_SRCDAT              7     12S 0
     D   O_SRCDTA             13    140
     D
     D NewRecord       DS                  Based(NewRecPtr)
     D   N_SRCSEQ              1      6S 2
     D   N_SRCDAT              7     12S 0
     D   N_SRCDTA             13    140
     D
     D WNomFile        S             10A
     D*===========================================================
     C     *ENTRY        PLIST
     C                   PARM                    TrgBuffer
     C                   PARM                    TrgBufferLen
      /FREE

       //-----------------------------------------------------
       // Utilizador Especial (Saltar por cima do Activador)
       //-----------------------------------------------------
       //IF (S#USR = 'CALIMERO');
       //   RETURN;
       //ENDIF;

       //-- DSPLY $getAppCaller();

       OldRecPtr  = %ADDR(TrgBuffer) + TOldRecOff;
       NewRecPtr  = %ADDR(TrgBuffer) + TNewRecOff;
       OldNullPtr = %ADDR(TrgBuffer) + TOldNullOff;
       NewNullPtr = %ADDR(TrgBuffer) + TNewNullOff;

       //-----------------------------------------------------
       // ForCompr registo com valores por defeito
       //-----------------------------------------------------
       IF (TTrgEvent = IO#INS);
       ELSEIF (TTrgEvent = IO#UPD);
       ELSEIF (TTrgEvent = IO#DEL);
          *INH1 = *ON;
       ENDIF;

       RETURN;
      /END-FREE
