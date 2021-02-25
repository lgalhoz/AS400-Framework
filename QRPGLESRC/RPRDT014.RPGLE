     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL) FIXNBR(*ZONED)
     H*==================================================================
     H* Programa..: RPRDT014
     H* Descrição.: Gerir Operações IO ao ficheiro de Limites e
     H*             Franquias com Histórico inteligente.
     H*==================================================================
     FFPRDP014  UF A E           K DISK    COMMIT
     F*==================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SGRLO012
      /Copy QRPGLESRC,SDBSO010

     D*-------------------------------------------------
     D* Formato de Registo
     D*-------------------------------------------------
     D OldRecord     E DS                  ExtName(FPRDP014)
     D                                     Prefix(O_)
     D                                     Based(OldRecPtr)
     D NewRecord     E DS                  ExtName(FPRDP014)
     D                                     Prefix(N_)
     D                                     Based(NewRecPtr)
     D
     D WEndDate        S                   LIKE(PRD01402)
     D WNuSequ         S                   LIKE(NUSEQU)
     D*===========================================================
     C     *ENTRY        PLIST
     C                   PARM                    TrgBuffer
     C                   PARM                    TrgBufferLen
      /FREE

       //-----------------------------------------------------
       // Utilizador Especial (Saltar por cima do Activador)
       //-----------------------------------------------------
       IF (S#USR = 'CALIMERO');
          RETURN;
       ENDIF;

       OldRecPtr  = %ADDR(TrgBuffer) + TOldRecOff;
       NewRecPtr  = %ADDR(TrgBuffer) + TNewRecOff;
       OldNullPtr = %ADDR(TrgBuffer) + TOldNullOff;
       NewNullPtr = %ADDR(TrgBuffer) + TNewNullOff;

       //-----------------------------------------------------
       // Formatar registo com valores por defeito
       //-----------------------------------------------------
       IF (TTrgEvent = IO#INS);
          IF (N_NUSEQU = *ZEROS);
             N_NUSEQU  = $GetNextKey('FPRDP014':'NUSEQU':'(CDRAMO='''+
                         %TRIM(%CHAR(N_CDRAMO)) + ''') AND (NUAPOL='''+
                         %TRIM(%CHAR(N_NUAPOL)) + ''')');
          ENDIF;
          IF (N_PRD01401 = *LOVAL);
             N_PRD01401 = $Today();
          ENDIF;
          IF (N_PRD01402 = *LOVAL);
             N_PRD01402 = %DATE('9999-12-31':*ISO);
          ENDIF;
          IF (N_PRD01401 > N_PRD01402);
             N_PRD01401 = N_PRD01402;
          ENDIF;
          IF (N_PRD01402 <= $Today());
             N_PRD01403 = *ZEROS;
          ELSE;
             N_PRD01403 = 1;
          ENDIF;
          IF (N_PRD01404 = *BLANKS);
             N_PRD01404 = S#USR;
          ENDIF;
          N_PRD01405 = S#USR;
       ELSE;
          //-----------------------------------------------------
          // Apenas alterar Registos Activas
          //-----------------------------------------------------
          IF (O_PRD01401 < $Today()) AND (O_PRD01402 > $Today());
             IF (TTrgEvent = IO#UPD);
                //------------------------------------------------
                // Guardar a imagem do registo antes da alteração
                //------------------------------------------------
                WNuSequ = *ZEROS;
                WEndDate = $Today();
                EXSR CrtRecord;

                //------------------------------------------------
                // Alterar o Registo
                //------------------------------------------------
                N_CDRAMO   = O_CDRAMO;
                N_NUAPOL   = O_NUAPOL;
                N_NUSEQU   = O_NUSEQU;
                N_CDLIMITE = O_CDLIMITE;
                N_PRD01401 = $Today();
                N_PRD01402 = O_PRD01402;
                N_PRD01404 = O_PRD01404;
                N_PRD01405 = S#USR;
             ELSEIF (TTrgEvent = IO#DEL);
                //-----------------------------------------------------
                // Recriar o registo Eliminado forçando uma data Termo
                //-----------------------------------------------------
                WEndDate = $Today();
                WNuSequ  = NUSEQU;
                EXSR CrtRecord;
             ENDIF;
          ELSEIF (O_PRD01401 < $Today());
             //-----------------------------------------------------
             // Repor os dados Registos se Histórico ou Futuro
             //-----------------------------------------------------
             IF (TTrgEvent = IO#UPD);
                *INH1 = *ON;
                RETURN;
             ELSEIF (TTrgEvent = IO#DEL);
                WNuSequ  = NUSEQU;
                WEndDate = O_PRD01402;
                EXSR CrtRecord;
             ENDIF;
          ENDIF;
       ENDIF;

       RETURN;

       //=========================================================
       // SubRotina..: CrtRecord
       // Objectivo..: Criar o registo Eliminado
       //=========================================================
       BEGSR CrtRecord;
          CDRAMO   = O_CDRAMO;
          NUAPOL   = O_NUAPOL;
          NUSEQU   = WNuSequ;
          CDLIMITE = O_CDLIMITE;
          CDINCIND = O_CDINCIND;
          VLPERIND = O_VLPERIND;
          VLMAXIND = O_VLMAXIND;
          CDINCFRQ = O_CDINCFRQ;
          VLPERFRQ = O_VLPERFRQ;
          VLMINFRQ = O_VLMINFRQ;
          PRD01401 = O_PRD01401;
          PRD01402 = WEndDate;
          PRD01404 = O_PRD01404;
          WRITE RPRDP014;
       ENDSR;
      /END-FREE
