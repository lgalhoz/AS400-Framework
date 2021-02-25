     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL) FIXNBR(*ZONED)
     H*==================================================================
     H* Programa..: RPRDT011
     H* Descrição.: Gerir Operações IO ao ficheiro de Riscos,
     H*             com Histórico inteligente.
     H*==================================================================
     FFPRDP011  UF A E           K DISK    COMMIT
     F*==================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SGRLO012
      /Copy QRPGLESRC,SDBSO010

     D*-------------------------------------------------
     D* Formato de Registo
     D*-------------------------------------------------
     D OldRecord     E DS                  ExtName(FPRDP011)
     D                                     Prefix(O_)
     D                                     Based(OldRecPtr)
     D NewRecord     E DS                  ExtName(FPRDP011)
     D                                     Prefix(N_)
     D                                     Based(NewRecPtr)
     D
     D WEndDate        S                   LIKE(PRD01102)
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
             N_NUSEQU  = $GetNextKey('FPRDP011':'NUSEQU':'(CDRAMO='''+
                         %TRIM(%CHAR(N_CDRAMO)) + ''') AND (NUAPOL='''+
                         %TRIM(%CHAR(N_NUAPOL)) + ''')');
          ENDIF;
          IF (N_PRD01101 = *LOVAL);
             N_PRD01101 = $Today();
          ENDIF;
          IF (N_PRD01102 = *LOVAL);
             N_PRD01102 = %DATE('9999-12-31':*ISO);
          ENDIF;
          IF (N_PRD01101 > N_PRD01102);
             N_PRD01101 = N_PRD01102;
          ENDIF;
          IF (N_PRD01102 <= $Today());
             N_PRD01103 = *ZEROS;
          ELSE;
             N_PRD01103 = 1;
          ENDIF;
          IF (N_PRD01104 = *BLANKS);
             N_PRD01104 = S#USR;
          ENDIF;
          N_PRD01105 = S#USR;
       ELSE;
          //-----------------------------------------------------
          // Apenas alterar Registos Activas
          //-----------------------------------------------------
          IF (O_PRD01101 < $Today()) AND (O_PRD01102 > $Today());
             IF (TTrgEvent = IO#UPD);
                //------------------------------------------------
                // Guardar a imagem do registo antes da alteração
                //------------------------------------------------
                WNuSequ  = *ZEROS;
                WEndDate = $Today();
                EXSR CrtRecord;

                //------------------------------------------------
                // Alterar o Registo
                //------------------------------------------------
                N_CDRAMO   = O_CDRAMO;
                N_NUAPOL   = O_NUAPOL;
                N_NUSEQU   = O_NUSEQU;
                N_CDRISCO  = O_CDRISCO;
                N_PRD01101 = $Today();
                N_PRD01102 = O_PRD01102;
                N_PRD01104 = O_PRD01104;
                N_PRD01105 = S#USR;
             ELSEIF (TTrgEvent = IO#DEL);
                //-----------------------------------------------------
                // Recriar o registo Eliminado forçando uma data Termo
                //-----------------------------------------------------
                WNuSequ  = O_NUSEQU;
                WEndDate = $Today();
                EXSR CrtRecord;
             ENDIF;
          ELSEIF (O_PRD01101 < $Today());
             //-----------------------------------------------------
             // Repor os dados Registos se Histórico ou Futuro
             //-----------------------------------------------------
             IF (TTrgEvent = IO#UPD);
                *INH1 = *ON;
                RETURN;
             ELSEIF (TTrgEvent = IO#DEL);
                WNuSequ  = O_NUSEQU;
                WEndDate = $Today();
                EXSR CrtRecord;
             ENDIF;
          ENDIF;
       ENDIF;

       RETURN;

       //=========================================================
       // SubRotina..: crtRecord
       // Objectivo..: Criar o registo Eliminado
       //=========================================================
       BEGSR crtRecord;
          CDRAMO   = O_CDRAMO;
          NUAPOL   = O_NUAPOL;
          NUSEQU   = WNuSequ;
          CDRISCO  = O_CDRISCO;
          IMCPRISC = O_IMCPRISC;
          TXRISCO  = O_TXRISCO;
          IMPRRISC = O_IMPRRISC;
          PRD01101 = O_PRD01101;
          PRD01102 = WEndDate;
          PRD01104 = O_PRD01104;
          WRITE RPRDP011;
       ENDSR;
      /END-FREE
