     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL) FIXNBR(*ZONED)
     H*==================================================================
     H* Programa..: RPRDT018
     H* Descrição.: Gerir Operações IO ao ficheiro Parâmetros do Edificio
     H*             com gestão do Histórico inteligente.
     H*==================================================================
     FFPRDP018  UF A E           K DISK    COMMIT
     F*==================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SGRLO012
      /Copy QRPGLESRC,SDBSO010

     D*-------------------------------------------------
     D* Formato de Registo
     D*-------------------------------------------------
     D OldRecord     E DS                  ExtName(FPRDP018)
     D                                     Prefix(O_)
     D                                     Based(OldRecPtr)
     D NewRecord     E DS                  ExtName(FPRDP018)
     D                                     Prefix(N_)
     D                                     Based(NewRecPtr)
     D
     D WNuSequ         S                   LIKE(NUSEQU)
     D WEndDate        S                   LIKE(PRD01802)
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
       // ForCompr registo com valores por defeito
       //-----------------------------------------------------
       IF (TTrgEvent = IO#INS);
          IF (N_NUSEQU = *ZEROS);
             N_NUSEQU  = $GetNextKey('FPRDP018':'NUSEQU':'(CDRAMO='''+
                         %TRIM(%CHAR(N_CDRAMO)) + ''') AND (NUAPOL='''+
                         %TRIM(%CHAR(N_NUAPOL)) + ''')');
          ENDIF;
          IF (N_PRD01801 = *LOVAL);
             N_PRD01801 = $Today();
          ENDIF;

          IF (N_PRD01802 = *LOVAL);
             N_PRD01802 = %DATE('9999-12-31':*ISO);
          ENDIF;
          IF (N_PRD01801 > N_PRD01802);
             N_PRD01801 = N_PRD01802;
          ENDIF;
          IF (N_PRD01802 <= $Today());
             N_PRD01803 = *ZEROS;
          ELSE;
             N_PRD01803 = 1;
          ENDIF;
          IF (N_PRD01804 = *BLANKS);
             N_PRD01804 = S#USR;
          ENDIF;
          N_PRD01805 = S#USR;
       ELSE;
          //-----------------------------------------------------
          // Apenas alterar Registos Activas
          //-----------------------------------------------------
          IF (O_PRD01801 < $Today()) AND (O_PRD01802 > $Today());
             IF (TTrgEvent = IO#UPD);
                //------------------------------------------------
                // Guardar a imagem do registo antes da alteração
                //------------------------------------------------
                WNUSequ  = *ZEROS;
                WEndDate = $Today();
                EXSR CrtRecord;

                //------------------------------------------------
                // Alterar o Registo
                //------------------------------------------------
                N_CDRAMO   = O_CDRAMO;
                N_NUAPOL   = O_NUAPOL;
                N_NUSEQU   = O_NUSEQU;
                N_PRD01801 = $Today();
                N_PRD01802 = O_PRD01802;
                N_PRD01804 = O_PRD01804;
                N_PRD01805 = S#USR;
             ELSEIF (TTrgEvent = IO#DEL);
                //-----------------------------------------------------
                // Recriar o registo Eliminado forçando uma data Termo
                //-----------------------------------------------------
                WNuSequ  = O_NUSEQU;
                WEndDate = $Today();
                EXSR CrtRecord;
             ENDIF;
          ELSEIF (O_PRD01801 < $Today());
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
       // SubRotina..: CrtRecord
       // Objectivo..: Criar o registo Eliminado
       //=========================================================
       BEGSR CrtRecord;
          CDRAMO   = O_CDRAMO;
          NUAPOL   = O_NUAPOL;
          NUSEQU   = WNuSequ;
          CPEDIFIC = O_CPEDIFIC;
          IMPREMIO = O_IMPREMIO;
          PRCOBERT = O_PRCOBERT;
          QTDFRAC  = O_QTDFRAC;
          TXCLCPRM = O_TXCLCPRM;
          PRD01801 = O_PRD01801;
          PRD01802 = WEndDate;
          PRD01804 = O_PRD01804;
          WRITE RPRDP018;
       ENDSR;
      /END-FREE
