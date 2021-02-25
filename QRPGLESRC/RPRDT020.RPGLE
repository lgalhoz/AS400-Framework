     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL) FIXNBR(*ZONED)
     H*==================================================================
     H* Programa..: RPRDT020
     H* Descrição.: Gerir Operações IO ao ficheiro Parâmetros do Edificio
     H*             com gestão do Histórico inteligente.
     H*==================================================================
     FFPRDP020  UF A E           K DISK    COMMIT
     F*==================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SGRLO012
      /Copy QRPGLESRC,SDBSO010

     D*-------------------------------------------------
     D* Formato de Registo
     D*-------------------------------------------------
     D OldRecord     E DS                  ExtName(FPRDP020)
     D                                     Prefix(O_)
     D                                     Based(OldRecPtr)
     D NewRecord     E DS                  ExtName(FPRDP020)
     D                                     Prefix(N_)
     D                                     Based(NewRecPtr)
     D
     D WNuSequ         S                   LIKE(NUSEQU)
     D WEndDate        S                   LIKE(PRD02002)
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
             N_NUSEQU  = $GetNextKey('FPRDP020':'NUSEQU':'(CDRAMO='''+
                         %TRIM(%CHAR(N_CDRAMO)) + ''') AND (NUAPOL='''+
                         %TRIM(%CHAR(N_NUAPOL)) + ''')');
          ENDIF;
          IF (N_PRD02001 = *LOVAL);
             N_PRD02001 = $Today();
          ENDIF;
          IF (N_PRD02002 = *LOVAL);
             N_PRD02002 = %DATE('9999-12-31':*ISO);
          ENDIF;
          IF (N_PRD02001 > N_PRD02002);
             N_PRD02001 = N_PRD02002;
          ENDIF;
          IF (N_PRD02002 <= $Today());
             N_PRD02003 = *ZEROS;
          ELSE;
             N_PRD02003 = 1;
          ENDIF;
          IF (N_PRD02004 = *BLANKS);
             N_PRD02004 = S#USR;
          ENDIF;
          N_PRD02005 = S#USR;
       ELSE;
          //-----------------------------------------------------
          // Apenas alterar Registos Activas
          //-----------------------------------------------------
          IF (O_PRD02001 < $Today()) AND (O_PRD02002 > $Today());
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
                N_PRD02001 = $Today();
                N_PRD02002 = O_PRD02002;
                N_PRD02004 = O_PRD02004;
                N_PRD02005 = S#USR;
             ELSEIF (TTrgEvent = IO#DEL);
                //-----------------------------------------------------
                // Recriar o registo Eliminado forçando uma data Termo
                //-----------------------------------------------------
                WNuSequ  = O_NUSEQU;
                WEndDate = $Today();
                EXSR CrtRecord;
             ENDIF;
          ELSEIF (O_PRD02001 < $Today());
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
          PRD02001 = O_PRD02001;
          PRD02002 = WEndDate;
          PRD02004 = O_PRD02004;
          WRITE RPRDP020;
       ENDSR;
      /END-FREE
