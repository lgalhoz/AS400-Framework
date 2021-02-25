     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL) FIXNBR(*ZONED)
     H*==================================================================
     H* Programa..: RPRDT016
     H* Descrição.: Gerir Operações IO ao ficheiro Estado de Conservação
     H*             com gestão do Histórico inteligente.
     H*==================================================================
     FFPRDP016  UF A E           K DISK    COMMIT
     F*==================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SGRLO012
      /Copy QRPGLESRC,SDBSO010

     D*-------------------------------------------------
     D* Formato de Registo
     D*-------------------------------------------------
     D OldRecord     E DS                  ExtName(FPRDP016)
     D                                     Prefix(O_)
     D                                     Based(OldRecPtr)
     D NewRecord     E DS                  ExtName(FPRDP016)
     D                                     Prefix(N_)
     D                                     Based(NewRecPtr)
     D
     D WNuSequ         S                   LIKE(NUSEQU)
     D WEndDate        S                   LIKE(PRD01602)
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
             N_NUSEQU  = $GetNextKey('FPRDP016':'NUSEQU':'(CDRAMO='''+
                         %TRIM(%CHAR(N_CDRAMO)) + ''') AND (NUAPOL='''+
                         %TRIM(%CHAR(N_NUAPOL)) + ''')');
          ENDIF;
          IF (N_PRD01601 = *LOVAL);
             N_PRD01601 = $Today();
          ENDIF;
          IF (N_PRD01602 = *LOVAL);
             N_PRD01602 = %DATE('9999-12-31':*ISO);
          ENDIF;
          IF (N_PRD01601 > N_PRD01602);
             N_PRD01601 = N_PRD01602;
          ENDIF;
          IF (N_PRD01602 <= $Today());
             N_PRD01603 = *ZEROS;
          ELSE;
             N_PRD01603 = 1;
          ENDIF;
          IF (N_PRD01604 = *BLANKS);
             N_PRD01604 = S#USR;
          ENDIF;
          N_PRD01605 = S#USR;
       ELSE;
          //-----------------------------------------------------
          // Apenas alterar Registos Activas
          //-----------------------------------------------------
          IF (O_PRD01601 < $Today()) AND (O_PRD01602 > $Today());
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
                N_PRD01601 = $Today();
                N_PRD01602 = O_PRD01602;
                N_PRD01604 = O_PRD01604;
                N_PRD01605 = S#USR;
             ELSEIF (TTrgEvent = IO#DEL);
                //-----------------------------------------------------
                // Recriar o registo Eliminado forçando uma data Termo
                //-----------------------------------------------------
                WNuSequ  = O_NUSEQU;
                WEndDate = $Today();
                EXSR crtRecord;
             ENDIF;
          ELSEIF (O_PRD01601 < $Today());
             //-----------------------------------------------------
             // Repor os dados Registos se Histórico ou Futuro
             //-----------------------------------------------------
             IF (TTrgEvent = IO#UPD);
                *INH1 = *ON;
                RETURN;
             ELSEIF (TTrgEvent = IO#DEL);
                WNuSequ  = O_NUSEQU;
                WEndDate = O_PRD01602;
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
          CDPOST   = O_CDPOST;
          CDESTCONS= O_CDESTCONS;
          CDTIPCONS= O_CDTIPCONS;
          CDTIPCOB = O_CDTIPCOB;
          CDEDIADJ = O_CDEDIADJ;
          PRD01601 = O_PRD01601;
          PRD01602 = WEndDate;
          PRD01604 = O_PRD01604;
          WRITE RPRDP016;
       ENDSR;
      /END-FREE
