     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL) FIXNBR(*ZONED)
     H*==================================================================
     H* Programa..: RPRDT008
     H* Descrição.: Gerir Operações IO ao ficheiro de Coberturas, com
     H*             gestão do Histórico inteligente.
     H*==================================================================
     FFPRDP003  UF A E           K DISK    COMMIT
     F*==================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SGRLO012
      /Copy QRPGLESRC,SDBSO010

     D*-------------------------------------------------
     D* Formato de Registo
     D*-------------------------------------------------
     D OldRecord     E DS                  ExtName(FPRDP003)
     D                                     Prefix(O_)
     D                                     Based(OldRecPtr)
     D NewRecord     E DS                  ExtName(FPRDP003)
     D                                     Prefix(N_)
     D                                     Based(NewRecPtr)
     D
     D WEndDate        S                   LIKE(PRD00302)
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

       //-- DSPLY $getAppCaller();

       OldRecPtr  = %ADDR(TrgBuffer) + TOldRecOff;
       NewRecPtr  = %ADDR(TrgBuffer) + TNewRecOff;
       OldNullPtr = %ADDR(TrgBuffer) + TOldNullOff;
       NewNullPtr = %ADDR(TrgBuffer) + TNewNullOff;

       //-----------------------------------------------------
       // Formatar registo com valores por defeito
       //-----------------------------------------------------
       IF (TTrgEvent = IO#INS);
          IF (N_NUSEQU = *ZEROS);
             N_NUSEQU  = $GetNextKey('FPRDP008':'NUSEQU':'(CDRAMO='''+
                           %TRIM(%CHAR(N_CDRAMO)) + ''') AND (NUAPOL='''+
                           %TRIM(%CHAR(N_NUAPOL)) + ''')');
          ENDIF;
          IF (N_PRD00301 = *LOVAL);
             N_PRD00301 = $Today();
          ENDIF;
          IF (N_PRD00302 = *LOVAL);
             N_PRD00302 = %DATE('2999-12-31':*ISO);
          ENDIF;
          IF (N_PRD00301 > N_PRD00302);
             N_PRD00301 = N_PRD00302;
          ENDIF;
          IF (N_PRD00302 <= $Today());
             N_PRD00303 = *ZEROS;
          ELSE;
             N_PRD00303 = 1;
          ENDIF;
          IF (N_PRD00304 = *BLANKS);
             N_PRD00304 = S#USR;
          ENDIF;
          N_PRD00305 = S#USR;
       ELSE;
          //-----------------------------------------------------
          // Apenas alterar Registos Activos
          //-----------------------------------------------------
          IF (O_PRD00301<$Today()) AND (O_PRD00302>$Today());
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
                N_PRD00301 = $Today();
                N_PRD00302 = O_PRD00302;
                N_PRD00304 = O_PRD00304;
                N_PRD00305 = S#USR;
             ELSEIF (TTrgEvent = IO#DEL);
                //-----------------------------------------------------
                // Recriar o registo Eliminado forçando uma data Termo
                //-----------------------------------------------------
                WEndDate = $Today();
                WNuSequ  = O_NUSEQU;
                EXSR crtRecord;
             ENDIF;
          ELSEIF (O_PRD00301 < $Today());
             //-----------------------------------------------------
             // Repor os dados Registos se Histórico ou Futuro
             //-----------------------------------------------------
             IF (TTrgEvent = IO#UPD);
                *INH1 = *ON;
                RETURN;
             ELSEIF (TTrgEvent = IO#DEL);
                WEndDate = O_PRD00302;
                WNuSequ  = O_NUSEQU;
                EXSR crtRecord;
             ENDIF;
          ENDIF;
       ENDIF;

       Return;

       //=========================================================
       // SubRotina..: crtRecord
       // Objectivo..: Criar o registo Eliminado
       //=========================================================
       BEGSR CrtRecord;
          CDRAMO   = O_CDRAMO;
          NUAPOL   = O_NUAPOL;
          NUSEQU   = WNuSequ;
          NMSEGNET = O_NMSEGNET;
          PRD00301 = O_PRD00301;
          PRD00302 = WEndDate;
          PRD00304 = O_PRD00304;
          WRITE RPRDP003;
       ENDSR;
      /END-FREE
