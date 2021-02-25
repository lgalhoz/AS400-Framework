     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL) FIXNBR(*ZONED)
     H*==================================================================
     H* Programa..: RPRDT008
     H* Descrição.: Gerir Operações IO ao ficheiro de Coberturas, com
     H*             gestão do Histórico inteligente.
     H*==================================================================
     FFPRDP008  UF A E           K DISK    COMMIT
     F*==================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SGRLO012
      /Copy QRPGLESRC,SDBSO010

     D*-------------------------------------------------
     D* Formato de Registo
     D*-------------------------------------------------
     D OldRecord     E DS                  ExtName(FPRDP008)
     D                                     Prefix(O_)
     D                                     Based(OldRecPtr)
     D NewRecord     E DS                  ExtName(FPRDP008)
     D                                     Prefix(N_)
     D                                     Based(NewRecPtr)
     D
     D WEndDate        S                   LIKE(PRD00802)
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
          IF (N_PRD00801 = *LOVAL);
             N_PRD00801 = $Today();
          ENDIF;
          IF (N_PRD00802 = *LOVAL);
             N_PRD00802 = %DATE('2999-12-31':*ISO);
          ENDIF;
          IF (N_PRD00801 > N_PRD00802);
             N_PRD00801 = N_PRD00802;
          ENDIF;
          IF (N_PRD00802 <= $Today());
             N_PRD00803 = *ZEROS;
          ELSE;
             N_PRD00803 = 1;
          ENDIF;
          IF (N_PRD00804 = *BLANKS);
             N_PRD00804 = S#USR;
          ENDIF;
          N_PRD00805 = S#USR;
       ELSE;
          //-----------------------------------------------------
          // Apenas alterar Registos Activos
          //-----------------------------------------------------
          IF (O_PRD00801<$Today()) AND (O_PRD00802>$Today());
             IF (TTrgEvent = IO#UPD);
                *INH1 = *ON;
                RETURN;
             ELSEIF (TTrgEvent = IO#DEL);
                //-----------------------------------------------------
                // Recriar o registo Eliminado forçando uma data Termo
                //-----------------------------------------------------
                WEndDate = $Today();
                EXSR crtDelRecord;
             ENDIF;
          ELSEIF (O_PRD00801 < $Today());
             //-----------------------------------------------------
             // Repor os dados Registos se Histórico ou Futuro
             //-----------------------------------------------------
             IF (TTrgEvent = IO#UPD);
                *INH1 = *ON;
                RETURN;
             ELSEIF (TTrgEvent = IO#DEL);
                WEndDate = O_PRD00802;
                EXSR crtDelRecord;
             ENDIF;
          ENDIF;
       ENDIF;

       Return;

       //=========================================================
       // SubRotina..: crtDelRecord
       // Objectivo..: Criar o registo Eliminado
       //=========================================================
       BEGSR CrtDelRecord;
          CDRAMO   = O_CDRAMO;
          NUAPOL   = O_NUAPOL;
          NUSEQU   = O_NUSEQU;
          CDCOBERT = O_CDCOBERT;
          PRD00801 = O_PRD00801;
          PRD00802 = WEndDate;
          PRD00804 = O_PRD00804;
          WRITE RPRDP008;
       ENDSR;
      /END-FREE
