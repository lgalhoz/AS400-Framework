     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL) FIXNBR(*ZONED)
     H*==================================================================
     H* Programa..: RPRDT022
     H* Descrição.: Gerir Operações IO ao ficheiro de Descontos e Agravamentos
     H*             com uma gestão do Histórico inteligente.
     H*==================================================================
     FFPRDP022  UF A E           K DISK    COMMIT
     F*==================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SGRLO012
      /Copy QRPGLESRC,SDBSO010

     D*-------------------------------------------------
     D* Formato de Registo
     D*-------------------------------------------------
     D OldRecord     E DS                  ExtName(FPRDP022)
     D                                     Prefix(O_)
     D                                     Based(OldRecPtr)
     D NewRecord     E DS                  ExtName(FPRDP022)
     D                                     Prefix(N_)
     D                                     Based(NewRecPtr)
     D
     D WEndDate        S                   LIKE(PRD02202)
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
             N_NUSEQU  = $GetNextKey('FPRDP022':'NUSEQU':'(CDRAMO='''+
                         %TRIM(%CHAR(N_CDRAMO)) + ''') AND (NUAPOL='''+
                         %TRIM(%CHAR(N_NUAPOL)) + ''')');
          ENDIF;
          IF (N_PRD02201 = *LOVAL);
             N_PRD02201 = $Today();
          ENDIF;
          IF (N_PRD02202 = *LOVAL);
             N_PRD02202 = %DATE('9999-12-31':*ISO);
          ENDIF;
          IF (N_PRD02201 > N_PRD02202);
             N_PRD02201 = N_PRD02202;
          ENDIF;
          IF (N_PRD02202 <= $Today());
             N_PRD02203 = *ZEROS;
          ELSE;
             N_PRD02203 = 1;
          ENDIF;
          IF (N_PRD02204 = *BLANKS);
             N_PRD02204 = S#USR;
          ENDIF;
          N_PRD02205 = S#USR;
       ELSE;
          //-----------------------------------------------------
          // Apenas alterar Registos Activas
          //-----------------------------------------------------
          IF (O_PRD02201 < $Today()) AND (O_PRD02202 > $Today());
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
          ELSEIF (O_PRD02201 < $Today());
             //-----------------------------------------------------
             // Repor os dados Registos se Histórico ou Futuro
             //-----------------------------------------------------
             IF (TTrgEvent = IO#UPD);
                *INH1 = *ON;
                RETURN;
             ELSEIF (TTrgEvent = IO#DEL);
                WEndDate = O_PRD02202;
                EXSR crtDelRecord;
             ENDIF;
          ENDIF;
       ENDIF;

       RETURN;

       //=========================================================
       // SubRotina..: crtDelRecord
       // Objectivo..: Criar o registo Eliminado
       //=========================================================
       BEGSR crtDelRecord;
          CDRAMO   = O_CDRAMO;
          NUAPOL   = O_NUAPOL;
          NUSEQU   = O_NUSEQU;
          CDITEM   = O_CDITEM;
          PRD02201 = O_PRD02201;
          PRD02202 = WEndDate;
          PRD02204 = O_PRD02204;
          WRITE RPRDP022;
       ENDSR;
      /END-FREE
