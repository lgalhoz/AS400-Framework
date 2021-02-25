     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL) FIXNBR(*ZONED)
     H*==================================================================
     H* Programa..: RPRDT009
     H* Descrição.: Gerir Operações IO ao ficheiro de Condições e Clausulas
     H*             com uma gestão do Histórico inteligente.
     H*==================================================================
     FFPRDP009  UF A E           K DISK    COMMIT
     F*==================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SGRLO012
      /Copy QRPGLESRC,SDBSO010

     D*-------------------------------------------------
     D* Formato de Registo
     D*-------------------------------------------------
     D OldRecord     E DS                  ExtName(FPRDP009)
     D                                     Prefix(O_)
     D                                     Based(OldRecPtr)
     D NewRecord     E DS                  ExtName(FPRDP009)
     D                                     Prefix(N_)
     D                                     Based(NewRecPtr)
     D
     D WEndDate        S                   LIKE(PRD00902)
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
             N_NUSEQU  = $GetNextKey('FPRDP009':'NUSEQU':'(CDRAMO='''+
                         %TRIM(%CHAR(N_CDRAMO)) + ''') AND (NUAPOL='''+
                         %TRIM(%CHAR(N_NUAPOL)) + ''')');
          ENDIF;
          IF (N_PRD00901 = *LOVAL);
             N_PRD00901 = $Today();
          ENDIF;
          IF (N_PRD00902 = *LOVAL);
             N_PRD00902 = %DATE('9999-12-31':*ISO);
          ENDIF;
          IF (N_PRD00901 > N_PRD00902);
             N_PRD00901 = N_PRD00902;
          ENDIF;
          IF (N_PRD00902 <= $Today());
             N_PRD00903 = *ZEROS;
          ELSE;
             N_PRD00903 = 1;
          ENDIF;
          IF (N_PRD00904 = *BLANKS);
             N_PRD00904 = S#USR;
          ENDIF;
          N_PRD00905 = S#USR;
       ELSE;
          //-----------------------------------------------------
          // Apenas alterar Registos Activas
          //-----------------------------------------------------
          IF (O_PRD00901 < $Today()) AND (O_PRD00902 > $Today());
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
          ELSEIF (O_PRD00901 < $Today());
             //-----------------------------------------------------
             // Repor os dados Registos se Histórico ou Futuro
             //-----------------------------------------------------
             IF (TTrgEvent = IO#UPD);
                *INH1 = *ON;
                RETURN;
             ELSEIF (TTrgEvent = IO#DEL);
                WEndDate = O_PRD00902;
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
          PRD00901 = O_PRD00901;
          PRD00902 = WEndDate;
          PRD00904 = O_PRD00904;
          WRITE RPRDP009;
       ENDSR;
      /END-FREE
