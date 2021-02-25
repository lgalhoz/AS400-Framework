     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL) FIXNBR(*ZONED)
     H*==================================================================
     H* Programa..: RPRDT012
     H* Descrição.: Gerir Operações IO ao ficheiro de Texto das Notas Descritivas
     H*             com Histórico inteligente.
     H*==================================================================
     FFPRDP012  UF A E           K DISK    COMMIT
     F*==================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SGRLO012
      /Copy QRPGLESRC,SDBSO010

     D*-------------------------------------------------
     D* Formato de Registo
     D*-------------------------------------------------
     D OldRecord     E DS                  ExtName(FPRDP012)
     D                                     Prefix(O_)
     D                                     Based(OldRecPtr)
     D NewRecord     E DS                  ExtName(FPRDP012)
     D                                     Prefix(N_)
     D                                     Based(NewRecPtr)
     D
     D WEndDate        S                   LIKE(PRD01202)
     D WNuSqHst        S                   LIKE(NUSEQHST)
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
          N_NUSEQHST  = $GetNextKey('FPRDP012':'NUSEQHST':'(CDRAMO='''+
                        %TRIM(%CHAR(N_CDRAMO)) + ''') AND (NUAPOL='''+
                        %TRIM(%CHAR(N_NUAPOL)) + ''') AND (NUSEQU='''+
                        %TRIM(%CHAR(N_NUSEQU)) + ''')');
          IF (N_PRD01201 = *LOVAL);
             N_PRD01201 = $Today();
          ENDIF;
          IF (N_PRD01202 = *LOVAL);
            N_PRD01202 = %DATE('9999-12-31':*ISO);
          ENDIF;
          IF (N_PRD01201 > N_PRD01202);
             N_PRD01201 = N_PRD01202;
          ENDIF;
          IF (N_PRD01202 <= $Today());
             N_PRD01203 = *ZEROS;
          ELSE;
             N_PRD01203 = 1;
          ENDIF;
          IF (N_PRD01204 = *BLANKS);
             N_PRD01204 = S#USR;
          ENDIF;
          N_PRD01205 = S#USR;
       ELSE;
          //-----------------------------------------------------
          // Apenas alterar Registos Activas
          //-----------------------------------------------------
          IF (O_PRD01201 < $Today()) AND (O_PRD01202 > $Today());
             IF (TTrgEvent = IO#UPD);
                //------------------------------------------------
                // Guardar a imagem do registo antes da alteração
                //------------------------------------------------
                WNuSqHst = *ZEROS;
                WEndDate = $Today();
                EXSR CrtRegisto;

                //------------------------------------------------
                // Alterar o Registo
                //------------------------------------------------
                N_CDRAMO   = O_CDRAMO;
                N_NUAPOL   = O_NUAPOL;
                N_NUSEQU   = O_NUSEQU;
                N_NUSEQHST = O_NUSEQHST;
                N_PRD01201 = $Today();
                N_PRD01202 = O_PRD01202;
                N_PRD01204 = O_PRD01204;
                N_PRD01205 = S#USR;
             ELSEIF (TTrgEvent = IO#DEL);
                //-----------------------------------------------------
                // Recriar o registo Eliminado forçando uma data Termo
                //-----------------------------------------------------
                WNuSqHst = O_NUSEQHST;
                WEndDate = $Today();
                EXSR CrtRegisto;
             ENDIF;
          ELSEIF (O_PRD01201 < $Today());
             //-----------------------------------------------------
             // Repor os dados Registos se Histórico ou Futuro
             //-----------------------------------------------------
             IF (TTrgEvent = IO#UPD);
                *INH1 = *ON;
                RETURN;
             ELSEIF (TTrgEvent = IO#DEL);
                WNuSqHst = O_NUSEQHST;
                WEndDate = $Today();
                EXSR CrtRegisto;
             ENDIF;
          ENDIF;
       ENDIF;

       RETURN;

       //=========================================================
       // SubRotina..: crtRegisto
       // Objectivo..: Criar o registo Eliminado
       //=========================================================
       BEGSR crtRegisto;
          CDRAMO   = O_CDRAMO;
          NUAPOL   = O_NUAPOL;
          NUSEQU   = O_NUSEQU;
          NUSEQHST = WNuSqHst;
          CPSEGURO = O_CPSEGURO;
          DSDETAIL = O_DSDETAIL;
          ICDETAIL = O_ICDETAIL;
          PRD01201 = O_PRD01201;
          PRD01202 = WEndDate;
          PRD01204 = O_PRD01204;
          WRITE RPRDP012;
       ENDSR;
      /END-FREE
