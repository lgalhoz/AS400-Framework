     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL) FIXNBR(*ZONED)
     H*==================================================================
     H* Programa..: RCATT114
     H* Descrição.: Gerir Operações IO ao ficheiro de Opções.
     H*==================================================================
     FFCATP114  UF A E           K DISK    COMMIT
     F*==================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SGRLO012
      /Copy QRPGLESRC,SDBSO010

     D*-------------------------------------------------
     D* Formato de Registo
     D*-------------------------------------------------
     D OldRecord     E DS                  ExtName(FCATP114)
     D                                     Prefix(O_)
     D                                     Based(OldRecPtr)
     D NewRecord     E DS                  ExtName(FCATP114)
     D                                     Prefix(N_)
     D                                     Based(NewRecPtr)
     D
     D WEndDate        S                   LIKE(CAT11402)
     D WCdSequ         S                   LIKE(CDSEQU)
     D*===========================================================
     C     *ENTRY        PLIST
     C                   PARM                    TrgBuffer
     C                   PARM                    TrgBufferLen
      /FREE


       //-- DSPLY $getAppCaller();

       OldRecPtr  = %ADDR(TrgBuffer) + TOldRecOff;
       NewRecPtr  = %ADDR(TrgBuffer) + TNewRecOff;
       OldNullPtr = %ADDR(TrgBuffer) + TOldNullOff;
       NewNullPtr = %ADDR(TrgBuffer) + TNewNullOff;

       //-----------------------------------------------------
       // ForCompr registo com valores por defeito
       //-----------------------------------------------------
       IF (TTrgEvent = IO#INS);
          IF (N_CDSEQU = *ZEROS);
             N_CDSEQU  = $GetNextKey('FCATP114':'CDSEQU':'(CDPROD='''+
                           %TRIM(%CHAR(N_CDPROD)) + ''')');
          ENDIF;
          IF (N_CAT11401 = *LOVAL);
             N_CAT11401 = $Today();
          ENDIF;
          IF (N_CAT11402 = *LOVAL);
             N_CAT11402 = %DATE('2999-12-31':*ISO);
          ENDIF;
          IF (N_CAT11401 > N_CAT11402);
             N_CAT11401 = N_CAT11402;
          ENDIF;
          IF (N_CAT11402 <= $Today());
             N_CAT11403 = *ZEROS;
          ELSE;
             N_CAT11403 = 1;
          ENDIF;
          IF (N_CAT11404 = *BLANKS);
             N_CAT11404 = S#USR;
          ENDIF;
          N_CAT11405 = S#USR;
       ELSE;
          //-----------------------------------------------------
          // Apenas alterar Registos Activos
          //-----------------------------------------------------
          IF (O_CAT11401<$Today()) AND (O_CAT11402>$Today());
             IF (TTrgEvent = IO#UPD);
                //------------------------------------------------
                // Guardar a imagem do registo antes da alteração
                //------------------------------------------------
                WEndDate = $Today();
                WCdSequ  = *ZEROS;
                EXSR CrtRegisto;

                //------------------------------------------------
                // Alterar o Registo
                //------------------------------------------------
                N_CDPROD   = O_CDPROD;
                N_CDSEQU   = O_CDSEQU;
                N_CDFUNCT  = O_CDFUNCT;
                N_CAT11401 = $Today();
                N_CAT11402 = O_CAT11402;
                N_CAT11404 = O_CAT11404;
                N_CAT11405 = S#USR;
             ELSEIF (TTrgEvent = IO#DEL);
                //-----------------------------------------------------
                // Recriar o registo Eliminado forçando uma data Termo
                //-----------------------------------------------------
                WEndDate = $Today();
                WCdSequ  = O_CDSEQU;
                EXSR CrtRegisto;
             ENDIF;
          ELSEIF (O_CAT11401 < $Today());
             //-----------------------------------------------------
             // Repor os dados Registos se Histórico ou Futuro
             //-----------------------------------------------------
             IF (TTrgEvent = IO#UPD);
                *INH1 = *ON;
                RETURN;
             ELSEIF (TTrgEvent = IO#DEL);
                WEndDate = O_CAT11402;
                WCdSequ  = O_CDSEQU;
                EXSR CrtRegisto;
             ENDIF;
          ENDIF;
       ENDIF;

       RETURN;

       //=========================================================
       // SubRotina..: CrtRegisto
       // Objectivo..: Criar o registo Eliminado
       //=========================================================
       BEGSR CrtRegisto;
          CDPROD   = O_CDPROD;
          CDSEQU   = WCdSequ;
          NUORDEM  = O_NUORDEM;
          CDFUNCT  = O_CDFUNCT;
          CAT11401 = O_CAT11401;
          CAT11402 = WEndDate;
          CAT11404 = O_CAT11404;
          WRITE RCATP114;
       ENDSR;
      /END-FREE
