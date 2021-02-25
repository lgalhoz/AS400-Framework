     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL) FIXNBR(*ZONED)
     H*==================================================================
     H* Programa..: RPRDT015
     H* Descrição.: Gerir Operações IO ao ficheiro de Detalhe Generico (ND)
     H*             com Histórico inteligente.
     H*==================================================================
     FFPRDP015  UF A E           K DISK    COMMIT
     F*==================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SGRLO012
      /Copy QRPGLESRC,SDBSO010

     D*-------------------------------------------------
     D* Formato de Registo
     D*-------------------------------------------------
     D OldRecord     E DS                  ExtName(FPRDP015)
     D                                     Prefix(O_)
     D                                     Based(OldRecPtr)
     D NewRecord     E DS                  ExtName(FPRDP015)
     D                                     Prefix(N_)
     D                                     Based(NewRecPtr)
     D
     D WEndDate        S                   LIKE(PRD01502)
     D WNuSeqHst       S                   LIKE(NUSEQU)
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
          N_NUSEQHST  = $GetNextKey('FPRDP015':'NUSEQHST':'(CDRAMO='''+
                        %TRIM(%CHAR(N_CDRAMO)) + ''') AND (NUAPOL='''+
                        %TRIM(%CHAR(N_NUAPOL)) + ''') AND (NUSEQU='''+
                        %TRIM(%CHAR(N_NUSEQU)) + ''')');
          IF (N_PRD01501 = *LOVAL);
             N_PRD01501 = $Today();
          ENDIF;
          IF (N_PRD01502 = *LOVAL);
             N_PRD01502 = %DATE('9999-12-31':*ISO);
          ENDIF;
          IF (N_PRD01501 > N_PRD01502);
             N_PRD01501 = N_PRD01502;
          ENDIF;
          IF (N_PRD01502 <= $Today());
             N_PRD01503 = *ZEROS;
          ELSE;
             N_PRD01503 = 1;
          ENDIF;
          IF (N_PRD01504 = *BLANKS);
             N_PRD01504 = S#USR;
          ENDIF;
          N_PRD01505 = S#USR;
       ELSE;
          //-----------------------------------------------------
          // Apenas alterar Registos Activas
          //-----------------------------------------------------
          IF (O_PRD01501 < $Today()) AND (O_PRD01502 > $Today());
             IF (TTrgEvent = IO#UPD);
                //------------------------------------------------
                // Guardar a imagem do registo antes da alteração
                //------------------------------------------------
                WEndDate  = $Today();
                WNuSeqHst = *ZEROS;
                EXSR CrtRecord;

                //------------------------------------------------
                // Alterar o Registo
                //------------------------------------------------
                N_CDRAMO   = O_CDRAMO;
                N_NUAPOL   = O_NUAPOL;
                N_NUSEQU   = O_NUSEQU;
                N_NUSEQHST = O_NUSEQHST;
                N_PRD01501 = $Today();
                N_PRD01502 = O_PRD01502;
                N_PRD01504 = O_PRD01504;
                N_PRD01505 = S#USR;
             ELSEIF (TTrgEvent = IO#DEL);
                //-----------------------------------------------------
                // Recriar o registo Eliminado forçando uma data Termo
                //-----------------------------------------------------
                WEndDate  = $Today();
                WNuSeqHst = O_NUSEQHST;
                EXSR CrtRecord;
             ENDIF;
          ELSEIF (O_PRD01501 < $Today());
             //-----------------------------------------------------
             // Repor os dados Registos se Histórico ou Futuro
             //-----------------------------------------------------
             IF (TTrgEvent = IO#UPD);
                *INH1 = *ON;
                RETURN;
             ELSEIF (TTrgEvent = IO#DEL);
                WEndDate  = $Today();
                WNuSeqHst = O_NUSEQHST;
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
          NUSEQU   = O_NUSEQU;
          NUSEQHST = WNuSeqHst;
          DSITEM   = O_DSITEM;
          CDRISCO01= O_CDRISCO01;
          CDRISCO02= O_CDRISCO02;
          CDRISCO03= O_CDRISCO03;
          DSOBSERV1= O_DSOBSERV1;
          DSOBSERV2= O_DSOBSERV2;
          PRD01501 = O_PRD01501;
          PRD01502 = WEndDate;
          PRD01504 = O_PRD01504;
          WRITE RPRDP015;
       ENDSR;
      /END-FREE
