     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL) FIXNBR(*ZONED)
     H*==================================================================
     H* Programa..: RPRDT013
     H* Descrição.: Gerir Operações IO ao ficheiro de Detalhe Habitação (ND)
     H*             com Histórico inteligente.
     H*
     H* Observação: Implicações com os valores (FPRDP012)
     H*             Criar    --> Indicador com valor (Detalhe = Sim)
     H*             Eliminar --> Indicador com valor (Detalhe = Não)
     H*==================================================================
     FFPRDP013  UF A E           K DISK    COMMIT
     F*==================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SGRLO012
      /Copy QRPGLESRC,SDBSO010

     D*-------------------------------------------------
     D* Formato de Registo
     D*-------------------------------------------------
     D OldRecord     E DS                  ExtName(FPRDP013)
     D                                     Prefix(O_)
     D                                     Based(OldRecPtr)
     D NewRecord     E DS                  ExtName(FPRDP013)
     D                                     Prefix(N_)
     D                                     Based(NewRecPtr)
     D
     D WEndDate        S                   LIKE(PRD01302)
     D WNuSeqHst       S                   LIKE(NUSEQHST)
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
          N_NUSEQHST  = $GetNextKey('FPRDP013':'NUSEQHST':'(CDRAMO='''+
                        %TRIM(%CHAR(N_CDRAMO)) + ''') AND (NUAPOL='''+
                        %TRIM(%CHAR(N_NUAPOL)) + ''') AND (NUSEQU='''+
                        %TRIM(%CHAR(N_NUSEQU)) + ''')');
          IF (N_PRD01301 = *LOVAL);
             N_PRD01301 = $Today();
          ENDIF;
          IF (N_PRD01302 = *LOVAL);
             N_PRD01302 = %DATE('9999-12-31':*ISO);
          ENDIF;
          IF (N_PRD01301 > N_PRD01302);
             N_PRD01301 = N_PRD01302;
          ENDIF;
          IF (N_PRD01302 <= $Today());
             N_PRD01303 = *ZEROS;
          ELSE;
             N_PRD01303 = 1;
          ENDIF;
          IF (N_PRD01304 = *BLANKS);
             N_PRD01304 = S#USR;
          ENDIF;
          N_PRD01305 = S#USR;
       ELSE;
          //-----------------------------------------------------
          // Apenas alterar Registos Activas
          //-----------------------------------------------------
          IF (O_PRD01301 < $Today()) AND (O_PRD01302 > $Today());
             IF (TTrgEvent = IO#UPD);
                //------------------------------------------------
                // Guardar a imagem do registo antes da alteração
                //------------------------------------------------
                WNuSeqHst = *ZEROS;
                WEndDate  = $Today();
                EXSR CrtRecord;

                //------------------------------------------------
                // Alterar o Registo
                //------------------------------------------------
                N_CDRAMO   = O_CDRAMO;
                N_NUAPOL   = O_NUAPOL;
                N_NUSEQU   = O_NUSEQU;
                N_NUSEQHST = O_NUSEQHST;
                N_PRD01301 = $Today();
                N_PRD01302 = O_PRD01302;
                N_PRD01304 = O_PRD01304;
                N_PRD01305 = S#USR;
             ELSEIF (TTrgEvent = IO#DEL);
                //-----------------------------------------------------
                // Recriar o registo Eliminado forçando uma data Termo
                //-----------------------------------------------------
                WEndDate  = $Today();
                WNuSeqHst = NUSEQHST;
                EXSR CrtRecord;
             ENDIF;
          ELSEIF (O_PRD01301 < $Today());
             //-----------------------------------------------------
             // Repor os dados Registos se Histórico ou Futuro
             //-----------------------------------------------------
             IF (TTrgEvent = IO#UPD);
                *INH1 = *ON;
                RETURN;
             ELSEIF (TTrgEvent = IO#DEL);
                WEndDate = O_PRD01302;
                WNuSeqHst = NUSEQHST;
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
          CDSEGUR  = O_CDSEGUR;
          MORADA   = O_MORADA;
          CDPOSTAL = O_CDPOSTAL;
          LOCALIDADE=O_LOCALIDADE;
          CDCREDOR = O_CDCREDOR;
          DSOBSERV1= O_DSOBSERV1;
          DSOBSERV2= O_DSOBSERV2;
          PRD01301 = O_PRD01301;
          PRD01302 = WEndDate;
          PRD01304 = O_PRD01304;
          WRITE RPRDP013;
       ENDSR;
      /END-FREE
