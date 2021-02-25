     H OPTION(*DEBUGIO:*SRCSTMT)
     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL) FIXNBR(*ZONED)
     H*==================================================================
     H* Programa..: RPRDT010
     H* Descrição.: Gerir Operações IO ao ficheiro de Notas Descritivas
     H*             com Histórico inteligente.
     H*==================================================================
     FFPRDP010  UF A E           K DISK    COMMIT
     F*==================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SGRLO012
      /Copy QRPGLESRC,SDBSO010

     D*-------------------------------------------------
     D* Formato de Registo
     D*-------------------------------------------------
     D OldRecord     E DS                  ExtName(FPRDP010)
     D                                     Prefix(O_)
     D                                     Based(OldRecPtr)
     D NewRecord     E DS                  ExtName(FPRDP010)
     D                                     Prefix(N_)
     D                                     Based(NewRecPtr)
     D
     D*==> Campos de Trabalho
     D WEndDate        S               D
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
          IF (N_NUSEQU = *ZEROS);
             N_NUSEQU  = $GetNextKey('FPRDP010':'NUSEQU':'(CDRAMO='''+
                         %TRIM(%CHAR(N_CDRAMO)) + ''') AND (NUAPOL='''+
                         %TRIM(%CHAR(N_NUAPOL)) + ''')');
          ENDIF;
          IF (N_NUSQSUP = *ZEROS);
             N_NUSQSUP = N_NUSEQU;
          ENDIF;
          IF (N_PRD01001 = *LOVAL);
             N_PRD01001 = $Today();
          ENDIF;
          IF (N_PRD01002 = *LOVAL);
             N_PRD01002 = %DATE('9999-12-31':*ISO);
          ENDIF;
          IF (N_PRD01001 > N_PRD01002);
             N_PRD01001 = N_PRD01002;
          ENDIF;
          IF (N_PRD01002 <= $Today());
             N_PRD01003 = *ZEROS;
          ELSE;
             N_PRD01003 = 1;
          ENDIF;
          IF (N_PRD01004 = *BLANKS);
             N_PRD01004 = S#USR;
          ENDIF;
          N_PRD01005 = S#USR;
       ELSE;
          //-----------------------------------------------------
          // Apenas alterar Registos Activas
          //-----------------------------------------------------
          IF (O_PRD01001 < $Today()) AND (O_PRD01002 > $Today());
             IF (TTrgEvent = IO#UPD);
                *INH1 = *ON;
                RETURN;
             ELSEIF (TTrgEvent = IO#DEL);
                //-----------------------------------------------------
                // Recriar o registo Eliminado forçando uma data Termo
                //-----------------------------------------------------
                WEndDate = $Today();
                EXSR crtDelRecord;

                //-----------------------------------------------------
                // Actualizar Relação com outros ficheiros de dados
                //-----------------------------------------------------
                // Localizar o registo activo (aplicar regra)
                // Forçar eliminação do registo
                //-----------------------------------------------------
                // FPRDP012 - Texto das Notas Descritivas
                // FPRDP013 - Detalhe Habitação
                // FPRDP017 - Detalhe Condominio
                // FPRDP015 - Detalhe Genérico
                //-----------------------------------------------------
             ENDIF;
          ELSEIF (O_PRD01001 < $Today());
             //-----------------------------------------------------
             // Repor os dados Registos se Histórico ou Futuro
             //-----------------------------------------------------
             IF (TTrgEvent = IO#UPD);
                *INH1 = *ON;
                RETURN;
             ELSEIF (TTrgEvent = IO#DEL);
                WEndDate = O_PRD01002;
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
          NUSQSUP  = O_NUSQSUP;
          CDTEXTO  = O_CDTEXTO;
          PRD01001 = O_PRD01001;
          PRD01002 = WEndDate;
          PRD01004 = O_PRD01004;
          WRITE RPRDP010;
       ENDSR;
      /END-FREE
