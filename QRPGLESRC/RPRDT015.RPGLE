     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL) FIXNBR(*ZONED)
     H*==================================================================
     H* Programa..: RPRDT015
     H* Descri��o.: Gerir Opera��es IO ao ficheiro de Detalhe Generico (ND)
     H*             com Hist�rico inteligente.
     H*
     H* Observa��o: Implica��es com o ficheiro de Capitais (FPRDP012)
     H*             Criar    --> Indicador com valor (Detalhe = Sim)
     H*             Eliminar --> Indicador com valor (Detalhe = N�o)
     H*==================================================================
     FFPRDP012  UF   E           K DISK    COMMIT PREFIX(EXT_)
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
     D WDBSession      S              5S 0
     D WExtNuSequ      S              5S 0
     D WCodRamo        S              2S 0
     D WNumApol        S              7S 0
     D WNumSequ        S              5S 0
     D WIcDetail       S              1A
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
          EXSR ActFPRDP012;
       ELSE;
          //-----------------------------------------------------
          // ###############   Registos Activos   ###############
          //-----------------------------------------------------
          IF (O_PRD01501 < $Today()) AND (O_PRD01502 > $Today());
             IF (TTrgEvent = IO#UPD);
                //------------------------------------------------
                // Guardar a imagem do registo antes da altera��o
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
                // Recriar o registo Eliminado for�ando uma data Termo
                //-----------------------------------------------------
                WEndDate  = $Today();
                WNuSeqHst = O_NUSEQHST;
                EXSR CrtRecord;
                EXSR ActFPRDP012;
             ENDIF;
          ELSEIF (O_PRD01501 < $Today());
             //-----------------------------------------------------
             // Repor os dados Registos se Hist�rico ou Futuro
             //-----------------------------------------------------
             IF (TTrgEvent = IO#UPD);
                *INH1 = *ON;
                RETURN;
             ELSEIF (TTrgEvent = IO#DEL);
                WEndDate  = $Today();
                WNuSeqHst = O_NUSEQHST;
                EXSR CrtRecord;
             ENDIF;
          ELSEIF (TTrgEvent = IO#DEL);
             EXSR ActFPRDP012;
          ENDIF;
       ENDIF;

       RETURN;

       //=========================================================
       // SubRotina..: ActFPRDP012;
       // Objectivo..: Actualizar o indicador de exit�ncia detalhe
       //=========================================================
       BEGSR ActFPRDP012;
          //-------------------------------------------------------------
          // Actualizar flag no FPRDP012
          //-------------------------------------------------------------
          IF (TTrgEvent = IO#INS);
             WCodRamo  = N_CDRAMO;
             WNumApol  = N_NUAPOL;
             WNumSequ  = N_NUSEQU;
             WIcDetail = 'S';
          ELSEIF (TTrgEvent = IO#DEL);
             WCodRamo  = O_CDRAMO;
             WNumApol  = O_NUAPOL;
             WNumSequ  = O_NUSEQU;
             WIcDetail = 'N';
          ELSE;
             LEAVESR;
          ENDIF;
          WExtNuSequ = *ZEROS;

          WDBSession = $Setfiles('FPRDP012');
          $SetCriteria('(CDRAMO = '''+%CHAR(WCodRamo)+''') AND +
                        (NUAPOL = '''+%CHAR(WNumApol)+''') AND +
                        (NUSEQU = '''+%CHAR(WNumSequ)+''')');
          $AddDBDate($Today());
          IF ($GetRecord(1) > *ZEROS);
             WExtNusequ = $GetFieldNum('NUSEQHST');
          ENDIF;
          $RmvDBSession(WDBSession);
          IF (WExtNuSequ > *ZEROS);
             CHAIN (WCodRamo:WNumApol:WNumSequ:WExtNuSequ) RPRDP012;
             IF (%FOUND(FPRDP012));
                EXT_ICDETAIL = WIcDetail;
                UPDATE RPRDP012 %FIELDS(EXT_ICDETAIL);
             ENDIF;
          ENDIF;
       ENDSR;

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
