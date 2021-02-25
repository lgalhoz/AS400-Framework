     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL) FIXNBR(*ZONED)
     H*==================================================================
     H* Programa..: RPRDT017
     H* Descrição.: Gerir Operações IO ao ficheiro de Detalhe Condominio (ND)
     H*             com Histórico inteligente.
     H*
     H* Observação: Implicações com o ficheiro de Capitais (FPRDP012)
     H*             Criar    --> Indicador com valor (Detalhe = Sim)
     H*             Eliminar --> Indicador com valor (Detalhe = Não)
     H*==================================================================
     FFPRDP012  UF   E           K DISK    COMMIT PREFIX(EXT_)
     FFPRDP017  UF A E           K DISK    COMMIT
     F*==================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SGRLO012
      /Copy QRPGLESRC,SDBSO010

     D*-------------------------------------------------
     D* Formato de Registo
     D*-------------------------------------------------
     D OldRecord     E DS                  ExtName(FPRDP017)
     D                                     Prefix(O_)
     D                                     Based(OldRecPtr)
     D NewRecord     E DS                  ExtName(FPRDP017)
     D                                     Prefix(N_)
     D                                     Based(NewRecPtr)
     D
     D WEndDate        S                   LIKE(PRD01702)
     D WNuSeqHst       S                   LIKE(NUSEQHST)
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
          N_NUSEQHST  = $GetNextKey('FPRDP017':'NUSEQHST':'(CDRAMO='''+
                        %TRIM(%CHAR(N_CDRAMO)) + ''') AND (NUAPOL='''+
                        %TRIM(%CHAR(N_NUAPOL)) + ''') AND (NUSEQU='''+
                        %TRIM(%CHAR(N_NUSEQU)) + ''')');
          IF (N_PRD01701 = *LOVAL);
             N_PRD01701 = $Today();
          ENDIF;
          IF (N_PRD01702 = *LOVAL);
             N_PRD01702 = %DATE('9999-12-31':*ISO);
          ENDIF;
          IF (N_PRD01701 > N_PRD01702);
             N_PRD01701 = N_PRD01702;
          ENDIF;
          IF (N_PRD01702 <= $Today());
             N_PRD01703 = *ZEROS;
          ELSE;
             N_PRD01703 = 1;
          ENDIF;
          IF (N_PRD01704 = *BLANKS);
             N_PRD01704 = S#USR;
          ENDIF;
          N_PRD01705 = S#USR;
          EXSR ActFPRDP012;
       ELSE;
          //-----------------------------------------------------
          // ###############   Registos Activos   ###############
          //-----------------------------------------------------
          IF (O_PRD01701 < $Today()) AND (O_PRD01702 > $Today());
             IF (TTrgEvent = IO#UPD);
                //------------------------------------------------
                // Guardar a imagem do registo antes da alteração
                //------------------------------------------------
                WNuSeqHst = *ZEROS;
                WEndDate = $Today();
                EXSR CrtRecord;

                //------------------------------------------------
                // Alterar o Registo
                //------------------------------------------------
                N_CDRAMO   = O_CDRAMO;
                N_NUAPOL   = O_NUAPOL;
                N_NUSEQU   = O_NUSEQU;
                N_NUSEQHST = O_NUSEQHST;
                N_PRD01701 = $Today();
                N_PRD01702 = O_PRD01702;
                N_PRD01704 = O_PRD01704;
                N_PRD01705 = S#USR;
             ELSEIF (TTrgEvent = IO#DEL);
                //-----------------------------------------------------
                // Recriar o registo Eliminado forçando uma data Termo
                //-----------------------------------------------------
                WNuSeqHst = O_NUSEQHST;
                WEndDate  = $Today();
                EXSR CrtRecord;
                EXSR ActFPRDP012;
             ENDIF;
          ELSEIF (O_PRD01701 < $Today());
             //-----------------------------------------------------
             // Repor os dados Registos se Histórico ou Futuro
             //-----------------------------------------------------
             IF (TTrgEvent = IO#UPD);
                *INH1 = *ON;
                RETURN;
             ELSEIF (TTrgEvent = IO#DEL);
                WNuSeqHst = O_NUSEQHST;
                WEndDate  = $Today();
                EXSR CrtRecord;
             ENDIF;
          ELSEIF (TTrgEvent = IO#DEL);
             EXSR ActFPRDP012;
          ENDIF;
       ENDIF;

       RETURN;

       //=========================================================
       // SubRotina..: ActFPRDP012;
       // Objectivo..: Actualizar o indicador de exitência detalhe
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
          CDSEGUR  = O_CDSEGUR;
          CDFRAC   = O_CDFRAC;
          DSFRAC   = O_DSFRAC;
          PRFRAC   = O_PRFRAC;
          CPSEGURO = O_CPSEGURO;
          IMPREMIO = O_IMPREMIO;
          CDCREDOR = O_CDCREDOR;
          IMCREDOR = O_IMCREDOR;
          DSOBSERV1= O_DSOBSERV1;
          DSOBSERV2= O_DSOBSERV2;
          PRD01701 = O_PRD01701;
          PRD01702 = WEndDate;
          PRD01704 = O_PRD01704;
          WRITE RPRDP017;
       ENDSR;
      /END-FREE
