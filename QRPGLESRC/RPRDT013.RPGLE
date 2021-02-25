     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL) FIXNBR(*ZONED)
     H*==================================================================
     H* Programa..: RPRDT013
     H* Descrição.: Gerir Operações IO ao ficheiro de Detalhe Habitação (ND)
     H*             com Histórico inteligente.
     H*
     H* Observação: Implicações com o ficheiro de Capitais (FPRDP012)
     H*             Criar    --> Indicador com valor (Detalhe = Sim)
     H*             Eliminar --> Indicador com valor (Detalhe = Não)
     H*==================================================================
     FFPRDP012  UF   E           K DISK    COMMIT PREFIX(EXT_)
     FFPRDP013  UF A E           K DISK    COMMIT PREFIX(EXT_)
     F*==================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SGRLO012
      /Copy QRPGLESRC,SDBSO010

     D*-------------------------------------------------
     D* Formato de Registo
     D*-------------------------------------------------
     D OldRecord     E DS                  ExtName(FPRDP013)
     D                                     Prefix(OLD_)
     D                                     Based(OldRecPtr)
     D NewRecord     E DS                  ExtName(FPRDP013)
     D                                     Prefix(NEW_)
     D                                     Based(NewRecPtr)
     D
     D WEndDate        S                   LIKE(EXT_PRD01302)
     D WNuSeqHst       S                   LIKE(EXT_NUSEQHST)
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
       // Formatar registo com valores por defeito
       //-----------------------------------------------------
       IF (TTrgEvent = IO#INS);
          NEW_NUSEQHST  = $GetNextKey('FPRDP013':'NUSEQHST':'(CDRAMO='''+
                        %TRIM(%CHAR(NEW_CDRAMO)) + ''') AND (NUAPOL='''+
                        %TRIM(%CHAR(NEW_NUAPOL)) + ''') AND (NUSEQU='''+
                        %TRIM(%CHAR(NEW_NUSEQU)) + ''')');
          IF (NEW_PRD01301 = *LOVAL);
             NEW_PRD01301 = $Today();
          ENDIF;
          IF (NEW_PRD01302 = *LOVAL);
             NEW_PRD01302 = %DATE('9999-12-31':*ISO);
          ENDIF;
          IF (NEW_PRD01301 > NEW_PRD01302);
             NEW_PRD01301 = NEW_PRD01302;
          ENDIF;
          IF (NEW_PRD01302 <= $Today());
             NEW_PRD01303 = *ZEROS;
          ELSE;
             NEW_PRD01303 = 1;
          ENDIF;
          IF (NEW_PRD01304 = *BLANKS);
             NEW_PRD01304 = S#USR;
          ENDIF;
          NEW_PRD01305 = S#USR;

          EXSR ActFPRDP012;
       ELSE;
          //-----------------------------------------------------
          // ###############   Registos Activos   ###############
          //-----------------------------------------------------
          IF (OLD_PRD01301 < $Today()) AND (OLD_PRD01302 > $Today());
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
                NEW_CDRAMO   = OLD_CDRAMO;
                NEW_NUAPOL   = OLD_NUAPOL;
                NEW_NUSEQU   = OLD_NUSEQU;
                NEW_NUSEQHST = OLD_NUSEQHST;
                NEW_PRD01301 = $Today();
                NEW_PRD01302 = OLD_PRD01302;
                NEW_PRD01304 = OLD_PRD01304;
                NEW_PRD01305 = S#USR;
             ELSEIF (TTrgEvent = IO#DEL);
                //-----------------------------------------------------
                // Recriar o registo Eliminado forçando uma data Termo
                //-----------------------------------------------------
                WEndDate  = $Today();
                WNuSeqHst = OLD_NUSEQHST;
                EXSR CrtRecord;
                EXSR ActFPRDP012;
             ENDIF;
          ELSEIF (OLD_PRD01301 < $Today());
             //-----------------------------------------------------
             // Repor os dados Registos se Histórico ou Futuro
             //-----------------------------------------------------
             IF (TTrgEvent = IO#UPD);
                *INH1 = *ON;
                RETURN;
             ELSEIF (TTrgEvent = IO#DEL);
                WEndDate = OLD_PRD01302;
                WNuSeqHst = OLD_NUSEQHST;
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
             WCodRamo  = NEW_CDRAMO;
             WNumApol  = NEW_NUAPOL;
             WNumSequ  = NEW_NUSEQU;
             WIcDetail = 'S';
          ELSEIF (TTrgEvent = IO#DEL);
             WCodRamo  = OLD_CDRAMO;
             WNumApol  = OLD_NUAPOL;
             WNumSequ  = OLD_NUSEQU;
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
          EXT_CDRAMO   = OLD_CDRAMO;
          EXT_NUAPOL   = OLD_NUAPOL;
          EXT_NUSEQU   = OLD_NUSEQU;
          EXT_NUSEQHST = WNuSeqHst;
          EXT_CDSEGUR  = OLD_CDSEGUR;
          EXT_MORADA   = OLD_MORADA;
          EXT_CDPOSTAL = OLD_CDPOSTAL;
          EXT_LOCALIDADE=OLD_LOCALIDADE;
          EXT_CDCREDOR = OLD_CDCREDOR;
          EXT_DSOBSERV1= OLD_DSOBSERV1;
          EXT_DSOBSERV2= OLD_DSOBSERV2;
          EXT_PRD01301 = OLD_PRD01301;
          EXT_PRD01302 = WEndDate;
          EXT_PRD01304 = OLD_PRD01304;
          WRITE RPRDP013;
       ENDSR;
      /END-FREE
