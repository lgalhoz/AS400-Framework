     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL) FIXNBR(*ZONED)
     H*==================================================================
     H* Programa..: RPRDT012
     H* Descrição.: Gerir Operações IO ao ficheiro de Texto das Notas Descritivas
     H*             com Histórico inteligente.
     H*             Actualizar também Capitais dos nós Superiores
     H*
     H* Observação: Implicações com o ficheiro de Capitais (FPRDP012)
     H*             Actualizar os níveis superiores quando alterar o
     H*             Capital, com gestão automática da Informação de
     H*             Histórico.
     H*==================================================================
     FFPRDP012  UF A E           K DISK    COMMIT PREFIX(EXT_)
     F*==================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SGRLO012
      /Copy QRPGLESRC,SDBSO010

     D*-------------------------------------------------
     D* Formato de Registo
     D*-------------------------------------------------
     D OldRecord     E DS                  ExtName(FPRDP012)
     D                                     Prefix(OLD_)
     D                                     Based(OldRecPtr)
     D NewRecord     E DS                  ExtName(FPRDP012)
     D                                     Prefix(NEW_)
     D                                     Based(NewRecPtr)
     D
     D WEndDate        S                   LIKE(EXT_PRD01202)
     D WNuSqHst        S                   LIKE(EXT_NUSEQHST)
     D WDifCapital     S                   LIKE(NEW_CPSEGURO)
     D WDBSession      S              5S 0
     D WExtNuSequ      S              5S 0
     D WExtNuSqHst     S              5S 0
     D WCodRamo        S              2S 0
     D WNumApol        S              7S 0
     D WNumSequ        S              5S 0
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
       WDifCapital= *ZEROS;

       //-----------------------------------------------------
       // Formatar registo com valores por defeito
       //-----------------------------------------------------
       IF (TTrgEvent = IO#INS);
          NEW_NUSEQHST  = $GetNextKey('FPRDP012':'NUSEQHST':'(CDRAMO='''+
                        %TRIM(%CHAR(NEW_CDRAMO)) + ''') AND (NUAPOL='''+
                        %TRIM(%CHAR(NEW_NUAPOL)) + ''') AND (NUSEQU='''+
                        %TRIM(%CHAR(NEW_NUSEQU)) + ''')');
          IF (NEW_PRD01201 = *LOVAL);
             NEW_PRD01201 = $Today();
          ENDIF;
          IF (NEW_PRD01202 = *LOVAL);
            NEW_PRD01202 = %DATE('9999-12-31':*ISO);
          ENDIF;
          IF (NEW_PRD01201 > NEW_PRD01202);
             NEW_PRD01201 = NEW_PRD01202;
          ENDIF;
          IF (NEW_PRD01202 <= $Today());
             NEW_PRD01203 = *ZEROS;
          ELSE;
             NEW_PRD01203 = 1;
             WDifCapital= NEW_CPSEGURO;
          ENDIF;
          IF (NEW_PRD01204 = *BLANKS);
             NEW_PRD01204 = S#USR;
          ENDIF;
          NEW_PRD01205 = S#USR;
       ELSE;
          //-----------------------------------------------------
          // Apenas alterar Registos Activas
          //-----------------------------------------------------
          IF (OLD_PRD01201 < $Today()) AND (OLD_PRD01202 > $Today());
             IF (TTrgEvent = IO#UPD);
                //------------------------------------------------
                // Guardar a imagem do registo antes da alteração
                //------------------------------------------------
                WDifCapital= NEW_CPSEGURO - OLD_CPSEGURO;
                WNuSqHst = *ZEROS;
                WEndDate = $Today();
                EXSR CrtRegisto;

                //------------------------------------------------
                // Alterar o Registo
                //------------------------------------------------
                NEW_CDRAMO   = OLD_CDRAMO;
                NEW_NUAPOL   = OLD_NUAPOL;
                NEW_NUSEQU   = OLD_NUSEQU;
                NEW_NUSEQHST = OLD_NUSEQHST;
                NEW_PRD01201 = $Today();
                NEW_PRD01202 = OLD_PRD01202;
                NEW_PRD01204 = OLD_PRD01204;
                NEW_PRD01205 = S#USR;
             ELSEIF (TTrgEvent = IO#DEL);
                //-----------------------------------------------------
                // Recriar o registo Eliminado forçando uma data Termo
                //-----------------------------------------------------
                WDifCapital = OLD_CPSEGURO;
                WNuSqHst    = OLD_NUSEQHST;
                WEndDate    = $Today();
                EXSR CrtRegisto;
             ENDIF;
          ELSEIF (OLD_PRD01201 < $Today());
             //-----------------------------------------------------
             // Repor os dados Registos se Histórico ou Futuro
             //-----------------------------------------------------
             IF (TTrgEvent = IO#UPD);
                IN *LOCK LDA#;
                L#CODERR = 'IOE0001';
                OUT LDA#;
                UNLOCK LDA#;

                *INH1 = *ON;
                RETURN;
             ELSEIF (TTrgEvent = IO#DEL);
                WNuSqHst = OLD_NUSEQHST;
                WEndDate = OLD_PRD01202;
                EXSR CrtRegisto;
             ENDIF;
          ELSEIF (TTrgEvent = IO#DEL);
             WDifCapital = OLD_CPSEGURO;
          ELSEIF (TTrgEvent = IO#UPD);
             WDifCapital = NEW_CPSEGURO - OLD_CPSEGURO;
          ENDIF;
       ENDIF;

       IF (WDifCapital <> *ZEROS);
          EXSR UpdSuperior;
       ENDIF;

       RETURN;

       //=========================================================
       // SubRotina..: UpdSuperior
       // Objectivo..: Actualizar nós Superiores
       //=========================================================
       BEGSR UpdSuperior;
          //-------------------------------------------------------------
          // Formatar Chaves do Registo
          //-------------------------------------------------------------
          IF (TTrgEvent = IO#INS) OR (TTrgEvent = IO#UPD);
             WCodRamo  = NEW_CDRAMO;
             WNumApol  = NEW_NUAPOL;
             WNumSequ  = NEW_NUSEQU;
          ELSEIF (TTrgEvent = IO#DEL);
             WCodRamo  = OLD_CDRAMO;
             WNumApol  = OLD_NUAPOL;
             WNumSequ  = OLD_NUSEQU;
          ENDIF;
          WExtNuSequ = *ZEROS;

          //----------------------------------------------------
          // Obter o nível Superior
          //----------------------------------------------------
          WDBSession = $Setfiles('FPRDP010');
          $SetCriteria('(CDRAMO = '''+%CHAR(WCodRamo)+''') AND +
                        (NUAPOL = '''+%CHAR(WNumApol)+''') AND +
                        (NUSEQU = '''+%CHAR(WNumSequ)+''')');
          $AddDBDate($Today());
          IF ($GetRecord(1) > *ZEROS);
             WExtNusequ = $GetFieldNum('NUSQSUP');
          ENDIF;
          $RmvDBSession(WDBSession);

          //----------------------------------------------------
          // Actualizar nível Superior
          //----------------------------------------------------
          IF (WExtNuSequ > *ZEROS) AND (WextNuSequ <> WNumSequ);
             WDBSession = $Setfiles('FPRDP012');
             $SetCriteria('(CDRAMO = '''+%CHAR(WCodRamo)+''') AND +
                           (NUAPOL = '''+%CHAR(WNumApol)+''') AND +
                           (NUSEQU = '''+%CHAR(WExtNuSequ)+''')');
             $AddDBDate($Today());
             IF ($GetRecord(1) > *ZEROS);
                WExtNusequ  = $GetFieldNum('NUSEQU');
                WExtNuSqHst = $GetFieldNum('NUSEQHST');
             ENDIF;
             $RmvDBSession(WDBSession);

             IF (WExtNuSequ > *ZEROS) AND (WextNuSequ <> WNumSequ);
                CHAIN (WCodRamo:WNumApol:WExtNusequ:WExtNuSqHst) RPRDP012;
                IF (%FOUND(FPRDP012));
                   EXT_CPSEGURO += WDifCapital;
                   UPDATE RPRDP012 %FIELDS(EXT_CPSEGURO);
                ENDIF;
             ENDIF;
          ENDIF;
       ENDSR;

       //=========================================================
       // SubRotina..: crtRegisto
       // Objectivo..: Criar o registo Eliminado
       //=========================================================
       BEGSR crtRegisto;
          EXT_CDRAMO   = OLD_CDRAMO;
          EXT_NUAPOL   = OLD_NUAPOL;
          EXT_NUSEQU   = OLD_NUSEQU;
          EXT_NUSEQHST = WNuSqHst;
          EXT_CPSEGURO = OLD_CPSEGURO;
          EXT_DSDETAIL = OLD_DSDETAIL;
          EXT_ICDETAIL = OLD_ICDETAIL;
          EXT_PRD01201 = OLD_PRD01201;
          EXT_PRD01202 = WEndDate;
          EXT_PRD01204 = OLD_PRD01204;
          WRITE RPRDP012;
       ENDSR;
      /END-FREE
