     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL) FIXNBR(*ZONED)
     H*==================================================================
     H* Programa..: RPRDT012
     H* Descrição.: Testar as operações de Commit e Rollback com Triggers
     H*==================================================================
     FFPRDP010  UF A E           K DISK    COMMIT PREFIX(EXT_)
     FFPRDP012  UF A E           K DISK    COMMIT PREFIX(EXT_)
     F*==================================================================
      /Copy QRPGLESRC,SGRLO010

     D WCodRamo        S              2S 0
     D WNumApol        S              7S 0
     D WNumSequ        S              5S 0
     D WNumHist        S              1S 0
     D*==================================================================
      /FREE
       WCodRamo = 5;
       WNumApol = 9910401;
       WNumSequ = 10;
       WNumHist = 1;

       EXSR DelRegisto;

       // EXSR crtRegisto;
       // EXSR ChgRegisto;
       // DSPLY 'Esperar....';
       // ROLBK;
       RETURN;

       //=========================================================
       // SubRotina..: delRegisto
       // Objectivo..: Eliminar o Registo
       //=========================================================
       BEGSR delRegisto;
          CHAIN (WCodRamo:WNumApol:WNumSequ) RPRDP010;
          IF (%FOUND(FPRDP010));
             DELETE FPRDP010;
          ENDIF;
       ENDSR;

       //=========================================================
       // SubRotina..: chgRegisto
       // Objectivo..: Alterar o Registo
       //=========================================================
       BEGSR chgRegisto;
          CHAIN (WCodRamo:WNumApol:WNumSequ:WNumHist) RPRDP012;
          IF (%FOUND(FPRDP012));
             EXT_CPSEGURO += 25000;
             UPDATE RPRDP012 %FIELDS(EXT_CPSEGURO);
          ENDIF;

          WNumHist = 2;
          CHAIN (WCodRamo:WNumApol:WNumSequ:WNumHist) RPRDP012;
          IF (%FOUND(FPRDP012));
             EXT_CPSEGURO += 25000;
             MONITOR;
                UPDATE RPRDP012 %FIELDS(EXT_CPSEGURO);
             ON-ERROR;
                IF (S#MSGID = 'RNX1023') OR (S#MSGID = 'RNX1024');
                   IN *LOCK LDA#;
                   S#MSGID = %SUBST(L#CODERR:1:7);
                   UNLOCK LDA#;
                ENDIF;
                DSPLY S#MSGID;
             ENDMON;
          ENDIF;
       ENDSR;

       //=========================================================
       // SubRotina..: crtRegisto
       // Objectivo..: Criar o registo
       //=========================================================
       BEGSR crtRegisto;
          EXT_CDRAMO   = WCodRamo;
          EXT_NUAPOL   = WNumApol;
          EXT_NUSEQU   = WNumSequ;
          EXT_NUSEQHST = *ZEROS;
          EXT_CPSEGURO = 1000000;
          EXT_DSDETAIL = 'Teste';
          EXT_ICDETAIL = 'N';
          EXT_PRD01201 = %DATE('2001-01-01');
          EXT_PRD01202 = %DATE('2999-12-31');
          EXT_PRD01204 = 'LGALHOZ';
          WRITE RPRDP012;
       ENDSR;
      /END-FREE
