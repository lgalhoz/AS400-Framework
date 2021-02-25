     H OPTION(*DEBUGIO:*SRCSTMT)
     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL) FIXNBR(*ZONED)
     H*==================================================================
     H* Programa..: RPRDT010
     H* Descrição.: Gerir Operações IO ao ficheiro de Notas Descritivas
     H*             com Histórico inteligente.
     H*
     H* Observação: Implicações com o ficheiro de Capitais (FPRDP012)
     H*             Por cada nó obriga à existência de um registo de
     H*             Capitais, para acumulação dos totais finais ou
     H*             parciais.
     H*==================================================================
     FFPRDP012  UF A E           K DISK    COMMIT PREFIX(EXT_)
     FFPRDP010  UF A E           K DISK    COMMIT PREFIX(EXT_)
     F*==================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SGRLO012
      /Copy QRPGLESRC,SDBSO010

     D*-------------------------------------------------
     D* Formato de Registo
     D*-------------------------------------------------
     D OldRecord     E DS                  ExtName(FPRDP010)
     D                                     Prefix(OLD_)
     D                                     Based(OldRecPtr)
     D NewRecord     E DS                  ExtName(FPRDP010)
     D                                     Prefix(NEW_)
     D                                     Based(NewRecPtr)
     D
     D*==> Campos de Trabalho
     D WEndDate        S               D
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

       //-----------------------------------------------------
       // ForCompr registo com valores por defeito
       //-----------------------------------------------------
       IF (TTrgEvent = IO#INS);
          IF (NEW_NUSEQU = *ZEROS);
             NEW_NUSEQU  = $GetNextKey('FPRDP010':'NUSEQU':'(CDRAMO='''+
                         %TRIM(%CHAR(NEW_CDRAMO)) + ''') AND (NUAPOL='''+
                         %TRIM(%CHAR(NEW_NUAPOL)) + ''')');
          ENDIF;
          IF (NEW_NUSQSUP = *ZEROS);
             NEW_NUSQSUP = NEW_NUSEQU;
          ENDIF;
          IF (NEW_PRD01001 = *LOVAL);
             NEW_PRD01001 = $Today();
          ENDIF;
          IF (NEW_PRD01002 = *LOVAL);
             NEW_PRD01002 = %DATE('9999-12-31':*ISO);
          ENDIF;
          IF (NEW_PRD01001 > NEW_PRD01002);
             NEW_PRD01001 = NEW_PRD01002;
          ENDIF;
          IF (NEW_PRD01002 <= $Today());
             NEW_PRD01003 = *ZEROS;
          ELSE;
             NEW_PRD01003 = 1;
          ENDIF;
          IF (NEW_PRD01004 = *BLANKS);
             NEW_PRD01004 = S#USR;
          ENDIF;
          NEW_PRD01005 = S#USR;
          EXSR CrtCapitais;
       ELSE;
          //-----------------------------------------------------
          // Apenas alterar Registos Activas
          //-----------------------------------------------------
          IF (OLD_PRD01001 < $Today()) AND (OLD_PRD01002 > $Today());
             IF (TTrgEvent = IO#UPD);
                *INH1 = *ON;
                RETURN;
             ELSEIF (TTrgEvent = IO#DEL);
                //-----------------------------------------------------
                // Recriar o registo Eliminado forçando uma data Termo
                //-----------------------------------------------------
                WEndDate = $Today();
                EXSR DelCapitais;
                EXSR crtDelRecord;
             ENDIF;
          ELSEIF (OLD_PRD01001 < $Today());
             //-----------------------------------------------------
             // Repor os dados Registos se Histórico ou Futuro
             //-----------------------------------------------------
             IF (TTrgEvent = IO#UPD);
                *INH1 = *ON;
                RETURN;
             ELSEIF (TTrgEvent = IO#DEL);
                WEndDate = OLD_PRD01002;
                EXSR DelCapitais;
                EXSR crtDelRecord;
             ENDIF;
          ENDIF;
       ENDIF;

       RETURN;

       //=========================================================
       // SubRotina..: CrtCapitais
       // Objectivo..: Criar o registode Capitais Correspondente
       //=========================================================
       BEGSR CrtCapitais;
          IF (TTrgEvent = IO#INS);
          ENDIF;
       ENDSR;

       //=========================================================
       // SubRotina..: DelCapitais
       // Objectivo..: Eliminar os registos de Capitais
       //=========================================================
       BEGSR CrtCapitais;
          IF (TTrgEvent = IO#INS);
          ENDIF;
       ENDSR;

       //=========================================================
       // SubRotina..: crtDelRecord
       // Objectivo..: Criar o registo Eliminado
       //=========================================================
       BEGSR crtDelRecord;
          EXT_CDRAMO   = OLD_CDRAMO;
          EXT_NUAPOL   = OLD_NUAPOL;
          EXT_NUSEQU   = OLD_NUSEQU;
          EXT_NUSQSUP  = OLD_NUSQSUP;
          EXT_CDTEXTO  = OLD_CDTEXTO;
          EXT_PRD01001 = OLD_PRD01001;
          EXT_PRD01002 = WEndDate;
          EXT_PRD01004 = OLD_PRD01004;
          WRITE RPRDP010;
       ENDSR;
      /END-FREE
