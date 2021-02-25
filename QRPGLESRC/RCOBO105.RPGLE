     H AUT(*ALL) ALWNULL(*USRCTL) DFTACTGRP(*NO) ACTGRP(*CALLER)
     H BNDDIR('JMDIR')
     H*==================================================================
     H* Programa..: RCOBO105
     H* Descrição.: Anular Geração de Ficheiro XML/TXT
     H*==================================================================
     FTBIDDPL3  UF   E           K DISK
     FTBIDDEF   UF   E           K DISK
     FTBIDDFF   UF   E           K DISK
     F*==================================================================
      /COPY QRPGLESRC,SGRLO010
      /COPY QRPGLESRC,SSECO010
      /COPY QRPGLESRC,SDBSO010

     D*==> Campos de Trabalho
     D  WCodFileAct    S             10P 0
     D*==================================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCodFileAct
      /FREE
       //------------------------------------------------
       // Eliminar ficheiro de Header
       //------------------------------------------------
       CHAIN (WCodFileAct) TBIDDEF;
       IF (%FOUND());
          IF (TEDATR > *ZEROS);
             $ShowDialog('COB0018');
             RETURN;
          ENDIF;
          DELETE TBIDDE;
       ENDIF;

       //------------------------------------------------
       // Eliminar registos de Detalhe
       //------------------------------------------------
       SETLL (WCodFileAct) TBIDDF;
       READE (WCodFileAct) TBIDDF;
       DOW (NOT %EOF(TBIDDFF));
          DELETE TBIDDF;
          READE (WCodFileAct) TBIDDF;
       ENDDO;

       //------------------------------------------------
       // Repor Intruções de Débito
       //------------------------------------------------
       SETLL (WCodFileAct) TBIDDP;
       READE (WCodFileAct) TBIDDP;
       DOW (NOT %EOF(TBIDDPL3));
          IDSIT  = '1';
          IDUSER = *BLANKS;
          IDDTAP = *ZEROS;
          IDFENV = *ZEROS;
          UPDATE TBIDDP %FIELDS(IDSIT:IDUSER:IDDTAP:IDFENV);
          READE (WCodFileAct) TBIDDP;
       ENDDO;
       RETURN;
      /END-FREE
