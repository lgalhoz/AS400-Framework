     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*===========================================================
     H* Programa..: PINTO022
     H* Descri��o.: Seleccionar todos os recibos Standard na fase
     H*             a aguardar pela liga��o aos Recibos.
     H*
     H*===========================================================
     FFINTP028  IF   E           K DISK    PREFIX(INT_)
     F*===========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010

     D*==> Rotina para Localizar a Ap�lice
     D FindApolice     PR                  EXTPGM('RINTO015')
     D   CodRecibo                    9S 0 CONST
     D   CodErro                      7A   CONST
     D
     D*==> Campos de Trabalho
     D WCdRegis        S              9S 0
     D WCodErro        S              7A
     D WCodCompanhia   S              3S 0
     D*===========================================================
      /FREE
        WCodErro  = *BLANKS;

        //-----------------------------------------------------------------
        // Actualizar informa��o das Ap�lices
        //-----------------------------------------------------------------
        SETLL *LOVAL RINTP028;
        READ RINTP028;
        DOW (NOT %EOF(FINTP028));
           IF (INT_CDESTAD = '2') OR (INT_CDESTAD = '3');
              WCdRegis = INT_CDREGIS;
              WCodErro = *BLANKS;
              FindApolice(WCdRegis:WCodErro);
           ENDIF;
           READ RINTP028;
        ENDDO;

        *INLR = *ON;
      /END-FREE
