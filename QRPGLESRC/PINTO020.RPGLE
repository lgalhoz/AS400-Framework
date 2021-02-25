     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*===========================================================
     H* Programa..: PINTO020
     H* Descrição.: Seleccionar todos os recibos Standard na fase
     H*             a aguardar pela ligação à Apólice.
     H*
     H*===========================================================
     FFINTP028  IF   E           K DISK    PREFIX(INT_)
     F*===========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010

     D*==> Rotina para Localizar a Apólice
     D FindRecibo      PR                  EXTPGM('RINTO014')
     D   CodRecibo                    9S 0 CONST
     D   CodErro                      7A   CONST
     D
     D*==> Campos de Trabalho
     D WDatSelect      S               Z
     D WDatDia         S               Z
     D WCdRegis        S              9S 0
     D WCodErro        S              7A
     D WCodCompanhia   S              3S 0
     D*===========================================================
      /FREE
        WCodErro  = *BLANKS;
        WDatDia   = %TimeStamp();

        //-----------------------------------------------------------------
        // Actualizar informação das Apólices
        //-----------------------------------------------------------------
        SETLL *LOVAL RINTP028;
        READ RINTP028;
        DOW (NOT %EOF(FINTP028));
           IF (INT_CDESTAD = '1');
              WDatSelect = INT_DTHSTAT + %Hours(6);
           // IF (WdatDia >= WDatSelect);
                 WCdRegis = INT_CDREGIS;
                 WCodErro = *BLANKS;
                 FindRecibo(WCdRegis:WCodErro);
           // ENDIF;
           ENDIF;
           READ RINTP028;
        ENDDO;

        *INLR = *ON;
      /END-FREE
