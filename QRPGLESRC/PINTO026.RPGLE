     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*===========================================================
     H* Programa..: PINTO022
     H* Descrição.: Seleccionar todos os recibos Standard na fase
     H*             a aguardar pela ligação aos Recibos.
     H*
     H* (Sit.4 => Sit.5)
     H*
     H*===========================================================
     FFINTP028  IF   E           K DISK    PREFIX(INT_)
     F*===========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010

     D*==> Rotina para Localizar a Apólice
     D FindApolice     PR                  EXTPGM('PCOBO201')
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
        // Actualizar informação das Apólices
        //-----------------------------------------------------------------
        SETLL *LOVAL RINTP028;
        READ RINTP028;
        DOW (NOT %EOF(FINTP028));
           IF (INT_CDESTAD = '4');
              WCdRegis = INT_CDREGIS;
              WCodErro = *BLANKS;
              FindApolice(WCdRegis:WCodErro);
           ENDIF;
           READ RINTP028;
        ENDDO;

        *INLR = *ON;
      /END-FREE
