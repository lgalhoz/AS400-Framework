     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: RENTO015
     H* Descrição.: Validar NIF, Penhoras
     H*==================================================================
      /Copy QRPGLESRC,SENTO010

     D*==> Campos de Trabalho
     D WNumNIF         S              9S 0
     D WTipNIF         S              1A
     D WMsgErro        S              7A
     D WIcErro         S               N
     D*==================================================================
     C     *ENTRY        PLIST
     C                   PARM                    WNumNIF
     C                   PARM                    WTipNIF
     C                   PARM                    WMsgErro
      /FREE
        WIcErro = $CheckNIF(WNumNIF);
        IF (WIcErro = *ON);
           WMsgErro = *BLANKS;
        ELSE;
           WMsgErro = 'MSG0000';
        ENDIF;
        RETURN;
      /END-FREE
