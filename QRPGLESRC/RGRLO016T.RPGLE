     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*===========================================================
      /Copy QRPGLESRC,SGRLO010


     D*==> Campos de Trabalho
     D WMsgError       S              7A
     D WNomFile        S             20A
     D*===========================================================
      /FREE
        // WMsgError = $SendFTP(8:'QTEMP':'TESTE':'indice.txt');
        // IF (WMsgError <> *BLANKS);
        //   $ShowDialog(WMsgError);
        // ENDIF;

        WMsgError = $GetFTP(5:'9902911.csv':'QTEMP':'TESTE');
        IF (WMsgError <> *BLANKS);
           $ShowDialog(WMsgError);
        ENDIF;

        WMsgError = $InzFTPList(5);
        IF (WMsgError <> *BLANKS);
           $ShowDialog(WMsgError);
        ELSE;
           WNomFile = $GetFTPFirst();
           DOW (NOT $IsFTPEof());
              WNomFile = $GetFTPNext();
           ENDDO;
        ENDIF;

        *INLR = *ON;
      /END-FREE
