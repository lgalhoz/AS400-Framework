     H DFTACTGRP(*NO) BNDDIR('JMDIR')
     H*==============================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SSECO012

     D*==> Campos de Trabalho
     D WTemplate       S              5S 0
     D WCodErro        S              7A
     D WFileName       S             40A
     D WPcName         S             40A
     D*==============================================================
      /FREE
       WTemplate = 1;
       WCodErro = $InzFTPList(WTemplate);
       IF (WCodErro <> *BLANKS);
          $showDialog(WCodErro);
       ELSE;
          WFileName = $GetFTPFirst();
          DOW (NOT  $IsFTPEof);
             WFileName = $GetFTPNext();
          ENDDO;

          WPcName  = 'teste.txt';
          WCodErro = $SendFTP(WTemplate:'QTEMP':'JRNSTS':%TRIM(WPcName));
          IF (WCodErro <> *BLANKS);
             $showDialog(WCodErro);
          ENDIF;
       ENDIF;
       RETURN;
      /END-FREE
