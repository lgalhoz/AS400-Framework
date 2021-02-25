     H DFTACTGRP(*NO) BNDDIR('JMDIR')
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SSECO012
      /FREE
           $getHTTPmsg('HTP0200':'OK');
         RETURN;
      /END-FREE
