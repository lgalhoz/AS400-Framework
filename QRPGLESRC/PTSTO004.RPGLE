     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H DECEDIT(',') DFTNAME(GP015) DATEDIT(*DMY.) FIXNBR(*ZONED) AUT(*ALL)
     H*==================================================================
     H* Programa..: PTESO004
     H* Descrição.: Enviar uma mensagem de Erro de Programa
     H*==================================================================
      /Copy QRPGLESRC,SGRLO010

      /FREE
       $sendMessage('ENT0001');
       $sendMessage('ENT0002');

       *INLR = *ON;
       RETURN;
      /END-FREE
