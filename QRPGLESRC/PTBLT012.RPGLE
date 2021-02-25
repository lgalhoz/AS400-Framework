     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*=======================================================
     H* Dat.Cria...: 2011-05-12
     H* Autor......: Luis Galhoz
     H* Processo...: runPopUp
     H* Objectivo..: Executar uma Pop-Up, para seleccionar
     H*              valores possíveis Tabela de Códigos.
     H*=======================================================
      /Copy QRPGLESRC,STBLO010

     D WRetCode        S             15A
     D WCodPost        S              8S 0
     D*=====================================================================
      /FREE
           WRetCode = $runPopUp(57:'a');
           WCodPost = $RunCPPopUp(*ZEROS);
           WRetCode = 'Teste';

           *INLR = *ON;
           RETURN;
      /END-FREE
