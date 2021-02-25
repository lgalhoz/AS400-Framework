     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*=======================================================
     H* Dat.Cria...: 2011-05-12
     H* Autor......: Luis Galhoz
     H* Processo...: runPopUp
     H* Objectivo..: Executar uma Pop-Up, para seleccionar
     H*              valores possíveis Tabela de Códigos.
     H*=======================================================
      /Copy QRPGLESRC,SENTO010

     D WCodEntid       S              8S 0
     D WNomCredi       S             40A
     D*=====================================================================
      /FREE
           WCodEntid = $runEntPopUp(*ZEROS);
           WCodEntid = $RunCRedPopUp(*ZEROS);

           *INLR = *ON;
           RETURN;
      /END-FREE
