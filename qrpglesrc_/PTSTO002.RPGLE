     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: TSECO010
     H* Descrição.: Testar PopUp de componentes
     H*==================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SSECO010

     D WCdItem         S              5S 0
      /FREE
        WCDITEM = $RunPopUpComp(WCDITEM);
        *INLR = *ON;
      /END-FREE
