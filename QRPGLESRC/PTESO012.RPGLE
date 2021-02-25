     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL) FIXNBR(*ZONED)
     H*==================================================================
     H* Programa..: PTESO012
     H* Descrição.: TestarChamda activa de Programas
     H*==================================================================
     FFPRDP010  UF A E           K DISK    COMMIT PREFIX(EXT_)
     FFPRDP012  UF A E           K DISK    COMMIT PREFIX(EXT_)
     F*==================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SGRLO012

     D*==> Campos de Trabalho
     D WNomProgr       S             10A
     D*==================================================================
      /FREE
         WNomProgr = $getAppCaller('PTESO012');

       RETURN;
      /END-FREE
