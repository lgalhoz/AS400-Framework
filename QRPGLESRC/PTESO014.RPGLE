     H AUT(*ALL) ALWNULL(*USRCTL) DFTACTGRP(*NO) ACTGRP(*NEW)
     H BNDDIR('JMDIR')
     H*==================================================================
     H* Programa..:
     H* Descrição.:
     H*
     H*==================================================================
      /COPY QRPGLESRC,SGRLO010
      /COPY QRPGLESRC,SGRLO016

     D*==> Campos de Trabalho
     D WDir            S            100A
     D WDirH           S               *
     D*==================================================================
      /FREE
         WDir = '/sources/controlo/qrpglesrc';
         WDirH = $OpenDir(%TRIMR(WDir));
         p_dirent = $readdir(WDirH);
         dow p_dirent <> *NULL;
            p_dirent = $readdir(WDirH);
         enddo;


       RETURN;
       *INLR = *ON;
      /END-FREE
