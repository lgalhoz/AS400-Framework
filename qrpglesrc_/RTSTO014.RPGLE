     H*BNDDIR('JMDIR')
     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
      *=======================================================
      * Dat.Cria...: 2012-10-23
      * Objectivo..: Devolver o Header da função, junto com as
      *              teclas a que o utilizador está autorizado.
      *=======================================================
     D*/Copy QRPGLESRC,SSECO010
     D
     D cRand           PR            10i 0 ExtProc('random')
     D
     D add4            PR            10i 0 ExtProc('addInt')
     D  Parm01                       10i 0 VALUE
     D
     D add5            PR              *   ExtProc('strChange')
     D  Parm01                         *   VALUE OPTIONS(*String)
     D
     D WResult         S             10i 0
     D WResultC        S             10A
      *=======================================================
      /FREE

         WResult  = 6;
         WResult  = Add4(WResult);
         WResultc = %STR(Add5('Test'));
         WResult  = cRand();
         WResult  = cRand();

         *INLR = *ON;
         RETURN;
      /END-FREE
