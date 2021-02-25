     H*PGMINFO(*PCML:*MODULE)
     H DFTACTGRP(*NO)
     D
     D $getUserName    PR            10i 0
     D  PCodUser                     10A   Const
     D
     D WVar01          S             10A
     D*==================================================================
     C     *ENTRY        PLIST
     C                   PARM                    WVar01
     C
      /FREE
          WVar01 = 'O que vai apa';
         *INLR = *ON;
      /END-FREE
     P*=======================================================
     P* Processo..: $getUserName
     P* Descrição.: Obter Informação sobre o Utilizador
     P*=======================================================
     P $getUserName    B                   EXPORT
     D                 PI            10I 0
     D  PCodUser                     10A   Const
     D WUserName       S            100A
     D WResult         S             10i 0
     C*==================================================================
      /FREE
        WUserName = 'Nome do Utilizador';

        RETURN 10;
      /END-FREE
     P $getUserName    E
