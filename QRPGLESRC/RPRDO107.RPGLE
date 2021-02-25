     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: RPRDO107
     H* Descrição.: Devolve Credor Hipotecário
     H*==================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SENTO010

     D*==> Estrutura de dados do Registo
     D RBASEDIO      E DS                  ExtName(GBASEDIO)
     D                                     Prefix(OR_)
     D                                     Based(ptrOR)
     D
     D*==> Campos de Trabalho
     D WCodProduto     S              5S 0
     D WDatInicio      S              8S 0
     D WPtrOR          S               *
     D WModo           S               N
     D WResultado      S            100A
     D WBullet         S              3A
     D WCodErro        S              7A
     C*========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCodProduto
     C                   PARM                    WDatInicio
     C                   PARM                    WPtrOR
     C                   PARM                    WModo
     C                   PARM                    WResultado
     C                   PARM                    WBullet
     C                   PARM                    WCodErro
      /FREE
       ptrOr      = WptrOR;
       WResultado = ATR#Blink+'<Falta definir o Credor>';
       WCodErro   = 'PRD0204';
       MONITOR;
          IF (OR_CRHIP > *ZEROS);
             WResultado = $getDescCred(OR_CRHIP);
             WCodErro   = *BLANKS;
          ENDIF;
       ON-ERROR;
       ENDMON;

       RETURN;
      /END-FREE
