     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: RPRDO106
     H* Descri��o.: Devolve Taxa de Indexa��o
     H*==================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SCATO010
      /Copy QRPGLESRC,SSECO010

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
     C*========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCodProduto
     C                   PARM                    WDatInicio
     C                   PARM                    WPtrOR
     C                   PARM                    WModo
     C                   PARM                    WResultado
      /FREE
       ptrOr       = WptrOR;
       WResultado = '<falta definir a Taxa>';
       MONITOR;
          IF (OR_TXIND > *ZEROS);
             WResultado = %EDITW(OR_TXIND:' 0, %');
          ENDIF;
       ON-ERROR;
       ENDMON;
       RETURN;
      /END-FREE
