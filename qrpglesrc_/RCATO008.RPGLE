     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: RCATO008
     H* Descrição.: Pré-Visualizar textos da Apólice
     H*
     H*==================================================================
     FFCATP100  IF   E           K DISK    PREFIX(CAT_)
     FFCATP116  IF   E           K DISK    PREFIX(CAT_)
     F*===================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SSECO010

     D*==> Chamada de função Dynamic, para trabalhar detalhe do Item
     D RetrText        PR                  EXTPGM(DynamicCall)
     D   CdProd                       5S 0 CONST
     D   CodItem1                     9S 0 CONST Options(*NOPASS)
     D   CodItem2                     9S 0 CONST Options(*NOPASS)
     D
     D*==> Lista de Campos
     D DynamicCall     S             10A   INZ(*BLANKS)
     D WComando        S            300A   INZ(*BLANKS)
     D WCodPagina      S              5S 0
     D WCodProduto     S              5S 0
     D WTipItem        S              2A
     D WCodItem1       S              9S 0
     D WCodItem2       S              9S 0
     D WLargura        S              3S 0
     D
     D*===================================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCodPagina
     C                   PARM                    WTipItem
     C                   PARM                    WCodProduto
     C                   PARM                    WCodItem1
     C                   PARM                    WCodItem2
     C                   PARM                    #RetTexto
     C                   PARM                    WLargura
      /FREE
       CHAIN (WTipItem) RCATP116;
       IF (%FOUND());
          $InzLista(WCodPagina);
          Chain (WCodProduto) RCATP100;
          IF (%FOUND());
                MONITOR;
                   DynamicCall = $GetFunction(CAT_CDFUNCT);
                   RetrText(WCodProduto:WCodItem1:WCodItem2);
                ON-ERROR;
                ENDMON;
             ENDIF;
          #RetTexto = $ProcessTexto('T':WLargura);
       ENDIF;

       *INLR = *ON;
       RETURN;
      /END-FREE
