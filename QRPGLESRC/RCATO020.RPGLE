     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL) FIXNBR(*ZONED)
     H*==================================================================
     H* Programa..: RCATO020
     H* Descrição.: Pré-visualização dos textos das Coberturas
     H*
     H*==================================================================
     FFCATP100  IF   E           K DISK    PREFIX(CAT_)
     FFCATP102  IF   E           K DISK    PREFIX(CAT_)
     FFCATP10701IF   E           K DISK    PREFIX(CAT_)
     F                                     RENAME(RCATP107:RCATP10701)
     FFCATP117  IF   E           K DISK    PREFIX(CAT_)
     D*==========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SCATO010
      /Copy QRPGLESRC,STBLO010

     D*==================================================================
     D WCdAlias        S                   LIKE(CAT_ALIAS)
     D WCdProd         S              5S 0
     D WCodItem1       S              9S 0
     D WCodItem2       S              9S 0
     D WCodDocum       S              5S 0
     D WCdNatura       S                   LIKE(CAT_CDNATURA) INZ('F')
     D WLinha          S            500A
     D WIcTitulo       S               N
     D WCdCobert       S                   LIKE(CAT_CDCOBERT)
     D
     F*==========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCdProd
     C                   PARM                    WCodItem1
     C                   PARM                    WCodItem2
      /FREE

       WIcTitulo = *OFF;
       EXSR AddTexto; // Formatar as Linhas de Texto

       RETURN;
       RETURN;

       //==================================================================
       // Subrotina..: AddTexto
       // Objectivo..: Adicionar o texto das Coberturas Base e Coberturas
       //              Facultativas.
       //==================================================================
       BEGSR AddTexto;
          EXSR AddTitulo;

          //-----------------------------------------
          // Obter o código do Documento
          //-----------------------------------------
          CHAIN (WCdProd:WCodItem1:WCodItem2) RCATP117;
          IF (%FOUND());
             SETLL (WCdProd:WCodItem1:WCodItem2) RCATP10701;
             READE (WCdProd:WCodItem1:WcodItem2) RCATP10701;
             DOW (NOT %EOF());
                $AddLineType(CAT_CDIDENT);
                $AddTextLine(CAT_TEXTO);
             // $AddTextLine(*BLANKS);
                READE (WCdProd:WCodItem1:WCodItem2) RCATP10701;
             ENDDO;
          ENDIF;

          $AddTextLine(*BLANKS);
       ENDSR;

       //==================================================================
       // Subrotina..: addTitulo
       // Objectivo..: Adicionar o Cabeçalho.
       //==================================================================
       BEGSR AddTitulo;
          CHAIN (WCdProd) RCATP100;
          IF (%FOUND());
             $AddLineType('H1');
             WLinha = 'Ramo ' + %TRIM(CAT_DSPROD);
             $AddTextLine(WLinha);
          ENDIF;
          CHAIN (WCdProd:WCodItem1) RCATP102;
          IF (%FOUND());
             $AddLineType('H1');
             WLinha = 'Cobertura ' + %TRIM(CAT_DSCOBERT);
             $AddTextLine(WLinha);
          ENDIF;
          $AddTextLine(*BLANKS);
       ENDSR;
      /END-FREE
