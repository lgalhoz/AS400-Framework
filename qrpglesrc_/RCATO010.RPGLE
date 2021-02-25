     H*DFTACTGRP(*NO)
     F*=======================================================
     F* Dat.Cria...: 2012-04-26
     F* Autor......: Luis Galhoz
     F* Objectivo..: Funções de Catálogo de Produtos
     F*=======================================================
     FFCATP100  IF   E           K DISK    PREFIX(CAT_)
     FFGRLP010  IF   E           K DISK    PREFIX(GRL_)
     FFCATP101  IF   E           K DISK    PREFIX(CAT_)
     FFCATP10001IF   E           K DISK    RENAME(RCATP100:RCATP10001)
     F                                     PREFIX(CAT_)
     D*=======================================================
      /Copy QRPGLESRC,SCATO010

     D WCdProd         S              5S 0
     D WDsProd         S            100A
     D
     C*=======================================================
      /FREE

        WCdProd = $GetCodProduto(5);
        WDsProd = $GetDescRamo(5);
        WCdProd = $GetCodProduto(5:2);
        WDsProd = $GetDescRamo(5:2);
        WCdProd = $GetCodProduto(5:3);
        WDsProd = $GetDescRamo(5:3);

        *INLR = *ON;
        RETURN;
      /END-FREE

     P*=======================================================
     P* Processo..: $GetCodProduto
     P* Objectivo.: Devolve o Código do Produto de Catalogo
     P*             com a parameterização do Ramo.
     P*=======================================================
     P $getCodProduto  B                   EXPORT
     D                 PI             5S 0
     D    CdRamo                      5S 0 Const
     D    CdVariante                  5S 0 Const
     D                                     Options(*NOPASS)
     D
     D WCdRamo         S              5S 0 INZ(*ZEROS)
     D WCdVariante     S              5S 0 INZ(*ZEROS)
     D WResultado      S              5S 0 INZ(*ZEROS)
     D*=======================================================
      /FREE

         WCdRamo = CdRamo;
         IF (%Parms() = 2);
            WCdVariante = CdVariante;
            CHAIN (WCdRamo:WCdVariante) RCATP10001;
         ELSE;
            CHAIN (WCdRamo) RCATP10001;
         ENDIF;
         IF (%FOUND());
            RETURN CAT_CDPROD;
         ENDIF;

         RETURN WResultado;
      /END-FREE
     P $getCodProduto  E

     P*=======================================================
     P* Processo..: $GetDescRamo
     P* Objectivo.: Devolve a descrição para o Ramo
     P*=======================================================
     P $GetDescRamo    B                   EXPORT
     D                 PI           100A
     D    CdRamo                      5S 0 Const
     D    CdVariante                  5S 0 Const Options(*NOPASS)
     D
     D WCdProduto      S              5S 0 INZ(*ZEROS)
     D WCdRamo         S              5S 0 INZ(*ZEROS)
     D WCdVariante     S              5S 0 INZ(*ZEROS)
     D WResultado      S            100A   INZ(*BLANKS)
     D*=======================================================
      /FREE

         WCdRamo = CdRamo;
         MONITOR;
            WCdVariante = CdVariante;
            WCdProduto = $GetCodProduto(WCdRamo:WCdVariante);
         ON-ERROR;
            WCdProduto = $GetCodProduto(WCdRamo);
         ENDMON;

         CHAIN (WCdProduto) RCATP100;
         IF (%FOUND());
            RETURN CAT_DSPROD;
         ENDIF;

         RETURN WResultado;
      /END-FREE
     P $GetDescRamo    E
     P*=======================================================
     P* Processo..: $GetDescRisco
     P* Objectivo.: Devolve a descrição para o Risco
     P*=======================================================
     P $GetDescRisco   B                   EXPORT
     D                 PI           100A
     D    CdProduto                   5S 0 Const
     D    CdRisco                     5S 0 Const
     D*=======================================================
      /FREE

         MONITOR;
            CHAIN (CdProduto:CdRisco) RCATP101;
            IF (%FOUND());
               RETURN CAT_DSRISCO;
            ENDIF;
         ON-ERROR;
         ENDMON;
         RETURN *BLANKS;
      /END-FREE
     P $GetDescRisco   E
