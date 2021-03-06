     H NOMAIN
     F*=======================================================
     F* Dat.Cria...: 2012-04-26
     F* Autor......: Luis Galhoz
     F* Objectivo..: Fun��es de Cat�logo de Produtos
     F*=======================================================
     FFCATP100  IF   E           K DISK    USROPN PREFIX(CAT_)
     FFCATP101  IF   E           K DISK    USROPN PREFIX(CAT_)
     FFCATP10001IF   E           K DISK    RENAME(RCATP100:RCATP10001)
     F                                     USROPN PREFIX(CAT_)
     D*=======================================================
      /Copy QRPGLESRC,SCATO010
      /Copy QRPGLESRC,SSECO010

     D WCdProd         S              5S 0
     D WDsProd         S            100A
     D
     C*=======================================================

     P*=======================================================
     P* Processo..: $GetCobValor
     P* Objectivo.: Devolve o C�digo do Produto de Catalogo
     P*             com a parameteriza��o do Ramo.
     P*
     P* Tipo  Descri��o
     P* ----  ------------------------------------------------
     P*  CP   Capital Seguro
     P*  PR   Pr�mio
     P*  LI   Limite Indemniza��o
     P*  FR   Franquia
     P*=======================================================
     P $getCobValor    B                   EXPORT
     D                 PI            11S 2
     D    CdProd                      5S 0 CONST
     D    CdRamo                      5S 0 Const
     D    NuApol                      7S 0 CONST
     D    CdCobert                    5S 0 Const
     D    CdTipo                      2A   Const
     D
     D*==> Campos de Trabalho
     D WResultado      S             11S 2
     D*=======================================================
      /FREE
         IF (CdTipo = 'CP');
         ELSEIF (CdTipo = 'PR');
         ELSEIF (CdTipo = 'LI');
         ELSEIF (CdTipo = 'FR');
         ENDIF;

         RETURN WResultado;
      /END-FREE
     P $getCobValor    E

     P*=======================================================
     P* Processo..: $GetCodProduto
     P* Objectivo.: Devolve o C�digo do Produto de Catalogo
     P*             com a parameteriza��o do Ramo.
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
         OPEN FCATP10001;
         WCdRamo = CdRamo;
         IF (%Parms() = 2);
            WCdVariante = CdVariante;
            CHAIN (WCdRamo:WCdVariante) RCATP10001;
         ELSE;
            CHAIN (WCdRamo) RCATP10001;
         ENDIF;
         IF (%FOUND());
            WResultado = CAT_CDPROD;
         ELSE;
            WResultado = *ZEROS;
         ENDIF;
         CLOSE FCATP10001;
         RETURN WResultado;
      /END-FREE
     P $getCodProduto  E

     P*=======================================================
     P* Processo..: $GetAppApol
     P* Objectivo.: Devolve a fun��o para apresentar os dados
     P*             da Ap�lice.
     P*=======================================================
     P $getAppApol     B                   EXPORT
     D                 PI            10A
     D    CdProd                      5S 0 Const
     D
     D*==> Campos de Trabalho
     D WResultado      S             10A
     D*=======================================================
      /FREE
         OPEN FCATP10001;
         WResultado = *BLANKS;
         CHAIN (CdProd) RCATP10001;
         IF (%FOUND());
             WResultado = $GetFunction(CAT_CDFUNCT);
         ENDIF;
         CLOSE FCATP10001;
         RETURN WResultado;
      /END-FREE
     P $getAppApol     E

     P*=======================================================
     P* Processo..: $GetDescRamo
     P* Objectivo.: Devolve a descri��o para o Ramo
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

         WCdVariante = 1;
         IF (%PARMS() > 1);
            WCdVariante = CdVariante;
         ENDIF;

         OPEN  FCATP100;
         WCdRamo = CdRamo;
         MONITOR;
            WCdProduto = $GetCodProduto(WCdRamo:WCdVariante);
         ON-ERROR;
            WCdProduto = $GetCodProduto(WCdRamo);
         ENDMON;

         CHAIN (WCdProduto) RCATP100;
         IF (%FOUND());
            WResultado = CAT_DSPROD;
         ELSE;
            WResultado = *BLANKS;
         ENDIF;
         CLOSE FCATP100;
         RETURN WResultado;
      /END-FREE
     P $GetDescRamo    E

     P*=======================================================
     P* Processo..: $GetDescRisco
     P* Objectivo.: Devolve a descri��o para o Risco
     P*=======================================================
     P $GetDescRisco   B                   EXPORT
     D                 PI           100A
     D    CdProduto                   5S 0 Const
     D    CdRisco                     5S 0 Const
     D WResultado      S            100A
     D*=======================================================
      /FREE

         OPEN  FCATP101;
         MONITOR;
            CHAIN (CdProduto:CdRisco) RCATP101;
            IF (%FOUND());
               WResultado =  CAT_DSRISCO;
            ELSE;
               WResultado = *BLANKS;
            ENDIF;
         ON-ERROR;
         ENDMON;
         CLOSE FCATP101;
         RETURN WResultado;
      /END-FREE
     P $GetDescRisco   E
