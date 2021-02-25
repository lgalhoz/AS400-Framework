     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: RPRDO008
     H* DescriÁ„o.: Bloco que devolve o texto de detalhe a escrever nas
     H*             actas.
     H*==================================================================
     FFCATP118  IF   E           K DISK    PREFIX(CAT_)
     FFCATP11801IF   E           K DISK    PREFIX(CAT_)
     F                                     RENAME(RCATP118:RCATP11801)
     F*===================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SGRLO014
      /Copy QRPGLESRC,SCATO010
      /Copy QRPGLESRC,SPRDO002
      /Copy QRPGLESRC,SSECO010

     D*==> Estrutura de dados do Registo
     D RBASEDIO      E DS                  ExtName(GBASEDIO)
     D                                     Prefix(OR_)
     D                                     Based(ptrDio)
     D RBASEDIA      E DS                  ExtName(GBASEDIA)
     D                                     Prefix(AU_)
     D                                     Based(ptrDia)
     D RGEXAP        E DS                  ExtName(GEXAP)
     D                                     Prefix(HST_)
     D                                     Based(ptrHist)
     D
     D
     D*==> Lista de Textos a escrever
     D WIdx            S              5S 0 INZ(*ZEROS)
     D
     D*==> Lista de Textos a escrever
     D #LinhasTxt      DS                  QUALIFIED DIM(300) BASED(pLinhasTxt)
     D   Tipo                         2A
     D   Exception                     N
     D   CdIdent                      8A
     D   Texto                      500A
     D   Valor                       20A
     D   CdMascara                    2A
     D   IsEMpty                       N
     D WIdxLine        S              5S 0 INZ(*ZEROS)
     D WMaxEntries     S              5S 0 INZ(50)
     D WLinha          S            120A   INZ(*BLANKS)
     D WCdGrupo        S              5S 0
     D WGEXAP          S               *
     D WGBaseDio       S               *
     D
     D*==> Chamada de funÁ„o Dynamic, para trabalhar detalhe do Item
     D RetrText        PR                  EXTPGM(DynamicCall)
     D   CdProd                       5S 0 CONST
     D   Ramo                         2S 0 CONST
     D   Apolice                      7S 0 CONST
     D   Data                         8S 0 CONST
     D   ptrHist                       *   CONST Options(*NOPASS)
     D   ptrDiario                     *   CONST Options(*NOPASS)
     D
     D*==> Lista de Campos
     D DynamicCall     S             10A   INZ(*BLANKS)
     D WOrigem         S              1A
     D WCarrega        S              1A   INZ(*BLANKS)
     D PRM_Carrega     S                   LIKE(WCarrega)
     D WAnoMes         S              6S 0 INZ(*ZEROS)
     D PRM_AnoMes      S                   LIKE(WAnoMes)
     D PRM_Ramo        S                   LIKE(WRAMO)
     D PRM_Apolice     S                   LIKE(WApolice)
     D WDatEmissao     S              8S 0 INZ(*ZEROS)
     D PRM_DatEmissao  S                   LIKE(WDatEmissao)
     D WNomFilHist     S             10A   INZ(*BLANKS)
     D WNomFilAuto     S             10A   INZ(*BLANKS)
     D WNomFilOr       S             10A   INZ(*BLANKS)
     D WComando        S            300A   INZ(*BLANKS)
     D WCodProduto     S              5S 0 INZ(*ZEROS)
     D WRamoApolice    S              9A
     D WIcErro         S               N   INZ(*OFF)
     D WIcIniciar      S               N   INZ(*OFF)
     D
     D*==> Campos do ficheiro (Base/Di·rio/HistÛrico)
     D WHST_Ramo       S                   LIKE(HST_RR)
     D WHST_Apolice    S                   LIKE(HST_APOL)
     D WHST_Condom     S                   LIKE(HST_Condom)
     D
     D StrLo           C                   'abcdefghijklmnopqrstuvwxyzÁ+
     D                                     „·‚‡ÈËÍÌÏÓÛÚıÙ˙˘˚'
     D StrUp           C                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ«+
     D                                     √¡¬¿…» ÕÃŒ”“’‘⁄Ÿ€'
     D*===================================================================
     C     *ENTRY        PLIST
     C                   PARM                    WGEXAP
     C                   PARM                    WGBaseDio
     C                   PARM                    #RetTexto
     C                   PARM                    WCdGrupo
      /FREE
       //------------------------------------
       // Validar os par‚metros recebidos
       //------------------------------------
       ptrHist     = WGEXAP;
       ptrDio      = WGBaseDio;

       MONITOR;
          WHST_Ramo   = HST_RR;
          WHST_APolice= HST_APOL;
          WHST_Condom = HST_CONDOM;
       ON-ERROR;
          MONITOR;
             WHST_Ramo   = OR_RAMO;
             WHST_APolice= OR_APOL;
             WHST_Condom = OR_CONDOM;
          ON-ERROR;
             *INLR = *ON;
             RETURN;
          ENDMON;
       ENDMON;

       EXSR getDetail;

       *INLR = *ON;
       RETURN;

       //==================================================================
       // Subrotina..: getDetail
       // Objectivo..: Obter os blocos de texto a escrever na Acta
       //==================================================================
       BEGSR getDetail;
          IF (WHST_Ramo = 5);
             IF (WHST_Condom = 'C');
                WCodProduto = $GetCodProduto(WHST_Ramo:2);
             ELSE;
                WCodProduto = $GetCodProduto(WHST_Ramo:1);
             ENDIF;
          ELSE;
             WCodProduto = $GetCodProduto(WHST_Ramo);
          ENDIF;

          //---------------------------------------------------
          // Escrever o texto dos Itens
          //---------------------------------------------------
          WIcIniciar = *OFF;
          IF (WCdGrupo = *ZEROS);
             SETLL (WCodProduto) RCATP118;
             READE (WCodProduto) RCATP118;
          ELSE;
             SETLL (WCodProduto:WCdGrupo) RCATP11801;
             READE (WCodProduto:WCdGrupo) RCATP11801;
          ENDIF;
          DOW (NOT %EOF());
             IF (NOT WIcIniciar);
                WIcIniciar = *ON;
                $InzLista(CAT_CdPagina);
             ENDIF;
             MONITOR;
                DynamicCall = $GetFunction(CAT_CDFUNCT);
                WDatEmissao = *ZEROS;
                RetrText(WCodProduto:WHST_Ramo:WHST_Apolice:WDatEmissao:
                         WGEXAP:WGBaseDio);
             ON-ERROR;
             ENDMON;
             IF (WCdGrupo = *ZEROS);
                READE (WCodProduto) RCATP118;
             ELSE;
                READE (WCodProduto:WCdGrupo) RCATP11801;
             ENDIF;
          ENDDO;

          //---------------------------------------------------
          // Escrever os Anexos se existirem
          //---------------------------------------------------

          #RetTexto = $ProcessTexto();
       ENDSR;
      /END-FREE
