     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: RRDO008
     H* DescriÁ„o.: Escrita das ApÛlices para todos os Ramos. Programa
     H*             estruturado para escrever nas actas e apÛlices.
     H*
     H*==================================================================
     FGBASEDIA  IF   F  700     9AIDISK    KEYLOC(1) USROPN
     FGBASEDIO  IF   F  700     9AIDISK    KEYLOC(1) USROPN
     FGEXAP     IF   F 1000        DISK    USROPN
     FFCATP110  IF   E           K DISK    PREFIX(CAT_)
     FFGRLP010  IF   E           K DISK    PREFIX(GRL_)
     FSPRDI008  O    E             PRINTER PREFIX(PRT_)
     F                                     OFLIND(*IN99)
     F*===================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SSECO010
      /Copy QRPGLESRC,SGRLO014
      /Copy QRPGLESRC,SCATO010
      /Copy QRPGLESRC,SPRDO002

     D*==> Estrutura de dados do Registo
     D RBASEDIO      E DS                  ExtName(GBASEDIO)
     D                                     Prefix(OR_)
     D RBASEDIA      E DS                  ExtName(GBASEDIA)
     D                                     Prefix(AU_)
     D RGEXAP        E DS                  ExtName(GEXAP)
     D                                     Prefix(PRD_)
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
     D
     D*==> Chamada de funÁ„o Dynamic, para trabalhar detalhe do Item
     D ShellExec       PR                  EXTPGM('QCMDEXC')
     D   stdcmd                     300A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5 CONST
     D
     D*==> Chamada de funÁ„o Dynamic, para trabalhar detalhe do Item
     D RetrText        PR                  EXTPGM(DynamicCall)
     D   Produto                      5S 0 CONST
     D   Ramo                         2S 0 CONST
     D   Apolice                      7S 0 CONST
     D   Data                         8S 0 CONST
     D   ptrHist                       *   CONST Options(*NOPASS)
     D   ptrDiario                     *   CONST Options(*NOPASS)
     D
     D*==> Lista de Campos
     D WCdProd         S              5S 0 INZ(*ZEROS)
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
     D*==> InformaÁ„o referente ao Formul·rio
     D WActLine        S              5S 0 INZ(*ZEROS)
     D WActPage        S              5S 0 INZ(*ZEROS)
     D WBlanks         S             20A   INZ(*BLANKS)
     D WLineWidth      S              3S 0
     D WValor          S             20A
     D WValorMask      S             20A
     D WCdMascara      S              2A
     D WTipo           S              2A
     D WLastTipo       S              2A
     D WException      S               N
     D WCdIdent        S              8A
     D WVlIdent        S              3S 0
     D WTexto          S            500A
     D WTmpTexto       S            500A
     D WIsEMpty        S               N
     D WIsFirst        S               N
     D
     D*==> Estilos dos Par·grafos
     D WGRL_IsUpper    S                   LIKE(GRL_ISUPPER)
     D WGRL_IsExcept   S                   LIKE(GRL_ISEXCEPT)
     D WGRL_QtdeIdent  S                   LIKE(GRL_QtdeIdent)
     D WGRL_TxtSize    S                   LIKE(GRL_TxtSize)
     D WGRL_DftBull    S                   LIKE(GRL_DftBull)
     D WGRL_DimBull    S                   LIKE(GRL_DimBull)
     D WGRL_TxtAlign   S                   LIKE(GRL_TxtAlign)
     D
     D StrLo           C                   'abcdefghijklmnopqrstuvwxyzÁ+
     D                                     „·‚‡ÈËÍÌÏÓÛÚıÙ˙˘˚'
     D StrUp           C                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ«+
     D                                     √¡¬¿…» ÕÃŒ”“’‘⁄Ÿ€'
     D*===================================================================
     IGEXAP     AA  11
     I                                  1 1000  RGEXAP
     IGBASEDIO  AA  11
     I                                  1  700  RBASEDIO
     IGBASEDIA  AA  11
     I                                  1  700  RBASEDIA
     I*===================================================================
     C     *ENTRY        PLIST
     C                   PARM                    PRM_AnoMes
     C                   PARM                    PRM_Carrega
     C                   PARM                    PRM_Ramo
     C                   PARM                    PRM_Apolice
      /FREE
       //------------------------------------
       // Validar os par‚metros recebidos
       //------------------------------------
       MONITOR;
          WAnoMes  = PRM_AnoMes;
          MONITOR;
             WCarrega = PRM_Carrega;
          ON-ERROR;
          ENDMON;
       ON-ERROR;
          WAnoMes  = 201201;
          WCarrega = '4';
       ENDMON;

       MONITOR;
          WRamo      = PRM_Ramo;
          WApolice   = PRM_Apolice;
       ON-ERROR;
       ENDMON;

       //------------------------------------
       // Gerar as Actas
       //------------------------------------
       WIcErro = *ON;
       EXSR chgPrinter;
       EXSR OpenFiles;
       IF (NOT WIcErro);
          READ GEXAP;
          DOW (NOT %EOF());
             WRamoApolice = %TRIM(%EDITW(PRD_RR:'0  ')+%CHAR(PRD_APOL));
             IF (PRD_RR = 50);
                CHAIN WRamoApolice GBASEDIA;
             ELSE;
                CHAIN WRamoApolice GBASEDIO;
             ENDIF;

             IF (%FOUND());
                WRamo    = PRD_RR;
                WApolice = PRD_Apol;
                EXSR getDetail;
             ENDIF;
             READ GEXAP;
          ENDDO;
       ENDIF;

       *INLR = *ON;
       RETURN;

       //==================================================================
       // Subrotina..: getDetail
       // Objectivo..: Obter os blocos de texto a escrever na Acta
       //==================================================================
       BEGSR getDetail;
          IF (WRamo = 5);
             IF (PRD_Condom = 'C');
                WCodProduto = $GetCodProduto(WRamo:2);
             ELSE;
                WCodProduto = $GetCodProduto(WRamo:1);
             ENDIF;
          ELSE;
             WCodProduto = $GetCodProduto(WRamo);
          ENDIF;

          WIcIniciar = *OFF;
          SETLL (WCodProduto) RCATP110;
          READE (WCodProduto) RCATP110;
          DOW (NOT %EOF());
             IF (NOT WIcIniciar);
                $InzLista(CAT_CDPAGINA);
             ENDIF;

             MONITOR;
                DynamicCall = $GetFunction(CAT_CDFUNCT);
                RetrText(WCdProd:WRamo:WApolice:WDatEmissao:
                            %Addr(RGEXAP):%Addr(RBASEDIO));
                *IN99 = *OFF;
             ON-ERROR;
             ENDMON;
             READE (WCodProduto) RCATP110;
          ENDDO;
          pLinhasTxt = $getLista();
          EXSR WrtDetail;
       ENDSR;

       //==================================================================
       // Subrotina..: WrtDetail
       // Objectivo..: Escrever o detalhe do Mapa
       //==================================================================
       BEGSR WrtDetail;
          WIdxLine  = 1;
          WActLine  = *ZEROS;
          WLastTipo = *BLANKS;
          WIsEMpty  = #LinhasTxt(WIdxLine).IsEMpty;
          DOW ((NOT WIsEmpty) AND (WIdxLine < 300));
             PRT_Linha  = *BLANKS;
             WException = #LinhasTxt(WIdxLine).Exception;
             WCdIdent   = #LinhasTxt(WIdxLine).CdIdent;
             WValor     = #LinhasTxt(WIdxLine).Valor;
             WTipo      = #LinhasTxt(WIdxLine).Tipo;
             WTexto     = #LinhasTxt(WIdxLine).Texto;
             WCdMascara = #LinhasTxt(WIdxLine).CdMascara;
             IF ((NOT WException) AND (%TRIM(WTexto) <> *BLANKS));
                //-----------------------------------------
                // Obter as configuraÁıes para o Par·grafo
                //-----------------------------------------
                IF (WLastTipo <> WTipo);
                   WLastTipo = WTipo;
                   CHAIN (WTipo) RGRLP010;
                   IF (%FOUND);
                       WGRL_IsUpper   = GRL_ISUPPER;
                       WGRL_ISEXCEPT  = GRL_ISEXCEPT;
                       WGRL_QtdeIdent = GRL_QTDEIDENT;
                       WGRL_TxtSize   = GRL_TXTSIZE;
                       WGRL_TxtAlign  = GRL_TXTALIGN;
                       WGRL_DftBull   = GRL_DFTBULL;
                       WGRL_DimBull   = GRL_DIMBULL;
                   ENDIF;
                ENDIF;

                //-----------------------------------------
                // Converter o Texto em Mai˙sculas
                //-----------------------------------------
                IF (WGRL_IsUpper = 'S');
                   WTexto = %XLATE(StrLo:StrUp:WTexto);
                ENDIF;

                //-----------------------------------------
                // Aplicar ExcepÁ„o
                //-----------------------------------------
                IF (WGRL_ISEXCEPT <> 'S');
                   EXSR ApplyCSStyle;
                ELSE;
                   EXSR WriteTextLine;
                ENDIF;
             ELSE;
                EXSR WriteTextLine;
             ENDIF;
             WIdxLine += 1;
          ENDDO;
       ENDSR;

       //==================================================================
       // Subrotina..: ApplyCSStyle
       // ObservaÁ„o.: O algoritmo utilizado nesta parte do CÛdigo faz
       //              A criaÁ„o de colunas de uma forma autom·tica,
       //              transformando o texto de uma atÈ trÍs colunas
       //              independentes.
       //==================================================================
       BEGSR ApplyCSStyle;
          //-------------------------------------------------------
          //
          //-------------------------------------------------------
          WVlIdent   = WGRL_QtdeIdent;
          IF (WVlIdent = *ZEROS);
             WVlIdent += 1;
          ENDIF;
          WLineWidth = WGRL_TxtSize - WVlIdent;
          IF (WGRL_DftBull <> *BLANKS) AND (WCdIdent = *BLANKS);
             WCdIdent = WGRL_DftBull;
          ENDIF;

          //-------------------------------------------------------
          // Identifica a Terceira ou Segunda Coluna (Valores)
          //-------------------------------------------------------
          IF (%TRIM(WValor) <> *BLANKS);
             IF (WCdMascara = 'VL');
                WValorMask  = $SetTextMask(WValor:'EUR ###.###.###,##':'*');
             ELSE;
                WValorMask = %TRIM(WValor);
             ENDIF;
             WLineWidth -= (%LEN(%TRIM(WValorMask))+1);
          ENDIF;

          //-------------------------------------------------------
          // Escreve a Coluna central de dimens„o relativa
          //-------------------------------------------------------
          WIsFirst = *ON;
          $SetTexto(WTexto:WLineWidth:WGRL_TxtAlign);
          $GetFirstLine(*OFF);
          DOW (NOT $EOFText());
             //-------------------------------------------
             // SÛ escreve a linha seguinte
             //-------------------------------------------
             IF (NOT WIsFirst);
                EXSR WriteTextLine;
             ENDIF;

             //-------------------------------------------
             // Escrever a primeira Coluna
             //-------------------------------------------
             WTexto = *BLANKS;
             IF (%TRIM(WCdIdent) <> *BLANKS);
                %SUBST(WTexto:WVlIdent)=%TRIM(%XLATE('?':' ':WCdIdent));
                WVlIdent += (%LEN(%TRIM(WCdIdent)) + 1);
                WCdIdent  = *BLANKS;
             ENDIF;
             %SUBST(WTexto:WVlIdent:WLineWidth)=$GetNextLine();
             WIsFirst = *OFF;
          ENDDO;

          IF (%TRIM(WValor) <> *BLANKS);
             %SUBST(WTexto:(WGRL_TxtSize-(%LEN(%TRIM(WValorMask)))))=
                       %TRIM(WValorMask);
          ENDIF;
          EXSR WriteTextLine;
       ENDSR;
       //==================================================================
       // Subrotina..: WriteTextLine
       // Objectivo..: Escrever o detalhe do Mapa
       //==================================================================
       BEGSR WriteTextLine;
          PRT_Linha = WTexto;
          WActLine += 1;
          WRITE RPRDI00801;
          PRT_LINHA = *BLANKS;
          IF (*IN99);
             *IN99 = *OFF;
          ENDIF;
          IF (WActLine > 79);
             PRT_LINHA = $AlignText('.../...':90:'R');
             WRITE RPRDI00801;
             WActLine = *ZEROS;
             *IN99 = *OFF;
             WRITE RPRDI00800;
             WActPage += 1;  // Incrementar a p·gina actual
             DOW (WActLine < 5);
                WActLine += 1;
                PRT_LINHA = *BLANKS;
                WRITE RPRDI00801;
             ENDDO;
          ENDIF;
       ENDSR;

       //==================================================================
       // Subrotina..: ChgPrinter
       // Objectivo..: Aplicar alteraÁıes ao ficheiro de Impress„o
       //==================================================================
       BEGSR ChgPrinter;
          MONITOR;
             WComando = 'OVRPRTF FILE(SPRDI008) +
                         TOFILE(SOAPSERV/SPRDI008) +
                         DEV(PRT03) DRAWER(2) SAVE(*YES) CPI(12) +
                         FORMTYPE(LUAP) MAXRCDS(*NOMAX) PAGESIZE(84) +
                         PRTQLTY(*NLQ) LPI(8) PAGRTT(0) DEVTYPE(*IPDS) +
                         FRONTMGN(.075 .000) HOLD(*YES) +
                         USRDTA(''Original'') DUPLEX(*YES)';
   29        ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
          ON-ERROR;
          ENDMON;
       ENDSR;

       //==================================================================
       // Subrotina..: OpenFiles
       // Objectivo..: Obter os dados das ApÛlices partindo do HistÛrico
       //==================================================================
       BEGSR OpenFiles;
          MONITOR;
             WComando = 'DLTF FILE(QTEMP/GEXAP)';
   29        ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
          ON-ERROR;
          ENDMON;

          MONITOR;
             WComando = 'CRTPF FILE(QTEMP/GEXAP) SRCFILE(SOAPSERV/QS36DDS) +
                         SRCMBR(GEXAP) IGCDTA(*N) OPTION(*NOLIST *NOSRC)';
   29        ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
          ON-ERROR;
          ENDMON;
          IF (WCarrega = *BLANKS);
             WNomFilHist = 'GEAP'+%SUBST(%CHAR(WAnoMes):3:4);
             WNomFilAuto = 'BGRA'+%SUBST(%CHAR(WAnoMes):3:4)+%TRIM(WCarrega);
             WNomFilOr   = 'BGRO'+%SUBST(%CHAR(WAnoMes):3:4)+%TRIM(WCarrega);
             WDatEmissao = (WAnoMes*100) + 1;
          ELSE;
             WNomFilHist = 'GEXAP'+%SUBST(%CHAR(WAnoMes):5:2)+%TRIM(WCarrega);
             WNomFilAuto = 'GBDIA'+%SUBST(%CHAR(WAnoMes):5:2)+%TRIM(WCarrega);
             WNomFilOr   = 'GBDIO'+%SUBST(%CHAR(WAnoMes):5:2)+%TRIM(WCarrega);
             Obj$Data = $GetObject('QS36F':%TRIM(WNomFilHist):'*FILE');
             WDatEmissao = Obj$Data.DatCria;
          ENDIF;
          MONITOR;
             WComando = 'OVRDBF FILE(GEXAP) TOFILE(QS36F/'+
                          %TRIM(WNomFilHist)+') LVLCHK(*NO)';
   29        ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
             WComando = 'OVRDBF FILE(GBASEDIA) TOFILE(QS36F/'+
                        %TRIM(WNomFilAuto)+') LVLCHK(*NO)';
   29        ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
             WComando = 'OVRDBF FILE(GBASEDIO) TOFILE(QS36F/'+
                        %TRIM(WNomFilOr)+') LVLCHK(*NO)';
   29        ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
             OPEN GEXAP;
             OPEN GBASEDIA;
             OPEN GBASEDIO;
             WIcErro = *OFF;
          ON-ERROR;
          ENDMON;
       ENDSR;
      /END-FREE
