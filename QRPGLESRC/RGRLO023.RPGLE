     H NOMAIN
     H*==================================================================
     H* Programa..: RPRDO002
     H* Descrição.: Tratar linhas de Textos das Actas
     H*
     H*==================================================================
     FFGRLP008  IF   E           K DISK    USROPN PREFIX(GRL_)
     FFGRLP010  IF   E           K DISK    USROPN PREFIX(GRL_)
      /Copy QRPGLESRC,SGRLO010

     D*==> Lista de Textos a escrever
     D #LinhasTxt      DS                  QUALIFIED DIM(2000)
     D   Tipo                         2A   INZ(*BLANKS)
     D   Exception                     N   INZ(*OFF)
     D   CdIdent                      8A   INZ(*BLANKS)
     D   Texto                      500A   INZ(*BLANKS)
     D   Valor                       20A   INZ(*BLANKS)
     D   CdMascara                    2A   INZ(*BLANKS)
     D   IsEmpty                       N   INZ(*ON)
     D pLinhasTxt      S               *   INZ(%ADDR(#LinhasTxt))
     D WIdxLine        S              5S 0 INZ(*ZEROS)
     D WLinha          S            120A   INZ(*BLANKS)
     D WDftException   S               N   INZ(*OFF)
     D WDftLineType    S              2A   INZ('T')
     D WBulNumVal      S              2S 0 INZ(*ZEROS)
     D WPagina         S              5S 0 INZ(*ZEROS)
     D*==================================================================

     P*=======================================================
     P* Processo..: $getLista
     P* Objectivo.: Obter a Lista com as linhas
     P*=======================================================
     P $GetLista       B                   EXPORT
     D                 PI              *
     D
     D*=======================================================
      /FREE
         RETURN  %ADDR(#LinhasTxt);
      /END-FREE
     P $GetLista       E

     P*=======================================================
     P* Processo..: $addException
     P* Objectivo.: Inicializar a lista de Trabalho
     P*=======================================================
     P $addException   B                   EXPORT
     D                 PI
     D DftException                    N   Const
     D
      /FREE
         WDftException = DftException;
      /END-FREE
     P $addException   E

     P*=======================================================
     P* Processo..: $addLineType
     P* Objectivo.: Inicializar o tipo de Linha
     P*=======================================================
     P $AddLineType    B                   EXPORT
     D                 PI
     D DftLineType                    2A   Const
     D
      /FREE
         WDftLineType  =  DftLineType;
      /END-FREE
     P $AddLineType    E

     P*=======================================================
     P* Processo..: $InzLista
     P* Objectivo.: Inicializar a lista de Trabalho
     P*=======================================================
     P $InzLista       B                   EXPORT
     D                 PI
     D Pagina                         5S 0 CONST
     D*=======================================================
      /FREE
         WPagina = Pagina;
         WIdxLine = *ZEROS;
         #LinhasTxt(1).IsEMpty = *ON;

         WIdxLine = *ZEROS;
         #RetTexto(1).IsEMpty = *ON;
      /END-FREE
     P $InzLista       E

     P*=======================================================
     P* Processo..: AddTextLine
     P* Objectivo.: Adicionar uma linha de Texto
     P*=======================================================
     P $AddTextLine    B                   EXPORT
     D                 PI
     D    Texto                     500A   Const Options(*NOPASS)
     D    Qtde                        3S 0 Const Options(*NOPASS)
     D    CdIdent                     8A   Const Options(*NOPASS)
     D    ImValor                    20A   Const Options(*NOPASS)
     D    CdMascara                   2A   Const Options(*NOPASS)
     D
     D WTexto          S            500A   INZ(*BLANKS)
     D WQtde           S              3S 0 INZ(1)
     D WCdIdent        S              8A   INZ(*BLANKS)
     D WImValor        S             20A   INZ(*BLANKS)
     D WCdMascara      S              2A   INZ(*BLANKS)
     D WIdx            S              3S 0 INZ(*ZEROS)
     D*=======================================================
      /FREE

         IF (%Parms() > 0);
            WTexto = Texto;
            IF (%Parms() > 1);
               WQtde = Qtde;
               IF (%Parms() > 2);
                  WCdIdent = CdIdent;
                  IF (%Parms() > 3);
                     WImValor = ImValor;
                     IF (%Parms() > 4);
                        WCdMascara = CdMascara;
                     ENDIF;
                  ENDIF;
               ENDIF;
            ENDIF;
         ENDIF;

         WIdx = *ZEROS;
         DOW (WIdx < WQtde);
            WIdx     += 1;
            WIdxLine += 1;
            #LinhasTxt(WIdxLine).Tipo      = WDftLineType;
            #LinhasTxt(WIdxLine).Exception = WDftException;
            #LinhasTxt(WIdxLine).Texto     = WTexto;
            #LinhasTxt(WIdxLine).IsEMpty   = *OFF;
            #LinhasTxt(WIdxLine).CdIdent   = WCdIdent;
            #LinhasTxt(WIdxLine).Valor     = WImValor;
            #LinhasTxt(WIdxLine).CdMascara = WCdMascara;
            #LinhasTxt(WIdxLine + 1).IsEMpty   = *ON;
         ENDDO;

      /END-FREE
     P $AddTextLine    E

     P*=======================================================
     P* Processo..: $getQtdeLinhas
     P* Objectivo.: Obter o Número de Linhas até ao próximo
     P*             Nivel Superior.
     P* Observação: Por exemplo, pretende-se saber quantas
     P*             linhas estão debaixo do Header 1, nesta
     P*             situação devolve as do Header 1 inclusiver
     P*             Header 2, 3 e pára quanto encontrar um
     P*             Header do mesmo nível, neste caso igual a 1.
     P*=======================================================
     P $getQtdeLinhas  B                   EXPORT
     D                 PI             5S 0
     D    Registo                     5S 0 Const
     D
     D WEstilo         S              2A
     D WQtde           S              3S 0 INZ(1)
     D WIdx            S              3S 0 INZ(*ZEROS)
     D*=======================================================
      /FREE

         //------------------------------------------------------
         // Obter o estilo corrente
         //------------------------------------------------------
         WEstilo = #LinhasTxt(Registo).Tipo;
         WQtde   = 1;
         WIdx    = (Registo + 1);
         DOW (NOT #LinhasTxt(WIdx).IsEMpty);
            WIdx += 1;
            IF (WEstilo = #LinhasTxt(WIdx).Tipo);
               Leave;
            ENDIF;
            WQtde += 1;
         ENDDO;
         Return WQtde;
      /END-FREE
     P $GetQtdeLinhas  E

     P*=======================================================
     P* Processo..: $getLineHeader
     P* Objectivo.: O
     P* Observação:
     P*=======================================================
     P $getLineHeader  B                   EXPORT
     D                 PI           300A
     D    Registo                     5S 0 Const
     D
     D WEstilo         S              2A
     D WQtde           S              3S 0 INZ(1)
     D WIdx            S              3S 0 INZ(*ZEROS)
     D WResultado      S            300A
     D*=======================================================
      /FREE
         Return WResultado;
      /END-FREE
     P $getLineHeader  E

     P*====================================================================
     P* Processo..: $ProcessTexto
     P* Objectivo.: Gerar um componente de Texto, com a aplicação das
     P*             regras parameterizadas no ficheiro de estilos
     P*====================================================================
     P $ProcessTexto   B                   EXPORT
     D                 PI                  LikeDS(#RetTexto) DIM(2000)
     D    DTipo                       1A   Const Options(*NOPASS)
     D    DLargura                    3S 0 Const Options(*NOPASS)
     D
     D*==> Informação referente ao Formulário
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
     D*==> Estilos dos Parágrafos
     D WCdGrupo        S              5S 0
     D WGRL_SpcAntes   S                   LIKE(GRL_SpcAntes)
     D WGRL_SpcDepois  S                   LIKE(GRL_SpcDepois)
     D WGRL_IsUpper    S                   LIKE(GRL_IsUpper)
     D WGRL_IsExcept   S                   LIKE(GRL_IsExcept)
     D WGRL_QtdeIdent  S                   LIKE(GRL_QtdeIdent)
     D WGRL_TxtSize    S                   LIKE(GRL_TxtSize)
     D WGRL_TxtSizeBk  S                   LIKE(GRL_TxtSize)
     D WGRL_DftBull    S                   LIKE(GRL_DftBull)
     D WGRL_DimBull    S                   LIKE(GRL_DimBull)
     D WGRL_TxtAlign   S                   LIKE(GRL_TxtAlign)
     D WCountEmpty     S              5S 0 INZ(*ZEROS)
     D WQtdeLinhas     S              5S 0 INZ(*ZEROS)
     D
     D
     D WIdx            S              5S 0 INZ(*ZEROS)
     D WAddLinhas      S              5S 0 INZ(*ZEROS)
     D WTipPagina      S              1A   INZ(*BLANKS)
     D WLargura        S              3S 0 INZ(*ZEROS)
     D*====================================================================
      /FREE
         OPEN FGRLP010;
         OPEN FGRLP008;
         WTipPagina = 'T';
         WLargura = *ZEROS;

         IF (%PARMS() > 1);
            WTipPagina = DTipo;
            WLargura = DLargura;
         ELSEIF (%PARMS() > 0);
            WTipPagina = DTipo;
         ENDIF;
         EXSR WrtDetail;
         CLOSE FGRLP010;
         CLOSE FGRLP008;
         RETURN #RetTexto;

       //==================================================================
       // Subrotina..: WrtDetail
       // Objectivo..: Escrever o detalhe do Mapa
       //==================================================================
       BEGSR WrtDetail;
          WIdxLine  = 1;
          WActLine  = *ZEROS;
          WLastTipo = *BLANKS;
          WIsEmpty  = #LinhasTxt(WIdxLine).IsEMpty;
          DOW (NOT WIsEmpty);
             WException = #LinhasTxt(WIdxLine).Exception;
             WCdIdent   = #LinhasTxt(WIdxLine).CdIdent;
             WValor     = #LinhasTxt(WIdxLine).Valor;
             WTipo      = #LinhasTxt(WIdxLine).Tipo;
             WTexto     = #LinhasTxt(WIdxLine).Texto;
             WCdMascara = #LinhasTxt(WIdxLine).CdMascara;
             IF ((NOT WException) AND (%TRIM(WTexto) <> *BLANKS));
                //-----------------------------------------
                // Obter as configurações para o Parágrafo
                //-----------------------------------------
                IF (WLastTipo <> WTipo);
                   WGRL_IsExcept  = 'S';
                   WLastTipo = WTipo;
                   CHAIN (WPagina) RGRLP008;
                   IF (%Found());
                      IF (WLargura = *ZEROS);
                         WGRL_TxtSize = GRL_TxtSize;
                         WLargura     = GRL_TxtSize;
                      ELSE;
                         WGRL_TxtSize = WLargura;
                      ENDIF;
                      CHAIN (WPagina:WTipo) RGRLP010;
                      IF (%Found);
                         WGRL_IsUpper   = GRL_IsUpper;
                         WGRL_IsExcept  = GRL_IsExcept;
                         WGRL_QtdeIdent = GRL_QtdeIdent;
                         WGRL_TxtAlign  = GRL_TxtAlign;
                         WGRL_DftBull   = GRL_DftBull;
                         WGRL_TxtSize  -= GRL_DimBull;
                         WGRL_DimBull   = GRL_DimBull;
                         WGRL_SpcAntes  = GRL_SpcAntes;
                         WGRL_SpcDepois = GRL_SpcDepois;
                         IF (WGRL_DftBull <> '##.');
                            WBulNumVal = *ZEROS;
                         ENDIF;
                      ENDIF;
                   ENDIF;
                ENDIF;

                //-----------------------------------------
                // Converter o Texto em Maiúsculas
                //-----------------------------------------
                IF (WGRL_IsUpper = 'S');
                   WTexto = $StrToUpper(WTexto);
                ENDIF;

                //-----------------------------------------
                // Aplicar Excepção
                //-----------------------------------------
                IF (WGRL_IsExcept <> 'S');
                   EXSR ApplyCSStyle;
                ELSE;
                   EXSR AjustDimensao;
                   EXSR WriteTextLine;
                ENDIF;
             ELSE;
                EXSR WriteTextLine;
             ENDIF;
             WIdxLine += 1;
             WIsEmpty  = #LinhasTxt(WIdxLine).IsEMpty;
          ENDDO;
       ENDSR;

       //==================================================================
       // Subrotina..: AjustDimensao
       // Observação.: Ajustar o texto à largura do texto
       //==================================================================
       BEGSR AjustDimensao;
          IF (%SUBST(WTexto:WLargura) <> *BLANKS);
          ENDIF;
       ENDSR;

       //==================================================================
       // Subrotina..: ApplyCSStyle
       // Observação.: O algoritmo utilizado nesta parte do Código faz
       //              A criação de colunas de uma forma automática,
       //              transformando o texto de uma até três colunas
       //              independentes.
       //==================================================================
       BEGSR ApplyCSStyle;
          //-------------------------------------------------------
          //
          //-------------------------------------------------------
          WTmpTexto = WTexto;
          IF (WGRL_SpcAntes > *ZEROS);
             WAddLinhas = *ZEROS;
             DOW (WAddLinhas < WGrl_SpcAntes);
                WTexto = *BLANKS;
                EXSR WriteTextLine;
                WAddLinhas += 1;
             ENDDO;
          ENDIF;
          WTexto = WTmpTexto;

          WVlIdent   = WGRL_QtdeIdent + 1;
          WLineWidth = WGRL_TxtSize - WGRL_QtdeIdent;
          IF (WGRL_DftBull <> *BLANKS) AND (WCdIdent = *BLANKS);
             IF (WGRL_DftBull = '##.');
                WBulNumVal += 1;
                WCdIdent = %TRIM(%EDITW(WBulNumVal:'0  '))+'.';
             ELSE;
                WBulNumVal = *ZEROS;
                WCdIdent = WGRL_DftBull;
             ENDIF;
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
          // Escreve a Coluna central de dimensão relativa
          //-------------------------------------------------------
          WIsFirst = *ON;
          $SetTexto(WTexto:WLineWidth:WGRL_TxtAlign);
          $GetFirstLine(*OFF);
          DOW (NOT $EOFText());
             //-------------------------------------------
             // Só escreve a linha seguinte
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
                WVlIdent += (%LEN(%TRIM(WCdIdent)))+1;
                WCdIdent  = *BLANKS;
             ENDIF;
             %SUBST(WTexto:WVlIdent:WLineWidth)=$GetNextLine();
             WIsFirst = *OFF;
          ENDDO;

          IF (%TRIM(WValor) <> *BLANKS);
             %SUBST(WTexto:(WGRL_TxtSize-(%LEN(%TRIM(WValorMask)))+1))=
                       %TRIM(WValorMask);
          ENDIF;
          EXSR WriteTextLine;

          IF (WGRL_SpcDepois > *ZEROS);
             WAddLinhas = *ZEROS;
             DOW (WAddLinhas < WGrl_SpcDepois);
                WTexto = *BLANKS;
                EXSR WriteTextLine;
                WAddLinhas += 1;
             ENDDO;
          ENDIF;
       ENDSR;

       //==================================================================
       // Subrotina..: WriteTextLine
       // Objectivo..: Escrever o detalhe do Mapa
       //==================================================================
       BEGSR WriteTextLine;
          IF (%TRIM(WTexto) = *BLANKS);
             WCountEmpty += 1;
          ELSE;
             WCountEmpty = *ZEROS;
          ENDIF;

          IF (WCountEmpty < 4);
             WActLine += 1;
             WIdx += 1;
             #RetTexto(Widx).Texto     = WTexto;
             #RetTexto(Widx).isEmpty   = *OFF;
             #RetTexto(Widx+1).isEmpty = *ON;
          ENDIF;
       ENDSR;
      /END-FREE
     P $ProcessTexto   E
