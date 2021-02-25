     H NOMAIN
     H*===================================================================
     H* Objectivo..: Carregar Campos de trabalho.
     H* DescriÁ„o..: Segmentar um String, em linhas de texto de valor fixo
     H*              com o texto, justificado, alinhado ‡ esquerda ou ‡
     H*              direita e centrado.
     H*===================================================================
      /Copy QRPGLESRC,SGRLO010

     D*==> Frase decomposta em Palavras
     D Lst$Frase       DS                  QUALIFIED DIM(100)
     D   Palavra                     35A   INZ(*BLANKS)
     D   Dimensao                     2S 0 INZ(*ZEROS)
     D   IsEscrita                     N   INZ(*OFF)
     D   IsEmpty                       N   INZ(*ON)
     D WIdx            S              3S 0
     D WActPalavra     S              3S 0
     D WQtdPalavra     S              3S 0
     D
     D*==> Vari·veis de trabalho
     D WTexto          S           1000A
     D WDimensao       S              2S 0
     D WAlinhar        S              1A
     D
     D WTeste          S            100A
     D StrSpace1       C                   ' '
     D StrSpace2       C                   'ß'
     D
     D*===================================================================

     P*===================================================================
     P* Processo..: $SetTexto
     P* DescriÁ„o.: Carregar campos de trabalho com a palavra a forCompr.
     P*===================================================================
     P $SetTexto       B                   EXPORT
     D                 PI
     D    Texto                    1000A   Const
     D    Dimensao                    3S 0 Const
     D    Alinhar                     1A   Const Options(*NOPASS)
     D W_Alinhar       S              1A   INZ('E')
     C*==============================================================
      /FREE
        IF (%Parms() > 2);
           W_Alinhar = Alinhar;
        ELSE;
           W_Alinhar = 'E';
        ENDIF;
        WTexto    = Texto;
        WDimensao = Dimensao;
        WAlinhar  = W_Alinhar;
        WActPalavra = *ZEROS;
        DOW (WActPalavra < 100);
           WActPalavra += 1;
           Lst$Frase(WActPalavra).Palavra   = *BLANKS;
           Lst$Frase(WActPalavra).IsEmpty   = *ON;
           Lst$Frase(WActPalavra).IsEscrita = *OFF;
           Lst$Frase(WActPalavra).Dimensao  = *ZEROS;
        ENDDO;
        WQtdPalavra = *ZEROS;
        WActPalavra = *ZEROS;

      /END-FREE
     P $SetTexto       E

     P*===================================================================
     P* Processo..: $GetFirstLine
     P* DescriÁ„o.: Inicializar a funÁ„o e obter primeira linha
     P*===================================================================
     P $GetFirstLine   B                   EXPORT
     D                 PI           150A
     D PRM_Posiciona                   N   CONST Options(*NOPASS)
     D
     D WDimText        S              5S 0
     D WPalavra        S             35A
     D WIdx            S              5S 0
     D WLetra          S              1A
     D*===================================================================
      /FREE
         //-------------------------------------------------------
         // Decompor uma frase em Palavras
         //-------------------------------------------------------
         IF ($EOFText());
            RETURN *BLANKS;
         ENDIF;

         WPalavra = *BLANKS;
         WIdx     = *ZEROS;
         WDimText = %LEN(%TRIM(WTexto));
         DOW (WIdx < WDimText);
            WIdx += 1;
            WLetra = %SUBST(%TRIM(WTexto):Widx:1);
            IF (WLetra <> ' ');
               WPalavra = %TRIM(WPalavra) + WLetra;
            ELSE;
               IF (%TRIM(WPalavra) <> *BLANKS);
                  WActPalavra += 1;
                  Lst$Frase(WActPalavra).Palavra   = WPalavra;
                  Lst$Frase(WActPalavra).IsEmpty   = *OFF;
                  Lst$Frase(WActPalavra).IsEscrita = *OFF;
                  Lst$Frase(WActPalavra).Dimensao = %LEN(%TRIM(WPalavra));
                  WPalavra = *BLANKS;
                  WQtdPalavra += 1;
               ENDIF;
            ENDIF;
         ENDDO;

         //-------------------------------------------------------
         // Gravar a ⁄ltima Palavra
         //-------------------------------------------------------
         IF (%TRIM(WPalavra) <> *BLANKS);
            WActPalavra += 1;
            Lst$Frase(WActPalavra).Palavra   = WPalavra;
            Lst$Frase(WActPalavra).IsEmpty   = *OFF;
            Lst$Frase(WActPalavra).IsEscrita = *OFF;
            Lst$Frase(WActPalavra).Dimensao  = %LEN(%TRIM(WPalavra));
            WQtdPalavra += 1;
         ENDIF;

         WActPalavra = 1;
         IF (%Parms() > 0);
            IF (PRM_Posiciona = *ON);
               Return $GetNextLine();
            ELSE;
               Return *BLANKS;
            ENDIF;
         ELSE;
            Return $GetNextLine();
         ENDIF;
      /END-FREE
     P $GetFirstLine   E

     P*===================================================================
     P* Processo..: $GetNextLine
     P* DescriÁ„o.: Inicializar a funÁ„o e obter primeira linha
     P*===================================================================
     P $GetNextLine    B                   EXPORT
     D                 PI           150A
     D
     D WDimText        S              5S 0
     D WFolga          S              5S 0
     D WEspacos        S            150A
     D WQtdeEspacos    S              5S 0
     D WIdx            S              5S 0
     D WLetra          S              1A
     D WResult         S            150A
     D*===================================================================
      /FREE
        WResult = *BLANKS;
         IF (NOT $EOFText());
           //-------------------------------------
           // Determinar os extremos da Lista
           //-------------------------------------
           WDimText = *ZEROS;
           WFolga   = *ZEROS;
           WEspacos = *BLANKS;
           WIdx     = WActPalavra; // Marcar o Fim
           DOW (WIdx < 100);
              WDimText += Lst$Frase(WIdx).Dimensao + 1;
              IF (WDimText > WDimensao);
                 WDimText -= (Lst$Frase(WIdx).Dimensao + 1);
                 WFolga    = WDimensao - WDimText;
                 WIdx -= 1;
                 LEAVE;
              ELSEIF (WDimText = WDimensao);
                 LEAVE;
              ENDIF;
              WIdx += 1;
           ENDDO;

           //-------------------------------------
           // Construir a Frase
           //-------------------------------------
           DOW (WActPalavra <= WIdx);
              WQtdeEspacos = 1;
              IF (WAlinhar = 'J');
                 IF (WFolga > *ZEROS);
                    WQtdeEspacos = 1;
                    WFolga      -= 1;
                 ENDIF;
              ENDIF;
              WResult = %TRIM(WResult)+%SUBST(WEspacos:1:WQtdeEspacos)+
                         Lst$Frase(WActPalavra).Palavra;
              WActPalavra += 1;
           ENDDO;
        ENDIF;
        RETURN $AlignText(WResult:WDimensao:WAlinhar);
        //RETURN WResult;
      /END-FREE
     P $GetNextLine    E

     P*===================================================================
     P* Processo..: $EOFText
     P* DescriÁ„o.: Validar se chegou ao fim da ilnha de texto.
     P*===================================================================
     P $EOFText        B                   EXPORT
     D                 PI              N
     D
     D WResult         S               N
     D*===================================================================
      /FREE
        IF (WActPalavra > WQtdPalavra) OR (WDimensao <= *ZEROS);
           RETURN *ON;
        ELSE;
           RETURN *OFF;
        ENDIF;
      /END-FREE
     P $EOFText        E

     P*===================================================================
     P* Processo..: $SetTextMask
     P* DescriÁ„o.: Aplicar uma mascara a qualquer texto
     P*===================================================================
     P $SetTextMask    B                   EXPORT
     D                 PI           150A
     D    Texto                     130A   Const
     D    Mascara                    20A   Const
     D    DftChar                     1A   Const
     D
     D WTexto          S            130A   INZ(*BLANKS)
     D WBlanks         S             50A   INZ(*BLANKS)
     D WLimite         S              3S 0 INZ(*ZEROS)
     D WDimValor       S              3S 0 INZ(*ZEROS)
     D WResultado      S            150A   INZ(*BLANKS)
      /FREE
        WResultado = %TRIM(Mascara);
        WTexto     = %TRIM(Texto);
        WLimite = %LEN(%TRIM(WResultado));
        DOW (WLimite > *ZEROS);
           WDimValor = %LEN(%TRIM(WTexto));
           IF (%SUBST(WResultado:WLimite:1) = '#');
              IF (WDimValor > *ZEROS);
                 %SUBST(WResultado:WLimite:1) = %SUBST(WTexto:WDimValor:1);
                 WTexto = %SUBST(WTexto:1:(WDimValor - 1));
                 //WDimValor = %LEN(%TRIM(WTexto));
                 //IF ((%SUBST(WTexto:WDimValor:1)=',') OR
                 //    (%SUBST(WTexto:WDimValor:1)='.'));
                 //   WTexto = %SUBST(WTexto:1:(WDimValor - 1));
                 //ENDIF;
              ELSE;
                 %SUBST(WResultado:WLimite:1) = DftChar;
              ENDIF;
           ELSE;
              IF ((WDImValor = *ZEROS) AND
                   (%SUBST(WResultado:WLimite:1) = '.'));
                 %SUBST(WResultado:WLimite:1) = DftChar;
              ENDIF;
           ENDIF;
           WLimite -= 1;
        ENDDO;

        RETURN WResultado;
      /END-FREE
     P $SetTextMask    E

     P*===================================================================
     P* Processo..: $AlignText
     P* DescriÁ„o.: Alinhar um texto ‡ esquerda ou ‡ direita
     P*===================================================================
     P $AlignText      B                   EXPORT
     D                 PI           150A
     D    Texto                     150A   Const
     D    Dimensao                    3S 0 Const
     D    Alinha                      1A   Const
     D
     D WBlanks         S            150A   INZ(*BLANKS)
     D WDimValor       S              3S 0 INZ(*ZEROS)
     D WDimDifer       S              3S 0 INZ(*ZEROS)
     D WDimHalf        S              3S 0 INZ(*ZEROS)
     D WResultado      S            150A   INZ(*BLANKS)
     D WPosIni         S              5S 0
     D WPosFim         S              5S 0
     D WIdx            S              3S 0 INZ(*ZEROS)
      /FREE

        WResultado = %TRIM(Texto);
        WDimValor  = %LEN(%TRIM(Texto));
        WDimDifer  = (Dimensao - WDimValor);
        IF (WDimDifer > *ZEROS);
           IF (Alinha = 'D');     // Alinhar ‡ direita
              WResultado = %SUBST(WBlanks:1:WDimDifer) + %TRIM(Texto);
           ELSEIF (Alinha = 'E'); // Alinhar ‡ esquerda
              WResultado = %TRIM(Texto) + %SUBST(WBlanks:1:WDimDifer);
           ELSEIF (Alinha = 'C'); // Alinhar ao Centro
              WDimHalf = %INT(WDimDifer / 2);
              IF ((WDimHalf * 2) > WDimDifer);
                 WDimDifer += 1;
              ENDIF;
              WResultado = %SUBST(WBlanks:1:WDimHalf) + %TRIM(Texto);
           ELSEIF (Alinha = 'J'); // Justificado
              WIdx    = *ZEROS;
              WPosFim = *ZEROS;
              MONITOR;
                 DOW (WIdx < WDimDifer) AND (WIdx < 150);
                    WPosIni   = %SCAN(' ':WResultado:WPosFim+1);
                    IF (WPosIni > 0);
                        WPosFim    = WPosIni + 1;
                        WResultado = %SUBST(WResultado:1:(WPosIni - 1))+
                                    '  '+%SUBST(WResultado:WPosFim);
                    ENDIF;
                    WIdx += 1;
                 ENDDO;
              ON-ERROR;
                 RETURN WResultado;
              ENDMON;
           ENDIF;
        ENDIF;

        RETURN WResultado;
      /END-FREE
     P $AlignText      E

     P*===================================================================
     P* Processo..: $strReplace
     P* DescriÁ„o.: uma variavel pelo texto globais com valores
     P*===================================================================
     P $strReplace     B                   EXPORT
     D                 PI          2000A
     D    #What                     100A   Const
     D    #With                    1000A   Const
     D    #Where                   2000A   Const
     D
     D*==> Campos de Trabalho
     D WResult         S           2000A
     D WPosIni         S              5S 0
     D WPosFim         S              5S 0
     D WStrLen         S              5S 0
     D WFirstSeg       S           2000A
     D WLastSeg        S           2000A
     D*===================================================================
      /FREE
          IF (#What <> *BLANKS);
             WPosIni   = %SCAN(%TRIM(#What):#Where);
          ELSE;
             WPosIni   = %SCAN(' ':#Where);
          ENDIF;
          IF (WPosIni > 0);
             WStrLen = %LEN(%TRIM(#What));
             WPosFim = WPosIni + WStrLen;
             WResult = %SUBST(#Where:1:(WPosIni - 1))+%TRIMR(#With)+
                         %SUBST(#Where:WPosFim);
             WResult = $StrReplace(#What:#With:WResult);
          ELSE;
             WResult   = #Where;
          ENDIF;
          RETURN WResult;
      /END-FREE
     P $strReplace     E

     P*===================================================================
     P* Processo..: $strToUpper
     P* DescriÁ„o.: Colocar em mai˙sculas os caracteres de uma frase
     P*===================================================================
     P $strToUpper     B                   EXPORT
     D                 PI          1000A
     D    WTexto                   1000A   Const
     D StrLo           C                   'abcdefghijklmnopqrstuvwxyzÁ+
     D                                     „·‚‡ÈËÍÌÏÓÛÚıÙ˙˘˚'
     D StrUp           C                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ«+
     D                                     √¡¬¿…» ÕÃŒ”“’‘⁄Ÿ€'
     D*===================================================================
      /FREE
         RETURN %XLATE(StrLo:StrUp:WTexto);
      /END-FREE
     P $strToUpper     E

     P*===================================================================
     P* Processo..: $strToLower
     P* DescriÁ„o.: Colocar em min˙sculas os caracteres de uma frase
     P*===================================================================
     P $strToLower     B                   EXPORT
     D                 PI          1000A
     D    WTexto                   1000A   Const
     D StrLo           C                   'abcdefghijklmnopqrstuvwxyzÁ+
     D                                     „·‚‡ÈËÍÌÏÓÛÚıÙ˙˘˚'
     D StrUp           C                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ«+
     D                                     √¡¬¿…» ÕÃŒ”“’‘⁄Ÿ€'
     D*===================================================================
      /FREE
         RETURN %XLATE(StrUp:StrLo:WTexto);
      /END-FREE
     P $strToLower     E
