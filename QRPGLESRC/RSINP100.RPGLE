     D*==> Função para Comparar Nomes
     D*ValName         PR              N
     D*   WNome01                    50A   CONST
     D*   WNome02                    50A   CONST
     D*   WRigor                      3S 2 CONST
     D*
     D*==> Validar com que erro os nomes são iguais
     D*ValPrecisao     PR             3S 2
     D*   WNome01                    50A   CONST
     D*   WNome02                    50A   CONST
     D*   WRigor                      3S 0 CONST
     D*
     D*====================================================================
      */FREE
      *
      * // *INLR = *OFF;
      *   RETURN;
      *
      */END-FREE
     P*==================================================================
     P* Processo..: ValName
     P* Objectivo.: Comparar duas Strings de Nome e determinar se
     P*             são parecidas a (n %).
     P*==================================================================
     P ValName         B                   EXPORT
     D ValName         PI              N
     D    PNome01                    50A   CONST
     D    PNome02                    50A   CONST
     D    PRigor                      3S 2 CONST
     D
     D WResult         S               N
     D WNome           S             55A
     D WPosIni         S              5S 0
     D WPosFim         S              5S 0
     D WStrLen         S              5S 0
     D WIdx            S              5S 0
     D WActArray       S              1S 0
     D WSumPeso        S              3S 2
     D WElmPeso        S              3S 2
     D WCmpPeso        S              3S 2
     D
     D WArr01          DS                  QUALIFIED DIM(10)
     D    Nome                       40A   INZ(*BLANKS)
     D WIdx01          S              5S 0
     D WArr02          DS                  QUALIFIED DIM(10)
     D    Nome                       40A   INZ(*BLANKS)
     D WIdx02          S              5S 0
     D
     D*==> Tornar os caracteres homogeneos
     D StrLo           C                   'abcdefghijklmnopqrstuvwxyzçÇ+
     D                                     ãÃáÁâÂàÀéÉèÈêÊíÍìÌîÎóÓòÒõÕôÔúÚùÙûÛ+
     D                                     /,-'
     D StrUp           C                   'ABCDEFGHIJKLMNOPQRSTUVWXYZCC+
     D                                     AAAAAAAAEEEEEEIIIIIIOOOOOOOOUUUUUU+
     D                                     ...'
     D*==================================================================
      /FREE
         WResult = *OFF;
         WSumPeso = *ZEROS;
         WElmPeso = *ZEROS;

         //-------------------------------------------------------
         // Sair se pelo menos um dos nomes não vier preenchido
         //-------------------------------------------------------
         IF (%TRIM(PNome01) = *BLANKS) OR (%TRIM(PNome02) = *BLANKS);
             RETURN WResult;
         ENDIF;

         //----------------------------------------------
         // Actualizar com dados o Array
         //----------------------------------------------
         WNome  = '.'+%XLATE(StrLo:StrUp:%XLATE(' ':'.':%TRIM(PNome01)))+'.';
         WActArray = 1;
         EXSR FillArray;

         WNome    = '.'+%XLATE(StrLo:StrUp:%XLATE(' ':'.':%TRIM(PNome02)))+'.';
         WActArray = 2;
         EXSR FillArray;

         //------------------------------------------------
         // Determinar qual a lista a deslocar e a manter
         //------------------------------------------------
         WActArray = *ZEROS;
         IF (WIdx01 > WIdx02);
            WActArray = 1;           // O Array com menos elementos
            WElmPeso = 1.0 / WIdx02; // O valor parcial de cada comparação
         ELSE;
            WActArray = 2;           // O Array com menos elementos
            WElmPeso = 1.0 / WIdx01; // O Valor parcial de cada comparação
         ENDIF;
         WIdx = 1;
         WIdx01 = 1;
         WIdx02 = 1;

         //------------------------------------------------
         // Comparar o conteudo de cada elemento da lista
         //------------------------------------------------
         DOW (Widx <= 10);
            IF ((%TRIM(WArr01(WIdx01).Nome) = *BLANKS) OR
                  (%TRIM(WArr02(WIdx02).Nome) = *BLANKS));
               LEAVE;
            ENDIF;

            WCmpPeso = (ValPrecisao(WArr01(WIdx01).Nome:
                                   WArr02(WIdx02).Nome:PRigor));
            IF (WCmpPeso > PRigor);
               WSumPeso += (WCmpPeso * WElmPeso);
               WIdx01 += 1;
               WIdx02 += 1;
            ELSE;
               //------------------------------------------------
               // Deslocar o Cursor da Lista com mais elementos
               //------------------------------------------------
               IF (WActArray = 1);
                  WIdx01 += 1;
               ELSEIF (WActArray = 2);
                  WIdx02 += 1;
               ELSE;
                  WIdx01 += 1;
                  WIdx02 += 1;
               ENDIF;
            ENDIF;
            WIdx += 1;
         ENDDO;

         IF (WSumPeso > PRigor);
            WResult = *ON;
         ELSE;
            WResult = *OFF;
         ENDIF;
         RETURN WResult;

         //==============================================================
         // Subrotina.: FillArray
         // Objectivo.: Colocar o conteudo do nome num array
         //==============================================================
         BEGSR FillArray;
            MONITOR;
               WIdx = 1;
               WPosIni = 1;
               DOW (WPosIni > *ZEROS);
                  WPosIni = %SCAN('.':WNome:WPosIni);
                  IF (WPosIni = *ZEROS);
                     LEAVE;
                  ENDIF;

                  WPosIni += 1;
                  WPosFim = %SCAN('.':WNome:WPosIni);
                  WStrLen = (WPosFim - WPosIni);
                  IF (WStrLen > *ZEROS);
                     IF (WActArray = 1);
                        WArr01(WIdx).Nome =  %SUBST(WNome:WPosIni:WStrLen);
                        WIdx01 = WIdx; // Actualizar Limites
                     ELSE;
                        WArr02(WIdx).Nome =  %SUBST(WNome:WPosIni:WStrLen);
                        WIdx02 = WIdx; // Actualizar Limites
                     ENDIF;
                     WIdx += 1;
                  ELSE;
                     LEAVE;
                  ENDIF;
               ENDDO;
            ON-ERROR;
            ENDMON;
         ENDSR;
      /END-FREE
     P ValName         E
     P*==================================================================
     P* Processo..: ValPrecisao
     P* Objectivo.: Validar se o Nomes são semelhantes com uma taxa de
     P*             rigor (percisão) a n %.
     P*             Criterios:
     P*               [x] % de caracteres Comuns
     P*               [x] % na mesma Posição
     P*               [x] % na mesma Dimensão
     P*==================================================================
     P ValPrecisao     B                   EXPORT
     D ValPrecisao     PI             3S 2
     D    PNome01                    50A   CONST
     D    PNome02                    50A   CONST
     D    PRigor                      3S 2 CONST
     D
     D WIdx            S              5S 0
     D WIdx01          S              5S 0
     D WLenIdx         S              5S 0
     D WStrLen01       S              3S 0
     D WStrLen02       S              3S 0
     D WCmpValida      S              3S 2
     D WNome           S             50A
     D WPesquisa       S             50A
     D WPerCom         S              3S 2
     D WQtdCom         S              5S 0
     D WPerDim         S              3S 2
     D WPerPos         S              3S 2
     D WResult         S              3S 2
      /FREE
         //---------------------------------------------------
         // 1. Validar se está a comparar com um nome abreviado
         // 2. Validar se está a comparar com um nome abreviado
         // 3. Comparação Aproximação/Erro
         //---------------------------------------------------
         WResult = *ZEROS;
         IF (%TRIM(PNome01) = %TRIM(PNome02));
            WResult = 1;
         ELSEIF ((%LEN(%TRIM(PNome01)) < 2) OR (%LEN(%TRIM(PNome02)) < 2))
            AND (%SUBST(PNome01:1:1) = %SUBST(PNome02:1:1));
            WResult = 1;
         ELSE;
            WStrLen01 = %LEN(%TRIM(PNome01));
            WStrLen02 = %LEN(%TRIM(PNome02));
            EXSR ValSamDim; // Validar com a mesma Dimensão
            IF (WPerDim > 0.5);
               EXSR ValSamPos; // Validar com a mesma posição
               IF (WPerPos > *ZEROS);
                  EXSR ValComuns; // Validar quais os Caracteres Comuns
                  WResult = WPerCom;
               ENDIF;
            ENDIF;
         ENDIF;
         RETURN WResult;

       //==============================================================
       // Subrotina.: ValSamDim
       // Objectivo.: Validar % de palavras com a mesma dimensão
       //==============================================================
       BEGSR ValSamDim;
          IF (WStrLen01 > WStrLen02);
             WPerDim = 1 - ((WStrLen01 - WStrLen02) / WStrLen01);
          ELSE;
             WPerDim = 1 - ((WStrLen02 - WStrLen01) / WStrLen02);
          ENDIF;
       ENDSR;

       //==============================================================
       // Subrotina.: ValSamPos
       // Objectivo.: Validar % na mesma Posição e onde é obrigatório
       //             O primeiro caracter ser comun.
       //==============================================================
       BEGSR ValSamPos;
          WperPos = *ZEROS;
          IF (%SUBST(PNome01:1:1) = %SUBST(PNome02:1:1));
             IF (WStrLen01 > WStrLen02);
                WIdx = WStrLen02;
             ELSE;
                WIdx = WStrLen01;
             ENDIF;

             WPerPos = *ZEROS;
             DOW (WIdx > 0);
                IF (%SUBST(PNome01:WIdx:1) = %SUBST(PNome02:WIdx:1));
                   WPerPos += 1;
                ENDIF;
                WIdx -= 1;
             ENDDO;

             IF (WStrLen01 > WStrLen02);
                WPerPos = WPerPos / WStrLen02;
             ELSE;
                WPerPos = WPerPos / WStrLen01;
             ENDIF;
          ENDIF;
       ENDSR;

       //==============================================================
       // Subrotina.: ValComuns
       // Objectivo.: Validar % de Caracteres Comuns
       //==============================================================
       BEGSR ValComuns;
          IF (WStrLen01 < WStrLen02);
             WCmpValida = (WStrLen01 / WStrLen02);
             WIdx      = WStrLen01;
             WLenIdx   = WStrLen02;
             WPesquisa = PNome01;
             WNome     = PNome02;
          ELSE;
             WIdx      = WStrLen02;
             WLenIdx   = WStrLen01;
             WPesquisa = PNome02;
             WNome     = PNome01;
             WCmpValida = (WStrLen02 / WStrLen01);
          ENDIF;

          IF (WCmpValida < PRigor);
             WPerCom = 0;
          ENDIF;

          WQtdCom = *ZEROS;
          DOW (WIdx > *ZEROS);
             WIdx01 = WLenIdx;
             DOW (WIdx01 > *ZEROS);
                IF (%SUBST(WNome:WIdx01:1) = %SUBST(WPesquisa:WIdx:1));
                   %SUBST(WNome:Widx01:1) = 'Ç';
                   WQtdCom += 1;   // Quantidade Comum
                   LEAVE;
                ENDIF;
                WIdx01 -= 1;
             ENDDO;
             WIdx -= 1;
          ENDDO;

          IF (WStrLen02 > WStrLen01);
             WPerCom = WQtdCom / WStrLen01;
          ELSE;
             WPerCom = WQtdCom / WStrLen02;
          ENDIF;
          WNome = %TRIM(WNome);
       ENDSR;
      /END-FREE
     P ValPrecisao     E
