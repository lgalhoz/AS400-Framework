     H NOMAIN
     H*==================================================================
     H* Função....: RGRLO028
     H* Descrição.: Validar a Estrurua de uma String
     H*             Recebe uma String e um Template, a Template deverá obedecer à seguinte estrutura:
     H*             ####[V1][V2]###[Vn-1][Vn][Vn+1]###
     H*             Onde # - Corresponde a texto fixo e [Vn] corresponde a uma variável
     H*
     H*==================================================================
      /Copy QRPGLESRC,SGRLO010

     D*==> Actualizar lista de controlo
     D DumpStrVars     PR
     D    FileName                   10A   Const
     D    Template                   40A   Const
     D
     D*==> Lista de Trabalho
     D Lst#Name        DS                  QUALIFIED DIM(5)
     D   PosIni                       3S 0 INZ(*ZEROS)
     D   Dimens                       3S 0 INZ(*ZEROS)
     D   Codigo                       1A   INZ(*BLANKS)
     D   Palavra                     10A   INZ(*BLANKS)
     D   IsEmpty                       N   INZ(*ON)
     D
     D*==> Variáveis de Trabalho
     D WProduto        S              1A
     D WCarrega        S              1A
     D WMes            S              2S 0
     D WAno            S              4S 0
     D WCdGrupo        S             10A
     D
     D WFilName        S             10A
     D WTemplate       S             40A
     D*==========================================================

     P*=======================================================
     P* Processo..: DumpStrVars
     P* Descrição.: Extrair as variáveis de uma String em
     P*             função de uma Template.
     P*=======================================================
     P DumpStrVars     B                   EXPORT
     D                 PI
     D    FileName                   10A   Const
     D    Template                   40A   Const
     D
     D*==> Campos de Trabalho
     D WIdxReg         S              3S 0
     D WPosIni         S              3S 0
     D WPosFim         S              3S 0
     D WStrLen         S              3S 0
     D WPalavra        S             10A
     D
     D*===========================================================
      /FREE
         //-----------------------------------------------------------
         // Iniciar a Lista de Trabalho
         //-----------------------------------------------------------
         WIdxReg = *ZEROS;
         DOW (WIdxReg < 5);
            WIdxReg += 1;
            Lst#Name(WIdxReg).PosIni  = *ZEROS;
            Lst#Name(WIdxReg).Dimens  = *ZEROS;
            Lst#Name(WIdxReg).Codigo  = *BLANKS;
            Lst#Name(WIdxReg).Palavra = *BLANKS;
            Lst#Name(WIdxReg).IsEmpty = *ON;
         ENDDO;

         //-----------------------------------------------------------
         // Decompor os parâmetros da Template
         //-----------------------------------------------------------
         WIdxReg = 1;
         WPosIni = 1;
         DOW (WPosFim < %LEN(%TRIM(Template)));
            WPosFim = %SCAN('[':Template:WPosIni);
            IF (WPosFim = *ZEROS);
               WPosFim = %SCAN(']':Template:WPosIni);
               IF (WPosFim = *ZEROS);
                  WPosFim = %LEN(%TRIM(Template));
               ENDIF;
            ENDIF;
            WPalavra = %SUBST(Template:WPosIni:(WPosFim - WPosIni));

            IF (WPalavra = 'MM]') OR (WPalavra = 'MM');
               Lst#Name(WIdxReg).Palavra = 'MM';
               Lst#Name(WIdxReg).Codigo  = 'M';
               Lst#Name(WIdxReg).Dimens  = 2;
            ELSEIF (WPalavra = 'C]') OR (WPalavra = 'C');
               Lst#Name(WIdxReg).Palavra = 'C';
               Lst#Name(WIdxReg).Codigo  = 'C';
               Lst#Name(WIdxReg).Dimens  = 1;
            ELSEIF (WPalavra = 'AA]') OR (WPalavra = 'AA');
               Lst#Name(WIdxReg).Palavra = 'AA';
               Lst#Name(WIdxReg).Codigo  = 'A';
               Lst#Name(WIdxReg).Dimens  = 2;
            ELSEIF (WPalavra = 'A|O|T]') OR (WPalavra = 'A|O|T');
               Lst#Name(WIdxReg).Palavra = 'A|O|T';
               Lst#Name(WIdxReg).Codigo  = 'P';
               Lst#Name(WIdxReg).Dimens  = 1;
            ELSEIF (WPalavra = '1|2]') OR (WPalavra = '1|2');
               Lst#Name(WIdxReg).Palavra = '1|2';
               Lst#Name(WIdxReg).Codigo  = 'Z';
               Lst#Name(WIdxReg).Dimens  = 1;
            ELSEIF (WIdxReg = 1);
               Lst#Name(WIdxReg).Palavra = WPalavra;
               Lst#Name(WIdxReg).Codigo  = 'G';
               Lst#Name(WIdxReg).Dimens  = %LEN(%TRIM(WPalavra));
            ELSE;
               Lst#Name(WIdxReg).Palavra = WPalavra;
               Lst#Name(WIdxReg).Codigo  = 'K';
               Lst#Name(WIdxReg).Dimens  = %LEN(%TRIM(WPalavra));
            ENDIF;
            IF (WIdxReg > 1);
               Lst#Name(WIdxReg).PosIni  = Lst#Name(WIdxReg-1).PosIni +
                                           Lst#Name(WIdxReg-1).Dimens;
            ELSE;
               Lst#Name(WIdxReg).PosIni  = WPosIni;
            ENDIF;
            Lst#Name(WIdxReg).IsEmpty = *OFF;
            WIdxReg += 1;
            WPosIni = WPosFim+1;
         ENDDO;
      /END-FREE
     P DumpStrVars     E

     P*=======================================================
     P* Processo..: $ExtrStringVar
     P* Descrição.: Extrair vairavies de uma String com base
     P*             na Template. Para o efeito recebe como
     P*             parametros a String ou palavra, Template
     P*             junto com o código da Variável a extrair.
     P*
     P*  +--------+--------------------------------------+
     P*  |CodItem | Resultado                            |
     P*  +--------+--------------------------------------+
     P*  |   P    | Código do Produto (A|O|T|*BLANKS)    |
     P*  |   Z    | Código do Produto (1|2)              |
     P*  |   C    | Código do Carregamento ou *BLANKS    |
     P*  |   M    | Mês do nome ou *ZEROS                |
     P*  |   A    | Ano do nome ou *ZEROS                |
     P*  |   G    | Prefixo do nome do Ficheiro          |
     P*  +--------+--------------------------------------+
     P*=======================================================
     P $ExtrStringVar  B                   EXPORT
     D                 PI             4A
     D    FileName                   10A   Const
     D    Template                   40A   Const
     D    CodItem                     1A   Const
     D
     D*==> Campos de Trabalho
     D WIdxReg         S              3S 0
     D WIcExiste       S               N
     D WResultado      S              4A
     D*===========================================================
      /FREE
         //-----------------------------------------------------------
         // Decompor os parâmetros da Template
         //-----------------------------------------------------------
         DumpStrVars (FileName:Template);

         //-----------------------------------------------------------
         // Devolver o Resultado
         //-----------------------------------------------------------
         WIcExiste  = *OFF;
         WIdxReg = 1;
         DOW (NOT Lst#Name(WIdxReg).IsEmpty);
            IF (CodItem = Lst#Name(WIdxReg).Codigo);
               WIcExiste  = *ON;
               WResultado = %SUBST(FileName:Lst#Name(WIdxReg).Posini:
                                            Lst#Name(WIdxReg).Dimens);
               LEAVE;
            ENDIF;
            WIdxReg += 1;
         ENDDO;

         //--------------------------------------------------
         // Tratamento de Excepções
         //--------------------------------------------------
         IF (CodItem = 'A') AND (WResultado <> *BLANKS);
            WResultado = %CHAR(2000 + %DEC(WResultado:2:0));
         ENDIF;
         IF ((CodItem = 'M') OR (CodItem = 'A')) AND (WResultado = *BLANKS);
            WResultado = *ZEROS;
         ENDIF;
         Return WResultado;
      /END-FREE
     P $ExtrStringVar  E

     P*=======================================================
     P* Processo..: $ValStrStruct
     P* Descrição.: Validar se a estrutura da String é
     P*             válida.
     P*=======================================================
     P $ValStrStruct   B                   EXPORT
     D                 PI              N
     D    FileName                   10A   Const
     D    Template                   40A   Const
     D
     D*==> Variáveis de Trabalho
     D WIdxReg         S              3S 0
     D WFileSize       S              2S 0
     D WResultado      S               N
     D WValor          S             10A
     D WValNum         S              2S 0
     D
     D*===========================================================
      /FREE
         //-----------------------------------------------------------
         // Decompor os parâmetros da Template
         //-----------------------------------------------------------
         DumpStrVars (FileName:Template);

         WFileSize = *ZEROS;
         WIdxReg   = 1;
         DOW (NOT Lst#Name(WIdxReg).IsEmpty);
            WFileSize += Lst#Name(WIdxReg).Dimens;
            WValor = %SUBST(FileName:Lst#Name(WIdxReg).Posini:
                                            Lst#Name(WIdxReg).Dimens);
            IF ((Lst#Name(WIdxReg).Codigo='G') OR
                (Lst#Name(WIdxReg).Codigo='K')) AND
                (%TRIM(Lst#Name(WIdxReg).Palavra) <> %TRIM(WValor));
               RETURN *OFF;
            ELSEIF ((Lst#Name(WIdxReg).Codigo='P') AND (WValor<>'A') AND
                (WValor<>'O') AND (WValor<>'T'));
               RETURN *OFF;
            ELSEIF ((Lst#Name(WIdxReg).Codigo='Z') AND (WValor<>'1') AND
                (WValor<>'2'));
               RETURN *OFF;
            ELSEIF (Lst#Name(WIdxReg).Codigo='M');
               MONITOR;
                  WValNum = %DEC(%TRIM(WValor):2:0);
                  IF (WValNum = 0) OR (WValNum > 12);
                     RETURN *OFF;
                  ENDIF;
               ON-ERROR;
                  RETURN *OFF;
               ENDMON;
            ELSEIF (Lst#Name(WIdxReg).Codigo='A');
               MONITOR;
                  WValNum = %DEC(%TRIM(WValor):2:0);
                  IF (WValNum > (%SUBDT(%DATE():*YEARS))-2000+1);
                     RETURN *OFF;
                  ENDIF;
               ON-ERROR;
                  RETURN *OFF;
               ENDMON;
            ENDIF;
            WIdxReg += 1;
         ENDDO;
         IF (%LEN(%TRIM(FileName))<>WFileSize);
            RETURN *OFF;
         ENDIF;
         Return *ON;
      /END-FREE
     P $ValStrStruct   E
