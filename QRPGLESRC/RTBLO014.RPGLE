     H NOMAIN
     H*=======================================================
     H* Dat.Cria...: 2011-05-12
     H* Autor......: Luis Galhoz
     H* Processo...: ValCodigo
     H* Objectivo..: Validar se o valor inserido para o c�digo
     H*              � v�lido ou est� dentro dos valores pervistos.
     H*
     H* Observa��o.: Devolver um c�digo de mensagem de erro
     H*              TBL0001 - O c�digo n�o existe na Tabela
     H*              TBL0002 - ...
     H*=======================================================
     FFTBLP010  IF   E           K DISK    USROPN
     FFTBLP01202IF   E           K DISK    RENAME(RTBLP012:RTBLP01202)
     F                                     USROPN
     F*===========================================================
     F* Indicadores  Descri��o
     F* -----------  ---------------------------------------------
     F*
     F*=====================================================================
      /Copy QRPGLESRC,STBLO010

     P*====================================================================
     P* Processo..: ValCodStruct
     P* Descri��o.: Validar se o c�digo inserido � v�lido
     P*             Em fun��o das regras definidas na Tabela, realiza
     P*             as seguintes valida��es:
     P*             - Dominio do Campo - Valores Alfa, numerico
     P*             - Dimens�o - Dentro dos interv�los m�ximos e m�nimos
     P*             - Valores nulos - Possibilidade de usar nulos
     P*---------------------------------------------------------------------
     P* Observa��o: Esta fun��o � usada aquando da cria��o de um novo
     P*             c�digo na Tabela de c�digos e antes de inserir um
     P*             registo num ficheiro de dados.
     C*=====================================================================
     P $ValCodStruct   B                   EXPORT
     D                 PI              N
     D    Tabela                      5S 0 CONST
     D    Codigo                     15A   CONST
     D
     D WDominio        S              1A   INZ(*BLANKS)
     D WDimMin         S              5S 0 INZ(*ZEROS)
     D WDimMax         S              5S 0 INZ(*ZEROS)
     D WIcEmpty        S              1A   INZ(*BLANKS)
     D WResult         S               N
      /FREE
        OPEN FTBLP010;
        CHAIN (Tabela) RTBLP010;
        IF (%FOUND());
           WDominio = Dominio;
           WDimMin  = DimMin;
           WDimMax  = DimMax;
           WIcEmpty = IcEmpty;
        // EXSR ValCodigo;
        ELSE;
          RETURN *ON;
        ENDIF;
        CLOSE FTBLP010;

        RETURN WResult;
      /END-FREE
     P $ValCodStruct   E

     P*====================================================================
     P* Processo..: ValCodigo
     P* Descri��o.: Validar se o c�digo inserido � v�lido
     P*             Em fun��o das regras definidas na Tabela, realiza
     P*             as seguintes valida��es:
     P*             - Dominio do Campo - Valores Alfa, numerico
     P*             - Dimens�o - Dentro dos intervalos m�ximos e m�nimos
     P*---------------------------------------------------------------------
     P* Observa��o: Esta fun��o � usada aquando da cria��o de um novo
     P*             c�digo na Tabela de c�digos e antes de inserir um
     P*             registo num ficheiro de dados.
     C*=====================================================================
     P $ValCodigo      B                   EXPORT
     D                 PI              N
     D    Tabela                      5S 0 CONST
     D    Codigo                     15A   CONST
     D    Alias                        N   CONST Options(*NOPASS)
     D
     D WDominio        S              1A   INZ(*BLANKS)
     D WIcEmpty        S              1A   INZ(*BLANKS)
     D WResult         S               N   INZ(*OFF)
     D*===========================================================
      /FREE
        OPEN FTBLP010;
        CHAIN (Tabela) RTBLP010;
        IF (NOT %FOUND());
           WResult = *OFF; // Erro por inexist�ncia da Tabela
        ENDIF;

        WDominio = Dominio;
        WIcEmpty = IcEmpty; // Zero para numericos e espa�o para alfa
        EXSR ValCodigo;
        CLOSE FTBLP010;
        RETURN WResult;

       //==============================================================
       // Subrotina.: ValCodigo
       // Objectivo.: Validar se o C�digo � v�lida para a Tabela
       //==============================================================
       BEGSR ValCodigo;
          // Validar se o campo � nulo
          IF ((%TRIM(CODIGO) = *BLANKS) AND (WIcEmpty = 'S') AND
             (WDominio = 'A')) OR
             ((%TRIM(CODIGO) = '0') AND (WIcEmpty = 'S') AND
             (WDominio = 'N'));
             WResult = *ON; // Pode aceitar valores nulos
          ELSE;
             OPEN FTBLP01202;
             CHAIN (TABELA:Codigo) RTBLP01202;
             IF (%FOUND());
                WResult = *ON; // O c�digo existe na tabela
             ELSE;
                WResult = *OFF;  // O c�digo n�o existe na tabela
             ENDIF;
             CLOSE FTBLP01202;
          ENDIF;
       ENDSR;
      /END-FREE
     P $ValCodigo      E
