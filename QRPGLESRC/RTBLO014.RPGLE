     H NOMAIN
     H*=======================================================
     H* Dat.Cria...: 2011-05-12
     H* Autor......: Luis Galhoz
     H* Processo...: ValCodigo
     H* Objectivo..: Validar se o valor inserido para o código
     H*              é válido ou está dentro dos valores pervistos.
     H*
     H* Observação.: Devolver um código de mensagem de erro
     H*              TBL0001 - O código não existe na Tabela
     H*              TBL0002 - ...
     H*=======================================================
     FFTBLP010  IF   E           K DISK    USROPN
     FFTBLP01202IF   E           K DISK    RENAME(RTBLP012:RTBLP01202)
     F                                     USROPN
     F*===========================================================
     F* Indicadores  Descrição
     F* -----------  ---------------------------------------------
     F*
     F*=====================================================================
      /Copy QRPGLESRC,STBLO010

     P*====================================================================
     P* Processo..: ValCodStruct
     P* Descrição.: Validar se o código inserido é válido
     P*             Em função das regras definidas na Tabela, realiza
     P*             as seguintes validações:
     P*             - Dominio do Campo - Valores Alfa, numerico
     P*             - Dimensão - Dentro dos interválos máximos e mínimos
     P*             - Valores nulos - Possibilidade de usar nulos
     P*---------------------------------------------------------------------
     P* Observação: Esta função é usada aquando da criação de um novo
     P*             código na Tabela de códigos e antes de inserir um
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
     P* Descrição.: Validar se o código inserido é válido
     P*             Em função das regras definidas na Tabela, realiza
     P*             as seguintes validações:
     P*             - Dominio do Campo - Valores Alfa, numerico
     P*             - Dimensão - Dentro dos intervalos máximos e mínimos
     P*---------------------------------------------------------------------
     P* Observação: Esta função é usada aquando da criação de um novo
     P*             código na Tabela de códigos e antes de inserir um
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
           WResult = *OFF; // Erro por inexistência da Tabela
        ENDIF;

        WDominio = Dominio;
        WIcEmpty = IcEmpty; // Zero para numericos e espaço para alfa
        EXSR ValCodigo;
        CLOSE FTBLP010;
        RETURN WResult;

       //==============================================================
       // Subrotina.: ValCodigo
       // Objectivo.: Validar se o Código é válida para a Tabela
       //==============================================================
       BEGSR ValCodigo;
          // Validar se o campo é nulo
          IF ((%TRIM(CODIGO) = *BLANKS) AND (WIcEmpty = 'S') AND
             (WDominio = 'A')) OR
             ((%TRIM(CODIGO) = '0') AND (WIcEmpty = 'S') AND
             (WDominio = 'N'));
             WResult = *ON; // Pode aceitar valores nulos
          ELSE;
             OPEN FTBLP01202;
             CHAIN (TABELA:Codigo) RTBLP01202;
             IF (%FOUND());
                WResult = *ON; // O código existe na tabela
             ELSE;
                WResult = *OFF;  // O código não existe na tabela
             ENDIF;
             CLOSE FTBLP01202;
          ENDIF;
       ENDSR;
      /END-FREE
     P $ValCodigo      E
