     H NOMAIN
     H*==================================================================
     H* Função....: RGRLO028
     H* Descrição.: Validar a Estrutura de uma String
     H*             Utiliza Expressões regulares. Para o efeito invoca
     H*             funções de Java.
     H*
     H*             Para testar as Expressões Regulares, usar:
     H*                https://regex101.com
     H*
     H*==================================================================
      /include QSYSINC/QRPGLESRC,JNI
     D newString       pr              O   CLASS(*JAVA:'java.lang.String')
     D                                     EXTPROC(*JAVA:'java.lang.String':
     D                                     *CONSTRUCTOR)
     D    bytearray               32767A   VARYING CONST
     D getBytes        PR         65535A    VARYING
     D                                      EXTPROC(*JAVA:
     D                                       'java.lang.String':
     D                                       'getBytes')
     D PatternCompile  pr              O   CLASS(*JAVA:
     D                                     'java.util.regex.Pattern')
     D                                     EXTPROC(*JAVA:
     D                                     'java.util.regex.Pattern':
     D                                     'compile') STATIC
     D    pattern                      O   CLASS(*JAVA:'java.lang.String')
     D PatternMatcher  pr              O   CLASS(*JAVA:
     D                                     'java.util.regex.Matcher')
     D                                     EXTPROC(*JAVA:
     D                                     'java.util.regex.Pattern':
     D                                     'matcher')
     D    comparestr                   O   CLASS(*JAVA
     D                                     :'java.lang.CharSequence')
     D CheckMatches    pr             1N   EXTPROC(*JAVA
     D                                     :'java.util.regex.Matcher'
     D                                     :'matches')
     D DoReplace       pr              O   CLASS(*JAVA:'java.lang.String')
     D                                     EXTPROC(*JAVA
     D                                     :'java.util.regex.Matcher'
     D                                     :'replaceAll')
     D    replacement                  O   CLASS(*JAVA
     D                                     :'java.lang.String')
     D RegExPattern    s               O   CLASS(*JAVA:
     D                                      'java.util.regex.Pattern')
     D RegExMatcher    s               O   CLASS(*JAVA:
     D                                     'java.util.regex.Matcher')
     D jstrStmt        s                   like(jstring)
     D jPatStr         s                   like(jstring)
     D jRepStr         s                   like(jstring)
     D jRepStr2        s                   like(jstring)
     D result          S             30A
     D*==========================================================

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
     P* Processo..: $ValStrStructr
     P* Descrição.: Validar se a contrução é válida.
     P*
     P*=======================================================
     P $ValStrStruct   B                   EXPORT
     D                 PI              N
     D    FileName                   10A   Const
     D    Template                   40A   Const
     D
     D*===========================================================
      /free
        jPatStr = newString(%TRIM(Template));
        jstrStmt = newString(%TRIM(FileName));

        RegExPattern = PatternCompile(jPatStr);
        RegExMatcher = PatternMatcher(RegExPattern : jstrStmt);
        if (CheckMatches(RegExMatcher) = *ON);
            return *ON;
        else;
            return *OFF;
        endif;

        // jRepStr = newString('$2');
        // jRepStr2 = DoReplace(RegExMatcher : jRepStr);
        // result = getBytes(jRepStr2);
        // dsply (%subst(result : 1 : 30));
      /end-free
