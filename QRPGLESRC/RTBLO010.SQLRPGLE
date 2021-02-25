     H NOMAIN
      *=======================================================
      * Dat.Cria...: 2011-05-12
      * Autor......: Luis Galhoz
      * Processo...: GetDescricao
      * Objectivo..: Obter a descrição para um código de uma
      *              Tabela de Códigos.
      *=======================================================
      /Copy QRPGLESRC,STBLO010

     P*=======================================================
     P* Função....: $GetDescricao
     P* Descricao.:
     P*=======================================================
     P $getDescricao   B                   EXPORT
     D                 PI            40A
     D    WCdIndice                   5S 0 CONST
     D    WCodigo                    15A   CONST
     D    WAlias                       N   CONST Options(*NOPASS)
     D WDescCod        S             40A
     D WSqlStmt        S            300A
     D WSelectAdd      S             50A
     F*=====================================================================
     C
     C                   EXSR      GetRecords
     C
     C                   RETURN    WDescCod
      /FREE
       //============================================================
       // Subrotina..: GetRecords
       // Objectivo..: Obter a descrição para um Código
       //============================================================
       BEGSR GetRecords;
         WDescCod = 'Inválido';
         IF (%PARMS() > 2);
            WSelectAdd = 'AND (ALIAS = ''' + %TRIM(WCodigo) + ''')';
         ELSE;
            WSelectAdd = 'AND (CODIGO = ''' + %TRIM(WCodigo) + ''')';
         ENDIF;
         WSqlStmt = 'SELECT DescCod FROM FTBLP012 WHERE +
                      (CdIndice = ' + %CHAR(WCdIndice) + ') ' +
                      WSelectAdd;
         EXEC SQL PREPARE STMTC1 FROM :WSqlStmt;
         EXEC SQL DECLARE C1 CURSOR FOR STMTC1;
         EXEC SQL OPEN C1;
         IF (sqlcod = 0);
            EXEC SQL FETCH C1 INTO :WDescCod;
         ENDIF;
         EXEC SQL CLOSE C1;
       ENDSR;
      /END-FREE
     P$getDescricao    E

     P*=======================================================
     P* Função....: $GetDescAbrev
     P* Descricao.:
     P*=======================================================
     P $getDescAbrev   B                   EXPORT
     D                 PI            40A
     D    WCdIndice                   5S 0 CONST
     D    WCodigo                    15A   CONST
     D    WAlias                       N   CONST Options(*NOPASS)
     D WDescCod        S             40A
     D WSqlStmt        S            300A
     D WSelectAdd      S             50A
     F*=====================================================================
     C
     C                   EXSR      GetRecords
     C
     C                   RETURN    WDescCod
      /FREE
       //============================================================
       // Subrotina..: GetRecords
       // Objectivo..: Obter a descrição para um Código
       //============================================================
       BEGSR GetRecords;
         WDescCod = 'Inválido';
         IF (%PARMS() > 2);
            WSelectAdd = 'AND (ALIAS = ''' + %TRIM(WCodigo) + ''')';
         ELSE;
            WSelectAdd = 'AND (CODIGO = ''' + %TRIM(WCodigo) + ''')';
         ENDIF;
         WSqlStmt = 'SELECT DESCABRV FROM FTBLP012 WHERE +
                      (CdIndice = ' + %CHAR(WCdIndice) + ') ' +
                      WSelectAdd;
         EXEC SQL PREPARE STMTC2 FROM :WSqlStmt;
         EXEC SQL DECLARE C2 CURSOR FOR STMTC2;
         EXEC SQL OPEN C2;
         IF (sqlcod = 0);
            EXEC SQL FETCH C2 INTO :WDescCod;
         ENDIF;
         EXEC SQL CLOSE C2;
       ENDSR;
      /END-FREE
     P$getDescAbrev    E

     P*====================================================================
     P* Processo..: ValCodigo
     P* Descrição.: Validar se o código existe na Tabela de Códigos.
     C*=====================================================================
     P $ValCodigoTbl   B                   EXPORT
     D                 PI              N
     D    Tabela                      5S 0 CONST
     D    Codigo                     15A   CONST
     D    Alias                        N   CONST Options(*NOPASS)
     D
     D WDescCod        S             40A
     D WSqlStmt        S            300A
     D WSelectAdd      S             50A
     D WResult         S               N   INZ(*OFF)
     D*===========================================================
      /FREE
         WResult = *OFF; // Não existe
         IF (%PARMS() > 2);
            WSelectAdd = 'AND (ALIAS = ''' + %TRIM(Codigo) + ''')';
         ELSE;
            WSelectAdd = 'AND (CODIGO = ''' + %TRIM(Codigo) + ''')';
         ENDIF;
         WSqlStmt = 'SELECT DescCod FROM FTBLP012 WHERE +
                      (CdIndice = ' + %CHAR(Tabela) + ') ' +
                      WSelectAdd;
         EXEC SQL PREPARE STMTC3 FROM :WSqlStmt;
         EXEC SQL DECLARE C3 CURSOR FOR STMTC3;
         EXEC SQL OPEN C3;
         IF (sqlcod = 0);
            EXEC SQL FETCH C3 INTO :WDescCod;
         ENDIF;
         EXEC SQL CLOSE C3;
         IF (WDescCod <> *BLANKS);
            WResult = *ON; // O Código existe na Tabela
         ENDIF;
         RETURN WResult;
      /END-FREE
     P $ValCodigoTbl   E

      *=======================================================
      * Processo...: GetCodigo
      * Objectivo..: Obter o ALIAS de um Código ou o Código
      *              Associado a um ALIAS de uma Tabela.
     P*=======================================================
     P $getCodigo      B                   EXPORT
     D $getCodigo      PI            15A
     D    PCdIndice                   5S 0 CONST
     D    PCodigo                    15A   CONST
     D    PAlias                       N   CONST Options(*NOPASS)
     D
     D WCodigo         S             15A
     D WSqlStmt        S            300A
     D WSelectAdd      S             50A
     D*=====================================================================
      /FREE
         WCodigo = *BLANKS;
         IF (%PARMS() > 2);
            WSqlStmt = 'SELECT CODIGO FROM FTBLP012 WHERE +
                        (CdIndice = ' + %CHAR(PCdIndice) + ') ' +
                        'AND (ALIAS = ''' + %TRIM(PCodigo) + ''')';
         ELSE;
            WSqlStmt = 'SELECT ALIAS FROM FTBLP012 WHERE +
                        (CdIndice = ' + %CHAR(PCdIndice) + ') ' +
                        'AND (CODIGO = ''' + %TRIM(PCodigo) + ''')';
         ENDIF;
         EXEC SQL PREPARE STMTC4 FROM :WSqlStmt;
         EXEC SQL DECLARE C4 CURSOR FOR STMTC4;
         EXEC SQL OPEN C4;
         IF (sqlcod = 0);
            EXEC SQL FETCH C4 INTO :WCodigo;
         ENDIF;
         EXEC SQL CLOSE C4;

         RETURN WCodigo;
      /END-FREE
     P $getCodigo      E
