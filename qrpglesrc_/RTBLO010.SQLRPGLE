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
         WSqlStmt = 'SELECT DescCod FTBLP012 WHERE +
                      (WCdIndice = ' + %CHAR(WCdIndice) + ') ' +
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
     P                 E
