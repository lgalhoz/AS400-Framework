     H*=======================================================
     H* Dat.Cria...: 2012-03-29
     H* Autor......: Luis Galhoz
     H* Biblioteca.: RDBSO010
     H* Objectivo..: FunÁıes de Leitura aos Dados da Base de Dados
     H*=======================================================
      /Copy QRPGLESRC,SDBSO010
     D*==> DeclaraÁ„o de Vari·veis Globais
     D Lst#Data        DS                  QUALIFIED DIM(25)
     D   F#Rec                        9S 0
     D   F#Data                     300A
     D   F#Fill                        N
     D WIdx            S              2S 0
     D
     D WFields         S            300A   INZ(*BLANKS)
     D WFiles          S            300A   INZ(*BLANKS)
     D WWhere          S            300A   INZ(*BLANKS)
     D WOrderBy        S            100A   INZ(*BLANKS)
     D WActRec         S              9S 0 INZ(*ZEROS)
     D WQtdRec         S              9S 0 INZ(*ZEROS)
     D WMaxRec         S              9S 0 INZ(*ZEROS)
     D WIsEOF          S               N   INZ(*ON)
     D StrLo           C                   'abcdefghijklmnopqrstuvwxyzÁ«+
     D                                     „√·¡‚¬‡¿È…Ë»Í ÌÕÏÃÓŒÛ”Ú“ı’Ù‘˙⁄˘Ÿ˚€+
     D                                     /,-'
     D StrUp           C                   'ABCDEFGHIJKLMNOPQRSTUVWXYZCC+
     D                                     AAAAAAAAEEEEEEIIIIIIOOOOOOOOUUUUUU+
     D                                     ...'
     D*==============================================================
      /FREE
         *INLR = *ON;
         RETURN;
      /END-FREE
     P*=======================================================
     P* Processo..: SetQuery
     P* DescriÁ„o.: Preencher as vari·veis globais com valores
     C*=======================================================
     P SetQuery        B                   EXPORT
     D SetQuery        PI
     D    Fields                    300A   Const
     D    Files                     300A   Const
     D    Where                     300A   Const
     D    OrderBy                   100A   Const
     D*===========================================================
      /FREE
       WFields  = Fields;
       WFiles   = Files;
       WWhere   = Where;
       WOrderBy = OrderBy;
      /END-FREE
     P SetQuery        E

     P*=======================================================
     P* Processo..: SetFiles
     P* DescriÁ„o.: Formatar com  valores o parametro Ficheiros
     C*=======================================================
     P SetFiles        B                   EXPORT
     D SetFiles        PI
     D    Files                     300A   Const
     D*===========================================================
      /FREE
       WFiles   = Files;
      /END-FREE
     P SetFiles        E

     P*=======================================================
     P* Processo..: SetFields
     P* DescriÁ„o.: Formatar com  valores o parametro Ficheiros
     C*=======================================================
     P SetFields       B                   EXPORT
     D SetFields       PI
     D    Fields                    300A   Const
     D*===========================================================
      /FREE
       WFields  = Fields;
      /END-FREE
     P SetFields       E

     P*=======================================================
     P* Processo..: SetSort
     P* DescriÁ„o.: Formatar com  valores o parametro Ficheiros
     C*=======================================================
     P SetSort         B                   EXPORT
     D SetSort         PI
     D    OrderBy                   100A   Const
     D*===========================================================
      /FREE
       WOrderBy = OrderBy;
      /END-FREE
     P SetSort         E

     P*=======================================================
     P* Processo..: SetCriteria
     P* DescriÁ„o.: Formatar com  valores o parametro Ficheiros
     C*=======================================================
     P SetCriteria     B                   EXPORT
     D SetCriteria     PI
     D    Where                     300A   Const
     D*===========================================================
      /FREE
       WWhere   = Where;
      /END-FREE
     P SetCriteria     E

     P*=======================================================
     P* Processo..: getRecord
     P* DescriÁ„o.: Formatar com  valores o parametro Ficheiros
     C*=======================================================
     P GetRecord       B                   EXPORT
     D GetRecord       PI             5S 0
     D    RegIni                      9S 0 Const
     D    QtdReg                      3S 0 Const
     D
     D WSQLStmt        S           1000A
     D WResult         S              5S 0 INZ(*ZEROS)
     D*===========================================================
      /FREE

       //------------------------------------------------
       // Inicalizar lista de Trabalho
       //------------------------------------------------
       WIdx = 1;
       DOW (WIdx < 25);
          Lst#Data(WIdx).F#Rec  = *ZEROS;
          Lst#Data(WIdx).F#Data = *BLANKS;
          Lst#Data(WIdx).F#Fill = *OFF;
          WIdx += 1;
       ENDDO;

       //------------------------------------------------
       // Seleccionar os registos a apresentar
       //------------------------------------------------
       WIsEof  = *ON;
       WActRec = *ZEROS;
       WSqlStmt = getSQLStmt(%TRIM(WFields):%TRIM(WFiles):
                             %TRIM(WWhere):%TRIM(WOrderBy):RegIni:QtdReg:*Off);
       EXEC SQL PREPARE STMREC FROM :WSqlStmt;
       EXEC SQL DECLARE REC CURSOR FOR STMREC;
       EXEC SQL OPEN REC;
       DOW (sqlcod = 0);
          EXEC SQL FETCH NEXT FROM REC FOR :QtdReg ROWS INTO: Lst#Data;
          WIsEof  = *OFF;
       ENDDO;
       EXEC SQL CLOSE REC;

       //------------------------------------------------
       // Formatar lista com os registos
       //------------------------------------------------
       IF (NOT WIsEof);
          WIdx = 1;
          DOW (WIdx < QtdReg);
             IF (NOT Lst#Data(WIdx).F#Fill);
                WIdx -= 1;
                LEAVE;
             ENDIF;
             WIdx += 1;
          ENDDO;
          WQtdRec = WIdx;
          WActRec = 1;
       ENDIF;

       RETURN WResult;
      /END-FREE
     P GetRecord       E

     P*=======================================================
     P* Processo..: getFirst
     P* DescriÁ„o.: Posicionar no Primeiro Registo
     C*=======================================================
     P GetFirst        B                   EXPORT
     D GetFirst        PI
     D*===========================================================
      /FREE
        WIsEof  = *OFF;
        IF (WQtdRec = *ZEROS);
          WActRec = *ZEROS;
          WIsEof  = *ON;
        ENDIF;
      /END-FREE
     P GetFirst        E
     P*=======================================================
     P* Processo..: getLast
     P* DescriÁ„o.: Posicionar no ⁄ltimo Registo
     C*=======================================================
     P GetLast         B                   EXPORT
     D GetLast         PI
     D*===========================================================
      /FREE
        WIsEof  = *ON;
        IF (WQtdRec > *ZEROS);
           WIsEof  = *OFF;
        ENDIF;
        WActRec = WQtdRec;
      /END-FREE
     P GetLast         E
     P*=======================================================
     P* Processo..: getNext
     P* DescriÁ„o.: Posicionar no Registo Seguinte
     C*=======================================================
     P GetNext         B                   EXPORT
     D GetNext         PI
     D*===========================================================
      /FREE
        WIsEof  = *ON;
        IF (WActRec < WQtdRec);
           WActRec += 1;
           WIsEof  = *OFF;
        ENDIF;
      /END-FREE
     P GetNext         E
     P*=======================================================
     P* Processo..: getPrevious
     P* DescriÁ„o.: Posicionar no Registo Anterior
     C*=======================================================
     P GetPrevious     B                   EXPORT
     D GetPrevious     PI
     D*===========================================================
      /FREE
        WIsEof  = *ON;
        IF (WActRec > WQtdRec);
           WActRec -= 1;
           WIsEof  = *OFF;
        ENDIF;
      /END-FREE
     P GetPrevious     E
     P*=======================================================
     P* Processo..: getField
     P* DescriÁ„o.: Obter o Valor para o Campo
     C*=======================================================
     P GetField        B                   EXPORT
     D GetField        PI           150A
     D    FldName                    15A   Const
     D
     D WPosIni         S              3S 0 INZ(*ZEROS)
     D WPosFim         S              3S 0 INZ(*ZEROS)
     D WStrLen         S              3S 0 INZ(*ZEROS)
     D WPosField       S              3S 0 INZ(*ZEROS)
     D WField          S             15A   INZ(*BLANKS)
     D WPosFieldBk     S              3S 0 INZ(*ZEROS)
     D WFieldsBk       S                   LIKE(WFields)
     D WF#Data         S            300A   INZ(*BLANKS)
     D WResult         S            150A   INZ(*BLANKS)
     D*===========================================================
      /FREE
        IF (WIsEof);
           RETURN WResult;
        ENDIF;

        //----------------------------------------------------------
        // Validar a forma de acesso, por nome ou posiÁ„o
        //----------------------------------------------------------
        MONITOR;
           WPosField = %INT(FldName);
        ON-ERROR;
            EXSR getPosIni;
        ENDMON;

        //----------------------------------------------------------
        // Extrair o valor do Campo
        //----------------------------------------------------------
        IF (WPosField = *ZEROS);
           WResult = *BLANKS;
        ELSE;
           EXSR getFieldValue;
        ENDIF;

        RETURN WResult;

        //==========================================================
        // Subrotina..: getPosIni
        // Objectivo..: Obter o n˙mero da posiÁ„o do Campo
        //==========================================================
        BEGSR getPosIni;
           WPosField = 1;
           WFieldsBk = WFields;
           WPosIni   = %SCAN(%TRIM(','):WFieldsBk);
           DOW (WPosIni > *ZEROS);
              WPosFim   = WPosIni;
              WPosIni   = 1;
              WStrLen   = WPosFim - WPosIni;
              WField    = %SUBST(WFieldsBk:1:WStrLen);
              WFieldsBk = %SUBST(WFieldsBk:(WPosFim+1));
              IF (%TRIM(WField) = %TRIM(FldName));
                 LEAVE;
              ENDIF;
              WPosIni = %SCAN(%TRIM(','):WFieldsBk);
              WPosField+= 1;
           ENDDO;

           // %XLATE(StrLo:StrUp(WFieldsBk)
           IF (%TRIM(WFieldsBk) <> %TRIM(FldName)) AND
              (%TRIM(WField) <> %TRIM(FldName));
              WPosField = *ZEROS;
           ENDIF;
        ENDSR;

        //==========================================================
        // Subrotina..: getFieldValue
        // Objectivo..: Obter o valor para o Campo
        //==========================================================
        BEGSR getFieldValue;
           WPosFieldBk = 1;
           WF#Data     = Lst#Data(WActRec).F#Data;
           WPosIni     = %SCAN(%TRIM(';'):WF#Data);
           DOW (WPosIni > *ZEROS);
              WPosFim   = WPosIni;
              WPosIni   = 1;
              WStrLen   = WPosFim - WPosIni;
              IF (WPosFieldBk = WPosField);
                 WF#Data = %SUBST(WF#Data:1:WStrLen);
                 LEAVE;
              ELSE;
                 WF#Data = %SUBST(WF#Data:(WPosFim+1));
              ENDIF;
              WPosIni = %SCAN(%TRIM(';'):WF#Data);
              WPosFieldBk += 1;
           ENDDO;

           IF (WPosFieldBk = WPosField);
              WResult = WF#Data;
           ENDIF;
        ENDSR;
      /END-FREE
     P GetField        E

     P*=======================================================
     P* Processo..: isEOF
     P* DescriÁ„o.: Validar se Chegou ao fim do Ficheiro
     C*=======================================================
     P IsEof           B                   EXPORT
     D IsEof           PI              N
     D
     D WResult         S               N
     D*===========================================================
      /FREE
        WResult = WIsEof;
        RETURN WResult;
      /END-FREE
     P IsEof           E

     P*=======================================================
     P* Processo..: GetQtdeReg
     P* DescriÁ„o.: Obter o total de registos
     C*=======================================================
     P GetQtdeReg      B                   EXPORT
     D GetQtdeReg      PI             9S 0
     D
     D WSQLStmt        S           1000A
     D WResult         S              9S 0
     D*===========================================================
      /FREE

       //------------------------------------------------
       // Seleccionar os registos a apresentar
       //------------------------------------------------
       WActRec = *ZEROS;
       WSqlStmt = getSQLStmt(%TRIM(WFields):%TRIM(WFiles):
                             %TRIM(WWhere):%TRIM(WOrderBy):1:1:*ON);
       EXEC SQL PREPARE STMREC2 FROM :WSqlStmt;
       EXEC SQL DECLARE REC2 CURSOR FOR STMREC2;
       EXEC SQL OPEN REC2;
       DOW (sqlcod = 0);
          EXEC SQL FETCH REC2 INTO: WResult;
       ENDDO;
       EXEC SQL CLOSE REC2;

       RETURN WResult;
      /END-FREE
     P GetQtdeReg      E
