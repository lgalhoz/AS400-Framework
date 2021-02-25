     H*===================================================================
     H* Processo...: $getSqlStmt
     H* Objectivo..: ForCompr uma query DB2/400
     H* Descrição..: Possiblilitar obter segmentos de registos com a
     H*              indicação do registo inicial e a quantidade pedida.
     H*              Util para usar em ecrãs de consulta em lista.
     H*
     H* Parametros.:
     H*        <campos a obter>        Recebe   300 A    (#CAMPOS)
     H*        <Tabelas com ligações>  Recebe   300 A    (#TABELAS)
     H*        <Condições>             Recebe   300 A    (#CONDICAO)
     H*        <Ordenação>             Recebe   100 P 0  (#ORDENACAO)
     H*        <Repetidos>             Recebe     1 P 0  (#Repetidos)
     H*        <Posição Inicial>       Recebe     3 P 0  (#INI)
     H*        <Quantidade>            Recebe     3 P 0
     H*        <Resumo>                Recebe       N
     H*
     H* 1.   Exemplo:
     H* 1.1. Declaração:
     H*         D $getSQLStmt  PR   1000A
     H*         D      Fields        300A   Const
     H*         D      Files         300A   Const
     H*         D      Where         300A   Const
     H*         D      OrderBy       100A   Const
     H*         D      Repetidos       1A   Const
     H*         D      PosIni         10P 0 Const
     H*         D      Qtde            3P 0 Const
     H*         D      Resumo           N
     H*
     H* 1.2. Utilização:
     H*         SQLStmt = $getSQLStmt('FIELD01':'FILE001':'FIELD01 = 01':
     H*                              'ORDER BY FIELD01':1:10:*ON);
     H*
     H*===================================================================
     H NOMAIN
     H**********************************************************************
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010

     D concatFields    PR          1000A
     D    #Fields                  1000A   Const
     P*===================================================================
     P* Processo..: $getSQLStmt
     P* Descrição.: forComp a instrução de SQL para obter segmentos
     P*             de registos
     P*===================================================================
     P $getSQLStmt     B                   EXPORT
     D                 PI          2000A
     D    Fields                   1000A   Const
     D    Files                     300A   Const
     D    Where                     300A   Const
     D    OrderBy                   100A   Const
     D    Repetidos                   1A   Const
     D    PosIni                     10P 0 Const
     D    Qtde                        3P 0 Const Options(*NOPASS)
     D    Resumo                       N   Const Options(*NOPASS)
     D    IsLogical                    N   Const Options(*NOPASS)
     D
     D
     D WhereBk         S                   LIKE(Where)
     D WFiles          S            300A
     D WRepetidos      S            308A
     D WOrderBy        S            100A
     D WSortFld        S            100A
     D WFieldsbk       S           1000A
     D WResult         S           2000A
     D WSQLTmpl        S            300A
     D WPosFim         S             10P 0
     D WQtde           S              3P 0 INZ(12)
     D WResumo         S               N   INZ(*OFF)
     D WIsLogical      S               N   INZ(*OFF)
     C*==============================================================
      /FREE

       IF (%PARMS() >= 7);
          WQtde = Qtde;
          IF (%PARMS() >= 8);
             WResumo = Resumo;
             IF (%PARMS() >= 9);
                WIsLogical = IsLogical;
             ENDIF;
          ENDIF;
       ENDIF;

       //-----------------------------
       // Preparação das variáveis
       //-----------------------------
       //IF (%LEN(%TRIM(Files)) <= 12);
       //   WFiles = '"'+%TRIM(Files)+'"';
       //ELSE;
            WFILES = %TRIM(Files);
       //ENDIF;

       WPosFim = PosIni + Qtde - 1;
       IF (%TRIM(Where) <> *BLANKS);
          Wherebk = 'WHERE ' + %TRIMR(Where);
       ENDIF;

       //----------------------------------------
       // Ordenação dos Campos
       //----------------------------------------
       IF (%TRIM(OrderBy) <> *BLANKS);
          WSortFld = ', ' + %TRIMR(OrderBy);
          WSortFld = $strReplace(' #ASC':*BLANKS:WSortFld);
          WSortFld = $strReplace(' #DESC':*BLANKS:WSortFld);
          WOrderBy = 'ORDER BY ' + %TRIMR(OrderBy);
          WOrderBy = $strReplace(' #DESC':' DESC':WOrderBy);
          WOrderBy = $strReplace(' #ASC':' ASC':WOrderBy);
          IF (%SUBST(WOrderBy:11:1) = '.');
             WOrderBy = 'ORDER BY ' + %SUBST(WOrderBy:12);
          ENDIF;
       ENDIF;

       WRepetidos = *BLANKS;
       IF (Repetidos = 'S');
          WRepetidos = 'GROUP BY ' + %TRIM(Fields);
       ENDIF;

       //-----------------------------
       // ForCompção da Query
       //-----------------------------
       EXSR AddTemplate;
       WResult = WSqlTmpl;
       WFieldsbk = ConcatFields(Fields);
       WResult = $strReplace('#CAMPOS':WFieldsbk:WResult);
       WResult = $strReplace('#TABELAS':WFiles:WResult);
       WResult = $strReplace('#CONDICAO':Wherebk:WResult);
       WResult = $strReplace('#SORTFLD':WSortFld:WResult);
       WResult = $strReplace('#ORDENACAO':WOrderBy:WResult);
       WResult = $strReplace('#REPETIDOS':WRepetidos:WResult);
       WResult = $strReplace('#INI':%CHAR(PosIni):WResult);
       WResult = $strReplace('#FIM':%CHAR(WPosFim):WResult);

       RETURN WResult;

       //=============================================================
       // Subrotina..: addTemplate
       // Objectvo...: Criar um Template nova
       //=============================================================
       BEGSR addTemplate;
          //------------------------------------------------------
          // ForCompr Template para possibilitar page at a time
          //------------------------------------------------------
          IF (NOT Resumo);
             IF (WIsLogical);
                WSQLTmpl = 'WITH TMPJMTBL AS (SELECT ROW_NUMBER() +
                            OVER() AS RRNJM05, #CAMPOS, 1 as RRNJM07 +
                            #SORTFLD FROM #TABELAS #CONDICAO) +
                            SELECT * FROM TMPJMTBL WHERE RRNJM05 BETWEEN +
                            #INI AND #FIM #REPETIDOS #ORDENACAO';
             ELSE;
                WSQLTmpl = 'SELECT * FROM (SELECT ROW_NUMBER() OVER()  +
                            AS RRNJM05, #CAMPOS, 1 as RRNJM07 +
                            #SORTFLD FROM #TABELAS #CONDICAO) AS TBL +
                            WHERE RRNJM05 BETWEEN #INI AND #FIM +
                            #REPETIDOS #ORDENACAO';
             ENDIF;
          ELSE;
             IF (WIsLogical);
                WSQLTmpl = 'WITH TMPJMTBL AS (SELECT ROW_NUMBER() +
                            OVER() AS RRNJM05, #CAMPOS, 1 as RRNJM07 +
                            #SORTFLD FROM #TABELAS #CONDICAO) +
                            SELECT COUNT(*) FROM TMPJMTBL';
             ELSE;
                WSQLTmpl = 'SELECT COUNT(*) FROM #TABELAS #CONDICAO #REPETIDOS';
             ENDIF;
          ENDIF;
       ENDSR;
       //=============================================================
       // Subrotina..: addFldDate
       // Objectvo...: Adicionar uma Template para um campo Data, para
       //              este efeito deverá indicar quais os campos data.
       //=============================================================
       BEGSR addFldDate;
          WSQLTmpl = '(Year(%DATA)*10000+Month(%DATA)*100+Day(%DATA))';
       ENDSR;
      /END-FREE
     P $getSQLStmt     E
     P*===================================================================
     P* Processo..: concatFields
     P* Descrição.: Concatenar Campos
     P*===================================================================
     P concatFields    B                   EXPORT
     D                 PI          1000A
     D    #Fields                  1000A   Const
     D
     D WPosIni         S              5S 0
     D WPosFim         S              5S 0
     D WStrLen         S              5S 0
     D WField          S             25A
     D WAllFields      S           1000A
     D WFirstEntry     S               N   INZ(*ON)
     D WResult         S           1000A   INZ(*BLANKS)
     D*===================================================================
      /FREE
       WAllFields  = #Fields;
       WField      = WAllFields;
       WPosIni     = %SCAN(%TRIM(','):WAllFields);
       WResult     = *BLANKS;
       WFirstEntry = *ON;
       DOW (WPosIni > *ZEROS);
          WPosFim     = WPosIni;
          WPosIni     = 1;
          WStrLen     = WPosFim - WPosIni;
          WField      = %SUBST(WAllFields:1:WStrLen);
          IF (%SUBST(%TRIM(WField):1:1) = '#');
             WField = 'CHAR('+%SUBST(%TRIM(WField):2)+',ISO)';
          ENDIF;
          WAllFields  = %SUBST(WAllFields:(WPosFim+1));
          IF (NOT WFirstEntry);
             WResult     = 'CONCAT('+%TRIM(WResult)+','+%TRIM(WField)+')';
          ELSE;
             WResult     = %TRIM(WField);
             WFirstEntry = *OFF;
          ENDIF;
          WResult     = 'CONCAT('+%TRIM(WResult)+','';'')';
          WPosIni     = %SCAN(%TRIM(','):WAllFields);
       ENDDO;

       IF (WFirstEntry);
          WResult     = %TRIM(WField);
       ELSE;
          IF (%SUBST(%TRIM(WAllFields):1:1) = '#');
             WAllFields = 'CHAR('+%SUBST(%TRIM(WAllFields):2)+',ISO)';
          ENDIF;
          WResult     = 'CONCAT('+%TRIM(WResult)+','+%TRIM(WAllFields)+')';
       ENDIF;
       RETURN WResult;
      /END-FREE
     P concatFields    E
