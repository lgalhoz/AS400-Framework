     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*===================================================================
     H* Biblioteca.: RPRDO036
     H* Objectivo..: Ddevolver o Capital da viatura à data do Sinistro
     H*===================================================================
      /Copy QRPGLESRC,SPRDO002
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SGRLO010

     D*==> Campos de Trabalho
     D WDBSession      S              5S 0
     D WQtdeMeses      S              3S 0
     D WCategoria      S              1A
     D WDatNovo        S               D
     D WDatActual      S               D
     D WValNovo        S             13S 2
     D WValActual      S             13S 2
     D WMesActual      S              3S 0
     D WTaxaActual     S              7S 3
     D WTaxaAcumul     S              7S 3
     D WTaxa           S              7S 3
     D WIdx            S              3S 0
     D WEspacos        S             13A
     D
     D*==> Parâmetros de entrada
     D PCategoria      S              1A
     D PDatNovo        S              8  0
     D PDatActual      S              8  0
     D PValNovo        S             13  2
     D PValActual      S             13  2
     D WCodErro        S              7A
     D PADatNovo       S              8A
     D PADatActual     S              8A
     D PAValNovo       S             13A
     D PAValActual     S             13A
     D*===================================================================
     C     *ENTRY        PLIST
     C                   PARM                    PCategoria
     C                   PARM                    PADatNovo
     C                   PARM                    PADatActual
     C                   PARM                    PAValNovo
     C                   PARM                    PAValActual
     C                   PARM                    WCodErro
      /FREE
        //------------------------------------------------------
        // Trabalhar os Parâmetros Recebidos
        //------------------------------------------------------
        EXSR ValParms;

        //------------------------------------------------------
        // Obter a Taxa de desvalorização
        //------------------------------------------------------
        IF (WCodErro = *BLANKS);
           WQtdeMeses  = %DIFF(WDatActual:WDatNovo:*MONTHS) + 1;
           WTaxaActual = *ZEROS;
           WTaxaAcumul = *ZEROS;
           WTaxa       = *ZEROS;
           WDBSession  = $SetFiles('FPRDP200');
           $SetCriteria('(MESINICIO <= ''' + %CHAR(WQtdeMeses) + ''') AND +
                         (MESTERMO >= ''' + %CHAR(WQtdeMeses) + ''') AND +
                         (CATEGORI = ''' + %TRIM(WCategoria) + ''')');
           IF ($GetRecord() > *ZEROS);
              WTaxaActual = $GetFieldNum('VALTAXA');
              WTaxaAcumul = $GetFieldNum('VALACUM');
              WQtdeMeses  = WQtdeMeses - $GetFieldNum('MESINICIO') + 1;
              WTaxa       = (WQtdeMeses * WTaxaActual + WTaxaAcumul) / 100;
           ENDIF;
           $RmvDBSession(WDBSession);

           IF (WValActual = *ZEROS);
              WValActual = WValNovo * (1 - WTaxa);
           ELSE;
              WValNovo =  WValActual / (1 - WTaxa);
           ENDIF;
        ENDIF;
        PValActual = WValActual;
        PValNovo   = WValNovo;

        PADatNovo = %CHAR(PDatNovo);
        PADatActual = %CHAR(PDatActual);
        PAValActual = %CHAR(PValActual);
        PAValNovo = %CHAR(PValNovo);

        WIdx = %SCAN('.':%TRIM(PAValActual));
        IF (WIdx > *ZEROS);
           PAValActual = %SUBST(%TRIM(PAValActual):1:(WIdx-1)) +
                         %SUBST(%TRIM(PAValActual):(WIdx+1));
        ENDIF;
        WIdx = %SCAN('.':%TRIM(PAValNovo));
        IF (WIdx > *ZEROS);
           PAValNovo = %SUBST(%TRIM(PAValNovo):1:(WIdx-1)) +
                         %SUBST(%TRIM(PAValNovo):(WIdx+1));
        ENDIF;
        WEspacos = *BLANKS;
        WIdx = %LEN(%TRIM(PAValActual));
        PAValActual = %SUBST(WEspacos:1:(WIdx+1)) + %TRIM(PAValActual);
        WIdx = %LEN(%TRIM(PAValNovo));
        PAValNovo   = %SUBST(WEspacos:1:(WIdx+1)) + %TRIM(PAValNovo);
        RETURN;

        //==============================================================
        // Subrotina.: ValParms
        // Objectivo.: Validar os parâmetros recebidos
        //==============================================================
        BEGSR ValParms;
           WCodErro = *BLANKS;

           //-------------------------------------------------
           // A Categoria é de preenchimento obrigatório
           //-------------------------------------------------
           MONITOR;
              WCategoria = PCategoria;
           ON-ERROR;
              WCategoria = *BLANKS;
              WCodErro   = 'PRDxxxx';
              LEAVESR;
           ENDMON;

           //-------------------------------------------------
           // A Data da viatura em Novo é obrigatória
           //-------------------------------------------------
           MONITOR;
              PDatNovo = %DEC(PADatNovo:8:0);
              WDatNovo = $IntToDate(PDatNovo);
           ON-ERROR;
              WCodErro   = 'PRDxxxx';
              LEAVESR;
           ENDMON;

           //-------------------------------------------------
           // O Valor em novo é obrigatório
           //-------------------------------------------------
           MONITOR;
              PValNovo = %DEC(PAValNovo:13:2);
              IF (%SCAN('.':%TRIM(PAValNovo)) = 0) AND
                (%SCAN(',':%TRIM(PAValNovo)) = 0);
                 PValNovo = PValNovo / 100;
              ENDIF;
              WValNovo = PValNovo;
           ON-ERROR;
              WValNovo = *ZEROS;
           ENDMON;

           //-------------------------------------------------
           // A Data do Actual assume o dia por defeito
           //-------------------------------------------------
           MONITOR;
              PDatActual = %DEC(PADatActual:8:0);
              WDatActual = $IntToDate(PDatActual);
           ON-ERROR;
              WDatActual = %DATE();
           ENDMON;

           //-------------------------------------------------
           // O Valor actual é forCompdo com zeros
           //-------------------------------------------------
           MONITOR;
              PValActual = %DEC(PAValActual:13:2);
              IF (%SCAN('.':%TRIM(PAValActual)) = 0) AND
                (%SCAN(',':%TRIM(PAValActual)) = 0);
                 PValActual = PValActual / 100;
              ENDIF;
              WValActual = PValActual;
           ON-ERROR;
              WValActual = *ZEROS;
           ENDMON;

           //-------------------------------------------------
           // Validar se existem valores para o cálculo
           //-------------------------------------------------
           IF (WValActual = WValNovo) AND (WValNovo = *ZEROS);
              WCodErro   = 'PRDxxxx';
           ENDIF;
        ENDSR;
      /END-FREE
