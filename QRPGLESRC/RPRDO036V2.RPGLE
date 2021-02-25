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
     D
     D*==> Parâmetros de entrada
     D PCategoria      S              1A
     D PDatNovo        S              8  0
     D PDatActual      S              8  0
     D PValNovo        S             13  2
     D PValActual      S             13  2
     D WCodErro        S              7A
     D*===================================================================
     C     *ENTRY        PLIST
     C                   PARM                    PCategoria
     C                   PARM                    PDatNovo
     C                   PARM                    PDatActual
     C                   PARM                    PValNovo
     C                   PARM                    PValActual
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
              WDatNovo = $IntToDate(PDatNovo);
           ON-ERROR;
              WCodErro   = 'PRDxxxx';
              LEAVESR;
           ENDMON;

           //-------------------------------------------------
           // O Valor em novo é obrigatório
           //-------------------------------------------------
           MONITOR;
              WValNovo = PValNovo;
           ON-ERROR;
              WValNovo = *ZEROS;
           ENDMON;

           //-------------------------------------------------
           // A Data do Actual assume o dia por defeito
           //-------------------------------------------------
           MONITOR;
              WDatActual = $IntToDate(PDatActual);
           ON-ERROR;
              WDatActual = %DATE();
           ENDMON;

           //-------------------------------------------------
           // O Valor actual é forCompdo com zeros
           //-------------------------------------------------
           MONITOR;
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
