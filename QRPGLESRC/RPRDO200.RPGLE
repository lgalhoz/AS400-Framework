     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*===================================================================
     H* Biblioteca.: RPRDO200
     H* Objectivo..: Cálculo do Seguro Automóvel
     H*===================================================================
      /Copy QRPGLESRC,SPRDO002
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SGRLO010

     D*==> Campos de Trabalho
     D WDBSession      S              5S 0
     D WQtdeMeses      S              3S 0
     D WCategoria      S              1A
     D WDatNovo        S              8S 0
     D WDatSinistro    S              8S 0
     D WValNovo        S             13S 2
     D WValActual      S             13S 2
     D PCategoria      S              1A
     D PDatNovo        S              8S 0
     D PDatSinistro    S              8S 0
     D PValNovo        S             13S 2
     D PValActual      S             13S 2
     D WMesActual      S              3S 0
     D WTaxaActual     S              7S 3
     D WTaxaAcumul     S              7S 3
     D WTaxa           S              7S 3
     D WCodErro        S              7A
     D*===================================================================
     C     *ENTRY        PLIST
     C                   PARM                    PCategoria
     C                   PARM                    PDatNovo
     C                   PARM                    PValNovo
     C                   PARM                    WCodErro
     C                   PARM                    PDatSinistro
     C                   PARM                    PValActual
      /FREE
        //------------------------------------------------------
        // Trabalhar os Parâmetros Recebidos
        //------------------------------------------------------
        EXSR ValParms;

        //------------------------------------------------------
        // Obter a Taxa de desvalorização
        //------------------------------------------------------
        IF (WCodErro = *BLANKS);
           WTaxaActual = *ZEROS;
           WTaxaAcumul = *ZEROS;
           WQtdeMeses  = *ZEROS;
           WTaxa       = *ZEROS;
           WDBSession  = $SetFiles('FPRDP200');
           $SetCriteria('(MESINICIO <= ''' + %CHAR(WMesActual) + ''') AND +
                         (MESTERMO >= ''' + %CHAR(WMesActual) + ''') AND +
                         (CATEGORI = ''' + %TRIM(WCategoria) + ''')');
           IF ($GetRecord() > *ZEROS);
              WTaxaActual = $GetFieldNum('VALTAXA');
              WTaxaAcumul = $GetFieldNum('VALACUM');
              WQtdeMeses  = WMesActual - $GetFieldNum('MESINICIO') + 1;
              WTaxa       = 100 - (WQtdeMeses * WTaxaActual + WTaxaAcumul);
           ENDIF;
           WValActual = WTaxa * WValNovo;
        ENDIF;

        *INLR = *ON;

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
              WDatNovo = PDatNovo;
           ON-ERROR;
              WDatNovo = *ZEROS;
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
              WCodErro   = 'PRDxxxx';
              LEAVESR;
           ENDMON;

           //-------------------------------------------------
           // A Data do Sinistro assume o dia por defeito
           //-------------------------------------------------
           MONITOR;
              WDatSinistro = PDatSinistro;
           ON-ERROR;
              WDatSinistro = $DateToInt(%DATE());
           ENDMON;

           //-------------------------------------------------
           // O Valor actual é formatado com zeros
           //-------------------------------------------------
           MONITOR;
              WValActual = PValActual;
           ON-ERROR;
              WValActual = *ZEROS;
           ENDMON;

           //-------------------------------------------------
           // Calcular a quantidade de Meses da Viatura
           //-------------------------------------------------
        //    WDataEfeito = %DATE(DataEfeito:*ISO);
        // ELSE;
        //    WDataEfeito = %DATE();
        // ENDIF;
        //
        // WAno = %SubDt(WDataEfeito:*Years);
        // WMes = %SubDt(WDataInicio:*Months);
        // WDia = %SubDt(WDataInicio:*Days);
        // WMesActual = (WDatNovo - WDatSinistro) + 1;
        ENDSR;
      /END-FREE
