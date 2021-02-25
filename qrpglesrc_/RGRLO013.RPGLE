     H NOMAIN
     H*===================================================================
     H* Objectivo..: Carregar Campos de trabalho.
     H*===================================================================
      /Copy QRPGLESRC,SGRLO010

     D
     D WDataY          S              6S 0
     D WDataYY         S              8S 0
     D WDatChr         S             10A
     D*===================================================================

     P*===================================================================
     P* Processo..: $DateToInt
     P* Descrição.: Converter uma data num Inteiro
     P*===================================================================
     P $DateToInt      B                   EXPORT
     D                 PI             8S 0
     D    DataIn                       D   Const
      /FREE
        MONITOR;
           RETURN %SUBDT(DataIn:*Y)*10000+%SUBDT(DataIn:*M)*100+
                          %SUBDT(DataIn:*D);
        ON-ERROR;
           RETURN *ZEROS;
        ENDMON;
      /END-FREE
     P $DateToInt      E
     P*===================================================================
     P* Processo..: $IntToDate
     P* Descrição.: Converter um Interio numa Data
     P*===================================================================
     P $IntToDate      B                   EXPORT
     D                 PI              D
     D    DataIn                      8S 0 Const
      /FREE
        MONITOR;
           RETURN %DATE(DataIn:*ISO);
        ON-ERROR;
           RETURN %DATE();
        ENDMON;
      /END-FREE
     P $IntToDate      E
     P*===================================================================
     P* Processo..: $CvtData
     P* Descrição.: Converter um formato de data
     P*===================================================================
     P $CvtData        B                   EXPORT
     D                 PI             8S 0
     D    DataIn                      8S 0 Const
     D    FmtIn                       4A   Const
     D    FmtOut                      4A   Const Options(*NOPASS)
     D
     D WFmtOut         S              4A   INZ('YYMD')
     D WAno            S              4S 0 INZ(*ZEROS)
     D WMes            S              2S 0 INZ(*ZEROS)
     D WDia            S              2S 0 INZ(*ZEROS)
     D WResultado      S              8S 0 INZ(*ZEROS)
     C*==============================================================
      /FREE
        IF (%Parms() = 3);
           WFmtOut = FmtOut;
        ENDIF;

        //------------------------------------------------
        // Extrair os Elementos da Data
        //------------------------------------------------
        IF (FmtIn = 'YMD') OR (FmtIn = 'YYMD');
           WAno = %INT(DataIn/10000);
           WMes = %INT(DataIn/100) - (WAno*100);
           WDia = DataIn - (WAno*10000+WMes*100);
        ELSEIF (FmtIn = 'DMY');
           WDia = %INT(DataIn/10000);
           WMes = %INT(DataIn/100) - (WDia*100);
           WAno = DataIn - (WDia*10000+WMes*100);
        ELSEIF (FmtIn = 'DMYY');
           WDia = %INT(DataIn/1000000);
           WMes = %INT((DataIn-WDia*1000000)/10000);
           WAno = DataIn - (WDia*1000000+WMes*10000);
        ENDIF;
        IF ((WAno > 50) AND (WAno < 100));
           WAno += 1900   ;
        ELSEIF (WAno < 100);
           WAno += 2000   ;
        ENDIF;

        //------------------------------------------------
        // Formatar Saida
        //------------------------------------------------
        IF (WFmtOut = 'YMD');
           WResultado = (WAno-%INT(WAno/100)*100) * 10000 +
                         WMes * 100 + WDia;
        ELSEIF (WFmtOut = 'YYMD');
           WResultado = WAno*10000 + WMes*100 + WDia;
        ELSEIF (WFmtOut = 'DMYY');
           WResultado = WDia*1000000 + WMes*10000 + WAno;
        ELSEIF (WFmtOut = 'DMY');
           WResultado = WDia * 10000 + WMes * 100 +
                        (WAno-%INT(WAno/100)*100);
        ENDIF;

        RETURN WResultado;
      /END-FREE
     P $CvtData        E

     P*===================================================================
     P* Processo..: $ApplyDatFmt
     P* Descrição.: Colocar uma data numa máscara de edição
     P*===================================================================
     P $ApplyDatFmt    B                   EXPORT
     D                 PI            10A
     D    DataIn                      8S 0 Const
     D    FmtIn                       4A   Const
     D    SepChr                      1A   Const Options(*NOPASS)
     D    FmtOut                      4A   Const Options(*NOPASS)
     D
     D WFmtOut         S              4A   INZ('YYMD')
     D WSepChr         S              1A   INZ('/')
     D WAno            S              4S 0 INZ(*ZEROS)
     D WMes            S              2S 0 INZ(*ZEROS)
     D WDia            S              2S 0 INZ(*ZEROS)
     D WResultado      S             10A   INZ(*BLANKS)
     C*==============================================================
      /FREE
        IF (%Parms() = 3);
           WSepChr = SepChr;
        ELSEIF (%Parms() = 4);
           WSepChr = SepChr;
           WFmtOut = FmtOut;
        ENDIF;

        //------------------------------------------------
        // Extrair os Elementos da Data
        //------------------------------------------------
        IF (FmtIn = 'YMD') OR (FmtIn = 'YYMD');
           WAno = %INT(DataIn/10000);
           WMes = %INT(DataIn/100) - (WAno*100);
           WDia = DataIn - (WAno*10000+WMes*100);
        ELSEIF (FmtIn = 'DMY');
           WDia = %INT(DataIn/10000);
           WMes = %INT(DataIn/100) - (WDia*100);
           WAno = DataIn - (WDia*10000+WMes*100);
        ELSEIF (FmtIn = 'DMYY');
           WDia = %INT(DataIn/1000000);
           WMes = %INT((DataIn-WDia*1000000)/10000);
           WAno = DataIn - (WDia*1000000+WMes*10000);
        ENDIF;
        IF ((WAno > 50) AND (WAno < 100));
           WAno += 1900   ;
        ELSEIF (WAno < 100);
           WAno += 2000   ;
        ENDIF;

        //------------------------------------------------
        // Formatar Saida
        //------------------------------------------------
        IF (WFmtOut = 'YMD');
           WResultado = %SUBST(%TRIM(%EDITW(WAno:'0    ')):3:2) +
                        WSepChr+%TRIM(%EDITW(WMes:'0  '))+WSepChr +
                        %TRIM(%EDITW(WDia:'0  '));
        ELSEIF (WFmtOut = 'YYMD');
           WResultado = %TRIM(%EDITW(WAno:'0    '))+WSepChr+
                        %TRIM(%EDITW(WMes:'0  '))+WSepChr+
                        %TRIM(%EDITW(WDia:'0  '));
        ELSEIF (WFmtOut = 'DMYY');
           WResultado = %TRIM(%EDITW(WDia:'0  '))+WSepChr+
                        %TRIM(%EDITW(WMes:'0  '))+WSepChr+
                        %TRIM(%EDITW(WAno:'0    '));
        ELSEIF (WFmtOut = 'DMY');
           WResultado = %TRIM(%EDITW(WDia:'0  '))+
                        WSepChr+%TRIM(%EDITW(WMes:'0  '))+WSepChr +
                        %SUBST(%TRIM(%EDITW(WAno:'0    ')):3:2);
        ENDIF;

        RETURN WResultado;
      /END-FREE
     P $ApplyDatFmt    E

     P*===================================================================
     P* Processo..: $addToDate
     P* Descrição.: Somar ou subtrair a uma data
     P*===================================================================
     P $addToDate      B                   EXPORT
     D                 PI             8S 0
     D    DataIn                      8S 0 Const
     D    Qtde                        4S 0 Const
     D    Tipo                        1A   Const Options(*NOPASS)
     D
     D WTipo           S              1A   INZ('D')
     D WAno            S              4S 0 INZ(*ZEROS)
     D WMes            S              2S 0 INZ(*ZEROS)
     D WDia            S              2S 0 INZ(*ZEROS)
     D WDataD          S               D
     D WResultado      S              8S 0 INZ(*ZEROS)
     C*==============================================================
      /FREE
        IF (%Parms() = 3);
           WTipo = Tipo;
        ENDIF;

        //------------------------------------------------
        // Extrair os Elementos da Data
        //------------------------------------------------
        WAno = %INT(DataIn/10000);
        WMes = %INT(DataIn/100) - (WAno*100);
        WDia = DataIn - (WAno*10000+WMes*100);
        WDataD = %DATE(DataIn:*ISO);
        IF (WTipo = 'D');
           WDataD += %Days(Qtde);
        ELSEIF (WTipo = 'M');
           WDataD += %Months(Qtde);
        ELSEIF (WTipo = 'Y');
           WDataD += %Years(Qtde);
        ENDIF;

        WAno = %SubDt(WDataD:*Years);
        WMes = %SubDt(WDataD:*Months);
        WDia = %SubDt(WDataD:*Days);
        WResultado = (WAno*10000) + (WMes*100) + WDia;

        RETURN WResultado;
      /END-FREE
     P $addToDate      E

     P*===================================================================
     P* Processo..: $findDate
     P* Descrição.: Obter a data de inicio ou termo em função de uma data
     P*===================================================================
     P $findDate       B                   EXPORT
     D                 PI             8S 0
     D    DataInicio                  8S 0 Const
     D    Tipo                        1A   Const
     D    DataEfeito                  8S 0 Const Options(*NOPASS)
     D
     D WAno            S              4S 0 INZ(*ZEROS)
     D WMes            S              2S 0 INZ(*ZEROS)
     D WDia            S              2S 0 INZ(*ZEROS)
     D WDataEfeito     S               D
     D WDataInicio     S               D
     D WDataTermo      S               D
     D WResultado      S              8S 0 INZ(*ZEROS)
     C*==============================================================
      /FREE
        //------------------------------------------------
        // Extrair os Elementos da Data
        //------------------------------------------------
        WDataInicio = %DATE(DataInicio:*ISO);
        IF (%Parms() = 3);
           WDataEfeito = %DATE(DataEfeito:*ISO);
        ELSE;
           WDataEfeito = %DATE();
        ENDIF;

        WAno = %SubDt(WDataEfeito:*Years);
        WMes = %SubDt(WDataInicio:*Months);
        WDia = %SubDt(WDataInicio:*Days);
        WDataInicio = %DATE(((WAno*10000)+(WMes*100)+WDia):*ISO);
        IF (WDataInicio > WDataEfeito);
           WDataInicio -= %Years(1);
        ENDIF;
        WDataTermo  = WDataInicio;
        WDataTermo += %Years(1);
        WDataTermo -= %Days(1);

        IF (Tipo = 'I'); // Obter a data de Inicio
           WAno = %SubDt(WDataInicio:*Years);
           WMes = %SubDt(WDataInicio:*Months);
           WDia = %SubDt(WDataInicio:*Days);
           WResultado = (WAno*10000) + (WMes*100) + WDia;
        ELSE;
           WAno = %SubDt(WDataTermo:*Years);
           WMes = %SubDt(WDataTermo:*Months);
           WDia = %SubDt(WDataTermo:*Days);
           WResultado = (WAno*10000) + (WMes*100) + WDia;
        ENDIF;


        RETURN WResultado;
      /END-FREE
     P $findDate       E

     P*===================================================================
     P* Processo..: $chkDUtil
     P* Descrição.: Obter o dia util (considerando Feriado Fixos e Móveis)
     P*             devolve *ON se dia util ou *OFF se Feriado ou fim de
     P*             semana.
     P*===================================================================
     P $chkDUtil       B                   EXPORT
     D                 PI              N
     D    Data                        8S 0 Const
     D    Local                       5S 0 Const Options(*NOPASS)
     D
     D WResultado      S               N   INZ(*OFF)
     C*==============================================================
      /FREE
        RETURN WResultado;
      /END-FREE
     P $chkDUtil       E
