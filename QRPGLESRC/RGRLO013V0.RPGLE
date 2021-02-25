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
     P*             semana. Adaptada para a exclusão de Feriados em 2102.
     P*===================================================================
     P $chkDUtil       B                   EXPORT
     D                 PI              N
     D    Data                        8S 0 Const
     D    Local                       5S 0 Const Options(*NOPASS)
     D
     D*==> Campos de Trabalho
     D WData           S               D
     D WVarA           S              7S 0
     D WVarB           S              7S 0
     D WVarC           S              7S 0
     D WVarD           S              7S 0
     D WVarE           S              7S 0
     D WVarF           S              7S 0
     D WVarG           S              7S 0
     D WVarH           S              7S 0
     D WVarI           S              7S 0
     D WVarJ           S              7S 0
     D WVarK           S              7S 0
     D WVarL           S              7S 0
     D WVarM           S              7S 0
     D WAno            S              4S 0
     D WMes            S              2S 0
     D WDia            S              2S 0
     D
     D WDayOfWeek      S              1S 0
     D WDatNumerica    S              8S 0
     D WDatPascoa      S               D
     D WFerFixo        S               D
     D
     D*==> Constantes
     D DIF#Carnaval    C                   -47
     D DIF#Sexta       C                   -2
     D DIF#CorpoDeus   C                   +60
     D
     D DSM#Segunda     C                   0
     D DSM#Terca       C                   1
     D DSM#Quarta      C                   2
     D DSM#Quinta      C                   3
     D DSM#Sexta       C                   4
     D DSM#Sabado      C                   5
     D DSM#Domingo     C                   6
     C*==============================================================
      /FREE
        WData = %DATE(Data:*ISO);
        WAno = %SUBDT(WData:*YEARS);

        //------------------------------------------------------------
        // Verificar se a data corresponde a um fim de semana
        //------------------------------------------------------------
        WDayOfWeek = %rem(%diff(WData:d'0001-01-01':*days):7);
        IF (WdayOfWeek = DSM#Sabado) OR (WDayOfWeek = DSM#Domingo);
           RETURN *OFF;
        ENDIF;

        //------------------------------------------------------------
        // Verificar se a data corresponde a um Feriado fixo
        //------------------------------------------------------------
        EXSR ValFerFixo;

        //------------------------------------------------------------
        // Verificar se a data corresponde a um feriado movel
        //------------------------------------------------------------
        EXSR ValFerMovel;
        RETURN *ON;

        //========================================================================
        // Subrotina..: ValFerFixo
        // Objectivo..: Validar se a data corresponde a feriado fixo
        //========================================================================
        BEGSR ValFerFixo;
           //----------------------------------------------------------
           // Feriado do Ano Novo
           //----------------------------------------------------------
           WDatNumerica = (WAno * 10000) + (1 * 100) + 1;
           WFerFixo = $IntToDate(WDatNumerica);
           IF (WData = WFerFixo);
              RETURN *OFF;
           ENDIF;

           //----------------------------------------------------------
           // Feriado do dia da Indepêndencia (25 de Abril)
           //----------------------------------------------------------
           WDatNumerica = (WAno * 10000) + (4 * 100) + 25;
           WFerFixo = $IntToDate(WDatNumerica);
           IF (WData = WFerFixo);
              RETURN *OFF;
           ENDIF;

           //----------------------------------------------------------
           // Feriado do dia do Trabalhador  (01 de Maio)
           //----------------------------------------------------------
           WDatNumerica = (WAno * 10000) + (5 * 100) + 1;
           WFerFixo = $IntToDate(WDatNumerica);
           IF (WData = WFerFixo);
              RETURN *OFF;
           ENDIF;

           //----------------------------------------------------------
           // Feriado do dia de portugal (10 de Junho)
           //----------------------------------------------------------
           WDatNumerica = (WAno * 10000) + (6 * 100) + 10;
           WFerFixo = $IntToDate(WDatNumerica);
           IF (WData = WFerFixo);
              RETURN *OFF;
           ENDIF;

           //----------------------------------------------------------
           // Feriado do Nossa Senhora da Assunção  (15 de Agosto)
           //----------------------------------------------------------
           WDatNumerica = (WAno * 10000) + (8 * 100) + 15;
           WFerFixo = $IntToDate(WDatNumerica);
           IF (WData = WFerFixo);
              RETURN *OFF;
           ENDIF;

           //----------------------------------------------------------
           // Feriado da Imaculada Conceição (08 de Dezembro)
           //----------------------------------------------------------
           WDatNumerica = (WAno * 10000) + (12 * 100) + 8;
           WFerFixo = $IntToDate(WDatNumerica);
           IF (WData = WFerFixo);
              RETURN *OFF;
           ENDIF;

           //----------------------------------------------------------
           // Feriado do Natal (25 de Dezembro)
           //----------------------------------------------------------
           WDatNumerica = (WAno * 10000) + (12 * 100) + 25;
           WFerFixo = $IntToDate(WDatNumerica);
           IF (WData = WFerFixo);
              RETURN *OFF;
           ENDIF;

           IF (WAno < 2012);
              //-----------------------------------------------------------------------------
              // Feriado da Implantação da Republica (05 de Outubro) - Desactivado em 2012
              //-----------------------------------------------------------------------------
              WDatNumerica = (WAno * 10000) + (10 * 100) + 5;
              WFerFixo = $IntToDate(WDatNumerica);
              IF (WData = WFerFixo);
                 RETURN *OFF;
              ENDIF;

              //-----------------------------------------------------------------------------
              // Feriado do dia de todos os Santos (01 de Novembro) - Desactivado em 2012
              //-----------------------------------------------------------------------------
              WDatNumerica = (WAno * 10000) + (11 * 100) + 1;
              WFerFixo = $IntToDate(WDatNumerica);
              IF (WData = WFerFixo);
                 RETURN *OFF;
              ENDIF;

              //-----------------------------------------------------------------------------
              // Feriado da Restauração da Independência (01 de Dezembro) - Desactivado em 2012
              //-----------------------------------------------------------------------------
              WDatNumerica = (WAno * 10000) + (12 * 100) + 1;
              WFerFixo = $IntToDate(WDatNumerica);
              IF (WData = WFerFixo);
                 RETURN *OFF;
              ENDIF;
           ENDIF;
        ENDSR;

        //========================================================================
        // Subrotina..: ValFerMovel
        // Objectivo..: Validar se a data corresponde a feriado Móvel
        //========================================================================
        BEGSR ValFerMovel;
           EXSR CalcPascoa;

           //----------------------------------------------------------
           // Feriado da Pascoa
           //----------------------------------------------------------
           IF (WData = WDatPascoa);
              RETURN *OFF;
           ENDIF;

           //----------------------------------------------------------
           // Feriado do Carnaval
           //----------------------------------------------------------
           IF (WData = (WDatPascoa+%DAYS(DIF#Carnaval)));
              RETURN *OFF;
           ENDIF;

           //----------------------------------------------------------
           // Feriado do Sexta-Feira Santa
           //----------------------------------------------------------
           IF (WData = (WDatPascoa+%DAYS(DIF#Sexta)));
              RETURN *OFF;
           ENDIF;

           IF (WAno < 2012);
              //----------------------------------------------------------
              // Feriado do Corpo de Deus (Desactivado em 2012)
              //----------------------------------------------------------
              IF (WData = (WDatPascoa+%DAYS(DIF#CorpoDeus)));
                 RETURN *OFF;
              ENDIF;
           ENDIF;
        ENDSR;

        //========================================================================
        // Subrotina..: CalcPascoa
        // Objectivo..: Calcular o Feriado da Pascoa  em função do ano
        //========================================================================
        BEGSR CalcPascoa;
           WVarA = %REM(WAno:19);
           WVarB = (Wano / 100);
           WVarC = %REM(Wano:100);
           WVarD = WVarB / 4;
           WVarE = %REM(WVarB:4);
           WVarF = ((WVarb + 8) / 25);
           WVarG = ((WVarB - WVarF + 1) / 3);
           WVarH = %REM((19 * WVarA +WVarB - WVarD - WVarG + 15):30);
           WVarI = (WVarC / 4);
           WVarK = %REM(WVarc:4);
           WVarL = %REM((32 + 2*WVarE + 2*WVarI - WVarH - WVarK):7);
           WVarM = ((WVarA + 11*WVarH + 22*WVarL) / 451);
           WMes  = ((WVarH + WVarL - 7*WVarM + 114)/31);
           WDia  = %REM((WVarH + WVarL - 7*WVarM +114):31)  + 1;
           WDatNumerica = (WAno * 10000) + (WMes * 100) + WDia;
           WDatPascoa = $IntToDate(WDatNumerica);
        ENDSR;
      /END-FREE
     P $chkDUtil       E
     P*===================================================================
     P* Processo..: $Today
     P* Descrição.: Obter Data do dia. Ou a do Job (*JOB), ou a do
     P*             Sistema (*SYS)
     P*===================================================================
     P $Today          B                   EXPORT
     D                 PI              D
     D   TpData                       4A   CONST Options(*NOPASS)
     D
     D*==> Campos de Trabalho
     D WDatJob         S               D
     D WData           S             10A
     D WTpData         S              4A   INZ('*SYS')
     D*===========================================================
      /FREE
         WTpData = '*JOB';
         IF (%PARMS() > *ZEROS);
            IF (TpData = '*SYS');
               WTpData = '*SYS';
            ENDIF;
         ENDIF;

         IF (WTpData = '*JOB');
            WData = %CHAR(S#Ano)+'-'+%CHAR(S#Mes)+'-'+%CHAR(S#Dia);
            WDatJob = %DATE(WData:*ISO);
         ELSE;
            WDatJob = %DATE();
         ENDIF;
         RETURN WdatJob;
      /END-FREE
     P $Today          E
