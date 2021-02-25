     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*===========================================================
     H* Programa..: RCOBO010
     H* Objectivo.: Criar o Recibo em Demo Comp, Lda ou na Companhia.
     H* Descrição.: Esta rotina é invocada de diferentes formas, para
     H*             gerar os recibos no Corretor e/ou Companhia.
     H*             São criados todos os tipos de Recibos, e a
     H*             numeração autmáticamente. Com esta Rotina
     H*             Substituimos e descontinuamos um conjunto de
     H*             programas em RPGII.
     H*===========================================================
     FSD1RCA    UF A F  326     6AIDISK    KEYLOC(5) EXTFILE('SD.1.RCA')

     D R#SD1RCA      E DS                  EXTNAME('SD.1.REC') PREFIX('REC_')
     D*==> Campos de Trabalho
     D WNumOrdem       S              6S 0
     D WDatAlfa        S              6A
     D WAjuste         S              5S 4
     D WComiss01       S             11S 2
     D WComiss02       S             11S 2
     D WComiss03       S             11S 2
     D WPerComCCob     S              5S 3
     D WPerComSCob     S              5S 3
     D WComMedCCob     S             11S 2
     D WComMedSCob     S             11S 2
     D WPremSimples    S             11S 2
     D WComTotal       S             11S 2
     D*===========================================================
      /FREE
        WNumOrdem = 495334;
        WDatAlfa  = '090514';
        Exsr Corrigir;

        WNumOrdem = 495335;
        WDatAlfa  = '060514';
        Exsr Corrigir;

        WNumOrdem = 495336;
        WDatAlfa  = '040514';
        Exsr Corrigir;

        WNumOrdem = 495337;
        WDatAlfa  = '040614';
        Exsr Corrigir;

        WNumOrdem = 495338;
        WDatAlfa  = '010414';
        Exsr Corrigir;

        WNumOrdem = 495340;
        WDatAlfa  = '010614';
        Exsr Corrigir;

        WNumOrdem = 495341;
        WDatAlfa  = '040614';
        Exsr Corrigir;

        WNumOrdem = 495342;
        WDatAlfa  = '010614';
        Exsr Corrigir;

        WNumOrdem = 495343;
        WDatAlfa  = '020614';
        Exsr Corrigir;

        WNumOrdem = 495344;
        WDatAlfa  = '060614';
        Exsr Corrigir;

        WNumOrdem = 495345;
        WDatAlfa  = '010614';
        Exsr Corrigir;

        WNumOrdem = 495346;
        WDatAlfa  = '010614';
        Exsr Corrigir;

        RETURN;
        //=========================================================
        // Subrotina.: Corrigir
        // Objectivo.: Corrigir valores no recibo
        //=========================================================
        BEGSR Corrigir;
           CHAIN WNumOrdem SD1RCA R#SD1RCA;
           IF (%FOUND(SD1RCA));
              REC_DTLIM = WDatAlfa;
              UPDATE SD1RCA R#SD1RCA;
           ENDIF;
        ENDSR;

        //=========================================================
        // Subrotina.:
        // Objectivo.:
        //=========================================================
        BEGSR Calculos;
           //-----------------------------------------------------------------
           // Tratar Parametros Opcionais Recebidos
           //-----------------------------------------------------------------
           WComiss01 = 0.5;
           WComiss02 = 2.28;
           WComiss03 = 0;
           WPremSimples = 20;
           WAjuste = 1;
           WPerComCCob = .1083;
           WPerComSCob = 0;

           WComTotal   = WComiss01 + WComiss02 + WComiss03;
           WComMedCCob = %DECH((WPremSimples*WPerComCCob*WAjuste)+0.005:11:2);
           WComMedSCob = %DECH((WPremSimples*WPerComSCob*WAjuste)+0.005:11:2);
        ENDSR;
      /END-FREE
