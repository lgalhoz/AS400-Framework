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
     FSD1RE2    IF   E           K DISK    PREFIX(ERC_)
     FSD1CG3    UF   F   80        DISK    EXTFILE('SD.1.CG3')
     FSD1RCA    UF A F  326     6AIDISK    KEYLOC(5) EXTFILE('SD.1.RCA')
     FSD1AP     UF   F  320     7AIDISK    KEYLOC(1) EXTFILE('SD.1.AP')
     FSD1APM    UF   F  320     7AIDISK    KEYLOC(1) EXTFILE('SD.1.APM')
     F*===========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010

     D*==> Formatos de Registo
     D R#SD1CG3        DS
     D   NUM_TIPO              1      2S 0
     D   NUM_FIL001            3      9A
     D   NUM_NUMERO           10     15S 0
     D   NUM_FIL002           16     80A
     D
     D R#SD1RCA      E DS                  EXTNAME('SD.1.REC') PREFIX('REC_')
     D R#SD1AP       E DS                  EXTNAME('SD.1.AP') PREFIX('AP_')
     D
     D*==> Campos de Trabalho
     D WTipRecibo      S              1A
     D WNumOrdAP       S              7S 0
     D WDatInicio      S              8S 0
     D WDatTermo       S              8S 0
     D WPremSimples    S             11S 2
     D WPremTotal      S             11S 2
     D WCodErro        S              7A
     D PNumRecibo      S              9S 0
     D PDatEntrada     S              8S 0
     D PDatLimite      S              8S 0
     D PAjuste         S              5S 4
     D PComiss01       S             11S 2
     D PComiss02       S             11S 2
     D PComiss03       S             11S 2
     D WNumRecibo      S              9S 0
     D WNumRecibAlfa   S              9A
     D WDatEntrada     S              8S 0
     D WDatLimite      S              8S 0
     D WDataDMY        S              6S 0
     D WAjuste         S              5S 4
     D WComiss01       S             11S 2
     D WComiss02       S             11S 2
     D WComiss03       S             11S 2
     D
     D WCodMovim       S              2S 0
     D WIsHistorico    S               N
     D WCodMedCCob     S              3S 0
     D WCodMedSCob     S              3S 0
     D WPerComCCob     S              5S 5
     D WPerComSCob     S              5S 5
     D WComMedCCob     S             11S 2
     D WComMedSCob     S             11S 2
     D WComTotal       S             11S 2
     D WNumOrdem       S              7S 0
     D WNumAltera      S              3S 0
     D WCodGestor      S              2A
     D
     D #RRN            S              4S 0
     D
     D*==> Definição de Constantes
     D KTE#COMP        C                   1
     D KTE#CORR        C                   2
     D REC#Estorno     C                   21
     D REC#Premio      C                   15
     D REC#Indemniza   C                   43
     D REC#Compensa    C                   32
     D TIP#Novo        C                   'N'
     D TIP#Continuado  C                   'P'
     D TIP#Adicional   C                   'A'
     D TIP#Estorno     C                   'E'
     D TIP#Indemniza   C                   'I'
     D TIP#Compensa    C                   'C'
     D*===========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WTipRecibo
     C                   PARM                    WNumOrdAP
     C                   PARM                    WDatInicio
     C                   PARM                    WDatTermo
     C                   PARM                    WPremSimples
     C                   PARM                    WPremTotal
     C                   PARM                    WNumOrdem
     C                   PARM                    WCodErro
     C                   PARM                    PNumRecibo
     C                   PARM                    PDatEntrada
     C                   PARM                    PDatLimite
     C                   PARM                    PAjuste
     C                   PARM                    PComiss01
     C                   PARM                    PComiss02
     C                   PARM                    PComiss03
     C
      /FREE
        //-----------------------------------------------------------------
        // Tratar Parametros Opcionais Recebidos
        //-----------------------------------------------------------------
        WCodErro = *BLANKS;
        EXSR ValParametros;

        //-----------------------------------------------------------------
        // Obter informação da Apólice
        //-----------------------------------------------------------------
        WCodGestor = *BLANKS;
        IF (WCodErro = *BLANKS);
           WIsHistorico = *OFF;
           CHAIN WNumOrdAP SD1AP R#SD1AP;
           IF (NOT %FOUND(SD1AP));
              CHAIN WNumOrdAP SD1APM R#SD1AP;
              IF (NOT %FOUND(SD1APM));
                 WCodErro = 'COB0049';
              ENDIF;
              WIsHistorico = *ON;
           ENDIF;
           WCodGestor = AP_GEST + AP_TECN;
           WNumAltera = AP_NUMUL + 1;
        ENDIF;

        //------------------------------------------
        // Valdiar se o recibo já está criado
        //------------------------------------------
        WNumRecibAlfa = %SUBST(%EDITW(WNumRecibo:'         '):2:8);
        CHAIN (AP_CCOMP:WNumRecibAlfa) SD1RE2;
        IF (%FOUND(SD1RE2));
           WCodErro = 'COB0060';
        ENDIF;

        IF (WCodErro = *BLANKS);
           EXSR CalcComissao;
        ENDIF;

        //-----------------------------------------------------------------
        // Obter o último número disponível
        //-----------------------------------------------------------------
        IF (WCodErro = *BLANKS);
           #RRN = KTE#CORR;
           CHAIN #RRN SD1CG3 R#SD1CG3;
           NUM_NUMERO = NUM_NUMERO + 1;
           WNumOrdem  = NUM_NUMERO;
        ENDIF;

        //-----------------------------------------------------------------
        // Actualizar a Informação do Recibo e da Apólice
        //-----------------------------------------------------------------
        IF (WCodErro = *BLANKS);
           EXSR AddRecibo;
        ENDIF;

        IF (WCodErro = *BLANKS);
           EXSR UpdApolice;
        ENDIF;

        //-----------------------------------------------------------------
        // Actualizar a Numeração
        //-----------------------------------------------------------------
        IF (WCodErro = *BLANKS);
           MONITOR;
              NUM_NUMERO = WNumOrdem;
              UPDATE SD1CG3 R#SD1CG3;
           ON-ERROR;
              WCodErro = 'COB0058';
           ENDMON;
        ENDIF;
        WNumOrdem = WNumOrdem + 1000000;
        RETURN;
        *INLR = *ON;

        //=======================================================================
        // Subrotina.: ValParametros
        // Objectivo.: Validar os Parameteros Recebidos
        //=======================================================================
        BEGSR ValParametros;
           //----------------------------------------------------
           // Validar Parametros recebidos
           //----------------------------------------------------
           IF (WTipRecibo <> TIP#Continuado) AND (WTipRecibo <> TIP#Novo) AND
             (WTipRecibo <> TIP#Estorno) AND (WTipRecibo <> TIP#Indemniza) AND
             (WTipRecibo <> TIP#Compensa) AND (WTipRecibo <> TIP#Adicional);
              WCodErro = 'COB0048';
              LEAVESR;
           ENDIF;

           IF (WNumOrdAP <= *ZEROS);
              WCodErro = 'COB0050';
              LEAVESR;
           ENDIF;

           IF (WDatInicio = *ZEROS);
              WCodErro = 'COB0051';
              LEAVESR;
           ELSEIF (WDatInicio > WDatTermo);
              WCodErro = 'COB0051';
              LEAVESR;
           ELSE;
              TEST(DE) *ISO WDatInicio;
              IF (%ERROR());
                 WCodErro = 'COB0051';
                 LEAVESR;
              ELSE;
                 TEST(DE) *ISO WDatTermo;
                 IF (%ERROR());
                    WCodErro = 'COB0052';
                    LEAVESR;
                 ENDIF;
              ENDIF;
           ENDIF;

           IF (WPremSimples <= *ZEROS);
              WCodErro = 'COB0053';
              LEAVESR;
           ENDIF;

           IF (WPremTotal <= *ZEROS);
              WCodErro = 'COB0054';
              LEAVESR;
           ENDIF;

           //----------------------------------------------------
           // ForCompr valores pode defeito
           //----------------------------------------------------
           MONITOR;
              WNumRecibo = PNumRecibo;
           ON-ERROR;
              WNumRecibo = *ZEROS;
           ENDMON;

           MONITOR;
              WDatEntrada = PDatEntrada;
           ON-ERROR;
              WDatEntrada = $DatetoInt(%DATE());
           ENDMON;

           MONITOR;
              WDatLimite  = PDatLimite;
           ON-ERROR;
              WDatLimite = *ZEROS; // calcular com os dias
           ENDMON;

           MONITOR;
              WAjuste = PAjuste;
           ON-ERROR;
              WAjuste = 1;
           ENDMON;

           MONITOR;
              WComiss01 = PComiss01;
           ON-ERROR;
              WComiss01 = *ZEROS;
           ENDMON;

           MONITOR;
              WComiss02   = PComiss02;
           ON-ERROR;
              WComiss02 = *ZEROS;
           ENDMON;

           MONITOR;
              WComiss03   = PComiss03;
           ON-ERROR;
              WComiss03 = *ZEROS;
           ENDMON;
        ENDSR;

        //=======================================================================
        // Subrotina.: CalcComissao
        // Objectivo.: Calcular os valores para a Comissão
        //             WCodMedCCob -> Código do Mediador C/Cobrança
        //             WCodMedSCob -> Código do Mediador S/Cobrança
        //             WPerComCCob -> Percentagem Comissão do Mediador C/Cobrança
        //             WPerComSCob -> Percentagem Comissão do Mediador S/Cobrança
        //             WComMedCCob -> Comissão do Mediador C/Cobrança
        //             WComMedSCob -> Comissão do Mediador S/Cobrança
        //             WComTotal   -> Comissão Total
        //=======================================================================
        BEGSR CalcComissao;
           WCodMedCCob = AP_RI1;
           WCodMedSCob = AP_RI2;
           WPerComCCob = AP_RJ1;
           WPerComSCob = AP_RJ2;
           WComTotal   = WComiss01 + WComiss02 + WComiss03;
           WComMedCCob = %DECH((WPremSimples*WPerComCCob*WAjuste)+0.005:11:3);
           WComMedSCob = %DECH((WPremSimples*WPerComSCob*WAjuste)+0.005:11:3);
        ENDSR;

        //=======================================================================
        // Subrotina.: AddRecibo
        // Objectivo.: Adicionar o reguisto do Recibo
        //=======================================================================
        BEGSR AddRecibo;
           IF (WTipRecibo = TIP#Novo) OR (WTipRecibo = TIP#Continuado) OR
             (WTipRecibo = TIP#Adicional);
              WCodMovim = REC#Premio;
           ELSEIF (WTipRecibo = TIP#Estorno);
              WCodMovim = REC#Estorno;
           ELSEIF (WTipRecibo = TIP#Indemniza);
              WCodMovim = REC#Indemniza;
           ELSEIF (WTipRecibo = TIP#Compensa);
              WCodMovim = REC#Compensa;
           ENDIF;

           REC_TPCOB   = *BLANKS;
           REC_CODFACT = '0';
           REC_CODMOV  = WCodMovim;
           REC_NORD    = WNumOrdem;
           REC_APOLIC  = AP_APOLI;
           REC_NUMREC  = %SUBST(%EDITW(WNumRecibo:'         '):2:8);
           REC_CCOMP   = AP_CCOMP;
           REC_CSEGUR  = AP_CSEG;
           REC_CRAMO   = AP_CRAMO;
           REC_DTINIC  = $CvtData(WDatInicio:'YYMD':'DMY');
           REC_DTFIM   = $CvtData(WDatTermo:'YYMD':'DMY');
           REC_VTREC   = WPremTotal;
           REC_VCOMIS  = WComTotal;
           REC_DTREC   = $CvtData(WDatEntrada:'YYMD':'DMY');
           REC_NUMGR   = *BLANKS;
           REC_DTGR    = *ZEROS;
           REC_DTENTR  = $CvtData(WDatEntrada:'YYMD':'DMY');
           WDataDMY    = $CvtData(WDatLimite:'YYMD':'DMY');
           REC_DTLIM   = %EDITW(WdataDMY:'      ');
           REC_NUMAD   = *ZEROS;       // Número do Aviso de Débito
           REC_DTAVD   = *ZEROS;       // [FCT] Data do Aviso de Débito
           REC_CDF     = *ZEROS;       // [FCT] Código de Facturação
           REC_DVAR    = *BLANKS;      // [FCT] Descrição Variável
           REC_MCCOB   = WCodMedCCob;
           REC_COMIS1  = WComMedCCob;
           REC_MSCOB   = WCodMedSCob;
           REC_COMIS2  = WComMedSCob;
           REC_MED3    = *ZEROS;
           REC_COMIS3  = *ZEROS;
           REC_CGEST   = WCodGestor;
           REC_NODEV   = *ZEROS;
           REC_DTDEV   = *BLANKS;
           REC_DTCOB   = *BLANKS;
           REC_ORIGEM  = *BLANKS;
           REC_NRCOB   = *ZEROS;
           REC_DTRCOB  = *BLANKS;
           REC_USER    = S#USR;
           REC_DPAG    = *BLANKS;
           REC_FP      = *BLANKS;
           REC_NDP     = *BLANKS;
           REC_VPAG    = *ZEROS;
           REC_VDESC   = *ZEROS;
           REC_PRSIMP  = WPremSimples;
           REC_VALIE   = *ZEROS;
           REC_DIFPAG  = *ZEROS;
           REC_UNACHX  = *BLANKS;
           REC_CSEGX   = *BLANKS;
           REC_CARTA   = *BLANKS;
           REC_NORD2   = WNumOrdem + 1000000;
           REC_NREC2   = %EDITW(WNumRecibo:'         ');
           REC_FILL1   = *BLANKS;
           REC_FILL2   = $CvtData($DateToInt(%DATE()):'YYMD':'DMY');
           REC_FILL3   = *BLANKS;
           REC_CDFACT  = *BLANKS;      // [FCT] Código de Facturação
           REC_FILL4   = *BLANKS;
           REC_CDALT   = 'N';
           REC_FILL5   = *BLANKS;
           REC_SNDAVD  = *BLANKS;      // [FCT] Número do Aviso Débito
           REC_FILL6   = *BLANKS;
           REC_PANUL   = *BLANKS;
           REC_SAIDA   = *BLANKS;
           REC_ERC     = *BLANKS;
           REC_CODSIT  = 0;
           REC_CDEL    = *BLANKS;
           REC_FILL7   = *BLANKS;
           REC_CODEV   = *BLANKS;
           REC_SC      = *BLANKS;
           REC_CCONT   = *BLANKS;
           REC_FILL8   = *BLANKS;
           MONITOR;
              WRITE SD1RCA R#SD1RCA;
           ON-ERROR;
              WCodErro = 'COB0055';
           ENDMON;
        ENDSR;

        //=======================================================================
        // Subrotina.: UpdApolice
        // Objectivo.: Actualizar o registo
        //=======================================================================
        BEGSR UpdApolice;
           MONITOR;
              AP_PRSRN = WPremSimples;
              AP_PRTRN = WPremTotal;
              AP_DTULT = $CvtData($DateToInt(%DATE()):'YYMD':'DMY');
              AP_DFIX  = 15;
              AP_NUMUL = WNumAltera;
              AP_HORUL = %DEC(%TIME());
              AP_UTIUL = S#USR;
              IF (WIsHistorico);
                 UPDATE SD1APM R#SD1AP;
              ELSE;
                 UPDATE SD1AP R#SD1AP;
              ENDIF;
           ON-ERROR;
              IF (WIsHistorico);
                 WCodErro = 'COB0056';
              ELSE;
                 WCodErro = 'COB0057';
              ENDIF;
           ENDMON;
        ENDSR;
      /END-FREE
