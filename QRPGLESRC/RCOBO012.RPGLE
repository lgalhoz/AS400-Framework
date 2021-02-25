     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*===========================================================
     H* Programa..: RCOBO012
     H* Objectivo.: Facturar um recibo em Demo Comp, Lda ou Companhia
     H* Descrição.: Esta rotina é invocada de diferentes formas, para
     H*             Facturar os recibos.
     H*
     H*===========================================================
     FSD1HAV    UF A F  350        DISK    EXTFILE('SD.1.HAV')
     FSD1RCA    UF A F  326     6AIDISK    KEYLOC(5) EXTFILE('SD.1.RCA')
     FSD1PE     IF   F  512     6AIDISK    KEYLOC(5) EXTFILE('SD.1.PE')
     FSD1AP     IF   F  320     7AIDISK    KEYLOC(1) EXTFILE('SD.1.AP')
     FSD1CTL    IF   F  320     6AIDISK    KEYLOC(1) EXTFILE('SD.CTL')
     F*===========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,STBLO010

     D*==> Formatos de Registo
     D R#SDCTL         DS
     D   CTL_CORR1           154    155P 0
     D   CTL_CORR2           156    157P 0
     D   CTL_CORR3           158    159P 0
     D   CTL_CDSUPR          320    320A
     D
     D R#SD1HAV        DS
     D   HAV_FILLER01          1      4S 0
     D   HAV_NO                5     10S 0
     D   HAV_APOLIC           11     20A
     D   HAV_NUMREC           21     28A
     D   HAV_CCOMP            29     31S 0
     D   HAV_CSEG             32     37S 0
     D   HAV_CRAMO            38     40S 0
     D   HAV_DTINIC           41     46S 0
     D   HAV_DTFIM            47     52S 0
     D   HAV_VPREM            53     59P 2
     D   HAV_VCOMIS           60     64P 2
     D   HAV_DTREC            65     70S 0
     D   HAV_NUMGR            71     78A
     D   HAV_DTGR             79     84A
     D   HAV_DTENTR           85     90S 0
     D   HAV_NAVD             91    100P 0
     D   HAV_DTAVD           101    106S 0
     D   HAV_CFIX            107    109S 0
     D   HAV_FILLER02        110    110A
     D   HAV_DVAR            111    114A
     D   HAV_DUMMY01         115    144A
     D   HAV_CCOM1           145    146P 0
     D   HAV_NCOMIS          147    151P 2
     D   HAV_CCOM2           152    153P 0
     D   HAV_VCC1            154    158P 2
     D   HAV_CCOM3           159    160P 0
     D   HAV_VCC2            161    165P 2
     D   HAV_DUMMY02         166    301A
     D   HAV_CALT            302    302A
     D   HAV_DUMMY03         303    303A
     D   HAV_CFACT           304    304A
     D   HAV_DUMMY04         305    326A
     D   HAV_UDATE           327    332S 0
     D   HAV_DUMMY05         333    339A
     D   HAV_USER            340    347A
     D   HAV_DUMMY06         348    350A
     D
     D R#SD1PE       E DS                  EXTNAME('SD.1.PE') PREFIX('PE_')
     D R#SD1RCA      E DS                  EXTNAME('SD.1.REC') PREFIX('REC_')
     D R#SD1AP       E DS                  EXTNAME('SD.1.AP') PREFIX('AP_')
     D
     D*==> Campos de Trabalho
     D PDescFixa01     S              3S 0
     D PDescFixa02     S              3S 0
     D PDescVaria      S             35A
     D PCodFactura     S              1S 0
     D POrigem         S              6A
     D WForcar         S              1A
     D WCodErro        S              7A
     D WIsHistorico    S               N
     D WIsAvisDebit    S               N
     D WNumOrdRC       S              9S 0
     D WNumOrdem       S              6S 0
     D WNumOrdAP       S              8S 0
     D WOrigem         S              6A
     D WCodFactura     S              1S 0
     D WOrgOrdApol     S              7S 0
     D WDataAviso      S              6S 0
     D WDescFixa01     S              3S 0
     D WDescFixa02     S              3S 0
     D WDescVaria      S             35A
     D WMedComCobr     S              3S 0
     D WMedSemCobr     S              3S 0
     D WMed3           S              3S 0
     D WSnAvd          S              1A
     D WIsToFacturar   S               N
     D
     D*==> Definição de Constantes
     D KTE#CORR        C                   1
     D KTE#COMP        C                   2
     D REC#Estorno     C                   21
     D REC#Premio      C                   15
     D REC#Indemniza   C                   43
     D REC#Compensa    C                   32
     D*===========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WNumOrdRC
     C                   PARM                    WNumOrdAP
     C                   PARM                    WForcar
     C                   PARM                    WIsToFacturar
     C                   PARM                    WCodErro
     C                   PARM                    PDescFixa01
     C                   PARM                    PDescFixa02
     C                   PARM                    PDescVaria
     C                   PARM                    PCodFactura
     C                   PARM                    Porigem
     C
      /FREE
        //-----------------------------------------------------------------
        // Tratar Parametros Opcionais Recebidos
        //-----------------------------------------------------------------
        WCodErro = *BLANKS;
        EXSR ValParametros;

        //-----------------------------------------------------------------
        // Processar a facturação
        //-----------------------------------------------------------------
        IF (WCodErro = *BLANKS);
           EXSR Facturar;
        ENDIF;

        *INLR = *ON;

        //=======================================================================
        // Subrotina.: Facturar
        // Objectivo.: Validar os Parâmeteros Recebidos
        //=======================================================================
        BEGSR Facturar;
           IF (REC_CODMOV = REC#Premio);
              EXSR ChgRecibo;   // Actualizar recibos (SD.1.RCA)
              EXSR AddAvisoDeb; // Actualizar (SD.1.HAV)
           ELSEIF (REC_CODMOV = REC#Estorno);
              EXSR ChgRecibo;   // Actualizar recibos (SD.1.RCA)
              EXSR AddAvisoDeb; // Actualizar (SD.1.HAV)
           ELSEIF (REC_CODMOV = REC#Indemniza);
           ELSEIF (REC_CODMOV = REC#Compensa);
           ENDIF;
        ENDSR;

        //=======================================================================
        // Subrotina.: ChgRecibo
        // Objectivo.: Alterar os dados do Recibo
        //=======================================================================
        BEGSR ChgRecibo;
           IF (WIsToFacturar);
              IF (REC_CODMOV = REC#Estorno);
                 REC_ORIGEM = '999999';
              ELSE;
                 REC_ORIGEM = WOrigem;
              ENDIF;
              REC_DTAVD  = $CvtData($DateToInt(%DATE()):'YYMD':'DMY');
              REC_CDF    = WDescFixa01;
              REC_DVAR   = WDescVaria;
              REC_FILL3  = %CHAR(WDescFixa02);
              REC_SNDAVD = %CHAR(WCodFactura);
              REC_CDFACT = '1';
           ELSE;
              REC_DTAVD  = *ZEROS;
              REC_CDF    = *ZEROS;
              REC_DVAR   = *BLANKS;
              REC_CDFACT = *ZEROS;
              REC_SNDAVD = *ZEROS;
           ENDIF;
           REC_USER = S#USR;
           UPDATE SD1RCA R#SD1RCA;
        ENDSR;

        //=======================================================================
        // Subrotina.: AddAvisoDeb
        // Objectivo.: Adicionar registo no Aviso de Débito
        // Observação: Não se consegue perceber bem a utilização deste ficheiro uma
        //             vez que os campos tem diferentes estruturas em diferentes
        //             Programas. Avaliar se pode ser descontinuado no futuro.
        //=======================================================================
        BEGSR AddAvisoDeb;
           HAV_FILLER01 = 5302;
           HAV_NO       = REC_NORD;
           HAV_APOLIC   = REC_APOLIC;
           HAV_NUMREC   = REC_NUMREC;
           HAV_CCOMP    = REC_CCOMP;
           HAV_CSEG     = REC_CSEGUR;
           HAV_CRAMO    = REC_CRAMO;
           HAV_DTINIC   = REC_DTINIC;
           HAV_DTFIM    = REC_DTFIM;
           HAV_VPREM    = REC_VTREC;
           HAV_VCOMIS   = REC_VCOMIS;
           HAV_DTREC    = REC_DTREC;
           HAV_NUMGR    = REC_NUMGR;
           HAV_DTGR     = REC_DTGR;
           HAV_DTENTR   = REC_DTENTR;
           HAV_NAVD     = REC_NUMAD;
           HAV_DTAVD    = REC_DTAVD;
           HAV_CFIX     = REC_CDF;
           HAV_FILLER02 = *BLANKS;
           HAV_DVAR     = REC_DVAR;
           HAV_DUMMY01  = *BLANKS;
           HAV_CCOM1    = REC_MCCOB;
           HAV_NCOMIS   = REC_COMIS1;
           HAV_CCOM2    = REC_MSCOB;
           HAV_VCC1     = REC_COMIS2;
           HAV_CCOM3    = REC_MED3;
           HAV_VCC2     = REC_COMIS3;
           HAV_DUMMY02  = *BLANKS;
           HAV_CALT     = REC_CDALT;
           HAV_DUMMY03  = *BLANKS;
           HAV_CFACT    = REC_SNDAVD;
           HAV_DUMMY04  = *BLANKS;
           HAV_UDATE    = REC_DTAVD;
           HAV_DUMMY05  = *BLANKS;
           HAV_USER     = S#USR;
           HAV_DUMMY06  = *BLANKS;
           WRITE SD1HAV R#SD1HAV;
        ENDSR;

        //=======================================================================
        // Subrotina.: ValParametros
        // Objectivo.: Validar os Parameteros Recebidos
        //=======================================================================
        BEGSR ValParametros;
           //----------------------------------------------------
           // Validar Apólice Recebida
           //----------------------------------------------------
           WOrgOrdApol = WNumOrdAP;
           IF (WNumOrdAP <= *ZEROS);
              WCodErro = 'COB0050';
              LEAVESR;
           ELSE;
              CHAIN WOrgOrdApol SD1AP R#SD1AP;
              IF (NOT %FOUND(SD1AP));
                 WCodErro = 'COB0212'; // A Apólice não está activa
                 LEAVESR;
              ENDIF;
              IF (AP_CDSUP = 'A') AND (WForcar <> 'S');
                 WCodErro = 'COB0213'; // A Apólice Suprimida, forçar no parametro
                 LEAVESR;
              ENDIF;
              IF (AP_CDBLQ = 1);
                 WCodErro = 'COB0214'; // A Apólice sem ordem para ser facturada. Sit1
                 LEAVESR;
              ENDIF;
              IF (AP_CDBLQ = 2);
                 WCodErro = 'COB0215'; // A Apólice sem ordem para ser facturada. Sit2
                 LEAVESR;
              ENDIF;
           ENDIF;

           //----------------------------------------------------
           // Validar Recibo Recebido
           //----------------------------------------------------
           IF (WNumOrdRC <= *ZEROS);
              WIsAvisDebit = *OFF;
              WCodErro = 'COB0216'; // Número de ordem inválido
              LEAVESR;
           ELSE;
              WNumOrdem = WNumOrdRC;
              CHAIN WNumOrdem SD1RCA R#SD1RCA;
              IF (NOT %FOUND(SD1RCA));
                 WCodErro = 'COB0217'; // Recibo inexistente
                 LEAVESR;
              ELSE;
                 IF (REC_CDALT = 'S') OR (REC_CDALT = 'M');
                    WCodErro = 'COB0218'; // Registo Suprimido ou Morto
                    LEAVESR;
                 ENDIF;
                 IF (REC_VCOMIS = *ZEROS) AND (REC_SC = '*');
                    WCodErro = 'COB0219'; // Recibo de comissão sem valores
                    LEAVESR;
                 ENDIF;
                 IF (REC_NUMAD <> *ZEROS) AND (REC_DTAVD <> *ZEROS);
                    WIsAvisDebit = *ON; // Já existe um aviso de débito
                 ENDIF;
              ENDIF;
           ENDIF;

           //----------------------------------------------
           // Validar o Segurado
           //----------------------------------------------
           CHAIN AP_CSEG SD1PE R#SD1PE;
           IF (%FOUND(SD1PE));
              IF (PE_CSIT < 1) AND (PE_CODALT = 'S');
                 WCodErro = 'COB0220'; // Segurado Suprimido no ficheiro
                 LEAVESR;
              ENDIF;
              IF (PE_CSIT >= 1) AND (PE_CODALT <> 'S');
                 WCodErro = 'COB0221'; // Segurado sem Ordem para ser Facturado
                 LEAVESR;
              ENDIF;
              IF (PE_CODALT = 'C') AND (REC_APOLIC <> AP_APOLI);
                 WCodErro = 'COB0222'; // A Apólice do recibo não corresponde
                 LEAVESR;
              ENDIF;
           ENDIF;

           //----------------------------------------------
           // Validar Parametros Opcionais
           //----------------------------------------------
           MONITOR;
              WDescFixa01 = PDescFixa01;
           ON-ERROR;
              WDescFixa01 = *ZEROS;
           ENDMON;
           MONITOR;
              WDescFixa02 = PDescFixa02;
           ON-ERROR;
              WDescFixa02 = *ZEROS;
           ENDMON;

           MONITOR;
              WDescVaria = PDescVaria;
           ON-ERROR;
              WDescVaria = *ZEROS;
           ENDMON;

           MONITOR;
              WCodFactura = PCodFactura;
           ON-ERROR;
              WCodFactura = *ZEROS;
           ENDMON;

           MONITOR;
              WOrigem = POrigem;
           ON-ERROR;
              WOrigem = '999999';
           ENDMON;
        ENDSR;
      /END-FREE
