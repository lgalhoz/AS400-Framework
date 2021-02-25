     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*===========================================================
     H* Programa...: RINTO017
     H* Descrição..: Facturar recibos em Demo Comp, Lda
     H* Observação.: Baseado nos Programas LIb.SD/QS36SRC(SD101) e
     H*                                    LIb.SD/QS36SRC(SD101I)
     H*===========================================================
     FSD1CG3    UF   F   80        DISK    EXTFILE('QTEMP/SD.1.CG3')
     FSD1RCA    UF A F  326     6AIDISK    KEYLOC(5) EXTFILE('QTEMP/SD.1.RCA')
     FSD1AP     UF   F  320     7AIDISK    KEYLOC(1) EXTFILE('QTEMP/SD.1.AP')
     FSD1APM    UF   F  320     7AIDISK    KEYLOC(1) EXTFILE('QTEMP/SD.1.APM')
     F
     FFINTP028  UF A E           K DISK    PREFIX(INT_)
     FFINTP029  UF A E           K DISK    PREFIX(INT_)
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
     D WIsHistorico    S               N
     D WCodMedCCob     S              3S 0
     D WCodMedSCob     S              3S 0
     D WPerComCCob     S              5S 3
     D WPerComSCob     S              5S 3
     D WPerComTot      S              5S 3
     D WComMedCCob     S             11S 2
     D WComMedSCob     S             11S 2
     D WComTotal       S             11S 2
     D WNumOrdem       S              8S 0
     D WDataYMD        S              8S 0
     D WCdRegis        S              9S 0
     D WCodErro        S              7A
     D WNumAltera      S              3S 0
     D WNumOrdAP       S              7S 0
     D
     D #RRN            S              4S 0
     D KTE#CORR        C                   1
     D KTE#COMP        C                   2
     D*===========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCdRegis
     C                   PARM                    WCodErro
     C
      /FREE
        //-----------------------------------------------------------------
        // ForCompr os dados a  inserir no Recibo
        //-----------------------------------------------------------------
        WCodErro = *BLANKS;
        CHAIN (WCdRegis) RINTP028;
        IF (%FOUND(FINTP028));
           IF (INT_CDESTAD = '3');
              EXSR CrtRegisto;
              IF (WCodErro = *BLANKS);
                 EXSR ActRegisto;
              ENDIF;
           ENDIF;
           READ RINTP028;
        ENDIF;

        *INLR = *ON;

        //=======================================================================
        // Subrotina.: CrtRegisto
        // Objectivo.: Criar o Recibo
        //=======================================================================
        BEGSR CrtRegisto;
           //-----------------------------------------------------------------
           // Obter informação da Apólice
           //-----------------------------------------------------------------
           WIsHistorico = *OFF;
           WNumOrdAP = INT_NUMORDAP;
           CHAIN WNumOrdAP SD1AP R#SD1AP;
           IF (NOT %FOUND(SD1AP));
              CHAIN WNumOrdAP SD1APM R#SD1AP;
              IF (NOT %FOUND(SD1APM));
                 WCodErro = 'ERRRRRO';
                 LEAVESR;
              ENDIF;
              WIsHistorico = *ON;
           ENDIF;
           WNumAltera = AP_NUMUL + 1;
           EXSR CalcComissao;

           //-----------------------------------------------------------------
           // Obter o último número disponível
           //-----------------------------------------------------------------
           #RRN = KTE#CORR;
           CHAIN #RRN SD1CG3 R#SD1CG3;
           NUM_NUMERO = NUM_NUMERO + 1;
           WNumOrdem  = NUM_NUMERO;

           //-----------------------------------------------------------------
           // Actualizar a Informação do Recibo e da Apólice
           //-----------------------------------------------------------------
           EXSR AddRecibo;
           EXSR UpdApolice;

           //-----------------------------------------------------------------
           // Actualizar a Numeração
           //-----------------------------------------------------------------
           NUM_NUMERO = WNumOrdem;
           UPDATE SD1CG3 R#SD1CG3;
           WCodErro = *BLANKS;
        ENDSR;

        //=======================================================================
        // Subrotina.: ActRegisto
        // Objectivo.: Actualizar o registo
        //=======================================================================
        BEGSR ActRegisto;
           INT_CDESTAD = '4'; // Aguarda Facturação
           INT_NUMORDRC = WNumOrdem + 1000000;
           UPDATE RINTP028 %FIELDS(INT_NUMORDRC:INT_CDESTAD);

           INT_NUOPERA  = $GetNextKey('FINTP029':'NUOPERA':
                          '(CDREGIS = '''+%CHAR(INT_CDREGIS)+''')');
           INT_CDERRO  = WCodErro;
           INT_DTOPERA = %DATE();
           INT_HROPERA = %TIME();
           INT_USOPERA = S#USR;
           WRITE RINTP029;
        ENDSR;

        //=======================================================================
        // Subrotina.: CalcComissao
        // Objectivo.: Calcular os valores para a Comissão
        //             WCodMedCCob -> Código do Mediador C/Cobrança
        //             WCodMedSCob -> Código do Mediador S/Cobrança
        //             WPerComCCob -> Percentagem Comissão do Mediador C/Cobrança
        //             WPerComSCob -> Percentagem Comissão do Mediador S/Cobrança
        //             WPerComTot  -> Percentagem Comissão Total
        //             WComMedCCob -> Comissão do Mediador C/Cobrança
        //             WComMedSCob -> Comissão do Mediador S/Cobrança
        //             WComTotal   -> Comissão Total
        //=======================================================================
        BEGSR CalcComissao;
           WCodMedCCob = AP_RI1;
           WCodMedSCob = AP_RI2;
           WPerComCCob = AP_RJ1;
           WPerComSCob = AP_RJ2;
           WPerComTot  = AP_COMTO;
           WComMedCCob = %DECH(INT_IMPRSIM * WPerComCCob:11:2);
           WComMedSCob = %DECH(INT_IMPRSIM * WPerComSCob:11:2);
           WComTotal   = INT_IMCOMIS1 + INT_IMCOMIS2 + INT_IMCOMIS3;
        ENDSR;

        //=======================================================================
        // Subrotina.: AddRecibo
        // Objectivo.: Adicionar o reguisto do Recibo
        //=======================================================================
        BEGSR AddRecibo;
           REC_TPCOB   = *BLANKS;
           REC_CODFACT = '1';
           REC_CODMOV  = 15;
           REC_NORD    = WNumOrdem;
           REC_APOLIC  = INT_NUAPOL;
           REC_NUMREC  = INT_NUMREC;
           REC_CCOMP   = INT_CCOMP;
           REC_CSEGUR  = INT_NUMSEG;
           REC_CRAMO   = INT_CDRAMO;
           REC_DTINIC  = $CvtData($DateToInt(INT_DTINICIO):'YYMD':'DMY');
           REC_DTFIM   = $CvtData($DateToInt(INT_DTTERMO):'YYMD':'DMY');
           REC_VTREC   = INT_IMTOTREC;
           REC_VCOMIS  = WComTotal;
           REC_DTREC   = $CvtData($DateToInt(INT_DTENTRAD):'YYMD':'DMY');
           REC_NUMGR   = *BLANKS;
           REC_DTGR    = *BLANKS;
           REC_DTENTR  = $CvtData($DateToInt(%DATE()):'YYMD':'DMY');
           REC_DTLIM   = %CHAR($CvtData($DateToInt(INT_DTLIMITE):'YYMD':'DMY'));
           REC_NUMAD   = *ZEROS;       // Número do Aviso de Débito
           REC_DTAVD   = *ZEROS;       // Data do Aviso de Débito
           REC_CDF     = 15;
           REC_DVAR    = *BLANKS;      // Descrição Variável (Definir uma Constante)
           REC_MCCOB   = WCodMedCCob;
           REC_COMIS1  = WComMedCCob;
           REC_MSCOB   = WCodMedSCob;
           REC_COMIS2  = WComMedSCob;
           REC_MED3    = *ZEROS;
           REC_COMIS3  = *ZEROS;
           REC_CGEST   = '0B';
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
           REC_PRSIMP  = INT_IMPRSIM;
           REC_VALIE   = *ZEROS;
           REC_DIFPAG  = *ZEROS;
           REC_UNACHX  = *BLANKS;
           REC_CSEGX   = *BLANKS;
           REC_CARTA   = *BLANKS;
           REC_NORD2   = WNumOrdem + 1000000;
           REC_NREC2   = INT_NUMREC;
           REC_FILL1   = *BLANKS;
           REC_FILL2   = $CvtData($DateToInt(%DATE()):'YYMD':'DMY');
           REC_FILL3   = *BLANKS;
           REC_CDFACT  = '1';
           REC_FILL4   = *BLANKS;
           REC_CDALT   = 'N';
           REC_FILL5   = '1';
           REC_SNDAVD  = '0';
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
           WRITE SD1RCA R#SD1RCA;
        ENDSR;

        //=======================================================================
        // Subrotina.: UpdApolice
        // Objectivo.: Actualizar o registo
        //=======================================================================
        BEGSR UpdApolice;
           AP_PRSRN = INT_IMPRSIM;
           AP_PRTRN = INT_IMTOTREC;
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
        ENDSR;
      /END-FREE
