     H AUT(*ALL) ALWNULL(*USRCTL) DFTACTGRP(*NO) ACTGRP(*CALLER)
     H BNDDIR('JMDIR') DATEDIT(*DMY)
     H*========================================================================================
     H* Programa..: PRPTO012
     H* Objectivo.: Aplicar descontos às apólices sem descontos
     H*             e escrever carta para acompanhar o Aviso
     H* Observação:
     H* Compilar com a opção
     H*   CRTLF FILE(QTEMP/SINP01) SRCFILE(LIB.SINO/QDDSSRC)
     H*   CPYF FROMFILE(QS36F/GBASEGRA) TOFILE(QTEMP/GBASEGRA) MBROPT(*REPLACE) CRTFILE(*YES)
     H*========================================================================================
     FFRPTP001  UF   E           K DISK    PREFIX(RPT_)
     FFRPTP002  UF A E           K DISK    PREFIX(RPT_)
     FFRPTP003  UF A E           K DISK    PREFIX(RPT_)
     FGBASEGRO  UF   E           K DISK    PREFIX(AUT_) EXTFILE(WGBaseGra)
     F                                     USROPN
     FFPRDP12001IF   E           K DISK    PREFIX(TBL_)
     F                                     RENAME(RPRDP120:RPRDP12001)
     FSINP01    IF   E           K DISK    USROPN PREFIX(SIN_)
     FGFSEG     IF   E           K DISK    PREFIX(SEG_)

     F*GBASEGX1  IF   E           K DISK    RENAME(BASEGRFA:BASESEG) PREFIX(GX1_)
     F*GREGAW0   IF   E           K DISK    PREFIX(AW0_)
     F*GREGAW1   IF   E           K DISK    RENAME(REGA:REGA1) PREFIX(AW1)
     F*APSDESC   UF A E             DISK    PREFIX(DSC_)
     F*========================================================================================
      /Copy QRPGLESRC,SGRLO010

     D*==> Lista de Parametros
     D shell           PR                  EXTPGM('QCMDEXC')
     D   StdCmd                     200A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5
     D WStdCmd         S            200A
     D WStdLen         S             15P 5
     D
     D*==> Campos de Trabalho
     D WMes            S              2S 0
     D WAnoSinistro    S              4S 0
     D WAnoReferencia  S              4S 0
     D WIsToSelect     S               N
     D WDataSinistro   S               D
     D
     D WMesIni         S              2S 0
     D WMesCarrega     S              2S 0
     D WQtdeSin        S              3S 0
     D WAnoCont        S              4S 0 INZ(*ZEROS)
     D WMesCont        S              2S 0 INZ(*ZEROS)
     D WMesAlfa        S             12A
     D WMesData        S              2S 0 INZ(*ZEROS)
     D WDataContin     S             30A
     D WDataAltera     S              8S 0
     D WGBaseGra       S             21A
     D WLibProd        S             10A
     D WBaseDio        S             10A
     D WNuApol         S              7S 0
     D WModo           S              1A
     D WAnoMesLimit    S              6S 0
     D WCdReport       S              5S 0
     D WCdPeriodo      S              3S 0
     D WIcExiste       S               N
     D WMsgErro        S              7A   INZ(*BLANKS)
     D*========================================================================================
      /FREE
        //-----------------------------------------------------------
        // Determinar o modo de Execução
        //-----------------------------------------------------------
        WLibProd = 'QS36F';
        WCdReport = 2;
        EXSR GetLastProc;
        IF (WMsgErro <> *BLANKS);
           $ShowDialog(WMsgErro);
           RETURN;
        ENDIF;

        EXSR CrtFiles;
        IF (WMsgErro <> *BLANKS);
           $ShowDialog(WmsgErro);
           RETURN;
        ENDIF;

        WGBaseGra = %TRIM(WLibProd)+'/GBASEGRA';
        OPEN GBASEGRA;

        IF (WModo = 'S');
        WAnoReferencia = %SUBDT(%DATE():*YEARS) - 2;
        SETLL (*LOVAL) GBASEGRA;
        READ GBASEGRA;
        DOW (NOT %EOF(GBASEGRA));
           EXSR IsToSelect;
           IF (WIsToSelect);
              //------------------------------------------------------------
              // ForCompr a chave do Contrato
              //------------------------------------------------------------
              DSC_MES1 = AUT_MES1;
              DSC_NSEG = AUT_NSEG;
              DSC_RAMO = AUT_RAMO;
              DSC_APOL = AUT_APOL;
              DSC_MED1 = AUT_MED1;
              DSC_JMM1 = AUT_JMM1;
              DSC_JMM2 = AUT_JMM2;
              DSC_PRS  = *ZEROS;

              //------------------------------------------------------------
              // Analisar o tipo de Contrato
              //------------------------------------------------------------
              DSC_DPSN   = *BLANKS;
              IF (AUT_CAPDP <> *ZEROS);
                 DSC_DPSN = 'S'; // Tem danos próprios
              ENDIF;

              DSC_OCSN   = *BLANKS;
              IF (AUT_CAPOCU <> *ZEROS);
                 DSC_OCSN = 'S'; // Tem ocupantes
              ENDIF;

              DSC_PRIVSN = *BLANKS;
              IF (AUT_PRIV <> *ZEROS);
                 DSC_PRIVSN = 'S'; // Tem privação de uso
              ENDIF;

              DSC_VSASN  = *BLANKS;
              IF (AUT_VSA <> *ZEROS);
                 DSC_VSASN = 'S';
              ENDIF;

              DSC_VIASN  = *BLANKS;
              IF (AUT_VIA <> *ZEROS);
                 DSC_VIASN = 'S';
              ENDIF;

              DSC_AVSN   = *BLANKS;
              IF (AUT_VIAG <> *ZEROS);
                 DSC_AVSN = 'S';  // Tem assistência em viagem
              ENDIF;

              DSC_PJSN   = *BLANKS;
              IF (AUT_PJUR <> *ZEROS);
                 DSC_PJSN = 'S'; // Tem protecção juridica
              ENDIF;

              CHAIN (AUT_RAMO:AUT_APOL) REGA;
              IF (NOT %FOUND(GREGAW0));
                 CHAIN (AUT_RAMO:AUT_APOL) REGA1;
                 DSC_PRS = *ZEROS;
              ENDIF;

              DSC_OAPSN = *BLANKS;
              SETLL (AUT_NSEG) GBASEGX1;
              READE (AUT_NSEG) GBASEGX1;
              DOW (NOT %EOF(GBASEGX1));
                 IF (GX1_SIT = 1) OR (GX1_SIT = 4);
                    DSC_OAPSN = 'S';
                 ENDIF;
                 READE (AUT_NSEG) GBASEGX1;
              ENDDO;
              WRITE APSDESF1;
           ENDIF;
           READ GBASEGRA;
        ENDDO;
        RETURN;

        //=================================================================
        // Subrotina..: GetLastProc
        // Objectivo..: Obter o último ano e mês Processados, e validar se
        //              o programa está dentro do período de validade.
        //=================================================================
        BEGSR GetLastProc;
           WMsgErro = *BLANKS;
           //---------------------------------------------
           // Obter o ano e Mês do limite
           //---------------------------------------------
           CHAIN (WCdReport) RRPTP001;
           IF (NOT %FOUND(FRPTP001));
              WMSGERRO = 'RPT0001';
              LEAVESR;
           ENDIF;

           IF (RPT_CDESTADO <> 'A');
              WMSGERRO = 'RPT0002';
              LEAVESR;
           ENDIF;
           WAnoMesLimit = RPT_AnoMes;

           WCdPeriodo = RPT_PERACTUA;
           CHAIN (WCdReport:WCdPeriodo) RRPTP002;
           IF (NOT %FOUND(FRPTP002));
              WMSGERRO = 'RPT0004';
              LEAVESR;
           ENDIF;

           //-------------------------------------------------
           // Validar em que estado se encontra o período
           //-------------------------------------------------
           IF (RPT_ANOMES > WAnoMesLimit);
              RPT_CDESTADO = 'F';
              UPDATE RRPTP001 %FIELDS(RPT_CDESTADO);
              UPDATE RRPTP002 %FIELDS(RPT_CDESTADO);
              WMsgErro = 'RPT0003';
              LEAVESR;
           ENDIF;

           WAnoCont = %INT(RPT_ANOMES / 100);
           WMesCont = RPT_ANOMES - (WAnoCont * 100);
           IF (RPT_CDESTADO = 'F');
               WMesCont += 1;
               IF (WMesCont > 12);
                  WMesCont -= 12;
                  WAnoCont += 1;
               ENDIF;
           ENDIF;

           CHAIN (WAnoCont:WMesCont) RPRDP12001;
           IF (NOT %FOUND(FPRDP12001));
              IF (RPT_CDESTADO = 'F');
                 WMsgErro = 'RPT0005';
              ELSE;
                 WMsgErro = 'RPT0006';
              ENDIF;
              LEAVESR;
           ELSE;
              IF (TBL_ICMODO <> 'R') AND (TBL_ICMODO <> 'S');
                 WMsgErro = 'RPT0005';
                 LEAVESR;
              ENDIF;
           ENDIF;

           IF ((RPT_ANOMES < ((WAnoCont*100)+WMesCont)) AND
             (RPT_CDESTADO = 'F'));
              WCdPeriodo   += 1;
              RPT_PERACTUA  = WCdPeriodo;
              UPDATE RRPTP001 %FIELDS(RPT_PERACTUA);

              RPT_CDREPORT = WCdReport;
              RPT_NUPERIOD = WCdPeriodo;
              RPT_ANOMES   = ((WAnoCont*100)+WMesCont);
              RPT_CODCARR  = *BLANKS;
              WMesData     = %SUBDT(TBL_DATEXEC:*MONTHS);
              WMesAlfa     = $GetDescricao(38:%TRIM(%EDITW(WMesData:'0  ')));
              RPT_DTCARTAS = 'Lisboa, ' +
                             %CHAR(%SUBDT(TBL_DATEXEC:*DAYS)) + ' de '
                             + %TRIM(WMesAlfa) + ' de ' +
                             %CHAR(%SUBDT(TBL_DATEXEC:*YEARS));
              RPT_CDESTADO = 'A';
              WRITE RRPTP002;

              WModo = 'S'; // Modo de Simulação
           ELSEIF ((RPT_ANOMES = ((WAnocont*100)+WMescont)));
              IF (TBL_ICMODO = 'R');
                 WModo = 'R'; // Modo Real
              ELSE;
                 WModo = 'S'; // Modo Simulação
              ENDIF;
           ENDIF;
        ENDSR;

        //======================================================================
        // SubRotina.: IsToSelect
        // Descrição.: Aplicar o Critério de Selecção para seleccionar ou não
        //             as Apólices.
        //======================================================================
        BEGSR IsToSelect;
           WDataSinistro = $IntToDate($CvtData(AUT_DULSIN:'DMY':'YYMD'));
           WAnoSinistro = %SUBDT(WDataSinistro:*YEARS);
           WAnoSinistro = *ZEROS;
           WIsToSelect = *OFF;

           IF (AUT_RAMO = 50) AND (AUT_SIT = 1) AND (AUT_MES1 = WMes) AND
             (WAnoSinistro < WAnoReferencia) AND (AUT_DSCESP = *ZEROS) AND
             (AUT_MED1 <> 925) AND (AUT_JMM1 <> 272) AND (AUT_JMM2 <> 272) AND
             (AUT_JMM1 <> 415) AND (AUT_JMM2 <> 415) AND
             ((AUT_CATV = 1) OR (AUT_CATV = 2) OR (AUT_CATV = 6));

              WIsToSelect = *ON;
           ENDIF;
        ENDSR;
      /END-FREE
