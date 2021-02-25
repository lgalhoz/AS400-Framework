     H AUT(*ALL) ALWNULL(*USRCTL) DFTACTGRP(*NO) ACTGRP(*CALLER)
     H BNDDIR('JMDIR')
     H*=================================================
     H* Programa..: PRPTO012
     H* Objectivo.: Escrever Carta/Acta para acompanhar
     H*             os Avisos Cobrança.
     H* Compilar com a opção
     H*   CPYF FROMFILE(QS36F/GBASEDIA) TOFILE(QTEMP/GBASEDIA) MBROPT(*REPLACE) CRTFILE(*YES)
     H*=================================================
     FFRPTP001  UF   E           K DISK    PREFIX(RPT_)
     FFRPTP002  UF A E           K DISK    PREFIX(RPT_)
     FFRPTP003  UF A E           K DISK    PREFIX(RPT_)
     FGBASEGRA  IF   E           K DISK    PREFIX(PRD_)
     FGBASEDIA  UF   E           K DISK    PREFIX(PRD_) EXTFILE(WGBASEDIA)
     F                                     USROPN
     FFPRDP12001IF   E           K DISK    PREFIX(TBL_)
     F                                     RENAME(RPRDP120:RPRDP12001)
     FGFSEG     IF   E           K DISK    PREFIX(SEG_)
     F*=================================================
      /COPY QRPGLESRC,SGRLO010
      /COPY QRPGLESRC,STBLO010
      /COPY QRPGLESRC,SSECO010
      /COPY QRPGLESRC,SDBSO010

     D*==> Lista de Parametros
     D shell           PR                  EXTPGM('QCMDEXC')
     D   StdCmd                     200A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5
     D WStdCmd         S            200A
     D WStdLen         S             15P 5
     D
     D*==> Campos de Trabalho
     D WMesIni         S              2S 0
     D WDiaIni         S              2S 0
     D WMesCarrega     S              2S 0
     D WAnoCont        S              4S 0 INZ(*ZEROS)
     D WMesCont        S              2S 0 INZ(*ZEROS)
     D WMesAlfa        S             12A
     D WMesData        S              2S 0 INZ(*ZEROS)
     D WDataContin     S             30A
     D WDataAltera     S              8S 0
     D WGBaseDia       S             21A
     D WLibProd        S             10A
     D WBaseDia        S             10A
     D WNuApol         S              7S 0
     D WModo           S              1A
     D WAnoMesLimit    S              6S 0
     D WCdReport       S              5S 0
     D WCdPeriodo      S              3S 0
     D WIcExiste       S               N
     D WIsToSelect     S               N
     D
     D WDatSinist      S              8S 0
     D WDatlimSin      S              8S 0
     D WDatInicio      S              8S 0
     D WDatLimIni      S              8S 0
     D WDatVencim      S              8S 0
     D
     D WMsgErro        S              7A   INZ(*BLANKS)
     D
     D*==> Constantes
     D MOD#Simula      C                   'S'
     D MOD#Real        C                   'R'
     D*=================================================
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

        WGBaseDia = %TRIM(WLibProd)+'/GBASEDIA';

        IF (WModo = MOD#Simula);
           SETLL *LOVAL GBASEGRA;
           READ GBASEGRA;
           DOW (NOT %EOF(GBASEGRA));
              EXSR SelApolice;
              READ GBASEGRA;
           ENDDO;
        ELSEIF (WModo = MOd#Real);
           EXSR ValExecContin;
           IF (WMsgErro <> *BLANKS);
              $ShowDialog(WMsgErro);
           ELSE;
              EXSR Escrever;
              CHAIN (WCdReport:WCdPeriodo) RRPTP002;
              RPT_CDESTADO = 'F';
              UPDATE RRPTP002 %FIELDS(RPT_CDESTADO);
           ENDIF;
        ENDIF;

        *INLR = *ON;
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
           CHAIN(N) (WCdReport) RRPTP001;
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
           CHAIN(N) (WCdReport:WCdPeriodo) RRPTP002;
           IF (NOT %FOUND(FRPTP002));
              WMSGERRO = 'RPT0004';
              LEAVESR;
           ENDIF;

           //-------------------------------------------------
           // Validar em que estado se encontra o período
           //-------------------------------------------------
           IF (RPT_ANOMES > WAnoMesLimit);
              CHAIN (WCdReport) RRPTP001;
              CHAIN (WCdReport:WCdPeriodo) RRPTP002;
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
              CHAIN (WCdReport) RRPTP001;
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
              WModo = MOD#Simula; // Modo de Simulação
           ELSEIF ((RPT_ANOMES = ((WAnocont*100)+WMescont)));
              IF (TBL_ICMODO = 'R');
                 WModo = MOD#Real; // Modo Real
              ELSE;
                 WModo = MOD#Simula; // Modo Simulação
              ENDIF;
           ENDIF;
        ENDSR;

        //=================================================================
        // Subrotina..: SelApolice
        // Objectivo..: Selecionar as apólices a escrever e em conformidade
        //              com o critério de selecção
        //=================================================================
        BEGSR SelApolice;
           EXSR ChkCriteria;
           IF (WIsToSelect);
              WIcExiste = *OFF;
              CHAIN (WCdReport:WCdPeriodo:PRD_Ramo:PRD_APOL) RRPTP003;
              IF (%FOUND(FRPTP003));
                 WIcExiste = *ON;
              ENDIF;
              RPT_CDREPORT = WCdReport;
              RPT_NUPERIOD = WCdPeriodo;
              RPT_CDRAMO   = PRD_RAMO;
              RPT_NUAPOL   = PRD_APOL;
              RPT_NOME     = PRD_SEG;
              RPT_NUSEGU   = PRD_NSEG;
              RPT_RPT00302 = %DATE();

              CHAIN (PRD_NSEG) GFSEG;
              IF (%FOUND(GFSEG));
                 RPT_MORADA  = SEG_MORAD;
                 RPT_CDPOST  = SEG_CPOST;
                 RPT_LOCALID = SEG_DESCP;
              ENDIF;

              RPT_CODFORM = 'AU01';
              RPT_EXTDATA = 'Apólice a aguardar';

              IF (NOT WIcExiste);
                 RPT_RPT00301 = %DATE();
                 WRITE RRPTP003;
              ELSE;
                 UPDATE RRPTP003 %FIELDS(RPT_RPT00302:RPT_NOME:
                                      RPT_MORADA:RPT_NUSEGU:
                                      RPT_CDPOST:RPT_LOCALID:
                                      RPT_EXTDATA);
              ENDIF;
           ENDIF;
        ENDSR;

        //=================================================================
        // Subrotina..: ChkCriteria;
        // Objectivo..: Validar se A Apólice é para ser seleccionada ou não
        //              de acordo com o critério de selecção.
        //              São excluidas as Empresas (NSEG > 10.000).
        //=================================================================
        BEGSR ChkCriteria;
           WIsToSelect = *OFF;
           WMesIni = %DEC(%SUBST(%CHAR((PRD_DTINI+1000000)):4:2):2:0);
           WDiaIni = %DEC(%SUBST(%CHAR((PRD_DTINI+1000000)):2:2):2:0);
           IF (PRD_RAMO=50) AND (PRD_DSCESP = 0) AND (PRD_SIT = 1) AND
              (WMesIni = WMesCont) AND (PRD_MED1 <> 925) AND
              (PRD_JMM1 <> 272) AND (PRD_JMM2 <> 272)
              AND ((PRD_CATV = 1) OR (PRD_CATV = 2)
              OR (PRD_CATV = 6)) AND (PRD_NSEG > 10000);

              //-------------------------------------------------------
              // ForCompr as datas
              //-------------------------------------------------------
              WDatSinist = $cvtData(PRD_DULSIN:'DMY':'YYMD');
              WDatInicio = $CvtData(PRD_DTINI:'DMY':'YYMD');
              WDatlimSin = (WAnoCont-3)*10000+WMesIni*100+WDiaIni;
              WDatLimIni = (WAnoCont-3)*10000+WMesIni*100+WDiaIni;
              WDatVencim = WAnoCont*10000+WMesIni*100+WDiaIni;

              IF (WDatInicio < WDatLimIni) AND (WDatSinist < WDatLimSin);
                 WIsToSelect = *ON;
              ENDIF;
           ENDIF;
        ENDSR;

        //=================================================================
        // Subrotina..: Escrever
        // Objectivo..: Escrever o texto das Cartas
        //=================================================================
        BEGSR Escrever;
           WDataAltera = $DateToInt(%DATE());
           WDataAltera -= 20000000;
           WDataAltera  = $CvtData(WDataAltera:'YYMD':'DMY');

           SETLL (WCdReport:WCdPeriodo) RRPTP003;
           READE (WCdReport:WCdPeriodo) RRPTP003;
           DOW (NOT %EOF(FRPTP003));
              WNuApol = RPT_NUAPOL;
              CHAIN (RPT_CDRAMO:RPT_NUAPOL) GBASEGRA;
              IF (%FOUND(GBASEGRA));
                 EXSR ChkCriteria;
                 IF (WIsToSelect);
                    EXSR CpyRecord;
                    OPEN GBASEDIA;
                    CHAIN (RPT_CDRAMO:RPT_NUAPOL) GBASEDIA;
                    EXSR ChkCriteria;
                    IF (WIsToSelect);
                       PRD_TALT = 2;
                       PRD_MALT = 15;
                       PRD_DSCESP = 20,0;
                       PRD_DTALT = WDataAltera;
                       UPDATE BASEDIFA %FIELDS(PRD_TALT:PRD_MALT:
                                               PRD_DSCESP:PRD_DTALT);
                       IF (RPT_NUSEGU > 9999);
                          RPT_EXTDATA = 'Apólice Processada';
                          RPT_CODFORM = 'AU01';
                       ELSE;
                          RPT_EXTDATA = 'Apólice Processada - +
                                         Não imprime por ser uma Empresa';
                          RPT_CODFORM = 'NULO';
                       ENDIF;
                    ENDIF;
                 ENDIF;

                 IF (NOT WIsToSelect);
                    RPT_EXTDATA = 'Apólice Excluida';
                    RPT_CODFORM = 'NULO';
                 ENDIF;

                 RPT_RPT00302 = %DATE();
                 UPDATE RRPTP003 %FIELDS(RPT_RPT00302:RPT_EXTDATA:RPT_CODFORM);
                 CLOSE GBASEDIA;
              ENDIF;
              READE (WCdReport:WCdPeriodo) RRPTP003;
           ENDDO;
        ENDSR;

        //=================================================================
        // Subrotina..: CpyRecord
        // Objectivo..: Criar um novo registo se este ainda não existir
        //              por cópia do Base Dia
        //=================================================================
        BEGSR CpyRecord;
           //--------------------------------------------------------
           // Criar o ficheiro para receber os dados
           //--------------------------------------------------------
           IF (NOT $checkObject('GBASEDIA':%TRIM(WLibProd)));
              MONITOR;
                 WStdCmd = 'CRTPF FILE('+%TRIM(WLibProd)+'/GBASEDIA) +
                            SRCFILE(SOURCE/QDDSSRC) +
                            SIZE(*NOMAX)';
                 WStdLen = %LEN(WStdCmd);
                 Shell(WStdCmd:WStdLen);
              ON-ERROR;
              ENDMON;
           ENDIF;

           //--------------------------------------------------------
           // Só cria o registo se não estiver em modo de Simulação
           //--------------------------------------------------------
           MONITOR;
           WStdCmd = 'CPYF FROMFILE(QS36F/GBASEGRA) +
                      TOFILE('+%TRIM(WLibProd)+'/GBASEDIA) +
                      MBROPT(*ADD) INCREL((*IF RAMO *EQ 50) +
                      (*AND APOL *EQ ' + %CHAR(WNuApol) + '))';
           WStdLen = %LEN(WStdCmd);
           shell(WStdCmd:WStdLen);
           ON-ERROR;
           ENDMON;
        ENDSR;

        //=================================================================
        // Subrotina..: ValExecContin
        // Objectivo..: Confirmar se os continuados já correram
        //=================================================================
        BEGSR ValExecContin;
           IF ($checkObject('BGRA'+%SUBST(%CHAR(RPT_ANOMES):3:4):'QS36F'));
              WMsgErro = 'RPT0011';
           ELSE;
              WMsgErro = *BLANKS;
           ENDIF;
        ENDSR;
      /END-FREE
