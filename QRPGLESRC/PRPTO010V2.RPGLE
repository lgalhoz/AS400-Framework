     H AUT(*ALL) ALWNULL(*USRCTL) DFTACTGRP(*NO) ACTGRP(*CALLER)
     H BNDDIR('JMDIR')
     H*=================================================
     H* Programa..: PRPTO010
     H* Objectivo.: Escrever Carta/Acta para acompanhar
     H*             os Avisos Cobrança.
     H* Observação: A última é em Fevereiro de 2015
     H* Compilar com a opção
     H*   CRTLF FILE(QTEMP/SINP01) SRCFILE(LIB.SINO/QDDSSRC)
     H*   CPYF FROMFILE(QS36F/GBASEGRO) TOFILE(QTEMP/GBASEGRO) MBROPT(*REPLACE) CRTFILE(*YES)
     H*=================================================
     FFRPTP001  UF   E           K DISK    PREFIX(RPT_)
     FFRPTP002  UF A E           K DISK    PREFIX(RPT_)
     FFRPTP003  UF A E           K DISK    PREFIX(RPT_)
     FGBASEGRO  UF   E           K DISK    PREFIX(PRD_) EXTFILE(WGBaseGro)
     F                                     USROPN
     FFPRDP12001IF   E           K DISK    PREFIX(TBL_)
     F                                     RENAME(RPRDP120:RPRDP12001)
     FSINP01    IF   E           K DISK    USROPN PREFIX(SIN_)
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
     D WMesCarrega     S              2S 0
     D WQtdeSin        S              3S 0
     D WAnoCont        S              4S 0 INZ(*ZEROS)
     D WMesCont        S              2S 0 INZ(*ZEROS)
     D WMesAlfa        S             12A
     D WMesData        S              2S 0 INZ(*ZEROS)
     D WDataContin     S             30A
     D WDataAltera     S              8S 0
     D WGBaseGro       S             21A
     D WLibProd        S             10A
     D WBaseDio        S             10A
     D WNuApol         S              7S 0
     D WModo           S              1A
     D WAnoMesLimit    S              6S 0
     D WCdReport       S              5S 0
     D WCdPeriodo      S              3S 0
     D WIcExiste       S               N
     D WMsgErro        S              7A   INZ(*BLANKS)
     D*=================================================
      /FREE
        //-----------------------------------------------------------
        // Determinar o modo de Execução
        //-----------------------------------------------------------
        WLibProd = 'QS36F';
        WCdReport = 1;
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

        WGBaseGro = %TRIM(WLibProd)+'/GBASEGRO';
        OPEN GBASEGRO;

        IF (WModo = 'S');
           SETLL *LOVAL GBASEGRO;
           READ GBASEGRO;
           DOW (NOT %EOF(GBASEGRO));
              WMesIni = %DEC(%SUBST(%CHAR((PRD_DTINI+1000000)):4:2):2:0);
              EXSR SelApolice;
              READ GBASEGRO;
           ENDDO;
        ELSEIF (WModo = 'R');
           EXSR ValExecContin;
           IF (WMsgErro <> *BLANKS);
              $ShowDialog(WMsgErro);
           ELSE;
              EXSR FindGBDio;
              IF (WMsgErro = *BLANKS);
                 EXSR Escrever;
                 CHAIN (WCdReport:WCdPeriodo) RRPTP002;
                 RPT_CDESTADO = 'F';
                 UPDATE RRPTP002 %FIELDS(RPT_CDESTADO);
                 $ShowDialog('RPT0010');
              ELSE;
                 $ShowDialog(WMsgErro);
              ENDIF;
           ENDIF;
        ENDIF;

        CLOSE SINP01;
        CLOSE GBASEGRO;
        *INLR = *ON;
        RETURN;

        //=================================================================
        // Subrotina..: FindGBDio
        // Objectivo..: Obter o o ficheiro diário disponível
        //=================================================================
        BEGSR FindGBDio;
           WMesCarrega = %subdt(%date():*MONTHS);
           WBaseDio = 'GBDIO'+%TRIM(%EDITW(WMesCarrega:'0  '))+'Z';
           IF ($Checkobject(WBaseDio:WLibProd));
              WMsgErro = 'RPT0007';
              LEAVESR;
           ENDIF;

           //------------------------------------------------------
           // Criar o Base Dia para receber as Alterações
           //------------------------------------------------------
           MONITOR;
              WStdCmd = 'CRTPF FILE('+%TRIM(WLibProd)+'/'+
                         %TRIM(WBaseDio)+') +
                         SRCFILE(SOURCE/QDDSSRC) +
                         SRCMBR(GBASEDIO) SIZE(*NOMAX)';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);
           ON-ERROR;
              WMSGERRO = 'RPT0008';
              LEAVESR;
           ENDMON;
        ENDSR;

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

        //=================================================================
        // Subrotina..: SelApolice
        // Objectivo..: Selecionar as apólices a escrever e em conformidade
        //              com o critério de selecção
        //=================================================================
        BEGSR SelApolice;
           IF (PRD_RAMO=5) AND (PRD_MED1=924) AND (PRD_SIT = 1) AND
             (PRD_CONDOM <> 'C') AND ((PRD_CF05='S') OR
             (PRD_CF06='S')) AND (WMesIni = WMesCont);

              WQtdeSin = *ZEROS;
              SETLL (PRD_RAMO:PRD_APOL) SINP01;
              READE (PRD_RAMO:PRD_APOL) SINP01;
              DOW (NOT %EOF(SINP01));
                 IF (SIN_CAUSA = '28');
                    WQtdeSin += 1;
                 ENDIF;
                 READE (PRD_RAMO:PRD_APOL) SINP01;
              ENDDO;

              IF (WQtdeSin > 1);
                 LEAVESR;
              ENDIF;

              WIcExiste = *OFF;
              CHAIN (WCdReport:WCdPeriodo:PRD_Ramo:PRD_APOL) RRPTP003;
              IF (%FOUND(FRPTP003));
                 WIcExiste = *ON;
              ENDIF;
              RPT_CDREPORT = WCdReport;
              RPT_NUPERIOD = WCdPeriodo;
              RPT_CDRAMO   = PRD_RAMO;
              RPT_NUAPOL   = PRD_APOL;
              RPT_NUACTA   = PRD_ACTA + 1;
              RPT_NOME     = PRD_SEG;
              RPT_CF05     = PRD_CF05;
              RPT_CF06     = PRD_CF06;
              RPT_NUSEGU   = PRD_NSEG;
              RPT_RPT00302 = %DATE();
              CHAIN (PRD_NSEG) GFSEG;
              IF (%FOUND(GFSEG));
                 RPT_MORADA  = SEG_MORAD;
                 RPT_CDPOST  = SEG_CPOST;
                 RPT_LOCALID = SEG_DESCP;
              ENDIF;

              IF (NOT WIcExiste);
                 RPT_RPT00301 = %DATE();
                 WRITE RRPTP003;
              ELSE;
                 UPDATE RRPTP003 %FIELDS(RPT_RPT00302:RPT_NUACTA:RPT_NOME:
                                      RPT_CF05:RPT_CF06:RPT_MORADA:RPT_NUSEGU:
                                      RPT_CDPOST:RPT_LOCALID);
              ENDIF;
           ENDIF;
        ENDSR;

        //=================================================================
        // Subrotina..: Escrever
        // Objectivo..: Escrever o texto das Cartas
        //=================================================================
        BEGSR Escrever;
           SETLL (WCdReport:WCdPeriodo) RRPTP003;
           READE (WCdReport:WCdPeriodo) RRPTP003;
           DOW (NOT %EOF(FRPTP003));
              WNuApol = RPT_NUAPOL;
              CHAIN (5:WNuApol) GBASEGRO;
              IF (%FOUND(GBASEGRO));
                 WDataAltera = 1000000 + PRD_DTINI;
                 WDataAltera = %DEC(%SUBST(%CHAR(WDataAltera):2:4):4:0)*100+
                               (WAnoCont - 2000);
                 PRD_ACTA += 1;
                 PRD_DALT = WDataAltera;
                 PRD_MALT = 46;
                 UPDATE BASEGRFO %FIELDS(PRD_ACTA:PRD_DALT:PRD_MALT);

                 RPT_RPT00302 = %DATE();
                 RPT_NUACTA   = PRD_ACTA;
                 UPDATE RRPTP003 %FIELDS(RPT_NUACTA:RPT_RPT00302);
                 EXSR CpyRecord;
              ENDIF;
              READE (WCdReport:WCdPeriodo) RRPTP003;
           ENDDO;
        ENDSR;

        //=================================================================
        // Subrotina..: Crtiles
        // Objectivo..: Preparar os ficheiros necessários.
        //=================================================================
        BEGSR CrtFiles;
           WMsgErro = *BLANKS;

           //------------------------------------------------------
           // Criar o ficheiro de processos de Sinistro
           //------------------------------------------------------
           MONITOR;
              WStdCmd = 'DLTF FILE(QTEMP/SINP01)';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);
           ON-ERROR;
           ENDMON;
           MONITOR;
              WStdCmd = 'CRTLF FILE(QTEMP/SINP01) SRCFILE(LIB.SINO/QDDSSRC)';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);
              OPEN SINP01;
           ON-ERROR;
              WMSGERRO = 'RPT0009';
              LEAVESR;
           ENDMON;
        ENDSR;

        //=================================================================
        // Subrotina..: CpyRecord
        // Objectivo..: Criar um novo registo se este ainda não existir
        //              por cópia do Base Dia
        //=================================================================
        BEGSR CpyRecord;
           //--------------------------------------------------------
           // Só cria o registo se não estiver em modo de Simulação
           //--------------------------------------------------------
           MONITOR;
           WStdCmd = 'CPYF FROMFILE('+%TRIM(WLibProd)+'/GBASEGRO) +
                      TOFILE('+%TRIM(WLibProd)+'/'+%TRIM(WBaseDio)+') +
                      MBROPT(*ADD) INCREL((*IF RAMO *EQ 5) +
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
           IF ($checkObject('BGRO'+%SUBST(%CHAR(RPT_ANOMES):3:4):'QS36F'));
              WMsgErro = *BLANKS;
           ELSE;
              WMsgErro = 'RPT0011';
           ENDIF;
        ENDSR;
      /END-FREE
