     H AUT(*ALL) ALWNULL(*USRCTL) DFTACTGRP(*NO) ACTGRP(*CALLER)
     H BNDDIR('JMDIR')
     H*=================================================
     H* Programa..: PRPTO016
     H* Objectivo.: Alterar Clausulas dos Contratos de AT
     H* Compilar com a opção
     H*   CPYF FROMFILE(QS36F/GBASEGRO) TOFILE(QTEMP/GBASEGRO) MBROPT(*REPLACE) CRTFILE(*YES)
     H*=================================================
     FGFSEG     IF   E           K DISK    PREFIX(SEG_)
     FGBASEGRO  IF   E           K DISK    PREFIX(PRD_) EXTFILE(WGBaseGro)
     F                                     USROPN
     FGBASEDIO  UF   E           K DISK    PREFIX(PRD_) EXTFILE(WGBaseDio)
     F                                     USROPN
     FFRPTP001  UF   E           K DISK    PREFIX(RPT_)
     FFRPTP002  UF A E           K DISK    PREFIX(RPT_)
     FFRPTP003  UF A E           K DISK    PREFIX(RPT_)
     FFRPTP00301IF   E           K DISK    PREFIX(RPT_)
     F                                     RENAME(RRPTP003:RRPTP00301)
     FFPRDP12002IF   E           K DISK    PREFIX(TBL_)
     F                                     RENAME(RPRDP120:RPRDP12002)
     F*=================================================
      /COPY QRPGLESRC,SGRLO010
      /COPY QRPGLESRC,STBLO010
      /COPY QRPGLESRC,SSECO010
      /COPY QRPGLESRC,SDBSO010

     D*==> Estrutura de dados com campos a mudar
     D LST#AT          DS                  LIKEDS(REC#AT) DIM(1000)
     D REC#AT          DS                  QUALIFIED
     D   Ramo                         2S 0
     D   Apolice                      7S 0
     D   CL01                         2S 0
     D   CL02                         2S 0
     D   CL03                         2S 0
     D   CL04                         2S 0
     D   CL05                         2S 0
     D   CL06                         2S 0
     D   ACTA                         3S 0
     D   MOD                          1A
     D   DATALT                       6S 0
     D   MALT                         2S 0
     D   TPT                          1A
     D   IsEmpty                       N
     D WIdx            S              3S 0
     D
     D*==> Validar Clausulas
     D CLTOSEL         PR                  LIKEDS(REC#AT)
     D  Registo                            LIKEDS(REC#AT) Const
     D  Grupo                         1A   Const
     D
     D*==> Lista de Parametros
     D shell           PR                  EXTPGM('QCMDEXC')
     D   StdCmd                     200A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5
     D WStdCmd         S            200A
     D WStdLen         S             15P 5
     D
     D*==> Campos de Trabalho
     D WMesIni         S              2S 0
     D WAnoCont        S              4S 0 INZ(*ZEROS)
     D WMesCont        S              2S 0 INZ(*ZEROS)
     D WDataAltera     S              8S 0 INZ(*ZEROS)
     D WDataContin     S             30A
     D
     D WGBaseGro       S             21A
     D WGBaseDio       S             21A
     D WLibProd        S             10A
     D
     D WModo           S              1A
     D WAnoMesLimite   S              6S 0 INZ(*ZEROS)
     D WCdReport       S              5S 0
     D WCdPeriodo      S              3S 0
     D WNuSequ         S              5S 0
     D WNuApol         S              7S 0
     D
     D WSession        S              5S 0
     D WIsToSelect     S               N
     D WIcExiste       S               N
     D WIsToCreate     S               N
     D WMsgErro        S              7A   INZ(*BLANKS)
     D*=================================================
      /FREE
        //-----------------------------------------------------------
        // Determinar o modo de Execução
        //-----------------------------------------------------------
        WLibProd = 'QTEMP';  // Modo de testes
        WCdReport = 3;
        WNuSequ   = *ZEROS;
        EXSR GetLastProc;
        IF (WMsgErro <> *BLANKS);
           $ShowDialog(WMsgErro);
           RETURN;
        ENDIF;

        //--------------------------------------
        // Identificar os registos a Processar
        //--------------------------------------
        WIdx = *ZEROS;
        LST#AT(WIdx).IsEmpty = *ON;
        WGBaseGro = %TRIM(WLibProd)+'/GBASEGRO';
        OPEN GBASEGRO;
        SETLL *LOVAL GBASEGRO;
        DOW (NOT %EOF(GBASEGRO));
           EXSR ValApolice;
           READ GBASEGRO;
        ENDDO;
        CLOSE GBASEGRO;

        //--------------------------------------
        // Processar os registos
        //--------------------------------------
        IF (WModo = 'R');
           WIdx = *ZEROS;
           OPEN GBASEDIO;
           DOW (NOT LST#AT(WIdx).IsEmpty);
              WGBaseDio = %TRIM(WLibProd)+'/GBASEDIO';
              EXSR CrtRecDIO;
              WIdx += 1;
           ENDDO;

           CLOSE GBASEDIO;
           CHAIN (WCdReport:WCdPeriodo) RRPTP002;
           RPT_CDESTADO = 'F';
           UPDATE RRPTP002 %FIELDS(RPT_CDESTADO);
           $ShowDialog('RPT0010');
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
           // Obter informação sobre o prcesso
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
           WAnoMesLimite = RPT_AnoMes;   // Ano/Mes de termo do processo
           WCdPeriodo = RPT_PERACTUA;   // Último período processado

           //---------------------------------------------
           // Obter o ano e Mês dos Continuados e modo
           //---------------------------------------------
           WSession = $SetFiles('FPRDP120');
           $SetFields('ANOIDX,MESIDX,ICMODO');
           $SetCriteria('CODIDX = (SELECT MAX(CODIDX) FROM FPRDP120)');
           IF ($GetRecord(1) > *ZEROS);
              WModo    = $GetField('ICMODO');
              WAnoCont = $GetFieldNum('ANOIDX');
              WMesCont = $GetFieldNum('MESIDX');
           ENDIF;
           $RmvDBSession(WSession);

           //-------------------------------------------------
           // Validar se o processo está dentro da validade
           //-------------------------------------------------
           IF (((WAnoCont*100)+WMesCont) > WAnoMesLimite);
              RPT_CDESTADO = 'F';
              UPDATE RRPTP001 %FIELDS(RPT_CDESTADO);
              WMsgErro = 'RPT0003';
              LEAVESR;
           ENDIF;

           //-------------------------------------------------
           // Validar se o processo está dentro da validade
           //-------------------------------------------------
           WIsToCreate = *OFF;
           IF (WCdPeriodo > *ZEROS);
              CHAIN(N) (WCdReport:WCdPeriodo) RRPTP002;
              IF (NOT %FOUND(FRPTP002));
                 WMSGERRO = 'RPT0004';
                 LEAVESR;
              ENDIF;
              IF (((WAnoCont*100)+WMesCont) > RPT_ANOMES);
                 WIsToCreate = *ON;
              ENDIF;
           ELSE;
              WIsToCreate = *ON;
           ENDIF;

           IF (WIsToCreate);
              WCdPeriodo = $GetNextKey('FRPTP002':'NUPERIOD':
                           'CDREPORT = ' + %CHAR(WCdReport) + '');
              RPT_PERACTUA  = WCdPeriodo;
              UPDATE RRPTP001 %FIELDS(RPT_PERACTUA);

              RPT_CDREPORT = WCdReport;
              RPT_NUPERIOD = WCdPeriodo;
              RPT_ANOMES   = ((WAnoCont*100)+WMesCont);
              RPT_CODCARR  = *BLANKS;
           //-------------------------------------------------------
           // WMesData     = %SUBDT(TBL_DATEXEC:*MONTHS);
           // WMesAlfa     = $GetDescricao(38:%TRIM(%EDITW(WMesData:'0  ')));
           // RPT_DTCARTAS = 'Lisboa, ' +
           //                %CHAR(%SUBDT(TBL_DATEXEC:*DAYS)) + ' de '
           //                + %TRIM(WMesAlfa) + ' de ' +
           //                %CHAR(%SUBDT(TBL_DATEXEC:*YEARS));
           //-------------------------------------------------------
              RPT_CDESTADO = 'A';
              WRITE RRPTP002;
           ENDIF;
        ENDSR;

        //=================================================================
        // Subrotina..: ValApolice
        // Objectivo..: Selecionar as apólices a escrever e em conformidade
        //              com o critério de selecção
        //=================================================================
        BEGSR ValApolice;
           IF (PRD_RAMO=15) AND (PRD_SIT=1) AND (PRD_MES1=WMesCont) AND
             (PRD_MOD='FP');
          //  AND (CLTOSEL(PRD_CL01:'A') OR
          //  CLTOSEL(PRD_CL02:'A') OR CLTOSEL(PRD_CL03:'A') OR
          //  CLTOSEL(PRD_CL04:'A') OR CLTOSEL(PRD_CL05:'A') OR
          //  CLTOSEL(PRD_CL06:'A'));

              //--------------------------------------------------
              // Actualizar listagem de controlo
              //--------------------------------------------------
              LST#AT(WIdx).Ramo    = PRD_RAMO;
              LST#AT(WIdx).Apolice = PRD_APOL;
              LST#AT(WIdx).CL01    = PRD_CL01;
              LST#AT(WIdx).CL02    = PRD_CL02;
              LST#AT(WIdx).CL03    = PRD_CL03;
              LST#AT(WIdx).CL04    = PRD_CL04;
              LST#AT(WIdx).CL05    = PRD_CL05;
              LST#AT(WIdx).CL06    = PRD_CL06;
              LST#AT(WIdx).MOD     = %SUBST(PRD_MOD:1:1);
              LST#AT(WIdx).TPT     = %SUBST(PRD_MOD:2:1);
              LST#AT(WIdx).IsEmpty = *OFF;
              WIdx += 1;
              LST#AT(WIdx).IsEmpty = *ON;

              //--------------------------------------------------
              // Actualizar lista de registos alvo da alteração
              //--------------------------------------------------
              CHAIN (WCdReport:WCdPeriodo:PRD_Ramo:PRD_APOL) RRPTP00301;
              IF (%FOUND(FRPTP00301));
                 CHAIN (WCdReport:WCdPeriodo:RPT_NUSEQU) RRPTP003;
                 WIcExiste = *ON;
              ELSE;
                 IF (WNuSequ = *ZEROS);
                    WNuSequ = $GetNextKey('FRPTP003':'NUSEQU':
                              'CDREPORT = ' + %CHAR(WCdReport) + ' AND ' +
                              'CDPERIOD = ' + %CHAR(WCdPeriodo) + '');
                 ELSE;
                    WNuSequ += 1;
                 ENDIF;
                 WIcExiste = *OFF;
              ENDIF;

              RPT_CDREPORT = WCdReport;
              RPT_NUPERIOD = WCdPeriodo;
              RPT_NUSEQU   = WNuSequ;
              RPT_CDRAMO   = PRD_RAMO;
              RPT_NUAPOL   = PRD_APOL;
              RPT_NUACTA   = PRD_ACTA + 1;
              RPT_NOME     = PRD_SEG;
              RPT_NUSEGU   = PRD_NSEG;
              RPT_RPT00302 = %DATE();
              CHAIN (PRD_NSEG) GFSEG;
              IF (%FOUND(GFSEG));
                 RPT_MORADA  = SEG_MORAD;
                 RPT_CDPOST  = SEG_CPOST;
                 RPT_LOCALID = SEG_DESCP;
              ENDIF;

              IF (PRD_CF05 = 'S') AND (PRD_CF06 = 'S');
                 RPT_CODFORM = 'CF0506';
                 RPT_EXTDATA = 'CF05;CF06';
              ELSEIF (PRD_CF05 = 'S') AND (PRD_CF06 = ' ');
                 RPT_CODFORM = 'CF05';
                 RPT_EXTDATA = 'CF05';
              ELSEIF (PRD_CF05 = ' ') AND (PRD_CF06 = 'S');
                 RPT_CODFORM = 'CF06';
                 RPT_EXTDATA = 'CF06';
              ENDIF;

              IF (NOT WIcExiste);
                 RPT_RPT00301 = %DATE();
                 WRITE RRPTP003;
              ELSE;
                 UPDATE RRPTP003 %FIELDS(RPT_RPT00302:RPT_NUACTA:RPT_NOME:
                                      RPT_CODFORM:RPT_EXTDATA:RPT_MORADA:
                                      RPT_NUSEGU:RPT_CDPOST:RPT_LOCALID);
              ENDIF;
           ENDIF;
        ENDSR;

        //=================================================================
        // Subrotina..: CrtRecDIO
        // Objectivo..: Escrever o texto das Cartas
        //=================================================================
        BEGSR CrtRecDIO;
          //-------------------------------------------------
          // Validar Existência do Registo
          //-------------------------------------------------



           SETLL (WCdReport:WCdPeriodo) RRPTP003;
           READE (WCdReport:WCdPeriodo) RRPTP003;
           DOW (NOT %EOF(FRPTP003));
              WNuApol = RPT_NUAPOL;
              CHAIN (5:WNuApol) GBASEDIO;
              IF (%FOUND(GBASEDIO));
                 WDataAltera = 1000000 + PRD_DTINI;
                 WDataAltera = %DEC(%SUBST(%CHAR(WDataAltera):2:4):4:0)*100+
                               (WAnoCont - 2000);
                 PRD_ACTA += 1;
                 PRD_DALT = WDataAltera;
                 PRD_MALT = 46;
             //  UPDATE BASEDIFO %FIELDS(PRD_ACTA:PRD_DALT:PRD_MALT);

                 RPT_RPT00302 = %DATE();
                 RPT_NUACTA   = PRD_ACTA;
                 UPDATE RRPTP003 %FIELDS(RPT_NUACTA:RPT_RPT00302);
                 EXSR CpyRecord;
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
           // Só cria o registo se não estiver em modo de Simulação
           //--------------------------------------------------------
           MONITOR;
           WStdCmd = 'CPYF FROMFILE('+%TRIM(WLibProd)+'/GBASEGRO) +
                      TOFILE('+%TRIM(WLibProd)+'/GBASEDIO) +
                      MBROPT(*ADD) INCREL((*IF RAMO *EQ 5) +
                      (*AND APOL *EQ ' + %CHAR(WNuApol) + '))';
           WStdLen = %LEN(WStdCmd);
        // shell(WStdCmd:WStdLen);
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

     P*=======================================================
     P* Processo..: CLTOSEL
     P* Descrição.: Clausula para Seleccionar
     C*=======================================================
     P CLTOSEL         B                   EXPORT
     D                 PI                  LIKEDS(REC#AT)
     D  Registo                            LIKEDS(REC#AT) Const
     D  Grupo                         1A   Const
     D
     D*==> Campos de Trabalho
     D  WResultado     DS                  LIKEDS(REC#AT)
     D*===========================================================
      /FREE
        // WResultado = *OFF;
        // IF (Grupo = 'A') AND ((Clausula >= 6 AND CLausula <= 7) OR
        //                     (Clausula >= 9 AND Clausula <= 21));
        // ELSEIF (Grupo = 'B') AND (Clausula <> 23);
        // ENDIF;

        RETURN   WResultado;
      /END-FREE
     P CLTOSEL         E
