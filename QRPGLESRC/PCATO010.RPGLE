     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     **====================================================================
     ** Programa..: PCATO010
     ** Objectivo.: Aplicar em automático a operação de aumento de Capital
     **             nas apólice Indexadas.
     **
     ** Importante: Em Novembro de 2012, desactivar a limpeza automática dos
     **             campos de Observações.
     **
     ** Observação: Este programa está preparado para correr em dois modos
     **             Batch, com a colocação de Parâmetros automática e
     **             On-line com a colocação de parâmetros pelo utilizador.
     **             Se não receber nada então formata os parâmetros de
     **             forma Automática. Por defeito corre em modo Interactivo (I)
     **
     **             Se por algum motivo for necessário correr os Continuados
     **             antes do dia 20, então deverá desactivar a segurança,
     **             na Rotina ValParms;
     **
     **             Este programa foi alterado de forma a passar a considerar
     **             o que estiver no ficheiro de apólices gerado no modo de
     **             simulação ou então gerado no modo manual. Completar a
     **             leitura do ficheiro FPRDP122, quando o CDSTATUS <> 'E'
     **             Passa desta forma a gerar o histórico da Apólice, e
     **             actualiza directamente o base de Apólice Outros Ramos
     **             deixa desta forma de existir Carregamento.
     **====================================================================
     H
     H*====================================================================
     FFCATE010  CF   E             WORKSTN PREFIX(ECR_) USROPN
     F                                     INFDS(INFD#KEYS)
     FFALTER    UF A E           K DISK    PREFIX(CTL_)
     F                                     EXTFILE('QS36F/FALTER')
     FGBASEGRO  IF   E           K DISK    PREFIX(I_) RENAME(BASEGRFO:ORIGEM)
     FGBASEDIO  UF   E           K DISK    PREFIX(O_) RENAME(BASEDIFO:DESTIN)
     F                                     EXTFILE('QS36F/GBASEDIO')
     FFPRDP120  UF A E           K DISK
     FFPRDP12001IF   E           K DISK    RENAME(RPRDP120:RPRDP12001)
     FFPRDP122  UF A E           K DISK
     FFPRDP12202IF   E           K DISK    RENAME(RPRDP122:RPRDP12202)
     FFPRDP123  UF A E           K DISK
     FFPRDP124  UF A E           K DISK
     FFPRDP12401IF   E           K DISK    RENAME(RPRDP124:RPRDP12401)
     FFCATP010  IF   E           K DISK
     FFCATP01001IF   E           K DISK    RENAME(RCATP010:RCATP01001)
     FQSYSPRT   O    F  212        PRINTER OFLIND(PrintOf) PRTCTL(PrintDS)
     F                                     USROPN
     F*====================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SSECO010


     D*==> Invocar API de comandos
     D shell           PR                  EXTPGM('QCMDEXC')
     D   StdCmd                     200A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5
     D WStdCmd         S            200A
     D WStdLen         S             15P 5
     D
     D*==>
     D PrintDS         DS
     D   $Fill                 1     12
     D   $CurLin              13     15  0
     D
     D*==> Restantes campos de trabalho
     D WCMD            S            200A
     D WSelect         S               N   INZ(*OFF)
     D WAnoVenc        S              4S 0
     D WMesVencParm    S              2S 0
     D WAnoVencParm    S              4S 0
     D WModoExecParm   S              1A
     D WModoExec       S              1A   INZ('I')
     D WSimulacao      S              1A   INZ('N')
     D WMesVenc        S              2S 0
     D WAnoInic        S              4S 0
     D WMesInic        S              2S 0
     D WDiaInic        S              2S 0
     D WAnoCont        S              4S 0
     D WMesCont        S              2S 0
     D WDatHoje        S               D
     D WDiaHoje        S              2S 0
     D WMesHoje        S              2S 0
     D WAnoHoje        S              4S 0
     D WTrimVenc       S              2S 0
     D WValTrimVenc    S              9S 2
     D WAnoBase        S              4S 0
     D WMesBase        S              2S 0
     D WTrimBase       S              2S 0
     D WValTrimBase    S              9S 2
     D WIncidCode      S              1A
     D WIncidDesc      S             10A
     D WTaxa           S              5S 0
     D WTaxaReal       S              4S 3
     D WCodIndBase     S              3S 0
     D WCodIndVenc     S              3S 0
     D WDatAltera      S              6S 0
     D WLibProd        S             10A   INZ('QS36F')                         ### QS36F ###
     D WDatTermo       S              8S 0
     D WDatInicio      S              8S 0
     D WCapRisc01      S             11S 0
     D WCapRisc02      S             11S 0
     D WCapRisc03      S             11S 0
     D WCodHist        S              2S 0
     D WLastDatProc    S              8S 0
     D WNomFunction    S             10A
     D WCodIdx         S              5S 0
     D WNuSequ         S              5S 0
     D PrintOf         S               N
     D WIcErro         S               N   INZ(*ON)
     D WMsgErro        S              7A
     D
     D*-- Validar se existem alterações de Capital durante a vigência --
     D ChkHistorico    PR                  ExtPgm( 'RCATO011' )
     D  CodRamo                       2S 0 Const
     D  NumApol                       7S 0 Const
     D  DatTermo                      8S 0 Const
     D  CapRisc01                    11S 0
     D  CapRisc02                    11S 0
     D  CapRisc03                    11S 0
     D  Codigo                        2S 0 Const
     D  LastDatProc                   8S 0 Const
     D
     D*====================================================================
     C*==> Mês do processamento
     C     *ENTRY        PLIST
     C                   PARM                    WModoExecParm
     C                   PARM                    WAnoVencParm
     C                   PARM                    WMesVencParm
     C
      /FREE
        //------------------------------------
        // Se não estiver autorizado então sai
        //------------------------------------
        IF NOT $ValAccess(S#PGM:'':99999);
           *INLR = *ON;
           RETURN;
        ENDIF;

        //------------------------------------
        // Validar se o programa não está em
        // execução.
        //------------------------------------
        CHAIN 'E' RPRDP12401;
        IF (%FOUND(FPRDP12401));
           $ShowDialog('PRD0221');
           *INLR = *ON;
           RETURN;
        ENDIF;

        //------------------------------------
        //
        //
        //------------------------------------
        WNuSequ = *ZEROS;

        MONITOR;
           WModoExec = WModoExecParm;
           MONITOR;
              WAnoVenc = WAnoVencParm;
              WMesVenc = WMesVencparm;
           ON-ERROR;
              WAnoVenc = *ZEROS;
              WMesVenc = *ZEROS;
           ENDMON;
        ON-ERROR;
           WModoExec = 'I';
           WAnoVenc = *ZEROS;
           WMesVenc = *ZEROS;
        ENDMON;

        //------------------------------------------------------------------
        // Apresentar os Parâmetros da Função se estes não forem passados
        //------------------------------------------------------------------
        EXSR ShowParms;
        IF (*IN03 OR *IN12);
           WIcErro = *ON;
        ENDIF;

        //----------------------------------------------
        // Validar se os Parametros estão Correctos
        //----------------------------------------------
        IF (NOT WIcErro) AND (WSimulacao = 'N') AND (WModoExec <> 'B');
           EXSR ValParms;
        ENDIF;

        //----------------------------------------------
        // Procurar Apólices que obedecem ao Critério
        //----------------------------------------------
        IF (NOT WIcErro);
           MONITOR;
              WStdCmd = 'OVRPRTF FILE(QSYSPRT) TOFILE(QSYS/QSYSPRT) +
                         PAGESIZE(*N 212)';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);
              OPEN QSYSPRT;
           ON-ERROR;
           ENDMON;
           EXCEPT  PRT_HEADER;

           //----------> Desactivar <-----------//
           // *INLR = *ON;
           // RETURN;
           //----------> Desactivar <-----------//

           READ ORIGEM;
           DOW NOT %EOF;
              IF (I_DTINI > 100000);
                 WMesInic = %DEC(%SUBST(%CHAR(I_DTINI):3:2):2:0);
              ELSEIF (I_DTINI > *ZEROS);
                 WMesInic = %DEC(%SUBST(%CHAR(I_DTINI):2:2):2:0);
              ELSE;
                 WMesInic = 99;
              ENDIF;
              IF (I_RAMO=5) AND (I_CE003='S') AND ((I_SIT=1) OR (I_SIT=2)
                 OR (I_SIT=4)) AND ((I_CR1=1) OR (I_CR1=2) OR (I_CR2=1)
                 OR (I_CR2=2) OR (I_CR3 = 1) OR (I_CR3 = 2)) AND
                  (WMesInic=(%INT(I_MACONT / 100))) AND (WmesInic=WMesVenc);
                 EXSR Indexar;
              ENDIF;
              READ ORIGEM;
           ENDDO;
        ENDIF;

        //------------------------------------------------------------
        // Actualizar controlo se não estiver em modo de Simulação
        //------------------------------------------------------------
        IF (NOT WIcErro AND WSimulacao = 'N') AND (WModoExec <> 'B');
           CHAIN (WCodIdx) RPRDP120;
           ICMODO = 'R'; // Modo de execução real
           DATEXEC = %DATE();
           UPDATE RPRDP120 %FIELDS(ICMODO:DATEXEC);
        ENDIF;

        //---------------------------------------------------------
        // Libertar o programa
        //---------------------------------------------------------
        CHAIN 'E' RPRDP12401;
        IF (%FOUND(FPRDP12401));
           CHAIN (CODIDX:NUSEQU) RPRDP124;
           CDSTATUS = 'C'; // Fechada
           UPDATE RPRDP124 %FIELDS(CDSTATUS);
        ENDIF;

        *INLR = *ON;
        RETURN;
        //=================================================================
        // Subrotina..: ShowParms
        // Objectivo..: Validar se os Parametros Introduzidos estão
        //              Correctos e são válidos.
        //=================================================================
        BEGSR ShowParms;
           WIcErro = *OFF;

           //-----------------------------------------------
           // Validar se os Parâmetros não estão preenchidos
           // Formatar o Ano e Mês dos Próximos Continuados
           //-----------------------------------------------
           IF (WAnoVenc = *ZEROS) OR (WMesVenc = *ZEROS);
              EXSR GetLastProc;
              WMesVenc = (WMesCont + 1);
              WAnoVenc = WAnoCont;
              IF (WMesVenc > 12);
                 WMesVenc = WMesVenc - 12;
                 WAnoVenc = WAnoVenc + 1;
              ENDIF;
           ENDIF;

           //-------------------------------------------------------------
           // Marcar programa para não ser executado novamente
           //-------------------------------------------------------------
           CHAIN 'E' RPRDP12401;
           IF (NOT %FOUND(FPRDP12401));
              CODIDX    = WCodIdx;
              NUSEQU    = $GetNextKey('FPRDP124':
                                      'NUSEQU':'CODIDX=' + %CHAR(WCodIdx));
              CDSTATUS  = 'E';
              IF (WModoExec = 'B');
                 DSOBSERVA = 'Execução em Batch';
              ELSE;
                 DSOBSERVA = 'Execução em modo Iterativo';
              ENDIF;
              CDUSER    = S#USR;
              DATEXEC   = %DATE();
              WRITE RPRDP124;
           ELSE;
              *INLR = *ON;
              RETURN;
           ENDIF;

           IF (WModoExec = 'I');
              OPEN FCATE010;
              WIcErro = *ON;
              ECR_ERR#MSG = *BLANKS;

              *IN45 = *OFF;
              WNomFunction = 'RCATE01005';
              DOW ((WIcErro = *ON) AND NOT *IN03 AND NOT *IN12);
                 DS#HEADER    = $getHeader(S#PGM:WNomFunction);
                 DS#FOOTER    = $getFooter(S#PGM:WNomFunction:*BLANKS);
                 ECR_S#PGM    = S#PGM;
                 WRITE     RCATE01000;
                 WRITE     RCATE01099;
                 ECR_MESCONT = WMesVenc;
                 ECR_ANOCONT = WAnoVenc;
                 EXFMT RCATE01005;
                 IF ($ValidKey(Func#Key:KEY#F21));
                   WNomFunction = 'RCATE01021';
                   *IN45 = *ON;
                 ELSEIF (Func#Key = KEY#F12) AND *IN45;
                   WNomFunction = 'RCATE01005';
                    *IN45 = *OFF;
                    *IN12 = *OFF;
                 ELSEIF (Func#Key = KEY#Enter);
                    ECR_ERR#MSG = *BLANKS;
                    WMesVenc    = ECR_MESCONT;
                    WAnoVenc    = ECR_ANOCONT;
                    WSimulacao  = ECR_MODO;

                    IF (WSimulacao = 'N') AND (WModoExec <> 'B');
                       EXSR ValParms;
                       IF (NOT WIcErro);
                          LEAVE;
                       ENDIF;
                    ELSE;
                       WIcErro = *OFF;
                       LEAVE;
                    ENDIF;
                 ENDIF;
              ENDDO;
              CLOSE FCATE010;
           ENDIF;
        ENDSR;

        //=================================================================
        // Subrotina..: GetLastProc
        // Objectivo..: Obter o último ano e mês Processados.
        //=================================================================
        BEGSR GetLastProc;
           SETLL *LOVAL RPRDP120;
           READ RPRDP120;
           DOW NOT %EOF;
              IF ((ANOIDX * 100) + MESIDX) > ((WAnoCont * 100) + WMesCont)
                AND (ICMODO = 'R');
                 WAnoCont = ANOIDX;
                 WMesCont = MESIDX;
              ENDIF;
              READ RPRDP120;
           ENDDO;

           WMesVenc = (WMesCont + 1);
           WAnoVenc = WAnoCont;
           IF (WMesVenc > 12);
              WMesVenc = WMesVenc - 12;
              WAnoVenc = WAnoVenc + 1;
           ENDIF;
           CHAIN (WAnoVenc:WMesVenc) RPRDP12001;
           IF (NOT %FOUND(FPRDP12001));
              SETLL *HIVAL RPRDP120;
              READP RPRDP120;
              CODIDX = CODIDX + 1;
              ANOIDX = WAnoVenc;
              MESIDX = WMesVenc;
              ICMODO = 'S'; // Modo de Simulação
              DATEXEC = %DATE();
              WRITE RPRDP120;
           ELSE;
           // WSimulacao = ICMODO;
           ENDIF;
           WCodIdx = CODIDX;
        ENDSR;

        //=================================================================
        // Subrotina..: ValParms
        // Objectivo..: Validar se os Parametros Introduzidos estão
        //              Correctos e são válidos.
        //=================================================================
        BEGSR ValParms;
           //-----------------------------------------
           // Obter o último Ano e Mês Processado
           //-----------------------------------------
           WIcErro = *OFF;
           WAnoCont = *ZEROS;
           WMesCont = *ZEROS;
           EXSR GetLastProc;

           //-----------------------------------------
           // Validar se o Ano/Mês já foi processado
           //-----------------------------------------
           IF (WAnoCont = WAnoVenc) AND (WMesCont = WMesVenc);
              WIcErro = *ON;
              ECR_ERR#MSG = 'Este mês já foi processado para os Continuados';
           ENDIF;

           //-----------------------------------------
           // Validar se o Ano/Mês pedido é a seguir ao anterior
           //-----------------------------------------
           IF (NOT WIcErro);
              IF ((WMesVenc <>(WMesCont + 1)) OR (WAnoVenc <>WAnoCont)) AND
                 ((WMesVenc <>1) OR (WAnoVenc <>(WAnoCont + 1)));
                 WIcErro = *ON;
                 ECR_ERR#MSG = 'Não é possível saltar o mês dos Continuados';
              ENDIF;
           ENDIF;

           //-----------------------------------------
           // Validar se está numa data provável de Continuados
           //-----------------------------------------
           IF (NOT WIcErro);
              IF (%SUBDT(%DATE():*DAYS) < 18);
                 WIcErro = *ON;
                 ECR_ERR#MSG = 'Só é possivel executar depois do dia 17';
              ENDIF;
           ENDIF;

           //-----------------------------------------
           // Validar se o Mês é Provável para os Continuados
           //-----------------------------------------
           IF (NOT WIcErro);
              WAnoCont = %SUBDT(%DATE():*Years);
              WMesCont = %SUBDT(%Date():*Months);
              WMesCont = WMesCont + 2;
              IF (WMesCont > 12);
                 WMesCont = WMesCont - 12;
                 WAnoCont = WAnoCont + 1;
              ENDIF;
              IF (WAnoCont <> WAnoVenc) OR (WMesCont <> WMesVenc);
                 WIcErro = *ON;
                 ECR_ERR#MSG = 'Não está no ano/mês destes continuados';
              ENDIF;
           ENDIF;
        ENDSR;

        //=================================================================
        // Subrotina..: Indexar
        // Objectivo..: Proceder à actualização dos ficheiros de acordo
        //              com o valor de Indexação da Apólice.
        //=================================================================
        BEGSR Indexar;
           //-------------------------------------------------
           // Calcular datas de Inicio e termo da Apólice
           //-------------------------------------------------
           IF (I_DTINI > 100000);
              WAnoInic = WAnoVenc;
              WMesInic = %DEC(%SUBST(%CHAR(I_DTINI):3:2):2:0);
              WDiaInic = %DEC(%SUBST(%CHAR(I_DTINI):1:2):2:0);
           ELSE;
              WAnoInic = WAnoVenc;
              WMesInic = %DEC(%SUBST(%CHAR(I_DTINI):2:2):2:0);
              WDiaInic = %DEC(%SUBST(%CHAR(I_DTINI):1:1):1:0);
           ENDIF;
           WDatInicio = WAnoInic * 10000 + WMesInic * 100 + WDiaInic;
           TEST(DE) *ISO WDatInicio;
           IF (%ERROR());
              WDiaInic -= 1;
              WDatInicio = WAnoInic * 10000 + WMesInic * 100 + WDiaInic;
           ENDIF;
           WDatTermo  = WDatInicio;
           WDatHoje   = %DATE(WDatTermo:*ISO);
           WDatHoje  += %years(1);
           WDatHoje  -= %days(1);
           WDatTermo  = (%SubDt(WDatHoje:*Years) * 10000) +
                        (%SubDt(WDatHoje:*Months) * 100) +
                        %SubDt(WDatHoje:*Days);

           //-----------------------------------
           // Guardar Capitais de Risco
           //-----------------------------------
           WCapRisc01 = *ZEROS;
           IF (I_CR1 = 1) OR (I_CR1 = 2) OR (I_CR1 = 3);
              WCapRisc01 = I_CAP1;
           ENDIF;
           WCapRisc02 = *ZEROS;
           IF (I_CR2 = 1) OR (I_CR2 = 2) OR (I_CR2 = 3);
              WCapRisc02 = I_CAP2;
           ENDIF;
           WCapRisc03 = *ZEROS;
           IF (I_CR3 = 1) OR (I_CR2 = 3) OR (I_CR3 = 3);
              WCapRisc03 = I_CAP3;
           ENDIF;

           //-----------------------------------
           // Procurar Variações nos Históricos
           //-----------------------------------
           WMsgErro = *BLANKS;
           ChkHistorico(I_RAMO : I_APOL : WDatInicio :
                        WCapRisc01 : WCapRisc02 : WCapRisc03 :
                        WCodHist : WLastDatProc );
           EXSR GetIndice; // Calcular Taxa de Indexação

           //----------------------------------
           // Validar se o registo já existe
           //----------------------------------
           CHAIN (I_RAMO:I_APOL) DESTIN;
           IF (NOT %FOUND);
              EXSR CrtRecord;
              CHAIN (I_RAMO:I_APOL) DESTIN;
           ENDIF;
           IF (%FOUND) OR (WSimulacao = 'S');
              EXSR ChgRecord;
           ENDIF;

           //-------------------------------------------------
           // Actualizar ficheiro de Controlo
           //-------------------------------------------------
           CHAIN (WCodIdx:I_RAMO:I_APOL) RPRDP12202;
           IF (NOT %FOUND(FPRDP12202));
              IF (WNuSequ = *ZEROS);
                 WNuSequ = $GetNextKey('FPRDP122':
                                       'NUSEQU':'CODIDX=' + %CHAR(WCodIdx));
              ELSE;
                 WNuSequ += 1;
              ENDIF;
              CODIDX   = WCodIdx;
              NUSEQU   = WNuSequ;
              CDSTATUS = 'A'; // Actualização Automática
              DTSTATUS = %DATE();
              CDRAMO   = I_RAMO;
              NUAPOL   = I_APOL;
              CODVENC  = WCodIndVenc;
              CODBASE  = WCodIndBase;
              MONITOR;
                WRITE RPRDP122;
              ON-ERROR;
              ENDMON;

              CODIDX = WCodIDX;
              NUSEQU = WNuSequ;
              OBS1   = I_OBS1;
              OBS2   = I_OBS2;
              MONITOR;
                WRITE RPRDP123;
              ON-ERROR;
              ENDMON;
           ELSE;
              CHAIN (WCodIDx:NUSEQU) RPRDP122;
              IF (CDSTATUS = 'A');
                CODVENC  = WCodIndVenc;
                CODBASE  = WCodIndBase;
              MONITOR;
                UPDATE RPRDP122 %FIELDS(CODVENC:CODBASE);
              ON-ERROR;
              ENDMON;
              ENDIF;

              CHAIN (WCodIDx:NUSEQU) RPRDP123;
              OBS1   = I_OBS1;
              OBS2   = I_OBS2;
              UPDATE RPRDP123 %FIELDS(OBS1:OBS2);
           ENDIF;
        ENDSR;

        //=================================================================
        // Subrotina..: getIndice
        // Objectivo..: Calcular o indice de Indexação a Aplicar
        //
        //=================================================================
        BEGSR getIndice;
           WDatHoje = %Date();
           WMesHoje = %SubDt(WDatHoje: *MONTHS);
           WAnoHoje = %SubDt(WDatHoje: *YEARS);
           WDiaHoje = %SubDt(WDatHoje: *DAYS);
           WDatAltera = (WDiaHoje*10000) + (WMesHoje*100) + (WAnoHoje - 2000);

           //---------------------------------------
           // Calcular o Trimestre e o Ano actual
           //---------------------------------------
           WTrimVenc = %INT((WMesVenc - 1) / 3) + 1;
           IF ((WMesVenc >= 1) AND (WMesVenc <= 3));
              WTrimVenc= 1;
           ELSEIF ((WMesVenc >= 4) AND (WMesVenc <= 6));
              WTrimVenc= 2;
           ELSEIF ((WMesVenc >= 7) AND (WMesVenc <= 9));
              WTrimVenc= 3;
           ELSE;
              WTrimVenc= 4;
           ENDIF;

           //---------------------------------------------------
           // Calcular o valor do Trimestre para o ano anterior
           //---------------------------------------------------
           IF (WLastDatProc = *ZEROS);
              WAnoBase = WAnoVenc- 1;
              WTrimBase= WTrimVenc;
           ELSE;
              WAnoBase= %INT(WLastDatProc/10000);
              WMesBase= %DEC(%SUBST(%CHAR(WLastDatProc):5:2):2:0);
              WTrimBase = %INT((WMesBase - 1) / 3) + 1;
              IF ((WMesBase>= 1) AND (WMesBase<= 3));
                 WTrimBase= 1;
              ELSEIF ((WMesBase>= 4) AND (WMesBase<= 6));
                 WTrimBase= 2;
              ELSEIF ((WMesBase>= 7) AND (WMesBase<= 9));
                 WTrimBase= 3;
              ELSE;
                 WTrimBase= 4;
              ENDIF;
           ENDIF;


           //--------------------------------
           // Calcular o valor da Incidencia
           //--------------------------------
           WIncidCode  = *BLANKS;
           WIncidDesc  = *BLANKS;
           IF (I_CR1 = 1) OR (I_CR2 = 1) OR (I_CR3 = 1);
               WIncidCode = 'E';
               WIncidDesc = 'Edificio';
           ENDIF;
           IF (I_CR1 = 2) OR (I_CR2 = 2) OR (I_CR3 = 2);
              IF (WIncidCode = *BLANKS);
                  WIncidCode = 'R';
                  WIncidDesc = 'Recheio';
               ELSE;
                  WIncidCode = 'T';
                  WIncidDesc = 'Tudo';
               ENDIF;
           ENDIF;

           //---------------------------------------
           // Obter o valor para o Indice
           //---------------------------------------
           WValTrimVenc= *ZEROS;
           WValTrimBase= *ZEROS;
           WTaxa = *ZEROS;
           CHAIN (WAnoBase:WTrimBase:WIncidCode) RCATP01001;
           IF (NOT %FOUND(FCATP01001));
              WMsgErro = 'CAT0001';
              $ShowDialog(WMsgErro:%CHAR(WAnoBase)+'/'+%CHAR(WTrimBase)+
                                    '(' + WIncidCode+')');
              RETURN;
           ENDIF;
           WValTrimBase= VALOR;
           WCodIndBase = CodIndice;

           CHAIN (WAnoVenc:WTrimVenc:WIncidCode) RCATP01001;
           IF (NOT %FOUND(FCATP01001));
              WMsgErro = 'CAT0002';
              $ShowDialog(WMsgErro:%CHAR(WAnoVenc)+'/'+%CHAR(WTrimVenc)+
                                    '(' + WIncidCode+')');
              RETURN;
           ENDIF;
           WValTrimVenc= VALOR;
           WCodIndVenc = CodIndice;

           //---------------------------------------
           // Realizar o calculo da Taxa
           //---------------------------------------
           IF (WValTrimVenc > *ZEROS) AND (WValTrimBase > *ZEROS);
              WTaxaReal = (WValTrimVenc / WValTrimBase);
              IF (WTaxaReal >= 1);
                 WTaxa = %INTH(((WValTrimVenc / WValTrimBase)-1) * 1000);
              ELSE;
                 WTaxa = %INTH(((WValTrimVenc / WValTrimBase)) * 1000);
              ENDIF;
              //WTaxaReal = WTaxa;
           ENDIF;
        ENDSR;

        //=================================================================
        // Subrotina..: ChgRecord
        // Objectivo..: Alterar o registo existente
        //
        //=================================================================
        BEGSR ChgRecord;
           O_TALT = 2;           // Tipo de alteração
           O_DALT = WDatAltera;  // Formatar a data do dia
           O_TXIND = (WTAXA/10); // Aplicar a Taxa
           O_INDEX = 'S';
           // O_OBS1  = *BLANKS;
           // O_OBS2  = *BLANKS;
           O_FILO3 = %EDITC(WCodIndBase:'X') + %EDITC(WCodIndVenc:'X');
           //----------------------------------------------------------
           // Só altera o registo se não estiver em modo de Simulação
           //----------------------------------------------------------
           IF (WSimulacao = 'N') AND (WModoExec <> 'B');
              UPDATE  DESTIN %FIELDS(O_TALT:O_DALT:O_TXIND:
                                     O_FILO3:O_INDEX); //##
                               //    O_OBS1:O_OBS2);
              //  EXSR crtRegAlt;  // #### Descomentar ####
           ENDIF;
           // O_TXIND = O_TXIND * 10;
           EXCEPT  PRT_DETAIL;
           IF (PrintOf);
              EXCEPT PRT_HEADER;
              PrintOf = *OFF;
           ENDIF;
        ENDSR;

        //=================================================================
        // Subrotina..: crtRegAlt
        // Objectivo..: Criar registo no ficheiro controlo de Alterações.
        //
        //=================================================================
        BEGSR crtRegAlt;
           CTL_RAMO = I_RAMO;
           CTL_APOL = I_APOL;
           CTL_WSIT = I_SIT;      // Situação antes
           CTL_SIT = O_SIT;       // Situação depois
           CTL_WTALT = I_TALT;    // Tipo alteração antes
           CTL_TALT = O_TALT;     // Tipo alteração depois
           CTL_WMALT = I_MALT;    // Motivo alteração antes
           CTL_MALT = O_MALT;     // Motivo alteração depois
           CTL_WDTALT = I_DALT;   // Data Alteração antes
           CTL_DTALT = O_DALT;    // Data alteração depois
           CTL_WADC = I_ADC;      // ADC antes
           CTL_ADC = O_ADC;       // ADC depois
           CTL_WNIB = I_NIB;      // NIB antes
           CTL_NIB = O_NIB;       // NIB depois
           CTL_DATALT = O_DALT;   // Data de alteração
           CTL_HORALT = 180000;   // Hora de alteração
           CTL_USER = 'AUTOM';    // Utilizador da Alteração
           WRITE FALTERF1;
        ENDSR;

        //=================================================================
        // Subrotina..: CtrRecord
        // Objectivo..: Criar um novo registo se este ainda não existir
        //              por cópia do Base.
        //=================================================================
        BEGSR CrtRecord;
           //--------------------------------------------------------
           // Só cria o registo se não estiver em modo de Simulação
           //--------------------------------------------------------
           IF (WSimulacao = 'N') AND (WModoExec <> 'B');
              MONITOR;
              WStdCmd = 'CPYF FROMFILE(QS36F/GBASEGRO) TOFILE('+%TRIM(WLibProd)+
                        '/GBASEDIO) MBROPT(*ADD) INCREL((*IF RAMO *EQ 5) ' +
                        '(*AND APOL *EQ ' + %CHAR(I_APOL) + '))';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);
              ON-ERROR;
              ENDMON;
           ENDIF;
        ENDSR;

      /END-FREE
     C*============================================================
     OQSYSPRT   E            PRT_HEADER     1  1
     O                                            9 'RCATP010'
     O                                           84 'Indexação Apólices      '
     O                                          103 'Pag.:'
     O                       PAGE          Z    114
     O          E            PRT_HEADER     2
     O                                          103 'Data:'
     O                       WDatHoje           114
     O          E            PRT_HEADER     1
     O                                            4 '----'
     O                                           12 '-------'
     O                                           39 '--------------------------'
     O                                           53 '--------------'
     O                                           63 '---------'
     O                                           73 '--------'
     O                                           84 '----------'
     O                                           93 '--------'
     O                                          104 '----------'
     O                                          114 '---------'
     O                                          159 '--------------------------'
     O                                          205 '--------------------------'
     O                                          207 '-'
     O                                          212 '----'
     O          E            PRT_HEADER     1
     O                                           73 'Ano/Tri.'
     O                                           84 'Valor'
     O                                           93 'Ano/Tri.'
     O                                          104 'Valor'
     O          E            PRT_HEADER     1
     O                                            4 'Ramo'
     O                                           12 'Apólice'
     O                                           17 'Nome'
     O                                           63 'Aplicação'
     O                                           73 '  Base  '
     O                                           84 'Base'
     O                                           93 'Vencim. '
     O                                          104 'Venc.'
     O                                          114 'Aplicacao'
     O                                          133 'Observação Linha 1'
     O                                          179 'Observação Linha 2'
     O                                          207 'I'
     O                                          212 'Taxa'
     O          E            PRT_HEADER     1
     O                                            4 '----'
     O                                           12 '-------'
     O                                           39 '--------------------------'
     O                                           53 '--------------'
     O                                           63 '---------'
     O                                           73 '--------'
     O                                           84 '----------'
     O                                           93 '--------'
     O                                          104 '----------'
     O                                          114 '---------'
     O                                          159 '--------------------------'
     O                                          205 '--------------------------'
     O                                          207 '-'
     O                                          212 '----'
     OQSYSPRT   E            PRT_DETAIL
     O                       I_RAMO               3
     O                       I_APOL              12
     O                       I_SEG               53
     O                       WIncidDesc          64
     O                       WAnoBase            70
     O                                           71 '/'
     O                       WTrimBase           73
     O                       WValTrimBase        84 '       ,  '
     O                       WAnoVenc            90
     O                                           91 '/'
     O                       WTrimVenc           93
     O                       WValTrimVenc       104 '       ,  '
     O                       WTaxa              111 '0 ,   '
     O                                          113 '%'
     O                       I_OBS1             159
     O                       I_OBS2             205
     O                       I_INDEX            207
     O                       I_TXIND            212 '0  , '
