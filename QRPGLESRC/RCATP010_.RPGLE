     **====================================================================
     ** Programa..: RCATP010
     ** Objectivo.: Aplicar em autom�tico a opera��o de aumento de Capital
     **             nas ap�lice Indexadas.
     **
     ** Observa��o: Este programa est� preparado para correr em dois modos
     **             Batch, com a coloca��o de Par�metros autom�tica e
     **             On-line com a coloca��o de par�metros pelo utilizador.
     **             Se n�o receber nada ent�o forComp os par�metros de
     **             forma Autom�tica.
     **
     ** Observa��o: Se por algum motivo for necess�rio correr os Continuados
     **             antes do dia 20, ent�o dever� desactivar a seguran�a,
     **             na Rotina ValParms;
     **====================================================================
     H
     H*====================================================================
     FFCATE010  CF   E             WORKSTN PREFIX(ECR_)
     FFALTER    UF A E           K DISK    PREFIX(CTL_)
     F                                     EXTFILE('QS36F/FALTER')
     FGBASEGRO  IF   E           K DISK    PREFIX(I_) RENAME(BASEGRFO:ORIGEM)
     FGBASEDIO  UF   E           K DISK    PREFIX(O_) RENAME(BASEDIFO:DESTIN)
     F                                     EXTFILE('QS36F/GBASEDIO')
     FFCATP020  IF A E           K DISK
     FFCATP010  IF   E           K DISK
     FFCATP01001IF   E           K DISK    RENAME(RCATP010:RCATP01001)
     FQSYSPRT   O    F  120        PRINTER OFLIND(PrintOf) PRTCTL(PrintDS)
     F*====================================================================
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
     D PrintOf         S               N
     D WIcErro         S               N   INZ(*ON)
     D
     D*-- Validar se existem altera��es de Capital durante a vig�ncia --
     D ChkHistorico    PR                  ExtPgm( 'RCATP011' )
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
     C*==> M�s do processamento
     C     *ENTRY        PLIST
     C                   PARM                    WModoExecParm
     C                   PARM                    WAnoVencParm
     C                   PARM                    WMesVencParm
     C
      /FREE
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
           WModoExec = 'X';
           WAnoVenc = *ZEROS;
           WMesVenc = *ZEROS;
        ENDMON;
        //------------------------------------------------------------------
        // Apresentar os Par�metros das Fun��o se estes n�o forem passados
        //------------------------------------------------------------------
        EXSR ShowParms;
        IF (*IN03 OR *IN12);
           WIcErro = *ON;
        ENDIF;

        //----------------------------------------------
        // Validar se os Parametros est�o Correctos
        //----------------------------------------------
        IF (NOT WIcErro) AND (WSimulacao = 'N');
           EXSR ValParms;
        ENDIF;

        //----------------------------------------------
        // Procurar Ap�lices que obedecem ao Crit�rio
        //----------------------------------------------
        IF (NOT WIcErro);
           EXCEPT  PRT_HEADER;

           //----------> Desactivar <-----------//
           // *INLR = *ON;
           // RETURN;
           //----------> Desactivar <-----------//

           READ ORIGEM;
           DOW NOT %EOF;
           IF ((I_RAMO = 5) AND (I_CE003 = 'S') AND ((I_SIT = 1) OR (I_SIT = 2)
             OR (I_SIT = 4))) AND ((I_CR1 = 1) OR (I_CR1 = 2) OR (I_CR2 = 1)
             OR (I_CR2 = 2) OR (I_CR3 = 1) OR (I_CR3 = 2)) AND
             (I_MES1 = (%INT(I_MACONT / 100))) AND (I_MES1 = WMesVenc);
             EXSR Indexar;
           ENDIF;
           READ ORIGEM;
           ENDDO;
        ENDIF;

        //------------------------------------------------------------
        // Actualizar controlo se n�o estiver em modo de Simula��o
        //------------------------------------------------------------
        IF (WSimulacao = 'N') AND (NOT WIcErro);
           SETLL *HIVAL RCATP020;
           READP RCATP020;
           CODIDX = CODIDX + 1;
           ANOIDX = WAnoVenc;
           MESIDX = WMesVenc;
           DATEXEC = %DATE();
           WRITE RCATP020;
        ENDIF;

        RETURN;
        //=================================================================
        // Subrotina..: ShowParms
        // Objectivo..: Validar se os Parametros Introduzidos est�o
        //              Correctos e s�o v�lidos.
        //=================================================================
        BEGSR ShowParms;
           WIcErro = *OFF;

           //-----------------------------------------------
           // Validar se os Par�metros n�o est�o preenchidos
           // ForCompr o Ano e M�s dos Pr�ximos Continuados
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

           IF (WModoExec = 'I');
              WIcErro = *ON;
              ECR_MSGERRO = *BLANKS;
              DOW ((WIcErro = *ON) AND NOT *IN03 AND NOT *IN12);
                 ECR_MESCONT = WMesVenc;
                 ECR_ANOCONT = WAnoVenc;
                 EXFMT RCATE010;
                 ECR_MSGERRO = *BLANKS;
                 WMesVenc = ECR_MESCONT;
                 WAnoVenc = ECR_ANOCONT;
                 WSimulacao = ECR_MODO;
                 IF (WSimulacao = 'N');
                    EXSR ValParms;
                    IF (NOT WIcErro);
                       LEAVE;
                    ENDIF;
                 ELSE;
                    WIcErro = *OFF;
                    LEAVE;
                 ENDIF;
              ENDDO;
           ENDIF;
        ENDSR;

        //=================================================================
        // Subrotina..: GetLastProc
        // Objectivo..: Obter o �ltimo ano e m�s Processados.
        //=================================================================
        BEGSR GetLastProc;
           SETLL *LOVAL RCATP020;
           READ RCATP020;
           DOW NOT %EOF;
              IF (ANOIDX > WAnoCont);
                 WAnoCont = ANOIDX;
              ENDIF;
              IF (MESIDX > WMesCont);
                 WMesCont = MESIDX;
              ENDIF;
              READ RCATP020;
           ENDDO;
        ENDSR;

        //=================================================================
        // Subrotina..: ValParms
        // Objectivo..: Validar se os Parametros Introduzidos est�o
        //              Correctos e s�o v�lidos.
        //=================================================================
        BEGSR ValParms;
           //-----------------------------------------
           // Obter o �ltimo Ano e M�s Processado
           //-----------------------------------------
           WIcErro = *OFF;
           WAnoCont = *ZEROS;
           WMesCont = *ZEROS;
           EXSR GetLastProc;

           //-----------------------------------------
           // Validar se o Ano/M�s j� foi processado
           //-----------------------------------------
           IF (WAnoCont = WAnoVenc) AND (WMesCont = WMesVenc);
              WIcErro = *ON;
              ECR_MSGERRO = 'Este m�s j� foi processado para os Continuados';
           ENDIF;

           //-----------------------------------------
           // Validar se o Ano/M�s pedido � a seguir ao anterior
           //-----------------------------------------
           IF (NOT WIcErro);
              IF ((WMesVenc <>(WMesCont + 1)) OR (WAnoVenc <>WAnoCont)) AND
                 ((WMesVenc <>1) OR (WAnoVenc <>(WAnoCont + 1)));
                 WIcErro = *ON;
                 ECR_MSGERRO = 'N�o � poss�vel saltar o m�s dos Continuados';
              ENDIF;
           ENDIF;

           //-----------------------------------------
           // Validar se est� numa data prov�vel de Continuados
           //-----------------------------------------
           IF (NOT WIcErro);
              IF (%SUBDT(%DATE():*DAYS) < 20);
                 WIcErro = *ON;
                 ECR_MSGERRO = 'S� � possivel executar depois do dia 20';
              ENDIF;
           ENDIF;

           //-----------------------------------------
           // Validar se o M�s � Prov�vel para os Continuados
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
                 ECR_MSGERRO = 'N�o est� no ano/m�s destes continuados';
              ENDIF;
           ENDIF;
        ENDSR;

        //=================================================================
        // Subrotina..: Indexar
        // Objectivo..: Proceder � actualiza��o dos ficheiros de acordo
        //              com o valor de Indexa��o da Ap�lice.
        //=================================================================
        BEGSR Indexar;
           //-------------------------------------------------
           // Calcular datas de Inicio e termo da Ap�lice
           //-------------------------------------------------
           IF (I_DTINI > 100000);
              WDatInicio = %DEC((%CHAR(WAnoVenc) +
                           %SUBST(%CHAR(I_DTINI):3:2) +
                           %SUBST(%CHAR(I_DTINI):1:2)):8:0);
           ELSE;
              WDatInicio = %DEC((%CHAR(WAnoVenc) +
                           %SUBST(%CHAR(I_DTINI):2:2) +
                           '0' + %SUBST(%CHAR(I_DTINI):1:1)):8:0);
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
           // Procurar Varia��es nos Hist�ricos
           //-----------------------------------
           ChkHistorico(I_RAMO : I_APOL : WDatInicio :
                        WCapRisc01 : WCapRisc02 : WCapRisc03 :
                        WCodHist : WLastDatProc );
           EXSR GetIndice; // Calcular Taxa de Indexa��o

           //----------------------------------
           // Validar se o registo j� existe
           //----------------------------------
           CHAIN (I_RAMO:I_APOL) DESTIN;
           IF (NOT %FOUND);
              EXSR CrtRecord;
              CHAIN (I_RAMO:I_APOL) DESTIN;
           ENDIF;
           IF (%FOUND) OR (WSimulacao = 'S');
              EXSR ChgRecord;
           ENDIF;
        ENDSR;

        //=================================================================
        // Subrotina..: getIndice
        // Objectivo..: Calcular o indice de Indexa��o a Aplicar
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
           WValTrimBase= VALOR;
           WCodIndBase = CodIndice;
           CHAIN (WAnoVenc:WTrimVenc:WIncidCode) RCATP01001;
           WValTrimVenc= VALOR;
           WCodIndVenc = CodIndice;

           //---------------------------------------
           // Realizar o calculo da Taxa
           //---------------------------------------
           IF (WValTrimVenc > *ZEROS) AND (WValTrimBase > *ZEROS);
              WTaxa = %INTH(((WValTrimVenc / WValTrimBase)-1) * 1000);
           ENDIF;
        ENDSR;

        //=================================================================
        // Subrotina..: ChgRecord
        // Objectivo..: Alterar o registo existente
        //
        //=================================================================
        BEGSR ChgRecord;
           O_TALT = 2;             // Tipo de altera��o
           O_DALT = WDatAltera;   // ForCompr a data do dia
           O_TXIND = (WTAXA/10); // Aplicar a Taxa
           O_INDEX = 'S';
           O_FILO3 = %EDITC(WCodIndBase:'X') + %EDITC(WCodIndVenc:'X');
           //----------------------------------------------------------
           // S� altera o registo se n�o estiver em modo de Simula��o
           //----------------------------------------------------------
           IF (WSimulacao = 'N');
              UPDATE  DESTIN %FIELDS(O_TALT:O_DALT:O_TXIND:
                                     O_FILO3:O_INDEX); //##
              //  EXSR crtRegAlt;  // #### Descomentar ####
           ENDIF;
           O_TXIND = O_TXIND * 10;
           EXCEPT  PRT_DETAIL;
           IF (PrintOf);
              EXCEPT PRT_HEADER;
              PrintOf = *OFF;
           ENDIF;
        ENDSR;

        //=================================================================
        // Subrotina..: crtRegAlt
        // Objectivo..: Criar registo no ficheiro controlo de Altera��es.
        //
        //=================================================================
        BEGSR crtRegAlt;
           CTL_RAMO = I_RAMO;
           CTL_APOL = I_APOL;
           CTL_WSIT = I_SIT;      // Situa��o antes
           CTL_SIT = O_SIT;       // Situa��o depois
           CTL_WTALT = I_TALT;    // Tipo altera��o antes
           CTL_TALT = O_TALT;     // Tipo altera��o depois
           CTL_WMALT = I_MALT;    // Motivo altera��o antes
           CTL_MALT = O_MALT;     // Motivo altera��o depois
           CTL_WDTALT = I_DALT;   // Data Altera��o antes
           CTL_DTALT = O_DALT;    // Data altera��o depois
           CTL_WADC = I_ADC;      // ADC antes
           CTL_ADC = O_ADC;       // ADC depois
           CTL_WNIB = I_NIB;      // NIB antes
           CTL_NIB = O_NIB;       // NIB depois
           CTL_DATALT = O_DALT;   // Data de altera��o
           CTL_HORALT = 180000;   // Hora de altera��o
           CTL_USER = 'AUTOM';    // Utilizador da Altera��o
           WRITE FALTERF1;
        ENDSR;

        //=================================================================
        // Subrotina..: CtrRecord
        // Objectivo..: Criar um novo registo se este ainda n�o existir
        //              por c�pia do Base.
        //=================================================================
        BEGSR CrtRecord;
           //--------------------------------------------------------
           // S� cria o registo se n�o estiver em modo de Simula��o
           //--------------------------------------------------------
           IF (WSimulacao = 'N');
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
     O                                           84 'Indexa��o Ap�lices      '
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
     O          E            PRT_HEADER     1
     O                                           73 'Ano/Tri.'
     O                                           84 'Valor'
     O                                           93 'Ano/Tri.'
     O                                          104 'Valor'
     O          E            PRT_HEADER     1
     O                                            4 'Ramo'
     O                                           12 'Ap�lice'
     O                                           17 'Nome'
     O                                           63 'Aplica��o'
     O                                           73 '  Base  '
     O                                           84 'Base'
     O                                           93 'Vencim. '
     O                                          104 'Venc.'
     O                                          114 'Aplicacao'
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
     O                       O_TXIND            111 '0,    '
     O                                          113 '%'
