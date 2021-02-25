     H*====================================================================
     H* Programa..: SEDE26
     H* Descrição.: Obter os totais do mês para o Ramo Automóvel
     H* Observação: Código Convertido de S36 para RPG ILE, com uma
     H*             redução de 90% do código Original.
     H*
     H* Indices...: 1="Prem.Simples"; 2="Prem.Total"; 3="Prem.RC";
     H*             4="Prem.DP"; 5="Custo Apólice";6="Custo RC";
     H*             7="Prem.Assist.Viagem"; 8="Prem.Ress.Assist.Viag.";
     H*             9="Prem.Sede Assist.Viag."; 10="Prem.Prot.Jurid.";
     H*             11="Prem.Ress.Prot.Jurid."; 12="Prem.Sede Prot.Jurid.";
     H*             13="Prem.Ocupantes";
     H*
     H* Pro.Passos: Avaliar possibilidade de alterar os ficheiros de
     H*             INPUT, no seguimento da centralização de Informação.
     H*====================================================================
     H DECEDIT(',') DFTNAME(SEDE26) DATEDIT(*DMY.)
     H********************************************************************
     FNMPM1     IF   F  550        DISK    USROPN
     FGBASEGRA  IF   E           K DISK    PREFIX(APO_)
     FSEDE26M   O    E             PRINTER PREFIX(PRT_)
     D********************************************************************
     D*==> Tabelas Internas com a descrição de Códigos
     D CodMes          S              2  0 DIM(12) CTDATA PERRCD(1)
     D DesMes          S              9    DIM(12) ALT(CodMes)
     D CodRubrica      S              2  0 DIM(13) CTDATA PERRCD(1)
     D DesRubrica      S             15    DIM(13) ALT(CodRubrica)
     D CodCategoria    S              2  0 DIM(8) CTDATA PERRCD(1)
     D DesCategoria    S             15    DIM(8) ALT(CodCategoria)
     D
     D*==> Necessário para o Override ao Ficheiro
     D   DATAPARM      DS
     D      ANOMES             1      6S 0
     D      ANOMOV             1      4S 0
     D      MESMOV             5      6S 0
     D
     D shell           PR                  EXTPGM('QCMDEXC')
     D   StdCmd                     200A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5
     D WStdCmd         S            200A
     D WStdLen         S             15P 5
     D WNomFile        S             20A
     D
     D*==> Lista Interna com os valores
     D WTotCat         DS                  QUALIFIED DIM(48)
     D   Rub                         11S 2 DIM(13)
     D WI              S              5S 0 INZ(*ZEROS)
     D WIdx            S              5S 0 INZ(*ZEROS)
     D WIdxCol         S              5S 0
     D WIdxLin         S              5S 0
     D WQtdeVeicul     S              2S 0 INZ(*ZEROS)
     D WFactor         S              5S 0 INZ(*ZEROS)
     D WCodCat         S              2S 0 INZ(*ZEROS)
     D WCodCatBk       S              2S 0 INZ(*ZEROS)
     D WApolBK         S                   LIKE(APO_APOL)
     D WTRegBk         S              1S 0 INZ(*ZEROS)
     D WSomaLinha      C                   31
     D WSomaColuna     C                   32
     D WSomaPartic     C                   33
     D WSomaComerc     C                   34
     D WMaxCol         C                    6
     D WMaxLin         C                   13
     D
     D WPremTot        S              9S 2 INZ(*ZEROS)
     D WPrsRes         S              9S 2 INZ(*ZEROS)
     D WDifPJu         S              9S 2 INZ(*ZEROS)
     D WDifPrem        S              9S 2 INZ(*ZEROS)
     D WPrsPJu         S              9S 2 INZ(*ZEROS)
     D WIcErro         S              2S 0 INZ(*ZEROS)
     D WAnoMes         S              6S 0
     D PAnoMes         S              6A
     D WIcDP           S               N
     I********************************************************************
     INMPM1     AA  06
     I                                  1    9 0NREC
     I                                 10   18 0RRAPOL
     I                                 10   11 0RR
     I                                 12   18 0APOL
     I                                185  185 0TREG
     I                                192  192 0FRAC
     I                             P  199  204 0CAP
     I                             P  205  207 0CAPRC
     I                             P  208  212 2PRS
     I                             P  209  212 0YPRS                            asilva
     I                                208  212  WPRS
     I                             P  213  216 2BONUS
     I                             P  217  220 2ENC
     I                             P  221  222 2CUSTAA
     I                                221  222  WCUSTA
     I                             P  223  224 2CRC
     I                                223  224  WCRC
     I                                225  225  PJUR
     I                                226  226 0CR1
     I                             P  227  230 2SELOS
     I                             P  231  234 2SNA
     I                             P  235  238 2SNB
     I                             P  239  243 2PRTOT
     I                             P  246  247 0CONC
     I                                248  250 0MED1
     I                             P  251  255 2COM1
     I                             P  256  258 0MED2
     I                             P  259  263 2COM2
     I                             P  360  364 2PR1
     I                                360  364  WPR1
     I                             P  365  369 2PR2
     I                                365  369  WPR2
     I                                370  370  TIPCS
     I                                371  371  VIAG
     I                             P  376  379 2FAT
     I                             P  403  406 0VLNOVO
     I                             P  407  409 0VLEXTR
     I                             P  410  413 0CAPDPI
     I                                414  414  ARTG5
     I                                415  416  CODFRQ
     I                                417  421 0FRANQ
     I                             P  422  423 1DESCTO
     I                                424  424 0CATV
     I                                425  425 0CL
     I                                426  426 0PB
     I                                427  429  CATR
     I                             P  430  434 2PRSAGR
     I                             P  435  437 0CAPOCU
     I                             P  438  442 2PROCU
     I                             P  489  494 0CAPFAT
     I                                495  500 0CDCAE
     I                                501  501  MOD
     I                                502  502  TPT
     I                                503  505 0JMM1
     I                             P  506  508 3JMC1
     I                                509  511 0JMM2
     I                             P  512  514 3JMC2
     I                                515  515 0CR2
     I                                516  516 0CR3
     I                                517  517  QIV
     I                                524  524 0PRIV
     C****************************************************************
     C     *ENTRY        PLIST
     C                   PARM                    PAnoMes
      /FREE

          EXSR OpenFile; // Abrir ficheiro de Consulta
          IF (WIcErro = *ZEROS);
             //---------------------------------------
             // Inicializar a Lista de Trabalho
             //---------------------------------------
             WApolBk = *ZEROS;
             WTRegBk = *ZEROS;
             WIdx = 1;
             DOW (WIdx <= 48);
               WIdxLin = 1;
               DOW (WidxLin <= WMaxLin);
                  WTotCat(WIdx).Rub(WIdxLin) = *ZEROS;
                  WIdxLin += 1;
               ENDDO;
               WIdx += 1;
             ENDDO;

             //---------------------------------------
             // Contabilizar os Totais por Cobertura
             //---------------------------------------
             OPEN NMPM1;
             READ NMPM1;
             DOW (NOT %EOF());
                WIcErro = *ZEROS;
                IF (YPRS = 8080808) OR (YPRS = 4040404);
                   WIcErro = 2; // Valores inválidos (Não processar)
                ENDIF;

                //-----------------------------------
                // Tratar Valores Nulo
                //-----------------------------------
                IF (WIcErro = *ZEROS);
                   EXSR ExceptValores; // Tratar valores nulos
                   IF (WApolBk <> APOL) OR (WTRegBk <> TREG);
                      WApolBk = APOL;
                      WTRegBk = TREG;
                      EXSR getIndice; // Obter o Indice
                   ENDIF;
                ENDIF;

                //-----------------------------------
                // Tratar Coberturas
                //-----------------------------------
                IF (WIcErro = *ZEROS);
                   WIcDP = *OFF;
                   IF (RR = 51) AND (CR3 > *ZEROS);
                      EXSR AcPessoais;
                   ELSEIF (RR = 52);
                      EXSR AssiViagem;
                   ELSEIF (RR = 54);
                      EXSR ProtJuridic;
                   ELSEIF (RR <> 53);
                      WIcErro = 2; // Não processar
                   ENDIF;
                ENDIF;

                //-----------------------------------
                // Tratar Movimentos
                //-----------------------------------
                IF (WIcErro = *ZEROS);
                   IF (WIcDP);
                      PR1 = *ZEROS;
                      PR2 = *ZEROS;
                   ELSEIF (PRS > *ZEROS) AND (PR1 = *ZEROS);
                      PR2 = PRS;
                   ELSEIF (PR2 < *ZEROS);
                      IF (TREG = 1); // Recibos Adicionais
                         IF (PRS = *ZEROS);
                            PR2 = *ZEROS;
                            PR1 = *ZEROS;
                         ENDIF;
                      ENDIF;
                   ENDIF;
                   WPremTot = PR1 + PR2;

                   WTotCat(WIdx).Rub(1) += PRS;      // Prémio Simples
                   WTotCat(WIdx).Rub(2) += WPremTot; // Prémio Total
                   WTotCat(WIdx).Rub(3) += PR1;      // Prémio RC
                   WTotCat(WIdx).Rub(4) += PR2;      // Prémio DP
                   WTotCat(WIdx).Rub(5) += CUSTAA;   // Custo Apólice
                   WTotCat(WIdx).Rub(6) += CRC;      // Custo RC
                ENDIF;
                READ NMPM1;
             ENDDO;
             CLOSE NMPM1;
          ENDIF;

          //-------------------------------------------------
          // Imprimir Valores do Mapa
          //-------------------------------------------------
          IF (WIcErro <> 1);
             EXSR PrtValores;
          ENDIF;

          *INLR = *ON;
          RETURN;

       //==========================================================
       // Subrotina..: ExceptValores
       // Objectivo..: Tratar campos e trabalhar excepções de valores
       //              a nulos.
       //==========================================================
       BEGSR ExceptValores;
          IF (WPrs = *BLANKS);
             PRS = *ZEROS;
             PR1 = *ZEROS;
             PR2 = *ZEROS;
          ENDIF;
          IF (WPr1 = *BLANKS);
             PR1 = *ZEROS;
          ENDIF;
          IF (WPr2 = *BLANKS);
             PR2 = *ZEROS;
          ENDIF;
          IF (WCusta = *BLANKS);
             CUSTAA = *ZEROS;
          ENDIF;
          IF (WCRC = *BLANKS);
             CRC = *ZEROS;
          ENDIF;
       ENDSR;

       //==========================================================
       // Subrotina..: getIndice
       // Objectivo..: Obter o Indice da Tabela Interna
       //==========================================================
       BEGSR getIndice;
          IF (CATV > 7) OR (CATV = *ZEROS);
             CHAIN (50:APOL) BASEGRFA;
             IF (%FOUND());
                CATV = APO_CATV;
             ELSE;
                CATV = 5;
             ENDIF;
          ENDIF;

          //--------------------------------------------------------   -
          // Formatar a chave combinado Categorias com Tipo Registo
          //--------------------------------------------------------   -
          IF (CATV = 1) OR (CATV = 6) OR (CATV = 7);
             WFactor = 0;
          ELSEIF (CATV = 2);
             WFactor = 1;
          ELSEIF (CATV = 3);
             WFactor = 2;
          ELSEIF (CATV = 4);
             WFactor = 3;
          ELSEIF (CATV = 5);
             WFactor = 4;
          ENDIF;
          WIdx = (WFactor * WMaxCol) + (TREG + 1);
       ENDSR;

       //==========================================================
       // Subrotina..: AcPessoais
       // Objectivo..: Tratar informação de Acidentes Pessoais.
       //==========================================================
       BEGSR AcPessoais;
          WTotCat(WIdx).Rub(13) += PRS; // Prémio Ocupantes
          WIcDP = *ON;
       ENDSR;

       //==========================================================
       // Subrotina..: AssiViagem
       // Objectivo..: Tratar informação de Assistência em Viagem.
       //==========================================================
       BEGSR AssiViagem;
          IF (PRS = 12.61);
             WPrsRes = 8.73;
          ELSEIF (PRS = 12.90);
             WPrsRes = 8.98;
          ELSEIF (PRS = 10.75); // Com isenção
             WPrsRes = 8.98;
          ELSE;
             WPrsRes = *ZEROS;
          ENDIF;

          //----------------------------------------------------
          // Proceder à soma dos Valores
          //----------------------------------------------------
          WDifPrem = PRS - WPrsRes;
          WTotCat(WIdx).Rub(7) += PRS;     // Prémio Assist. Viagem
          WTotCat(WIdx).Rub(8) += WPrsRes; // Prémio Resseg. A. Viagem
          WTotCat(WIdx).Rub(9) += WDifPrem;// Prémio Sede Assist.Viagem
          WIcDP = *ON;
       ENDSR;

       //==========================================================
       // Subrotina..: ProtJuridic
       // Objectivo..: Tratar informação de Protecção Juridica.
       //==========================================================
       BEGSR ProtJuridic;
          IF (PRS = 6.66);
             WPrsPJu = 3.49;
          ELSEIF (PRS = 5.55);
             WPrsPJu = 3.49;
          ELSE;
             WPrsPJu = *ZEROS;
          ENDIF;

          WDifPJu = PRS - WPrsPJu;
          WTotCat(WIdx).Rub(10) += PRS;     // Premio Assist.Viagem
          WTotCat(WIdx).Rub(11) += WPrsPJu; // Premio Ress.Assist.Viagem
          WTotCat(WIdx).Rub(12) += WDifPJu; // Premio Sede Assist.Viagem
          WIcDP = *ON;
       ENDSR;

       //==========================================================
       // Subrotina..: PrtValores
       // Objectivo..: Imprimir os valores do Mapa
       //==========================================================
       BEGSR PrtValores;
          WIdx = 1;
          WQtdeVeicul = 1;
          WCodCat = *ZEROS;
          DOW (WIdx <= 8);
             //-------------------------------------------------
             // Imprimir 3 quadros por folha
             //-------------------------------------------------
             WQtdeVeicul -= 1;
             IF (WQtdeVeicul = *ZEROS);
                WRITE HDRGERAL;
                WQtdeVeicul = 3;
             ENDIF;

             PRT_DESVIAT = DesCategoria(WIdx);
             WRITE HDRCATV;
             WIdxLin = 1;
             DOW (WIdxLin <= WMaxLin);
                PRT_DESRUBR = DesRubrica(WIdxLin);
                PRT_IMTOTAL = *ZEROS;
                WIdxCol = ((WIdx - 1) * WMaxCol) + 1;
                DOW (WIdxCol <= ((WIdx - 1) * WMaxCol) + WMaxCol);
                   WI = (WIdxCol - %INT(WIdxCol/WMaxCol)*WMaxCol);
                   IF (WI = 1);
                      PRT_IMNOVOS = WTotCat(WIdxCol).Rub(WidxLin);
                      PRT_IMTOTAL += WTotCat(WIdxCol).Rub(WidxLin);
                   ELSEIF (WI = 2);
                      PRT_IMADICIONA = WTotCat(WIdxCol).Rub(WidxLin);
                      PRT_IMTOTAL += WTotCat(WIdxCol).Rub(WidxLin);
                   ELSEIF (WI = 3);
                      PRT_IMCONTINUA = WTotCat(WIdxCol).Rub(WidxLin);
                      PRT_IMTOTAL += WTotCat(WIdxCol).Rub(WidxLin);
                   ELSEIF (WI = 4);
                      PRT_IMANUESTOR = WTotCat(WIdxCol).Rub(WidxLin);
                      PRT_IMTOTAL += WTotCat(WIdxCol).Rub(WidxLin);
                   ELSEIF (WI = 5);
                      PRT_IMANULADOS = WTotCat(WIdxCol).Rub(WidxLin);
                      PRT_IMTOTAL -= WTotCat(WIdxCol).Rub(WidxLin);
                   ELSEIF (WI = 0);
                      PRT_IMESTORNOS = WTotCat(WIdxCol).Rub(WidxLin);
                      PRT_IMTOTAL -= WTotCat(WIdxCol).Rub(WIdxLin);
                   ENDIF;

                   //------------------------------------------------
                   // Actualizar Totais Gerais/Tipo
                   //------------------------------------------------
                   IF (WIdxCol <= 30);
                      IF (WI = *ZEROS);
                         WI = 6;
                      ENDIF;
                      WI += 30;
                      WTotCat(WI).Rub(WIdxLin)+=WTotCat(WIdxCol).Rub(WIdxLin);
                      IF (WIdx = 1) OR (WIdx = 4);
                         WI += 6;
                      ELSE;
                         WI += 12;
                      ENDIF;
                      WTotCat(WI).Rub(WIdxLin)+=WTotCat(WIdxCol).Rub(WIdxLin);
                   ENDIF;
                   WIdxCol += 1;
                ENDDO;
                WRITE DETALHE;
                WIdxLin += 1;
             ENDDO;
          WIdx += 1;
          ENDDO;
       ENDSR;

       //==========================================================
       // Subrotina..: OpenFile
       // Objectivo..: Abrir o ficheiro de dados com a informação
       //              no mês e data pedido, para processar.
       //==========================================================
       BEGSR OpenFile;
          WANoMes = %DEC(PAnoMes:6:0);
          WIcErro = *ZEROS;
          MONITOR;
             ANOMES   =  WAnoMes;
          // WNomFile = 'NMPM1'+ %TRIM(%CHAR(MESMOV));
             WNomFile = 'NREG'+ %TRIM(%CHAR(MESMOV));
             WStdCmd = 'OVRDBF FILE(NMPM1) TOFILE(QS36F/'+
                       %TRIM(WNomFile)+')';
             WStdLen = %LEN(WStdCmd);
             shell(WStdCmd:WStdLen);
          ON-ERROR;
             WIcErro = 1; // Não foi possível fazer Override ao Ficheiro
          ENDMON;
       ENDSR;
      /END-FREE
**
01Janeiro
02Fevereiro
03Março
04Abril
05Maio
06Junho
07Julho
08Agosto
09Setembro
10Outubro
11Novembro
12Dezembro
**
01Prémio Comerc..:
02Prémio (RC+DP).:
03Prémio (RC)....:
04Prémio (DP)....:
05Custo Apólice..:
06Custo CRC......:
07Prémio.A.Viagem:
08Pr.Resseg.A.V..:
09Pr.Sede A.V....:
10Prémio P.Jurid.:
11Pr.Resseg.P.J..:
12Pr.Sede P.Jur..:
13Prémio Ocupant.:
**
01PRIVATE
02VANS
03LORRIES
04MOTOR CYCLES
05OTHERS
06TOTAL (COLUNAS)
07TOTAL (PARTICULARES)
08TOTAL (COMERCIAIS)
