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
     FSEDE26M   O    E             PRINTER PREFIX(PRT_)                         USROPN
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
     D   DAT#PARM      DS                  QUALIFIED
     D      ANOMES             1      6S 0
     D      ANO                1      4A
     D      MES                5      6A
     D
     D*==> Formato de Registo
     D   REC#NMPM      DS
     D      NREC               1      9S 0
     D      RRAPOL            10     18S 0
     D      RR                10     11S 0
     D      APOL              12     18S 0
     D      TREG             185    185S 0
     D      FRAC             192    192  0
     D      CAP              199    204P 0
     D      CAPRC            205    207P 0
     D      PRS              208    212P 2
     D      YPRS             209    212P 0
     D      WPRS             208    212A
     D      BONUS            213    216P 2
     D      ENC              217    220P 2
     D      CUSTAA           221    222P 2
     D      WCUSTA           221    222A
     D      CRC              223    224P 2
     D      WCRC             223    224A
     D      PJUR             225    225A
     D      CR1              226    226S 0
     D      SELOS            227    230P 2
     D      SNA              231    234P 2
     D      SNB              235    238P 2
     D      PRTOT            239    243P 2
     D      CONC             246    247P 0
     D      MED1             248    250  0
     D      COM1             251    255P 2
     D      MED2             256    258P 0
     D      COM2             259    263P 2
     D      PRRC             360    364P 2
     D      WPRRC            360    364A
     D      PRDP             365    369P 2
     D      WPRDP            365    369A
     D      TIPCS            370    370A
     D      VIAG             371    371A
     D      FAT              376    379P 2
     D      VLNOVO           403    406P 0
     D      VLEXTR           407    409P 0
     D      CAPDPI           410    413P 0
     D      ARTG5            414    414A
     D      CODFRQ           415    416A
     D      FRANQ            417    421S 0
     D      DESCTO           422    423P 1
     D      CATV             424    424S 0
     D      CL               425    425S 0
     D      PB               426    426S 0
     D      CATR             427    429A
     D      PRSAGR           430    434P 2
     D      CAPOCU           435    437P 0
     D      PROCU            438    442P 2
     D      CAPFAT           489    494P 0
     D      CDCAE            495    500S 0
     D      MOD              501    501A
     D      TPT              502    502A
     D      JMM1             503    505S 0
     D      JMC1             506    508P 3
     D      JMM2             509    511S 0
     D      JMC2             512    514P 3
     D      CR2              515    515S 0
     D      CR3              516    516S 0
     D      QIV              517    517A
     D      PRIV             524    524S 0
     D      DUMMY            525    550A
     D
     D*==> Executar um Comando Externo
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
     D
     D*==> Definição de constantes
     D WSomaLinha      C                   31
     D WSomaColuna     C                   32
     D WSomaPartic     C                   33
     D WSomaComerc     C                   34
     D WMaxCol         C                    6
     D WMaxLin         C                   13
     D R#PrmSimples    C                   1
     D R#PrmTotal      C                   2
     D R#PrmRC         C                   3
     D R#PrmDP         C                   4
     D R#CustApolice   C                   5
     D R#CustRC        C                   6
     D R#PrmAV         C                   7
     D R#PrmResseg     C                   8
     D R#PrmSedeAV     C                   9
     D R#PrmProtJur    C                   10
     D R#PrmRessPJur   C                   11
     D R#PrmSedePJur   C                   12
     D R#PrmOcupantes  C                   13
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
             READ NMPM1 REC#NMPM;
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
                      PRRC = *ZEROS;
                      PRDP = *ZEROS;
                   ELSEIF (PRS > *ZEROS) AND (PRRC = *ZEROS);
                      PRDP = PRS;
                   ELSEIF (PRDP < *ZEROS);
                      IF (TREG = 1); // Recibos Adicionais
                         IF (PRS = *ZEROS);
                            PRDP = *ZEROS;
                            PRRC = *ZEROS;
                         ENDIF;
                      ENDIF;
                   ENDIF;
                   WPremTot = PRRC + PRDP;

                   WTotCat(WIdx).Rub(R#PrmSimples) += PRS;      // Prémio Simples
                   WTotCat(WIdx).Rub(R#PrmTotal)   += WPremTot; // Prémio Total
                   WTotCat(WIdx).Rub(R#PrmRC) += PRRC;          // Prémio RC
                   WTotCat(WIdx).Rub(R#PrmDP) += PRDP;          // Prémio DP
                   WTotCat(WIdx).Rub(R#CustApolice) += CUSTAA;  // Custo Apólice
                   WTotCat(WIdx).Rub(R#CustRC) += CRC;        // Custo RC
                ENDIF;
                READ NMPM1 REC#NMPM;
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
             PRS  = *ZEROS;
             PRRC = *ZEROS;
             PRDP = *ZEROS;
          ENDIF;
          IF (WPRRC = *BLANKS);
             PRRC = *ZEROS;
          ENDIF;
          IF (WPRDP = *BLANKS);
             PRDP = *ZEROS;
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
          WTotCat(WIdx).Rub(R#PrmOcupantes) += PRS; // Prémio Ocupantes
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
          WTotCat(WIdx).Rub(R#PrmAV) += PRS;         // Prémio Assist. Viagem
          WTotCat(WIdx).Rub(R#PrmResseg) += WPrsRes; // Prémio Resseg. A. Viagem
          WTotCat(WIdx).Rub(R#PrmSedeAV) += WDifPrem;// Prémio Sede Assist.Viagem
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
          WTotCat(WIdx).Rub(R#PrmProtJur) += PRS;      // Premio Assist.Viagem
          WTotCat(WIdx).Rub(R#PrmRessPJur) += WPrsPJu; // Premio Ress.Assist.Viagem
          WTotCat(WIdx).Rub(R#PrmSedePJur) += WDifPJu; // Premio Sede Assist.Viagem
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
             DAT#PARM.ANOMES   =  WAnoMes;
          // WNomFile = 'NMPM1'+ DAT#PARM.MES;
             WNomFile = 'NREG'+ DAT#PARM.MES;
             WStdCmd = 'OVRDBF FILE(NMPM1) TOFILE(QS36F/'+
                       %TRIM(WNomFile)+')';
             WStdLen = %LEN(WStdCmd);
             shell(WStdCmd:WStdLen);
          ON-ERROR;
             WIcErro = 1; // Não foi possível fazer Override ao Ficheiro
          ENDMON;

          MONITOR;
             WNomFile = 'SEDE26M';
             WStdCmd = 'OVRPRTF FILE(SEDE26M) TOFILE(LGALHOZ/'+
                       %TRIM(WNomFile)+') HOLD(*YES)';
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
