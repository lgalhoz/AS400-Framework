     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PCOBO140
     H* Descrição.: Actualizar Base Dados de Descontos em Vencimento,
     H* Observação: As Apólices da Companhia são Adicionadas
     H*             Automáticamente nesta rotina, as das outras
     H*             Companhias são adicionadas à mão.
     H*==================================================================
     FGFSEGEXT  IF   E           K DISK    PREFIX('SEG_')
     FGBASEGRL  IF   E           K DISK    PREFIX('BAS_')
     F
     FFCOBP141  UF   E           K DISK    PREFIX('COB_')
     FFCOBP142  UF A E           K DISK    PREFIX('COB_')
     D*==================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010

     D*==> Lista interna de Contratos do Segurado
     D LST#APOL        DS                  QUALIFIED DIM(20)
     D   NumOrdem                     8S 0
     D   Origem                       1A
     D   Companh                      3S 0
     D   Ramo                         3S 0
     D   RamoLusit                    2S 0
     D   Apolice                     10A
     D   Inicio                       6S 0
     D   Mediador                     3S 0
     D   Segurado                     8S 0
     D   IsEmpty                       N
     D WIdxApol        S              4S 0
     D
     D*==> Estrutra de dados para conversão de datas
     D CVT#MACONT      DS                  QUALIFIED
     D   MACONT                1      4S 0
     D   Mes                   1      2S 0
     D   Ano                   3      4S 0
     D
     D*==> Tradução de Ramos
     D RMCMP           S              2  0 DIM(22) CTDATA PERRCD(1)
     D RMCOR           S              3  0 DIM(22) ALT(RMCMP)
     D WIdxRamo        S              4S 0
     D
     D*==> Campos de Trabalho
     D WNomFile        S             10A
     D WOrigem         S              1A
     D WSessApol       S              5S 0
     D WSessSeg        S              5S 0
     D WRecApol        S              5S 0
     D WRecSeg         S              5S 0
     D WNumOrdem       S              8S 0
     D WCodRamo        S              3S 0
     D WNumApol        S              7S 0
     D WCodSegur       S              8S 0
     D WSegLoaded      S               N
     D WDatInicio      S               D
     D WDatTermo       S               D
     D WProxContin     S              6S 0
     D WPremTotal      S             13S 2
     D WObjecto        S             60A
     D
     D*==> Constantes
     D Char            C                   '###########################'
     D up              C                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ?'
     D lo              C                   'abcdefghijklmnopqrstuvwxyz?'
     D*==================================================================
      /FREE

       //-------------------------------------------------
       // Obter Segurados com desconto em vencimento
       //-------------------------------------------------
       WSessSeg = $SetFiles('FCOBP141');
       $SetFields('CODSEG');
       $SetCriteria('ISTOSELECT = ''S''');
       WRecSeg = 1;
       DOW ($GetRecord(WRecSeg) > *ZEROS);
          WIdxApol = 1;
          WCodSegur = $GetFieldNum('CODSEG');
          LST#APOL(WIdxApol).IsEmpty = *ON;

          WOrigem = 'A';
          WNomFile = '"SD.1.AP"';
          EXSR ExtFromCorretor;

          WOrigem = 'M';
          WNomFile = '"SD.1.APM"';
          EXSR ExtFromCorretor;

          //----------------------------------------------
          // Actualizar informação das Apólices
          //----------------------------------------------
          EXSR ActApolices;

          WRecSeg += 1;
       ENDDO;
       $RmvDBSession(WSessSeg);

       *INLR = *ON;
       RETURN;

       //========================================================
       // Subrotina.: ExtFromCorretor
       // Descrição.: Extrair Apólices do Corretor
       //========================================================
       BEGSR ExtFromCorretor;
          WSessApol= $SetFiles(WNomFile);
          $SetFields('NORD,APOLI,CCOMP,CRAMO,CSEG,DTINI,RI1');
          $SetCriteria('CSEG = ' + %CHAR(WCodSegur));
          WRecApol = 1;
          DOW ($GetRecord(WRecApol) > *ZEROS);
             LST#APOL(WIdxApol).NumOrdem = $GetFieldNum('NORD');
             LST#APOL(WIdxApol).Origem   = WOrigem;
             LST#APOL(WIdxApol).Companh  = $GetFieldNum('CCOMP');
             LST#APOL(WIdxApol).Ramo     = $GetFieldNum('CRAMO');
             LST#APOL(WIdxApol).Apolice  = $GetField('APOLI');
             LST#APOL(WIdxApol).Inicio   = $GetFieldNum('DTINI');
             LST#APOL(WIdxApol).Mediador = $GetFieldNum('RI1');
             LST#APOL(WIdxApol).Segurado = $GetFieldNum('CSEG');
             LST#APOL(WIdxApol).RamoLusit= *ZEROS;
             LST#APOL(WIdxApol).IsEmpty  = *OFF;

             IF (LST#APOL(WIdxApol).Companh = 54);
                FOR WIdxRamo = 1 By 1 to 22;
                   IF (LST#APOL(WIdxApol).Ramo = RMCOR(WIdxRamo));
                      LST#APOL(WIdxApol).RamoLusit = RMCMP(WIdxRamo);
                      LEAVE;
                   ENDIF;
                ENDFOR;
             ENDIF;

             WIdxApol += 1;
             LST#APOL(WIdxApol).IsEmpty  = *ON;
             WRecApol += 1;
          ENDDO;
          $RmvDBSession(WSessApol);
       ENDSR;

       //========================================================
       // Subrotina.: ActApolices;
       // Descrição.: Extrair Apólices do Corretor
       //========================================================
       BEGSR ActApolices;
          WSegLoaded = *OFF;
          WIdxApol = 1;
          DOW (NOT LST#APOL(WIdxApol).IsEmpty);
             IF (NOT WSegLoaded);
                CHAIN (LST#APOL(WIdxApol).Segurado) RCOBP141;
                COB_QTDEAPOL = *ZEROS;
                WSegLoaded = *ON;
             ENDIF;
             IF (LST#APOL(WIdxApol).Companh = 54);
                WNumOrdem = LST#APOL(WIdxApol).NumOrdem;
                COB_QTDEAPOL += 1;
                WCodRamo = LST#APOL(WIdxApol).RamoLusit;
                WNumApol = %DEC(%TRIM(LST#APOL(WIdxApol).Apolice):7:0);
                EXSR ActContratos;
             ENDIF;
             WIdxApol += 1;
          ENDDO;
          IF (WSegLoaded);
             UPDATE RCOBP141 %FIELDS(COB_QTDEAPOL);
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: ActContratos
       // Descrição.: Actualizar informação de Novos Contratos
       //             do Segurado.
       //========================================================
       BEGSR ActContratos;
          CHAIN (WCodRamo:WNumApol) GBASEGRL;
          IF (%FOUND(GBASEGRL));
             EXSR FmtFields;
             CHAIN (WNumOrdem) RCOBP142;
             IF (NOT %FOUND(FCOBP142));
                COB_NUMORDEM  = WNumOrdem;
                COB_CODRAMO   = BAS_RAMO;
                COB_NUAPOL    = LST#APOL(WIdxApol).Apolice;
                COB_CODCOMP   = LST#APOL(WIdxApol).Companh;

                COB_CODSEG    = BAS_NSEG;
                COB_CODSTAT   = %CHAR(BAS_SIT);
                COB_DATINICIO = WDatInicio;
                COB_DATTERMO  = WDatTermo;
                COB_MACONT    = WProxContin;
                COB_FRACCIO   = BAS_FRAC;
                COB_OBJECTO   = WObjecto;

                COB_INIPREV   = *ZEROS;
                COB_ULTCONTI  = WPremTotal;
                COB_QTDEREC   = *ZEROS;
                IF (COB_CODSTAT = '3');
                   COB_ISTOSELECT= 'N';
                ELSE;
                   COB_ISTOSELECT= 'S';
                ENDIF;
                COB_IMSALDO   = *ZEROS;
                COB_PRESTACOES= 2;

                COB_COB14201  = %DATE('2000-01-01');
                COB_COB14202  = %DATE('9999-12-31');
                WRITE RCOBP142;
             ELSE;
                COB_CODSEG    = BAS_NSEG;
                COB_CODSTAT   = %CHAR(BAS_SIT);
                COB_DATINICIO = WDatInicio;
                COB_DATTERMO  = WDatTermo;
                COB_QTDEREC   = *ZEROS;
                COB_MACONT    = WProxContin;
                COB_FRACCIO   = BAS_FRAC;
                COB_OBJECTO   = WObjecto;
                COB_INIPREV   = *ZEROS;
                COB_IMSALDO   = *ZEROS;
                IF (COB_CODSTAT = '3');
                   COB_ISTOSELECT= 'N';
                ENDIF;
                COB_ULTCONTI  = WPremTotal;
                UPDATE RCOBP142 %FIELDS(COB_CODSEG:COB_CODSTAT:COB_DATINICIO
                                        :COB_DATTERMO:COB_MACONT:COB_FRACCIO
                                        :COB_OBJECTO:COB_INIPREV
                                        :COB_ULTCONTI:COB_QTDEREC
                                        :COB_IMSALDO:COB_ISTOSELECT);
             ENDIF;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: FmtFields
       // Descrição.: FoCompr com valores os diferentes campos
       //             do Contrato.
       //========================================================
       BEGSR FmtFields;
          WDatInicio = $IntToDate($CvtData(BAS_DTINI:'DMY':'YYMD'));
          WDatTermo  = %DATE('9999-12-31'); // ForCompr Termo
          WPremTotal = BAS_TOTREC;

          CVT#MACONT.MACONT = BAS_MACONT;
          WProxContin = 200000 + (CVT#MACONT.Ano * 100) + CVT#MACONT.Mes;
          WObjecto    = *BLANKS;
          IF (BAS_RAMO = 50) OR (BAS_RAMO = 51);
             WObjecto = %TRIM(BAS_MATR1) + ' (' + %TRIM(BAS_MARC1) + ' '
                                                   + %TRIM(BAS_MODELO) + ')';
             IF (BAS_SIT <> 3);
                WDatTermo = $IntToDate($CvtData(BAS_DTALT:'DMY':'YYMD'));
             ENDIF;
          ELSE;
             WObjecto = %TRIM(BAS_LRPS);
             IF (BAS_SIT <> 3);
                WDatTermo = $IntToDate($CvtData(BAS_DALT:'DMY':'YYMD'));
             ENDIF;
          ENDIF;
       ENDSR;
      /END-FREE
** Conversão dos Ramos (Lusitânia vs Corretor)
01080  Incêndio e Elementos da Natureza
02014  Acidentes Pessoais
03130  Responsabilidade Civil Pessoal
04133  Responsabilidade Civil Geral
05342  MultiRiscos Habitação Residentia / Condominio
06160  Perda de Lucros
07092  Dinheiro em Trânsito
08093  Roubo Residências
09094  Roubo Estabelecimentos
10016  Viagem
15012  Acidentes de Trabalho
17346  Equipamento Electrónico
18096  Avaria de Máquinas
19345  Obras e Montagens
50310  Automóvel
51010  Ocupanetes
55343  Multiriscos estabelecimentos
65095  Cristais
67000  Bagagem  (Sem correspondência com o Corretor)
68071  Transportes Marítimo e Terrestre
69073  Transportes Postal
70072  Transportes Aéreo
