     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*===========================================================
     H* Programa..: RINTO014
     H* Descrição.: Obter as respectivas Apólices, e validar se a
     H*             comissão do recibo está em conformidade com o
     H*             esperado.
     H*
     H* Pesquisar a existência da apólice no Corretor nas Apólices
     H* activas e nas apólices em histórico (SD.1.APM).
     H*
     H*===========================================================
     F*==> Correspondência com as Apólices em JM e Modelo Comissionamento
     FFCATP205  IF   E           K DISK    PREFIX(CAT_)                         Modelos de Comissionamento
     FSD1RM     IF   F  128     3AIDISK    KEYLOC(5) EXTFILE('SD.1.RM')         Tabela Ramos e modelo Comissionamento
     FSD1AP1    IF   F  320    16AIDISK    KEYLOC(8) EXTFILE('SD.1.AP1')        Apólices Activas em J.Comp
     FSD1AP2    IF   F  320    16AIDISK    KEYLOC(8) EXTFILE('SD.1.AP2')        Apólice em Arquivo em J.Comp
     FFINTP021  UF A E           K DISK    PREFIX(REL_)                         Relação com Apólices J.Comp e regras DD
     F*
     F*==> Registo de Excepções quando detectado
     FFINTP023  UF A E           K DISK    PREFIX(INT_)                         Excepções
     FFINTP02301IF   E           K DISK    PREFIX(INT_)
     F                                     RENAME(RINTP023:RINTP02301)
     FFINTP024  UF A E           K DISK    PREFIX(INT_)                         Diferenças de Comissões
     FFINTP025  UF A E           K DISK    PREFIX(INT_)                         Relação Excepções com Recibos Standard
     FFINTP026  UF A E           K DISK    PREFIX(INT_)                         Tratamento excepções sobre recibos Stand
     F*
     F*==> Correspondência com os Recibos Standard e Interfaces
     FFINTP020  UF   E           K DISK    PREFIX(INT_)                         Relação dos Interfaces recebidos
     FFINTP028  UF   E           K DISK    PREFIX(INT_)                         Ficheiros de Recibos Standard
     FFINTP029  UF A E           K DISK    PREFIX(INT_)                         Acções sobre Recibos Standard
     FFINTP02701IF   E           K DISK    PREFIX(INT_)                         Relação Recibos Standard com Interfaces
     F                                     RENAME(RINTP027:RINTP02701)
     F*===========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010

     D*==> Processos
     D GetTaxComiss    PR             7S 4
     D   TipoComissao                 3A   CONST
     D   Posicao                      2S 0 CONST
     D   ComAngar                     7S 4 CONST
     D   ComCobra                     7S 4 CONST
     D   ComCorret                    7S 4 CONST
     D
     D*==> Tabela de Ramos - Corretor
     D R#SD1RM         DS
     D   RM_Tipo               1      4
     D   RM_Codigo             5      7S 0
     D   RM_DescCompl          8     30A
     D   RM_IcAnula           31     31A
     D   RM_DescAbrev         32     40A
     D   RM_SeqSumm           41     43S 0
     D   RM_EncFixos          44     46P 3
     D   RM_ComAngar          47     49P 3
     D   RM_ImpSelo           50     52P 3
     D   RM_ComCobra          53     55P 3
     D   RM_OutDesp           56     58P 3
     D   RM_ComCorr           59     61P 3
     D   RM_Filler            62    111A
     D   RM_NumAltera        112    114S 0
     D   RM_UseAltera        115    122A
     D   RM_DatAltera        123    128S 0
     D
     D*==> Registo de Apólices - Corretor
     D R#AP1         E DS                  EXTNAME('SD.1.AP1') PREFIX('AP_')
     D R#APKEY         DS
     D   AP_KEY                1     16
     D   APOLI                 1     10A
     D   CCOMP                11     13S 0
     D   CRAMO                14     16S 0
     D
     D*==> Controlo de Excepções P/Companhia
     D DS#EXCEPT       DS                  QUALIFIED DIM(MAX#Exception)
     D   Codigo                       9S 0
     D   TipExcept                    1A
     D   Companhia                    5S 0
     D   TotComAng                    9S 0
     D   TotComCobr                   9S 0
     D   TotComCorr                   9S 0
     D   TotComiss                    9S 0
     D   QtdeRecib                    5S 0
     D   IsEmpty                       N   INZ(*ON)
     D WIdx            S              2S 0
     D
     D*==> Campos de Trabalho
     D WCdExcept       S              9S 0
     D WTipExcept      S              1A
     D WAction         S              1A
     D WCdRegis        S              9S 0
     D WCodErro        S              7A
     D WCodCompanhia   S              3S 0
     D WNuApolice      S             10A
     D
     D WPerComAng      S              7S 4
     D WPerComCobr     S              7S 4
     D WPerComCorr     S              7S 4
     D WDifComAng      S              9S 2
     D WDifComCobr     S              9S 2
     D WDifComCorr     S              9S 2
     D WDifTotCom      S              9S 2
     D WPrComiss1      S              7S 4
     D WPrComiss2      S              7S 4
     D WPrComiss3      S              7S 4
     D
     D WValComiss      S              7S 3
     D WDbSession      S              5S 0
     D WTotComissao    S              9S 2
     D WComissao       S              7S 3
     D WNumSeg         S              8S 0
     D WCodRamo        S              3S 0
     D WCodFile        S              9S 0
     D WNumOrdem       S              8S 0
     D WConvRamo       S              3S 0
     D WIsArquivo      S              1A
     D
     D*==> Constantes
     D EXC#Apolice     C                   'A'
     D EXC#Comissao    C                   'E'
     D KTE#TOLERA      C                   0,05
     D MAX#Exception   C                   25
     D STS#Activo      C                   'A'
     D OPR#Criacao     C                   'C'
     D ACT#Criar       C                   'C'
     D ACT#Eliminar    C                   'E'
     D*===========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCdRegis
     C                   PARM                    WCodErro
     C
      /FREE
        DS#EXCEPT(1).IsEmpty = *ON;
        WCodErro  = *BLANKS;

        //-----------------------------------------------------------------
        // Actualizar informação para a Apólice
        //-----------------------------------------------------------------
        CHAIN (WCdRegis) RINTP028;
        IF (%FOUND(FINTP028));
           WCodCompanhia = INT_CCOMP;
           IF (INT_CDESTAD = '1');
              WCodRamo = *ZEROS;
              EXSR GetApolice;
              IF (WCodRamo > *ZEROS);
                 //-----------------------------------------------
                 // Registar o ramo e o Segurado do Recibo
                 //-----------------------------------------------
                 INT_CDESTAD  = '2';
                 INT_CDRAMO   = WCodRamo;
                 INT_NUMORDAP = WNumOrdem;
                 INT_NUMSEG   = WNumSeg;
                 INT_DTHSTAT  = %TimeStamp();
                 UPDATE RINTP028 %FIELDS(INT_CDRAMO:INT_NUMSEG:INT_CDESTAD:
                                         INT_DTHSTAT:INT_NUMORDAP);

                 //-----------------------------------------------
                 // Actualizar Operaçao
                 //-----------------------------------------------
                 INT_NUOPERA  = $GetNextKey('FINTP029':'NUOPERA':
                                '(CDREGIS = '''+%CHAR(INT_CDREGIS)+''')');
                 INT_CDOPERA = 'ST1'; // Associação com a Apólice
                 INT_CDERRO  = WCodErro;
                 INT_DTOPERA = %DATE();
                 INT_HROPERA = %TIME();
                 INT_USOPERA = S#USR;
                 WRITE RINTP029;

                 //-----------------------------------------------
                 // Marcar interface como finalizado
                 //-----------------------------------------------
                 CHAIN (INT_CDREGIS) RINTP02701;
                 IF (%FOUND(FINTP02701));
                    CHAIN (INT_CDFILE) RINTP020;
                    INT_CDESTAD = 'F';
                    UPDATE RINTP020 %FIELDS(INT_CDESTAD);
                 ENDIF;

                 //-----------------------------------------------
                 // Retirar registo de Anomalia da Apólice
                 //-----------------------------------------------
                 WTipExcept = EXC#Apolice;
                 WAction    = ACT#Eliminar;
                 EXSR ManutExcepcao;

                 //-----------------------------------------------
                 // Identificar diferenças nas Comissões
                 //-----------------------------------------------
                 EXSR ValComissao;
              ELSE;
                 //-----------------------------------------------
                 // Criar registo de Anomalias para a Apólice
                 //-----------------------------------------------
                 WTipExcept = EXC#Apolice;
                 WAction    = ACT#Criar;
                 EXSR ManutExcepcao;
              ENDIF;
           ENDIF;
        ENDIF;

        //-----------------------------------------------------------------
        // Actualizar Ficheiro de Excepções
        //-----------------------------------------------------------------
        WIdx = 1;
        DOW (NOT DS#EXCEPT(WIdx).IsEmpty);
           CHAIN (DS#EXCEPT(WIdx).Codigo) RINTP023;
           IF (%FOUND(FINTP023));
              INT_QTREGIS  = DS#EXCEPT(WIdx).QtdeRecib;
              UPDATE RINTP023 %FIELDS(INT_QTREGIS);

              IF (DS#EXCEPT(WIdx).TipExcept = 'E');
                 CHAIN (DS#EXCEPT(WIdx).Codigo) RINTP024;
                 IF (%FOUND(FINTP024));
                    INT_IMCOMIS1 = DS#EXCEPT(WIdx).TotComAng;
                    INT_IMCOMIS2 = DS#EXCEPT(WIdx).TotComCobr;
                    INT_IMCOMIS3 = DS#EXCEPT(WIdx).TotComCorr;
                    INT_DFTOCOM  = DS#EXCEPT(WIdx).TotComiss;
                    UPDATE RINTP024 %FIELDS(INT_IMCOMIS1:INT_IMCOMIS2:
                                            INT_IMCOMIS3:INT_DFTOCOM);
                 ENDIF;
              ENDIF;
           ENDIF;

           WIdx += 1;
           IF (WIdx > MAX#Exception);
              RETURN;
           ENDIF;
        ENDDO;

        *INLR = *ON;

       //=================================================================
       // SubRotina..: GetApolice
       // Objectivo..: Obter o registo da Apólice correspondente ao Recibo
       //=================================================================
       BEGSR GetApolice;
          //-----------------------------------------------
          // Obter a informação da Apólice
          //-----------------------------------------------
          WNuApolice = INT_NUAPOL;
          WConvRamo  = *ZEROS;
          CHAIN(N) (INT_CCOMP:INT_CDRMORG:INT_NUAPOLOR) RINTP021;
          IF (%FOUND(FINTP021));
             WConvRamo     = REL_CDRAMO;
             WCodCompanhia = REL_CDCOMP;
             WNuApolice    = REL_NUAPOLJM;
          ENDIF;

          WCodErro   = 'INT0001';
          WPrComiss1 = *ZEROS;
          WPrComiss2 = *ZEROS;
          WPrComiss3 = *ZEROS;
          WValComiss  = *ZEROS;
          APOLI       = WNuApolice;
          CCOMP       = WCodCompanhia;
          CRAMO       = *ZEROS;

          //---------------------------------------
          // Procurar nas Apólices Activas
          //---------------------------------------
          WIsArquivo = 'N';
          SETLL AP_KEY SD1AP1;
          READ  SD1AP1 R#AP1;
          DOW (NOT %EOF(SD1AP1));
             IF (APOLI = AP_APOLI) AND (AP_CCOMP = WCodCompanhia);
                EXSR ValidApolice;
             ELSE;
                LEAVE;
             ENDIF;
             READ  SD1AP1 R#AP1;
          ENDDO;

          //---------------------------------------
          // Procurar nas Apólices em Arquivo
          //---------------------------------------
          IF (WCodErro = 'INT0001');
             WIsArquivo = 'S';
             SETLL AP_KEY SD1AP2;
             READ  SD1AP2 R#AP1;
             DOW (NOT %EOF(SD1AP2));
                IF (APOLI = AP_APOLI) AND (CCOMP = WCodCompanhia);
                   EXSR ValidApolice;
                ELSE;
                   LEAVE;
                ENDIF;
                READ  SD1AP2 R#AP1;
             ENDDO;
          ENDIF;

          //----------------------------------------
          // Actualizar informação do Campo
          //----------------------------------------
          IF (WCodErro = *BLANKS);
             IF (WConvRamo = *ZEROS);
                REL_CCOMP    = INT_CCOMP;
                REL_CDCOMP   = INT_CCOMP;
                REL_CDRMORG  = INT_CDRMORG;
                REL_NUAPOL   = INT_NUAPOLOR;
                REL_NUAPOLJM = WNuApolice;
                REL_NUMORDAP = WNumOrdem;
                REL_CDRAMO   = WCodRamo;
                REL_ISDEBDIR = 'N';
                REL_ISARCHIV = WIsArquivo;
                WRITE RINTP021;
             ENDIF;
          ELSE;
             WNumOrdem = *ZEROS;
             WCodRamo  = *ZEROS;
             WNumSeg   = *ZEROS;
          ENDIF;
       ENDSR;

       //=================================================================
       // SubRotina..: ValidApolice
       // Objectivo..: Valdiar correspondência com a Apólice
       //=================================================================
       BEGSR ValidApolice;
          IF ((WConvRamo > *ZEROS) AND (WConvRamo = AP_CRAMO));
             EXSR GetComissao;
             WCodErro = *BLANKS;
             WNumOrdem = AP_NORD;
             WCodRamo = AP_CRAMO;
             WNumSeg  = AP_CSEG;
             LEAVESR;
          ELSEIF (WConvRamo = *ZEROS);
             IF (WCodErro = *BLANKS);
                WCodErro = 'INT0003';
             ELSEIF (WCodErro <> 'INT0003');
                EXSR GetComissao;
                WCodErro = *BLANKS;
                WNumOrdem = AP_NORD;
                WCodRamo = AP_CRAMO;
                WNumSeg  = AP_CSEG;
             ENDIF;
          ENDIF;
       ENDSR;

       //=================================================================
       // SubRotina..: Getcomissao;
       // Objectivo..: Obter o valor da comissão
       //=================================================================
       BEGSR GetComissao;
          WPrComiss1  = *ZEROS;
          WPrComiss2  = *ZEROS;
          WPrComiss3  = *ZEROS;
          WPerComAng  = *ZEROS;
          WPerComCobr = *ZEROS;
          WPerComCorr = *ZEROS;
          IF (AP_CDCOM = 'S') OR (AP_COMTO = *ZEROS);
             CHAIN AP_CRAMO SD1RM R#SD1RM;
             IF (%FOUND(SD1RM));
                WPerComAng  = RM_ComAngar;
                WPerComCobr = RM_ComCobra;
                WPerComCorr = RM_Comcorr;
                WValComiss  = RM_ComAngar + RM_ComCobra + RM_ComCorr;
             ELSE;
                CHAIN (INT_CCOMP:WCodRamo:1) RCATP205;
                IF (%FOUND(FCATP205));
                   WPerComAng  = CAT_PRANGA;
                   WPerComCobr = CAT_PRCOBR;
                   WPerComCorr = CAT_PRCORR;
                   WValComiss  = CAT_PRANGA + CAT_PRCOBR + CAT_PRCORR;
                ENDIF;
             ENDIF;
          ELSE;
             WValComiss = AP_COMTO;
          ENDIF;

          // WPrComiss1 = GetTaxComiss(INT_TPCALC:1:WPerComAnga:
          //                           WPerComCobr:WPerComCorr);
          // WPrComiss2 = GetTaxComiss(INT_TPCALC:2:WPerComAnga:
          //                           WPerComCobr:WPerComCorr);
          // WPrComiss3 = GetTaxComiss(INT_TPCALC:3:WPerComAnga:
          //                           WPerComCobr:WPerComCorr);
       ENDSR;

       //=================================================================
       // SubRotina..: ValComissao
       // Objectivo..: Validar o valor de Comissionamento
       //=================================================================
       BEGSR ValComissao;
          WTotComissao = INT_IMCOMIS1 + INT_IMCOMIS2 + INT_IMCOMIS3;
          WComissao    = WTotComissao / INT_IMPRCOM * 100;
          EXSR CalcDiferencas;
          IF (WDifTotCom > KTE#TOLERA) OR ((WDifTotCom * -1) > KTE#TOLERA);
             WTipExcept = EXC#Comissao;
             WAction    = ACT#Criar;
             EXSR ManutExcepcao;
          ENDIF;
       ENDSR;

       //=================================================================
       // SubRotina..: ManutExcepcao
       // Objectivo..: Adicionae um registo de Excepção
       //=================================================================
       BEGSR ManutExcepcao;
          //-------------------------------------------------
          // Obter o Código da Excepção para a Companhia
          //-------------------------------------------------
          WIdx = 1;
          WCdExcept = *ZEROS;
          DOW (NOT DS#EXCEPT(WIdx).IsEmpty);
             IF (DS#EXCEPT(WIdx).Companhia = WCodCompanhia) AND
               (WTipExcept = DS#EXCEPT(WIdx).TipExcept);
                WCdExcept = DS#EXCEPT(WIdx).Codigo;
                LEAVE;
             ENDIF;
             WIdx += 1;
             IF (WIdx > 25);
                RETURN;
             ENDIF;
          ENDDO;

          //-------------------------------------------------
          // Criar registo no Controlo de Comissões
          //-------------------------------------------------
          IF (WCdExcept = *ZEROS);
             CHAIN (WCodCompanhia:WTipExcept:STS#Activo) RINTP02301;
             IF (NOT %FOUND(FINTP02301));
                WCdExcept = $GetNextKey('FINTP023':'CDEXCEPT');
                DS#EXCEPT(WIdx).Companhia = WCodCompanhia;
                DS#EXCEPT(WIdx).Codigo    = WCdExcept;
                DS#EXCEPT(WIdx).TipExcept = WTipExcept;
                DS#EXCEPT(WIdx).TotComAng = *ZEROS;
                DS#EXCEPT(WIdx).TotComCobr= *ZEROS;
                DS#EXCEPT(WIdx).TotComCorr= *ZEROS;
                DS#EXCEPT(WIdx).TotComiss = *ZEROS;
                DS#EXCEPT(WIdx).QtdeRecib = *ZEROS;
                DS#EXCEPT(WIdx).IsEmpty   = *OFF;
                DS#EXCEPT(WIdx+1).IsEmpty   = *ON;

                INT_CDEXCEPT = WCdExcept;
                INT_CCOMP    = WCodCompanhia;
                INT_TPEXCEPT = WTipExcept;
                INT_CDESTADO = STS#Activo;
                INT_CDERRO   = *BLANKS;
                INT_DTEXCEP  = $DateToInt(%DATE());
                INT_QTREGIS  = *ZEROS;
                WRITE RINTP023;

                IF (WTipExcept = EXC#Comissao);
                   INT_TPCALC   = *BLANKS;
                   INT_IMCOMIS1 = *ZEROS;
                   INT_IMCOMIS2 = *ZEROS;
                   INT_IMCOMIS3 = *ZEROS;
                   INT_DFTOCOM  = *ZEROS;
                   INT_NURECIBO = *ZEROS;
                   INT_DTREGULA = %DATE('0001-01-01');
                   INT_USRREGUL = *ZEROS;
                   WRITE RINTP024;
                ENDIF;
             ELSE;
                WCdExcept = INT_CDEXCEPT;
                DS#EXCEPT(WIdx).Companhia = INT_CCOMP;
                DS#EXCEPT(WIdx).Codigo    = WCdExcept;
                DS#EXCEPT(WIdx).TipExcept = WTipExcept;
                DS#EXCEPT(WIdx).QtdeRecib = INT_QTREGIS;

                IF (WTipExcept = EXC#Comissao);
                   CHAIN (WCdExcept) RINTP024;
                   IF (%FOUND(FINTP024));
                      DS#EXCEPT(WIdx).TotComAng = INT_IMCOMIS1;
                      DS#EXCEPT(WIdx).TotComCobr= INT_IMCOMIS2;
                      DS#EXCEPT(WIdx).TotComCorr= INT_IMCOMIS3;
                      DS#EXCEPT(WIdx).TotComiss = INT_DFTOCOM;
                   ENDIF;
                ENDIF;
                DS#EXCEPT(WIdx).IsEmpty   = *OFF;
                DS#EXCEPT(WIdx+1).IsEmpty = *ON;
             ENDIF;
          ENDIF;

          //-------------------------------------------------
          // Associar a informaçao do Recibo com a diferença
          //-------------------------------------------------
          DS#EXCEPT(WIdx).TotComAng += WDifComAng;
          DS#EXCEPT(WIdx).TotComCobr+= WDifComCobr;
          DS#EXCEPT(WIdx).TotComCorr+= WDifComCorr;
          DS#EXCEPT(WIdx).TotComiss += WDifTotCom;
          DS#EXCEPT(WIdx).QtdeRecib += 1;

          INT_CDESTAD  = STS#Activo;
          INT_DTESTAD  = $DateToInt(%DATE());
          INT_PRTOTCL  = WComissao;
          INT_PRTOTPR  = WValComiss;
          INT_CDEXCEPT = WCdExcept;
          INT_CDREGIS  = WCdRegis;
          INT_IMCOMIS1 = WDifComAng;
          INT_IMCOMIS2 = WDifComCobr;
          INT_IMCOMIS3 = WDifComCorr;
          INT_DFTOCOM  = WDifTotCom;
          WRITE RINTP025;

          INT_NUSEQU   = 1;
          INT_CDESTAD  = STS#Activo;
          INT_DTESTAD  = $DateToInt(%DATE());
          INT_USESTAD  = S#USR;
          INT_CDOPER   = OPR#Criacao;
          WRITE RINTP026;
       ENDSR;

       //=================================================================
       // SubRotina..: CalcDiferencas
       // Objectivo..: Calcular o montante das diferenças nas comissões
       //=================================================================
       BEGSR CalcDiferencas;
          IF ((INT_IMCOMIS1 > *ZEROS) AND (WPrComiss1 > *ZEROS));
             WDifComAng  = INT_IMCOMIS1 - (INT_IMPRCOM * (WPrComiss1 / 100));
          ENDIF;
          IF ((INT_IMCOMIS2 > *ZEROS) AND (WPrComiss2 > *ZEROS));
             WDifComCobr = INT_IMCOMIS2 - (INT_IMPRCOM * (WPrComiss2 / 100));
          ENDIF;
          IF ((INT_IMCOMIS3 > *ZEROS) AND (WPrComiss3 > *ZEROS));
             WDifComCorr = INT_IMCOMIS3 - (INT_IMPRCOM * (WPrComiss3 / 100));
          ENDIF;
          WDifTotCom  = (INT_IMCOMIS1 + INT_IMCOMIS2 + INT_IMCOMIS3) -
                        %DECH((INT_IMPRCOM*WValComiss/100)+0.005:9:2);
       ENDSR;
      /END-FREE

     P*==================================================================
     P* Processo..: GetTaxComiss
     P* Descrição.: Obter a Taxa para o calculo da Comissão
     P*==================================================================
     P GetTaxComiss    B                   EXPORT
     D GetTaxComiss    PI             7S 4
     D   TipoComissao                 3A   CONST
     D   Posicao                      2S 0 CONST
     D   ComAngaria                   7S 4 CONST
     D   ComCobrador                  7S 4 CONST
     D   ComCorretor                  7S 4 CONST
     D
     D*==> Campos de Trabalho
     D WComissao       S              7S 4
     D*===============================================================
      /FREE
          WComissao = *ZEROS;
          IF (%SUBST(TipoComissao:Posicao:1) = '1');
             WComissao = ComAngaria;
          ELSEIF (%SUBST(TipoComissao:Posicao:1) = '2');
             WComissao = ComCobrador;
          ELSEIF (%SUBST(TipoComissao:Posicao:1) = '3');
             WComissao = ComCorretor;
          ELSEIF (%SUBST(TipoComissao:Posicao:1) = '4');
             WComissao = ComAngaria + ComCobrador;
          ELSEIF (%SUBST(TipoComissao:Posicao:1) = '5');
             WComissao = ComAngaria + ComCorretor;
          ELSEIF (%SUBST(TipoComissao:Posicao:1) = '6');
             WComissao = Comcobrador + ComCorretor;
          ELSEIF (%SUBST(TipoComissao:Posicao:1) = '7');
             WComissao = ComAngaria + ComCobrador + ComCorretor;
          ENDIF;
          RETURN WComissao;
      /END-FREE
     P GetTaxComiss    E
