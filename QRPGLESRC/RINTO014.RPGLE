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
     D   ComAngar                     7S 5 CONST
     D   ComCobra                     7S 5 CONST
     D   ComCorret                    7S 5 CONST
     D
     D AddExcepcao     PR             7A
     D   CodCompanhia                 3S 0 CONST
     D   TipExcepcao                  1A   CONST
     D   CodRegisto                   9S 0 CONST
     D   Comissao                     7S 5 CONST OPTIONS(*NOPASS)
     D   ValComiss                    7S 5 CONST OPTIONS(*NOPASS)
     D   DifComAng                    9S 2 CONST OPTIONS(*NOPASS)
     D   DifComCobr                   9S 2 CONST OPTIONS(*NOPASS)
     D   DifComCorr                   9S 2 CONST OPTIONS(*NOPASS)
     D   DifTotCom                    9S 2 CONST OPTIONS(*NOPASS)
     D
     D DelExcepcao     PR             7A
     D   CodCompanhia                 3S 0 CONST
     D   TipExcepcao                  1A   CONST
     D   CodRegisto                   9S 0 CONST
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
     D*==> Campos de Trabalho
     D WCdRegis        S              9S 0
     D WCodErro        S              7A
     D WCodCompanhia   S              3S 0
     D WNuApolice      S             10A
     D
     D WPerComAng      S              7S 5
     D WPerComCobr     S              7S 5
     D WPerComCorr     S              7S 5
     D WDifComAng      S              9S 2
     D WDifComCobr     S              9S 2
     D WDifComCorr     S              9S 2
     D WDifTotCom      S              9S 2
     D WPrComiss1      S              7S 5
     D WPrComiss2      S              7S 5
     D WPrComiss3      S              7S 5
     D
     D WValComiss      S              7S 5
     D WDbSession      S              5S 0
     D WTotComissao    S              9S 2
     D WComissao       S              7S 5
     D WNumSeg         S              8S 0
     D WCodRamo        S              3S 0
     D WCodFile        S              9S 0
     D WNumOrdem       S              8S 0
     D WConvRamo       S              3S 0
     D WIsArquivo      S              1A
     D WComRelacao     S               N
     D WIcEncontrou    S               N
     D WEspacos        S             10A   INZ(*BLANKS)
     D WDim            S              3S 0
     D
     D*==> Constantes
     D EXC#Apolice     C                   'A'
     D EXC#Comissao    C                   'E'
     D KTE#TOLERA      C                   0,05
     D MAX#Exception   C                   25
     D STS#Activo      C                   'A'
     D STS#Inactivo    C                   'D'
     D OPR#Criacao     C                   'C'
     D ACT#Criar       C                   'C'
     D ACT#Eliminar    C                   'E'
     D REC#Indemniza   C                   'I'
     D*===========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCdRegis
     C                   PARM                    WCodErro
     C
      /FREE
        WCodErro  = *BLANKS;
        //IF (S#USR <> 'LGALHOZ');
        //   WCodErro = 'XXXXXXXX';
        //   RETURN;
        //ENDIF;

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
                 IF (WCodErro = *BLANKS);
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
                    INT_CDESTAD = '2'; // Associação com a Apólice
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
                    DelExcepcao(WCodCompanhia:EXC#Apolice:WCdRegis);

                    //-----------------------------------------------
                    // Identificar diferenças nas Comissões
                    //-----------------------------------------------
                    IF (INT_TPRECIB <> REC#Indemniza);
                       EXSR ValComissao;
                    ENDIF;
                 ENDIF;
              ELSE;
                 //-----------------------------------------------
                 // Criar registo de Anomalias para a Apólice
                 //-----------------------------------------------
                 AddExcepcao(WCodCompanhia:EXC#Apolice:WCdRegis);
              ENDIF;
           ENDIF;
        ENDIF;

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
             WComRelacao   = *ON;
          ELSE;
             WConvRamo     = *ZEROS;
             WCodCompanhia = INT_CCOMP;
             WNuApolice    = INT_NUAPOLOR;
             WComRelacao   = *OFF;
          ENDIF;

          WCodErro   = *BLANKS;
          WPrComiss1 = *ZEROS;
          WPrComiss2 = *ZEROS;
          WPrComiss3 = *ZEROS;
          WValComiss  = *ZEROS;

          //-------------------------------------------
          // Procurar nas Apólices Activas e Anuladas
          //-------------------------------------------
          WDim = 10;
          WIsArquivo = 'N';
          WIcEncontrou = *OFF;
          DOW (WDim > 1) AND (NOT WIcEncontrou);
             APOLI = WNuApolice;
             CCOMP = WCodCompanhia;
             CRAMO = WConvRamo;
             EXSR FindApolAct;
             IF (Not WIcEncontrou);
                EXSR FindApolAnl;
             ENDIF;

             IF (Not WIcEncontrou);
                WDim = %LEN(%TRIM(WNuApolice)) - 1;
                WNuApolice = %SUBST(WEspacos:1:(10-WDim)) +
                          %SUBST(%TRIM(WNuApolice):2:WDim);
             ENDIF;
          ENDDO;

          //----------------------------------------
          // Actualizar informação do Campo
          //----------------------------------------
          IF (NOT WIcEncontrou);
             WCodErro = 'INT0002'; // Não localizou a Apólice
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
          ENDIF;
       ENDSR;

       //=================================================================
       // SubRotina..: FindApolAct
       // Objectivo..: Localizar nas Apólices Activas
       //=================================================================
       BEGSR FindApolAct;
          WIsArquivo = 'N';
          SETLL AP_KEY SD1AP1;
          READ  SD1AP1 R#AP1;
          DOW (NOT %EOF(SD1AP1));
             IF (APOLI = AP_APOLI) AND (AP_CCOMP = WCodCompanhia) AND
               ((WConvRamo = *ZEROS) OR (WConvRamo = AP_CRAMO));
                EXSR ValidApolice;
                WIcEncontrou = *ON;
             ELSE;
                LEAVE;
             ENDIF;
             READ  SD1AP1 R#AP1;
          ENDDO;
       ENDSR;

       //=================================================================
       // SubRotina..: FindApolAnl
       // Objectivo..: Localizar nas Apólices Anuladas
       //=================================================================
       BEGSR FindApolAnl;
          WIsArquivo = 'S';
          SETLL AP_KEY SD1AP2;
          READ  SD1AP2 R#AP1;
          DOW (NOT %EOF(SD1AP2));
             IF (APOLI = AP_APOLI) AND (CCOMP = WCodCompanhia) AND
               ((WConvRamo = *ZEROS) OR (WConvRamo = AP_CRAMO));
                EXSR ValidApolice;
                WIcEncontrou = *ON;
             ELSE;
                LEAVE;
             ENDIF;
             READ  SD1AP2 R#AP1;
          ENDDO;
       ENDSR;

       //=================================================================
       // SubRotina..: ValidApolice
       // Objectivo..: Validar correspondência com a Apólice
       //=================================================================
       BEGSR ValidApolice;
          IF (WIcEncontrou);
             WCodErro = 'INT0003'; // Existe mais do que um registo para a Apólice
             LEAVESR;
          ENDIF;

          EXSR GetComissao;
          WCodErro = *BLANKS;
          WNumOrdem = AP_NORD;
          WCodRamo = AP_CRAMO;
          WNumSeg  = AP_CSEG;
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
          WComissao    = %DECH((WTotComissao / INT_IMPRCOM * 100):7:5);
          EXSR CalcDiferencas;
          IF (WDifTotCom > KTE#TOLERA) OR ((WDifTotCom * -1) > KTE#TOLERA);
              AddExcepcao(WCodCompanhia:EXC#Comissao:WCdRegis:WComissao:
                          WValComiss:WDifComAng:WDifComCobr:WDifComCorr:
                          WDifTotCom);
          ENDIF;
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
     P* Processo..: AddExcepcao
     P* Descrição.: Adicionar um novo registo de Excepção para a Apólice
     P*==================================================================
     P AddExcepcao     B                   EXPORT
     D AddExcepcao     PI             7A
     D   CodCompanhia                 3S 0 CONST
     D   TipExcepcao                  1A   CONST
     D   CodRegisto                   9S 0 CONST
     D   Comissao                     7S 5 CONST OPTIONS(*NOPASS)
     D   ValComiss                    7S 5 CONST OPTIONS(*NOPASS)
     D   DifComAng                    9S 2 CONST OPTIONS(*NOPASS)
     D   DifComCobr                   9S 2 CONST OPTIONS(*NOPASS)
     D   DifComCorr                   9S 2 CONST OPTIONS(*NOPASS)
     D   DifTotCom                    9S 2 CONST OPTIONS(*NOPASS)
     D
     D*==> Campos de Trabalho
     D WCdExcept       S              9S 0
     D WCodErro        S              7A
     D WIcExiste       S               N
     D WDBSession      S              5S 0
     D WComissao       S              7S 5
     D WValComiss      S              7S 5
     D WDifComAng      S              9S 2
     D WDifComCobr     S              9S 2
     D WDifComCorr     S              9S 2
     D WDifTotCom      S              9S 2
     D*========================================================================
      /FREE
       //------------------------------------------------------------
       // ForCompr Parametros Recebidos com valores por defeito
       //------------------------------------------------------------
       WComissao   = *ZEROS;
       WValComiss  = *ZEROS;
       WDifComAng  = *ZEROS;
       WDifComCobr = *ZEROS;
       WDifComCorr = *ZEROS;
       WDifTotCom  = *ZEROS;
       IF (%PARMS() > 3);
          WComissao = Comissao;
          IF (%PARMS() > 4);
             WValComiss = ValComiss;
             IF (%PARMS() > 5);
                WDifComAng = DifComAng;
                IF (%PARMS() > 6);
                   WDifComCobr = DifComCobr;
                   IF (%PARMS() > 7);
                      WDifComCorr = DifComCorr;
                      IF (%PARMS() > 8);
                         WDifTotCom = DifTotCom;
                      ENDIF;
                   ENDIF;
                ENDIF;
             ENDIF;
          ENDIF;
       ENDIF;

       //------------------------------------------------------------
       // Validar existência do Registo
       //------------------------------------------------------------
       WCodErro  = *BLANKS;
       WIcExiste = *OFF;
       WDbSession = $SetFiles('FINTP023 A INNER JOIN FINTP025 B +
                        ON (A.CDEXCEPT = B.CDEXCEPT)');
       $SetFields('A.CDEXCEPT, B.CDREGIS');
       $SetCriteria('(A.TPEXCEPT = '''+ %TRIM(TipExcepcao) +''') AND +
                     (A.CCOMP  = ''' + %CHAR(CodCompanhia) + ''') AND +
                     (B.CDREGIS = ''' + %CHAR(CodRegisto) + ''') AND +
                     (B.CDESTAD = ''' + STS#Activo + ''')');
       IF ($GetRecord() > *ZEROS);
          WIcExiste = *ON;
       ENDIF;
       $RmvDBSession(WDBSession);
       IF (WIcExiste);
          RETURN WCodErro;
       ENDIF;

       //------------------------------------------------------------
       // Actualizar Registo Controlo de Excepções
       //------------------------------------------------------------
       CHAIN (CodCompanhia:TipExcepcao:STS#Activo) RINTP02301;
       IF (NOT %FOUND(FINTP02301));
          WCdExcept = $GetNextKey('FINTP023':'CDEXCEPT');
          INT_CDEXCEPT = WCdExcept;
          INT_CCOMP    = CodCompanhia;
          INT_TPEXCEPT = TipExcepcao;
          INT_CDESTADO = STS#Activo;
          INT_CDERRO   = *BLANKS;
          INT_DTEXCEP  = $DateToInt(%DATE());
          INT_QTREGIS  = 1;
          WRITE RINTP023;

          IF (TipExcepcao = EXC#Comissao);
             INT_TPCALC   = *BLANKS;
             INT_IMCOMIS1 = WDifComAng;
             INT_IMCOMIS2 = WDifComCobr;
             INT_IMCOMIS3 = WDifComCorr;
             INT_DFTOCOM  = WDifTotCom;
             INT_NURECIBO = *ZEROS;
             INT_DTREGULA = %DATE('0001-01-01');
             INT_USRREGUL = *ZEROS;
             WRITE RINTP024;
          ENDIF;
       ELSE;
          WCdExcept    = INT_CDEXCEPT;
          CHAIN (WCdExcept) RINTP023;
          INT_QTREGIS += 1;
          UPDATE RINTP023 %FIELDS(INT_QTREGIS);

          IF (TipExcepcao = EXC#Comissao);
             CHAIN (WCdExcept) RINTP024;
             INT_IMCOMIS1 += WDifComAng;
             INT_IMCOMIS2 += WDifComCobr;
             INT_IMCOMIS3 += WDifComCorr;
             INT_DFTOCOM  += WDifTotCom;
             UPDATE RINTP024 %FIELDS(INT_DFTOCOM:INT_IMCOMIS1:INT_IMCOMIS2:
                                     INT_IMCOMIS3);
          ENDIF;
       ENDIF;

       //------------------------------------------------------------
       // Actualizar Registo Detalhe da Excepção
       //------------------------------------------------------------
       INT_CDEXCEPT = WCdExcept;
       INT_CDREGIS  = CodRegisto;
       INT_CDESTAD  = STS#Activo;
       INT_DTESTAD  = $DateToInt(%DATE());
       INT_PRTOTCL  = WComissao;
       INT_PRTOTPR  = WValComiss;
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

       RETURN WCodErro;
      /END-FREE
     P AddExcepcao     E

     P*==================================================================
     P* Processo..: DelExcepcao
     P* Descrição.: Eliminar um registo de Excepcao
     P*==================================================================
     P DelExcepcao     B                   EXPORT
     D DelExcepcao     PI             7A
     D   CodCompanhia                 3S 0 CONST
     D   TipExcepcao                  1A   CONST
     D   CodRegisto                   9S 0 CONST
     D
     D*==> Campos de Trabalho
     D WIMCOMIS1       S                   LIKE(INT_IMCOMIS1)
     D WIMCOMIS2       S                   LIKE(INT_IMCOMIS2)
     D WIMCOMIS3       S                   LIKE(INT_IMCOMIS3)
     D
     D WCdExcept       S              9S 0
     D WCodErro        S              7A
     D WIcExiste       S               N
     D WDBSession      S              5S 0
     D*==================================================================
      /FREE
       //------------------------------------------------------------
       // Validar existência do Registo
       //------------------------------------------------------------
       WCodErro  = *BLANKS;
       WIcExiste = *OFF;
       WDbSession = $SetFiles('FINTP023 A INNER JOIN FINTP025 B +
                     ON (A.CDEXCEPT = B.CDEXCEPT)');
       $SetFields('A.CDEXCEPT, B.CDREGIS');
       $SetCriteria('(A.TPEXCEPT = '''+ %TRIM(TipExcepcao) +''') AND +
                     (A.CCOMP = ''' + %CHAR(CodCompanhia) + ''') AND +
                     (B.CDREGIS  = ''' + %CHAR(CodRegisto) + ''') AND +
                     (B.CDESTAD = ''' + STS#Activo + ''')');
       IF ($GetRecord() > *ZEROS);
          WCdExcept = $GetFieldNum('A.CDEXCEPT');
          WIcExiste = *ON;
       ENDIF;
       $RmvDBSession(WDBSession);
       IF (NOT WIcExiste);
          RETURN WCodErro;
       ENDIF;

       //-----------------------------------------------------------
       // Actualizar o Controlo do registo
       //-----------------------------------------------------------
       CHAIN (WCdExcept) RINTP023;
       IF (%FOUND(FINTP023));
          INT_QTREGIS -= 1;
          UPDATE RINTP023 %FIELDS(INT_QTREGIS);
       ENDIF;

       //------------------------------------------------------------
       // Actualizar Registo Detalhe da Excepção
       //------------------------------------------------------------
       WIMCOMIS1 = INT_IMCOMIS1;
       WIMCOMIS2 = INT_IMCOMIS2;
       WIMCOMIS3 = INT_IMCOMIS3;
       CHAIN (WCdExcept:CodRegisto) RINTP025;
       IF (%FOUND(FINTP025));
          INT_CDESTAD  = STS#Inactivo;
          INT_DTESTAD  = $DateToInt(%DATE());
          UPDATE RINTP025 %FIELDS(INT_CDESTAD:INT_DTESTAD);

          INT_NUSEQU = $GetNextKey('FINTP026':'NUSEQU':
                       '(CDEXCEPT = ''' + %CHAR(WCdExcept) + ''') AND +
                        (CDREGIS  = ''' + %CHAR(CodRegisto) + ''')');
          INT_CDESTAD = STS#Inactivo;
          INT_DTESTAD = $DateToInt(%DATE());
          INT_USESTAD = S#USR;
          INT_CDOPER  = OPR#Criacao;
          WRITE RINTP026;
       ENDIF;
       INT_IMCOMIS1 = WIMCOMIS1;
       INT_IMCOMIS2 = WIMCOMIS2;
       INT_IMCOMIS3 = WIMCOMIS3;
       RETURN WCodErro;

      /END-FREE
     P DelExcepcao     E

     P*==================================================================
     P* Processo..: GetTaxComiss
     P* Descrição.: Obter a Taxa para o calculo da Comissão
     P*==================================================================
     P GetTaxComiss    B                   EXPORT
     D GetTaxComiss    PI             7S 4
     D   TipoComissao                 3A   CONST
     D   Posicao                      2S 0 CONST
     D   ComAngaria                   7S 5 CONST
     D   ComCobrador                  7S 5 CONST
     D   ComCorretor                  7S 5 CONST
     D
     D*==> Campos de Trabalho
     D WComissao       S              7S 5
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
