     H*=====================================================================
     H* Rotina....: RCATO011
     H* Objectivo.: Validar se a apólice sofreu a inclusão ou exclusão dos
     H*             riscos de edificio ou conetudo, e se os mesmo sofreram
     H*             aumentos de Capital durante a vigência do contrato.
     H*
     H* Parâmetros Entrada:
     H*             Ramo (2,0)       = Código do Ramo
     H*             Apólice (7,0)    = Número da Apólice
     H*             DatVencim (8,0)  = Formato (YYYYMMDD)
     H*             CapRisco01 (11,0) =
     H*             CapRisco02 (11,0) =
     H*             CapRisco03 (11,0) =
     H*
     H* Parâmetros Saída:
     H*             Código        = Resultado da Análise
     H*                              =0 -> Sem Variação
     H*                              >0 -> Com Aumento Capital
     H*             Data          = Data do Código
     H*
     H*=====================================================================
     H Option(*SrcStmt: *NoDebugIO) DftActGRP(*No) BNDDIR('JMDIR')
     H*=====================================================================
     FGBASEDIO  IF   E           K DISK    EXTFILE(WFDIO) PREFIX(OR_) USROPN
     FGBASEGRO  IF   E           K DISK    PREFIX(OR_) USROPN EXTFILE(WFGRO)
     F*===================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SSECO010

     D*==> Formato de registo
     D RBASEDIO      E DS                  ExtName(GBASEDIO)
     D                                     Prefix(HS_)
     D RBASEGRO      E DS                  ExtName(GBASEDIO)
     D                                     Prefix(OR_)
     D
     D*==> Campos de Trabalho
     D WRamoApolice    S              9S 0
     D WCodRamo        S              2S 0
     D WNumApol        S              7S 0
     D WDatVencim      S              8S 0
     D WCapRisc01      S             11S 0
     D WCapRisc02      S             11S 0
     D WCapRisc03      S             11S 0
     D WCodDecimal     S              2S 0
     D WLastDatProc    S              8S 0
     D
     D WSession        S              5S 0
     D WDtAltera       S              8S 0
     D WDtVencimen     S              8S 0
     D WTrimVenc       S              2S 0
     D WAnoVenc        S              4S 0
     D WMesVenc        S              2S 0
     D WAnoMesFile     S              6S 0
     D WAnoMesVenc     S              6S 0
     D WIdx            S              3S 0
     D WNoFile         S             10A
     D WCdFile         S              5S 0
     D WNoLibl         S             10A
     D WDtCria         S             10A
     D WFDio           S             21A
     D WFGro           S             21A
     D WCapROR01       S             11S 0
     D WCapROR02       S             11S 0
     D WCapROR03       S             11S 0
     D WDatSelect      S               D
     D*=====================================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCodRamo
     C                   PARM                    WNumApol
     C                   PARM                    WDatVencim
     C                   PARM                    WCapRisc01
     C                   PARM                    WCapRisc02
     C                   PARM                    WCapRisc03
     C                   PARM                    WCodDecimal
     C                   PARM                    WLastDatProc
     C
      /FREE
        WLastDatProc = *ZEROS;
        WCodDecimal  = *ZEROS;
        WMesVenc     = %SUBDT($IntToDate(WDatVencim):*MONTHS);
        WAnoVenc     = %SUBDT($IntToDate(WDatVencim):*YEARS);
        WDatSelect   = $IntToDate(WAnoVenc * 10000 + WMesVenc * 100 + 1);
        WDatSelect  -= %MONTHS(2);
        WDatSelect  -= %YEARS(1);
        WAnoMesVenc  = %SUBDT(WDatSelect:*YEARS)*100 + WMesVenc;
        WTrimVenc    = %INT((WMesVenc - 1) / 3) + 1;

        //---------------------------------------------------------------
        // Seleccionar os registos de Histórico da apólice
        //---------------------------------------------------------------
        WSession = $SetFiles('FGRLP023 A INNER JOIN +
                   FGRLP026 B ON ((A.CDBATCH = B.CDBATCH) AND +
                                  (A.NUSEQU = B.NUSEQU))');
        $SetCriteria('(B.CDFIELD=2) AND (B.VALFIELD='''+%CHAR(WNumApol)+''') +
                      AND (A.CDFILE IN (6, 22)) AND +
                      (A.DTCRIA >= '''+%CHAR(WDatSelect)+''')');
        $SetFields('A.NOFILE,A.NOLIBL,#A.DTCRIA,A.CDFILE');
        $SetSort('A.DTCRIA #ASC');
        WIdx = 1;
        DOW ($GetRecord(WIdx) > *ZEROS);
           WNoFile = $GetField('A.NOFILE');
           WNoLibl = $GetField('A.NOLIBL');
           WCdFile = $GetFieldNum('A.CDFILE');
           WDtCria = $GetField('#A.DTCRIA');
           WFDIO   = *BLANKS;
           WFGRO   = *BLANKS;
           IF (WCdFile = 6); // Só processa o do vencimento anterior
              MONITOR;
                 WAnoMesFile = (2000 + %DEC(%SUBST(WNoFile:5:2):2:0)) * 100 +
                               %DEC(%SUBST(WNoFile:7:2):2:0);
                 IF (WAnoMesFile = WAnoMesVenc);
                 // WFGRO   = %TRIM(WNoLibl)+'/'+%TRIM(WNoFile);
                 // EXSR chkVariacao;
                 ENDIF;
              ON-ERROR;
              ENDMON;
           ELSE;
              WAnoMesFile = %INT($DateToInt(%DATE(WDtCria)) / 100);
              IF (WAnoMesFile >= WAnoMesVenc);
                 WFDio   = %TRIM(WNoLibl)+'/'+%TRIM(WNoFile);
                 EXSR chkVariacao;
              ENDIF;
           ENDIF;
           WIdx += 1;
        ENDDO;
        $RmvDBSession(WSession);

        RETURN;

        //=================================================================
        // Subrotina..: chkVariacao
        // Objectivo..: Validar aumentos e/ou inclusão de riscos
        //=================================================================
        BEGSR chkVariacao;
           IF (WFDIO <> *BLANKS);
              OPEN GBASEDIO;
              CHAIN (WCodRamo:WNumApol) GBASEDIO;
              IF (%FOUND(GBASEDIO));
                 EXSR ValCapitais;
              ENDIF;
              CLOSE GBASEDIO;
           ELSEIF (WFGRO <> *BLANKS);
              OPEN GBASEGRO;
              CHAIN (WCodRamo:WNumApol) GBASEGRO;
              IF (%FOUND(GBASEGRO));
                 EXSR ValCapitais;
              ENDIF;
              CLOSE GBASEGRO;
           ENDIF;
        ENDSR;

        //=================================================================
        // Subrotina..: ValCapitais
        // Objectivo..: Validar aumentos e/ou inclusão de riscos
        //=================================================================
        BEGSR ValCapitais;
           IF (OR_CR1 = 1) OR (OR_CR1 = 2) OR (OR_CR1 = 3);
              WCapROR01 = OR_CAP1;
           ENDIF;
           IF (OR_CR2 = 1) OR (OR_CR2 = 2) OR (OR_CR2 = 3);
              WCapROR02 = OR_CAP2;
           ENDIF;
           IF (OR_CR3 = 1) OR (OR_CR3 = 2) OR (OR_CR3 = 3);
              WCapROR03 = OR_CAP3;
           ENDIF;

           //----------------------------------------------
           // Analisar o histórico na Vigência do Contrato
           //----------------------------------------------
           IF (WCapRisc01 > WCapROR01);
              WCodDecimal = 1;
              WLastDatProc = $DateToInt(%DATE(WdtCria));
           ENDIF;

           IF (WCapRisc02 > WCapROR02);
              WCodDecimal = 1;
              WLastDatProc = $DateToInt(%DATE(WdtCria));
           ENDIF;

           IF (WCapRisc03 > WCapROR03);
              WCodDecimal = 1;
              WLastDatProc = $DateToInt(%DATE(WdtCria));
           ENDIF;

        //    IF (WCodDecimal > *ZEROS);
        //       WLastDatProc = #RegCap(WCodDecimal).DatCria / 1000000;
        //       WCodDecimal = 5; // Aumento de Capital
        //    ELSE;
        //       WCodDecimal = *ZEROS;
        //       WLastDatProc = *ZEROS;
        //    ENDIF;
        ENDSR;
      /END-FREE
