     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PCOBO140
     H* Descrição.: Actualizar Base Dados de Descontos em Vencimento,
     H*
     H*==================================================================
     FSD1RCA    IF   F  326     6AIDISK    KEYLOC(5) EXTFILE('SD.1.RCA')
     FGFSEGEXT  IF   E           K DISK    PREFIX('SEG_')
     FGBASEGRL  IF   E           K DISK    PREFIX('BAS_')
     F
     FFCOBP140  UF A E           K DISK    PREFIX('COB_')
     FFCOBP141  UF A E           K DISK    PREFIX('COB_')
     FFCOBP142  UF A E           K DISK    PREFIX('COB_')
     D*==================================================================
      /Copy QRPGLESRC,SGRLO010

     D*==> Estrutra de dados para conversão de datas
     D CVT#MACONT      DS                  QUALIFIED
     D   MACONT                1      4S 0
     D   Mes                   1      2S 0
     D   Ano                   3      4S 0
     D
     D*==> Campos de Trabalho
     D WIdx            S              4S 0
     D WCodEmpreg      S             30A
     D WNomEmpresa     S             20A
     D WCodempresa     S              3S 0
     D WIcEmpregado    S               N
     D WIcEmpresa      S               N
     D WCdRamo         S              2S 0
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
       EXSR ActSegurados;
       EXSR ActContratos;

       *INLR = *ON;
       RETURN;

       //========================================================
       // Subrotina.: ActSegurados
       // Descrição.: Actualizar informação de Novos Segurados
       //             com descontos em Vencimento, ou então
       //             desactivar Segurados.
       //========================================================
       BEGSR ActSegurados;
          SETLL *LOVAL GFSEGEXT;
          READ GFSEGEXT;
          DOW (NOT %EOF(GFSEGEXT));
             IF (SEG_ICDVEN = 'S');
                EXSR GetCodEmpresa;
                IF (WCodEmpreg <> *BLANKS);
                   CHAIN (SEG_NSEG) RCOBP141;
                   IF (NOT %FOUND(FCOBP141));
                      COB_CODSEG     = SEG_NSEG;
                      COB_CDEMPREGAD = %TRIM(WCodEmpreg);
                      COB_CDEMPRESA  = WCodEmpresa;
                      COB_ISTOSELECT = 'S';
                      COB_QTDEAPOL   = *ZEROS;
                      COB_DTINICIO   = %DATE('2011-01-01');
                      COB_DTTERMO    = %DATE('9999-12-31');
                      COB_COB14101   = %DATE('2000-01-01');
                      COB_COB14102   = %DATE('9999-12-31');
                      WRITE RCOBP141;
                   ELSE;
                      COB_QTDEAPOL   = *ZEROS;
                      COB_CDEMPREGAD = %TRIM(WCodEmpreg);
                      COB_CDEMPRESA  = WCodEmpresa;
                      UPDATE RCOBP141 %FIELDS(COB_QTDEAPOL:COB_CDEMPREGAD:
                                              COB_CDEMPRESA);
                   ENDIF;
                ENDIF;
             ENDIF;
             READ GFSEGEXT;
          ENDDO;
       ENDSR;

       //========================================================
       // Subrotina.: GetCodEmpresa
       // Descrição.: Obter o Código da Empresa e do Empregado
       //========================================================
       BEGSR GetCodEmpresa;
          WIcEmpregado= *ON;
          WIcEmpresa  = *OFF;
          WCodEmpreg  = *BLANKS;
          WNomEmpresa = *BLANKS;
          WCodEmpresa = *ZEROS;
          SEG_CODEMP  =  %XLATE(lo:up:SEG_CODEMP);
          FOR wIdx = 1 BY 1 TO %LEN(SEG_CODEMP);
             IF (WIcEmpregado AND ((%SUBST(SEG_CODEMP: WIdx: 1) = ' ')
               OR (%SUBST(SEG_CODEMP: WIdx: 1) = '-')));
                WIcEmpregado = *OFF;
                WIcEmpresa = *ON;
             ELSEIF (WIcEmpregado);
                WCodEmpreg = %TRIM(WCodEmpreg) + %SUBST(SEG_CODEMP: WIdx: 1);
             ELSEIF (WIcEmpresa AND (%SUBST(SEG_CODEMP: WIdx: 1) <> ' ')
               AND (%SUBST(SEG_CODEMP: WIdx: 1) <> '-'));
                IF (%XLATE(up:Char:%SUBST(SEG_CODEMP: WIdx: 1)) = '#');
                   WNomEmpresa = %TRIM(WNomEmpresa)+%SUBST(SEG_CODEMP:WIdx:1);
                ENDIF;
             ENDIF;
          ENDFOR;
          IF (WNomEmpresa = *BLANKS);
             WNomEmpresa = '??????';
          ENDIF;
          EXSR AddEmpresa;
       ENDSR;

       //========================================================
       // Subrotina.: AddEmpresa
       // Descrição.: Adicionar o registo da Empresa
       //========================================================
       BEGSR AddEmpresa;
          WCodEmpresa = -1;
          SETLL *LOVAL RCOBP140;
          READ(N) RCOBP140;
          DOW (NOT %EOF(FCOBP140));
             IF (%TRIM(COB_ALIAS) = %TRIM(WNomEmpresa));
                WCodEmpresa = COB_CDEMPRESA;
                LEAVE;
             ENDIF;
             WCodEmpresa = WCodEmpresa - 1;
             READ(N) RCOBP140;
          ENDDO;
          IF (WCodEmpresa < *ZEROS);
             WCodEmpresa    = WCodEmpresa * -1;
             COB_CDEMPRESA  = WCodEmpresa;
             COB_CODIGO     = %TRIM(WNomEmpresa);
             COB_ALIAS      = %TRIM(WNomEmpresa);
             COB_DTINICIO   = %DATE('2011-01-01');
             COB_DTTERMO    = %DATE('9999-12-31');
             COB_COB14001   = %DATE('2000-01-01');
             COB_COB14002   = %DATE('9999-12-31');
             WRITE RCOBP140;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: ActContratos
       // Descrição.: Actualizar informação de Novos Contratos
       //             do Segurado.
       //========================================================
       BEGSR ActContratos;
          SETLL *LOVAL GBASEGRL;
          READ GBASEGRL;
          DOW (NOT %EOF(GBASEGRL));
             CHAIN (BAS_NSEG) RCOBP141;
             IF (%FOUND(FCOBP141));
             IF (COB_ISTOSELECT = 'S');
                EXSR FmtFields;
                CHAIN (BAS_RAMO:BAS_APOL) RCOBP142;
                IF (NOT %FOUND(FCOBP142));
                   IF (BAS_SIT <> 3);
                      COB_CODRAMO   = BAS_RAMO;
                      COB_NUAPOL    = BAS_APOL;

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
                      COB_ISTOSELECT= 'S';
                      COB_PRESTACOES= 2;

                      COB_COB14201  = %DATE('2000-01-01');
                      COB_COB14202  = %DATE('9999-12-31');
                      WRITE RCOBP142;
                   ENDIF;
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
                   COB_ULTCONTI  = WPremTotal;
                   UPDATE RCOBP142 %FIELDS(COB_CODSEG:COB_CODSTAT:COB_DATINICIO
                                           :COB_DATTERMO:COB_MACONT:COB_FRACCIO
                                           :COB_OBJECTO:COB_INIPREV
                                           :COB_ULTCONTI:COB_QTDEREC);
                ENDIF;
                IF (BAS_SIT <> 3) AND (COB_ISTOSELECT = 'S');
                   COB_QTDEAPOL += 1;
                   UPDATE RCOBP141 %FIELDS(COB_QTDEAPOL);
                ENDIF;
             ENDIF;
             ENDIF;
             READ GBASEGRL;
          ENDDO;
       ENDSR;

       //========================================================
       // Subrotina.: FmtFields
       // Descrição.: Fomatar com valores os diferentes campos
       //             do Contrato.
       //========================================================
       BEGSR FmtFields;
          WDatInicio = $IntToDate($CvtData(BAS_DTINI:'DMY':'YYMD'));
          WDatTermo  = %DATE('9999-12-31'); // Formatar Termo
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
