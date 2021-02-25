     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PCOBO140
     H* Descrição.: Actualizar Base Dados de Descontos em Vencimento,
     H*
     H*==================================================================
     FGFSEGEXT  IF   E           K DISK    PREFIX('SEG_')
     FGBASEGRL  IF   E           K DISK    PREFIX('BAS_')
     F
     FFCOBP141  UF   E           K DISK    PREFIX('COB_')
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
     D WCdRamo         S              3S 0
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
       EXSR ActContratos;

       *INLR = *ON;
       RETURN;

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
