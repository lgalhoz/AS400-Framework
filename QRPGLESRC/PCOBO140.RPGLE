     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PCOBO140
     H* Descrição.: Actualizar Base Dados de Descontos em Vencimento,
     H*
     H*==================================================================
     FGFSEGEXT  IF   E           K DISK    PREFIX('SEG_')
     F
     FFCOBP140  UF A E           K DISK    PREFIX('COB_')
     FFCOBP141  UF A E           K DISK    PREFIX('COB_')
     D*==================================================================
      /Copy QRPGLESRC,SGRLO010

     D*==> Campos de Trabalho
     D WIdx            S              4S 0
     D WCodEmpreg      S             30A
     D WNomEmpresa     S             20A
     D WCodempresa     S              3S 0
     D WIcEmpregado    S               N
     D WIcEmpresa      S               N
     D
     D*==> Constantes
     D Char            C                   '###########################'
     D up              C                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ?'
     D lo              C                   'abcdefghijklmnopqrstuvwxyz?'
     D*==================================================================
      /FREE
       EXSR ActSegurados;

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
                      COB_IMSALDO    = *ZEROS;
                      COB_DTINICIO   = %DATE('2011-01-01');
                      COB_DTTERMO    = %DATE('9999-12-31');
                      COB_COB14101   = %DATE('2000-01-01');
                      COB_COB14102   = %DATE('9999-12-31');
                      WRITE RCOBP141;
                   ELSE;
                      COB_QTDEAPOL   = *ZEROS;
                      COB_IMSALDO    = *ZEROS;
                      COB_CDEMPREGAD = %TRIM(WCodEmpreg);
                      COB_CDEMPRESA  = WCodEmpresa;
                      UPDATE RCOBP141 %FIELDS(COB_QTDEAPOL:COB_CDEMPREGAD:
                                              COB_CDEMPRESA:COB_IMSALDO);
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
      /END-FREE
