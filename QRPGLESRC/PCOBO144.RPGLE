     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PCOBO142
     H* Descrição.: Criar planos de pagamento para os Recibos
     H*
     H*==================================================================
     FFPRDP120  IF   E           K DISK    PREFIX('CAT_')
     FFCOBP141  IF   E           K DISK    PREFIX('COB_')
     FFCOBP142  IF   E           K DISK    PREFIX('COB_')
     FFCOBP14401IF   E           K DISK    PREFIX('COB_')
     F                                     RENAME(RCOBP144:RCOBP14401)
     F
     FFCOBP144  UF A E           K DISK    PREFIX('COB_')
     FFCOBP145  UF A E           K DISK    PREFIX('COB_')
     D*==================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010

     D*==> Campos de Trabalho
     D WMaCont         S              6S 0
     D WNumPlano       S              8S 0
     D WNumMovim       S              3S 0
     D WQtdeMovim      S              3S 0
     D WCodRamo        S              2S 0
     D WNumApol        S              7S 0
     D WCodSeg         S              8S 0
     D WValEstimado    S             13S 2
     D WCodErro        S              7A
     D WDatInicio      S               D
     D WDatTermo       S               D
     D WIcExistPlano   S               N
     D*==================================================================
      /FREE

       WNumPlano = *ZEROS;
       EXSR ChkPlanos;

       *INLR = *ON;
       RETURN;

       //========================================================
       // Subrotina.: ChkPlanos
       // Descrição.: Validar a existência de planos de Pagamento
       //========================================================
       BEGSR ChkPlanos;
          //----------------------------------------------------
          // Seleccionar as Apólices com Continuado previsto
          //----------------------------------------------------
          SETLL *LOVAL RCOBP142;
          READ RCOBP142;
          DOW (NOT %EOF(FCOBP142));
             IF (COB_IsToSelect = 'S');
                WMacont      = COB_MACONT;
                WQtdeMovim   = COB_PRESTACOES;
                WCodRamo     = COB_CODRAMO;
                WNumApol     = COB_NUAPOL;
                WCodSeg      = COB_CODSEG;
                WValEstimado = COB_ULTCONTI;

                //----------------------------------------------------
                // Validar se já existe plano de pagamento
                //----------------------------------------------------
                WIcExistPlano = *OFF;
                SETLL (WCodRamo:WNumApol:WMaCont) RCOBP14401;
                READE (WCodRamo:WNumApol:WMaCont) RCOBP14401;
                DOW (NOT %EOF(FCOBP14401));
                   IF (COB_CODESTADO = 0);
                      WIcExistPlano = *ON;
                      LEAVE;
                   ENDIF;
                   READE (WCodRamo:WNumApol:WMaCont) RCOBP14401;
                ENDDO;

                //----------------------------------------------------
                // Validar se já existe plano de pagamento
                //----------------------------------------------------
                IF (NOT WIcExistPlano);
                   WNumMovim = *ZEROS;
                   EXSR CalcDatas;
                   EXSR CrtPlano;
                ENDIF;
             ENDIF;
             READ RCOBP142;
          ENDDO;
       ENDSR;

       //========================================================
       // Subrotina.: CrtPlano
       // Descrição.: Criar planos de Pagamento
       //========================================================
       BEGSR CrtPlano;
          IF (WNumPlano = *ZEROS);
             WNumPlano = $GetNextKey('FCOBP144':'NUMPLANO');
          ELSE;
             WNumPlano += 1;
          ENDIF;

          //-------------------------------------------------------
          // Criar plano de Pagamentos
          //-------------------------------------------------------
          COB_NUMPLANO  = WNumPlano;
          COB_DATINICIO = WDatInicio;
          COB_DATTERMO  = WDatTermo;
          COB_CODESTADO = 0; // Emitido
          COB_DATESTADO = %DATE();
          COB_CODRAMO   = WCodRamo;
          COB_NUAPOL    = WNumApol;
          COB_CODSEG    = WCodSeg;
          COB_NUMPREST  = WQtdeMovim;
          COB_NUMRECIBO = *ZEROS;
          COB_MACONT    = WMaCont;
          COB_IMESTIMAD = WValEstimado;
          COB_IMREAL    = *ZEROS;
          COB_IMSALDO   = WValEstimado;
          COB_COB14401  = %DATE('2000-01-01');
          COB_COB14402  = %DATE('2999-12-31');
          WRITE RCOBP144;

          //-------------------------------------------------------
          // Criar Movimentos do Plano
          //-------------------------------------------------------
          FOR WNumMovim = 1 BY 1 TO WQtdeMovim;
             COB_NUMPLANO = WNumPlano;
             COB_NUMMOVIM = WNumMovim;
             COB_CODMOV   = '001';                    // Movimento estimado
             COB_DATVALOR = WDatInicio + %MONTHS((WNumMovim - 1));
             COB_DATOPERA = %DATE();
             COB_IMMOVIM  = WValEstimado / WQtdeMovim;
             COB_COB14501 = %DATE('2000-01-01');
             COB_COB14502 = %DATE('2999-12-31');
             WRITE RCOBP145;
          ENDFOR;
       ENDSR;

       //========================================================
       // Subrotina.: CalcDatas
       // Descrição.: Calacular as datas de inicio e termo
       //========================================================
       BEGSR CalcDatas;
          WDatTermo  = $IntToDate((WMacont * 100) + 1);
          WDatInicio = WDatTermo - %MONTHS((WQtdeMovim - 1));
       ENDSR;
      /END-FREE
