     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PPRDO104
     H* Descrição.: Parameterizar Coberturas para uma Apólices.
     H*==================================================================
     FFCATP10202IF A E           K DISK    PREFIX(CAT_)
     F                                     RENAME(RCATP102:RCATP10202)
     FFPRDP008  UF A E           K DISK    PREFIX(PRD_)
     FFPRDP00801IF   E           K DISK    PREFIX(PRD_)
     F                                     RENAME(RPRDP008:RPRDP00801)
     FFPRDE104  CF   E             WORKSTN SFILE(RPRDE10402:REC#)
     F                                     PREFIX(ECR_)
     F                                     INFDS(INFD#KEYS)
     F*==========================================================
     F* Indicadores  Descrição
     F* -----------  --------------------------------------------
     F*      03      Tecla F3
     F*      06      Tecla F6
     F*      12      Tecla F12
     F*      45      IO Condicional - Capital
     F*      46      IO Condicional - Texto
     F*      50      Tecla PagDn
     F*      51      Tecla PagUp
     F*      52      DSP
     F*      53      CLR
     F*      54      END
     D*==========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SCATO010
      /Copy QRPGLESRC,SSECO010

     D*==> Estrutura de dados do Registo
     D RBASEDIO      E DS                  ExtName(GBASEDIO)
     D                                     Prefix(OR_)
     D                                     Based(ptrOR)
     D
     D*==> Manutenção de Coberturas
     D MntCoberturas   PR
     D   PCdProd                      5S 0 CONST
     D   PDsProd                     40A   CONST
     D   PCdSuper                     5S 0 CONST
     D   PDsSuper                    40A   CONST
     D
     D*==> Validar Existência da Cobertura
     D ChkCobertura    PR             5S 0
     D   CdCobert                     5S 0 CONST
     D   CdAlias                     10A   CONST
     D   Natureza                     1A   CONST
     D
     D*==> Pré-visualização das Actas
     D  PrintPreview   PR                  EXTPGM('RPRDO005')
     D   ptrOR                         *   CONST
     D   Cdfunction                   5S 0 CONST
     D
     D*==> Estrutura de dados do Registo
     D WIcErro         S               N   INZ(*OFF)
     D WIcOrigem       S              1A
     D REC#            S              4S 0
     D
     D*==> Outros Parâmetros
     D WCodProduto     S              5S 0
     D WIcDisplay      S               N   INZ(*ON)
     D WModo           S               N
     D WNoFunction     S             10A   INZ(*BLANKS)
     D WDatInicio      S              8S 0
     D WCdTipo         S              1A   INZ(*BLANKS)
     D WCdCobert       S                   LIKE(PRD_CDCOBERT)
     D WValor          S              1A
     D WptrOr          S               *
     D WSessionID      S              5S 0 INZ(*ZEROS)
     C*========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCodProduto
     C                   PARM                    WDatInicio
     C                   PARM                    WPtrOR
     C                   PARM                    WModo
      /FREE

       //------------------------------------
       // Se não estiver autorizado então sai
       //------------------------------------
       IF NOT $ValAccess(S#PGM:'':99999);
          *INLR = *ON;
          RETURN;
       ENDIF;
       ptrOr   = WptrOR;

       MntCoberturas(WCodProduto:*BLANKS:*ZEROS:*BLANKS);

       *INLR = *ON;
       RETURN;
      /END-FREE

     P*===================================================================
     P* Processo..: MntCoberturas
     P* Descrição.: Realizar a manutenção das Coberturas
     P*===================================================================
     P MntCoberturas   B                   EXPORT
     D MntCoberturas   PI
     D   PCdProd                      5S 0 CONST
     D   PDsProd                     40A   CONST
     D   PCdSuper                     5S 0 CONST
     D   PDsSuper                    40A   CONST
     D
     C*=================================================================
      /FREE

       WSessionID = $AddDBSession();
       $SetFiles('FCATP102');
       $SetFields('CDPROD, CDCOBERT, NUORDEM, DSCOBERT, CDNATURA, ALIAS');
       $SetSort('CDNATURA #DESC, NUORDEM');
       $SetCriteria('CDPROD = ' + %CHAR(WCodProduto));
       IF (PCdSuper = *ZEROS);
          $SetCriteria('CDPROD = ''' + %CHAR(WCodProduto) + ''' +
                        AND (CDCOBERT = CDSUPER)');
       ELSE;
          $SetCriteria('CDPROD = ''' + %CHAR(WCodProduto) + ''' +
                        AND CDSUPER = ''' + %CHAR(PCdSuper) + ''' +
                        AND (CDSUPER <> CDCOBERT)');
       ENDIF;
       $SetPage(12);
       WIcDisplay = WModo; // *ON
       EXSR inzRecords;
       EXSR getRecords;

       ECR_NOMSEG  = OR_Seg;
       ECR_RAMO    = OR_RAMO;
       ECR_APOLICE = OR_Apol;
       DOW (NOT *IN03 AND NOT *IN12);
          EXSR ShowCapRisco;
          IF (WIcDisplay);
             WNoFunction = 'RPRDE10405'; // Consulta Coberturas
          ELSE;
             WNoFunction = 'RPRDE10421'; // Alterar Coberturas
          ENDIF;

          DS#HEADER    = $getHeader(S#PGM:WNoFunction);
          DS#FOOTER    = $getFooter(S#PGM:WNoFunction:*BLANKS);
          ECR_S#PGM = S#PGM;
          WRITE     RPRDE10400;
          WRITE     RPRDE10499;
          EXFMT RPRDE10403;
          IF ($ValidKey(Func#Key:KEY#F21)) AND WIcDisplay;
             //-------------------------------------------------
             // Mudar Modo IO, Para alteração
             //-------------------------------------------------
             //WIcDisplay = *OFF;
             //EXSR inzRecords;
             //EXSR getRecords;
          ELSEIF ((Func#Key = KEY#PgDown) AND (NOT $IsLastPage())) OR
             ((Func#Key = KEY#PgUp) AND ($GetPageNbr() > 1));
             EXSR inzRecords;
             EXSR getRecords;
          ELSEIF (Func#Key = KEY#F14);
             MONITOR;
                PrintPreview(WPtrOr:31);
             ON-ERROR;
             ENDMON;
          ELSEIF (Func#Key = KEY#Enter);
             EXSR SelRec;
             *IN12 = *OFF;
             EXSR inzRecords;
             EXSR getRecords;
          ENDIF;
       ENDDO;
       $RmvDBSession(WSessionID);

       *INLR = *ON;
       RETURN;

       //============================================================
       // Subrotina..: ShowCapRisco
       // Objectivo..: repor Capitais de Risco
       //============================================================
       BEGSR ShowCapRisco;
          IF (OR_CR1 > *ZEROS);
             ECR_DSRISC001 = $getDescRisco(WCodProduto:OR_CR1);
             ECR_CPRISC001 = OR_CAP1;
          ENDIF;
          IF (OR_CR2 > *ZEROS);
             ECR_DSRISC002 = $getDescRisco(WCodProduto:OR_CR2);
             ECR_CPRISC002 = OR_CAP2;
          ENDIF;
          IF (OR_CR3 > *ZEROS);
             ECR_DSRISC003 = $getDescRisco(WCodProduto:OR_CR3);
             ECR_CPRISC003 = OR_CAP3;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: InzRecTxt
       // Objectivo.: Inicializar a lista de registos
       //========================================================
       BEGSR inzRecords;
          *IN46     = *OFF;
          *IN45     = *ON;
          *IN48     = *ON;
          REC#      = 0;        // Controlo de Registos da Lista
          *In53     = *On;      // Limpar Lista de Ecrã
          *In52     = *On;      // Mostrar a Lista de Ecrã
          WRITE RPRDE10403; // Limpar Lista
          *In53     = *Off;     // Repor Indicador
          *In54     = *Off;     // Repor Indicador
       ENDSR;

       //========================================================
       // Subrotina.: GetRecords
       // Objectivo.: Obter todos os registos de tabelas disponív
       //========================================================
       BEGSR getRecords;
          WCdTipo  = *BLANKS;
          IF ($GetPage(Func#Key) > *ZEROS);
             DOW (NOT $IsEOF());
                //---------------------------------------------
                // Escreve os Cabeçalhos da Listagem
                //---------------------------------------------
                IF (REC# < 12); // BRK001
                   IF (WCdTipo <> $GetField('CDNATURA'));
                      REC# += 1;
                      WCdTipo = $GetField('CDNATURA');
                      *IN48 = *OFF;
                      *IN49 = *ON;
                      ECR_CDSELECT = *BLANKS;
                      IF (WCdTipo = 'B');
                         ECR_DSCOBERT = 'Coberturas Base';
                      ELSE;
                         ECR_DSCOBERT = 'Coberturas Facultativas';
                      ENDIF;
                      WRITE RPRDE10402;
                   ENDIF;
                ENDIF;

                //---------------------------------------------
                // Escreve os Registo da Listagem
                //---------------------------------------------
                IF (REC# < 12); // BRK001
                   REC# += 1;
                   *IN49 = *OFF;
                   *IN45 = *ON;
                   *IN48 = *ON;
                   WCdCobert    =  %DEC($GetField('CDCOBERT'):5:0);
                   ECR_ICMORE   = 'N';
                   ECR_CDCOBERT = WCdCobert;
                   ECR_DSCOBERT = $GetField('DSCOBERT');
                   ECR_DSCOBERT ='  '+%TRIM($GetField('ALIAS'))+'. '+
                                    %TRIM(ECR_DSCOBERT);
                   SETLL (PCdProd:ECR_CDCOBERT) RCATP10202;
                   READE (PCdProd:ECR_CDCOBERT) RCATP10202;
                   DOW (NOT %EOF());
                      IF (CAT_CDSUPER <> CAT_CDCOBERT);
                         ECR_DSCOBERT = '  ... '+%TRIM(ECR_DSCOBERT);
                         ECR_ICMORE   = 'S';
                         LEAVE;
                      ENDIF;
                      READE (PCdProd:ECR_CDCOBERT) RCATP10202;
                   ENDDO;
                   ECR_CDNATURA = $GetField('CDNATURA');
                   ECR_ALIAS    = $GetField('ALIAS');
                   ECR_NUSEQU   = *ZEROS;
                   ECR_CDSELECT = 'N';
                   IF (WCdTipo = 'B') OR (ECR_ICMORE = 'S');
                      *IN48        = *OFF;
                      IF (WCdTipo = 'B');
                         ECR_CDSELECT = 'S';
                      ELSE;
                         ECR_NUSEQU=ChkCobertura(WCdCobert:
                                $GetField('ALIAS'):WCdTipo);
                         IF (ECR_NUSEQU > *ZEROS);
                            *IN48        = *OFF;
                            ECR_CDSELECT = 'S';
                         ENDIF;
                      ENDIF;
                   ELSE;
                      ECR_NUSEQU=ChkCobertura(WCdCobert:
                              $GetField('ALIAS'):WCdTipo);
                      IF (ECR_NUSEQU > *ZEROS);
                         *IN48        = *OFF;
                         ECR_CDSELECT = 'S';
                      ENDIF;

                      IF (NOT WIcDisplay);
                         *IN45  = *OFF;
                      ENDIF;
                   ENDIF;
                   WRITE RPRDE10402;
                ENDIF;

                IF (REC# = 11);
                   LEAVE;
                ENDIF;
                $getNext();
             ENDDO;
          ENDIF;

          //---------------------------------------
          // Validar se ainda existem mais registos
          //---------------------------------------
          IF (NOT $IsLastPage());
             *IN54    = *OFF;
          ELSE;
             *IN54    = *ON;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: SelRec
       // Objectivo.: Seleccionar os registos
       //========================================================
       BEGSR SelRec;
          READC RPRDE10402;
          DOW (NOT %EOF());
             IF (ECR_OPTION <> *BLANKS);
                IF (ECR_ICMORE = 'N');
                   ExSr ShowText;
                ELSE;
                   MntCoberturas(WCodProduto:*BLANKS:
                                 ECR_CdCobert:*BLANKS);
                ENDIF;
             ELSE;
                IF (ECR_CDNATURA <> 'B');
                   WCdCobert    = ECR_CDCOBERT;
                   EXSR UpdCoberturas;
                ENDIF;
             ENDIF;
             READC RPRDE10402;
          ENDDO;
       ENDSR;

       //========================================================
       // Subrotina.: ShowText
       // Objectivo.: Mostrar os textos associados à Cobertura
       //========================================================
       BEGSR ShowText;
          //---------------------------------------------------
          // Formatar o texto associado à Cobertura
          //---------------------------------------------------
       // EXFMT RPRDE10407;
       ENDSR;

       //========================================================
       // Subrotina.: UpdCoberturas
       // Objectivo.: Actualizar Coberturas
       //========================================================
       BEGSR UpdCoberturas;
          IF ((ECR_NUSEQU > *ZEROS) AND (ECR_CDSELECT = 'N'));
             DELETE (OR_Ramo:OR_Apol:ECR_NUSEQU) RPRDP008;
             WValor = *BLANKS;
             EXSR UpdOldData;
          ELSEIF ((ECR_NUSEQU = *ZEROS) AND (ECR_CDSELECT = 'S'));
             SETGT (OR_Ramo:OR_Apol) RPRDP008;
             READP(N) RPRDP008;
             PRD_CDRAMO   = OR_Ramo;
             PRD_NUAPOL   = OR_Apol;
             PRD_NUSEQU   = PRD_NUSEQU + 1;
             PRD_CDCOBERT = WCdCobert;
             WRITE RPRDP008;
             WValor = 'S';
             EXSR UpdOldData;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: UpdOldData
       // Objectivo.: Actualizar Tabelas Antigas
       //========================================================
       BEGSR UpdOldData;
          IF (%TRIM(ECR_ALIAS) = '01');
             OR_CF01 = WValor;
          ELSEIF (%TRIM(ECR_ALIAS) = '02');
             OR_CF02 = WValor;
          ELSEIF (%TRIM(ECR_ALIAS) = '03');
             OR_CF03 = WValor;
          ELSEIF (%TRIM(ECR_ALIAS) = '04');
             OR_CF04 = WValor;
          ELSEIF (%TRIM(ECR_ALIAS) = '05');
             OR_CF05 = WValor;
          ELSEIF (%TRIM(ECR_ALIAS) = '06');
             OR_CF06 = WValor;
          // ELSEIF (%TRIM(ECR_ALIAS) = '07');
          //    OR_CF07 = WValor;
          ELSEIF (%TRIM(ECR_ALIAS) = '08');
             OR_CF08 = WValor;
          ELSEIF (%TRIM(ECR_ALIAS) = '09');
             OR_CF09 = WValor;
          ELSEIF (%TRIM(ECR_ALIAS) = '10');
             OR_CF10 = WValor;
          ELSEIF (%TRIM(ECR_ALIAS) = '11');
             OR_CF11 = WValor;
          ELSEIF (%TRIM(ECR_ALIAS) = '12');
             OR_CF12 = WValor;
          ELSEIF (%TRIM(ECR_ALIAS) = '13');
             OR_CF13 = WValor;
          ELSEIF (%TRIM(ECR_ALIAS) = '0800');
             OR_CF0800 = WValor;
             OR_CF07   = WValor;
          ELSEIF (%TRIM(ECR_ALIAS) = '0830');
             OR_CF0830 = WValor;
             OR_CF07   = WValor;
          ENDIF;
       ENDSR;
      /END-FREE
     PMntCoberturas    E

     P*=======================================================
     P* Processo..: ChkCobertura
     P* Objectivo.: Validar se a Cobertura está contratada para
     P*             a Apólices (trabalha com as duas versões das
     P*             Tabelas). Actualizar nova Tabela, partindo
     P*             do que encontrar na antiga.
     P*=======================================================
     P ChkCobertura    B                   EXPORT
     D                 PI             5S 0
     D   PRM_CdCobert                 5S 0 CONST
     D   PRM_Alias                   10A   CONST
     D   PRM_Natureza                 1A   CONST
     D WNuSequ         S              5S 0 INZ(*ZEROS)
      /FREE
        WNuSequ = *ZEROS;
        CHAIN (OR_Ramo:OR_Apol:PRM_CdCobert) RPRDP00801;
        IF (%FOUND());
           DELETE (OR_Ramo:OR_Apol:PRD_NUSEQU) RPRDP008;
        ENDIF;
        //   WNuSequ = PRD_NUSEQU;
        //ELSE;
           EXSR ApplyOldSett;
        //ENDIF;
        RETURN WNuSequ;

       //========================================================
       // Subrotina.: ApplyOldSett
       // Objectivo.: Aplicar configuração partindo das antigas
       //             tabelas. Descontinuar, esta rotina depois
       //             de modificar os Sistemas.
       //========================================================
       BEGSR ApplyOldSett;
          WNuSequ = *ZEROS;
          IF (OR_CF01 = 'S') AND (%TRIM(PRM_ALIAS) = '01');
             WNuSequ = 9999;
          ELSEIF (OR_CF02 = 'S') AND (%TRIM(PRM_ALIAS) = '02');
             WNuSequ = 9999;
          ELSEIF (OR_CF03 = 'S') AND (%TRIM(PRM_ALIAS) = '03');
             WNuSequ = 9999;
          ELSEIF (OR_CF04 = 'S') AND (%TRIM(PRM_ALIAS) = '04');
             WNuSequ = 9999;
          ELSEIF (OR_CF05 = 'S') AND (%TRIM(PRM_ALIAS) = '05');
             WNuSequ = 9999;
          ELSEIF (OR_CF06 = 'S') AND (%TRIM(PRM_ALIAS) = '06');
             WNuSequ = 9999;
          ELSEIF (OR_CF08 = 'S') AND (%TRIM(PRM_ALIAS) = '08');
             WNuSequ = 9999;
          ELSEIF (OR_CF09 = 'S') AND (%TRIM(PRM_ALIAS) = '09');
             WNuSequ = 9999;
          ELSEIF (OR_CF10 = 'S') AND (%TRIM(PRM_ALIAS) = '10');
             WNuSequ = 9999;
          ELSEIF (OR_CF11 = 'S') AND (%TRIM(PRM_ALIAS) = '11');
             WNuSequ = 9999;
          ELSEIF (OR_CF12 = 'S') AND (%TRIM(PRM_ALIAS) = '12');
             WNuSequ = 9999;
          ELSEIF (OR_CF13 = 'S') AND (%TRIM(PRM_ALIAS) = '13');
             WNuSequ = 9999;
          ELSEIF (OR_CF07 = 'S') AND (%TRIM(PRM_ALIAS) = '07');
             IF (OR_CF0800<>'S') AND (OR_CF0830<>'S');
                OR_CF0800 = 'S';
             ENDIF;
             WNuSequ = 9999;
          ELSEIF (OR_CF0800 = 'S') AND (%TRIM(PRM_ALIAS) = '0800');
             WNuSequ = 9999;
          ELSEIF (OR_CF0830 = 'S') AND (%TRIM(PRM_ALIAS) = '0830');
             WNuSequ = 9999;
          ENDIF;

          IF (WNuSequ = 9999);
             SETGT (OR_Ramo:OR_Apol) RPRDP008;
             READP(N) RPRDP008;
             IF (PRD_CDRAMO <> OR_RAMO) OR (PRD_NUAPOL <> OR_APOL);
                PRD_NUSEQU = *ZEROS;
             ENDIF;
             PRD_NUSEQU   = PRD_NUSEQU + 1;
             PRD_CDRAMO   = OR_Ramo;
             PRD_NUAPOL   = OR_Apol;
             PRD_CDCOBERT = PRM_CdCobert;
             WRITE RPRDP008;
          ENDIF;
       ENDSR;
      /END-FREE
     P ChkCobertura    E
