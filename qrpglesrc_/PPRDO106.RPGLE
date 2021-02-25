     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PPRDO104
     H* Descrição.: Parameterizar Coberturas para uma Apólices.
     H*==================================================================
     FFPRDP009  UF A E           K DISK    PREFIX(PRD_)
     FFPRDP00901IF   E           K DISK    PREFIX(PRD_)
     F                                     RENAME(RPRDP009:RPRDP00901)
     FFPRDE106  CF   E             WORKSTN SFILE(RPRDE10602:REC#)
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

     D*==> Validar Existência da Clausula
     D ChkClausula     PR             5S 0
     D   CdItem                       3A   CONST
     D   CdAlias                     10A   CONST
     D   Natureza                     1A   CONST
     D
     D*==> Estrutura de dados do Registo
     D RBASEDIO      E DS                  ExtName(GBASEDIO)
     D                                     Prefix(OR_)
     D                                     Based(ptrOR)
     D
     D*==> Pré-visualização das Actas
     D  PrintPreview   PR                  EXTPGM('RPRDO005')
     D   ptrOR                         *   CONST
     D   Cdfunction                   5S 0 CONST
     D
     D WIcErro         S               N   INZ(*OFF)
     D WIcOrigem       S              1A
     D REC#            S              4S 0
     D
     D WIdxRisco       S              5S 0
     D WIdx            S              5S 0
     D WIcExtract      S               N
     D WExceptKeys     S             24A   INZ(*BLANKS)
     D
     D*==> Parâmetros recebidos
     D WCodProduto     S              5S 0
     D WpLstRisco      S               *
     D
     D*==> Outros Parâmetros
     D WListEnd        S               N
     D WIcDisplay      S               N   INZ(*ON)
     D WModo           S               N
     D WNoFunction     S             10A   INZ(*BLANKS)
     D WCdItem         S                   LIKE(PRD_CDITEM)
     D WTpItem         S              1A
     D WDatInicio      S              8S 0
     D WValor          S              1A
     D WPtrOR          S               *
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

       ptrOr       = WptrOR;

       ECR_NOMSEG  = OR_SEG;
       ECR_RAMO    = OR_RAMO;
       ECR_APOLICE = OR_Apol;

       WSessionID = $AddDBSession();
       $SetFiles('FCATP108');
       $SetFields('CDPROD, TPITEM, NUORDEM, CDITEM, DSITEM, ALIAS');
       $SetSort('TPITEM #DESC, NUORDEM');
       $SetCriteria('CDPROD = ' + %CHAR(WCodProduto));
       $SetPage(12);
       WIcDisplay = WModo; // *ON
       EXSR inzRecords;
       EXSR getRecords;

       DOW (NOT *IN03 AND NOT *IN12);
          EXSR ShowCapRisco;
          IF (WIcDisplay);
             WNoFunction = 'RPRDE10605'; // Consulta Coberturas
          ELSE;
             WNoFunction = 'RPRDE10621'; // Alterar Coberturas
          ENDIF;

          DS#HEADER    = $getHeader(S#PGM:WNoFunction);
          DS#FOOTER    = $getFooter(S#PGM:WNoFunction:*BLANKS);
          ECR_S#PGM = S#PGM;
          WRITE     RPRDE10600;
          WRITE     RPRDE10699;
          EXFMT RPRDE10603;
          IF ($ValidKey(Func#Key:KEY#F21)) AND WIcDisplay;
             //-------------------------------------------------
             // Mudar Modo IO, Para alteração
             //-------------------------------------------------
             //WIcDisplay = *OFF;
             //WIcMore    = *OFF;
             //EXSR inzRecords;
             //EXSR getRecords;
          ELSEIF ((Func#Key = KEY#PgDown) AND (NOT $IsLastPage())) OR
             ((Func#Key = KEY#PgUp) AND ($GetPageNbr() > 1));
             EXSR inzRecords;
             EXSR getRecords;
          ELSEIF (Func#Key = KEY#F14);
             MONITOR;
                PrintPreview(WPtrOr:29);
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
         ELSE;
            ECR_DSRISC001 = *BLANKS;
            ECR_CPRISC001 = *ZEROS;
         ENDIF;
         IF (OR_CR2 > *ZEROS);
            ECR_DSRISC002 = $getDescRisco(WCodProduto:OR_CR2);
            ECR_CPRISC002 = OR_CAP2;
         ELSE;
            ECR_DSRISC002 = *BLANKS;
            ECR_CPRISC002 = *ZEROS;
         ENDIF;
         IF (OR_CR3 > *ZEROS);
            ECR_DSRISC003 = $getDescRisco(WCodProduto:OR_CR3);
            ECR_CPRISC003 = OR_CAP3;
         ELSE;
            ECR_DSRISC003 = *BLANKS;
            ECR_CPRISC003 = *ZEROS;
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
          WRITE RPRDE10603;     // Limpar Lista
          *In53     = *Off;     // Repor Indicador
       ENDSR;

       //========================================================
       // Subrotina.: GetRecords
       // Objectivo.: Obter todos os registos de tabelas disponív
       //========================================================
       BEGSR getRecords;
          WTpItem  = *BLANKS;
          IF ($GetPage(Func#Key) > *ZEROS);
             DOW (NOT $IsEOF());
                *IN45  = *ON;
                *IN48  = *ON;
                IF ($GetField('TPITEM') <> WTpItem);
                   *IN49  = *ON;
                   REC# += 1;
                   ECR_CDSELECT = *BLANKS;
                   WTpItem = $GetField('TPITEM');
                   ECR_DSTIPO = $getDescricao(25:WTpItem);
                   WRITE RPRDE10602;
                ENDIF;
                *IN49  = *OFF;
                REC# += 1;
                ECR_NUSEQU   = *ZEROS;
                ECR_CDSELECT = 'N';
                ECR_TPITEM = $GetField('TPITEM');
                ECR_ALIAS  = $GetField('ALIAS');
                ECR_CDITEM = $GetField('CDITEM');
                ECR_DSTIPO = '  '+%TRIM(ECR_CDITEM)+
                                     ' = '+$GetField('DSITEM');
                WCdItem    = $GetField('CDITEM');
                ECR_NUSEQU = ChkClausula(WCdItem:
                                     %TRIM($GetField('ALIAS')):WTpItem);
                IF (ECR_NUSEQU > *ZEROS);
                   *IN48        = *OFF;
                   ECR_CDSELECT = 'S';
                ENDIF;

                IF (NOT WIcDisplay);
                   *IN45  = *OFF;
                ENDIF;
                WRITE RPRDE10602;
                IF (REC# = 12);
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
          READC RPRDE10602;
          DOW (NOT %EOF());
             IF (ECR_OPTION <> *BLANKS);
                ECR_DSSELITEM = $getDescricao(25:ECR_TPITEM);
                EXFMT RPRDE10607;
             ELSE;
                EXSR UpdClausula;
             ENDIF;
             READC RPRDE10602;
          ENDDO;
       ENDSR;

       //========================================================
       // Subrotina.: UpdClausula
       // Objectivo.: Actualizar Clausulas e Condições
       //========================================================
       BEGSR UpdClausula;
          IF ((ECR_NUSEQU > *ZEROS) AND (ECR_CDSELECT = 'N'));
             DELETE (OR_Ramo:OR_Apol:ECR_NUSEQU) RPRDP009;
             WValor = *BLANKS;
             EXSR UpdOldData;
          ELSEIF ((ECR_NUSEQU = *ZEROS) AND (ECR_CDSELECT = 'S'));
             SETGT (OR_Ramo:OR_Apol) RPRDP009;
             READP(N) RPRDP009;
             PRD_CDRAMO   = OR_Ramo;
             PRD_NUAPOL   = OR_Apol;
             PRD_NUSEQU   = PRD_NUSEQU + 1;
             PRD_CDITEM   = ECR_CDITEM;
             WRITE RPRDP009;
             WValor = 'S';
             EXSR UpdOldData;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: UpdOldData
       // Objectivo.: Actualizar Tabelas Antigas
       //========================================================
       BEGSR UpdOldData;
          IF (ECR_TPITEM = 'E'); // Condições Especiais
             IF (ECR_ALIAS = '001');
                OR_CE001 = WValor;
             ELSEIF (ECR_ALIAS = '002');
                OR_CE002 = WValor;
             ELSEIF (ECR_ALIAS = '003');
                OR_CE003 = WValor;
             ELSEIF (ECR_ALIAS = '004');
                OR_CE004 = WValor;
             ENDIF;
          ELSEIF (ECR_TPITEM = 'A'); // Cláusulas Aplicáveis
             IF (ECR_ALIAS = '030');
                OR_CA030 = WValor;
             ELSEIF (ECR_ALIAS = '031');
                OR_CA031 = WValor;
             ELSEIF (ECR_ALIAS = '100');
                OR_CA100 = WValor;
             ELSEIF (ECR_ALIAS = '101');
                OR_CA101 = WValor;
             ELSEIF (ECR_ALIAS = '102');
                OR_CA102 = WValor;
             ENDIF;
          ELSEIF (ECR_TPITEM = 'D'); // Cláusulas Diversas
             IF (ECR_ALIAS = '103');
                OR_CD103 = WValor;
             ELSEIF (ECR_ALIAS = '130');
                OR_CD130 = WValor;
             ELSEIF (ECR_ALIAS = '131');
                OR_CD131 = WValor;
             ELSEIF (ECR_ALIAS = '132');
                OR_CD132 = WValor;
             ELSEIF (ECR_ALIAS = '133');
                OR_CD133 = WValor;
             ELSEIF (ECR_ALIAS = '134');
                OR_CD134 = WValor;
             ELSEIF (ECR_ALIAS = '135');
                OR_CD135 = WValor;
             ELSEIF (ECR_ALIAS = '136');
                OR_CD136 = WValor;
             ELSEIF (ECR_ALIAS = '137');
                OR_CD137 = WValor;
             ENDIF;
          ENDIF;
       ENDSR;
      /END-FREE
     P*=======================================================
     P* Processo..: ChkClausula
     P* Objectivo.: Validar se a Cobertura está contratada para
     P*             a Apólices (trabalha com as duas versões das
     P*             Tabelas). Actualizar nova Tabela, partindo
     P*             do que encontrar na antiga.
     P*=======================================================
     P ChkClausula     B                   EXPORT
     D                 PI             5S 0
     D   PRM_CdItem                   3A   CONST
     D   PRM_Alias                   10A   CONST
     D   PRM_Natureza                 1A   CONST
     D WNuSequ         S              5S 0 INZ(*ZEROS)
      /FREE
        WNuSequ = *ZEROS;
        CHAIN (OR_Ramo:OR_Apol:PRM_CdItem) RPRDP00901;
        IF (%FOUND());
           DELETE (OR_Ramo:OR_Apol:PRD_NUSEQU) RPRDP009;
        ENDIF;
        //    WNuSequ = PRD_NUSEQU;
        // ELSE;
           EXSR ApplyOldSett;
        // ENDIF;
        RETURN WNuSequ;

       //========================================================
       // Subrotina.: ApplyOldSett
       // Objectivo.: Aplicar configuração partindo das antigas
       //             tabelas. Descontinuar, esta rotina depois
       //             de modificar os Sistemas.
       //========================================================
       BEGSR ApplyOldSett;
          WNuSequ = *ZEROS;
          IF (PRM_Natureza = 'E'); // Condições Especiais
             IF (OR_CE001 = 'S') AND (%TRIM(PRM_ALIAS) = '001');
                WNuSequ = 9999;
             ELSEIF (OR_CE002 = 'S') AND (%TRIM(PRM_ALIAS) = '002');
                WNuSequ = 9999;
             ELSEIF (OR_CE003 = 'S') AND (%TRIM(PRM_ALIAS) = '003');
                WNuSequ = 9999;
             ELSEIF (OR_CE004 = 'S') AND (%TRIM(PRM_ALIAS) = '004');
                WNuSequ = 9999;
             ENDIF;
          ELSEIF (PRM_Natureza = 'A'); // Cláusulas Aplicáveis
             IF (OR_CA030 = 'S') AND (%TRIM(PRM_ALIAS) = '030');
                WNuSequ = 9999;
             ELSEIF (OR_CA031 = 'S') AND (%TRIM(PRM_ALIAS) = '031');
                WNuSequ = 9999;
             ELSEIF (OR_CA100 = 'S') AND (%TRIM(PRM_ALIAS) = '100');
                WNuSequ = 9999;
             ELSEIF (OR_CA101 = 'S') AND (%TRIM(PRM_ALIAS) = '101');
                WNuSequ = 9999;
             ELSEIF (OR_CA102 = 'S') AND (%TRIM(PRM_ALIAS) = '102');
                WNuSequ = 9999;
             ENDIF;
          ELSEIF (PRM_Natureza = 'D'); // Cláusulas Diversas
             IF (OR_CD103 = 'S') AND (%TRIM(PRM_ALIAS) = '103');
                WNuSequ = 9999;
             ELSEIF (OR_CD130 = 'S') AND (%TRIM(PRM_ALIAS) = '130');
                WNuSequ = 9999;
             ELSEIF (OR_CD131 = 'S') AND (%TRIM(PRM_ALIAS) = '131');
                WNuSequ = 9999;
             ELSEIF (OR_CD132 = 'S') AND (%TRIM(PRM_ALIAS) = '132');
                WNuSequ = 9999;
             ELSEIF (OR_CD133 = 'S') AND (%TRIM(PRM_ALIAS) = '133');
                WNuSequ = 9999;
             ELSEIF (OR_CD134 = 'S') AND (%TRIM(PRM_ALIAS) = '134');
                WNuSequ = 9999;
             ELSEIF (OR_CD135 = 'S') AND (%TRIM(PRM_ALIAS) = '135');
                WNuSequ = 9999;
             ELSEIF (OR_CD136 = 'S') AND (%TRIM(PRM_ALIAS) = '136');
                WNuSequ = 9999;
             ELSEIF (OR_CD137 = 'S') AND (%TRIM(PRM_ALIAS) = '137');
                WNuSequ = 9999;
             ENDIF;
          ENDIF;

          IF (WNuSequ = 9999);
             SETGT (OR_Ramo:OR_Apol) RPRDP009;
             READP(N) RPRDP009;
             IF (PRD_CDRAMO <> OR_RAMO) OR (PRD_NUAPOL <> OR_APOL);
                PRD_NUSEQU = *ZEROS;
             ENDIF;
             PRD_NUSEQU   = PRD_NUSEQU + 1;
             PRD_CDRAMO   = OR_Ramo;
             PRD_NUAPOL   = OR_Apol;
             PRD_CDITEM   = PRM_CdItem;
             WRITE RPRDP009;
          ENDIF;
       ENDSR;
      /END-FREE
     P ChkClausula     E
