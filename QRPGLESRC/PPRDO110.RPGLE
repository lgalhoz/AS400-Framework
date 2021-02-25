     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PPRDO110
     H* Descrição.: Parameterizar Bónus e Agravamentos.
     H*==================================================================
     FFPRDT001  IF   E           K DISK    PREFIX(PRD_)
     FFPRDP022  UF A E           K DISK    PREFIX(PRD_) COMMIT
     FFPRDE110  CF   E             WORKSTN SFILE(RPRDE11002:REC#)
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
      /Copy QRPGLESRC,SPRDO002

     D*==> Estrutura de dados do Registo
     D RBASEDIO      E DS                  ExtName(GBASEDIO)
     D                                     Prefix(OR_)
     D                                     Based(ptrOR)
     D
     D*==> Pré-visualização das Actas
     D  PrintPreview   PR                  EXTPGM('RPRDO005')
     D   ptrOR                         *   CONST
     D   Cdfunction                   2A   CONST
     D   Data                         8S 0 CONST
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
     D*==> Lista de Riscos
     D LST#Risco       DS                  QUALIFIED DIM(10)
     D   CodRisco                     5S 0 INZ(*ZEROS)
     D   CapRisco                    11S 2 INZ(*ZEROS)
     D   IsEmpty                       N   INZ(*ON)
     D
     D*==> Outros Parâmetros
     D WListEnd        S               N
     D WIcDisplay      S               N   INZ(*ON)
     D WModo           S               N
     D WNoFunction     S             10A   INZ(*BLANKS)
     D WCdItem         S                   LIKE(PRD_CDITEM)
     D WTpItem         S              1A
     D WPtrOR          S               *
     D WSessionCat     S              5S 0 INZ(*ZEROS)
     D WSessionPrd     S              5S 0 INZ(*ZEROS)
     D WRecAct         S              5S 0 INZ(*ZEROS)
     D WCdRisco        S              5S 0 INZ(*ZEROS)
     D WCapRisco       S             11S 2 INZ(*ZEROS)
     D WCodProduto     S              5S 0
     D WIsToSelect     S              1A   INZ(*BLANKS)
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
          $ShowDialog('SEC0001');
          RETURN;
       ENDIF;

       ptrOr       = WptrOR;

       //---------------------------------------------
       // Validar se os dados estão convertidos
       //---------------------------------------------
       CHAIN (OR_RAMO:OR_APOL) RPRDP001;
       IF (%FOUND());
          IF (PRD_TPACTA = 'A');
             $ShowDialog('PRD0160');
             RETURN;
          ENDIF;
       ELSE;
          $ShowDialog('PRD0161');
          RETURN;
       ENDIF;

       ECR_NOMSEG  = OR_SEG;
       ECR_RAMO    = OR_RAMO;
       ECR_APOLICE = OR_Apol;

       LST#Risco = $GetCapRisco(WCodProduto:OR_RAMO:OR_Apol:*ZEROS:
                                *OFF:WDatInicio);
       EXSR ShowCapRisco;

       WSessionPrd= $SetFiles('FPRDP022');
       $AddDBDate($IntToDate(WDatInicio));

       WSessionCat = $SetFiles('FCATP120');
       $AddDBDate($IntToDate(WDatInicio));
       $SetSort('TPITEM #DESC');
       $SetCriteria('CDPROD = ' + %CHAR(WCodProduto));
       $SetPage(12);
       WIcDisplay = WModo; // *ON
       EXSR inzRecords;
       EXSR getRecords;

       DOW (NOT *IN03 AND NOT *IN12);
          IF (WIcDisplay);
             WNoFunction = 'RPRDE11005'; // Consulta Coberturas
          ELSE;
             WNoFunction = 'RPRDE11021'; // Alterar Coberturas
          ENDIF;

          DS#HEADER    = $getHeader(S#PGM:WNoFunction);
          DS#FOOTER    = $getFooter(S#PGM:WNoFunction:*BLANKS);
          ECR_S#PGM = S#PGM;
          WRITE     RPRDE11000;
          WRITE     RPRDE11099;
          EXFMT RPRDE11003;
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
          ELSEIF ($ValidKey(Func#Key:KEY#F14));
             MONITOR;
                PrintPreview(WPtrOr:'CE':WDatInicio);
             ON-ERROR;
             ENDMON;
          ELSEIF (Func#Key = KEY#Enter);
             EXSR SelRec;
             *IN12 = *OFF;
             EXSR inzRecords;
             EXSR getRecords;
          ENDIF;
       ENDDO;
       $RmvDBSession(WSessionCat);
       $RmvDBSession(WSessionPrd);
       *IN12 = *OFF;
       *IN03 = *OFF;
       RETURN;

       //============================================================
       // Subrotina..: ShowCapRisco
       // Objectivo..: repor Capitais de Risco
       //============================================================
       BEGSR ShowCapRisco;
          ECR_DSRISC001 = *BLANKS;
          ECR_CPRISC001 = *ZEROS;
          ECR_DSRISC002 = *BLANKS;
          ECR_CPRISC002 = *ZEROS;
          ECR_DSRISC003 = *BLANKS;
          ECR_CPRISC003 = *ZEROS;
          ECR_DSRISC004 = *BLANKS;
          ECR_CPRISC004 = *ZEROS;
          WRecAct = 1;
          DOW (NOT Lst#Risco(WRecAct).IsEmpty);
             WCdRisco  = Lst#Risco(WRecAct).CodRisco;
             WCapRisco = Lst#Risco(WRecAct).CapRisco;
             IF (WCdRisco > *ZEROS);
                IF (ECR_DSRISC001 = *BLANKS);
                   ECR_DSRISC001 = $getDescRisco(WCodProduto:WCdRisco);
                   ECR_CPRISC001 = WCapRisco;
                ELSEIF (ECR_DSRISC002 = *BLANKS);
                   ECR_DSRISC002 = $getDescRisco(WCodProduto:WCdRisco);
                   ECR_CPRISC002 = WCapRisco;
                ELSEIF (ECR_DSRISC003 = *BLANKS);
                   ECR_DSRISC003 = $getDescRisco(WCodProduto:WCdRisco);
                   ECR_CPRISC003 = WCapRisco;
                ELSEIF (ECR_DSRISC004 = *BLANKS);
                   ECR_DSRISC004 = $getDescRisco(WCodProduto:WCdRisco);
                   ECR_CPRISC004 = WCapRisco;
                   LEAVE;
                ENDIF;
             ENDIF;
             WRecAct += 1;
          ENDDO;
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
          WRITE RPRDE11003;     // Limpar Lista
          *In53     = *Off;     // Repor Indicador
       ENDSR;

       //========================================================
       // Subrotina.: GetRecords
       // Objectivo.: Obter todos os registos de tabelas disponív
       //========================================================
       BEGSR getRecords;
          WTpItem  = *BLANKS;
          $SetDBSession(WSessionCat);
          IF ($GetPage(Func#Key) > *ZEROS);
             DOW (NOT $IsEOF());
                *IN45  = *ON;
                *IN48  = *ON;

                WIsToSelect = 'S';
                WCdRisco  = $GetFieldNum('CDRISCO');
                IF (WCdRisco > *ZEROS);
                   WIsToSelect = 'N';
                   WRecAct = 1;
                   DOW (NOT Lst#Risco(WRecAct).IsEmpty);
                      IF (WCdRisco   = Lst#Risco(WRecAct).CodRisco);
                         WIsToSelect = 'S';
                         LEAVE;
                      ENDIF;
                      WRecAct += 1;
                   ENDDO;
                ENDIF;

                IF (WIsToSelect = 'S');
                   IF ($GetField('TPITEM') <> WTpItem);
                      *IN45  = *ON;
                      *IN48  = *OFF;
                      *IN49  = *ON;
                      REC# += 1;
                      ECR_CDSELECT = *BLANKS;
                      WTpItem = $GetField('TPITEM');
                      ECR_DSTIPO = $getDescricao(71:WTpItem);
                      WRITE RPRDE11002;
                   ENDIF;

                   *IN48  = *ON;
                   *IN49  = *OFF;
                   REC# += 1;
                   ECR_NUSEQU   = *ZEROS;
                   ECR_CDSELECT = 'N';
                   ECR_TPITEM = $GetField('TPITEM');
                   ECR_CDITEM = $GetFieldNum('CDITEM');
                   ECR_DSTIPO = $GetField('DSITEM');

                   $SetDBSession(WSessionPrd);
                   $SetCriteria('(CDRAMO='''+%CHAR(ECR_RAMO)+''') AND +
                                 (NUAPOL='''+%CHAR(ECR_APOLICE)+''') AND +
                                 (CDITEM='''+%CHAR(ECR_CDITEM)+''')');
                   IF ($GetRecord() > *ZEROS);
                      ECR_NUSEQU = $GetFieldNum('NUSEQU');
                      ECR_CDSELECT = 'S';
                      *IN48  = *OFF;
                   ENDIF;

                   $SetDBSession(WSessionCat);
                   IF ($GetField('CDMANUT')<>'M');
                      *IN45 = *ON;
                      *IN48  = *OFF;
                   ELSEIF (NOT WIcDisplay);
                      *IN45  = *OFF;
                   ENDIF;
                   WRITE RPRDE11002;
                   IF (REC# = 12);
                      LEAVE;
                   ENDIF;
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
          READC RPRDE11002;
          DOW (NOT %EOF());
             IF (ECR_OPTION <> *BLANKS);
                ECR_DSSELITEM = $getDescricao(25:ECR_TPITEM);
                EXFMT RPRDE11007;
             ELSE;
                EXSR UpdClausula;
             ENDIF;
             READC RPRDE11002;
          ENDDO;
       ENDSR;

       //========================================================
       // Subrotina.: UpdClausula
       // Objectivo.: Actualizar Clausulas e Condições
       //========================================================
       BEGSR UpdClausula;
          IF ((ECR_NUSEQU > *ZEROS) AND (ECR_CDSELECT = 'N'));
             DELETE (OR_Ramo:OR_Apol:ECR_NUSEQU) RPRDP022;
          ELSEIF ((ECR_NUSEQU = *ZEROS) AND (ECR_CDSELECT = 'S'));
             PRD_CDRAMO   = OR_Ramo;
             PRD_NUAPOL   = OR_Apol;
             PRD_CDITEM   = ECR_CDITEM;
             WRITE RPRDP022;
          ENDIF;
       ENDSR;
      /END-FREE
