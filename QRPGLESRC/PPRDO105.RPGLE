     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PPRDO105
     H* Descrição.: Parameterizar Riscos da Apólice
     H*==================================================================
     FFPRDT001  IF   E           K DISK    PREFIX(PRD_)
     FFPRDP010  IF A E           K DISK    PREFIX(PRD_) COMMIT
     FFPRDP011  UF A E           K DISK    PREFIX(PRD_) COMMIT
     FFPRDP012  IF A E           K DISK    PREFIX(PRD_) COMMIT
     FFPRDE105  CF   E             WORKSTN SFILE(RPRDE10502:REC#)
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
     D*==> Validar Existência do Risco
     D ChkRisco        PR             5S 0
     D   CdRisco                      5S 0 CONST
     D
     D*==> Riscos da Apólice
     D LST#Risco       DS                  QUALIFIED DIM(10)
     D   CodRisco                     5S 0 INZ(*ZEROS)
     D   CapRisco                    11S 2 INZ(*ZEROS)
     D   IsEmpty                       N   INZ(*ON)
     D WRecAct         S              5S 0 INZ(*ZEROS)
     D WCapRisco       S             11S 2 INZ(*ZEROS)
     D WIdx            S              3S 0 INZ(*ZEROS)
     D
     D WIcErro         S               N   INZ(*OFF)
     D WIcOrigem       S              1A
     D REC#            S              4S 0
     D
     D #LstRisco       DS                  QUALIFIED DIM(12) BASED(pLstRisco)
     D   CdRisco                      5S 0
     D   CapSeguroOr                 11S 2
     D   CapSeguroNv                 11S 2
     D pLstRisco       S               *   INZ(*NULL)
     D WIdxRisco       S              5S 0
     D WIcExtract      S               N
     D WExceptKeys     S             24A   INZ(*BLANKS)
     D
     D*==> Parâmetros recebidos
     D WNome           S             40A
     D WSuperior       S              5S 0
     D WpLstRisco      S               *
     D
     D*==> Outros Parâmetros
     D WIcDisplay      S               N   INZ(*ON)
     D WNoFunction     S             10A   INZ(*BLANKS)
     D WCdRisco        S                   LIKE(PRD_CDRISCO)
     D WIsEmpty        S               N
     D WCodProduto     S              5S 0
     D WModo           S               N
     D WPtrOR          S               *
     D WCdSelect       S              1A   INZ(*BLANKS)
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
          $ShowDialog('SEC0001');
          RETURN;
       ENDIF;
       ptrOr   = WptrOR;

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

       ECR_NOMSEG  = OR_Seg;
       ECR_RAMO    = OR_RAMO;
       ECR_APOLICE = OR_Apol;

       LST#Risco   = $GetCapRisco(WCodProduto:OR_RAMO:OR_Apol:*ZEROS:
                                  *OFF:WDatInicio);

       WSessionID = $SetFiles('FCATP101');
       $AddDBDate($IntToDate(WDatInicio));
       $SetSort('CDRISCO');
       $SetCriteria('CDPROD = ' + %CHAR(WCodProduto));
       $SetPage(12);
       WIcDisplay = WModo;
       EXSR inzRecords;
       EXSR getRecords;
       DOW (NOT *IN03 AND NOT *IN12);
          EXSR ShowCapRisco; // Mostrar Capitais de Risco
          IF (WIcDisplay);
             WNoFunction = 'RPRDE10505'; // Consulta Coberturas
          ELSE;
             WNoFunction = 'RPRDE10521'; // Alterar Coberturas
          ENDIF;

          DS#HEADER    = $getHeader(S#PGM:WNoFunction);
          DS#FOOTER    = $getFooter(S#PGM:WNoFunction:*BLANKS);
          ECR_S#PGM = S#PGM;
          WRITE     RPRDE10500;
          WRITE     RPRDE10599;
          EXFMT RPRDE10503;
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
                PrintPreview(WPtrOr:'CB':WDatInicio);
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
          WRITE RPRDE10503;     // Limpar Lista
          *In53     = *Off;     // Repor Indicador
       ENDSR;

       //========================================================
       // Subrotina.: GetRecTexto
       // Objectivo.: Obter todos os registos de tabelas disponív
       //========================================================
       BEGSR getRecords;
          IF ($GetPage(Func#Key) > *ZEROS);
             DOW (NOT $IsEOF());
                *IN45 = *ON;
                *IN48 = *ON;
                IF (REC# = 12);
                   LEAVE;
                ENDIF;
                *IN49 = *OFF;
                REC# += 1;
                ECR_NUSEQU   = *ZEROS;
                ECR_CDSELECT = 'N';
                ECR_CDRISCO  = $GetFieldNum('CDRISCO');
                ECR_DSRISCO  = '('+%TRIM($GetField('CDRISCO'))+') ' +
                               $GetField('DSRISCO');
                WCdRisco     = ECR_CDRISCO;
                ECR_VLRISCO  = *ZEROS;
                ECR_TXRISCO  = *ZEROS;
                ECR_NUSEQU=ChkRisco(WCdRisco);
                IF (ECR_NUSEQU > *ZEROS);
                   *IN48        = *OFF;
                   ECR_CDSELECT = 'S';
                ENDIF;

                IF (NOT WIcDisplay);
                   *IN45  = *OFF;
                ENDIF;
                WRITE RPRDE10502;
                $getNext();
             ENDDO;
          ENDIF;

          //---------------------------------------
          // Validar se ainda existem mais registos
          //---------------------------------------
          IF (REC# = *ZEROS);
             *IN52 = *OFF;
          ELSE;
             IF (NOT $IsLastPage());
                *IN54    = *OFF;
             ELSE;
                *IN54    = *ON;
             ENDIF;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: SelRec
       // Objectivo.: Seleccionar os registos
       //========================================================
       BEGSR SelRec;
          IF (REC# = *ZEROS);
             LEAVESR;
          ENDIF;
          READC RPRDE10502;
          DOW (NOT %EOF());
             IF (ECR_OPTION <> *BLANKS);
                EXFMT RPRDE10507;
             ELSE;
                EXSR UpdRiscos;
             ENDIF;
             READC RPRDE10502;
          ENDDO;
       ENDSR;

       //========================================================
       // Subrotina.: UpdRiscos
       // Objectivo.: Actualizar Informação sobre os Riscos
       //========================================================
       BEGSR UpdRiscos;
          IF ((ECR_NUSEQU > *ZEROS) AND (ECR_CDSELECT = 'N'));
             DELETE (OR_Ramo:OR_Apol:ECR_NUSEQU) RPRDP011;
             IF (OR_CR1 = ECR_CDRISCO);
                OR_CR1 = *ZEROS;
                OR_CAP1 = *ZEROS;
             ELSEIF (OR_CR2 = ECR_CDRISCO);
                OR_CR2 = *ZEROS;
                OR_CAP2 = *ZEROS;
             ELSEIF (OR_CR3 = ECR_CDRISCO);
                OR_CR3 = *ZEROS;
                OR_CAP3 = *ZEROS;
             ENDIF;
          ELSEIF ((ECR_NUSEQU = *ZEROS) AND (ECR_CDSELECT = 'S'));
             MONITOR;
                IF (OR_CR1 = *ZEROS);
                   OR_CR1  = ECR_CDRISCO;
                   OR_CAP1 = ECR_VLRISCO;
                   OR_TX1  = ECR_TXRISCO;
                ELSEIF (OR_CR2 = *ZEROS);
                   OR_CR2 = ECR_CDRISCO;
                   OR_CAP2 = ECR_VLRISCO;
                   OR_TX2  = ECR_TXRISCO;
                ELSEIF (OR_CR3 = *ZEROS);
                   OR_CR3 = ECR_CDRISCO;
                   OR_CAP3 = ECR_VLRISCO;
                   OR_TX3  = ECR_TXRISCO;
                ENDIF;
             ON-ERROR;
             ENDMON;

             PRD_CDRAMO   = OR_Ramo;
             PRD_NUAPOL   = OR_Apol;
             PRD_CDRISCO  = ECR_CDRISCO;
             PRD_IMCPRISC = ECR_VLRISCO;
             PRD_TXRISCO  = ECR_TXRISCO;
             WRITE RPRDP011;
          ELSEIF ((ECR_NUSEQU > *ZEROS) AND (ECR_CDSELECT = 'S'));
             IF (OR_CR1 = ECR_CDRISCO);
                OR_CAP1 = ECR_VLRISCO;
                OR_TX1  = ECR_TXRISCO;
             ELSEIF (OR_CR2 = ECR_CDRISCO);
                OR_CAP2 = ECR_VLRISCO;
                OR_TX2  = ECR_TXRISCO;
             ELSEIF (OR_CR3 = ECR_CDRISCO);
                OR_CAP3 = ECR_VLRISCO;
                OR_TX3  = ECR_TXRISCO;
             ENDIF;
             CHAIN (OR_RAMO:OR_APOL:ECR_NUSEQU) RPRDP011;
             IF (%FOUND());
                PRD_IMCPRISC = ECR_VLRISCO;
                PRD_TXRISCO  = ECR_TXRISCO;
                UPDATE RPRDP011 %FIELDS(PRD_IMCPRISC:PRD_TXRISCO);
             ENDIF;
          ENDIF;
       ENDSR;
      /END-FREE
     P*=======================================================
     P* Processo..: ChkRisco
     P* Objectivo.: Validar se o Risco está contratado para a
     P*             Apólice.
     P*=======================================================
     P ChkRisco        B                   EXPORT
     D                 PI             5S 0
     D   PRM_CdRisco                  5S 0 CONST
     D WSessRisco      S              5S 0 INZ(*ZEROS)
     D WIsNew          S               N   INZ(*OFF)
     D WCdTexto        S              5S 0 INZ(*ZEROS)
     D WDsTexto        S            100A   INZ(*BLANKS)
     D WNuSequ         S              5S 0 INZ(*ZEROS)
     D*=======================================================
      /FREE
        WNuSequ = *ZEROS;
        WIsNew  = *OFF;
        EXSR ApplyOldSett;
        RETURN WNuSequ;

       //========================================================
       // Subrotina.: ApplyOldSett
       // Objectivo.: Aplicar configuração partindo das antigas
       //             tabelas. Descontinuar, esta rotina depois
       //             de modificar os Sistemas.
       //========================================================
       BEGSR ApplyOldSett;
          WNuSequ = *ZEROS;
          IF (OR_CR1 = PRM_CdRisco) OR (OR_CR2 = PRM_CdRisco) OR
             (OR_CR3 = PRM_CdRisco);
             WNuSequ = 9999;
             IF (OR_CR1 = PRM_CdRisco);
                ECR_VLRISCO = OR_CAP1;
                ECR_TXRISCO = OR_TX1;
             ELSEIF (OR_CR2 = PRM_CdRisco);
                ECR_VLRISCO = OR_CAP2;
                ECR_TXRISCO = OR_TX2;
             ELSEIF (OR_CR3 = PRM_CdRisco);
                ECR_VLRISCO = OR_CAP3;
                ECR_TXRISCO = OR_TX3;
             ENDIF;
          ENDIF;

          //----------------------------------------------------------------
          // Obter o Registo para a Cobertura activo na data
          //----------------------------------------------------------------
          WSessRisco  = $SetFiles('FPRDP011');
          $AddDBDate($IntToDate(WDatInicio));
          $SetCriteria('(CDRAMO='''+%CHAR(OR_Ramo)+''') +
                    AND (NUAPOL='''+%CHAR(OR_Apol)+''') +
                  AND (CDRISCO ='''+%CHAR(PRM_Cdrisco)+ ''')')   ;
          IF ($getRecord() = *ZEROS);
             IF (WNuSequ = 9999) AND NOT (WIcDisplay);
                PRD_CDRAMO  = OR_Ramo;
                PRD_NUAPOL  = OR_Apol;
                PRD_CDRISCO = PRM_CdRisco;
                PRD_IMCPRISC= ECR_VLRISCO;
                PRD_TXRISCO = ECR_TXRISCO;
                WIsNew  = *ON;
                WRITE RPRDP011;
                $getRecord();
             ENDIF;
          ENDIF;

          WNuSequ = $GetFieldNum('NUSEQU');
          ECR_VLRISCO = $GetFieldNum('IMCPRISC');
          ECR_TXRISCO = $GetfieldNum('TXRISCO');
          ECR_IMPRRISC = $GetFieldNum('IMPRRISC');
          $RmvDBSession(WSessRisco);

          //----------------------------------------------------
          // ForComp valor na lista de ecrã
          //----------------------------------------------------
          IF (WNuSequ > *ZEROS);
             IF (ECR_VLRISCO > *ZEROS);
                ECR_VLRISCOALF = %EDITW(ECR_VLRISCO:'   .   .   ,  ');
                IF (WIsNew);
                   //--> Adicionar Registo correspondende nas Notas Descritivas
                   EXSR AddNotaDescritiva;
                ENDIF;
             ELSE;
                ECR_VLRISCOALF = ATR#Blink + '?? Capital ??';
             ENDIF;
          ELSE;
             ECR_VLRISCOALF = *BLANKS;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: AddNotaDescritiva
       // Objectivo.: Adicionar uma nota Descritiva
       //========================================================
       BEGSR AddNotaDescritiva;
          WCdTexto = *ZEROS;
          WSessRisco  = $SetFiles('FCATP105 A INNER JOIN FCATP106 B +
                                   ON (A.CDPROD = B.CDPROD) AND +
                                   (A.CDTEXTO = B.CDTEXTO)');
          $AddDBDate($IntToDate(WDatInicio));
          $SetFields('FCATP105':'A');
          $SetFields('FCATP106':'B':*OFF);
          $SetCriteria('(A.CDPROD='''+%CHAR(WCodProduto)+''') +
                  AND (A.CDRISCO ='''+%CHAR(PRM_Cdrisco)+ ''') +
                  AND (A.ICDEFEIT = ''S'')');
          IF ($getRecord() > *ZEROS);
             WCdTexto = $GetFieldNum('B.CDTEXTO');
             WDSTexto = $GetField('B.DSTEXTO');
          ENDIF;
          $RmvDBSession(WSessRisco);

          //-------------------------------------------------
          // Actualizar informação da Nota Descritiva
          //-------------------------------------------------
          IF (WCdTexto > *ZEROS);
             PRD_CDRAMO  = OR_Ramo;
             PRD_NUAPOL  = OR_Apol;
             PRD_NUSEQU  = *ZEROS;
             PRD_NUSQSUP = *ZEROS;
             PRD_CDTEXTO = WCdTexto;
             WRITE RPRDP010;

             PRD_CDRAMO = OR_Ramo;
             PRD_NUAPOL = OR_Apol;
             PRD_NUSEQU = $GetNextKey('FPRDP010':'NUSEQU':'(CDRAMO='''+
                          %TRIM(%CHAR(OR_Ramo)) + ''') AND (NUAPOL='''+
                          %TRIM(%CHAR(OR_Apol)) + ''')');
             PRD_NUSEQU  -= 1;
             PRD_ICDETAIL = 'N';
             PRD_DSDETAIL = WDsTexto;
             PRD_CPSEGURO = ECR_VLRISCO;
             WRITE RPRDP012;
          ENDIF;
       ENDSR;
      /END-FREE
     P ChkRisco        E
