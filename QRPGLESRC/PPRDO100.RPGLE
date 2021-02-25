     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL) FIXNBR(*ZONED)
     H*==================================================================
     H* Programa..: PRDO100
     H* Descrição.: Este programa destina-se a gerir as opções do Menu
     H*             Interno.
     H*
     H*==================================================================
     FFPRDT001  IF   E           K DISK    PREFIX(PRD_)
     FFCATP114  IF   E           K DISK    PREFIX(CAT_)
     FFSECP010  IF   E           K DISK    PREFIX(SEC_)
     FFPRDE100  CF   E             WORKSTN INFDS(INFD#KEYS)
     F                                     PREFIX(ECR_)
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
      /Copy QRPGLESRC,SDBSO012
      /Copy QRPGLESRC,SCATO010
      /Copy QRPGLESRC,SSECO010
      /Copy QRPGLESRC,SDBSO010

     D*==> Estrutura de dados do Registo
     D RBASEDIO      E DS                  ExtName(GBASEDIO)
     D                                     Prefix(OR_)
     D                                     BASED(WPtrData)
     D*==> Parametros recebidos
     D WRamoApolice    DS
     D   WRamApol              1      9S 0
     D   WRamo                 1      2S 0
     D   WApolice              3      9S 0
     D
     D*==> Opções disponíveis
     D #LstOption      DS                  QUALIFIED DIM(10)
     D   NumOption                    2S 0 INZ(*ZEROS)
     D   ptrOption                     *
     D   ValOption                   10A   INZ(*ZEROS)
     D   IsEmpty                       N   INZ(*ON)
     D WDesOption      S             50A   BASED(ptrOption)
     D WIdx            S              5S 0 INZ(*ZEROS)
     D
     D*==> Chamada de função Dynamic, para trabalhar detalhe do Item
     D ShellExec       PR                  EXTPGM('QCMDEXC')
     D   stdcmd                     300A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5 CONST
     D
     D*-----------------------------------------------------
     D WParmRamApol    S              9S 0
     D PDatInicio      S              8S 0
     D WDatInicio      S              8S 0
     D WIcErro         S               N   INZ(*OFF)
     D WIcOrigem       S              1A
     D
     D*==> Pré-visualização das Actas
     D  PrintPreview   PR                  EXTPGM('RPRDO005')
     D   ptrOR                         *   CONST
     D   Cdfunction                   2A   CONST
     D   Data                         8S 0 CONST
     D
     D*==> Manutenção das Descrições Fixas
     D CallOption      PR                  EXTPGM(DynamicCall)
     D   CodProduto                   5S 0 CONST
     D   DatInicio                    8S 0 CONST
     D   ptrOR                         *   CONST
     D   WModo                         N   CONST
     D
     D DynamicCall     S             10A   INZ(*BLANKS)
     D WComando        S            200A
     D WIcAltera       S               N
     D WCodProduto     S              5S 0
     D WDatSelect      S              8S 0
     D WOrigem         S              1A
     D WModo           S               N
     D WModoBK         S               N
     D
     D WSessCapRis     S              5S 0 INZ(*ZEROS)
     D WRecAct         S              5S 0 INZ(*ZEROS)
     D WCdRisco        S              5S 0 INZ(*ZEROS)
     D WCapRisco       S             11S 2 INZ(*ZEROS)
     D WConsulta       S               N   INZ(*ON)
     C*========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WParmRamApol
     C                   PARM                    WPtrData
     C                   PARM                    PDatInicio
     C                   PARM                    WModo
      /FREE
       //------------------------------------
       // Se não estiver autorizado então sai
       //------------------------------------
       IF NOT $ValAccess(S#PGM:'':99999);
          $ShowDialog('SEC0001');
          RETURN;
       ENDIF;

       //------------------------------------
       // Validar os parâmetros recebidos
       //------------------------------------
       MONITOR;
          WRamApol = WParmRamApol;
          MONITOR;
              WDatInicio = PDatInicio;
          ON-ERROR;
              WDatInicio = $DateToInt(%DATE());
          ENDMON;
          IF (WDatInicio = *ZEROS);
             WDatInicio = $DateToInt(%DATE());
          ENDIF;
       ON-ERROR;
          RETURN;
       ENDMON;

       WModoBK    = WModo;
       WConsulta  = WModo;
       *IN45      = WModo;
       WDatSelect = WDatInicio;
       ECR_DtSelect = WDatSelect;
       IF (OR_CONDOM = 'C');
          WCodProduto = $GetCodProduto(WRamo:2);
       ELSE;
          WCodProduto = $GetCodProduto(WRamo:1);
       ENDIF;
       CHAIN (WCodProduto) RCATP114;
       IF (NOT %FOUND());
          RETURN;
       ENDIF;

       ECR_NOMSEG  = OR_SEG;
       ECR_RAMO    = WRamo;
       ECR_APOLICE = WApolice;
       EXSR ChkApolice;
       IF (NOT $getCMTStatus);
          $strCommitment();
       ENDIF;
       EXSR MakeDynMenu;
       EXSR ShowInternMenu;

       //-----------------------------------------------
       // Aplicar valores dos capitais de Risco
       //-----------------------------------------------
       IF (WRamo <> 50) AND (WRamo <> 51);
       // OR_PR1 = OR_CAP1 * (OR_TX1 / 100);
       // OR_PR2 = OR_CAP2 * (OR_TX2 / 100);
       // OR_PR3 = OR_CAP3 * (OR_TX3 / 100);
       // OR_CAPTOT = OR_CAP1 + OR_CAP2 + OR_CAP3;
       // OR_PRSACT  = OR_PR1 + OR_PR2 + OR_PR3;
       ENDIF;
       //--IF (NOT $isCMTPending());
       // $EndCommitment();
       // ENDIF;
       PDatInicio = *ZEROS;
       WModo      = WModoBk;
       RETURN;

       //============================================================
       // Subrotina..: ChkApolice
       // Objectivo..: Valdiar Existência da Apólice, se Não existir
       //              então sai.
       //============================================================
       BEGSR ChkApolice;
          CHAIN (WRamo:WApolice) RPRDP001;
          IF (NOT %FOUND());
             RETURN;
          ENDIF;
       ENDSR;

       //============================================================
       // Subrotina..: ShowCapRisco
       // Objectivo..: Salvar Capitais de Risco
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
          WSessCapRis = $SetFiles('FPRDP011');
          $AddDBDate($IntToDate(ECR_DTSelect));
          $SetCriteria('(CDRAMO='''+%CHAR(OR_RAMO)+''') +
                    AND (NUAPOL='''+%CHAR(OR_Apol)+''')');
          DOW ($GetRecord(WRecAct) > *ZEROS);
             WCdRisco = $GetFieldNum('CDRISCO');
             WCapRisco = $GetFieldNum('IMCPRISC');
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
             ENDIF;
             WRecAct += 1;
          ENDDO;
          $RmvDBSession(WSessCapRis);
       ENDSR;

       //============================================================
       // Subrotina..: MakeDynMenu
       // Objectivo..: Construir opções do Sub-Menu
       //============================================================
       BEGSR MakeDynMenu;
          WIdx = 1;
          #LstOption(WIdx).NumOption = WIdx;
          #LstOption(WIdx).ptrOption = %ADDR(ECR_DSOPT01);
          WIdx += 1;
          #LstOption(WIdx).NumOption = WIdx;
          #LstOption(WIdx).ptrOption = %ADDR(ECR_DSOPT02);
          WIdx += 1;
          #LstOption(WIdx).NumOption = WIdx;
          #LstOption(WIdx).ptrOption = %ADDR(ECR_DSOPT03);
          WIdx += 1;
          #LstOption(WIdx).NumOption = WIdx;
          #LstOption(WIdx).ptrOption = %ADDR(ECR_DSOPT04);
          WIdx += 1;
          #LstOption(WIdx).NumOption = WIdx;
          #LstOption(WIdx).ptrOption = %ADDR(ECR_DSOPT05);
          WIdx += 1;
          #LstOption(WIdx).NumOption = WIdx;
          #LstOption(WIdx).ptrOption = %ADDR(ECR_DSOPT06);
          WIdx += 1;
          #LstOption(WIdx).NumOption = WIdx;
          #LstOption(WIdx).ptrOption = %ADDR(ECR_DSOPT07);
          WIdx += 1;
          #LstOption(WIdx).NumOption = WIdx;
          #LstOption(WIdx).ptrOption = %ADDR(ECR_DSOPT08);
          WIdx += 1;
          #LstOption(WIdx).NumOption = WIdx;
          #LstOption(WIdx).ptrOption = %ADDR(ECR_DSOPT09);
          WIdx += 1;
          #LstOption(WIdx).NumOption = WIdx;
          #LstOption(WIdx).ptrOption = %ADDR(ECR_DSOPT10);

          SETLL (WCodProduto) RCATP114;
          READE (WCodProduto) RCATP114;
          DOW (NOT %EOF());
             IF (CAT_NUORDEM >= 1) AND (CAT_NUORDEM <= 10); // Limitação ecrã
                CHAIN (CAT_CDFUNCT) RSECP010;
                IF (%FOUND);
                   IF (SEC_CDTMPLT = 35); // Template da Opção
                      WIdx = CAT_NUORDEM;
                      ptrOption = #LstOption(WIdx).ptrOption;
                      WDesOption = %TRIM(%EDITW(CAT_NUORDEM:'     '))+'. '+
                                      %TRIM(SEC_DSPROGR);
                      #LstOption(WIdx).ValOption = SEC_NMPROGR;
                      #LstOption(WIdx).isEmpty = *OFF;
                   ENDIF;
                ENDIF;
             ENDIF;
             READE (WCodProduto) RCATP114;
          ENDDO;
       ENDSR;

       //============================================================
       // Subrotina..: ShowInternMenu
       // Objectivo..: Mostrar o Menu Interno
       //============================================================
       BEGSR ShowInternMenu;
          DOW (NOT *IN03 AND NOT *IN12);
             DS#HEADER    = $getHeader(S#PGM:'RPRDE10005');
             DS#FOOTER    = $getFooter(S#PGM:'RPRDE10005':*BLANKS);
             ECR_S#PGM    = S#PGM;
             WRITE     RPRDE10000;
             WRITE     RPRDE10099;
             EXSR ShowCapRisco;
             EXFMT RPRDE10005;
             IF (Func#Key = KEY#Enter);
                MONITOR;
                   WIdx = %DEC(ECR_OPTION:2:0);
                ON-ERROR;
                   WIdx = *ZEROS;
                ENDMON;
                IF (WIdx >= 1) AND (WIdx <= 10);
                   IF (ECR_DtSelect = *ZEROS);
                      ECR_DTSelect = $DateToInt(%DATE());
                   ENDIF;
                   WDatSelect = ECR_DtSelect;
                   DynamicCall = %TRIM(#LstOption(WIdx).ValOption);
                   IF (DynamicCall <> *BLANKS);
                      MONITOR;
                      IF (NOT WConsulta);
                         IF (WDatSelect<>$DateToInt(%DATE()));
                            WModo = *ON;  // Consulta
                         ELSE;
                            WModo = *OFF; // Manutenção
                         ENDIF;
                      ENDIF;
                      CallOption(WCodProduto:WDatSelect:
                                     WPtrData:WModo);
                      DynamicCall = *BLANKS;
                      ON-ERROR;
                      ENDMON;
                      *IN12 = *OFF;
                   ENDIF;
                ENDIF;
             ELSEIF (Func#Key = KEY#F14);
                MONITOR;
                   PrintPreview(WPtrData:'00':WDatInicio);
                ON-ERROR;
                ENDMON;
             ENDIF;
          ENDDO;
       ENDSR;
      /END-FREE
