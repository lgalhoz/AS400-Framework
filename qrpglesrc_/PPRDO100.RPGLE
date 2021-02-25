     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL) FIXNBR(*ZONED)
     H*==================================================================
     H* Programa..: PRDO100
     H* Descrição.: Este programa destina-se a gerir as opções do Menu
     H*             Interno.
     H*
     H*==================================================================
     FFPRDP001  IF A E           K DISK    PREFIX(PRD_)
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
      /Copy QRPGLESRC,SCATO010
      /Copy QRPGLESRC,SSECO010

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
     D   Cdfunction                   5S 0 CONST
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
     D WOrigem         S              1A
     D WModo           S               N
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
          *INLR = *ON;
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
              WDatInicio = *ZEROS;
          ENDMON;
       ON-ERROR;
          *INLR = *ON;
          RETURN;
       ENDMON;


       IF (OR_CONDOM = 'C');
          WCodProduto = $GetCodProduto(WRamo:2);
       ELSE;
          WCodProduto = $GetCodProduto(WRamo:1);
       ENDIF;
       CHAIN (WCodProduto) RCATP114;
       IF (NOT %FOUND());
          *INLR = *ON;
          RETURN;
       ENDIF;

       ECR_NOMSEG  = OR_SEG;
       ECR_RAMO    = WRamo;
       ECR_APOLICE = WApolice;
       EXSR AddApolice;
       EXSR MakeDynMenu;
       EXSR ShowInternMenu;

       //-----------------------------------------------
       // Aplicar valores dos capitais de Risco
       //-----------------------------------------------
       OR_PR1 = OR_CAP1 * (OR_TX1 / 100);
       OR_PR2 = OR_CAP2 * (OR_TX2 / 100);
       OR_PR3 = OR_CAP3 * (OR_TX3 / 100);
       OR_CAPTOT = OR_CAP1 + OR_CAP2 + OR_CAP3;
       OR_PRSACT  = OR_PR1 + OR_PR2 + OR_PR3;

       *INLR = *ON;
       RETURN;

       //============================================================
       // Subrotina..: AddApolice
       // Objectivo..: Adicionar o registo para uma nova Apólice se
       //              esta ainda não existir
       //============================================================
       BEGSR AddApolice;
          CHAIN (WRamo:WApolice) RPRDP001;
          IF (NOT %FOUND());
             PRD_CDRAMO = WRamo;
             PRD_NUAPOL = WAPolice;
             WRITE RPRDP001;
          ENDIF;
       ENDSR;

       //============================================================
       // Subrotina..: ShowCapRisco
       // Objectivo..: Salvar Capitais de Risco
       //============================================================
       BEGSR ShowCapRisco;
          ECR_DSRISC001 = *BLANKS;
          ECR_CPRISC001 = *ZEROS;
          IF (OR_CR1 > *ZEROS);
             ECR_DSRISC001 = $getDescRisco(WCodProduto:OR_CR1);
             ECR_CPRISC001 = OR_CAP1;
          ENDIF;

          ECR_DSRISC002 = *BLANKS;
          ECR_CPRISC002 = *ZEROS;
          IF (OR_CR2 > *ZEROS);
             ECR_DSRISC002 = $getDescRisco(WCodProduto:OR_CR2);
             ECR_CPRISC002 = OR_CAP2;
          ENDIF;

          ECR_DSRISC003 = *BLANKS;
          ECR_CPRISC003 = *ZEROS;
          IF (OR_CR3 > *ZEROS);
             ECR_DSRISC003 = $getDescRisco(WCodProduto:OR_CR3);
             ECR_CPRISC003 = OR_CAP3;
          ENDIF;
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
             EXSR ShowCapRisco;
             DS#HEADER    = $getHeader(S#PGM:'RPRDE10005');
             DS#FOOTER    = $getFooter(S#PGM:'RPRDE10005':*BLANKS);
             ECR_S#PGM    = S#PGM;
             WRITE     RPRDE10000;
             WRITE     RPRDE10099;
             EXFMT RPRDE10005;
             IF (Func#Key = KEY#Enter);
                MONITOR;
                   WIdx = %DEC(ECR_OPTION:2:0);
                ON-ERROR;
                   WIdx = *ZEROS;
                ENDMON;
                IF (WIdx >= 1) AND (WIdx <= 10);
                   DynamicCall = %TRIM(#LstOption(WIdx).ValOption);
                   IF (DynamicCall <> *BLANKS);
                      MONITOR;
                      CallOption(WCodProduto:WDatInicio:
                                     WPtrData:WModo);
                      //             %Addr(RBASEDIO):WModo);
                      DynamicCall = *BLANKS;
                      ON-ERROR;
                      ENDMON;
                      *IN12 = *OFF;
                   ENDIF;
                ENDIF;
             ELSEIF (Func#Key = KEY#F14);
                MONITOR;
                   PrintPreview(WPtrData:*ZEROS);
                ON-ERROR;
                ENDMON;
             ENDIF;
          ENDDO;
       ENDSR;
      /END-FREE
