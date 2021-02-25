     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PPRDO112
     H* Descrição.: Manutenção de Intruções de Débito em Conta
     H*==================================================================
     FTBADCPF   UF A E           K DISK    PREFIX(ADC_)
     FTBADCAF   IF   E           K DISK    PREFIX(ADC_)
     FFPRDE112  CF   E             WORKSTN PREFIX(ECR_)
     F                                     INFDS(INFD#KEYS)
     F*==========================================================
     F* Indicadores  Descrição
     F* -----------  --------------------------------------------
     F*      45      Condicionar campos com Input/Output
     F*      52      DSP
     F*      53      CLR
     F*      54      END
     D*==========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SGRLO013
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SCATO010
      /Copy QRPGLESRC,SSECO010

     D*==> Estrutura de dados do Registo
     D RBASEDIO      E DS                  ExtName(GBASEDIO)
     D                                     Prefix(OR_)
     D                                     BASED(WPtrData)
     D
     D*==> Estrutura de dados do Registo
     D RBASEDIA      E DS                  ExtName(GBASEDIA)
     D                                     Prefix(AU_)
     D                                     BASED(WPtrData)
     D
     D*==> Parâmetros de Entrada
     D WCodProduto     S              2S 0
     D WDatInicio      S              8S 0
     D WPtrOR          S               *
     D WModo           S               N
     D WTipHerdeiro    S              1A
     D
     D*==> Campos de Trabalho
     D WIcOrigem       S              1A
     D WDatSelect      S              8S 0
     D WIcDisplay      S               N   INZ(*ON)
     D WRow            S              3S 0
     D WCol            S              3S 0
     D WSomQuotPart    S              3S 0
     D WIcNomina       S              1A
     D WNofunction     S             10A
     D WQtdePessoas    S              5S 0
     D WSessScr        S              3S 0
     D
     D KTE#QtdePes     S              5S 0
     C*========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCodProduto
     C                   PARM                    WDatInicio
     C                   PARM                    WPtrOR
     C                   PARM                    WModo
     C
      /FREE

       //------------------------------------
       // Validar o acesso a esta função
       //------------------------------------
       IF NOT $ValAccess(S#PGM:'':99999);
          *INLR = *ON;
          RETURN;
       ENDIF;

       //------------------------------------
       // Formatar parâmetros recebidos
       //------------------------------------
       WPtrData   = WPtrOR;
       WDatSelect = WDatInicio;
       WIcDisplay = WModo;

       //------------------------------------
       // Preparar Controlo do ecrã
       //------------------------------------
       WSessScr = $AddScreen('FPRDE112':'RPRDE11205');
       $AddScPFld(%ADDR(ECR_F#01):'CDIBAN');
       $AddScPFld(%ADDR(ECR_F#02):'NUNIB');
       $AddScPFld(%ADDR(ECR_F#03):'CDADC');
       $AddScCoord(%ADDR(ECR_CM#ROW):%ADDR(ECR_CM#COL));
       $AddScMsg(%ADDR(ECR_ERR#MSG));

       EXSR Parametros;

       $RmvScreen(WSessScr);
       *INLR = *ON;
       RETURN;

       //==================================================================
       // Subrotina.: LoadParms
       // Objectivo.: Carregar parametros da Apólice
       //==================================================================
       BEGSR LoadParms;
          ECR_RAMO = OR_RAMO;
          ECR_APOL = OR_APOL;
          ECR_RMDESC = $GetDescRamo(OR_Ramo);
          ECR_NSEG     = OR_NSEG;
          ECR_SEG      = OR_SEG;
          MONITOR;
             ECR_NUNIB    = %DEC(OR_NIB:21:0);
          ON-ERROR;
             ECR_NUNIB    = *ZEROS;
          ENDMON;
          IF (ECR_CDIBAN = *BLANKS);
             ECR_CDIBAN = 'PT50'+%TRIM(
                           %EDITW(ECR_NUNIB:'0                     '));
          ENDIF;
          IF (OR_RAMO = 50);
             ECR_CDADC = AU_ADC;
          ELSE;
             ECR_CDADC = OR_ADC;
          ENDIF;

          ECR_CDSITUA = *BLANKS;
          ECR_DSSITUA = *BLANKS;
          ECR_DTSITUA = *ZEROS;
          CHAIN(N) (ECR_CDADC) TBADCP;
          IF (%FOUND());
             ECR_CDSITUA = ADC_TPSIT;
             ECR_DSSITUA =  $GetDescricao(79:ADC_TPSIT);
             CHAIN (ECR_CDADC) TBADCA;
             IF (%FOUND());
                ECR_DTSITUA = ADC_TADATA;
             ENDIF;
          ENDIF;
       ENDSR;

       //==================================================================
       // Subrotina.: Parametros
       // Objectivo.: Definir parametros para o Seguro
       //==================================================================
       BEGSR Parametros;
          EXSR LoadParms;
          WNoFunction = 'RPRDE11205';
          *IN50 = *OFF;

          DOW (NOT *IN03 AND NOT *IN12);
             DS#HEADER   = $getHeader(S#PGM:WNoFunction);
             IF (WIcDisplay);
                DS#FOOTER   = $getFooter(S#PGM:WNoFunction:
                                        '000001000000000000001000');
             ELSE;
                DS#FOOTER   = $getFooter(S#PGM:WNoFunction:*BLANKS);
             ENDIF;
             ECR_S#PGM   = S#PGM;
             WRITE     RPRDE11200;
             WRITE     RPRDE11299;
             EXFMT RPRDE11205;
             IF ($ValidKey(Func#Key:KEY#F21) AND NOT WIcDisplay);
                WNoFunction = 'RPRDE11221';
                *IN50 = *ON;
             ELSEIF ($ValidKey(Func#Key:KEY#F22) AND NOT WIcDisplay);

             ELSEIF ($ValidKey(Func#Key:KEY#F04) AND *IN50);
                WRow = ECR_CM#CROW;
                WCol = ECR_CM#CCOL;
                EXSR RunPopUp;
             ELSEIF (Func#Key=KEY#F12) AND *IN50;
                *IN50 = *OFF;
                WNoFunction = 'RPRDE11205';
                *IN12 = *OFF;
             ELSEIF (Func#Key=KEY#Enter);
                IF (*IN50);
                   EXSR Gravar;
                   IF ($GetScStatus() = 'O');
                      *IN50 = *OFF;
                      WNoFunction = 'RPRDE11205';
                      *IN12 = *OFF;
                   ENDIF;
                ELSE;
                   LEAVE;
                ENDIF;
             ENDIF;
          ENDDO;
          *IN12 = *OFF;
       ENDSR;

       //============================================================
       // Subrotina..: RunPopUp
       // Objectivo..: Apresentar lista de valores possíveis para o
       //              campo.
       //============================================================
       BEGSR RunPopUp;
          MONITOR;
             IF (ECR_CM#NMFLD = 'TPRISC');
             // ECR_TPRISC = $runPopUp(75:ECR_TPRISC);
             // ECR_DSRIS = $GetDescricao(75:ECR_TPRISC);
             ENDIF;
          ON-ERROR;
          ENDMON;
          ECR_CM#ROW = WRow;
          ECR_CM#COL = WCol;
       ENDSR;

       //==================================================================
       // Subrotina.: Gravar
       // Objectivo.: Gravar os Parametros da Apólice
       //==================================================================
       BEGSR Gravar;
          EXSR Validar;
          IF ($GetScStatus() = 'O');
             OR_NIB = %EDITW(ECR_NUNIB:'                     ');
             IF (OR_RAMO = 50);
                AU_ADC = ECR_CDADC;
             ELSE;
                OR_ADC = ECR_CDADC;
             ENDIF;
          ENDIF;
       ENDSR;

       //==================================================================
       // Subrotina.: Validar
       // Objectivo.: Validar os registos inseridos
       //==================================================================
       BEGSR Validar;
          $InzScStatus();                      // Inicalizar campos em controlo
          IF (ECR_NUNIB = *ZEROS);
             $SetScError('NUNIB':'PRD0XXX');
          ENDIF;
       ENDSR;
      /END-FREE
