     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PPRDO112
     H* Descrição.: Manutenção de Intruções de Débito em Conta
     H*==================================================================
     FTBADCPF   UF A E           K DISK    PREFIX(ADC_)
     FTBADCAF   IF   E           K DISK    PREFIX(ADC_)
     FFPRDE112  CF   E             WORKSTN SFILE(RPRDE11202:REC#)
     F                                     PREFIX(ECR_)
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
     D REC#            S              4S 0
     D WIcOrigem       S              1A
     D WDatSelect      S              8S 0
     D WIcDisplay      S               N   INZ(*ON)
     D WOrigDisplay    S               N
     D WRow            S              3S 0
     D WCol            S              3S 0
     D WSomQuotPart    S              3S 0
     D WIcNomina       S              1A
     D WNofunction     S             10A
     D WOptions        S             24A
     D WIcInsert       S               N
     D WSelected       S               N
     D WSessScr        S              3S 0
     D WIdcRet         S              2S 0
     D
     D KTE#ADC         S              5S 0
     D KTE#IDD         S              5S 0
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
          $ShowDialog('SEC0001');
          *INLR = *ON;
          RETURN;
       ENDIF;

       //------------------------------------
       // ForCompr parâmetros recebidos
       //------------------------------------
       WPtrData     = WPtrOR;
       WDatSelect   = WDatInicio;
       WOrigDisplay = WModo;

       //------------------------------------------------------
       // Sessão das ADC's
       //------------------------------------------------------
       KTE#ADC = $SETFILES('TBADCPF INNER JOIN TBADCAF ON +
                             (TPNADC = TANADC)':*BLANKS);
       $addCriteria('ADC........:':'TPNADC':'GE':'D');
       $SetFields('TBADCPF':*BLANKS);
       $SetFields('TBADCAF':*BLANKS:*OFF);
       $SetLstFld('TPNADC':'Nº.ADC':10:'D':'A');
       $SetLstFld('TPSIT':'Sit.':30:'E');
       $SetLstFld('TANIB':'Identificação Bancária':26:'D');
       $SetLstFld('TATMOV':'Movimento.':15:'D');
       $SetLstFld('TADATA':'Data':10:'D':'D');
       ECR_DSCRITER = $getCritText();
       $SetPage(12);

       //------------------------------------------------------
       // Sessão das IDD's
       //------------------------------------------------------
       KTE#IDD = $SETFILES('TBIDDPF');
       $addCriteria('Data.......:':'IDDATA':'GE':'D');
       $addCriteria('Recibo.....:':'IDREC':'GE':'D');
       $SetLstFld('IDDATA':'Data':10:'D':'D');
       $SetLstFld('IDREC':'Recibo':9:'D');
       $SetLstFld('IDVAL':'Valor':7:'D');
       $SetLstFld('IDSIT':'Situação':10:'E');
       $SetLstFld('IDRET':'Retorno':10:'E');
       $SetLstFld('IDNIB':'NIB':21:'E');
       $SetPage(12);

       $SetDBSession(KTE#ADC); // Repor Sessão das Funções
       EXSR ShowAllADC;

       $RmvDBSession(KTE#ADC);
       $RmvDBSession(KTE#IDD);
       *INLR = *ON;
       RETURN;

       //========================================================
       // Subrotina.: ShowAllADC
       // Objectivo.: Mostrar todas as ADC's da Apólice
       //========================================================
       BEGSR ShowAllADC;
          ECR_RAMO = OR_RAMO;
          ECR_APOL = OR_APOL;
          ECR_SEG      = OR_SEG;
          $SetCriteria('(TPRAMO = '''+%CHAR(OR_RAMO)+''') AND +
                        (TPAPOL = '''+%CHAR(OR_APOL)+''') AND +
                        ((TATMOV=''T'') OR (TATMOV=''A''))');

          DOW (NOT *IN03 AND NOT *IN12);
             EXSR inzRecords;
             EXSR getRecords;
             IF ($GetDBSession() = KTE#ADC);
                WNoFunction = 'RPRDE11203'; // Listar Componentes
                WOptions = *BLANKS;
                IF (WOrigDisplay);
                   %SUBST(WOptions:6:1) = '1'; // Desactivar F06
                ENDIF;
             ELSE;
                WNoFunction = 'RPRDE11204'; // Listar Operações
                WOptions = *BLANKS;
             ENDIF;
             DS#HEADER   = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER   = $getFooter(S#PGM:WNoFunction:WOptions);
             DS#OPTIONS  = $getOption(S#PGM:WNoFunction:WOptions);
             ECR_S#PGM   = S#PGM;
             ECR_ERR#MSG = *BLANKS;
             WRITE RPRDE11200;
             WRITE RPRDE11299;
             WRITE RPRDE11298;
             EXFMT RPRDE11203;
             IF (Func#Key = KEY#F05) AND ($GetDBSession()=KTE#IDD);
                $ChgCriteria();
                ECR_DSCRITER = $getCritText();
             ELSEIF ($ValidKey(Func#Key:KEY#F06) AND ($GetDBSession()=KTE#ADC));
                WIcDisplay = *OFF;
                WIcInsert  = *ON;
                EXSR ShowDetADC;
                *IN12 = *OFF;
                WIcDisplay = *ON;
                WIcInsert  = *OFF;
             ELSEIF (Func#Key = KEY#F12);
                IF ($GetDBSession() = KTE#IDD);
                   $SetDBSession(KTE#ADC);
                   ECR_DSCRITER = $getCritText();
                   *IN12 = *OFF;
                ENDIF;
             ELSEIF (Func#Key = KEY#Enter);
                EXSR SelRec;
                *IN12 = *OFF;
             ENDIF;
          ENDDO;
       ENDSR;

       //========================================================
       // Subrotina.: InzRecTxt
       // Objectivo.: Inicializar a lista de registos
       //========================================================
       BEGSR inzRecords;
          REC#      = *ZEROS;   // Controlo de Registos da Lista
          *In53     = *On;      // Limpar Lista de Ecrã
          *In52     = *On;      // Mostrar a Lista de Ecrã
          WRITE RPRDE11203;     // Limpar Lista de Funções
          *In53     = *Off;     // Repor Indicador
          *In52     = *Off;     // Mostrar a Lista de Ecrã
       ENDSR;

       //========================================================
       // Subrotina.: GetRecords
       // Objectivo.: Obter todos os registos de tabelas disponív
       //========================================================
       BEGSR getRecords;
          IF ($GetPage(Func#Key) > *ZEROS);
             DOW (NOT $IsEOF());
                //------------------------------------------------
                // Escreve os Registo da Listagem
                //------------------------------------------------
                ECR_OPTION   = *BLANKS;
                ECR_LSTHDR = $GetLstHdr();
                ECR_LSTDET = $GetLstLine();
                ECR_NUADC  = $GetFieldNum('TPNADC');
                IF ($GetDbSession() = KTE#ADC);
                  %SUBST(ECR_LSTDET:12:40) = %TRIM($GetField('TPSIT')) + ' ' +
                                             $GetDescricao(79:
                                              %TRIM($GetField('TPSIT')));
                ELSE;
                  %SUBST(ECR_LSTDET:30:10) = $GetDescricao(78:
                                             %TRIM($GetField('IDSIT')));
                  WIdcRet = $GetFieldNum('IDCRET');
                  %SUBST(ECR_LSTDET:41:10) = $GetDescricao(81:
                                             %TRIM(%EDITW(WIdcRet:'0  ')));
                ENDIF;
                REC# += 1;
                WRITE RPRDE11202;
                *IN52 = *ON;       // Encontrou registos
                $getNext();
             ENDDO;
          ENDIF;
          *IN54 = $IsLastPage();   // Indicação de Fim de Página
       ENDSR;

       //========================================================
       // Subrotina.: SelRec
       // Objectivo.: Seleccionar os registos
       //========================================================
       BEGSR SelRec;
          WSelected = *OFF;
          READC RPRDE11202;
          DOW (NOT %EOF());
             IF ($ValidOption(ECR_OPTION:'2'));
                EXSR ShowDetADC;
                WIcDisplay = *ON;
             ELSEIF ($ValidOption(ECR_OPTION:'5'));
                $SetDBSession(KTE#IDD);
                $SetCriteria('(IDNADC = '''+%CHAR(ECR_NUADC)+''')');
                ECR_DSCRITER = $getCritText();
                LEAVESR;
             ENDIF;
             READC RPRDE11202;
          ENDDO;
          IF (NOT WSelected AND WIcDisplay);
             IF (ECR_CRITERIA <> *BLANKS);
                $SetCriteria(ECR_CRITERIA:*ON);
             ELSE;
                $SetCriteria(*BLANKS:*ON);
             ENDIF;
          ENDIF;
       ENDSR;

       //==================================================================
       // Subrotina.: ShowDetADC
       // Objectivo.: Definir ShowDetADC para o Seguro
       //==================================================================
       BEGSR ShowDetADC;
          //------------------------------------
          // Preparar Controlo do ecrã
          //------------------------------------
          WSessScr = $AddScreen('FPRDE112':'RPRDE11205');
          $AddScPFld(%ADDR(ECR_F#01):'CDIBAN');
          $AddScPFld(%ADDR(ECR_F#02):'NUNIB');
          $AddScPFld(%ADDR(ECR_F#03):'CDADC');
          $AddScCoord(%ADDR(ECR_CM#ROW):%ADDR(ECR_CM#COL));
          $AddScMsg(%ADDR(ECR_ERR#MSG));

          EXSR LoadParms;
          WNoFunction = 'RPRDE11205';
          *IN50 = *OFF;

          DOW (NOT *IN03 AND NOT *IN12);
             DS#HEADER   = $getHeader(S#PGM:WNoFunction);
             IF (WIcDisplay);
                WOptions  = *BLANKS;
                %SUBST(WOptions:21:1) = '1';
                %SUBST(WOptions:22:1) = '1';
                DS#FOOTER = $getFooter(S#PGM:WNoFunction:WOptions);
             ELSE;
                WOptions  = *BLANKS;
                DS#FOOTER = $getFooter(S#PGM:WNoFunction:WOptions);
             ENDIF;
             ECR_S#PGM   = S#PGM;
             WRITE     RPRDE11200;
             WRITE     RPRDE11299;
             EXFMT RPRDE11205;
             IF ($ValidKey(Func#Key:KEY#F21) AND NOT WIcDisplay);
                WNoFunction = 'RPRDE11221';
                *IN50 = *ON;
             ELSEIF ($ValidKey(Func#Key:KEY#F22) AND NOT WIcDisplay);
                // Eliminar o registo
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
          $RmvScreen(WSessScr);
       ENDSR;

       //==================================================================
       // Subrotina.: LoadParms
       // Objectivo.: Carregar ShowDetADC da Apólice
       //==================================================================
       BEGSR LoadParms;
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
       // Objectivo.: Gravar os ShowDetADC da Apólice
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
