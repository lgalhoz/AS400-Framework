     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PPRDO112
     H* Descrição.: Manutenção de Intruções de Débito em Conta
     H*------------------------------------------------------------------
     H* A2. Não deve permitir a criação de um novo ADC, se existir um
     H*     activo para a Apólice.
     H*------------------------------------------------------------------
     H* A3. Quando alterar a ADC e se esta estiver ainda pendente na IDD
     H*     então devel alterar o NIB e a ADC na instrução.
     H*==================================================================
     FTBIDDPF   UF   E           K DISK    PREFIX(IDD_) COMMIT
     FFCOBP220  UF A E           K DISK    PREFIX(ADC_) COMMIT
     FFCOBP221  UF A E           K DISK    PREFIX(ADC_) COMMIT
     FFCOBP222  UF A E           K DISK    PREFIX(ADC_) COMMIT
     FFCOBP22001IF   E           K DISK    PREFIX(ADC_)
     F                                     RENAME(RCOBP220:RCOBP22001)
     FFCOBP22002IF   E           K DISK    PREFIX(ADC_)
     F                                     RENAME(RCOBP220:RCOBP22002)
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
      /Copy QRPGLESRC,SENTO010

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
     D NIB             DS
     D  ECR_NUNIB              1     21S 0
     D  WCdBanco               1      4A
     D  WNIBAlfa               1     21A
     D
     D REC#            S              4S 0
     D WIcOrigem       S              1A
     D WMsgErro        S              7A
     D WDatSelect      S              8S 0
     D WIcDisplay      S               N   INZ(*ON)
     D WRow            S              3S 0
     D WCol            S              3S 0
     D WSomQuotPart    S              3S 0
     D WIcNomina       S              1A
     D WNofunction     S             10A
     D WOptions        S             24A
     D WIcInsert       S               N
     D WIcChange       S               N
     D WSelected       S               N
     D WSessScr        S              3S 0
     D WIdcRet         S              2S 0
     D WKeysTohide     S             24A   INZ(*ZEROS)
     D WOptTohide      S             24A   INZ(*ZEROS)
     D WCdOpera        S              3S 0
     D WSitADC         S              1A
     D WDescricao      S             40A
     D WNuADC          S                   LIKE(ADC_NUNADC)
     D WNuNIB          S                   LIKE(ADC_NUNIB)
     D WNuIBAN         S                   LIKE(ADC_NUIBAN)
     D WIdx            S              3S 0
     D
     D KTE#ADC         S              5S 0
     D KTE#IDD         S              5S 0
     D
     D*==> Declaração de Constantes
     D ADC#SemEfeito   C                   '3'
     D ADC#Cancela     C                   '5'
     D ADC#Altera      C                   '6'
     D ADC#Regista     C                   '1'
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
       // Formatar parâmetros recebidos
       //------------------------------------
       WPtrData     = WPtrOR;
       WDatSelect   = WDatInicio;
       IF (OR_RAMO = 50) OR (OR_RAMO = 51);
          IF (AU_NIB <> *BLANKS);
             WNIBAlfa = AU_NIB;
          ELSE;
             ECR_NUNIB = *ZEROS;
          ENDIF;
       ELSE;
          IF (OR_NIB <> *BLANKS);
             WNIBAlfa = OR_NIB;
          ELSE;
             ECR_NUNIB = *ZEROS;
          ENDIF;
       ENDIF;

       //-----------------------------------------------------
       // Definir o modo de execução em função do chamador
       // ou existência de registos pendentes no Banco
       //-----------------------------------------------------
       WKeysTohide = *ZEROS;
       WOptTohide  = *ZEROS;
       IF (WModo); // Modo de Consulta
          %SUBST(WKeysTohide:6:1)  = '1'; // Criar uma nova ADC
          %SUBST(WKeysTohide:21:1) = '1'; // Alterar uma ADC existente
          %SUBST(WKeysTohide:22:1) = '1'; // Desactivar uma ADC
       ELSE;
          //----------------------------------------------
          // Se ADC Activa, então não permite a criação
          //----------------------------------------------
          KTE#ADC = $SETFILES('FCOBP220':*BLANKS);
          $SetFields('CDRAMO');
          $SetCriteria('(CDRAMO = '''+%CHAR(OR_RAMO)+''') AND +
                        (NUAPOL = '''+%CHAR(OR_APOL)+''') AND +
                        ((SITADC =''1'') OR (SITADC = ''2'') OR +
                         (SITADC =''4'') OR (SITADC = ''6''))');
          IF ($GetRecord() > *ZEROS);
             %SUBST(WKeysTohide:6:1) = '1'; // Criar uma nova ADC
          ENDIF;
          $RmvDBSession(KTE#ADC);
       ENDIF;
       $KeysToOmit(S#PGM:'RPRDE11203':WKeysTohide);
       $KeysToOmit(S#PGM:'RPRDE11205':WKeysTohide);

       //------------------------------------
       // Preparar Controlo do ecrã
       //------------------------------------
       WSessScr = $AddScreen('FPRDE112':'RPRDE11205');
       $AddScPFld(%ADDR(ECR_F#01):'CDIBAN');
       $AddScPFld(%ADDR(ECR_F#02):'NUNIB');
       $AddScPFld(%ADDR(ECR_F#03):'NUADC');
       $AddScPFld(%ADDR(ECR_F#04):'ISPT');
       $AddScPFld(%ADDR(ECR_F#05):'CDBANCO');
       $AddScCoord(%ADDR(ECR_CM#ROW):%ADDR(ECR_CM#COL));
       $AddScMsg(%ADDR(ECR_ERR#MSG));

       //------------------------------------------------------
       // Sessão das ADC's
       //------------------------------------------------------
       KTE#ADC = $SETFILES('FCOBP220 A INNER JOIN FCOBP221 B ON +
                           (A.CDNADC = B.CDNADC)':*BLANKS);
       $SetCriteria('(A.CDRAMO = '''+%CHAR(OR_RAMO)+''') AND +
                     (A.NUAPOL = '''+%CHAR(OR_APOL)+''')');
       $SetFields('FCOBP220':'A');
       $SetFields('FCOBP221':'B':*OFF);
       $addCriteria('ADC........:':'A.NUNADC':'GE');
       $SetLstFld('A.NUNADC':'Nº.ADC':10:'D':'A');
       $SetLstFld('A.SITADC':'Sit.':30:'E');
       $SetLstFld('B.NUIBAN':'Identificação Bancária':26:'D');
       $SetLstFld('B.CDMOV':'Movimento.':15:'D');
       $SetLstFld('B.DATALT':'Data':10:'D':'D');
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

       $SetDBSession(KTE#ADC); // Repor Sessão das ADC's
       EXSR ShowAllADC;

       $RmvDBSession(KTE#ADC);
       $RmvDBSession(KTE#IDD);
       $RmvScreen(WSessScr);
       *INLR = *ON;
       RETURN;

       //========================================================
       // Subrotina.: ShowAllADC
       // Objectivo.: Mostrar todas as ADC's da Apólice
       //========================================================
       BEGSR ShowAllADC;
          ECR_RAMO = OR_RAMO;
          ECR_APOL = OR_APOL;
          ECR_SEG  = OR_SEG;
          DOW (NOT *IN03 AND NOT *IN12);
             EXSR getRecords;
             WOptions = *BLANKS;
             IF ($GetDBSession() = KTE#ADC);
                WNoFunction = 'RPRDE11203'; // Listar ADC's
             ELSE;
                WNoFunction = 'RPRDE11204'; // Listar Recibos
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
                WIcChange  = *OFF;
                EXSR ShowDetADC;
                *IN12 = *OFF;
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
       // Subrotina.: GetRecords
       // Objectivo.: Obter todos os registos de tabelas disponív
       //========================================================
       BEGSR getRecords;
          REC#      = *ZEROS;   // Controlo de Registos da Lista
          *In53     = *On;      // Limpar Lista de Ecrã
          *In52     = *On;      // Mostrar a Lista de Ecrã
          WRITE RPRDE11203;     // Limpar Lista de Funções
          *In53     = *Off;     // Repor Indicador
          *In52     = *Off;     // Mostrar a Lista de Ecrã
          IF ($GetPage(Func#Key) > *ZEROS);
             DOW (NOT $IsEOF());
                //------------------------------------------------
                // Escreve os Registo da Listagem
                //------------------------------------------------
                ECR_OPTION   = *BLANKS;
                ECR_LSTHDR = $GetLstHdr();
                ECR_LSTDET = $GetLstLine();
                ECR_CDADC  = $GetFieldNum('A.CDNADC');
                ECR_NUADC  = $GetFieldNum('A.NUNADC');
                IF ($GetDbSession() = KTE#ADC);
                   ECR_CDNIB  = $GetFieldNum('B.CDNIB');
                   WSitADC = $GetField('A.SITADC');
                   WDescricao = $GetDescricao(87:WSitADC);
                   %SUBST(ECR_LSTDET:12:30) = WSitADC+' '+' '+%TRIM(WDescricao);
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
                WIcInsert  = *OFF;
                EXSR ShowDetADC;
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
          $InzScStatus(); // Inicalizar campos em controlo
          EXSR LoadParms;
          WNoFunction = 'RPRDE11205';

          %SUBST(WKeysTohide:21:1) = '1'; // Alterar uma ADC existente
          %SUBST(WKeysTohide:22:1) = '1'; // Desactivar uma ADC
          IF ((ECR_CDSITUA='4') OR (ECR_CDSITUA='1') OR (ECR_CDSITUA='6')) AND
             (NOT WModo);
             %SUBST(WKeysTohide:22:1) = '0'; // Desactivar uma ADC
             %SUBST(WKeysTohide:21:1) = '0'; // Alterar uma ADC existente
          ENDIF;
          $KeysToOmit(S#PGM:WNoFunction:WKeysTohide);
          DOW (NOT *IN03 AND NOT *IN12);
             IF (NOT WIcInsert);
                $SetScFldProt('NUADC');
             // $SetScFldProt('ISPT');
             ENDIF;
             WOptions  = *ZEROS;
             DS#HEADER = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER = $getFooter(S#PGM:WNoFunction);
             ECR_S#PGM = S#PGM;
             WRITE     RPRDE11200;
             WRITE     RPRDE11299;
             EXFMT RPRDE11205;
             IF ($ValidKey(Func#Key:KEY#F21));
                WNoFunction = 'RPRDE11221';
                WIcDisplay = *OFF;
                WIcInsert  = *OFF;
                WIcChange  = *ON;
                *IN50 = *ON;
             ELSEIF ($ValidKey(Func#Key:KEY#F22));
                IF ($ShowDialog('COB0002':*BLANKS:'NS') = 'S');
                   EXSR Cancelar;
                   LEAVESR;
                ENDIF;
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
                      WIcInsert  = *OFF;
                      WIcChange  = *OFF;
                      WIcDisplay = *ON;
                   ENDIF;
                ELSE;
                   LEAVE;
                ENDIF;
             ENDIF;
          ENDDO;
          *IN50 = *OFF;
          WIcDisplay = *ON;
          WIcInsert  = *OFF;
          WIcChange  = *OFF;
          *IN12 = *OFF;
       ENDSR;

       //==================================================================
       // Subrotina.: Cancelar
       // Objectivo.: Cancelar uma ADC
       //==================================================================
       BEGSR Cancelar;
          //------------------------------------------------
          // Não aceita cancelar uma ADC se este tiver
          // uma instrução IDD enviada ao Banco
          //------------------------------------------------
          EXSR ValidarIDD;
          IF (WMsgErro <> *BLANKS);
             LEAVESR;
          ENDIF;

          //------------------------------------------------
          // Criar a Operação de Anulação
          //------------------------------------------------
          ADC_NUNADC  = ECR_NUADC;
          ADC_CDNIB   = ECR_CDNIB;
          WCdOpera    = $GetNextKey('FCOBP222':'NUOPERA':
                        '(CDNADC = '''+%CHAR(ECR_CDADC)+''') AND +
                         (CDNIB = '''+%CHAR(ECR_CDNIB)+''')');
          ADC_NUOPERA = WCdOpera;
          ADC_DATALT  = $DateToInt(%DATE());
          ADC_NOFILE  = *ZEROS;
          ADC_CDRET   = *ZEROS;
          ADC_CDSIT   = '5';
          ADC_CDUSER  = S#USR;
          ADC_DTAPROV = *ZEROS;
          ADC_ADCSEPA = ' ' + %TRIM(%EDITW(ADC_NUNADC:'0           '));
          WRITE RCOBP222;

          CHAIN (ECR_CDADC:ECR_CDNIB) RCOBP221;
          IF (NOT %FOUND(FCOBP221));
             ROLBK;
             $ShowDialog('COB0042');
             LEAVESR;
          ENDIF;

          ADC_DATALT  = $DateToInt(%DATE());
          ADC_CDSIT   = '5';
          ADC_CDUSER  = S#USR;
          ADC_NUOPERA = WCdOpera;
          UPDATE RCOBP221 %FIELDS(ADC_DATALT:ADC_CDSIT:
                                  ADC_CDUSER:ADC_NUOPERA);

          IF (NOT WIcChange);
             CHAIN (ECR_CDADC) RCOBP220;
             IF (NOT %FOUND(FCOBP220));
                ROLBK;
                $ShowDialog('COB0043');
                LEAVESR;
             ENDIF;
             ADC_CDUSER = S#USR;
             ADC_SITADC = ADC#Cancela;
             UPDATE RCOBP220 %FIELDS(ADC_SITADC:ADC_CDUSER);

             IF (OR_RAMO = 50) OR (OR_RAMO = 51);
                AU_NIB = *BLANKS;
                AU_ADC = *ZEROS;
             ELSE;
                OR_NIB = *BLANKS;
                OR_ADC = *ZEROS;
             ENDIF;
             %SUBST(WKeysTohide:22:1) = '1'; // Desactivar uma ADC
             $KeysToOmit(S#PGM:'RPRDE11205':WKeysTohide);
             %SUBST(WKeysTohide:6:1)  = '0'; // Passa a poder inserir uma ADC
             $KeysToOmit(S#PGM:'RPRDE11203':WKeysTohide);
          ENDIF;
       ENDSR;

       //==================================================================
       // Subrotina.: ValidarIDD
       // Objectivo.: Validar se existe alguma intrução
       //==================================================================
       BEGSR ValidarIDD;
          $SetDBSession(KTE#IDD);
          $SetCriteria('(IDNADC = '''+%CHAR(ECR_NUADC)+''')');
          WIdx = 1;
          DOW ($GetRecord(WIdx) > *ZEROS);
             IF ($GetField('IDSIT') < '4');
                WMsgErro = 'COB0116';
                $ShowDialog(WMsgErro:$GetField('IDREC'));
                LEAVE;
             ENDIF;
             WIdx += 1;
          ENDDO;
          $SetDBSession(KTE#ADC);
          ECR_DSCRITER = $getCritText();
       ENDSR;

       //==================================================================
       // Subrotina.: LoadParms
       // Objectivo.: Carregar ShowDetADC da Apólice
       //==================================================================
       BEGSR LoadParms;
          WNuADC      = *ZEROS;
          WNuNIB      = *ZEROS;
          WNuIBAN     = *BLANKS;
          ECR_NUNIB   = *ZEROS;
          ECR_ADCSEPA = *BLANKS;
          ECR_CDIBAN  = *BLANKS;
          ECR_CDSITUA = *BLANKS;
          ECR_DSSITUA = *BLANKS;
          ECR_DTSITUA = *ZEROS;
          ECR_CDBANCO = *ZEROS;
          ECR_DSBANCO = *BLANKS;
          ECR_CDSWIFT = *BLANKS;
          ECR_ISPT    = 'S';
          IF (WIcInsert = *OFF);
             *IN50 = *OFF;
             CHAIN(N) (ECR_CDADC) RCOBP220;
             IF (%FOUND(FCOBP220));
                WNuADC      = ADC_NUNADC;
                ECR_NUADC   = ADC_NUNADC;
                ECR_ADCSEPA = ADC_ADCSEPA;
                ECR_DTSITUA = ADC_DTAPROV;
                CHAIN(N) (ECR_CDADC:ECR_CDNIB) RCOBP221;
                IF (%FOUND());
                   WNuNIB      = ADC_NUNIB;
                   WNuIBAN     = ADC_NUIBAN;
                   ECR_NUNIB   = ADC_NUNIB;
                   ECR_DTSITUA = $CvtData(ADC_DATALT:'YYMD':'DMYY');
                   ECR_CDSITUA = ADC_CDSIT;
                   ECR_DSSITUA = $GetDescricao(87:ECR_CDSITUA);
                   ECR_CDBANCO = ADC_CDBANCO;
                   ECR_CDIBAN  = ADC_NUIBAN;
                   IF (%SUBST(ECR_CDIBAN:1:4) = 'PT50') OR
                        (ECR_CDIBAN = *BLANKS);
                      ECR_ISPT = 'S';
                   ELSE;
                      ECR_ISPT = 'N';
                   ENDIF;
                   ECR_DSBANCO = $GetDescricao(45:
                                       %TRIM(%EDITW(ECR_CDBANCO:'0    ')));
                   ECR_CDSWIFT = $GetCodigo(45:
                                       %TRIM(%EDITW(ECR_CDBANCO:'0    ')));
                ENDIF;
             ENDIF;
          ELSE;
             *IN50 = *ON;
             ECR_NUADC   = *ZEROS;
             ECR_NUNIB   = *ZEROS;
             ECR_CDIBAN  = *BLANKS;
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
             IF (WIcInsert);
                CHAIN(N) (ECR_RAMO:ECR_APOL:ECR_NUADC) RCOBP22001;
                IF (NOT %FOUND(FCOBP22001));
                   ADC_CDNADC  = $GetNextKey('FCOBP220':'CDNADC');
                   ADC_NUNADC  = ECR_NUADC;
                   ADC_CDRAMO  = OR_RAMO;
                   ADC_NUAPOL  = OR_APOL;
                   ADC_SITADC  = ADC#Regista;
                   ADC_CDUSER  = S#USR;
                   ADC_DTAPROV = *ZEROS;
                   ADC_ADCSEPA = ' ' + %TRIM(%EDITW(ADC_NUNADC:'0           '));
                   ADC_CDNIB   = 1;
                   WRITE RCOBP220;
                ELSE;
                   CHAIN (ADC_CDNADC) RCOBP220;
                   ADC_SITADC  = ADC#Altera;
                   ADC_CDUSER  = S#USR;
                   ADC_CDNIB  += 1;
                   UPDATE RCOBP220 %FIELDS(ADC_SITADC:ADC_CDUSER:ADC_CDNIB);
                ENDIF;

                ADC_NUNADC  = ECR_NUADC;
                ADC_DATALT  = $DateToInt(%DATE());
                ADC_NUNIB   = ECR_NUNIB;
                ADC_NUIBAN  = ECR_CDIBAN;
                ADC_CDBANCO = ECR_CDBANCO;
                IF (ECR_ISPT = 'S');
                   ADC_NUIBAN = 'PT50'+%TRIM(
                                 %EDITW(ECR_NUNIB:'0                     '));
                   ADC_CDBANCO = %DEC(WCdBanco:4:0);
                ENDIF;
                ADC_CDMOV  = 'T';
                ADC_CDSIT  = '1';
                ADC_CDUSER = S#USR;
                ADC_NUOPERA= 1;
                WRITE RCOBP221;

                ADC_NUNADC = ECR_NUADC;
                ADC_NUOPERA= 1;
                ADC_DATALT = $DateToInt(%DATE());
                ADC_NOFILE = *ZEROS;
                ADC_CDRET  = *ZEROS;
                ADC_CDSIT  = '1';
                ADC_CDUSER = S#USR;
                ADC_DTAPROV= $DateToInt(%DATE());
                WRITE RCOBP222;
                EXSR AlteraBase;

                %SUBST(WKeysTohide:6:1)  = '1'; // F06 desactivado
                $KeysToOmit(S#PGM:'RPRDE11203':WKeysTohide);
             ELSEIF (WicChange);
                IF (WNuADC <> ECR_NUADC) OR (WNuNIB <> ECR_NUNIB)
                  OR (WNuIBAN <> ECR_CDIBAN);
                   //---------------------------------------------
                   // Cancelar NIB anterior
                   //---------------------------------------------
                   EXSR Cancelar;
                   CHAIN (ECR_CDADC) RCOBP220;
                   IF (NOT %FOUND(FCOBP220));
                      ROLBK;
                      $ShowDialog('COB0043');
                      LEAVESR;
                   ENDIF;
                   ADC_CDUSER = S#USR;
                   ADC_SITADC = ADC#Altera;
                   ADC_CDNIB  = $GetNextKey('FCOBP221':'CDNIB':
                              '(CDNADC = '''+%CHAR(ECR_CDADC)+''')');
                   UPDATE RCOBP220 %FIELDS(ADC_CDNIB:ADC_SITADC:ADC_CDUSER);

                   //---------------------------------------------
                   // Criar novo NIB
                   //---------------------------------------------
                   ADC_NUNADC = ECR_NUADC;
                   ADC_DATALT = $DateToInt(%DATE());
                   ADC_NUNIB  = ECR_NUNIB;
                   ADC_NUIBAN = ECR_CDIBAN;
                   ADC_CDBANCO = ECR_CDBANCO;
                   IF (ECR_ISPT = 'S');
                      ADC_NUIBAN = 'PT50'+%TRIM(
                                    %EDITW(ECR_NUNIB:'0                     '));
                      ADC_CDBANCO = %DEC(WCdBanco:4:0);
                   ENDIF;
                   ADC_CDMOV  = 'T';
                   ADC_CDSIT  = '1';
                   ADC_CDUSER = S#USR;
                   ADC_NUOPERA= 1;
                   WRITE RCOBP221;

                   ADC_NUNADC = ECR_NUADC;
                   ADC_CDNIB  = ADC_CDNIB;
                   ADC_NUOPERA= 1;
                   ADC_DATALT = $DateToInt(%DATE());
                   ADC_NOFILE = *ZEROS;
                   ADC_CDRET  = *ZEROS;
                   ADC_CDSIT  = '1';
                   ADC_CDUSER = S#USR;
                   ADC_DTAPROV= $DateToInt(%DATE());
                   WRITE RCOBP222;

                   EXSR AlteraBase;
                   %SUBST(WKeysTohide:21:1)  = '0'; // F21 activado
                   %SUBST(WKeysTohide:22:1)  = '0'; // F22 activado
                   $KeysToOmit(S#PGM:'RPRDE11203':WKeysTohide);
                ENDIF;
             ENDIF;
          ENDIF;
       ENDSR;

       //==================================================================
       // Subrotina.: AlteraBase
       // Objectivo.: Alterar o Base de Apólices
       //==================================================================
       BEGSR AlteraBase;
          IF (OR_RAMO = 50) OR (OR_RAMO = 51);
             AU_NIB = WNIBAlfa;
             AU_ADC = ECR_NUADC;
          ELSE;
             OR_ADC = ECR_NUADC;
             OR_NIB = WNIBAlfa;
          ENDIF;
       ENDSR;

       //==================================================================
       // Subrotina.: Validar
       // Objectivo.: Validar os registos inseridos
       //==================================================================
       BEGSR Validar;
          $InzScStatus(); // Inicalizar campos em controlo

          IF (ECR_ISPT = 'S');
             //-----------------------------------------------
             // Validar o NIB para as contas nacionais
             //-----------------------------------------------
             ECR_CDBANCO = %DEC(WCdBanco:4:0);
             IF (ECR_NUNIB = *ZEROS);
                $SetScError('NUNIB':'COB0005');
             ELSE;
                WMsgErro = $CheckNIB(ECR_NUNIB);
                IF (WMsgErro <> *BLANKS);
                   $SetScError('NUNIB':WMsgErro);
                ENDIF;
             ENDIF;

             //-----------------------------------------------
             // Validar se o Banco existe e tem Código Swift
             //-----------------------------------------------
             IF (NOT $valCodigo(45:WCdBanco));
                $SetScError('CDBANCO':'COB0016');
             ELSE;
                IF ($getCodigo(45:WcdBanco) = '????') OR
                    ($getCodigo(45:WcdBanco) = *BLANKS);
                   $SetScError('CDBANCO':'COB0015');
                ENDIF;
             ENDIF;
          ELSEIF (ECR_ISPT = 'N');
             //-----------------------------------------------
             // Validar o IBAN para contas internacionais
             //-----------------------------------------------
             IF (ECR_CDIBAN = *BLANKS);
                $SetScError('CDIBAN':'COB0044');
             ENDIF;

             //-----------------------------------------------
             // Validar o Banco para contas internacionais
             //-----------------------------------------------
             IF (NOT $valCodigo(45:%TRIM(%EDITW(ECR_CDBANCO:'0    '))));
                $SetScError('CDBANCO':'COB0016');
             ELSE;
                ECR_CDSWIFT = $getCodigo(45:
                                    %TRIM(%EDITW(ECR_CDBANCO:'0    ')));
                IF (ECR_CDSWIFT = '????') OR (ECR_CDSWIFT = *BLANKS);
                   $SetScError('CDBANCO':'COB0015');
                ENDIF;
             ENDIF;
          ELSE;
             $SetScError('ISPT':'COB0045');
          ENDIF;

          //-----------------------------------------------
          // Validar a ADC
          //-----------------------------------------------
          IF (ECR_NUADC = *ZEROS);
             $SetScError('NUADC':'COB0003');
          ELSE;
             WMsgErro = $CheckADC(ECR_NUADC);                   //-------------------------------
             IF (WMsgErro <> *BLANKS);                          // Valida estrutura da ADC
                $SetScError('NUADC':WMsgErro);                  //-------------------------------
             ELSE;
                SETLL (OR_RAMO:OR_APOL) RCOBP22001;              //-------------------------------------
                READE (OR_RAMO:OR_APOL) RCOBP22001;              // Validar se já existe uma ADC activa
                DOW (NOT %EOF(FCOBP22001));                      // para a Apólice
                   IF (ADC_SITADC <> ADC#SemEfeito) AND            //-----------------------------
                    (ADC_SITADC <> ADC#Cancela) AND
                    (ECR_NUADC <> ADC_NUNADC);
                      $SetScError('NUADC':'COB0017');
                      LEAVE;
                   ENDIF;
                   READE (OR_RAMO:OR_APOL) RCOBP22001;
                ENDDO;

                SETLL (ECR_NUADC) RCOBP22002;
                READE (ECR_NUADC) RCOBP22002;
                DOW (NOT %EOF(FCOBP22002));                                  //-------------------------------------
                   IF ((ADC_CDRAMO <> OR_RAMO) OR (ADC_NUAPOL <> OR_APOL))   // Validar se a ADC está associada a
                    AND (ADC_SITADC <> ADC#Cancela)                          // outra Apólice
                    AND (ADC_SITADC <> ADC#SemEfeito);                       //-------------------------------------
                      $SetScError('NUADC':'COB0014');
                      LEAVE;
                   ENDIF;
                   READE (ECR_NUADC) RCOBP22002;
                ENDDO;
             ENDIF;
          ENDIF;
       ENDSR;
      /END-FREE
