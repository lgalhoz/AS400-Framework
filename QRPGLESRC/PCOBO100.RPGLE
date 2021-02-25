     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PCOBO100
     H* Descrição.: Manutenção de IDD's
     H* Observação: Programa reescrito na nova framework.
     H*
     H*             Criada a constante DAT#Exception, para prever a data
     H*             do sistema forçada.
     H*==================================================================
     FTBIDDPF   UF A E           K DISK    PREFIX(IDD_)
     FFCOBP22002IF   E           K DISK    PREFIX(ADC_)
     F                                     RENAME(RCOBP220:RCOBP22002)
     FFCOBP221  IF   E           K DISK    PREFIX(ADC_)
     FGBASEGRL  IF   E           K DISK    PREFIX(BAS_)
     FGCOBGR    IF   E           K DISK    PREFIX(REC_)
     FSD1RE2    IF   E           K DISK    PREFIX(REC_)
     FFCOBE100  CF   E             WORKSTN SFILE(RCOBE10002:REC#)
     F                                     PREFIX(ECR_)
     F                                     INFDS(INFD#KEYS)
     F*==================================================================*
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SCATO010
      /Copy QRPGLESRC,SSECO010
      /Copy QRPGLESRC,SGRLO013

     D*==> Chamada Dinâmica a funções
     D CallDynamic     PR                  EXTPGM(WDynamic)
     D   CodErro                      7A   Const
     D   NumRecibo                    9S 0 Const Options(*NOPASS)
     D WDynamic        S             10A
     D
     D*==> Campos de Trabalho
     D REC#            S              4S 0
     D KTE#IDD         S              5S 0
     D KTE#IDDQTD      S              5S 0
     D WNoFunction     S             10A
     D WIsAll          S               N
     D WSelected       S               N
     D WIdSit          S              1A
     D WIdUser         S              8A
     D WIddTap         S              8S 0
     D WDatInt         S              8S 0
     D WIdVal          S             13S 2
     D WSessScr        S              3S 0
     D WExtKeys        S             24A
     D WTxtErro        S            250A
     D WIDDTPP         S               D
     D WIDDATA         S               D
     D
     D WCdRamo         S              2S 0
     D WNuApol         S              7S 0
     D WNumNIB         S                   LIKE(ADC_NUNIB)
     D WNumADC         S                   LIKE(ADC_NUNADC)
     D WSitADC         S                   LIKE(ADC_SITADC)
     D WDatSuger       S             10A
     D WCodSeg         S              9S 0
     D WCodMed         S              9S 0
     D WNuRecJM        S              9A
     D
     D WJobDate        S               D
     D WQtdeADC        S              3S 0
     D WCodErro        S              7A
     D WIcModo         S              1A
     D WModoExec       S              1A
     D
     D WDataEmiss      S              8S 0
     D
     D*==> Constantes utilizadas
     D MOD#Consulta    C                   'R'
     D MOD#Inserir     C                   'I'
     D MOD#Confirmar   C                   'C'
     D EXC#Pendente    C                   'P'
     D EXC#Tratada     C                   'T'
     D DAT#Exception   C                   20160104
     D*==================================================================*
      /FREE

       //------------------------------------
       // Validar o acesso a esta função
       //------------------------------------
       IF NOT $ValAccess(S#PGM:'':99999);
          *INLR = *ON;
          RETURN;
       ENDIF;

       //----------------------------------------------
       // Sessão de IDD's
       //----------------------------------------------
       WJobDate = %DATE(UDATE);
       WDatInt = %SUBDT(WJobDate:*D)*1000000+%SUBDT(WJobDate:*M)*10000+
                          %SUBDT(WJobDate:*Y);
       KTE#IDD = $SetFiles('TBIDDPF');
       $SetFields('TBIDDPF':*BLANKS);
       $addCriteria('Data cobr..:':'IDDATA':'GE':'A');
       $addCriteria('Recibo.....:':'IDREC':'EQ':'D');
       $addCriteria('ADC........:':'IDNADC':'EQ');
       ECR_DSCRITER = $getCritText();
       $SetCriteria('(IDSIT <= ''2'')');
       $SetLstFld('IDDATA':'Data Cobr.':10:'D':'D');
       $SetLstFld('IDNADC':'ADC':9:'D':'A');
       $SetLstFld('IDREC':'Recibo':9:'D':'R');
       $SetLstFld('IDVAL':'Valor':10:'D':'M');
       $SetLstFld('IDSIT':'Estado':12:'E');
       $SetLstFld('IDCRET':'Observação':20:'E');
       $SetPage(12);

       EXSR ShowIDD;
       $RmvDBSession(KTE#IDD);
       *INLR = *ON;

       //=========================================================
       // Subrotina.: ShowIDD
       // Objectivo.: Apresentar Lista de IDD's
       //=========================================================
       BEGSR ShowIDD;
          $SetCriteria('(IDSIT <= ''2'')');
          WModoExec = EXC#Pendente;
          DOW (NOT *IN03 AND NOT *IN12);
             EXSR getRecords;
             IF (WModoExec = EXC#Pendente);
                WNoFunction = 'RCOBE10003'; // Listar IDDs - Pendentes
             ELSE;
                WNoFunction = 'RCOBE10004'; // Listar IDDs - Tratadas
             ENDIF;
             DS#HEADER   = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER   = $getFooter(S#PGM:WNoFunction:*BLANKS);
             DS#OPTIONS  = $getOption(S#PGM:WNoFunction:*BLANKS);
             ECR_S#PGM   = S#PGM;
             ECR_ERR#MSG = *BLANKS;
             WRITE RCOBE10000;
             WRITE RCOBE10099;
             WRITE RCOBE10098;
             EXFMT RCOBE10003;

             IF ($ValidKey(Func#Key:KEY#F05:*BLANKS:S#PGM:WnoFunction)      );
                   $ChgCriteria();
                   ECR_DSCRITER = $getCritText();
             ELSEIF ($ValidKey(Func#Key:KEY#F06:*BLANKS:S#PGM:WnoFunction));
                   EXSR CrtIDD;
             ELSEIF ($ValidKey(Func#Key:KEY#F09:*BLANKS:S#PGM:WnoFunction));
                IF (WIsAll);
                // $addCriteria('Data cobr..:':'IDDATA':'GE':'A');
                   WModoExec = EXC#Pendente;
                   $SetCriteria('(IDSIT <= ''2'')');
                ELSE;
                // $addCriteria('Data cobr..:':'IDDATA':'GE':'D');
                // $SetCriteria(*BLANKS);
                   WModoExec = EXC#Tratada;
                   $SetCriteria('(IDSIT > ''2'')');
                ENDIF;
                WIsAll = NOT WIsAll;
             ELSEIF (Func#Key = KEY#Enter);
                EXSR SelRec;
             ENDIF;
          ENDDO;
       ENDSR;

       //========================================================
       // Subrotina.: CrtIDD
       // Objectivo.: Criar o registo de Instrução para enviar.
       //========================================================
       BEGSR CrtIDD;
          WSessScr = $AddScreen('FCOBE100':'RCOBE10007');
          $AddScPFld(%ADDR(ECR_F#04):'IDREC');
          $AddScPFld(%ADDR(ECR_F#05):'TPRAMO');
          $AddScPFld(%ADDR(ECR_F#05):'TPAPOL');
          $AddScCoord(%ADDR(ECR_CM#ROW):%ADDR(ECR_CM#COL));
          $AddScMsg(%ADDR(ECR_ERR#MSG));

          ECR_IDREC = *ZEROS;
          ECR_TPRAMO = *ZEROS;
          ECR_TPAPOL = *ZEROS;
          WIcModo    = MOD#Inserir;
          DOW (NOT *IN03 AND NOT *IN12);
             IF (WIcModo = MOD#Inserir);
                WNoFunction = 'RCOBE10007';
                *IN70 = *OFF;
                *IN50 = *ON;
                %SUBST(WExtKeys:21:1) = '1';
             ELSEIF (WIcModo = MOD#Confirmar);
                WNoFunction = 'RCOBE10008';
                *IN70 = *ON;
                *IN50 = *OFF;
                *IN60 = *OFF;
                %SUBST(WExtKeys:21:1) = '0';
             ENDIF;
             DS#HEADER   = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER   = $getFooter(S#PGM:WNoFunction:WExtKeys);
             ECR_S#PGM   = S#PGM;
             WRITE RCOBE10000;
             WRITE RCOBE10099;
             EXFMT RCOBE10007;
             IF ($ValidKey(Func#Key:KEY#F21));
                EXSR CrtRegistoIDD;
                LEAVE;
             ELSEIF ($ValidKey(Func#Key:KEY#F12));
                IF (WIcModo = MOD#Confirmar);
                   *IN12 = *OFF;
                   WIcModo = MOD#Inserir;
                ENDIF;
             ELSEIF ($ValidKey(Func#Key:KEY#Enter));
                IF (WIcModo = MOD#Inserir);
                   $InzScStatus();
                   EXSR LoadData;
                   IF (WCodErro = *BLANKS);
                      WIcModo = MOD#Confirmar;
                   ELSE;
                      $SetSCError('IDREC':WCodErro);
                   ENDIF;
                ENDIF;
             ENDIF;
          ENDDO;
          *IN12 = *OFF;
          $RmvScreen(WSessScr);
       ENDSR;

       //========================================================
       // Subrotina.: CrtRegitoIDD
       // Objectivo.: Criar O registo IDD.
       //========================================================
       BEGSR CrtRegistoIDD;
          IDD_IDDATA = $DateToInt(%DATE(UDATE));
          IDD_IDNADC = ECR_IDNADC;
          IDD_IDNIB  = ECR_IDNIB;
          IDD_IDREC  = ECR_IDREC;
          IDD_IDNSEG = WCodSeg; // Código do Segurado
          IDD_IDCCOB = WCodMed; // Mediador Com Cobrança
          IDD_IDVAL  = ECR_IDVAL;
          IDD_IDSIT  = '1';
          IDD_IDUSER = S#USR;
          IDD_IDDTAP = *ZEROS;
          IDD_IDUSEP = *BLANKS;
          IDD_IDDTPP = *ZEROS;
          IDD_IDFENV = *ZEROS;
          IDD_IDCRET = *ZEROS;
          IDD_IDNAGR = *ZEROS;  // Número Carta de Agradecimento
          WRITE TBIDDP;
       ENDSR;

       //========================================================
       // Subrotina.: CrtInterfaces
       // Objectivo.: Criar ficheiros de Interface, para enviar ao
       //             Banco.
       //========================================================
       BEGSR CrtInterfaces;
          WCodErro = *BLANKS;
          WDynamic = 'RCOBO100';
          CallDynamic(WCodErro:ECR_IDREC);  // Preparar envio dos ficheiros

       // KTE#IDDQTD = $SetFiles('TBIDDPF');
       // $SetCriteria('(IDSIT = ''2'') AND (IDDATA = '''+
       //               %CHAR($DateToInt(WJobDate)) + ''')');
       // WQtdeADC = $GetQtdeReg();
       // DOW (WQtdeADC > *ZEROS);
       //    WDynamic = 'RCOBO100';
       //    CallDynamic();  // Preparar envio dos ficheiros
       //    $ShowDialog('COB0013':%CHAR(WjobDate));
       //    WQtdeADC -= 1;
       // ENDDO;
       // $RmvDBSession(KTE#IDDQTD);
       // IF (WDynamic = *BLANKS);
       //    $ShowDialog('COB0012':%CHAR(WJobDate));
       // ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: GetRecords
       // Objectivo.: Obter todos os registos de tabelas disponív
       //========================================================
       BEGSR getRecords;
          //-------------------------------------------------------
          // Inicializar os ReEgistos
          //-------------------------------------------------------
          REC#      = *ZEROS;   // Controlo de Registos da Lista
          *In53     = *On;      // Limpar Lista de Ecrã
          *In52     = *On;      // Mostrar a Lista de Ecrã
          WRITE RCOBE10003;     // Limpar Lista de Funções
          *In53     = *Off;     // Repor Indicador
          *In52     = *Off;     // Mostrar a Lista de Ecrã

          //-------------------------------------------------------
          // Obter os Registos
          //-------------------------------------------------------
          IF ($GetPage(Func#Key) > *ZEROS);
             DOW (NOT $IsEOF());
                //------------------------------------------------
                // Escreve os Registo da Listagem
                //------------------------------------------------
                ECR_OPTION   = *BLANKS;
                ECR_LST#HDR = $GetLstHdr();
                ECR_LST#DET = $GetLstLine();

                ECR_IDSIT   = $GetField('IDSIT');
                ECR_IDREC   = $GetFieldNum('IDREC');
                ECR_IDDATA  = $GetFieldNum('IDDATA');
                ECR_IDDTPP  = $GetFieldNum('IDDTPP');
                WNumADC     = $GetFieldNUM('IDNADC');

                WIdVal = $GetFieldNum('IDVAL');
                %SUBST(ECR_LST#DET:43:13) = $GetDescricao(78:ECR_IDSIT);
                REC# += 1;

                //---------------------------------------------------
                // Validar existencia de erros
                //---------------------------------------------------
                ECR_CODERRO = *BLANKS;
                *IN80 = *OFF; // Branco
                IF (WNumADC = *ZEROS);
                  ECR_CODERRO = 'COB0106';
                ELSEIF ($GetFieldNum('IDVAL') = *ZEROS);
                  ECR_CODERRO = 'COB0107';
                ELSEIF ($GetFieldNum('IDNSEG') = *ZEROS);
                  ECR_CODERRO = 'COB0108';
                ELSE;
                   CHAIN (ECR_IDREC) GCOBGR;
                   CHAIN (WNumADC:REC_GCRAMO:REC_GCAPOL) RCOBP22002;
                   IF (%FOUND());
                      WSitADC = ADC_SITADC;
                      IF (ADC_SITADC = '5');
                         ECR_CODERRO = 'COB0109';
                      ELSEIF (ADC_SITADC = '2') AND (ECR_IDSIT = '1');
                         ECR_CODERRO = 'COB0110';
                      ELSE;
                         WCdRamo = ADC_CDRAMO;
                         WNuApol = ADC_NUAPOL;
                         CHAIN (WCdRamo:WNuApol) GBASEGRL;
                         IF (%FOUND(GBASEGRL));
                            IF (BAS_SIT <> 1);
                               ECR_CODERRO = 'COB0111';
                            ELSE;
                               IF (WNumADC <> BAS_ADC);
                                  ECR_CODERRO = 'COB0228';
                               ENDIF;
                            ENDIF;
                         ELSE;
                            ECR_CODERRO = 'COB0112';
                         ENDIF;
                      ENDIF;
                   ENDIF;
                ENDIF;

                IF (ECR_CODERRO = *BLANKS);
                   WNuRecJM = %TRIM(%EDITW(ECR_IDREC:'0         '));
                   CHAIN (054:WNuRecJM) SD1RE2;
                   IF (%FOUND(SD1RE2));
                      IF (REC_CODSIT = 2);
                         ECR_CODERRO = 'COB0113';
                      ELSEIF (REC_CODSIT = 1) OR (REC_CODSIT = 3);
                         ECR_CODERRO = 'COB0114';
                      ENDIF;
                   ELSE;
                      ECR_CODERRO = 'COB0073';
                   ENDIF;
                ENDIF;

                WDatSuger = *BLANKS;
                IF (WSitADC = '4') AND (ECR_CODERRO = *BLANKS) AND
                  (ECR_IDSIT = '1');
                   ECR_CODERRO = 'COB0226';
                   WDatSuger = '';
                ENDIF;

                IF (ECR_CODERRO = *BLANKS);
                   *IN60 = *OFF;
                   *IN80 = *OFF;
                   IF (ECR_IDSIT = '1');
                      *IN80 = *ON; // Registo pendente
                   ENDIF;
                   %SUBST(ECR_LST#DET:56:20) = *BLANKS;
                ELSE;
                   *IN60 = *ON;
                   %SUBST(ECR_LST#DET:56:20)=$GetMessage(ECR_CODERRO:WDatSuger);
                ENDIF;

                ECR_RRN = 1;
                WRITE RCOBE10002;
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
          READC RCOBE10002;
          DOW (NOT %EOF());
             WSelected = *ON;
             WCodErro = *BLANKS;
             IF ($ValidOption(ECR_OPTION:'1':*BLANKS:S#PGM:WNoFunction));
                IF (ECR_IDSIT = '1');
                   WIdSit  = '2';
                   WIdUser = S#USR;
                   WIddTap = WDatInt;
                   EXSR SavStatus;
                ELSE;
                   $ShowDialog('COB0008':%CHAR(ECR_IDREC));
                ENDIF;
             ELSEIF ($ValidOption(ECR_OPTION:'2':*BLANKS:S#PGM:WNoFunction));
                IF (ECR_IDSIT = '2');
                   WIdSit  = '1';
                   WIdUser = *BLANKS;
                   WIddTap = *ZEROS;
                   EXSR SavStatus;
                ELSE;
                   $ShowDialog('COB0009':%CHAR(ECR_IDREC));
                ENDIF;
             ELSEIF ($ValidOption(ECR_OPTION:'3':*BLANKS:S#PGM:WNoFunction));
                IF (ECR_IDSIT = '1');
                   WIdSit  = '5';
                   WIdUser = S#USR;
                   WIddTap = WDatInt;
                   EXSR SavStatus;
                ELSE;
                   $ShowDialog('COB0010':%CHAR(ECR_IDREC));
                ENDIF;
             ELSEIF ($ValidOption(ECR_OPTION:'4':*BLANKS:S#PGM:WNoFunction));
                IF (ECR_IDSIT = '5');
                   WIdSit  = '1';
                   WIdUser = *BLANKS;
                   WIddTap = *ZEROS;
                   EXSR SavStatus;
                ELSE;
                   $ShowDialog('COB0011':%CHAR(ECR_IDREC));
                ENDIF;
             ELSEIF ($ValidOption(ECR_OPTION:'5':*BLANKS:S#PGM:WNoFunction));
                IF (ECR_IDSIT = '1');
                   IF ($ShowDialog('COB0227':*BLANKS:'NS') = 'S');
                      EXSR ActIDD;
                   ENDIF;
                ELSE;
                   $ShowDialog('COB0008':%CHAR(ECR_IDREC));
                ENDIF;
             ELSEIF ($ValidOption(ECR_OPTION:'6':*BLANKS:S#PGM:WNoFunction));
                EXSR ShowDetail;
                IF (*IN12);
                   *IN12 = *OFF;
                   LEAVE;
                ENDIF;
             ELSEIF ($ValidOption(ECR_OPTION:'8':*BLANKS:S#PGM:WNoFunction));
                IF (ECR_IDSIT = '2');
                   IF (ECR_CODERRO <> *BLANKS);
                      $ShowDialog(ECR_CODERRO);
                   ELSE;
                      IF ($ShowDialog('COB0069':*BLANKS:'NS') = 'S');
                         EXSR CrtInterfaces;
                         IF (WCodErro <> *BLANKS);
                             $ShowDialog(WCodErro:%CHAR(ECR_IDREC));
                         ELSE;
                             $ShowDialog('COB0013':%CHAR(ECR_IDREC));
                         ENDIF;
                      ENDIF;
                   ENDIF;
                ELSE;
                   $ShowDialog('COB0101':%CHAR(ECR_IDREC));
                ENDIF;
             ENDIF;
             READC RCOBE10002;
          ENDDO;
          IF (NOT WSelected);
             IF (ECR_CRITERIA <> *BLANKS);
                $SetCriteria(ECR_CRITERIA:*ON);
             ELSE;
                $SetCriteria(*BLANKS:*ON);
             ENDIF;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: ShowDetail
       // Objectivo.: Apresentar o detalhe do registo
       //========================================================
       BEGSR ShowDetail;
          *IN12    = *OFF;
          WIcModo  = MOd#Consulta;
          WSessScr = $AddScreen('FCOBE100':'RCOBE10005');
          $AddScPFld(%ADDR(ECR_F#01):'IDDATA');
          $AddScPFld(%ADDR(ECR_F#02):'IDDTPP');
          $AddScCoord(%ADDR(ECR_CM#ROW):%ADDR(ECR_CM#COL));
          $AddScMsg(%ADDR(ECR_ERR#MSG));

          WExtKeys = *ZEROS;
          *IN50 = *OFF;
          *IN60 = *OFF;

          EXSR LoadData;
          IF (WCodErro = *BLANKS);
             IF (ECR_IDSIT <> '1');
                %SUBST(WExtKeys:21:1) = '1';
             ENDIF;
          ELSE;
             %SUBST(WExtKeys:21:1) = '1';
             $SetSCError('IDREC':WCodErro);
          ENDIF;

          DOW (NOT *IN03 AND NOT *IN12);
             IF (NOT *IN50);
                WNoFunction = 'RCOBE10005'; // Mostrar o Detalhe
             ELSE;
                WNoFunction = 'RCOBE10021'; // Mostrar o Detalhe
             ENDIF;
             DS#HEADER   = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER   = $getFooter(S#PGM:WNoFunction:WExtKeys);
             DS#OPTIONS  = $getOption(S#PGM:WNoFunction:WExtKeys);
             ECR_S#PGM   = S#PGM;
             WRITE RCOBE10000;
             WRITE RCOBE10099;
             EXFMT RCOBE10005;
             IF ($ValidKey(Func#Key:KEY#F21) AND (ECR_IDSIT = '1'));
                *IN50 = *ON;
             ELSEIF (Func#Key = KEY#Enter);
                IF (*IN50);
                   EXSR SavData;
                   IF ($GetScStatus() = 'O');
                      *IN50 = *OFF;
                      *IN60 = *OFF;
                   ENDIF;
                ELSE;
                   LEAVE;
                ENDIF;
             ENDIF;
          ENDDO;
          $RmvScreen(WSessScr);
       ENDSR;

       //========================================================
       // Subrotina.: LoadData
       // Objectivo.: Carregar a informação do Recibo
       //========================================================
       BEGSR LoadData;
          WCodErro = *BLANKS;
          EXSR LoadIDD;     // Obter o registo da IDD
          EXSR LoadRecibo;  // Obter a Informação do Recibo
          EXSR LoadApolice; // Obter a Informação da Apólice
          EXSR LoadADC;     // Obter a ADC da Apólice
       ENDSR;

       //========================================================
       // Subrotina.: LoadIDD
       // Objectivo.: Carregar a informação da IDD
       //========================================================
       BEGSR LoadIDD;
          IF (ECR_IDREC = *ZEROS);
             WCodErro = 'COB0078';
             LEAVESR;
          ENDIF;
          CHAIN(N) (ECR_IDREC) TBIDDP;
          IF (%FOUND(TBIDDPF));
             IF (WIcModo = MOD#Consulta);
                ECR_IDDATA = IDD_IDDATA;
                ECR_IDDTPP = IDD_IDDTPP;
                ECR_IDVAL  = IDD_IDVAL;
                ECR_IDNADC = IDD_IDNADC;
                ECR_IDNIB  = IDD_IDNIB;
                ECR_IBAN   = 'PT50'+%TRIM(%EDITW(IDD_IDNIB:
                             '0                     '));
             // ECR_FORMATO = 'XML';
                ECR_IDSIT  = IDD_IDSIT;
                ECR_DIDD   = $GetDescricao(78:ECR_IDSIT);
                ECR_IDFENV = IDD_IDFENV;
                ECR_IDCRET = IDD_IDCRET;
                ECR_DREJ   = $GetDescricao(81:%TRIM(%EDITW(ECR_IDCRET:'0  ')));
             ELSE;
                WCodErro = 'COB0070'; // Já existe uma IDD criada
             ENDIF;
          ELSE;
             IF (WIcModo = MOD#Consulta);
                WCodErro = 'COB0071'; // IDD Inexistente;
             ENDIF;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: LoadRecibo
       // Objectivo.: Obter a informação do Recibo
       //========================================================
       BEGSR LoadRecibo;
          IF (ECR_IDREC = *ZEROS);
             WCodErro = 'COB0078';
             LEAVESR;
          ENDIF;

          WCdRamo = *ZEROS;
          WNuApol = *ZEROS;
          CHAIN (ECR_IDREC) GCOBGR;
          IF (%FOUND(GCOBGR));
             WCdRamo    = REC_GCRAMO;
             WNuApol    = REC_GCAPOL;
             ECR_IDVAL  = REC_GCTREC;
             WCodSeg    = REC_GCNSEG;
             WCodMed    = REC_GCJMM1;
             WDataEmiss = $CvtData(REC_GCDEMI:'YMD':'YYMD');
          ELSE;
             WCodErro = 'COB0072'; // O Recibo não existe
          ENDIF;

          //----------------------------------------------
          // Obter o Estado do Recibo em José Mata (Corretor)
          //----------------------------------------------
          WNuRecJM = %TRIM(%EDITW(ECR_IDREC:'0         '));
          CHAIN (054:WNuRecJM) SD1RE2;
          IF (%FOUND(SD1RE2));
             ECR_GCSITR = REC_CODSIT;
             ECR_DREC   = $GetDescricao(94:%TRIM(%EDITW(ECR_GCSITR:'0 ')));
             IF (WIcModo <> MOD#Consulta);
                IF (ECR_SIT <> 0);
                   WCodErro = 'COB0081';
                ENDIF;
             ENDIF;
          ELSE;
             WCodErro = 'COB0073'; // O Recibo não existe em José Mata, Lda
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: LoadApolice
       // Objectivo.: Obter a informação da Apólice
       //========================================================
       BEGSR LoadApolice;
          WNumADC = *ZEROS;
          WNumNIB = *ZEROS;
          IF (WCodErro = *BLANKS);
             IF (WIcModo = MOD#Consulta);
                WCodErro = *BLANKS;
                CHAIN (WCdRamo:WNuApol) GBASEGRL;
             ELSE;
                IF (ECR_TPRAMO = *ZEROS) OR (ECR_TPAPOL = *ZEROS);
                   WCodErro = 'COB0079';
                ELSEIF (WCdRamo <> ECR_TPRAMO) OR (WNuApol <> ECR_TPAPOL);
                   WCodErro = 'COB0074'; // Não é a Apólice do Recibo
                ELSE;
                   CHAIN (ECR_TPRAMO:ECR_TPAPOL) GBASEGRL;
                ENDIF;
             ENDIF;
          ENDIF;

          IF (WCodErro = *BLANKS);
             IF (%FOUND(GBASEGRL));
                ECR_TPRAMO = BAS_RAMO;
                ECR_TPAPOL = BAS_APOL;
                ECR_SIT    = BAS_SIT;
                ECR_DAPOL  = $GetDescricao(54:%CHAR(ECR_SIT));
                ECR_NOSEG  = BAS_SEG;
                WNumADC    = BAS_ADC;
                MONITOR;
                   WNumNIB = %DEC(BAS_NIB:21:0);
                ON-ERROR;
                   WNumNIB = *ZEROS;
                ENDMON;
                IF (WIcModo <> MOD#Consulta);
                   IF (ECR_SIT <> 1);
                      WCodErro = 'COB0080';
                   ENDIF;
                ENDIF;
             ELSE;
                WCodERRO = 'COB0075'; // Apólice Inexistente
             ENDIF;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: LoadADC
       // Objectivo.: Obter a informação da ADC
       //========================================================
       BEGSR LoadADC;
          IF (WCodErro = *BLANKS);
             IF (WIcModo = MOD#Consulta);
                WCodErro = *BLANKS;
                CHAIN (ECR_IDNADC:WCdRamo:WNuApol) RCOBP22002;
             ELSE;
                IF (WNumADC = *ZEROS);
                   WCodErro = 'COB0076'; // Não existe ADC para a Apólice
                ELSE;
                   ECR_IDNADC= WNumADC;
                   CHAIN (WNumADC:WCdRamo:WNuApol) RCOBP22002;
                ENDIF;
             ENDIF;
          ENDIF;

          IF (WCodErro = *BLANKS);
             IF (%FOUND(FCOBP22002));
                ECR_TPSIT = ADC_SITADC;
                ECR_DADC  = $GetDescricao(87:ECR_TPSIT);
                ECR_IDNIB = WNumNib;
                ECR_IBAN  = 'PT50'+%CHAR(WNumNib);
             ELSE;
                WCodErro = 'COB0077'; // A ADC não está registada
             ENDIF;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: ActIDD
       // Objectivo.: Actualizar Instrução
       //========================================================
       BEGSR ActIDD;
          //------------------------------------------------
          // Obter a Apólice da Instrução
          //------------------------------------------------
          CHAIN (IDD_IDREC) GCOBGR;
          IF (NOT %FOUND(GCOBGR));
             LEAVESR;
          ENDIF;

          //------------------------------------------------
          // Obter a ADC e o NIB
          //------------------------------------------------
          CHAIN (REC_GCRAMO:REC_GCAPOL) GBASEGRL;
          IF (NOT (%FOUND(GBASEGRL)));
             LEAVESR;
          ENDIF;

          //------------------------------------------------
          // Actualizar o NIB e a ADC
          //------------------------------------------------
          CHAIN (ECR_IDREC) TBIDDP;
          IF (%FOUND());
             MONITOR;
                IDD_IDNIB  = %DEC(BAS_NIB:21:0);
                IDD_IDNADC = BAS_ADC;
                IF (IDD_IDNIB > *ZEROS) AND (IDD_IDNADC > *ZEROS);
                   UPDATE TBIDDP %FIELDS(IDD_IDNIB:IDD_IDNADC);
                ENDIF;
             ON-ERROR;
             ENDMON;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: SavStatus
       // Objectivo.: Salvaguardar o registo
       //========================================================
       BEGSR SavStatus;
          IF (WIdSit = '2');
             IF (ECR_IDDATA <> $DateToInt(%DATE()) AND
               ($DateToInt(%DATE()) <> DAT#Exception));
                $ShowDialog('COB0119':%CHAR(ECR_IDREC));
                LEAVESR;
             ENDIF;

             EXSR LoadRecibo;
             IF (WCodErro <> *BLANKS);
                $ShowDialog(WCodErro:%CHAR(ECR_IDREC));
                LEAVESR;
             ENDIF;

             IF (ECR_IDDTPP <> *ZEROS);
                TEST(DE) *ISO ECR_IDDATA;
                IF (%ERROR());
                   $ShowDialog('COB0001':%CHAR(ECR_IDREC));
                   LEAVESR;
                ENDIF;

             ELSE;
                $ShowDialog('COB0121':%CHAR(ECR_IDREC));
                LEAVESR;
             ENDIF;
          ENDIF;

          CHAIN (ECR_IDREC) TBIDDP;
          IF (%FOUND());
             IDD_IDSIT  = WIdSit;
             IDD_IDUSER = WIdUser;
             IDD_IDDTAP = WIddTap;
             IDD_IDCRET  = *ZEROS;
             UPDATE TBIDDP %FIELDS(IDD_IDSIT:IDD_IDUSER:IDD_IDDTAP:IDD_IDCRET);
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: SavData
       // Objectivo.: Salvaguardar o registo
       //========================================================
       BEGSR SavData;
          $InzScStatus();
          WCodErro = *BLANKS;
          EXSR LoadRecibo;
          IF (WCodErro <> *BLANKS);
             $SetScError('IDDATA':WCodErro);
             LEAVESR;
          ENDIF;

          // IF (WDataEmiss > ECR_IDDATA);
          //    $SetScError('IDDATA':'COB0118');
          //    LEAVESR;
          // ENDIF;

          TEST(DE) *ISO ECR_IDDATA;
          IF (%ERROR());
             $SetScError('IDDATA':'COB0001');
             LEAVESR;
          ENDIF;

          IF (ECR_IDSIT = '2');
             IF (ECR_IDDTPP <> *ZEROS);
                TEST(DE) *ISO ECR_IDDATA;
                IF (%ERROR());
                   $SetScError('IDDTPP':'COB0001');
                   LEAVESR;
                ENDIF;

                WIDDATA = $IntToDate(ECR_IDDATA);
                WIDDTPP = $IntToDate(ECR_IDDTPP);
                IF ((ECR_TPSIT = '7') AND
                     (ECR_IDDTPP < $AddToDate(ECR_IDDATA:3:'U')))
                  OR ((ECR_TPSIT <> '7') AND
                     (ECR_IDDTPP < $AddToDate(ECR_IDDATA:6:'U')));
                   $SetScError('IDDTPP':'COB0120');
                   LEAVESR;
                ENDIF;
             ELSE;
                $SetScError('IDDATA':'COB0121');
                LEAVESR;
             ENDIF;
          ENDIF;

          IF ((ECR_IDDATA < $DateToInt(%DATE())) AND
               ($DateToInt(%DATE()) <> DAT#Exception));
               $SetScError('IDDATA':'COB0117');
               LEAVESR;
          ENDIF;

          CHAIN (ECR_IDREC) TBIDDP;
          IF (%FOUND());
             IDD_IDDATA = ECR_IDDATA;
             IF (ECR_TPSIT = '7');
                IDD_IDDTPP = $addToDate(ECR_IDDATA:3:'U');
             ELSE;
                IDD_IDDTPP = $addToDate(ECR_IDDATA:6:'U');
             ENDIF;
             ECR_IDDTPP = IDD_IDDTPP;
             UPDATE TBIDDP %FIELDS(IDD_IDDATA:IDD_IDDTPP);
          ENDIF;
       ENDSR;
      /END-FREE
