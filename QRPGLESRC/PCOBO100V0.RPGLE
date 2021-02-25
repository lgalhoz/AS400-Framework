     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PCOBO100
     H* Descrição.: Manutenção de IDD's
     H* Observação: Programa reescrito na nova framework.
     H*==================================================================
     FTBIDDPF   UF   E           K DISK    PREFIX(IDD_)
     FFCOBP220  IF   E           K DISK    PREFIX(ADC_)
     FGBASEGRL  IF   E           K DISK    PREFIX(BAS_)
     FGCOBGR    IF   E           K DISK    PREFIX(REC_)
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
     D   FileCode                    10P 0 Const Options(*NOPASS)
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
     D WCdRamo         S              2S 0
     D WNuApol         S              7S 0
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
       WDatInt = %SUBDT(%DATE():*D)*1000000+%SUBDT(%DATE():*M)*10000+
                          %SUBDT(%DATE():*Y);
       KTE#IDD = $SetFiles('TBIDDPF INNER JOIN FCOBP220 ON +
                             (NUNADC = IDNADC)');
       $SetFields('TBIDDPF':*BLANKS);
       $SetFields('FCOBP220':*BLANKS:*OFF);
       $addCriteria('Data cobr..:':'IDDATA':'GE':'A');
       $addCriteria('Recibo.....:':'IDREC':'GE':'D');
       $addCriteria('ADC........:':'IDNADC':'GE');
       ECR_DSCRITER = $getCritText();
       $SetCriteria('(IDSIT <= ''2'')');
       $SetLstFld('IDDATA':'Data Cobr.':10:'D':'D');
       $SetLstFld('IDNADC':'ADC':9:'D':'A');
       $SetLstFld('IDREC':'Recibo':11:'D':'R');
       $SetLstFld('IDVAL':'Valor':18:'D':'M');
       $SetLstFld('IDSIT':'Estado':12:'E');
       $SetLstFld('IDCRET':'Ret':2:'E');
       $SetPage(12);

       EXSR ShowIDD;
       $RmvDBSession(KTE#IDD);
       *INLR = *ON;

       //=========================================================
       // Subrotina.: ShowIDD
       // Objectivo.: Apresentar Lista de IDD's
       //=========================================================
       BEGSR ShowIDD;
          DOW (NOT *IN03 AND NOT *IN12);
             EXSR getRecords;
             WNoFunction = 'RCOBE10003'; // Listar IDDs
             DS#HEADER   = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER   = $getFooter(S#PGM:WNoFunction:*BLANKS);
             DS#OPTIONS  = $getOption(S#PGM:WNoFunction:*BLANKS);
             ECR_S#PGM   = S#PGM;
             ECR_ERR#MSG = *BLANKS;
             WRITE RCOBE10000;
             WRITE RCOBE10099;
             WRITE RCOBE10098;
             EXFMT RCOBE10003;
             IF (Func#Key = KEY#F05);
                $ChgCriteria();
                ECR_DSCRITER = $getCritText();
             ELSEIF (Func#Key = KEY#F06);
                KTE#IDDQTD = $SetFiles('TBIDDPF');
                $SetCriteria('(IDSIT = ''2'') AND (IDDATA = '''+
                              %CHAR($DateToInt(%DATE())) + ''')');
                IF ($GetQtdeReg() = *ZEROS);
                   $ShowDialog('COB0012':%CHAR(%DATE()));
                ELSE;
                   WDynamic = 'RCOBO100';
                   CallDynamic();  // Preparar envio dos ficheiros
                   $ShowDialog('COB0013':%CHAR(%DATE()));
                ENDIF;
                $RmvDBSession(KTE#IDDQTD);
             ELSEIF (Func#Key = KEY#F07);
                IF (WIsAll);
                // $addCriteria('Data cobr..:':'IDDATA':'GE':'A');
                   $SetCriteria('(IDSIT <= ''2'')');
                ELSE;
                // $addCriteria('Data cobr..:':'IDDATA':'GE':'D');
                   $SetCriteria(*BLANKS);
                ENDIF;
                WIsAll = NOT WIsAll;
             ELSEIF (Func#Key = KEY#Enter);
                EXSR SelRec;
             ENDIF;
          ENDDO;
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
                WIdVal = $GetFieldNum('IDVAL');
                %SUBST(ECR_LST#DET:53:13) = $GetDescricao(78:ECR_IDSIT);
                REC# += 1;

                //---------------------------------------------------
                // Validar existencia de erros
                //---------------------------------------------------
                *IN60 = *OFF;
                *IN80 = *OFF;
                IF (%SUBST(ECR_LST#DET:19:1) = *BLANKS);
                  *IN60 = *ON;
                ELSEIF ($GetFieldNum('IDVAL') = *ZEROS);
                  *IN60 = *ON;
                ELSEIF ($GetFieldNum('IDNSEG') = *ZEROS);
                  *IN60 = *ON;
                ELSE;
                   WCdRamo = $GetFieldNum('CDRAMO');
                   WNuApol = $GetFieldNum('NUAPOL');
                   CHAIN (WCdRamo:WNuApol) GBASEGRL;
                   IF (%FOUND(GBASEGRL));
                      IF (BAS_SIT <> 1);
                         *IN60 = *ON;
                      ENDIF;
                   ELSE;
                      *IN60 = *ON;
                   ENDIF;
                ENDIF;
                IF (NOT *IN60);
                   *IN80 = *OFF;
                   IF (ECR_IDSIT = '1');
                      *IN80 = *ON;
                   ENDIF;
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
             IF ($ValidOption(ECR_OPTION:'1'));
                IF (ECR_IDSIT = '1');
                   WIdSit  = '2';
                   WIdUser = S#USR;
                   WIddTap = WDatInt;
                   EXSR SavStatus;
                ELSE;
                   $ShowDialog('COB0008':%CHAR(ECR_IDREC));
                ENDIF;
             ELSEIF ($ValidOption(ECR_OPTION:'2'));
                IF (ECR_IDSIT = '2');
                   WIdSit  = '1';
                   WIdUser = *BLANKS;
                   WIddTap = *ZEROS;
                   EXSR SavStatus;
                ELSE;
                   $ShowDialog('COB0009':%CHAR(ECR_IDREC));
                ENDIF;
             ELSEIF ($ValidOption(ECR_OPTION:'3'));
                IF (ECR_IDSIT = '1');
                   WIdSit  = '5';
                   WIdUser = S#USR;
                   WIddTap = WDatInt;
                   EXSR SavStatus;
                ELSE;
                   $ShowDialog('COB0010':%CHAR(ECR_IDREC));
                ENDIF;
             ELSEIF ($ValidOption(ECR_OPTION:'4'));
                IF (ECR_IDSIT = '5');
                   WIdSit  = '1';
                   WIdUser = *BLANKS;
                   WIddTap = *ZEROS;
                   EXSR SavStatus;
                ELSE;
                   $ShowDialog('COB0011':%CHAR(ECR_IDREC));
                ENDIF;
             ELSEIF (ECR_OPTION='6');
                EXSR ShowDetail;
                IF (*IN12);
                   *IN12 = *OFF;
                   LEAVE;
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
          *IN12 = *OFF;
          WSessScr = $AddScreen('FCOBE100':'RCOBE10005');
          $AddScPFld(%ADDR(ECR_F#01):'IDDATA');
          $AddScCoord(%ADDR(ECR_CM#ROW):%ADDR(ECR_CM#COL));
          $AddScMsg(%ADDR(ECR_ERR#MSG));
          EXSR LoadData;
          WExtKeys = *ZEROS;
          IF (ECR_IDSIT <> '2');
             %SUBST(WExtKeys:21:1) = '1';
          ENDIF;
          *IN50 = *OFF;
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
             IF ($ValidKey(Func#Key:KEY#F21) AND (ECR_IDSIT = '2'));
                *IN50 = *ON;
             ELSEIF (Func#Key = KEY#Enter);
                IF (*IN50);
                   EXSR SavData;
                   IF ($GetScStatus() = 'O');
                      *IN50 = *OFF;
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
          CHAIN(N) (ECR_IDREC) TBIDDP;
          IF (%FOUND());
             ECR_IDDATA = IDD_IDDATA;
             ECR_IDVAL  = IDD_IDVAL;
             ECR_IDNADC = IDD_IDNADC;
             ECR_IDNIB  = IDD_IDNIB;
             ECR_IBAN   = 'PT50'+%TRIM(%EDITW(IDD_IDNIB:
                          '0                     '));
             ECR_IDSIT  = IDD_IDSIT;
             ECR_DIDD   = $GetDescricao(78:ECR_IDSIT);
             ECR_IDFENV = IDD_IDFENV;
             ECR_IDCRET = IDD_IDCRET;
             ECR_DREJ   = $GetDescricao(81:%TRIM(%EDITW(ECR_IDCRET:'0  ')));

             //----------------------------------------------
             // Obter informação do Recibo
             //----------------------------------------------
             CHAIN (ECR_IDREC) GCOBGR;
             IF (%FOUND());
                ECR_TPRAMO = REC_GCRAMO;
                ECR_TPAPOL = REC_GCAPOL;
                ECR_GCSITR = REC_GCSITR;
                ECR_DREC   = $GetDescricao(80:%TRIM(%EDITW(ECR_GCSITR:'0 ')));
             ENDIF;

             //----------------------------------------------
             // Obter informação da ADC
             //----------------------------------------------
             CHAIN (ECR_IDNADC) RCOBP220;
             IF (%FOUND());
                ECR_TPSIT = ADC_SITADC;
                ECR_DADC  = $GetDescricao(79:ECR_TPSIT);
             ENDIF;

             //----------------------------------------------
             // Obter informação da Apólice
             //----------------------------------------------
             CHAIN (ECR_TPRAMO:ECR_TPAPOL) GBASEGRL;
             IF (%FOUND());
                ECR_SIT   = BAS_SIT;
                ECR_DAPOL = $GetDescricao(54:%CHAR(ECR_SIT));
                ECR_NOSEG = BAS_SEG;
             ENDIF;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: SavStatus
       // Objectivo.: Salvaguardar o registo
       //========================================================
       BEGSR SavStatus;
          CHAIN (ECR_IDREC) TBIDDP;
          IF (%FOUND());
             IDD_IDSIT  = WIdSit;
             IDD_IDUSER = WIdUser;
             IDD_IDDTAP = WIddTap;
             UPDATE TBIDDP %FIELDS(IDD_IDSIT:IDD_IDUSER:IDD_IDDTAP);
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: SavData
       // Objectivo.: Salvaguardar o registo
       //========================================================
       BEGSR SavData;
          $InzScStatus();
          TEST(DE) *ISO ECR_IDDATA;
          IF (%ERROR());
             $SetScError('IDDATA':'COB0001');
          ELSE;
             CHAIN (ECR_IDREC) TBIDDP;
             IF (%FOUND());
                IDD_IDDATA = ECR_IDDATA;
                UPDATE TBIDDP %FIELDS(IDD_IDDATA);
             ENDIF;
          ENDIF;
       ENDSR;
      /END-FREE
