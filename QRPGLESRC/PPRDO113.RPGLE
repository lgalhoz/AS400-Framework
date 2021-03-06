     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PPRDO113
     H* Descri��o.: Consulta de Autoriza��es de D�bito em Conta
     H*==================================================================
     FGBASEGRL  IF   E           K DISK    PREFIX(GRL_)
     FFCOBP220  IF   E           K DISK    PREFIX(ADC_)
     FFCOBP221  IF   E           K DISK    PREFIX(ADC_)
     FFCOBP222  IF   E           K DISK    PREFIX(ADC_)
     FFCOBP22001IF   E           K DISK    PREFIX(ADC_)
     F                                     RENAME(RCOBP220:RCOBP22001)
     FFPRDE113  CF   E             WORKSTN SFILE(RPRDE11302:REC#)
     F                                     PREFIX(ECR_)
     F                                     INFDS(INFD#KEYS)
     F*==========================================================
     F* Indicadores  Descri��o
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

     D*==> Campos de Trabalho
     D NIB             DS
     D  ECR_NUNIB              1     21S 0
     D  WCdBanco               1      4A
     D  WNIBAlfa               1     21A
     D
     D REC#            S              4S 0
     D WIcOrigem       S              1A
     D WMsgErro        S              7A
     D WNofunction     S             10A
     D WOptions        S             24A
     D WSelected       S               N
     D WSessScr        S              3S 0
     D WIdcRet         S              2S 0
     D WKeysTohide     S             24A   INZ(*ZEROS)
     D WOptTohide      S             24A   INZ(*ZEROS)
     D WCdOpera        S              3S 0
     D WSitADC         S              1A
     D WDescricao      S             40A
     D WCdRamo         S              2S 0
     D WNuApol         S              7S 0
     D
     D KTE#ADC         S              5S 0
     D KTE#IDD         S              5S 0
     C*========================================================
      /FREE

       //------------------------------------
       // Validar o acesso a esta fun��o
       //------------------------------------
       IF NOT $ValAccess(S#PGM:'':99999);
          $ShowDialog('SEC0001');
          *INLR = *ON;
          RETURN;
       ENDIF;

       //------------------------------------------------------
       // Sess�o das ADC's
       //------------------------------------------------------
       KTE#ADC = $SETFILES('FCOBP220 A INNER JOIN FCOBP221 B ON +
                           (A.CDNADC = B.CDNADC) AND (A.CDNIB = B.CDNIB)'
                           :*BLANKS);
       $SetFields('FCOBP220':'A');
       $SetFields('FCOBP221':'B':*OFF);
       $addCriteria('Autoriza��o:':'A.NUNADC':'GE':'D');
       $addCriteria('N.I.B......:':'B.NUNIB':'GE');
       $addCriteria('IBAN.......:':'B.NUIBAN':'GE');
       $addCriteria('Ap�lice....:':'A.NUAPOL':'GE');
       $SetLstFld('A.NUNADC':'N�.ADC':10:'D':'A');
       $SetLstFld('A.NUAPOL':'Ap�lice':7:'D');
       $SetLstFld('A.NUAPOL':'Segurado':16:'E');
       $SetLstFld('A.SITADC':'Situa��o':12:'E');
       $SetLstFld('B.NUNIB':'Identifica��o Banc�ria':26:'D');
       $SetLstFld('B.CDMOV':'Movimento.':15:'D');
       $SetLstFld('B.DATALT':'Data':10:'D':'D');
       ECR_DSCRITER = $getCritText();
       $SetPage(12);

       //------------------------------------------------------
       // Sess�o das IDD's
       //------------------------------------------------------
       KTE#IDD = $SETFILES('TBIDDPF');
       $addCriteria('Data.......:':'IDDATA':'GE':'D');
       $addCriteria('Recibo.....:':'IDREC':'GE':'D');
       $SetLstFld('IDDATA':'Data':10:'D':'D');
       $SetLstFld('IDREC':'Recibo':9:'D');
       $SetLstFld('IDVAL':'Valor':7:'D');
       $SetLstFld('IDSIT':'Situa��o':10:'E');
       $SetLstFld('IDRET':'Retorno':10:'E');
       $SetLstFld('IDNIB':'NIB':21:'E');
       $SetPage(12);

       $SetDBSession(KTE#ADC); // Repor Sess�o das ADC's
       EXSR ShowAllADC;

       $RmvDBSession(KTE#ADC);
       $RmvDBSession(KTE#IDD);
       *INLR = *ON;
       RETURN;

       //========================================================
       // Subrotina.: ShowAllADC
       // Objectivo.: Mostrar todas as ADC's da Ap�lice
       //========================================================
       BEGSR ShowAllADC;
          DOW (NOT *IN03 AND NOT *IN12);
             EXSR getRecords;
             WOptions = *BLANKS;
             IF ($GetDBSession() = KTE#ADC);
                WNoFunction = 'RPRDE11303'; // Listar ADC's
             ELSE;
                WNoFunction = 'RPRDE11304'; // Listar Recibos
             ENDIF;
             DS#HEADER   = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER   = $getFooter(S#PGM:WNoFunction:WOptions);
             DS#OPTIONS  = $getOption(S#PGM:WNoFunction:WOptions);
             ECR_S#PGM   = S#PGM;
             ECR_ERR#MSG = *BLANKS;
             WRITE RPRDE11300;
             WRITE RPRDE11399;
             WRITE RPRDE11398;
             EXFMT RPRDE11303;
             IF (Func#Key = KEY#F05);
                $ChgCriteria();
                ECR_DSCRITER = $getCritText();
                EXSR GetRecords;
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
       // Objectivo.: Obter todos os registos de tabelas dispon�v
       //========================================================
       BEGSR getRecords;
          REC#      = *ZEROS;   // Controlo de Registos da Lista
          *In53     = *On;      // Limpar Lista de Ecr�
          *In52     = *On;      // Mostrar a Lista de Ecr�
          WRITE RPRDE11303;     // Limpar Lista de Fun��es
          *In53     = *Off;     // Repor Indicador
          *In52     = *Off;     // Mostrar a Lista de Ecr�
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
                WNuApol    = $GetFieldNum('A.NUAPOL');
                WCdRamo    = $GetFieldNum('A.CDRAMO');
                IF ($GetDbSession() = KTE#ADC);
                   ECR_CDNIB  = $GetFieldNum('B.CDNIB');
                   CHAIN (WCdRamo:WNuApol) GBASEGRL;
                   IF (%FOUND());
                      %SUBST(ECR_LSTDET:19:19) = ' '+GRL_SEG;
                   ENDIF;

                   WSitADC = $GetField('A.SITADC');
                   WDescricao = $GetDescricao(87:WSitADC);
                   %SUBST(ECR_LSTDET:39:14) = WSitADC+' '+%TRIM(WDescricao);
                ELSE;
                  %SUBST(ECR_LSTDET:30:10) = $GetDescricao(78:
                                             %TRIM($GetField('IDSIT')));
                  WIdcRet = $GetFieldNum('IDCRET');
                  %SUBST(ECR_LSTDET:41:10) = $GetDescricao(81:
                                             %TRIM(%EDITW(WIdcRet:'0  ')));
                ENDIF;
                REC# += 1;
                WRITE RPRDE11302;
                *IN52 = *ON;       // Encontrou registos
                $getNext();
             ENDDO;
          ENDIF;
          *IN54 = $IsLastPage();   // Indica��o de Fim de P�gina
       ENDSR;

       //========================================================
       // Subrotina.: SelRec
       // Objectivo.: Seleccionar os registos
       //========================================================
       BEGSR SelRec;
          WSelected = *OFF;
          READC RPRDE11302;
          DOW (NOT %EOF());
             IF ($ValidOption(ECR_OPTION:'2'));
                EXSR ShowDetADC;
             ELSEIF ($ValidOption(ECR_OPTION:'5'));
                $SetDBSession(KTE#IDD);
                $SetCriteria('(IDNADC = '''+%CHAR(ECR_NUADC)+''')');
                ECR_DSCRITER = $getCritText();
                LEAVESR;
             ENDIF;
             READC RPRDE11302;
          ENDDO;
          IF (NOT WSelected);
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
          EXSR LoadParms;
          WNoFunction = 'RPRDE11305';
          DOW (NOT *IN03 AND NOT *IN12);
             WOptions  = *ZEROS;
             DS#HEADER = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER = $getFooter(S#PGM:WNoFunction:WKeysToHide);
             ECR_S#PGM = S#PGM;
             WRITE     RPRDE11300;
             WRITE     RPRDE11399;
             EXFMT RPRDE11305;
             IF (Func#Key=KEY#Enter);
                LEAVE;
             ELSIF (Func#Key=KEY#F21);

             ENDIF;
          ENDDO;
          *IN12 = *OFF;
       ENDSR;

       //==================================================================
       // Subrotina.: LoadParms
       // Objectivo.: Carregar ShowDetADC da Ap�lice
       //==================================================================
       BEGSR LoadParms;
          ECR_CDIBAN = *BLANKS;
          CHAIN(N) (ECR_CDADC) FCOBP220;
          IF (%FOUND());
             ECR_ADCSEPA = ADC_ADCSEPA;
             CHAIN (ADC_CDRAMO:ADC_NUAPOL) GBASEGRL;
             IF (%FOUND());
                ECR_SEG   = GRL_SEG;
                ECR_SIT   = GRL_SIT;
                ECR_DSSIT = $GetDescricao(54:%CHAR(ECR_SIT));
             ELSE;
                ECR_SIT   = *ZEROS;
                ECR_SEG   = *BLANKS;
                ECR_DSSIT = *BLANKS;
             ENDIF;
             ECR_RAMO    = ADC_CDRAMO;
             ECR_APOL    = ADC_NUAPOL;
             ECR_NUADC   = ADC_NUNADC;
             ECR_DTSITUA = ADC_DTAPROV;
             ECR_CDSITUA = ADC_SITADC;
             ECR_DSSITUA =  $GetDescricao(87:ADC_SITADC);
             CHAIN (ECR_CDADC:ECR_CDNIB) FCOBP221;
             IF (%FOUND());
                ECR_NUNIB   = ADC_NUNIB;
             // ECR_DTSITUA = $CvtData(ADC_DATALT:'YYMD':'DMYY');
             // ECR_CDSITUA = ADC_CDSIT;
                IF (ECR_CDIBAN = *BLANKS);
                   ECR_CDIBAN = 'PT50'+%TRIM(
                              %EDITW(ECR_NUNIB:'0                     '));
                ENDIF;
             ENDIF;
          ENDIF;
       ENDSR;
      /END-FREE
