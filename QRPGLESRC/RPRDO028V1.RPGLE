     H DFTACTGRP(*NO) BNDDIR('JMDIR')
     H*====================================================================
     H* Programa..: RPRDO028
     H* Descrição.: Informação das Apólices e Segurados
     H*
     H* Execução..: http://20.0.0.1/josemata?frame=login&user=apache
     H*             http://20.0.0.1/josemata?frame=apolseg&nif=198757751
     H*             http://20.0.0.1/josemata?frame=apolseg&apolice=9958672&matricula=99-00-NZ
     H*
     H*====================================================================
      /Copy QRPGLESRC,SSECO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SSECO012
      /Copy QRPGLESRC,SGRLO010

     D*==> Campos de Trabalho
     D   WURI          S          65535A
     D   HTTPCODE      S          65535A
     D
     D*==> Restantes campos de Trabalho
     D WNIF            S              9A
     D WNSeg           S              8A
     D WMatricula      S             15A
     D WRamo           S              3A
     D WApolice        S             10A
     D WOrdem          S              8A
     D
     D WCodSegLS       S              8S 0
     D WCodSegCR       S              8S 0
     D WData           S              8S 0
     D WBlanks         S             20A
     D
     D WRemoteAddr     S             25A
     D WSession        S              5S 0
     D WRecIni         S              5S 0
     D WCriteria       S            100A
     D WLocApolice     S              1A
     D WIcFound        S               N
     D
     D*==> Constantes
     D LOC#Activas     C                   'A'
     D LOC#Historico   C                   'H'
     D LOC#Nulas       C                   'N'
     D*=============================================================
     C     *ENTRY        PLIST
     C                   PARM                    WURI
     C                   PARM                    HTTPCODE
      /FREE
        MONITOR;
           WRemoteAddr = %str($getenv('REMOTE_ADDR'));
        ON-ERROR;
           WRemoteAddr  = '20.0.0.53';
        ENDMON;

        //-----------------------------------------------
        // Validar se pode utilizar a função
        //-----------------------------------------------
        $inzHTTPSrc('XML');  // Possivel(XML, HTTP, TXT)
        IF NOT $ValAccess(S#PGM);
           HTTPCode = $getHTTPmsg('HTP0500');
           RETURN;
        ENDIF;

        //-----------------------------------------------------
        // Inicializar campos de Trabalho
        //-----------------------------------------------------
        WBlanks = *BLANKS;
        WSession = *ZEROS;
        WCodSegLS = *ZEROS;
        WCodSegCR = *ZEROS;

        $addHTTPVal('IP':WRemoteAddr);
        $addHTTPVal('user':S#USR);
        $addHTTPVal('stscod':'200');

        //-----------------------------------------------
        // Validar os Parametros recebidos
        //-----------------------------------------------
        MONITOR;
           WNIF       = $getHTTPVar('nif');
           WNSeg      = $getHTTPVar('segurado');
           WMatricula = $getHTTPVar('matricula');
           WOrdem     = $getHTTPVar('ordem');
           WRamo      = $getHTTPVar('ramo');
           WApolice   = $getHTTPVar('apolice');
           WApolice   = %SUBST(WBlanks:1:(10-%LEN(%TRIM(WApolice))))
                        + %TRIM(WApolice);
        ON-ERROR;
           WOrdem     = *BLANKS;
           WNIF       = *BLANKS;
           WNSeg      = *BLANKS;
           WMatricula = *BLANKS;
           WRamo      = *BLANKS;
           WApolice   = *BLANKS;
        ENDMON;

        //-----------------------------------------------------
        // Obter informação do Segurado
        //-----------------------------------------------------
        IF (WNSeg <> *BLANKS) OR (WNIF <> *BLANKS);
           WCodSegLS = -1;
           IF (WNSeg <> *BLANKS);
              WCriteria = 'NSEG = ' + WNSeg;
           ELSEIF (WNIF <> *BLANKS);
              WCriteria = 'NIF = ' + WNIF;
           ENDIF;
           EXSR GetSegurado;
        ENDIF;

        //-----------------------------------------------------
        // Obter informação da Apólice (Simples ou Combinada)
        //-----------------------------------------------------
        IF (WMatricula <> *BLANKS) OR (WApolice <> *BLANKS)
           OR (WOrdem <> *BLANKS);
           IF ((WMatricula <> *BLANKS) AND (WApolice <> *BLANKS));
              WRamo = '310';
              WCriteria = '(MATRI=''' + %TRIM(WMatricula) + ''') ' +
                          ' AND (APOLI='''+WApolice+''')';
           ELSEIF ((WRamo <> *BLANKS) AND (WApolice <> *BLANKS));
              WCriteria = '(CRAMO=' + %TRIM(WRamo) + ') +
                           AND (APOLI='''+WApolice+''')';
           ELSEIF (WMatricula <> *BLANKS);
              WRamo = '310';
              WCriteria = '(MATRI=''' + %TRIM(WMatricula) + ''')' +
                          ' AND (CRAMO=310)';
           ELSEIF (WApolice <> *BLANKS);
              WCriteria = '(APOLI='''+WApolice+''')';
           ELSEIF ((WOrdem <> *BLANKS) AND (WRamo <> *BLANKS));
              WCriteria = '(NORD='''+%TRIM(WOrdem)+''') AND +
                           (CRAMO = ' + %TRIM(WRamo) + ')';
           ENDIF;
           IF (WCodSegLS <> *ZEROS); // Informação Combinada
              WCriteria = %TRIM(WCriteria) +
                          ' AND (CSEG = ' + %CHAR(WCODSegLS) + ')';
           ENDIF;
           EXSR GetApolice;
        ENDIF;

        //-----------------------------------------------------
        // Validar se encontrou registos
        //-----------------------------------------------------
        IF (WRecIni = 1);
        // $addHTTPVal('erro');
        // $addHTTPVal('criteria':WCriteria);
        // $addHTTPVal();
           HTTPCode = $getHTTPmsg('HTP0207');
           RETURN;
        ENDIF;

        HTTPCODE = $getHTTPSrc();
        RETURN;

        //=====================================================
        // Subrotina.: GetSegurado
        // Descrição.: Obter os registos do Segurado
        //=====================================================
        BEGSR GetSegurado;
           WSession = $SetFiles('GFSEG');
           $SetFields('NSEG,NOM,NIF');
           $SetCriteria(%TRIM(WCriteria));

           WRecIni = 1;
           DOW (($GetRecord(WRecIni) > *ZEROS) AND (WRecIni < 3));
              WCodSegLS = $GetFieldNum('NSEG');
              $addHTTPVal('segurado');
              $addHTTPVal('segurado':$GetField('NSEG'));
              $addHTTPVal('nome':$GetField('NOM'));
              $addHTTPVal('nif':$GetField('NIF'));
              $addHTTPVal();
              WRecIni  += 1;
           ENDDO;
           $RmvDBSession(WSession);
        ENDSR;

        //=====================================================
        // Subrotina.: GetApolice
        // Descrição.: Obter os registos da Apólice
        //=====================================================
        BEGSR GetApolice;
           WIcFound    = *OFF;
           WLocApolice = LOC#Activas;
           WSession    = $SetFiles('"SD.1.AP"');
           DOW (WLocApolice <> LOC#Nulas);
              $SetFields('CRAMO,APOLI,CSEG,CCOMP,MATRI,DTULT');
              $SetCriteria(%TRIM(WCriteria));
              WRecIni = 1;
              DOW (($GetRecord(WRecIni) > *ZEROS) AND (WRecIni < 3));
                 WIcFound  = *ON;
                 WCodSegCR = $GetFieldNum('CSEG');
                 WData     = $CvtData($GetFieldNum('DTULT'):'DMY':'YYMD');
                 $addHTTPVal('apolice');
                 IF ($GetFieldNum('CCOMP') = 54);
                    EXSR GetLusitRamo;
                 ELSE;
                    $addHTTPVal('ramo':$GetField('CRAMO'));
                 ENDIF;
                 $addHTTPVal('apolice':$GetField('APOLI'));
                 $addHTTPVal('segurado':$GetField('CSEG'));
                 $addHTTPVal('companhia':$GetField('CCOMP'));
                 $addHTTPVal('matricula':$GetField('MATRI'));
                 $addHTTPVal('alteracao':%CHAR(WData));
                 $addHTTPVal('origem':'sd.1.ap');
                 $addHTTPVal();
                 WRecIni += 1;
              ENDDO;
              $RmvDBSession(WSession);

              IF (WRecIni > 1);
                 WLocApolice = LOC#Nulas;
              ELSEIF (WRecIni = 1) AND (WLocApolice = LOC#Activas);
                 WLocApolice = LOC#Historico;
                 WSession = $SetFiles('"SD.1.APM"');
              ELSEIF (WRecIni = 1) AND (WLocApolice = LOC#Historico);
                 WLocApolice = LOC#Nulas;
              ENDIF;
           ENDDO;

           //-------------------------------------------------------
           // Se não encontrou no Corretor, então procura a Apólice
           // na Lusitânia Azul.
           //-------------------------------------------------------
           IF ((WMatricula <> *BLANKS) AND (WApolice <> *BLANKS) AND
               (NOT WIcFound));
              WRamo = '50';
           // WCriteria = '(MATR1=''' + %TRIM(WMatricula) + ''') ' +
              WCriteria = '(APOL='+WApolice+')';
              WSession = $SetFiles('"GBASEGRA"');
              $SetFields('RAMO,APOL,NSEG,DTALT,MATR1');
              $SetCriteria(%TRIM(WCriteria));
              WRecIni = 1;
              IF ($GetRecord(WRecIni) > *ZEROS);
                 WIcFound  = *ON;
                 WCodSegCR = $GetFieldNum('NSEG');
                 WData     = $CvtData($GetFieldNum('DTALT'):'DMY':'YYMD');
                 $addHTTPVal('apolice');
                 $addHTTPVal('ramo':$GetField('RAMO'));
                 $addHTTPVal('apolice':$GetField('APOL'));
                 $addHTTPVal('segurado':$GetField('NSEG'));
                 $addHTTPVal('companhia':'54');
                 IF (%trim(WMatricula) = %trim($GetField('MATR1')));
                    $addHTTPVal('matricula':$GetField('MATR1'));
                    $addHTTPVal('matrigual':'sim');
                 ELSE;
                    $addHTTPVal('matricula':$GetField('MATR1'));
                    $addHTTPVal('matrigual':'nao');
                 ENDIF;
                 $addHTTPVal('alteracao':%CHAR(WData));
                 $addHTTPVal('origem':'gbasegra');
                 $addHTTPVal();
                 WRecIni += 1;
              ENDIF;
              $RmvDBSession(WSession);
           ENDIF;
        ENDSR;

        //=====================================================
        // Subrotina.: GetLusitRamo
        // Descrição.: Obter o Código de Ramo da Lusitânia
        //=====================================================
        BEGSR GetLusitRamo;
           IF (WRamo = '80');
              $addHTTPVal('ramo':'01');
           ELSEIF (WRamo = '14');
              $addHTTPVal('ramo':'02');
           ELSEIF (WRamo = '130');
              $addHTTPVal('ramo':'03');
           ELSEIF (WRamo = '133');
              $addHTTPVal('ramo':'04');
           ELSEIF (WRamo = '342');
              $addHTTPVal('ramo':'05');
           ELSEIF (WRamo = '160');
              $addHTTPVal('ramo':'06');
           ELSEIF (WRamo = '92');
              $addHTTPVal('ramo':'07');
           ELSEIF (WRamo = '93');
              $addHTTPVal('ramo':'08');
           ELSEIF (WRamo = '94');
              $addHTTPVal('ramo':'09');
           ELSEIF (WRamo = '16');
              $addHTTPVal('ramo':'10');
           ELSEIF (WRamo = '12');
              $addHTTPVal('ramo':'15');
           ELSEIF (WRamo = '346');
              $addHTTPVal('ramo':'17');
           ELSEIF (WRamo = '96');
              $addHTTPVal('ramo':'18');
           ELSEIF (WRamo = '345');
              $addHTTPVal('ramo':'19');
           ELSEIF (WRamo = '310');
              $addHTTPVal('ramo':'50');
           ELSEIF (WRamo = '10');
              $addHTTPVal('ramo':'51');
           ELSEIF (WRamo = '343');
              $addHTTPVal('ramo':'55');
           ELSEIF (WRamo = '95');
              $addHTTPVal('ramo':'65');
           ELSEIF (WRamo = '71');
              $addHTTPVal('ramo':'68');
           ELSEIF (WRamo = '73');
              $addHTTPVal('ramo':'69');
           ELSEIF (WRamo = '72');
              $addHTTPVal('ramo':'70');
           ENDIF;
        ENDSR;
      /END-FREE
