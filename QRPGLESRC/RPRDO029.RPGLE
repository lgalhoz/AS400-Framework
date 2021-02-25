     H DFTACTGRP(*NO) BNDDIR('JMDIR')
     H*====================================================================
     H* Programa..: RPRDO029
     H* Descrição.: Devolve Informação das Cartas Verdes
     H*
     H* Execução..: http://20.0.0.1/DemoComp?frame=login&user=apache
     H*             http://20.0.0.1/DemoComp?frame=getcverde&apolice=9956715
     H*
     H* Combinações:
     H*
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
     D WApolice        S              7A
     D WRecibo         S              9A
     D WCverdIni       S              8A
     D WCverdFim       S              8A
     D WAnoIni         S              4A
     D WAnoFim         S              4A
     D
     D WNuApol         S              3S 0
     D WCodRamo        S              2S 0
     D WRemoteAddr     S             25A
     D WSession        S              5S 0
     D WRecIni         S              5S 0
     D WCriteria       S            100A
     D WRecSize        S              5S 0
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
        WSession = *ZEROS;

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
           WApolice   = $getHTTPVar('apolice');
           WRecibo    = $getHTTPVar('recibo');
           WCverdIni  = $getHTTPVar('ini');
           WCverdFim  = $getHTTPVar('fim');
        ON-ERROR;
           WNIF       = *BLANKS;
           WNSeg      = *BLANKS;
           WMatricula = *BLANKS;
           WApolice   = *BLANKS;
           WRecibo    = *BLANKS;
           WCverdIni  = *BLANKS;
           WCverdFim  = *BLANKS;
        ENDMON;

        IF (WCverdFim = *BLANKS);
           WCverdFim = WCverdIni;
        ENDIF;

        //-----------------------------------------------------
        // Obter informação da Carta Verde
        //-----------------------------------------------------
        IF ((WRecibo <> *BLANKS) OR (WApolice <> *BLANKS)) OR
          ((WCverdIni <= WCverdFim) AND (WCverdIni > *ZEROS));
           IF (WRecibo <> *BLANKS);
              WCriteria = '(NREC=' + %TRIM(WREcibo) +')';
           ELSEIF (WMatricula <> *BLANKS);
              WCriteria = '(NUMATR1=''' + %TRIM(WMatricula) + ''') OR ' +
                          '(NUMATR2=''' + %TRIM(WMatricula) + ''')';
           ELSEIF (WApolice <> *BLANKS);
              WCriteria = '(NUAPOL='''+WApolice+''')';
           ELSE;
              WAnoIni   = '20' + %SUBST(WCVerdIni:7:2);
              WCVerdIni = %SUBST(WCVerdIni:1:6);
              WAnoFim   = '20' + %SUBST(WCVerdFim:7:2);
              WCVerdFim = %SUBST(WCVerdFim:1:6);
              WCriteria = '(ANNCV >= ' + WAnoIni + ') AND +
                           (ANNCV <= ' + WAnoFim + ') AND +
                           (NCV >= ' + WCverdIni + ') AND +
                           (NCV <= ' + WCVerdFim + ')';
           ENDIF;
           EXSR GetCVerde;
        ENDIF;


        //-----------------------------------------------------
        // Validar se encontrou registos
        //-----------------------------------------------------
        IF (WRecIni = 1);
           $addHTTPVal('erro');
           $addHTTPVal('criteria':WCriteria);
           $addHTTPVal();
        // HTTPCode = $getHTTPmsg('HTP0207');
        // RETURN;
        ENDIF;

        HTTPCODE = $getHTTPSrc();
        RETURN;

        //=====================================================
        // Subrotina.: GetCVerde
        // Descrição.: Obter os registos da Carta Verde
        //=====================================================
        BEGSR GetCVerde;
           WSession = $SetFiles('FDOCP060');
           $SetFields('NCV,ANNCV,NUAPOL,NREC,NOMSEG');
           $SetCriteria(%TRIM(WCriteria));
           WRecIni = 1;
           DOW ($GetRecord(WRecIni) > *ZEROS);
              WNuApol = $GetFieldNum('NUAPOL');
              WCodRamo = $GetFieldNum('CRAMO');

              $addHTTPVal('apolice');
              $addHTTPVal('N':%CHAR(WRecIni));
              $addHTTPVal('CV':$GetField('NCV'));
              $addHTTPVal('ANCV':$GetField('ANNCV'));
              $addHTTPVal('APOL':$GetField('NUAPOL'));
              $addHTTPVal('REC':$GetField('NREC'));
              $addHTTPVal('SEG':$GetField('NOMSEG'));
              $addHTTPVal();
              WRecIni += 1;
           ENDDO;
           $RmvDBSession(WSession);
        ENDSR;
      /END-FREE
