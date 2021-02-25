     H DFTACTGRP(*NO) BNDDIR('JMDIR')
     H*=============================================================
     FFCATP100  IF   E           K DISK
     F*=============================================================
      /Copy QRPGLESRC,SSECO010
      /Copy QRPGLESRC,SSECO012
      /Copy QRPGLESRC,SGRLO010

     D*==> Campos de Trabalho
     D   WURI          S          65535A
     D   HTTPCODE      S          65535A
     D*=============================================================
     C     *ENTRY        PLIST
     C                   PARM                    WURI
     C                   PARM                    HTTPCODE
      /FREE
        //-----------------------------------------------
        // Validar se pode utilizar a função
        //-----------------------------------------------
        $inzHTTPSrc('XML');  // Possivel(XML, HTTP, TXT)
        IF NOT $ValAccess(S#PGM);
           HTTPCode = $getHTTPmsg('HTP0500');
           RETURN;
        ENDIF;

        //-----------------------------------------------
        // Obter a chave
        //-----------------------------------------------
        IF ($getHTTPVar('todos') = 'sim');
           $addHTTPVal('IP':%str($getenv('REMOTE_ADDR')));
           $addHTTPVal('user':S#USR);
           SETLL (*LOVAL) RCATP100;
           READ RCATP100;
           DOW (NOT %EOF());
              EXSR AddXMLRec;
              READ RCATP100;
           ENDDO;
        ELSE;
           CDPROD = %INT($getHTTPVar('codigo':*ON));
           IF (CDPROD = *ZEROS);
              HTTPCode = $getHTTPmsg('HTP0201');
              RETURN;
           ENDIF;

           CHAIN CDPROD RCATP100;
           IF NOT %FOUND;
              HTTPCode = $getHTTPmsg('HTP0201');
              RETURN;
           ELSE;
              EXSR AddXMLRec;
           ENDIF;
        ENDIF;

        HTTPCODE = $getHTTPSrc();
        RETURN;

        //------------------------------------------------------
        // Obter valores da Tabela
        //------------------------------------------------------
        BEGSR AddXMLRec;
           $addHTTPVal('data');
           $addHTTPVal('codigo':%CHAR(CDPROD));
           $addHTTPVal('ramo':%CHAR(CDRAMO));
           $addHTTPVal('nome':DSPROD);
           $addHTTPVal('frame':$getHTTPVar('frame'));
           $addHTTPVal('apolice':$getHTTPVar('apolice':*ON));
           $addHTTPVal('observa':'Chamada externa');
           $addHTTPVal();
        ENDSR;
      /END-FREE
