     H DFTACTGRP(*NO) BNDDIR('JMDIR')
     H*=============================================================
     H* Descrição.: Esta Template é o molde para gerar exportar o
     H*             Resultado para um qualquer browser Web, acedido
     H*             através de: http://20.0.0.1/DemoComp?...
     H*
     H* Deverá registar o Componente na inventário de Componentes do
     H* sistema através do prgrama PSECO0001.
     H*
     H* As Mensagens de erro deverá ser registadas no ficheiro de
     H* mensagens em SOAPSERV/MSG01POR.
     H*
     H* O resultado a apresentar no Browser, poderá ser em XML,
     H* HTTP (uma página Web) ou texto simples.
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
        IF ($getHTTPVar(WURI:'todos') = 'sim');
           $addHTTPVal('IP':%str($getenv('REMOTE_ADDR')));
           $addHTTPVal('user':S#USR);
           SETLL (*LOVAL) RCATP100;
           READ RCATP100;
           DOW (NOT %EOF());
              EXSR AddXMLRec;
              READ RCATP100;
           ENDDO;
        ELSE;
           MONITOR;
              CDPROD = %INT($getHTTPVar(WURI:'codigo'));
           ON-ERROR;
              HTTPCode = $getHTTPmsg('HTP0504');
              RETURN;
           ENDMON;
           CHAIN CDPROD RCATP100;
           IF NOT %FOUND;
              HTTPCode = $getHTTPmsg('HTP0201');
              RETURN;
           ELSE;
              EXSR AddXMLRec;
           ENDIF;
        ENDIF;

        HTTPCODE = $getHTTPSrc();
        *INLR = *ON;

        //------------------------------------------------------
        // Obter valores da Tabela
        //------------------------------------------------------
        BEGSR AddXMLRec;
           $addHTTPVal('data');
           $addHTTPVal('codigo':%CHAR(CDPROD));
           $addHTTPVal('ramo':%CHAR(CDRAMO));
           $addHTTPVal('nome':DSPROD);
           $addHTTPVal('frame':$getHTTPVar(WURI:'frame'));
           $addHTTPVal('apolice':$getHTTPVar(WURI:'apolice'));
           $addHTTPVal('observa':'Chamada externa');
           $addHTTPVal();
        ENDSR;
      /END-FREE
