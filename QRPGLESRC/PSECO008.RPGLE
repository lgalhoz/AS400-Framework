     H DFTACTGRP(*NO) BNDDIR('JMDIR')
     H*=============================================================
     H* Este objecto destina-se a gerir as chamadas a funções do AS400
     H* invocadas através de HTTP, equivale a um INDEX.PHP noutras linguagens.
     H*
     H* Como medida de segurança adoptada, foi implementada uma
     H* gestão de sessões, com uma validade de 30 minutos.
     H* Para utilizar qualquer função, deverá primeiro autenticar-se
     H*
     H* Por esta via passa a ser possível entregar reportings gerados
     H* on AS/400, por HTTP de uma forma segura.
     H*--------------------------------------------------------------
     H* Alterações a implementar no ficheiro de configuração do Apache
     H*   Ficheiro: /www/apachedft/conf/httpd.conf
     H*--------------------------------------------------------------
     H*       ScriptAlias /DemoComp /qsys.lib/#JMPRDOBJ.lib/pseco008.pgm
     H*       ScriptAlias /test /qsys.lib/#JMPRDOBJ.lib/ptsto100.pgm
     H*       <Directory /qsys.lib/#JMPRDOBJ.lib>
     H*          Order Allow,Deny
     H*          Allow From All
     H*       </Directory>
     H*--------------------------------------------------------------
     H*  Terminar o Apache: ENDTCPSVR SERVER(*HTTP)
     H*  Iniciar o Apache.: STRTCPSVR SERVER(*HTTP)
     H*=============================================================
     FFSECP01002IF   E           K DISK    RENAME(RSECP010:RSECP01002)
     F                                     PREFIX(SEC_)
     F*=============================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SSECO012

     D*==> Chamada dynamica da Frame
     D CallFrame       PR                  EXTPGM(DynamicCall)
     D   PARMS                    65535A   CONST
     D   XMLCODE                  65535A   CONST
     D
     D*==> Campos de Trabalho
     D DynamicCall     S             10A
     D WURI            S           5000A   varying
     D WIP             S             15A   varying
     D WData           S          65535A
     D WSSID           S              5S 0
     D WFrame          S             10A
     D WUser           S             10A
     D WPassw          S             10A
     D*=============================================================
      /FREE
        //-------------------------------------------------------
        // Obter o valor escrito no Site
        //-------------------------------------------------------
        MONITOR;
           WURI   = %str($getenv('REQUEST_URI'));
           WIP    = %str($getenv('REMOTE_ADDR'));
           WFrame = $getHTTPVar('frame');
           WUser  = $getHTTPVar('user');
           WPassw = $getHTTPVar('passw');
        ON-ERROR;
           WURI = 'http://20.0.0.1/DemoComp?frame=apolseg&nif=198757751';
           WIP = '20.0.0.53';
           WFrame = 'apolseg';
           WUser  = 'APACHE';
           WPassw = *BLANKS;
        ENDMON;
        MONITOR;
           IF (WFrame = 'login');
              WSSID = $addHTTPSess(WUser:WPassw:WIP);
              IF (WSSID > *ZEROS);
                 WData = $getHTTPmsg('HTP0200':'OK');
              ELSE;
                 WData = $getHTTPmsg('HTP0499');
              ENDIF;
           ELSEIF ($ValHTTPSess(WIP)); // Validar se está autorizado
              IF (WFrame <> *BLANKS);
                 CHAIN (WFrame) RSECP01002;
                 IF (%FOUND());
                    DynamicCall = SEC_NMPROGR;
                    CallFrame(WURI:WData);
                 ELSE;
                    WData = $getHTTPmsg('HTP0501');
                    WData = $StrReplace('#VAR01':WFrame:WData);
                 ENDIF;
              ELSE;
                 WData = $getHTTPmsg('HTP0502');
              ENDIF;
           ELSE;
              WData = $getHTTPmsg('HTP0500');
           ENDIF;
        ON-ERROR;
           WData = $getHTTPmsg('HTP0503');
        ENDMON;
        IF (WData = *BLANKS);
           WData = $getHTTPmsg('HTP0001');
           WData = $StrReplace('#VAR02':WFrame:WData);
           WData = $StrReplace('#VAR01':DynamicCall:WData);
        ENDIF;
        $WrtStOut(WData:%len(%trimr(WData)): $ErrDS);

        *INLR = *ON;
      /END-FREE
