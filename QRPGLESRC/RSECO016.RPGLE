     H DFTACTGRP(*NO) BNDDIR('JMDIR')
     H*=============================================================
     H* Programa..: RRECO010
     H* Objectivo.: Disponibilizar informação sobre os Recibos
     H*
     H* Descrição.: Este programa é invocado por uma qualquer browser
     H*             de Internet, e devolve o resultado em XML
     H*             para o efeito está preparado para devolver o
     H*             diferentes resultados em função dos parametros.
     H*
     H*=============================================================
     FFSECP034  IF   E           K DISK    PREFIX(SEC_)
     FFSECP035  IF   E           K DISK    PREFIX(SEC_)
     F*=============================================================
      /Copy QRPGLESRC,SSECO010
      /Copy QRPGLESRC,SSECO012
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010

     D*==> Campos de Trabalho
     D WURI            S          65535A
     D HTTPCODE        S          65535A
     D WSessionID      S              5S 0 INZ(*ZEROS)
     D WUserIDAlf      S              5A   INZ(*BLANKS)
     D WUserIDNum      S              5S 0 INZ(*ZEROS)
     D WPortalAlf      S              5A   INZ(*BLANKS)
     D WPortalNum      S              5S 0 INZ(*ZEROS)
     D WRemoteAddr     S             25A
     D WIcRegistos     S               N   INZ(*OFF)
     D*=============================================================
     C     *ENTRY        PLIST
     C                   PARM                    WURI
     C                   PARM                    HTTPCODE
      /FREE
        //-----------------------------------------------
        // Trabalhar função em dois modos
        //-----------------------------------------------
        MONITOR;
           WRemoteAddr = %str($getenv('REMOTE_ADDR'));
           WUserIDAlf   = $getHTTPVar('codigo');
           WPortalAlf   = $getHTTPVar('portal');
        ON-ERROR;
           WRemoteAddr  = '020.000.000.053';
           WUserIdAlf   = *BLANKS;
           WPortalAlf   = *BLANKS;
        ENDMON;
        //-----------------------------------------------
        // Validar se pode utilizar a função
        //-----------------------------------------------
        $inzHTTPSrc('XML');  // Possivel(XML, HTTP, TXT)
        IF NOT $ValAccess(S#PGM);
           HTTPCode = $getHTTPmsg('HTP0500');
           RETURN;
        ENDIF;
        $addHTTPVal('IP':WRemoteAddr);
        $addHTTPVal('user':S#USR);

        //-----------------------------------------------
        // Obter parametros passados ao Browser Web
        //-----------------------------------------------
        IF (WUserIDAlf = *BLANKS);
           HTTPCode = $getHTTPmsg('HTP0504');
           RETURN;
        ENDIF;

        MONITOR;
           WUserIDNum = %DEC(WUserIdAlf:5:0);
           WPortalNum = %DEC(WPortalAlf:5:0);
        ON-ERROR;
           HTTPCode = $getHTTPmsg('HTP0504');
           RETURN;
        ENDMON;

        WIcRegistos = *OFF;
        CHAIN (WPortalNum) RSECP035;
        IF (%FOUND());
           CHAIN (WUserIDNum:WPortalNum) RSECP034;
           IF (%FOUND());
              $addHTTPVal('cduser':%TRIM(SEC_CDUSER));
              $addHTTPVal('passw':%TRIM(SEC_PASSW));
              $addHTTPVal('url':%TRIM(SEC_DSURL));
              $addHTTPVal('curl':%TRIM(SEC_DSCURL));
              $addHTTPVal('tipo':%TRIM(SEC_TPTPORAL));
              WIcRegistos = *ON;
           ENDIF;
        ENDIF;
        IF (WIcRegistos = *OFF);
           HTTPCode = $getHTTPmsg('HTP0504');
           RETURN;
        ENDIF;

        HTTPCODE = $getHTTPSrc();
        RETURN;
      /END-FREE
