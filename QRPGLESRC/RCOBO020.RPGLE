     H DFTACTGRP(*NO) BNDDIR('JMDIR')
     H*=============================================================
     H* Programa..: RCOBO020
     H* Objectivo.: Disponibilizar informação sobre as Cartas dos
     H*             Credores Hipotecários.
     H*
     H* Descrição.: Este programa é invocado por uma qualquer browser
     H*             de Internet, e devolve o resultado em XML
     H*             para o efeito está preparado para devolver o
     H*             diferentes resultados em função dos parametros.
     H*
     H*
     H*=============================================================
     FGC895     IF   E           K DISK    PREFIX(REC_)
     F*=============================================================
      /Copy QRPGLESRC,SSECO010
      /Copy QRPGLESRC,SCATO010
      /Copy QRPGLESRC,SSECO012
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010

     D*==> Campos de Trabalho
     D WURI            S          65535A
     D HTTPCODE        S          65535A
     D WSessionID      S              5S 0 INZ(*ZEROS)
     D WRemoteAddr     S             25A
     D
     D*==> Parametros Recebidos
     D WRecIni         S             10S 0 INZ(*ZEROS)
     D WQtdeRec        S              2S 0 INZ(*ZEROS)
     D
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
           // WNumMed      = $getHTTPVar('mediador');
           MONITOR;
           ON-ERROR;
           ENDMON;
        ON-ERROR;
           WRemoteAddr  = '020.000.000.053';
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
        $addHTTPVal('stscod':'200');
        // $addHTTPVal('ini':%CHAR(WRecIni));
        // $addHTTPVal('qtde':%CHAR(WQtdeRec));

        //-----------------------------------------------
        // Obter parametros passados ao Browser Web
        //-----------------------------------------------
        // IF (WNumMed = *BLANKS);
        //    HTTPCode = $getHTTPmsg('HTP0504');
        //    RETURN;
        // ENDIF;

        SETLL *LOVAL GC895R;
        READ GC895R;
        DOW (NOT %EOF(GC895));
           IF (REC_JMMCC > 0);
              $addHTTPVal('data');
              $addHTTPVal('recibo':%EDITW(REC_GCNREC:'0         '));
              $addHTTPVal('ramo':%EDITW(REC_GCRAMO:'0  '));
              $addHTTPVal('apolice':%EDITW(REC_GCAPOL:'0       '));
              $addHTTPVal('nome':REC_GCNOM);
              $addHTTPVal('mediador':%EDITW(REC_JMMCC:'0   '));
              $addHTTPVal();
           ENDIF;
           READ GC895R;
        ENDDO;

        HTTPCODE = $getHTTPSrc();
        RETURN;
      /END-FREE
