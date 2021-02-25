     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     **====================================================================
     ** Programa..: PRPTO010
     ** Objectivo.: Extracto das Cobranças realizadas no dia para o Mediador
     **
     H* Execução..: http://20.0.0.1/josemata?frame=login&user=apache
     H*             http://20.0.0.1/josemata?frame=reccobdia
     H*             http://20.0.0.1/josemata?frame=reccobdia&data=20150707
     H*             http://20.0.0.1/josemata?frame=reccobdia&data=20150707&med=1
     H*
     **====================================================================
      /Copy QRPGLESRC,SSECO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SSECO012
      /Copy QRPGLESRC,SGRLO010

     D*==> Campos de trabalho
     D WURI            S          65535A
     D HTTPCODE        S          65535A
     D
     D WData           S              8A
     D WData           S              6A
     D WNumMed         S              6A
     D
     D WSession        S              5S 0
     D WRecDIR         S              5S 0
     D WMsgErro        S              7A
     D*====================================================================
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

        //------------------------------------------------------------------
        // Tratar parametros de entrada
        //------------------------------------------------------------------
        WMsgErro = *BLANKS;
        MONITOR;
           WData      = $getHTTPVar('data');
           WNumMed    = $getHTTPVar('mediador');
        ON-ERROR;
           WData    = *BLANKS; // Formatar com a data do dia
           WNumMed  = *BLANKS;
        ENDMON;

        EXSR ExtractData;

        HTTPCODE = $getHTTPSrc();
        RETURN;

        //=============================================================
        // Subrotina.: ExtractData
        // Objectivo.: Extair os registos do ficheiro de Recibos
        //=============================================================
        BEGSR ExtractData;
           WSession = $SetFiles('QS36F/"SD.1.REC"');
           $SetFields('NDP,UNACHX,FP,DPAG,DTCOB,NORD2,CODMOV,CRAMO,+
                       APOLIC,NREC2,DTINIC,DTFIM,VTREC,COMIS1,+
                       MCCOB');
           $SetSort('MCCOB,UNACHX');
           IF (WNumMed <> *BLANKS);
               $SetCriteria('(MCCOB = WNumMed) +
                             AND (DTCOB=''' + WDatCobr + ''')');
           ELSE;
           ENDIF;
               $SetCriteria('(DTCOB=''' + WDatCobr + ''')');
           DOW ($GetRecord(WRecDIR) > *ZEROS);
           // WNomFile    = $GetField('NREC2');
              WRecDIR += 1;
           ENDDO;
           $RmvDBSession(WSession);
        ENDSR;
      /END-FREE
