     H DFTACTGRP(*NO) BNDDIR('JMDIR')
     H*=============================================================
     H* Programa..: RPRDO020
     H* Descrição.: Devolver informação das Actas. através dos
     H*             seguintes modos:
     H*             Modo Lista - Devolve a lista das Actas
     H*             Modo Acta  - Devolve a informação da Acta
     H*=============================================================
     FHISTORICO IF   F 1000        DISK    USROPN EXTFILE(WFileName)
     F*=============================================================
      /Copy QRPGLESRC,SSECO010
      /Copy QRPGLESRC,SSECO012
      /Copy QRPGLESRC,SGRLO010

     D*==> Formato de Registo
     D RHISTORIC     E DS                  ExtName('GEXAP')
     D
     D
     D*==> Campos de Trabalho
     D   WURI          S          65535A
     D   HTTPCODE      S          65535A
     D
     D*==> Restantes campos de Trabalho
     D WAno            S              4A
     D WMes            S              2A
     D WCarrega        S              1A
     D WRamo           S              2S 0
     D WApolice        S              7S 0
     D WRecibo         S              9S 0
     D WFileName       S             10A
     D WIcExiste       S               N
     D WData           S              8S 0
     D WIcCarrega      S               N
     D WOperacao       S             15A
     D*=============================================================
     IHISTORICO AA  11
     I                                  1 1000  RHISTORIC
     I*=============================================================
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
        // Validar os Parametros recebidos
        //-----------------------------------------------
        WData = %DEC($getHTTPVar('data':*ON):8:0);
        IF (WData = *ZEROS);
           HTTPCode = $getHTTPmsg('HTP0204');
           RETURN;
        ENDIF;

        WCarrega = $getHTTPVar('carrega');
        IF (WCarrega <> *BLANKS);
           WIcCarrega = *ON;
           WMes = %TRIM($getHTTPVar('mes'));
           IF (WMes <> *BLANKS);
              WFileName = 'GEXAP'+WMes+WCarrega;
           ELSE;
              HTTPCode = $getHTTPmsg('HTP0202');
              RETURN;
           ENDIF;
        ELSE;
           WIcCarrega = *OFF;
           WAno = %TRIM($getHTTPVar('ano'));
           WMes = %TRIM($getHTTPVar('mes'));
           IF (WAno <> *BLANKS) AND (WMes <> *BLANKS);
              WFileName = 'GEAP'+%SUBST(WANO:3:2)+WMes;
           ELSE;
              HTTPCode = $getHTTPmsg('HTP0203');
              RETURN;
           ENDIF;
        ENDIF;

        //-----------------------------------------------------
        // Validar a existência do ficheiro
        //-----------------------------------------------------
        IF (NOT $checkObject(WFileName:'QS36F'));
           HTTPCode = $getHTTPmsg('HTP0205');
           HTTPCode = $StrReplace('#VAR01':WFileName:HTTPCode);
           HTTPCode = $StrReplace('#VAR02':'QS36F':HTTPCode);
           RETURN;
        ENDIF;

        //-----------------------------------------------------
        // Validar a operação pedida
        //-----------------------------------------------------
        WOperacao = $getHTTPVar('opera');
        IF (WOperacao = 'lista');
           EXSR MakeList;
        ELSE;
           EXSR ValParms;
        ENDIF;

        RETURN;

        //=================================================================
        // Subrotina..: MakeList
        // Objectivo..: construir uma lista do que contém o ficheiro
        //=================================================================
        BEGSR MakeList;
           $inzHTTPSrc('XML');  // Possivel(XML, HTTP, TXT)
           OPEN HISTORICO;
           READ HISTORICO;
           DOW (NOT %EOF());
              $addHTTPVal('data');
              $addHTTPVal('ramo':%CHAR(RR));
              $addHTTPVal('apol':%CHAR(APOL));
              $addHTTPVal('recib':%CHAR(NREC));
              $addHTTPVal('segurado':SEG);
              $addHTTPVal();
              READ HISTORICO;
           ENDDO;
           CLOSE HISTORICO;
           HTTPCODE = $getHTTPSrc();
        ENDSR;

        //=================================================================
        // Subrotina..: ValParms
        // Objectivo..: Validar se os parametros estão correctos
        //=================================================================
        BEGSR ValParms;
          WRecibo  = %DEC($getHTTPVar('rec':*ON):9:0);  // 9,0
          IF (WRecibo = *ZEROS);
             WRamo    = %DEC($getHTTPVar('ramo':*ON):2:0);  // 2,0
             WApolice = %DEC($getHTTPVar('apol':*ON):7:0);  // 7,0
             IF (WRamo <> *ZEROS) AND (WApolice <> *ZEROS);
                EXSR GetActa;
             ELSE;
                HTTPCode = $getHTTPmsg('HTP0206');
                RETURN;
             ENDIF;
          ELSE;
             EXSR GetActa;
          ENDIF;
        ENDSR;

        //=================================================================
        // Subrotina..: GetActa
        // Objectivo..: Obter a informação da Acta
        //=================================================================
        BEGSR GetActa;
           WIcExiste = *OFF;
           $inzHTTPSrc('XML');  // Possivel(XML, HTTP, TXT)
           OPEN HISTORICO;
           READ HISTORICO;
           DOW (NOT %EOF());
              IF ((WRamo = RR) AND (WApolice = APOL)) OR
                  (WRecibo = NREC);
                 WIcExiste = *ON;
                 $addHTTPVal('data');
                 $addHTTPVal('ramo':%CHAR(RR));
                 $addHTTPVal('apol':%CHAR(APOL));
                 $addHTTPVal('recib':%CHAR(NREC));
                 $addHTTPVal('segurado':SEG);
                 $addHTTPVal();
              ENDIF;
              READ HISTORICO;
           ENDDO;
           CLOSE HISTORICO;

           //-------------------------------------------
           // Validar se encontrou registos
           //-------------------------------------------
           IF (NOT WIcExiste);
              HTTPCode = $getHTTPmsg('HTP0207');
              RETURN;
           ELSE;
              HTTPCODE = $getHTTPSrc();
           ENDIF;
        ENDSR;
      /END-FREE
