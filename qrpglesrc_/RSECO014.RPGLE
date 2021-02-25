     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') AUT(*ALL)
     H*DFTACTGRP(*NO)
     H*=======================================================
     H* Dat.Cria...: 2012-11-13
     H* Autor......: Luis Galhoz
     H* Objectivo..: Controlar funções HTTP
     H*=======================================================
     FFSECP031  UF A E           K DISK    PREFIX(SEC_)
     FFSECP032  IF   E           K DISK    PREFIX(SEC_)
     FFSECP033  UF A E           K DISK    PREFIX(SEC_)
     FFSECP03101IF   E           K DISK    PREFIX(SEC_)
     F                                     RENAME(RSECP031:RSECP03101)
     F*=======================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SSECO012
     D
     D WHTTPSrc        S          65535A
     D WTipSource      S             25A
     D WTipo           S              5A
     D WCodigo         S              3S 0
     D WMensagem       S             20A
     D
     D*==> Controlo de Tags
     D WTagList        DS                  QUALIFIED DIM(25)
     D   TagName                     50A   INZ(*BLANKS)
     D   IsEmpty                       N   INZ(*ON)
     D*=======================================================
      /FREE

         RETURN;
         *INLR = *ON;
      /END-FREE

     P*=======================================================
     P* Processo..: $getHTTPVar
     P* Objectivo.: Obter o parametro HTTP inserido
     P*=======================================================
     P $getHTTPVar     B                   EXPORT
     D                 PI           200A
     D  KEY                          20A   CONST
     D  DEC                            N   CONST OPTIONS(*NOPASS)
     D  URL                        5000A   CONST OPTIONS(*NOPASS)
     D
     D WPosIni         S              4S 0
     D WPosFim         S              4S 0
     D WStrLen         S              4S 0
     D WURL            S                   LIKE(URL)
     D WURL_BAK        S                   LIKE(URL)
     D WKey            S                   LIKE(KEY)
     D WDec            S                   LIKE(DEC)
     D WResult         S            200A   INZ(*BLANKS)
     D WTesIntVal      S             20i 0
     D*=============================================
      /FREE
         //-----------------------------------------------------------
         // A URL só é passada como parâmetro em ambiente de testes
         //-----------------------------------------------------------
         WDec = *OFF;
         IF (%PARMS() > 1);
            WDec = DEC;
            IF (%PARMS() > 2);
               WURL = URL;
            ELSE;
               WURL = %str($getenv('REQUEST_URI'));
            ENDIF;
         ELSE;
            WDec = *OFF;
            WURL = %str($getenv('REQUEST_URI'));
         ENDIF;
         WURL_BAK = WURL;
         WURL = %TRIM($StrToUpper(WURL));

         WKey = '?'+%TRIM($StrToUpper(KEY))+'=';
         WPosIni = %SCAN(%TRIM(WKey):WURL);
         IF (WPosIni = *ZEROS);
            WKey = '&'+%TRIM($StrToUpper(KEY))+'=';
            WPosIni = %SCAN(%TRIM(WKey):WURL);
            IF (WPosIni = *ZEROS);
               WResult = *BLANKS;
               RETURN *BLANKS;
            ENDIF;
         ENDIF;

         WStrLen = %LEN(%TRIM(WKey));
         WPosFim = WPosIni + WStrLen;
         WURL = %SUBST(WURL:WPosFim);
         WPosIni = WPosFim;

         WPosFim = %SCAN('&':WURL);
         IF (WPosFim > 0);
            WPosFim += WPosIni;
            WResult = %SUBST(WURL_BAK:WPosIni:(WPosFim-WPosIni-1));
         ELSE;
            WResult = %SUBST(WURL_BAK:WPosIni);
         ENDIF;

         IF (WDec);
            IF (WResult = *BLANKS);
               WResult = '0';
            ELSE;
               MONITOR;
                  WTesIntVal = %INT(%TRIM(WResult));
               ON-ERROR;
                  WResult = '0';
               ENDMON;
            ENDIF;
         ENDIF;
         RETURN %TRIM(WResult);
      /END-FREE
     P $getHTTPVar     E

     P*=======================================================
     P* Processo..: $valHTTPSess
     P* Objectivo.: Validar sessão
     P*=======================================================
     P $valHTTPSess    B                   EXPORT
     D                 PI              N
     D  WIPValue                     15A   CONST
     D
     D  WDatTime       S               Z
     D  WMinutos       S              3S 0
     D  WAutoriza      S               N
     D  WSession       S              3S 0
     D*=============================================
      /FREE
         //------------------------------------------------
         //
         //------------------------------------------------
         WAutoriza = *OFF;
         CHAIN WIPValue RSECP03101;
         IF (%FOUND());
            //-----------------------------------
            // validar se está dentro do prazo
            //-----------------------------------
            WDatTime = SEC_DTACT + SEC_HRACT;
            WMinutos = %diff(%TimeStamp():WDatTime:*minutes);
            IF (WMinutos > 30);
               DELETE (SEC_CDSESSAO) RSECP031;
               EXSR ValExcept;
            ELSE;
               CHAIN (SEC_CDSESSAO) RSECP031;
               SEC_DTACT = %DATE();
               SEC_HRACT = %TIME();
               UPDATE RSECP031 %FIELDS(SEC_DTACT:SEC_HRACT);
               WAutoriza = *ON;
            ENDIF;
         ELSE;
            EXSR ValExcept;
         ENDIF;
         RETURN WAutoriza;

        //==========================================================
        // Subrotina.: ValExcept
        // Objectivo.: Validar Excepção
        //==========================================================
        BEGSR ValExcept;
           CHAIN WIPValue RSECP032;
           IF (%FOUND());
              IF (SEC_ISACESS = 'A');
                 WSession = $addHTTPSess(SEC_DFUSER:*BLANKS:WIPValue);
                 IF (WSession > *ZEROS);
                    WAutoriza = *ON;
                 ENDIF;
              ENDIF;
           ENDIF;
        ENDSR;
      /END-FREE
     P $valHTTPSess    E

     P*=======================================================
     P* Processo..: $addHTTPSess
     P* Objectivo.: Criar uma nova sessão HTTP
     P*=======================================================
     P $addHTTPSess    B                   EXPORT
     D                 PI             5S 0
     D   WUser                       10A   CONST
     D   WPassw                      10A   CONST
     D   WIPVal                      15A   CONST
     D
     D ValidUser       PR                  EXTPGM('QSYGETPH')
     D   WKUser                      10A   CONST
     D   WKPassword                  10A   CONST
     D   ProfileHandl                12A   CONST
     D   QUSEC                             LIKEDS(QUSEC) CONST
     D   PasswordLen                 10i 0 CONST
     D   CCSID                       10i 0 CONST
     D
     D ProfileHandle   S             12A
     D PassWordLength  S             10i 0 inz(10)
     D CCSID           S             10i 0 inz(37)
     D
     D QUSEC           DS
     D   QUSBPRV               1      4B 0
     D   QUSBAVL               5      8B 0
     D   QUSEI                 9     15
     D   QUSERVED             16     16
     D   ERRC0100             17    274    Varying
     D ERROR           S               N
     D WSession        S              5S 0
     D WNuTenta        S              5S 0
     D*=============================================
      /FREE
        //-------------------------------------------
        // Se já existir um registo então elimina
        //-------------------------------------------
        WSession = *ZEROS;
        CHAIN WIPVAL RSECP03101;
        IF (%FOUND());
           DELETE (SEC_CDSESSAO) RSECP031;
        ENDIF;

        //-------------------------------------------
        // Validar situação do Utilizador
        //-------------------------------------------
        CHAIN(N) ($StrToUpper(WUser)) RSECP033;
        IF (%FOUND());
           IF (SEC_ISACESS = 'R'); // Acesso recusado para o Utilizador
              WSession = *ZEROS;
           ELSEIF (SEC_ISPASS = 'N'); // Pode aceder sem password
              IF (%TRIM($StrToUpper(WPassw)) = %TRIM(SEC_PASSW));
                 EXSR AddSession;
              ENDIF;
           ELSE;
              EXSR ValidPass; // Só pode aceder com uma password valida
           ENDIF;
        ELSE;
           EXSR ValidPass; // Só pode aceder com uma password valida
        ENDIF;

        RETURN WSession;

        //==========================================================
        // Subrotina.: ValidPass
        // Objectivo.: Validar Palavra passe
        //==========================================================
        BEGSR ValidPass;
           IF (WUser <> *BLANKS);
              ValidUser(%TRIM($StrToUpper(WUser)):%TRIM($StrToUpper(WPassw))
                               :ProfileHandle:QUSEC:10:CCSID);
              IF  (QUSBAVL = 0);
                 EXSR AddSession;
              ELSE;
                 WNuTenta = 1;
                 EXSR ActSession;
              ENDIF;
           ENDIF;
        ENDSR;

        //==========================================================
        // Subrotina.: AddSession
        // Objectivo.: Criar uma sessão para o Utilizador
        //==========================================================
        BEGSR AddSession;
           WSession = $Random();

           SEC_CDSESSAO = *ZEROS;
           SETGT (*HIVAL) RSECP031;
           READP(N) RSECP031;
           SEC_CDSESSAO += 1;
           SEC_IP       = WIPVAL;
           SEC_CDUSER   = WUser;
           SEC_SSID     = WSession;
           SEC_ICESTADO = 'A';
           SEC_DTINI    = %DATE();
           SEC_HRINI    = %TIME();
           SEC_DTACT    = %DATE();
           SEC_HRACT    = %TIME();
           WRITE RSECP031;

           WNuTenta = *ZEROS;
           EXSR ActSession;
        ENDSR;

        //==========================================================
        // Subrotina.: ActSession
        // Objectivo.: Actualizar informação da Sessão
        //==========================================================
        BEGSR ActSession;
           CHAIN ($StrToUpper(WUser)) RSECP033;
           IF (%FOUND());
              SEC_SEC03302 = %DATE();
              SEC_NUTENTA  = (SEC_NUTENTA*WNuTenta)+WNuTenta;
              IF (SEC_NUTENTA > 3);
                 SEC_ISACESS = 'R';
              ENDIF;
              UPDATE RSECP033 %FIELDS(SEC_NUTENTA:SEC_ISACESS:SEC_SEC03302);
           ELSE;
              SEC_CDUSER   = $StrToUpper(WUser);
              SEC_ISPASS   = 'S';
              SEC_ISACESS  = 'A';
              SEC_NUTENTA  = (SEC_NUTENTA*WNuTenta)+WNuTenta;
              SEC_SEC03301 = %DATE();
              SEC_SEC03302 = %DATE();
              WRITE RSECP033;
           ENDIF;
        ENDSR;
      /END-FREE
     P $addHTTPSess    E

     P*=======================================================
     P* Processo..: $inzHTTPSrc
     P* Objectivo.: Iniciar o source HTTP
     P*=======================================================
     P $inzHTTPSrc     B                   EXPORT
     D                 PI
     D  TipSource                     5A   CONST OPTIONS(*NOPASS)
     D*=============================================
      /FREE
        WTipo      = TipSource;
        WTipSource = CNT_TXT;
        WHTTPSrc   = *BLANKS;
        WCodigo    = 200;
        WMensagem  = *BLANKS;
        WTagList(1).IsEmpty = *ON;

        IF (%PARMS() > *ZEROS);
           IF (TipSource = 'XML');
           ELSEIF (TipSource = 'HTML');
              WTipSource = CNT_HTML;
           ELSEIF (TipSource = 'TXT');
              WTipSource = CNT_TXT;
           ENDIF;
        ENDIF;
      /END-FREE
     P $inzHTTPSrc     E

     P*=======================================================
     P* Processo..: $getHTTPSrc
     P* Objectivo.: Obter o texto a apresentar no Site
     P*=======================================================
     P $getHTTPSrc     B                   EXPORT
     D                 PI         65535A
     D
     D WIdx            S              3S 0
     D WLastIdx        S              3S 0
     D*=============================================
      /FREE
        WIdx = 1;
        DOW (NOT WTagList(WIdx).IsEmpty);
           WLastIdx = WIdx;
           WIdx += 1;
        ENDDO;

        DOW (WLastIdx > *ZEROS);
           WHTTPSrc = %TRIM(WHTTPSrc)+'</'+
                      %TRIM(WTagList(WLastIdx).TagName)+'>'+CRLF;
           WTagList(WLastIdx).IsEmpty = *ON;
           WTagList(WLastIdx).TagName = *BLANKS;
           WLastIdx -= 1;
        ENDDO;

        IF (WTipo = 'HTML');
           WHTTPSrc = %TRIM(WHTTPSrc) + '</HTML>';
        ELSEIF (WTipo = 'XML');
           WHTTPSrc = %TRIM(WHTTPSrc) + '</result>';
        ENDIF;

        RETURN WHTTPSrc;
      /END-FREE
     P $getHTTPSrc     E

     P*=======================================================
     P* Processo..: $addHTTPInfo
     P* Objectivo.: Adicionar o Estado da Operação
     P*=======================================================
     P $addHTTPInfo    B                   EXPORT
     D                 PI
     D   PCodigo                      4S 0 CONST
     D   PMensagem                  300A   CONST OPTIONS(*NOPASS)
     D*=============================================
      /FREE
        WCodigo   = PCodigo;
        WMensagem = PMensagem;
      /END-FREE
     P $addHTTPInfo    E

     P*=======================================================
     P* Processo..: $addHTTPval
     P* Objectivo.: Adicionar um registo HTTP
     P*=======================================================
     P $addHTTPval     B                   EXPORT
     D                 PI
     D   PTagName                    50A   CONST OPTIONS(*NOPASS)
     D   PValue                     300A   CONST OPTIONS(*NOPASS)
     D
     D WIdx            S              2S 0
     D WTagName        S             50A
     D WValue          S            300A
     D*=============================================
      /FREE
        WTagName = PTagName;
        WValue   = PValue;

        IF (WHTTPSrc = *BLANKS);
           WHTTPSrc =  'Status: '+ %CHAR(WCodigo)
                    + ' ' + %TRIM(WMensagem) + CRLF
                    + %TRIM(WTipSource)+ ' href="cdcatalog.xsl"'+CRLF
                    + CRLF;
           IF (WTipo = 'HTML');
              WHTTPSrc = %TRIM(WHTTPSrc) + '<HTML>' + CRLF;
           ELSEIF (WTipo = 'XML');
           // WHTTPSrc = %TRIM(WHTTPSrc) + '<result>' + CRLF;
              WHTTPSrc = 'Content-type: text/xml ' + CRLF + CRLF
              + '<?xml version="1.0" encoding="ISO-8859-1"?>'
              + '<result>' + CRLF;
           ENDIF;
        ENDIF;

        //----------------------------------
        // Posicionar no último livre
        //----------------------------------
        WIdx = 1;
        DOW (NOT WTagList(WIdx).IsEmpty);
           WIdx += 1;
        ENDDO;

        //----------------------------------
        // 2=TAG+NAME; 1=TAG; 0=''
        //----------------------------------
        IF (%PARMS() = 2);
           WValue = $StrReplace('&':'&#x26;':WValue);
           WValue = $StrReplace('<':'&#x60;':WValue);
           WValue = $StrReplace('>':'&#x62;':WValue);
           IF (WTipo <> 'TXT');
              WHTTPSrc = %TRIM(WHTTPSrc) + '<' + %TRIM(WTagName) + '>' +
                         %TRIM(WValue)+'</'+%TRIM(WTagName)+'>' + CRLF;
           ELSE;
              WHTTPSrc = %TRIM(WHTTPSrc)+';'+%TRIM(WValue)+CRLF;
           ENDIF;
        ELSEIF (%PARMS() = 1);
           IF (WTipo = 'XML');
              WTagList(WIdx).IsEmpty = *OFF;
              WTagList(WIdx).TagName = WTagName;
              WHTTPSrc = %TRIM(WHTTPSrc) + '<' + %TRIM(WTagName) + '>' + CRLF;
           ENDIF;
        ELSE;
           IF (WIdx > 1);
              IF (WTipo = 'XML');
                 WIdx -= 1;
                 WHTTPSrc = %TRIM(WHTTPSrc) + '</'+
                            %TRIM(WTagList(WIdx).TagName)+'>'+CRLF;
                 WTagList(WIdx).IsEmpty = *ON;
                 WTagList(WIdx).TagName = *BLANKS;
              ENDIF;
           ENDIF;
        ENDIF;
      /END-FREE
     P $addHTTPval     E

     P*=======================================================
     P* Processo..: $getHTTPmsg
     P* Objectivo.: Obter a mensagem de erro formatada em XML
     P*=======================================================
     P $getHTTPmsg     B                   EXPORT
     D                 PI          1000A
     D   PMsgCode                     7A   CONST
     D   PStatus                      3A   CONST OPTIONS(*NOPASS)
     D
     D WMensagem       S            100A
     D WCodigo         S              4A
     D WStatus         S              3A
     D WCodURL         S              3A
     D*=============================================
      /FREE
        WMensagem = $getMessage(PMsgCode);
        WCodigo   = %SUBST(PMsgCode:4:4);
        IF (%Parms() > 1);
           WStatus = PStatus;
           WCodURL = '200';
        ELSE;
           WStatus = 'ERR';
           WCodURL = '500';
        ENDIF;

        RETURN  'Content-type: text/xml ' + CRLF + CRLF
              + '<?xml version="1.0" encoding="ISO-8859-1"?>'
              + '<result>' + CRLF
              + '<stsstr>'+%TRIM(WStatus)+'</stsstr>' + CRLF
              + '<stscod>'+ WCodigo + '</stscod>' + CRLF
              + '<stsTes>Validar versão</stsTes>' + CRLF
              + '<stsid>'+ PMsgCode + '</stsid>' + CRLF
              + '<stsmsg>'+%TRIM(WMensagem)+'</stsmsg>' + CRLF
              + '</result>';
      /END-FREE
     P $getHTTPmsg     E
