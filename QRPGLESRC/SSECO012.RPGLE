     D*=================================================================
     D* Fun��es de ficheiros XML/HTML/TEXTO
     D*=================================================================
     D*==> Obter Autoriza��o para usar o portal
     D $addHTTPSess    PR             5S 0
     D   WUser                       10A   CONST
     D   WPassw                      10A   CONST
     D   WIPVal                      15A   CONST
     D
     D*==> Validar se sess�o ainda est� v�lida
     D $ValHTTPSess    PR              N
     D   WIPVal                      15A   CONST
     D
     D*==> Obter uma vari�vel espec�fica
     D $getHTTPVar     PR           200A
     D  KEY                          20A   CONST
     D  DEC                            N   CONST OPTIONS(*NOPASS)
     D  URL                        5000A   CONST OPTIONS(*NOPASS)
     D
     D*==> Inicializar o Script HTML/XML ou TXT
     D $inzHTTPSrc     PR
     D  TipSource                     5A   CONST OPTIONS(*NOPASS)
     D
     D*==> Obter o Script XML, HTML ou TXT
     D $getHTTPSrc     PR         65535A
     D
     D*==> Adicionar informa��o sobre a P�gina
     D $addHTTPInfo    PR
     D   PCodigo                      4S 0 CONST
     D   PMensagem                  300A   CONST OPTIONS(*NOPASS)
     D
     D*==> Adicionar um novo valor ao Script
     D $addHTTPval     PR
     D   WTagName                    50A   CONST OPTIONS(*NOPASS)
     D   WValue                     300A   CONST OPTIONS(*NOPASS)
     D
     D*==> ForCompr Mensagem de erro
     D $getHTTPmsg     PR          1000A
     D   PMsgCode                     7A   CONST
     D   PStatus                      3A   CONST OPTIONS(*NOPASS)
     D
     D*==> Constantes
     D CRLF            C                   x'0d25'
     D CNT_XML         C                   'Content-type: text/xml'
     D CNT_HTML        C                   'Content-type: text/html'
     D CNT_TXT         C                   'Content-type: text/plain'
