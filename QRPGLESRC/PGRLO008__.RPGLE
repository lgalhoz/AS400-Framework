     H DFTACTGRP(*NO) BNDDIR('JMDIR')
     FFCATP100  IF   E           K DISK
     D getenv          PR              *   extproc('getenv')
     D var                             *   value options(*string)
     D QtmhWrStout     PR                  extproc('QtmhWrStout')
     D DtaVar                     65535a   options(*varsize)
     D DtaVarLen                     10I 0 const
     D ErrorCode                   8000A   options(*varsize)
     D err             ds                  qualified
     D bytesProv                     10i 0 inz(0)
     D bytesAvail                    10i 0 inz(0)
     D xml             pr          5000a   varying
     D inp                         5000a   varying const
     D CRLF            C                   x'0d25'
     D pos             s             10i 0
     D uri             s           5000a   varying
     D data            s           5000a

      /free
        uri = %str( getenv('REQUEST_URI') );
        monitor;
           pos = %scan('/test/': uri) + %len('/test/');
           cdprod = %int(%subst(uri:pos));
        on-error;
           data = 'Status: 500 Invalid URI' + CRLF
               + 'Content-type: text/xml' + CRLF
               + CRLF
               + '<error>Ocorreram Erros</error>' + CRLF;
           QtmhWrStout(data: %len(%trimr(data)): err);
           return;
        endmon;

           chain cdprod RCATP100;
           if not %found;
              data = 'Status: 500 Unknown Customer' + CRLF
                      + 'Content-type: text/xml' + CRLF
                      + CRLF
                      + '<error>Código de Produto ?????</error>' + CRLF;
              QtmhWrStout(data: %len(%trimr(data)): err);
              return;
           endif;


           data = 'Status: 200 OK' + CRLF
                  + 'Content-type: text/xml' + CRLF
                  + CRLF
                  + '<result>'
                  + '<cust id="' + %char(cdprod) + '">'
                  + '<name>' +     dsprod  + '</name>'
                  + '<ramo>' +     %char(cdramo)  + '</ramo>'
                  + '</cust>'
                  + '</result>' + CRLF;
           QtmhWrStout(data: %len(%trimr(data)): err);

        *INLR = *ON;

      /end-free
