     H DFTACTGRP(*NO) BNDDIR('JMDIR')
     H*=============================================================
     FFCATP100  IF   E           K DISK
     F*=============================================================
     D getenv          PR              *   extproc('getenv')
     D   var                           *   value options(*string)
     D
     D QtmhWrStout     PR                  extproc('QtmhWrStout')
     D   DtaVar                   65535A   options(*varsize)
     D   DtaVarLen                   10I 0 const
     D   ErrorCode                 8000A   options(*varsize)
     D
     D $addXMLTAG      PR           300A
     D   WTagName                    50A   CONST
     D   WTipo                        1A   CONST
     D   WValue                     300A   CONST OPTIONS(*NOPASS)
     D
     D $addXMLHdr      PR           300A
     D   WCodigo                      4S 0 CONST
     D   WMensagem                  300A   CONST OPTIONS(*NOPASS)
     D
     D err             DS                  qualified
     D bytesProv                     10i 0 inz(0)
     D bytesAvail                    10i 0 inz(0)
     D
     D xml             PR          5000A   varying
     D inp                         5000A   varying const
     D
     D CRLF            C                   x'0d25'
     D pos             S             10i 0
     D uri             S           5000A   varying
     D data            S           5000A

      /FREE
        //-------------------------------------------------------
        // Obter o valor escrito no Site
        //-------------------------------------------------------
        URI = %str(getenv('REQUEST_URI'));
        MONITOR;
           POS = %SCAN('/DemoComp?val=': uri) + %len('/DemoComp?val=');
           CDPROD = %int(%subst(uri:pos));
        ON-ERROR;
           DATA = $addXMLHdr(500:'URI inválido')
                + $AddXMLTag('header':'F':'Ocorreram erros');
           QtmhWrStout(data: %len(%trimr(data)): err);
           RETURN;
        ENDMON;

        //-------------------------------------------------------
        // Obter valores da Tabela
        //-------------------------------------------------------
        CHAIN CDPROD RCATP100;
        IF NOT %FOUND;
           DATA =  $addXMLHdr(500:'Produto inválido')
                 + $addXMLTag('erro':'F':'Não existe');
           QtmhWrStout(data: %len(%trimr(data)): err);
           RETURN;
        ENDIF;

        DATA =  $addXMLHdr(200:'OK')
              + $addXMLTag('result':'B')
              + $addXMLTag('data':'B')
              + $addXMLTag('codigo':'F':%CHAR(CDPROD))
              + $addXMLTag('nome':'F':DSPROD)
              + $addXMLTag('nome':'F':%CHAR(CDRAMO))
              + $addXMLTag('data':'E')
              + $addXMLTag('result':'E');
        QtmhWrStout(data:%len(%trimr(data)): err);

        *INLR = *ON;

      /END-FREE
     P*=======================================================
     P* Processo..: $addXMLHdr
     P* Objectivo.: Adicionar o Header do ficheiro XML
     P*=======================================================
     P $addXMLHdr      B                   EXPORT
     D                 PI           300A
     D   WCodigo                      4S 0 CONST
     D   WMensagem                  300A   CONST OPTIONS(*NOPASS)
     D*=============================================
      /FREE
         RETURN   'Status: '+%CHAR(WCodigo)
                  + ' ' + %TRIM(WMensagem) + CRLF
                  + 'Content-type: text/xml' + CRLF
                  + CRLF;
      /END-FREE
     P $addXMLHdr      E

     P*=======================================================
     P* Processo..: $addXMLTag
     P* Objectivo.: Criar uma TAG XML
     P*=======================================================
     P $addXMLTAG      B                   EXPORT
     D                 PI           300A
     D   WTagName                    50A   CONST
     D   WTipo                        1A   CONST
     D   WValue                     300A   CONST OPTIONS(*NOPASS)
     D*=============================================
      /FREE
        IF (WTipo = 'B');
           RETURN '<'+%TRIM(WTagName)+'>';
        ELSEIF (WTipo = 'E');
           RETURN '</'+%TRIM(WTagName)+'>';
        ELSE;
           RETURN '<'+%TRIM(WTagName)+'>'+%TRIM(WValue)+
                  '</'+%TRIM(WTagName)+'>';
        ENDIF;
      /END-FREE
     P $addXMLTAG      E
