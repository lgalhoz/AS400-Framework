     H DFTACTGRP(*NO) BNDDIR('JMDIR')
     H*=============================================================
     H* Programa..: RDOCO010
     H* Objectivo.: Disponibilizar informação da CartaVerde
     H*
     H* Descrição.: Este programa é invocado por uma qualquer browser
     H*             de Internet, e devolve o resultado em XML
     H*             para o efeito está preparado para devolver o
     H*             diferentes resultados em função dos parametros.
     H*
     H*=============================================================
     FFDOCP060  IF   E           K DISK    PREFIX(DOC_)
     FGBASEGRX  IF   E           K DISK    PREFIX(BAS_)
     FGFSEG     IF   E           K DISK    PREFIX(SEG_)
     FGBASEDIA  IF   E           K DISK    PREFIX(DIA_)
     FGBASEGRA  IF   E           K DISK    PREFIX(GRA_)
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
     D WNumMed         S              3A   INZ(*BLANKS)
     D WUserID         S              5S 0 INZ(*ZEROS)
     D WQtdeRegistos   S              2S 0 INZ(*ZEROS)
     D WSituacao       S             15A
     D WDsTipRecibo    S             15A
     D WRemoteAddr     S             25A
     D WNumRecibo      S              9S 0
     D WDatCobra       S              8S 0
     D WCriteria       S            300A
     D WDatChar        S             10A
     D
     D*==> Parametros Recebidos
     D WNuApolice      S              7S 0
     D WNumCVerdeIni   S              7S 0
     D WNumCVerdeFim   S              7S 0
     D WAno            S              4S 0
     D WIsException    S              1A
     D WIsReboque      S              1A
     D WIsViatura      S              1A
     D*=============================================================
     C     *ENTRY        PLIST
     C                   PARM                    WURI
     C                   PARM                    HTTPCODE
      /FREE
        //-----------------------------------------------
        // Trabalhar função em dois modos
        //-----------------------------------------------
        WNuApolice    = *ZEROS;
        WNumCVerdeIni = *ZEROS;
        WNumCVerdeFim = *ZEROS;
        WAno          = *ZEROS;
        WIsException  = 'N';
        WIsReboque    = 'N';
        WIsViatura    = 'N';

        MONITOR;
           WRemoteAddr = %str($getenv('REMOTE_ADDR'));
        ON-ERROR;
        ENDMON;
        MONITOR;
           WNuApolice   = $getHTTPVar('nuapolice');
        ON-ERROR;
        ENDMON;
        MONITOR;
           WNumCVerdeIni= $getHTTPVar('ncvini');
           WNumCVerdeFim= $getHTTPVar('ncvfim');
        ON-ERROR;
        ENDMON;
        MONITOR;
           WAno         = $getHTTPVar('ano');
        ON-ERROR;
        ENDMON;

        //-----------------------------------------------
        // Validar se pode utilizar a função
        //-----------------------------------------------
        $inzHTTPSrc('XML');  // Possivel(XML, HTTP, TXT)
        IF NOT $ValAccess(S#PGM);
           HTTPCode = $getHTTPmsg('HTP0500');
           RETURN;
        ENDIF;

        IF ((WNumCVerdIni > WNumCVerdeFim) OR
          (WNumCVerdeIni = *ZEROS) OR (WNumCVerdeFim = *ZEROS))
           AND (WNuApol = *ZEROS);
           HTTPCode = $getHTTPmsg('HTP0500');
           RETURN;
        ENDIF;

        $addHTTPVal('IP':WRemoteAddr);
        $addHTTPVal('user':S#USR);
        $addHTTPVal('ini':%CHAR(WRecIni));
        $addHTTPVal('qtde':%CHAR(WQtdeRec));

        //-----------------------------------------------
        // Obter parametros passados ao Browser Web
        //-----------------------------------------------
        IF (WNuApolice <> *ZEROS);
           EXSR GetApolices; // Lista de Cartas Verdes da Apólice
        ELSE;
           EXSR GetCVerde;   // Lista de Cartas Verdes
        ENDIF;
        RETURN;

        //============================================================
        // Subrotina.: GetApolices
        // Descrição.: Obter a lista de Cartas Verdes Emitidas para a
        //             a Apólice.
        //============================================================
        BEGSR GetApolices;
           WSessionID = $SETFILES('FDOCP060');
           $SetPage(WQtdeRec);
           $RmvDBSession(WSessionID);
        ENDSR;

        //============================================================
        // Subrotina.: GetCVerde
        // Descrição.: Obter a lista de Cartas Verdes dentro range
        //             recebido como parametro.
        //============================================================
        BEGSR GetCVerde;
        ENDSR;

        //-----------------------------------------------
        // Parameterizar a Query
        //-----------------------------------------------
        //---------------------------------------------------------------
        // Aplicar Criterio de Selecção
        //---------------------------------------------------------------
        EXSR AddCriteria;
        $addHTTPVal('criteria':WCRITERIA);
        $SetCriteria(WCriteria);
        $SetSort('NNUMREC #DESC');

        //-----------------------------------------------
        // Obter o Sub-set de Registos
        //-----------------------------------------------
        $addHTTPVal('registos':%CHAR($GetQtdeReg()));
        IF ($GetRecord(WRecIni:WQtdeRec) > *ZEROS);
           DOW (NOT $IsEOF());
              WIcRegistos = *ON;
              $addHTTPVal('data');
              //----------------------------------------
              // Informação da Carta Verde
              //----------------------------------------
              $addHTTPVal('apolice':$GetField('NUMAPOL'));      // Número de Apólice
              $addHTTPVal('recibo':$GetField('NNUMREC'));       // Número de Recibo
              $addHTTPVal('inicio':$GetField('DTINICIO'));      // Data de Inicio
              $addHTTPVal('termo':$GetField('DTTERMO'));        // Data de Termo
              $addHTTPVal('periodo':%TRIM($GetField('DTINICIO'))+' a '+
                                    %TRIM($GetField('DTTERMO')));
              $addHTTPVal('limite':$GetField('DTLIMITE'));      // Data limite
              $addHTTPVal('cobranca':$GetField('DTCOBRANCA'));  // Data de Cobrança
              $addHTTPVal('emissao':$GetField('DTEMISSAO'));    // Data de Emissão
              $addHTTPVal('simples':$GetField('SIMPLE'));       // Prémio Simples
              $addHTTPVal('total':$GetField('VALREC'));         // Valor Total
              $addHTTPVal('aviso':$GetField('NAVD1'));          // Numero do Aviso

              $addHTTPVal('comissao':$GetField('VCOMIS'));      // Valor de Comissão

              $addHTTPVal('situaca':$GetField('CSIT'));         // Situação do Recibo

              //----------------------------------------
              // Informação do Segurado
              //----------------------------------------
              $addHTTPVal('segurado':$GetField('CSEGUR'));      // Código do Segurado
              $addHTTPVal('nome':$GetField('NOM'));             // Nome
              $addHTTPVal('morada':$GetField('MORAD'));         // Morada
              $addHTTPVal('localidade':$GetField('DESCP'));     // Localidade
              $addHTTPVal('codpostalnome':$GetField('CPOST'));  // Código Postal
              $addHTTPVal('NIF':$GetField('NIF'));              // Número de Identificação Fiscal
           ENDDO;
        ELSE;
              $addHTTPVal('data');
              $addHTTPVal('erro':'sem registos');            // Não encontrou registos
              $addHTTPVal();
        ENDIF;
        $RmvDBSession(WSessionID);

        HTTPCODE = $getHTTPSrc();
        RETURN;

        //=================================================================
        // Subrotina..: GetMedCode
        // Objectivo..: Obter o Código de Mediador Interno
        //=================================================================
        BEGSR GetMedCode;
           MONITOR;
              WUserID = %DEC(WNumMed:3:0);
           ON-ERROR;
              HTTPCode = $getHTTPmsg('HTP0504');
              RETURN;
           ENDMON;

           CHAIN (WUserID) RSECP034;
           IF (%FOUND());
              IF (SEC_CDMEDIA = 80999);
                 WNumMed = 'XXX'; // Acesso a toda a Carteira
              ELSE;
                 WNumMed = %SUBST(%CHAR(SEC_CDMEDIA):3:3);
              ENDIF;
              $addHTTPVal('mediador':WNumMed);
              $addHTTPVal('nome':SEC_NOME);
           ELSE;
              HTTPCode = $getHTTPmsg('HTP0504');
              RETURN;
           ENDIF;
        ENDSR;
      /END-FREE
