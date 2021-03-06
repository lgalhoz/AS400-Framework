     H DFTACTGRP(*NO) BNDDIR('JMDIR')
     H*=============================================================
     H* Programa..: RRECO010
     H* Objectivo.: Disponibilizar informa��o sobre os Recibos
     H*
     H* Descri��o.: Este programa � invocado por uma qualquer browser
     H*             de Internet, e devolve o resultado em XML
     H*             para o efeito est� preparado para devolver o
     H*             diferentes resultados em fun��o dos parametros.
     H*
     H*
     H*=============================================================
     FFSECP034  IF   E           K DISK    PREFIX(SEC_)
     FGCOBGR    IF   E           K DISK    PREFIX(REC_)
     F*=============================================================
      /Copy QRPGLESRC,SSECO010
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
     D
     D*==> Parametros Recebidos
     D WRecIni         S             10S 0 INZ(*ZEROS)
     D WQtdeRec        S              2S 0 INZ(*ZEROS)
     D WCdSitRecibo    S              1A
     D WTipData        S              1A
     D WDatIni         S              8S 0
     D WNIF            S              9A
     D WIcRegistos     S               N   INZ(*OFF)
     D WSegurado       S             40A
     D WRecibo         S              9A
     D WRamo           S              2A
     D WApolice        S              7A
     D
     D*==> Situa��o dos Recibos
     D KTE_Todos       C                   'T'
     D KTE_Anulado     C                   'A'
     D KTE_Cobrado     C                   'C'
     D KTE_Emitido     C                   'E'
     D KTE_preanula    C                   'P'
     D
     D*==> Tipo de Data Recebido
     D KTE_Inicio      C                   'I'
     D KTE_Emissao     C                   'E'
     D KTE_Cobranca    C                   'C'
     D KTE_Limite      C                   'L'
     D*=============================================================
     C     *ENTRY        PLIST
     C                   PARM                    WURI
     C                   PARM                    HTTPCODE
      /FREE
        //-----------------------------------------------
        // Trabalhar fun��o em dois modos
        //-----------------------------------------------
        WIcRegistos = *OFF;
        WSegurado    = *BLANKS;
        WCdSitRecibo = KTE_Todos;
        WRecIni      = 1;
        WQtdeRec     = 24;
        WRecibo      = *BLANKS;
        WRamo        = *BLANKS;
        WApolice     = *BLANKS;
        WNIF         = *BLANKS;
        WDatIni      = *ZEROS;
        WTipData     = *BLANKS;

        MONITOR;
           WRemoteAddr = %str($getenv('REMOTE_ADDR'));
           WNumMed      = $getHTTPVar('mediador');
           WNIF         = $getHTTPVar('nif');
           WCdSitRecibo = $getHTTPVar('tipo');
           WTipData     = $getHTTPVar('tdat');
           WSegurado    = $getHTTPVar('nome');
           WRecibo      = $getHTTPVar('recibo');
           WRamo        = $getHTTPVar('ramo');
           WApolice     = $getHTTPVar('apolice');
           MONITOR;
              WRecIni  = %DEC($getHTTPVar('ini'):10:0);
              WQtdeRec = %DEC($getHTTPVar('qtde'):2:0);
           ON-ERROR;
              WRecIni  = 1;
              WQtdeRec = 24;
           ENDMON;
           MONITOR;
              WDatIni  = %DEC($getHTTPVar('data'):8:0);
           ON-ERROR;
              WDatIni  = *ZEROS;
           ENDMON;
        ON-ERROR;
           WRemoteAddr  = '020.000.000.053';
           WNumMed      = '14';
        ENDMON;
        //-----------------------------------------------
        // Validar se pode utilizar a fun��o
        //-----------------------------------------------
        $inzHTTPSrc('XML');  // Possivel(XML, HTTP, TXT)
        IF NOT $ValAccess(S#PGM);
           HTTPCode = $getHTTPmsg('HTP0500');
           RETURN;
        ENDIF;
        $addHTTPVal('IP':WRemoteAddr);
        $addHTTPVal('user':S#USR);
        $addHTTPVal('ini':%CHAR(WRecIni));
        $addHTTPVal('qtde':%CHAR(WQtdeRec));

        // EXSR AddTableSpecs; // Criar Especifica��es das Tabelas

        //-----------------------------------------------
        // Obter parametros passados ao Browser Web
        //-----------------------------------------------
        IF (WNumMed = *BLANKS);
           HTTPCode = $getHTTPmsg('HTP0504');
           RETURN;
        ENDIF;
        EXSR GetMedCode;

        //-----------------------------------------------
        // Parameterizar a Query
        //-----------------------------------------------
        WSessionID = $AddDBSession();
        $SETFILES('FRECT010   INNER JOIN GFSEG   ON (CSEGUR = NSEG)');
        $SETFIELDS('CRAMO,NUMAPOL,NNUMREC,NOM,DTTERMO,DTLIMITE,+
                    VALREC,VCOMIS,CSIT,DTINICIO,CSEGUR,SIMPLE,+
                    MORAD,NIF,DESCP,CPOST,CMP05,DTEMISSAO,DTCOBRANCA');
        //---------------------------------------------------------------
        // Aplicar Criterio de Selec��o
        //---------------------------------------------------------------
        IF (WnumMed = 'XXX');
           WCriteria = '(CODMOV=15 AND CCOMP=54)';
        ELSE;
           WCriteria = '(CC0='''+WNumMed+''' AND CODMOV=15 AND CCOMP=54)';
        ENDIF;
        IF (%TRIM(WApolice) <> *BLANKS);
           WCriteria = %TRIM(WCriteria) + ' AND NUMAPOL=''   '+WApolice+'''';
        ENDIF;
        IF (%TRIM(WRecibo) <> *BLANKS);
           WCriteria = %TRIM(WCriteria) + ' AND NNUMREC='+WRecibo;
        ENDIF;
        IF (%TRIM(WNIF) <> *BLANKS);
           WCriteria = %TRIM(WCriteria) + ' AND NIF='+WNIF;
        ENDIF;
        IF (%TRIM(WSegurado) <> *BLANKS);
           WSegurado = $StrReplace('%20':'%':WSegurado);
           WSegurado = $StrReplace('%20':'%':WSegurado);
           WSegurado = $StrReplace('%20':'%':WSegurado);
           WSegurado = $StrReplace('%20':'%':WSegurado);
           WSegurado = $StrReplace('%20':'%':WSegurado);
           WSegurado = $StrReplace('%20':'%':WSegurado);
           WCriteria = %TRIM(WCriteria) + ' AND NOM LIKE ''%'+
                       %TRIM(WSegurado)+'%''';
        ENDIF;
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
              // Informa��o do Recibo
              //----------------------------------------
              $addHTTPVal('apolice':$GetField('NUMAPOL'));      // N�mero de Ap�lice
              $addHTTPVal('recibo':$GetField('NNUMREC'));       // N�mero de Recibo
              $addHTTPVal('inicio':$GetField('DTINICIO'));      // Data de Inicio
              $addHTTPVal('termo':$GetField('DTTERMO'));        // Data de Termo
              $addHTTPVal('limite':$GetField('DTLIMITE'));      // Data limite
              $addHTTPVal('cobranca':$GetField('DTCOBRANCA'));  // Data de Cobran�a
              $addHTTPVal('emissao':$GetField('DTEMISSAO'));    // Data de Emiss�o
              $addHTTPVal('simples':$GetField('SIMPLE'));       // Pr�mio Simples
              $addHTTPVal('total':$GetField('VALREC'));         // Valor Total
              $addHTTPVal('comissao':$GetField('VCOMIS'));      // Valor de Comiss�o
              $addHTTPVal('situaca':$GetField('CSIT'));         // Situa��o do Recibo

              //----------------------------------------
              // Informa��o do Segurado
              //----------------------------------------
              $addHTTPVal('segurado':$GetField('CSEGUR'));      // C�digo do Segurado
              $addHTTPVal('nome':$GetField('NOM'));             // Nome
              $addHTTPVal('morada':$GetField('MORAD'));         // Morada
              $addHTTPVal('localidade':$GetField('DESCP'));     // Localidade
              $addHTTPVal('codpostalnome':$GetField('CPOST'));  // C�digo Postal
              $addHTTPVal('NIF':$GetField('NIF'));              // N�mero de Identifica��o Fiscal

              //----------------------------------------
              // Informa��o Complementar do Recibo
              //----------------------------------------
              EXSR GetRecInfo;
              $addHTTPVal('tipo':WDsTipRecibo);              // Tipo de Recibo
              $addHTTPVal();
              $GetNext();
           ENDDO;
        ELSE;
              $addHTTPVal('data');
              $addHTTPVal('erro':'sem registos');            // N�o encontrou registos
              $addHTTPVal();
        ENDIF;
        $RmvDBSession(WSessionID);

        HTTPCODE = $getHTTPSrc();
        RETURN;

        //=================================================================
        // Subrotina..: GetRecInfo
        // Objectivo..: Obter Informa��o do Recibo
        //=================================================================
        BEGSR GetRecInfo;
           //-----------------------------------------------
           // ForCompr a Situa��o dos Recibos
           //-----------------------------------------------
           WDsTipRecibo = 'Desconhecido';
           WSituacao  = 'Desconhecidio';
           MONITOR;
              WNumRecibo = %DEC($GetField('NNUMREC'):9:0);
           ON-ERROR;
              WNumRecibo = *ZEROS;
              LEAVESR;
           ENDMON;

           IF ($GetField('CSIT') = '2');
              WSituacao = 'Cobrado';
              $addHTTPVal('icon':'CBR');             // Situa��o do Recibo
           ELSEIF ($GetField('CSIT') > '2');
              WSituacao = 'Anulado';
              $addHTTPVal('icon':'ANL');             // Situa��o do Recibo
           ELSEIF ($GetField('CSIT') = '1');
              WSituacao = 'Anulado';
              $addHTTPVal('icon':'ANL');             // Situa��o do Recibo
           ELSE;
              IF ($GetField('CMP05') = 'S');
                 WSituacao = 'Devolvido';
                 $addHTTPVal('icon':'DEV');             // Situa��o do Recibo
              ELSE;
                 WSituacao = 'Pendente';
                 $addHTTPVal('icon':'PND');             // Situa��o do Recibo
              ENDIF;
           ENDIF;
           $addHTTPVal('situacao':WSituacao);             // Situa��o do Recibo

           //-----------------------------------------------
           // Descodificar Tipo de recibo
           //-----------------------------------------------
           CHAIN (WNumRecibo) GCOBGR;
           IF (REC_GCTIPR = 0);
              WDsTipRecibo = 'Novo';
           ELSEIF (REC_GCTIPR = 1);
              WDsTipRecibo = 'Adicional';
           ELSEIF (REC_GCTIPR = 2);
              WDsTipRecibo = 'Continuado';
           ENDIF;
           $addHTTPVal('ramo':%CHAR(REC_GCRAMO));
        ENDSR;

        //=================================================================
        // Subrotina..: AddTableSpecs
        // Objectivo..: Definir as regras da Tabela
        //=================================================================
        BEGSR AddTableSpecs;
           $addHTTPVal('tabela');
           $addHTTPVal('coluna');
           $addHTTPVal('label':'Recibo');
           $addHTTPVal('campo':'recibo');
           $addHTTPVal('tipo':'N');
           $addHTTPVal();
           $addHTTPVal('coluna');
           $addHTTPVal('label':'Ramo');
           $addHTTPVal('campo':'ramo');
           $addHTTPVal('tipo':'N');
           $addHTTPVal();
           $addHTTPVal('coluna');
           $addHTTPVal('label':'Ap�lice');
           $addHTTPVal('campo':'apolice');
           $addHTTPVal('tipo':'N');
           $addHTTPVal();
           $addHTTPVal('coluna');
           $addHTTPVal('label':'Emiss�o');
           $addHTTPVal('campo':'emissao');
           $addHTTPVal('tipo':'D');
           $addHTTPVal();
           $addHTTPVal('coluna');
           $addHTTPVal('label':'Limite');
           $addHTTPVal('campo':'limite');
           $addHTTPVal('tipo':'D');
           $addHTTPVal();
           $addHTTPVal('coluna');
           $addHTTPVal('label':'Segurado');
           $addHTTPVal('campo':'nome');
           $addHTTPVal('tipo':'A');
           $addHTTPVal();
           $addHTTPVal('coluna');
           $addHTTPVal('label':'Total');
           $addHTTPVal('campo':'total');
           $addHTTPVal('tipo':'M');
           $addHTTPVal();
           $addHTTPVal('coluna');
           $addHTTPVal('label':'Tipo');
           $addHTTPVal('campo':'tipo');
           $addHTTPVal('tipo':'A');
           $addHTTPVal();
           $addHTTPVal('coluna');
           $addHTTPVal('label':'Situa��o');
           $addHTTPVal('campo':'situacao');
           $addHTTPVal('tipo':'A');
           $addHTTPVal();
           $addHTTPVal();
        ENDSR;

        //=================================================================
        // Subrotina..: GetMedCode
        // Objectivo..: Obter o C�digo de Mediador Interno
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
