     H DFTACTGRP(*NO) BNDDIR('JMDIR')
     H*=============================================================
     H* Programa..: RPRDO030
     H* Objectivo.: Disponibilizar informação sobre os apólices
     H*
     H* Descrição.: Este programa é invocado por uma qualquer browser
     H*             de Internet, e devolve o resultado em XML
     H*             para o efeito está preparado para devolver o
     H*             diferentes resultados em função dos parametros.
     H*
     H*
     H*=============================================================
     FFSECP034  IF   E           K DISK    PREFIX(SEC_)
     FFRECT010  IF   E           K DISK    PREFIX(REC_)
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
     D WCodigo         S              7A   INZ(*BLANKS)
     D WTipdata        S              3A   INZ(*BLANKS)
     D WQtdeRegistos   S              2S 0 INZ(*ZEROS)
     D WRemoteAddr     S             25A
     D WCriteria       S            300A
     D WDatChar        S             10A
     D WRela           S              5A
     D
     D*==> Parametros Recebidos
     D WRecIni         S             10S 0 INZ(*ZEROS)
     D WQtdeRec        S              2S 0 INZ(*ZEROS)
     D WApolice        S              7A
     D WNIF            S              9A
     D WIcRegistos     S               N   INZ(*OFF)
     D WSegurado       S             40A
     D
     D*=============================================================
     C     *ENTRY        PLIST
     C                   PARM                    WURI
     C                   PARM                    HTTPCODE
      /FREE
        //-----------------------------------------------
        // Trabalhar função em dois modos
        //-----------------------------------------------
        WIcRegistos = *OFF;
        WApolice     = *BLANKS;
        WNumMed      = *BLANKS;
        WTipData     = *BLANKS;
        WCodigo      = *BLANKS;
        WSegurado    = *BLANKS;
        WRecIni      = 1;
        WQtdeRec     = 24;
        WNIF         = *BLANKS;

        MONITOR;
           WRemoteAddr = %str($getenv('REMOTE_ADDR'));
           WNumMed      = $getHTTPVar('mediador');
           WCodigo      = $getHTTPVar('codigo');
           WApolice     = $getHTTPVar('apolice');
           WNIF         = $getHTTPVar('nif');
           WSegurado    = $getHTTPVar('nome');
           WTipData     = $getHTTPVar('tdat');
           MONITOR;
              WRecIni  = %DEC($getHTTPVar('ini'):10:0);
              WQtdeRec = %DEC($getHTTPVar('qtde'):2:0);
           ON-ERROR;
              WRecIni  = 1;
              WQtdeRec = 24;
           ENDMON;
        ON-ERROR;
           WRemoteAddr  = '020.000.000.053';
           WNumMed      = '14';
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
        $addHTTPVal('ini':%CHAR(WRecIni));
        $addHTTPVal('qtde':%CHAR(WQtdeRec));

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
        WSessionID = $SETFILES('FPRDT001');

        //---------------------------------------------------------------
        // Aplicar Criterio de Selecção
        //---------------------------------------------------------------
        EXSR AddCriteria;
        $addHTTPVal('criteria':WCRITERIA);
        $SetCriteria(WCriteria);
        $SetSort('NUAPOL #DESC');

        //-----------------------------------------------
        // Obter o Sub-set de Registos
        //-----------------------------------------------
        $addHTTPVal('registos':%CHAR($GetQtdeReg()));
        IF ($GetRecord(WRecIni:WQtdeRec) > *ZEROS);
           DOW (NOT $IsEOF());
              WIcRegistos = *ON;
              $addHTTPVal('data');
              //----------------------------------------
              // Informação do Segurado
              //----------------------------------------
              $addHTTPVal('ramo':$GetField('CDRAMO'));      // Código de Ramo
              $addHTTPVal('apolice':$GetField('NUAPOL'));   // Número de Apólice
              $addHTTPVal('numero':$GetField('NUSEG'));     // Código do Segurado
              $addHTTPVal('apelido':$GetField('APELIDO'));  // Apelido
              $addHTTPVal('nome':$GetField('NOSEG'));       // Nome
              $addHTTPVal('nif':$GetField('NUNIF'));        // NIF
              $addHTTPVal('matricula':$GetField('MATRIC')); // Matricula
              $addHTTPVal('estado':$GetField('STATUS'));    // Estado da Apólice
              $addHTTPVal('risco':$GetField('LCRISK'));     // Locali de Risco
              $addHTTPVal('corretor':$GetField('CDMED1'));  // Corretor
              $addHTTPVal('medcc':$GetField('CDJMM1'));     // Mediador C/Cobrança
              $addHTTPVal('medsc':$GetField('CDJMM2'));     // Mediador S/Cobrança
              $addHTTPVal('alteracao':$GetField('ICCARR')); // Apólice em Alteração
              IF ($GetField('STATUS') = '1');
                 $addHTTPVal('icon':'ACT');                    // Apólice em Alteração
              ELSEIF ($GetField('STATUS') = '2');
                 $addHTTPVal('icon':'SUS');                    // Apólice em Alteração
              ELSEIF ($GetField('STATUS') = '3');
                 $addHTTPVal('icon':'ANU');                    // Apólice em Alteração
              ELSEIF ($GetField('STATUS') = '4');
                 $addHTTPVal('icon':'PRE');                    // Apólice em Alteração
              ENDIF;
              $addHTTPVal();
              $GetNext();
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
        // Subrotina..: AddCriteria
        // Objectivo..: Obter os dados em função dos Parametros recebidos
        //=================================================================
        BEGSR AddCriteria;
           //------------------------------------------------------------
           // Formatar Variáveis recebidas
           //------------------------------------------------------------
           WRela = *BLANKS;
           IF (WnumMed <> 'XXX');
              WCriteria = '(CDMED1='''+WNumMed+''' OR CDJMM1='''+WNumMed+''' +
                            OR CDJMM2='''+WNumMed+''')';
              WRela     = ' AND ';
           ELSEIF (WCodigo <> *BLANKS);
              WCriteria = '(CDMED1='''+WCodigo+''' OR CDJMM1='''+WCodigo+''' +
                            OR CDJMM2='''+WCodigo+''')';
              WRela     = ' AND ';
           ENDIF;

           IF (WTipData = 'ACT');
              WCriteria = %TRIM(WCriteria) + WRela + 'STATUS=1';
              WRela     = ' AND ';
           ELSEIF (WTipData = 'SUS');
              WCriteria = %TRIM(WCriteria) + WRela + 'STATUS=2';
              WRela     = ' AND ';
           ELSEIF (WTipData = 'ANU');
              WCriteria = %TRIM(WCriteria) + WRela + 'STATUS=3';
              WRela     = ' AND ';
           ELSEIF (WTipData = 'PRE');
              WCriteria = %TRIM(WCriteria) + WRela + 'STATUS=4';
              WRela     = ' AND ';
           ENDIF;

           IF (%TRIM(WNIF) <> *BLANKS);
              WCriteria = %TRIM(WCriteria) + WRela + 'NUNIF='+WNIF;
              WRela     = ' AND ';
           ENDIF;

           IF (%TRIM(WApolice) <> *BLANKS);
              WCriteria = %TRIM(WCriteria) + WRela + 'NUAPOL='+WApolice;
              WRela     = ' AND ';
           ENDIF;

           IF (%TRIM(WSegurado) <> *BLANKS);
              WSegurado = $StrReplace('%20':'%':WSegurado);
              WSegurado = $StrReplace('%20':'%':WSegurado);
              WSegurado = $StrReplace('%20':'%':WSegurado);
              WSegurado = $StrReplace('%20':'%':WSegurado);
              WSegurado = $StrReplace('%20':'%':WSegurado);
              WSegurado = $StrReplace('%20':'%':WSegurado);
              WCriteria = %TRIM(WCriteria) + WRela + 'NOSEG LIKE ''%'+
                          %TRIM(WSegurado)+'%''';
           ENDIF;
        ENDSR;

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
