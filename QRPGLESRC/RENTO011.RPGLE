     H DFTACTGRP(*NO) BNDDIR('JMDIR')
     H*=============================================================
     H* Programa..: RENTO011
     H* Objectivo.: Disponibilizar informa��o sobre os Segurados
     H*             para a Campanha de Marketing da Saude Prime.
     H*
     H* Observa��o: Dever� ser actualizada uma estrutura de dados
     H*             para controlar a sa�da da informa��o para a
     H*             sa�de Prime.
     H*
     H* Descri��o.:
     H*     Para o efeito devem ser seleccionados os em
     H*     conformidade com o seguinte crit�rio:
     H*     [x] Segurados de Media��o Directa
     H*     [x] Segurados sem Mediador com Cobran�a
     H*     [x] Segurados da Media��o da Demo Comp Clube TAP
     H*     [x] Pessoas (NIF: [100000000;300000000])
     H*     [x] Segurados com Contratos Activos
     H*
     H*     Os Campos a Seleccionar:
     H*     [x] NIF       -> N�mero Fiscal
     H*     [x] Nome      -> Nome do Segurado
     H*     [x] DTNasc    -> Data de Nascimento
     H*     [x] Sexo      -> Sexo
     H*     [x] Morada    -> Morada
     H*     [x] CodPostal -> C�digo Postal
     H*     [x] Contacto  -> Contacto Telef�nico
     H*     [x] EMail     -> Endere�o de Mail
     H*
     H*=============================================================
     FSD1AP1    IF   F  320    16AIDISK    KEYLOC(8) EXTFILE('SD.1.AP1')
     FSD1AP2    IF   F  320    16AIDISK    KEYLOC(8) EXTFILE('SD.1.AP2')
     FPESSOAS   IF   F  512     6AIDISK    KEYLOC(5) EXTFILE('SD.1.PE')
     FGFSEG     IF   E           K DISK    PREFIX(SEG_)
     F*=============================================================
      /Copy QRPGLESRC,SSECO010
      /Copy QRPGLESRC,SSECO012
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010

     D*==> Formatos de Registo
     D R#PESSOAS     E DS                  EXTNAME('SD.1.PE') PREFIX('PE_')
     D
     D*==> Registo de Ap�lices - Corretor
     D R#AP1         E DS                  EXTNAME('SD.1.AP1') PREFIX('AP_')
     D R#APKEY         DS
     D   AP_KEY                1     16
     D   APOLI                 1     10A
     D   CCOMP                11     13S 0
     D   CRAMO                14     16S 0
     D
     D*==> Estrutura de Dados para a Selec��o
     D LST#SEGURAD     DS                  QUALIFIED
     D   NIF                          9S 0
     D   Nome                        40A
     D   DTNasc                       8S 0
     D   Sexo                         1A
     D   Morada                      40A
     D   CodPostal                    7S 0
     D   TipCont                      2A
     D   Contacto                    15A
     D   EMail                      100A
     D
     D*==> Campos de Trabalho
     D WURI            S          65535A
     D HTTPCODE        S          65535A
     D WSessionID      S              5S 0 INZ(*ZEROS)
     D WQtdeRegistos   S              3S 0
     D WIstoProceed    S               N   INZ(*OFF)
     D WNumNIF         S              9S 0
     D
     D*== Defini��o de Constantes
     D TRUE            C                   *ON
     D FALSE           C                   *OFF
     D SEX#MASCULINO   C                   'M'
     D SEX#FEMININO    C                   'F'
     D SEX#INDEFINIDO  C                   'I'
     D CNT#TELEMOVEL   C                   'M'
     D CNT#TELEFONE    C                   'T'
     D CNT#INDEFINIDO  C                   'I'
     D NIF#MAXIMO      C                   399999999
     D*=============================================================
     C     *ENTRY        PLIST
     C                   PARM                    WURI
     C                   PARM                    HTTPCODE
      /FREE
        //-----------------------------------------------
        // Trabalhar fun��o em dois modos
        //-----------------------------------------------
        WIcRegistos = *OFF;
        WRecIni      = 1;
        WQtdeRec     = 24;

        MONITOR;
           WRemoteAddr = %str($getenv('REMOTE_ADDR'));
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

        EXSR AddHeader;
        EXSR ExtractSegurados;
        EXSR AddFooter;

        HTTPCODE = $getHTTPSrc();
        RETURN;

        //=================================================================
        // Subrotina..: ExtractSegurados
        // Objectivo..: Extrair a informa��o dos Segurados
        //=================================================================
        BEGSR ExtractSegurados;
           //--------------------------------------------------------------
           // Posicionar-se no ficheiro de Segurados (SD.1.PE)
           //--------------------------------------------------------------
           READ PESSOAS R#PESSOA;
           DOW (NOT %EOF(PESSOAS));
              WNumNIF  = PE_NRC;
              IF (WNumNIF > NIF#MAXIMO);
                 WisToProceed = FALSE;
              ENDIF;

              //--------------------------------------------------------------
              // ForCompr o registo com a informa��o existente o Corretor
              //--------------------------------------------------------------
              IF (WIsToProceed = TRUE);
                 LST#SEGURAD.NIF       = WNumNif;
                 LST#SEGURAD.Nome      = PE_NOME;
                 LST#SEGURAD.DTNasc    = *ZEROS;
                 LST#SEGURAD.Sexo      = *BLANKS;
                 LST#SEGURAD.Morada    = PE_MORADA;
                 LST#SEGURAD.CodPostal = PE_CPOST;
                 IF (PE_TLM <> *ZEROS);
                    LST#SEGURAD.TipCont   = 'T';
                    LST#SEGURAD.Contacto  = %CHAR(PE_TLM);
                 ELSEIF (PE_TELEF <> *BLANKS);
                    LST#SEGURAD.TipCont   = 'T';
                    LST#SEGURAD.Contacto  = PE_TELEF;
                 ENDIF;
                 LST#SEGURAD.EMail     = PE_EMAIL;
              ENDIF;

              //--------------------------------------------------------------
              // Posicionar-se no ficheiro de Segurados (GFSEG)
              //--------------------------------------------------------------
              IF (WIsToProceed = TRUE);
                 CHAIN (PE_NSEG) GFSEG;
                 IF (%FOUND(GFSEG));
                    IF (WNumNif = *ZEROS);
                       WNumNif = SEG_NIF;
                       LST#SEGURAD.DTNasc = *ZEROS;
                       LST#SEGURAD.Sexo   = *BLANKS;
                    ENDIF;
                 ENDIF;
                 IF (WNumNif = *ZEROS) OR (WNumNif > NIF#MAXIMO);
                    WIsToProceed = FALSE;
                 ENDIF;
              ENDIF;

              //--------------------------------------------------------------
              // Validar se o registo j� foi processado
              //--------------------------------------------------------------
              IF (WIsToProceed = TRUE);
                 // Validar se j� processou algum registo com o mesmo NIF
                 WIsToProceed = TRUE;
              ENDIF;

              //--------------------------------------------------------------
              // Validar se o Segurado � directo, pertence a um mediador sem
              // Cobran�a ou Aeroporto Clube TAP.
              //--------------------------------------------------------------
              IF (IsToProceed);
                 // Aceder aos contratos activos para despistar
              ENDIF;

              //--------------------------------------------------------------
              // Actualizar o registo XML
              //--------------------------------------------------------------
              IF (IsToProceed);
                 EXSR AddRecord;
              ENDIF;
              READ PESSOAS R#PESSOA;
           ENDDO;
        ENDSR;

        //=================================================================
        // Subrotina..: AddHeader
        // Objectivo..: Adicionar o Cabe�alho
        //=================================================================
        BEGSR AddHeader;
            $addHTTPVal('data'); // Abrir Registo
            $addHTTPVal('informa');
            $addHTTPVal('data':%CHAR(%DATE()));
            $addHTTPVal('hora':%CHAR(%DATE()));
            $addHTTPVal('user':S#USER);
            $addHTTPVal('comentarios':WComentario);
            $addHTTPVal();
            $addHTTPVal('codigos');
            $addHTTPVal('sexo');
            $addHTTPVal('masculino':'M');
            $addHTTPVal('feminino':'F');
            $addHTTPVal('indefinido':'I');
            $addHTTPVal();
            $addHTTPVal('contactos');
            $addHTTPVal('telemovel':'M');
            $addHTTPVal('telefone':'T');
            $addHTTPVal('indefindo':'I');
            $addHTTPVal();
            $addHTTPVal();
        ENDSR;

        //=================================================================
        // Subrotina..: AddFooter
        // Objectivo..: Adicionar o p� de p�gina
        //=================================================================
        BEGSR AddFooter;
            $addHTTPVal('resumo');
            $addHTTPVal('totalRegistos':%CHAR(WQtdRegistos));
            $addHTTPVal();
            $addHTTPVal();
        ENDSR;

        //=================================================================
        // Subrotina..: AddRecord
        // Objectivo..: Adicionar o Registo XML
        //=================================================================
        BEGSR AddRecord;
            $addHTTPVal('registo');
            $addHTTPVal('nome':LST#Segurado.Nome);
            $addHTTPVal('datanasc':%CHAR(LST#Segurado.DtNasc));
            $addHTTPVal('sexo':LST#Segurado.Sexo);
            $addHTTPVal('morada');
            $addHTTPVal('morada':LST#Segurado.Morada);
            $addHTTPVal('codpostal':%CHAR(LST#Segurado.CodPostal));
            $addHTTPVal();
            $addHTTPVal('nif':%CHAR(LST#Segurado.NIF));
            $addHTTPVal('contacto');
            $addHTTPVal('tipo':LST#Segurado.TipContacto);
            $addHTTPVal('contacto':LST#Segurado.Contacto);
            $addHTTPVal();
            $addHTTPVal('email':LST#Segurado.Email);
            $addHTTPVal();
        ENDSR;
      /END-FREE
