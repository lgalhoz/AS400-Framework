     H DFTACTGRP(*NO) BNDDIR('JMDIR')
     H*=============================================================
     H* Programa..: RCOBO018
     H* Objectivo.: Disponibilizar informação sobre os recibos
     H*             Cobrados para os Credores Hipotecários.
     H*
     H* Descrição.: Este programa é invocado por uma qualquer browser
     H*             de Internet, e devolve o resultado em XML
     H*             para o efeito está preparado para devolver o
     H*             diferentes resultados em função dos parametros.
     H*
     H*
     H* Parâmetros: Este programa recebe como parâmetros duas datas
     H*             que correspondem ao período de pesquisa.
     H*
     H*             http://20.0.0.1/josemata?frame=login&user=apache
     H*             http://20.0.0.1/josemata?frame=recchip&dtini=20150101&dtfim=20150110
     H*=============================================================
     FSD1RM     IF   F  128     3AIDISK    KEYLOC(5)   EXTFILE('SD.1.RM')
     FSD1CP     IF   F   64     7AIDISK    KEYLOC(2)   EXTFILE('SD.1.CP')
     FGSINENT   IF   F  400     7AIDISK    KEYLOC(1)   EXTFILE('G.SINENT')
     FSD1REC    IF   F  326     7AIDISK    KEYLOC(270) EXTFILE('SD.1.REC')
     FFDTREC    IF   E           K DISK    PREFIX('VAL_')
     FGFSEG     IF   E           K DISK    PREFIX('SEG_')
     FGBASEGRA  IF   E           K DISK    PREFIX('AUT_')
     FGBASEGRO  IF   E           K DISK    PREFIX('OR_')
     FGCOBGR    IF   E           K DISK    PREFIX('LUS_')
     F*=============================================================
      /Copy QRPGLESRC,SSECO010
      /Copy QRPGLESRC,SCATO010
      /Copy QRPGLESRC,SSECO012
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010

     D*==> Formato de registo de Recibos do Corretor
     D R#SD1RCA      E DS                  EXTNAME('SD.1.REC') PREFIX('REC_')
     D R#SINENT      E DS                  EXTNAME('G.SINENT') PREFIX('ENT_')
     D R#SD1CP       E DS                  EXTNAME('SD.1.CP') PREFIX('CP_')
     D R#SD1RM         DS
     D   RM_F0001              1      4
     D   RM_RAMO               5      7
     D   RM_DSRAMO             8     30
     D   RM_DUMMY             31    128
     D
     D*==> Campos de Trabalho
     D WURI            S          65535A
     D HTTPCODE        S          65535A
     D WRemoteAddr     S             15A
     D
     D WDatInicio      S              8S 0 INZ(*ZEROS)
     D WDatTermo       S              8S 0 INZ(*ZEROS)
     D
     D WDatPagam       S              8S 0 INZ(*ZEROS)
     D WDatLimite      S              8S 0 INZ(*ZEROS)
     D WNumRecibo      S              9S 0 INZ(*ZEROS)
     D WValRecibo      S             13S 2 INZ(*ZEROS)
     D WCodEstado      S              1S 0 INZ(*ZEROS)
     D
     D WCodRamoJM      S              3S 0
     D WCodRamo        S              2S 0 INZ(*ZEROS)
     D WNumApol        S              7S 0 INZ(*ZEROS)
     D WDescRamo       S             30A
     D WObjecto        S             50A
     D WDatResol       S              8S 0
     D
     D WCodSegurado    S              8S 0
     D WNome           S             40A
     D WNIF            S              9S 0
     D
     D WKeyCredor      S              7S 0
     D WCodCredor      S              5S 0 INZ(*ZEROS)
     D WNomCredor      S             40A
     D WEmail          S            100A
     D WFax            S             15A
     D WMorada         S             40A
     D WLocalidade     S             20A
     D WDatCarta       S              8S 0
     D WCodPostal      S              7S 0
     D WDesPostal      S             30A
     D*=============================================================
     C     *ENTRY        PLIST
     C                   PARM                    WURI
     C                   PARM                    HTTPCODE
      /FREE
        //-----------------------------------------------
        // Trabalhar função em dois modos
        //-----------------------------------------------
        WDatInicio = *ZEROS;
        WDatTermo  = *ZEROS;

        MONITOR;
           WRemoteAddr = %str($getenv('REMOTE_ADDR'));
           MONITOR;
              WDatInicio = %DEC($getHTTPVar('dtini'):8:0);
           ON-ERROR;
              WDatInicio = *ZEROS;
           ENDMON;
           MONITOR;
              WDatTermo = %DEC($getHTTPVar('dtfim'):8:0);
           ON-ERROR;
              WDatTermo = WDatInicio;
           ENDMON;
        ON-ERROR;
           WRemoteAddr  = '020.000.000.053';
           WDatInicio   = 20151029;
           WDatTermo    = 20151029;
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

        //-----------------------------------------------
        // Validar parametros recebidos
        //-----------------------------------------------
        IF (WDatInicio = *ZEROS);
           HTTPCode = $getHTTPmsg('HTP0504');
           RETURN;
        ENDIF;
        EXSR GetRecibos;

        HTTPCODE = $getHTTPSrc();
        RETURN;

        //=================================================================
        // Subrotina..: GetRecibos
        // Objectivo..: Obter informação dos Recibos.
        //=================================================================
        BEGSR GetRecibos;
           //------------------------------------------------------------
           // Formatar Variáveis recebidas
           //------------------------------------------------------------
           SETLL *LOVAL SD1REC;
           READ SD1REC R#SD1RCA;
           DOW (NOT %EOF(SD1REC));
              IF (REC_CCOMP = 54) AND (REC_CODSIT = 2);
                 MONITOR;
                    WNumRecibo = %DEC(REC_NREC2:9:0);
                    WDatLimite = $CvtData(%DEC(REC_DTLIM:6:0):'DMY':'YYMD');
                    WDatPagam  = $CvtData(%DEC(REC_DTCOB:6:0):'DMY':'YYMD');
                    WValRecibo = REC_VTREC;
                    WCodRamoJM = REC_CRAMO;
                    IF (WDatPagam > WDatLimite) AND (WDatPagam >= WDatInicio)
                      AND (WDatPagam <= WDatTermo);
                       EXSR GetData;
                       IF (WCodCredor > *ZEROS);
                          EXSR AddRecord;
                       ENDIF;
                    ENDIF;
                 ON-ERROR;
                 ENDMON;
              ENDIF;
              READ SD1REC R#SD1RCA;
           ENDDO;
        ENDSR;

        //=================================================================
        // Subrotina..: GetData
        // Objectivo..: Validar se existe Crtedor.
        //=================================================================
        BEGSR GetData;
           //-------------------------------------------------
           // Obter o código do Ramo a partir do Recibo
           //-------------------------------------------------
           WCodCredor = *ZEROS;
           CHAIN (WNumRecibo) GCOBGR;
           IF (NOT %FOUND(GCOBGR));
              //---> Ocorreram erros
              LEAVESR;
           ENDIF;
           IF (LUS_GCDCH = *ZEROS);
              //---> Não foi notificado o Credor
              LEAVESR;
           ENDIF;
           WCodRamo   = LUS_GCRAMO;
           WNumApol   = LUS_GCAPOL;
           WCodEstado = LUS_GCSITR;
           WDatCarta  = $cvtData(LUS_GCDCH:'YMD':'YYMD');

           //-------------------------------------------------
           // Obter o código do respectivo Credor Hipotecario
           //-------------------------------------------------
           IF (WCodRamo = 50);
              CHAIN (WCodRamo:WNumApol) GBASEGRA;
              IF (NOT %FOUND(GBASEGRA));
                 //---> Ocorreram erros
                 LEAVESR;
              ENDIF;
              WObjecto   = 'AUTOMÓVEL ' + %TRIM(AUT_MARC1) + ' ' +
                           %TRIM(AUT_MODELO) + ' ' + %TRIM(AUT_MATR1);
              WCodCredor = AUT_CRHIP;
              WCodSegurado = AUT_NSEG;
           ELSE;
              CHAIN (WCodRamo:WNumApol) GBASEGRO;
              IF (NOT %FOUND(GBASEGRO));
                 //---> Ocorreram erros
                 LEAVESR;
              ENDIF;
              WObjecto     = OR_LRPS;
              WCodCredor   = OR_CRHIP;
              WCodSegurado = OR_NSEG;
           ENDIF;

           //-----------------------------------------------------
           // Obter a informação do Segurado
           //-----------------------------------------------------
           IF (WCodCredor > *ZEROS);
              CHAIN (WCodSegurado) GFSEG;
              IF (NOT %FOUND(GFSEG));
                 WNome = *BLANKS;
                 WNIF  = *ZEROS;
              ELSE;
                 WNome = SEG_NOM;
                 WNIF  = SEG_NIF;
              ENDIF;
           ENDIF;

           //-----------------------------------------------------
           // Obter a data de resolução do Contrato
           //-----------------------------------------------------
           IF (WCodCredor > *ZEROS);
              CHAIN (WNumRecibo) FDTREC;
              IF (NOT %FOUND(FDTREC));
                 WDatResol = *ZEROS;
              ELSE;
                 WDatResol = $CvtData(VAL_WDTRE:'DMY':'YYMD');
              ENDIF;
           ENDIF;

           //-----------------------------------------------------
           // Obter a Descrição do Ramo
           //-----------------------------------------------------
           IF (WCodCredor > *ZEROS);
              CHAIN WCodRamoJM SD1RM R#SD1RM;
              IF (%FOUND(SD1RM));
                 WDescRamo = RM_DSRAMO;
              ELSE;
                 WDescRamo = *BLANKS;
              ENDIF;
           ENDIF;

           //-----------------------------------------------------
           // Obter a informação do Credor
           //-----------------------------------------------------
           IF (WCodCredor > *ZEROS);
              WKeyCredor = 2000000 + WCodCredor;
              CHAIN WKeyCredor GSINENT R#SINENT;
              IF (NOT %FOUND(GSINENT));
                 WNomCredor  = *BLANKS;
                 WMorada     = *BLANKS;
                 WLocalidade = *BLANKS;
                 WCodPostal  = *ZEROS;
                 WEmail      = *BLANKS;
                 WFax        = *BLANKS;
              ELSE;
                 WNomCredor  = ENT_NOME;
                 WMorada     = ENT_MORADA;
                 WLocalidade = ENT_LOCAL;
                 WCodPostal  = ENT_CDPOST;
                 WEmail      = ENT_FILLE2;
                 WFax        = ENT_CNTENT;
                 CHAIN WCodPostal SD1CP R#SD1CP;
                 IF (%FOUND (SD1CP));
                    WDesPostal = CP_DPOST;
                 ELSE;
                    WDesPostal = *BLANKS;
                 ENDIF;
              ENDIF;
           ENDIF;
        ENDSR;

        //=================================================================
        // Subrotina..: AddRecord
        // Objectivo..: Adicionar o registo do Credor Hipotecário.
        //=================================================================
        BEGSR AddRecord;
           $addHTTPVal('data');
           //----------------------------------------------------------
           // Informação da Apólice
           //----------------------------------------------------------
           $addHTTPVal('ramo':%EDITW(WCodRamo:'0  '));
           $addHTTPVal('apolice':%EDITW(WNumapol:'0       '));
           $addHTTPVal('desramo':WDescRamo);
           $addHTTPVal('objecto':WObjecto);
           $addHTTPVal('resolucao':%EDITW(WDatResol:'0    -  -  '));

           //----------------------------------------------------------
           // Informação do Recibo
           //----------------------------------------------------------
           $addHTTPVal('recibo':%EDITW(WNumRecibo:'0         '));
           $addHTTPVal('datapagamento':%EDITW(WDatPagam:'0    -  -  '));
           $addHTTPVal('datalimite':%EDITW(WDatLimite:'0    -  -  '));
           $addHTTPVal('valorrecibo':%EDITW(WValRecibo:'0           ,  '));
           $addHTTPVal('estado':%EDITW(WCodEstado:'0 '));

           //----------------------------------------------------------
           // Informação do Segurado
           //----------------------------------------------------------
           $addHTTPVal('codtomador':%EDITW(WCodSegurado:'0        '));
           $addHTTPVal('tomador':WNome);
           $addHTTPVal('nif':%EDITW(WNIF:'0         '));

           //----------------------------------------------------------
           // Informação do Credor
           //----------------------------------------------------------
           $addHTTPVal('credor':%EDITW(WCodCredor:'0     '));
           $addHTTPVal('nomecredor':WNomCredor);
           $addHTTPVal('morada':WMorada);
           $addHTTPVal('localidade':WLocalidade);
           $addHTTPVal('codpostal':%EDITW(WCodPostal:'0    -   '));
           $addHTTPVal('datacarta':%EDITW(WDatCarta:'0    -  -  '));
           $addHTTPVal('despostal':WDesPostal);
           $addHTTPVal('email':WEmail);
           $addHTTPVal('fax':WFax);

           $addHTTPVal();
        ENDSR;
      /END-FREE
