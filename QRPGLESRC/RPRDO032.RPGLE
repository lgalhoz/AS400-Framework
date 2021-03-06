     H DFTACTGRP(*NO) BNDDIR('JMDIR')
     H*=============================================================
     H* Programa..: RPRDO032
     H* Objectivo.: Disponibilizar informa��o sobre as Ap�lices
     H*             Anuladas com Credor Hipotec�rio.
     H*
     H* Descri��o.: Este programa � invocado por uma qualquer browser
     H*             de Internet, e devolve o resultado em XML
     H*             para o efeito est� preparado para devolver o
     H*             diferentes resultados em fun��o dos parametros.
     H*
     H*
     H* Par�metros: Este programa recebe como par�metros duas datas
     H*             que correspondem ao per�odo de pesquisa.
     H*
     H*             http://20.0.0.1/DemoComp?frame=login&user=apache
     H*             http://20.0.0.1/DemoComp?frame=apolchip&dtini=20150101&dtfim=20150110
     H*=============================================================
     FSD1RM     IF   F  128     3AIDISK    KEYLOC(5)   EXTFILE('SD.1.RM')
     FSD1CP     IF   F   64     7AIDISK    KEYLOC(2)   EXTFILE('SD.1.CP')
     FGSINENT   IF   F  400     7AIDISK    KEYLOC(1)   EXTFILE('G.SINENT')
     FSD1RE2    IF   E           K DISK    PREFIX(REC_)
     FFDTREC    IF   E           K DISK    PREFIX('VAL_')
     FGFSEG     IF   E           K DISK    PREFIX('SEG_')
     FGBASEGRA  IF   E           K DISK    PREFIX('AUT_')
     FGBASEGRO  IF   E           K DISK    PREFIX('OR_')
     FGCOBGRX4  IF   E           K DISK    PREFIX('LUS_')
     F*=============================================================
      /Copy QRPGLESRC,SSECO010
      /Copy QRPGLESRC,SCATO010
      /Copy QRPGLESRC,SSECO012
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SDBSO010

     D*==> Formato de registo de Recibos do Corretor
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
     D WSitApolice     S              1S 0 INZ(*ZEROS)
     D
     D WCodRamoJM      S              3S 0
     D WCodRamo        S              2S 0 INZ(*ZEROS)
     D WNumApol        S              7S 0 INZ(*ZEROS)
     D WDescRamo       S             30A
     D WObjecto        S             50A
     D WDatResol       S              8S 0
     D WDatAnula       S              8S 0
     D WCodMotivo      S              2S 0
     D WCodEstado      S              2S 0
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
        // Trabalhar fun��o em dois modos
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
        // Validar se pode utilizar a fun��o
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
        EXSR GetApolice;

        HTTPCODE = $getHTTPSrc();
        RETURN;

        //=================================================================
        // Subrotina..: GetApolice
        // Objectivo..: Obter informa��o das Apolices.
        //=================================================================
        BEGSR GetApolice;
           //-------------------------------------------------
           // Obter informa��o das Ap�lices Autom�vel
           //-------------------------------------------------
           SETLL *LOVAL GBASEGRA;
           READ GBASEGRA;
           DOW (NOT %EOF(GBASEGRA));
              WDatAnula  = $CvtData(AUT_DTALT:'DMY':'YYMD');
              WCodMotivo = AUT_MALT;
              WSitApolice  = AUT_SIT;
              WCodCredor = AUT_CRHIP;
              IF (WSitApolice = 3) AND (WDatAnula >= WDatInicio) AND
                 (WDatAnula <= WDatTermo) AND (WCodCredor > *ZEROS) AND
                 ((WCodMotivo = 61) OR (WCodMotivo = 64) OR (WCodMotivo = 65)
                 OR (WCodMotivo = 84) OR (WCodMotivo = 87));
                 WObjecto   = 'AUTOM�VEL ' + %TRIM(AUT_MARC1) + ' ' +
                              %TRIM(AUT_MODELO) + ' ' + %TRIM(AUT_MATR1);
                 WCodSegurado = AUT_NSEG;
                 WCodRamo     = AUT_RAMO;
                 WNumApol     = AUT_APOL;
                 EXSR GetRecibo;
              ENDIF;
              READ GBASEGRA;
           ENDDO;

           //-------------------------------------------------
           // Obter informa��o das Ap�lices Outros Ramos
           //-------------------------------------------------
           SETLL *LOVAL GBASEGRO;
           READ GBASEGRO;
           DOW (NOT %EOF(GBASEGRO));
              WDatAnula    = $CvtData(OR_DALT:'DMY':'YYMD');
              WCodMotivo   = OR_MALT;
              WSitApolice  = OR_SIT;
              WCodCredor   = OR_CRHIP;
              IF (WSitApolice = 3) AND (WDatAnula = WDatInicio) AND
                 (WDatAnula <= WDatTermo) AND (WCodCredor > *ZEROS) AND
                 ((WCodMotivo = 61) OR (WCodMotivo = 64) OR (WCodMotivo = 65)
                 OR (WCodMotivo = 84) OR (WCodMotivo = 87));
                 WObjecto     = OR_LRPS;
                 WCodSegurado = OR_NSEG;
                 WCodRamo     = OR_RAMO;
                 WNumApol     = OR_APOL;
                 EXSR GetRecibo;
              ENDIF;
              READ GBASEGRO;
           ENDDO;
        ENDSR;

        //=================================================================
        // Subrotina..: GetRecibo
        // Objectivo..: Obter informa��o do recibo da Ap�lice.
        //=================================================================
        BEGSR GetRecibo;
           //-------------------------------------------------
           // Obter o c�digo do Ramo a partir do Recibo
           //-------------------------------------------------
           WNumRecibo = *ZEROS;
           SETLL (WCodRamo:WNumApol) GCOBGRX4;
           READE (WCodRamo:WNumApol) GCOBGRX4;
           DOW (NOT %EOF(GCOBGRX4));
              IF (LUS_GCDCH > *ZEROS) AND (LUS_GCNREC > WNumRecibo);
                 WNumRecibo = LUS_GCNREC;
                 WCodEstado = LUS_GCSITR;
                 WDatCarta  = $cvtData(LUS_GCDCH:'YMD':'YYMD');
              ENDIF;
              READE (WCodRamo:WNumApol) GCOBGRX4;
           ENDDO;

           //------------------------------------------------------------
           // Obter mais informa��o do Recibo
           //------------------------------------------------------------
           CHAIN (54:%CHAR(WNumRecibo)) SD1RE2;
           IF (%FOUND(SD1RE2));
              MONITOR;
                 WNumRecibo = %DEC(REC_NREC2:9:0);
                 WDatLimite = $CvtData(%DEC(REC_DTLIM:6:0):'DMY':'YYMD');
                 WDatPagam  = $CvtData(%DEC(REC_DTCOB:6:0):'DMY':'YYMD');
                 WValRecibo = REC_VTREC;
                 WCodRamoJM = REC_CRAMO;
              ON-ERROR;
              ENDMON;
           ENDIF;

           //-----------------------------------------------------
           // Obter a informa��o do Segurado
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
           // Obter a data de resolu��o do Contrato
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
           // Obter a Descri��o do Ramo
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
           // Obter a informa��o do Credor
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

           //------------------------------------------------------------
           // Escrever oregisto
           //------------------------------------------------------------
           EXSR AddRecord;
        ENDSR;

        //=================================================================
        // Subrotina..: AddRecord
        // Objectivo..: Adicionar o registo do Credor Hipotec�rio.
        //=================================================================
        BEGSR AddRecord;
           $addHTTPVal('data');
           //----------------------------------------------------------
           // Informa��o da Ap�lice
           //----------------------------------------------------------
           $addHTTPVal('ramo':%EDITW(WCodRamo:'0  '));
           $addHTTPVal('apolice':%EDITW(WNumapol:'0       '));
           $addHTTPVal('desramo':WDescRamo);
           $addHTTPVal('objecto':WObjecto);
           $addHTTPVal('resolucao':%EDITW(WDatResol:'0    -  -  '));
           $addHTTPVal('anulacao':%EDITW(WDatAnula:'0    -  -  '));
           $addHTTPVal('motivo':%EDITW(WCodMotivo:'0  '));
           $addHTTPVal('descmot':$GetDescricao(27:%CHAR(WCodMotivo)));
           $addHTTPVal('estado':%EDITW(WSitApolice:'0 '));

           //----------------------------------------------------------
           // Informa��o do Recibo
           //----------------------------------------------------------
           $addHTTPVal('recibo':%EDITW(WNumRecibo:'0         '));
           $addHTTPVal('datapagamento':%EDITW(WDatPagam:'0    -  -  '));
           $addHTTPVal('datalimite':%EDITW(WDatLimite:'0    -  -  '));
           $addHTTPVal('valorrecibo':%EDITW(WValRecibo:'0           ,  '));
           $addHTTPVal('estado':%EDITW(WCodEstado:'0 '));

           //----------------------------------------------------------
           // Informa��o do Segurado
           //----------------------------------------------------------
           $addHTTPVal('codtomador':%EDITW(WCodSegurado:'0        '));
           $addHTTPVal('tomador':WNome);
           $addHTTPVal('nif':%EDITW(WNIF:'0         '));

           //----------------------------------------------------------
           // Informa��o do Credor
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
