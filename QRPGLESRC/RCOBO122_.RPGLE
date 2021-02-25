     H AUT(*ALL) ALWNULL(*USRCTL) DFTACTGRP(*NO) ACTGRP(*CALLER)
     H BNDDIR('JMDIR')
     H*==================================================================
     H* Programa..: RCOBO120
     H* Descrição.: Ler ficheiros XML, com recurso a funções RPG ILE.
     H*
     H*==================================================================
     FXMLFILE   IF   F  300        DISK    USROPN
     FTBIDDEF   UF   E           K DISK
     FTBIDDPL3  IF   E           K DISK    RENAME(TBIDDP:RBIDDPL3)
     F*TB2201M   O    E             PRINTER USROPN
     F*==================================================================
      /COPY QRPGLESRC,SGRLO010
      /COPY QRPGLESRC,SSECO010
      /COPY QRPGLESRC,SDBSO010

     D*==> Registo XML a Processar
     D LST#Registo     DS                  QUALIFIED
     D   NomFile                     10S 0 INZ(*ZEROS)
     D   MsgOriginal                 15A   INZ(*BLANKS)
     D   ValTotEnv                   13S 3 INZ(*ZEROS)
     D   QtdTotEnv                    4S 0 INZ(*ZEROS)
     D   ValTotRec                   13S 3 INZ(*ZEROS)
     D   QtdTotRec                    4S 0 INZ(*ZEROS)
     D   CodRetLote                   4A   INZ(*BLANKS)
     D   CodRetFile                   4A   INZ(*ZEROS)
     D   Rec                               LIKEDS(LST#RegRec) DIM(10)
     D
     D LST#Rec         DS                  QUALIFIED DIM(20)
     D   Numero                       9S 0 INZ(*ZEROS)
     D   Estado                       1A   INZ(*BLANKS)
     D   Valor                       13S 2 INZ(*ZEROS)
     D   IBAN                        25A   INZ(*BLANKS)
     D   ADC                         11S 0 INZ(*ZEROS)
     D   IcExiste                      N   INZ(*OFF)
     D   IsEmpty                       N   INZ(*ON)
     D
     D LST#RegRec      DS                  QUALIFIED
     D   ADC                         11S 0 INZ(*ZEROS)
     D   DatADC                       8S 0 INZ(*ZEROS)
     D   IBAN                        25A   INZ(*BLANKS)
     D   CodSwift                    10A   INZ(*BLANKS)
     D   Nome                        50A   INZ(*BLANKS)
     D   Ramo                         2S 0 INZ(*ZEROS)
     D   Apolice                      7S 0 INZ(*ZEROS)
     D   Recibo                       9S 0 INZ(*ZEROS)
     D   ValPago                     10S 2 INZ(*ZEROS)
     D   DatPago                      8S 0 INZ(*ZEROS)
     D   Estado                       4A   INZ(*BLANKS)
     D WIdxDet         S              5S 0
     D
     D*=======================================================
     D*  Declaração de Processos
     D*=======================================================
     D $OpenXMLData    PR             7A
     D   FileName                   140A   CONST
     D   Server                       5S 0 CONST OPTIONS(*NOPASS)
     D
     D $GetXMLAlfa     PR            40A
     D   TagNameIni                  40A   CONST
     D   TagNameFim                  40A   CONST OPTIONS(*NOPASS)
     D   Parametro                    2A   CONST OPTIONS(*NOPASS)
     D
     D $GetXMLNumero   PR            15S 5
     D   TagNameIni                  40A   CONST
     D   TagNameFim                  40A   CONST OPTIONS(*NOPASS)
     D   Parametro                    2A   CONST OPTIONS(*NOPASS)
     D
     D $GetXMLQtdReg   PR             4S 0
     D   TagName                     40A   CONST
     D
     D $GotoXMLTag     PR
     D   TagTree                    120A   CONST
     D   Registo                      4S 0 CONST OPTIONS(*NOPASS)
     D   Reset                         N   CONST OPTIONS(*NOPASS)
     D
     D LST#Tree        DS                  QUALIFIED DIM(10)
     D   Node                        20A   INZ(*BLANKS)
     D   Grupo                        3S 0 INZ(*ZEROS)
     D   LimInfer                     5S 0 INZ(*ZEROS)
     D   LimSuper                     5S 0 INZ(*ZEROS)
     D   IsEmpty                       N   INZ(*ON)
     D WXMLData        S          20000A
     D
     D*==> Campos de Trabalho
     D WIdx            S              5S 0
     D WIdxRec         S              5S 0
     D WFileName       S            140A
     D WServer         S              5S 0
     D WMsgErro        S              7A
     D WCodFile        S             10S 0
     D WVariavel       S             40A
     D WQtdRegis       S              4S 0
     D WQtdEnviada     S              5S 0
     D WTotEnviada     S             13S 0
     D
     D*==> Constantes
     D KTE_ENVIADO     C                   '3'
     D KTE_CONFIRMADO  C                   '4'
     D KTE_SUSPENDO    C                   '5'
     D*==================================================================
     C     *ENTRY        PLIST
     C                   PARM                    WFileName
     C                   PARM                    WServer
     C                   PARM                    WMsgErro
      /FREE
       //------------------------------------------------------
       // Transferir conteudo do ficheiro e validar se o
       // ficheiro XML já foi processado
       //------------------------------------------------------
       WMsgErro = $OpenXMLData(WFileName:WServer);
       IF (WMsgErro = *BLANKS);
          WIdxDet = 1;
          $GotoXMLTag('OrgnlGrpInfAndSts');
          LST#Registo.NomFile     = $GetXMLNumero('OrgnlMsgId':*BLANKS:'FN');
          EXSR ValFile; // Validar se o ficheiro já foi processado
          IF (WMsgErro <> *BLANKS);
             RETURN;
          ENDIF;

          //------------------------------------------------------
          // Validar se a informação está coerente
          //------------------------------------------------------
          LST#Registo.MsgOriginal = $GetXMLAlfa('OrgnlMsgNmId');
          LST#Registo.ValTotEnv   = $GetXMLNumero('OrgnlCtrlSum');
          LST#Registo.QtdTotEnv   = $GetXMLNumero('OrgnlNbOfTxs');
          IF (WQtdEnviada <> LST#Registo.QtdTotEnv);
             WMsgErro = 'COB0023'; // A quantidade enviada é diferente
             RETURN;
          ELSEIF (WTotEnviada <> LST#Registo.ValTotEnv);
             WMsgErro = 'COB0024'; // O Valor total enviado não coincide
             RETURN;
          ENDIF;

          $GotoXMLTag('OrgnlPmtInfAndSts');
          LST#Registo.ValTotRec   = $GetXMLNumero('OrgnlCtrlSum');
          LST#Registo.QtdTotRec   = $GetXMLNumero('OrgnlNbOfTxs');
          IF (WQtdEnviada <> LST#Registo.QtdTotRec);
             WMsgErro = 'COB0025'; // A quantidade enviada é diferente
             RETURN;
          ELSEIF (WTotEnviada <> LST#Registo.ValTotRec);
             WMsgErro = 'COB0026'; // O Valor total enviado não coincide
             RETURN;
          ENDIF;

          $GotoXMLTag('OrgnlPmtInfAndSts.StsRsnInf.Rsn');
          LST#Registo.CodRetLote  = $GetXMLAlfa('Prtry');
          IF (LST#Registo.CodRetLote <> 'M000');
             WMsgErro = 'COB0027'; // Ocorreram erros no procesamento do lote
             RETURN;
          ENDIF;

          $GotoXMLTag('OrgnlPmtInfAndSts');
          LST#Registo.CodRetFile  = $GetXMLAlfa('Prtry');
          IF (LST#Registo.CodRetLote <> 'L000');
             WMsgErro = 'COB0028'; // Ocorreram erros no procesamento dos registos
             RETURN;
          ENDIF;

          //---------------------------------------------
          // Actualizar detalhe dos registos
          //---------------------------------------------
          WQtdRegis = $GetXMLQtdReg('TxInfAndSts');
          DOW (Widx <= WQtdRegis);
             $GotoXMLTag('OrgnlPmtInfAndSts.TxInfAndSts':WIdx);
             LST#Registo.Rec(WIdx).ADC      =
                              $GetXMLNumero('MndtId':*BLANKS:'AU');
             LST#Registo.Rec(WIdx).DatADC   =
                              $GetXMLNumero('DtOfSgntr':*BLANKS:'DT');
             LST#Registo.Rec(WIdx).IBAN     = $GetXMLAlfa('IBAN');
             LST#Registo.Rec(WIdx).CodSwift = $GetXMLAlfa('BIC');
             LST#Registo.Rec(WIdx).Nome     = $GetXMLAlfa('Nm');
             LST#Registo.Rec(WIdx).Ramo     =
                              $GetXMLNumero('Ustrd':*BLANKS:'RM');
             LST#Registo.Rec(WIdx).Apolice  =
                              $GetXMLNumero('Ustrd':*BLANKS:'AP');
             LST#Registo.Rec(WIdx).Recibo   =
                              $GetXMLNumero('Ustrd':*BLANKS:'RC');
             LST#Registo.Rec(WIdx).ValPago  =
                              $GetXMLNumero('InstdAmt Ccy=''EUR''':'InstdAmt');
             LST#Registo.Rec(WIdx).DatPago  =
                              $GetXMLNumero('ReqdColltnDt':*BLANKS:'DT');
             LST#Registo.Rec(WIdx).Estado   = $GetXMLAlfa('Cd');
             EXSR Processar;
             WIdx += 1;
          ENDDO;
       ENDIF;
       *INLR = *ON;

       //===============================================================
       // Subrotina.: ValFile
       // Descrição.: Validar se o ficheiro já foi processado
       //===============================================================
       BEGSR ValFile;
          WMsgErro = *BLANKS;
          WCodFile = LST#Registo.NomFile;

          //--------------------------------------------------
          // Validar se o ficheiro já foi processado
          //--------------------------------------------------
          CHAIN (WCodFile) TBIDDE;
          IF (%FOUND(TBIDDEF));
             IF (TETIPF <> 'XML');
                WMsgErro = 'COB0020'; // O ficheiro recebido não é XML
                LEAVESR;
             ENDIF;
             IF (TEDATR <> *ZEROS);
                WMsgErro = 'COB0022'; // O ficheiro já foi processado
                LEAVESR;
             ENDIF;
          ELSE;
             WMsgErro = 'COB0021'; // Não existe este ficheiro
          ENDIF;

          //--------------------------------------------------
          // Listar Recibos constantes do XML
          //--------------------------------------------------
          WQtdEnviada = *ZEROS;
          WTotEnviada = *ZEROS;
          WIdx        = 1;
          SETLL (WCodFile) RBIDDPL3;
          READE (WCodFile) RBIDDPL3;
          DOW (NOT %EOF(TBIDDPL3));
             WQtdEnviada += 1;
             LST#Rec(WIdx).Numero   = IDREC;
             LST#Rec(WIdx).Estado   = IDSIT;
             LST#Rec(WIdx).Valor    = IDVAL;
          // LST#Rec(WIdx).IBAN     = IDSIT;
             LST#Rec(WIdx).ADC      = IDNADC;
             LST#Rec(WIdx).IcExiste = *OFF;
             LST#Rec(WIdx).IsEmpty  = *OFF;
             WQtdEnviada += IDVAL;
             WIdx += 1;
             LST#Rec(WIdx).IsEmpty  = *ON;
             READE (WCodFile) RBIDDPL3;
          ENDDO;
       ENDSR;

       //===============================================================
       // Subrotina.: Processar
       // Descrição.: Processar a informação do Recibo
       //===============================================================
       BEGSR Processar;
          //-------------------------------------------------
          // Localizar o recibo
          //-------------------------------------------------
          WIdxRec = 1;
          DOW (NOT LST#Rec(WIdxRec).IsEmpty);
             IF (LST#Rec(WIdxRec).Numero = LST#Registo.Rec(WIdx).Recibo);
             ENDIF;
          ENDDO;
       ENDSR;
      /END-FREE

     P*==================================================================
     P* Processo..: $GetXMLAlfa
     P* Descrição.: Obter a variável Alfanumérica
     P*==================================================================
     P $GetXMLAlfa     B                   EXPORT
     D $GetXMLAlfa     PI            40A
     D   TagNameIni                  40A   CONST
     D   TagNameFim                  40A   CONST OPTIONS(*NOPASS)
     D   Parametro                    2A   CONST OPTIONS(*NOPASS)
     D
     D*==> Campos de Trabalho
     D WPosIni         S              5S 0
     D WPosFim         S              5S 0
     D WLimInfer       S              5S 0
     D WLimSuper       S              5S 0
     D WXMLDTBK        S          10000A
     D WTagNameIni     S             40A
     D WTagNameFim     S             40A
     D WParametro      S              2A
     D WValor          S             40A
     D WNivel          S              4S 0
     D
     D*==> Definição de constantes
     D StrLower        C                   'abcdefghijklmnopqrstuvxzyw '
     D StrUpper        C                   'ABCDEFGHIJKLMNOPQRSTUVXZYW#'
     D*==================================================================
      /FREE
         WParametro  = *BLANKS;
         WXMLDTBK = %XLATE(StrLower:StrUpper:%TRIM(WXMLData));
         WTagNameIni = %XLATE(StrLower:StrUpper:%TRIM(TagNameIni));
         WTagNameFim = WTagNameIni;
         IF (%PARMS() > 1);
            IF (TagNameFim <> *BLANKS);
               WTagNameFim = %XLATE(StrLower:StrUpper:%TRIM(TagNameFim));
            ENDIF;
            IF (%PARMS() > 2);
               WParametro = Parametro;
            ENDIF;
         ENDIF;

         //-----------------------------------------------------
         // Posicionar o cursor na TAG
         //-----------------------------------------------------
         WLimInfer = 1;
         WLimSuper = *ZEROS;
         WNivel = 1;
         DOW (NOT LST#Tree(WNivel).IsEmpty);
            WLimInfer = LST#Tree(WNivel).LimInfer;
            WLimSuper = LST#Tree(WNivel).LimSuper;
            WNivel += 1;
         ENDDO;

         //-----------------------------------------------------
         // Extrair o registo
         //-----------------------------------------------------
         WPosIni = WLimInfer;
         WPosFim = %SCAN('<'+%TRIM(WTagNameIni)+'>':WXMLDTBK:WPosIni);
         IF (WPosFim > *ZEROS);
            WPosIni = WPosFim + %LEN('<'+%TRIM(WTagNameIni)+'>');
            WPosFim = %SCAN('</'+%TRIM(WTagNameFim)+'>':WXMLDTBK:WPosIni);
            IF (WPosFim > *ZEROS) AND ((WLimSuper = *ZEROS) OR
               (WPosFim <= WLimSuper));
               EXSR TrataTags;
            ENDIF;
         ENDIF;

         RETURN WValor;

         //======================================================
         // Subrotina.: TrataTags
         // Objectivo.: Tratamento especial das Tags
         //======================================================
         BEGSR TrataTags;
            WValor =  %SUBST(WXMLData:WPosIni:(WPosFim-WPosIni));
            IF (WParametro = 'FN');
               WValor = %SUBST(%TRIM(WValor):6:10);
            ELSEIF (WParametro = 'RM');
               WValor = %SUBST(%TRIM(WValor):8:2);
            ELSEIF (WParametro = 'AP');
               WValor = %SUBST(%TRIM(WValor):11:7);
            ELSEIF (WParametro = 'RC');
               WValor = %SUBST(%TRIM(WValor):21:9);
            ELSEIF (WParametro = 'DT');
               WValor = %SUBST(%TRIM(WValor):1:4)+
                        %SUBST(%TRIM(WValor):6:2)+
                        %SUBST(%TRIM(WValor):9:2);
            ELSEIF (WParametro = 'AU');
               WValor = %SUBST(%TRIM(WValor):6);
            ENDIF;
         ENDSR;
      /END-FREE
     P $GetXMLAlfa     E

     P*==================================================================
     P* Processo..: $GetXMLNumero
     P* Descrição.: Obter a variável Numérica
     P*==================================================================
     P $GetXMLNumero   B                   EXPORT
     D $GetXMLNumero   PI            15S 5
     D   TagNameIni                  40A   CONST
     D   TagNameFim                  40A   CONST OPTIONS(*NOPASS)
     D   Parametro                    2A   CONST OPTIONS(*NOPASS)
     D
     D*==> Campos de Trabalho
     D WTagNameIni     S             40A
     D WTagNameFim     S             40A
     D WParametro      S              2A
     D WValor          S             40A
     D*==================================================================
      /FREE
         WParametro  = *BLANKS;
         WTagNameIni = TagNameIni;
         WTagNameFim = *BLANKS;
         IF (%PARMS() > 1);
            IF (TagNameFim <> *BLANKS);
               WTagNameFim = TagNameFim;
            ENDIF;
            IF (%PARMS() > 2);
               WParametro = Parametro;
            ENDIF;
         ENDIF;

         WValor = $GetXMLAlfa(WTagNameIni:WTagNameFim:WParametro);
         MONITOR;
            RETURN %DEC(%TRIM(WValor):15:5);
         ON-ERROR;
            RETURN *ZEROS;
         ENDMON;
      /END-FREE
     P $GetXMLNumero   E

     P*==================================================================
     P* Processo..: $OpenXMLData
     P* Descrição.: Abrir um ficheiro XML para proceder ao respectivo
     P*             tratamento. Pode abrir o XML no ambiente AS400, ou
     P*             de um qualquer servidor FTP.
     P*==================================================================
     P $OpenXMLData    B                   EXPORT
     D $OpenXMLData    PI             7A
     D   FileName                   140A   CONST
     D   Server                       5S 0 CONST OPTIONS(*NOPASS)
     D
     D*==> Formato de Registo
     D RECORD          DS
     D   DATA                  1    300
     D
     D*==> Comando para Criar o Ficheiro
     D shell           PR                  EXTPGM('QCMDEXC')
     D   StdCmd                     200A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5
     D WStdCmd         S            200A
     D WStdLen         S             15P 5
     D
     D*==> Campos de Trabalho
     D WServer         S              5S 0
     D WFileName       S            140A
     D WMsgErro        S              7A
     D*==================================================================
      /FREE
         WServer   = *ZEROS;
         WXMLData  = *BLANKS;
         WFileName = FileName;
         IF (%PARMS() > 1);
            WServer = Server;
         ENDIF;

         WMsgErro = *BLANKS;
         IF (WServer <> *ZEROS);
            WMsgErro  = $GetFTP(WServer:%TRIM(WFileName):'QTEMP':'XMLFILE');
            WFileName = 'QTEMP/XMLFILE';
         ENDIF;
         IF (WMsgErro = *BLANKS);
            EXSR ReadNative;
         ENDIF;
         RETURN WMsgErro;

         //=========================================================================
         // Subrotina.: ReadNative
         // Objectivo.: Ler um ficheiro DB2
         //=========================================================================
         BEGSR ReadNative;
            MONITOR;
               WStdCmd = 'OVRDBF FILE(XMLFILE) TOFILE('+%TRIM(WFileName)+')';
               WStdLen = %LEN(WStdCmd);
               shell(WStdCmd:WStdLen);
               OPEN XMLFILE;
               READ XMLFILE  RECORD;
               DOW (NOT %EOF(XMLFILE));
                 WXMLData = %TRIM(WXMLDATA) + %TRIM(Data);
                 READ XMLFILE  RECORD;
               ENDDO;
               CLOSE XMLFILE;
            ON-ERROR;
               WMsgErro = 'XML0001'; // Ocorreram erros a abrir o ficheiro
            ENDMON;
         ENDSR;
      /END-FREE
     P $OpenXMLData    E

     P*==================================================================
     P* Processo..: GotoXMLTag
     P* Descrição.: Posicionar numa tag Específica
     P*==================================================================
     P $GotoXMLTag     B                   EXPORT
     D $GotoXMLTag     PI
     D   TagTree                    120A   CONST
     D   NumRegisto                   4S 0 CONST OPTIONS(*NOPASS)
     D   Reset                         N   CONST OPTIONS(*NOPASS)
     D
     D*==> Campos de Trabalho
     D WReset          S               N
     D WNumRegisto     S              5S 0
     D WIdxRecord      S              5S 0
     D WRegActual      S              5S 0
     D WPosIni         S              5S 0
     D WPosFim         S              5S 0
     D WPosIni_        S              5S 0
     D WPosFim_        S              5S 0
     D WLimSup_        S              5S 0
     D WTagTree        S            120A
     D WXMLDTBK        S          10000A
     D WTagName        S             40A
     D WNivel          S              4S 0
     D WGrupo          S              4S 0
     D WIsLast         S               N
     D
     D*==> Definição de constantes
     D StrLower        C                   'abcdefghijklmnopqrstuvxzyw '
     D StrUpper        C                   'ABCDEFGHIJKLMNOPQRSTUVXZYW#'
     D*==================================================================
      /FREE
        WNumRegisto = 1;
        WReset      = *ON;
        IF (%PARMS() > 1);
           WNumRegisto = NumRegisto;
           IF (%PARMS() > 2);
              WReset = Reset;
           ENDIF;
        ENDIF;
        WTagTree = %XLATE(StrLower:StrUpper:%TRIM(TagTree));
        WXMLDTBK = %XLATE(StrLower:StrUpper:%TRIM(WXMLData));

        //--------------------------------------------
        // Incrementar nível da TAG
        //--------------------------------------------
        WGrupo = 1;
        WNivel = 1;
        IF (NOT WReset);
           DOW (NOT LST#Tree(WNivel).IsEmpty);
              WGrupo = LST#Tree(Wnivel).Grupo + 1;
              WNivel += 1;
           ENDDO;
        ENDIF;

        //--------------------------------------------
        // Decompôr a Arvore em níveis
        //--------------------------------------------
        WIsLast  = *OFF;
        WPosFim = 1;
        LST#Tree(WNivel).IsEmpty = *ON;
        DOW (WPosFim > *ZEROS);
           WPosIni = 1;
           WPosFim = %SCAN('.':WTagTree:WPosIni);
           IF (WPosFim > *ZEROS);
              WTagName = %SUBST(WTagTree:WPosIni:(WPosFim-WPosIni));
              WPosFim += 1;
              WTagTree = %SUBST(WTagTree:WPosFim);
           ELSE;
              WTagName = WTagTree;
              WIsLast  = *ON;
           ENDIF;
           LST#Tree(WNivel).Node     = WTagName;
           LST#Tree(WNivel).IsEmpty  = *OFF;
           LST#Tree(WNivel).Grupo    = WGrupo;
           EXSR GetExtremos;
           WNivel += 1;
           LST#Tree(WNivel).IsEmpty  = *ON;
        ENDDO;

        //============================================================
        // Subrotina.: GetExtremos
        // Objectivo.: Obter a posição dos extremos da Tag
        //============================================================
        BEGSR GetExtremos;
           LST#Tree(WNivel).LimInfer = *ZEROS; // Limite Inferior
           LST#Tree(WNivel).LimSuper = *ZEROS; // Limite Superior
           WTagName = %XLATE(StrLower:StrUpper:%TRIM(WTagName));

           WPosIni_ = 1;
           WLimSup_ = 10000;
           IF (WNivel > 1);
              WPosIni_ = LST#Tree(WNivel-1).LimInfer;
              WLimSup_ = LST#Tree(WNivel-1).LimSuper;
           ENDIF;

           WRegActual = 1;
           WIdxRecord = 1;
           IF (WIsLast);
              WRegActual = WNumRegisto;
           ENDIF;
           DOW (WIdxRecord <= WRegActual);
              WPosFim_ = %SCAN('<'+%TRIM(WTagName)+'>':WXMLDTBK:WPosIni_);
              IF (WPosFim_ > *ZEROS) AND (WPosFim_ <= WLimSup_);
                 WPosIni_ = WPosFim_ + %LEN('<'+%TRIM(WTagName)+'>');
                 LST#Tree(WNivel).LimInfer = WPosIni_;
                 WPosFim_ = %SCAN('</'+%TRIM(WTagName)+'>':WXMLDTBK:WPosIni_);
                 IF (WPosFim_ > *ZEROS);
                    LST#Tree(WNivel).LimSuper = (WPosFim_ - 1); // Limite Superior
                 ENDIF;
              ENDIF;
              WIdxRecord += 1;
           ENDDO;
        ENDSR;
      /END-FREE
     P $GotoXMLTag     E

     P*==================================================================
     P* Processo..: $GetXMLQtdReg
     P* Descrição.: Obter a quantidade de registos de uma Tag
     P*==================================================================
     P $GetXMLQtdReg   B                   EXPORT
     D $GetXMLQtdReg   PI             4S 0
     D   TagName                     40A   CONST
     D
     D*==> Campos de Trabalho
     D WXMLDTBK        S          10000A
     D WTagName        S             40A
     D WPosIni         S              5S 0
     D WPosFim         S              5S 0
     D WQtdRegistos    S              4S 0
     D
     D*==> Definição de constantes
     D StrLower        C                   'abcdefghijklmnopqrstuvxzyw '
     D StrUpper        C                   'ABCDEFGHIJKLMNOPQRSTUVXZYW#'
     D*==================================================================
      /FREE
         WXMLDTBK = %XLATE(StrLower:StrUpper:%TRIM(WXMLData));
         WTagName = %XLATE(StrLower:StrUpper:%TRIM(TagName));

         //-----------------------------------------------------
         // Contar as Tags
         //-----------------------------------------------------
         WQtdRegistos = *ZEROS;
         WPosIni   = 1;
         WPosFim   = WPosIni;
         DOW (WPosFim > *ZEROS);
            WPosFim = %SCAN('<'+%TRIM(WTagName)+'>':WXMLDTBK:WPosIni);
            IF (WPosFim > *ZEROS);
               WQtdRegistos += 1;
               WPosIni = WPosFim + %LEN('<'+%TRIM(WTagName)+'>');
            ENDIF;
         ENDDO;
         RETURN WQtdRegistos;
      /END-FREE
     P $GetXMLQtdReg   E
