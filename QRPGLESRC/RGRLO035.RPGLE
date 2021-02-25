     H NOMAIN
     H*==================================================================
     H* Programa..: RGRLO035
     H* Descrição.: Grupo de Funções XML.
     H*
     H*==================================================================
     FXMLFILE   IF   F  300        DISK    USROPN
     F*==================================================================
      /COPY QRPGLESRC,SGRLO010
      /COPY QRPGLESRC,SGRLO018
      /COPY QRPGLESRC,SSECO010
      /COPY QRPGLESRC,SDBSO010

     D*==> Caminho XML
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
     D*==================================================================

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
