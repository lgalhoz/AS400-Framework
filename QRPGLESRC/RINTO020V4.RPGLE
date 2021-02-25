     H NOMAIN
     H*==============================================================
     H* Programa..: RINTO020
     H*--------------------------------------------------------------
     H* Descrição.: Grupo de Api's para a geração dos Interfaces dos
     H*             Mediadores.
     H*
     H*==============================================================
     F*==> Informação sobre os Mediadores Alvo
     FFINTP050  IF   E           K DISK    PREFIX(PRM_) USROPN
     FFINTP051  IF   E           K DISK    PREFIX(PRM_) USROPN
     FFINTP052  IF   E           K DISK    PREFIX(PRM_) USROPN
     FFINTP053  IF   E           K DISK    PREFIX(PRM_) USROPN
     FFINTP054  IF   E           K DISK    PREFIX(PRM_) USROPN
     FFINTP058  UF A E           K DISK    PREFIX(INT_) USROPN
     FFINTP059  UF A E           K DISK    PREFIX(INT_) USROPN
     F*==============================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SINTO010

     D*==> Obter a Template da Interface para o Mediador
     D GetTemplate     PR                  LIKEDS(LST#FldParm) DIM(DIM#MaxParm)
     D   CodMedia                     9S 0 CONST
     D   TipInter                     3A   CONST
     D
     D*==> Obter o Código de Sessão do Mediador
     D GetCodSess      PR             3S 0
     D   CodMedia                     9S 0 CONST
     D   TipInter                     3A   CONST
     D
     D*==> Sessões abertas para o Mediador
     D LST#SesMed      DS                  QUALIFIED DIM(DIM#MaxSess)
     D   CodMedia                     9S 0 INZ(*ZEROS)
     D   TipInter                     3A   INZ(*BLANKS)
     D   CodInter                     5S 0 INZ(*ZEROS)
     D   NuSequ                       5S 0 INZ(*ZEROS)
     D   QtdeRegis                    5S 0 INZ(*ZEROS)
     D   CodErro                      7A   INZ(*BLANKS)
     D   CodProcess                   5S 0 INZ(*ZEROS)
     D   CodFile                      5S 0 INZ(*ZEROS)
     D   Duplicate                    1A   INZ(*BLANKS)
     D   Fields                            LIKEDS(LST#Field) DIM(DIM#MaxField)
     D   LastField                    3S 0 INZ(*ZEROS)
     D   Parms                             LIKEDS(LST#FldParm) DIM(DIM#MaxParm)
     D   LastParm                     3S 0 INZ(*ZEROS)
     D   IsEmpty                       N   INZ(*ON)
     D
     D*==> Conteudo dos Campos
     D LST#Field       DS                  QUALIFIED
     D   Name                        10A   INZ(*BLANKS)
     D   Value                      100A   INZ(*BLANKS)
     D   IsEmpty                       N   INZ(*ON)
     D
     D*==> Parametrização Standard para o Mediador
     D LST#Standard    DS                  QUALIFIED DIM(DIM#MaxStd)
     D   TipInter                     3A   INZ(*BLANKS)
     D   Parms                             LIKEDS(LST#FldParm) DIM(DIM#MaxParm)
     D   IsEmpty                       N   INZ(*ON)
     D
     D*==> Parametrização do ficheiro parao Mediador
     D LST#FldParm     DS                  QUALIFIED
     D   IsKey                        1A   INZ(*BLANKS)
     D   Name                        10A   INZ(*BLANKS)
     D   Label                       20A   INZ(*BLANKS)
     D   Descr                       20A   INZ(*BLANKS)
     D   CvtCode                      2A   INZ(*BLANKS)
     D   NuTabl                       5S 0 INZ(*ZEROS)
     D   Duplicate                    1A   INZ(*BLANKS)
     D   LatChars                     1A   INZ(*BLANKS)
     D   UpperChars                   1A   INZ(*BLANKS)
     D   IsEmpty                       N   INZ(*ON)
     D
     D*==> Definição das Constantes
     D MED#Standard    C                   1
     D DIM#MaxSess     C                   10
     D DIM#MaxField    C                   50
     D DIM#MaxParm     C                   50
     D DIM#MaxStd      C                   10
     D STS#Aberto      C                   'OPN'
     D*===========================================================

     P*==========================================================
     P* Processo..: $StrIntMedia
     P* Objectivo.: Iniciar o Interface para um Mediador
     P*==========================================================
     P $StrIntMedia    B                   EXPORT
     D                 PI             7A
     D   CodMedia                     9S 0 CONST
     D   CodProcess                   5S 0 CONST
     D   CodFile                      5S 0 CONST
     D   TipInter                     3A   CONST
     D
     D*==> Campos de Trabalho
     D WCodErro        S              7A
     D WCodInter       S              3S 0
     D WCurSess        S              3S 0
     D WIsTheFirst     S               N
     D WSesInterf      S              5S 0
     D*==========================================================
      /FREE
        //-------------------------------------------------------
        // Validar se a Sessão é a primeira a activar
        //-------------------------------------------------------
        WCodErro    = *BLANKS;
        WIsTheFirst = *ON;
        WCurSess    = 1;
        DOW (WCurSess <= DIM#MaxSess);
           IF (NOT LST#SesMed(WCurSess).IsEmpty);
              WIsTheFirst = *OFF;
              LEAVE;
           ENDIF;
           WCurSess += 1;
        ENDDO;

        //------------------------------------------------------
        // Se o Interface não estiver aberto então abre
        //------------------------------------------------------
        WCurSess = GetCodSess(CodMedia:TipInter);
        IF (WCurSess = *ZEROS);
           WCurSess  = 1;
           DOW (WCurSess <= DIM#MaxSess);
              IF (LST#SesMed(WCurSess).IsEmpty);
                 LEAVE;
              ENDIF;
              WCurSess += 1;
           ENDDO;
           IF (WCurSess > DIM#MaxSess);
              RETURN 'INT0015';
           ENDIF;
        ELSE;
           RETURN 'INT0014';
        ENDIF;

        //------------------------------------------------------
        // Abrir os ficheiros de trabalho para a primeira sessão
        //------------------------------------------------------
        IF (WIsTheFirst);
           MONITOR;
              OPEN FINTP051;
              OPEN FINTP052;
              OPEN FINTP053;
              OPEN FINTP054;
              OPEN FINTP058;
              OPEN FINTP059;
           ON-ERROR;
              RETURN 'INT0016';
           ENDMON;
        ENDIF;

        //------------------------------------------------------
        // Actualizar o registo para a nova sessão
        //------------------------------------------------------
        LST#SesMed(WCurSess).IsEmpty = *OFF;
        LST#SesMed(WCurSess).CodMedia  = CodMedia;
        LST#SesMed(WCurSess).TipInter  = TipInter;
        LST#SesMed(WCurSess).CodProcess= CodProcess;
        LST#SesMed(WCurSess).CodFile   = CodFile;
        LST#SesMed(WCurSess).CodInter  = *ZEROS;
        LST#SesMed(WCurSess).Nusequ    = *ZEROS;
        LST#SesMed(WCurSess).QtdeRegis = *ZEROS;
        LST#SesMed(WCurSess).LastParm  = *ZEROS;

        WSesInterf = $SetFiles('FINTP058');
        $SetCriteria('(CDMEDIA = '''+%CHAR(CodMedia)+''') +
                       AND (TPINTER = '''+TipInter+''') AND +
                       (CDSTATUS = ''OPN'')');
        IF ($GetRecord(1) > *ZEROS);
           WCodInter = $GetFieldNum('CDINTER');
           LST#SesMed(WCurSess).CodInter = WCodInter;
           LST#SesMed(WCurSess).Nusequ = $GetNextKey('FINTP059':'NUSEQU':
                             '(CDINTER = '''+%CHAR(WCodInter)+''') AND +
                              (CDMEDIA = '''+%CHAR(CodMedia)+''')');
        ENDIF;
        $RmvDBSession(WSesInterf);
        RETURN WCodErro;
      /END-FREE
     P $StrIntMedia    E

     P*==========================================================
     P* Processo..: $EndIntMedia
     P* Objectivo.: Termianr o Interface do Mediador
     P*==========================================================
     P $EndIntMedia    B                   EXPORT
     D                 PI             7A
     D   CodMedia                     9S 0 CONST
     D   TipInter                     3A   CONST
     D
     D*==> Campos de Trabalho
     D WCodErro        S              7A
     D WIsTheLast      S               N
     D WCodInter       S              3S 0
     D WCurSess        S              3S 0
     D*==========================================================
      /FREE
        //------------------------------------------------------
        // Localizar a Sessão
        //------------------------------------------------------
        WCodErro = *BLANKS;
        WCurSess = GetCodSess(CodMedia:TipInter);
        IF (WCurSess = *ZEROS);
           RETURN 'INT0017';
        ENDIF;

        //------------------------------------------------------
        // Actualizar o registo da Sessão
        //------------------------------------------------------
        IF (LST#SesMed(WCurSess).QtdeRegis > *ZEROS);
           WCodInter = LST#SesMed(WCurSess).CodInter;
           IF (WcodInter > *ZEROS);
              CHAIN (CodMedia:WCodInter) RINTP058;
              IF (%FOUND(FINTP058));
                 INT_QTDREGIS += LST#SesMed(WCurSess).QtdeRegis;
                 INT_INT05802 = %DATE('9999-12-31');
                 UPDATE RINTP058 %FIELDS(INT_QTDREGIS:INT_INT05802);
              ELSE;
                 WCodErro = 'INT0019';
              ENDIF;
           ELSE;
              WCodErro = 'INT0018';
           ENDIF;
        ENDIF;

        LST#SesMed(WCurSess).CodErro = WCodErro;
        IF (WCodErro <> *BLANKS);
           RETURN WCodErro;
        ENDIF;

        //-------------------------------------------------------
        // Libertar a Sessão
        //-------------------------------------------------------
        LST#SesMed(WCurSess).IsEmpty = *ON;

        //-------------------------------------------------------
        // Fechar ficheiros de trabalho se não existirem activas
        //-------------------------------------------------------
        WIsTheLast = *ON;
        WCurSess  = 1;
        DOW (WCurSess <= DIM#MaxSess);
           IF (NOT LST#SesMed(WCurSess).IsEmpty);
              WIsTheLast = *OFF;
              LEAVE;
           ENDIF;
           WCurSess += 1;
        ENDDO;

        IF (WIsTheLast);
           MONITOR;
              CLOSE FINTP051;
              CLOSE FINTP052;
              CLOSE FINTP053;
              CLOSE FINTP054;
              CLOSE FINTP058;
              CLOSE FINTP059;
           ON-ERROR;
              WCodErro = 'INT0020';
           ENDMON;
        ENDIF;
        RETURN WCodErro;
      /END-FREE
     P $EndIntMedia    E

     P*==========================================================
     P* Processo..: $GetIntMedia
     P* Objectivo.: Posicionar-se no Interface do Mediador
     P*==========================================================
     P $GetIntMedia    B                   EXPORT
     D                 PI             3S 0
     D   CodMedia                     9S 0 CONST
     D   TipInter                     3A   CONST
     D
     D*==> Campos de Trabalho
     D WCodErro        S              7A
     D WCurSess        S              3S 0
     D*==========================================================
      /FREE
        //------------------------------------------------------
        // Localizar a Sessão
        //------------------------------------------------------
        WCurSess = GetCodSess(CodMedia:TipInter);
        IF (WCurSess = *ZEROS);
           RETURN *ZEROS;
        ENDIF;
        RETURN WCurSess;
      /END-FREE
     P $GetIntMedia    E

     P*==========================================================
     P* Processo..: $GetIntMedErro
     P* Objectivo.: Obter o estado do erro
     P*==========================================================
     P $GetIntMedErro  B                   EXPORT
     D                 PI             7A
     D   CodMedia                     9S 0 CONST
     D   TipInter                     3A   CONST
     D
     D*==> Campos de Trabalho
     D WCurSess        S              3S 0
     D*==========================================================
      /FREE
        //------------------------------------------------------
        // Localizar a Sessão
        //------------------------------------------------------
        WCurSess = GetCodSess(CodMedia:TipInter);
        IF (WCurSess = *ZEROS);
           RETURN 'INT0017';
        ENDIF;
        RETURN LST#SesMed(WCurSess).CodErro;
      /END-FREE
     P $GetIntMedErro  E

     P*==========================================================
     P* Processo..: $AddIntRecMed
     P* Objectivo.: Adicionar um registo ao Mediador
     P*==========================================================
     P $AddIntRecMed   B                   EXPORT
     D                 PI             7A
     D   CodMediador                  9S 0 CONST
     D   TipInter                     3A   CONST
     D
     D*==> Campos de Trabalho
     D WCodErro        S              7A
     D WParms          DS                  LIKEDS(LST#FldParm) DIM(DIM#MaxParm)
     D WFields         DS                  LIKEDS(LST#Field) DIM(DIM#MaxField)
     D WCurSess        S              3S 0
     D WIdxParm        S              3S 0
     D WIdxFld         S              3S 0
     D WRegisto        S                   LIKE(INT_DETAIL)
     D WKeyField       S                   LIKE(INT_KEYFLD)
     D WIsToWrite      S               N
     D WFieldValue     S            100A
     D WFieldNum       S              8S 0
     D WNuSqBak        S              5S 0
     D
     D*==> Definição de Constantes
     D CHR#Delimiter   C                   '|'
     D*==========================================================
      /FREE
        //-----------------------------------------------
        // Localizar a sessão para o Mediador
        //-----------------------------------------------
        WCodErro = *BLANKS;
        WCurSess = GetCodSess(CodMediador:TipInter);
        IF (WCurSess = *ZEROS);
           RETURN 'INT0017';
        ENDIF;

        //-----------------------------------------------
        // Obter a lista de Parâmetros
        //-----------------------------------------------
        EXSR GetParms;
        IF (WCodErro <> *BLANKS);
           RETURN WCodErro;
        ENDIF;

        //-----------------------------------------------
        // Criar Novo Ficheiro de Interface
        //-----------------------------------------------
        IF (LST#SesMed(WCurSess).CodInter = *ZEROS);
           EXSR AddHeader;
           IF (WCodErro <> *BLANKS);
              RETURN WCodErro;
           ENDIF;
        ENDIF;


        //----------------------------------------------
        // Escrever o Detalhe do registo
        //----------------------------------------------
        EXSR FormatRecord;
        WIsToWrite = *ON;
        IF (LST#SesMed(WCurSess).Duplicate = 'N');
           SETLL (CodMediador:LST#SesMed(WCurSess).CodInter) RINTP059;
           READE(N) (CodMediador:LST#SesMed(WCurSess).CodInter) RINTP059;
           DOW (NOT %EOF(FINTP059));
              IF (%TRIM(INT_KEYFLD) = %TRIM(WKeyField)) AND
               (%TRIM(INT_KEYFLD) <> *BLANKS);
                 WIsToWrite = *OFF;
                 WNuSqBak = INT_NUSEQU;
                 LEAVE;
              ENDIF;
              READE(N) (CodMediador:LST#SesMed(WCurSess).CodInter) RINTP059;
           ENDDO;
        ENDIF;

        IF (WIsToWrite);
           INT_CDMEDIA  = CodMediador;
           INT_CDINTER  = LST#SesMed(WCurSess).CodInter;
           INT_NUSEQU   = LST#SesMed(WCurSess).NuSequ;
           INT_CDFILE   = LST#SesMed(WCurSess).CodFile;
           INT_DETAIL   = WRegisto;
           INT_KEYFLD   = WKeyField;
           INT_INT05901 = %DATE();
           INT_INT05902 = %DATE('9999-12-31');
           WRITE RINTP059;
           LST#SesMed(WCurSess).NuSequ += 1;
           LST#SesMed(WCurSess).QtdeRegis += 1;
        ELSE;
           CHAIN (CodMediador:LST#SesMed(WCurSess).CodInter:WNuSqBak) RINTP059;
           INT_DETAIL = WRegisto;
           UPDATE RINTP059 %FIELDS(INT_DETAIL);
        ENDIF;
        RETURN WCodErro;

        //================================================================
        // Subrotina.: FormatRecord
        // objectivo.: Formatar o registo do Interface
        //================================================================
        BEGSR FormatRecord;
           WRegisto  = *BLANKS;
           WKeyField = *BLANKS;
           WParms    = LST#SesMed(WCurSess).Parms;
           WFields   = LST#SesMed(WCurSess).Fields;
           WIdxParm  = 1;
           DOW (NOT WParms(WidxParm).IsEmpty);
              WIdxFld = 1;
              DOW (NOT WFields(WIdxFld).IsEmpty);
                 IF (%TRIM(WFields(WIdxFld).Name)=%TRIM(WParms(WIdxParm).Name));
                    WFieldValue = %TRIM(WFields(WidxFld).Value);
                    WFieldValue =$CvtCode(WFieldValue:WParms(WIdxParm).CvtCode);
                    WRegisto = %TRIM(WRegisto) + WFieldValue;
                    IF (WParms(WIdxParm).IsKey = 'S');
                      WKeyField=%TRIM(WKeyField)+%TRIM(WFields(WidxFld).Value);
                    ENDIF;
                    LEAVE;
                 ENDIF;
                 WIdxFld += 1;
              ENDDO;
              WRegisto = %TRIM(WRegisto)+CHR#Delimiter;
              WIdxParm += 1;
           ENDDO;
        ENDSR;

        //================================================================
        // Subrotina.: AddHeader
        // objectivo.: Adicionar o Cabeçalho
        //================================================================
        BEGSR AddHeader;
           INT_CDMEDIA  = CodMediador;
           LST#SesMed(WCurSess).CodInter = $GetNextKey('FINTP059':'CDINTER':
                             '(CDMEDIA = '''+%CHAR(CodMediador)+''')');
           INT_CDINTER  = LST#SesMed(WCurSess).CodInter;
           INT_NMINTER  = *BLANKS;
           INT_TPINTER  = TipInter;
           INT_CDPROCE  = LST#SesMed(WCurSess).CodProcess;
           INT_DTPRODUC = $DateToInt(%DATE());
           INT_DTENVIO  = *ZEROS;
           INT_CDSTATUS = STS#Aberto;
           INT_QTDREGIS = *ZEROS;
           INT_INT05801 = %DATE();
           INT_INT05802 = %DATE('9999-12-31');
           WRITE RINTP058;
           LST#SesMed(WCurSess).Nusequ = 1;

           //----------------------------------------------
           // Escrever o Header do registo
           //----------------------------------------------
           WRegisto = *BLANKS;
           WIdxParm = 1;
           WParms   = LST#SesMed(WCurSess).Parms;
           DOW (NOT WParms(WIdxParm).IsEmpty);
              WRegisto = %TRIM(WRegisto)+%TRIM(WParms(WIdxParm).Label)+
                         CHR#Delimiter;
              WIdxParm += 1;
           ENDDO;

           INT_CDMEDIA  = LST#SesMed(WCurSess).CodMedia;
           INT_CDINTER  = LST#SesMed(WCurSess).CodInter;
           INT_NUSEQU   = LST#SesMed(WCurSess).Nusequ;
           INT_CDFILE   = *ZEROS;
           INT_DETAIL   = WRegisto;
           INT_KEYFLD   = 'Header';
           INT_INT05901 = %DATE();
           INT_INT05902 = %DATE('9999-12-31');
           WRITE RINTP059;
           LST#SesMed(WCurSess).Nusequ += 1;
        ENDSR;

        //================================================================
        // Subrotina.: GetParms
        // objectivo.: Obter a Lista de Parâmetros
        //================================================================
        BEGSR GetParms;
           //--------------------------------------------------
           // Obter a Lista de Parâmetros do Mediador
           //--------------------------------------------------
           IF (LST#SesMed(WCurSess).LastParm > *ZEROS);
              WParms = LST#SesMed(WcurSess).Parms;
           ELSE;
              WParms = GetTemplate(CodMediador:TipInter);
              WIdxParm = 1;
              IF (NOT WParms(WidxParm).IsEmpty);
                 DOW (NOT WParms(WidxParm).IsEmpty);
                    WIdxParm += 1;
                 ENDDO;
                 LST#SesMed(WCurSess).Parms = WParms;
                 LST#SesMed(WCurSess).LastParm = WIdxParm - 1;
              ELSE;
                 EXSR GetParmFromStd;
                 IF (WCodErro = *BLANKS);
                    WIdxParm = 1;
                    DOW (NOT WParms(WidxParm).IsEmpty);
                       WIdxParm += 1;
                    ENDDO;

                    LST#SesMed(WCurSess).Parms = WParms;
                    LST#SesMed(WCurSess).LastParm = WIdxParm - 1;
                 ENDIF;
              ENDIF;
           ENDIF;
        ENDSR;

        //================================================================
        // Subrotina.: GetParmFromStd
        // objectivo.: Obter os Parâmetros a partir do Standard
        //================================================================
        BEGSR GetParmFromStd;
           //----------------------------------------------
           // Obter a Configuração Standard Guardada
           //----------------------------------------------
           WParms(1).IsEmpty = *ON;
           WIdxParm = 1;
           DOW (NOT LST#Standard(WIdxParm).IsEmpty);
              IF (LST#Standard(WIdxParm).TipInter = TipInter);
                 WParms = LST#Standard(WIdxParm).Parms;
                 LEAVESR;
              ENDIF;
              WIdxParm += 1;
           ENDDO;

           //----------------------------------------------
           // Obter a Configuração Standard Registada
           //----------------------------------------------
           WParms = GetTemplate(MED#Standard:TipInter);
           IF (NOT WParms(1).IsEmpty);
              WIdxParm = 1;
              DOW (NOT LST#Standard(WIdxParm).IsEmpty);
                 LEAVE;
                 WIdxParm += 1;
              ENDDO;
              IF (WIdxParm >= DIM#MaxParm);
                 WCodErro = 'INT0022';
              ELSE;
                 LST#Standard(WIdxParm).TipInter = TipInter;
                 LST#Standard(WIdxParm).Parms = WParms;
                 LST#Standard(WIdxParm).IsEmpty = *OFF;
                 WIdxParm += 1;
                 LST#Standard(WIdxParm).IsEmpty = *ON;
              ENDIF;
           ELSE;
              WCodErro = 'INT0021';
           ENDIF;
        ENDSR;
      /END-FREE
     P $AddIntRecMed   E

     P*==========================================================
     P* Processo..: $AddFldIntRec
     P* Objectivo.: Adicionar o Campo à lista de Campos
     P*==========================================================
     P $AddFldIntRec   B                   EXPORT
     D                 PI             7A
     D   CurSess                      3S 0 CONST
     D   Nome                        20A   CONST
     D   Valor                      100A   CONST
     D   Reset                         N   CONST OPTIONS(*NOPASS)
     D
     D*==> Campos de Trabalho
     D WCodErro        S              7A
     D WCurSess        S              3S 0
     D WIdxFld         S              3S 0
     D WTipOpera       S              1A
     D
     D*==> Definição de constantes
     D Reserv          C                   ';|'
     D Transf          C                   '#,'
     D OPR#Criar       C                   'C'
     D OPR#Alterar     C                   'A'
     D*==========================================================
      /FREE
        WCodErro = *BLANKS;
        WCurSess = CurSess;
        LST#SesMed(WCurSess).CodErro = *BLANKS;
        WTipOpera = OPR#Criar;

        //-----------------------------------------------------
        // Localizar o campo e actualizar ou inserir
        //-----------------------------------------------------
        IF (%PARMS() > 3);
           IF (Reset);
              LST#SesMed(WCurSess).LastField = *ZEROS;
           ENDIF;
        ELSE;
           WIdxFld = 1;
           DOW (NOT LST#SesMed(WCurSess).Fields(WIdxFld).IsEmpty);
               IF (LST#SesMed(WCurSess).Fields(WIdxFld).Name = Nome);
                  WTipOpera = OPR#Alterar;
                  LEAVE;
               ENDIF;
               WIdxFld += 1;
           ENDDO;
        ENDIF;

        //-----------------------------------------------------
        // Obter o novo indice para o campo
        //-----------------------------------------------------
        IF (WTipOpera = OPR#Criar);
           WIdxFld = LST#SesMed(WCurSess).LastField + 1;
           IF (WIdxFld > DIM#MaxField);
              WCodErro = 'INT0023';
              LST#SesMed(WCurSess).CodErro = WCoderro;
              RETURN WCodErro;
           ENDIF;
           LST#SesMed(WCurSess).Fields(WIdxFld).IsEmpty = *OFF;
           LST#SesMed(WCurSess).LastField = WIdxFld;
           LST#SesMed(WCurSess).Fields(WIdxFld+1).IsEmpty = *ON;
        ENDIF;

        //-----------------------------------------------------
        // Actualizar/Inserir a Informação do Campo
        //-----------------------------------------------------
        LST#SesMed(WCurSess).Fields(WIdxFld).Name = %TRIM(Nome);
        LST#SesMed(WCurSess).Fields(WIdxFld).Value = %TRIM(
                                              %XLATE(Reserv:Transf:Valor));

        RETURN WCodErro;
      /END-FREE
     P $AddFldIntRec   E

     P*==========================================================
     P* Processo..: $GetCodMedia
     P* Objectivo.: Obter o Código do Mediador
     P*==========================================================
     P $GetCodMedia    B                   EXPORT
     D                 PI             3S 0
     D   CodMedia                     9S 0 CONST
     D
     D*==> Campos de Trabalho
     D WCodMedAlfa     S              3A
     D WCodMedNum      S              3S 0
     D
     D*==> Definição de constantes
     D*==========================================================
      /FREE
        OPEN FINTP050;
        CHAIN (CodMedia) RINTP050;
        IF (%FOUND(FINTP050));
           WCodMedNum = PRM_ALIAS;
           WCodMedAlfa = %CHAR(PRM_ALIAS);
        ELSE;
           WCodMedNum = *ZEROS;
           WCodMedAlfa = *BLANKS;
        ENDIF;
        CLOSE FINTP050;
        RETURN WCodMedNum;
      /END-FREE
     P $GetCodMedia    E

     P*==========================================================
     P* Processo..: $CvtCode
     P* Objectivo.: Aplicar máscara de Conversão
     P*==========================================================
     P $CvtCode        B                   EXPORT
     D                 PI           100A
     D   FieldValue                 100A   CONST
     D   CvtCode                      2A   CONST
     D
     D*==> Campos de Trabalho
     D WFieldValue     S            100A
     D WFieldNum       S              8S 0
     D
     D*==> Definição de constantes
     D*==========================================================
      /FREE
         WFieldValue = FieldValue;
         IF (CvtCode <> *BLANKS);
            IF (CvtCode = 'D1');
               MONITOR;
                  WFieldNum   = %DEC(%TRIM(WFieldValue):6:0);
               // WFieldNum  += $CvtData(WFieldNum:'YMD':'DMYY');
                  WFieldValue = %CHAR(WFieldNum);

                  WFieldValue = %SUBST(WFieldValue:6:2) +     // Dia
                                %SUBST(WFieldValue:4:2) +     // Mes
                                '20'+%SUBST(WFieldValue:2:2); // Ano
               ON-ERROR;
                  WFieldValue = *ZEROS;
               ENDMON;
            ELSEIF (CvtCode = 'D2');
               MONITOR;
                  WFieldValue = %TRIM(WFieldValue);

                  WFieldValue = %SUBST(WFieldValue:1:2) +     // Dia
                                %SUBST(WFieldValue:3:2) +     // Mes
                                '20'+%SUBST(WFieldValue:5:2); // Ano
               ON-ERROR;
                  WFieldValue = *ZEROS;
               ENDMON;
            ENDIF;
         ENDIF;

         RETURN WFieldValue;
      /END-FREE
     P $CvtCode        E

     P*==========================================================
     P* Processo..: GetTemplate
     P* Objectivo.: Obter a Tempalte da Interface
     P*==========================================================
     P GetTemplate     B                   EXPORT
     D                 PI                  LIKEDS(LST#FldParm) DIM(DIM#MaxParm)
     D   CodMediador                  9S 0 CONST
     D   TipInterface                 3A   CONST
     D
     D*==> Campos de Trabalho
     D WCodErro        S              7A
     D WIdxParm        S              3S 0
     D WCurSess        S              3S 0
     D WCodInterface   S              3S 0
     D WLabelField     S             20A
     D WLabelDesc      S             20A
     D WCodField       S              3S 0
     D WDuplicate      S              1A
     D WLatChars       S              1A
     D WUpperChars     S              1A
     D WParms          DS                  LIKEDS(LST#FldParm) DIM(DIM#MaxParm)
     D*==========================================================
      /FREE
        WIdxParm = 1;
        WParms(WIdxParm).IsEmpty = *ON;
        SETLL (CodMediador) RINTP051;
        READE (CodMediador) RINTP051;
        DOW (NOT %EOF(FINTP051));
           IF (PRM_TPINTER = TipInterface) AND (PRM_CDESTAD = 'O');
              WCodInterface = PRM_CDINTER;
              CHAIN (WCodInterface) RINTP053;
              IF (NOT %FOUND(FINTP053));
                 WCodErro = 'INTXXXX';
                 LEAVE;
              ENDIF;

              WDuplicate = PRM_DUPLICATE;
              WLatChars  = PRM_LATCHARS;
              WUpperChars= PRM_UPPERCHR;
              SETLL (CodMediador:WCodInterface) RINTP052;
              READE (CodMediador:WCodInterface) RINTP052;
              DOW (NOT %EOF(FINTP052));
                 WCodField     = PRM_CDFIELD;
                 WLabelField   = PRM_LBLFIELD;
                 WLabelDesc    = PRM_LBLDESC;
                 CHAIN (WCodInterface:WCodField) RINTP054;
                 IF (NOT %FOUND(FINTP054));
                    WCodErro = 'INTXXXX';
                    LEAVE;
                 ENDIF;

                 WParms(WIdxParm).IsKey     = PRM_ISKEY;
                 WParms(WIdxParm).Duplicate = WDuplicate;
                 WParms(WIdxParm).LatChars  = WLatChars;
                 WParms(WIdxParm).UpperChars= WUpperChars;
                 WParms(WIdxParm).Name      = PRM_NOFIELD;
                 IF (WLabelField = *BLANKS);
                    WLabelField = PRM_LBLFIELD;
                 ENDIF;
                 IF (WLabelDesc = *BLANKS);
                    WLabelDesc = PRM_LBLDESC;
                 ENDIF;
                 WParms(WIdxParm).Label     = WLabelField;
                 WParms(WIdxParm).Descr     = WLabelDesc;
                 WParms(WIdxParm).CvtCode   = PRM_CVTCODE;
                 WParms(WIdxParm).NuTabl    = PRM_NUTABL;
                 WParms(WIdxParm).IsEmpty   = *OFF;
                 WIdxParm += 1;
                 WParms(WIdxParm).IsEmpty = *ON;
                 READE (CodMediador:WCodInterface) RINTP052;
              ENDDO;
              LEAVE;
           ENDIF;
           READE (CodMediador) RINTP051;
        ENDDO;
        RETURN WParms;
      /END-FREE
     P GetTemplate     E

     P*==========================================================
     P* Processo..: GetCodSess
     P* Objectivo.: Obter o Código da Sessão
     P*==========================================================
     P GetCodSess      B                   EXPORT
     D                 PI             3S 0
     D   CodMedia                     9S 0 CONST
     D   TipInter                     3A   CONST
     D
     D*==> Campos de Trabalho
     D WCodSessao      S              3S 0
     D WCurSess        S              3S 0
     D*==========================================================
      /FREE
        //------------------------------------------------------
        // Obter o Código de Sessão do Mediador/Interface
        //------------------------------------------------------
        WCodSessao = *ZEROS;
        WCurSess  = 1;
        DOW (NOT LST#SesMed(WCurSess).IsEmpty);
           IF (LST#SesMed(WCurSess).CodMedia = CodMedia) AND
             (LST#SesMed(WCurSess).TipInter = TipInter);
                WCodSessao  = WCurSess;
                LEAVE;
           ENDIF;
           WCurSess += 1;
        ENDDO;
        RETURN WCodSessao;
      /END-FREE
     P GetCodSess      E

     P*==========================================================
     P* Processo..: $GetRefMB
     P* Objectivo.: Obter a Referência MultiBanco
     P*==========================================================
     P $GetRefMB       B                   EXPORT
     D                 PI             9A
     D   NumRecibo                    9S 0 CONST
     D   TotalRecibo                 11S 2 CONST
     D
     D*==> Estrutura dos Campos
     D NUM#OPERA       DS                  QUALIFIED
     D   NumComp               1     20A   INZ(*ZEROS)
     D   Const                 1      5A   INZ('20122')
     D   Ano                   6      7A
     D   Sequenc               8     12A
     D   TotRec               13     20A
     D
     D*==> Campos de Trabalho
     D WNumRecAlfa     S              9A
     D WTotRecAlfa     S             11A
     D WResultado      S              9S 0
     D WDigit01        S              1S 0
     D WDigit02        S              2S 0
     D WIdx            S              2S 0
     D
     D*==> Definição de constantes
     D OPR#Multipl     C                   '51731789386245531550054934817627900+
     D                                     93003'
     D OPR#Div01       C                   97
     D OPR#Div02       C                   98
     D KTE#Addon       C                   20122
     D*==========================================================
      /FREE
       WNumRecAlfa = %TRIM(%EDITW(NumRecibo:'0         '));
       WTotRecAlfa = %TRIM(%EDITW(TotalRecibo:'0           '));

       NUM#OPERA.NumComp = *ZEROS;
       NUM#OPERA.Const   = '20122';
       NUM#OPERA.Ano     = %SUBST(WNumRecAlfa:1:2);
       NUM#OPERA.Sequenc = %SUBST(WNumRecAlfa:5:5);
       NUM#OPERA.TotRec  = %SUBST(WTotRecAlfa:4:8);

       WResultado = *ZEROS;
       WIdx = 1;
       DOW (WIdx <= 20);
          MONITOR;
             WDigit01 = %DEC(%SUBST(NUM#OPERA.NumComp:WIdx:1):1:0);
             WDigit02 = %DEC(%SUBST(OPR#Multipl:((Widx*2)-1):2):2:0);
          ON-ERROR;
             WDigit01 = *ZEROS;
             WDigit02 = *ZEROS;
          ENDMON;
          WResultado += WDigit01 * WDigit02;
          WIdx += 1;
       ENDDO;

       WResultado = %REM(WResultado:OPR#Div01);
       WResultado = OPR#Div02 - WResultado;
       MONITOR;
          WResultado = %DEC(%SUBST(NUM#OPERA.NumComp:6:7):7:0)*100
                        + WResultado;
       ON-ERROR;
          WResultado = *ZEROS;
       ENDMON;

       RETURN %CHAR(WResultado);
      /END-FREE
     P $GetRefMB       E
