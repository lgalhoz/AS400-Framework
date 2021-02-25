     H NOMAIN
     H*===========================================================
     H* Programa..: RINTO020
     H* Descrição.: Grupo de Api's para a geração dos Interfaces dos
     H*             Mediadores.
     H*
     H*===========================================================
     F*==> Informação sobre os Mediadores Alvo
     FFINTP051  IF   E           K DISK    PREFIX(PRM_) USROPN
     FFINTP052  IF   E           K DISK    PREFIX(PRM_) USROPN
     FFINTP058  UF A E           K DISK    PREFIX(INT_) USROPN
     FFINTP059  UF A E           K DISK    PREFIX(INT_) USROPN
     F*===========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SINTO010

     D*==> Obter a Template da Interface para o Mediador
     D GetTemplate     PR                  LIKEDS(LST#FldParm) DIM(100)
     D   CodMedia                     9S 0 CONST
     D   TipInter                     3A   CONST
     D
     D*==> Obter o Código de Sessão do Mediador
     D GetCodSess      PR             3S 0
     D   CodMedia                     9S 0 CONST
     D   TipInter                     3A   CONST
     D
     D*==> Sessões abertas para o Mediador
     D LST#SesMed      DS                  QUALIFIED DIM(10)
     D   CodMedia                     9S 0
     D   TipInter                     3A
     D   CodInter                     5S 0
     D   NuSequ                       5S 0
     D   QtdeRegis                    5S 0
     D   CodErro                      7A
     D   CodProcess                   5S 0
     D   Duplicate                    1A
     D   Fields                            LIKEDS(LST#Field) DIM(100)
     D   LastField                    3S 0
     D   Parms                             LIKEDS(LST#FldParm) DIM(100)
     D   LastParm                     3S 0
     D   IsEmpty                       N
     D
     D*==> Conteudo dos Campos
     D LST#Field       DS                  QUALIFIED
     D   Name                        10A
     D   Value                      100A
     D   IsEmpty                       N
     D
     D*==> Parametrização Standard para o Mediador
     D LST#Standard                        LIKEDS(LST#FldParm) DIM(100)
     D
     D*==> Parametrização do ficheiro parao Mediador
     D LST#FldParm     DS                  QUALIFIED
     D   IsKey                        1A
     D   Name                        10A
     D   Label                       20A
     D   Duplicate                    1A
     D   IsEmpty                       N
     D
     D*==> Definição das Constantes
     D STS#Aberto      C                   'OPN'
     D INT#Apolice     C                   'APL'
     D*===========================================================

     P*==========================================================
     P* Processo..: $StrIntMedia
     P* Objectivo.: Iniciar o Interface para um Mediador
     P*==========================================================
     P $StrIntMedia    B                   EXPORT
     D                 PI
     D   CodMedia                     9S 0 CONST
     D   TipInter                     3A   CONST
     D
     D*==> Campos de Trabalho
     D WCodInter       S              3S 0
     D WIdx            S              3S 0
     D WCurSess        S              3S 0
     D WSesInterf      S              3S 0
     D*==========================================================
      /FREE
        //------------------------------------------------------
        // Se o Interface não estiver aberto então abre
        //------------------------------------------------------
        WCurSess = GetCodSess(CodMedia:TipInter);
        IF (WCurSess = *ZEROS);
           WIdx      = 1;
           DOW (WIdx < 100);
              IF (LST#SesMed(Widx).IsEmpty);
                 WCurSess  = WIdx;
                 LEAVE;
              ENDIF;
              WIdx += 1;
           ENDDO;
        ENDIF;

        //------------------------------------------------------
        // Actualizar o registo para a nova sessão
        //------------------------------------------------------
        OPEN FINTP051;
        OPEN FINTP052;
        OPEN FINTP058;
        OPEN FINTP059;

        LST#SesMed(WCurSess).IsEmpty = *OFF;
        LST#SesMed(WCurSess).CodMedia  = CodMedia;
        LST#SesMed(WCurSess).TipInter  = TipInter;
        LST#SesMed(WCurSess).CodInter  = *ZEROS;
        LST#SesMed(WCurSess).Nusequ    = *ZEROS;
        LST#SesMed(WCurSess).QtdeRegis = *ZEROS;
        LST#SesMed(WCurSess).LastParm  = *ZEROS;

        WSesInterf = $SetFiles('FINTP058');
        $SetCriteria('(CDMEDIA = '''+%CHAR(CodMedia)+''') +
                       AND (TPINTER = '''+TipInter+''') AND +
                       (CDSTATUS = ''OPN'')');
        IF ($GetRecord(1) > *ZEROS);
           LST#SesMed(WCurSess).CodInter = $GetFieldNum('CDINTER');
           LST#SesMed(WCurSess).Nusequ = $GetNextKey('FINTP059':'NUSEQU':
                             '(CDINTER = '''+%CHAR(WCodInter)+''') AND +
                              (CDMEDIA = '''+%CHAR(CodMedia)+''')');
        ENDIF;
        $RmvDBSession(WSesInterf);
      /END-FREE
     P $StrIntMedia    E

     P*==========================================================
     P* Processo..: $EndIntMedia
     P* Objectivo.: Termianr o Interface do Mediador
     P*==========================================================
     P $EndIntMedia    B                   EXPORT
     D                 PI
     D   CodMedia                     9S 0 CONST
     D   TipInter                     3A   CONST
     D
     D*==> Campos de Trabalho
     D WIsTheLast      S               N
     D WCodInter       S              3S 0
     D WCurSess        S              3S 0
     D*==========================================================
      /FREE
        //------------------------------------------------------
        // Localizar a Sessão
        //------------------------------------------------------
        WIsTheLast = *ON;
        WCurSess = GetCodSess(CodMedia:TipInter);
        IF (WCurSess = *ZEROS);
           LST#SesMed(WCurSess).CodErro = 'INTXXX0';
           RETURN;
        ENDIF;

        //------------------------------------------------------
        // Actualizar o registo da Sessão
        //------------------------------------------------------
        LST#SesMed(WCurSess).CodErro = *BLANKS;
        IF (LST#SesMed(WCurSess).QtdeRegis > *ZEROS);
           WCodInter = LST#SesMed(WCurSess).CodInter;
           IF (WcodInter > *ZEROS);
              CHAIN (CodMedia:WCodInter) RINTP058;
              IF (%FOUND(FINTP058));
                 INT_QTDREGIS += LST#SesMed(WCurSess).QtdeRegis;
                 INT_INT05802 = %DATE('9999-12-31');
                 UPDATE RINTP058 %FIELDS(INT_QTDREGIS:INT_INT05802);
              ELSE;
                 LST#SesMed(WCurSess).CodErro = 'INTXXX1';
              ENDIF;
           ELSE;
              LST#SesMed(WCurSess).CodErro = 'INTXXX2';
           ENDIF;
        ENDIF;

        //-------------------------------------------------------
        // Libertar a Sessão
        //-------------------------------------------------------
        LST#SesMed(WCurSess).IsEmpty = *ON;
      /END-FREE
     P $EndIntMedia    E

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
           RETURN 'INTXXX3';
        ENDIF;
        RETURN LST#SesMed(WCurSess).CodErro;
      /END-FREE
     P $GetIntMedErro  E

     P*==========================================================
     P* Processo..: $AddIntRecMed
     P* Objectivo.: Adicionar um registo ao Mediador
     P*==========================================================
     P $AddIntRecMed   B                   EXPORT
     D                 PI
     D   CodMediador                  9S 0 CONST
     D   TipInter                     3A   CONST
     D
     D*==> Campos de Trabalho
     D WParms          DS                  LIKEDS(LST#FldParm) DIM(100)
     D WFields         DS                  LIKEDS(LST#Field) DIM(100)
     D WIdx            S              3S 0
     D WCurSess        S              3S 0
     D WIdxParm        S              3S 0
     D WIdxFld         S              3S 0
     D WRegisto        S            500A
     D WKeyField       S            100A
     D WIsToWrite      S               N
     D WNuSqBak        S              5S 0
     D*==========================================================
      /FREE
        //-----------------------------------------------
        // Localizar a sessão para o Mediador
        //-----------------------------------------------
        WCurSess = GetCodSess(CodMediador:TipInter);
        IF (WCurSess = *ZEROS);
           RETURN;
        ENDIF;

        //-----------------------------------------------
        // Abrir Configuração Standar do Ficheiro
        //-----------------------------------------------
        IF (LST#SesMed(WCurSess).CodInter = *ZEROS);
        ENDIF;

        //-----------------------------------------------
        // Criar Novo Ficheiro de Interface
        //-----------------------------------------------
        WIdx = 1;
        IF (LST#SesMed(WCurSess).LastParm = *ZEROS);
        // Obter Configuração
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
        RETURN;

        //================================================================
        // Subrotina.: FormatRecord
        // objectivo.: ForCompr o registo do Interface
        //================================================================
        BEGSR FormatRecord;
           WIdxParm = 1;
           WRegisto = *BLANKS;
           WKeyField = *BLANKS;
           WParms      = LST#SesMed(WCurSess).Parms;
           WFields     = LST#SesMed(WCurSess).Fields;
           DOW (NOT WParms(WidxParm).IsEmpty);
              WIdxFld = 1;
              DOW (NOT WFields(WIdxFld).IsEmpty);
                 IF (%TRIM(WFields(WIdxFld).Name) =
                                            %TRIM(WParms(WIdxParm).Name));
                   WRegisto = %TRIM(WRegisto)+%TRIM(WFields(WidxFld).Value)+';';
                    IF (WParms(WIdxParm).IsKey = 'S');
                     WKeyField=%TRIM(WKeyField)+%TRIM(WFields(WidxFld).Value);
                    ENDIF;
                    LEAVE;
                 ENDIF;
                 WIdxFld += 1;
              ENDDO;
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
           INT_TPINTER  = INT#Apolice;
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
              WRegisto = %TRIM(WRegisto)+%TRIM(WParms(WIdxParm).Label)+';';
              WIdxParm += 1;
           ENDDO;
           INT_CDMEDIA  = LST#SesMed(WCurSess).CodMedia;
           INT_CDINTER  = LST#SesMed(WCurSess).CodInter;
           INT_NUSEQU   = LST#SesMed(WCurSess).Nusequ;
           INT_DETAIL   = WRegisto;
           INT_KEYFLD   = 'Header';
           INT_INT05901 = %DATE();
           INT_INT05902 = %DATE('9999-12-31');
           WRITE RINTP059;
           LST#SesMed(WCurSess).Nusequ += 1;
        ENDSR;
      /END-FREE
     P $AddIntRecMed   E

     P*==========================================================
     P* Processo..: $AddFldIntRec
     P* Objectivo.: Adicionar o Campo à lista de Campos
     P*==========================================================
     P $AddFldIntRec   B                   EXPORT
     D                 PI
     D   CodMedia                     9S 0 CONST
     D   TipInter                     3A   CONST
     D   Nome                        20A   CONST
     D   Valor                      100A   CONST
     D   Reset                         N   CONST OPTIONS(*NOPASS)
     D
     D*==> Campos de Trabalho
     D WIdx            S              3S 0
     D WCurSess        S              3S 0
     D WIdxFld         S              3S 0
     D*==========================================================
      /FREE
        WCurSess = GetCodSess(CodMedia:TipInter);
        IF (WCurSess = *ZEROS);
           RETURN;
        ENDIF;

        IF (%PARMS() > 2);
           IF (Reset);
              WIdxFld = 1;
           ELSE;
              WidxFld = LST#SesMed(WCurSess).LastField + 1;
           ENDIF;
        ENDIF;

        LST#SesMed(WCurSess).Fields(WIdxFld).Name = %TRIM(Nome);
        LST#SesMed(WCurSess).Fields(WIdxFld).Value = %TRIM(Valor);
        LST#SesMed(WCurSess).Fields(WIdxFld).IsEmpty = *OFF;
        WIdxFld += 1;
        LST#SesMed(WCurSess).Fields(WIdxFld).IsEmpty = *ON;
        LST#SesMed(WCurSess).LastField = WIdxFld;
      /END-FREE
     P $AddFldIntRec   E

     P*==========================================================
     P* Processo..: GetTemplate
     P* Objectivo.: Obter a Tempalte da Interface
     P*==========================================================
     P GetTemplate     B                   EXPORT
     D                 PI                  LIKEDS(LST#FldParm) DIM(100)
     D   CodMediador                  9S 0 CONST
     D   TipInterface                 3A   CONST
     D
     D*==> Campos de Trabalho
     D WIdx            S              3S 0
     D WIdxParm        S              3S 0
     D WCurSess        S              3S 0
     D WCodInterface   S              3S 0
     D WDuplicate      S              1A
     D WParms          DS                  LIKEDS(LST#FldParm) DIM(100)
     D*==========================================================
      /FREE
        WIdxParm = 1;
        WParms(WIdxParm).IsEmpty = *ON;
        SETLL (CodMediador) RINTP051;
        READE (CodMediador) RINTP051;
        DOW (NOT %EOF(FINTP051));
           IF (PRM_TPINTER = TipInterface) AND (PRM_CDESTAD = 'O')
             AND (PRM_QTDFIELD > *ZEROS);
              WDuplicate = PRM_DUPLICATE;
              WCodInterface = PRM_CDINTER;
              SETLL (CodMediador:WCodInterface) RINTP052;
              READE (CodMediador:WCodInterface) RINTP052;
               DOW (NOT %EOF(FINTP052));
                 WParms(WIdxParm).IsKey     = PRM_ISKEY;
                 WParms(WIdxParm).Duplicate = WDuplicate;
                 WParms(WIdxParm).Name      = PRM_NOFIELD;
                 WParms(WIdxParm).Label     = PRM_LBLFIELD;
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
     D WIdx            S              3S 0
     D*==========================================================
      /FREE
        //------------------------------------------------------
        // Obter o Código de Sessão do Mediador/Interface
        //------------------------------------------------------
        WCodSessao = *ZEROS;
        WIdx      = 1;
        DOW (NOT LST#SesMed(WIdx).IsEmpty);
           IF (LST#SesMed(WIdx).CodMedia = CodMedia) AND
             (LST#SesMed(WIdx).TipInter = TipInter);
                WCodSessao  = WIdx;
                LEAVE;
           ENDIF;
           WIdx += 1;
        ENDDO;
        RETURN WCodSessao;
      /END-FREE
     P GetCodSess      E
