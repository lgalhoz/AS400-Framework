     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*===========================================================
     H* Programa..: RINTO050
     H* Descrição.: Gerar interfaces para os Mediadores, para o efeito
     H*             Localizar os ficheiros temporários gerados nos
     H*             Sistema, e por cada novo que localizar, procede
     H*             à extracção dos registos.
     H*
     H*===========================================================
     F*==> Temporários a Processar
     FBASETMPA  IF   F  700     9AIDISK    KEYLOC(1) EXTFILE(WFileDia) USROPN
     FBASETMPO  IF   F  700     9AIDISK    KEYLOC(1) EXTFILE(WFileDia) USROPN
     FBASETMPT  IF   F  435     9AIDISK    KEYLOC(1) EXTFILE(WFileDia) USROPN
     F
     F*==> Informação sobre os Mediadores Alvo
     FFINTP051  IF   E           K DISK    PREFIX(PRM_)
     FFINTP052  IF   E           K DISK    PREFIX(PRM_)
     FFINTP058  UF A E           K DISK    PREFIX(INT_)
     FFINTP059  UF A E           K DISK    PREFIX(INT_)
     F*===========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010

     D*==> Actualizar Lista de Campos
     D ActLstField     PR
     D   Nome                        20A   CONST
     D   Valor                      100A   CONST
     D   Reset                         N   CONST OPTIONS(*NOPASS)
     D
     D*==> Formato de Registo da Apólices
     D R#DIO         E DS                  EXTNAME(GBASEDIO) QUALIFIED
     D R#DIA         E DS                  EXTNAME(GBASEDIA) QUALIFIED
     D R#DIT         E DS                  EXTNAME(GBASEDIT) QUALIFIED
     D
     D*==> Conteudo dos Campos
     D LST#Field       DS                  QUALIFIED DIM(100)
     D   Name                        10A
     D   Value                      100A
     D   IsEmpty                       N
     D WIdxFld         S              3S 0
     D
     D*==> Parametrização do ficheiro parao Mediador
     D LST#FldParm     DS                  QUALIFIED DIM(100)
     D   IsKey                        1A
     D   Name                        10A
     D   Label                       20A
     D   IsEmpty                       N
     D WIdxParm        S              3S 0
     D LST#StdParm     DS                  LIKEDS(LST#FldParm) DIM(100)
     D
     D*==> Campos de Trabalho
     D WIdx            S              3S 0
     D WCodProcess     S              5S 0
     D WSesMediad      S              5S 0
     D WSesInterf      S              5S 0
     D WFileDia        S             21A
     D WTipFile        S              2A
     D WDatFile        S              8S 0
     D WCodErro        S              7A
     D WCodMediador    S              9S 0
     D WCodMedParms    S              9S 0
     D WCodInterface   S              3S 0
     D WHaveParms      S               N
     D WDuplicate      S              1A
     D WIsToWrite      S               N
     D WNuSequBak      S              5S 0
     D WKeyField       S            100A
     D WAlias          S              3S 0
     D WCodInter       S              5S 0
     D WNusequ         S              5S 0
     D WHeadIsToCrt    S               N
     D WQtdeRegis      S              5S 0
     D WRegisto        S            500A
     D
     D WJMM1           S              3S 0
     D WJMM2           S              3S 0
     D WMED1           S              3S 0
     D
     D*==> Declaração de Constantes
     D TIP#Automovel   C                   'AU'
     D TIP#Outros      C                   'OR'
     D TIP#Transport   C                   'TR'
     D INT#Apolice     C                   'APL'
     D INT#Recibos     C                   'REC'
     D INT#Segurado    C                   'SEG'
     D INT#Apolices    C                   'APL'
     D STS#Aberto      C                   'OPN'
     D STS#Fechado     C                   'CLO'
     D MED#Standard    C                   1
     D*===========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCodProcess
     C                   PARM                    WFileDia
     C                   PARM                    WTipFile
     C                   PARM                    WDatFile
     C                   PARM                    WCodErro
      /FREE
        //-----------------------------------------------------------------
        // Carregar a Parametrização Standard
        //-----------------------------------------------------------------
        WCodMedParms = MED#Standard;
        EXSR OpenParms;
        IF (NOT WHaveParms);
           RETURN;
        ENDIF;

        //-----------------------------------------------------------------
        // Seleccionar os Mediadores com instruções para gerar Interfaces
        //-----------------------------------------------------------------
        WIdx = 1;
        WSesMediad = $SetFiles('FINTP050');
        $SetCriteria('(ISAPOLIC = ''S'') AND +
                      (DTFIMSPN <= '''+%CHAR(WDatFile)+''') +
                      AND (CDMEDIA <> 1)');
        DOW ($GetRecord(WIdx) > *ZEROS);
           EXSR OpenHeader;
           WHaveParms = *OFF;
           WCodMedParms = WCodMediador;
           EXSR OpenParms;
           EXSR ReadTemporario;
           EXSR UpdHeader;
           WIdx += 1;
        ENDDO;
        $RmvDBSession(WSesMediad);

        RETURN;

        //================================================================
        // Subrotina.: ReadTemporario
        // objectivo.: Ler o ficheiro Temporário para identificar registos
        //================================================================
        BEGSR ReadTemporario;
           WIdxFld = 1;
           LST#Field(WIdxFld).IsEmpty = *ON;
           IF (WTipFile = TIP#Automovel);
              OPEN BASETMPA;
              EXSR ReadAutomovel;
              CLOSE BASETMPA;
           ELSEIF (WTipFile = TIP#Outros);
              OPEN BASETMPO;
              EXSR ReadOutros;
              CLOSE BASETMPO;
           ELSEIF (WTipFile = TIP#Transport);
              OPEN BASETMPT;
              EXSR ReadTransport;
              CLOSE BASETMPT;
           ENDIF;
        ENDSR;

        //================================================================
        // Subrotina.: ReadAutomovel
        // objectivo.: Ler Informação especifica do Ramos Automovel
        //================================================================
        BEGSR ReadAutomovel;
           READ BASETMPA R#DIA;
           DOW (NOT %EOF(BASETMPA));
              WMED1 = R#DIA.MED1;
              WJMM1 = R#DIA.JMM1;
              WJMM2 = R#DIA.JMM2;

              ActLstField('RAMO'  :%CHAR(R#DIA.RAMO):*ON);
              ActLstField('APOL'  :%CHAR(R#DIA.APOL));
              ActLstField('NSEG'  :%CHAR(R#DIA.NSEG));
              ActLstField('SEG'   :R#DIA.SEG);
              EXSR AddRecord;
              READ BASETMPA R#DIA;
           ENDDO;
        ENDSR;

        //================================================================
        // Subrotina.: ReadOutros
        // objectivo.: Ler Informação Específica dos Outros Ramos
        //================================================================
        BEGSR ReadOutros;
           READ BASETMPO R#DIO;
           DOW (NOT %EOF(BASETMPO));
              WMED1 = R#DIO.MED1;
              WJMM1 = R#DIO.JMM1;
              WJMM2 = R#DIO.JMM2;

              ActLstField('RAMO'  :%CHAR(R#DIO.RAMO):*ON);
              ActLstField('APOL'  :%CHAR(R#DIO.APOL));
              ActLstField('NSEG'  :%CHAR(R#DIO.NSEG));
              ActLstField('SEG'   :R#DIO.SEG);
              EXSR AddRecord;
              READ BASETMPO R#DIO;
           ENDDO;
        ENDSR;

        //================================================================
        // Subrotina.: ReadTransport
        // objectivo.: Ler Informação Especifica do Ramo de Transportes
        //================================================================
        BEGSR ReadTransport;
           READ BASETMPT R#DIT;
           DOW (NOT %EOF(BASETMPT));
              WMED1 = R#DIT.MED1;
              WJMM1 = R#DIT.JMM1;
              WJMM2 = R#DIT.JMM2;

              ActLstField('RAMO'  :%CHAR(R#DIT.RAMO):*ON);
              ActLstField('APOL'  :%CHAR(R#DIT.APOL));
              ActLstField('NSEG'  :%CHAR(R#DIT.NSEG));
              ActLstField('SEG'   :R#DIT.SEG);
              EXSR AddRecord;
              READ BASETMPT R#DIT;
           ENDDO;
        ENDSR;

        //================================================================
        // Subrotina.: AddRecord
        // objectivo.: Adicionar o registo
        //================================================================
        BEGSR AddRecord;
           IF (WJMM1 <> WAlias) AND (WMED1 <> WAlias) AND (WJMM2 <> WAlias);
              LEAVESR;
           ENDIF;

           IF (WHeadIsToCrt);
              EXSR AddHeader;
           ENDIF;

           //----------------------------------------------
           // Escrever o Detalhe do registo
           //----------------------------------------------
           EXSR FormatRecord;
           WIsToWrite = *ON;
           IF (WDuplicate = 'N');
              SETLL (WCodMediador:WCodInter) RINTP059;
              READE(N) (WCodMediador:WCodInter) RINTP059;
              DOW (NOT %EOF(FINTP059));
                 IF (%TRIM(INT_KEYFLD) = %TRIM(WKeyField)) AND
                  (%TRIM(INT_KEYFLD) <> *BLANKS);
                    WIsToWrite = *OFF;
                    WNuSequBak = INT_NUSEQU;
                    LEAVE;
                 ENDIF;
                 READE(N) (WCodMediador:WCodInter) RINTP059;
              ENDDO;
           ENDIF;

           IF (WIsToWrite);
              INT_CDMEDIA  = WCodMediador;
              INT_CDINTER  = WCodInter;
              INT_NUSEQU   = WNuSequ;
              INT_DETAIL   = WRegisto;
              INT_KEYFLD   = WKeyField;
              INT_INT05901 = %DATE();
              INT_INT05902 = %DATE('9999-12-31');
              WRITE RINTP059;
              WNuSequ += 1;
              WQtdeRegis += 1;
           ELSE;
              CHAIN (WCodMediador:WCodInter:WNusequBak) RINTP059;
              INT_DETAIL = WRegisto;
              UPDATE RINTP059 %FIELDS(INT_DETAIL);
           ENDIF;

        ENDSR;

        //================================================================
        // Subrotina.: FormatRecord
        // objectivo.: ForCompr o registo do Interface
        //================================================================
        BEGSR FormatRecord;
           WIdxParm = 1;
           WRegisto = *BLANKS;
           WKeyField = *BLANKS;
           DOW (NOT LST#FldParm(WIdxParm).IsEmpty);
              WIdxFld = 1;
              DOW (NOT LST#Field(WIdxFld).IsEmpty);
                 IF (%TRIM(LST#Field(WIdxFld).Name) =
                                          %TRIM(LST#FldParm(WIdxParm).Name));
                 WRegisto = %TRIM(WRegisto)+%TRIM(LST#Field(WidxFld).Value)+';';
                    IF (LST#FldParm(WIdxParm).IsKey = 'S');
                     WKeyField=%TRIM(WKeyField)+%TRIM(LST#Field(WidxFld).Value);
                    ENDIF;
                    LEAVE;
                 ENDIF;
                 WIdxFld += 1;
              ENDDO;
              WIdxParm += 1;
           ENDDO;
        ENDSR;

        //================================================================
        // Subrotina.: OpenParms
        // objectivo.: Abrir a Parametrização para o Interface
        //================================================================
        BEGSR OpenParms;
           WHaveParms = *OFF;
           WIdxParm = 1;
           LST#FldParm(WIdxParm).IsEmpty = *ON;
           SETLL (WCodMedParms) RINTP051;
           READE (WCodMedParms) RINTP051;
           DOW (NOT %EOF(FINTP051));
              IF (PRM_TPINTER = INT#Apolice) AND (PRM_CDESTAD = 'O')
                AND (PRM_QTDFIELD > *ZEROS);
                 WDuplicate = PRM_DUPLICATE;
                 WHaveParms = *ON;
                 WCodInterface = PRM_CDINTER;
                 SETLL (WCodMedParms:WCodInterface) RINTP052;
                 READE (WCodMedParms:WCodInterface) RINTP052;
                 DOW (NOT %EOF(FINTP052));
                    LST#FldParm(WIdxParm).IsKey   = PRM_ISKEY;
                    LST#FldParm(WIdxParm).Name    = PRM_NOFIELD;
                    LST#FldParm(WIdxParm).Label   = PRM_LBLFIELD;
                    LST#FldParm(WIdxParm).IsEmpty = *OFF;
                    WIdxParm += 1;
                    LST#FldParm(WIdxParm).IsEmpty = *ON;
                    READE (WCodMedParms:WCodInterface) RINTP052;
                 ENDDO;
                 LEAVE;
              ENDIF;
              READE (WCodMedParms) RINTP051;
           ENDDO;

           //-----------------------------------------------------
           // Actualizar a lista de parametros (Standard)
           //-----------------------------------------------------
           IF (NOT WHaveParms) AND (WCodMedParms <> MED#Standard);
              LST#FldParm = LST#StdParm;
           ELSEIF (WhaveParms AND WCodMedParms = MED#Standard);
              LST#StdParm = LST#FldParm;
           ENDIF;
        ENDSR;

        //================================================================
        // Subrotina.: OpenHeader
        // objectivo.: Abrir o cabeçalho do ficheiro de Interface
        //================================================================
        BEGSR OpenHeader;
           WCodMediador = $GetFieldNum('CDMEDIA');
           WAlias       = $GetFieldNum('ALIAS');
           WSesInterf = $SetFiles('FINTP058');
           $SetCriteria('(CDMEDIA = '''+%CHAR(WCodMediador)+''') +
                         AND (TPINTER = '''+INT#Apolice+''') AND +
                         (CDSTATUS = ''OPN'')');
           IF ($GetRecord(1) > *ZEROS);
              WHeadIsToCrt = *OFF;
              WCodInter = $GetFieldNum('CDINTER');
              WNuSequ = $GetNextKey('FINTP059':'NUSEQU':
                                '(CDINTER = '''+%CHAR(WCodInter)+''') AND +
                                 (CDMEDIA = '''+%CHAR(WCodMediador)+''')');
           ELSE;
              WHeadIsToCrt = *ON;
              WCodInter = $GetNextKey('FINTP059':'CDINTER':
                                '(CDMEDIA = '''+%CHAR(WCodMediador)+''')');
              WNuSequ = 1;
           ENDIF;
           $RmvDBSession(WSesInterf);
           WQtdeRegis = *ZEROS;
        ENDSR;

        //================================================================
        // Subrotina.: AddHeader
        // objectivo.: Adicionar o Cabeçalho
        //================================================================
        BEGSR AddHeader;
           WHeadIsToCrt = *OFF;
           INT_CDMEDIA  = WCodMediador;
           INT_CDINTER  = WCodInter;
           INT_NMINTER  = *BLANKS;
           INT_TPINTER  = INT#Apolice;
           INT_CDPROCE  = WCodProcess;
           INT_DTPRODUC = $DateToInt(%DATE());
           INT_DTENVIO  = *ZEROS;
           INT_CDSTATUS = STS#Aberto;
           INT_QTDREGIS = *ZEROS;
           INT_INT05801 = %DATE();
           INT_INT05802 = %DATE('9999-12-31');
           WRITE RINTP058;

           //----------------------------------------------
           // Escrever o Header do registo
           //----------------------------------------------
           WRegisto = *BLANKS;
           WIdxParm = 1;
           DOW (NOT LST#FldParm(WIdxParm).IsEmpty);
              WRegisto = %TRIM(WRegisto)+%TRIM(LST#FldParm(WIdxParm).Label)+';';
              WIdxParm += 1;
           ENDDO;
           INT_CDMEDIA  = WCodMediador;
           INT_CDINTER  = WCodInter;
           INT_NUSEQU   = WNusequ;
           INT_DETAIL   = WRegisto;
           INT_KEYFLD   = 'Header';
           INT_INT05901 = %DATE();
           INT_INT05902 = %DATE('9999-12-31');
           WRITE RINTP059;
           WNuSequ += 1;
        ENDSR;

        //================================================================
        // Subrotina.: UpdHeader
        // objectivo.: Actualizar o Cabeçalho
        //================================================================
        BEGSR UpdHeader;
           IF (WQtdeRegis > *ZEROS);
              CHAIN (WCodMediador:WCodInter) RINTP058;
              IF (%FOUND(FINTP058));
                 INT_QTDREGIS += WQtdeRegis;
                 INT_INT05802 = %DATE('9999-12-31');
                 UPDATE RINTP058 %FIELDS(INT_QTDREGIS:INT_INT05802);
              ELSE;
                 WCodErro = 'INTXXXX';
              ENDIF;
           ENDIF;
        ENDSR;
      /END-FREE

     P*==========================================================
     P* Processo..: ActLstField
     P* Objectivo.: Adicionar o Campo à lista de Campos
     P*==========================================================
     P ActLstField     B                   EXPORT
     D                 PI
     D   Nome                        20A   CONST
     D   Valor                      100A   CONST
     D   Reset                         N   CONST OPTIONS(*NOPASS)
     D
     D*==> Campos de Trabalho
     D*==========================================================
      /FREE
        IF (%PARMS() > 2);
           IF (Reset);
              WIdxFld = 1;
           ENDIF;
        ENDIF;

        LST#Field(WIdxFld).Name = %TRIM(Nome);
        LST#Field(WIdxFld).Value = %TRIM(Valor);
        LST#Field(WIdxFld).IsEmpty = *OFF;
        WIdxFld += 1;
        LST#Field(WIdxFld).IsEmpty = *ON;
      /END-FREE
     P ActLstField     E
