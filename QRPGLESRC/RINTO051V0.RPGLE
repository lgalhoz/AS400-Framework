     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*===========================================================
     H* Programa..: RINTO051
     H* Descrição.: Gerar interfaces para os Mediadores, para o efeito
     H*             Localizar os ficheiros temporários gerados nos
     H*             Sistema, e por cada novo que localizar, procede
     H*             à extracção dos registos.
     H*
     H*===========================================================
     F*==> Temporários a Processar
     FBASETMPR  IF   F  550     9AIDISK    KEYLOC(1) EXTFILE(WFileRec) USROPN
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
     D R#DIR           DS                  QUALIFIED
     D   NREC                  1      9S 0
     D   RAMO                 10     11S 0
     D   APOL                 12     18S 0
     D   NOMSEG               19     58A
     D   MORADA               59     98A
     D   CPMOR2              102    121A
     D   LCB                 122    161A
     D   CPCLB2              162    181A
     D   NSEG                182    184P 0
     D   TREG                185    186S 0
     D   DTINIC              186    191S 0
     D   FRAC                192    192S 0
     D   DTFIMC              193    198S 0
     D   CAP                 199    204P 0
     D   CAPRC               205    207P 0
     D   PRMSIMP             208    212P 2
     D   BONUS               213    216P 0
     D   ENCARG              217    220P 0
     D   CUSTAA              221    222P 2
     D   CRC                 223    224P 0
     D   PJUR                225    225A
     D   CR1                 226    226S 0
     D   SELOS               227    230P 2
     D   SNA                 231    234P 2
     D   SNB                 235    238P 2
     D   TOTREC              239    243P 2
     D   ISEN                244    244S 0
     D   CONC                246    247P 0
     D   MED1                248    250S 0
     D   COM1                251    255P 2
     D   MED2                256    258P 0
     D   COM2                259    263P 0
     D   MARC1               264    277A
     D   MATR1               278    288A
     D   LRPS                315    359A
     D   LRPS4               315    318A
     D   VPART               370    370S 0
     D   AVIAG               371    371A
     D   VFRAC               372    372P 0
     D   PRBAG               380    386P 2
     D   NDIAS               398    400S 0
     D   ZONA                401    401S 0
     D   MTRAN               402    402S 0
     D   BAGAGE              407    409P 0
     D   CAPDPI              410    413P 0
     D   CAPDT               435    437P 0
     D   JMCOM1              465    468P 2
     D   JMCOM2              469    472P 2
     D   JMM1                503    505S 0                                      Mediador C/Cobrança
     D   JMC1                506    508P 3                                      % Comissão C/Cobrança
     D   JMM2                509    511S 0                                      Mediador Sem Cobrança
     D   JMC2                512    514P 3                                      % Commissão S/Cobrança
     D   MES1                531    532S 0
     D   MMMOV               533    534S 0
     D   AAMOV               535    536S 0
     D   NP                  537    538S 0
     D   ASTER               542    542A                                        Recibo Impresso ?
     D   CPMOR               543    546P 0                                      Código Postal da Morada
     D   CPLCB1              547    550P 0                                      Código Postal Local de Cobrança
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
     D WFileRec        S             21A
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
     D INT#Recibo      C                   'REC'
     D STS#Aberto      C                   'OPN'
     D STS#Fechado     C                   'CLO'
     D MED#Standard    C                   1
     D*===========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCodProcess
     C                   PARM                    WFileRec
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
           OPEN BASETMPR;
           EXSR ReadRecibo;
           CLOSE BASETMPR;
        ENDSR;

        //================================================================
        // Subrotina.: ReadRecibo
        // objectivo.: Ler Informação especifica do Recibos
        //================================================================
        BEGSR ReadRecibo;
           READ BASETMPR R#DIR;
           DOW (NOT %EOF(BASETMPR));
              WMED1 = R#DIR.MED1;
              WJMM1 = R#DIR.JMM1;
              WJMM2 = R#DIR.JMM2;

              ActLstField('RAMO'  :%CHAR(R#DIR.RAMO):*ON);
              ActLstField('NREC'  :%CHAR(R#DIR.NREC));
              ActLstField('APOL'  :%CHAR(R#DIR.APOL));
              ActLstField('NSEG'  :%CHAR(R#DIR.NSEG));
              ActLstField('NOMSEG':R#DIR.NOMSEG);
              EXSR AddRecord;
              READ BASETMPR R#DIR;
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
                 IF (%TRIM(INT_KEYFLD) = %TRIM(WKeyField));
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
        // objectivo.: Formatar o registo do Interface
        //================================================================
        BEGSR FormatRecord;
           WIdxParm  = 1;
           WRegisto  = *BLANKS;
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
           WIdxParm = 1;
           LST#FldParm(WIdxParm).IsEmpty = *ON;
           SETLL (WCodMedParms) RINTP051;
           READE (WCodMedParms) RINTP051;
           DOW (NOT %EOF(FINTP051));
              IF (PRM_TPINTER = INT#Recibo) AND (PRM_CDESTAD = 'O')
                AND (PRM_QTDFIELD > *ZEROS);
                 WDuplicate = PRM_DUPLICATE;
                 WHaveParms = *ON;
                 WCodInterface = PRM_CDINTER;
                 SETLL (WCodMedParms:WCodInterface) RINTP052;
                 READE (WCodMedParms:WCodInterface) RINTP052;
                 DOW (NOT %EOF(FINTP052));
                    LST#FldParm(WIdxParm).IsKey   = PRM_IsKey;
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
                         AND (TPINTER = '''+INT#Recibo+''') AND +
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
           INT_TPINTER  = INT#Recibo;
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
           INT_KEYFLD   = 'Header';
           INT_DETAIL   = WRegisto;
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
