     H ALWNULL(*USRCTL)  BNDDIR('JMDIR':'JMINT') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*===========================================================
     H* Programa..: RINTO051
     H* Descri��o.: Gerar interfaces para os Mediadores, para o efeito
     H*             Localizar os ficheiros tempor�rios gerados nos
     H*             Sistema, e por cada novo que localizar, procede
     H*             � extrac��o dos registos.
     H*
     H*===========================================================
     F*==> Tempor�rios a Processar
     FGCOBGR    IF   E           K DISK    PREFIX(REC_)
     FBASETMPR  IF   F  550     9AIDISK    KEYLOC(1) EXTFILE(WFileRec) USROPN
     F*===========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SINTO010

     D*==> Formato de Registo da Ap�lices
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
     D   JMM1                503    505S 0                                      Mediador C/Cobran�a
     D   JMC1                506    508P 3                                      % Comiss�o C/Cobran�a
     D   JMM2                509    511S 0                                      Mediador Sem Cobran�a
     D   JMC2                512    514P 3                                      % Commiss�o S/Cobran�a
     D   MES1                531    532S 0
     D   MMMOV               533    534S 0
     D   AAMOV               535    536S 0
     D   NP                  537    538S 0
     D   ASTER               542    542A                                        Recibo Impresso ?
     D   CPMOR               543    546P 0                                      C�digo Postal da Morada
     D   CPLCB1              547    550P 0                                      C�digo Postal Local de Cobran�a
     D
     D*==> Campos de Trabalho
     D WCodProcess     S              5S 0
     D WCodFile        S              5S 0
     D WFileRec        S             21A
     D WTipFile        S              3A
     D WDatFile        S              8S 0
     D WCodErro        S              7A
     D
     D WIdx            S              3S 0
     D WAlias          S              3S 0
     D WSesMediad      S              5S 0
     D WSesInterf      S              5S 0
     D WCodMedia       S              9S 0
     D WTipInter       S              3A
     D WCurSess        S              3S 0
     D
     D*==> Declara��o de Constantes
     D INT#Recibo      C                   'REC'
     D STS#Aberto      C                   'OPN'
     D STS#Fechado     C                   'CLO'
     D MED#Standard    C                   1
     D*===========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCodProcess
     C                   PARM                    WCodFile
     C                   PARM                    WFileRec
     C                   PARM                    WTipFile
     C                   PARM                    WDatFile
     C                   PARM                    WCodErro
      /FREE
        //-----------------------------------------------------------------
        // Seleccionar os Mediadores com instru��es para gerar Interfaces
        //-----------------------------------------------------------------
        WIdx = 1;
        WSesMediad = $SetFiles('FINTP050');
        $SetCriteria('(ISAPOLIC = ''S'') AND +
                      (DTFIMSPN <= '''+%CHAR(WDatFile)+''') +
                      AND (CDMEDIA <> 1)');
        DOW ($GetRecord(WIdx) > *ZEROS);
           WCodErro  = *BLANKS;
           WCodMedia = $GetFieldNum('CDMEDIA');
           WAlias    = $GetFieldNum('ALIAS');
           WTipInter = INT#Recibo;
           WCodErro  = $StrIntMedia(WCodMedia:WCodProcess:WCodFile:WTipInter);
           IF (WCodErro = *BLANKS);
              EXSR ReadTemporario;
              IF (WCodErro = *BLANKS);
                 WCodErro = $EndIntMedia(WCodMedia:WTipInter);
              ENDIF;
           ENDIF;
           WIdx += 1;
        ENDDO;
        $RmvDBSession(WSesMediad);

        RETURN;

        //================================================================
        // Subrotina.: ReadTemporario
        // objectivo.: Ler o ficheiro Tempor�rio para identificar registos
        //================================================================
        BEGSR ReadTemporario;
           OPEN BASETMPR;
           EXSR ReadRecibo;
           CLOSE BASETMPR;
        ENDSR;

        //================================================================
        // Subrotina.: ReadRecibo
        // objectivo.: Ler Informa��o especifica do Recibos
        //================================================================
        BEGSR ReadRecibo;
           READ BASETMPR R#DIR;
           DOW (NOT %EOF(BASETMPR));
              IF (WAlias = R#DIR.MED1) OR (WAlias = R#DIR.JMM1) OR
                (WAlias = R#DIR.JMM2);
                 CHAIN (R#DIR.NREC) GCOBGR;

                 WCurSess = $GetIntMedia(WCodMedia:WTipInter);

                 $AddFldIntRec(WCurSess:'GCTIPR':%CHAR(R#DIR.RAMO):*ON);  // Tipo de Recibo
                 $AddFldIntRec(WCurSess:'GCRAMO':%CHAR(R#DIR.RAMO));      // C�digo de Ramo
                 $AddFldIntRec(WCurSess:'GCAPOL':%CHAR(R#DIR.APOL));      // N�mero de Ap�lice
                 $AddFldIntRec(WCurSess:'GCNREC':%CHAR(R#DIR.NREC));      // N�mero de Recibo
                 $AddFldIntRec(WCurSess:'GCNOM ':R#DIR.NOMSEG);           // Nome do Segurado
                 $AddFldIntRec(WCurSess:'GCDLIM':R#DIR.NOMSEG);           // Data de Vencimento
                 $AddFldIntRec(WCurSess:'GCDFIM':R#DIR.NOMSEG);           // Data de Fim de Per�odo
                 $AddFldIntRec(WCurSess:'GCDPAG':R#DIR.NOMSEG);           // Estado do Recibo
                 $AddFldIntRec(WCurSess:'GCDEMI':R#DIR.NOMSEG);           // Data de Emiss�o
                 $AddFldIntRec(WCurSess:'GCPREM':R#DIR.NOMSEG);           // Pr�mio Comercial
                 $AddFldIntRec(WCurSess:'GCTREC':R#DIR.NOMSEG);           // Valor total do Recibo
                 $AddFldIntRec(WCurSess:'GCCMED':R#DIR.NOMSEG);           // Valor total da Comiss�o

                 $AddIntRecMed(WCodMedia:WtipInter);
              ENDIF;
              READ BASETMPR R#DIR;
           ENDDO;
        ENDSR;
