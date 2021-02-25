     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H DECEDIT(',') DFTNAME(GP015) DATEDIT(*DMY.) FIXNBR(*ZONED) AUT(*ALL)
     H********************************************************************
     H*  Empresa....:  JOSE MATA, LDA                                    *
     H*  Programa...:  PPRDO012                                          *
     H*  Autor......:  Luis Galhoz                                       *
     H*  Data.......:  24/05/2012                                        *
     H*  Observação.:  Programa reestruturado partindo do original GP015 *
     H*                trabalho realizado num mês.                       *
     H********************************************************************
     H* Indicadores  Descrição
     H* -----------  --------------------------------------------
     H*      11      Ramos diferentes de 01, 21 e 31
     H*      12      Ramos diferentes de 25, 35 e 55
     H*      15      Apólices no Dia
     H*      16      Apólices no Base
     H*      19      Opção de Criação de Apólices
     H*      20      Opção de Consulta de Apólices
     H*      21      Opção de Alteração de Apólices
     H*      22      Opção de Alteração do Segurado da Apólice
     H*      95      Ramo 15 (Acidentes de Trabalho)
     H*      98      Ramo diferente do 15
     H********************************************************************
     FFPRDE012  CF   E             WORKSTN INFDS(INFD#KEYS)
     FGFSEG     IF   F  267     5AIDISK    KEYLOC(1) EXTFILE('GFSEG')
     FGBASEDIO  UF A F  700     9AIDISK    KEYLOC(1) EXTFILE('LGALHOZ/GBASEDIO')
     FGBASEGRO  IF   F  700     9AIDISK    KEYLOC(1) EXTFILE('GBASEGRO')
     FBASEGROT  IF   F  700     9AIDISK    KEYLOC(1) EXTFILE('BASEGROT')
     FBASEGRO   IF   F  700     9AIDISK    KEYLOC(1) EXTFILE('GBASEGRO')
     FFCONC     IF   F   30     3AIDISK    KEYLOC(1) EXTFILE('FCONC')
     FFMED      IF   F  130     4AIDISK    KEYLOC(1) EXTFILE('FMED')
     FMCORNP    IF   F  128     3AIDISK    KEYLOC(4) EXTFILE('SD.1.CR')
     FTABELAS   IF   F   64    12AIDISK    KEYLOC(2) EXTFILE('G.SINTAB')
     FMPOSTNP   IF   F   64     7AIDISK    KEYLOC(2) EXTFILE('SD.1.CP')
     FFTEXTOS   IF   F   46     6AIDISK    KEYLOC(1) EXTFILE('FTEXTOS')
     FGPSAT     IT   F    4        DISK    EXTFILE('MEDSAT')
     FREGAPOL   UF A F  100     9AIDISK    KEYLOC(3) EXTFILE('LGALHOZ/GREGAPOL')
     FREGAPO2   O  A F   23    23AIDISK    KEYLOC(1) EXTFILE('LGALHOZ/GREGAPO2')
     FENTIDADE  IF   F  400     7AIDISK    KEYLOC(1) EXTFILE('G.SINENT')        GS0004
     FFALTER    O  A F  223        DISK    EXTFILE('LGALHOZ/FALTER')
     D****************************************************
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SENTO010
      /Copy QRPGLESRC,SSECO010

     D TABRR           S              2  0 DIM(16) CTDATA PERRCD(1) ASCEND
     D TABPC           S             30    DIM(16) ALT(TABRR)
     D CL              S              2  0 DIM(6) ASCEND
     D TABMS           S              4  0 DIM(40) FROMFILE(GPSAT) PERRCD(1)
     D                                     ASCEND
     D********************************************************************
     D                UDS
     D  USER                   1      8
     D  #APOL                101    109  0
     D
     D*==> Controlo dos Atributos dos Campos no Ecrã
     D CtlScreen       DS                  QUALIFIED DIM(24)
     D   Nome                        10A   INZ(*BLANKS)
     D   ptrAtr                        *   INZ(*NULL)
     D   IsEmpty                      1A   INZ(*ON)
     D WAtributo       S              1A   BASED(ptrAtr)
     D
     D*==> Linhas de Texto
     D WECR#LINES      DS
     D   LINE01                      40A
     D   LINE02                      40A
     D   LINE03                      40A
     D   LINE04                      40A
     D
     D*==> Passagem dos valores entre programas
     D EXPORTDTA       DS
     D RBASEBK                 1    700A
     D CR1                   217    217  0
     D CAP1                  218    223P 0
     D TX1                   224    226P 4
     D TX1AT                 224    226P 3
     D PR1                   227    231P 2
     D CR2                   232    232  0
     D CAP2                  233    238P 0
     D TX2                   239    241P 4
     D PR2                   242    246P 2
     D CR3                   247    247  0
     D CAP3                  248    253P 0
     D TX3                   254    256P 4
     D PR3                   257    261P 2
     D CF01                  568    568
     D CF02                  569    569
     D CF03                  570    570
     D CF04                  571    571
     D CF05                  572    572
     D CF06                  573    573
     D CF07                  574    574
     D CF08                  575    575
     D CF09                  576    576
     D CF10                  577    577
     D CF11                  578    578
     D CF12                  579    579
     D CF13                  533    533
     D
     D*==> Declaração dos campos para ficheiros de descrição interna
     D                 DS
     D  RRAPOL                 1      9  0
     D  RR                     1      2  0
     D  APOL                   3      9  0
     D  WRR                    1      2
     D  WAPOL                  3      9
     D                 DS
     D  WMSG                   1     60
     D  WRM                   35     36
     D  WAP                   38     44
     D                 DS
     D  K23                    1     23
     D  TA                     1      1
     D  ANO                    2      3  0
     D  ARG                    4     14
     D  RRAP                  15     23
     D                 DS
     D  CPMOR1                 1      7  0
     D  CPOSM                  1      4  0
     D  ZPOSM                  5      7  0
     D                 DS
     D  CPLCB1                 1      7  0
     D  CPOSL                  1      4  0
     D  ZPOSL                  5      7  0
     D                 DS
     D  COMISS                 1     30                                         RAMO/APOLICE (CHAVE)
     D*---- COMISSÕES NORMAIS sem fenomenos sismicos
     D  CORR1                  1      5  3                                      RAMO/APOLICE (CHAVE)
     D  ANGA1                  6     10  3                                      RAMO/APOLICE (CHAVE)
     D  COBR1                 11     15  3                                      RAMO/APOLICE (CHAVE)
     D*---- COMISSÕES C/ FEN.SISM. TRAB.INDEP.                            RAMO/APOLICE (CHAVE)
     D  CORR2                 16     20  3                                      RAMO/APOLICE (CHAVE)
     D  ANGA2                 21     25  3                                      RAMO/APOLICE (CHAVE)
     D  COBR2                 26     30  3                                      RAMO/APOLICE (CHAVE)
     D                 DS
     D  MODALI                 1      2
     D  MOD                    1      1
     D  TPT                    2      2
     D                 DS
     D  CDCAE                  1      6
     D  CTLCAE                 5      6
     D                 DS
     D  KDTALT                 1      6  0
     D  DDALT                  1      2  0
     D  MMALT                  3      4  0
     D  KAAALT                 5      6  0
     D                 DS
     D  ACR                    1      3  0 DIM(3) ASCEND
     D
     D  SAVIN45        S               N
     D  WPtrData       S               *
     D  WIcErro        S               N
     D  WIcSair        S               N
     D  WCodErro       S              7A
     D  WFldName       S             15A
     D  WFmtName       S             10A
     D  WDatInicio     S              8S 0
     D  WModo          S               N
     D  WIsBase        S               N
     D  WAno           S              4S 0
     D  WRow           S              3P 0
     D  WCol           S              3P 0
     D  WADC           S             11S 0
     D  WNIB           S             21A
     D  WRecS          S              1A
     D  WCodGravid     S              3S 0
     D
     D*==> Opções do Menu
     D OPR#Inserir     C                   1
     D OPR#Mostrar     C                   2
     D OPR#Alterar     C                   3
     D OPR#AltNumSeg   C                   4
     I****************************************************
      /Copy QRPGLESRC,GP015DTI
     C*==================================================================
     C*---> Validar se o Base de Outros Ramos está fechado
+----C                   IF        ($CheckObject('GP015A':'LGALHOZ'))
|    C     'Está fechado'DSPLY
+----C                   ENDIF
     C
     C*---> Variáveis usadas dentro do programa
     C                   EVAL      WAno = %SUBDT(%DATE():*Years)
     C                   EVAL      WIcSair = *OFF
     C* ==============================================================
     C* Gerir as Opções de Menu
     C* ==============================================================
+----C                   DOW       (NOT WIcSair)
|    C                   Z-ADD     0             OPCAO
|    C                   Z-ADD     0             RRAPOL
|    C                   EXFMT     RPRDE01201                           92
|    C                   MOVE      *BLANKS       MENSER
|    C                   MOVE      'A'           INDREG
|    C                   SETOFF                                       1314      desinibir clausulas
|    C*--> Se seleccionou a opção Sair então sai
|+---C                   IF        (Func#Key = KEY#F07)
||   C                   LEAVE
|+---C                   ENDIF
|    C*
|    C*---> Inicializar Campos de Trabalho                                      opção CONSULTA
|    C                   MOVE      *BLANK        RA                              RG 17/04/91
|    C                   MOVE      *BLANK        NATUR                           PC 30/10/91
|    C                   MOVE      *BLANK        CLASS                           PC 30/10/91
|    C                   MOVE      *BLANK        PERDCQ                          PC 30/10/91
|    C                   MOVE      *BLANK        TPERD                           PC 30/10/91
|    C                   Z-ADD     0             PPERDI                          PC 30/10/91
|    C                   Z-ADD     0             ENCFIX                          PC 30/10/91
|    C                   Z-ADD     0             PINDMN                          PC 30/10/91
|    C                   Z-ADD     0             TXIND                           RG 12/03/97
|    C                   MOVE      *BLANK        INDEX                           RG 12/03/97
|    C                   MOVE      *BLANKS       MOD
|    C                   MOVE      *BLANKS       TPT
|    C                   MOVEL     *BLANKS       CDCAE
|    C                   Z-ADD     299           MED1
|    C                   Z-ADD     *ZEROS        COM1
|    C                   Z-ADD     *ZEROS        MED2
|    C                   Z-ADD     *ZEROS        COM2
|    C                   MOVE      'S'           AVISN
|    C
|    C*--> CONTROLE ALTERAÇÕES
|    C                   Z-ADD     0             WTB               1 0
|    C                   Z-ADD     0             TB                1 0
|    C                   Z-ADD     0             WAGRAV            3 0
|    C                   Z-ADD     0             AGRAV             3 0
|    C                   Z-ADD     0             WNSIN             1 0
|    C                   Z-ADD     0             NSIN              1 0
|    C                   Z-ADD     0             WDUSIN            6 0
|    C                   Z-ADD     0             DTUSIN            6 0
|    C                   Z-ADD     SIT           WSIT              1 0
|    C                   Z-ADD     TALT          WTALT             1 0
|    C                   Z-ADD     MALT          WMALT             2 0
|    C                   Z-ADD     XDTALT        ZDTALT            6 0
|    C                   Z-ADD     ADC           ZADC             11 0
|    C                   MOVEL     NIB           ZNIB             21
|    C                   Z-ADD     FRANQO        WFRQ              7 0
|    C                   MOVE      '0'           FLAGA             1
|    C                   MOVE      '0'           FLAGR             1
|    C                   MOVE      '0'           FLAGU             1
|    C                   MOVE      '0'           FLAGI             1
|    C*
|    C*  INICIALIZA INDICADORES DE ECRAN (DETERMINA CAMPOS A VISUALIZAR).
|+---C                   IF        (RR<>01) AND (RR<>21) AND (RR<>31)
||   C                   EVAL      *IN11 = *ON
|+---C                   ENDIF
|+---C                   IF        (RR = 15)
||   C                   EVAL      *IN95 = *ON
     C                   ELSE
     C                   EVAL      *IN95 = *OFF
|+---C                   ENDIF
|+---C                   IF        (RR<>25)  AND (RR<>35) AND (RR<>55)
||   C                   EVAL      *IN12 = *ON
|+---C                   ENDIF
|    C*
|    C* ===============================================================
|    C*  Extrair registos das tabelas (Base, Diario e/ou Temporario
|    C* ===============================================================
|    C*---> Obter o registo e validar se existem em saldo                       opção CONSULTA
|    C                   MOVE      *BLANK        RSALDO           25
|    C                   EVAL      WRecS  = 'N'
|    C                   SETOFF                                       1516
|    C     RRAPOL        CHAIN(N)  GBASEGRO                           16
|+---C                   IF        (*IN16 AND (Opcao = OPR#Mostrar))
||   C     RRAPOL        CHAIN(N)  BASEGROT                           16
|+---C                   ENDIF
|    C                   EVAL      WRecS = RECS
|    C     RRAPOL        CHAIN(N)  GBASEDIO                           15
|    C
|    C* ===============================================================
|    C*  Gerir as Opções do Menu
|    C* ===============================================================
|+---C                   IF        (Opcao=OPR#Inserir)
||   C                   EXSR      AddApolice
|||  C                   EVAL      *IN20 = *OFF
||   C                   EVAL      WModo = *OFF
|+---C                   ELSEIF    (OPCAO > 1) AND (OPCAO <= 4)
||   C
||+--C                   IF        (*IN16) AND (*IN15)
|||  C                   EVAL      WCodErro = 'PRD0007'
|||  C                   EVAL      WFldName = 'OPCAO'
|||  C                   EXSR      DSPMESSAGE
|||  C                   ITER
||+--C                   ENDIF
||   C
||   C                   EXSR      BEGS04
||+--C                   IF        ((RECS = 'S') AND NOT *IN16)
|||  C                   EVAL      RSALDO = $getMessage('PRD0006')
||+--C                   END
||   C
||+--C                   IF        (NOT *IN16)
|||  C                   Z-ADD     ADC           WADC
|||  C                   MOVEL     NIB           WNIB
||+--C                   ENDIF
||   C
||   C*---> Tratar restantes Opções
||+--C                   IF        (Opcao = OPR#Alterar)
|||  C                   EVAL      WModo = *OFF
|||  C                   EVAL      *IN21 = *ON
|||  C                   EVAL      *IN20 = *OFF
|||  C                   EXSR      ChgApolice
||+--C                   ELSEIF    (Opcao=OPR#AltNumSeg)
|||  C                   EVAL      WModo = *OFF
|||  C                   EVAL      *IN22 = *ON
|||  C                   EVAL      *IN20 = *OFF
|||  C                   MOVE      'ALT.NSEG'    WOPCAO
|||  C*                  EXSR      AltNumSeg
||+--C                   ELSEIF    (Opcao = OPR#Mostrar)
|||  C                   EVAL      WModo = *ON
|||  C                   EVAL      *IN20 = *ON
|||  C                   EXSR      DspApolice
|||  C                   ITER
||+--C                   ENDIF
|+---C                   ELSE
||   C                   EVAL      WCoderro = 'PRD0001'
||   C                   EVAL      WFldName = 'OPCAO'
||   C                   EXSR      DSPMESSAGE
||   C                   ITER
|+---C                   ENDIF
|    C
|    C                   Z-ADD     0             OPCAO             1 0
|    C                   MOVE      ' '           SPAC40           40
+----C                   ENDDO
     C
     C                   SETON                                        LR
     C*====================================================================
     C* Subrotina.: DSPMESSAGE
     C* Objectivo.: Apresentar a Mensagem como Erro ou Aviso
     C*====================================================================
+----C     DSPMESSAGE    BEGSR
|    C                   EVAL      MENSER=%TRIM(WCodErro) + ' ('
|    C                             + %TRIM(WFldName) + ') - '
|    C                             + $getMessage(WCodErro)
|    C                   MOVE      'Z'           INDREG
|+---C                   MONITOR
||   C                   EVAL      WCodGravid = %DEC($getMessage(WCodErro
||   C                                          :'C'):3:0)
|+---C                   ON-ERROR
||   C                   EVAL      WCodGravid = *ZEROS
|+---C                   ENDMON
|    C
|+---C                   IF        (WCodGravid > 10)
||   C                   EVAL      WFmtName = CM#NMREC
||+--C                   IF        (WFmtName <> 'RPRDE01205')
|||  C                   EXFMT     RPRDE01210
||+--C                   ELSEIF    (WFmtName <> *BLANKS)
|||+-C                   IF        $getFldLoc('FPRDE012':
|||| C                                       WFmtName :
|||| C                                       WFldName:
|||| C                                        WRow :
|||| C                                        WCol)
|||| C                   EVAL      CM#ROW = WRow
|||| C                   EVAL      CM#COL = WCol
|||+-C                   ENDIF
||+--C                   ENDIF
|+---C                   ENDIF
+----C                   ENDSR
     C*====================================================================
     C* Subrotina.: dspNewProdInf
     C* Objectivo.: Apresentar informação sobre os novos produtos
     C*====================================================================
+----C     dspNewProdInf BEGSR
|+---C                   IF        (RR = 05) AND (NEWPRD = 'S') AND
||   C                             (CONDOM = ' ')
||   C                   EVAL      WCodErro = 'PRD0057'
||   C                   EXSR      DSPMESSAGE
||   C                   SETON                                        13        inibir clausulas
||   C* Dar mensagem a avisar que a apolice é do novo produto MERCANTILE
|+---C                   ELSEIF    (RR = 55) AND (NEWPRD = 'S')
||   C                   EVAL      WCodErro = 'PRD0056'
||   C                   EXSR      DSPMESSAGE
||   C                   SETON                                        13        inibir clausulas
||   C* Dar mensagem a avisar que a apolice é do novo produto CONDOMINIO
|+---C                   ELSEIF    (RR = 05) AND (NEWPRD = 'S') AND
||   C                             (CONDOM = 'C')
||   C                   EVAL      WCodErro = 'PRD0055'
||   C                   EXSR      DSPMESSAGE
||   C                   SETON                                        13        inibir clausulas
|+---C                   END
+----C                   ENDSR
     C*====================================================================
     C* Subrotina.: AddNumSeg
     C* Objectivo.: Associar um Segurado à Apólice
     C*====================================================================
+----C     AddNumseg     BEGSR
|    C                   EVAL      WIcSair = *OFF
|+---C                   DOW       (NOT WIcSair)
||   C                   Z-ADD     NSEG          WNSEG             5 0
||   C                   MOVE      'B'           INDREG
||   C                   EXFMT     RPRDE01202                           92
||+--C                   IF        (Func#Key = KEY#F04)
|||  C                   EXSR      RunPopUp
|||  C     NSEG          CHAIN     GFSEG                              05
|||  C                   ITER
||+--C                   ELSEIF    (Func#Key = KEY#F07)
|||  C                   LEAVE
||+--C                   ENDIF
||   C
||   C     NSEG          CHAIN     GFSEG                              05
||+--C                   IF        (*IN05)
|||  C                   EVAL      WCodErro = 'PRD0058'
|||  C                   EVAL      WFldName = 'NSEG'
|||  C                   EVAL      WIcErro = *ON
|||  C                   EXSR      DSPMESSAGE
|||  C                   ITER
||+--C                   ELSE
|||  C                   EVAL      WIcErro = *OFF
|||  C                   MOVE      NSEG          NSEG2
||+--C                   ENDIF
||   C                   LEAVE
|+---C                   ENDDO
+----C                   ENDSR
     C*====================================================================
     C* Subrotina.: RunPopUp
     C* Objectivo.: Adicionar um novo registo de uma Apólice
     C*====================================================================
+----C     RunPopUp      BEGSR
|    C                   EVAL      WRow = CM#CROW
|    C                   EVAL      WCol = CM#CCOL
|+---C                   IF        (CM#NMFLD = 'CRHIP')
||   C                   EVAL      CRHIP = $RunCredPopUp(CRHIP)
||   C                   EVAL      DSCCRHIP = $getDescCred(CRHIP)
|+---C                   ELSEIF    (CM#NMFLD = 'NSEG')
||   C                   EVAL      NSEG = $RunEntPopUp(NSEG)
|+---C                   ELSEIF    (CM#NMFLD = 'SIT')
||   C                   EVAL      SIT = %DEC($RunPopUp(54:%CHAR(SIT)):1:0)
|+---C                   ELSEIF    (CM#NMFLD = 'FRAC')
||   C                   EVAL      FRAC = %DEC($RunPopUp(15:%CHAR(FRAC)):1:0)
|+---C                   ELSEIF    (CM#NMFLD = 'CONC')
||   C                   EVAL      CONC = %DEC($RunPopUp(31:%CHAR(CONC)):3:0)
|+---C                   ENDIF
|    C                   EVAL      CM#ROW = WRow
|    C                   EVAL      CM#COL = WCol
+----C                   ENDSR
     C*====================================================================
     C* Subrotina.: AddApolice
     C* Objectivo.: Adicionar um novo registo de uma Apólice
     C*====================================================================
+----C     AddApolice    BEGSR
|    C*--> Validar se está autorizado a executar a operação
|+---C                   IF        (*INU8)
||   C                   EVAL      WCoderro = 'PRD0005'
||   C                   EVAL      WFldName = 'RR'
||   C                   EXSR      DSPMESSAGE
||   C                   LEAVESR
|+---C                   ENDIF
|    C
|    C*--> Validar Existência do Ramo
|    C                   MOVE      'O'           REF
|    C     RR            LOOKUP    TABRR                                  52
|+---C                   IF        (NOT *IN52)
||   C                   EVAL      WCoderro = 'PRD0002'
||   C                   EVAL      WFldName = 'RR'
||   C                   EXSR      DSPMESSAGE
||   C                   LEAVESR
|+---C                   ENDIF
|    C
|    C*--> Validar Existência da Apolice
|+---C                   IF        (NOT *IN15)
||   C                   EVAL      WCoderro = 'PRD0003'
||   C                   EVAL      WFldName = 'RR'
||   C                   EXSR      DSPMESSAGE
||   C                   LEAVESR
|+---C                   ELSEIF    (NOT *IN16)
||   C                   EVAL      WCoderro = 'RR'
||   C                   EVAL      WFldName = 'OPCAO'
||   C                   EXSR      DSPMESSAGE
||   C                   LEAVESR
|+---C                   ENDIF
|    C
|    C*--> Inicializar variáveis de Trabalho
|+---C                   IF        (OPCAO = OPR#Inserir)
||   C     RR            LOOKUP    TABRR         TABPC                    41
||+--C                   IF        (*IN41)
|||  C                   MOVE      TABPC         COMISS
|||  C                   Z-ADD     CORR1         COM1
||+--C                   ENDIF
||   C                   Z-ADD     299           MED1
||   C                   SETOFF                                           41
|+---C                   ENDIF
|    C                   EVAL      *IN19 = *ON
|    C                   MOVE      *BLANKS       APOL
|    C                   MOVE      ' NOVOS  '    WOPCAO            8
|    C                   Z-ADD     0             FRANQO
|    C                   Z-ADD     0             TALT
|    C                   Z-ADD     0             KDTALT
|    C                   Z-ADD     0             DDALT
|    C                   Z-ADD     0             MMALT
|    C                   Z-ADD     0             AAALT
|    C                   Z-ADD     0             AAALTX
|    C                   Z-ADD     0             MALT
|    C                   Z-ADD     0             ACTA
|    C                   Z-ADD     0             PRSANT
|    C                   Z-ADD     0             PRIANT
|    C                   Z-ADD     0             TEST
|    C                   Z-ADD     0             TXEST
|    C                   Z-ADD     0             TOTREC
|    C                   Z-ADD     0             NSEG
|    C                   MOVE      SPAC40        SEG
|    C                   MOVE      SPAC40        MOR
|    C                   Z-ADD     0             CPMOR1
|    C                   MOVE      SPAC40        CPMOR2
|    C                   Z-ADD     0             TXIND                          RG 12/03/97
|    C                   MOVE      *BLANK        INDEX                          RG 12/03/97
|    C                   MOVE      *ZEROS        CAPFAT
|    C                   MOVE      *ZEROS        CRHIP
|    C                   MOVE      'N'           RECS
|    C                   MOVE      *ZEROS        CPEDI
|    C                   MOVE      *ZEROS        ADC
|    C                   MOVE      *BLANK        NIB
|    C                   MOVE      *BLANKS       NOMENT
|    C                   MOVE      'C'           INDREG
|+---C                   IF        (RR <> 15)
||   C                   EVAL      *IN98 = *ON
|+---C                   ENDIF
|     *
|    C************************************************************************
|    C*       , Dezembro 2006
|    C* Inibir ou Desinibir Clausulas no ecran de criação da apolice
|    C* consoante é ou não do novo produto RESIDENTIA, MERCANTILE, CONDOMINIO
|    C************************************************************************
|+---C                   IF        (RR = 05) OR (RR = 55)
||   C                   SETON                                        06        desinibir clausulas
|+---C                   ELSE
||   C                   SETOFF                                       06        desinibir clausulas
|+---C                   ENDIF
|    C************************************************************************
|    C                   EXSR      AddNumSeg
|    C*----> Pressionou a tecla F07
|+---C                   IF        (Func#Key = KEY#F07)
||   C                   LEAVESR
|+---C                   ENDIF
|    C                   SETOFF                                           35    6164
|    C                   EXSR      BEGS03
|    C                   EVAL      WIcSair = *OFF
|+---C                   DOW       (NOT WIcSair)
||   C                   EXFMT     RPRDE01205                           92
||   C* =======================================================
||   C*  Teclas de Função
||   C* =======================================================
||   C*----> Pressionou a tecla F07
||+--C                   IF        (Func#Key = KEY#F07)
|||  C                   LEAVESR
||+--C                   ENDIF
||   C*----> Pressionou a tecla F07
||+--C                   IF        (Func#Key = KEY#F04)
|||  C                   EXSR      RunPopUp
|||  C                   ITER
||+--C                   ENDIF
||   C
||   C*----> Ramo Acidentes Pessoais e Tecla F08 Pressionada
||+--C                   IF        (RR = 02) AND (Func#Key = KEY#F08)           Acid pessoais
|||  C                   ITER
|||  C                   CALL      'AP6000J'
|||  C                   PARM                    WRR
|||  C                   PARM                    WAPOL
||+--C                   END
||   C
||   C*----> Ramo Acidentes Trabalho e Tecla F09 Pressionada
||+--C                   IF        (RR = 15) AND (Func#Key = KEY#F09)           Acid trabalho
|||  C                   ITER
||+--C                   END
||   C* =======================================================
||   C*  Validações
||   C* =======================================================
||+--C                   IF        (SIT <> 1)
|||  C                   EVAL      WCoderro = 'PRD0084'
|||  C                   EVAL      WFldName = 'SIT'
|||  C                   EXSR      DSPMESSAGE
|||  C                   ITER
||+--C                   ENDIF
||   C* =======================================================
||   C*  Calculos
||   C* =======================================================
||   C*--> Calculo de Comissão
||   C                   MOVE      *BLANKS       COMISS
||   C                   EXSR      SR094
||   C
||   C*---> Grupo de Validações Comuns
||   C                   EXSR      SR001
||+--C                   IF        (WIcErro)
|||  C                   EXSR      DSPMESSAGE
|||  C                   ITER
||+--C                   ENDIF
||   C*
||   C                   EXSR      VMES1
||+--C                   IF        (WIcErro)
|||  C                   EVAL      WCodErro = 'PRD0085'
|||  C                   EVAL      WFldName = 'MES1'
|||  C                   EXSR      DSPMESSAGE
|||  C                   ITER
||+--C                   ENDIF
||   C*
||   C                   EXSR      VCONT1
||+--C                   IF        (WIcErro)
|||  C                   EVAL      WCodErro = 'PRD0085'
|||  C                   EVAL      WFldName = 'MMCONT'
|||  C                   EXSR      DSPMESSAGE
|||  C                   ITER
||+--C                   ENDIF
||   C*
||   C                   EXSR      SR008
||+--C                   IF        (WIcErro)
|||  C                   EXSR      DSPMESSAGE
|||  C                   ITER
||+--C                   ENDIF
||   C
||   C* validar ADC ou NIB
||+--C                   IF        ((ADC = *ZEROS) AND (NIB <> *BLANKS)) OR
|||  C                             ((ADC <> *ZEROS) AND (NIB = *BLANKS))
|||  C                   EXSR      VALADC
|||+-C     OUMSG         IFNE      *BLANKS
|||| C                   MOVEL     OUMSG         MENSER
|||| C                   EXSR      DSPMESSAGE
|||| C                   ITER
|||+-C                   END
|||  C* se ADC e NIB preenchidos o AVISN têm que ser S
||+--C                   ELSEIF    (ADC <> *ZERO) AND (NIB <> *BLANKS)
|||  C                   MOVE      'S'           AVISN
||+--C                   END
||   C*
||   C* validação do codigo postal do edífico
||   C                   EXSR      SR022
||+--C                   IF        (WIcErro)
|||  C                   EXSR      DSPMESSAGE
|||  C                   ITER
||+--C                   END
||   C*
||   C                   Z-ADD     0             TALT
||   C                   Z-ADD     0             DDALT
||   C                   Z-ADD     0             MMALT
||   C                   Z-ADD     0             AAALT
||   C                   Z-ADD     0             AAALTX
||   C                   Z-ADD     0             MALT
||   C                   Z-ADD     0             ACTA
||   C*********************************************************************
||   C* validação: no ramo 05 ou 55 (novos produtos) não pode
||   C* haver simultâneamente o Risco 2 e 3
||   C*********************************************************************
||+--C                   IF        ((RR = 05) OR (RR = 55)) AND
|||  C                             (((CR1 = 2) AND (CR2 = 3)) OR
|||  C                             ((CR1 = 2) AND (CR3 = 3)) OR
|||  C                             ((CR1 = 3) AND (CR2 = 2)) OR
|||  C                             ((CR1 = 3) AND (CR3 = 2)) OR
|||  C                             ((CR2 = 3) AND (CR3 = 2)) OR
|||  C                             ((CR2 = 2) AND (CR3 = 3)))
|||  C                   EVAL      WCodErro = 'PRD0052'
|||  C                   EVAL      WFldName = 'CR1'
|||  C                   EXSR      DSPMESSAGE
|||  C                   ITER
||+--C                   ENDIF
||   C*
||   C* ----------------------------------------------------
||   C                   Z-ADD     0             PRSANT
||   C                   Z-ADD     0             PRIANT
||   C                   Z-ADD     0             TEST
||   C                   Z-ADD     0             TXEST
||   C*   =   Novas Validações às clausulas de RAMOS   =   *
||   C                   MOVE      *ZEROS        WRKCL             2 0
||   C                   SETOFF                                           99
||   C*
||   C                   EXSR      SR040
||+--C                   IF        (WIcErro)
|||  C                   EXSR      DSPMESSAGE
|||  C                   ITER
||+--C                   ENDIF
||   C
||   C                   Z-ADD     0             TOTREC
||   C*
||   C                   EXSR      SR080
||+--C                   IF        (WIcErro)
|||  C                   EXSR      DSPMESSAGE
|||  C                   ITER
||+--C                   ENDIF
||   C*
||   C                   EXSR      SR060
||+--C                   IF        (WIcErro)
|||  C                   EXSR      DSPMESSAGE
|||  C                   ITER
||+--C                   ENDIF
||   C*
||+--C                   IF        (RR = 15) AND (FLAGA = '0')
|||  C                   EVAL      WCodErro = 'PRD0030'
|||  C                   EVAL      WFldName = 'FLAGA'
|||  C                   MOVE      '1'           FLAGA
|||  C                   ITER
||+--C                   END
||   C*
||   C                   MOVE      9999999       APOL
||   C     RRAPOL        SETLL     REGAPOL
||   C                   READP     REGAPOL                                92
||+--C                   IF        (*IN92)
|||  C                   EVAL      WCodErro = 'PRD0051'
|||  C                   EVAL      WFldName = 'RRAPOL'
|||  C                   EXSR      DSPMESSAGE
|||  C                   ITER
||+--C                   ENDIF
||   C*
||+--C     RR            IFNE      RRR
|||  C                   EVAL      WCodErro = 'PRD0051'
|||  C                   EVAL      WFldName = 'RR'
|||  C                   EXSR      DSPMESSAGE
|||  C                   ITER
||+--C                   END
||   C*
||   C************************************************************************
||   C*       , Dezembro 2006 - Fevereiro 2007
||   C* ecran para perguntar se é RESIDENTIAL ou CONDOMINIO
||   C************************************************************************
||   C*
-----C
     C
 +---C                   IF        (RR = 05) OR (RR = 55)
 |+--C                   IF        (RR = 05)                                    criar apol
 ||  C                   EXSR      SelCondom
 ||+-C                   IF        (Func#Key = KEY#F07)
 ||| C                   LEAVE
 ||+-C                   ENDIF
 ||  C************************************************************************
 ||  C* 2º ecran de CRIAÇÃO de apolice do novo produto RESIDENTIA
 ||  C* com Coberturas Facultativas e Clausulas Especiais
 ||  C************************************************************************
 |+--C                   ENDIF
 |   C
 |+--C                   IF        (RR = 05) AND (CONDOM = ' ')
 ||  C                   SETON                                        079623
 ||  C                   MOVE      *BLANK        FLAG              1
 ||  C* opções com S (sim) por defeito quando vou criar apolice
 ||  C                   EVAL      CE001 = 'S'
 ||  C                   EVAL      CA102 = 'S'
 ||  C
 ||  C* se houver risco 1 (edificio) por defeito tem garagem
 ||+-C                   IF        (CR1=1) OR (CR2=1) OR (CR3=1)
 ||| C                   EVAL      CD131 = 'S'
 ||+-C                   ENDIF
 ||  C
 ||  C* se houver risco 2 (conteúdo por defeito tem livros, quadros e garagem
 ||+-C                   IF        (CR1=2) OR (CR2=2) OR (CR3=2)
 ||| C                   EVAL      CD103 = 'S'
 ||| C                   EVAL      CD130 = 'S'
 ||| C                   EVAL      CD131 = 'S'
 ||+-C                   ENDIF
 ||  C*
 ||  C                   EXSR      DspMRHResid
 ||  C************************************************************************
 ||  C* 2º ecran de CRIAÇÃO apolice nova para o novo produto MERCANTILE
 ||  C*    (coberturas e clausulas)
 ||  C************************************************************************
 |+--C                   ELSEIF    (RR = 55)
 ||  C                   EXSR      DspMercant
 ||  C************************************************************************
 ||  C* 2º ecran de CRIAÇÃO de apolice do novo produto CONDOMINIO
 ||  C* com Coberturas Facultativas e Clausulas Especiais
 ||  C************************************************************************
 |+--C                   ELSEIF    (RR = 05) AND (CONDOM = 'C')                 ramo habitação mas
 ||  C                   EVAL      CE001 = 'S'
 ||  C                   EVAL      CA102 = 'S'
 ||  C                   EXSR      DspMRCondom
 |+--C                   END
 |   C
 |+--C                   IF        (Func#Key = KEY#F03)
 ||  C                   EXFMT     RPRDE01202
 ||  C                   ITER
 |+--C                   ENDIF
 |   C*
 |   C*---> Pressionou tecla de Função F7
 |+--C                   IF        (Func#Key = KEY#F07)
 ||  C                   LEAVESR
 |+--C                   ENDIF
 |   C*------------------------------------------------------------------
 |   C*
 |   C* cobertura facultativa de fenómenos sísmicos - cálculo da comissão
 |+--C     CF07          IFEQ      'S'
 ||  C     RR            LOOKUP    TABRR         TABPC                    41
 ||+-C                   IF        (*IN41)
 ||| C                   MOVE      TABPC         COMISS
 ||+-C                   ENDIF
 ||  C                   EXSR      COMCFS
 |+--C                   ENDIF
 |   C                   SETOFF                                           23
 +---C                   ENDIF
     C*
-----C
||   C************************************************************************
||   C* Adição novo registo ao GBASEDIO
||   C************************************************************************
||   C                   EXSR      BEGS05
||   C     RAPOL         ADD       1             APOL
||+--C                   IF        (RR = 05) OR (RR = 55)
|||  C                   MOVE      'S'           NEWPRD            1
||+--C                   END
||   C*
||   C                   EXCEPT    GBAADD
||   C                   EXSR      SRALT                                        -CONTROLE
||   C*
||   C                   EXCEPT    RAADD
||+--C                   IF        (ADC <> WADC) OR (NIB <> WNIB)
|||  C                   EXSR      VALADC
||+--C                   END
||   C                   EVAL      WCodErro = 'PRD0012'
||   C                   MOVE      RR            WRM
||   C                   MOVE      APOL          WAP
||   C                   MOVE      WMSG          MENSER
||   C                   EXSR      DSPMESSAGE
||   C* adição registo 1 ao REGAPOL
||   C                   Z-ADD     UYEAR         ANO
||   C                   MOVE      RRAPOL        RRAP
||   C                   MOVE      *BLANK        ARG
||   C                   MOVE      '1'           TA
||   C                   EXCEPT    ADDR
||   C* adição registo 2 ao REGAPOL
||   C                   MOVE      *BLANK        ARG
||   C                   MOVE      '2'           TA
||   C                   MOVEL     NSEG          ARG
||   C                   EXCEPT    ADDR
||   C* adição registo 3 ao REGAPOL
||   C                   MOVE      *BLANK        ARG
||   C                   MOVE      '3'           TA
||   C                   MOVEL     DUR           ARG
||   C                   EXCEPT    ADDR
||   C* adição registo 4 ao REGAPOL
||   C                   MOVE      *BLANK        ARG
||   C                   MOVE      '4'           TA
||   C                   MOVE      UDAY          MMDD              4 0
||   C                   MOVEL     UMONTH        MMDD
||   C                   MOVEL     MMDD          ARG
||   C                   EXCEPT    ADDR
||   C
||   C* ACIDENTES DE TRABALHO
||+--C                   IF        (RR = 15)
|||+-C                   IF        (MOD = 'F')
|||| C                   CALL      'ATP900'
|||| C                   PARM                    RRAPOL
|||+-C                   ELSEIF    (MOD = 'V')
|||| C                   CALL      'ATP910'
|||| C                   PARM                    RRAPOL
|||+-C                   END
||+--C                   END
||   C                   LEAVE
|+---C                   ENDDO
+----C                   ENDSR
     C*====================================================================
     C* Subrotina.: ChgApolice
     C* Objectivo.: alterar o registo para uma Apólice existente
     C*====================================================================
+----C     ChgApolice    BEGSR
|    C*--> Validar se está autorizado a executar a operação
|+---C                   IF        (*INU8)
||   C                   EVAL      WCoderro = 'PRD0005'
||   C                   EVAL      WFldName = 'RR'
||   C                   EXSR      DSPMESSAGE
||   C                   LEAVESR
|+---C                   ENDIF
|    C                   EXSR      dspNewProdInf
|    C
|    C                   EVAL      *IN21 = *ON
|+---C                   IF        (NOT *IN15)
||   C                   EVAL      WCoderro = 'PRD0009'
||   C                   EVAL      WFldName = 'OPCAO'
||   C                   EXSR      DSPMESSAGE
|+---C                   ELSE
||   C                   MOVE      PRSACT        PRSANT
|+---C                   ENDIF
|    C                   MOVE      'ALTERAC.'    WOPCAO
|    C                   SETOFF                                       356164
|    C                   MOVE      'D'           INDREG
|    C*
|+---C                   IF        (RR <> 15)
||   C                   EVAL      *IN98 = *ON
|+---C                   ENDIF
|    C*
|    C*==> Formatar os Campos que não poderam ser apresentados no ecrã
|    C                   EVAL      CM#COL = 6
|    C                   EVAL      CM#ROW = 5
|    C                   EVAL      WIcSair = *OFF
|+---C                   DOW       (NOT WIcSair)
||   C                   EXFMT     RPRDE01205
||   C
||   C* F10-Ver as taxas para Multiriscos
||+--C                   IF        (RR = 05) AND (Func#Key = KEY#F10)
|||  C                   CALL      'PPRDO101'
|||  C                   PARM                    RRAPOL
|||  C                   ITER
|||  C                   ENDIF
||+--C
||   C* F04-Obter valores possíveis para os Campos (Pop-Up)
||+--C                   IF        (Func#Key = KEY#F04)
|||  C                   EXSR      RunPopUp
|||  C                   ITER
||+--C                   ENDIF
||   C* F05-Manutenção de Textos, limites e Franquias
||+--C                   IF        ((RR = 05) OR (RR = 55)) AND
|||  C                             (Func#Key = KEY#F05)
     C                   EVAL      WDatInicio = 20120615
|||  C                   EVAL      WPtrData = %ADDR(RBASEBK)
|||  C                   CALL      'PPRDO100'
|||  C                   PARM                    RRAPOL
|||  C                   PARM                    WPtrData
|||  C                   PARM                    WDatInicio
|||  C                   PARM                    WModo
|||  C                   ITER
||+--C                   ENDIF
||   C* F12-ver doc memo da apol
||+--C                   IF        (Func#Key = KEY#F12)
|||  C                   MOVE      RRAPOL        #APOL             9 0
|||  C                   SETON                                        LRU1U2
||+--C                   ENDIF
||   C*---> Foi pressionada a tecla de Função F09
||+--C                   IF        (RR = 15) AND (Func#Key = KEY#F09)           AC.TRABALHO
|||+-C                   IF        (MOD = 'F')
|||| C                   CALL      'ATP900'
|||| C                   PARM                    RRAPOL
|||+-C                   ELSEIF    (MOD = 'V')
|||| C                   CALL      'ATP910'
|||| C                   PARM                    RRAPOL
|||+-C                   END
|||  C                   ITER
||+--C                   ENDIF
||   C
||   C*---> Pressiou a tecla de Função F07 para Sair
||+--C                   IF        (Func#Key = KEY#F07)
|||  C                   LEAVESR
||+--C                   ENDIF
||   C
||   C*----> Tecla de Função F08 Pressionada
||+--C                   IF        (RR = 02) AND (Func#Key = KEY#F08)
|||  C                   CALL      'AP6000J'
|||  C                   PARM                    WRR
|||  C                   PARM                    WAPOL
|||  C                   ITER
||+--C                   ENDIF
||   C
||   C************************************************************************
||   C*       , Dezembro 2006
||   C* se a OPÇÃO é CONSULTA vai para o fim desta rotina
||   C* sem necessidade de fazer validações
||   C************************************************************************
||+--C                   IF        (SIT < *ZEROS) OR (SIT > 4)
|||  C                   EVAL      WCoderro = 'PRD0084'
|||  C                   EVAL      WFldName = 'SIT'
|||  C                   EXSR      DSPMESSAGE
|||  C                   ITER
||+--C                   ENDIF
||   C*
||   C                   EXSR      SR001
||+--C                   IF        (WIcErro)
|||  C                   EXSR      DSPMESSAGE
|||  C                   ITER
||+--C                   ENDIF
||   C*
||   C                   EXSR      VMES13
||+--C                   IF        (WIcErro)
|||  C                   EVAL      WCodErro = 'PRD0085'
|||  C                   EVAL      WFldName = 'MES1'
|||  C                   EXSR      DSPMESSAGE
|||  C                   ITER
||+--C                   ENDIF
||   C*
||   C                   EXSR      VMCONT
||+--C                   IF        (WIcErro)
|||  C                   EVAL      WCodErro = 'PRD0049'
|||  C                   EVAL      WFldName = 'MCONT'
|||  C                   EXSR      DSPMESSAGE
|||  C                   ITER
||+--C                   ENDIF
||   C*
||+--C                   IF        (AACON < WAno)
|||  C                   EVAL      WCodErro = 'PRD0016'
|||  C                   EVAL      WFldName = 'AACON'
|||  C                   EXSR      DSPMESSAGE
|||  C                   ITER
||+--C                   ENDIF
||   C*
||   C                   EXSR      SR008
||+--C                   IF        (WIcErro)
|||  C                   EXSR      DSPMESSAGE
|||  C                   ITER
||+--C                   ENDIF
||   C
||   C*----> Calculo de Comissão
||   C                   EXSR      SR094
||   C*
||---C                   IF        (TALT < *ZEROS) OR (TALT > 5)
|||  C                   EVAL      WCoderro = 'PRD0086'
|||  C                   EVAL      WFldName = 'TALT'
|||  C                   EXSR      DSPMESSAGE
|||  C                   ITER
||+--C                   ENDIF
||   C*
||   C                   EXSR      VTALT
||+--C                   IF        (WIcErro)
|||  C                   EVAL      WCodErro = 'PRD0025'
|||  C                   EVAL      WFldName = 'VTALT'
|||  C                   EXSR      DSPMESSAGE
|||  C                   ITER
||+--C                   ENDIF
||   C*
||+--C                   IF        (DDALT < *ZEROS) OR (DDALT > 31)
|||  C                   EVAL      WCoderro = 'PRD0010'
|||  C                   EVAL      WFldName = 'DDALT'
|||  C                   EXSR      DSPMESSAGE
|||  C                   ITER
||+--C                   ENDIF
||   C*
||+--C                   IF        (MMALT < *ZEROS) OR (MMALT > 12)
|||  C                   EVAL      WCoderro = 'PRD0010'
|||  C                   EVAL      WFldName = 'MMALT'
|||  C                   EXSR      DSPMESSAGE
|||  C                   ITER
||+--C                   ENDIF
||   C*
||+--C     DTALT         IFGT      0
|||+-C                   IF        (AAALTX< *ZEROS) OR (AAALTX> (WAno+1))
|||| C                   EVAL      WCoderro = 'PRD0010'
|||| C                   EVAL      WFldName = 'AALTX'
|||| C                   EXSR      DSPMESSAGE
|||| C                   ITER
|||+-C                   ENDIF
||+--C                   ENDIF
||   C*
||   C                   EXSR      VDALT
||+--C                   IF        (WIcErro)
|||  C                   EVAL      WCodErro = 'PRD0085'
|||  C                   EXSR      DSPMESSAGE
|||  C                   ITER
||+--C                   ENDIF
||   C*
||+--C                   IF        (MALT < *ZEROS) OR (MALT > 99)
|||  C                   EVAL      WCoderro = 'PRD0010'
|||  C                   EVAL      WFldName = 'MALT'
|||  C                   EXSR      DSPMESSAGE
|||  C                   ITER
||+--C                   ENDIF
||    *
||   C
||+--C     ADC           IFNE      WADC
|||  C                   EXSR      VALADC
||||-C     OUMSG         IFNE      *BLANKS
|||| C                   MOVEL     OUMSG         MENSER
|||| C                   EXSR      DSPMESSAGE
|||| C                   ITER
|||+-C                   END
||+--C                   END
||    *
||+--C     NIB           IFNE      WNIB
|||  C                   EXSR      VALADC
|||+-C     OUMSG         IFNE      *BLANKS
|||| C                   MOVEL     OUMSG         MENSER
|||| C                   EXSR      DSPMESSAGE
|||| C                   ITER
|||+-C                   END
||+--C                   END
||   C* se ADC e NIB preenchidos o AVISN têm que ser S
||+--C                   IF        (ADC <> *ZEROS) AND (NIB <> *BLANKS)
|||  C                   MOVE      'S'           AVISN
||+--C                   ENDIF
||   C*
||   C                   EXSR      SR022
||+--C                   IF        (WIcErro)
|||  C                   EXSR      DSPMESSAGE
|||  C                   ITER
||+--C                   END
||    *
||+--C                   IF        (CS < *ZEROS) OR (CS > 2)
|||  C                   EVAL      WCoderro = 'PRD0010'
|||  C                   EVAL      WFldName = 'CS'
|||  C                   EXSR      DSPMESSAGE
|||  C                   ITER
||+--C                   ENDIF
||   C*
||+--C                   IF        (RR = 05)
|||  C                   SETON                                        97
|||  C     7             LOOKUP    ACR                                    99
|||+-C                   IF        (*IN99)
|||| C     2             LOOKUP    ACR                                    97
||||+C                   IF        (NOT *IN97)
|||||C                   SETON                                        0880
|||||C                   EVAL      WCodErro = 'PRD0053'
|||||C                   EXSR      DSPMESSAGE
|||||C                   ITER
||||+C                   END
|||+-C                   END
||+--C                   END
||    *
||   C* ACIDENTES DE TRABALHO - DETERMINA PREMIO
||+--C                   IF        (RR = 15)
|||  C
-----C
     C
 +---C                   IF        (PR1 = *ZEROS)
 |   C     CAP1          MULT      TX1AT         WORK             11 2
 |   C     WORK          DIV(H)    100           PR1
 |+--C                   IF        (ISEN = 1)
 ||  C     PR1           DIV(H)    1.20          PR1
 |+--C                   END
 |   C*
 +---C                   ELSEIF    (PR1 < 30)
 |+--C                   IF        (FLAGR = '0')
 ||+-C                   IF        (ISEN = 1)
 ||| C                   EVAL      WCodErro = 'PRD0061'
 ||| C                   EVAL      WFldName = 'ISEN'
 ||+-C                   ELSE
 ||| C                   EVAL      WCodErro = 'PRD0062'
 ||| C                   EVAL      WFldName = 'ISEN'
 ||+-C                   END
 ||  C                   MOVE      '1'           FLAGR
 ||  C                   ITER
 |+--C                   END
 +---C                   END
     C
     C*
 +---C     FLAGI         IFEQ      '0'
 |+--C     ISEN          IFEQ      1
 ||  C                   MOVE      '1'           FLAGI
 ||  C                   EVAL      WCodErro = 'PRD0011'
 ||  C                   EVAL      WFldName = 'ISEN'
 ||  C                   ITER
 |+--C                   END
 +---C                   END
     C*
-----C
|||  C                   Z-ADD     PR1           PRSACT
|||  C                   Z-ADD     CAP1          CAPTOT
|||  C                   Z-ADD     TX1AT         TXMAT
||+--C                   END
||   C* ----------------------------------------------------
||   C*
||+--C                   IF        (TEST < *ZEROS) OR (TEST > 3)
|||  C                   EVAL      WCoderro = 'PRD0010'
|||  C                   EVAL      WFldName = 'TEST'
|||  C                   EXSR      DSPMESSAGE
|||  C                   ITER
||+--C                   ENDIF
||   C*
||+--C                   IF        ((TALT = 5) AND (TEST = *ZEROS)) OR
|||  C                             ((TALT <> 5) AND (TEST <> *ZEROS))
|||  C                   EVAL      WCodErro = 'PRD0025'
|||  C                   EVAL      WFldName = 'TALT'
|||  C                   EXSR      DSPMESSAGE
|||  C                   ITER
||+--C                   ENDIF
||   C*
||+--C                   IF        ((TEST = 3) AND (TXEST = *ZEROS)) OR
|||  C                             ((TEST <> 3) AND (TXEST <> *ZEROS))
|||  C                   EVAL      WCodErro = 'PRD0084'
|||  C                   EVAL      WFldName = 'TEST'
|||  C                   EXSR      DSPMESSAGE
|||  C                   ITER
||+--C                   ENDIF
||   C*
||   C                   EXSR      SR080
||+--C                   IF        (WIcErro)
|||  C                   EXSR      DSPMESSAGE
|||  C                   ITER
||+--C                   ENDIF
||   C******************************************************
||   C*        validacao as clausulas para o RAMO = 55.    *
||   C                   MOVE      *ZEROS        WRKCL
||   C                   SETOFF                                           99
||   C*
||   C                   EXSR      SR040
||+--C                   IF        (WIcErro)
|||  C                   EXSR      DSPMESSAGE
|||  C                   ITER
||+--C                   ENDIF
||   C*
||   C                   EXSR      SR060
||+--C                   IF        (WIcErro)
|||  C                   EXSR      DSPMESSAGE
|||  C                   ITER
||+--C                   ENDIF
||   C*
||+--C                   IF        (RR = 15) AND (FLAGU = '0')
|||  C                   EVAL      WCodErro = 'PRD0030'
|||  C                   EVAL      WFldName = 'RR'
|||  C                   MOVE      '1'           FLAGU
|||  C                   ITER
||+--C                   END
||   C*
||   C                   EXSR      BEGS05
||    *
||   C     RRAPOL        CHAIN     BASEGRO                            99
||+--C                   IF        (NOT *IN99)
|||+-C                   IF        (NPESS <> WNPESS)
|||| C                   MOVE      WNPESS        NPESS
|||+-C                   ENDIF
|||  C
|||+-C                   IF        (WTPRIS <> TPRISC)
|||| C                   MOVE      WTPRIS        TPRISC
|||+-C                   ENDIF
|||  C
|||+-C                   IF        (WCLTAR <> CLTAR)
|||| C                   MOVE      WCLTAR        CLTAR
|||+-C                   ENDIF
||+--C                   ENDIF
||   C*
||   C********************************************************************
||   C* ecran de CONSULTA com condições e coberturas dos novos produtos
||   C********************************************************************
||+--C                   IF        (RR = 05) OR (RR = 55)
|||+-C                   IF        (RR = 05) AND (NEWPRD =  'S') AND
|||| C                             (CONDOM = ' ')
|||| C                   EXSR      DspMRHResid
|||+-C                   ELSEIF    (RR = 55) AND (NEWPRD = 'S')
|||| C                   EXSR      DspMercant
|||+-C                   ELSEIF    (RR = 05) AND (NEWPRD = 'S') AND             é multiriscos
|||| C                             (CONDOM = 'C')                               é novo produto
|||| C                   EXSR      DspMRCondom
|||+-C                   ENDIF
|||  C
|||  C*-----> Tecla de Função F03 Pressionada
|||+-C                   IF        (Func#Key = KEY#F03)
|||| C                   ITER
|||+-C                   ENDIF
|||  C
|||  C*-----> Tecla de função F12 Pressionada
|||+-C                   IF        (Func#Key = KEY#F12)
|||| C                   MOVE      RRAPOL        #APOL             9 0
|||| C                   SETON                                        LRU1U2
|||+-C                   ENDIF
|||  C
|||  C*-----> Tecla de função F07 Pressionada
|||+-C                   IF        (Func#Key = KEY#F07)
|||| C                   LEAVESR
|||+-C                   ENDIF
|||  C*
|||  C* cobertura facultativa de fenómenos sísmicos - cálculo da comissão
|||+-C     CF07          IFEQ      'S'
|||| C     RR            LOOKUP    TABRR         TABPC                    41
||||+C                   IF        (*IN41)
|||||C                   MOVE      TABPC         COMISS
||||+C                   ENDIF
|||| C                   EXSR      COMCFS
|||+-C                   ENDIF
||+--C                   ENDIF
||   C*---> Confirmar Alteracoes
||   C                   EVAL      ICSAVE = 'N'
||   C                   EXFMT     CONFIRMAR
||+--C                   IF        (ICSAVE = 'S')
|||  C*---> Adicionar ou alterar registo no Diário
|||+-C                   IF        (*IN15)
|||| C                   EXCEPT    GBAADD
|||+-C                   ELSE
|||| C                   EXCEPT    GBAUPD
|||+-C                   ENDIF
|||  C                   EXSR      SRALT                                        -CONTROLE
||+--C                   ENDIF
||   C                   LEAVE
|+---C                   ENDDO
+----C                   ENDSR
     C*====================================================================
     C* Subrotina.: DspApolice
     C* Objectivo.: Mostrar os dados para uma Apólice
     C*====================================================================
+----C     DspApolice    BEGSR
|    C                   EXSR      dspNewProdInf
|    C                   EVAL      *IN20 = *ON
|    C                   MOVE      'CONSULTA'    WOPCAO
|    C                   MOVE      *BLANKS       NOMENT
|    C                   SETON                                        14        inibir clausulas
|+---C                   IF        (NOT *IN15)
||   C                   EVAL      WCoderro = 'PRD0008'
||   C                   EXSR      DSPMESSAGE
|+---C                   ENDIF
|    C                   SETOFF                                       356164
|    C                   MOVE      'D'           INDREG
|    C*
|+---C                   IF        (RR <> 15)
||   C                   EVAL      *IN98 = *ON
|+---C                   ENDIF
|    C*
|    C
|    C*==> Formatar os Campos que não poderam ser apresentados no ecrã
|    C                   EVAL      WIcSair = *OFF
|+---C                   DOW       (NOT WIcSair)
||   C                   EXFMT     RPRDE01205                           92
||   C
||   C**************************************************************************
||   C* F10-Ver as taxas para Multiriscos
||   C**************************************************************************
||+--C                   IF        (RR = 05) AND (Func#Key = KEY#F10)
|||  C                   CALL      'PPRDO101'
|||  C                   PARM                    RRAPOL
|||  C                   ITER
||+--C                   ENDIF
||   C
||   C*----> Tecla de Função F08 Pressionada ?
||+--C                   IF        (RR=02) AND (Func#Key = KEY#F08)
|||  C                   CALL      'AP6000J'
|||  C                   PARM                    WRR
|||  C                   PARM                    WAPOL
|||  C                   ITER
||+--C                   END
||   C**************************************************************************
||   C* F04-Obter valores possíveis para os Campos (Pop-Up)
||   C**************************************************************************
||+--C                   IF        (Func#Key = KEY#F04)
|||  C                   ITER
||+--C                   ENDIF
||   C**************************************************************************
||   C* F05-Manutenção de Textos, limites e Franquias
||   C**************************************************************************
||+--C                   IF        ((RR = 05) OR (RR = 55)) AND
|||  C                             (Func#Key = KEY#F05)
|||  C                   EVAL      WPtrData = %ADDR(RBASEBK)
|||  C                   CALL      'PPRDO100'
|||  C                   PARM                    RRAPOL
|||  C                   PARM                    WPtrData
|||  C                   PARM                    WDatInicio
|||  C                   PARM                    WModo
|||  C                   ITER
||+--C                   ENDIF
||   C*---> Foi pressionada a tecla de Função F09
||+--C                   IF        (Func#Key = KEY#F09)
|||+-C                   IF        (MOD = 'F')
|||| C                   CALL      'ATP901'
|||| C                   PARM                    RRAPOL
|||+-C                   ELSEIF    (MOD = 'V')
|||| C                   CALL      'ATP911'
|||| C                   PARM                    RRAPOL
|||+-C                   END
|||  C                   ITER
||+--C                   ENDIF
||   C**************************************************************************
||   C* F12-ver doc memo da apol
||+--C                   IF        (Func#Key = KEY#F12)
|||  C                   MOVE      RRAPOL        #APOL             9 0
|||  C                   SETON                                        LRU1U2
||+--C                   ENDIF
||   C
||   C*---> Pressiou a tecla de Função F07 para Sair
||+--C                   IF        (Func#Key = KEY#F07)
|||  C                   LEAVE
||+--C                   ENDIF
||   C********************************************************************
||   C* ecran de CONSULTA com condições e coberturas dos novos produtos
||   C********************************************************************
||+--C                   IF        (RR = 05) AND (NEWPRD =  'S') AND
|||  C                             (CONDOM = ' ')
|||  C                   EXSR      DspMRHResid
|||  C                   EVAL      WIcSair = *OFF
||+--C                   ELSEIF    (RR = 55) AND (NEWPRD = 'S')
|||  C                   EXSR      DspMercant
||+--C                   ELSEIF    (RR = 05) AND (NEWPRD = 'S') AND             é multiriscos
|||  C                             (CONDOM = 'C')                               é novo produto
|||  C                   EXSR      DspMRCondom
|||  C                   EVAL      WIcSair = *OFF
||+--C                   ENDIF
||   C
||   C*-----> Tecla de Função F03 Pressionada
||+--C                   IF        (Func#Key = KEY#F03)
|||  C                   ITER
||+--C                   ENDIF
||   C
||   C*-----> Tecla de função F12 Pressionada
||+--C                   IF        (Func#Key = KEY#F12)
|||  C                   MOVE      RRAPOL        #APOL             9 0
|||  C                   SETON                                        LRU1U2
||+--C                   ENDIF
||   C
||   C*-----> Recla de função F07 Pressionada
||+--C                   IF        (Func#Key = KEY#F07)
|||  C                   LEAVE
||+--C                   ENDIF
||   C                   LEAVE
|+---C                   ENDDO
|    C                   EXSR      SRALT                                        -CONTROLE
+----C                   ENDSR
     C*====================================================================
     C* Subrotina.: DspMRCondom
     C* Objectivo.: Seleccionar a Apólices Multi-Riscos Condominio
     C*====================================================================
+----C     DspMRCondom   BEGSR
|    C                   SETON                                        079623
|    C                   MOVE      *BLANK        FLAG              1
|    C                   EVAL      WIcSair = *OFF
|+---C                   DOW       (NOT WIcSair)
||   C                   EVAL      WIcErro = *OFF
||   C                   SETON                                        96
||   C                   EXFMT     RPRDE01208                           92
||   C*---> Pressionou a tecla de Função F03
||+--C                   IF        (Func#Key = KEY#F03)
|||  C                   LEAVE
|||  C*
|||  C*---> Pressionou a tecla de Função F07
||+--C                   ELSEIF    (Func#Key = KEY#F07)
|||  C                   LEAVE
||+--C                   ENDIF
||   C
||   C*---> Validar as Opções Seleccionadas
||   C                   EXSR      ValOpcoes
||+--C                   IF        (WIcErro)
|||  C                   SETOFF                                       96
|||  C                   EVAL      WFldName = 'RR'
|||  C                   EXSR      DSPMESSAGE
|||  C                   ITER
||+--C                   ENDIF
||   C
||+--C                   IF        (ESTCON <> *BLANKS) AND (ESTCON <> '1') AND
|||  C                             (ESTCON <> '2') AND (ESTCON <> '3')
|||  C                   EVAL      WIcErro = *ON
|||  C                   EVAL      WCoderro = 'PRD0091'
|||  C                   EVAL      WFldName = 'ESTCON'
|||  C                   EXSR      DSPMESSAGE
|||  C                   SETOFF                                       96
|||  C                   ITER
||+--C                   END
||   C
||   C* não pode haver CE003 e CA031 simultâneamente
||+--C                   IF        (CE003 = 'S') AND (CA031 = 'S')
|||  C                   EVAL      WIcErro = *ON
|||  C                   EVAL      WCoderro = 'PRD0092'
|||  C                   EVAL      WFldName = 'CE003'
|||  C                   EXSR      DSPMESSAGE
|||  C                   SETOFF                                       96
|||  C                   ITER
||+--C                   END
||   C
||   C* não pode haver CE003 e CA102 simultâneamente
||+--C                   IF        (CE003 = 'S') AND (CA102 = 'S')
|||  C                   EVAL      WIcErro = *ON
|||  C                   EVAL      WCoderro = 'PRD0092'
|||  C                   EVAL      WFldName = 'CE003'
|||  C                   EXSR      DSPMESSAGE
|||  C                   SETOFF                                       96
|||  C                   ITER
||+--C                   END
||   C                   LEAVE
|+---C                   ENDDO
+----C                   ENDSR
     C*====================================================================
     C* Subrotina.: DspMercant
     C* Objectivo.: Seleccionar a Apólices Mercantil
     C*====================================================================
+----C     DspMercant    BEGSR
|    C                   SETON                                        079623
|    C                   MOVE      *BLANK        FLAG
|    C                   EVAL      WIcSair = *OFF
|+---C                   DOW       (NOT WIcSair)
||   C                   EVAL      WIcErro = *OFF
||   C                   SETON                                        96
||   C                   EXFMT     RPRDE01209                           92
||   C*---> Tecla de Função F3 pressionada
||+--C                   IF        (Func#Key = KEY#F03)
|||  C                   LEAVE
|||  C
|||  C*---> Tecla de Função F7 pressionada
||+--C                   ELSEIF    (Func#Key = KEY#F07)
|||  C                   LEAVE
||+--C                   ENDIF
||   C
||   C*--> validar as Opções
||   C                   EXSR      ValOpcoes
||+--C                   IF        (WIcErro)
|||  C                   SETOFF                                       96
|||  C                   EVAL      WFldName = 'RR'
|||  C                   EXSR      DSPMESSAGE
|||  C                   ITER
||+--C                   ENDIF
||   C                   LEAVE
|+---C                   ENDDO
+----C                   ENDSR
     C*====================================================================
     C* Subrotina.: DspMRHResid
     C* Objectivo.: Apólices Multi-Riscos Residencia
     C*====================================================================
+----C     DspMRHResid   BEGSR
|    C                   EVAL      WIcSair = *OFF
|+---C                   DOW       (NOT WIcSair)
||   C                   EVAL      WIcErro = *OFF
||   C                   SETON                                        96
||   C                   EXFMT     RPRDE01207                           92
||   C*---> Pressionou tecla de Função F3
||+--C                   IF        (Func#Key = KEY#F03)
|||  C                   LEAVE
|||  C*
|||  C*---> Pressionou tecla de Função F7
||+--C                   ELSEIF    (Func#Key = KEY#F07)
|||  C                   LEAVE
||+--C                   ENDIF
||   C*
||   C*---> Validar as Opções Seleccionadas
||   C                   EXSR      ValOpcoes
||+--C                   IF        (WIcErro)
|||  C                   SETOFF                                       96
|||  C                   EVAL      WFldName = 'RR'
|||  C                   EXSR      DSPMESSAGE
|||  C                   ITER
||+--C                   ENDIF
||   C*
||   C*---> Validar Estado de Conservação do Edificio
||+--C                   IF        (ESTCON <> *BLANKS) AND (ESTCON <> '1') AND
|||  C                             (ESTCON <> '2') AND (ESTCON <> '3')
|||  C                   EVAL      WIcErro = *ON
|||  C                   EVAL      WCoderro = 'PRD0091'
|||  C                   EVAL      WFldName = 'ESTCON'
|||  C                   EXSR      DSPMESSAGE
|||  C                   SETOFF                                       96
|||  C                   ITER
||+--C                   END
||   C*
||   C
||   C* não pode haver CE003 e CA031 simultâneamente
||+--C                   IF        (CE003 = 'S') AND (CA031 = 'S')
|||  C                   EVAL      WIcErro = *ON
|||  C                   SETOFF                                       96
|||  C                   EVAL      WCoderro = 'PRD0092'
|||  C                   EVAL      WFldName = 'CE003'
|||  C                   EXSR      DSPMESSAGE
|||  C                   ITER
||+--C                   END
||   C
||   C* não pode haver CE003 e CA102 simultâneamente
||+--C                   IF        (CE003 = 'S') AND (CA102 = 'S')
|||  C                   EVAL      WIcErro = *ON
|||  C                   EVAL      WCoderro = 'PRD0092'
|||  C                   EVAL      WFldName = 'CE003'
|||  C                   EXSR      DSPMESSAGE
|||  C                   SETOFF                                       96
|||  C                   ITER
||+--C                   END
||   C                   LEAVE
|+---C                   ENDDO
+----C                   ENDSR
     C*====================================================================
     C* Subrotina.: SelCondom
     C* Objectivo.: Seleccionar a Apólices Multiriscos como sendo Condominio
     C*====================================================================
+----C     SelCondom     BEGSR
|    C                   SETON                                        96
|    C                   MOVE      *BLANK        SIMNAO
|+---C                   DOW       (SIMNAO <> 'S') AND (SIMNAO <> 'N')
||   C                   EXFMT     CONDOMIN
||   C                   SETON                                        96
||+--C                   IF        (Func#Key = KEY#F07)
|||  C                   LEAVE
||+--C                   ENDIF
||   C*
||+--C                   IF        (SIMNAO = 'SIM')
|||  C                   MOVE      'C'           CONDOM
||+--C                   ELSEIF    (SIMNAO = 'NAO')
|||  C                   MOVE      ' '           CONDOM                         é residential
||+--C                   ELSE
|||  C                   SETOFF                                       96
||+--C                   ENDIF
|+---C                   ENDDO
+----C                   ENDSR
     C*====================================================================
     C* Subrotina.: SR080
     C* Objectivo.: Grupo de validações Comuns
     C*====================================================================
+----C     SR080         BEGSR
|    C                   EVAL      WIcErro = *OFF
|    C
|+---C                   IF        (RR = 05)
||   C     7             LOOKUP    ACR                                    99
||+--C                   IF        (*IN99)
|||  C     2             LOOKUP    ACR                                    97
|||+-C                   IF        (NOT *IN97)
|||| C                   EVAL      WCodErro = 'PRD0053'
|||| C                   EVAL      WIcErro  = *ON
|||+-C                   END
||+--C                   END
||   C                   SETOFF                                       97
|+---C                   END
|    C
|    C* ----------------------------------------------------
|    C* ACIDENTES DE TRABALHO - DETERMINA PREMIO
|    C* ----------------------------------------------------
|+---C                   IF        (RR = 15)
||+--C                   IF        (PR1 = *ZEROS)
|||  C     CAP1          MULT      TX1AT         WORK             11 2
|||  C     WORK          DIV(H)    100           PR1
|||+-C                   IF        (ISEN = 1)
|||| C     PR1           DIV(H)    1.20          PR1
|||+-C                   END
||+--C                   END
||   C*
||+--C                   IF        (PR1 < 30) AND (FLAGR = '0') AND
|||  C                             (NOT WIcErro)
|||+-C                   IF        (ISEN = 1)
|||| C                   EVAL      WCodErro = 'PRD0061'
|||| C                   EVAL      WFldName = 'ISEN'
|||| C                   EVAL      WIcErro  = *ON
|||+-C                   ELSE
|||| C                   EVAL      WCodErro = 'PRD0062'
|||| C                   EVAL      WFldName = 'ISEN'
|||| C                   EVAL      WIcErro  = *ON
|||+-C                   END
|||  C                   MOVE      '1'           FLAGR
||+--C                   END
|+---C                   END
|    C*
|+---C                   IF        (FLAGI = '0') AND (ISEN = 1)
||   C                             AND (NOT WIcErro)
||   C                   MOVE      '1'           FLAGI
||   C                   EVAL      WCodErro = 'PRD0011'
||   C                   EVAL      WFldName = 'ISEN'
||   C                   EVAL      WIcErro  = *ON
|+---C                   END
|    C*
|    C                   Z-ADD     PR1           PRSACT
|    C                   Z-ADD     CAP1          CAPTOT
|    C                   Z-ADD     TX1AT         TXMAT
|+---C                   IF        (RR = 02)
||+--C                   IF        (CGRUPO = *ZEROS) AND (NOT WIcErro)
|||  C                   EVAL      WCodErro = 'PRD0068'
|||  C                   EVAL      WFldName = 'CGRUPO'
|||  C                   EVAL      WIcErro = *ON
||+--C                   END
||   C
||+--C                   IF        (NPESS = *ZEROS) AND (NOT WIcErro)
|||  C                   EVAL      WCodErro = 'PRD0068'
|||  C                   EVAL      WFldName = 'NPESS'
|||  C                   EVAL      WIcErro = *ON
||+--C                   END
||   C
||+--C                   IF        (NOT $ValCodigo(53:%CHAR(CGRUPO)))
|||  C                             AND (NOT WIcErro)
|||  C                   EVAL      WCodErro = 'PRD0022'
|||  C                   EVAL      WFldName = 'CGRUPO'
|||  C                   EVAL      WIcErro = *ON
||+--C                   END
||   C*
|+---C                   ELSE
||+--C                   IF        (CGRUPO <> *ZEROS)
|||  C                   EVAL      WCodErro = 'PRD0071'
|||  C                   EVAL      WFldName = 'CGRUPO'
|||  C                   EVAL      WIcErro = *ON
||+--C                   END
||   C
||+--C                   IF        (NPESS <> *ZEROS) AND (NOT WIcErro)
|||  C                   EVAL      WCodErro = 'PRD0071'
|||  C                   EVAL      WFldName = 'NPESS'
|||  C                   EVAL      WIcErro = *ON
||+--C                   END
|+---C                   END
|     *
|    C*   Validacao de  Campos Especificos do RAMO = 15      PC 03/03/00
|+---C                   IF        (RR = 15) AND (NOT WIcErro)
||+--C                   IF        (CAPFAT = *ZEROS)
|||  C                   EVAL      WCodErro = 'PRD0017'
|||  C                   EVAL      WFldName = 'CAPFT'
|||  C                   EVAL      WIcErro = *ON
||+--C                   END
||   C*
||+--C                   IF        (PROFSL = *ZEROS) AND (NOT WIcErro)
|||  C                   EVAL      WCodErro = 'PRD0064'
|||  C                   EVAL      WFldName = 'PROFSL'
|||  C                   EVAL      WIcErro = *ON
||+--C                   ELSEIF    (NOT WIcErro)
|||  C                   Z-ADD     *ZEROS        KPROF             6 0
|||  C                   MOVEL     00            WKPROF            5 0
|||  C                   MOVE      PROFSL        WKPROF
|||  C                   MOVE      WKPROF        KPROF
|||  C                   MOVE      1             KPROF
|||  C     KPROF         CHAIN     FTEXTOS                            05
|||+-C                   IF        (*IN05)
|||| C                   EVAL      WCodErro = 'PRD0073'
|||| C                   EVAL      WFldName = 'PROFSL'
|||| C                   EVAL      WIcErro = *ON
|||+-C                   END
||+--C                   END
||   C*
||+--C                   IF        (MOD = *BLANKS) AND (NOT WIcErro)
|||  C                   EVAL      WCodErro = 'PRD0063'
|||  C                   EVAL      WFldName = 'MOD'
|||  C                   EVAL      WIcErro = *ON
||+--C                   ELSEIF    (NOT WIcErro) AND
|||  C                             ((MOD <> 'F') AND (MOD <> 'V'))
|||  C                   EVAL      WCodErro = 'PRD0050'
|||  C                   EVAL      WFldName = 'MOD'
|||  C                   EVAL      WIcErro = *ON
||+--C                   ENDIF
||   C*
||+--C                   IF        (TPT = *BLANKS) AND (NOT WIcErro)
|||  C                   EVAL      WCodErro = 'PRD0072'
|||  C                   EVAL      WFldName = 'TPT'
|||  C                   EVAL      WIcErro = *ON
||+--C                   ELSEIF    (NOT WIcErro) AND
|||  C                             (TPT <> 'P') AND (TPT <> 'O')
|||  C                   EVAL      WCodErro = 'PRD0083'
|||  C                   EVAL      WFldName = 'TPT'
|||  C                   EVAL      WIcErro = *ON
||+--C                   ENDIF
||   C*
||+--C                   IF        (CDCAE = *BLANKS) AND (NOT WIcErro)
|||  C                   EVAL      WCodErro = 'PRD0067'
|||  C                   EVAL      WFldName = 'CDCAE'
|||  C                   EVAL      WIcErro = *ON
||+--C                   ELSEIF    (NOT WIcErro)
|||  C                   MOVE      *BLANKS       KCAE             12
|||  C                   MOVEL     20            WKCAE             8
|||  C                   MOVE      CDCAE         WKCAE
|||  C                   MOVEL     WKCAE         KCAE
|||  C     KCAE          CHAIN     TABELAS                            05
|||+-C                   IF        (*IN05)
|||| C                   EVAL      WCodErro = 'PRD0023'
|||  C                   EVAL      WFldName = 'CDCAE'
|||  C                   EVAL      WIcErro = *ON
|||+-C                   ENDIF
||+--C                   ENDIF
||   C*
||+--C                   IF        (MODALI <> 'FP') AND (NOT WIcErro)
|||  C                             AND (CTLCAE = *BLANKS)
|||  C                   EVAL      WCodErro = 'PRD0023'
|||  C                   EVAL      WFldName = 'MODALI'
|||  C                   EVAL      WIcErro = *ON
||+--C                   ENDIF
||   C*
||+--C                   IF        (CR1 < 1) OR (CR1 > 2)
|||  C                             AND (NOT WIcErro)
|||  C                   EVAL      WCodErro = 'PRD0075'
|||  C                   EVAL      WFldName = 'CR1'
|||  C                   EVAL      WIcErro = *ON
||+--C                   ENDIF
||   C*
||+--C                   IF        (CR1 = 2) AND (MOD <> 'V')
|||  C                             AND (NOT WIcErro)
|||  C                   EVAL      WFldName = 'CR1'
|||  C                   EVAL      WCodErro = 'PRD0070'
|||  C                   EVAL      WIcErro = *ON
||+--C                   END
|+---C                   END
+----C                   ENDSR
     C*====================================================================
     C* Subrotina.: SR060
     C* Objectivo.: Grupo de validações Comuns
     C*====================================================================
+----C     SR060         BEGSR
|    C                   EVAL      WIcErro = *OFF
|    C*   Validacao de  Campos Especificos do RAMO = 55      PC 30/10/91
|+---C                   IF        ((RR=25) OR (RR=35) OR (RR=55))
||+--C                   IF        (CLASS<>'A') AND (CLASS<>'B') AND
|||  C                             (CLASS <>'C') AND (CLASS<>'C')
|||  C                   EVAL      WCodErro = 'PRD0019'
|||  C                   EVAL      WFldName = 'CLASS'
|||  C                   EVAL      WIcErro  = *ON
||+--C                   ENDIF
||   C*
||+--C                   IF        ((PERDCQ<>'S') AND (PERDCQ<>'N'))
|||  C                             AND (NOT WIcErro)
|||  C                   EVAL      WCodErro = 'PRD0031'
|||  C                   EVAL      WFldName = 'PERDCQ'
|||  C                   EVAL      WIcErro  = *ON
||+--C                   ENDIF
||   C*
||+--C                   IF        (PERDCQ='N')
|||+-C                   IF        (TPERD<>' ') AND (NOT WIcErro)
|||| C                   EVAL      WCodErro = 'PRD0079'
|||  C                   EVAL      WFldName = 'PERDCQ'
|||| C                   EVAL      WIcErro  = *ON
|||+-C                   ENDIF
|||  C*
|||+-C                   IF        (PPERDI <> 00) AND (NOT WIcErro)
|||| C                   EVAL      WCodErro = 'PRD0078'
|||  C                   EVAL      WFldName = 'PPERDI'
|||| C                   EVAL      WIcErro  = *ON
|||+-C                   ENDIF
|||  C*
|||+-C                   IF        (ENCFIX <> 0) AND (NOT WIcErro)
|||| C                   EVAL      WCodErro = 'PRD0076'
|||  C                   EVAL      WFldName = 'ENCFIX'
|||| C                   EVAL      WIcErro  = *ON
|||+-C                   ENDIF
|||  C*
|||+-C                   IF        (PINDMN <> *ZEROS) AND (NOT WIcErro)
|||| C                   EVAL      WCodErro = 'PRD0077'
|||  C                   EVAL      WFldName = 'PINDMN'
|||| C                   EVAL      WIcErro  = *ON
|||+-C                   ENDIF
||+--C                   ENDIF
||   C
||+--C                   IF        (PERDCQ = 'S') AND (TPERD<> 'I')
|||  C                             AND (TPERD <> 'F') AND (NOT WIcErro)
|||  C                   EVAL      WCodErro = 'PRD0080'
|||  C                   EVAL      WFldName = 'TPERD'
|||  C                   EVAL      WIcErro  = *ON
||+--C                   ENDIF
||   C*
||+--C                   IF        (TPERD='I')
|||+-C                   IF        (PPERDI <> 20) AND (NOT WIcErro) AND
|||| C                             (PPERDI <> 25) AND (PPERDI <> 30)
|||| C                   EVAL      WCodErro = 'PRD0036'
|||  C                   EVAL      WFldName = 'PPERDI'
|||| C                   EVAL      WIcErro  = *ON
|||+-C                   ENDIF
|||  C*
|||+-C                   IF        (ENCFIX <> *ZEROS) AND (NOT WIcErro)
|||| C                   EVAL      WCodErro = 'PRD0037'
|||  C                   EVAL      WFldName = 'ENCFIX'
|||| C                   EVAL      WIcErro  = *ON
|||+-C                   ENDIF
|||  C*
|||+-C                   IF        (PINDMN <> *ZEROS) AND (NOT WIcErro)
|||| C                   EVAL      WCodErro = 'PRD0038'
|||  C                   EVAL      WFldName = 'PINDMN'
|||| C                   EVAL      WIcErro  = *ON
|||+-C                   ENDIF
||+--C                   ENDIF
||   C*
||+--C                   IF        (TPERD = 'F') AND (NOT WIcErro)
|||+-C                   IF        (ENCFIX = *ZEROS)
|||| C                   EVAL      WCodErro = 'PRD0043'
|||  C                   EVAL      WFldName = 'TPERD'
|||| C                   EVAL      WIcErro  = *ON
|||+-C                   ENDIF
|||  C*
|||+-C                   IF        (PINDMN = *ZEROS) AND (NOT WIcErro)
|||| C                   EVAL      WCodErro = 'PRD0081'
|||  C                   EVAL      WFldName = 'PINDMN'
|||| C                   EVAL      WIcErro  = *ON
|||+-C                   ENDIF
|||  C*
|||+-C                   IF        (PINDMN <> 3) AND (PINDMN <> 6)
|||| C                             AND (PINDMN <> 12) AND (NOT WIcErro)
|||| C                   EVAL      WCodErro = 'PRD0042'
|||  C                   EVAL      WFldName = 'PINDMN'
|||| C                   EVAL      WIcErro  = *ON
|||+-C                   ENDIF
||+--C                   ENDIF
|+---C                   ENDIF
|    C*
|+---C                   IF        (JMM1 > *ZEROS) AND (NOT WIcErro)
||   C     JMM1          CHAIN     MCORNP                             05
||+--C                   IF        (*IN05)
|||  C                   EVAL      WCodErro = 'PRD0059'
|||  C                   EVAL      WFldName = 'JMM1'
|||  C                   EVAL      WIcErro  = *ON
||+--C                   ENDIF
||    * Controla se activo (PF 12.02.08)
||+--C                   IF        (ALTCR = 'S') AND (NOT WIcErro)
|||  C                   EVAL      WCodErro = 'PRD0048'
|||  C                   EVAL      WFldName = 'ALTCR'
|||  C                   EVAL      WIcErro  = *ON
||+--C                   END
||    *
||+--C                   IF        (TMEDCR <> 1) AND (NOT WIcErro)
|||  C                   EVAL      WCodErro = 'PRD0046'
|||  C                   EVAL      WFldName = 'TMEDCR'
|||  C                   EVAL      WIcErro  = *ON
||+--C                   END
||    *
||+--C                   IF        (JMC1 = *ZEROS) AND (JMM1 <> 418)
|||  C                             AND (NOT WIcErro)
|||  C                   EVAL      WCodErro = 'PRD0034'
|||  C                   EVAL      WFldName = 'JMC1'
|||  C                   EVAL      WIcErro  = *ON
||+--C                   END
|+---C                   ELSEIF    (NOT WIcErro)
||+--C     JMC1          IFGT      *ZEROS
|||  C                   EVAL      WCodErro = 'PRD0044'
|||  C                   EVAL      WFldName = 'JMC1'
|||  C                   EVAL      WIcErro  = *ON
||+--C                   END
|+---C                   END
|    C*
|+---C                   IF        (JMC1 > COM1) AND (NOT WIcErro)
||   C                   EVAL      WCodErro = 'PRD0029'
|||  C                   EVAL      WFldName = 'JMC1'
||   C                   EVAL      WIcErro  = *ON
|+---C                   END
|    C*
|+---C                   IF        (JMM2 > *ZEROS) AND (WIcErro)
||   C     JMM2          CHAIN     MCORNP                             05
||+--C                   IF        (*IN05)
|||  C                   EVAL      WCodErro = 'PRD0060'
|||  C                   EVAL      WFldName = 'JMM2'
|||  C                   EVAL      WIcErro  = *ON
||+--C                   ENDIF
||   C* Controla se activo (PF 12.02.08)
||+--C                   IF        (ALTCR = 'S') AND (NOT WIcErro)
|||  C                   EVAL      WCodErro = 'PRD0048'
|||  C                   EVAL      WFldName = 'ALTCR'
|||  C                   EVAL      WIcErro  = *ON
||+--C                   END
||    *
||+--C                   IF        (JMC2 = *ZEROS) AND (JMM2 <> 418)
|||  C                             AND (NOT WIcErro)
|||  C                   EVAL      WCodErro = 'PRD0035'
|||  C                   EVAL      WFldName = 'JMC2'
|||  C                   EVAL      WIcErro  = *ON
||+--C                   END
|+---C                   ELSEIF    (NOT WIcErro)
||+--C     JMC2          IFGT      *ZEROS
|||  C                   EVAL      WCodErro = 'PRD0045'
|||  C                   EVAL      WFldName = 'JMC2'
|||  C                   EVAL      WIcErro  = *ON
||+--C                   END
|+---C                   END
|    C*
|+---C                   IF        (JMC2 > COM1) AND (NOT WIcErro)
||   C                   EVAL      WCodErro = 'PRD0029'
|||  C                   EVAL      WFldName = 'JMC2'
||   C                   EVAL      WIcErro  = *ON
|+---C                   END
|    C*
|+---C                   IF        ((JMC1 + JMC2) > COM1)
||   C                             AND (NOT WIcErro)
||   C                   EVAL      WCodErro = 'PRD0029'
|||  C                   EVAL      WFldName = 'JMC1'
||   C                   EVAL      WIcErro  = *ON
|+---C                   END
+----C                   ENDSR
     C*====================================================================
     C* Subrotina.: SR040
     C* Objectivo.: Grupo de validações Comuns
     C*====================================================================
+----C     SR040         BEGSR
|    C                   EVAL      WIcErro = *OFF
|+---C                   IF        (CL01 >= *ZEROS)
||   C                   MOVE      CL01          WRKCL
||   C                   EXSR      VCLAUS
||+--C                   IF        (WIcErro)
|||  C                   EVAL      WCodErro = 'CL01'
|||  C                   EVAL      WFldName = 'OPCAO'
||+--C                   ENDIF
|+---C                   ENDIF
|    C*
|+---C                   IF        (CL02 >= *ZEROS) AND (NOT WIcErro)
||   C                   MOVE      CL02          WRKCL
||   C                   EXSR      VCLAUS
||+--C                   IF        (WIcErro)
|||  C                   EVAL      WCodErro = 'PRD0021'
|||  C                   EVAL      WFldName = 'CL02'
||+--C                   ENDIF
|+---C                   ENDIF
|    C*
|+---C                   IF        (CL03 >= *ZEROS) AND (NOT WIcErro)
||   C                   MOVE      CL03          WRKCL
||   C                   EXSR      VCLAUS
||+--C                   IF        (WIcErro)
|||  C                   EVAL      WCodErro = 'PRD0021'
|||  C                   EVAL      WFldName = 'CL03'
||+--C                   ENDIF
|+---C                   ENDIF
|    C*
|+---C                   IF        (CL04 >= *ZEROS) AND (NOT WIcErro)
||   C                   MOVE      CL04          WRKCL
||   C                   EXSR      VCLAUS
||+--C                   IF        (WIcErro)
|||  C                   EVAL      WCodErro = 'PRD0021'
|||  C                   EVAL      WFldName = 'CL04'
||+--C                   ENDIF
|+---C                   ENDIF
|    C*
|+---C                   IF        (CL05 >= *ZEROS) AND (NOT WIcErro)
||   C                   MOVE      CL05          WRKCL
||   C                   EXSR      VCLAUS
||+--C                   IF        (WIcErro)
|||  C                   EVAL      WCodErro = 'PRD0021'
|||  C                   EVAL      WFldName = 'CL05'
||+--C                   ENDIF
|+---C                   ENDIF
|    C*
|+---C                   IF        (CL06 >= *ZEROS) AND (NOT WIcErro)
||   C                   MOVE      CL06          WRKCL
||   C                   EXSR      VCLAUS
||+--C                   IF        (WIcErro)
|||  C                   EVAL      WCodErro = 'PRD0021'
|||  C                   EVAL      WFldName = 'CL06'
||+--C                   ENDIF
|+---C                   ENDIF
|    C*
|+---C                   IF        (NOT WIcErro)
||+--C                   IF        (RR = 01)
|||+-C                   IF        ((RA = 'S') OR (RA = 'N'))
|||| C                   EVAL      WCodErro = 'PRD0032'
|||| C                   EVAL      WFldName = 'RA'
|||+-C                   ENDIF
|||  C*
|||+-C                   IF        ((RA = 'S') OR (WCL = 0))
|||| C                   EVAL      WCodErro = 'PRD0074'
|||| C                   EVAL      WFldName = 'RA'
|||+-C                   ENDIF
||+--C                   ENDIF
|+---C                   ENDIF
|    C*
|+---C                   IF        (NOT WIcErro)
||+--C                   IF        (TXIND <= *ZEROS)
|||  C                   MOVE      *BLANK        INDEX                           RG 12/03/97
||+--C                   ELSE
|||+-C                   IF        (INDEX <> 'S') AND (INDEX <> 'N')
|||| C                   EVAL      WCodErro = 'PRD0032'
|||| C                   EVAL      WFldName = 'INDEX'
|||+-C                   ENDIF
||+--C                   ENDIF
|+---C                   ENDIF
+----C                   ENDSR
     C*====================================================================
     C* Subrotina.: SR022
     C* Objectivo.: Grupo de validações Comuns
     C*====================================================================
+----C     SR022         BEGSR
|    C                   EVAL      WIcErro = *OFF
|    C* validação do codigo empregado
|+---C                   IF        (EMPREG <> 'S') AND (EMPREG <> ' ')
||   C                   EVAL      WCodErro = 'PRD0024'
||   C                   EVAL      WFldName = 'EMPREG'
||   C                   EVAL      WIcErro  = *ON
|+---C                   END
|    C
|    C* validação do ano do edífico
|+---C                   IF        (ANOEDI <> *ZERO) AND ((ANOEDI < 1800) OR
||   C                             (ANOEDI > WAno)) AND (NOT WIcErro)
||   C                   EVAL      WCodErro = 'PRD0015'
||   C                   EVAL      WFldName = 'ANOEDI'
||   C                   EVAL      WIcErro = *ON
|+---C                   END
|    C
|+---C                   IF        (CPEDI <> *ZEROS) AND (CPEDI < 1000)
||   C                             AND (NOT WIcErro)
||   C                   EVAL      WCodErro = 'PRD0026'
||   C                   EVAL      WFldName = 'CPEDI'
||   C                   EVAL      WIcErro = *ON
|+---C                   END
|    C* validar risco 1
|+---C                   IF        ((CR1 < *ZEROS) OR ((CR1 > 7)
||   C                               AND (CR1 <> 9)))
||   C                             AND (NOT WIcErro)
||   C                   EVAL      WCodErro = 'PRD0028'
||   C                   EVAL      WFldName = 'CR1'
||   C                   EVAL      WIcErro = *ON
|+---C                   ENDIF
|    C* validar capital + risco
|+---C                   IF        (((CR1 = *ZEROS) AND (CAP1 > *ZEROS)) OR
||   C                             ((CR2 = *ZEROS) AND (CAP2 > *ZEROS)) OR
||   C                             ((CR3 = *ZEROS) AND (CAP3 > *ZEROS)))
||   C                             AND (NOT WIcErro)
||   C                   EVAL      WCodErro = 'PRD0028'
||   C                   EVAL      WFldName = 'CR1'
||   C                   EVAL      WIcErro = *ON
|+---C                   ENDIF
|     *
|    C* validar capital do risco 1 (*IN26)
|+---C                   IF        (CAP1 = *ZEROS) AND (NOT WIcErro)
||   C                   EVAL      WCodErro = 'PRD0066'
||   C                   EVAL      WFldName = 'CAP1'
||   C                   EVAL      WIcErro = *ON
|+---C                   ENDIF
|    C*
|+---C                   IF        (NOT WIcErro)
||   C                   EXSR      VCAPT
||+--C                   IF        (WIcErro)
|||  C                   EVAL      WCodErro = 'PRD0087'
|||  C                   EVAL      WFldName = 'CAPTOT'
||+--C                   ENDIF
|+---C                   ENDIF
|    C* validar taxa do risco 1
|+---C                   IF        (((RR <> 15) AND (TX1 = *ZEROS)) OR
||   C                             ((RR = 15) AND (TX1AT = *ZEROS)))
||   C                             AND (NOT WIcErro)
||   C                   EVAL      WCodErro = 'PRD0065'
||   C                   EVAL      WFldName = 'TX1AT'
||   C                   EVAL      WIcErro = *ON
|+---C                   ENDIF
|    C
|+---C                   IF        (PR1 <= *ZEROS) AND (NOT WIcErro)
||   C                   EVAL      WCodErro = 'PRD0069'
||   C                   EVAL      WFldName = 'PR1'
||   C                   EVAL      WIcErro = *ON
|+---C                   ENDIF
|    C*
|+---C                   IF        (NOT WIcErro)
||   C                   Z-ADD     0             PRTOT             9 2
||   C     PR1           ADD       PR2           PRTOT
||   C     PRTOT         ADD       PR3           PRTOT
||+--C                   IF        (PRSACT <> PRTOT)
|||  C                   EVAL      WIcErro  = *ON
|||  C                   EVAL      WCodErro = 'PRD0088'
|||  C                   EVAL      WFldName = 'PRSACT'
||+--C                   ENDIF
|+---C                   ENDIF
|    C*
|+---C                   IF        (NOT WIcErro)
||   C                   EXSR      VNASC
||+--C                   IF        (WIcErro)
|||  C                   EVAL      WCodErro = 'PRD0085'
|||  C                   EVAL      WFldName = 'NASCX'
||+--C                   ENDIF
|+---C                   ENDIF
+----C                   ENDSR
     C*====================================================================
     C* Subrotina.: SR008
     C* Objectivo.: Grupo de validações Comuns
     C*====================================================================
+----C     SR008         BEGSR
|    C                   EVAL      WIcErro = *OFF
|+---C                   IF        (CEMIS < 1) OR (CEMIS > 3)
||   C                   EVAL      WCoderro = 'PRD0010'
|||  C                   EVAL      WFldName = 'CEMIS'
||   C                   EVAL      WIcErro = *ON
|+---C                   ENDIF
|    C*
|+---C                   IF        ((ISEN < *ZEROS) OR ((ISEN > 3)
||   C                             AND (ISEN < 6)) OR (ISEN > 7))
||   C                             AND (NOT WIcErro)
||   C                   EVAL      WCoderro = 'PRD0010'
|||  C                   EVAL      WFldName = 'ISEN'
||   C                   EVAL      WIcErro = *ON
|+---C                   ENDIF
|    C*
|+---C                   IF        (NOT WIcErro)
||   C     CONC          CHAIN     FCONC                              34
||+--C                   IF        (*IN34)
|||  C                   EVAL      WCodErro = 'PRD0025'
|||  C                   EVAL      WFldName = 'CONC'
|||  C                   EVAL      WIcErro = *ON
||+--C                   ENDIF
|+---C                   ENDIF
|    C*
|+---C                   IF        (NOT WIcErro)
||   C                   EXSR      VMED1
|+---C                   ENDIF
|    C*
|+---C                   IF        (NOT WIcErro)
||   C                   EXSR      VCOM1
||+--C                   IF        (WIcErro)
|||  C                   EVAL      WCodErro = 'PRD0084'
|||  C                   EVAL      WFldName = 'COM1'
||+--C                   ENDIF
|+---C                   ENDIF
|    C*
|+---C                   IF        (NOT WIcErro)
||   C                   EXSR      VMED2
||+--C                   IF        (WIcErro)
|||  C                   EVAL      WCodErro = 'PRD0018'
|||  C                   EVAL      WFldName = 'MED2'
||+--C                   ENDIF
|+---C                   ENDIF
|    C*
|    C*--> Se o Credor Hipotecário estiver preenchido então valida
|+---C                   IF        (NOT WIcErro)
||   C                   EVAL      DSCCRHIP = *BLANKS
||+--C     CRHIP         IFGT      *ZEROS
|||  C                   EVAL      DSCCRHIP = $getDescCred(CRHIP)
|||+-C                   IF        *IN94
|||| C                   EVAL      WCodErro = 'PRD0020'
|||  C                   EVAL      WFldName = 'CRHIP'
|||+-C                   END
||+--C                   END
|+---C                   ENDIF
|    C*
|+---C                   IF        (AVISN <> 'S') AND (AVISN <> 'N')
||   C                             AND (NOT WIcErro)
||   C                   EVAL      WCodErro = 'PRD0039'
|||  C                   EVAL      WFldName = 'AVISN'
||   C                   EVAL      WIcErro = *ON
|+---C                   ENDIF
+----C                   ENDSR
     C*====================================================================
     C* Subrotina.: SR001
     C* Objectivo.: Grupo de validações Comuns
     C*====================================================================
+----C     SR001         BEGSR
|    C                   EVAL      WIcErro = *OFF
|    C*---> Obter a descrição postal
|+---C                   IF        (CPLCB1 <> *ZEROS)
||   C     CPLCB1        CHAIN     MPOSTNP                            90
||+--C                   IF        (NOT *IN90)
|||  C                   MOVEL     DCPOST        CPLCB2
||+--C                   ELSE
|||  C                   MOVEL     *BLANKS       CPLCB2
|||  C                   EVAL      WCodErro = 'PRD0027'
|||  C                   EVAL      WFldName = 'CPLCB1'
|||  C                   EVAL      WIcErro  = *ON
||+--C                   ENDIF
|+---C                   ELSE
||   C                   MOVEL     *BLANKS       CPLCB2
|+---C                   END
|    C*
|+---C                   IF        ((IM < *ZEROS) OR (IM > 1))
||   C                             AND (NOT WIcErro)
||   C                   EVAL      WCoderro = 'PRD0010'
|||  C                   EVAL      WFldName = 'IM'
||   C                   EVAL      WIcErro = *ON
|+---C                   ENDIF
|    C*---> Validar o dia de inicio
|+---C                   IF        ((DDINIC < 1) OR (DDINIC > 31))
||   C                             AND (NOT WIcErro)
||   C                   EVAL      WCoderro = 'PRD0010'
|||  C                   EVAL      WFldName = 'DDINIC'
||   C                   EVAL      WIcErro = *ON
|+---C                   ENDIF
|    C*---> Validar o mês de inicio
|+---C                   IF        ((MMINIC < 1) OR (MMINIC > 12))
||   C                             AND (NOT WIcErro)
||   C                   EVAL      WIcErro = *ON
||   C                   EVAL      WCoderro = 'PRD0010'
||   C                   EVAL      WFldName = 'MMINIC'
|+---C                   ENDIF
|    C*
|+---C                   IF        ((((OPCAO=OPR#Inserir) AND (AAINI<(WANO-1)))
||   C                             OR ((OPCAO<>OPR#Inserir) AND (AAINI<1910)))
||   C                              OR (AAINI > (WAno+1)))
||   C                             AND (NOT WIcErro)
||   C                   EVAL      WCoderro = 'PRD0010'
|||  C                   EVAL      WFldName = 'AAINI'
||   C                   EVAL      WIcErro = *ON
|+---C                   ENDIF
|    C*
|+---C                   IF        (DUR <> 1) AND (NOT WIcErro)
||   C                   EVAL      WCoderro = 'PRD0010'
|||  C                   EVAL      WFldName = 'DUR'
||   C                   EVAL      WIcErro = *ON
|+---C                   ENDIF
|    C*
|+---C                   IF        ((DDFIM < *ZEROS) OR (DDFIM > 31))
||   C                             AND (NOT WIcErro)
||   C                   EVAL      WCoderro = 'PRD0010'
|||  C                   EVAL      WFldName = 'DDFIM'
||   C                   EVAL      WIcErro = *ON
|+---C                   ENDIF
|    C*
|+---C                   IF        ((MMFIM < *ZEROS) OR (MMFIM > 31))
||   C                             AND (NOT WIcErro)
||   C                   EVAL      WCoderro = 'PRD0010'
|||  C                   EVAL      WFldName = 'MMFIM'
||   C                   EVAL      WIcErro = *ON
|+---C                   ENDIF
|    C
|    C*---> Validar a data de Fim
|+---C                   IF        (NOT WIcErro)
||   C                   EXSR      VDTFIM
||+--C                   IF        (WIcErro)
|||  C                   EVAL      WCodErro = 'PRD0085'
|||  C                   EVAL      WFldName = 'DTFIM'
|||  C                   EVAL      WIcErro = *ON
||+--C                   ENDIF
|+---C                   ENDIF
|    C
|    C*--> Validar o Fraccionamento
|+---C                   IF        (NOT WIcErro)
||   C                   EXSR      VFRAC
||+--C                   IF        (WIcErro)
|||  C                   EVAL      WCodErro = 'PRD0025'
|||  C                   EVAL      WFldName = 'FRAC'
|||  C                   EVAL      WIcErro = *ON
||+--C                   ENDIF
|+---C                   ENDIF
+----C                   ENDSR
     C*====================================================================
     C* Subrotina.: ValOpcoes
     C* Objectivo.: Validar as opções do Ecrã de Opções Extras
     C*====================================================================
+----C     ValOpcoes     BEGSR
|    C                   EVAL      WIcErro = *OFF
|    C*---> Validar Coberturas Facultativas
|+---C                   IF        ((CF01 <> 'S') AND (CF01 <> ' ')) OR
||   C                             ((CF02 <> 'S') AND (CF02 <> ' ')) OR
||   C                             ((CF03 <> 'S') AND (CF03 <> ' ')) OR
||   C                             ((CF04 <> 'S') AND (CF04 <> ' ')) OR
||   C                             ((CF05 <> 'S') AND (CF05 <> ' ')) OR
||   C                             ((CF06 <> 'S') AND (CF06 <> ' ')) OR
||   C                             ((CF07 <> 'S') AND (CF07 <> ' ')) OR
||   C                             ((CF08 <> 'S') AND (CF08 <> ' ')) OR
||   C                             ((CF09 <> 'S') AND (CF09 <> ' ')) OR
||   C                             ((CF10 <> 'S') AND (CF10 <> ' ')) OR
||   C                             ((CF11 <> 'S') AND (CF11 <> ' ')) OR
||   C                             ((CF12 <> 'S') AND (CF12 <> ' ')) OR
||   C                             ((CF13 <> 'S') AND (CF12 <> ' ')) OR
||   C                             ((CF21 <> 'S') AND (CF21 <> ' ')) OR
||   C                             ((CF22 <> 'S') AND (CF22 <> ' '))
||   C                   EVAL      WIcErro = *ON
||   C                   EVAL      WCoderro = 'PRD0090'
|||  C                   EVAL      WFldName = 'CF001'
|+---C                   ENDIF
|    C*
|+---C                   IF        (((CF0800 <> 'S') AND (CF0800 <> ' ')) OR
||   C                             ((CF0830 <> 'S') AND (CF0830 <> ' ')))
||   C                             AND (NOT WIcErro)
||   C                   EVAL      WIcErro = *ON
||   C                   EVAL      WCoderro = 'PRD0090'
|||  C                   EVAL      WFldName = 'CD0800'
|+---C                   ENDIF
|    C*
|+---C                   IF        (((CF21 <> ' ') AND (DCF21 = ' ')) OR
||   C                             ((CF21 = ' ') AND (DCF21 <> ' ')))
||   C                             AND (NOT WIcErro)
||   C                   SETOFF                                       96
||   C                   EVAL      WIcErro = *ON
||   C                   EVAL      WCoderro = 'PRD0090'
|||  C                   EVAL      WFldName = 'CF21'
|+---C                   ENDIF
|    C
|    C*---> Validar Condições Especiais
|+---C                   IF        ((CE001 <> 'S') AND (CE001 <> ' ')) OR
||   C                             ((CE002 <> 'S') AND (CE002 <> ' ')) OR
||   C                             ((CE003 <> 'S') AND (CE003 <> ' ')) OR
||   C                             ((CE004 <> 'S') AND (CE004 <> ' '))
||   C                             AND (WIcErro)
||   C                   SETOFF                                       96
||   C                   EVAL      WIcErro = *ON
||   C                   EVAL      WCoderro = 'PRD0090'
|||  C                   EVAL      WFldName = 'CE001'
|+---C                   END
|    C*
|    C*----> Clausulas Aplicáveis
|+---C                   IF        (((CA030 <> 'S') AND (CA030 <> ' ')) OR
||   C                             ((CA031 <> 'S') AND (CA031 <> ' ')) OR
||   C                             ((CA100 <> 'S') AND (CA100 <> ' ')) OR
||   C                             ((CA101 <> 'S') AND (CA101 <> ' ')) OR
||   C                             ((CA102 <> 'S') AND (CA102 <> ' ')))
||   C                             AND (NOT WIcErro)
||   C                   SETOFF                                       96
||   C                   EVAL      WIcErro = *ON
||   C                   EVAL      WCoderro = 'PRD0090'
|||  C                   EVAL      WFldName = 'CA030'
|+---C                   END
|    C*
|    C*--> Cláusulas Diversas
|+---C                   IF        (((CD103 <> 'S') AND (CD103 <> ' ')) OR
||   C                             ((CD130 <> 'S') AND (CD130 <> ' ')) OR
||   C                             ((CD131 <> 'S') AND (CD131 <> ' ')) OR
||   C                             ((CD132 <> 'S') AND (CD132 <> ' ')))
||   C                             AND (NOT WIcErro)
||   C                   SETOFF                                       96
||   C                   EVAL      WIcErro = *ON
||   C                   EVAL      WCoderro = 'PRD0090'
|||  C                   EVAL      WFldName = 'CD103'
|+---C                   END
+----C                   ENDSR
     C
     C*====================================================================
     C* Subrotina.: VDTFIM
     C* Objectivo.: Validar a datas de Fim
     C*====================================================================
+----C     VDTFIM        BEGSR
|    C                   EVAL      WIcErro = *OFF
|+---C                   IF        (DUR = 2)
||+--C                   IF        (DDFIM = *ZEROS) OR (MMFIM = *ZEROS)
|||  C                             OR (AAFI = *ZEROS)
|||  C                   EVAL      WIcErro = *ON
||+--C                   ENDIF
|+---C                   ELSE
||+--C                   IF        (DDFIM<>*ZEROS) OR (MMFIM<>*ZEROS)
|||  C                             OR (AAFI<>*ZEROS)
|||  C                   EVAL      WIcErro = *ON
||+--C                   ENDIF
|+---C                   ENDIF
+----C                   ENDSR
     C*====================================================================
     C* Subrotina.: VFRAC
     C* Objectivo.: Validar o Fraccionamento
     C*====================================================================
+----C     VFRAC         BEGSR
|    C                   EVAL      WIcErro = *OFF
|+---C                   IF        (DUR = 2)
||+--C                   IF        (FRAC <> 5)
|||  C                   EVAL      WIcErro = *ON
||+--C                   ENDIF
|+---C                   ELSE
||+--C                   IF        (FRAC < 1) OR (FRAC > 4)
|||  C                   EVAL      WIcErro = *ON
||+--C                   ENDIF
|+---C                   ENDIF
+----C                   ENDSR
     C*====================================================================
     C* Subrotina.: VMES1
     C* Objectivo.: Validar se o valor inserido no campo  MES1 é válido
     C*====================================================================
+----C     VMES1         BEGSR
|    C                   EVAL      WIcErro = *OFF
|+---C                   IF        (DUR = 2)
||+--C                   IF        (MES1 <> 99)
|||  C                   EVAL      WIcErro = *ON
||+--C                   ENDIF
|+---C                   ELSE
||+--C                   IF        (MES1 <> MMINIC)
|||  C                   EVAL      WIcErro = *ON
||+--C                   ENDIF
|+---C                   ENDIF
+----C                   ENDSR
     C*====================================================================
     C* Subrotina.: VMES13
     C* Objectivo.: Validar se o valor inserido no campo  MES1 é válido
     C*====================================================================
+----C     VMES13        BEGSR
|    C                   EVAL      WIcErro = *OFF
|+---C                   IF        (DUR = 2)
||+--C                   IF        (MES1 <> 99)
|||  C                   EVAL      WIcErro = *ON
||+--C                   ENDIF
|+---C                   ELSE
||+--C                   IF        (MES1 < 1) OR (MES1 > 12)
|||  C                   EVAL      WIcErro = *ON
||+--C                   ENDIF
|+---C                   ENDIF
+----C                   ENDSR
     C*====================================================================
     C* Subrotina.: VCONT1
     C* Objectivo.: Determinar a data do próximo continuado em função
     C*             da data de inicio e fraccionamento, e se o valor
     C*             for diferente do inserido no ecrã então dá DSPMESSAGE
     C*====================================================================
+----C     VCONT1        BEGSR
|    C                   EVAL      WIcErro = *OFF
|    C                   Z-ADD     MMINIC        WKMES             2 0             DUR=1
|    C                   Z-ADD     AAINI         WKANO             4 0
|    C
|+---C                   IF        (DUR = 2) AND
||   C                             (MMCONT <> 99) AND (AACON <> 9999)
||   C                   EVAL      WIcerro = *ON
|+---C                   ELSE
||+--C                   IF        (FRAC = 1)
|||  C                   EVAL      WKMES = MES1
|||  C                   EVAL      WKANO = WKANO + 1
||+--C                   ELSEIF    (FRAC = 2)
|||  C                   EVAL      WKMES = WKMES + 6
||+--C                   ELSEIF    (FRAC = 3)
|||  C                   EVAL      WKMES = WKMES + 3
||+--C                   ELSEIF    (FRAC = 4)
|||  C                   EVAL      WKMES = WKMES + 1
||+--C                   ENDIF
||   C
||+--C                   IF        (WKMES >= 13)
|||  C                   EVAL      WKMES = WKMES - 12
|||  C                   EVAL      WKANO = WKANO + 1
||+--C                   ENDIF
||   C
||+--C                   IF        (MMCONT<>WKMES) AND (AACONT<>WKANO)
|||  C                   EVAL      WIcErro = *ON
||+--C                   ENDIF
|+---C                   ENDIF
+----C                   ENDSR
     C*====================================================================
     C* Subrotina.: VMCONT
     C* Objectivo.: Validar o mês do Continuado
     C*====================================================================
+----C     VMCONT        BEGSR
|    C                   EVAL      WIcErro = *OFF
|+---C                   IF        (DUR <> 2)
||+--C                   IF        (MES1 <> MMCONT)
|||  C                   Z-ADD     MES1          WKMES             2 0             DUR=1
|||  C                   EVAL      WKMES = WKMES + (12 / FRAC)
|||+-C                   IF        (WKMES > 12)
|||| C                   EVAL      WKMES = WKMES - 12
|||+-C                   ENDIF
|||+-C                   IF        (WKMES <> MMCONT)
|||| C                   EVAL      WIcErro = *ON
|||+-C                   ENDIF
||+--C                   ENDIF
|+---C                   ELSE
||+--C                   IF        (MES1 <> 99)
|||  C                   EVAL      WIcErro = *ON
||+--C                   ENDIF
|+---C                   ENDIF
+----C                   ENDSR
     C*====================================================================
     C* Subrotina.: VMED1
     C* Objectivo.:
     C*====================================================================
+----C     VMED1         BEGSR
|    C                   EVAL      WIcErro = *OFF
|    C
|    C* VALIDAR SE MEDIADOR EXISTE EM FICHEIRO
|    C                   MOVEL     0             MED               4 0
|    C                   MOVE      MED1          MED               4 0
|+---C                   IF        (MED <> 031)
||+--C                   IF        (MED < 299)
|||  C                   EVAL      WIcErro = *ON
|||  C                   EVAL      WCodErro = 'PRD0025'
|||  C                   EVAL      WFldName = 'MED1'
||+--C                   ELSE
|||  C     MED           CHAIN     FMED                               35
|||+-C                   IF        *IN35
|||| C                   EVAL      WIcErro = *ON
|||| C                   EVAL      WCodErro = 'PRD0025'
|||  C                   EVAL      WFldName = 'MED1'
|||+-C                   ELSE
|||| C
-----C
     C
     C* VALIDA SE MEDIADOR É SATÉLITE OU CORRETOR
 +---C     MED           IFGE      900
 |+--C                   IF        (JMM1 > *ZEROS) OR (JMC1 > *ZEROS)
 ||  C                   EVAL      WIcErro = *ON
 ||  C                   EVAL      WCodErro = 'PRD0040'
|||  C                   EVAL      WFldName = 'JMM1'
 |+--C                   END
 |+--C                   IF        (JMM2 > *ZEROS) OR (JMC2 > *ZEROS)
 ||  C                   EVAL      WIcErro = *ON
 ||  C                   EVAL      WCodErro = 'PRD0041'
|||  C                   EVAL      WFldName = 'JMM2'
 |+--C                   END
 +---C                   END                                                    <---
     C
-----C
|||| C
|||+-C                   ENDIF
||+--C                   ENDIF
|+---C                   END
+----C                   ENDSR
     C*====================================================================
     C* Subrotina.: VCOM1
     C* Objectivo.:
     C*====================================================================
+----C     VCOM1         BEGSR
|    C                   EVAL      WIcErro = *OFF
|+---C                   IF        (COM1 <= *ZEROS)
||   C                   EVAL      WIcErro = *ON
|+---C                   ENDIF
+----C                   ENDSR
     C*====================================================================
     C* Subrotina.: VMED2
     C* Objectivo.:
     C*====================================================================
+----C     VMED2         BEGSR
|    C                   EVAL      WIcErro = *OFF
|    C                   SETOFF                                       02
|+---C                   IF        (MED2 <> *ZEROS)
||+--C                   IF        (MED1 <> 299)
|||  C                   MOVE      *ZEROS        MED2                            FORÇA ZEROS
||+--C                   ELSEIF    (MED2 <> 1) AND (MED2 <> 6)                   COBRADOR
|||  C                             AND (MED2 <> 8)                               MONTEPIO
|||  C                   EVAL      WIcErro = *ON
||+--C                   ELSE
|||+-C     JMM1          IFNE      *ZEROS
|||  C                   EVAL      WIcErro = *ON
|||+-C                   ENDIF
||+--C                   ENDIF
|+---C                   ENDIF
+----C                   ENDSR
     C*====================================================================
     C* Subrotina.: VTALT
     C* Objectivo.: Validar Tipo de Alteração
     C*====================================================================
+----C     VTALT         BEGSR
|    C                   EVAL      WIcErro = *OFF
|+---C                   IF        (SIT <> 1) AND (TALT = 3)
||   C                   EVAL      WIcErro = *ON
|+---C                   ENDIF
+----C                   ENDSR
     C*====================================================================
     C* Subrotina.: VDALT
     C* Objectivo.: Validar a data de Alteração
     C*====================================================================
+----C     VDALT         BEGSR
|    C                   EVAL      WIcErro = *OFF
|    C                   MOVEL     AAALTX        WK6               6 0
|    C                   MOVE      MMALT         WK6
|    C                   MOVEL     WK6           YMDALT            8 0
|    C                   MOVE      DDALT         YMDALT
|    C                   MOVEL     AACON         WK6               6 0
|    C                   MOVE      MMCONT        WK6
|    C                   MOVEL     WK6           YMDCON            8 0
|    C                   MOVE      DDINIC        YMDCON
|+---C                   IF        (YMDALT > YMDCON)
||   C                   EVAL      WIcErro = *ON
|+---C                   ENDIF
+----C                   ENDSR
     C*====================================================================
     C* Subrotina.: VCAPT
     C* Objectivo.:
     C*====================================================================
+----C     VCAPT         BEGSR
|    C                   EVAL      WIcErro = *OFF
|+---C                   IF        (RR = 02) OR (RR = 04)
||+--C                   IF        (CAPTOT <> CAP1)
|||  C                   EVAL      WIcErro = *ON
||+--C                   ENDIF
|+---C                   ELSEIF    (RR = 05)
||   C                   Z-ADD     0             WCAPT
||+--C                   IF        (CR2<>3) AND (CR2<>4) AND (CR2<>6)
|||  C                   ADD       CAP2          WCAPT
||+--C                   ENDIF
||+--C                   IF        (CR3<>3) AND (CR3<>4) AND (CR3<>6)
|||  C                   ADD       CAP3          WCAPT
||+--C                   ENDIF
||   C                   ADD       CAP1          WCAPT
||+--C                   IF        (CAPTOT <> WCAPT)
|||  C                   EVAL      WIcErro = *ON
||+--C                   ENDIF
|+---C                   ELSEIF    (RR = 55)
||   C                   Z-ADD     0             WCAPT
||+--C                   IF        (CR2 <> 4)
|||  C                   ADD       CAP2          WCAPT
||+--C                   ENDIF
||+--C                   IF        (CR3 <> 4)
|||  C                   ADD       CAP3          WCAPT
||+--C                   ENDIF
||   C                   ADD       CAP1          WCAPT
||+--C                   IF        (CAPTOT <> WCAPT)
|||  C                   EVAL      WIcErro = *ON
||+--C                   ENDIF
|+---C                   ELSEIF    (RR <> 15)
||   C     CAP1          ADD       CAP2          WCAPT            11 0
||   C     WCAPT         ADD       CAP3          WCAPT
||+--C                   IF        (CAPTOT <> WCapt)
|||  C                   EVAL      WIcErro = *ON
||+--C                   ENDIF
|+---C                   ENDIF
+----C                   ENDSR
     C*====================================================================
     C* Subrotina.: VNasc
     C* Objectivo.:
     C*====================================================================
+----C     VNASC         BEGSR
|    C                   EVAL      WICERRO = *OFF
|+---C                   IF        (RR = 02) AND (NASCX <> 9999)
||   C     WAno          SUB       NASCX         WK4               4 0
||+--C                   IF        (WK4 > 75)
|||  C                   EVAL      WICERRO = *ON
||+--C                   ENDIF
|+---C                   ENDIF
+----C                   ENDSR
     C*====================================================================
     C* Subrotina.: VClaus
     C* Objectivo.:
     C* Observação: Substituir esta Rotina por uma Chamada a um
     C*              procedimento de Catálogo, Validação de Clausulas
     C*==================================================================== ****
+----C     VCLAUS        BEGSR
|    C                   EVAL      WIcErro = *OFF
|+---C                   IF        (RR = 01) OR (RR = 21) OR (RR = 31)
||+--C                   IF        (WRKCL < 00) OR ((WRKCL > 43)
|||  C                             AND (WRKCL <> 99))
|||  C                   SETON                                        99                   |
|||  C                   EVAL      WIcErro = *ON
||+--C                   ENDIF                                                     <-------+
|+---C                   END
|    C*
|+---C                   IF        (RR = 02) OR (RR = 22) OR (RR = 32)
||+--C                   IF        (WRKCL < *ZEROS) OR ((WRKCL > 7)
|||  C                             AND (WRKCL <> 99))
|||  C                   SETON                                        99               | | |
|||  C                   EVAL      WIcErro = *ON
||+--C                   ENDIF                                                     <-------+
|+---C                   END
|    C*
|+---C     RR            IFEQ      03
||+--C                   IF        (WRKCL < *ZEROS) OR ((WRKCL <> 1)
|||  C                             AND (WRKCL <> 60) AND (WRKCL <> 64))
|||  C                   SETON                                        99
|||  C                   EVAL      WIcErro = *ON
||+--C                   ENDIF
|+---C                   END
|    C*
|+---C     RR            IFEQ      04
||+--C                   IF        (WRKCL < *ZEROS) OR ((WRKCL > 77)
|||  C                             AND (WRKCL <> 99))
|||  C                   SETON                                        99               | | |
|||  C                   EVAL      WIcErro = *ON
||+--C                   ENDIF                                                     <-------+
|+---C                   END
|    C*
|+---C     RR            IFEQ      05
||+--C                   IF        (WRKCL < *ZEROS) OR ((WRKCL > 7)
|||  C                             AND (WRKCL <> 99))
|||  C                   SETON                                        99               | | |
|||  C                   EVAL      WIcErro = *ON
||+--C                   ENDIF                                                     <-------+
|+---C                   END
|    C*
|+---C                   IF        (RR = 06) OR (RR = 26) OR (RR = 36)
||+--C                   IF        (WRKCL < *ZEROS) OR ((WRKCL > 41)
|||  C                             AND (WRKCL < 93)) OR (WRKCL > 99)
|||  C                   SETON                                        99               | | |
|||  C                   EVAL      WIcErro = *ON
||+--C                   ENDIF                                                     <-------+
|+---C                   ENDIF
|    C*
|+---C     RR            IFEQ      15
||+--C                   IF        (WRKCL < *ZEROS) OR ((WRKCL > 8)
|||  C                             AND (WRKCL <> 99))
|||  C                   SETON                                        99               | | |
|||  C                   EVAL      WIcErro = *ON
||+--C                   ENDIF                                                     <-------+
|+---C                   END
|    C*
|+---C     RR            IFEQ      17
||+--C                   IF        (WRKCL < *ZEROS) OR ((WRKCL > 6)
|||  C                             AND (WRKCL <> 99))
|||  C                   SETON                                        99               | | |
|||  C                   EVAL      WIcErro = *ON
||+--C                   ENDIF                                                     <-------+
|+---C                   END
|    C*
|+---C     RR            IFEQ      19
||+--C                   IF        (WRKCL < *ZEROS) OR ((WRKCL > 13)
|||  C                             AND (WRKCL <> 90) AND (WRKCL <> 99))
|||  C                   SETON                                        99               | | |
|||  C                   EVAL      WIcErro = *ON
||+--C                   ENDIF                                                     <-------+
|+---C                   END
|    C*
|+---C                   IF        (RR = 25) OR (RR = 35) OR (RR = 55)
||+--C                   IF        (WRKCL < *ZEROS) OR ((WRKCL > 14)
|||  C                             AND (WRKCL <> 99))
|||  C                   SETON                                        99               | | |
|||  C                   EVAL      WIcErro = *ON
||+--C                   ENDIF                                                     <-------+
|+---C                   ENDIF
+----C                   ENDSR
     C*====================================================================
     C* Subrotina.: BEGS03
     C* Objectivo.: Limpar campos para o Ecrãn de Criação de Apólices
     C*====================================================================
+----C     BEGS03        BEGSR
|    C                   MOVE      'N'           RECS
|    C                   MOVE      *BLANK        LCB
|    C                   Z-ADD     0             CPLCB1
|    C                   MOVE      *BLANK        CPLCB2
|    C                   Z-ADD     0             TALT
|    C                   Z-ADD     0             IM
|    C                   Z-ADD     0             SIT
|    C                   Z-ADD     0             DDINIC
|    C                   Z-ADD     0             MMINIC
|    C                   Z-ADD     0             AAINIC
|    C                   Z-ADD     0             AAINI
|    C                   Z-ADD     0             DUR
|    C                   Z-ADD     0             DDFIM
|    C                   Z-ADD     0             MMFIM
|    C                   Z-ADD     0             AAFIM
|    C                   Z-ADD     0             AAFI
|    C                   Z-ADD     0             FRAC
|    C                   Z-ADD     0             MES1
|    C                   Z-ADD     0             MACONT
|    C                   Z-ADD     0             MMCONT
|    C                   Z-ADD     0             AACONT
|    C                   Z-ADD     0             AACON
|    C                   Z-ADD     0             CEMIS
|    C                   Z-ADD     0             ISEN
|    C                   Z-ADD     0             CONC
|    C                   Z-ADD     0             MED1
|    C                   Z-ADD     0             COM1
|    C                   Z-ADD     0             MED2
|    C                   Z-ADD     0             COM2
|    C                   Z-ADD     0             CR1
|    C                   Z-ADD     0             CAP1
|    C                   Z-ADD     0             TX1
|    C                   Z-ADD     0             TX1AT
|    C                   Z-ADD     0             PR1
|    C                   Z-ADD     0             CR2
|    C                   Z-ADD     0             CAP2
|    C                   Z-ADD     0             TX2
|    C                   Z-ADD     0             PR2
|    C                   Z-ADD     0             CR3
|    C                   Z-ADD     0             CAP3
|    C                   Z-ADD     0             TX3
|    C                   Z-ADD     0             PR3
|    C                   Z-ADD     0             CAPTOT
|    C                   Z-ADD     0             TXMED
|    C                   Z-ADD     0             TXMAT
|    C                   Z-ADD     0             PRSACT
|    C                   Z-ADD     0             FRANQO
|    C                   MOVE      *BLANK        LRPS
|    C                   Z-ADD     0             TXIND
|    C                   MOVE      *BLANK        INDEX
|    C                   Z-ADD     0             TXSNB
|    C                   MOVE      *BLANK        NASC
|    C                   Z-ADD     0             NASCX
|    C                   Z-ADD     0             PROFSL
|    C                   Z-ADD     0             CGRUPO
|    C                   Z-ADD     0             NPESS
|    C                   MOVE      *BLANK        RA
|    C                   MOVE      *BLANK        NATUR
|    C                   MOVE      *BLANK        CLASS
|    C                   MOVE      *BLANK        PERDCQ
|    C                   MOVE      *BLANK        TPERD
|    C                   Z-ADD     0             PPERDI
|    C                   Z-ADD     0             ENCFIX
|    C                   Z-ADD     0             PINDMN
|    C                   Z-ADD     0             PFRC
|    C                   Z-ADD     0             CL01
|    C                   Z-ADD     0             CL02
|    C                   Z-ADD     0             CL03
|    C                   Z-ADD     0             CL04
|    C                   Z-ADD     0             CL05
|    C                   Z-ADD     0             CL06
|    C                   MOVE      *BLANK        OBS1
|    C                   MOVE      *BLANK        OBS2
|    C*
|    C                   Z-ADD     *ZEROS        JMM1
|    C                   Z-ADD     *ZEROS        JMC1
|    C                   Z-ADD     *ZEROS        JMM2
|    C                   Z-ADD     *ZEROS        JMC2
|    C*
|    C                   MOVEL     *BLANKS       CDCAE
|    C                   MOVE      *BLANKS       MOD
|    C                   MOVE      *BLANKS       NOMENT
|    C                   Z-ADD     0             CRHIP
|    C************************************************************************
|    C                   MOVE      *BLANK        CONDOM
|    C                   MOVE      *BLANK        NEWPRD
|    C                   MOVE      *BLANK        ESTCON
|    C                   MOVE      *BLANK        TIPCON
|    C* limpar coberturas facultativas
|    C                   MOVE      *BLANK        CF01
|    C                   MOVE      *BLANK        CF02
|    C                   MOVE      *BLANK        CF03
|    C                   MOVE      *BLANK        CF04
|    C                   MOVE      *BLANK        CF05
|    C                   MOVE      *BLANK        CF06
|    C                   MOVE      *BLANK        CF07
|    C                   MOVE      *BLANK        CF08
|    C                   MOVE      *BLANK        CF0800
|    C                   MOVE      *BLANK        CF0830
|    C                   MOVE      *BLANK        CF09
|    C                   MOVE      *BLANK        CF10
|    C                   MOVE      *BLANK        CF11
|    C                   MOVE      *BLANK        CF12
|    C                   MOVE      *BLANK        CF13
|    C                   MOVE      *BLANK        CF21
|    C                   MOVE      *BLANK        DCF21
|    C                   MOVE      *BLANK        CF22
|    C* limpar condições especiais
|    C                   MOVE      *BLANK        CE001
|    C                   MOVE      *BLANK        CE002
|    C                   MOVE      *BLANK        CE003
|    C* limpar clausulas aplicaveis
|    C                   MOVE      *BLANK        CA030
|    C                   MOVE      *BLANK        CA031
|    C                   MOVE      *BLANK        CA100
|    C                   MOVE      *BLANK        CA101
|    C                   MOVE      *BLANK        CA102
|    C* limpar clausulas diversas
|    C                   MOVE      *BLANK        CD103
|    C                   MOVE      *BLANK        CD130
|    C                   MOVE      *BLANK        CD131
|    C                   MOVE      *BLANK        CD132
|    C                   MOVE      *BLANK        CD133
|    C                   MOVE      *BLANK        CD134
|    C                   MOVE      *BLANK        CD135
|    C                   MOVE      *BLANK        CD136
|    C                   MOVE      *BLANK        CD137
|    C                   EVAL      MED1 = 299
|    C                   EVAL      COM1 = 17.916
+----C                   ENDSR
     C*====================================================================
     C* Subrotina.: BEGS04
     C* Objectivo.: Formata campos do Base para Display
     C*====================================================================
+----C     BEGS04        BEGSR
|    C                   MOVE      *BLANKS       NOMENT
|    C                   EVAL      DSCCRHIP = *BLANKS
|+---C     CRHIP         IFGT      *ZEROS
||   C                   EVAL      DSCCRHIP = $getDescCred(CRHIP)
|+---C                   END
|    C*
|+---C     DTINIC        IFGT      *ZEROS
||   C                   MOVE      AAINIC        AAINI
||+--C     AAINIC        IFGT      23
|||  C                   MOVEL     19            AAINI
||+--C                   ELSE
|||  C                   MOVEL     20            AAINI
||+--C                   END
|+---C                   ELSE
||   C                   MOVE      *ZEROS        AAINI
|+---C                   END
|    C*
|+---C     DTFIM         IFGT      *ZEROS
||   C                   MOVE      AAFIM         AAFI
||+--C     AAFIM         IFGT      40
|||  C                   MOVEL     19            AAFI
||+--C                   ELSE
|||  C                   MOVEL     20            AAFI
||+--C                   END
|+---C                   ELSE
||   C                   MOVE      *ZEROS        AAFI
|+---C                   END
|    C*
|+---C     MMCONT        IFGT      *ZEROS
||   C                   MOVE      AACONT        AACON
||+--C     MACONT        IFEQ      9999
|||  C                   MOVEL     99            AACON
||+--C                   ELSE
|||+-C     AACONT        IFGT      60
|||| C                   MOVEL     19            AACON
|||+-C                   ELSE
|||| C                   MOVEL     20            AACON
|||+-C                   END
||+--C                   END
|+---C                   ELSE
||   C                   MOVE      *ZEROS        AACON
|+---C                   END
|    C*
|+---C     MMALT         IFGT      *ZEROS
||   C                   MOVE      AAALT         AAALTX
||+--C     AAALT         IFGT      40
|||  C                   MOVEL     19            AAALTX
||+--C                   ELSE
|||  C                   MOVEL     20            AAALTX
||+--C                   END
|+---C                   ELSE
||   C                   MOVE      *ZEROS        AAALTX
|+---C                   END
|    C*
|+---C                   IF        (RR = 02)                                    <----------
||   C                   MOVE      *ZEROS        NASCX                                    |
||+--C                   IF        NASC = 'XX'                                  <-------- |
|||  C                   MOVE      9999          NASCX                                  | |
||+--C                   ELSE                                                           | |
|||+-C                   IF        (NASCX > 10)                                 <------ | |
|||| C                   MOVEL     19            NASCX                                | | |
|||+-C                   ELSE                                                         * | |
|||| C                   MOVEL     20            NASCX                                | | |
|||+-C                   END                                                    <------ | |
||+--C                   END                                                    <------ | |
|+---C                   ELSEIF    (RR = 06)                                    <-------- |
||   C                   MOVE      NASC          NASCX                                  | |
|+---C                   ENDIF
+----C                   ENDSR
     C*====================================================================
     C* Subrotina.: BEGS05
     C* Objectivo.: Formata campos do Display para o Base
     C*====================================================================
+----C     BEGS05        BEGSR
|    C                   MOVE      AAINI         AAINIC
|    C                   MOVE      AAFI          AAFIM
|    C                   MOVE      AACON         AACONT
|    C                   MOVE      AAALTX        AAALT
|+---C     NASCX         IFEQ      9999
||   C                   MOVE      'XX'          NASC
|+---C                   ELSE
||   C                   MOVE      NASCX         NASC
|+---C                   END
+----C                   ENDSR
     C
     C*====================================================================
     C* Subrotina.: SR094
     C* Objectivo.: Calculo de Comissão (94)
     C*====================================================================
+----C     SR094         BEGSR
|+---C     TALT          IFEQ      0                                            apolice nova
||   C     RR            LOOKUP    TABRR         TABPC                    41
||+--C                   IF        *IN41
|||  C* calcular % de comissões SEM fenomenos sismicos
|||  C                   MOVE      TABPC         COMISS
|||  C                   EXSR      COMSFS
||+--C                   ENDIF
||   C* ACIDENTES DE TRABALHO
||+--C                   IF        (RR = 15) AND (TPT = 'P')
|||  C* calcular % de comissões COM fenomenos sismicos
|||  C                   EXSR      COMCFS
||+--C                   END
||   C* INCÊNDIO
||+--C     RR            IFEQ      01
|||  C                   Z-ADD     1             X                 1 0
|||  C     16            LOOKUP    CL(X)                                  99
|||+-C                   IF        (*IN99)
|||| C* calcular % de comissões COM fenomenos sismicos
|||| C                   EXSR      COMCFS
|||+-C                   ENDIF
||+--C                   END
||   C* HABITAÇÃO
||+--C     RR            IFEQ      05
|||  C                   Z-ADD     1             X                 1 0
|||  C     01            LOOKUP    CL(X)                                  99
|||+-C                   IF        (*IN99)
|||| C* calcular % de comissões COM fenomenos sismicos
|||| C                   EXSR      COMCFS
|||+-C                   ENDIF
||+--C                   END
||   C* PERDA DE LUCROS
||+--C     RR            IFEQ      06
|||  C                   Z-ADD     1             X                 1 0
|||  C     16            LOOKUP    CL(X)                                  99
|||+-C                   IF        (*IN99)
|||| C* calcular % de comissões COM fenomenos sismicos
|||| C                   EXSR      COMCFS
|||+-C                   ENDIF
||+--C                   END
||   C* MERCANTILE
||+--C     RR            IFEQ      55
|||  C                   Z-ADD     1             X                 1 0
|||  C     01            LOOKUP    CL(X)                                  99
|||+-C                   IF        (*IN99)
|||| C* calcular % de comissões COM fenomenos sismicos
|||| C                   EXSR      COMCFS
|||+-C                   ENDIF
||+--C                   END
|+---C                   END
+----C                   ENDSR
     C
     C*====================================================================
     C* Subrotina.: COMSFS
     C* Objectivo.: Cálculo de Comissões sem fenomenos sismicos
     C*====================================================================
+----C     COMSFS        BEGSR
|+---C     ISEN          IFEQ      1
||   C     CORR1         MULT(H)   1.20          CORR              5 3
||   C     ANGA1         MULT(H)   1.20          ANGA              5 3
||   C     COBR1         MULT(H)   1.20          COBR              5 3
|+---C                   ELSE
||   C                   Z-ADD     CORR1         CORR
||   C                   Z-ADD     ANGA1         ANGA
||   C                   Z-ADD     COBR1         COBR
|+---C                   END
|    C*
|    C                   EXSR      CCCF
+----C                   ENDSR
     C*====================================================================
     C* Subrotina.: COMCFS
     C* Objectivo.: Calculo de comissões com fenómenos sismicos
     C*====================================================================
+----C     COMCFS        BEGSR
|+---C     ISEN          IFEQ      1
||   C     CORR2         MULT(H)   1.20          CORR
||   C     ANGA2         MULT(H)   1.20          ANGA
||   C     COBR2         MULT(H)   1.20          COBR
|+---C                   ELSE
||   C                   Z-ADD     CORR2         CORR
||   C                   Z-ADD     ANGA2         ANGA
||   C                   Z-ADD     COBR2         COBR
|+---C                   END
|    C*
|    C                   EXSR      CCCF
+----C                   ENDSR
     C*====================================================================
     C* Subrotina.: CCFS
     C* Objectivo.: Carregar comissões nos campos do ficheiro
     C*====================================================================
+----C     CCCF          BEGSR
|    C                   Z-ADD     CORR          COM1                           comi de corretor
|    C*
|+---C     JMM1          IFGT      *ZEROS
||+--C     JMM2          IFEQ      0
|||  C     ANGA          ADD       COBR          JMC1                           comi de med com cobr
||+--C                   END
|+---C                   ELSE
||+--C     JMM2          IFGT      *ZEROS
|||  C                   Z-ADD     ANGA          JMC2                           comi de med sem cobr
||+--C                   END
|+---C                   END
|    C
|    C* mediadores com comissões de EXCEPÇÃO
|+---C     JMM2          IFEQ      418                                          JOÃO ALVADIA
||   C                   Z-ADD     0             JMC2                           sem comissão
|+---C                   END
|    C*
|+---C     JMM2          IFEQ      443                                          FERNANDO SILVA
||   C     ANGA          ADD       COBR          JMC2                           recebe como mediador
|+---C                   END                                                    com cobrança
|    C*
|+---C     JMM2          IFEQ      447                                          VERA SANTOS
||   C     ANGA          ADD       COBR          JMC2                           recebe como mediador
|+---C                   END                                                    com cobrança
|    C*
|+---C     JMM2          IFEQ      801                                          JORGE DIAZ
||   C     ANGA          ADD       COBR          JMC2                           recebe como mediador
|+---C                   END                                                    com cobrança
|    C*
|+---C     JMM1          IFEQ      001                                          JM-CLUBE TAP
||   C                   Z-ADD     CORR          JMC1                           recebe como corretor
|+---C                   END
|    C*
|+---C     JMM1          IFEQ      826                                          ANTRAL
||   C                   Z-ADD     CORR          JMC1                           recebe como corretor
|+---C                   END
|    C*
|+---C                   IF        (JMM1 = 827) AND (JMM2 = *ZEROS)
||   C                   Z-ADD     CORR          JMC1                           recebe como corretor
|+---C                   END
|    C*
|+---C                   IF        (JMM1 = 811) AND (JMM2 = *ZEROS)
||   C                   Z-ADD     CORR          JMC1                           recebe como corretor
|+---C                   END
|     *
|+---C     MED1          IFEQ      906                                          MARSH
||   C                   ADD       3.00          COM1
|+---C                   END
+----C                   ENDSR
     C*====================================================================
     C* Subrotina.: SRALT
     C* Objectivo.: Controle de Alterações
     C*====================================================================
+----CSR   SRALT         BEGSR
|    C                   Z-ADD     0             CTRL              1 0
|+---C     SIT           IFNE      WSIT
||   C                   Z-ADD     1             CTRL
|+---C                   ENDIF
|+---C     TALT          IFNE      WTALT
||   C                   Z-ADD     1             CTRL
|+---C                   ENDIF
|+---C     MALT          IFNE      WMALT
||   C                   Z-ADD     1             CTRL
|+---C                   ENDIF
|+---C     KDTALT        IFNE      ZDTALT
||   C                   Z-ADD     1             CTRL
|+---C                   ENDIF
|+---C     ADC           IFNE      ZADC
||   C                   Z-ADD     1             CTRL
|+---C                   ENDIF
|+---C     NIB           IFNE      ZNIB
||   C                   Z-ADD     1             CTRL
|+---C                   ENDIF
|+---C     FRANQO        IFNE      WFRQ
||   C                   Z-ADD     1             CTRL
|+---C                   ENDIF
|    C*
|+---C     CTRL          IFEQ      1
||   C                   Z-ADD     UDATE         DATALT            6 0
||   C                   TIME                    HORALT            6 0
||   C                   EXCEPT    FALADD
|+---C                   END
+----CSR                 ENDSR
      *
     C*====================================================================
     C* Subrotina.: VALADC
     C* Objectivo.:
     C*====================================================================
+----CSR   VALADC        BEGSR
|    C                   MOVE      RR            $RR               2
|    C                   MOVE      APOL          $APOL             7
|    C                   MOVE      ADC           $ADC             11
|    C                   MOVEL     *BLANKS       OUMSG            50
|    C                   MOVEL     *BLANKS       MENSER
|    C                   CALL      'TBADC'
|    C                   PARM                    $RR
|    C                   PARM                    $APOL
|    C                   PARM                    $ADC
|    C                   PARM                    NIB
|    C                   PARM                    OUMSG
+----CSR                 ENDSR
     O*================================================================
      /Copy QRPGLESRC,GP015DTO
     O*================================================================
**      TAB PERCENTAGENS COMISSÕES DOS RAMOS
01179161250002500137490833302500A
02154160833304167               A
03137490833302500               A
04133330833302084               A
05179161250002500137490833302500A
06179161250002500137490833302500A
07179161250002500               A
08179161250002500               A
09179161250002500               A
10154160833304167               A
15124990833301250133330833302084A
17104160625001250               A
18104160625001250               A
19104160625001250               A
55179161250002500137490833302500A
65179161250002500               A
