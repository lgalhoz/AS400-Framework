     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H DECEDIT(',') DFTNAME(GP015) DATEDIT(*DMY.) FIXNBR(*ZONED) AUT(*ALL)
     H********************************************************************
     H*  EMPRESA    :  GUARDIAN INSURANCE LIMITED                        *
     H*  PROGRAMA   :  GP015                                             *
     H*  TIPO       :  SRT                                               *
     H*  APLICAÇÃO  :  PRODUÇÃO GUARDIAN                                 *
     H*  FUNÇÃO     :  - INTRODUÇÃO / ALTERAÇÃO ( RECOLHA ), DE APÓLICES *
     H*             :    DOS OUTROS RAMOS.                               *
     H*  AUTOR      :  RUI GUERREIRO                                     *
     H*  DATA       :  04.03.2000                                        *
     H*==================================================================*
     H*                    REGISTO DE MANUTENÇÃO                         *
     H*                                                                  *
     H*   DATA   ABREV.  DESCRIÇÃO DA ALTERAÇÃO                          *
     H* -------- ------  ----------------------                          *
     H* 02/10/96 PAULOC  INCLUSÃO DE VALIDAÇÕES ÀS CLAUSULAS. ESTAS PAS- *
     H*                  SARAM A TER OUTRO SIGNIFICADO DAQUELE COM QUE   *
     H*                  FORAM CRIADOS. SÃO UTILIZADOS PARA CONDIÇÕES    *
     H*                  ESPECIAIS (F.SISMICOS, R.ELECTRICO ETC.).       *
     H*                                                                  *
     H* 12/03/97 RGUE    INCLUSÃO DE VALIDAÇÕES À INDEXAÇÃO (TXIND INDEX)*
     H*                  INTRODUÇÃO CAMPO INDEX (INDEXAÇÃO S/N).         *
     H*                                                                  *
     H* 06/01/99 RGUE    CONVERSÃO (LEITURA/ESCRITA) DE CAMPOS DATAS.    *
     H*                                                                  *
     H* 11/02/99 RGUE    EXPANSÃO FICH. GBASEGR/GBASEDIO (532 A 620)     *
     H*                                                                  *
     H* 04/03/00 RGUE    CRIADO O RAMO DE ACIDENTES DE TRABALHO.         *
     H*                                                                  *
     H* 11/11/2000-RG-ADAPTAÇÃO DO CAMPO MED2 PARA IDENTIFICAÇÃO DE      *
     H*               CANAL DE COBRANÇA DE FORMA A POSSIBILITAR A        *
     H*               EMISSÃO DE GUIAS INTERNAS PARA:                    *
     H*                  0000-TESOURARIA                                 *
     H*                  0001-COBRADOR                                   *
     H*                  0006-MONTEPIO GERAL                             *
     H*                                                                  *
     H* 14/05/01  E.G.   INCLUSÃO MAN/CONS ANEXOS DE ACIDENTES TRABALHO  *
     H* 11/03/03  R.G.   NOS ACIDENTES TRABALHO, CALCULO P/ ISENÇÃO ENC  *
     H* 28/06/04  R.G.   INTRODUÇÃO CLAUSULAS 06 E 07 PARA O RAMO 15.    *
     H*                                                                  *
     H********************************************************************
     FECRAN     CF   F  794        WORKSTN EXTFILE('SOAPSERV/FPRDE002')
     F                                     INFDS(INFD#KEYS)
     FGFSEG     IF   F  267     5AIDISK    KEYLOC(1)
     FGBASEDIO  UF A F  700     9AIDISK    KEYLOC(1) EXTFILE('LGALHOZ/GBASEDIO')
     FGBASEGRO  IF   F  700     9AIDISK    KEYLOC(1)
     FBASEGROT  IF   F  700     9AIDISK    KEYLOC(1)
     FBASEGRO   IF   F  700     9AIDISK    KEYLOC(1) EXTFILE('GBASEGRO')
     FFCONC     IF   F   30     3AIDISK    KEYLOC(1)
     FFMED      IF   F  130     4AIDISK    KEYLOC(1)
     FMCORNP    IF   F  128     3AIDISK    KEYLOC(4) EXTFILE('SD.1.CR')
     FTABELAS   IF   F   64    12AIDISK    KEYLOC(2) EXTFILE('G.SINTAB')
     FMPOSTNP   IF   F   64     7AIDISK    KEYLOC(2) EXTFILE('SD.1.CP')
     FFTEXTOS   IF   F   46     6AIDISK    KEYLOC(1)
     FGPSAT     IT   F    4        DISK    EXTFILE('MEDSAT')
     FREGAPOL   UF A F  100     9AIDISK    KEYLOC(3) EXTFILE('LGALHOZ/GREGAPOL')
     FREGAPO2   O  A F   23    23AIDISK    KEYLOC(1) EXTFILE('LGALHOZ/GREGAPO2')
     FENTIDADE  IF   F  400     7AIDISK    KEYLOC(1) EXTFILE('G.SINENT')        GS0004
     FFALTER    O  A F  223        DISK    EXTFILE('LGALHOZ/FALTER')
     D****************************************************
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SCATO010
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SENTO010

     D MSG             S             60    DIM(159) CTDATA PERRCD(1)
     D TABRAM          S              2    DIM(80) CTDATA PERRCD(40)
     D TABRR           S              2  0 DIM(16) CTDATA PERRCD(1) ASCEND
     D TABPC           S             30    DIM(16) ALT(TABRR)
     D ACG             S              1  0 DIM(5) CTDATA PERRCD(1) ASCEND
     D CL              S              2  0 DIM(6) ASCEND
     D TABMS           S              4  0 DIM(40) FROMFILE(GPSAT) PERRCD(1)
     D                                     ASCEND
     D********************************************************************
     D                UDS
     D  USER                   1      8
     D  #APOL                101    109  0                                      asilva
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
     D  ACR                    1      3  0
     D                                     DIM(3) ASCEND
     D  CR1                    1      1  0
     D  CR2                    2      2  0
     D  CR3                    3      3  0
     D  WDatInicio     S              8S 0
     D  WModo          S               N
     D  WPtrData       S               *
     D  WCodProd       S              5S 0
     D
     D*==> Passagem dos valores entre programas
     D EXPORTDTA       DS
     D RBASEBK                 1    700
     D RAMO                    1      2  0
     D APOLICE                 3      9  0
     D SEG                    11     50
     D CR1_                  217    217  0
     D CAP1                  218    223P 0
     D TX1                   224    226P 4
     D TX1AT                 224    226P 3
     D PR1                   227    231P 2
     D CR2_                  232    232  0
     D CAP2                  233    238P 0
     D TX2                   239    241P 4
     D PR2                   242    246P 2
     D CR3_                  247    247  0
     D CAP3                  248    253P 0
     D TX3                   254    256P 4
     D PR3                   257    261P 2
     D CAPTOT                262    267P 0
     D PRSACT                271    275P 2
     D LRPS                  280    324
     D ANOEDI                560    563  0
     D CPEDI                 564    567  0
     D CF01                  568    568
     D CF02                  569    569
     D CF03                  570    570
     D CF04                  571    571
     D CF05                  572    572
     D CF06                  573    573
     D CF07                  574    574
     D CF08                  575    575
     D CF0800                621    621
     D CF0830                622    622
     D CF09                  576    576
     D CF10                  577    577
     D CF11                  578    578
     D CF12                  579    579
     D CF13                  533    533
     D*==> Condições especiais
     D CE001                 580    580
     D CE002                 534    534
     D CE003                 581    581
     D CE004                 640    640
     D*==> Clausulas Aplicáveis
     D CA030                 582    582
     D CA031                 583    583
     D CA100                 584    584
     D CA101                 585    585
     D CA102                 586    586
     D*==> Clausulas Diversas
     D CD103                 587    587
     D CD130                 588    588
     D CD131                 589    589
     D CD132                 590    590
     D CD133                 591    591
     D CD134                 592    592
     D CD135                 593    593
     D CD136                 594    594
     D CD137                 595    595
     D CRHIP                 604    608  0
     D CF21                  623    623
     D DCF21                 624    638
     D CF22                  639    639
     D TIPCON                641    641
     D ESTCON                642    642
     D
     D PRM_Opcao       S              1A
     D PRM_RRApol      S              9A
     D WFirstTime      S               N
     D
     D*==> Constantes
     D OPT#Novo        C                   1
     D OPT#Consulta    C                   2
     D OPT#Altera      C                   3
     D OPT#Mudar       C                   4
     I****************************************************
     I* FICHEIRO DE SEGURADOS.                           *
     I****************************************************
     IGFSEG     AA
     I                                  1    5 0NSEG
     I                                  6   25  APEL
     I                                 26   65  SEG
     I                                 66  105  MOR
     I                                106  125  CPMOR2
     I                             P  126  129 0CPMOR1
     I                                130  130  GEST
     I                                131  131  TECN
     I                                132  132  EXCONT
     I****************************************************
     I* FICHEIRO DE RECOLHA DE APÓLICES                  *
     I****************************************************
     IGBASEDIO  AA
     I                                  1    2 0RR
     I                                  1    9 0RRAPOL
     I                                 10   10  REF
     I                                 11   50  SEG
     I                                 51   90  MOR
     I                             P   91   93 3COM1
     I                                 94  113  CPMOR2
     I                                114  114 0IM
     I                                115  154  LCB
     I                             P  155  157 3COM2
     I                                158  177  CPLCB2
     I                                178  178 0SIT
     I                                179  184 0DTINIC
     I                                179  180 0DDINIC
     I                                181  182 0MMINIC
     I                                183  184 0AAINIC
     I                                185  185 0DUR
     I                                186  186 0FRAC
     I                                187  188 0MES1
     I                                189  192 0MACONT
     I                                189  190 0MMCONT
     I                                191  192 0AACONT
     I                                193  193 0CEMIS
     I                                194  194 0ISEN
     I                             P  195  196 0CONC
     I                             P  197  198 0MED1
     I                                199  199  EXCONT
     I                                200  200  PILOTO
     I                             P  201  203 0MED2
     I*                                     204 205 FILLER
     I                                206  206 0TALT
     I                                207  212 0XDTALT
     I                                207  208 0DDALT
     I                                209  210 0MMALT
     I                                211  212 0AAALT
     I                                213  214 0MALT
     I                             P  215  216 0ACTA
     I                                217  217 0CR1
     I                             P  218  223 0CAP1
     I                             P  224  226 4TX1
     I                             P  224  226 3TX1AT
     I                             P  227  231 2PR1
     I                                232  232 0CR2
     I                             P  233  238 0CAP2
     I                             P  239  241 4TX2
     I                             P  242  246 2PR2
     I                                247  247 0CR3
     I                             P  248  253 0CAP3
     I                             P  254  256 4TX3
     I                             P  257  261 2PR3
     I                             P  262  267 0CAPTOT
     I                             P  268  270 4TXMED
     I                             P  268  270 3TXMAT
     I                             P  271  275 2PRSACT
     I                             P  276  279 0FRANQO
     I                                280  324  LRPS
     I                             P  325  326 1TXIND
     I                             P  327  328 2TXSNB
     I                                329  330  NASC
     I                             P  331  332 0PROFSL
     I                             P  333  334 2PFRC
     I                                335  336 0CL01
     I                                337  338 0CL02
     I                                339  340 0CL03
     I                                341  342 0CL04
     I                                343  344 0CL05
     I                                345  346 0CL06
     I                                335  346 0WCL
     I                                347  391  OBS1
     I                                392  436  OBS2
     I                             P  437  441 2TOTREC
     I                             P  442  446 2PRSANT
     I                             P  447  450 2PRIANT
     I                             P  451  453 0NSEG
     I                                454  459 0DTFIM
     I                                454  455 0DDFIM
     I                                456  457 0MMFIM
     I                                458  459 0AAFIM
     I                                460  460 0TEST
     I                             P  461  462 0TXEST
     I                                463  463 0CS
     I                                464  464  RA                              RISCO
     I*                                                                   APLICA
     I                                465  465  INDEX
     I                                466  470 0NSEG2
     I                                471  500  NATUR                           NATURE
     I                                501  501  CLASS                           CLASSE
     I                                502  502  PERDCQ                          PERDAS
     I                                503  503  TPERD                           TIPO P
     I*                                                                        I
     I*                                                                        F
     I                                504  505 0PPERDI                          PERCEN
     I                                506  514 0ENCFIX                          TOTAL
     I                                515  516 0PINDMN                          PERIOD
     I*                                                                   APLICA
     I                                517  517  MOD                             MODALI
     I*                                                                     F=PR
     I*                                                                     V=PR
     I                                518  518  TPT                             TIPO D
     I*                                                                     P=CO
     I*                                                                     O=CO
     I                                519  524  CDCAE                           CODIGO
     I                             P  525  530 0CAPFAT                          CODIGO
     I                                531  531  AVISN                           RECIBO
     I                                532  532  RECS                            RECIBO
     I                                535  535 0CGRUPO
     I                             P  536  538 0NPESS
     I                                539  539  TPRISC
     I                                540  541  CLTAR
     I*                                     542 551 FILLER
     I                                546  551  FILO3
     I                             P  596  599 0CPMOR1
     I                             P  600  603 0CPLCB1
     I                                604  608 0CRHIP
     I* cedência de Comissões em JM
     I                                609  611 0JMM1                            CEDÊNC
     I                             P  612  614 3JMC1                            CEDÊNC
     I                                615  617 0JMM2                            CEDÊNC
     I                             P  618  620 3JMC2                            CEDÊNC
     I************************************************************************
     I* ASILVA, Dezembro 2006
     I* novos campos dos novos produtos RESIDENTIA, MERCANTILE e CONDOMINIO
     I                                204  204  CONDOM
     I                                205  205  NEWPRD
     I* coberturas facultativas
     I                                568  568  CF01
     I                                569  569  CF02
     I                                570  570  CF03
     I                                571  571  CF04
     I                                572  572  CF05
     I                                573  573  CF06
     I                                574  574  CF07
     I                                575  575  CF08
     I                                576  576  CF09
     I                                577  577  CF10
     I                                578  578  CF11
     I                                579  579  CF12
     I                                533  533  CF13
     I* condições especiais
     I                                580  580  CE001
     I                                534  534  CE002
     I                                581  581  CE003
     I* cláusulas aplicáveis
     I                                582  582  CA030
     I                                583  583  CA031
     I                                584  584  CA100
     I                                585  585  CA101
     I                                586  586  CA102
     I* cláusulas especiais
     I                                587  587  CD103
     I                                588  588  CD130
     I                                589  589  CD131
     I                                590  590  CD132
     I                                591  591  CD133
     I                                592  592  CD134
     I                                593  593  CD135
     I                                594  594  CD136
     I                                595  595  CD137
     I* novos campos adicionados em Maio 2007
     I                             P  552  557 0ADC
     I                                558  558  EMPREG
     I                                559  559  MEMO
     I                                560  563 0ANOEDI
     I                                564  567 0CPEDI
      *
     I                                621  621  CF0800
     I                                622  622  CF0830
     I                                623  623  CF21
     I                                624  638  DCF21
     I                                639  639  CF22
     I                                640  640  CE004
     I                                641  641  TIPCON
     I                                642  642  ESTCON
      *
     I                                680  700  NIB
     I                                  1  700  RBASEBK
     I****************************************************
     I* FICHEIRO - BASE DE APÓLICES                      *
     I****************************************************
     IGBASEGRO  AA
     I                                  1    2 0RR
     I                                  1    9 0RRAPOL
     I                                 10   10  REF
     I                                 11   50  SEG
     I                                 51   90  MOR
     I                             P   91   93 3COM1
     I                                 94  113  CPMOR2
     I                                114  114 0IM
     I                                115  154  LCB
     I                             P  155  157 3COM2
     I                                158  177  CPLCB2
     I                                178  178 0SIT
     I                                179  184 0DTINIC
     I                                179  180 0DDINIC
     I                                181  182 0MMINIC
     I                                183  184 0AAINIC
     I                                185  185 0DUR
     I                                186  186 0FRAC
     I                                187  188 0MES1
     I                                189  192 0MACONT
     I                                189  190 0MMCONT
     I                                191  192 0AACONT
     I                                193  193 0CEMIS
     I                                194  194 0ISEN
     I                             P  195  196 0CONC
     I                             P  197  198 0MED1
     I                                199  199  EXCONT
     I                                200  200  PILOTO
     I                             P  201  203 0MED2
     I*                                     204 205 FILLER
     I                                206  206 0TALT
     I                                207  212 0XDTALT
     I                                207  208 0DDALT
     I                                209  210 0MMALT
     I                                211  212 0AAALT
     I                                213  214 0MALT
     I                             P  215  216 0ACTA
     I                                217  217 0CR1
     I                             P  218  223 0CAP1
     I                             P  224  226 4TX1
     I                             P  224  226 3TX1AT
     I                             P  227  231 2PR1
     I                                232  232 0CR2
     I                             P  233  238 0CAP2
     I                             P  239  241 4TX2
     I                             P  242  246 2PR2
     I                                247  247 0CR3
     I                             P  248  253 0CAP3
     I                             P  254  256 4TX3
     I                             P  257  261 2PR3
     I                             P  262  267 0CAPTOT
     I                             P  268  270 4TXMED
     I                             P  268  270 3TXMAT
     I                             P  271  275 2PRSACT
     I                             P  276  279 0FRANQO
     I                                280  324  LRPS
     I                             P  325  326 1TXIND
     I                             P  327  328 2TXSNB
     I                                329  330  NASC
     I                             P  331  332 0PROFSL
     I                             P  333  334 2PFRC
     I                                335  336 0CL01
     I                                337  338 0CL02
     I                                339  340 0CL03
     I                                341  342 0CL04
     I                                343  344 0CL05
     I                                345  346 0CL06
     I                                335  346 0WCL
     I                                347  391  OBS1
     I                                392  436  OBS2
     I                             P  437  441 2TOTREC
     I                             P  442  446 2PRSANT
     I                             P  447  450 2PRIANT
     I                             P  451  453 0NSEG
     I                                454  459 0DTFIM
     I                                454  455 0DDFIM
     I                                456  457 0MMFIM
     I                                458  459 0AAFIM
     I                                460  460 0TEST
     I                             P  461  462 0TXEST
     I                                463  463 0CS
     I                                464  464  RA                              RISCO
     I*                                                                   APLICA
     I                                465  465  INDEX
     I                                466  470 0NSEG2
     I                                471  500  NATUR                           NATURE
     I                                501  501  CLASS                           CLASSE
     I                                502  502  PERDCQ                          PERDAS
     I                                503  503  TPERD                           TIPO P
     I*                                                                        I
     I*                                                                        F
     I                                504  505 0PPERDI                          PERCEN
     I                                506  514 0ENCFIX                          TOTAL
     I                                515  516 0PINDMN                          PERIOD
     I*
     I*                                                                   APLICA
     I                                517  517  MOD                             MODALI
     I*                                                                     F=PR
     I*                                                                     V=PR
     I                                518  518  TPT                             TIPO D
     I*                                                                     P=CO
     I*                                                                     O=CO
     I                                519  524  CDCAE                           CODIGO
     I                             P  525  530 0CAPFAT                          CODIGO
     I                                531  531  AVISN                           RECIBO
     I                                532  532  RECS                            RECIBO
     I                                535  535 0CGRUPO
     I                             P  536  538 0NPESS
      *                                     542 551 FILLER
     I                                546  551  FILO3
     I                             P  596  599 0CPMOR1
     I                             P  600  603 0CPLCB1
     I                                604  608 0CRHIP
     I* cedência de Comissões em JM
     I                                609  611 0JMM1                            CEDÊNC
     I                             P  612  614 3JMC1                            CEDÊNC
     I                                615  617 0JMM2                            CEDÊNC
     I                             P  618  620 3JMC2                            CEDÊNC
     I************************************************************************
     I* ASILVA, Dezembro 2006
     I* novos campos dos novos produtos RESIDENTIA, MERCANTILE e CONDOMINIO
     I                                204  204  CONDOM
     I                                205  205  NEWPRD
     I* coberturas facultativas
     I                                568  568  CF01
     I                                569  569  CF02
     I                                570  570  CF03
     I                                571  571  CF04
     I                                572  572  CF05
     I                                573  573  CF06
     I                                574  574  CF07
     I                                575  575  CF08
     I                                576  576  CF09
     I                                577  577  CF10
     I                                578  578  CF11
     I                                579  579  CF12
     I                                533  533  CF13
     I* condições especiais
     I                                580  580  CE001
     I                                534  534  CE002
     I                                581  581  CE003
     I* cláusulas aplicáveis
     I                                582  582  CA030
     I                                583  583  CA031
     I                                584  584  CA100
     I                                585  585  CA101
     I                                586  586  CA102
     I* cláusulas especiais
     I                                587  587  CD103
     I                                588  588  CD130
     I                                589  589  CD131
     I                                590  590  CD132
     I                                591  591  CD133
     I                                592  592  CD134
     I                                593  593  CD135
     I                                594  594  CD136
     I                                595  595  CD137
     I* novos campos adicionados em Maio 2007
     I                             P  552  557 0ADC
     I                                558  558  EMPREG
     I                                559  559  MEMO
     I                                560  563 0ANOEDI
     I                                564  567 0CPEDI
      *
     I                                621  621  CF0800
     I                                622  622  CF0830
     I                                623  623  CF21
     I                                624  638  DCF21
     I                                639  639  CF22
     I                                640  640  CE004
     I                                641  641  TIPCON
     I                                642  642  ESTCON
      *
     I                                680  700  NIB
     I                                  1  700  RBASEBK
     I****************************************************
     IBASEGROT  AA
     I                                  1    2 0RR
     I                                  1    9 0RRAPOL
     I                                 10   10  REF
     I                                 11   50  SEG
     I                                 51   90  MOR
     I                             P   91   93 3COM1
     I                                 94  113  CPMOR2
     I                                114  114 0IM
     I                                115  154  LCB
     I                             P  155  157 3COM2
     I                                158  177  CPLCB2
     I                                178  178 0SIT
     I                                179  184 0DTINIC
     I                                179  180 0DDINIC
     I                                181  182 0MMINIC
     I                                183  184 0AAINIC
     I                                185  185 0DUR
     I                                186  186 0FRAC
     I                                187  188 0MES1
     I                                189  192 0MACONT
     I                                189  190 0MMCONT
     I                                191  192 0AACONT
     I                                193  193 0CEMIS
     I                                194  194 0ISEN
     I                             P  195  196 0CONC
     I                             P  197  198 0MED1
     I                                199  199  EXCONT
     I                                200  200  PILOTO
     I                             P  201  203 0MED2
     I*                                     204 205 FILLER
     I                                206  206 0TALT
     I                                207  212 0XDTALT
     I                                207  208 0DDALT
     I                                209  210 0MMALT
     I                                211  212 0AAALT
     I                                213  214 0MALT
     I                             P  215  216 0ACTA
     I                                217  217 0CR1
     I                             P  218  223 0CAP1
     I                             P  224  226 4TX1
     I                             P  224  226 3TX1AT
     I                             P  227  231 2PR1
     I                                232  232 0CR2
     I                             P  233  238 0CAP2
     I                             P  239  241 4TX2
     I                             P  242  246 2PR2
     I                                247  247 0CR3
     I                             P  248  253 0CAP3
     I                             P  254  256 4TX3
     I                             P  257  261 2PR3
     I                             P  262  267 0CAPTOT
     I                             P  268  270 4TXMED
     I                             P  268  270 3TXMAT
     I                             P  271  275 2PRSACT
     I                             P  276  279 0FRANQO
     I                                280  324  LRPS
     I                             P  325  326 1TXIND
     I                             P  327  328 2TXSNB
     I                                329  330  NASC
     I                             P  331  332 0PROFSL
     I                             P  333  334 2PFRC
     I                                335  336 0CL01
     I                                337  338 0CL02
     I                                339  340 0CL03
     I                                341  342 0CL04
     I                                343  344 0CL05
     I                                345  346 0CL06
     I                                335  346 0WCL
     I                                347  391  OBS1
     I                                392  436  OBS2
     I                             P  437  441 2TOTREC
     I                             P  442  446 2PRSANT
     I                             P  447  450 2PRIANT
     I                             P  451  453 0NSEG
     I                                454  459 0DTFIM
     I                                454  455 0DDFIM
     I                                456  457 0MMFIM
     I                                458  459 0AAFIM
     I                                460  460 0TEST
     I                             P  461  462 0TXEST
     I                                463  463 0CS
     I                                464  464  RA                              RISCO
     I*                                                                   APLICA
     I                                465  465  INDEX
     I                                466  470 0NSEG2
     I                                471  500  NATUR                           NATURE
     I                                501  501  CLASS                           CLASSE
     I                                502  502  PERDCQ                          PERDAS
     I                                503  503  TPERD                           TIPO P
     I*                                                                        I
     I*                                                                        F
     I                                504  505 0PPERDI                          PERCEN
     I                                506  514 0ENCFIX                          TOTAL
     I                                515  516 0PINDMN                          PERIOD
     I*
     I*                                                                   APLICA
     I                                517  517  MOD                             MODALI
     I*                                                                     F=PR
     I*                                                                     V=PR
     I                                518  518  TPT                             TIPO D
     I*                                                                     P=CO
     I*                                                                     O=CO
     I                                519  524  CDCAE                           CODIGO
     I                             P  525  530 0CAPFAT                          CODIGO
     I                                531  531  AVISN                           RECIBO
     I                                532  532  RECS                            RECIBO
     I                                535  535 0CGRUPO
     I                             P  536  538 0NPESS
      *                                     542 551 FILLER
     I                             P  596  599 0CPMOR1
     I                             P  600  603 0CPLCB1
     I                                604  608 0CRHIP
     I* cedência de Comissões em JM
     I                                609  611 0JMM1                            CEDÊNC
     I                             P  612  614 3JMC1                            CEDÊNC
     I                                615  617 0JMM2                            CEDÊNC
     I                             P  618  620 3JMC2                            CEDÊNC
     I************************************************************************
     I* ASILVA, Dezembro 2006
     I* novos campos dos novos produtos RESIDENTIA, MERCANTILE e CONDOMINIO
     I                                204  204  CONDOM
     I                                205  205  NEWPRD
     I* coberturas facultativas
     I                                568  568  CF01
     I                                569  569  CF02
     I                                570  570  CF03
     I                                571  571  CF04
     I                                572  572  CF05
     I                                573  573  CF06
     I                                574  574  CF07
     I                                575  575  CF08
     I                                576  576  CF09
     I                                577  577  CF10
     I                                578  578  CF11
     I                                579  579  CF12
     I                                533  533  CF13
     I* condições especiais
     I                                580  580  CE001
     I                                534  534  CE002
     I                                581  581  CE003
     I* cláusulas aplicáveis
     I                                582  582  CA030
     I                                583  583  CA031
     I                                584  584  CA100
     I                                585  585  CA101
     I                                586  586  CA102
     I* cláusulas especiais
     I                                587  587  CD103
     I                                588  588  CD130
     I                                589  589  CD131
     I                                590  590  CD132
     I                                591  591  CD133
     I                                592  592  CD134
     I                                593  593  CD135
     I                                594  594  CD136
     I                                595  595  CD137
     I* novos campos adicionados em Maio 2007
     I                             P  552  557 0ADC
     I                                558  558  EMPREG
     I                                559  559  MEMO
     I                                560  563 0ANOEDI
     I                                564  567 0CPEDI
      *
     I                                621  621  CF0800
     I                                622  622  CF0830
     I                                623  623  CF21
     I                                624  638  DCF21
     I                                639  639  CF22
     I                                640  640  CE004
     I                                641  641  TIPCON
     I                                642  642  ESTCON
      *
     I                                680  700  NIB
     I                                  1  700  RBASEBK
      *
     IBASEGRO   AA
     I                             P  536  538 0WNPESS
     I                                539  539  WTPRIS
     I                                540  541  WCLTAR
     I****************************************************
     I* FICHEIRO DE CONCELHOS                            *
     I****************************************************
     IFCONC     AA
     I                                  1    3 0CONC
     I                                  4   30  DESCON
     I****************************************************
     I* FICHEIRO DE MEDIADORES                           *
     I****************************************************
     IFMED      AA
     I                                  1    4 0MED
     IREGAPOL   NS
     I****************************************************
     I* GUARDIAN - REGISTO DE APÓLICES  (livro)          *
     I****************************************************
     I                                  1    1  RACTIV                          ACTIVI
     I*                                                                    ' '=
     I*                                                                     E =
     I                                  2    2 0RSIT                            SITUAÇ
     I*                                                                     1 =
     I*                                                                     2 =
     I*                                                                     3 =
     I                                  3    4 0RRR                             RAMO
     I                                  5   11 0RAPOL                           APOLIC
     I                                 12   12 0RTIPO                           TIPO
     I*                                                                     1 =
     I*                                                                     2 =
     I*                                                                     3 =
     I                                 13   18 0RDTINI                          DATA I
     I                                 19   24 0RDTFIM                          DATA F
     I                                 25   29 0RNSEG                           N. SEG
     I                                 30   69  RSEG                            NOME S
     I                                 70   80  RMATR1                          MATRIC
     I                                 81   86 0RDTREG                          DATA R
     I                                 87   92 0RDTANU                          DATA A
     I                                 93   98 0RDTPRO                          DATA P
     I*                                      99 100 FILLER                DISPON
     I****************************************************
     I* GUARDIAN - TABELA AGENTES (MEDIADORES JOSE MATA) *
     I****************************************************
     IMCORNP    NS
     I                                  4    6 0CDCR
     I                                  7   32  NOMCR
     I                                 33   33  ALTCR
     I                                 34   39 0CPECR
     I                                 40   48  ABVCR
     I                                 49   49 0TMEDCR
     I                             P   50   54 0NMEDCR
     I                                 57   58 0NBFCR
     I                                 59   62 0CBFCR
     I                                111  111  IDEPCR
     I                                112  114 0SA11
     I                                115  122  SA12
     I                                123  128 0SA13
     I****************************************************
     I* GUARDIAN - TABELA GERAIS (COD. CAE)              *
     I****************************************************
     ITABELAS   NS
     I                                  1    1  ACTREG
     I                                  2    3 0NTAB
     I                                  4   13  CTAB
     I                                 14   53  DESCR
     I*                                      54  64 FILLER
     I****************************************************
     I* TABELA CODIGOS POSTAIS                           *
     I****************************************************
     IMPOSTNP   NS
     I                                  9   38  DCPOST
     I****************************************************
     I* GUARDIAN - TABELA TEXTOS / PROFISSOES            *
     I****************************************************
     IFTEXTOS   AA  12
     I                                  1  006 0CHTEXT
     I                                  7  046  DESC
     IENTIDADE  NS                                                              GS0004
     I                                  1    2 0TPENT                           GS0004
     I                                  3    7 0NRENT                           GS0004
     I                                  8   47  NOMENT                          GS0004
     I                                 48   87  RESENT                          GS0004
     I                                 88  127  MORENT                          GS0004
     I                                128  152  LOCENT                          GS0004
     I                                153  156 0CDPENT                          GS0004
     I                                157  165 0TELENT                          GS0004
     I                                166  174 0NCONTR                          GS0004
     I                                175  184  NCNTB                           GS0004
     I                                185  194 0FAXENT
     I                                201  201  EMPIND                          GS0004
     I                                202  202  IRSENT                          GS0004
     I                                388  393 0DATCRI                          GS0004
     I                                394  399 0DATUAT                          GS0004
     I                                400  400  CDEL                            GS0004
     I****************************************************
     I* DEFINIÇÃO DE ECRANS                              *
     I****************************************************
     IECRAN     AA  91    1 C
     I* FORMAT-INPUT
     I          AA  01    1 CC
     I                                  1    1  INDREG
     I                                  2   41  LCB
     I                                 42   45 0CPOSL
     I                                 46   48 0ZPOSL
     I                                 49   68  CPLCB2
     I                                 69   69 0SIT
     I                                 70   71 0DDINIC
     I                                 72   73 0MMINIC
     I                                 74   77 0AAINI
     I                                 78   78 0DUR
     I                                 79   80 0DDFIM
     I                                 81   82 0MMFIM
     I                                 83   86 0AAFI
     I                                 87   87 0IM
     I                                 88   88 0FRAC
     I                                 89   90 0MES1
     I                                 91   92 0MMCONT
     I                                 93   96 0AACON
     I                                 97   97 0CEMIS
     I                                 98   98 0ISEN
     I                                 99  101 0CONC
     I                                102  106 0CRHIP
     I                                107  109 0MED1
     I                                110  114 3COM1
     I                                115  119 0MED2
     I                                120  124 3COM2
     I* novos campos EMPREG e ANOEDI, Maio 2007
     I                                125  125  EMPREG
     I                                126  129 0ANOEDI
     I*
     I                                130  130  AVISN
     I* novo campo CPEDI, Maio 2007
     I                                131  134 0CPEDI
     I*
     I                                135  135 0CR1
     I                                136  146 0CAP1
     I                                147  151 4TX1
     I                                147  151 3TX1AT
     I                                152  160 2PR1
     I                                161  161 0CS
     I                                162  162 0CR2
     I                                163  173 0CAP2
     I                                174  178 4TX2
     I                                179  187 2PR2
     I                                188  188 0CR3
     I                                189  199 0CAP3
     I                                200  204 4TX3
     I                                205  213 2PR3
     I                                214  224 0CAPTOT
     I                                225  229 4TXMED
     I                                225  229 3TXMAT
     I                                230  238 2PRSACT
     I                                239  245 0FRANQO
     I                                246  256 0CAPFAT
     I* novos campos ADC e NIB, Maio 2007
     I                                257  267 0ADC
     I                                268  288  NIB
     I*
     I                                289  333  LRPS
     I                                334  336 1TXIND
     I                                337  337  INDEX
     I                                338  340 2TXSNB
     I                                341  344 0NASCX
     I                                345  347 0PROFSL
     I                                348  348 0CGRUPO
     I                                349  353 0NPESS
     I                                354  354  RA
     I                                355  384  NATUR
     I                                385  385  CLASS
     I                                386  386  PERDCQ
     I                                387  387  TPERD
     I                                388  389 0PPERDI
     I                                390  398 0ENCFIX
     I                                399  400 0PINDMN
     I                                401  412  CL                              CLAUSULA PARTICULAR
     I                                401  412 0WCL
     I                                401  402 0CL01
     I                                403  404 0CL02
     I                                405  406 0CL03
     I                                407  408 0CL04
     I                                409  410 0CL05
     I                                411  412 0CL06
     I                                413  415 2PFRC
     I                                416  416  MOD
     I                                417  417  TPT
     I                                418  423  CDCAE
     I                                424  426 0JMM1
     I                                427  431 3JMC1
     I                                432  476  OBS1
     I                                477  479 0JMM2
     I                                480  484 3JMC2
     I                                485  529  OBS2
     I                                530  532 0ECR_CM#ROW
     I                                533  535 0ECR_CM#COL
     I                                536  545  ECR_CM#NMREC
     I                                546  555  ECR_CM#NMFLD
     I* FORMAT-INPUT1
     I          AA  01    1 CB
     I                                  1    1  INDREG
     I                                  2    6 0NSEG
     I                                  7    9 0ECR_CM#ROW
     I                                 10   12 0ECR_CM#COL
     I                                 13   22  ECR_CM#NMREC
     I                                 23   32  ECR_CM#NMFLD
     I* FORMAT-INPUT3       coberturas facultativas do produto MERCANTILE
     I          AA  04    1 CM
     I                                  2    2  CF01
     I                                  3    3  CE001
     I                                  4    4  CA030
     I                                  5    5  CD103
     I                                  6    6  CF02
     I                                  7    7  CE002
     I                                  8    8  CA100
     I                                  9    9  CD133
     I                                 10   10  CF03
     I                                 11   11  CA101
     I                                 12   12  CD134
     I                                 13   13  CF04
     I                                 14   14  CA102
     I                                 15   15  CD135
     I                                 16   16  CF05
     I                                 17   17  CD136
     I                                 18   18  CF06
     I                                 19   19  CD137
     I                                 20   20  CF07
     I                                 21   21  CF08
     I                                 22   22  CF09
     I                                 23   23  CF10
     I                                 24   24  CF11
     I                                 25   25  CF12
     I                                 26   26  CF13
     I************************************************************************
     I* FORMAT-OUTPUT
     I          AA  01    1 CD
     I                                  1    1  INDREG
     I                                  2   41  LCB
     I                                 42   45 0CPOSL
     I                                 46   48 0ZPOSL
     I                                 49   68  CPLCB2
     I                                 69   69 0SIT
     I                                 70   71 0DDINIC
     I                                 72   73 0MMINIC
     I                                 74   77 0AAINI
     I                                 78   78 0DUR
     I                                 79   80 0DDFIM
     I                                 81   82 0MMFIM
     I                                 83   86 0AAFI
     I                                 87   87 0IM
     I                                 88   88 0FRAC
     I                                 89   90 0MES1
     I                                 91   92 0MMCONT
     I                                 93   96 0AACON
     I                                 97   97 0CEMIS
     I                                 98   98 0ISEN
     I                                 99  101 0CONC
     I                                102  106 0CRHIP
     I                                107  109 0MED1
     I                                110  114 3COM1
     I                                115  119 0MED2
     I                                120  124 3COM2
     I* novos campos EMPREG e ANOEDI, Maio 2007
     I                                125  125  EMPREG
     I                                126  129 0ANOEDI
     I*
     I                                130  130 0TALT
     I                                131  138 0DTALT
     I                                131  132 0DDALT
     I                                133  134 0MMALT
     I                                135  138 0AAALTX
     I                                137  138 0KAAALT
     I                                139  140 0MALT
     I                                141  143 0ACTA
     I                                144  144  AVISN
     I* novo campo CPEDI, Maio 2007
     I                                145  148 0CPEDI
     I*
     I                                149  149 0CR1
     I                                150  160 0CAP1
     I                                161  165 4TX1
     I                                161  165 3TX1AT
     I                                166  174 2PR1
     I                                175  175 0CS
     I                                176  184 2PRSANT
     I                                185  185 0CR2
     I                                186  196 0CAP2
     I                                197  201 4TX2
     I                                202  210 2PR2
     I                                211  211 0CR3
     I                                212  222 0CAP3
     I                                223  227 4TX3
     I                                228  236 2PR3
     I                                237  247 0CAPTOT
     I                                248  252 4TXMED
     I                                248  252 3TXMAT
     I                                253  261 2PRSACT
     I                                262  268 0FRANQO
     I                                269  279 0CAPFAT
     I* novos campos ADC e NIB, Maio 2007
     I                                280  290 0ADC
     I                                291  311  NIB
     I*
     I                                312  356  LRPS
     I                                357  359 1TXIND
     I                                360  360  INDEX
     I                                361  363 2TXSNB
     I                                364  367 0NASCX
     I                                368  370 0PROFSL
     I                                371  371 0CGRUPO
     I                                372  376 0NPESS
     I                                377  377  RA
     I                                378  407  NATUR
     I                                408  408  CLASS
     I                                409  409  PERDCQ
     I                                410  410  TPERD
     I                                411  412 0PPERDI
     I                                413  421 0ENCFIX
     I                                422  423 0PINDMN
     I                                424  435  CL                              CLAUSULA PARTICULAR
     I                                424  435 0WCL
     I                                424  425 0CL01
     I                                426  427 0CL02
     I                                428  429 0CL03
     I                                430  431 0CL04
     I                                432  433 0CL05
     I                                434  435 0CL06
     I                                436  438 2PFRC
     I                                439  439 0TEST
     I                                440  442 0TXEST
     I                                443  443  MOD
     I                                444  444  TPT
     I                                445  450  CDCAE
     I                                451  453 0JMM1
     I                                454  458 3JMC1
     I                                459  503  OBS1
     I                                504  506 0JMM2
     I                                507  511 3JMC2
     I                                512  556  OBS2
     I                                557  565 2TOTREC
     I                                566  568 0ECR_CM#ROW
     I                                569  571 0ECR_CM#COL
     I                                572  581  ECR_CM#NMREC
     I                                582  591  ECR_CM#NMFLD
     I* FORMAT-ECERRO
     I          AA  01    1 CZ
     I                                  1    1  INDREG
     I* FORMAT-CONDOM
     I          AA  01    1 CO
     I                                  2    4  SIMNAO
     I*===================================================================
     C     *ENTRY        PLIST
     C                   PARM                    PRM_OPCAO
     C                   PARM                    PRM_RRAPOL
     C*
     C                   EVAL      WFirstTime = *ON
     C                   Z-ADD     2000          YYYY              4 0
     C                   ADD       UYEAR         YYYY
     C*
     C                   MONITOR
     C*                  READ      ECRAN                                  92
     C                   ON-ERROR
     C                   ENDMON
     C* ecran do MENU
     C     XPTO01        TAG
     C                   MOVE      *BLANKS       MENSER
     C                   MOVE      'A'           INDREG
     C************************************************************************
     C* ASILVA, Dezembro 2006
     C                   SETOFF                                       1314      desinibir clausulas
     C************************************************************************
     C*
     C*                  EXCEPT    MENU01
     C*                  READ      ECRAN                                  92
+----C                   IF        (Func#Key=Key#F03) OR (Func#Key=Key#F12)
|    C*                  IF        (Func#Key = Key#F07)
|    C                   SETON                                        LR
|    C                   GOTO      ACABLR
+----C                   ENDIF
     C                   MOVE      PRM_OPCAO     OPCAO
     C                   MOVE      PRM_RRAPOL    RRAPOL
     C*
     C     OPCAO         COMP      OPT#Novo                             49      opção CRIAÇÃO
     C  N49OPCAO         COMP      OPT#Mudar                          49        opção ALTERAÇÃO NOME
     C   49              MOVE      MSG(01)       MENSER           60
     C   49              EXSR      ERRO
     C   49              GOTO      XPTO01
     C*-------------------------------------------------------------------
     C* INICIO DA ALTERAÇÃO - só permite a opção 2 (consulta) para
     C* determinados utilizadores (U8 on)
     C   U8OPCAO         COMP      OPT#Consulta                       4949      opção CONSULTA
     C   U8
     CAN 49              MOVE      MSG(01)       MENSER
     C   U8
     CAN 49              EXSR      ERRO
     C   U8
     CAN 49              GOTO      XPTO01
     C* FIM DA ALTERAÇÃO
     C*-------------------------------------------------------------------
     C                   MOVE      UDATE         WANO              2 0
     C                   MOVE      WANO          AAIPL             4 0
+----C     WANO          IFLT      40
|    C                   MOVEL     20            AAIPL
+----C                   ELSE
|    C                   MOVEL     19            AAIPL
+----C                   END
     C     AAIPL         ADD       1             AAIP1             4 0
     C     AAIPL         ADD       2             AAIP2             4 0
     C*                  MOVE      RR            WRR               2
     C                   SETOFF                                       52
     C     WRR           LOOKUP    TABRAM                                 49
     C  N49              SETON                                        52
     C   52              MOVE      MSG(02)       MENSER
     C   52              EXSR      ERRO
     C   52              GOTO      XPTO01
     C                   SETOFF                                       1516
     C                   MOVE      'O'           REF
     C*
     C*  INICIALIZA CAMPOS DE ECRAN A BRANCOS OU A ZEROS.
     C     OPCAO         COMP      OPT#Consulta                           20    opção CONSULTA
     C                   MOVE      *BLANK        RSALDO           25
[LG] C*                    MOVE *BLANK    FILO3
     C     RRAPOL        CHAIN     GBASEGRO                           16
     C   16
     CAN 20RRAPOL        CHAIN     BASEGROT                           16
+----C  N16RECS          IFEQ      'S'
|    C                   MOVEL     MSG(104)      RSALDO
+----C                   END
     C*
     C                   MOVE      *BLANK        RA                              RG 17/04/91
     C                   MOVE      *BLANK        NATUR                           PC 30/10/91
     C                   MOVE      *BLANK        CLASS                           PC 30/10/91
     C                   MOVE      *BLANK        PERDCQ                          PC 30/10/91
     C                   MOVE      *BLANK        TPERD                           PC 30/10/91
     C                   Z-ADD     0             PPERDI                          PC 30/10/91
     C                   Z-ADD     0             ENCFIX                          PC 30/10/91
     C                   Z-ADD     0             PINDMN                          PC 30/10/91
     C                   Z-ADD     0             TXIND                           RG 12/03/97
     C                   MOVE      *BLANK        INDEX                           RG 12/03/97
     C*
     C                   MOVE      *BLANKS       MOD
     C                   MOVE      *BLANKS       TPT
     C                   MOVEL     *BLANKS       CDCAE
     C*
     C                   Z-ADD     299           MED1
     C                   Z-ADD     *ZEROS        COM1
     C                   Z-ADD     *ZEROS        MED2
     C                   Z-ADD     *ZEROS        COM2
      *
     C                   MOVE      'S'           AVISN
     C*
     C*  INICIALIZA INDICADORES DE ECRAN (DETERMINA CAMPOS A VISUALIZAR).
     C     RR            COMP      01                                 1111
     C   11RR            COMP      21                                 1111
     C   11RR            COMP      31                                 1111
     C     RR            COMP      15                                     95
     C     RR            COMP      25                                 1212
     C   12RR            COMP      35                                 1212
     C   12RR            COMP      55                                 1212
     C*
     C                   SETOFF                                       1516
     C     RRAPOL        CHAIN(N)  GBASEDIO                           15
     C   15RRAPOL        CHAIN     GBASEGRO                           16
     C   16
     CAN 20RRAPOL        CHAIN     BASEGROT                           16
     C  N15
+----CORN16              DO
|    C                   Z-ADD     ADC           WADC             11 0
|    C                   MOVEL     NIB           WNIB             21
+----C                   END
     C     OPCAO         COMP      OPT#Novo                               19    opção CRIAÇÃO
     C   19              MOVE      *BLANKS       APOL
     C     OPCAO         COMP      OPT#Consulta                           20    opção CONSULTA
     C     OPCAO         COMP      OPT#Altera                             21    opção ALTERAÇÂO
     C     OPCAO         COMP      OPT#Mudar                              22    opção ALTERAÇÃO NOME
     C
     C   19              EVAL      WModo = *OFF
     C   20              EVAL      WModo = *ON
     C   21              EVAL      WModo = *OFF
     C   22              EVAL      WModo = *OFF
     C*
     C   19
     CANN15              MOVE      MSG(03)       MENSER
     C   19
     CANN15              EXSR      ERRO
     C   19
     CANN15              GOTO      XPTO01
     C   19
     CANN16              MOVE      MSG(04)       MENSER
     C   19
     CANN16              EXSR      ERRO
     C   19
     CANN16              GOTO      XPTO01
     C   20
     CAN 16
     COR 21
     CAN 16
     COR 22
     CAN 16              MOVE      MSG(05)       MENSER
     C   20
     CAN 16
     COR 21
     CAN 16
     COR 22
     CAN 16              EXSR      ERRO
     C   20
     CAN 16
     COR 21
     CAN 16
     COR 22
     CAN 16              GOTO      XPTO01
     C* CONTROLE ALTERAÇÕES
     C                   Z-ADD     0             WTB               1 0
     C                   Z-ADD     0             TB                1 0
     C                   Z-ADD     0             WAGRAV            3 0
     C                   Z-ADD     0             AGRAV             3 0
     C                   Z-ADD     0             WNSIN             1 0
     C                   Z-ADD     0             NSIN              1 0
     C                   Z-ADD     0             WDUSIN            6 0
     C                   Z-ADD     0             DTUSIN            6 0
      *
     C                   Z-ADD     SIT           WSIT              1 0
     C                   Z-ADD     TALT          WTALT             1 0
     C                   Z-ADD     MALT          WMALT             2 0
     C                   Z-ADD     XDTALT        ZDTALT            6 0
     C                   Z-ADD     ADC           ZADC             11 0
     C                   MOVEL     NIB           ZNIB             21
     C                   Z-ADD     FRANQO        WFRQ              7 0
     C*
     C   19              MOVE      ' NOVOS  '    WOPCAO            8
     C   20              MOVE      'CONSULTA'    WOPCAO
     C   21              MOVE      'ALTERAC.'    WOPCAO
     C   22              MOVE      'ALT.NSEG'    WOPCAO
     C                   MOVE      '0'           FLAGA             1
     C                   MOVE      '0'           FLAGR             1
     C                   MOVE      '0'           FLAGU             1
     C                   MOVE      '0'           FLAGI             1
     C                   Z-ADD     0             W1                1 0
     C   19              EVAL      FILO3 = *BLANKS
     C   19              Z-ADD     0             FRANQO
     C   19              Z-ADD     0             TALT
     C   19              Z-ADD     0             KDTALT
     C   19              Z-ADD     0             DDALT
     C   19              Z-ADD     0             MMALT
     C   19              Z-ADD     0             AAALT
     C   19              Z-ADD     0             AAALTX
     C   19              Z-ADD     0             MALT
     C   19              Z-ADD     0             ACTA
     C   19              Z-ADD     0             PRSANT
     C   19              Z-ADD     0             PRIANT
     C   19              Z-ADD     0             TEST
     C   19              Z-ADD     0             TXEST
     C   19              Z-ADD     0             TOTREC
     C   19              Z-ADD     0             NSEG
     C   19              MOVE      SPAC40        SEG
     C   19              MOVE      SPAC40        MOR
     C   19              Z-ADD     0             CPMOR1
     C   19              MOVE      SPAC40        CPMOR2
     C   19              Z-ADD     0             TXIND                          RG 12/03/97
     C   19              MOVE      *BLANK        INDEX                          RG 12/03/97
     C   19              MOVE      *ZEROS        CAPFAT
     C   19              MOVE      *ZEROS        CRHIP
     C   19              MOVE      'N'           RECS
     C   19              MOVE      *ZEROS        CPEDI
     C   19              MOVE      *ZEROS        ADC
     C   19              MOVE      *BLANK        NIB
     C   19
     COR 20              MOVE      *BLANKS       NOMENT
     C   19
     COR 22              GOTO      XPTO02
     C*
     C   21
     CAN 15              MOVE      PRSACT        PRSANT
     C  N20              EXSR      VFSEG
     C  N20
     CAN 05              GOTO      XPTO01
     C*
     C   20
     CANN15              MOVE      MSG(112)      MENSER
     C   21
     CANN15              MOVE      MSG(113)      MENSER
     C************************************************************************
     C* ASILVA, Dezembro 2006
     C* Dar mensagem a avisar que a apolice é do novo produto RESIDENTIA
     C   20              SETON                                        14        inibir clausulas
     C*
     C   20
+----COR 21              DO
|+---C     RR            IFEQ      05
||+--C     NEWPRD        IFEQ      'S'
|||+-C     CONDOM        IFEQ      ' '
|||| C                   MOVE      MSG(145)      MENSER
|||| C                   EXSR      ERRO
|||| C                   SETON                                        13        inibir clausulas
|||| C                   GOTO      QW
|||+-C                   END
||+--C                   END
|+---C                   END
+----C                   END
     C* Dar mensagem a avisar que a apolice é do novo produto MERCANTILE
     C   20
+----COR 21              DO
|+---C     RR            IFEQ      55
||+--C     NEWPRD        IFEQ      'S'
|||  C                   MOVE      MSG(146)      MENSER
|||  C                   EXSR      ERRO
|||  C                   SETON                                        13        inibir clausulas
|||  C                   GOTO      QW
||+--C                   END
|+---C                   END
+----C                   END
     C* Dar mensagem a avisar que a apolice é do novo produto CONDOMINIO
     C   20
+----COR 21              DO
|+---C     RR            IFEQ      05
||+--C     NEWPRD        IFEQ      'S'
|||+-C     CONDOM        IFEQ      'C'
|||| C                   MOVE      MSG(147)      MENSER
|||| C                   EXSR      ERRO
|||| C                   SETON                                        13        inibir clausulas
|||| C                   GOTO      QW
|||+-C                   END
||+--C                   END
|+---C                   END
+----C                   END
     C*
     C     QW            TAG
     C************************************************************************
     C   20
     COR 21              EXSR      BEGS04
     C   20
     COR 21              GOTO      XPTO04
     C*
     C                   Z-ADD     0             OPCAO             1 0
     C                   MOVE      ' '           SPAC40           40
     C**************************************************************************
     C*     1º ECRAN DE INPUT para
     C*     criação de novas apolices - NOME E MORADA DO SEGURADO
     C**************************************************************************
     C     XPTO02        TAG
      *
     C                   Z-ADD     NSEG          WNSEG             5 0
      *
     C                   MOVE      'B'           INDREG
     C*
     C                   EVAL      Func#Key = *BLANKS
+----C                   DOW       (Func#Key<>Key#F03) AND (Func#Key<>Key#F12)
|    C*                  DOW       (Func#Key <> KEY#F07)
|    C                   EXCEPT    INPU02
|    C                   READ      ECRAN                                  92
|+---C                   IF        (Func#Key = Key#F04)
||   C                   ExSr      RunPopUp
||   C                   ITER
|+---C                   ELSEIF    (Func#Key = Key#F01)
||   C                   ExSr      ViewHelp
||   C                   ITER
|+---C                   ELSE
||   C                   LEAVE
|+---C                   ENDIF
+----C                   ENDDO
     C
+----C                   IF        (Func#Key=Key#F03) OR (Func#Key=Key#F12)
|    C*                  IF        (Func#Key = Key#F07)
|    C                   EXSR      BEGS01
|    C                   GOTO      XPTO01
+----C                   ENDIF
     C*
     C                   EXSR      VFSEG
     C   05              GOTO      XPTO02
     C*
     C                   SETON                                        21
     C     W1            ADD       1             W1
     C     W1            COMP      2                                    10
+----C   10              DO
|+---C     NSEG          IFNE      WNSEG
||   C                   GOTO      XPTO02
|+---C                   END
+----C                   END
     C*
+----C     FLAG85        IFEQ      *BLANK
|    C   19              EXSR      BEGS03
+----C                   ELSE
|    C                   MOVE      *BLANK        FLAG85
+----C                   END
     C*
     C   19              GOTO      XPTO03
     C*
     C   21
     CAN 15              MOVE      PRSACT        PRSANT
     C   22              EXSR      BEGS04
     C   22              GOTO      XPTO04
     C**************************************************************************
     C*     2º ECRAN DE INPUT para
     C*     criação de novas apolices - DADOS GERAIS, RISCOS, MEDIADORES, ETC
     C**************************************************************************
     C     XPTO03        TAG
     C                   MOVE      'C'           INDREG
     C*
     C     RR            COMP      15                                 9898
      *
     C************************************************************************
     C* ASILVA, Dezembro 2006
     C* Inibir ou Desinibir Clausulas no ecran de criação da apolice
     C* consoante é ou não do novo produto RESIDENTIA, MERCANTILE, CONDOMINIO
     C                   SETOFF                                       06        desinibir clausulas
     C*
+----C     RR            IFEQ      05                                           se é residentia ou
|    C*                                                    condominio
|    C                   SETON                                        06        inibir clausulas
+----C                   END
     C*
+----C     RR            IFEQ      55                                           se é mercantile
|    C                   SETON                                        06        inibir clausulas
+----C                   END
     C************************************************************************
     C                   EVAL      Func#Key = *BLANKS
+----C                   DOW       (Func#Key<>Key#F03) AND (Func#Key<>Key#F12)
|    C*                  DOW       (Func#Key <> KEY#F07)
|    C                   EXCEPT    INPU02
|    C                   EXCEPT    INPU03
|    C                   READ      ECRAN                                  92
|+---C                   IF        (Func#Key = Key#F04)
||   C                   ExSr      RunPopUp
||   C                   ITER
|+---C                   ELSEIF    (Func#Key = Key#F01)
||   C                   ExSr      ViewHelp
||   C                   ITER
|+---C                   ELSEIF    (Func#Key = KEY#F05)
||   C                   EXSR      Opcoes
||   C                   ITER
|+---C                   ELSE
||   C                   LEAVE
|+---C                   ENDIF
+----C                   ENDDO
     C                   SETOFF                                           35    6164
     C                   READ      ECRAN                                  92
     C
+----C                   IF        (Func#Key=Key#F03) OR (Func#Key=Key#F12)
|    C*                  IF        (Func#Key = Key#F07)
|+---C                   IF        (Opcao <> OPT#Consulta)
||   C                   ROLBK
|+---C                   ENDIF
|    C                   EXSR      BEGS01                                                      asilv
|    C                   GOTO      ACABLR                                                      asilv
+----C                   ENDIF
     C
+----C     RR            IFEQ      02                                           Acid pessoais
|    C   KH              GOTO      XPTO03
+----C                   END
+----C     RR            IFEQ      15                                           Acid trabalho
|    C   KI              GOTO      XPTO03
+----C                   END
     C*
     C                   MOVE      *BLANKS       COMISS
     C*
+----C     TALT          IFEQ      0                                            apolice nova
|    C*
|    C     RR            LOOKUP    TABRR         TABPC                    41
|    C   41              MOVE      TABPC         COMISS
|    C* calcular % de comissões SEM fenomenos sismicos
|    C   41              EXSR      COMSFS
|    C* ACIDENTES DE TRABALHO
|+---C     RR            IFEQ      15
||+--C     TPT           IFEQ      'P'
|||  C* calcular % de comissões COM fenomenos sismicos
|||  C                   EXSR      COMCFS
||+--C                   END
|+---C                   END
|    C* INCÊNDIO
|+---C     RR            IFEQ      01
||   C                   Z-ADD     1             X                 1 0
||   C     16            LOOKUP    CL(X)                                  99
||   C* calcular % de comissões COM fenomenos sismicos
||   C   99              EXSR      COMCFS
|+---C                   END
|    C* HABITAÇÃO
|+---C     RR            IFEQ      05
||   C                   Z-ADD     1             X                 1 0
||   C     01            LOOKUP    CL(X)                                  99
||   C* calcular % de comissões COM fenomenos sismicos
||   C   99              EXSR      COMCFS
|+---C                   END
|    C* PERDA DE LUCROS
|+---C     RR            IFEQ      06
||   C                   Z-ADD     1             X                 1 0
||   C     16            LOOKUP    CL(X)                                  99
||   C* calcular % de comissões COM fenomenos sismicos
||   C   99              EXSR      COMCFS
|+---C                   END
|    C* MERCANTILE
|+---C     RR            IFEQ      55
||   C                   Z-ADD     1             X                 1 0
||   C     01            LOOKUP    CL(X)                                  99
||   C* calcular % de comissões COM fenomenos sismicos
||   C   99              EXSR      COMCFS
|+---C                   END
+----C                   END
      * DESCRIÇÃO POSTAL
+----C     CPLCB1        IFNE      0
|    C     CPLCB1        CHAIN     MPOSTNP                            90
|    C  N90              MOVEL     DCPOST        CPLCB2
|    C   90              MOVEL     *BLANKS       CPLCB2
|    C   90              MOVE      MSG(139)      MENSER
|    C   90              EXSR      ERRO
|    C   90              GOTO      XPTO03
+----C                   ELSE
|    C                   MOVEL     *BLANKS       CPLCB2
+----C                   END
     C*
     C                   SETOFF                                       08
     C     IM            COMP      0                                    53
     C  N53IM            COMP      1                                  53
     C   53              MOVE      MSG(06)       MENSER
     C   53              EXSR      ERRO
     C   53              GOTO      XPTO03
     C*
     C     SIT           COMP      1                                  5454
     C   54              MOVE      MSG(07)       MENSER
     C   54              EXSR      ERRO
     C   54              GOTO      XPTO03
     C*
     C     DDINIC        COMP      1                                    55
     C  N55DDINIC        COMP      31                                 55
     C   55              MOVE      MSG(08)       MENSER
     C   55              EXSR      ERRO
     C   55              GOTO      XPTO03
     C*
     C     MMINIC        COMP      1                                    56
     C  N56MMINIC        COMP      12                                 56
     C   56              MOVE      MSG(09)       MENSER
     C   56              EXSR      ERRO
     C   56              GOTO      XPTO03
     C*
     C     AAINI         COMP      AAIPL                                57
     C   57AAIPL         SUB       1             AAIP0             4 0
     C   57AAINI         COMP      AAIP0                                57
     C  N57AAINI         COMP      AAIP1                              57
     C   57              MOVE      MSG(10)       MENSER
     C   57              EXSR      ERRO
     C   57              GOTO      XPTO03
     C*
     C     DUR           COMP      1                                    58
     C  N58DUR           COMP      2                                  58
     C   58              MOVE      MSG(11)       MENSER
     C   58              EXSR      ERRO
     C   58              GOTO      XPTO03
     C*
     C     DDFIM         COMP      0                                    59
     C  N59DDFIM         COMP      31                                 59
     C   59              MOVE      MSG(12)       MENSER
     C   59              EXSR      ERRO
     C   59              GOTO      XPTO03
     C*
     C     MMFIM         COMP      0                                    60
     C  N60MMFIM         COMP      12                                 60
     C   60              MOVE      MSG(13)       MENSER
     C   60              EXSR      ERRO
     C   60              GOTO      XPTO03
     C*
     C                   EXSR      VDTFIM
     C   30              MOVE      MSG(15)       MENSER
     C   30              EXSR      ERRO
     C   30              GOTO      XPTO03
     C*
     C                   EXSR      VFRAC
     C   31              MOVE      MSG(16)       MENSER
     C   31              EXSR      ERRO
     C   31              GOTO      XPTO03
     C*
     C                   EXSR      VMES1
     C   32              MOVE      MSG(17)       MENSER
     C   32              EXSR      ERRO
     C   32              GOTO      XPTO03
     C*
     C                   EXSR      VCONT1
     C   33              MOVE      MSG(18)       MENSER
     C   33              EXSR      ERRO
     C   33              GOTO      XPTO03
     C*
     C     CEMIS         COMP      1                                    62
     C  N62CEMIS         COMP      3                                  62
     C   62              MOVE      MSG(19)       MENSER
     C   62              EXSR      ERRO
     C   62              GOTO      XPTO03
     C*
     C     ISEN          COMP      0                                    63
     C  N63ISEN          COMP      3                                  63
+----C   63              DO
|    C     ISEN          COMP      6                                    63
|    C  N63ISEN          COMP      7                                  63
+----C                   END
     C   63              MOVE      MSG(20)       MENSER
     C   63              EXSR      ERRO
     C   63              GOTO      XPTO03
     C*
     C     CONC          CHAIN     FCONC                              34
     C*
     C   34              MOVE      MSG(21)       MENSER
     C   34              EXSR      ERRO
     C   34              GOTO      XPTO03
     C*
     C                   EXSR      VMED1
     C   61
     COR 64              EXSR      ERRO
     C   61
     COR 64              GOTO      XPTO03
     C   35              MOVE      MSG(22)       MENSER
     C   35              EXSR      ERRO
     C   35              GOTO      XPTO03
     C*
     C                   EXSR      VCOM1
     C   36              MOVE      MSG(23)       MENSER
     C   36              EXSR      ERRO
     C   36              GOTO      XPTO03
     C*
     C                   EXSR      VMED2
     C   02              MOVE      MSG(24)       MENSER
     C   02              EXSR      ERRO
     C   02              GOTO      XPTO03
     C*
     C                   SETOFF                                       94
+----C     CRHIP         IFGT      *ZEROS
|    C                   MOVEL     20            K7                7 0
|    C                   MOVE      CRHIP         K7
|    C     K7            CHAIN     ENTIDADE                           94
|    C   94              MOVE      MSG(138)      MENSER
|    C   94              EXSR      ERRO
|    C   94              GOTO      XPTO03
+----C                   END
     C*
     C     AVISN         COMP      'S'                                7979
     C   79AVISN         COMP      'N'                                7979
     C   79              MOVE      MSG(144)      MENSER
     C   79              EXSR      ERRO
     C   79              GOTO      XPTO03
     C* validar ADC
+----C     ADC           IFEQ      0
|+---C     NIB           IFNE      *BLANK
||   C                   EXSR      VALADC
||+--C     OUMSG         IFNE      *BLANKS
|||  C                   MOVEL     OUMSG         MENSER
|||  C                   EXSR      ERRO
|||  C                   GOTO      XPTO03
||+--C                   END
|+---C                   END
+----C                   END
     C* validar NIB
+----C     NIB           IFEQ      *BLANK
|+---C     ADC           IFNE      *ZERO
||   C                   EXSR      VALADC
||+--C     OUMSG         IFNE      *BLANKS
|||  C                   MOVEL     OUMSG         MENSER
|||  C                   EXSR      ERRO
|||  C                   GOTO      XPTO03
||+--C                   END
|+---C                   END
+----C                   END
     C* validação Seguro Novo com ADC e NIB preenchido
     C*          TALT      IFEQ *ZERO                      seguro novo
     C*          ADC       IFNE *ZERO
     C*                    MOVE MSG,154   MENSER
     C*                    EXSR ERRO
     C*                    GOTO XPTO03
     C*                    END
     C*                    END
     C* se ADC e NIB preenchidos o AVISN têm que ser S
+----C     ADC           IFNE      *ZERO
|+---C     NIB           IFNE      *BLANK
||   C                   MOVE      'S'           AVISN
|+---C                   END
+----C                   END
     C* validação do codigo empregado
+----C     EMPREG        IFNE      'S'
|+---C     EMPREG        IFNE      ' '
||   C                   MOVE      MSG(150)      MENSER
||   C                   EXSR      ERRO
||   C                   GOTO      XPTO03
|+---C                   END
+----C                   END
     C* validação do ano do edífico
+----C     ANOEDI        IFNE      *ZERO
|+---C     ANOEDI        IFLT      1800
||   C                   MOVE      MSG(151)      MENSER
||   C                   EXSR      ERRO
||   C                   GOTO      XPTO03
|+---C                   END
|+---C     ANOEDI        IFGT      YYYY
||   C                   MOVE      MSG(151)      MENSER
||   C                   EXSR      ERRO
||   C                   GOTO      XPTO03
|+---C                   END
+----C                   END
     C* validação do codigo postal do edífico
+----C     CPEDI         IFNE      *ZERO
|+---C     CPEDI         IFLT      1000
||   C                   MOVE      MSG(152)      MENSER
||   C                   EXSR      ERRO
||   C                   GOTO      XPTO03
|+---C                   END
+----C                   END
     C*
     C                   Z-ADD     0             TALT
     C                   Z-ADD     0             DDALT
     C                   Z-ADD     0             MMALT
     C                   Z-ADD     0             AAALT
     C                   Z-ADD     0             AAALTX
     C                   Z-ADD     0             MALT
     C                   Z-ADD     0             ACTA
     C* validar risco 1
     C     CR1           COMP      0                                    08
     C  N08CR1           COMP      7                                  08
     C   08CR1           COMP      9                                  0808
     C   08              MOVE      MSG(26)       MENSER
     C   08              EXSR      ERRO
     C   08              GOTO      XPTO03
     C* validar capital + risco
     C     CR1           COMP      0                                      08
     C   08CAP1          COMP      0                                  08
     C   08              MOVE      MSG(26)       MENSER
     C   08              EXSR      ERRO
     C   08              GOTO      XPTO03
      *
     C     CR2           COMP      0                                      80
     C   80CAP2          COMP      0                                  80
     C   80              MOVE      MSG(26)       MENSER
     C   80              EXSR      ERRO
     C   80              GOTO      XPTO03
      *
     C     CR3           COMP      0                                      80
     C   80CAP3          COMP      0                                  80
     C   80              MOVE      MSG(26)       MENSER
     C   80              EXSR      ERRO
     C   80              GOTO      XPTO03
     C* validar capital do risco 1
     C     CAP1          COMP      0                                      26
     C   26              MOVE      MSG(108)      MENSER
     C   26              EXSR      ERRO
     C   26              GOTO      XPTO03
     C*
     C                   EXSR      VCAPT
     C   38              MOVE      MSG(27)       MENSER
     C   38              EXSR      ERRO
     C   38              GOTO      XPTO03
     C* validar taxa do risco 1
     C  N95TX1           COMP      0                                      17
     C   95TX1AT         COMP      0                                      17
     C   17              MOVE      MSG(109)      MENSER
     C   17              EXSR      ERRO
     C   17              GOTO      XPTO03
     C*********************************************************************
     C* validação: no ramo 05 ou 55 (novos produtos) não pode
     C* haver simultâneamente o Risco 2 e 3
     C*********************************************************************
+--- C     RR            IFEQ      05
|    C                   GOTO      MACACO
+----C                   END
+----C     RR            IFEQ      55
|    C                   GOTO      MACACO
+----C                   END
     C                   GOTO      PATO
     C*
     C     MACACO        TAG
     C*
     C     CR1           COMP      2                                      97
     C   97CR2           COMP      3                                      97
     C   97              MOVE      MSG(148)      MENSER
     C   97              EXSR      ERRO
     C   97              GOTO      XPTO03
     C*
     C     CR1           COMP      2                                      97
     C   97CR3           COMP      3                                      97
     C   97              MOVE      MSG(148)      MENSER
     C   97              EXSR      ERRO
     C   97              GOTO      XPTO03
     C*
     C     CR2           COMP      2                                      97
     C   97CR3           COMP      3                                      97
     C   97              MOVE      MSG(148)      MENSER
     C   97              EXSR      ERRO
     C   97              GOTO      XPTO03
     C*
     C     CR1           COMP      3                                      97
     C   97CR2           COMP      2                                      97
     C   97              MOVE      MSG(148)      MENSER
     C   97              EXSR      ERRO
     C   97              GOTO      XPTO03
     C*
     C     CR1           COMP      3                                      97
     C   97CR3           COMP      2                                      97
     C   97              MOVE      MSG(148)      MENSER
     C   97              EXSR      ERRO
     C   97              GOTO      XPTO03
     C*
     C     CR2           COMP      3                                      97
     C   97CR3           COMP      2                                      97
     C   97              MOVE      MSG(148)      MENSER
     C   97              EXSR      ERRO
     C   97              GOTO      XPTO03
      *
     C     RR            IFEQ      05
     C                   SETON                                        97
     C     7             LOOKUP    ACR                                    99
     C   992             LOOKUP    ACR                                    97
     C  N97              DO
     C                   SETON                                        0880
     C                   MOVE      MSG(159)      MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   END
     C                   SETOFF                                       97
     C                   END
      *
     C     PATO          TAG
     C* ----------------------------------------------------
     C* ACIDENTES DE TRABALHO - DETERMINA PREMIO
     C*
+----C     RR            IFEQ      15
|    C*
|+---C     PR1           IFEQ      *ZEROS
||   C     CAP1          MULT      TX1AT         WORK             11 2
||   C     WORK          DIV(H)    100           PR1
||+--C     ISEN          IFEQ      1
|||  C     PR1           DIV(H)    1.20          PR1
||+--C                   END
|+---C                   END
|    C*
|+---C     PR1           IFLT      30
||+--C     FLAGR         IFEQ      '0'
|||+-C     ISEN          IFEQ      1
|||| C                   MOVE      MSG(142)      MENSER
|||+-C                   ELSE
|||| C                   MOVE      MSG(134)      MENSER
|||+-C                   END
|||  C                   MOVE      '1'           FLAGR
|||  C                   GOTO      XPTO03
||+--C                   END
|+---C                   END
|    C*
|+---C     FLAGI         IFEQ      '0'
||+--C     ISEN          IFEQ      1
|||  C                   MOVE      '1'           FLAGI
|||  C                   MOVE      MSG(143)      MENSER
|||  C                   GOTO      XPTO03
||+--C                   END
|+---C                   END
|    C*
|    C                   Z-ADD     PR1           PRSACT
|    C                   Z-ADD     CAP1          CAPTOT
|    C                   Z-ADD     TX1AT         TXMAT
|    C*
+----C                   END
     C* ----------------------------------------------------
     C* validar prémio do risco 1
     C     PR1           COMP      0                                      18
     C   18              MOVE      MSG(110)      MENSER
     C   18              EXSR      ERRO
     C   18              GOTO      XPTO03
     C*
     C                   Z-ADD     0             PRTOT             9 2
     C     PR1           ADD       PR2           PRTOT
     C     PRTOT         ADD       PR3           PRTOT
     C     PRSACT        COMP      PRTOT                              3939
     C   39              MOVE      MSG(28)       MENSER
     C   39              EXSR      ERRO
     C   39              GOTO      XPTO03
     C*
     C                   Z-ADD     0             PRSANT
     C                   Z-ADD     0             PRIANT
     C*
     C* Introduzido em 06/04/88
     C****                 Z-ADD0         PRIACT
     C*    --- / ---
     C                   EXSR      VNASC
     C   40              MOVE      MSG(29)       MENSER
     C   40              EXSR      ERRO
     C   40              GOTO      XPTO03
     C*
     C                   Z-ADD     0             TEST
     C                   Z-ADD     0             TXEST
     C*   =   Novas Validações às clausulas de RAMOS   =   *
     C                   MOVE      *ZEROS        WRKCL             2 0
     C                   SETOFF                                           99
     C*
     C     CL01          COMP      0                                    65
     C  N65              MOVE      CL01          WRKCL
     C  N65              EXSR      VCLAUS
     C  N65
     CAN 99              SETON                                        65
     C   65              MOVE      MSG(30)       MENSER
     C   65              EXSR      ERRO
     C   65              GOTO      XPTO03
     C*
     C     CL02          COMP      0                                    66
     C  N66              MOVE      CL02          WRKCL
     C  N66              EXSR      VCLAUS
     C  N66
     CAN 99              SETON                                        66
     C   66              MOVE      MSG(31)       MENSER
     C   66              EXSR      ERRO
     C   66              GOTO      XPTO03
     C*
     C     CL03          COMP      0                                    67
     C  N67              MOVE      CL03          WRKCL
     C  N67              EXSR      VCLAUS
     C  N67
     CAN 99              SETON                                        67
     C   67              MOVE      MSG(32)       MENSER
     C   67              EXSR      ERRO
     C   67              GOTO      XPTO03
     C*
     C     CL04          COMP      0                                    68
     C  N68              MOVE      CL04          WRKCL
     C  N68              EXSR      VCLAUS
     C  N68
     CAN 99              SETON                                        68
     C   68              MOVE      MSG(33)       MENSER
     C   68              EXSR      ERRO
     C   68              GOTO      XPTO03
     C*
     C     CL05          COMP      0                                    69
     C  N69              MOVE      CL05          WRKCL
     C  N69              EXSR      VCLAUS
     C  N69
     CAN 99              SETON                                        69
     C   69              MOVE      MSG(34)       MENSER
     C   69              EXSR      ERRO
     C   69              GOTO      XPTO03
     C*
     C     CL06          COMP      0                                    70
     C  N70              MOVE      CL06          WRKCL
     C  N70              EXSR      VCLAUS
     C  N70
     CAN 99              SETON                                        70
     C   70              MOVE      MSG(35)       MENSER
     C   70              EXSR      ERRO
     C   70              GOTO      XPTO03
     C*
     C                   Z-ADD     0             TOTREC
     C     RR            COMP      01                                     50     RG 17/04/91
     C   50RA            COMP      'S'                                5050       RG 17/04/91
     C   50RA            COMP      'N'                                5050       RG 17/04/91
     C   50              MOVE      MSG(36)       MENSER
     C   50              EXSR      ERRO
     C   50              GOTO      XPTO03
     C*
     C     RR            COMP      01                                     50
     C   50RA            COMP      'S'                                    50
     C   50WCL           COMP      0                                      50
     C   50              MOVE      MSG(107)      MENSER
     C   50              EXSR      ERRO
     C   50              GOTO      XPTO03
     C*
     C     TXIND         COMP      0                                  89         RG 12/03/97
     C  N89              MOVE      *BLANK        INDEX                           RG 12/03/97
     C   89INDEX         COMP      'S'                                8989       RG 12/03/97
     C   89INDEX         COMP      'N'                                8989       RG 12/03/97
     C   89              MOVE      MSG(36)       MENSER                          RG 12/03/97
     C   89              EXSR      ERRO                                          RG 12/03/97
     C   89              GOTO      XPTO03                                        RG 12/03/97
     C*
+----C     RR            IFEQ      02
|+---C     CGRUPO        IFEQ      0
||   C                   MOVE      MSG(157)      MENSER
||   C                   EXSR      ERRO
||   C                   GOTO      XPTO03
|+---C                   END
|+---C     NPESS         IFEQ      0
||   C                   MOVE      MSG(157)      MENSER
||   C                   EXSR      ERRO
||   C                   GOTO      XPTO03
|+---C                   END
|    C     CGRUPO        LOOKUP    ACG                                    99
|+---C  N99              DO
||   C                   MOVE      MSG(158)      MENSER
||   C                   EXSR      ERRO
||   C                   GOTO      XPTO03
|+---C                   END
+----C                   END
     C*
+----C     RR            IFNE      02
|+---C     CGRUPO        IFNE      0
||   C                   MOVE      MSG(156)      MENSER
||   C                   EXSR      ERRO
||   C                   GOTO      XPTO03
|+---C                   END
|+---C     NPESS         IFNE      0
||   C                   MOVE      MSG(156)      MENSER
||   C                   EXSR      ERRO
||   C                   GOTO      XPTO03
|+---C                   END
+----C                   END
      *
     C*   Validacao de  Campos Especificos do RAMO = 15      PC 03/03/00
+----C     RR            IFEQ      15
|    C*
|+---C     CAPFAT        IFEQ      *ZEROS
||   C                   MOVE      MSG(133)      MENSER
||   C                   EXSR      ERRO
||   C                   GOTO      XPTO03
|+---C                   END
|    C*
|+---C     PROFSL        IFEQ      *ZEROS
||   C                   MOVE      MSG(114)      MENSER
||   C                   EXSR      ERRO
||   C                   GOTO      XPTO03
|+---C                   ELSE
||   C                   Z-ADD     *ZEROS        KPROF             6 0
||   C                   MOVEL     00            WKPROF            5 0
||   C                   MOVE      PROFSL        WKPROF
||   C                   MOVE      WKPROF        KPROF
||   C                   MOVE      1             KPROF
||   C     KPROF         CHAIN     FTEXTOS                            05
||+--C   05              DO
|||  C                   MOVE      MSG(117)      MENSER
|||  C                   EXSR      ERRO
|||  C                   GOTO      XPTO03
||+--C                   END
|+---C                   END
|    C*
|+---C     MOD           IFEQ      *BLANKS
||   C                   MOVE      MSG(115)      MENSER
||   C                   EXSR      ERRO
||   C                   GOTO      XPTO03
|+---C                   ELSE
||+--C     MOD           IFNE      'F'
|||+-C     MOD           IFNE      'V'
|||| C                   MOVE      MSG(118)      MENSER
|||| C                   EXSR      ERRO
|||| C                   GOTO      XPTO03
|||+-C                   ELSE
|||+-C                   END
||+--C                   ELSE
||+--C                   END
|+---C                   END
|    C*
|+---C     TPT           IFEQ      *BLANKS
||   C                   MOVE      MSG(131)      MENSER
||   C                   EXSR      ERRO
||   C                   GOTO      XPTO03
|+---C                   ELSE
||+--C     TPT           IFNE      'P'
|||+-C     TPT           IFNE      'O'
|||| C                   MOVE      MSG(132)      MENSER
|||| C                   EXSR      ERRO
|||| C                   GOTO      XPTO03
|||+-C                   ELSE
|||+-C                   END
||+--C                   ELSE
||+--C                   END
|+---C                   END
|    C*
|+---C     CDCAE         IFEQ      *BLANKS
||   C                   MOVE      MSG(116)      MENSER
||   C                   EXSR      ERRO
||   C                   GOTO      XPTO03
|+---C                   ELSE
||   C                   MOVE      *BLANKS       KCAE             12
||   C                   MOVEL     20            WKCAE             8
||   C                   MOVE      CDCAE         WKCAE
||   C                   MOVEL     WKCAE         KCAE
||   C     KCAE          CHAIN     TABELAS                            05
||+--C   05              DO
|||  C                   MOVE      MSG(119)      MENSER
|||  C                   EXSR      ERRO
|||  C                   GOTO      XPTO03
||+--C                   END
|+---C                   END
|    C*
|+---C     MODALI        IFNE      'FP'
||+--C     CTLCAE        IFEQ      '  '
|||  C                   MOVE      MSG(119)      MENSER
|||  C                   EXSR      ERRO
|||  C                   GOTO      XPTO03
||+--C                   END
|+---C                   END
|    C*
|+---C     CR1           IFLT      1
||   C                   MOVE      MSG(120)      MENSER
||   C                   EXSR      ERRO
||   C                   GOTO      XPTO03
|+---C                   END
|    C*
|+---C     CR1           IFGT      2
||   C                   MOVE      MSG(120)      MENSER
||   C                   EXSR      ERRO
||   C                   GOTO      XPTO03
|+---C                   END
|    C*
|+---C     CR1           IFEQ      2
||+--C     MOD           IFNE      'V'
|||  C                   MOVE      MSG(121)      MENSER
|||  C                   EXSR      ERRO
|||  C                   GOTO      XPTO03
||+--C                   END
|+---C                   END
+----C                   END
     C*   Validacao de  Campos Especificos do RAMO = 55      PC 30/10/91
     C     RR            COMP      25                                     84
     C  N84RR            COMP      35                                     84
     C  N84RR            COMP      55                                     84
     C   84CLASS         COMP      'A'                                8484
     C   84CLASS         COMP      'B'                                8484
     C   84CLASS         COMP      'C'                                8484
     C   84CLASS         COMP      'D'                                8484
     C   84              MOVE      MSG(37)       MENSER
     C   84              EXSR      ERRO
     C   84              GOTO      XPTO03
     C*
     C     RR            COMP      25                                     87
     C  N87RR            COMP      35                                     87
     C  N87RR            COMP      55                                     87
     C   87PERDCQ        COMP      'S'                                8787
     C   87PERDCQ        COMP      'N'                                8787
     C   87              MOVE      MSG(38)       MENSER
     C   87              EXSR      ERRO
     C   87              GOTO      XPTO03
     C*
     C     RR            COMP      25                                     87
     C  N87RR            COMP      35                                     87
     C  N87RR            COMP      55                                     87
     C   87PERDCQ        COMP      'N'                                    87
     C   87TPERD         COMP      ' '                                8787
     C   87              MOVE      MSG(39)       MENSER
     C   87              EXSR      ERRO
     C   87              GOTO      XPTO03
     C*
     C     RR            COMP      25                                     87
     C  N87RR            COMP      35                                     87
     C  N87RR            COMP      55                                     87
     C   87PERDCQ        COMP      'N'                                    87
     C   87PPERDI        COMP      00                                 8787
     C   87              MOVE      MSG(40)       MENSER
     C   87              EXSR      ERRO
     C   87              GOTO      XPTO03
     C*
     C     RR            COMP      25                                     87
     C  N87RR            COMP      35                                     87
     C  N87RR            COMP      55                                     87
     C   87PERDCQ        COMP      'N'                                    87
     C   87ENCFIX        COMP      0                                  8787
     C   87              MOVE      MSG(41)       MENSER
     C   87              EXSR      ERRO
     C   87              GOTO      XPTO03
     C*
     C     RR            COMP      25                                     87
     C  N87RR            COMP      35                                     87
     C  N87RR            COMP      55                                     87
     C   87PERDCQ        COMP      'N'                                    87
     C   87PINDMN        COMP      0                                  8787
     C   87              MOVE      MSG(42)       MENSER
     C   87              EXSR      ERRO
     C   87              GOTO      XPTO03
     C*
     C     RR            COMP      25                                     25
     C  N25RR            COMP      35                                     25
     C  N25RR            COMP      55                                     25
     C   25PERDCQ        COMP      'S'                                    25
     C   25TPERD         COMP      'I'                                2525
     C   25TPERD         COMP      'F'                                2525
     C   25              MOVE      MSG(43)       MENSER
     C   25              EXSR      ERRO
     C   25              GOTO      XPTO03
     C*
     C     RR            COMP      25                                     25
     C  N25RR            COMP      35                                     25
     C  N25RR            COMP      55                                     25
     C   25TPERD         COMP      'I'                                    25
     C   25PPERDI        COMP      20                                 2525
     C   25PPERDI        COMP      25                                 2525
     C   25PPERDI        COMP      30                                 2525
     C   25              MOVE      MSG(44)       MENSER
     C   25              EXSR      ERRO
     C   25              GOTO      XPTO03
     C*
     C     RR            COMP      25                                     25
     C  N25RR            COMP      35                                     25
     C  N25RR            COMP      55                                     25
     C   25TPERD         COMP      'I'                                    25
     C   25ENCFIX        COMP      0                                  2525
     C   25              MOVE      MSG(45)       MENSER
     C   25              EXSR      ERRO
     C   25              GOTO      XPTO03
     C*
     C     RR            COMP      25                                     25
     C  N25RR            COMP      35                                     25
     C  N25RR            COMP      55                                     25
     C   25TPERD         COMP      'I'                                    25
     C   25PINDMN        COMP      0                                  2525
     C   25              MOVE      MSG(46)       MENSER
     C   25              EXSR      ERRO
     C   25              GOTO      XPTO03
     C*
     C     RR            COMP      25                                     25
     C  N25RR            COMP      35                                     25
     C  N25RR            COMP      55                                     25
     C   25TPERD         COMP      'F'                                    25
     C   25ENCFIX        COMP      0                                      25
     C   25              MOVE      MSG(101)      MENSER
     C   25              EXSR      ERRO
     C   25              GOTO      XPTO03
     C*
     C     RR            COMP      25                                     25
     C  N25RR            COMP      35                                     25
     C  N25RR            COMP      55                                     25
     C   25TPERD         COMP      'F'                                    25
     C   25PINDMN        COMP      0                                      25
     C   25              MOVE      MSG(111)      MENSER
     C   25              EXSR      ERRO
     C   25              GOTO      XPTO03
     C*
     C     RR            COMP      25                                     25
     C  N25RR            COMP      35                                     25
     C  N25RR            COMP      55                                     25
     C   25TPERD         COMP      'F'                                    25
     C   25PINDMN        COMP      3                                  2525
     C   25PINDMN        COMP      6                                  2525
     C   25PINDMN        COMP      12                                 2525
     C   25              MOVE      MSG(47)       MENSER
     C   25              EXSR      ERRO
     C   25              GOTO      XPTO03
     C*
     C                   SETOFF                                       93
+----C     JMM1          IFGT      *ZEROS
|    C     JMM1          CHAIN     MCORNP                             05
|    C   05              SETON                                        93
|    C   93              MOVE      MSG(122)      MENSER
|    C   93              EXSR      ERRO
|    C   93              GOTO      XPTO03
|     * Controla se activo (PF 12.02.08)
|+---C  N05ALTCR         IFEQ      'S'
||   C                   SETON                                        93
||   C   93              MOVE      MSG(155)      MENSER
||   C   93              EXSR      ERRO
||   C   93              GOTO      XPTO03
|+---C                   END
|     *
|+---C  N05TMEDCR        IFNE      1
||   C                   SETON                                        93
||   C   93              MOVE      MSG(129)      MENSER
||   C   93              EXSR      ERRO
||   C   93              GOTO      XPTO03
|+---C                   END
|     *
|+---C     JMC1          IFEQ      *ZEROS
||+--C     JMM1          IFNE      418
|||  C                   SETON                                        92
|||  C   92              MOVE      MSG(123)      MENSER
|||  C   92              EXSR      ERRO
|||  C   92              GOTO      XPTO03
||+--C                   END
|+---C                   END
+----C                   ELSE
|+---C     JMC1          IFGT      *ZEROS
||   C                   SETON                                        92
||   C   92              MOVE      MSG(136)      MENSER
||   C   92              EXSR      ERRO
||   C   92              GOTO      XPTO03
|+---C                   END
+----C                   END
     C*
+----C     JMC1          IFGT      COM1
|    C                   SETON                                        92
|    C   92              MOVE      MSG(124)      MENSER
|    C   92              EXSR      ERRO
|    C   92              GOTO      XPTO03
+----C                   END
     C*
+----C     JMM2          IFGT      *ZEROS
|    C     JMM2          CHAIN     MCORNP                             05
|    C   05              SETON                                        37
|    C   37              MOVE      MSG(125)      MENSER
|    C   37              EXSR      ERRO
|    C   37              GOTO      XPTO03
|     * Controla se activo (PF 12.02.08)
|+---C  N05ALTCR         IFEQ      'S'
||   C                   SETON                                        37
||   C   37              MOVE      MSG(155)      MENSER
||   C   37              EXSR      ERRO
||   C   37              GOTO      XPTO03
|+---C                   END
|     *
|+---C     JMC2          IFEQ      *ZEROS
||+--C     JMM2          IFNE      418
|||  C                   SETON                                        64
|||  C   64              MOVE      MSG(126)      MENSER
|||  C   64              EXSR      ERRO
|||  C   64              GOTO      XPTO03
||+--C                   END
|+---C                   END
+----C                   ELSE
|+---C     JMC2          IFGT      *ZEROS
||   C                   SETON                                        64
||   C   64              MOVE      MSG(137)      MENSER
||   C   64              EXSR      ERRO
||   C   64              GOTO      XPTO03
|+---C                   END
+----C                   END
     C*
+----C     JMC2          IFGT      COM1
|    C                   SETON                                        64
|    C   64              MOVE      MSG(127)      MENSER
|    C   64              EXSR      ERRO
|    C   64              GOTO      XPTO03
+----C                   END
     C*
     C     JMC1          ADD       JMC2          WCOM              5 3
+----C     WCOM          IFGT      COM1
|    C                   SETON                                        6492
|    C   64              MOVE      MSG(127)      MENSER
|    C   64              EXSR      ERRO
|    C   64              GOTO      XPTO03
+----C                   END
     C*
+----C     RR            IFEQ      15                                           acidentes trabalho
|+---C     FLAGA         IFEQ      '0'
||   C                   MOVE      MSG(135)      MENSER
||   C                   MOVE      '1'           FLAGA
||   C                   GOTO      XPTO03
|+---C                   END
+----C                   END
     C*
     C                   MOVE      9999999       APOL
     C     RRAPOL        SETLL     REGAPOL
     C                   READP     REGAPOL                                92
     C   92              MOVE      MSG(105)      MENSER
     C   92              EXSR      ERRO
     C   92              GOTO      XPTO03
     C*
+----C     RR            IFNE      RRR
|    C                   MOVE      MSG(105)      MENSER
|    C                   EXSR      ERRO
|    C                   GOTO      XPTO03
+----C                   END
     C*
     C************************************************************************
     C* ASILVA, Dezembro 2006 - Fevereiro 2007
     C* ecran para perguntar se é RESIDENTIAL ou CONDOMINIO
     C************************************************************************
+----C                   IF        (OPCAO = OPT#Novo) AND (RR = 05)
|    C*
|    C                   SETON                                        96
|    C                   MOVE      *BLANK        SIMNAO            3
|    C*
|    C     TAG271        TAG
|    C                   EXCEPT    COND
|    C                   READ      ECRAN                                  92
|    C                   SETON                                        96
|+---C                   IF        (Func#Key=Key#F03) OR (Func#Key=Key#F12)
||   C*                  IF        (Func#Key = Key#F07)
||   C                   EXSR      BEGS01
||   C                   GOTO      ACABLR
|+---C                   ENDIF
|    C*
|+---C     SIMNAO        IFNE      'SIM'                                        é condominio
||+--C     SIMNAO        IFNE      'NAO'                                        é residential
|||  C                   SETOFF                                       96
|||  C                   GOTO      TAG271
||+--C                   END
|+---C                   END
|    C*
|+---C     SIMNAO        IFEQ      'SIM'                                        é condominio
||   C                   MOVE      'C'           CONDOM
|+---C                   ELSE
||   C                   MOVE      ' '           CONDOM                         é residential
|+---C                   END
+----C                   END
     C************************************************************************
     C* 2º ecran de CRIAÇÃO de apolice do novo produto RESIDENTIA
     C* com Coberturas Facultativas e Clausulas Especiais
     C************************************************************************
+----C                   IF        (OPCAO = OPT#Novo) AND (RR = 05)
|    C                             AND (CONDOM = *BLANKS)
|    C                   SETON                                        079623
|+---C                   IF        (WFirstTime)
||   C                   EVAL      WFirstTime = *OFF
||   C                   MOVE      *BLANK        FLAG              1
||   C                   MOVE      'S'           CE001                          assistencia ao lar
||   C                   MOVE      'S'           CA102                          exclui act index cap
||   C* se houver risco 1 (edificio) por defeito tem garagem
||+--C     CR1           IFEQ      1
|||  C                   MOVE      'S'           CD131                          garagem
||+--C                   END
||+--C     CR2           IFEQ      1
|||  C                   MOVE      'S'           CD131                          garagem
||+--C                   END
||+--C     CR3           IFEQ      1
|||  C                   MOVE      'S'           CD131                          garagem
||+--C                   END
||   C* se houver risco 2 (conteúdo por defeito tem livros, quadros e garagem
||+--C     CR1           IFEQ      2
|||  C                   MOVE      'S'           CD103                          livros
|||  C                   MOVE      'S'           CD130                          quadros
|||  C                   MOVE      'S'           CD131                          garagem
||+--C                   END
|||+-C     CR2           IFEQ      2
|||  C                   MOVE      'S'           CD103                          livros
|||  C                   MOVE      'S'           CD130                          quadros
|||  C                   MOVE      'S'           CD131                          garagem
||+--C                   END
||+--C     CR3           IFEQ      2
|||  C                   MOVE      'S'           CD103                          livros
|||  C                   MOVE      'S'           CD130                          quadros
|||  C                   MOVE      'S'           CD131                          garagem
||+--C                   END
||   C
||   C*==> Regressar ao exrã anterior
||   C                   MOVE      '*'           FLAG85            1
||   C                   EXCEPT    INPU02
||   C                   GOTO      XPTO02
|+---C                   ELSE
||   C
||   C* cobertura facultativa de fenómenos sísmicos - cálculo da comissão
||+--C     CF07          IFEQ      'S'
|||  C     RR            LOOKUP    TABRR         TABPC                    41
|||  C   41              MOVE      TABPC         COMISS
|||  C                   EXSR      COMCFS
||+--C                   END
|+---C                   END
|    C*------------------------------------------------------------------
|    C                   SETOFF                                           23
|    C                   GOTO      THEEND
+----C                   END
     C************************************************************************
     C* 2º ecran de CRIAÇÃO apolice nova para o novo produto MERCANTILE
     C*    (coberturas e clausulas)
     C************************************************************************
+----C                   IF        (OPCAO = OPT#Novo) AND (WRR = '55')
|    C                   SETON                                        079623
|    C                   MOVE      *BLANK        FLAG
|    C*
|    C     TAG13         TAG
|    C                   EXCEPT    MERC
|    C                   READ      ECRAN                                  92
|    C                   SETON                                        96
|    C*
|    C   KC              MOVE      '*'           FLAG85            1
|    C   KC              EXCEPT    INPU02
|    C   KC              GOTO      XPTO02
|    C*
|+---C                   IF        (Func#Key=Key#F03) OR (Func#Key=Key#F12)
||   C*                  IF        (Func#Key = Key#F07)
||   C                   EXSR      BEGS01
||   C                   GOTO      ACABLR
|+---C                   ENDIF
|    C*
|+---C     CF01          IFNE      'S'
||+--C     CF01          IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      TAG13
||+--C                   END
|+---C                   END
|    C*
|+---C     CF02          IFNE      'S'
||+--C     CF02          IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      TAG13
||+--C                   END
|+---C                   END
|    C*
|+---C     CF03          IFNE      'S'
||+--C     CF03          IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      TAG13
||+--C                   END
|+---C                   END
|    C*
|+---C     CF04          IFNE      'S'
||+--C     CF04          IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      TAG13
||+--C                   END
|+---C                   END
|    C*
|+---C     CF05          IFNE      'S'
||+--C     CF05          IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      TAG13
||+--C                   END
|+---C                   END
|    C*
|+---C     CF06          IFNE      'S'
||+--C     CF06          IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      TAG13
||+--C                   END
|+---C                   END
|    C*
|+---C     CF07          IFNE      'S'
||+--C     CF07          IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      TAG13
||+--C                   END
|+---C                   END
|    C*
|+---C     CF08          IFNE      'S'
||+--C     CF08          IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      TAG13
||+--C                   END
|+---C                   END
|    C*
|+---C     CF0800        IFNE      'S'
||+--C     CF0800        IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      TAG13
||+--C                   END
|+---C                   END
|    C*
|+---C     CF0830        IFNE      'S'
||+--C     CF0830        IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      TAG13
||+--C                   END
|+---C                   END
|    C*
|+---C     CF09          IFNE      'S'
||+--C     CF09          IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      TAG13
||+--C                   END
|+---C                   END
|    C*
|+---C     CF10          IFNE      'S'
||+--C     CF10          IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      TAG13
||+--C                   END
|+---C                   END
|    C*
|+---C     CF11          IFNE      'S'
||+--C     CF11          IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      TAG13
||+--C                   END
|+---C                   END
|    C*
|+---C     CF12          IFNE      'S'
||+--C     CF12          IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      TAG13
||+--C                   END
|+---C                   END
|    C*
|+---C     CF13          IFNE      'S'
||+--C     CF13          IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      TAG13
||+--C                   END
|+---C                   END
|    C*
|+---C     CF21          IFNE      'S'
||+--C     CF21          IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      TAG13
||+--C                   END
|+---C                   END
|    C*
|+---C     CF21          IFNE      ' '
||+--C     DCF21         IFEQ      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      TAG13
||+--C                   END
|+---C                   END
|    C*
|+---C     CF21          IFEQ      ' '
||+--C     DCF21         IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      TAG13
||+--C                   END
|+---C                   END
|    C*
|+---C     CF22          IFNE      'S'
||+--C     CF22          IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      TAG13
||+--C                   END
|+---C                   END
|    C*
|+---C     CE001         IFNE      'S'
||+--C     CE001         IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      TAG13
||+--C                   END
|+---C                   END
|    C*
|+---C     CE002         IFNE      'S'
||+--C     CE002         IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      TAG13
||+--C                   END
|+---C                   END
|    C*
|+---C     CE003         IFNE      'S'
||+--C     CE003         IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      TAG13
||+--C                   END
|+---C                   END
|    C*
|+---C     CE004         IFNE      'S'
||+--C     CE004         IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      TAG13
||+--C                   END
|+---C                   END
|    C*
|+---C     CA030         IFNE      'S'
||+--C     CA030         IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      TAG13
||+--C                   END
|+---C                   END
|    C*
|+---C     CA100         IFNE      'S'
||+--C     CA100         IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      TAG13
||+--C                   END
|+---C                   END
|    C*
|+---C     CA101         IFNE      'S'
||+--C     CA101         IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      TAG13
||+--C                   END
|+---C                   END
|    C*
|+---C     CA102         IFNE      'S'
||+--C     CA102         IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      TAG13
||+--C                   END
|+---C                   END
|    C*
|+---C     CD103         IFNE      'S'
||+--C     CD103         IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      TAG13
||+--C                   END
|+---C                   END
|    C*
|+---C     CD133         IFNE      'S'
||+--C     CD133         IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      TAG13
||+--C                   END
|+---C                   END
|    C*
|+---C     CD134         IFNE      'S'
||+--C     CD134         IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      TAG13
||+--C                   END
|+---C                   END
|    C*
|+---C     CD135         IFNE      'S'
||+--C     CD135         IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      TAG13
||+--C                   END
|+---C                   END
|    C*
|+---C     CD136         IFNE      'S'
||+--C     CD136         IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      TAG13
||+--C                   END
|+---C                   END
|    C*
|+---C     CD137         IFNE      'S'
||+--C     CD137         IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      TAG13
||+--C                   END
|+---C                   END
|    C* rotina para utilizador confirmar as opções
|+---C     FLAG          IFEQ      ' '
||   C                   MOVE      '*'           FLAG
||   C                   SETOFF                                       07
||   C                   GOTO      TAG13
|+---C                   ELSE
||   C*
||   C* cobertura facultativa de fenómenos sísmicos - cálculo da comissão
||+--C     CF07          IFEQ      'S'
|||  C     RR            LOOKUP    TABRR         TABPC                    41
|||  C   41              MOVE      TABPC         COMISS
|||  C                   EXSR      COMCFS
||+--C                   END
||   C*
||   C*------------------------------------------------------------------
||   C                   SETOFF                                           23
||   C                   GOTO      THEEND
|+---C                   END
|    C*
+----C                   END
     C************************************************************************
     C* 2º ecran de CRIAÇÃO de apolice do novo produto CONDOMINIO
     C* com Coberturas Facultativas e Clausulas Especiais
     C************************************************************************
+----C                   IF        (OPCAO = OPT#Novo) AND (RR = 05) AND
|    C                             (CONDOM = 'C')
|    C                   SETON                                        079623
|    C                   MOVE      *BLANK        FLAG              1
|+---C                   IF        (WFirstTime)
||   C                   EVAL      WFirstTime = *OFF
||   C                   MOVE      'S'           CE001
||   C                   MOVE      'S'           CA102
||   C
||   C*==> Regressar ao exrã anterior
||   C                   MOVE      '*'           FLAG85            1
||   C                   EXCEPT    INPU02
||   C                   GOTO      XPTO02
|+---C                   ELSE
||   C*
||   C* cobertura facultativa de fenómenos sísmicos - cálculo da comissão
||+--C     CF07          IFEQ      'S'
|||  C     RR            LOOKUP    TABRR         TABPC                    41
|||  C   41              MOVE      TABPC         COMISS
|||  C                   EXSR      COMCFS
||+--C                   END
|+---C                   ENDIF
|    C*
|    C*------------------------------------------------------------------
|    C                   SETOFF                                           23
|    C                   GOTO      THEEND
+----C                   END
     C*
     C     THEEND        TAG
     C************************************************************************
     C* adição novo registo ao GBASEDIO
     C                   EXSR      BEGS05
     C     RAPOL         ADD       1             APOL
     C*
+----C                   IF        (OPCAO=OPT#Novo) AND ((RR=05) OR (RR=55))
|    C                   MOVE      'S'           NEWPRD            1
+----C                   END
     C*
     C     RRAPOL        CHAIN     GBASEDIO                           15
     C                   EXCEPT    GBAADD
     C                   EXSR      SRALT                                        -CONTROLE
     C*
+----C                   IF        (Opcao = OPT#Novo)
|    C                   EXCEPT    RAADD
|+---C     ADC           IFNE      WADC
||   C                   EXSR      VALADC
|+---C                   ELSE
||+--C     NIB           IFNE      WNIB
|||  C                   EXSR      VALADC
||+--C                   END
|+---C                   END
|    C                   MOVE      MSG(106)      WMSG
|    C                   MOVE      RR            WRM
|    C                   MOVE      APOL          WAP
|    C                   MOVE      WMSG          MENSER
|    C                   EXSR      ERRO
|    C* adição registo 1 ao REGAPOL
|    C                   Z-ADD     UYEAR         ANO
|    C                   MOVE      RRAPOL        RRAP
|    C                   MOVE      *BLANK        ARG
|    C                   MOVE      '1'           TA
|    C                   EXCEPT    ADDR
|    C* adição registo 2 ao REGAPOL
|    C                   MOVE      *BLANK        ARG
|    C                   MOVE      '2'           TA
|    C                   MOVEL     NSEG          ARG
|    C                   EXCEPT    ADDR
|    C* adição registo 3 ao REGAPOL
|    C                   MOVE      *BLANK        ARG
|    C                   MOVE      '3'           TA
|    C                   MOVEL     DUR           ARG
|    C                   EXCEPT    ADDR
|    C* adição registo 4 ao REGAPOL
|    C                   MOVE      *BLANK        ARG
|    C                   MOVE      '4'           TA
|    C                   MOVE      UDAY          MMDD              4 0
|    C                   MOVEL     UMONTH        MMDD
|    C                   MOVEL     MMDD          ARG
|    C                   EXCEPT    ADDR
|    C* ACIDENTES PESSOAIS
|+---C     RR            IFEQ      02
||+--C   KH              DO
|||  C                   CALL      'AP6000J'
|||  C                   PARM                    WRR
|||  C                   PARM                    WAPOL
||+--C                   END
|+---C                   END
|    C* ACIDENTES DE TRABALHO
|+---C     RR            IFEQ      15
||+--C     MOD           IFEQ      'F'
|||  C                   CALL      'ATP900'
|||  C                   PARM                    RRAPOL
||+--C                   ELSE
|||+-C     MOD           IFEQ      'V'
|||| C                   CALL      'ATP910'
|||| C                   PARM                    RRAPOL
|||+-C                   END
||+--C                   END
|+---C                   END
|    C* MULTIRISCOS
|+---C                   IF        (RR = 05)
||   C                   EXSR      OPCOES
||   C     RRAPOL        CHAIN     GBASEDIO                           15
||+--C                   IF        NOT *IN15
|||  C                   EXCEPT    GBAUPD
||+--C                   ENDIF
|+---C                   ENDIF
+----C                   END
     C*
     C     FIM1          TAG
     C                   EXSR      BEGS01                                       limpa opção e #apol
     C                   GOTO      XPTO01                                       ecran Menu
     C**************************************************************************
     C* ECRANS DE - consulta e alterações
     C**************************************************************************
     C     XPTO04        TAG
     C   20
     CANN15              MOVE      MSG(112)      MENSER
     C   21
     CANN15              MOVE      MSG(113)      MENSER
     C   20
     COR 21              EXCEPT    OUTP05
     C                   SETOFF                                       356164
     C                   MOVE      'D'           INDREG
     C*
     C     RR            COMP      15                                 9898
     C*
     C                   EVAL      INDREG = 'D'
     C                   EVAL      Func#Key = *BLANKS
+----C                   DOW       (Func#Key<>Key#F03) AND (Func#Key<>Key#F12)
|    C*                  DOW       (Func#Key <> KEY#F07)
|    C                   EXCEPT    OUTP04
|    C                   READ      ECRAN                                  92
|+---C                   IF        (Func#Key = Key#F04)
||   C                   ExSr      RunPopUp
||   C                   ITER
|+---C                   ELSEIF    (Func#Key = Key#F01)
||   C                   ExSr      ViewHelp
||   C                   ITER
|+---C                   ELSE
||   C                   LEAVE
|+---C                   ENDIF
+----C                   ENDDO
     C**************************************************************************
     C* F05-Manutenção de Textos, limites e Franquias
     C**************************************************************************
+----C                   IF        (Func#Key = KEY#F05)
|    C                   EXSR      Opcoes
|    C                   GOTO      XPTO04
+----C                   ENDIF
     C**************************************************************************
     C* F12-ver doc memo da apol (Desactivar esta função)                 asilva
     C**************************************************************************
+----C                   IF        (Func#Key = Key#F12)
|    C*                  MOVE      RRAPOL        #APOL             9 0                         asilv
|    C*                  SETON                                        LRU1U2                   asilv
|    C*                  EXSR      BEGS01                                                      asilv
|    C*                  GOTO      ACABLR                                                      asilv
+----C                   ENDIF
     C
     C**************************************************************************
     C* F07-sair da função                                                asilva
     C**************************************************************************
+----C                   IF        (Func#Key=Key#F03) OR (Func#Key=Key#F12)
|    C*                  IF        (Func#Key = Key#F07)
|+---C                   IF        (Opcao <> OPT#Consulta)
||   C                   ROLBK
|+---C                   ENDIF
|    C                   EXSR      BEGS01                                                      asilv
|    C                   GOTO      ACABLR                                                      asilv
+----C                   ENDIF
     C* MERCANTILE
+----C     OPCAO         IFEQ      OPT#Consulta                                 opção CONSULTA
|+---C     WRR           IFEQ      '55'
||+--C     NEWPRD        IFEQ      'S'
|||  C
-----C
     C                   EXCEPT    OUTP03
     C                   MONITOR
     C                   READ      ECRAN                                  92
     C                   ON-ERROR
     C                   ENDMON
 +---C                   IF        (Func#Key = KEY#F03)
 |   C                   GOTO      XPTO04
 +---C                   ENDIF
 +---C                   IF        (Func#Key = KEY#F12)
 |   C                   MOVE      RRAPOL        #APOL             9 0                         asilv
 |   C                   SETON                                        LRU1U2                   asilv
 |   C                   EXSR      BEGS01                                                      asilv
 |   C                   GOTO      ACABLR                                                      asilv
 +---C                   ENDIF
     C
 +---C                   IF        (Func#Key=Key#F03) OR (Func#Key=Key#F12)
 |   C*                  IF        (Func#Key=KEY#F07)
 |   C                   EXSR      BEGS01                                                      asilv
 |   C                   GOTO      ACABLR                                                      asilv
 +---C                   ENDIF
     C                   GOTO      FIM22
-----C
|||  C
||+--C                   END
|+---C                   END
+----C                   END
     C
     C************************************************************************
     C* ASILVA, Dezembro 2006
     C* se a OPÇÃO é CONSULTA vai para o fim desta rotina
     C* sem necessidade de fazer validações
+----C     OPCAO         IFEQ      OPT#Consulta                                 opção CONSULTA
|+---C     RR            IFNE      02
||+--C     RR            IFNE      15
|||  C                   GOTO      FIM22
||+--C                   END
|+---C                   END
+----C                   END
     C************************************************************************
+----C     RR            IFEQ      15                                           AC.TRABALHO
|+---C   KI              DO
||+--C     MOD           IFEQ      'F'
|||  C  N20              CALL      'ATP900'
|||  C                   PARM                    RRAPOL
|||  C   20              CALL      'ATP901'
|||  C                   PARM                    RRAPOL
||+--C                   ELSE
|||+-C     MOD           IFEQ      'V'
|||| C  N20              CALL      'ATP910'
|||| C                   PARM                    RRAPOL
|||| C   20              CALL      'ATP911'
|||| C                   PARM                    RRAPOL
|||+-C                   END
||+--C                   END
||   C                   GOTO      XPTO04
|+---C                   END
+----C                   ELSE
|+---C     RR            IFEQ      02                                           AC.PESSOAIS
||   C   KH
||+--CAN 20              DO
|||  C                   CALL      'AP6000J'
|||  C                   PARM                    WRR
|||  C                   PARM                    WAPOL
|||  C                   GOTO      XPTO04
||+--C                   END
|+---C                   END
+----C                   END
     C
+----C                   IF        (Func#Key = Key#F08) AND
|    C                             (Opcao = OPT#Consulta)
|    C                   GOTO      XPTO04
+----C                   ENDIF
     C
+----C                   IF        (Func#Key = Key#F09)
|    C                   GOTO      XPTO04
+----C                   ENDIF
     C*
+----C                   IF        (Func#Key=Key#F03) OR (Func#Key=Key#F12)
|    C*                  IF        (Func#Key = Key#F07)
|    C                             OR (Opcao = OPT#Consulta)
|    C                   GOTO      FIM2
+----C                   ENDIF
      * DESCRIÇÃO POSTAL
+----C     CPLCB1        IFNE      0
|    C     CPLCB1        CHAIN     MPOSTNP                            90
|    C  N90              MOVEL     DCPOST        CPLCB2
|    C   90              MOVEL     *BLANKS       CPLCB2
|    C   90              MOVE      MSG(139)      MENSER
|    C   90              EXSR      ERRO
|    C   90              GOTO      XPTO04
+----C                   ELSE
|    C                   MOVEL     *BLANKS       CPLCB2
+----C                   END
     C*
     C     IM            COMP      0                                    71
     C  N71IM            COMP      1                                  71
     C   71              MOVE      MSG(48)       MENSER
     C   71              EXSR      ERRO
     C   71              GOTO      XPTO04
     C*
     C     SIT           COMP      1                                    72
     C  N72SIT           COMP      4                                  72
     C   72              MOVE      MSG(49)       MENSER
     C   72              EXSR      ERRO
     C   72              GOTO      XPTO04
     C*
     C     DDINIC        COMP      1                                    73
     C  N73DDINIC        COMP      31                                 73
     C   73              MOVE      MSG(50)       MENSER
     C   73              EXSR      ERRO
     C   73              GOTO      XPTO04
     C*
     C     MMINIC        COMP      1                                    74
     C  N74MMINIC        COMP      12                                 74
     C   74              MOVE      MSG(51)       MENSER
     C   74              EXSR      ERRO
     C   74              GOTO      XPTO04
     C*
     C     AAINI         COMP      1910                                 75
     C  N75AAINI         COMP      AAIP1                              75
     C   75              MOVE      MSG(52)       MENSER
     C   75              EXSR      ERRO
     C   75              GOTO      XPTO04
     C*
     C     DUR           COMP      1                                    76
     C  N76DUR           COMP      2                                  76
     C   76              MOVE      MSG(53)       MENSER
     C   76              EXSR      ERRO
     C   76              GOTO      XPTO04
     C*
     C     DDFIM         COMP      0                                    77
     C  N77DDFIM         COMP      31                                 77
     C   77              MOVE      MSG(54)       MENSER
     C   77              EXSR      ERRO
     C   77              GOTO      XPTO04
     C*
     C     MMFIM         COMP      0                                    78
     C  N78MMFIM         COMP      12                                 78
     C   78              MOVE      MSG(55)       MENSER
     C   78              EXSR      ERRO
     C   78              GOTO      XPTO04
     C*
     C                   EXSR      VDTFIM
     C   30              MOVE      MSG(57)       MENSER
     C   30              EXSR      ERRO
     C   30              GOTO      XPTO04
     C*
     C                   EXSR      VFRAC
     C   31              MOVE      MSG(58)       MENSER
     C   31              EXSR      ERRO
     C   31              GOTO      XPTO04
     C*
     C                   EXSR      VMES13
     C   29              MOVE      MSG(59)       MENSER
     C   29              EXSR      ERRO
     C   29              GOTO      XPTO04
     C*
     C                   EXSR      VMCONT
     C   28              MOVE      MSG(60)       MENSER
     C   28              EXSR      ERRO
     C   28              GOTO      XPTO04
     C*
     C     AACON         COMP      AAIPL                                28
     C   28              MOVE      MSG(61)       MENSER
     C   28              EXSR      ERRO
     C   28              GOTO      XPTO04
     C*
     C     CEMIS         COMP      1                                    81
     C  N81CEMIS         COMP      3                                  81
     C   81              MOVE      MSG(62)       MENSER
     C   81              EXSR      ERRO
     C   81              GOTO      XPTO04
     C*
     C     ISEN          COMP      0                                    82
     C  N82ISEN          COMP      3                                  82
     C   82              DO
     C     ISEN          COMP      6                                    82
     C  N82ISEN          COMP      7                                  82
     C                   END
     C   82              MOVE      MSG(63)       MENSER
     C   82              EXSR      ERRO
     C   82              GOTO      XPTO04
     C*
     C     CONC          CHAIN     FCONC                              34
     C*
     C   34              MOVE      MSG(64)       MENSER
     C   34              EXSR      ERRO
     C   34              GOTO      XPTO04
     C*
     C                   EXSR      VMED1
     C   61
     COR 64              EXSR      ERRO
     C   61
     COR 64              GOTO      XPTO04
     C   35              MOVE      MSG(65)       MENSER
     C   35              EXSR      ERRO
     C   35              GOTO      XPTO04
     C*
+----C     TALT          IFEQ      0
|    C     RR            LOOKUP    TABRR         TABPC                    41
|    C   41              MOVE      TABPC         COMISS
|    C* calcular % de comissões SEM fenomenos sismicos
|    C                   EXSR      COMSFS
|    C*
|+---C     RR            IFEQ      15                                           AC.TRABALHO
||+--C     TPT           IFEQ      'P'
|||  C* calcular % de comissões COM fenomenos sismicos
|||  C                   EXSR      COMCFS
||+--C                   END
|+---C                   END
|    C*
|+---C     RR            IFEQ      01
||   C                   Z-ADD     1             X                 1 0
||   C     16            LOOKUP    CL(X)                                  99
||   C* calcular % de comissões COM fenomenos sismicos
||   C   99              EXSR      COMCFS
|+---C                   END
|    C*
|+---C     RR            IFEQ      05                                           MULTIRISCOS-OLD PROD
||   C                   Z-ADD     1             X                 1 0
||   C     01            LOOKUP    CL(X)                                  99
||   C* calcular % de comissões COM fenomenos sismicos
||   C   99              EXSR      COMCFS
|+---C                   END
|    C*
|+---C     RR            IFEQ      06
||   C                   Z-ADD     1             X                 1 0
||   C     16            LOOKUP    CL(X)                                  99
||   C* calcular % de comissões COM fenomenos sismicos
||   C   99              EXSR      COMCFS
|+---C                   END
|    C*
|+---C     RR            IFEQ      55                                           MULTIRISCOS-OLD PROD
||   C                   Z-ADD     1             X                 1 0
||   C     01            LOOKUP    CL(X)                                  99
||   C* calcular % de comissões COM fenomenos sismicos
||   C   99              EXSR      COMCFS
|+---C                   END
|    C*
|    C*---------------------------------------------------------------
+----C                   END
     C*
     C                   EXSR      VCOM1
     C   36              MOVE      MSG(66)       MENSER
     C   36              EXSR      ERRO
     C   36              GOTO      XPTO04
     C*
     C                   EXSR      VMED2
     C   02              MOVE      MSG(24)       MENSER
     C   02              EXSR      ERRO
     C   02              GOTO      XPTO04
     C*
     C                   SETOFF                                       94
     C                   MOVE      *BLANKS       NOMENT
+----C     CRHIP         IFGT      *ZEROS
|    C                   MOVEL     20            K7                7 0
|    C                   MOVE      CRHIP         K7
|    C     K7            CHAIN     ENTIDADE                           94
|    C   94              MOVE      MSG(138)      MENSER
|    C   94              EXSR      ERRO
|    C   94              GOTO      XPTO04
+----C                   END
     C*
     C     TALT          COMP      0                                    27
     C  N27TALT          COMP      5                                  27
     C   27              MOVE      MSG(69)       MENSER
     C   27              EXSR      ERRO
     C   27              GOTO      XPTO04
     C*
     C                   EXSR      VTALT
     C   27              MOVE      MSG(70)       MENSER
     C   27              EXSR      ERRO
     C   27              GOTO      XPTO04
     C*
     C     DDALT         COMP      0                                    85
     C  N85DDALT         COMP      31                                 85
     C   85              MOVE      MSG(71)       MENSER
     C   85              EXSR      ERRO
     C   85              GOTO      XPTO04
     C*
     C     MMALT         COMP      0                                    86
     C  N86MMALT         COMP      12                                 86
     C   86              MOVE      MSG(72)       MENSER
     C   86              EXSR      ERRO
     C   86              GOTO      XPTO04
     C*
     C     DTALT         IFGT      0
     C     AAALTX        COMP      0                                    24
     C  N24AAALTX        COMP      AAIP1                              24
     C   24              MOVE      MSG(73)       MENSER
     C   24              EXSR      ERRO
     C   24              GOTO      XPTO04
     C                   END
     C*
     C                   EXSR      VDALT
     C   24              MOVE      MSG(74)       MENSER
     C   24              EXSR      ERRO
     C   24              GOTO      XPTO04
     C*
     C     MALT          COMP      0                                    88
     C* N88      MALT      COMP 70                   88    02.05.2002
     C  N88MALT          COMP      99                                 88
     C   88              MOVE      MSG(75)       MENSER
     C   88              EXSR      ERRO
     C   88              GOTO      XPTO04
      *
     C     AVISN         COMP      'S'                                7979
     C   79AVISN         COMP      'N'                                7979
     C   79              MOVE      MSG(144)      MENSER
     C   79              EXSR      ERRO
     C   79              GOTO      XPTO04
     C* validar ADC
     C*          ADC       IFEQ 0
     C*          NIB       IFNE *BLANK
     C*                    MOVE MSG,149   MENSER
     C*                    EXSR ERRO
     C*                    GOTO XPTO04
     C*                    END
     C*                    END
     C* validar NIB
     C*          NIB       IFEQ *BLANK
     C*          ADC       IFNE *ZERO
     C*                    MOVE MSG,153   MENSER
     C*                    EXSR ERRO
     C*                    GOTO XPTO04
     C*                    END
     C*                    END
      *
+----C     ADC           IFNE      WADC
|    C                   EXSR      VALADC
|+---C     OUMSG         IFNE      *BLANKS
||   C                   MOVEL     OUMSG         MENSER
||   C                   EXSR      ERRO
||   C                   GOTO      XPTO04
|+---C                   END
+----C                   END
      *
+----C     NIB           IFNE      WNIB
|    C                   EXSR      VALADC
|+---C     OUMSG         IFNE      *BLANKS
||   C                   MOVEL     OUMSG         MENSER
||   C                   EXSR      ERRO
||   C                   GOTO      XPTO04
|+---C                   END
+----C                   END
     C* validação Seguro Novo com ADC e NIB preenchido
     C*          TALT      IFEQ *ZERO                      seguro novo
     C*          ADC       IFNE *ZERO
     C*                    MOVE MSG,154   MENSER
     C*                    EXSR ERRO
     C*                    GOTO XPTO04
     C*                    END
     C*                    END
     C* se ADC e NIB preenchidos o AVISN têm que ser S
+----C     ADC           IFNE      *ZERO
|+---C     NIB           IFNE      *BLANK
||   C                   MOVE      'S'           AVISN
|+---C                   END
+----C                   END
     C* validação do codigo empregado
+----C     EMPREG        IFNE      'S'
|+---C     EMPREG        IFNE      ' '
||   C                   MOVE      MSG(150)      MENSER
||   C                   EXSR      ERRO
||   C                   GOTO      XPTO04
|+---C                   END
+----C                   END
     C* validação do ano do edífico
+----C     ANOEDI        IFNE      *ZERO
|+---C     ANOEDI        IFLT      1800
||   C                   MOVE      MSG(151)      MENSER
||   C                   EXSR      ERRO
||   C                   GOTO      XPTO04
|+---C                   END
|+---C     ANOEDI        IFGT      YYYY
||   C                   MOVE      MSG(151)      MENSER
||   C                   EXSR      ERRO
||   C                   GOTO      XPTO04
|+---C                   END
+----C                   END
     C* validação do codigo postal do edífico
+----C     CPEDI         IFNE      *ZERO
|+---C     CPEDI         IFLT      1000
||   C                   MOVE      MSG(152)      MENSER
||   C                   EXSR      ERRO
||   C                   GOTO      XPTO04
|+---C                   END
+----C                   END
      *
     C     CS            COMP      0                                    51
     C  N51CS            COMP      2                                  51
     C   51              MOVE      MSG(76)       MENSER
     C   51              EXSR      ERRO
     C   51              GOTO      XPTO04
     C*
     C     CR1           COMP      0                                    08
     C  N08CR1           COMP      7                                  08
     C   08CR1           COMP      9                                  0808
     C   08              MOVE      MSG(78)       MENSER
     C   08              EXSR      ERRO
     C   08              GOTO      XPTO04
     C* validar capital + risco
     C     CR1           COMP      0                                      08
     C   08CAP1          COMP      0                                  08
     C   08              MOVE      MSG(26)       MENSER
     C   08              EXSR      ERRO
     C   08              GOTO      XPTO04
      *
     C     CR2           COMP      0                                      80
     C   80CAP2          COMP      0                                  80
     C   80              MOVE      MSG(26)       MENSER
     C   80              EXSR      ERRO
     C   80              GOTO      XPTO04
      *
     C     CR3           COMP      0                                      80
     C   80CAP3          COMP      0                                  80
     C   80              MOVE      MSG(26)       MENSER
     C   80              EXSR      ERRO
     C   80              GOTO      XPTO04
      *
+----C     RR            IFEQ      05
|    C                   SETON                                        97
|    C     7             LOOKUP    ACR                                    99
|    C   992             LOOKUP    ACR                                    97
|+---C  N97              DO
||   C                   SETON                                        0880
||   C                   MOVE      MSG(159)      MENSER
||   C                   EXSR      ERRO
||   C                   GOTO      XPTO04
|+---C                   END
|    C                   SETOFF                                       97
+----C                   END
      *
     C* ACIDENTES DE TRABALHO - DETERMINA PREMIO
+----C     RR            IFEQ      15
|    C*
|+---C     PR1           IFEQ      *ZEROS
||   C     CAP1          MULT      TX1AT         WORK             11 2
||   C     WORK          DIV(H)    100           PR1
||+--C     ISEN          IFEQ      1
|||  C     PR1           DIV(H)    1.20          PR1
||+--C                   END
|+---C                   END
|    C*
|+---C     PR1           IFLT      30
||+--C     FLAGR         IFEQ      '0'
|||+-C     ISEN          IFEQ      1
|||| C                   MOVE      MSG(142)      MENSER
|||+-C                   ELSE
|||| C                   MOVE      MSG(134)      MENSER
|||+-C                   END
|||  C                   MOVE      '1'           FLAGR
|||  C                   GOTO      XPTO04
||+--C                   END
|+---C                   END
|    C*
|+---C     FLAGI         IFEQ      '0'
||+--C     ISEN          IFEQ      1
|||  C                   MOVE      '1'           FLAGI
|||  C                   MOVE      MSG(143)      MENSER
|||  C                   GOTO      XPTO04
||+--C                   END
|+---C                   END
|    C*
|    C                   Z-ADD     PR1           PRSACT
|    C                   Z-ADD     CAP1          CAPTOT
|    C                   Z-ADD     TX1AT         TXMAT
|    C*
+----C                   END
     C* ----------------------------------------------------
     C     CAP1          COMP      0                                      26
     C   26              MOVE      MSG(108)      MENSER
     C   26              EXSR      ERRO
     C   26              GOTO      XPTO04
     C*
     C                   EXSR      VCAPT
     C   38              MOVE      MSG(77)       MENSER
     C   38              EXSR      ERRO
     C   38              GOTO      XPTO04
     C*
     C  N95TX1           COMP      0                                      17
     C   95TX1AT         COMP      0                                      17
     C   17              MOVE      MSG(109)      MENSER
     C   17              EXSR      ERRO
     C   17              GOTO      XPTO04
     C*
     C     PR1           COMP      0                                      18
     C   18              MOVE      MSG(110)      MENSER
     C   18              EXSR      ERRO
     C   18              GOTO      XPTO04
     C*
     C                   Z-ADD     0             PRTOT
     C     PR1           ADD       PR2           PRTOT
     C     PRTOT         ADD       PR3           PRTOT
     C     PRSACT        COMP      PRTOT                              3939
     C   39              MOVE      MSG(79)       MENSER
     C   39              EXSR      ERRO
     C   39              GOTO      XPTO04
     C*
     C                   EXSR      VNASC
     C   40              MOVE      MSG(80)       MENSER
     C   40              EXSR      ERRO
     C   40              GOTO      XPTO04
     C*
     C     TEST          COMP      0                                    41
     C  N41TEST          COMP      3                                  41
     C   41              MOVE      MSG(81)       MENSER
     C   41              EXSR      ERRO
     C   41              GOTO      XPTO04
     C*
     C                   EXSR      VTEST
     C   41              MOVE      MSG(82)       MENSER
     C   41              EXSR      ERRO
     C   41              GOTO      XPTO04
     C*
     C                   EXSR      VTXEST
     C   42              MOVE      MSG(83)       MENSER
     C   42              EXSR      ERRO
     C   42              GOTO      XPTO04
     C*
+----C     RR            IFEQ      02
|+---C     CGRUPO        IFEQ      0
||   C                   MOVE      MSG(157)      MENSER
||   C                   EXSR      ERRO
||   C                   GOTO      XPTO04
|+---C                   END
|+---C     NPESS         IFEQ      0
||   C                   MOVE      MSG(157)      MENSER
||   C                   EXSR      ERRO
||   C                   GOTO      XPTO04
|+---C                   END
|    C     CGRUPO        LOOKUP    ACG                                    99
|+---C  N99              DO
||   C                   MOVE      MSG(158)      MENSER
||   C                   EXSR      ERRO
||   C                   GOTO      XPTO04
|+---C                   END
|+---C   KH              DO
||   C                   CALL      'AP6000J'
||   C                   PARM                    WRR
||   C                   PARM                    WAPOL
||   C                   GOTO      XPTO04
|+---C                   END
+----C                   END
     C*
+----C     RR            IFNE      02
|+---C     CGRUPO        IFNE      0
||   C                   MOVE      MSG(156)      MENSER
||   C                   EXSR      ERRO
||   C                   GOTO      XPTO04
|+---C                   END
|+---C     NPESS         IFNE      0
||   C                   MOVE      MSG(156)      MENSER
||   C                   EXSR      ERRO
||   C                   GOTO      XPTO04
|+---C                   END
+----C                   END
      *
     C   KH              GOTO      XPTO04
     C*  ===============================================
     C*   Validacao de  Campos Especificos do RAMO = 15      PC 03/03/00
     C*  ===============================================
+----C     RR            IFEQ      15
|    C*
|+---C     PROFSL        IFEQ      *ZEROS
||   C                   MOVE      MSG(114)      MENSER
||   C                   EXSR      ERRO
||   C                   GOTO      XPTO04
|+---C                   ELSE
||   C                   Z-ADD     *ZEROS        KPROF             6 0
||   C                   MOVEL     00            WKPROF            5 0
||   C                   MOVE      PROFSL        WKPROF
||   C                   MOVE      WKPROF        KPROF
||   C                   MOVE      1             KPROF
||   C     KPROF         CHAIN     FTEXTOS                            05
||+--C   05              DO
|||  C                   MOVE      MSG(117)      MENSER
|||  C                   EXSR      ERRO
|||  C                   GOTO      XPTO04
||+--C                   END
|+---C                   END
|    C*
|+---C     CAPFAT        IFEQ      *ZEROS
||   C                   MOVE      MSG(133)      MENSER
||   C                   EXSR      ERRO
||   C                   GOTO      XPTO04
|+---C                   END
|    C*
|+---C     MOD           IFEQ      *BLANKS
||   C                   MOVE      MSG(115)      MENSER
||   C                   EXSR      ERRO
||   C                   GOTO      XPTO04
|+---C                   ELSE
||+--C     MOD           IFNE      'F'
|||+-C     MOD           IFNE      'V'
|||| C                   MOVE      MSG(118)      MENSER
|||| C                   EXSR      ERRO
|||| C                   GOTO      XPTO04
|||+-C                   ELSE
|||+-C                   END
||+--C                   ELSE
||+--C                   END
|+---C                   END
|    C*
|+---C     TPT           IFEQ      *BLANKS
||   C                   MOVE      MSG(131)      MENSER
||   C                   EXSR      ERRO
||   C                   GOTO      XPTO04
|+---C                   ELSE
||+--C     TPT           IFNE      'P'
|||+-C     TPT           IFNE      'O'
|||| C                   MOVE      MSG(132)      MENSER
|||| C                   EXSR      ERRO
|||| C                   GOTO      XPTO04
|||+-C                   ELSE
|||+-C                   END
||+--C                   ELSE
||+--C                   END
|+---C                   END
|    C*
|+---C     CDCAE         IFEQ      *BLANKS
||   C                   MOVE      MSG(115)      MENSER
||   C                   EXSR      ERRO
||   C                   GOTO      XPTO04
|+---C                   ELSE
||   C                   MOVE      *BLANKS       KCAE             12
||   C                   MOVEL     20            WKCAE             8
||   C                   MOVE      CDCAE         WKCAE
||   C                   MOVEL     WKCAE         KCAE
||   C     KCAE          CHAIN     TABELAS                            05
||+--C   05              DO
|||  C                   MOVE      MSG(119)      MENSER
|||  C                   EXSR      ERRO
|||  C                   GOTO      XPTO04
||+--C                   END
|+---C                   END
|    C*
|+---C     MODALI        IFNE      'FP'
||+--C     CTLCAE        IFEQ      '  '
|||  C                   MOVE      MSG(119)      MENSER
|||  C                   EXSR      ERRO
|||  C                   GOTO      XPTO04
||+--C                   END
|+---C                   END
|    C*
|+---C     CR1           IFLT      1
||   C                   MOVE      MSG(120)      MENSER
||   C                   EXSR      ERRO
||   C                   GOTO      XPTO04
|+---C                   END
|    C*
|+---C     CR1           IFGT      2
||   C                   MOVE      MSG(120)      MENSER
||   C                   EXSR      ERRO
||   C                   GOTO      XPTO04
|+---C                   END
|    C*
|+---C     CR1           IFEQ      2
||+--C     MOD           IFNE      'V'
|||  C                   MOVE      MSG(121)      MENSER
|||  C                   EXSR      ERRO
|||  C                   GOTO      XPTO04
||+--C                   END
|+---C                   END
+----C                   END
     C******************************************************
     C*        validacao as clausulas para o RAMO = 55.    *
     C                   MOVE      *ZEROS        WRKCL
     C                   SETOFF                                           99
     C*
     C     CL01          COMP      0                                    43
     C  N43              MOVE      CL01          WRKCL
     C  N43              EXSR      VCLAUS
     C  N43
     CAN 99              SETON                                        43
     C   43              MOVE      MSG(84)       MENSER
     C   43              EXSR      ERRO
     C   43              GOTO      XPTO04
     C*
     C     CL02          COMP      0                                    44
     C  N44              MOVE      CL02          WRKCL
     C  N44              EXSR      VCLAUS
     C  N44
     CAN 99              SETON                                        44
     C   44              MOVE      MSG(85)       MENSER
     C   44              EXSR      ERRO
     C   44              GOTO      XPTO04
     C*
     C     CL03          COMP      0                                    45
     C  N45              MOVE      CL03          WRKCL
     C  N45              EXSR      VCLAUS
     C  N45
     CAN 99              SETON                                        45
     C   45              MOVE      MSG(86)       MENSER
     C   45              EXSR      ERRO
     C   45              GOTO      XPTO04
     C*
     C     CL04          COMP      0                                    46
     C  N46              MOVE      CL04          WRKCL
     C  N46              EXSR      VCLAUS
     C  N46
     CAN 99              SETON                                        46
     C   46              MOVE      MSG(87)       MENSER
     C   46              EXSR      ERRO
     C   46              GOTO      XPTO04
     C*
     C     CL05          COMP      0                                    47
     C  N47              MOVE      CL05          WRKCL
     C  N47              EXSR      VCLAUS
     C  N47
     CAN 99              SETON                                        47
     C   47              MOVE      MSG(88)       MENSER
     C   47              EXSR      ERRO
     C   47              GOTO      XPTO04
     C*
     C     CL06          COMP      0                                    48
     C  N48              MOVE      CL06          WRKCL
     C  N48              EXSR      VCLAUS
     C  N48
     CAN 99              SETON                                        48
     C   48              MOVE      MSG(89)       MENSER
     C   48              EXSR      ERRO
     C   48              GOTO      XPTO04
     C*
     C     RR            COMP      01                                     50     RG 17/04/91
     C   50RA            COMP      'S'                                5050       RG 17/04/91
     C   50RA            COMP      'N'                                5050       RG 17/04/91
     C   50              MOVE      MSG(90)       MENSER
     C   50              EXSR      ERRO
     C   50              GOTO      XPTO04
     C*
     C     RR            COMP      01                                     50
     C   50RA            COMP      'S'                                    50
     C   50WCL           COMP      0                                      50
     C   50              MOVE      MSG(107)      MENSER
     C   50              EXSR      ERRO
     C   50              GOTO      XPTO04
     C*
     C     TXIND         COMP      0                                  89         RG 12/03/97
     C  N89              MOVE      *BLANK        INDEX                           RG 12/03/97
     C   89INDEX         COMP      'S'                                8989       RG 12/03/97
     C   89INDEX         COMP      'N'                                8989       RG 12/03/97
     C   89              MOVE      MSG(90)       MENSER                          RG 12/03/97
     C   89              EXSR      ERRO                                          RG 12/03/97
     C   89              GOTO      XPTO04                                        RG 12/03/97
     C*
     C*****************************************************************
     C*   Validacao de  Campos Especificos do RAMO = 55     PC 01/10/92
     C*   ==============================================
     C     RR            COMP      25                                     84
     C  N84RR            COMP      35                                     84
     C  N84RR            COMP      55                                     84
     C   84CLASS         COMP      'A'                                8484
     C   84CLASS         COMP      'B'                                8484
     C   84CLASS         COMP      'C'                                8484
     C   84CLASS         COMP      'D'                                8484
     C   84              MOVE      MSG(91)       MENSER
     C   84              EXSR      ERRO
     C   84              GOTO      XPTO04
     C*
     C     RR            COMP      25                                     87
     C  N87RR            COMP      35                                     87
     C  N87RR            COMP      55                                     87
     C   87PERDCQ        COMP      'S'                                8787
     C   87PERDCQ        COMP      'N'                                8787
     C   87              MOVE      MSG(92)       MENSER
     C   87              EXSR      ERRO
     C   87              GOTO      XPTO04
     C*
     C     RR            COMP      25                                     87
     C  N87RR            COMP      35                                     87
     C  N87RR            COMP      55                                     87
     C   87PERDCQ        COMP      'N'                                    87
     C   87TPERD         COMP      ' '                                8787
     C   87              MOVE      MSG(93)       MENSER
     C   87              EXSR      ERRO
     C   87              GOTO      XPTO04
     C*
     C     RR            COMP      25                                     87
     C  N87RR            COMP      35                                     87
     C  N87RR            COMP      55                                     87
     C   87PERDCQ        COMP      'N'                                    87
     C   87PPERDI        COMP      00                                 8787
     C   87              MOVE      MSG(94)       MENSER
     C   87              EXSR      ERRO
     C   87              GOTO      XPTO04
     C*
     C     RR            COMP      25                                     87
     C  N87RR            COMP      35                                     87
     C  N87RR            COMP      55                                     87
     C   87PERDCQ        COMP      'N'                                    87
     C   87ENCFIX        COMP      0                                  8787
     C   87              MOVE      MSG(95)       MENSER
     C   87              EXSR      ERRO
     C   87              GOTO      XPTO04
     C*
     C     RR            COMP      25                                     87
     C  N87RR            COMP      35                                     87
     C  N87RR            COMP      55                                     87
     C   87PERDCQ        COMP      'N'                                    87
     C   87PINDMN        COMP      0                                  8787
     C   87              MOVE      MSG(96)       MENSER
     C   87              EXSR      ERRO
     C   87              GOTO      XPTO04
     C*
     C     RR            COMP      25                                     25
     C  N25RR            COMP      35                                     25
     C  N25RR            COMP      55                                     25
     C   25PERDCQ        COMP      'S'                                    25
     C   25TPERD         COMP      'I'                                2525
     C   25TPERD         COMP      'F'                                2525
     C   25              MOVE      MSG(97)       MENSER
     C   25              EXSR      ERRO
     C   25              GOTO      XPTO04
     C*
     C     RR            COMP      25                                     25
     C  N25RR            COMP      35                                     25
     C  N25RR            COMP      55                                     25
     C   25TPERD         COMP      'I'                                    25
     C   25PPERDI        COMP      20                                 2525
     C   25PPERDI        COMP      25                                 2525
     C   25PPERDI        COMP      30                                 2525
     C   25              MOVE      MSG(98)       MENSER
     C   25              EXSR      ERRO
     C   25              GOTO      XPTO04
     C*
     C     RR            COMP      25                                     25
     C  N25RR            COMP      35                                     25
     C  N25RR            COMP      55                                     25
     C   25TPERD         COMP      'I'                                    25
     C   25ENCFIX        COMP      0                                  2525
     C   25              MOVE      MSG(99)       MENSER
     C   25              EXSR      ERRO
     C   25              GOTO      XPTO04
     C*
     C     RR            COMP      25                                     25
     C  N25RR            COMP      35                                     25
     C  N25RR            COMP      55                                     25
     C   25TPERD         COMP      'I'                                    25
     C   25PINDMN        COMP      0                                  2525
     C   25              MOVE      MSG(100)      MENSER
     C   25              EXSR      ERRO
     C   25              GOTO      XPTO04
     C*
     C     RR            COMP      25                                     25
     C  N25RR            COMP      35                                     25
     C  N25RR            COMP      55                                     25
     C   25TPERD         COMP      'F'                                    25
     C   25ENCFIX        COMP      0                                      25
     C   25              MOVE      MSG(101)      MENSER
     C   25              EXSR      ERRO
     C   25              GOTO      XPTO04
     C*
     C     RR            COMP      25                                     25
     C  N25RR            COMP      35                                     25
     C  N25RR            COMP      55                                     25
     C   25TPERD         COMP      'F'                                    25
     C   25PINDMN        COMP      0                                      25
     C   25              MOVE      MSG(111)      MENSER
     C   25              EXSR      ERRO
     C   25              GOTO      XPTO04
     C*
     C     RR            COMP      25                                     25
     C  N25RR            COMP      35                                     25
     C  N25RR            COMP      55                                     25
     C   25TPERD         COMP      'F'                                    25
     C   25PINDMN        COMP      3                                  2525
     C   25PINDMN        COMP      6                                  2525
     C   25PINDMN        COMP      12                                 2525
     C   25              MOVE      MSG(47)       MENSER
     C   25              EXSR      ERRO
     C   25              GOTO      XPTO04
     C*
     C* ----------------------------------------------------
     C*
     C                   SETOFF                                       93
+----C     JMM1          IFGT      *ZEROS
|    C     JMM1          CHAIN     MCORNP                             05
|    C   05              SETON                                        93
|    C   93              MOVE      MSG(122)      MENSER
|    C   93              EXSR      ERRO
|    C   93              GOTO      XPTO04
|     * Controla se activo (PF 12.02.08)
|+---C  N05ALTCR         IFEQ      'S'
||   C                   SETON                                        93
||   C   93              MOVE      MSG(155)      MENSER
||   C   93              EXSR      ERRO
||   C   93              GOTO      XPTO04
|+---C                   END
|     *
|+---C  N05TMEDCR        IFNE      1
||   C                   SETON                                        93
||   C   93              MOVE      MSG(129)      MENSER
||   C   93              EXSR      ERRO
||   C   93              GOTO      XPTO04
|+---C                   END
|+---C     JMC1          IFEQ      *ZEROS
||+--C     JMM1          IFNE      418
|||  C                   SETON                                        92
|||  C   92              MOVE      MSG(123)      MENSER
|||  C   92              EXSR      ERRO
|||  C   92              GOTO      XPTO04
||+--C                   END
|+---C                   END
+----C                   ELSE
|+---C     JMC1          IFGT      *ZEROS
||   C                   SETON                                        92
||   C   92              MOVE      MSG(136)      MENSER
||   C   92              EXSR      ERRO
||   C   92              GOTO      XPTO04
|+---C                   END
+----C                   END
     C*
+----C     JMC1          IFGT      COM1
|    C                   SETON                                        92
|    C   92              MOVE      MSG(124)      MENSER
|    C   92              EXSR      ERRO
|    C   92              GOTO      XPTO04
+----C                   END
     C*
+----C     JMM2          IFGT      *ZEROS
|    C     JMM2          CHAIN     MCORNP                             05
|    C   05              SETON                                        37
|    C   37              MOVE      MSG(125)      MENSER
|    C   37              EXSR      ERRO
|    C   37              GOTO      XPTO04
|     * Controla se activo (PF 12.02.08)
|+---C  N05ALTCR         IFEQ      'S'
||   C                   SETON                                        37
||   C   37              MOVE      MSG(155)      MENSER
||   C   37              EXSR      ERRO
||   C   37              GOTO      XPTO04
|+---C                   END
|     *
|+---C     JMC2          IFEQ      *ZEROS
||+--C     JMM2          IFNE      418
|||  C                   SETON                                        83
|||  C   83              MOVE      MSG(126)      MENSER
|||  C   83              EXSR      ERRO
|||  C   83              GOTO      XPTO04
||+--C                   END
|+---C                   END
+----C                   ELSE
|+---C     JMC2          IFGT      *ZEROS
||   C                   SETON                                        83
||   C   83              MOVE      MSG(137)      MENSER
||   C   83              EXSR      ERRO
||   C   83              GOTO      XPTO04
|+---C                   END
+----C                   END
     C*
+----C     JMC2          IFGT      COM1
|    C                   SETON                                        83
|    C   83              MOVE      MSG(127)      MENSER
|    C   83              EXSR      ERRO
|    C   83              GOTO      XPTO04
+----C                   END
     C*
     C     JMC1          ADD       JMC2          WCOM                 53
+----C     WCOM          IFGT      COM1
|    C                   SETON                                        8392
|    C   83              MOVE      MSG(127)      MENSER
|    C   83              EXSR      ERRO
|    C   83              GOTO      XPTO04
+----C                   END
     C*
+----C     RR            IFEQ      15
|+---C     FLAGU         IFEQ      '0'
||   C                   MOVE      MSG(135)      MENSER
||   C                   MOVE      '1'           FLAGU
||   C                   GOTO      XPTO04
|+---C                   END
+----C                   END
     C*
     C************************************************************************
     C* ASILVA, Dezembro 2006 a Fevereiro 2007
     C* ECRAN DE UPDATE
     C* ECRAN DE UPDATE
     C* ECRAN DE UPDATE
     C* ECRAN DE UPDATE
     C* ECRAN DE UPDATE
     C* ECRAN DE UPDATE
     C* ECRAN DE UPDATE
     C* ECRAN DE UPDATE
     C* ECRAN DE UPDATE
     C* ECRAN DE UPDATE
     C* ECRAN DE UPDATE
     C*--------------------------------------------------------------------
     C* 2º ecran UPDATE para o novo produto RESIDENTIA (coberturas e clausulas)
     C*--------------------------------------------------------------------
+----C                   IF        (OPCAO=OPT#Altera) AND (WRR='05') AND
|    C                             (NEWPRD='S') AND (CONDOM=*BLANKS)
|    C                   SETON                                        96
|    C* re-calculo da comissão para cobertura facultativa de fenómenos sísmicos
|+---C     CF07          IFEQ      'S'
||   C     RR            LOOKUP    TABRR         TABPC                    41
||   C   41              MOVE      TABPC         COMISS
||   C                   EXSR      COMCFS
|+---C                   END
|    C*
|    C*---------------------------------------------------------------------
|    C                   GOTO      FIM2
+----C                   END
     C*--------------------------------------------------------------------
     C* 2º ecran UPDATE para o novo produto MERCANTILE (coberturas e clausulas)
     C*--------------------------------------------------------------------
+----C                   IF        (OPCAO=OPT#Altera) AND (WRR='55') AND
|    C                             (NEWPRD = 'S')
|    C                   SETON                                        96
|    C*
|    C     INIIN3        TAG
|    C                   EXCEPT    MERC
|    C                   READ      ECRAN                                  92
|    C                   SETON                                        96
|+---C                   IF        (Func#Key = Key#F03)
||   C                   GOTO      XPTO04
|+---C                   ENDIF
|    C
|
|+---C                   IF        (Func#Key=Key#F03) OR (Func#Key=Key#F12)
||   C*                  IF        (Func#Key = Key#F07)
||   C                   EXSR      BEGS01
||   C                   GOTO      ACABLR
|+---C                   ENDIF
|    C*
|+---C     CF01          IFNE      'S'
||+--C     CF01          IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      INIIN3
||+--C                   END
|+---C                   END
|    C*
|+---C     CF02          IFNE      'S'
||+--C     CF02          IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      INIIN3
||+--C                   END
|+---C                   END
|    C*
|+---C     CF03          IFNE      'S'
||+--C     CF03          IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      INIIN3
||+--C                   END
|+---C                   END
|    C*
|+---C     CF04          IFNE      'S'
||+--C     CF04          IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      INIIN3
||+--C                   END
|+---C                   END
|    C*
|+---C     CF05          IFNE      'S'
||+--C     CF05          IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      INIIN3
||+--C                   END
|+---C                   END
|    C*
|+---C     CF06          IFNE      'S'
||+--C     CF06          IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      INIIN3
||+--C                   END
|+---C                   END
|    C*
|+---C     CF07          IFNE      'S'
||+--C     CF07          IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      INIIN3
||+--C                   END
|+---C                   END
|    C*
|+---C     CF08          IFNE      'S'
||+--C     CF08          IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      INIIN3
||+--C                   END
|+---C                   END
|    C*
|+---C     CF0800        IFNE      'S'
||+--C     CF0800        IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      INIIN3
||+--C                   END
|+---C                   END
|    C*
|+---C     CF0830        IFNE      'S'
||+--C     CF0830        IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      INIIN3
||+--C                   END
|+---C                   END
|    C*
|+---C     CF09          IFNE      'S'
||+--C     CF09          IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      INIIN3
||+--C                   END
|+---C                   END
|    C*
|+---C     CF10          IFNE      'S'
||+--C     CF10          IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      INIIN3
||+--C                   END
|+---C                   END
|    C*
|+---C     CF11          IFNE      'S'
||+--C     CF11          IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      INIIN3
||+--C                   END
|+---C                   END
|    C*
|+---C     CF12          IFNE      'S'
||+--C     CF12          IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      INIIN3
||+--C                   END
|+---C                   END
|    C*
|+---C     CF13          IFNE      'S'
||+--C     CF13          IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      INIIN3
||+--C                   END
|+---C                   END
|    C*
|+---C     CF21          IFNE      'S'
||+--C     CF21          IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      INIIN3
||+--C                   END
|+---C                   END
|    C*
|+---C     CF21          IFNE      ' '
||+--C     DCF21         IFEQ      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      INIIN3
||+--C                   END
|+---C                   END
|    C*
|+---C     CF21          IFEQ      ' '
||+--C     DCF21         IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      INIIN3
||+--C                   END
|+---C                   END
|    C*
|+---C     CF22          IFNE      'S'
||+--C     CF22          IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      INIIN3
||+--C                   END
|+---C                   END
|    C*
|+---C     CE001         IFNE      'S'
||+--C     CE001         IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      INIIN3
||+--C                   END
|+---C                   END
|    C*
|+---C     CE002         IFNE      'S'
||+--C     CE002         IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      INIIN3
||+--C                   END
|+---C                   END
|    C*
|+---C     CE003         IFNE      'S'
||+--C     CE003         IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      INIIN3
||+--C                   END
|+---C                   END
|    C*
|+---C     CE004         IFNE      'S'
||+--C     CE004         IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      INIIN3
||+--C                   END
|+---C                   END
|    C*
|+---C     CA030         IFNE      'S'
||+--C     CA030         IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      INIIN3
||+--C                   END
|+---C                   END
|    C*
|+---C     CA100         IFNE      'S'
||+--C     CA100         IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      INIIN3
||+--C                   END
|+---C                   END
|    C*
|+---C     CA101         IFNE      'S'
||+--C     CA101         IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      INIIN3
||+--C                   END
|+---C                   END
|    C*
|+---C     CA102         IFNE      'S'
||+--C     CA102         IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      INIIN3
||+--C                   END
|+---C                   END
|    C*
|+---C     CD103         IFNE      'S'
||+--C     CD103         IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      INIIN3
||+--C                   END
|+---C                   END
|    C*
|+---C     CD133         IFNE      'S'
||+--C     CD133         IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      INIIN3
||+--C                   END
|+---C                   END
|    C*
|+---C     CD134         IFNE      'S'
||+--C     CD134         IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      INIIN3
||+--C                   END
|+---C                   END
|    C*
|+---C     CD135         IFNE      'S'
||+--C     CD135         IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      INIIN3
||+--C                   END
|+---C                   END
|    C*
|+---C     CD136         IFNE      'S'
||+--C     CD136         IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      INIIN3
||+--C                   END
|+---C                   END
|    C*
|+---C     CD137         IFNE      'S'
||+--C     CD137         IFNE      ' '
|||  C                   SETOFF                                       96
|||  C                   GOTO      INIIN3
||+--C                   END
|+---C                   END
|    C*
|    C* re-calculo da comissão para cobertura facultativa de fenómenos sísmicos
|+---C     CF07          IFEQ      'S'
||   C     RR            LOOKUP    TABRR         TABPC                    41
||   C   41              MOVE      TABPC         COMISS
||   C                   EXSR      COMCFS
|+---C                   END
|    C*
|    C                   GOTO      FIM2
+----C                   END
     C*--------------------------------------------------------------------
     C* 2º ecran UPDATE para o novo produto CONDOMINIO (coberturas e clausulas)
     C*--------------------------------------------------------------------
+----C                   IF        (OPCAO=OPT#Altera) AND (WRR='05') AND
|    C                             (NEWPRD='S') AND (CONDOM='C')
|    C                   SETON                                        96
|    C*
|    C* re-calculo da comissão para cobertura facultativa de fenómenos sísmicos
|+---C     CF07          IFEQ      'S'
||   C     RR            LOOKUP    TABRR         TABPC                    41
||   C   41              MOVE      TABPC         COMISS
||   C                   EXSR      COMCFS
|+---C                   END
|    C*
|    C*---------------------------------------------------------------------
|    C                   GOTO      FIM2
+----C                   END
     C*
     C**************************************************************************
     C     FIM2          TAG
     C*
+----C                   IF        (Opcao = OPT#Altera)
|    C*  21              DO
|    C                   EXSR      BEGS05
|     *
|    C     RRAPOL        CHAIN     BASEGRO                            99
|+---C  N99              DO
||+--C     NPESS         IFNE      WNPESS
|||  C                   MOVE      WNPESS        NPESS
||+--C                   END
||+--C     TPRISC        IFNE      WTPRIS
|||  C                   MOVE      WTPRIS        TPRISC
||+--C                   END
||+--C     CLTAR         IFNE      WCLTAR
|||  C                   MOVE      WCLTAR        CLTAR
||+--C                   END
|+---C                   END
|     *
     C     RRAPOL        CHAIN     GBASEDIO                           15
|    C   15              EXCEPT    GBAADD
|    C  N15              EXCEPT    GBAUPD
+----C                   END
     C
     C*==> Tornar efectivas as Alterações
+----C                   IF        (Opcao <> OPT#Consulta)
|    C                   COMMIT
+----C                   ENDIF
     C                   EXSR      SRALT                                        -CONTROLE
     C*
     C     FIM22         TAG
     C*
     C                   EXSR      BEGS01
     C                   GOTO      ACABLR
     C                   GOTO      XPTO01
     C     ACABLR        TAG
     C                   SETON                                        LR
     C                   RETURN
     C**************************************************************************
     C*  SUB-ROTINAS                                                           *
     C*  SUB-ROTINAS                                                           *
     C*  SUB-ROTINAS                                                           *
     C*  SUB-ROTINAS                                                           *
     C*  SUB-ROTINAS                                                           *
     C*  SUB-ROTINAS                                                           *
     C*  SUB-ROTINAS                                                           *
     C*  SUB-ROTINAS                                                           *
     C*  SUB-ROTINAS                                                           *
     C*  SUB-ROTINAS                                                           *
     C*  SUB-ROTINAS                                                           *
     C*  SUB-ROTINAS                                                           *
     C**************************************************************************
     C*  SUB-ROTINA                                                            *
     C**************************************************************************
+----C     OPCOES        BEGSR
|    C                   EVAL      WDatInicio = 20120615
|    C                   EVAL      CR1_ = CR1
|    C                   EVAL      CR2_ = CR2
|    C                   EVAL      CR3_ = CR3
|    C
|    C                   EVAL      WPtrData = %ADDR(RBASEBK)
|    C                   CALL      'PPRDO100'
|    C                   PARM                    RRAPOL
|    C                   PARM                    WPtrData
|    C                   PARM                    WDatInicio
|    C                   PARM                    WModo
|    C                   EVAL      CR1 = CR1_
|    C                   EVAL      CR2 = CR2_
|    C                   EVAL      CR3 = CR3_
+----C                   ENDSR
     C**************************************************************************
     C*  SUB-ROTINA                                                            *
     C**************************************************************************
     C     ERRO          BEGSR
     C                   MOVE      'Z'           INDREG
     C                   EXCEPT    ECERRO
     C                   READ      ECRAN                                  92
     C                   ENDSR
     C**************************************************************************
     C*  SUB-ROTINA                                                            *
     C**************************************************************************
     C     VDTFIM        BEGSR
     C                   SETOFF                                       30
     C     DUR           COMP      2                                      10
     C   10              GOTO      DUR2
     C     DDFIM         COMP      0                                  3030         DUR=1
     C  N30MMFIM         COMP      0                                  3030
     C  N30AAFI          COMP      0                                  3030
     C                   GOTO      EDTFIM
     C     DUR2          TAG
     C     DDFIM         COMP      0                                      30       DUR=2
     C  N30MMFIM         COMP      0                                      30
     C  N30AAFI          COMP      0                                      30
     C     EDTFIM        ENDSR
     C**************************************************************************
     C*  SUB-ROTINA                                                            *
     C**************************************************************************
     C     VFRAC         BEGSR
     C                   SETOFF                                       31
     C     DUR           COMP      2                                      10
     C   10              GOTO      FRAC5
     C     FRAC          COMP      1                                    31         DUR=1
     C  N31FRAC          COMP      4                                  31           DUR=1
     C                   GOTO      EFRAC
     C     FRAC5         TAG
     C     FRAC          COMP      5                                  3131         DUR=2
     C     EFRAC         ENDSR
     C**************************************************************************
     C*  SUB-ROTINA                                                            *
     C**************************************************************************
     C     VMES1         BEGSR
     C                   SETOFF                                       32
     C     DUR           COMP      2                                      10
     C   10              GOTO      MES99
     C     MES1          COMP      MMINIC                             3232         DUR=1
     C                   GOTO      EMES1
     C     MES99         TAG
     C     MES1          COMP      99                                 3232         DUR=2
     C     EMES1         ENDSR
     C**************************************************************************
     C*  SUB-ROTINA                                                            *
     C**************************************************************************
     C     VMES13        BEGSR
     C                   SETOFF                                       29
     C     DUR           COMP      2                                      10
     C   10              GOTO      MM99
     C     MES1          COMP      1                                    29         DUR=1
     C  N29MES1          COMP      12                                 29
     C                   GOTO      EMES13
     C     MM99          TAG
     C     MES1          COMP      99                                 2929         DUR=2
     C     EMES13        ENDSR
     C**************************************************************************
     C*  SUB-ROTINA                                                            *
     C**************************************************************************
     C     VCONT1        BEGSR
     C                   SETOFF                                       3310
     C     DUR           COMP      2                                      10
     C   10              GOTO      MA9999
     C     FRAC          COMP      1                                      10
     C   10MMCONT        COMP      MES1                               3333         FR=1 ANUAL
     C   10
     CANN33AAINI         ADD       1             WKANO             4 0
     C   10
     CANN33AACON         COMP      WKANO                              3333
     C   10              GOTO      ECONT1
     C                   Z-ADD     MMINIC        WKMES             2 0             DUR=1
     C                   Z-ADD     AAINI         WKANO
     C     FRAC          COMP      2                                      10
     C   10WKMES         ADD       6             WKMES                             FR=2 SEMEST.
     C   10              GOTO      COMPAR
     C     FRAC          COMP      3                                      10
     C   10WKMES         ADD       3             WKMES                             FR=3 TRIMEST.
     C   10              GOTO      COMPAR
     C     FRAC          COMP      4                                      10
     C   10WKMES         ADD       1             WKMES                             FR=4 MENSAL
     C     COMPAR        TAG
     C     WKMES         COMP      13                                   10
     C  N10WKMES         SUB       12            WKMES
     C  N10              ADD       1             WKANO
     C     MMCONT        COMP      WKMES                              3333
     C  N33AACON         COMP      WKANO                              3333
     C                   GOTO      ECONT1
     C     MA9999        TAG
     C     MMCONT        COMP      99                                 3333         DUR=2
     C  N33AACON         COMP      9999                               3333
     C     ECONT1        ENDSR
     C**************************************************************************
     C*  SUB-ROTINA                                                            *
     C**************************************************************************
     C     VMCONT        BEGSR
     C                   SETOFF                                       2810
     C     DUR           COMP      2                                      10
     C   10              GOTO      MMC99
     C     FRAC          COMP      1                                      10
     C   10MMCONT        COMP      MES1                               2828         FR=1 ANUAL
     C   10              GOTO      ECONT2
     C                   Z-ADD     MES1          WKMES             2 0             DUR=1
     C*-----------------------------------------------------
     C     FRAC          COMP      2                                      10       RG 9/09/92
     C   10MES1          COMP      MMCONT                                 10       RG 9/09/92
     C   10              GOTO      ECONT2
     C*-----------------------------------------------------
     C     FRAC          COMP      2                                      10
     C   10WKMES         ADD       6             WKMES                             FR=2 SEMEST.
     C   10              GOTO      VALMMC
     C*
     C     FRAC          IFEQ      3
     C     WKMES         ADD       3             WKMES3            2 0             FR=3 TRIMEST.
     C     WKMES         ADD       6             WKMES6            2 0             FR=3 TRIMEST.
     C     WKMES         ADD       9             WKMES9            2 0             FR=3 TRIMEST.
     C     WKMES         IFGE      13
     C                   SUB       12            WKMES
     C                   END
     C     WKMES3        IFGE      13
     C                   SUB       12            WKMES3
     C                   END
     C     WKMES6        IFGE      13
     C                   SUB       12            WKMES6
     C                   END
     C     WKMES9        IFGE      13
     C                   SUB       12            WKMES9
     C                   END
     C     MMCONT        COMP      WKMES                              2828
     C   28MMCONT        COMP      WKMES3                             2828
     C   28MMCONT        COMP      WKMES6                             2828
     C   28MMCONT        COMP      WKMES9                             2828
     C                   GOTO      ECONT2
     C                   END
     C*
     C     FRAC          COMP      4                                      10
     C   10              GOTO      ECONT2
     C*
     C     VALMMC        TAG
     C     WKMES         COMP      13                                   10
     C  N10WKMES         SUB       12            WKMES
     C     MMCONT        COMP      WKMES                              2828
     C                   GOTO      ECONT2
     C     MMC99         TAG
     C     MMCONT        COMP      99                                 2828         DUR=2
     C     ECONT2        ENDSR
     C**************************************************************************
     C*  SUB-ROTINA                                                            *
     C**************************************************************************
     C     VMED1         BEGSR
     C                   SETOFF                                       356164
     C* VALIDAR SE MEDIADOR EXISTE EM FICHEIRO
     C                   MOVEL     0             MED               4 0
     C                   MOVE      MED1          MED               4 0
     C     MED           IFEQ      031                                          MORTAGUA
     C                   GOTO      EMED1
     C                   ELSE
     C     MED           IFGE      299
     C     MED           CHAIN     FMED                               35
     C   35              GOTO      EMED1
     C                   ELSE
     C                   SETON                                        35
     C                   END
     C                   END
     C*
     C* VALIDA SE MEDIADOR É SATÉLITE OU CORRETOR
     C     MED           IFGE      900                                          <---
     C     JMM1          IFGT      *ZEROS                                       <- |
     C                   SETON                                            61     | |
     C   61              MOVE      MSG(140)      MENSER                          | |
     C                   END                                                    <- |
     C     JMC1          IFGT      *ZEROS                                       <- |
     C                   SETON                                            61     | |
     C   61              MOVE      MSG(140)      MENSER                          | |
     C                   END                                                    <- |
     C     JMM2          IFGT      *ZEROS                                       <- |
     C                   SETON                                            64     | |
     C   64              MOVE      MSG(141)      MENSER                          | |
     C                   END                                                    <- |
     C     JMC2          IFGT      *ZEROS                                       <- |
     C                   SETON                                            64     | |
     C   64              MOVE      MSG(141)      MENSER                          | |
     C                   END                                                    <- |
     C                   END                                                    <---
     C     EMED1         ENDSR
     C**************************************************************************
     C*  SUB-ROTINA                                                            *
     C**************************************************************************
     C     VCOM1         BEGSR
     C                   SETOFF                                       36
     C**         RR        COMP 20                   10  10
     C** 10      RR        COMP 40                     1010
     C**N10      COM1      COMP 19.583               36
     C     COM1          COMP      0                                    3636
     C     ECOM1         ENDSR
     C**************************************************************************
     C*  SUB-ROTINA                                                            *
     C**************************************************************************
     C     VMED2         BEGSR
     C                   SETOFF                                       02
     C     MED2          COMP      0                                      10
     C   10              GOTO      EMED2
     C*
     C     MED1          IFNE      299
     C                   MOVE      *ZEROS        MED2                            FORÇA ZEROS
     C                   GOTO      EMED2
     C                   END
     C*
     C*                    MOVE MED2      MED     40
     C*          MED       CHAINFMED                 02
     C     MED2          IFNE      0001                                          COBRADOR
     C     MED2          IFNE      0006                                          MONTEPIO
     C     MED2          IFNE      0008                                          MONTEPIO
     C                   SETON                                        02
     C                   GOTO      EMED2
     C                   END
     C                   END
     C                   END
     C*
     C     JMM1          IFNE      *ZEROS
     C                   SETON                                        02
     C                   END
     C*
     C     EMED2         ENDSR
     C**************************************************************************
     C*  SUB-ROTINA                                                            *
     C**************************************************************************
     C     VTALT         BEGSR
     C                   SETOFF                                       27
     C     SIT           COMP      1                                      10
     C  N10              GOTO      ETALT
     C     TALT          COMP      3                                      27       SIT=1
     C     ETALT         ENDSR
     C**************************************************************************
     C*  SUB-ROTINA                                                            *
     C**************************************************************************
     C     VDALT         BEGSR
     C                   SETOFF                                       24
     C                   MOVEL     AAALTX        WK6               6 0
     C                   MOVE      MMALT         WK6
     C                   MOVEL     WK6           YMDALT            8 0
     C                   MOVE      DDALT         YMDALT
     C                   MOVEL     AACON         WK6               6 0
     C                   MOVE      MMCONT        WK6
     C                   MOVEL     WK6           YMDCON            8 0
     C                   MOVE      DDINIC        YMDCON
     C     YMDALT        COMP      YMDCON                             24
     C     EDALT         ENDSR
     C**************************************************************************
     C*  SUB-ROTINA                                                            *
     C**************************************************************************
     C     VCAPT         BEGSR
     C                   SETOFF                                       38
     C     RR            COMP      02                                     10
     C   10              GOTO      CAPT02
     C     RR            COMP      04                                     10
     C   10              GOTO      CAPT02
     C     RR            COMP      05                                     10
     C   10              GOTO      CAPT05
     C     RR            COMP      15                                     10
     C   10              GOTO      ECAPT
     C     RR            COMP      55                                     10
     C   10              GOTO      CAPT55
     C* restantes ramos
     C     CAP1          ADD       CAP2          WCAPT            11 0
     C     WCAPT         ADD       CAP3          WCAPT
     C     CAPTOT        COMP      WCAPT                              3838
     C                   GOTO      ECAPT
     C* ramos acidentes pessoais, responsabilidade civil
     C     CAPT02        TAG
     C     CAPTOT        COMP      CAP1                               3838
     C                   GOTO      ECAPT
     C* ramo multirriscos habitação
     C     CAPT05        TAG
     C                   Z-ADD     0             ERRO2             1 0
     C                   Z-ADD     0             ERRO3             1 0
     C                   Z-ADD     0             WCAPT
     C     CR2           IFEQ      3
     C                   Z-ADD     1             ERRO2
     C                   END
     C     CR2           IFEQ      4
     C                   Z-ADD     1             ERRO2
     C                   END
     C     CR2           IFEQ      6
     C                   Z-ADD     1             ERRO2
     C                   END
     C     CR3           IFEQ      3
     C                   Z-ADD     1             ERRO3
     C                   END
     C     CR3           IFEQ      4
     C                   Z-ADD     1             ERRO3
     C                   END
     C     CR3           IFEQ      6
     C                   Z-ADD     1             ERRO3
     C                   END
     C                   ADD       CAP1          WCAPT
     C     ERRO2         IFNE      1
     C                   ADD       CAP2          WCAPT
     C                   END
     C     ERRO3         IFNE      1
     C                   ADD       CAP3          WCAPT
     C                   END
     C     CAPTOT        COMP      WCAPT                              3838
     C                   GOTO      ECAPT
     C* ramo multirriscos estabelecimento
     C     CAPT55        TAG
     C                   Z-ADD     0             ERRO2
     C                   Z-ADD     0             ERRO3
     C                   Z-ADD     0             WCAPT
     C     CR2           IFEQ      4
     C                   Z-ADD     1             ERRO2
     C                   END
     C     CR3           IFEQ      4
     C                   Z-ADD     1             ERRO3
     C                   END
     C                   ADD       CAP1          WCAPT
     C     ERRO2         IFNE      1
     C                   ADD       CAP2          WCAPT
     C                   END
     C     ERRO3         IFNE      1
     C                   ADD       CAP3          WCAPT
     C                   END
     C     CAPTOT        COMP      WCAPT                              3838
     C     ECAPT         ENDSR
     C**************************************************************************
     C*  SUB-ROTINA                                                            *
     C**************************************************************************
     C     VNASC         BEGSR
     C                   SETOFF                                       40
     C     RR            COMP      02                                     10
     C  N10              GOTO      ENASC
     C     NASCX         COMP      9999                                   10
     C   10              GOTO      ENASC
     C     AAIPL         SUB       NASCX         WK4               4 0
     C     WK4           COMP      75                                 40
     C     ENASC         ENDSR
     C**************************************************************************
     C*  SUB-ROTINA                                                            *
     C**************************************************************************
     C     VTEST         BEGSR
     C                   SETOFF                                       41
     C     TALT          COMP      5                                      10
     C   10              GOTO      TALT5
     C     TEST          COMP      0                                  4141
     C                   GOTO      ETEST
     C     TALT5         TAG
     C     TEST          COMP      0                                      41       TALT=5
     C     ETEST         ENDSR
     C**************************************************************************
     C*  SUB-ROTINA                                                            *
     C**************************************************************************
     C     VTXEST        BEGSR
     C                   SETOFF                                       42
     C     TEST          COMP      3                                      10
     C   10              GOTO      TEST3
     C     TXEST         COMP      0                                  4242         TEST=0/1/2
     C                   GOTO      ETXEST
     C     TEST3         TAG
     C     TXEST         COMP      0                                      42       TEST=3
     C     ETXEST        ENDSR
     C**************************************************************************
     C*  SUB-ROTINA                                                            *
     C**************************************************************************
     C     VCLAUS        BEGSR
     C     RR            IFEQ      01
     C                   EXSR      CLRM01
     C                   ELSE
     C     RR            IFEQ      21
     C                   EXSR      CLRM01
     C                   ELSE
     C     RR            IFEQ      31
     C                   EXSR      CLRM01
     C                   ELSE
     C                   END
     C                   END
     C                   END
     C*
     C     RR            IFEQ      02
     C                   EXSR      CLRM02
     C                   ELSE
     C     RR            IFEQ      22
     C                   EXSR      CLRM02
     C                   ELSE
     C     RR            IFEQ      32
     C                   EXSR      CLRM02
     C                   ELSE
     C                   END
     C                   END
     C                   END
     C*
     C     RR            IFEQ      03
     C                   EXSR      CLRM03
     C                   END
     C*
     C     RR            IFEQ      04
     C                   EXSR      CLRM04
     C                   END
     C*
     C     RR            IFEQ      05
     C                   EXSR      CLRM05
     C                   END
     C*
     C     RR            IFEQ      06
     C                   EXSR      CLRM06
     C                   ELSE
     C     RR            IFEQ      26
     C                   EXSR      CLRM06
     C                   ELSE
     C     RR            IFEQ      36
     C                   EXSR      CLRM06
     C                   ELSE
     C                   END
     C                   END
     C                   END
     C*
     C     RR            IFEQ      15
     C                   EXSR      CLRM15
     C                   END
     C*
     C     RR            IFEQ      17
     C                   EXSR      CLRM17
     C                   END
     C*
     C     RR            IFEQ      19
     C                   EXSR      CLRM19
     C                   END
     C*
     C     RR            IFEQ      25
     C                   EXSR      CLRM55
     C                   ELSE
     C     RR            IFEQ      35
     C                   EXSR      CLRM55
     C                   ELSE
     C     RR            IFEQ      55
     C                   EXSR      CLRM55
     C                   END
     C                   END
     C                   END
     C*
     C                   ENDSR
     C*************************************************************************
     C*  SUB-ROTINA                                                            *
     C**************************************************************************
     C     CLRM01        BEGSR
     C     WRKCL         IFGE      00                                             <--------+
     C     WRKCL         IFLE      43                                             <------+ |
     C                   ELSE                                                            * |
     C     WRKCL         IFEQ      99                                             <----+ | |
     C                   ELSE                                                          * | |
     C                   SETON                                        99               | | |
     C                   END                                                      <----+ | |
     C                   END                                                      <------+ |
     C                   ELSE                                                              *
     C                   SETON                                        99                   |
     C                   END                                                       <-------+
     C                   ENDSR
     C*************************************************************************
     C*  SUB-ROTINA                                                            *
     C**************************************************************************
     C     CLRM02        BEGSR
     C     WRKCL         IFGE      00                                             <--------+
     C     WRKCL         IFLE      07                                             <------+ |
     C                   ELSE                                                            * |
     C     WRKCL         IFEQ      99                                             <----+ | |
     C                   ELSE                                                          * | |
     C                   SETON                                        99               | | |
     C                   END                                                      <----+ | |
     C                   END                                                      <------+ |
     C                   ELSE                                                              *
     C                   SETON                                        99                   |
     C                   END                                                       <-------+
     C                   ENDSR
     C*************************************************************************
     C*  SUB-ROTINA                                                            *
     C**************************************************************************
     C     CLRM03        BEGSR
      *
     C                   SETON                                        99
     C     WRKCL         IFEQ      00
     C                   SETOFF                                       99
     C                   ELSE
     C     WRKCL         IFEQ      01
     C                   SETOFF                                       99
     C                   ELSE
     C     WRKCL         IFEQ      60
     C                   SETOFF                                       99
     C                   ELSE
     C     WRKCL         IFEQ      64
     C                   SETOFF                                       99
     C                   END
     C                   END
     C                   END
     C                   END
      *
     C                   ENDSR
     C*************************************************************************
     C*  SUB-ROTINA                                                            *
     C**************************************************************************
     C     CLRM04        BEGSR
     C     WRKCL         IFGE      00                                             <--------+
     C     WRKCL         IFLE      77                                             <------+ |
     C                   ELSE                                                            * |
     C     WRKCL         IFEQ      99                                             <----+ | |
     C                   ELSE                                                          * | |
     C                   SETON                                        99               | | |
     C                   END                                                      <----+ | |
     C                   END                                                      <------+ |
     C                   ELSE                                                              *
     C                   SETON                                        99                   |
     C                   END                                                       <-------+
     C                   ENDSR
     C*************************************************************************
     C*  SUB-ROTINA                                                            *
     C**************************************************************************
     C     CLRM05        BEGSR
     C     WRKCL         IFGE      00                                             <--------+
     C     WRKCL         IFLE      07                                             <------+ |
     C                   ELSE                                                            * |
     C     WRKCL         IFEQ      99                                             <----+ | |
     C                   ELSE                                                          * | |
     C                   SETON                                        99               | | |
     C                   END                                                      <----+ | |
     C                   END                                                      <------+ |
     C                   ELSE                                                              *
     C                   SETON                                        99                   |
     C                   END                                                       <-------+
     C                   ENDSR
     C*************************************************************************
     C*  SUB-ROTINA                                                            *
     C**************************************************************************
     C     CLRM06        BEGSR
     C     WRKCL         IFGE      00                                             <--------+
     C     WRKCL         IFLE      41                                             <------+ |
     C                   ELSE                                                            * |
     C     WRKCL         IFGE      93                                             <----+ | |
     C     WRKCL         IFLE      99                                             <--+ | | |
     C                   ELSE                                                        | | | |
     C                   SETON                                        99             | | | |
     C                   END                                                      <--+ | | |
     C                   ELSE                                                          * | |
     C                   SETON                                        99               | | |
     C                   END                                                      <----+ | |
     C                   END                                                      <------+ |
     C                   ELSE                                                              *
     C                   SETON                                        99                   |
     C                   END                                                       <-------+
     C                   ENDSR
     C*************************************************************************
     C*  SUB-ROTINA                                                            *
     C**************************************************************************
     C     CLRM15        BEGSR
     C     WRKCL         IFGE      00                                             <--------+
     C     WRKCL         IFLE      08                                             <------+ |
     C                   ELSE                                                            * |
     C     WRKCL         IFEQ      99                                             <----+ | |
     C                   ELSE                                                          * | |
     C                   SETON                                        99               | | |
     C                   END                                                      <----+ | |
     C                   END                                                      <------+ |
     C                   ELSE                                                              *
     C                   SETON                                        99                   |
     C                   END                                                       <-------+
     C                   ENDSR
     C*************************************************************************
     C*  SUB-ROTINA                                                            *
     C**************************************************************************
     C     CLRM17        BEGSR
     C     WRKCL         IFGE      00                                             <--------+
     C     WRKCL         IFLE      06                                             <------+ |
     C                   ELSE                                                            * |
     C     WRKCL         IFEQ      99                                             <----+ | |
     C                   ELSE                                                          * | |
     C                   SETON                                        99               | | |
     C                   END                                                      <----+ | |
     C                   END                                                      <------+ |
     C                   ELSE                                                              *
     C                   SETON                                        99                   |
     C                   END                                                       <-------+
     C                   ENDSR
     C*************************************************************************
     C*  SUB-ROTINA                                                            *
     C**************************************************************************
     C     CLRM19        BEGSR
     C     WRKCL         IFGE      00                                             <--------+
     C     WRKCL         IFLE      13                                             <------+ |
     C                   ELSE                                                            * |
     C     WRKCL         IFEQ      90                                             <----+ | |
     C                   ELSE                                                          * | |
     C     WRKCL         IFEQ      99                                             <--+ | | |
     C                   ELSE                                                        | | | |
     C                   SETON                                        99             | | | |
     C                   END                                                      <--+ | | |
     C                   END                                                      <----+ | |
     C                   END                                                      <------+ |
     C                   ELSE                                                              *
     C                   SETON                                        99                   |
     C                   END                                                       <-------+
     C                   ENDSR
     C*************************************************************************
     C*  SUB-ROTINA                                                            *
     C**************************************************************************
     C     CLRM55        BEGSR
     C     WRKCL         IFGE      00                                             <--------+
     C     WRKCL         IFLE      14                                             <------+ |
     C                   ELSE                                                            * |
     C     WRKCL         IFEQ      99                                             <----+ | |
     C                   ELSE                                                          * | |
     C                   SETON                                        99               | | |
     C                   END                                                      <----+ | |
     C                   END                                                      <------+ |
     C                   ELSE                                                              *
     C                   SETON                                        99                   |
     C                   END                                                       <-------+
     C                   ENDSR
     C*************************************************************************
     C*  SUB-ROTINA                                                            *
     C*  Validacao do NSEG no Ficheiro de Segurados da GUARDIAN
     C**************************************************************************
     C     VFSEG         BEGSR
     C     NSEG          CHAIN     GFSEG                              05
     C   05              MOVE      MSG(103)      MENSER
     C   05              EXSR      ERRO
     C  N05              MOVE      NSEG          NSEG2
     C                   ENDSR
     C*************************************************************************
     C*  SUB-ROTINA                                                            *
     C*  LIMPAR CAMPOS PARA ECRAN DE CRIAÇÃO APOLICES
     C**************************************************************************
     C     BEGS03        BEGSR
     C                   MOVE      'N'           RECS
     C                   MOVE      *BLANK        LCB
     C                   Z-ADD     0             CPLCB1
     C                   MOVE      *BLANK        CPLCB2
     C                   Z-ADD     0             TALT
     C                   Z-ADD     0             IM
     C                   Z-ADD     0             SIT
     C                   Z-ADD     0             DDINIC
     C                   Z-ADD     0             MMINIC
     C                   Z-ADD     0             AAINIC
     C                   Z-ADD     0             AAINI
     C                   Z-ADD     0             DUR
     C                   Z-ADD     0             DDFIM
     C                   Z-ADD     0             MMFIM
     C                   Z-ADD     0             AAFIM
     C                   Z-ADD     0             AAFI
     C                   Z-ADD     0             FRAC
     C                   Z-ADD     0             MES1
     C                   Z-ADD     0             MACONT
     C                   Z-ADD     0             MMCONT
     C                   Z-ADD     0             AACONT
     C                   Z-ADD     0             AACON
     C                   Z-ADD     0             CEMIS
     C                   Z-ADD     0             ISEN
     C                   Z-ADD     0             CONC
     C                   Z-ADD     0             MED1
     C                   Z-ADD     0             COM1
     C                   Z-ADD     0             MED2
     C                   Z-ADD     0             COM2
     C                   Z-ADD     0             CR1
     C                   Z-ADD     0             CAP1
     C                   Z-ADD     0             TX1
     C                   Z-ADD     0             TX1AT
     C                   Z-ADD     0             PR1
     C                   Z-ADD     0             CR2
     C                   Z-ADD     0             CAP2
     C                   Z-ADD     0             TX2
     C                   Z-ADD     0             PR2
     C                   Z-ADD     0             CR3
     C                   Z-ADD     0             CAP3
     C                   Z-ADD     0             TX3
     C                   Z-ADD     0             PR3
     C                   Z-ADD     0             CAPTOT
     C                   Z-ADD     0             TXMED
     C                   Z-ADD     0             TXMAT
     C                   Z-ADD     0             PRSACT
     C                   Z-ADD     0             FRANQO
     C                   MOVE      *BLANK        LRPS
     C                   Z-ADD     0             TXIND
     C                   MOVE      *BLANK        INDEX
     C                   Z-ADD     0             TXSNB
     C                   MOVE      *BLANK        NASC
     C                   Z-ADD     0             NASCX
     C                   Z-ADD     0             PROFSL
     C                   Z-ADD     0             CGRUPO
     C                   Z-ADD     0             NPESS
     C                   MOVE      *BLANK        RA
     C                   MOVE      *BLANK        NATUR
     C                   MOVE      *BLANK        CLASS
     C                   MOVE      *BLANK        PERDCQ
     C                   MOVE      *BLANK        TPERD
     C                   Z-ADD     0             PPERDI
     C                   Z-ADD     0             ENCFIX
     C                   Z-ADD     0             PINDMN
     C                   Z-ADD     0             PFRC
     C                   Z-ADD     0             CL01
     C                   Z-ADD     0             CL02
     C                   Z-ADD     0             CL03
     C                   Z-ADD     0             CL04
     C                   Z-ADD     0             CL05
     C                   Z-ADD     0             CL06
     C                   MOVE      *BLANK        OBS1
     C                   MOVE      *BLANK        OBS2
     C*
     C                   Z-ADD     *ZEROS        JMM1
     C                   Z-ADD     *ZEROS        JMC1
     C                   Z-ADD     *ZEROS        JMM2
     C                   Z-ADD     *ZEROS        JMC2
     C*
     C                   MOVEL     *BLANKS       CDCAE
     C                   MOVE      *BLANKS       MOD
     C                   MOVE      *BLANKS       NOMENT
     C                   Z-ADD     0             CRHIP
     C************************************************************************
     C* ASILVA, Dezembro 2006
     C                   MOVE      *BLANK        CONDOM
     C                   MOVE      *BLANK        NEWPRD
     C                   MOVE      *BLANK        ESTCON
     C                   MOVE      *BLANK        TIPCON
     C* limpar coberturas facultativas
     C                   MOVE      *BLANK        CF01
     C                   MOVE      *BLANK        CF02
     C                   MOVE      *BLANK        CF03
     C                   MOVE      *BLANK        CF04
     C                   MOVE      *BLANK        CF05
     C                   MOVE      *BLANK        CF06
     C                   MOVE      *BLANK        CF07
     C                   MOVE      *BLANK        CF08
     C                   MOVE      *BLANK        CF0800
     C                   MOVE      *BLANK        CF0830
     C                   MOVE      *BLANK        CF09
     C                   MOVE      *BLANK        CF10
     C                   MOVE      *BLANK        CF11
     C                   MOVE      *BLANK        CF12
     C                   MOVE      *BLANK        CF13
     C                   MOVE      *BLANK        CF21
     C                   MOVE      *BLANK        DCF21
     C                   MOVE      *BLANK        CF22
     C* limpar condições especiais
     C                   MOVE      *BLANK        CE001
     C                   MOVE      *BLANK        CE002
     C                   MOVE      *BLANK        CE003
     C* limpar clausulas aplicaveis
     C                   MOVE      *BLANK        CA030
     C                   MOVE      *BLANK        CA031
     C                   MOVE      *BLANK        CA100
     C                   MOVE      *BLANK        CA101
     C                   MOVE      *BLANK        CA102
     C* limpar clausulas diversas
     C                   MOVE      *BLANK        CD103
     C                   MOVE      *BLANK        CD130
     C                   MOVE      *BLANK        CD131
     C                   MOVE      *BLANK        CD132
     C                   MOVE      *BLANK        CD133
     C                   MOVE      *BLANK        CD134
     C                   MOVE      *BLANK        CD135
     C                   MOVE      *BLANK        CD136
     C                   MOVE      *BLANK        CD137
     C************************************************************************
     C   19RR            LOOKUP    TABRR         TABPC                    41
     C   19
     CAN 41              MOVE      TABPC         COMISS
     C   19
     CAN 41              Z-ADD     CORR1         COM1
     C   19              Z-ADD     299           MED1
     C   19              SETOFF                                           41
     C                   ENDSR
     C*************************************************************************
     C*  SUB-ROTINA                                                            *
     C*  LIMPAR CAMPOS DO ECRAN MENU
     C**************************************************************************
     C     BEGS01        BEGSR
     C                   Z-ADD     0             OPCAO
     C                   Z-ADD     0             RRAPOL
     C                   ENDSR
     C*************************************************************************
     C*  SUB-ROTINA                                                            *
     C*  FORMATA CAMPOS DO BASE PARA DISPLAY
     C**************************************************************************
     C     BEGS04        BEGSR
     C                   MOVE      *BLANKS       NOMENT
     C     CRHIP         IFGT      *ZEROS
     C                   MOVEL     20            K7                7 0
     C                   MOVE      CRHIP         K7
     C     K7            CHAIN     ENTIDADE                           94
     C                   END
     C*
     C     DTINIC        IFGT      *ZEROS
     C                   MOVE      AAINIC        AAINI
     C     AAINIC        IFGT      23
     C                   MOVEL     19            AAINI
     C                   ELSE
     C                   MOVEL     20            AAINI
     C                   END
     C                   ELSE
     C                   MOVE      *ZEROS        AAINI
     C                   END
     C*
     C     DTFIM         IFGT      *ZEROS
     C                   MOVE      AAFIM         AAFI
     C     AAFIM         IFGT      40
     C                   MOVEL     19            AAFI
     C                   ELSE
     C                   MOVEL     20            AAFI
     C                   END
     C                   ELSE
     C                   MOVE      *ZEROS        AAFI
     C                   END
     C*
     C     MMCONT        IFGT      *ZEROS
     C                   MOVE      AACONT        AACON
     C     MACONT        IFEQ      9999
     C                   MOVEL     99            AACON
     C                   ELSE
     C     AACONT        IFGT      60
     C                   MOVEL     19            AACON
     C                   ELSE
     C                   MOVEL     20            AACON
     C                   END
     C                   END
     C                   ELSE
     C                   MOVE      *ZEROS        AACON
     C                   END
     C*
     C     MMALT         IFGT      *ZEROS
     C                   MOVE      AAALT         AAALTX
     C     AAALT         IFGT      40
     C                   MOVEL     19            AAALTX
     C                   ELSE
     C                   MOVEL     20            AAALTX
     C                   END
     C                   ELSE
     C                   MOVE      *ZEROS        AAALTX
     C                   END
     C*
     C     RR            IFEQ      02                                           <----------
     C                   MOVE      *ZEROS        NASCX                                    |
     C     NASC          IFEQ      'XX'                                         <-------- |
     C                   MOVE      9999          NASCX                                  | |
     C                   ELSE                                                           | |
     C                   MOVE      NASC          NASCX                                  | |
     C     NASCX         IFGT      10                                           <------ | |
     C                   MOVEL     19            NASCX                                | | |
     C                   ELSE                                                         * | |
     C                   MOVEL     20            NASCX                                | | |
     C                   END                                                    <------ | |
     C                   END                                                    <-------- |
     C                   END                                                    <----------
     C*
     C     RR            IFEQ      06                                           <----------
     C*                    MOVE *ZEROS    NASCX                      |
     C*          NASC      IFEQ 'XX'                       <-------- |
     C*                    MOVE 9999      NASCX                    | |
     C*                    ELSE                                    | |
     C                   MOVE      NASC          NASCX                                  | |
     C*          NASCX     IFGT 10                         <------ | |
     C*                    MOVEL19        NASCX                  | | |
     C*                    ELSE                                  * | |
     C*                    MOVEL20        NASCX                  | | |
     C*                    END                             <------ | |
     C*                    END                             <-------- |
     C                   END                                                    <----------
     C                   ENDSR
     C*************************************************************************
     C*  SUB-ROTINA                                                            *
     C*  FORMATA CAMPOS DO DISPLAY PARA O BASE
     C**************************************************************************
     C     BEGS05        BEGSR
     C                   MOVE      AAINI         AAINIC
     C                   MOVE      AAFI          AAFIM
     C                   MOVE      AACON         AACONT
     C                   MOVE      AAALTX        AAALT
     C     NASCX         IFEQ      9999
     C                   MOVE      'XX'          NASC
     C                   ELSE
     C                   MOVE      NASCX         NASC
     C                   END
     C                   ENDSR
     C*************************************************************************
     C*  SUB-ROTINA
     C*  calculo de comissões SEM fenomenos sismicos
     C*************************************************************************
     C     COMSFS        BEGSR
     C     ISEN          IFEQ      1
     C     CORR1         MULT(H)   1.20          CORR              5 3
     C     ANGA1         MULT(H)   1.20          ANGA              5 3
     C     COBR1         MULT(H)   1.20          COBR              5 3
     C                   ELSE
     C                   Z-ADD     CORR1         CORR
     C                   Z-ADD     ANGA1         ANGA
     C                   Z-ADD     COBR1         COBR
     C                   END
     C*
     C                   EXSR      CCCF
     C                   ENDSR
     C*************************************************************************
     C*  SUB-ROTINA
     C*  calculo de comissões COM fenomenos sismicos
     C*************************************************************************
     C     COMCFS        BEGSR
     C     ISEN          IFEQ      1
     C     CORR2         MULT(H)   1.20          CORR
     C     ANGA2         MULT(H)   1.20          ANGA
     C     COBR2         MULT(H)   1.20          COBR
     C                   ELSE
     C                   Z-ADD     CORR2         CORR
     C                   Z-ADD     ANGA2         ANGA
     C                   Z-ADD     COBR2         COBR
     C                   END
     C*
     C                   EXSR      CCCF
     C                   ENDSR
     C*************************************************************************
     C*  SUB-ROTINA
     C*  carregar comissões nos campos do ficheiro
     C*************************************************************************
     C     CCCF          BEGSR
     C                   Z-ADD     CORR          COM1                           comi de corretor
     C*
     C     JMM1          IFGT      *ZEROS
     C     JMM2          IFEQ      0
     C     ANGA          ADD       COBR          JMC1                           comi de med com cobr
     C                   END
     C                   ELSE
     C     JMM2          IFGT      *ZEROS
     C                   Z-ADD     ANGA          JMC2                           comi de med sem cobr
     C                   END
     C                   END
     C* mediadores com comissões de EXCEPÇÃO
     C                   EXSR      COMEXC                                       med com comi excepçã
     C                   ENDSR
     C*************************************************************************
     C*  SUB-ROTINA                                                            *
     C*  CALCULO DE COMISSÕES DE EXCEPÇÃO
     C**************************************************************************
     C     COMEXC        BEGSR
     C     JMM2          IFEQ      418                                          JOÃO ALVADIA
     C                   Z-ADD     0             JMC2                           sem comissão
     C                   END
     C*
     C     JMM2          IFEQ      443                                          FERNANDO SILVA
     C     ANGA          ADD       COBR          JMC2                           recebe como mediador
     C                   END                                                    com cobrança
     C*
     C     JMM2          IFEQ      447                                          VERA SANTOS
     C     ANGA          ADD       COBR          JMC2                           recebe como mediador
     C                   END                                                    com cobrança
     C*
     C     JMM2          IFEQ      801                                          JORGE DIAZ
     C     ANGA          ADD       COBR          JMC2                           recebe como mediador
     C                   END                                                    com cobrança
     C*
     C     JMM1          IFEQ      001                                          JM-CLUBE TAP
     C                   Z-ADD     CORR          JMC1                           recebe como corretor
     C                   END
     C*
     C     JMM1          IFEQ      826                                          ANTRAL
     C                   Z-ADD     CORR          JMC1                           recebe como corretor
     C                   END
     C*
     C     JMM1          IFEQ      827                                          EUROPATRIMONIAL
     C     JMM2          IFEQ      0
     C                   Z-ADD     CORR          JMC1                           recebe como corretor
     C                   END
     C                   END
     C*
     C     JMM1          IFEQ      811                                          POLINSUR
     C     JMM2          IFEQ      0
     C                   Z-ADD     CORR          JMC1                           recebe como corretor
     C                   END
     C                   END
      *
     C     MED1          IFEQ      906                                          MARSH
     C                   ADD       3.00          COM1
     C                   END
     C                   ENDSR
     C****************************************************************
     C* CONTROLE ALTERAÇÕES
     C****************************************************************
     CSR   SRALT         BEGSR
     C                   Z-ADD     0             CTRL              1 0
     C     SIT           IFNE      WSIT
     C                   Z-ADD     1             CTRL
     C                   ELSE
     C     TALT          IFNE      WTALT
     C                   Z-ADD     1             CTRL
     C                   ELSE
     C     MALT          IFNE      WMALT
     C                   Z-ADD     1             CTRL
     C                   ELSE
     C     KDTALT        IFNE      ZDTALT
     C                   Z-ADD     1             CTRL
     C                   ELSE
     C     ADC           IFNE      ZADC
     C                   Z-ADD     1             CTRL
     C                   ELSE
     C     NIB           IFNE      ZNIB
     C                   Z-ADD     1             CTRL
     C                   ELSE
     C     FRANQO        IFNE      WFRQ
     C                   Z-ADD     1             CTRL
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
      *
     C     CTRL          IFEQ      1
     C                   Z-ADD     UDATE         DATALT            6 0
     C                   TIME                    HORALT            6 0
     C                   EXCEPT    FALADD
     C                   END
     CSR                 ENDSR
      *
     CSR   VALADC        BEGSR
     C                   MOVE      RR            $RR               2
     C                   MOVE      APOL          $APOL             7
     C                   MOVE      ADC           $ADC             11
     C                   MOVEL     *BLANKS       OUMSG            50
     C                   MOVEL     *BLANKS       MENSER
     C                   CALL      'TBADC'
     C                   PARM                    $RR
     C                   PARM                    $APOL
     C                   PARM                    $ADC
     C                   PARM                    NIB
     C                   PARM                    OUMSG
     CSR                 ENDSR
     C*=============================================================
     C* Subrotina.: PSSR
     C* Objectivo.: Controlar erros de Programa
     C*=============================================================
     C     *PSSR         BEGSR
     C                   IF        S#Err = 102
     C*                  ADD       1             Divisor
     C*                  MOVE      '*GETIN'      ReturnPt          6
     C*                  ELSE
     C*                  MOVE      '*CANCL'      ReturnPt
     C                   ENDIF
     C                   SETON                                        LR
     C                   RETURN
     C                   ENDSR
      /FREE
       //==================================================================
       // Subrotina.: ViewHelp
       // Objectivo.: Visualizar ajuda sobre o ecrã ou campos
       //==================================================================
       BEGSR ViewHelp;
          MONITOR;
          ON-ERROR;
          ENDMON;
          ECR_CM#ROW = CM#CROW;
          ECR_CM#COL = CM#CCOL;
       ENDSR;

       //==================================================================
       // Subrotina.: RunPopUp
       // Objectivo.: Adicionar um novo registo de uma Apólice
       //==================================================================
       BEGSR RunPopUp;
          MONITOR;
             IF (CONDOM = 'C') AND (RR=5);
                WCodProd = $GetCodProduto(RR:2);
             ELSE;
                WCodProd = $GetCodProduto(RR:1);
             ENDIF;

             IF (ECR_CM#NMFLD = 'SIT');
                SIT = %DEC($runPopUp(54:%CHAR(SIT)):1:0);
             ELSEIF (ECR_CM#NMFLD = 'CONC');
                CONC = %DEC($runPopUp(31:%CHAR(CONC)):3:0);
             ELSEIF (ECR_CM#NMFLD = 'ISEN');
                ISEN = %DEC($runPopUp(64:%CHAR(ISEN)):1:0);
             ELSEIF (ECR_CM#NMFLD = 'PFRC');
                PFRC = %DEC($runPopUp(65:%CHAR(PFRC)):3:2)/100;
             ELSEIF (ECR_CM#NMFLD = 'MALT');
                MALT = %DEC($runPopUp(27:%CHAR(MALT)):2:0);
             ELSEIF (ECR_CM#NMFLD = 'TALT');
                TALT = %DEC($runPopUp(16:%CHAR(TALT)):1:0);
             ELSEIF (ECR_CM#NMFLD = 'DUR');
                DUR = %DEC($runPopUp(48:%CHAR(DUR)):1:0);
             ELSEIF (ECR_CM#NMFLD = 'PROFSL');
                PROFSL = %DEC($runPopUp(46:%CHAR(PROFSL)):3:0);
             ELSEIF (ECR_CM#NMFLD = 'FRAC');
                FRAC = %DEC($runPopUp(15:%CHAR(FRAC)):1:0);
             ELSEIF (ECR_CM#NMFLD = 'CRHIP');
                CRHIP = $RunCredPopUp(CRHIP);
             ELSEIF (ECR_CM#NMFLD = 'NSEG');
                NSEG = $RunEntPopUp(NSEG);
             ELSEIF (ECR_CM#NMFLD = 'CR1');
                CR1 = $RunRiskPopUp(WCodProd:CR1);
             ELSEIF (ECR_CM#NMFLD = 'CR2');
                CR2 = $RunRiskPopUp(WCodProd:CR2);
             ELSEIF (ECR_CM#NMFLD = 'CR3');
                CR3 = $RunRiskPopUp(WCodProd:CR3);
             ENDIF;
          ON-ERROR;
          ENDMON;
          ECR_CM#ROW = CM#CROW;
          ECR_CM#COL = CM#CCOL;
       ENDSR;
      /END-FREE
     OREGAPOL   EADD         RAADD
     O                                            2 '1'
     O                       RRAPOL              11
     O                       DUR                 12
     O                       AAINIC              14
     O                       MMINIC              16
     O                       DDINIC              18
     O                       AAFIM               20
     O                       MMFIM               22
     O                       DDFIM               24
     O                       NSEG                29
     O                       SEG                 69
     O                       UYEAR               82
     O                       UMONTH              84
     O                       UDAY                86
     O*
     OREGAPO2   EADD         ADDR
     O                       K23                 23
     O*
     OGBASEDIO  EADD         GBAADD
     O                       RRAPOL               9
     O                       REF                 10
     O                       SEG                 50
     O                       MOR                 90
     O                       COM1                93P
     O                       CPMOR2             113
     O                       IM                 114
     O                       LCB                154
     O                       COM2               157P
     O                       CPLCB2             177
     O                       SIT                178
     O                       DDINIC             180
     O                       MMINIC             182
     O                       AAINIC             184
     O                       DUR                185
     O                       FRAC               186
     O                       MES1               188
     O                       MMCONT             190
     O                       AACONT             192
     O                       CEMIS              193
     O                       ISEN               194
     O                       CONC               196P
     O                       MED1               198P
     O                       EXCONT             199
     O                       PILOTO             200
     O                       MED2               203P
     O                       TALT               206
     O                       DDALT              208
     O                       MMALT              210
     O                       AAALT              212
     O                       MALT               214
     O                       ACTA               216P
     O                       CR1                217
     O                       CAP1               223P
     O                 N95   TX1                226P
     O                  95   TX1AT              226P
     O                       PR1                231P
     O                       CR2                232
     O                       CAP2               238P
     O                       TX2                241P
     O                       PR2                246P
     O                       CR3                247
     O                       CAP3               253P
     O                       TX3                256P
     O                       PR3                261P
     O                       CAPTOT             267P
     O                 N95   TXMED              270P
     O                  95   TXMAT              270P
     O                       PRSACT             275P
     O                       FRANQO             279P
     O                       LRPS               324
     O                       TXIND              326P
     O                       TXSNB              328P
     O                       NASC               330
     O                       PROFSL             332P
     O                       PFRC               334P
     O                       CL01               336
     O                       CL02               338
     O                       CL03               340
     O                       CL04               342
     O                       CL05               344
     O                       CL06               346
     O                       OBS1               391
     O                       OBS2               436
     O                       TOTREC             441P
     O                       PRSANT             446P
     O                       PRIANT             450P
     O                       NSEG               453P
     O                       DDFIM              455
     O                       MMFIM              457
     O                       AAFIM              459
     O                       TEST               460
     O                       TXEST              462P
     O                       CS                 463
     O                       RA                 464
     O                       INDEX              465
     O                       NSEG2              470
     O                       NATUR              500
     O                       CLASS              501
     O                       PERDCQ             502
     O                       TPERD              503
     O                       PPERDI             505
     O                       ENCFIX             514
     O                       PINDMN             516
     O                       MOD                517
     O                       TPT                518
     O                       CDCAE              524
     O                       CAPFAT             530P
     O                       AVISN              531
     O                       RECS               532
     O                       CGRUPO             535
     O                       NPESS              538P
     O                       TPRISC             539
     O                       CLTAR              541
     O                       FILO3              551
     O                       CPMOR1             599P
     O                       CPLCB1             603P
     O                       CRHIP              608
     O                       JMM1               611
     O                       JMC1               614P
     O                       JMM2               617
     O                       JMC2               620P
     O************************************************************************
     O* ASILVA, Dezembro 2006
     O* novos campos dos novos produtos RESIDENTIA, MERCANTILE e CONDOMINIO
     O                       CONDOM             204
     O                       NEWPRD             205
     O*
     O                       CF01               568
     O                       CF02               569
     O                       CF03               570
     O                       CF04               571
     O                       CF05               572
     O                       CF06               573
     O                       CF07               574
     O                       CF08               575
     O                       CF09               576
     O                       CF10               577
     O                       CF11               578
     O                       CF12               579
     O                       CF13               533
     O*
     O                       CE001              580
     O                       CE002              534
     O                       CE003              581
     O*
     O                       CA030              582
     O                       CA031              583
     O                       CA100              584
     O                       CA101              585
     O                       CA102              586
     O*
     O                       CD103              587
     O                       CD130              588
     O                       CD131              589
     O                       CD132              590
     O                       CD133              591
     O                       CD134              592
     O                       CD135              593
     O                       CD136              594
     O                       CD137              595
     O* novos campos adicionados em Maio 2007
     O                       ADC                557P
     O                       EMPREG             558
     O                       MEMO               559
     O                       ANOEDI             563
     O                       CPEDI              567
      *
     O                       CF0800             621
     O                       CF0830             622
     O                       CF21               623
     O                       DCF21              638
     O                       CF22               639
     O                       CE004              640
     O                       TIPCON             641
     O                       ESTCON             642
      *
     O                       NIB                700
     O************************************************************************
     OGBASEDIO  E            GBAUPD
     O                       RRAPOL               9
     O                       REF                 10
     O                       SEG                 50
     O                       MOR                 90
     O                       COM1                93P
     O                       CPMOR2             113
     O                       IM                 114
     O                       LCB                154
     O                       COM2               157P
     O                       CPLCB2             177
     O                       SIT                178
     O                       DDINIC             180
     O                       MMINIC             182
     O                       AAINIC             184
     O                       DUR                185
     O                       FRAC               186
     O                       MES1               188
     O                       MMCONT             190
     O                       AACONT             192
     O                       CEMIS              193
     O                       ISEN               194
     O                       CONC               196P
     O                       MED1               198P
     O                       EXCONT             199
     O                       PILOTO             200
     O                       MED2               203P
     O                       TALT               206
     O                       DDALT              208
     O                       MMALT              210
     O                       AAALT              212
     O                       MALT               214
     O                       ACTA               216P
     O                       CR1                217
     O                       CAP1               223P
     O                 N95   TX1                226P
     O                  95   TX1AT              226P
     O                       PR1                231P
     O                       CR2                232
     O                       CAP2               238P
     O                       TX2                241P
     O                       PR2                246P
     O                       CR3                247
     O                       CAP3               253P
     O                       TX3                256P
     O                       PR3                261P
     O                       CAPTOT             267P
     O                 N95   TXMED              270P
     O                  95   TXMAT              270P
     O                       PRSACT             275P
     O                       FRANQO             279P
     O                       LRPS               324
     O                       TXIND              326P
     O                       TXSNB              328P
     O                       NASC               330
     O                       PROFSL             332P
     O                       PFRC               334P
     O                       CL01               336
     O                       CL02               338
     O                       CL03               340
     O                       CL04               342
     O                       CL05               344
     O                       CL06               346
     O                       OBS1               391
     O                       OBS2               436
     O                       TOTREC             441P
     O                       PRSANT             446P
     O                       PRIANT             450P
     O                       NSEG               453P
     O                       DDFIM              455
     O                       MMFIM              457
     O                       AAFIM              459
     O                       TEST               460
     O                       TXEST              462P
     O                       CS                 463
     O                       RA                 464
     O                       INDEX              465
     O                       NSEG2              470
     O                       NATUR              500
     O                       CLASS              501
     O                       PERDCQ             502
     O                       TPERD              503
     O                       PPERDI             505
     O                       ENCFIX             514
     O                       PINDMN             516
     O                       MOD                517
     O                       TPT                518
     O                       CDCAE              524
     O                       CAPFAT             530P
     O                       AVISN              531
     O                       RECS               532
     O                       CGRUPO             535
     O                       NPESS              538P
     O                       TPRISC             539
     O                       CLTAR              541
     O                       FILO3              551
     O                       CPMOR1             599P
     O                       CPLCB1             603P
     O                       CRHIP              608
     O                       JMM1               611
     O                       JMC1               614P
     O                       JMM2               617
     O                       JMC2               620P
     O************************************************************************
     O* ASILVA, Dezembro 2006
     O* novos campos dos novos produtos RESIDENTIA, MERCANTILE e CONDOMINIO
     O                       CF01               568
     O                       CF02               569
     O                       CF03               570
     O                       CF04               571
     O                       CF05               572
     O                       CF06               573
     O                       CF07               574
     O                       CF08               575
     O                       CF09               576
     O                       CF10               577
     O                       CF11               578
     O                       CF12               579
     O                       CF13               533
     O*
     O                       CE001              580
     O                       CE002              534
     O                       CE003              581
     O*
     O                       CA030              582
     O                       CA031              583
     O                       CA100              584
     O                       CA101              585
     O                       CA102              586
     O*
     O                       CD103              587
     O                       CD130              588
     O                       CD131              589
     O                       CD132              590
     O                       CD133              591
     O                       CD134              592
     O                       CD135              593
     O                       CD136              594
     O                       CD137              595
     O* novos campos adicionados em Maio 2007
     O                       ADC                557P
     O                       EMPREG             558
     O                       MEMO               559
     O                       ANOEDI             563
     O                       CPEDI              567
      *
     O                       CF0800             621
     O                       CF0830             622
     O                       CF21               623
     O                       DCF21              638
     O                       CF22               639
     O                       CE004              640
     O                       TIPCON             641
     O                       ESTCON             642
      *
     O                       NIB                700
     O*--------------------------------------------------------
     OFALTER    EADD         FALADD
     O                       WTB                 10
     O                       TB                  11
     O                       WAGRAV              13P
     O                       AGRAV               15P
     O                       WNSIN               16
     O                       NSIN                17
     O                       WDUSIN              23
     O                       DTUSIN              29
      *
     O                       RRAPOL               9
     O                       WSIT                30
     O                       SIT                 31
     O                       WTALT               32
     O                       TALT                33
     O                       WMALT               35
     O                       MALT                37
     O                       ZDTALT              43
     O                       KDTALT              49
      *
     O                       ZADC                55P
     O                       ADC                 61P
     O                       ZNIB                82
     O                       NIB                103
     O                       WFRQ               107P
     O                       FRANQO             111P
      *
     O                       DATALT             209
     O                       HORALT             215
     O                       USER               223
     O*
     O* menu incial - 1º ecran
#[1]#OECRAN     E            INPU02
     O                                           K6 'INPUT1'
     O                       INDREG               1
     O                       RSALDO              26
     O                       WOPCAO              34
     O                       RRAPOL              43
     O                       REF                 44
     O                       NSEG          Z     49
     O                       SEG                 89
     O                       MOR                129
     O                       CPOSM              133
     O                       ZPOSM              136
     O                       CPMOR2             156
     O* dados gerais da apolice
#[2]#O          E            INPU03
     O                                           K5 'INPUT'
     O                       INDREG               1
     O                       LCB                 41
     O                       CPOSL               45
     O                       ZPOSL               48
     O                       CPLCB2              68
     O                       SIT           Z     69
     O                       DDINIC        Z     71
     O                       MMINIC        Z     73
     O                       AAINI         Z     77
     O                       DUR           Z     78
     O                       DDFIM         Z     80
     O                       MMFIM         Z     82
     O                       AAFI          Z     86
     O                       IM                  87
     O                       FRAC          Z     88
     O                       MES1          Z     90
     O                       MMCONT        Z     92
     O                       AACON         Z     96
     O                       CEMIS         Z     97
     O                       EXCONT              98
     O                       ISEN                99
     O                       CONC          Z    102
     O                       CRHIP         Z    107
     O                       NOMENT             147
     O                       MED1          Z    150
     O                       COM1          Z    155
     O                       MED2          Z    160
     O                       COM2          Z    165
     O* novos campos EMPREG e ANOEDI, Maio 2007
     O                       EMPREG             166
     O                       ANOEDI        Z    170
     O*
     O                       TALT               171
     O                       DDALT         Z    173
     O                       MMALT         Z    175
     O                       AAALTX        Z    179
     O                       MALT          Z    181
     O                       ACTA          Z    184
     O                       AVISN              185
     O* novo campo CPEDI, Maio 2007
     O                       CPEDI         Z    189
     O*
     O                       CR1           Z    190
     O                       CAP1          Z    201
     O                 N95   TX1           Z    206
     O                  95   TX1AT              206
     O                       PR1           Z    215
     O                       CS            Z    216
     O                       PRSANT        Z    225
     O                       CR2           Z    226
     O                       CAP2          Z    237
     O                       TX2           Z    242
     O                       PR2           Z    251
     O                       CR3           Z    252
     O                       CAP3          Z    263
     O                       TX3           Z    268
     O                       PR3           Z    277
     O                       CAPTOT        Z    288
     O                 N95   TXMED         Z    293
     O                  95   TXMAT              293
     O                       PRSACT        Z    302
     O                       FRANQO        Z    309
     O                       PRIANT        Z    316
     O                       CAPFAT        Z    327
     O* novos campos ADC e NIB, Maio 2007
     O                       ADC           Z    338
     O                       NIB                359
     O*
     O                       LRPS               404
     O                       TXIND         Z    407
     O                       INDEX              408
     O                       TXSNB         Z    411
     O                       NASCX         Z    415
     O                       PROFSL        Z    418
     O                       CGRUPO        Z    419
     O                       NPESS         Z    424
     O                       RA                 425
     O                       NATUR              455
     O                       CLASS              456
     O                       PERDCQ             457
     O                       TPERD              458
     O                       PPERDI        Z    460
     O                       ENCFIX        Z    469
     O                       PINDMN        Z    471
     O                       CL01               473
     O                       CL02               475
     O                       CL03               477
     O                       CL04               479
     O                       CL05               481
     O                       CL06               483
     O                       PFRC          Z    486
     O                       TEST          Z    487
     O                       TXEST         Z    490
     O                       MOD                491
     O                       TPT                492
     O                       CDCAE              498
     O                       JMM1          Z    501
     O                       JMC1          Z    506
     O                       OBS1               551
     O                       JMM2          Z    554
     O                       JMC2          Z    559
     O                       OBS2               604
     O                       TOTREC        Z    613
     O                       ECR_CM#ROW         616
     O                       ECR_CM#COL         619
     O                       ECR_CM#NMREC       629
     O                       ECR_CM#NMFLD       639
     O* dados gerais da apolice
#[3]#O          E            OUTP04
     O                                           K6 'OUTPUT'
     O                       INDREG               1
     O                       WOPCAO               9
     O                       RRAPOL              18
     O                       REF                 19
     O                       NSEG          Z     24
     O                       SEG                 64
     O                       RSALDO              89
     O                       MOR                129
     O                       CPOSM              133
     O                       ZPOSM              136
     O                       CPMOR2             156
     O                       LCB                196
     O                       CPOSL              200
     O                       ZPOSL              203
     O                       CPLCB2             223
     O                       SIT                224
     O                       DDINIC             226
     O                       MMINIC             228
     O                       AAINI              232
     O                       DUR                233
     O                       DDFIM         Z    235
     O                       MMFIM         Z    237
     O                       AAFI          Z    241
     O                       IM                 242
     O                       FRAC               243
     O                       MES1               245
     O                       MMCONT             247
     O                       AACON              251
     O                       CEMIS              252
     O                       EXCONT             253
     O                       ISEN               254
     O                       CONC               257
     O                       CRHIP         Z    262
     O                       NOMENT             302
     O                       MED1          Z    305
     O                       COM1          Z    310
     O                       MED2          Z    315
     O                       COM2          Z    320
     O* novos campos EMPREG e ANOEDI, Maio 2007
     O                       EMPREG             321
     O                       ANOEDI        Z    325
     O*
     O                       TALT               326
     O                       DDALT              328
     O                       MMALT              330
     O                       AAALTX             334
     O                       MALT               336
     O                       ACTA               339
     O                       AVISN              340
     O* novo campo CPEDI, Maio 2007
     O                       CPEDI         Z    344
     O*
     O                       CR1           Z    345
     O                       CAP1          Z    356
     O                 N95   TX1           Z    361
     O                  95   TX1AT              361
     O                       PR1           Z    370
     O                       CS            Z    371
     O                       PRSANT        Z    380
     O                       CR2           Z    381
     O                       CAP2          Z    392
     O                       TX2           Z    397
     O                       PR2           Z    406
     O                       CR3           Z    407
     O                       CAP3          Z    418
     O                       TX3           Z    423
     O                       PR3           Z    432
     O                       CAPTOT        Z    443
     O                 N95   TXMED         Z    448
     O                  95   TXMAT              448
     O                       PRSACT        Z    457
     O                       FRANQO        Z    464
     O                       PRIANT        Z    471
     O                       CAPFAT        Z    482
     O* novos campos ADC e NIB, Maio 2007
     O                       ADC           Z    493
     O                       NIB                514
     O*
     O                       LRPS               559
     O                       TXIND         Z    562
     O                       INDEX              563
     O                       TXSNB         Z    566
     O                       NASCX         Z    570
     O                       PROFSL        Z    573
     O                       CGRUPO        Z    574
     O                       NPESS         Z    579
     O                       RA                 580
     O                       NATUR              610
     O                       CLASS              611
     O                       PERDCQ             612
     O                       TPERD              613
     O                       PPERDI        Z    615
     O                       ENCFIX        Z    624
     O                       PINDMN        Z    626
     O                       CL01               628
     O                       CL02               630
     O                       CL03               632
     O                       CL04               634
     O                       CL05               636
     O                       CL06               638
     O                       PFRC          Z    641
     O                       TEST          Z    642
     O                       TXEST         Z    645
     O                       MOD                646
     O                       TPT                647
     O                       CDCAE              653
     O                       JMM1          Z    656
     O                       JMC1          Z    661
     O                       OBS1               706
     O                       JMM2          Z    709
     O                       JMC2          Z    714
     O                       OBS2               759
     O                       TOTREC        Z    768
     O                       ECR_CM#ROW         771
     O                       ECR_CM#COL         774
     O                       ECR_CM#NMREC       784
     O                       ECR_CM#NMFLD       794
     O* coberturas facultativas do MERCANTILE
#[4]#O          E            MERC
     O                                           K6 'INPUT3'
     O                       APOL                 7
     O                       CF01                 8
     O                       CE001                9
     O                       CA030               10
     O                       CD103               11
     O                       CF02                12
     O                       CE002               13
     O                       CA100               14
     O                       CD133               15
     O                       CF03                16
     O                       CA101               17
     O                       CD134               18
     O                       CF04                19
     O                       CA102               20
     O                       CD135               21
     O                       CF05                22
     O                       CD136               23
     O                       CF06                24
     O                       CD137               25
     O                       CF07                26
     O                       CF08                27
     O                       CF09                28
     O                       CF10                29
     O                       CF11                30
     O                       CF12                31
     O                       CF13                32
     O* coberturas facultativas do MERCANTILE
#[5]#O          E            OUTP03
     O                                           K5 'OUTP3'
     O                       APOL                 7
     O                       CF01                 8
     O                       CE001                9
     O                       CA030               10
     O                       CD103               11
     O                       CF02                12
     O                       CE002               13
     O                       CA100               14
     O                       CD133               15
     O                       CF03                16
     O                       CA101               17
     O                       CD134               18
     O                       CF04                19
     O                       CA102               20
     O                       CD135               21
     O                       CF05                22
     O                       CD136               23
     O                       CF06                24
     O                       CD137               25
     O                       CF07                26
     O                       CF08                27
     O                       CF09                28
     O                       CF10                29
     O                       CF11                30
     O                       CF12                31
     O                       CF13                32
     O* mensagem de erro
#[6]#O          E            ECERRO
     O                                           K6 'ECERRO'
     O                       INDREG               1
     O                       MENSER              61
     O* mensagens
#[7]#O          E            OUTP05
     O                                           K6 'ECMENS'
     O                       MENSER              60
     O* perguntar se é CONDOMINIO
#[8]#O          E            COND
     O                                           K8 'CONDOM  '
     O                       SIMNAO               3
**
OPÇÃO INVÁLIDA                                                    1
RAMO INVÁLIDO                                                     2
JÁ EXISTE NO BASE DIA                                             3
JÁ EXISTE NO BASE-GRANDE                                          4
NÃO EXISTENTE                                                     5
NOT IN RANGE                                                      6
NOT IN RANGE                                                      7
NOT IN RANGE                                                      8
NOT IN RANGE                                                      9
NOT IN RANGE                                                      10
NOT IN RANGE                                                      1
NOT IN RANGE                                                      2
NOT IN RANGE                                                      3
NOT IN RANGE                                                      4
DATA INVÁLIDA                                                     5
CÓDIGO INVÁLIDO                                                   6
DATA INVÁLIDA                                                     7
DATA INVÁLIDA                                                     8
NOT IN RANGE                                                      9
NOT IN RANGE                                                      20
CODIGO INVÁLIDO                                                   1
CODIGO INVÁLIDO                                                   2
VALOR INVÁLIDO                                                    3
CCOB TEM QUE SER = 0001 OU 0006 E JMM1 TEM QUE SER = 000.         4
NOT IN RANGE                                                      5
CÓDIGO RISCO INVÁLIDO                                             6
VALOR INVÁLIDO                                                    7
VALOR INVÁLIDO                                                    8
DATA INVÁLIDA                                                     9
CÓDIGO DE CLÁUSULA INVÁLIDO.                                      30
CÓDIGO DE CLÁUSULA INVÁLIDO.                                      1
CÓDIGO DE CLÁUSULA INVÁLIDO.                                      2
CÓDIGO DE CLÁUSULA INVÁLIDO.                                      3
CÓDIGO DE CLÁUSULA INVÁLIDO.                                      4
CÓDIGO DE CLÁUSULA INVÁLIDO.                                      5
INDIQUE Sim Ou Nao                                                6
Classe Inválida. Classe deve ser A, B, C, D.                      7
INDIQUE S=Sim ou N=Nao para Perdas Consequênciais.                8
Se NÃO tem Perdas Conseq. deve preencher TPERD=N.                 9
Se NAO tem Perdas Conseq. deve preencher PPERDI=00.               40
Se NAO tem Perdas Conseq. deve preencher ENCFIX=0.                1
Se NAO tem Perdas Conseq. deve preencher PINDMN=0.                2
Se tem P.Conseq. Indique tipo de perdas; I=IND./F=FIX.            3
INVÁLIDA % DE PERDAS INDIRECTAS.                                  4
INVÁLIDA RELAÇÃO TPERD/ENCFIX.                                    5
INVÁLIDA RELAÇÃO TPERD/PINDMN.                                    6
Inválido PERÍODO  DE INDEMNIZAÇÃO. Deve ser 3, 6, 12.             7
NOT IN RANGE                                                      8
NOT IN RANGE                                                      9
NOT IN RANGE                                                      50
NOT IN RANGE                                                      1
NOT IN RANGE                                                      2
NOT IN RANGE                                                      3
NOT IN RANGE                                                      4
NOT IN RANGE                                                      5
NOT IN RANGE                                                      6
DATA INVÁLIDA                                                     7
CODIGO INVÁLIDO                                                   8
DATA INVÁLIDA                                                     9
MES INVÁLIDO                                                      60
ANO INVÁLIDO                                                      1
NOT IN RANGE                                                      2
NOT IN RANGE                                                      3
CÓDIGO INVÁLIDO                                                   4
CÓDIGO INVÁLIDO                                                   5
VALOR INVÁLIDO                                                    6
CÓDIGO INVÁLIDO                                                   7
NOT IN RANGE                                                      8
NOT IN RANGE                                                      9
CÓDIGO INVÁLIDO                                                   70
NOT IN RANGE                                                      1
NOT IN RANGE                                                      2
NOT IN RANGE                                                      3
DATA INVÁLIDA                                                     4
NOT IN RANGE                                                      5
NOT IN RANGE                                                      6
VALOR INVÁLIDO                                                    7
NOT IN RANGE                                                      8
VALOR INVÁLIDO                                                    9
DATA INVÁLIDA                                                     80
NOT IN RANGE                                                      1
CÓDIGO INVÁLIDO                                                   2
VALOR INVÁLIDO                                                    3
CÓDIGO DE CLÁUSULA INVÁLIDO.                                      4
CÓDIGO DE CLÁUSULA INVÁLIDO.                                      5
CÓDIGO DE CLÁUSULA INVÁLIDO.                                      6
CÓDIGO DE CLÁUSULA INVÁLIDO.                                      7
CÓDIGO DE CLÁUSULA INVÁLIDO.                                      8
CÓDIGO DE CLÁUSULA INVÁLIDO.                                      9
INDIQUE Sim ou Nao                                                90
Classe Inválida. Classe deve ser A, B, C, D.                      1
INDIQUE S=Sim ou N=Nao para Perdas Consequênciais.                2
Se NÃO tem Perdas Conseq. deve preencher TPERD=N.                 3
Se NAO tem Perdas Conseq. deve preencher PPERDI=00.               4
Se NAO tem Perdas Conseq. deve preencher ENCFIX=0.                5
Se NAO tem Perdas Conseq. deve preencher PINDMN=0.                6
Se tem P.Conseq. Indique tipo de perdas; I=IND./F=FIX.            7
INVÁLIDA % DE PERDAS INDIRECTAS.                                  8
INVÁLIDA RELAÇÃO TPERD/ENCFIX.                                    9
INVÁLIDA RELAÇÃO TPERD/PINDMN.                                    100
INVÁLIDO VALOR DE ENCARGOS FIXOS                                  1
Intencionalmente em Branco                                        2
NSEG Invalido                                                     3
*** PRÉMIOS EM DÍVIDA ***                                         4
NÃO É POSSÍVEL ATRIBUIR Nº A ESTA APÓLICE.   CONTACTE O C.I.      5
A ESTA APÓLICE FOI ATRIBUÍDO O Nº XX-XXXXXXX.   "FAÇA ENTER"      6
RISCO ADICIONAL='S' PREENCHIMENTO DE CLAUSULAS É OBRIGATÓRIO      7
OBRIGATÓRIO O PREENCHIMENTO DO CAPITAL.                           8
OBRIGATÓRIO O PREENCHIMENTO DE TAXA                               9
OBRIGATÓRIO O PREENCHIMENTO DO PRÉMIO                             110
SE TPERD = 'F' ENTÃO PINDMN TEM QUE SER DIFERENTE DE ZERO         1
CONSULTA À APÓLICE REFLECTE ALTERAÇÃO EXISTENTE NO CARREG.        2
EXISTE UMA ALTERAÇÃO A ESTA APÓLICE NESTE CARREGAMENTO ...        3
OBRIGATÓRIO O PREENCHIMENTO DA PROFISSÃO...                       4
OBRIGATÓRIO O PREENCHIMENTO DA MODALIDADE...                      5
OBRIGATÓRIO O PREENCHIMENTO DO CÓDIGO DO CAE...                   6
PROFISSAO INEXISTENTE.ACTUALIZE FICHEIRO TEXTOS (FTEXTOS)         7
MODALIDADE INVÁLIDA. SÃO VÁLIDAS, F=P.FIXO V=P.VARIAVEL.          8
CÓDIGO DO CAE INEXISTENTE. ACTUALIZE FICHEIRO ACT.ECONOMICAS...   9
RISCO INVÁLIDO PARA O RAMO DE ACIDENTES DE TRABALHO...            120
PARA RISCO R=2 A MODALIDADE DEVE SER V (P.VARIAVEL/F.FÉRIAS)      1
O MEDIADOR DE JOSÉ MATA (JMM1) NÃO EXISTE EM FICHEIRO.            2
INTRODUÇAO MEDIADOR JMM1 OBRIGA PREENCHIMENTO COMISSAO JMC1       3
COMISSÃO JM NÃO PODE SER SUPERIOR À COMISSAO DA LUSITANIA.        4
O MEDIADOR DE JOSÉ MATA (JMM2) NÃO EXISTE EM FICHEIRO.            5
INTRODUÇAO MEDIADOR JMM2 OBRIGA PREENCHIMENTO COMISSAO JMC2       6
COMISSÃO JM NÃO PODE SER SUPERIOR À COMISSAO DA LUSITANIA.        7
A SOMA DAS COMISSÕES NÃO PODE SER SUPERIOR ÀS DA LUSITANIA        8
MEDIADOR DE JOSÉ MATA NÃO É MEDIADOR C/COBRANÇA.                  9
MEDIADOR DE JOSÉ MATA NÃO É MEDIADOR S/COBRANÇA                   130
PREENCHIMENTO OBRIGATÓRIO PARA TIPO DE TRABALHADOR...             1
TIPO TRABALHADOR INVÁLIDO. P=T.CONTA PROPRIA O=T.CONTA OUTREM     2
CAPITAL PARA FAT (FUNDO ACID. TRABALHO), DEVE SER PREENCHIDO..    3
O PRÉMIO COMERCIAL É INFERIOR A 30 EUROS < - - A T E N Ç Ã O      4
CONFIRA O PRÉMIO COMERCIAL CALCULADO     < - - A T E N Ç Ã O      5
JMC1 NÃO TEM A INDICAÇÃO DO MEDIADOR EM JMM1.                     6
JMC2 NÃO TEM A INDICAÇÃO DO MEDIADOR EM JMM2.                     7
CÓD. CR HIPOTECÁRIO INEXISTENTE.                                  8
CÓDIGO POSTAL INEXISTENTE.                                        9
INVÁLIDO MEDIADOR EM JMM1.                                        140
INVÁLIDO MEDIADOR EM JMM2.                                        1
O PRC É INFERIOR A 30 EUROS. TEM ISENÇÃO.  < - A T E N Ç Ã O      2
A APÓLICE TEM ISENÇÃO DE ENCARGOS.  < - - A T E N Ç Ã O           3
Inválido IMPRIME AVISO COBRANÇA. Deve ser "S" ou "N".             4
                     Novo Produto RESIDENTIA                      5
                     Novo Produto MERCANTILE                      6
                     Novo Produto CONDOMINIO                      7
NÃO PODE HAVER RISCO 2 E 3 SIMULTÂNEAMENTE                        8
ADC INVÁLIDO                                                      9
CODIGO EMPREGADO INVÁLIDO. DEVE SER BRANCO OU "S"                 150
ANO DO EDÍFICIO INVÁLIDO                                          1
CODIGO POSTAL DO EDÍFICIO INVÁLIDO                                2
NIB INVÁLIDO                                                      3
SEGURO NOVO-NÃO PODE PREENCHER ADC E NIB                          4
MEDIADOR DESACTIVADO                                              5
PREENCHIMENTO DO GRUPO E N.PESS SÓ PARA O RAMO 02                 6
OBRIGATÓRIO O PREENCHIMENTO DO GRUPO E N.PESS PARA O RAMO 02      7
CÓDIGO DE GRUPO INVÁLIDO                                          8
NÃO PODE HAVER RISCO 7 SEM RISCO 2                                9
**
01020304050607080910        15  171819
                            55                  65
**      TAB PERCENTAGENS COMISSÕES DOS RAMOS
01179161250002500137490833302500
02154160833304167
03137490833302500
04133330833302084
05179161250002500137490833302500
06179161250002500137490833302500
07179161250002500
08179161250002500
09179161250002500
10154160833304167
15124990833301250133330833302084
17104160625001250
18104160625001250
19104160625001250
55179161250002500137490833302500
65179161250002500
** CODIGO GRUPO
1INDIVIDUAL
2FAMILIAR
3GRUPO
4ESCOLAR
5OCUPANTES VIATURAS
