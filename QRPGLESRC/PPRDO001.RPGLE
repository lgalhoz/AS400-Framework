     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H DECEDIT(',') DFTNAME(GP010) DATEDIT(*DMY.) FIXNBR(*ZONED) AUT(*ALL)
     H********************************************************************
     H*  EMPRESA    :  LUSITANIA COMPANHIA DE SEGUROS, SA                *
     H*  PROGRAMA   :  GP010                                             *
     H*  TIPO       :  SRT                                               *
     H*  APLICAÇÃO  :  PRODUÇÃO AUTOMÓVEL                                *
     H*  FUNÇÃO     :  RECOLHA DE ELEMENTOS PARA EMISSÃO DE CARREGAMENTO *
     H*             :  NOVA VERSÃO. CONTEMPLA CALCULO AUTOMÁTICO PRS'S.  *
     H*  AUTOR      :  RUI GUERREIRO                                     *
     H*  DATA       :  24.10.98                                          *
     H*==================================================================*
     H*                    REGISTO DE MANUTENÇÃO                         *
     H*                                                                  *
     H*   DATA   ABREV.  DESCRIÇÃO DA ALTERAÇÃO                          *
     H* -------- ------  ----------------------                          *
     H* 03/10/1998-RG-ALTERAÇÃO PERMITINDO ASS.VIAGEM A VEÍCULOS CAT-5   *
     H*                                                                  *
     H* 04/02/1999-RG-EXPANSÃO LAYOUT DO GBASEDIA/GBASEGR, POR INCLUSÃO  *
     H*               DE NOVOS CAMPOS.                                   *
     H*               INCLUSÃO DA COBERTURA DE PROTECÇÃO JURÍDICA.       *
     H*               INCLUSÃO DO RISCO QUEBRA ISOLADA DE VIDROS.        *
     H*                  CR1=1 R.C.   + Q.I.V.                           *
     H*                                                                  *
     H* 13/04/1999-RG-CRIADO COD. ISEN 5, ISENTA CUSTO ACTA E CISA.      *
     H*               VÁLIDO SÓ PARA REGISTOS DE ALTERAÇÃO.              *
     H*                                                                  *
     H* 05/03/2000-RG-INTRODUÇÃO DE NOVOS CAMPOS, RELATIVOS À CEDÊNCIA   *
     H*               DE COMISSÕES DE JOSÉ MATA (SEGURO DIRECTO).        *
     H*                                                                  *
     H* 11/11/2000-RG-ADAPTAÇÃO DO CAMPO MED2 PARA IDENTIFICAÇÃO DE      *
     H*               CANAL DE COBRANÇA DE FORMA A POSSIBILITAR A        *
     H*               EMISSÃO DE GUIAS INTERNAS PARA:                    *
     H*                  0000-TESOURARIA                                 *
     H*                  0001-COBRADOR                                   *
     H*                  0006-MONTEPIO GERAL                             *
     H*                                                                  *
     H* 12/10/2001-RG-INTRODUÇÃO TARIFAÇÃO AUTOMATICA PARA OCUPANTES.    *
     H*               ALTERAÇÕES DIVERSAS RELATIVAS À ADAPTAÇÃO AO EURO  *
     H*               PREMIOS SIMPLES ALTERADOS PARA PREMIOS COMERCIAIS. *
     H*                                                                  *
     H* 24/07/2002-RG-PR COMERCIAL DE QIV, PASSA DE ¤ 33,32 A ¤ 37,76 A  *
     H*               PARTIR DE 01.08.2002.                              *
     H*                                                                  *
     H* 21.11.2002-RG  AUMENTO (3%) PRRC DE 01.01.2003 A 30.04.2003      *
     H* 13.03.2003-RG  AUMENTO (6%) PRRC DE 01.05.2003 A 30.04.2004      *
     H*                                                                  *
     H* 08.04.2003-RG  A) RISCO FR OU IRE ISOLADAMENTE, DEIXA DE SER     *
     H*                   ACEITE PARA SEGUROS NOVOS A PARTIR DE 1.05.2003*
     H*                B) A PARTIR DE 1.05.2003, QIV DEIXE DE SER ACEITE *
     H*                   SALVO COM O RISCO DP = 3 OU 4 OU 8.            *
     H*                C) INTRODUÇÃO NOVO RISCO DP (7 = FR + IRE) COM    *
     H*                   EFEITO A PARTIR DE 01/05/2003.                 *
     H*                                                                  *
     H* 10.05.2004-RG  AUMENTOS PRRC E PRDP APARTIR DE 01.06.2004        *
     H*                                                                  *
     H* 13.08.2004-RG  INTRODUÇÃO CAMPO CS (COSEGURO).                   *
     H********************************************************************
     FECRAN     CF   F 1024        WORKSTN EXTFILE('SOAPSERV/FPRDE001')
     F                                     INFDS(INFD#KEYS)
     FGFSEG     IF   F  267     5AIDISK    KEYLOC(1)
     FGBASEDIA  UF A F  700     9AIDISK    KEYLOC(1) EXTFILE('LGALHOZ/GBASEDIA')
     FGBASEGRA  IF   F  700     9AIDISK    KEYLOC(1)
     FBASEGRAT  IF   F  700     9AIDISK    KEYLOC(1)
     FFCONC     IF   F   30     3AIDISK    KEYLOC(1)
     FFMED      IF   F  130     4AIDISK    KEYLOC(1)
     FREGAPOL   UF A F  100     9AIDISK    KEYLOC(3) EXTFILE('LGALHOZ/GREGAPOL')
     FTAROCU    IF   F   28    10AIDISK    KEYLOC(1) EXTFILE('TAROC')
     FDESVAUT   IF   F   12     8AIDISK    KEYLOC(1)
     FTABDESV   IF   F   16    12AIDISK    KEYLOC(1)
     FMCORNP    IF   F  128     3AIDISK    KEYLOC(4) EXTFILE('SD.1.CR')
     FGPSAT     IT   F    4        DISK              EXTFILE('MEDSAT')
     FENTIDADE  IF   F  400     7AIDISK    KEYLOC(1) EXTFILE('G.SINENT')
     FMPOSTNP   IF   F   64     7AIDISK    KEYLOC(2) EXTFILE('SD.1.CP')
     FFTABX     IF   F  274     7AIDISK    KEYLOC(1)
     FCLASSIC   IF   F   56     9AIDISK    KEYLOC(1)
     FREGAPO2   O  A F   23    23AIDISK    KEYLOC(1) EXTFILE('LGALHOZ/GREGAPO2')
     FFALTER    O  A F  223        DISK    EXTFILE('LGALHOZ/FALTER')
     D****************************************************
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SENTO010

     D*==================================================================
     D MSG             S             60    DIM(134) CTDATA PERRCD(1)
     D TABCAP          S              5  0 DIM(4) CTDATA PERRCD(1)
     D TABFRQ          S              2  0 DIM(6) CTDATA PERRCD(1)
     D TABPF1          S              3  3 DIM(6) CTDATA PERRCD(1)
     D TABPF2          S              3  3 DIM(6) CTDATA PERRCD(1)
     D*
     D MDES            S              4  0 DIM(999) ASCEND                      MESES P/ DESVAL.
     D DESV            S              4  4 DIM(999)                             DESVALORIZ/CAP
     D*
     D TABBON          S              1  0 DIM(9) CTDATA PERRCD(1) ASCEND       TB/%BONUS
     D TABPBO          S              3  2 DIM(9) ALT(TABBON)
     D TABRR           S              2  0 DIM(2) CTDATA PERRCD(1) ASCEND       % COM RAMO
     D TABPC           S             30    DIM(2) ALT(TABRR)
     D TABMS           S              4  0 DIM(40) FROMFILE(GPSAT) PERRCD(1)    MED SATELITES
     D                                     ASCEND
     D TABMES          S              2  0 DIM(12) CTDATA PERRCD(1) ASCEND      MES/DIAS
     D TABDIA          S              2  0 DIM(12) ALT(TABMES)
     D TABRIS          S              1  0 DIM(6) CTDATA PERRCD(1)              RISCOS DP
     D TABMAT          S             11    DIM(3) CTDATA PERRCD(1)              MATRICULAS
     D TABM            S              3  0 DIM(9) CTDATA PERRCD(1)              NºMESES
     D                UDS
     D  USER                   1      8
     D  #APOL                101    109  0                                      asilva
     D                 DS
     D  DTMATR                 1      6  0
     D  DDMATR                 1      2  0
     D  MMMATR                 3      4  0
     D  AAMATR                 5      6  0
     D                 DS
     D  DTMATX                 1      8  0
     D  DDMATX                 1      2  0
     D  MMMATX                 3      4  0
     D  AAMATX                 5      8  0
     D                 DS
     D  DATMAT                 1      8  0
     D  AAMAT                  1      4  0
     D  MMMAT                  5      6  0
     D  DDMAT                  7      8  0
     D                 DS
     D  DATINI                 1      8  0
     D  AAINI                  1      4  0
     D  MMINIC                 5      6  0
     D  DDINIC                 7      8  0
     D                 DS
     D  DTAR5                  1      8  0
     D  DDAR5                  1      2  0
     D  MMAR5                  3      4  0
     D  AAAR5                  5      8  0
     D                 DS
     D  DTCRE                  1      8  0
     D  DDCRE                  1      2  0
     D  MMCRE                  3      4  0
     D  AACRE                  5      8  0
     D                 DS
     D  DTCREP                 1      6  0
     D  DDCREP                 1      2  0
     D  MMCREP                 3      4  0
     D  AACREP                 5      6  0
     D                 DS
     D  DTART5                 1      6  0
     D  DDART5                 1      2  0
     D  MMART5                 3      4  0
     D  AAART5                 5      6  0
     D                 DS
     D  KTABS                  1      4  0
     D  NTAB                   1      1  0
     D  NMESES                 2      4  0
     D                 DS
     D  RRAPOL                 1      9  0
     D  RR                     1      2  0
     D  APOL                   3      9  0
     D                 DS
     D  WMSG                   1     60
     D  RM                    35     36
     D  AP                    38     44
     D                 DS
     D  K23                    1     23
     D  TA                     1      1
     D  ANO                    2      3  0
     D  ARG                    4     14
     D  RA                    15     23
     D                 DS
     D  DTINV                  1      8  0
     D  DDINVG                 1      2  0
     D  MMINVG                 3      4  0
     D  AAINV                  5      8  0
     D                 DS
     D  DTEXV                  1      8  0
     D  DDEXVG                 1      2  0
     D  MMEXVG                 3      4  0
     D  AAEXV                  5      8  0
     D                 DS
     D  DTINPJ                 1      8  0
     D  DDINPJ                 1      2  0
     D  MMINPJ                 3      4  0
     D  AAINP                  5      8  0
     D                 DS
     D  DTEXPJ                 1      8  0
     D  DDEXPJ                 1      2  0
     D  MMEXPJ                 3      4  0
     D  AAEXP                  5      8  0
     D                 DS
     D  CPMOR1                 1      7  0
     D  CPOSM                  1      4  0
     D  ZPOSM                  5      7  0
     D                 DS
     D  CPLCB1                 1      7  0
     D  CPOSL                  1      4  0
     D  ZPOSL                  5      7  0
     D                 DS
     D  K10                    1     10
     D  ANOTB                  1      4  0
     D  CR3                    5      5  0
     D  CAP3                   6     10  0
     D                 DS
     D  COMISS                 1     30                                         RAMO/A
     D  CORR1                  1      5  3                                      RAMO/A
     D  ANGA1                  6     10  3                                      RAMO/A
     D  COBR1                 11     15  3                                      RAMO/A
     D                 DS
     D  DTUSIN                 1      6  0
     D  DULSIN                 1      2  0
     D  MULSIN                 3      4  0
     D  AULSIN                 5      6  0
     D                 DS
     D  WDTFIM                 1      8  0
     D  AAFI                   1      4  0
     D  MMFIM                  5      6  0
     D  DDFIM                  7      8  0
     D                 DS
     D  MATR1                  1     11
     D  MATH1                  3      3
     D  MATH2                  6      6
     D*           DS
     D*                                       1   80WDATA
     D*                                       1   40WAA
     D*                                       5   60WMM
     D*                                       7   80WDD
     D                 DS
     D  WDTALT                 1      8  0
     D  WAMALT                 1      6  0
     D  WAAALT                 1      4  0
     D  WMMALT                 5      6  0
     D                 DS
     D  AMCONT                 1      6  0
     D  AACON                  1      4  0
     D  MMCONT                 5      6  0
     D                 DS
     D  KTAB                   1      7
     D  TPTAB                  1      2
     D  CDTAB                  3      7
     D                 DS
     D  KDTALT                 1      6  0
     D  DDALT                  1      2  0
     D  MMALT                  3      4  0
     D  KAAALT                 5      6  0
     D                 DS
     D  YDATA                  1      8  0
     D  YAACON                 1      4  0
     D  MES1                   5      6  0
     D  YDDINI                 7      8  0
     D
     D PRM_Opcao       S              1A
     D PRM_RRApol      S              9A
     D*=====================================================================
     IGFSEG     NS
     I                                  1    5 0NSEG
     I                                  6   25  APEL
     I                                 26   65  SEG
     I                                 66  105  MOR
     I                                106  125  CPMOR2
     I                             P  126  129 0CPMOR1
     I                                132  132  EXCONT
     I                                133  133  CORSEL
     I                                134  134  PILOTO
     I                                135  135  STESP
     IGBASEDIA  NS
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
     I                                189  190 0MMCONT
     I                                191  192 0AACONT
     I                                193  193 0CEMIS
     I                                194  194 0ISEN
     I                             P  195  196 0CONC
     I                             P  197  198 0MED1
     I                                199  199  EXCONT
     I                                200  200  PILOTO
     I                             P  201  203 0MED2
     I                                204  204  VALMAT
     I                                205  205  AVISN
     I                                206  206 0TALT
     I                                207  212 0XDTALT
     I                                207  208 0DDALT
     I                                209  210 0MMALT
     I                                211  212 0AAALT
     I                                213  214 0MALT
     I                             P  215  216 0ACTA
     I                                217  217 0CR1
     I                             P  218  220 0CAPRC
     I                             P  221  225 2PR1
     I                                226  226 0CR2
     I                             P  227  229 0ANOCON
     I                             P  230  234 2PR2
     I                             P  235  239 2PRS
     I                             P  240  242 0FRANQ
     I                             P  243  246 2BONUS
     I                             P  247  251 2PRSAGR
     I                                252  265  MARC1
     I                                266  276  MATR1
     I                                277  278 0AC1
     I                                279  279 0CL
     I                                280  280 0PB
     I                                281  281 0NLUG1
     I                                282  282 0CATV
     I                                283  296  MARC2
     I                                297  307  MATR2
     I                                308  308 0TC
     I*                                     309 309 FILLER
     I                                310  310 0NLUG2
     I                                311  312  NASC
     I                                313  314  CARTA
     I                                315  315 0CS
     I                                316  317 0DULSIN
     I                                318  319 0MULSIN
     I                                320  321 0AULSIN
     I                                322  322 0NSIN
     I                             P  323  324 2AGRAV
     I                                325  325 0TB
     I                                326  326  CL1
     I                                327  327  CL2
     I                                328  328  CL3
     I                                329  329  CL4
     I                                330  330  CL5
     I                                331  331  CL6
     I                                332  332  CL7
     I                                333  333  CL8
     I                                334  378  OBS1
     I                                379  423  OBS2
     I                             P  424  428 2TOTREC
     I                                430  430  FLAGX
     I*                                     431 4310FILLER
     I                                432  437 0DTFIM
     I                                432  433 0DDFIM
     I                                434  435 0MMFIM
     I                                436  437 0AAFIM
     I                                438  438 0TEST
     I                             P  439  440 0TEXTO
     I                                441  446 0DTMATR
     I                             P  447  449 0WCAPRC
     I                                450  450  ART5
     I                             P  451  453 0NSEG
     I                                454  459 0DTART5
     I                                460  460  VSA
     I*                                               1 - ¤  32,86 (AV.ATÉ 1900C
     I*                                               2 - ¤  45,72 (AV.ATÉ 2500C
     I                                461  461  VIA
     I*                                               1 - ¤  16,43 (AC.ATÉ 1900C
     I*                                               2 - ¤  23,58 (AC.ATÉ 2500C
     I*                                     462 462 FILLER
     I                             P  463  464 1DSCESP
     I*                                     465 465 FILLER
     I                                466  470 0NSEG2
     I                                471  471  VIAG
     I                                472  473 0DDINVG
     I                                474  475 0MMINVG
     I                                476  477 0AAINVG
     I                                478  479 0DDEXVG
     I                                480  481 0MMEXVG
     I                                482  483 0AAEXVG
     I                             P  484  487 2PRSVG
     I                             P  488  491 2PVGAGR
     I                                492  492 0TCALVG
     I                             P  493  496 2TOTVG
     I                                497  503 2PRCSPA                          P.R.CA
     I                                504  510 2PRCSP                           PRS. R
     I                             P  511  513 0VEXTRA
     I                             P  514  517 0CAPDPI
     I                                518  518 0CR3                             CAT.RE
     I*                                             1 FAM. S/ COND.
     I*                                             2 FAM. C/ COND.
     I*                                             3 TODOS OCUPANTES
     I                             P  519  521 0CAP3                            CAT.RE
     I                             P  522  526 2PR3
     I                                527  529 0CATREB                          CAT.RE
     I                                530  531 0CODFRQ                          COD.FR
     I                                532  532  RECS                            RECIBO
     I                                533  533  PJUR
     I                                534  535 0DDINPJ
     I                                536  537 0MMINPJ
     I                                538  539 0AAINPJ
     I                                540  541 0DDEXPJ
     I                                542  543 0MMEXPJ
     I                                544  545 0AAEXPJ
     I                             P  546  549 2PRSPJ
     I                             P  550  553 2PPJAGR
     I                                554  554 0TCALPJ
     I                             P  555  558 2TOTPJ
     I                                559  578  MODELO
     I***                                   579 5870APREF
     I                                588  588  QIV
     I                             P  589  591 0CILIND
     I                                592  592  PRIV
     I*                                               1 - ¤  28,50 (15D 23,00¤)
     I*                                               2 - ¤  44,53 (15D 37,50¤)
     I*                                               3 - ¤  69,46 (15D 50,00¤)
     I*                                               4 - ¤  44,53 (30D 23,00¤)
     I*                                               5 - ¤  66,79 (30D 37,50¤)
     I*                                               6 - ¤ 111,32 (30D 50,00¤)
     I                                593  593  CONTSN
     I                             P  664  667 2PRQIV
     I                             P  668  671 2PRPRIV
     I                             P  596  599 0CPMOR1                          CODIGO
     I                             P  600  603 0CPLCB1                          COD. P
     I                                604  608 0CRHIP
     I* cedência de Comissões em JM
     I                                609  611 0JMM1                            CEDÊNC
     I                             P  612  614 3JMC1                            CEDÊNC
     I                                615  617 0JMM2                            CEDÊNC
     I                             P  618  620 3JMC2                            CEDÊNC
     I                             P  621  624 0CAPDP
     I                             P  625  628 0VLNOVO
     I                             P  629  632 0CAPREP
     I                                633  638 0DTCREP
     I                             P  639  642 0VLART5
     I                             P  672  675 2QIVAGR
     I                             P  676  679 2PRVAGR
     I                             P  649  653 2PROCUA
     I*--------------------------------------------------------
     I* campos novos adicionados em Maio 2007
     I                             P  643  648 0ADC
     I                                680  700  NIB
     I                                309  309  EMPREG
     I                                465  465  COMB
     I                                431  431  MEMO
     I*--------------------------------------------------------
     IGBASEGRA  NS
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
     I                                189  190 0MMCONT
     I                                191  192 0AACONT
     I                                193  193 0CEMIS
     I                                194  194 0ISEN
     I                             P  195  196 0CONC
     I                             P  197  198 0MED1
     I                                199  199  EXCONT
     I                                200  200  PILOTO
     I                             P  201  203 0MED2
     I                                204  204  VALMAT
     I                                205  205  AVISN
     I                                206  206 0TALT
     I                                207  212 0XDTALT
     I                                207  208 0DDALT
     I                                209  210 0MMALT
     I                                211  212 0AAALT
     I                                213  214 0MALT
     I                             P  215  216 0ACTA
     I                                217  217 0CR1
     I                             P  218  220 0CAPRC
     I                             P  221  225 2PR1
     I                                226  226 0CR2
     I                             P  227  229 0ANOCON
     I                             P  230  234 2PR2
     I                             P  235  239 2PRS
     I                             P  240  242 0FRANQ
     I                             P  243  246 2BONUS
     I                             P  247  251 2PRSAGR
     I                                252  265  MARC1
     I                                266  276  MATR1
     I                                277  278 0AC1
     I                                279  279 0CL
     I                                280  280 0PB
     I                                281  281 0NLUG1
     I                                282  282 0CATV
     I                                283  296  MARC2
     I                                297  307  MATR2
     I                                308  308 0TC
     I*                                     309 309 FILLER
     I                                310  310 0NLUG2
     I                                311  312  NASC
     I                                313  314  CARTA
     I                                315  315 0CS
     I                                316  317 0DULSIN
     I                                318  319 0MULSIN
     I                                320  321 0AULSIN
     I                                322  322 0NSIN
     I                             P  323  324 2AGRAV
     I                                325  325 0TB
     I                                326  326  CL1
     I                                327  327  CL2
     I                                328  328  CL3
     I                                329  329  CL4
     I                                330  330  CL5
     I                                331  331  CL6
     I                                332  332  CL7
     I                                333  333  CL8
     I                                334  378  OBS1
     I                                379  423  OBS2
     I                             P  424  428 2TOTREC
     I                                430  430  FLAGX
     I*                                     431 4310FILLER
     I                                432  437 0DTFIM
     I                                432  433 0DDFIM
     I                                434  435 0MMFIM
     I                                436  437 0AAFIM
     I                                438  438 0TEST
     I                             P  439  440 0TEXTO
     I                                441  446 0DTMATR
     I                             P  447  449 0WCAPRC
     I                                450  450  ART5
     I                             P  451  453 0NSEG
     I                                454  459 0DTART5
     I                                460  460  VSA
     I*                                               1 - ¤  32,86 (AV.ATÉ 1900C
     I*                                               2 - ¤  45,72 (AV.ATÉ 2500C
     I                                461  461  VIA
     I*                                               1 - ¤  16,43 (AC.ATÉ 1900C
     I*                                               2 - ¤  23,58 (AC.ATÉ 2500C
     I*                                     462 462 FILLER
     I                             P  463  464 1DSCESP
     I*                                     465 465 FILLER
     I                                466  470 0NSEG2
     I                                471  471  VIAG
     I                                472  473 0DDINVG
     I                                474  475 0MMINVG
     I                                476  477 0AAINVG
     I                                478  479 0DDEXVG
     I                                480  481 0MMEXVG
     I                                482  483 0AAEXVG
     I                             P  484  487 2PRSVG
     I                             P  488  491 2PVGAGR
     I                                492  492 0TCALVG
     I                             P  493  496 2TOTVG
     I                                497  503 2PRCSPA                          P.RISC
     I                                504  510 2PRCSP                           PRS. R
     I                             P  511  513 0VEXTRA
     I                             P  514  517 0CAPDPI
     I                                518  518 0CR3                             CAT.RE
     I*                                             1 FAM. S/ COND.
     I*                                             2 FAM. C/ COND.
     I*                                             3 TODOS OCUPANTES
     I                             P  519  521 0CAP3                            CAT.RE
     I                             P  522  526 2PR3                             CAT.RE
     I                                527  529 0CATREB                          CAT.RE
     I                                530  531 0CODFRQ                          COD.FR
     I                                532  532  RECS                            RECIBO
     I                                533  533  PJUR
     I                                534  535 0DDINPJ
     I                                536  537 0MMINPJ
     I                                538  539 0AAINPJ
     I                                540  541 0DDEXPJ
     I                                542  543 0MMEXPJ
     I                                544  545 0AAEXPJ
     I                             P  546  549 2PRSPJ
     I                             P  550  553 2PPJAGR
     I                                554  554 0TCALPJ
     I                             P  555  558 2TOTPJ
     I                                559  578  MODELO
     I***                                   579 5870APREF
     I                                588  588  QIV
     I                             P  589  591 0CILIND
     I                             P  664  667 2PRQIV
     I                                592  592  PRIV
     I*                                               1 - ¤  28,50 (15D 23,00¤)
     I*                                               2 - ¤  44,53 (15D 37,50¤)
     I*                                               3 - ¤  69,46 (15D 50,00¤)
     I*                                               4 - ¤  44,53 (30D 23,00¤)
     I*                                               5 - ¤  66,79 (30D 37,50¤)
     I*                                               6 - ¤ 111,32 (30D 50,00¤)
     I                                593  593  CONTSN
     I                             P  668  671 2PRPRIV
     I                             P  596  599 0CPMOR1                          CODIGO
     I                             P  600  603 0CPLCB1                          COD. P
     I                                604  608 0CRHIP
     I* cedência de Comissões em JM
     I                                609  611 0JMM1                            CEDÊNC
     I                             P  612  614 3JMC1                            CEDÊNC
     I                                615  617 0JMM2                            CEDÊNC
     I                             P  618  620 3JMC2                            CEDÊNC
     I                             P  621  624 0CAPDP
     I                             P  625  628 0VLNOVO
     I                             P  629  632 0CAPREP
     I                                633  638 0DTCREP
     I                             P  639  642 0VLART5
     I                             P  672  675 2QIVAGR
     I                             P  676  679 2PRVAGR
     I                             P  649  653 2PROCUA
     I                                654  654  FROTA
     I                                655  656 0RRMAE
     I                                657  663 0APMAE
     I*--------------------------------------------------------
     I* campos novos adicionados em Maio 2007
     I                             P  643  648 0ADC
     I                                680  700  NIB
     I                                309  309  EMPREG
     I                                465  465  COMB
     I                                431  431  MEMO
     I*--------------------------------------------------------
     IBASEGRAT  NS
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
     I                                189  190 0MMCONT
     I                                191  192 0AACONT
     I                                193  193 0CEMIS
     I                                194  194 0ISEN
     I                             P  195  196 0CONC
     I                             P  197  198 0MED1
     I                                199  199  EXCONT
     I                                200  200  PILOTO
     I                             P  201  203 0MED2
     I                                204  204  VALMAT
     I                                205  205  AVISN
     I                                206  206 0TALT
     I                                207  212 0XDTALT
     I                                207  208 0DDALT
     I                                209  210 0MMALT
     I                                211  212 0AAALT
     I                                213  214 0MALT
     I                             P  215  216 0ACTA
     I                                217  217 0CR1
     I                             P  218  220 0CAPRC
     I                             P  221  225 2PR1
     I                                226  226 0CR2
     I                             P  227  229 0ANOCON
     I                             P  230  234 2PR2
     I                             P  235  239 2PRS
     I                             P  240  242 0FRANQ
     I                             P  243  246 2BONUS
     I                             P  247  251 2PRSAGR
     I                                252  265  MARC1
     I                                266  276  MATR1
     I                                277  278 0AC1
     I                                279  279 0CL
     I                                280  280 0PB
     I                                281  281 0NLUG1
     I                                282  282 0CATV
     I                                283  296  MARC2
     I                                297  307  MATR2
     I                                308  308 0TC
     I*                                     309 309 FILLER
     I                                310  310 0NLUG2
     I                                311  312  NASC
     I                                313  314  CARTA
     I                                315  315 0CS
     I                                316  317 0DULSIN
     I                                318  319 0MULSIN
     I                                320  321 0AULSIN
     I                                322  322 0NSIN
     I                             P  323  324 2AGRAV
     I                                325  325 0TB
     I                                326  326  CL1
     I                                327  327  CL2
     I                                328  328  CL3
     I                                329  329  CL4
     I                                330  330  CL5
     I                                331  331  CL6
     I                                332  332  CL7
     I                                333  333  CL8
     I                                334  378  OBS1
     I                                379  423  OBS2
     I                             P  424  428 2TOTREC
     I                                430  430  FLAGX
     I*                                     431 4310FILLER
     I                                432  437 0DTFIM
     I                                432  433 0DDFIM
     I                                434  435 0MMFIM
     I                                436  437 0AAFIM
     I                                438  438 0TEST
     I                             P  439  440 0TEXTO
     I                                441  446 0DTMATR
     I*                                     447 449 FILLER
     I                                450  450  ART5
     I                             P  451  453 0NSEG
     I                                454  459 0DTART5
     I                                460  460  VSA
     I*                                               1 - ¤  32,86 (AV.ATÉ 1900C
     I*                                               2 - ¤  45,72 (AV.ATÉ 2500C
     I                                461  461  VIA
     I*                                               1 - ¤  16,43 (AC.ATÉ 1900C
     I*                                               2 - ¤  23,58 (AC.ATÉ 2500C
     I*                                     462 462 FILLER
     I                             P  463  464 1DSCESP
     I*                                     465 465 FILLER
     I                                466  470 0NSEG2
     I                                471  471  VIAG
     I                                472  473 0DDINVG
     I                                474  475 0MMINVG
     I                                476  477 0AAINVG
     I                                478  479 0DDEXVG
     I                                480  481 0MMEXVG
     I                                482  483 0AAEXVG
     I                             P  484  487 2PRSVG
     I                             P  488  491 2PVGAGR
     I                                492  492 0TCALVG
     I                             P  493  496 2TOTVG
     I                                497  503 2PRCSPA                          P.RISC
     I                                504  510 2PRCSP                           PRS. R
     I                             P  511  513 0VEXTRA
     I                             P  514  517 0CAPDPI
     I                                518  518 0CR3                             CAT.RE
     I*                                             1 FAM. S/ COND.
     I*                                             2 FAM. C/ COND.
     I*                                             3 TODOS OCUPANTES
     I                             P  519  521 0CAP3                            CAT.RE
     I                             P  522  526 2PR3                             CAT.RE
     I                                527  529 0CATREB                          CAT.RE
     I                                530  531 0CODFRQ                          COD.FR
     I                                532  532  RECS                            RECIBO
     I                                533  533  PJUR
     I                                534  535 0DDINPJ
     I                                536  537 0MMINPJ
     I                                538  539 0AAINPJ
     I                                540  541 0DDEXPJ
     I                                542  543 0MMEXPJ
     I                                544  545 0AAEXPJ
     I                             P  546  549 2PRSPJ
     I                             P  550  553 2PPJAGR
     I                                554  554 0TCALPJ
     I                             P  555  558 2TOTPJ
     I                                559  578  MODELO
     I***                                   579 5870APREF
     I                                588  588  QIV
     I                             P  589  591 0CILIND
     I                             P  664  667 2PRQIV
     I                                592  592  PRIV
     I*                                               1 - ¤  28,50 (15D 23,00¤)
     I*                                               2 - ¤  44,53 (15D 37,50¤)
     I*                                               3 - ¤  69,46 (15D 50,00¤)
     I*                                               4 - ¤  44,53 (30D 23,00¤)
     I*                                               5 - ¤  66,79 (30D 37,50¤)
     I*                                               6 - ¤ 111,32 (30D 50,00¤)
     I                             P  668  671 2PRPRIV
     I                             P  596  599 0CPMOR1                          CODIGO
     I                             P  600  603 0CPLCB1                          COD. P
     I                                604  608 0CRHIP
     I* cedência de Comissões em JM
     I                                609  611 0JMM1                            CEDÊNC
     I                             P  612  614 3JMC1                            CEDÊNC
     I                                615  617 0JMM2                            CEDÊNC
     I                             P  618  620 3JMC2                            CEDÊNC
     I                             P  621  624 0CAPDP
     I                             P  625  628 0VLNOVO
     I                             P  629  632 0CAPREP
     I                                633  638 0DTCREP
     I                             P  639  642 0VLART5
     I                             P  672  675 2QIVAGR
     I                             P  676  679 2PRVAGR
     I                             P  649  653 2PROCUA
     I                                654  654  FROTA
     I                                655  656 0RRMAE
     I                                657  663 0APMAE
     I*--------------------------------------------------------
     I* campos novos adicionados em Maio 2007
     I                             P  643  648 0ADC
     I                                680  700  NIB
     I                                309  309  EMPREG
     I                                465  465  COMB
     I                                431  431  MEMO
     I*--------------------------------------------------------
     IFCONC     NS
     I                                  1    3 0CONC
     I                                  4   30  DESCON
     IFMED      NS
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
     I*                                      99 100 FILLER                DISPON
     I****************************************************
     I* GUARDIAN - TABELAS PU E VG                       *
     I****************************************************
     IFTABX     NS
     I                                  1    2  TPTAB
     I                                  3    7  CDTAB
     I                             P    8   11 2VALOR1
     I                                 12   91  DCTAB1
     I                                 92  171  DCTAB2
     I                             P  172  175 2VALOR2
     I                                187  266  DCTAB3
     I                             P  267  270 2VALOR3
     IECRAN     NS  01    1 C
     I* FORMAT-MENU
     I          NS  01    1 CA
     I                                  1    1  INDREG
     I                                  2    2 0OPCAO
     I                                  3   11 0RRAPOL
     I* FORMAT-INPUT1
     I          NS  01    1 CB
     I                                  1    1  INDREG
     I                                  2    6 0NSEG
     I                                  7    9 0ECR_CM#ROW
     I                                 10   12 0ECR_CM#COL
     I                                 13   22  ECR_CM#NMREC
     I                                 23   32  ECR_CM#NMFLD
     I* FORMAT-INPUT
     I          NS  01    1 CC
     I                                  1    1  INDREG
     I                                  2   41  LCB
     I                                 42   45 0CPOSL
     I                                 46   48 0ZPOSL
     I                                 49   68  CPLCB2
     I                                 69   69 0SIT
     I                                 70   77 0XDTINI
     I                                 70   71 0DDINIC
     I                                 72   73 0MMINIC
     I                                 74   77 0AAINI
     I                                 78   78 0DUR
     I                                 79   80 0DDFIM
     I                                 81   82 0MMFIM
     I                                 83   86 0AAFI
     I                                 87   87 0FRAC
     I                                 88   89 0MES1
     I                                 90   91 0MMCONT
     I                                 92   95 0AACON
     I                                 96   96 0CEMIS
     I                                 97   97 0ISEN
     I                                 98  100 0CONC
     I                                101  103 0MED1
     I                                104  108 3COM1
     I                                109  113 0MED2
     I                                114  118 0CRHIP
     I                                119  119 0IM
     I                                120  122 1DSCESP
     I                                123  123 0CS
     I                                124  124 0CR1
     I                                125  129 0CAPRC
     I                                130  138 2PR1
     I                                139  139 0CR2
     I                                140  145 0ECAPDP
     I                                146  154 2PR2
     I                                155  159 0VEXTRA
     I                                160  161 0CODFRQ
     I                                162  166 0FRANQ
     I                                167  167  PRIV
     I                                168  168  VSA
     I                                169  169  VIA
     I                                170  176 2PRPRIV
     I                                177  177  QIV
     I                                178  184 2PRQIV
     I                                185  185 0CR3
     I                                186  190 0CAP3
     I                                191  199 2PR3
     I                                200  200  AVISN
     I                                201  209 2PRS
     I                                210  215 0CAPRE
     I                                216  223 0DTCRE
     I                                224  234  MATR1
     I                                235  242 0DTMATX
     I                                243  245 0WCATV
     I                                243  243 0CATV
     I                                244  244 0CL
     I                                245  245 0PB
     I                                246  246 0NLUG1
     I* novo campo COMB, Maio 2007
     I                                247  247  COMB
     I*
     I                                248  261  MARC1
     I                                262  281  MODELO
     I                                282  286 0CILIND
     I                                287  290 0AC1X
     I                                291  296 0EVNOVO
     I                                297  297  VALMAT
     I                                298  298  ART5
     I                                299  304 0EVART5
     I                                305  312 0DTAR5
     I                                313  323  MATR2
     I                                324  337  MARC2
     I                                338  340 0CATREB
     I                                341  341 0TC
     I                                342  345 0NASCX
     I                                346  349 0CARTAX
     I                                350  350 0TB
     I                                351  353 0TEXTO
     I                                354  354  CL1
     I                                355  355  CL2
     I                                356  356  CL3
     I                                357  357  CL4
     I                                358  358  CL5
     I                                359  359  CL6
     I                                360  360  CL7
     I                                361  361  CL8
     I                                362  406  OBS1
     I                                407  409 0JMM1
     I                                410  414 3JMC1
     I                                415  459  OBS2
     I                                460  462 0JMM2
     I                                463  467 3JMC2
     I* novo campo EMPREG, Maio 2007
     I                                468  468  EMPREG
     I*
     I                                469  469  VIAG
     I                                470  477 0DTINV
     I                                478  485 0DTEXV
     I                                486  492 2PRSVG
     I                                493  493 0TCALVG
     I* novo campo ADC, Maio 2007
     I                                494  504 0ADC
     I*
     I                                505  505  PJUR
     I                                506  513 0DTINPJ
     I                                514  521 0DTEXPJ
     I                                522  528 2PRSPJ
     I                                529  529 0TCALPJ
     I* novo campo NIB, Maio 2007
     I                                530  550  NIB
     I* Controlo da Tecla de F04 (Valores possíveis)
     I                                551  553 0ECR_CM#ROW
     I                                554  556 0ECR_CM#COL
     I                                557  566  ECR_CM#NMREC
     I                                567  576  ECR_CM#NMFLD
     I*
     I* FORMAT-OUTPUT
     I          NS  01    1 CD
     I                                  1    1  INDREG
     I                                  2   41  LCB
     I                                 42   45 0CPOSL
     I                                 46   48 0ZPOSL
     I                                 49   68  CPLCB2
     I                                 69   69 0SIT
     I                                 70   77 0XDTINI
     I                                 70   71 0DDINIC
     I                                 72   73 0MMINIC
     I                                 74   77 0AAINI
     I                                 78   78 0DUR
     I                                 79   80 0DDFIM
     I                                 81   82 0MMFIM
     I                                 83   86 0AAFI
     I                                 87   87 0FRAC
     I                                 88   89 0MES1
     I                                 90   91 0MMCONT
     I                                 92   95 0AACON
     I                                 96   96 0CEMIS
     I                                 97   97 0ISEN
     I                                 98  100 0CONC
     I                                101  103 0MED1
     I                                104  108 3COM1
     I                                109  113 0MED2
     I                                114  114 0TALT
     I                                115  122 0DTALT
     I                                115  116 0DDALT
     I                                117  118 0MMALT
     I                                119  122 0AAALTX
     I                                121  122 0KAAALT
     I                                123  124 0MALT
     I                                125  127 0ACTA
     I                                128  132 0CRHIP
     I                                133  133 0IM
     I                                134  136 1DSCESP
     I                                137  137 0CS
     I                                138  138 0CR1
     I                                139  143 0CAPRC
     I                                144  152 2PR1
     I                                153  153 0CR2
     I                                154  159 0ECAPDP
     I                                160  168 2PR2
     I                                169  173 0VEXTRA
     I                                174  175 0CODFRQ
     I                                176  180 0FRANQ
     I                                181  187 2BONUS
     I                                188  188  PRIV
     I                                189  189  VSA
     I                                190  190  VIA
     I                                191  197 2PRPRIV
     I                                198  198  QIV
     I                                199  205 2PRQIV
     I                                206  206 0CR3
     I                                207  211 0CAP3
     I                                212  220 2PR3
     I                                221  221  AVISN
     I                                222  230 2PRS
     I                                231  236 0CAPRE
     I                                237  244 0DTCRE
     I                                245  253 2PRSAGR
     I                                254  260 2PRCSPA
     I                                261  271  MATR1
     I                                272  279 0DTMATX
     I                                280  282 0WCATV
     I                                280  280 0CATV
     I                                281  281 0CL
     I                                282  282 0PB
     I                                283  283 0NLUG1
     I* novo campo COMB, Maio 2007
     I                                284  284  COMB
     I*
     I                                285  298  MARC1
     I                                299  318  MODELO
     I                                319  323 0CILIND
     I                                324  327 0AC1X
     I                                328  333 0EVNOVO
     I                                334  334  VALMAT
     I                                335  335  ART5
     I                                336  341 0EVART5
     I                                342  349 0DTAR5
     I                                350  360  MATR2
     I                                361  374  MARC2
     I                                375  377 0CATREB
     I                                378  378 0TC
     I                                379  382 0NASCX
     I                                383  386 0CARTAX
     I                                387  387 0TEST
     I                                388  389 0DULSIN
     I                                390  391 0MULSIN
     I                                392  395 0AULSI
     I                                396  396 0NSIN
     I                                397  399 2AGRAV
     I                                400  400 0TB
     I                                401  403 0TEXTO
     I                                404  404  CL1
     I                                405  405  CL2
     I                                406  406  CL3
     I                                407  407  CL4
     I                                408  408  CL5
     I                                409  409  CL6
     I                                410  410  CL7
     I                                411  411  CL8
     I                                412  456  OBS1
     I                                457  459 0JMM1
     I                                460  464 3JMC1
     I                                465  509  OBS2
     I                                510  512 0JMM2
     I                                513  517 3JMC2
     I* novo campo EMPREG, Maio 2007
     I                                518  518  EMPREG
     I*
     I                                519  519  VIAG
     I                                520  527 0DTINV
     I                                528  535 0DTEXV
     I                                536  536 0TCALVG
     I* novo campo ADC, Maio 2007
     I                                537  547 0ADC
     I*
     I                                548  548  PJUR
     I                                549  556 0DTINPJ
     I                                557  564 0DTEXPJ
     I                                565  565 0TCALPJ
     I* novo campo NIB, Maio 2007
     I                                566  586  NIB
     I* Controlo da Tecla de F04 (Valores possíveis)
     I                                587  589 0ECR_CM#ROW
     I                                590  592 0ECR_CM#COL
     I                                593  602  ECR_CM#NMREC
     I                                603  612  ECR_CM#NMFLD
     I* FORMAT-ECERRO
     I          NS  01    1 CZ
     I                                  1    1  INDREG
     I*
     ITAROCU    NS
     I                                  1    4 0ANOTB
     I                                  5    5 0TIPOC
     I                                  6   10 0CAPITA
     I                                 11   17 2PRC6
     I                                 18   24 2PRC7
     I                                 26   28 1PERCDT
     IDESVAUT   NS
     I                                  1    4 0ANOTAB
     I                                  5    5 0NTAB
     I                                  6    8 0TMDES
     I                                  9   11 1PERV
     ITABDESV   NS
     I                                  1    8 0DATAI
     I                                  9    9 0NTAB
     I                                 10   12 0TMDES
     I                                 13   15 1PERV
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
     I* GUARDIAN - TABELA GERAIS (COD. CAE)              *
     I****************************************************
     IMPOSTNP   NS  03
     I                                  9   38  DESCR
     I****************************************************
     I*        VIATURAS CLASSICAS                        *
     I****************************************************
     ICLASSIC   NS
     I                                 56   56  ICCLAS
     C*=====================================================================
     C     *ENTRY        PLIST
     C                   PARM                    PRM_OPCAO
     C                   PARM                    PRM_RRAPOL
     C*
     C***                  EXSR LOADD                       TAB DESVAL.
     C*
     C                   MONITOR
     C*                  READ      ECRAN                                  92
     C                   ON-ERROR
     C                   ENDMON
     C     XPTO01        TAG
     C                   MOVE      *BLANKS       MENSER
     C                   MOVE      ' '           SPAC40           40
     C                   MOVE      'A'           INDREG
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
     C     OPCAO         COMP      1                                    26
     C  N26OPCAO         COMP      4                                  26
     C   26              MOVE      MSG(01)       MENSER           60
     C   26              EXSR      ERRO
     C   26              GOTO      XPTO01
     C* INICIO DA ALTERAÇÃO - só permite a opção 2 (consulta) para
     C* determinados utilizadores (U8 on)
     C   U8OPCAO         COMP      2                                  2626
     C   U8
     CAN 26              MOVE      MSG(01)       MENSER
     C   U8
     CAN 26              EXSR      ERRO
     C   U8
     CAN 26              GOTO      XPTO01
     C* FIM DA ALTERAÇÃO
     C     RR            COMP      50                                   2750
     C  N27RR            COMP      51                                 27  51
     C   27              MOVE      MSG(02)       MENSER           60
     C   27              EXSR      ERRO
     C   27              GOTO      XPTO01
     C*
     C                   SETOFF                                       27
     C     OPCAO         COMP      1                                      35
     C   35APOL          IFGT      *ZEROS
     C                   SETON                                        27
     C                   MOVE      MSG(80)       MENSER           60
     C                   EXSR      ERRO
     C                   GOTO      XPTO01
     C                   END
     C*
     C   35              MOVE      *BLANKS       APOL
     C     OPCAO         COMP      2                                      36
     C     OPCAO         COMP      3                                      37
     C     OPCAO         COMP      4                                      14
     C*
     C                   MOVE      UDATE         WANO              2 0
     C                   MOVE      WANO          AAIPL             4 0
     C     WANO          IFLT      40
     C                   MOVEL     20            AAIPL
     C                   ELSE
     C                   MOVEL     19            AAIPL
     C                   END
     C     AAIPL         SUB       1             AAIP0             4 0
     C     AAIPL         ADD       1             AAIP1             4 0
     C     AAIPL         ADD       2             AAIP2             4 0
     C                   MOVE      'A'           REF
     C                   MOVE      *BLANK        RSALDO           25
     C     RRAPOL        CHAIN     GBASEGRA                           16
     C   16
     CAN 36RRAPOL        CHAIN     BASEGRAT                           16
     C   16              MOVE      'NOVO'        FLAG              4
     C  N16              MOVE      'ALTE'        FLAG
     C  N16RECS          IFEQ      'S'
     C                   MOVEL     MSG(65)       RSALDO
     C                   END
     C*
     C                   SETOFF                                       1516
     C     RRAPOL        CHAIN     GBASEDIA                           15
     C   15RRAPOL        CHAIN     GBASEGRA                           16
     C   16
     CAN 36RRAPOL        CHAIN     BASEGRAT                           16
     C   35
     CANN15              MOVE      MSG(03)       MENSER           60
     C   35
     CANN15              EXSR      ERRO
     C   35
     CANN15              GOTO      XPTO01
     C*
     C   35
     CANN16              MOVE      MSG(04)       MENSER           60
     C   35
     CANN16              EXSR      ERRO
     C   35
     CANN16              GOTO      XPTO01
     C*
     C   36
     CAN 16
     COR 37
     CAN 16
     COR 14
     CAN 16              MOVE      MSG(05)       MENSER           60
     C   36
     CAN 16
     COR 37
     CAN 16
     COR 14
     CAN 16              EXSR      ERRO
     C   36
     CAN 16
     COR 37
     CAN 16
     COR 14
     CAN 16              GOTO      XPTO01
      *
     C  N15
     CORN16              DO
     C                   Z-ADD     ADC           WADC             11 0
     C                   MOVEL     NIB           WNIB             21
     C                   Z-ADD     AMCONT        WAMCON            6 0
     C                   END
      *
     C* CONTROLE PARA SINISTROS AUTO
     C                   Z-ADD     TB            WTB               1 0
     C                   Z-ADD     AGRAV         WAGRAV            3 0
     C                   Z-ADD     NSIN          WNSIN             1 0
     C                   Z-ADD     DTUSIN        WDUSIN            6 0
      *
     C                   Z-ADD     SIT           WSIT              1 0
     C                   Z-ADD     TALT          WTALT             1 0
     C                   Z-ADD     MALT          WMALT             2 0
     C                   Z-ADD     XDTALT        ZDTALT            6 0
     C                   Z-ADD     ADC           ZADC             11 0
     C                   MOVEL     NIB           ZNIB             21
     C                   Z-ADD     FRANQ         WFRQ              7 0
     C                   Z-ADD     CODFRQ        WCDFRQ            2 0
      *
     C                   Z-ADD     CAPDP         ECAPDP            6 0
     C                   Z-ADD     CAPDPI        ECAPDI            6 0
     C                   Z-ADD     VLNOVO        EVNOVO            6 0
     C                   Z-ADD     VLART5        EVART5            6 0
     C*
     C                   MOVE      *BLANKS       WOPCAO            9
     C   35              MOVEL     '  NOVOS '    WOPCAO
     C   36              MOVE      'CONSULTA'    WOPCAO
     C   37              MOVEL     'ALTERAÇÃ'    WOPCAO
     C   37              MOVE      'O'           WOPCAO
     C   14              MOVEL     'ALT.NºSE'    WOPCAO
     C   14              MOVE      'G'           WOPCAO
     C*
     C                   Z-ADD     0             W1                1 0
     C   35              DO
     C                   Z-ADD     0             TALT
     C                   Z-ADD     0             KDTALT
     C                   Z-ADD     0             DDALT
     C                   Z-ADD     0             MMALT
     C                   Z-ADD     0             AAALTX
     C                   Z-ADD     0             MALT
     C                   Z-ADD     0             ACTA
     C                   Z-ADD     0             BONUS
     C                   Z-ADD     0             PRSAGR
     C                   Z-ADD     0             PRCSPA
     C                   Z-ADD     0             PRCSP
     C                   Z-ADD     0             TEST
     C                   Z-ADD     0             DULSIN
     C                   Z-ADD     0             MULSIN
     C                   Z-ADD     0             AULSI
     C                   Z-ADD     0             NSIN
     C                   Z-ADD     0             AGRAV
     C                   Z-ADD     0             TOTREC
     C                   Z-ADD     0             NSEG
     C                   MOVE      SPAC40        SEG
     C                   MOVE      SPAC40        MOR
     C                   Z-ADD     0             CPMOR1
     C                   MOVE      SPAC40        CPMOR2
     C                   MOVE      'N'           RECS
     C                   Z-ADD     0             ADC
     C                   MOVE      *BLANKS       NIB
     C*--- NOVOS CAMPOS INTRODUZIDOS NO BASE
     C                   Z-ADD     0             CATREB
     C                   Z-ADD     0             VLNOVO
     C                   Z-ADD     0             VLART5
     C                   Z-ADD     0             VEXTRA
     C                   Z-ADD     0             CAPDPI
     C                   Z-ADD     0             DSCESP
     C                   Z-ADD     0             DTMATX
     C                   Z-ADD     0             CODFRQ
     C                   MOVE      'N'           ART5
     C                   Z-ADD     0             DTAR5
     C                   Z-ADD     0             CILIND
     C                   MOVE      *BLANKS       MODELO
     C   50              MOVE      *BLANK        PJUR
     C   50              MOVE      *BLANK        VIAG
     C   51              MOVE      *BLANK        PJUR
     C   51              MOVE      *BLANK        VIAG
     C                   Z-ADD     0             DTINPJ
     C                   Z-ADD     0             DTEXPJ
     C                   Z-ADD     0             TCALPJ
     C                   Z-ADD     0             JMM1
     C                   Z-ADD     0             JMC1
     C                   Z-ADD     0             JMM2
     C                   Z-ADD     0             JMC2
     C                   Z-ADD     0             CRHIP
     C                   MOVE      'N'           QIV
     C                   Z-ADD     0             PRQIV
     C                   Z-ADD     0             QIVAGR
     C                   MOVE      *BLANK        PRIV
     C                   MOVE      *BLANK        VSA
     C                   MOVE      *BLANK        VIA
     C                   Z-ADD     0             PRPRIV
     C                   Z-ADD     0             PRVAGR
     C                   Z-ADD     0             ECAPDP
     C                   Z-ADD     0             ECAPDI
     C                   Z-ADD     0             EVNOVO
     C                   Z-ADD     0             EVART5
     C                   Z-ADD     0             CAPRE
     C                   Z-ADD     0             DTCRE
     C                   Z-ADD     0             TC
     C                   Z-ADD     0             TCALVG
     C                   Z-ADD     0             TCALPJ
     C                   Z-ADD     0             PROCUA
     C                   Z-ADD     0             WCAPRC
     C                   MOVEL     *BLANK        CONTSN
     C                   Z-ADD     0             FRQ               7 0
     C                   Z-ADD     0             CDFRQ             2 0
     C                   END
     C*---------------------------------------
     C   35
     COR 14              GOTO      XPTO02
     C  N36              EXSR      VFSEG
     C*
     C   36
     CANN15              MOVE      MSG(70)       MENSER
     C   37
     CANN15              MOVE      MSG(71)       MENSER
     C*
     C   36
     COR 37              EXSR      BEGS04
     C   36
     COR 37              GOTO      XPTO04
     C**************************************************************************
     C*     * * *    1 - I N P U T    * * *
     C**************************************************************************
     C     XPTO02        TAG
      *
     C     NSEG          CHAIN     GFSEG                              10
     C  N10              DO
     C     JMM1          IFEQ      272
     C     PILOTO        COMP      'S'                                1010
     C                   ELSE
     C     CORSEL        IFEQ      'S'
     C                   SETON                                        10
     C                   ELSE
     C     STESP         COMP      ' '                                    10
     C  N10STESP         COMP      'N'                                    10
     C                   END
     C                   END
     C                   END
      *
     C                   Z-ADD     NSEG          WNSEG             5 0
     C                   MOVE      'B'           INDREG
     C                   EVAL      Func#Key = *BLANKS
+----C                   DOW       (Func#Key<>Key#F03) AND (Func#Key<>Key#F12)
|    C*                  DOW       (Func#Key <> KEY#F07)
|    C                   EXCEPT    INPU02
|    C                   READ      ECRAN                                  92
|+---C                   IF        (Func#Key = Key#F04)
||   C                   ExSr      RunPopUp
||   C                   ITER
|+---C                   ELSEIF    (Func#Key = KEY#F05)
||   C                   EXSR      Opcoes
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
     C                   SETON                                        37
     C     W1            ADD       1             W1
     C     W1            COMP      2                                    10
     C   10              DO
     C     NSEG          IFNE      WNSEG
     C                   GOTO      XPTO02
     C                   END
     C                   END
      *
     C   35              EXSR      BEGS03
     C   35              GOTO      XPTO03
     C   14              EXSR      BEGS04
     C   14              GOTO      XPTO04
     C**************************************************************************
     C*     * * *    2 - I N P U T    * * *
     C**************************************************************************
     C     XPTO03        TAG
      *
     C                   MOVE      *BLANKS       MENSER
     C                   MOVE      'C'           INDREG
     C                   EVAL      Func#Key = *BLANKS
+----C                   DOW       (Func#Key<>Key#F03) AND (Func#Key<>Key#F12)
|    C*                  DOW       (Func#Key <> KEY#F07)
|    C                   EXCEPT    INPU02
|    C                   EXCEPT    INPU03
|    C                   READ      ECRAN                                  92
|+---C                   IF        (Func#Key = Key#F04)
||   C                   ExSr      RunPopUp
||   C                   ITER
|+---C                   ELSEIF    (Func#Key = KEY#F05)
||   C                   EXSR      Opcoes
||   C                   ITER
|+---C                   ELSE
||   C                   LEAVE
|+---C                   ENDIF
+----C                   ENDDO
     C
+----C                   IF        (Func#Key=Key#F03) OR (Func#Key=Key#F12)
|    C*                  IF        (Func#Key = Key#F07)
|    C                   GOTO      FIM1
+----C                   ENDIF
      * DESCRIÇÃO POSTAL
     C     CPLCB1        IFNE      0
     C     CPLCB1        CHAIN     MPOSTNP                            95
     C  N95              MOVEL     DESCR         CPLCB2
     C   95              MOVEL     *BLANKS       CPLCB2
      *
     C   95              MOVE      MSG(114)      MENSER
     C   95              EXSR      ERRO
     C   95              GOTO      XPTO03
     C                   ELSE
     C                   MOVEL     *BLANKS       CPLCB2
     C                   END
     C*********************************************************
     C* INICIALIZA INDICAD. PARA POSSIBILITAR VALIDAÇÕES      *
     C                   SETOFF                                       020708       *
     C                   SETOFF                                       121318       *
     C                   SETOFF                                       232425       *
     C                   SETOFF                                       262829       *
     C                   SETOFF                                       313233       *
     C                   SETOFF                                       555657       *
     C                   SETOFF                                       585960       *
     C                   SETOFF                                       656667       *
     C                   SETOFF                                       727375       *
     C                   SETOFF                                       838485       *
     C                   SETOFF                                       8695         *
     C                   SETOFF                                       919394       *
     C*********************************************************
     C     IM            COMP      0                                    53
     C  N53IM            COMP      1                                  53
     C   53              MOVE      MSG(06)       MENSER           60
     C   53              EXSR      ERRO
     C   53              GOTO      XPTO03
     C*
     C     CS            COMP      0                                    18
     C  N18CS            COMP      2                                  18
     C   18              MOVE      MSG(119)      MENSER
     C   18              EXSR      ERRO
     C   18              GOTO      XPTO03
     C*
     C     SIT           COMP      1                                  5454
     C   54              MOVE      MSG(07)       MENSER
     C   54              EXSR      ERRO
     C   54              GOTO      XPTO03
     C*
     C     DDINIC        COMP      1                                    55
     C  N55DDINIC        COMP      31                                 55
     C   55              MOVE      MSG(08)       MENSER           60
     C   55              EXSR      ERRO
     C   55              GOTO      XPTO03
     C*
     C     MMINIC        COMP      1                                    56
     C  N56MMINIC        COMP      12                                 56
     C   56              MOVE      MSG(08)       MENSER           60
     C   56              EXSR      ERRO
     C   56              GOTO      XPTO03
     C*
     C     AAINI         COMP      AAIP0                                57
     C   57AAIPL         SUB       1             AAIP0             4 0
     C   57AAINI         COMP      AAIP0                                57
     C  N57AAINI         COMP      AAIP1                              57
     C   57              MOVE      MSG(08)       MENSER           60
     C   57              EXSR      ERRO
     C   57              GOTO      XPTO03
     C*
     C     DUR           COMP      1                                    30
     C  N30DUR           COMP      2                                  30
     C   30              MOVE      MSG(09)       MENSER           60
     C   30              EXSR      ERRO
     C   30              GOTO      XPTO03
     C*
     C                   EXSR      VDUR
     C   30              MOVE      MSG(09)       MENSER           60
     C   30              EXSR      ERRO
     C   30              GOTO      XPTO03
     C*
     C     DDFIM         COMP      0                                    59
     C  N59DDFIM         COMP      31                                 59
     C   59              MOVE      MSG(08)       MENSER           60
     C   59              EXSR      ERRO
     C   59              GOTO      XPTO03
     C*
     C     MMFIM         COMP      0                                    60
     C  N60MMFIM         COMP      12                                 60
     C   60              MOVE      MSG(08)       MENSER           60
     C   60              EXSR      ERRO
     C   60              GOTO      XPTO03
     C*
     C     AAFI          COMP      0                                    31
     C  N31AAFI          COMP      AAIP2                              31
     C   31              MOVE      MSG(08)       MENSER           60
     C   31              EXSR      ERRO
     C   31              GOTO      XPTO03
     C*
     C                   EXSR      VDTFIM
     C   31              SETON                                        5960
     C   31              MOVE      MSG(08)       MENSER           60
     C   31              EXSR      ERRO
     C   31              GOTO      XPTO03
     C*
     C     WDTFIM        IFNE      0
     C     DATINI        IFGT      WDTFIM
     C                   SETON                                        555657
     C                   MOVE      MSG(08)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   END
     C                   END
     C*
     C                   EXSR      VFRAC
     C   32              MOVE      MSG(10)       MENSER           60
     C   32              EXSR      ERRO
     C   32              GOTO      XPTO03
     C*
     C                   EXSR      VMES1
     C   33              MOVE      MSG(11)       MENSER           60
     C   33              EXSR      ERRO
     C   33              GOTO      XPTO03
     C*
     C                   EXSR      VCONT1
     C   34              MOVE      MSG(12)       MENSER           60
     C   34              EXSR      ERRO
     C   34              GOTO      XPTO03
     C*
     C     CEMIS         COMP      1                                    62
     C  N62CEMIS         COMP      2                                  62
     C   62              MOVE      MSG(13)       MENSER           60
     C   62              EXSR      ERRO
     C   62              GOTO      XPTO03
     C*
     C     ISEN          COMP      0                                    63
     C  N63ISEN          COMP      7                                  63
     C   63              MOVE      MSG(14)       MENSER           60
     C   63              EXSR      ERRO
     C   63              GOTO      XPTO03
     C*
     C                   EXSR      VCONC
     C   06              MOVE      MSG(15)       MENSER           60
     C   06              EXSR      ERRO
     C   06              GOTO      XPTO03
      *
     C     AVISN         COMP      'S'                                2020
     C   20AVISN         COMP      'N'                                2020
     C   20              MOVE      MSG(120)      MENSER
     C   20              EXSR      ERRO
     C   20              GOTO      XPTO03
     C*
     C     VALMAT        COMP      'S'                                2727
     C   27VALMAT        COMP      'N'                                2727
     C   27              MOVE      MSG(121)      MENSER
     C   27              EXSR      ERRO
     C   27              GOTO      XPTO03
     C* validação do combustível
     C     COMB          IFNE      'B'                                          biodiesel
     C     COMB          IFNE      'D'                                          diesel
     C     COMB          IFNE      'E'                                          eléctrico
     C     COMB          IFNE      'G'                                          gasolina
     C     COMB          IFNE      'H'                                          hibrido
     C     COMB          IFNE      'L'                                          gpl
     C                   MOVE      MSG(122)      MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C* validação da ADC
     C     ADC           IFEQ      0
     C     NIB           IFNE      *BLANK
     C                   EXSR      VALADC
     C     OUMSG         IFNE      *BLANKS
     C                   MOVEL     OUMSG         MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   END
     C                   END
     C                   END
     C* validação do NIB
     C     NIB           IFEQ      *BLANK
     C     ADC           IFNE      0
     C                   EXSR      VALADC
     C     OUMSG         IFNE      *BLANKS
     C                   MOVEL     OUMSG         MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   END
     C                   END
     C                   END
     C* validação Seguro Novo com ADC e NIB preenchido
     C*          TALT      IFEQ *ZERO                      seguro novo
     C*          ADC       IFNE *ZERO
     C*                    MOVE MSG,126   MENSER
     C*                    EXSR ERRO
     C*                    GOTO XPTO03
     C*                    END
     C*                    END
     C* se ADC e NIB preenchidos o AVISN têm que ser S
     C     ADC           IFNE      *ZERO
     C     NIB           IFNE      *BLANK
     C                   MOVE      'S'           AVISN
     C                   END
     C                   END
     C* validação do codigo empregado
     C     EMPREG        IFNE      'S'
     C     EMPREG        IFNE      ' '
     C                   MOVE      MSG(125)      MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   END
     C                   END
     C*
     C                   EXSR      VMED1
     C   65
     COR 66              EXSR      ERRO
     C   65
     COR 66              GOTO      XPTO03
     C   07              MOVE      MSG(16)       MENSER           60
     C   07              EXSR      ERRO
     C   07              GOTO      XPTO03
     C*
     C     TALT          IFEQ      0
     C     RR            LOOKUP    TABRR         TABPC                    80
     C   80              MOVE      TABPC         COMISS
      *
     C     ISEN          IFEQ      1
     C     CORR1         MULT(H)   1.20          CORR              5 3
     C     ANGA1         MULT(H)   1.20          ANGA              5 3
     C     COBR1         MULT(H)   1.20          COBR              5 3
     C                   ELSE
     C                   Z-ADD     CORR1         CORR
     C                   Z-ADD     ANGA1         ANGA
     C                   Z-ADD     COBR1         COBR
     C                   END
     C                   Z-ADD     CORR          COM1
     C*
     C     JMM1          IFGT      *ZEROS
     C     JMM2          IFEQ      0
     C     ANGA          ADD       COBR          JMC1
     C                   END
     C                   ELSE
     C     JMM2          IFGT      *ZEROS
     C                   Z-ADD     ANGA          JMC2
     C                   END
     C                   END
      *
     C     MED1          IFEQ      906                                          MARSH
     C                   ADD       3.00          COM1
     C                   ELSE
     C     JMM2          IFEQ      418                                          JOÃO ALVADIA
     C                   Z-ADD     0             JMC2
     C                   ELSE
     C     JMM2          IFEQ      443                                          FERNANDO SILVA
     C     ANGA          ADD       COBR          JMC2
     C                   ELSE
     C     JMM2          IFEQ      447                                          VERA SANTOS
     C     ANGA          ADD       COBR          JMC2
     C                   ELSE
     C     JMM2          IFEQ      801                                          JORGE DIAZ
     C     ANGA          ADD       COBR          JMC2
     C                   ELSE
     C     JMM1          IFEQ      001                                          JM-CLUBE TAP
     C                   Z-ADD     CORR          JMC1
     C                   ELSE
     C     JMM1          IFEQ      826                                          ANTRALMED
     C                   Z-ADD     CORR          JMC1
     C                   ELSE
     C     JMM1          IFEQ      827                                          EUROPATRIMONIAL
     C     JMM2          IFEQ      0
     C                   Z-ADD     CORR          JMC1
     C                   END
     C                   ELSE
     C     JMM1          IFEQ      811                                          POLINSUR
     C     JMM2          IFEQ      0
     C                   Z-ADD     CORR          JMC1
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C*
     C     COM1          COMP      0                                    6464
     C   64              MOVE      MSG(17)       MENSER           60
     C   64              EXSR      ERRO
     C   64              GOTO      XPTO03
     C*
     C                   EXSR      VMED2
     C   02              MOVE      MSG(18)       MENSER           60
     C   02              EXSR      ERRO
     C   02              GOTO      XPTO03
     C*
     C                   SETOFF                                       58
     C     CRHIP         IFGT      *ZEROS
     C                   MOVEL     20            K7                7 0
     C                   MOVE      CRHIP         K7
     C     K7            CHAIN     ENTIDADE                           58
     C   58              MOVE      MSG(112)      MENSER
     C   58              EXSR      ERRO
     C   58              GOTO      XPTO03
     C                   END
     C*
     C                   Z-ADD     0             TALT
     C                   Z-ADD     0             DDALT
     C                   Z-ADD     0             MMALT
     C                   Z-ADD     0             AAALTX
     C                   Z-ADD     0             MALT
     C                   Z-ADD     0             ACTA
     C*
     C     DSCESP        COMP      0                                    90
     C   90              MOVE      MSG(72)       MENSER           60
     C   90              EXSR      ERRO
     C   90              GOTO      XPTO03
     C*
     C   50              DO
     C     CR1           COMP      2                                  3838
     C   38              MOVE      MSG(20)       MENSER           60
     C   38              EXSR      ERRO
     C   38              GOTO      XPTO03
     C*
     C*          CR2       IFEQ *ZEROS
     C*          QIV       IFEQ 'S'
     C*          MED1      IFNE 031                        MORTÁGUA
     C*          MED1      IFNE 921                        CRE
     C*          JMM1      IFNE 272                        CORSEL
     C*                    SETON                     73
     C*                    MOVE MSG,99    MENSER
     C*                    EXSR ERRO
     C*                    GOTO XPTO03
     C*                    END
     C*                    END
     C*                    END
     C*                    END
     C*                    END
     C*
     C                   END
     C*
     C   51              DO
     C     CR1           COMP      1                                  3838
     C   38              MOVE      MSG(20)       MENSER           60
     C   38              EXSR      ERRO
     C   38              GOTO      XPTO03
     C                   END
     C*
     C                   EXSR      SCAPRC
     C   08              MOVE      MSG(21)       MENSER           60
     C   08              EXSR      ERRO
     C   08              GOTO      XPTO03
     C*
     C     CAPRC         IFLT      3250
     C     DATINI        IFGE      20091201
     C                   SETON                                        08
     C                   MOVE      MSG(21)       MENSER           60
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   END
     C                   END
     C*
     C     CAPRC         IFGE      3250
     C     CAPRC         IFNE      99999
     C     DATINI        IFLT      20091201
     C                   SETON                                        08
     C                   MOVE      MSG(21)       MENSER           60
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   END
     C                   END
     C                   END
     C*
     C   50CR2           IFGT      *ZERO
     C                   MOVE      DDMATX        DDMAT
     C                   MOVE      MMMATX        MMMAT
     C                   MOVE      AAMATX        AAMAT
      *
     C   35DATMAT        COMP      DATINI                             12
     C   35
     CAN 12              MOVE      MSG(08)       MENSER           60
     C   35
     CAN 12              EXSR      ERRO
     C   35
     CAN 12              GOTO      XPTO03
     C                   END
     C*
     C                   EXSR      VCR2
     C   39              MOVE      MSG(22)       MENSER           60
     C   39              EXSR      ERRO
     C   39              GOTO      XPTO03
     C*
     C                   SETOFF                                       09
     C   50VEXTRA        COMP      0                                    09
     C   50
     CAN 09              MOVE      MSG(75)       MENSER
     C   50
     CAN 09              EXSR      ERRO
     C   50
     CAN 09              GOTO      XPTO03
     C*
     C                   SETOFF                                       11
     C     CODFRQ        LOOKUP    TABFRQ                                 80
     C  N80              SETON                                        11
     C   11              MOVE      MSG(76)       MENSER
     C   11              EXSR      ERRO
     C   11              GOTO      XPTO03
     C*
     C**         CODFRQ    IFEQ 09
     C**         ECAPDI    COMP 50000                11
     C** 11                MOVE MSG,130   MENSER
     C** 11                EXSR ERRO
     C** 11                GOTO XPTO03
     C**                   END
     C*
     C     CODFRQ        IFEQ      09
     C     CATREB        IFEQ      0
     C     CATV          COMP      2                                  11
     C   11CATV          COMP      6                                    11
     C   11              MOVE      MSG(131)      MENSER
     C   11              EXSR      ERRO
     C   11              GOTO      XPTO03
     C                   END
     C                   END
     C*
     C     CODFRQ        IFEQ      09
     C                   EXSR      ANTIG
     C     #MESES        IFGE      96
     C                   SETON                                        11
     C   11              MOVE      MSG(132)      MENSER
     C   11              EXSR      ERRO
     C   11              GOTO      XPTO03
     C                   END
     C                   END
     C*
     C   50              DO
     C                   SETOFF                                       21
     C     CR2           IFGT      *ZEROS
     C     ART5          IFNE      'S'
     C     ART5          IFNE      'N'
     C                   SETON                                        21
     C   21              MOVE      MSG(78)       MENSER
     C   21              EXSR      ERRO
     C   21              GOTO      XPTO03
     C                   END
     C                   END
     C                   END
      *
     C                   EXSR      SRPU
     C  N80              Z-ADD     TVALOR        PRPRIV
     C   80              DO
     C                   MOVE      *BLANKS       PRPRIV
     C                   SETON                                        23
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   END
      *
     C     CR2           IFEQ      *ZERO
     C     PRIV          IFNE      ' '
     C                   MOVE      *BLANKS       PRPRIV
     C                   SETON                                        23
     C                   MOVE      MSG(116)      MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   END
     C     ISEN          IFEQ      1
     C     PRPRIV        DIV(H)    1.20          PRPRIV
     C                   END
     C                   END
     C*
     C                   SETOFF                                       73
     C     QIV           IFNE      'S'
     C     QIV           IFNE      'N'
     C                   SETON                                        73
     C                   MOVE      MSG(99)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   END
     C                   END
     C*
     C                   SETOFF                                       73
     C     QIV           IFEQ      'S'
     C     CR2           IFGT      0
     C     CR2           IFEQ      3
     C                   SETON                                        73
     C                   MOVE      MSG(99)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   ELSE
     C     CR2           IFEQ      4
     C                   SETON                                        73
     C                   MOVE      MSG(99)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   ELSE
     C     CR2           IFEQ      8
     C                   SETON                                        73
     C                   MOVE      MSG(99)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C*
     C     QIV           IFEQ      'S'
     C**** ATÉ 31.07.2002  Z-ADD33.32     PRQIV
     C                   Z-ADD     37.76         PRQIV
     C     ISEN          IFEQ      1
     C     PRQIV         DIV(H)    1.20          PRQIV
     C                   END
     C                   ELSE
     C                   Z-ADD     *ZEROS        PRQIV
     C                   END
     C                   END
     C*
     C                   SETOFF                                       22
     C   50              DO
     C     CR2           IFGT      *ZEROS
     C     ART5          IFEQ      'S'
     C     EVART5        IFLE      *ZEROS
     C                   SETON                                        22
     C   22              MOVE      MSG(79)       MENSER
     C   22              EXSR      ERRO
     C   22              GOTO      XPTO03
     C                   END
     C                   Z-ADD     EVART5        CAPDP
     C                   Z-ADD     EVART5        CAPDPI
     C                   END
     C                   END
     C*
     C                   SETOFF                                       13
     C     CR2           IFGT      *ZEROS
     C     ART5          IFEQ      'N'
     C     EVNOVO        IFLE      *ZEROS
     C                   SETON                                        13
     C   13              MOVE      MSG(77)       MENSER
     C   13              EXSR      ERRO
     C   13              GOTO      XPTO03
     C                   END
     C                   END
     C                   END
     C*
     C     CR3           COMP      0                                    24
     C  N24CR3           COMP      3                                  24
     C   24              MOVE      MSG(22)       MENSER
     C   24              EXSR      ERRO
     C   24              GOTO      XPTO03
     C                   END
     C*
     C     MARC1         COMP      *BLANK                                 97
     C   97              MOVE      MSG(44)       MENSER
     C   97              EXSR      ERRO
     C   97              GOTO      XPTO03
     C*
     C                   EXSR      VMATR
     C   98              MOVE      MSG(45)       MENSER
     C   98              EXSR      ERRO
     C   98              GOTO      XPTO03
     C*
     C     DTMATX        COMP      *ZEROS                                 12
     C   12              MOVE      MSG(08)       MENSER
     C   12              EXSR      ERRO
     C   12              GOTO      XPTO03
     C     AAMATX        COMP      *ZEROS                                 12
     C  N12AAMATX        COMP      AAIPL                              12
     C   12              MOVE      MSG(08)       MENSER
     C   12              EXSR      ERRO
     C   12              GOTO      XPTO03
     C     MMMATX        LOOKUP    TABMES        TABDIA                   80
     C  N80              SETON                                          12
     C   12              MOVE      MSG(08)       MENSER
     C   12              EXSR      ERRO
     C   12              GOTO      XPTO03
     C     DDMATX        COMP      *ZEROS                                 12
     C   12              MOVE      MSG(08)       MENSER
     C   12              EXSR      ERRO
     C   12              GOTO      XPTO03
     C     DDMATX        COMP      TABDIA                             12
     C   12              MOVE      MSG(08)       MENSER
     C   12              EXSR      ERRO
     C   12              GOTO      XPTO03
     C*
     C     NLUG1         COMP      *ZERO                                9999
     C   99              MOVE      MSG(46)       MENSER
     C   99              EXSR      ERRO
     C   99              GOTO      XPTO03
     C*
     C                   SETOFF                                       95
     C     CILIND        IFEQ      0
     C                   SETON                                        95
     C                   MOVE      MSG(127)      MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   END
     C*
     C     AC1X          COMP      *ZEROS                                 68
     C  N68AC1X          COMP      AAIPL                              68
     C  N68AC1X          COMP      AAMATX                             68
     C   68              MOVE      MSG(26)       MENSER           60
     C   68              EXSR      ERRO
     C   68              GOTO      XPTO03
     C*
     C     CL            COMP      0                                    69
     C  N69CL            COMP      5                                  69
     C   69              MOVE      MSG(27)       MENSER           60
     C   69              EXSR      ERRO
     C   69              GOTO      XPTO03
     C*
     C     PB            COMP      0                                    70
     C  N70PB            COMP      6                                  70
     C   70              MOVE      MSG(28)       MENSER           60
     C   70              EXSR      ERRO
     C   70              GOTO      XPTO03
     C*
     C     CATV          COMP      1                                    71
     C  N71CATV          COMP      7                                  71
     C   71              MOVE      MSG(29)       MENSER           60
     C   71              EXSR      ERRO
     C   71              GOTO      XPTO03
     C*
     C                   MOVEL     '1000'        W4                4 0
     C     WCATV         IFGE      501
     C     WCATV         IFLE      504
     C                   MOVEL     '2000'        W4
     C                   END
     C                   END
     C*-------------------------------------------------------
     C* A PARTIR DE 01.03.99 A CATV-150 FOI EXTINTA PARA R.C.
     C* EXISTEM SÓ 2 CATV 130/140, ATÉ 1500CC +1500CC
     C                   MOVEL     AAINI         WRK6              6 0
     C                   MOVE      MMINIC        WRK6
     C                   MOVEL     WRK6          WRK8              8 0
     C                   MOVE      DDINIC        WRK8
     C     WRK8          IFGE      19990301
     C     WCATV         IFEQ      150
     C                   MOVE      140           WCATV
     C                   END
     C                   END
     C*-------------------------------------------------------
     C     WCATV         IFNE      500                                          <-------
     C                   MOVE      WCATV         W4                                    |
     C                   EXSR      SRCALL                                              |
     C     WDESC         COMP      *BLANKS                                71           |
     C   71              MOVE      MSG(29)       MENSER           60                   |
     C   71              EXSR      ERRO                                                |
     C   71              GOTO      XPTO03                                              |
     C                   END                                                    <-------
     C*
     C                   SETOFF                                       94
     C     CATREB        IFGT      *ZEROS                                       <-------
     C     MATR2         IFNE      *BLANK                                       <----  |
     C                   MOVEL     '2000'        W4                                 |  |
     C                   MOVE      CATREB        W4                                 |  |
     C                   EXSR      SRCALL                                           |  |
     C     WDESC         COMP      *BLANKS                                93        |  |
     C   93              MOVE      MSG(73)       MENSER           60                |  |
     C   93              EXSR      ERRO                                             |  |
     C   93              GOTO      XPTO03                                           |  |
     C                   ELSE                                                       *  |
     C                   SETON                                        94            |  |
     C   94              MOVE      MSG(74)       MENSER           60                |  |
     C   94              EXSR      ERRO                                             |  |
     C   94              GOTO      XPTO03                                           |  |
     C                   END                                                    <----  |
     C                   END                                                    <-------
     C*
     C   50TC            COMP      0                                  4242
     C   51TC            COMP      0                                      42
     C   51
     CANN42TC            COMP      3                                  42
     C   42              MOVE      MSG(30)       MENSER
     C   42              EXSR      ERRO
     C   42              GOTO      XPTO03
     C*
     C                   EXSR      VNASC
     C   43              GOTO      XPTO03
     C*
     C                   EXSR      VCARTA
     C   44              GOTO      XPTO03
     C*
     C                   Z-ADD     0             TEST
     C                   Z-ADD     0             DULSIN
     C                   Z-ADD     0             MULSIN
     C                   Z-ADD     0             AULSI
     C                   Z-ADD     0             NSIN
     C                   Z-ADD     0             AGRAV
     C                   EXSR      VTB
     C   45              MOVE      MSG(35)       MENSER           60
     C   45              EXSR      ERRO
     C   45              GOTO      XPTO03
     C*
     C     CL1           COMP      ' '                                  46
     C  N46CL1           COMP      'K'                                46
     C   46              MOVE      MSG(36)       MENSER           60
     C   46              EXSR      ERRO
     C   46              GOTO      XPTO03
     C*
     C     CL1           COMP      'A'                                    46
     C   46              MOVE      MSG(36)       MENSER           60
     C   46              EXSR      ERRO
     C   46              GOTO      XPTO03
     C*
     C     CL2           COMP      ' '                                  76
     C  N76CL2           COMP      'K'                                76
     C   76              MOVE      MSG(36)       MENSER           60
     C   76              EXSR      ERRO
     C   76              GOTO      XPTO03
     C*
     C     CL3           COMP      ' '                                  77
     C  N77CL3           COMP      'K'                                77
     C   77              MOVE      MSG(36)       MENSER           60
     C   77              EXSR      ERRO
     C   77              GOTO      XPTO03
     C*
     C     CL4           COMP      ' '                                  78
     C  N78CL4           COMP      'K'                                78
     C   78              MOVE      MSG(36)       MENSER           60
     C   78              EXSR      ERRO
     C   78              GOTO      XPTO03
     C*
     C     CL5           COMP      ' '                                  79
     C  N79CL5           COMP      'K'                                79
     C   79              MOVE      MSG(36)       MENSER           60
     C   79              EXSR      ERRO
     C   79              GOTO      XPTO03
     C*
     C     CL6           COMP      ' '                                  88
     C  N88CL6           COMP      'K'                                88
     C   88              MOVE      MSG(36)       MENSER           60
     C   88              EXSR      ERRO
     C   88              GOTO      XPTO03
     C*
     C     CL7           COMP      ' '                                  81
     C  N81CL7           COMP      'K'                                81
     C   81              MOVE      MSG(36)       MENSER           60
     C   81              EXSR      ERRO
     C   81              GOTO      XPTO03
     C*
     C     CL8           COMP      ' '                                  82
     C  N82CL8           COMP      'K'                                82
     C   82              MOVE      MSG(36)       MENSER           60
     C   82              EXSR      ERRO
     C   82              GOTO      XPTO03
     C*
     C                   EXSR      SRAV
     C   80              DO
     C                   SETON                                          28
     C                   MOVE      MSG(47)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   END
     C*
     C     VIAG          IFNE      ' '
     C                   MOVEL     CATV          W1CATV            2 0
     C                   MOVE      CL            W1CATV
     C                   MOVEL     W1CATV        W2CATV            3 0
     C                   MOVE      PB            W2CATV
     C     W2CATV        IFGE      300
     C     W2CATV        IFLT      400
     C     VIAG          IFNE      'P'
     C                   SETON                                        28
     C                   MOVE      MSG(49)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   END
     C                   END
     C                   END
     C*
     C     DTINV         IFEQ      *ZEROS
     C                   SETON                                          29
     C                   MOVE      MSG(50)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   END
     C*
     C     DDINVG        IFGT      0
     C     DDINVG        IFLT      32
     C                   ELSE
     C                   SETON                                          29
     C                   MOVE      MSG(08)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   END
     C                   ELSE
     C                   SETON                                          29
     C                   MOVE      MSG(08)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   END
     C*
     C     MMINVG        IFGT      0
     C     MMINVG        IFLT      13
     C                   ELSE
     C                   SETON                                          29
     C                   MOVE      MSG(08)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   END
     C                   ELSE
     C                   SETON                                          29
     C                   MOVE      MSG(08)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   END
     C                   END
     C*
     C     VIAG          IFEQ      ' '
     C     TCALVG        IFGT      0
     C                   SETON                                          75
     C                   MOVE      MSG(54)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   END
     C                   END
     C*
     C     VIAG          IFNE      ' '
     C     TCALVG        IFLT      0
     C                   SETON                                          75
     C                   MOVE      MSG(58)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   ELSE
     C     TCALVG        IFGT      2
     C                   SETON                                          75
     C                   MOVE      MSG(58)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   END
     C                   END
     C*
     C     TALT          IFEQ      0
     C     TCALVG        IFNE      1
     C                   SETON                                          75
     C                   MOVE      MSG(55)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   END
     C                   END
     C*
     C     TALT          IFEQ      1
     C     TCALVG        IFGT      1
     C                   SETON                                          75
     C                   MOVE      MSG(59)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   END
     C                   END
     C*
     C     TALT          IFGT      1
     C     TALT          IFLT      5
     C     TCALVG        IFGT      0
     C                   SETON                                          75
     C                   MOVE      MSG(60)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   END
     C                   END
     C                   END
     C*
     C     TALT          IFEQ      5
     C     TCALVG        IFEQ      0
     C                   ELSE
     C     TCALVG        IFNE      2
     C                   SETON                                          75
     C                   MOVE      MSG(57)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   END
     C                   END
     C                   END
     C*
     C     TALT          IFEQ      6
     C     TCALVG        IFNE      2
     C                   SETON                                          75
     C                   MOVE      MSG(56)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   END
     C                   END
     C*
     C     TALT          IFEQ      7
     C     TCALVG        IFNE      1
     C                   SETON                                          75
     C                   MOVE      MSG(61)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   END
     C                   END
     C*
     C*          FLAG      IFEQ 'ALTE'
     C*          TALT      IFEQ 0
     C*                    SETON                       75
     C*                    MOVE MSG,37    MENSER
     C*                    EXSR ERRO
     C*                    GOTO XPTO03
     C*                    END
     C*                    END
     C*
     C                   MOVEL     AAINV         WDTINV            8 0
     C                   MOVEL     MMINVG        FLD4              4 0
     C                   MOVE      DDINVG        FLD4
     C                   MOVE      FLD4          WDTINV
     C                   EXSR      BEGS10
     C                   END
     C*
     C* PROTECÇÃO JURÍDICA
     C     PJUR          IFEQ      'S'
     C                   ELSE
     C     PJUR          IFEQ      ' '
     C                   ELSE
     C                   SETON                                          83
     C                   MOVE      MSG(84)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   END
     C                   END
     C*
     C     PJUR          IFEQ      'S'
     C     DTINPJ        IFEQ      *ZEROS
     C                   SETON                                          84
     C                   MOVE      MSG(87)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   ELSE
     C                   MOVEL     AAINP         WDTINP            8 0
     C                   MOVEL     MMINPJ        FLD4              4 0
     C                   MOVE      DDINPJ        FLD4
     C                   MOVE      FLD4          WDTINP
     C     WDTINP        IFLT      19990301
     C                   SETON                                          84
     C                   MOVE      MSG(86)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   END
     C                   END
     C*
     C     DDINPJ        IFGT      0
     C     DDINPJ        IFLT      32
     C                   ELSE
     C                   SETON                                          84
     C                   MOVE      MSG(08)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   END
     C                   ELSE
     C                   SETON                                          84
     C                   MOVE      MSG(08)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   END
     C*
     C     MMINPJ        IFGT      0
     C     MMINPJ        IFLT      13
     C                   ELSE
     C                   SETON                                          84
     C                   MOVE      MSG(08)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   END
     C                   ELSE
     C                   SETON                                          84
     C                   MOVE      MSG(08)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   END
     C                   END
     C*
     C     PJUR          IFEQ      ' '
     C     TCALPJ        IFNE      0
     C                   SETON                                          86
     C                   MOVE      MSG(89)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   END
     C                   END
     C*
     C     PJUR          IFEQ      'S'
     C     TCALPJ        IFLT      0
     C                   SETON                                          86
     C                   MOVE      MSG(93)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   ELSE
     C     TCALPJ        IFGT      2
     C                   SETON                                          86
     C                   MOVE      MSG(93)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   END
     C                   END
     C*
     C     TALT          IFEQ      0
     C     TCALPJ        IFNE      1
     C                   SETON                                          86
     C                   MOVE      MSG(90)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   END
     C                   END
     C*
     C     TALT          IFEQ      1
     C     TCALPJ        IFGT      1
     C                   SETON                                          86
     C                   MOVE      MSG(94)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   END
     C                   END
     C*
     C     TALT          IFGT      1
     C     TALT          IFLT      5
     C     TCALPJ        IFGT      0
     C                   SETON                                          86
     C                   MOVE      MSG(95)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   END
     C                   END
     C                   END
     C*
     C     TALT          IFEQ      5
     C     TCALPJ        IFEQ      0
     C                   ELSE
     C     TCALPJ        IFNE      2
     C                   SETON                                          86
     C                   MOVE      MSG(92)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   END
     C                   END
     C                   END
     C*
     C     TALT          IFEQ      8
     C     TCALPJ        IFNE      2
     C                   SETON                                          86
     C                   MOVE      MSG(91)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   END
     C                   END
     C*
     C     TALT          IFEQ      9
     C     TCALPJ        IFNE      1
     C                   SETON                                          86
     C                   MOVE      MSG(96)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   END
     C                   END
     C*
     C                   MOVEL     AAINP         WDTINP            8 0
     C                   MOVEL     MMINPJ        FLD4              4 0
     C                   MOVE      DDINPJ        FLD4
     C                   MOVE      FLD4          WDTINP
     C                   EXSR      BEGS11
     C                   END
     C*-----------------------------------------------------
     C                   SETOFF                                       40
     C     ECAPDP        IFNE      0
     C     ECAPDP        IFGT      80000
     C     USER          IFNE      'MMATA'
     C     USER          IFNE      'EDUARDO'
[LG] C     USER          IFNE      'LGALHOZ'
     C                   SETON                                        40
     C                   MOVE      MSG(113)      MENSER           60
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
[LG] C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C     USER          IFNE      'EDUARDO'
[LG] C     USER          IFNE      'LGALHOZ'
     C     ECAPDP        COMP      225000                             40
     C   40              MOVE      MSG(23)       MENSER           60
     C   40              EXSR      ERRO
     C   40              GOTO      XPTO03
[LG] C                   END
     C                   END
     C*
     C   50              DO                                                     <-------------------
     C     CR2           IFEQ      *ZEROS
     C     ECAPDP        IFNE      *ZEROS
     C                   SETON                                        40
     C                   MOVE      MSG(69)       MENSER           60
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   END
     C                   END
     C                   END                                                    <-------------------
     C*
     C   KA
     CAN 50              DO
     C                   EXSR      GP700                                        TRATA RC
     C     CR3           IFGT      *ZEROS
     C                   MOVE      2001          ANOTB
     C     K10           CHAIN     TAROCU                             25
     C   25              MOVE      MSG(24)       MENSER           60
     C   25              EXSR      ERRO
     C   25              GOTO      XPTO03
     C     NLUG1         IFGT      6
     C                   Z-ADD     PRC7          PR3
     C                   ELSE
     C                   Z-ADD     PRC6          PR3
     C                   END
     C     ISEN          IFEQ      1
     C     PR3           DIV(H)    1.20          PR3
     C                   END
     C                   END
     C     CR2           IFGT      *ZEROS
     C                   EXSR      GP800                                        TRATA DP
     C   13              MOVE      MSG(77)       MENSER
     C   13              EXSR      ERRO
     C   13              GOTO      XPTO03
     C                   END
     C                   MOVE      MSG(81)       MENSER           60
     C                   GOTO      XPTO03
     C                   END
     C*
     C   KA
     CAN 51              DO
     C                   MOVE      2001          ANOTB
     C                   MOVE      CAPRC         CAP3
     C                   MOVE      TC            CR3
     C     K10           CHAIN     TAROCU                             08
     C                   MOVE      *ZEROS        CAP3
     C                   MOVE      *ZEROS        CR3
     C   08              MOVE      MSG(24)       MENSER
     C   08              EXSR      ERRO
     C   08              GOTO      XPTO03
     C     NLUG1         IFGT      6
     C                   Z-ADD     PRC7          PR1
     C                   ELSE
     C                   Z-ADD     PRC6          PR1
     C                   END
     C     ISEN          IFEQ      1
     C     PR1           DIV(H)    1.20          PR1
     C                   END
     C                   MOVE      1             CR1
     C                   MOVE      2             CR2
     C     CAPRC         MULT      .10           CAPDP
     C                   END
     C*
     C   50              DO                                                     <-------------------
     C     CR2           IFNE      *ZEROS
     C     ECAPDP        IFEQ      *ZEROS
     C                   SETON                                        39
     C                   MOVE      MSG(68)       MENSER           60
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   END
     C                   END
     C                   SETOFF                                       2426
     C     CR3           IFNE      *ZEROS
     C     CATV          IFEQ      4                                             MOTOCICLO
     C                   SETON                                        24
     C                   MOVE      MSG(118)      MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   END
     C                   END
     C     CR3           IFEQ      *ZEROS
     C     CAP3          IFNE      *ZEROS
     C                   SETON                                        24
     C                   MOVE      MSG(22)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   END
     C     PR3           IFNE      *ZEROS
     C                   SETON                                        26
     C                   MOVE      MSG(25)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   END
     C                   END
     C*
     C     ECAPDI        IFGT      80000
     C     USER          IFNE      'MMATA'
     C     USER          IFNE      'EDUARDO'
[LG] C     USER          IFNE      'LGALHOZ'
     C                   MOVE      MSG(113)      MENSER           60
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
[LG] C                   END
     C                   END
     C                   END
     C                   END
     C                   END                                                    <-------------------
     C*
     C     PR1           COMP      *ZEROS                               9696
     C   96              MOVE      MSG(43)       MENSER
     C   96              EXSR      ERRO
     C   96              GOTO      XPTO03
     C*
     C     PR1           ADD       PR2           PRTOT             9 2
     C                   ADD       PR3           PRTOT
     C     PRS           COMP      PRTOT                              4141
     C   41              MOVE      MSG(25)       MENSER
     C   41              EXSR      ERRO
     C   41              GOTO      XPTO03
     C*
     C   50              DO
     C     CAPRE         COMP      0                                  9191
     C   91              Z-ADD     1             INCAP             1 0
     C  N91              Z-ADD     0             INCAP
     C     DTCRE         COMP      0                                  9191
     C   91              Z-ADD     1             INDAT             1 0
     C  N91              Z-ADD     0             INDAT
     C     INCAP         IFNE      INDAT
     C                   SETON                                        91
     C                   MOVE      MSG(117)      MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   ELSE
     C                   SETOFF                                       91
     C                   END
     C                   END
     C*
     C     JMM1          IFGT      *ZEROS
     C     JMM1          CHAIN     MCORNP                             80
     C   80              SETON                                        66
     C   80              MOVE      MSG(102)      MENSER
     C   80              EXSR      ERRO
     C   80              GOTO      XPTO03
     C     TMEDCR        IFEQ      2
     C                   SETON                                        66
     C                   MOVE      MSG(109)      MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   END
     C     JMC1          IFEQ      *ZEROS
     C     JMM1          IFNE      418                                          ALVADIA
     C                   SETON                                        67
     C   67              MOVE      MSG(104)      MENSER
     C   67              EXSR      ERRO
     C   67              GOTO      XPTO03
     C                   END
     C                   END
     C                   ELSE
     C     JMC1          IFGT      *ZEROS
     C                   SETON                                        67
     C   67              MOVE      MSG(110)      MENSER
     C   67              EXSR      ERRO
     C   67              GOTO      XPTO03
     C                   END
     C                   END
     C*
     C     JMC1          IFGT      COM1
     C   67              SETON                                        67
     C   67              MOVE      MSG(103)      MENSER
     C   67              EXSR      ERRO
     C   67              GOTO      XPTO03
     C                   END
     C*
     C     JMM2          IFGT      *ZEROS
     C     JMM2          CHAIN     MCORNP                             80
     C   80              SETON                                        65
     C   80              MOVE      MSG(105)      MENSER
     C   80              EXSR      ERRO
     C   80              GOTO      XPTO03
     C     JMC2          IFEQ      *ZEROS
     C     JMM2          IFNE      418                                          ALVADIA
     C                   SETON                                        65
     C   65              MOVE      MSG(107)      MENSER
     C   65              EXSR      ERRO
     C   65              GOTO      XPTO03
     C                   END
     C                   END
     C                   ELSE
     C     JMC2          IFGT      *ZEROS
     C                   SETON                                        65
     C   65              MOVE      MSG(111)      MENSER
     C   65              EXSR      ERRO
     C   65              GOTO      XPTO03
     C                   END
     C                   END
     C*
     C     JMC2          IFGT      COM1
     C   65              SETON                                        65
     C   65              MOVE      MSG(106)      MENSER
     C   65              EXSR      ERRO
     C   65              GOTO      XPTO03
     C                   END
     C*
     C     JMC1          ADD       JMC2          WCOM              5 3
     C     WCOM          IFGT      COM1
     C                   SETON                                        6567
     C   65              MOVE      MSG(108)      MENSER
     C   65              EXSR      ERRO
     C   65              GOTO      XPTO03
     C                   END
     C*
     C                   Z-ADD     0             TOTREC
     C                   MOVE      9999999       APOL
     C     RRAPOL        SETLL     REGAPOL
     C                   READP     REGAPOL                                92
     C   92              MOVE      MSG(66)       MENSER
     C   92              EXSR      ERRO
     C   92              GOTO      XPTO03
     C*
     C     RR            IFNE      RRR
     C                   MOVE      MSG(66)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO03
     C                   END
     C*
     C                   EXSR      BEGS05
     C     EVNOVO        IFEQ      *ZEROS
     C                   Z-ADD     *ZEROS        ECAPDI
     C                   END
     C     ECAPDI        IFEQ      *ZEROS
     C                   Z-ADD     ECAPDP        ECAPDI
     C                   END
     C     RAPOL         ADD       1             APOL
     C                   Z-ADD     ECAPDP        CAPDP
     C                   Z-ADD     ECAPDI        CAPDPI
     C                   Z-ADD     EVNOVO        VLNOVO
     C                   Z-ADD     EVART5        VLART5
     C                   Z-ADD     AC1X          ANOCON            4 0
     C                   EXCEPT    GBAADD
     C                   EXSR      SRALT                                        -CONTROLE
     C   35              DO                                                      SINISTROS
     C                   EXCEPT    RAADD
     C     ADC           IFNE      WADC
     C                   EXSR      VALADC
     C                   ELSE
     C     NIB           IFNE      WNIB
     C                   EXSR      VALADC
     C                   END
     C                   END
     C                   MOVE      MSG(67)       WMSG
     C                   MOVE      RR            RM
     C                   MOVE      APOL          AP
     C                   MOVE      WMSG          MENSER
     C                   EXSR      ERRO
     C*
     C                   Z-ADD     UYEAR         ANO
     C                   MOVE      RRAPOL        RA
     C                   MOVE      *BLANK        ARG
     C                   MOVE      '1'           TA
     C                   EXCEPT    ADDR
     C*
     C                   MOVE      *BLANK        ARG
     C                   MOVE      '2'           TA
     C                   MOVEL     NSEG          ARG
     C                   EXCEPT    ADDR
     C*
     C                   MOVE      *BLANK        ARG
     C                   MOVE      '3'           TA
     C                   MOVEL     DUR           ARG
     C                   EXCEPT    ADDR
     C*
     C                   MOVE      *BLANK        ARG
     C                   MOVE      '4'           TA
     C                   MOVE      UDAY          MMDD              4 0
     C                   MOVEL     UMONTH        MMDD
     C                   MOVEL     MMDD          ARG
     C                   EXCEPT    ADDR
     C*
     C                   MOVE      *BLANK        ARG
     C                   MOVE      '5'           TA
     C                   MOVEL     MATR1         ARG
     C                   EXCEPT    ADDR
     C                   END
     C*
     C     FIM1          TAG
     C                   EXSR      BEGS01
     C                   GOTO      XPTO01
     C*************************************************************************
     C     XPTO04        TAG
     C   36              EXCEPT    OUTP05
     C   36              GOTO      XPTO05
     C*
     C   37              EXCEPT    OUTP05
     C*
     C     XPTO05        TAG
     C     NSEG          CHAIN     GFSEG                              10
     C  N10              DO
     C     JMM1          IFEQ      272
     C     PILOTO        COMP      'S'                                1010
     C                   ELSE
     C     CORSEL        IFEQ      'S'
     C                   SETON                                        10
     C                   ELSE
     C     STESP         COMP      ' '                                    10
     C  N10STESP         COMP      'N'                                    10
     C                   END
     C                   END
     C                   END
      *
     C                   MOVE      *BLANKS       MENSER
     C                   MOVE      'D'           INDREG
     C                   EVAL      Func#Key = *BLANKS
+----C                   DOW       (Func#Key<>Key#F03) AND (Func#Key<>Key#F12)
|    C*                  DOW       (Func#Key <> KEY#F07)
|    C                   EXCEPT    OUTP04
|    C                   READ      ECRAN                                  92
|+---C                   IF        (Func#Key = Key#F04)
||   C                   ExSr      RunPopUp
||   C                   ITER
|+---C                   ELSEIF    (Func#Key = KEY#F05)
||   C                   EXSR      Opcoes
||   C                   ITER
|+---C                   ELSE
||   C                   LEAVE
|+---C                   ENDIF
+----C                   ENDDO
     C*********************************************************
     C* INICIALIZA INDICAD. PARA POSSIBILITAR VALIDAÇÕES      *
     C                   SETOFF                                       020712       *
     C                   SETOFF                                       131718       *
     C                   SETOFF                                       262728       *
     C                   SETOFF                                       293132       *
     C                   SETOFF                                       334555       *
     C                   SETOFF                                       565758       *
     C                   SETOFF                                       656667       *
     C                   SETOFF                                       727374       *
     C                   SETOFF                                       75           *
     C                   SETOFF                                       838485       *
     C                   SETOFF                                       868995       *
     C                   SETOFF                                       919394       *
     C*********************************************************
     C                   MOVEL     AAALTX        WDTALT            8 0
     C                   MOVEL     MMALT         FLD4              4 0
     C                   MOVE      DDALT         FLD4
     C                   MOVE      FLD4          WDTALT
     C*
     C                   MOVEL     AAINV         WDTINV            8 0
     C                   MOVEL     MMINVG        FLD4              4 0
     C                   MOVE      DDINVG        FLD4
     C                   MOVE      FLD4          WDTINV
     C*
     C                   MOVEL     AAINP         WDTINP            8 0
     C                   MOVEL     MMINPJ        FLD4              4 0
     C                   MOVE      DDINPJ        FLD4
     C                   MOVE      FLD4          WDTINP
     C**************************************************************************
     C* F5-ver sinistros da apol;  F12-ver doc memo da apol               asilva
     C* Desactivado por Luis Galhoz
     C**************************************************************************
     C*   KE                                                                                   asilv
     C*OR KL              MOVE      RRAPOL        #APOL             9 0                        asilv
     C*  KE              SETON                                        LRU5                     asilv
     C*  KL              SETON                                        LRU1U2                   asilv
     C*  KE                                                                                    asilv
     C*OR KL              GOTO      ACABLR                                                     asilv
     C**************************************************************************
+----C                   IF        (Func#Key=Key#F03) OR (Func#Key=Key#F12)
|    C*                  IF        (Func#Key = Key#F07)
|    C                             OR (*IN36)
|    C
|    C                   GOTO      FIM2
+----C                   ENDIF
      * DESCRIÇÃO POSTAL
     C     CPLCB1        IFNE      0
     C     CPLCB1        CHAIN     MPOSTNP                            95
     C  N95              MOVEL     DESCR         CPLCB2
     C   95              MOVEL     *BLANKS       CPLCB2
      *
     C   95              MOVE      MSG(114)      MENSER
     C   95              EXSR      ERRO
     C   95              GOTO      XPTO05
     C                   ELSE
     C                   MOVEL     *BLANKS       CPLCB2
     C                   END
     C*
     C     IM            COMP      0                                    53
     C  N53IM            COMP      1                                  53
     C   53              MOVE      MSG(06)       MENSER           60
     C   53              EXSR      ERRO
     C   53              GOTO      XPTO04
     C*
     C     CS            COMP      0                                    18
     C  N18CS            COMP      2                                  18
     C   18              MOVE      MSG(119)      MENSER
     C   18              EXSR      ERRO
     C   18              GOTO      XPTO04
     C*
     C     SIT           COMP      1                                    54
     C  N54SIT           COMP      4                                  54
     C   54              MOVE      MSG(07)       MENSER           60
     C   54              EXSR      ERRO
     C   54              GOTO      XPTO04
     C*
     C     DDINIC        COMP      1                                    55
     C  N55DDINIC        COMP      31                                 55
     C   55              MOVE      MSG(08)       MENSER           60
     C   55              EXSR      ERRO
     C   55              GOTO      XPTO04
     C*
     C     MMINIC        COMP      1                                    56
     C  N56MMINIC        COMP      12                                 56
     C   56              MOVE      MSG(08)       MENSER           60
     C   56              EXSR      ERRO
     C   56              GOTO      XPTO04
     C*
     C     AAINI         COMP      1950                                 57
     C  N57AAINI         COMP      AAIP1                              57
     C   57              MOVE      MSG(08)       MENSER           60
     C   57              EXSR      ERRO
     C   57              GOTO      XPTO04
     C*
     C     DUR           COMP      1                                    30
     C  N30DUR           COMP      2                                  30
     C   30              MOVE      MSG(09)       MENSER           60
     C   30              EXSR      ERRO
     C   30              GOTO      XPTO04
     C*
     C                   EXSR      VDUR
     C   30              MOVE      MSG(09)       MENSER           60
     C   30              EXSR      ERRO
     C   30              GOTO      XPTO04
     C*
     C     DDFIM         COMP      0                                    89
     C  N89DDFIM         COMP      31                                 89
     C   89              MOVE      MSG(08)       MENSER           60
     C   89              EXSR      ERRO
     C   89              GOTO      XPTO04
     C*
     C     MMFIM         COMP      0                                    17
     C  N17MMFIM         COMP      12                                 17
     C   17              MOVE      MSG(08)       MENSER           60
     C   17              EXSR      ERRO
     C   17              GOTO      XPTO04
     C*
     C     AAFI          COMP      0                                    31
     C  N31AAFI          COMP      AAIP2                              31
     C   31              MOVE      MSG(08)       MENSER           60
     C   31              EXSR      ERRO
     C   31              GOTO      XPTO04
     C*
     C                   EXSR      VDTFIM
     C   31              SETON                                        8917
     C   31              MOVE      MSG(08)       MENSER           60
     C   31              EXSR      ERRO
     C   31              GOTO      XPTO04
     C*
     C     WDTFIM        IFNE      0
     C     DATINI        IFGT      WDTFIM
     C                   SETON                                        555657
     C                   MOVE      MSG(08)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   END
     C*
     C                   EXSR      VFRAC
     C   32              MOVE      MSG(10)       MENSER           60
     C   32              EXSR      ERRO
     C   32              GOTO      XPTO04
     C*
     C                   EXSR      VMES13
     C   33              MOVE      MSG(11)       MENSER           60
     C   33              EXSR      ERRO
     C   33              GOTO      XPTO04
     C*
     C                   EXSR      VMCONT
     C                   SETOFF                                       11
     C   34              MOVE      MSG(12)       MENSER           60
     C   34              EXSR      ERRO
     C   34              GOTO      XPTO04
     C*
     C     AACON         COMP      AAIPL                                34
     C  N34AACON         COMP      9999                               34
     C   34              MOVE      MSG(12)       MENSER           60
     C   34              EXSR      ERRO
     C   34              GOTO      XPTO04
     C*
     C     CEMIS         COMP      1                                    19
     C  N19CEMIS         COMP      2                                  19
     C   19              MOVE      MSG(13)       MENSER           60
     C   19              EXSR      ERRO
     C   19              GOTO      XPTO04
     C*
     C     ISEN          COMP      0                                    20
     C  N20ISEN          COMP      7                                  20
     C   20              MOVE      MSG(14)       MENSER           60
     C   20              EXSR      ERRO
     C   20              GOTO      XPTO04
      *
     C     AVISN         COMP      'S'                                0404
     C   04AVISN         COMP      'N'                                0404
     C   04              MOVE      MSG(120)      MENSER
     C   04              EXSR      ERRO
     C   04              GOTO      XPTO04
     C*
     C     VALMAT        COMP      'S'                                0505
     C   05VALMAT        COMP      'N'                                0505
     C   05              MOVE      MSG(121)      MENSER
     C   05              EXSR      ERRO
     C   05              GOTO      XPTO04
     C* validação do combustível
     C     COMB          IFNE      'B'                                          biodiesel
     C     COMB          IFNE      'D'                                          diesel
     C     COMB          IFNE      'E'                                          eléctrico
     C     COMB          IFNE      'G'                                          gasolina
     C     COMB          IFNE      'H'                                          hibrido
     C     COMB          IFNE      'L'                                          gpl
     C                   MOVE      MSG(122)      MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C* validação da ADC
     C     ADC           IFNE      WADC
     C                   EXSR      VALADC
     C     OUMSG         IFNE      *BLANKS
     C                   MOVEL     OUMSG         MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   END
     C* validação do NIB
     C     NIB           IFNE      WNIB
     C                   EXSR      VALADC
     C     OUMSG         IFNE      *BLANKS
     C                   MOVEL     OUMSG         MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   END
     C* validação Seguro Novo com ADC e NIB preenchido
     C*          TALT      IFEQ *ZERO                      seguro novo
     C*          ADC       IFNE *ZERO
     C*                    MOVE MSG,126   MENSER
     C*                    EXSR ERRO
     C*                    GOTO XPTO04
     C*                    END
     C*                    END
     C* se ADC e NIB preenchidos o AVISN têm que ser S
     C     ADC           IFNE      *ZERO
     C     NIB           IFNE      *BLANK
     C                   MOVE      'S'           AVISN
     C                   END
     C                   END
     C* validação do codigo empregado
     C     EMPREG        IFNE      'S'
     C     EMPREG        IFNE      ' '
     C                   MOVE      MSG(125)      MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   END
     C*
     C                   EXSR      VCONC
     C   06              MOVE      MSG(15)       MENSER           60
     C   06              EXSR      ERRO
     C   06              GOTO      XPTO04
     C*
     C                   EXSR      VMED1
     C   65
     COR 66              EXSR      ERRO
     C   65
     COR 66              GOTO      XPTO04
     C   07              MOVE      MSG(16)       MENSER           60
     C   07              EXSR      ERRO
     C   07              GOTO      XPTO04
     C*
     C     TALT          IFEQ      0
     C     RR            LOOKUP    TABRR         TABPC                    80
     C   80              MOVE      TABPC         COMISS
      *
     C     ISEN          IFEQ      1
     C     CORR1         MULT(H)   1.20          CORR              5 3
     C     ANGA1         MULT(H)   1.20          ANGA              5 3
     C     COBR1         MULT(H)   1.20          COBR              5 3
     C                   ELSE
     C                   Z-ADD     CORR1         CORR
     C                   Z-ADD     ANGA1         ANGA
     C                   Z-ADD     COBR1         COBR
     C                   END
     C                   Z-ADD     CORR          COM1
     C*
     C     JMM1          IFGT      *ZEROS
     C     JMM2          IFEQ      0
     C     ANGA          ADD       COBR          JMC1
     C                   END
     C                   ELSE
     C     JMM2          IFGT      *ZEROS
     C                   Z-ADD     ANGA          JMC2
     C                   END
     C                   END
     C*
     C     MED1          IFEQ      906                                          MARSH
     C                   ADD       3.00          COM1
     C                   ELSE
     C     JMM2          IFEQ      418                                          JOÃO ALVADIA
     C                   Z-ADD     0             JMC2
     C                   ELSE
     C     JMM2          IFEQ      443                                          FERNANDO SILVA
     C     ANGA          ADD       COBR          JMC2
     C                   ELSE
     C     JMM2          IFEQ      447                                          VERA SANTOS
     C     ANGA          ADD       COBR          JMC2
     C                   ELSE
     C     JMM2          IFEQ      801                                          JORGE DIAZ
     C     ANGA          ADD       COBR          JMC2
     C                   ELSE
     C     JMM1          IFEQ      001                                          JM-CLUBE TAP
     C                   Z-ADD     CORR          JMC1
     C                   ELSE
     C     JMM1          IFEQ      826                                          ANTRALMED
     C                   Z-ADD     CORR          JMC1
     C                   ELSE
     C     JMM1          IFEQ      827                                          EUROPATRIMONIAL
     C     JMM2          IFEQ      0
     C                   Z-ADD     CORR          JMC1
     C                   END
     C                   ELSE
     C     JMM1          IFEQ      811                                          POLINSUR
     C     JMM2          IFEQ      0
     C                   Z-ADD     CORR          JMC1
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C*
     C     COM1          COMP      0                                    6464
     C   64              MOVE      MSG(17)       MENSER           60
     C   64              EXSR      ERRO
     C   64              GOTO      XPTO04
     C*
     C                   EXSR      VMED2
     C   02              MOVE      MSG(18)       MENSER           60
     C   02              EXSR      ERRO
     C   02              GOTO      XPTO04
     C*
     C                   SETOFF                                       58
     C     CRHIP         IFGT      *ZEROS
     C                   MOVEL     20            K7                7 0
     C                   MOVE      CRHIP         K7
     C     K7            CHAIN     ENTIDADE                           58
     C   58              MOVE      MSG(112)      MENSER
     C   58              EXSR      ERRO
     C   58              GOTO      XPTO04
     C                   END
     C*
     C     TALT          COMP      0                                    49
     C  N49TALT          COMP      9                                  49
     C   49              MOVE      MSG(37)       MENSER           60
     C   49              EXSR      ERRO
     C   49              GOTO      XPTO04
     C*
     C                   EXSR      VTALT
     C   49              MOVE      MSG(37)       MENSER           60
     C   49              EXSR      ERRO
     C   49              GOTO      XPTO04
     C*
     C*          FLAG      IFEQ 'ALTE'
     C*          TALT      IFEQ 0
     C*                    SETON                       49
     C*                    MOVE MSG,37    MENSER
     C*                    EXSR ERRO
     C*                    GOTO XPTO04
     C*                    END
     C*                    END
     C*
     C     TALT          IFEQ      0
     C                   SETOFF                                       232425
     C                   GOTO      NOVO
     C                   END
     C*
     C     DDALT         COMP      0                                    2323
     C  N23DDALT         COMP      31                                 23
     C   23              MOVE      MSG(08)       MENSER           60
     C   23              EXSR      ERRO
     C   23              GOTO      XPTO04
     C*
     C     MMALT         COMP      0                                    2424
     C  N24MMALT         COMP      12                                 24
     C   24              MOVE      MSG(08)       MENSER           60
     C   24              EXSR      ERRO
     C   24              GOTO      XPTO04
     C*
     C     AAALTX        COMP      0                                    2525
     C  N25AAALTX        COMP      AAIP1                              25
     C   25              MOVE      MSG(08)       MENSER           60
     C   25              EXSR      ERRO
     C   25              GOTO      XPTO04
     C*
     C     DTALT         IFGT      0
     C     AAALTX        COMP      0                                      25
     C   25              MOVE      MSG(08)       MENSER           60
     C   25              EXSR      ERRO
     C   25              GOTO      XPTO04
     C                   END
     C*
     C*                    Z-ADDUDAY      WDD
     C*                    Z-ADDUMONTH    WMM
     C*          AAIPL     SUB  1         WAAIPL  40
     C*                    MOVELWAAIPL    WDATA
     C*          WDTALT    IFLT WDATA
     C*                    SETON                     232425
     C*                    MOVE MSG,08    MENSER
     C*                    EXSR ERRO
     C*                    GOTO XPTO04
     C*                    END
     C*
     C                   EXSR      VDALT
     C   25              MOVE      MSG(08)       MENSER           60
     C   25              EXSR      ERRO
     C   25              GOTO      XPTO04
     C*
     C     NOVO          TAG
     C*
     C     TALT          IFNE      0
     C     WDTALT        IFLT      DATINI
     C                   SETON                                        232425
     C                   MOVE      MSG(08)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   END
     C*
     C     MALT          COMP      0                                    52
     C* N52      MALT      COMP 70                   52    02.05.2002
     C  N52MALT          COMP      99                                 52        02.05.2002
     C   52              MOVE      MSG(38)       MENSER           60
     C   52              EXSR      ERRO
     C   52              GOTO      XPTO04
     C*
     C     DSCESP        COMP      0                                    90
     C   90              MOVE      MSG(72)       MENSER           60
     C   90              EXSR      ERRO
     C   90              GOTO      XPTO04
     C*
     C   50              DO
     C     CR1           COMP      2                                  3838
     C   38              MOVE      MSG(20)       MENSER           60
     C   38              EXSR      ERRO
     C   38              GOTO      XPTO04
     C*
     C*          TALT      IFEQ *ZERO
     C*          CR2       IFEQ *ZEROS
     C*          QIV       IFEQ 'S'
     C*          MED1      IFNE 031                        MORTÁGUA
     C*          MED1      IFNE 921                        CRE
     C*          JMM1      IFNE 272                        CORSEL
     C*                    SETON                     73
     C*                    MOVE MSG,99    MENSER
     C*                    EXSR ERRO
     C*                    GOTO XPTO04
     C*                    END
     C*                    END
     C*                    END
     C*                    END
     C*                    END
     C*                    END
     C*
     C                   END
     C*
     C   51              DO
     C     CR1           COMP      1                                  3838
     C   38              MOVE      MSG(20)       MENSER           60
     C   38              EXSR      ERRO
     C   38              GOTO      XPTO04
     C                   END
     C*
     C                   EXSR      SCAPRC
     C   08              MOVE      MSG(21)       MENSER           60
     C   08              EXSR      ERRO
     C   08              GOTO      XPTO04
     C*
     C     WDTALT        IFEQ      0
     C                   Z-ADD     DATINI        QDTALT            8 0
     C                   ELSE
     C                   Z-ADD     WDTALT        QDTALT
     C                   END
     C*
     C     CAPRC         IFLT      3250
     C     QDTALT        IFGE      20091201
     C                   SETON                                        08
     C                   MOVE      MSG(21)       MENSER           60
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   END
     C*
     C     CAPRC         IFGE      3250
     C     CAPRC         IFNE      99999
     C     QDTALT        IFLT      20091201
     C                   SETON                                        08
     C                   MOVE      MSG(21)       MENSER           60
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   END
     C                   END
     C*
     C****       USER      IFNE 'AFONSO'
     C   50CR2           IFGT      *ZERO
     C                   MOVE      DDMATX        DDMAT
     C                   MOVE      MMMATX        MMMAT
     C                   MOVE      AAMATX        AAMAT
     C                   END
     C****                 END
     C*
     C                   EXSR      VCR2
     C   39              MOVE      MSG(22)       MENSER           60
     C   39              EXSR      ERRO
     C   39              GOTO      XPTO04
     C*
     C                   SETOFF                                       09
     C   50VEXTRA        COMP      0                                    09
     C   50
     CAN 09              MOVE      MSG(75)       MENSER
     C   50
     CAN 09              EXSR      ERRO
     C   50
     CAN 09              GOTO      XPTO04
     C*
     C                   SETOFF                                       11
     C     CODFRQ        LOOKUP    TABFRQ                                 80
     C  N80              SETON                                        11
     C   11              MOVE      MSG(76)       MENSER
     C   11              EXSR      ERRO
     C   11              GOTO      XPTO04
     C*
     C**         CODFRQ    IFEQ 09
     C**         ECAPDI    COMP 50000                11
     C** 11                MOVE MSG,130   MENSER
     C** 11                EXSR ERRO
     C** 11                GOTO XPTO04
     C**                   END
     C*
     C     CODFRQ        IFEQ      09
     C     CATREB        IFEQ      0
     C     CATV          COMP      2                                  11
     C   11CATV          COMP      6                                    11
     C   11              MOVE      MSG(131)      MENSER
     C   11              EXSR      ERRO
     C   11              GOTO      XPTO04
     C                   END
     C                   END
     C*
     C     CODFRQ        IFEQ      09
     C                   EXSR      ANTIG
     C     #MESES        IFGE      96
     C                   SETON                                        11
     C   11              MOVE      MSG(132)      MENSER
     C   11              EXSR      ERRO
     C   11              GOTO      XPTO04
     C                   END
     C                   END
     C*
     C   50              DO
     C                   SETOFF                                       21
     C     CR2           IFGT      *ZEROS
     C     ART5          IFNE      'S'
     C     ART5          IFNE      'N'
     C                   SETON                                        21
     C   21              MOVE      MSG(78)       MENSER
     C   21              EXSR      ERRO
     C   21              GOTO      XPTO04
     C                   END
     C                   END
     C                   END
      *
     C                   EXSR      SRPU
     C  N80              Z-ADD     TVALOR        PRPRIV
     C   80              DO
     C                   MOVE      *BLANKS       PRPRIV
     C                   SETON                                        26
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
      *
     C     CR2           IFEQ      *ZERO
     C     PRIV          IFNE      ' '
     C                   MOVE      *BLANKS       PRPRIV
     C                   SETON                                        26
     C                   MOVE      MSG(116)      MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C     ISEN          IFEQ      1
     C     PRPRIV        DIV(H)    1.20          PRPRIV
     C                   END
     C                   END
     C*
     C                   SETOFF                                       73
     C     QIV           IFNE      'S'
     C     QIV           IFNE      'N'
     C                   SETON                                        73
     C                   MOVE      MSG(99)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   END
     C*
     C                   SETOFF                                       73
     C     QIV           IFEQ      'S'
     C     CR2           IFGT      0
     C     CR2           IFEQ      3
     C                   SETON                                        73
     C                   MOVE      MSG(99)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   ELSE
     C     CR2           IFEQ      4
     C                   SETON                                        73
     C                   MOVE      MSG(99)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   ELSE
     C     CR2           IFEQ      8
     C                   SETON                                        73
     C                   MOVE      MSG(99)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C*
     C     QIV           IFEQ      'S'
     C** ATÉ 31.07.2002    Z-ADD33.32     PRQIV
     C     TALT          IFEQ      0
     C                   Z-ADD     37.76         PRQIV
     C                   END
     C     TALT          IFNE      0
     C********             Z-ADDQIVAGR    PRQIV
     C     PRQIV         IFEQ      *ZEROS
     C                   Z-ADD     37.76         PRQIV
     C                   END
     C                   END
     C     ISEN          IFEQ      1
     C     PRQIV         DIV(H)    1.20          PRQIV
     C                   END
     C                   ELSE
     C                   Z-ADD     *ZEROS        PRQIV
     C                   END
     C                   END
     C*
     C                   SETOFF                                       22
     C   50              DO
     C     CR2           IFGT      *ZEROS
     C     ART5          IFEQ      'S'
     C     EVART5        IFLE      *ZEROS
     C                   SETON                                        22
     C   22              MOVE      MSG(79)       MENSER
     C   22              EXSR      ERRO
     C   22              GOTO      XPTO04
     C                   END
     C                   Z-ADD     EVART5        ECAPDP
     C                   Z-ADD     EVART5        ECAPDI
     C                   END
     C                   END
     C*
     C                   SETOFF                                       13
     C     CR2           IFGT      *ZEROS
     C     ART5          IFEQ      'N'
     C     EVNOVO        IFLE      *ZEROS
     C                   SETON                                        13
     C   13              MOVE      MSG(77)       MENSER
     C   13              EXSR      ERRO
     C   13              GOTO      XPTO04
     C                   END
     C                   END
     C                   END
     C*
     C     CR3           COMP      0                                    27
     C  N27CR3           COMP      3                                  27
     C   27              MOVE      MSG(22)       MENSER
     C   27              EXSR      ERRO
     C   27              GOTO      XPTO04
     C                   END
     C*
     C     MARC1         COMP      *BLANK                                 97
     C   97              MOVE      MSG(44)       MENSER
     C   97              EXSR      ERRO
     C   97              GOTO      XPTO04
     C*
     C                   EXSR      VMATR
     C   98              MOVE      MSG(45)       MENSER
     C   98              EXSR      ERRO
     C   98              GOTO      XPTO04
     C*
     C     DTMATX        COMP      *ZEROS                                 12
     C   12              MOVE      MSG(08)       MENSER
     C   12              EXSR      ERRO
     C   12              GOTO      XPTO04
     C     AAMATX        COMP      *ZEROS                                 12
     C  N12AAMATX        COMP      AAIPL                              12
     C   12              MOVE      MSG(08)       MENSER
     C   12              EXSR      ERRO
     C   12              GOTO      XPTO04
     C     MMMATX        LOOKUP    TABMES        TABDIA                   80
     C  N80              SETON                                          12
     C   12              MOVE      MSG(08)       MENSER
     C   12              EXSR      ERRO
     C   12              GOTO      XPTO04
     C     DDMATX        COMP      *ZEROS                                 12
     C   12              MOVE      MSG(08)       MENSER
     C   12              EXSR      ERRO
     C   12              GOTO      XPTO04
     C     DDMATX        COMP      TABDIA                             12
     C   12              MOVE      MSG(08)       MENSER
     C   12              EXSR      ERRO
     C   12              GOTO      XPTO04
     C*
     C     NLUG1         COMP      *ZERO                                9999
     C   99              MOVE      MSG(46)       MENSER
     C   99              EXSR      ERRO
     C   99              GOTO      XPTO04
     C*
     C                   SETOFF                                       95
     C     CILIND        IFEQ      0
     C                   SETON                                        95
     C                   MOVE      MSG(127)      MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C*
     C     AC1X          COMP      *ZEROS                                 68
     C  N68AC1X          COMP      AAIPL                              68
     C  N68AC1X          COMP      AAMATX                             68
     C   68              MOVE      MSG(26)       MENSER           60
     C   68              EXSR      ERRO
     C   68              GOTO      XPTO04
     C*
     C     CL            COMP      0                                    69
     C  N69CL            COMP      5                                  69
     C   69              MOVE      MSG(27)       MENSER           60
     C   69              EXSR      ERRO
     C   69              GOTO      XPTO04
     C*
     C     PB            COMP      0                                    70
     C  N70PB            COMP      6                                  70
     C   70              MOVE      MSG(28)       MENSER           60
     C   70              EXSR      ERRO
     C   70              GOTO      XPTO04
     C*
     C     CATV          COMP      1                                    71
     C  N71CATV          COMP      7                                  71
     C   71              MOVE      MSG(29)       MENSER           60
     C   71              EXSR      ERRO
     C   71              GOTO      XPTO04
     C*
     C                   MOVEL     '1000'        W4                4 0
     C     WCATV         IFGE      501
     C     WCATV         IFLE      504
     C                   MOVEL     '2000'        W4
     C                   END
     C                   END
     C*-------------------------------------------------------
     C* A PARTIR DE 01.03.99 A CATV-150 FOI EXTINTA PARA R.C.
     C* EXISTEM SÓ 2 CATV 130/140, ATÉ 1500CC +1500CC
     C                   MOVEL     AAINI         WRK6              6 0
     C                   MOVE      MMINIC        WRK6
     C                   MOVEL     WRK6          WRK8              8 0
     C                   MOVE      DDINIC        WRK8
     C     TALT          IFEQ      0
     C     WRK8          IFGE      19990301
     C     WCATV         IFEQ      150
     C                   MOVE      140           WCATV
     C                   END
     C                   END
     C                   ELSE
     C     WDTALT        IFGE      19990301
     C     WCATV         IFEQ      150
     C                   MOVE      140           WCATV
     C                   END
     C                   END
     C                   END
     C*
     C     WCATV         IFNE      500                                          <------------
     C                   MOVE      WCATV         W4                                         |
     C     TALT          IFEQ      0                                            <---------- |
     C                   EXSR      SRCALL                                                 |
     C     WDESC         COMP      *BLANKS                                71              |
     C                   ELSE                                                             * |
     C     FRAC          IFGE      1                                            <-------- | |
     C                   EXSR      SRCAL2                                               | |
     C     WDESC         COMP      *BLANKS                                71            | |
     C                   END                                                    <-------- | |
     C                   END                                                    <---------- |
     C   71              MOVE      MSG(29)       MENSER           60                        |
     C   71              EXSR      ERRO                                                     |
     C   71              GOTO      XPTO04                                                   |
     C                   END                                                    <------------
     C*
     C                   SETOFF                                       94
     C     CATREB        IFGT      *ZEROS                                       <----------------
     C     MATR2         IFNE      *BLANK                                       <-------------- |
     C                   MOVEL     '2000'        W4                                           | |
     C                   MOVE      CATREB        W4                                           | |
     C     TALT          IFEQ      0                                            <------------ | |
     C                   EXSR      SRCALL                                                 |
     C     WDESC         COMP      *BLANKS                                93              |
     C                   ELSE                                                               * | |
     C     FRAC          IFGE      1                                            <---------- | | |
     C                   EXSR      SRCAL2                                                 |
     C     WDESC         COMP      *BLANKS                                93              |
     C                   END                                                    <---------- | | |
     C                   END                                                    <------------ | |
     C   93              MOVE      MSG(73)       MENSER           60                          | |
     C   93              EXSR      ERRO                                                       | |
     C   93              GOTO      XPTO04                                                     | |
     C                   ELSE                                                                 * |
     C                   SETON                                        94                      | |
     C   94              MOVE      MSG(74)       MENSER           60                          | |
     C   94              EXSR      ERRO                                                       | |
     C   94              GOTO      XPTO04                                                     | |
     C                   END                                                    <-------------- |
     C                   END                                                    <----------------
     C*
     C   50TC            COMP      0                                  4242
     C   51TC            COMP      0                                      42
     C   51
     CANN42TC            COMP      3                                  42
     C   42              MOVE      MSG(30)       MENSER
     C   42              EXSR      ERRO
     C   42              GOTO      XPTO04
     C*
     C                   EXSR      VNASC
     C   43              GOTO      XPTO04
     C*
     C                   EXSR      VCARTA
     C   44              GOTO      XPTO04
     C*
     C     TEST          COMP      0                                    47
     C  N47TEST          COMP      2                                  47
     C   47              MOVE      MSG(39)       MENSER           60
     C   47              EXSR      ERRO
     C   47              GOTO      XPTO04
     C*
     C                   EXSR      VTEST
     C   47              MOVE      MSG(39)       MENSER           60
     C   47              EXSR      ERRO
     C   47              GOTO      XPTO04
     C*
     C     DULSIN        COMP      0                                    59
     C  N59DULSIN        COMP      31                                 59
     C   59              MOVE      MSG(08)       MENSER           60
     C   59              EXSR      ERRO
     C   59              GOTO      XPTO04
     C*
     C     MULSIN        COMP      0                                    60
     C  N60MULSIN        COMP      12                                 60
     C   60              MOVE      MSG(08)       MENSER           60
     C   60              EXSR      ERRO
     C   60              GOTO      XPTO04
     C*
     C     AULSI         COMP      0                                    61
     C  N61AULSI         COMP      AAIPL                              61
     C   61              MOVE      MSG(08)       MENSER           60
     C   61              EXSR      ERRO
     C   61              GOTO      XPTO04
     C*
     C     NSIN          COMP      0                                    62
     C  N62NSIN          COMP      9                                  62
     C   62              MOVE      MSG(40)       MENSER           60
     C   62              EXSR      ERRO
     C   62              GOTO      XPTO04
     C*
     C     AGRAV         COMP      0                                    63
     C  N63AGRAV         COMP      999                                63
     C   63              MOVE      MSG(41)       MENSER           60
     C   63              EXSR      ERRO
     C   63              GOTO      XPTO04
     C*
     C                   SETOFF                                       48
     C   51TB            COMP      0                                  4848
     C   50TB            LOOKUP    TABBON                                 10
     C   50
     CANN10              SETON                                        48
     C   48              MOVE      MSG(35)       MENSER           60
     C   48              EXSR      ERRO
     C   48              GOTO      XPTO04
     C*
     C                   EXSR      VAGRAV
     C   48              MOVE      MSG(41)       MENSER           60
     C   48              EXSR      ERRO
     C   48              GOTO      XPTO04
     C*
     C     CL1           COMP      ' '                                  46
     C  N46CL1           COMP      'K'                                46
     C   46              MOVE      MSG(36)       MENSER           60
     C   46              EXSR      ERRO
     C   46              GOTO      XPTO04
     C*
     C     CL1           COMP      'A'                                    46
     C   46              MOVE      MSG(36)       MENSER           60
     C   46              EXSR      ERRO
     C   46              GOTO      XPTO04
     C*
     C     CL2           COMP      ' '                                  76
     C  N76CL2           COMP      'K'                                76
     C   76              MOVE      MSG(36)       MENSER           60
     C   76              EXSR      ERRO
     C   76              GOTO      XPTO04
     C*
     C     CL3           COMP      ' '                                  77
     C  N77CL3           COMP      'K'                                77
     C   77              MOVE      MSG(36)       MENSER           60
     C   77              EXSR      ERRO
     C   77              GOTO      XPTO04
     C*
     C     CL4           COMP      ' '                                  78
     C  N78CL4           COMP      'K'                                78
     C   78              MOVE      MSG(36)       MENSER           60
     C   78              EXSR      ERRO
     C   78              GOTO      XPTO04
     C*
     C     CL5           COMP      ' '                                  79
     C  N79CL5           COMP      'K'                                79
     C   79              MOVE      MSG(36)       MENSER           60
     C   79              EXSR      ERRO
     C   79              GOTO      XPTO04
     C*
     C     CL6           COMP      ' '                                  88
     C  N88CL6           COMP      'K'                                88
     C   88              MOVE      MSG(36)       MENSER           60
     C   88              EXSR      ERRO
     C   88              GOTO      XPTO04
     C*
     C     CL7           COMP      ' '                                  81
     C  N81CL7           COMP      'K'                                81
     C   81              MOVE      MSG(36)       MENSER           60
     C   81              EXSR      ERRO
     C   81              GOTO      XPTO04
     C*
     C     CL8           COMP      ' '                                  82
     C  N82CL8           COMP      'K'                                82
     C   82              MOVE      MSG(36)       MENSER           60
     C   82              EXSR      ERRO
     C   82              GOTO      XPTO04
     C*
     C                   EXSR      SRAV
     C   80              DO
     C                   SETON                                          28
     C                   MOVE      MSG(47)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C*
     C     VIAG          IFNE      ' '
     C                   MOVEL     CATV          W1CATV            2 0
     C                   MOVE      CL            W1CATV
     C                   MOVEL     W1CATV        W2CATV            3 0
     C                   MOVE      PB            W2CATV
     C     W2CATV        IFGE      300
     C     W2CATV        IFLT      400
     C     VIAG          IFNE      'P'
     C                   SETON                                        28
     C                   MOVE      MSG(49)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   END
     C                   END
     C*
     C     DTINV         IFEQ      *ZEROS
     C                   SETON                                          29
     C                   MOVE      MSG(50)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C*
     C     DTINV         IFGT      *ZEROS
     C     DTEXV         IFGT      *ZEROS
     C                   SETON                                          72
     C                   MOVE      MSG(53)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   END
     C*
     C     DDINVG        IFGT      0
     C     DDINVG        IFLT      32
     C                   ELSE
     C                   SETON                                          29
     C                   MOVE      MSG(08)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   ELSE
     C                   SETON                                          29
     C                   MOVE      MSG(08)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C*
     C     MMINVG        IFGT      0
     C     MMINVG        IFLT      13
     C                   ELSE
     C                   SETON                                          29
     C                   MOVE      MSG(08)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   ELSE
     C                   SETON                                          29
     C                   MOVE      MSG(08)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   END
     C*
     C     VIAG          IFEQ      ' '
     C     DTEXV         IFGT      0
     C     DDEXVG        IFGT      0
     C     DDEXVG        IFLT      31
     C                   ELSE
     C                   SETON                                          72
     C                   MOVE      MSG(08)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   ELSE
     C                   SETON                                          72
     C                   MOVE      MSG(08)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C*
     C     MMEXVG        IFGT      0
     C     MMEXVG        IFLT      13
     C                   ELSE
     C                   SETON                                          72
     C                   MOVE      MSG(08)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   ELSE
     C                   SETON                                          72
     C                   MOVE      MSG(08)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C*
     C     AAEXV         IFGT      AAIPL
     C                   SETON                                          72
     C                   MOVE      MSG(08)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   END
     C                   END
     C*
     C     VIAG          IFEQ      ' '
     C     DTINV         IFGT      *ZEROS
     C     DTEXV         IFEQ      *ZEROS
     C                   SETON                                          72
     C                   MOVE      MSG(51)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   END
     C*
     C                   MOVEL     AAINV         WDTINV            8 0
     C                   MOVEL     MMINVG        FLD4              4 0
     C                   MOVE      DDINVG        FLD4
     C                   MOVE      FLD4          WDTINV
     C*
     C                   MOVEL     AAEXV         WDTEXV            8 0
     C                   MOVEL     MMEXVG        FLD4              4 0
     C                   MOVE      DDEXVG        FLD4
     C                   MOVE      FLD4          WDTEXV
     C*
     C     WDTEXV        IFGE      WDTINV
     C                   ELSE
     C                   SETON                                          72
     C                   MOVE      MSG(52)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   END
     C*
     C     VIAG          IFEQ      ' '
     C     TCALVG        IFGT      0
     C                   SETON                                          75
     C                   MOVE      MSG(54)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   END
     C*
     C     VIAG          IFNE      ' '
     C     TCALVG        IFLT      0
     C                   SETON                                          75
     C                   MOVE      MSG(58)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   ELSE
     C     TCALVG        IFGT      2
     C                   SETON                                          75
     C                   MOVE      MSG(58)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   END
     C*
     C     TALT          IFEQ      0
     C     TCALVG        IFNE      1
     C                   SETON                                          75
     C                   MOVE      MSG(55)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   END
     C*
     C     TALT          IFEQ      1
     C     TCALVG        IFGT      1
     C                   SETON                                          75
     C                   MOVE      MSG(59)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   END
     C*
     C     TALT          IFGT      1
     C     TALT          IFLT      5
     C     TCALVG        IFGT      0
     C                   SETON                                          75
     C                   MOVE      MSG(60)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   END
     C                   END
     C*
     C     TALT          IFEQ      5
     C     TCALVG        IFEQ      0
     C                   ELSE
     C     TCALVG        IFNE      2
     C                   SETON                                          75
     C                   MOVE      MSG(57)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   END
     C                   END
     C*
     C     TALT          IFEQ      6
     C     TCALVG        IFNE      2
     C                   SETON                                          75
     C                   MOVE      MSG(56)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   END
     C*
     C     TALT          IFEQ      7
     C     TCALVG        IFNE      1
     C                   SETON                                          75
     C                   MOVE      MSG(61)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   END
     C                   EXSR      BEGS10
     C                   END
     C*
     C* PROTECÇÃO JURÍDICA
     C     PJUR          IFEQ      'S'
     C                   ELSE
     C     PJUR          IFEQ      ' '
     C                   ELSE
     C                   SETON                                          83
     C                   MOVE      MSG(84)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   END
     C*
     C     PJUR          IFEQ      'S'
     C     DTINPJ        IFEQ      *ZEROS
     C                   SETON                                          84
     C                   MOVE      MSG(87)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   ELSE
     C                   MOVEL     AAINP         WDTINP            8 0
     C                   MOVEL     MMINPJ        FLD4              4 0
     C                   MOVE      DDINPJ        FLD4
     C                   MOVE      FLD4          WDTINP
     C     WDTINP        IFLT      19990301
     C                   SETON                                          84
     C                   MOVE      MSG(86)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   END
     C*
     C     DTINPJ        IFGT      *ZEROS
     C     DTEXPJ        IFGT      *ZEROS
     C                   SETON                                          85
     C                   MOVE      MSG(97)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   END
     C*
     C     DDINPJ        IFGT      0
     C     DDINPJ        IFLT      32
     C                   ELSE
     C                   SETON                                          84
     C                   MOVE      MSG(08)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   ELSE
     C                   SETON                                          84
     C                   MOVE      MSG(08)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C*
     C     MMINPJ        IFGT      0
     C     MMINPJ        IFLT      13
     C                   ELSE
     C                   SETON                                          84
     C                   MOVE      MSG(08)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   ELSE
     C                   SETON                                          84
     C                   MOVE      MSG(08)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   END
     C*
     C     PJUR          IFEQ      ' '
     C     DTEXPJ        IFGT      0
     C     DDEXPJ        IFGT      0
     C     DDEXPJ        IFLT      32
     C                   ELSE
     C                   SETON                                          85
     C                   MOVE      MSG(08)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   ELSE
     C                   SETON                                          85
     C                   MOVE      MSG(08)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C*
     C     MMEXPJ        IFGT      0
     C     MMEXPJ        IFLT      13
     C                   ELSE
     C                   SETON                                          85
     C                   MOVE      MSG(08)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   ELSE
     C                   SETON                                          85
     C                   MOVE      MSG(08)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C*
     C     AAEXP         IFGT      AAIPL
     C                   SETON                                          85
     C                   MOVE      MSG(08)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   END
     C                   END
     C*
     C     PJUR          IFEQ      ' '
     C     DTINPJ        IFGT      *ZEROS
     C     DTEXPJ        IFEQ      *ZEROS
     C                   SETON                                          85
     C                   MOVE      MSG(98)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   END
     C*
     C                   MOVEL     AAINP         WDTINP            8 0
     C                   MOVEL     MMINPJ        FLD4              4 0
     C                   MOVE      DDINPJ        FLD4
     C                   MOVE      FLD4          WDTINP
     C*
     C                   MOVEL     AAEXP         WDTEXP            8 0
     C                   MOVEL     MMEXPJ        FLD4              4 0
     C                   MOVE      DDEXPJ        FLD4
     C                   MOVE      FLD4          WDTEXP
     C*
     C     WDTEXP        IFGE      WDTINP
     C                   ELSE
     C                   SETON                                          85
     C                   MOVE      MSG(52)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   END
     C*
     C     PJUR          IFEQ      ' '
     C     TCALPJ        IFGT      0
     C                   SETON                                          86
     C                   MOVE      MSG(89)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   END
     C*
     C     PJUR          IFEQ      'S'
     C     TCALPJ        IFLT      0
     C                   SETON                                          86
     C                   MOVE      MSG(93)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   ELSE
     C     TCALPJ        IFGT      2
     C                   SETON                                          86
     C                   MOVE      MSG(93)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   END
     C*
     C     TALT          IFEQ      0
     C     TCALPJ        IFNE      1
     C                   SETON                                          86
     C                   MOVE      MSG(90)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   END
     C*
     C     TALT          IFEQ      1
     C     TCALPJ        IFGT      1
     C                   SETON                                          86
     C                   MOVE      MSG(94)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   END
     C*
     C     TALT          IFGT      1
     C     TALT          IFLT      5
     C     TCALPJ        IFGT      0
     C                   SETON                                          86
     C                   MOVE      MSG(95)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   END
     C                   END
     C*
     C     TALT          IFEQ      5
     C     TCALPJ        IFEQ      0
     C                   ELSE
     C     TCALPJ        IFNE      2
     C                   SETON                                          86
     C                   MOVE      MSG(92)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   END
     C                   END
     C*
     C     TALT          IFEQ      8
     C     TCALPJ        IFNE      2
     C                   SETON                                          86
     C                   MOVE      MSG(91)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   END
     C*
     C     TALT          IFEQ      9
     C     TCALPJ        IFNE      1
     C                   SETON                                          86
     C                   MOVE      MSG(96)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   END
     C                   EXSR      BEGS11
     C                   END
     C*-----------------------------------------------------
     C                   SETOFF                                       40
     C     ECAPDP        IFNE      0
     C     ECAPDP        IFGT      80000
     C     USER          IFNE      'MMATA'
     C     USER          IFNE      'EDUARDO'
[LG] C     USER          IFNE      'LGALHOZ'
     C                   SETON                                        40
     C                   MOVE      MSG(113)      MENSER           60
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
[LG] C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C     USER          IFNE      'EDUARDO'
[LG] C     USER          IFNE      'LGALHOZ'
     C     ECAPDP        COMP      225000                             40
     C   40              MOVE      MSG(23)       MENSER           60
     C   40              EXSR      ERRO
     C   40              GOTO      XPTO04
[LG] C                   END
     C                   END
     C*
     C   50              DO                                                     <-------------------
     C     CR2           IFEQ      *ZEROS
     C     ECAPDP        IFNE      *ZEROS
     C                   SETON                                        40
     C                   MOVE      MSG(69)       MENSER           60
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   END
     C                   END                                                    <-------------------
     C*
     C   KA
     CAN 37
     CAN 50              DO
     C                   EXSR      GP700                                        TRATA RC
     C     CR3           IFGT      *ZEROS
     C                   MOVE      2001          ANOTB
     C     K10           CHAIN     TAROCU                             45
     C   45              MOVE      MSG(24)       MENSER           60
     C   45              EXSR      ERRO
     C   45              GOTO      XPTO04
     C     NLUG1         IFGT      6
     C                   Z-ADD     PRC7          PR3
     C                   ELSE
     C                   Z-ADD     PRC6          PR3
     C                   END
     C     ISEN          IFEQ      1
     C     PR3           DIV(H)    1.20          PR3
     C                   END
     C                   END
     C     CR2           IFGT      *ZEROS
     C                   EXSR      GP800                                        TRATA DP
     C   13              MOVE      MSG(77)       MENSER
     C   13              EXSR      ERRO
     C   13              GOTO      XPTO04
     C                   END
     C                   MOVE      MSG(81)       MENSER           60
     C                   GOTO      XPTO04
     C                   END
     C*
     C   KA
     CAN 51              DO
     C                   MOVE      2001          ANOTB
     C                   MOVE      CAPRC         CAP3
     C                   MOVE      TC            CR3
     C     K10           CHAIN     TAROCU                             08
     C                   MOVE      *ZEROS        CAP3
     C                   MOVE      *ZEROS        CR3
     C   08              MOVE      MSG(24)       MENSER
     C   08              EXSR      ERRO
     C   08              GOTO      XPTO04
     C     NLUG1         IFGT      6
     C                   Z-ADD     PRC7          PR1
     C                   ELSE
     C                   Z-ADD     PRC6          PR1
     C                   END
     C     ISEN          IFEQ      1
     C     PR1           DIV(H)    1.20          PR1
     C                   END
     C                   MOVE      1             CR1
     C                   MOVE      2             CR2
     C     CAPRC         MULT      .10           CAPDP
     C                   END
     C*
     C   50              DO                                                     <-------------------
     C     CR2           IFNE      *ZEROS
     C     ECAPDP        IFEQ      *ZEROS
     C                   SETON                                        39
     C                   MOVE      MSG(68)       MENSER           60
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   END
     C                   SETOFF                                       2774
     C     CR3           IFNE      *ZEROS
     C     CATV          IFEQ      4                                             MOTOCICLO
     C                   SETON                                        27
     C                   MOVE      MSG(118)      MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   END
     C     CR3           IFEQ      *ZEROS
     C     CAP3          IFNE      *ZEROS
     C                   SETON                                        27
     C                   MOVE      MSG(22)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C     PR3           IFNE      *ZEROS
     C                   SETON                                        74
     C                   MOVE      MSG(25)       MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C                   END
     C*
     C     ECAPDI        IFGT      80000
     C     USER          IFNE      'MMATA'
     C     USER          IFNE      'EDUARDO'
[LG] C     USER          IFNE      'LGALHOZ'
     C                   MOVE      MSG(113)      MENSER           60
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
[LG] C                   END
     C                   END
     C                   END
     C                   END
     C                   END                                                    <-------------------
     C*
     C     PR1           COMP      *ZEROS                               9696
     C   96              MOVE      MSG(43)       MENSER
     C   96              EXSR      ERRO
     C   96              GOTO      XPTO04
     C*
     C     PR1           ADD       PR2           PRTOT
     C                   ADD       PR3           PRTOT
     C     PRS           COMP      PRTOT                              4141
     C   41              MOVE      MSG(25)       MENSER           60
     C   41              EXSR      ERRO
     C   41              GOTO      XPTO04
     C*
     C   50              DO
     C     CAPRE         COMP      0                                  9191
     C   91              Z-ADD     1             INCAP
     C  N91              Z-ADD     0             INCAP
     C     DTCRE         COMP      0                                  9191
     C   91              Z-ADD     1             INDAT
     C  N91              Z-ADD     0             INDAT
     C     INCAP         IFNE      INDAT
     C                   SETON                                        91
     C                   MOVE      MSG(117)      MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   ELSE
     C                   SETOFF                                       91
     C                   END
     C                   END
     C*
     C     JMM1          IFGT      *ZEROS
     C     JMM1          CHAIN     MCORNP                             80
     C   80              SETON                                        66
     C   80              MOVE      MSG(102)      MENSER
     C   80              EXSR      ERRO
     C   80              GOTO      XPTO04
     C     TMEDCR        IFEQ      2
     C                   SETON                                        66
     C                   MOVE      MSG(109)      MENSER
     C                   EXSR      ERRO
     C                   GOTO      XPTO04
     C                   END
     C     JMC1          IFEQ      *ZEROS
     C     JMM1          IFNE      418                                          ALVADIA
     C                   SETON                                        67
     C   67              MOVE      MSG(104)      MENSER
     C   67              EXSR      ERRO
     C   67              GOTO      XPTO04
     C                   END
     C                   END
     C                   ELSE
     C     JMC1          IFGT      *ZEROS
     C                   SETON                                        67
     C   67              MOVE      MSG(110)      MENSER
     C   67              EXSR      ERRO
     C   67              GOTO      XPTO04
     C                   END
     C                   END
     C*
     C     JMC1          IFGT      COM1
     C   67              SETON                                        67
     C   67              MOVE      MSG(103)      MENSER
     C   67              EXSR      ERRO
     C   67              GOTO      XPTO04
     C                   END
     C*
     C     JMM2          IFGT      *ZEROS
     C     JMM2          CHAIN     MCORNP                             80
     C   80              SETON                                        65
     C   80              MOVE      MSG(105)      MENSER
     C   80              EXSR      ERRO
     C   80              GOTO      XPTO04
     C     JMC2          IFEQ      *ZEROS
     C     JMM2          IFNE      418                                          ALVADIA
     C                   SETON                                        65
     C   65              MOVE      MSG(107)      MENSER
     C   65              EXSR      ERRO
     C   65              GOTO      XPTO04
     C                   END
     C                   END
     C                   ELSE
     C     JMC2          IFGT      *ZEROS
     C                   SETON                                        65
     C   65              MOVE      MSG(111)      MENSER
     C   65              EXSR      ERRO
     C   65              GOTO      XPTO04
     C                   END
     C                   END
     C*
     C     JMC2          IFGT      COM1
     C   65              SETON                                        65
     C   65              MOVE      MSG(106)      MENSER
     C   65              EXSR      ERRO
     C   65              GOTO      XPTO04
     C                   END
     C*
     C     JMC1          ADD       JMC2          WCOM              5 3
     C     WCOM          IFGT      COM1
     C                   SETON                                        6567
     C   65              MOVE      MSG(108)      MENSER
     C   65              EXSR      ERRO
     C   65              GOTO      XPTO04
     C                   END
     C*
     C   37              EXSR      BEGS05
     C   37EVNOVO        IFEQ      *ZEROS
     C                   Z-ADD     *ZEROS        ECAPDI
     C                   END
     C   37ECAPDI        IFEQ      *ZEROS
     C                   Z-ADD     ECAPDP        ECAPDI
     C                   END
     C                   Z-ADD     ECAPDP        CAPDP
     C                   Z-ADD     ECAPDI        CAPDPI
     C                   Z-ADD     EVNOVO        VLNOVO
     C                   Z-ADD     EVART5        VLART5
     C                   Z-ADD     AC1X          ANOCON
     C   37
     CAN 15              EXCEPT    GBAADD
     C   37
     CANN15              EXCEPT    GBAUPD
     C                   EXSR      SRALT                                        -CONTROLE
     C*                                                     SINISTROS
     C     FIM2          TAG
     C                   EXSR      BEGS01
     C                   GOTO      XPTO01
     C     ACABLR        TAG
     C**************************************************************************
     C*  SUB-ROTINA                                                            *
     C**************************************************************************
+----C     OPCOES        BEGSR
|    C*                  EVAL      WDatInicio = 20120615
|    C*                  EVAL      CR1_ = CR1
|    C*                  EVAL      CR2_ = CR2
|    C*                  EVAL      CR3_ = CR3
|    C*
|    C*                  EVAL      WPtrData = %ADDR(RBASEBK)
|    C*                  CALL      'PPRDO100'
|    C*                  PARM                    RRAPOL
|    C*                  PARM                    WPtrData
|    C*                  PARM                    WDatInicio
|    C*                  PARM                    WModo
|    C*                  EVAL      CR1 = CR1_
|    C*                  EVAL      CR2 = CR2_
|    C*                  EVAL      CR3 = CR3_
+----C                   ENDSR
     C**************************************************************************
     C     ERRO          BEGSR
     C                   MOVE      'Z'           INDREG
     C                   EXCEPT    ECERRO
     C                   READ      ECRAN                                  92
     C                   ENDSR
     C**************************************************************************
     C     VDUR          BEGSR
     C                   SETOFF                                       30
     C                   ENDSR
     C**************************************************************************
     C     VDTFIM        BEGSR
     C                   SETOFF                                       31
     C     DUR           COMP      2                                      10
     C   10              GOTO      DUR2
     C     DDFIM         COMP      0                                  3131         DUR=1
     C  N31MMFIM         COMP      0                                  3131
     C  N31AAFI          COMP      0                                  3131
     C                   GOTO      EDTFIM
     C     DUR2          TAG
     C     DDFIM         COMP      0                                      31       DUR=2
     C  N31MMFIM         COMP      0                                      31
     C  N31AAFI          COMP      0                                      31
     C     EDTFIM        ENDSR
     C**************************************************************************
     C     VFRAC         BEGSR
     C                   SETOFF                                       32
     C     DUR           COMP      2                                      10
     C   10              GOTO      FRAC5
     C   50FRAC          COMP      1                                    32         DUR=1
     C   50
     CANN32FRAC          COMP      3                                  32
     C   51FRAC          COMP      1                                  3232
     C                   GOTO      EFRAC
     C     FRAC5         TAG
     C   50FRAC          COMP      5                                  3232         DUR=2
     C     EFRAC         ENDSR
     C**************************************************************************
     C     VMES1         BEGSR
     C                   SETOFF                                       33
     C     DUR           COMP      2                                      10
     C   10              GOTO      MES99
     C     MES1          COMP      MMINIC                             3333         DUR=1
     C                   GOTO      EMES1
     C     MES99         TAG
     C     MES1          COMP      99                                 3333         DUR=2
     C     EMES1         ENDSR
     C**************************************************************************
     C     VMES13        BEGSR
     C                   SETOFF                                       33
     C     DUR           COMP      2                                      10
     C   10              GOTO      MM99
     C     MES1          COMP      1                                    33         DUR=1
     C  N33MES1          COMP      12                                 33
     C                   GOTO      EMES13
     C     MM99          TAG
     C     MES1          COMP      99                                 3333         DUR=2
     C     EMES13        ENDSR
     C**************************************************************************
     C     VCONT1        BEGSR                                                     OPÇÃO=NOVOS
     C                   SETOFF                                       34
     C     DUR           COMP      2                                      10
     C   10              GOTO      MA9999
     C                   MOVE      MMINIC        WMES              2 0             DUR=1
     C                   MOVE      AAINI         WK2               4 0
     C     FRAC          COMP      2                                      10
     C   10              GOTO      SEM
     C     FRAC          COMP      3                                      10
     C   10              GOTO      SEM
     C     WK2           ADD       1             WK2                               FR=1
     C                   GOTO      COMPAR
     C     SEM           TAG
     C     FRAC          IFEQ      2
     C     WMES          ADD       6             WMES                              FR=2
     C                   ELSE
     C     FRAC          IFEQ      3
     C     WMES          ADD       3             WMES                              FR=3
     C                   END
     C                   END
     C     WMES          COMP      13                                   10
     C   10              GOTO      COMPAR
     C     WMES          SUB       12            WMES
     C     WK2           ADD       1             WK2
     C     COMPAR        TAG
     C     MMCONT        COMP      WMES                               3434
     C  N34AACON         COMP      WK2                                3434
     C                   GOTO      ECONT1
     C     MA9999        TAG
     C     MMCONT        COMP      99                                 3434         DUR=2
     C  N34AACON         COMP      9999                               3434
     C     ECONT1        ENDSR
     C**************************************************************************
     C     VMCONT        BEGSR                                                     OPÇÃO=ALTERAÇÕES
     C                   SETOFF                                       34
     C     DUR           COMP      2                                      10
     C   10              GOTO      M99
     C     SIT           COMP      1                                      10
     C  N10              GOTO      EMCONT
     C     FRAC          COMP      2                                      10
     C   10              GOTO      SEMEST
     C     FRAC          COMP      3                                      10
     C   10              GOTO      SEMEST
     C     MMCONT        COMP      MES1                               3434         FR=1
     C                   GOTO      EMCONT
     C     SEMEST        TAG
     C     MES1          COMP      MMCONT                                 10
     C   10              GOTO      EMCONT
     C     FRAC          IFEQ      2
     C     MES1          ADD       6             WMES              2 0             FR=2
     C                   ELSE
     C     FRAC          IFEQ      3
     C     MES1          ADD       3             WMES                              FR=3
     C                   END
     C                   END
     C     WMES          COMP      13                                   10
     C   10              GOTO      COMPA
     C     WMES          SUB       12            WMES              2 0
     C     COMPA         TAG
     C     WMES          COMP      MMCONT                             3434
     C   34FRAC          IFEQ      3
     C     WMES          IFLT      MMCONT
     C                   ADD       3             WMES
     C                   GOTO      COMPA
     C                   END
     C                   END
     C                   GOTO      EMCONT
     C     M99           TAG
     C     MMCONT        COMP      99                                 3434         DUR=2
     C     EMCONT        ENDSR
     C**************************************************************************
     C     VCONC         BEGSR
     C     CONC          CHAIN     FCONC                              06
     C                   ENDSR
     C**************************************************************************
     C     VMED1         BEGSR
     C                   SETOFF                                       076566
     C*
     C* VALIDA SE MEDIADOR EXISTE EM FICHEIRO
     C                   MOVEL     0             MED               4 0
     C                   MOVE      MED1          MED               4 0
     C     MED           IFEQ      031                                          MORTAGUA
     C                   GOTO      EMED1
     C                   ELSE
     C     MED           IFGE      299
     C     MED           CHAIN     FMED                               07
     C   07              GOTO      EMED1
     C                   ELSE
     C                   SETON                                        07
     C                   END
     C                   END
     C*
     C* VALIDAR SE MEDIADOR É SATÉLITE OU CORRETOR
     C     MED           IFGE      900                                          <---
     C     JMM1          IFGT      *ZEROS                                       <- |
     C                   SETON                                            66     | |
     C   66              MOVE      MSG(102)      MENSER                          | |
     C                   END                                                    <- |
     C     JMC1          IFGT      *ZEROS                                       <- |
     C                   SETON                                            66     | |
     C   66              MOVE      MSG(102)      MENSER                          | |
     C                   END                                                    <- |
     C     JMM2          IFGT      *ZEROS                                       <- |
     C                   SETON                                            65     | |
     C   65              MOVE      MSG(105)      MENSER                          | |
     C                   END                                                    <- |
     C     JMC2          IFGT      *ZEROS                                       <- |
     C                   SETON                                            65     | |
     C   65              MOVE      MSG(105)      MENSER                          | |
     C                   END                                                    <- |
     C                   END                                                    <---
     C     EMED1         ENDSR
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
     C     MED2          IFNE      0001                                          COBRADOR
     C     MED2          IFNE      0006                                          MONTEPIO
     C     MED2          IFNE      0008                                          OFERTAS
     C                   SETON                                        02
     C                   GOTO      EMED2
     C                   END
     C                   END
     C                   END
     C*
     C     JMM1          IFNE      *ZEROS
     C                   SETON                                        02
     C                   END
     C     EMED2         ENDSR
     C**************************************************************************
     C     VTALT         BEGSR
     C                   SETOFF                                       49
     C     SIT           COMP      1                                      10
     C  N10              GOTO      ETALT
     C     TALT          COMP      3                                      49       SIT=1
     C     ETALT         ENDSR
     C**************************************************************************
     C     VDALT         BEGSR
     C                   SETOFF                                       25
     C                   MOVEL     AAALTX        WK4               6 0
     C                   MOVE      MMALT         WK4
     C                   MOVEL     WK4           YMDALT            8 0
     C                   MOVE      DDALT         YMDALT
     C                   MOVEL     AACON         WK4               6 0
     C                   MOVE      MMCONT        WK4
     C                   MOVEL     WK4           YMDCON            8 0
     C                   MOVE      DDINIC        YMDCON
     C     YMDALT        COMP      YMDCON                             25
     C     EDALT         ENDSR
     C**************************************************************************
     C     VCR2          BEGSR
     C                   SETOFF                                       39
     C   50CR2           COMP      0                                      10
     C   50
     CAN 10              DO
     C                   MOVE      *ZEROS        ECAPDP
     C                   MOVE      *ZEROS        ECAPDI
     C                   MOVE      *ZEROS        VEXTRA
     C                   MOVE      *ZEROS        CODFRQ
     C                   MOVE      *ZEROS        FRANQ
     C                   MOVE      *ZEROS        PR2
     C                   GOTO      ECR2
     C                   END
     C*
     C   50CR2           IFGT      0
     C     CR2           LOOKUP    TABRIS                                 10
     C  N10              SETON                                        39
     C   10DATINI        IFGE      20030501
     C     CR2           IFEQ      5
     C                   SETON                                        39
     C                   ELSE
     C*          CR2       IFEQ 6
     C*                    SETON                     39
     C*                    END
     C                   END
     C                   END
     C                   END
     C*
     C   51CR2           COMP      2                                  3939
     C     ECR2          ENDSR
     C**************************************************************************
     C     VNASC         BEGSR
     C                   SETOFF                                       43
     C     NASCX         COMP      9999                                   10
     C   10              GOTO      ENASC
     C     NASCX         COMP      0                                      43
     C     AAIPL         SUB       NASCX         WK2               4 0
     C  N43WK2           COMP      16                                   43         RR=50/51
     C     CR3           IFGT      0
     C  N43WK2           COMP      75                                 43
     C                   END
     C   43              MOVE      MSG(32)       MENSER           60
     C   43              EXSR      ERRO
      *-------
     C  N43CODFRQ        IFEQ      09
     C     WK2           COMP      25                                   43
     C   43              SETON                                        11
     C   43              MOVE      MSG(133)      MENSER           60
     C   43              EXSR      ERRO
     C                   END
     C     ENASC         ENDSR
     C**************************************************************************
     C     VCARTA        BEGSR
     C                   SETOFF                                       44
     C     CARTAX        COMP      9999                                   10
     C   10              GOTO      ECARTA
     C     CARTAX        COMP      0                                      44
     C  N44CARTAX        COMP      AAIPL                              44
     C   44              MOVE      MSG(33)       MENSER           60
     C   44              EXSR      ERRO
      *-------
     C  N44CODFRQ        IFEQ      09
     C     CARTAX        COMP      9999                                   10
     C   10              GOTO      ECARTA
     C     CARTAX        COMP      0                                      44
     C     AAIPL         SUB       CARTAX        WK3               4 0
     C  N44WK3           COMP      2                                    44
     C   44              SETON                                        11
     C   44              MOVE      MSG(134)      MENSER           60
     C   44              EXSR      ERRO
     C                   END
      *
     C     ECARTA        ENDSR
     C**************************************************************************
     C     VTEST         BEGSR
     C                   SETOFF                                       47
     C     TALT          COMP      5                                      10
     C   10              GOTO      TALT5
     C     TEST          COMP      0                                  4747
     C                   GOTO      ETEST
     C     TALT5         TAG
     C     TEST          COMP      0                                      47       TALT=5
     C     ETEST         ENDSR
     C**************************************************************************
     C     VTB           BEGSR
     C                   SETOFF                                       45
     C   51TB            COMP      0                                  4545
     C   51              GOTO      ETB
     C     TB            COMP      1                                      10
     C   10              GOTO      ETB
     C     TB            COMP      5                                  10  10
     C   10TB            COMP      6                                    1010
     C   10              GOTO      ETB
     C     TB            IFEQ      8                                            jan07,asilva
     C                   GOTO      ETB                                          jan07,asilva
     C                   END                                                    jan07,asilva
     C     TB            IFEQ      9                                            jan07,asilva
     C                   GOTO      ETB                                          jan07,asilva
     C                   END                                                    jan07,asilva
     C     TB            COMP      3                                  4545
     C     ETB           ENDSR
     C**************************************************************************
     C     VAGRAV        BEGSR
     C                   SETOFF                                       48
     C   51AGRAV         COMP      0                                  4848
     C   51              GOTO      EAGRAV
     C     TB            COMP      1                                      10
     C  N10              GOTO      EAGRAV
     C     AGRAV         COMP      0                                  4848         TB=1
     C     EAGRAV        ENDSR
     C**************************************************************************
     C     SCAPRC        BEGSR
     C                   SETOFF                                       08
     C   50              DO
     C     CAPRC         LOOKUP    TABCAP                                 80
     C  N80              SETON                                        08
     C                   END
     C     OK1           ENDSR
     C********************************************************************
     C*   Validação do NSEG
     C*   =================
     C     VFSEG         BEGSR
     C                   SETOFF                                       87
     C     NSEG          CHAIN     GFSEG                              05
     C   05              MOVE      MSG(42)       MENSER           60
     C   05              EXSR      ERRO
     C  N05              DO
     C                   MOVE      NSEG          NSEG2
     C     CORSEL        IFEQ      'N'
     C                   MOVE      'N'           PILOTO
     C                   SETON                                        87
     C                   ELSE
     C                   MOVE      272           JMM1                           CORSEL
     C                   END
     C   51              SETON                                        87
     C                   END
     C                   ENDSR
     C**************************************************************************
     C     VMATR         BEGSR
     C     MATR1         COMP      *BLANK                                 98
     C  N98VALMAT        IFEQ      'S'
     C     MATR1         LOOKUP    TABMAT                                 97
     C  N97MATH1         COMP      '-'                                9898
     C  N97
     CANN98MATH2         COMP      '-'                                9898
     C                   END
      *
     C                   SETOFF                                       97
     C     EMATR         ENDSR
     C********************************************************************
     C*   LIMPEZA DE CAMPOS ( ECRAN DE NOVOS )
     C*   ====================================
     C     BEGS03        BEGSR
     C                   MOVE      'N'           RECS
     C                   MOVE      *BLANK        LCB
     C                   Z-ADD     0             IM
     C                   Z-ADD     0             CS
     C                   Z-ADD     0             CPLCB1
     C                   MOVE      *BLANK        CPLCB2
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
     C                   Z-ADD     0             MMCONT
     C                   Z-ADD     0             AACONT
     C                   Z-ADD     0             AACON
     C                   Z-ADD     0             CEMIS
     C                   Z-ADD     0             ISEN
     C                   Z-ADD     0             CONC
     C                   Z-ADD     299           MED1
     C                   MOVE      'S'           VALMAT
     C                   MOVE      'S'           AVISN
     C     RR            LOOKUP    TABRR         TABPC                    41
     C   41              MOVE      TABPC         COMISS
     C   41              Z-ADD     CORR1         COM1
     C                   SETOFF                                           41
     C                   Z-ADD     0             MED2
     C                   Z-ADD     0             COM2
     C                   Z-ADD     0             CR1
     C                   Z-ADD     0             CAPRC
     C                   Z-ADD     0             PR1
     C                   Z-ADD     0             CR2
     C                   Z-ADD     0             CAPDP
     C                   Z-ADD     0             PR2
     C                   Z-ADD     0             CR3
     C                   Z-ADD     0             CAP3
     C                   Z-ADD     0             PR3
     C                   Z-ADD     0             PRS
     C                   Z-ADD     0             FRANQ
     C                   MOVE      *BLANK        MARC1
     C                   MOVE      *BLANK        MATR1
     C                   Z-ADD     0             AC1X
     C                   Z-ADD     0             CL
     C                   Z-ADD     0             PB
     C                   Z-ADD     0             NLUG1
     C                   Z-ADD     0             CATV
     C                   MOVE      *BLANK        MARC2
     C                   MOVE      *BLANK        MATR2
     C                   Z-ADD     0             NLUG2
     C                   Z-ADD     0             NASCX
     C                   Z-ADD     0             CARTAX
     C                   Z-ADD     0             TB
     C                   Z-ADD     0             TEXTO
     C                   MOVE      *BLANK        CL1
     C                   MOVE      *BLANK        CL2
     C                   MOVE      *BLANK        CL3
     C                   MOVE      *BLANK        CL4
     C                   MOVE      *BLANK        CL5
     C                   MOVE      *BLANK        CL6
     C                   MOVE      *BLANK        CL7
     C                   MOVE      *BLANK        CL8
     C                   MOVE      *BLANK        OBS1
     C                   MOVE      *BLANK        OBS2
     C   50              MOVE      *BLANK        VIAG
     C   51              MOVE      *BLANK        VIAG
     C                   Z-ADD     0             DTINV
     C                   Z-ADD     0             AAINVG
     C                   Z-ADD     0             DTEXV
     C                   Z-ADD     0             AAEXVG
     C                   Z-ADD     0             PRSVG
     C                   Z-ADD     0             PVGAGR
     C                   Z-ADD     0             TOTVG
     C                   Z-ADD     0             TCALVG
     C                   Z-ADD     0             WPRSVG            7 2
     C*
     C                   Z-ADD     0             VEXTRA
     C                   Z-ADD     0             VLNOVO
     C                   Z-ADD     0             CAPDPI
     C                   Z-ADD     0             DSCESP
     C                   Z-ADD     0             CATREB
     C                   Z-ADD     0             CODFRQ
     C                   Z-ADD     0             DTMATR
     C                   Z-ADD     0             DTMATX
     C                   Z-ADD     0             CILIND
     C                   MOVE      *BLANKS       MODELO
     C                   MOVE      'N'           ART5
     C                   Z-ADD     0             VLART5
     C                   Z-ADD     0             DTART5
     C                   Z-ADD     0             DTAR5
     C   50              MOVE      *BLANK        PJUR
     C   51              MOVE      ' '           PJUR
     C                   Z-ADD     0             DTINPJ
     C                   Z-ADD     0             AAINPJ
     C                   Z-ADD     0             DTEXPJ
     C                   Z-ADD     0             AAEXPJ
     C                   Z-ADD     0             PRSPJ
     C                   Z-ADD     0             PPJAGR
     C                   Z-ADD     0             TOTPJ
     C                   Z-ADD     0             TCALPJ            1 0
     C                   Z-ADD     0             WPRSPJ            7 2
     C                   Z-ADD     0             CRHIP
     C                   MOVE      *BLANK        PRIV
     C                   MOVE      *BLANK        VSA
     C                   MOVE      *BLANK        VIA
     C                   Z-ADD     0             PRPRIV
     C                   MOVE      'N'           QIV
     C                   Z-ADD     0             PRQIV
     C                   Z-ADD     0             CAPREP            7 0
     C                   Z-ADD     0             DTCREP            6 0
     C                   Z-ADD     0             CAPDP
     C                   Z-ADD     0             ECAPDP            6 0
     C                   Z-ADD     0             ECAPDI            6 0
     C                   Z-ADD     0             EVNOVO            6 0
     C                   Z-ADD     0             EVART5            6 0
     C                   Z-ADD     0             TC
     C                   Z-ADD     0             CAPRE             6 0
     C                   Z-ADD     0             DTCRE             8 0
     C                   Z-ADD     0             QIVAGR
     C                   Z-ADD     0             PRVAGR
     C                   Z-ADD     0             PROCUA
     C                   ENDSR
     C********************************************************************
     C*   LIMPEZA DE CAMPOS ( ECRAN DO MENU )
     C*   ===================================
     C     BEGS01        BEGSR
     C                   Z-ADD     0             OPCAO
     C                   Z-ADD     0             RRAPOL
     C                   ENDSR
     C********************************************************************
     C*   FORMATA CAMPOS DO BASE PARA DISPLAY
     C*   ===================================
     C     BEGS04        BEGSR
     C     PJUR          IFEQ      *BLANK
     C                   MOVE      ' '           PJUR
     C                   END
     C     ART5          IFEQ      *BLANK
     C                   MOVE      'N'           ART5
     C                   END
     C     DTINIC        IFGT      *ZEROS
     C                   MOVE      AAINIC        AAINI
     C     AAINIC        IFGT      40
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
     C     MMCONT        IFEQ      99
     C     AACONT        IFEQ      99
     C                   MOVE      9999          AACON
     C                   END
     C                   ELSE
     C                   MOVE      AACONT        AACON
     C     AACONT        IFGT      40
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
     C     DTMATR        IFGT      *ZEROS
     C                   MOVE      DDMATR        DDMATX
     C                   MOVE      MMMATR        MMMATX
     C                   MOVE      AAMATR        AAMATX
     C     AAMATR        IFGT      40
     C                   MOVEL     19            AAMATX
     C                   ELSE
     C                   MOVEL     20            AAMATX
     C                   END
     C                   ELSE
     C                   MOVE      *ZEROS        DDMATX
     C                   MOVE      *ZEROS        MMMATX
     C                   MOVE      *ZEROS        AAMATX
     C                   END
     C*
     C                   MOVE      AC1           AC1X
     C     AC1           IFGT      20
     C                   MOVEL     19            AC1X
     C                   ELSE
     C                   MOVEL     20            AC1X
     C                   END
     C*
     C     DTART5        IFGT      *ZEROS
     C                   MOVE      DDART5        DDAR5
     C                   MOVE      MMART5        MMAR5
     C                   MOVE      AAART5        AAAR5
     C     AAART5        IFGT      40
     C                   MOVEL     19            AAAR5
     C                   ELSE
     C                   MOVEL     20            AAAR5
     C                   END
     C                   ELSE
     C                   MOVE      *ZEROS        DDAR5
     C                   MOVE      *ZEROS        MMAR5
     C                   MOVE      *ZEROS        AAAR5
     C                   END
     C*
     C                   MOVE      *ZEROS        NASCX
     C     NASC          IFEQ      'XX'
     C                   MOVE      9999          NASCX
     C                   ELSE
     C                   MOVE      NASC          NASCX
     C     NASCX         IFGT      10
     C                   MOVEL     19            NASCX
     C                   ELSE
     C                   MOVEL     20            NASCX
     C                   END
     C                   END
     C*
     C                   MOVE      *ZEROS        CARTAX
     C     CARTA         IFEQ      'XX'
     C                   MOVE      9999          CARTAX
     C                   ELSE
     C                   MOVE      CARTA         CARTAX
     C     CARTAX        IFGT      19
     C                   MOVEL     19            CARTAX
     C                   ELSE
     C                   MOVEL     20            CARTAX
     C                   END
     C                   END
     C*
     C     MULSIN        IFGT      *ZEROS
     C                   MOVE      AULSIN        AULSI
     C     AULSIN        IFGT      40
     C                   MOVEL     19            AULSI
     C                   ELSE
     C                   MOVEL     20            AULSI
     C                   END
     C                   ELSE
     C                   MOVE      *ZEROS        AULSI
     C                   END
     C*
     C     MMINVG        IFGT      *ZEROS
     C                   MOVE      AAINVG        AAINV
     C     AAINVG        IFGT      40
     C                   MOVEL     19            AAINV
     C                   ELSE
     C                   MOVEL     20            AAINV
     C                   END
     C                   ELSE
     C                   MOVE      *ZEROS        AAINV
     C                   END
     C*
     C     MMEXVG        IFGT      *ZEROS
     C                   MOVE      AAEXVG        AAEXV
     C     AAEXVG        IFGT      40
     C                   MOVEL     19            AAEXV
     C                   ELSE
     C                   MOVEL     20            AAEXV
     C                   END
     C                   ELSE
     C                   MOVE      *ZEROS        AAEXV
     C                   END
     C*
     C     MMINPJ        IFGT      *ZEROS
     C                   MOVE      AAINPJ        AAINP
     C     AAINPJ        IFGT      40
     C                   MOVEL     19            AAINP
     C                   ELSE
     C                   MOVEL     20            AAINP
     C                   END
     C                   ELSE
     C                   MOVE      *ZEROS        AAINP
     C                   END
     C*
     C     MMEXPJ        IFGT      *ZEROS
     C                   MOVE      AAEXPJ        AAEXP
     C     AAEXPJ        IFGT      40
     C                   MOVEL     19            AAEXP
     C                   ELSE
     C                   MOVEL     20            AAEXP
     C                   END
     C                   ELSE
     C                   MOVE      *ZEROS        AAEXP
     C                   END
     C*
     C     CAPREP        IFGT      *ZEROS
     C                   Z-ADD     CAPREP        CAPRE
     C                   ELSE
     C                   Z-ADD     0             CAPRE
     C                   END
     C*
     C     DTCREP        IFGT      *ZEROS
     C                   MOVE      DDCREP        DDCRE
     C                   MOVE      MMCREP        MMCRE
     C                   MOVE      AACREP        AACRE
     C                   MOVEL     20            AACRE
     C                   ELSE
     C                   Z-ADD     0             DTCRE
     C                   END
     C*
     C                   ENDSR
     C********************************************************************
     C*   FORMATA CAMPOS DO DISPLAY PARA O BASE
     C*   ===================================
     C     BEGS05        BEGSR
     C                   MOVE      AAINI         AAINIC
     C                   MOVE      AAFI          AAFIM
     C                   MOVE      AACON         AACONT
     C                   MOVE      AAALTX        AAALT
     C                   MOVE      DDMATX        DDMATR
     C                   MOVE      MMMATX        MMMATR
     C                   MOVE      AAMATX        AAMATR
     C                   MOVE      AC1X          AC1
     C                   MOVE      DDAR5         DDART5
     C                   MOVE      MMAR5         MMART5
     C                   MOVE      AAAR5         AAART5
     C     NASCX         IFEQ      9999
     C                   MOVE      'XX'          NASC
     C                   ELSE
     C                   MOVE      NASCX         NASC
     C                   END
     C     CARTAX        IFEQ      9999
     C                   MOVE      'XX'          CARTA
     C                   ELSE
     C                   MOVE      CARTAX        CARTA
     C                   END
     C                   MOVE      AULSI         AULSIN
     C                   MOVE      AAINV         AAINVG
     C                   MOVE      AAEXV         AAEXVG
     C                   MOVE      AAINP         AAINPJ
     C                   MOVE      AAEXP         AAEXPJ
     C                   MOVE      DDCRE         DDCREP
     C                   MOVE      MMCRE         MMCREP
     C                   MOVE      AACRE         AACREP
     C                   Z-ADD     CAPRE         CAPREP
     C                   ENDSR
     C********************************************************************
     C*   CÁLCULO PRÉMIO SIMPLES ASSISTÊNCIA EM VIAGEM
     C*   ===========================================
     C     BEGS10        BEGSR
     C*
     C*  ALT.: PAULOC 11/01/96 - AUMENTO DO PRÉMIO DE ASSISTENCIA EM
     C*                          PASSOU DE 1295$00 PARA 1484$00
     C*  ALT.: PAULOC 11/01/96 - AUMENTO DO PRÉMIO DE ASSISTENCIA EM
     C*                          PASSOU DE 1484$00 PARA 1856$00
     C*  ALT.: PAULOC 12/01/98 - AUMENTO DO PRÉMIO DE ASSISTENCIA EM
     C*                          PASSOU DE 1856$00 PARA 1956$00
     C*
     C* DETERMINA PRÉMIO DA ASSISTÊNCIA EM VIAGEM COM FRACCIONAMENTO
     C* E COM O AGRAVAMENTO DE 5% PELO FACTO DE SER FRACCIONADO.
     C*
     C*  ALT.: PAULOC 12/01/98 - A PARTIR DE 01.01.98 DEIXA DE SER
     C*                          FRACCIONADO O PRÉMIO DE A.VIAGEM.
     C*   (NOVOS, ADICIONAIS,ESTORNOS PASSAM A TER VALOR 1956$00).
     C*
     C*  ALT.: RGUE   28/12/98 - A PARTIR DE 01.01.99 PRS ASS.VIAGEM
     C*                          PASSA A SER 2106$00.
     C*
     C*  ALT.: RGUE   01/05/00 - A PARTIR DE 01.05.00 PRS ASS.VIAGEM
     C*                          PASSA A SER 2156$00.
     C*
     C     TALT          IFNE      0                                            <------------------*
     C     WDTALT        IFLE      20000430                                     <---------------+  |
     C     WDTINV        IFLE      20000430                                     <------------+  |  |
     C** EUR               MOVE 1261      PRSVG                         |  |  |
     C                   Z-ADD     WVALOR        PRSVG                                       |  |  |
     C     ISEN          IFEQ      1
     C     PRSVG         DIV(H)    1.20          PRSVG
     C                   END
     C     FRAC          IFEQ      2                                            <---------+  |  |  |
     C*          PRSVG     MULT 0.05      PVIAFR  72H                |  |  |  |
     C                   ADD       PRSVG         WPRSVG            7 2                    |  |  |  |
     C*                    ADD  PVIAFR    WPRSVG                     |  |  |  |
     C     WPRSVG        DIV(H)    2             WPRSVG                                   |  |  |  |
     C                   ELSE                                                             *  |  |  |
     C     FRAC          IFEQ      3                                            <---------+  |  |  |
     C*          PRSVG     MULT 0.05      PVIAFR    H                |  |  |  |
     C                   ADD       PRSVG         WPRSVG                                   |  |  |  |
     C*                    ADD  PVIAFR    WPRSVG                     |  |  |  |
     C     WPRSVG        DIV(H)    4             WPRSVG                                   |  |  |  |
     C                   ELSE                                                             *  |  |  |
     C** EUR               MOVE 1261      WPRSVG                     |  |  |  |
     C                   Z-ADD     WVALOR        WPRSVG                                   |  |  |  |
     C     ISEN          IFEQ      1                                                      |
     C     WPRSVG        DIV(H)    1.20          WPRSVG                                   |
     C                   END                                                              |
     C                   END                                                    <---------+  |  |  |
     C                   END                                                    <---------+  |  |  |
     C                   END                                                    <------------+  |  |
     C                   ELSE                                                                   *  |
     C** EUR               MOVE 1290      PRSVG                            |  |
     C** EUR               MOVE 1290      WPRSVG                           |  |
     C                   Z-ADD     WVALOR        PRSVG
     C                   Z-ADD     PRSVG         WPRSVG
     C     ISEN          IFEQ      1
     C     PRSVG         DIV(H)    1.20          PRSVG
     C     WPRSVG        DIV(H)    1.20          WPRSVG
     C                   END
     C                   END                                                    <---------------*  |
     C                   ELSE                                                                      *
     C     WDTINV        IFGT      20000430                                     <---------------+  |
     C** EUR               MOVE 1290      PRSVG                            |  |
     C** EUR               MOVE 1290      WPRSVG                           |  |
     C                   Z-ADD     WVALOR        PRSVG
     C                   Z-ADD     PRSVG         WPRSVG
     C     ISEN          IFEQ      1
     C     PRSVG         DIV(H)    1.20          PRSVG
     C     WPRSVG        DIV(H)    1.20          WPRSVG
     C                   END
     C                   ELSE                                                                   *  |
     C** EUR               MOVE 1261      PRSVG                            |  |
     C** EUR               MOVE 1261      WPRSVG                           |  |
     C                   Z-ADD     WVALOR        PRSVG                                       |  |  |
     C                   Z-ADD     PRSVG         WPRSVG                                      |  |  |
     C     ISEN          IFEQ      1
     C     PRSVG         DIV(H)    1.20          PRSVG
     C     WPRSVG        DIV(H)    1.20          WPRSVG
     C                   END
     C                   END                                                    <---------------+  |
     C                   END                                                    <------------------*
     C                   ENDSR
     C********************************************************************
     C*   CÁLCULO PRÉMIO SIMPLES PROTECÇÃO JURÍDICA
     C*   =========================================
     C     BEGS11        BEGSR
      *
     C*    INDICE DIVISÃO FRAC
     C*    -------------------
     C                   Z-ADD     1             $FRAC             2 0
     C     FRAC          IFEQ      2
     C                   Z-ADD     2             $FRAC
     C     ADC           IFEQ      0
     C                   Z-ADD     1.04          $PFRAC            3 2
     C                   ELSE
     C                   Z-ADD     1.02          $PFRAC
     C                   END
     C                   ELSE
     C     FRAC          IFEQ      3
     C                   Z-ADD     4             $FRAC
     C     ADC           IFEQ      0
     C                   Z-ADD     1.06          $PFRAC
     C                   ELSE
     C                   Z-ADD     1.03          $PFRAC
     C                   END
     C                   ELSE
     C     FRAC          IFEQ      4
     C                   Z-ADD     12            $FRAC
     C                   Z-ADD     1.04          $PFRAC
     C                   END
     C                   END
     C                   END
     C*    -------------------
      *
     C     WDTINP        IFGE      19990301                                     <---------------+
     C** EUR               MOVE 1113      PRSPJ                            |
     C** EUR               MOVE 1113      WPRSPJ                           |
     C     FLAGX         IFEQ      'X'
     C                   Z-ADD     6.66          PRSPJ                                          |
     C                   Z-ADD     6.66          WPRSPJ                                         |
     C                   ELSE
     C     6.66          DIV(H)    $FRAC         PRSPJ
     C     6.66          DIV(H)    $FRAC         WPRSPJ
     C                   END
     C     ISEN          IFEQ      1
     C     PRSPJ         DIV(H)    1.20          PRSPJ
     C     WPRSPJ        DIV(H)    1.20          WPRSPJ
     C                   END
     C                   END                                                    <---------------+
     C                   ENDSR
     C*************************************************************************
     C* DETERMINA PRS DA R.C.
     C*
     C     GP700         BEGSR
     C     DSCESP        DIV       100           DESCTO            3 3
     C*
     C                   MOVEL     '1000'        W4                4 0
     C     WCATV         IFGE      501
     C     WCATV         IFLE      504
     C                   MOVEL     '2000'        W4
     C                   END
     C                   END
     C                   MOVE      WCATV         W4
     C*
     C     TARIRC        TAG
     C     WCATV         IFEQ      500                                          PRS-RC MANUAL
     C                   GOTO      REB
     C                   END
     C*
     C     TALT          IFEQ      0                                            <-----------
     C                   EXSR      SRCALL                                                 |
     C     WDESC         COMP      *BLANKS                                80              |
     C                   ELSE                                                              *
     C     FRAC          IFGE      1                                            <--------- |
     C                   EXSR      SRCAL2                                                 |
     C     WDESC         COMP      *BLANKS                                80              |
     C                   END                                                    <--------- |
     C                   END                                                    <-----------
      *
     C  N80              DO
     C     CAPRC         IFEQ      01800
     C                   Z-ADD     RC120         PR1
     C                   ELSE
     C     CAPRC         IFEQ      02500
     C                   Z-ADD     RC250         PR1
     C                   ELSE
     C     CAPRC         IFEQ      03250
     C                   Z-ADD     RC120         PR1
     C     CONTSN        IFEQ      ' '
     C     WCAPRC        IFEQ      2500
     C                   Z-ADD     RC250         PR1
     C                   END
     C                   END
     C                   ELSE
     C     CAPRC         IFEQ      06000
     C                   Z-ADD     RC250         PR1
     C                   ELSE
     C     CAPRC         IFEQ      99999
     C                   Z-ADD     RCILI         PR1
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C*
     C     REB           TAG
     C     CATREB        IFGT      *ZEROS
     C     MATR2         IFNE      *BLANK
     C                   MOVEL     '2000'        W4
     C                   MOVE      CATREB        W4
     C     TALT          IFEQ      0                                            <-----------
     C                   EXSR      SRCALL                                                 |
     C     WDESC         COMP      *BLANKS                                80              |
     C                   ELSE                                                              *
     C     FRAC          IFGE      1                                            <--------- |
     C                   EXSR      SRCAL2                                                 |
     C     WDESC         COMP      *BLANKS                                80              |
     C                   END                                                    <--------- |
     C                   END                                                    <-----------
      *
     C  N80              DO
     C     CAPRC         IFEQ      01800
     C                   ADD       RC120         PR1
     C                   ELSE
     C     CAPRC         IFEQ      02500
     C                   ADD       RC250         PR1
     C                   ELSE
     C     CAPRC         IFEQ      03250
     C                   ADD       RC120         PR1
     C     CONTSN        IFEQ      ' '
     C     WCAPRC        IFEQ      2500
     C                   ADD       RC250         PR1
     C                   END
     C                   END
     C                   ELSE
     C     CAPRC         IFEQ      06000
     C                   ADD       RC250         PR1
     C                   ELSE
     C     CAPRC         IFEQ      99999
     C                   ADD       RCILI         PR1
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C*
     C     ISEN          IFEQ      1                                            ISENÇ. ENC.
     C     PR1           DIV(H)    1.20          PR1
     C                   END
     C*
     C     DESCTO        IFGT      *ZEROS                                       DESCONTO ?
     C     PR1           MULT(H)   DESCTO        WORK72            7 2
     C                   SUB       WORK72        PR1
     C                   END
     C*
     C     CR2           IFEQ      *ZEROS                                       RISCOS DP ?
     C                   Z-ADD     PR1           PRS
     C                   END
     C                   ADD       PR3           PRS
     C                   ENDSR
     C*************************************************************************
     C* DETERMINA CAPITAL-DP DESVALORIZADO E CORRESPONDENTE PRÉMIO COMERCIAL
     C*************************************************************************
     C     GP800         BEGSR
      *
     C     TALT          IFEQ      0                                            <----------
     C                   Z-ADD     DATINI        ZDATA             8 0                    |
     C                   ELSE                                                             *
     C     FRAC          IFGE      1                                            <-------- |
     C                   Z-ADD     WDTALT        ZDATA                                    |
     C                   END                                                    <-------- |
     C                   END                                                    <----------
      *
     C     ZDATA         IFLT      20070101
     C                   EXSR      LOADDV                                        TAB DESVAL.
     C                   ELSE
     C                   EXSR      LOADD                                         TAB DESVAL.
     C                   END
      *
     C                   SETOFF                                       13
     C                   Z-ADD     0             ROUBO
      *
     C     WCATV         IFEQ      500
     C                   Z-ADD     ECAPDP        ECAPDI
     C*** EUR    ECAPDP    MULT 1000      WCAPDP
     C                   Z-ADD     ECAPDP        WCAPDP
     C                   GOTO      E800
     C                   END
     C*
     C                   EXSR      ANTIG
      *
     C     JMM1          IFEQ      272                                          CORSEL ?
     C     #MESES        IFLE      24
     C                   Z-ADD     0             #MESES
     C                   Z-ADD     EVNOVO        ECAPDP                         FORÇA CAPDP=VLNOVO
     C                   Z-ADD     EVNOVO        ECAPDI                         FORÇA CAPDPI=VLNOVO
     C                   END
     C                   END
     C*
     C                   Z-ADD     #MESES        NMESES
     C*
     C     NMESES        IFGT      108
     C                   Z-ADD     109           NMESES
     C                   END
     C*
     C                   EXSR      LOADT                                         TAB TARIFAÇ.
     C*
     C** DETERMINA SE APLICA TABELAS OU ART.5º DA LEI-SOCRATES
     C     ART5          IFEQ      'S'
     C                   EXSR      ARTG5
     C                   GOTO      E800
     C                   END
     C*
     C*          #MESES    IFGT 108
     C*                    EXSR ARTG5
     C*                    GOTO E800
     C*                    END
     C*
     C** DESVALORIZAÇÃO CAP-DP
     C     EVNOVO        IFLE      *ZEROS
     C                   SETON                                        13
     C                   GOTO      S800
     C                   END
     C*
     C                   MOVEL     *BLANKS       ZCATV             1
     C     ZDATA         IFGE      20070101
     C     CATV          IFEQ      1                                            LIGEIRO
     C                   MOVE      'S'           ZCATV
     C                   ELSE
     C     CATV          IFEQ      6                                            JEEP
     C                   MOVE      'S'           ZCATV
     C                   ELSE
     C     CATV          IFEQ      7                                            CARR. FAMIL.
     C                   MOVE      'S'           ZCATV
     C                   END
     C                   END
     C                   END
     C                   ELSE
     C     CATV          IFEQ      1                                            CARR. FAMIL.
     C                   MOVE      'S'           ZCATV
     C                   END
     C                   END
     C*
     C     ZDATA         IFGE      20070101
     C     ZCATV         IFEQ      'S'
     C*** EUR    EVNOVO    IFGT 7000
     C*** EUR    EVNOVO    IFGT 34916
     C     EVNOVO        IFGT      25000
     C                   MOVE      2             NTAB
     C                   ELSE
     C                   MOVE      1             NTAB
     C                   END
     C                   END
     C                   END
     C*
     C     ZDATA         IFLT      20070101
     C     ZCATV         IFEQ      'S'
     C*** EUR    EVNOVO    IFGT 7000
     C     EVNOVO        IFGT      34916
     C                   MOVE      2             NTAB
     C                   ELSE
     C                   MOVE      1             NTAB
     C                   END
     C                   END
     C                   END
     C*
     C     CATV          IFEQ      2                                            MISTO
     C                   MOVE      4             NTAB
     C                   END
     C*
     C     CATV          IFEQ      3                                            PESADO
     C                   MOVE      5             NTAB
     C                   END
     C*
     C     CATV          IFEQ      4                                            MOTOCICLO
     C                   MOVE      3             NTAB
     C                   END
     C*
     C     ZDATA         IFLT      20070101
     C     CATV          IFEQ      6                                            JEEP
     C                   MOVE      6             NTAB
     C                   END
     C     CATV          IFEQ      7                                            CARR. FAMIL.
     C                   MOVE      6             NTAB
     C                   END
     C                   END
     C*
     C                   Z-ADD     1             D                 4 0
     C     KTABS         LOOKUP    MDES(D)                              8080
     C   80EVNOVO        MULT(H)   DESV(D)       ECAPDI
     C     JMM1          IFEQ      272                                          CORSEL ?
     C     #MESES        IFLE      24
     C                   Z-ADD     EVNOVO        ECAPDI                         FORÇA CAPDPI=VLNOVO
     C                   END
     C                   END
     C*
     C**  TARIFAÇÃO
     C                   ADD       6             NMESES                         PONDERAÇÃO
     C                   Z-ADD     1             D
     C     KTABS         LOOKUP    MDES(D)                              8080
     C   80EVNOVO        MULT(H)   DESV(D)       ECAPDP
     C     JMM1          IFEQ      272                                          CORSEL ?
     C     #MESES        IFLE      24
     C                   Z-ADD     EVNOVO        ECAPDP                         FORÇA CAPDP=VLNOVO
     C                   END
     C                   END
     C*
     C                   Z-ADD     ECAPDP        WORK              6 0
     C     VEXTRA        IFGT      *ZEROS
     C                   ADD       VEXTRA        WORK
     C                   END
     C** EUR     WORK      MULT 1000      WCAPDP
     C                   Z-ADD     WORK          WCAPDP
     C*
     C     NMESES        IFGT      108
     C                   Z-ADD     109           NMESES
     C                   END
     C*
     C                   Z-ADD     1             D
     C     $TOTAL        IFNE      *BLANKS
     C     CR2           IFEQ      3                                            TODOS RISCOS
     C     WCAPDP        MULT(H)   TMED          PR2
     C                   ELSE
     C     CR2           IFEQ      4                                            CCC/IRE
     C     WCAPDP        MULT(H)   TCCC          PRCCC             9 2
     C     WCAPDP        MULT(H)   TI            PRI               9 2
     C     PRCCC         ADD       PRI           PR2
     C                   ELSE
     C     CR2           IFEQ      5                                            IRE
     C     WCAPDP        MULT(H)   TI            PR2
     C                   ELSE
     C     CR2           IFEQ      6                                            ROUBO
     C     WCAPDP        MULT(H)   TR            PR2
     C                   ELSE
     C     CR2           IFEQ      7                                            ROUBO + IRE
     C     WCAPDP        MULT(H)   TR            PRR               9 2
     C     WCAPDP        MULT(H)   TI            PRI               9 2
     C     PRR           ADD       PRI           PR2
     C                   ELSE
     C     CR2           IFEQ      8                                            TODOS/RCSP
     C     WCAPDP        MULT(H)   TMED          PR2
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C*
     C** APLICA DESCONTO FRANQUIA
     C     CR2           IFNE      3
     C     CR2           IFNE      7
     C     CR2           IFNE      8
     C                   GOTO      TAGF1
     C                   END
     C                   END
     C                   END
     C*
     C     WCAPDP        MULT(H)   TR            ROUBO             9 2
     C                   SUB       ROUBO         PR2
     C*
     C     TAGF1         TAG
     C                   ADD       ROUBO         PR2
      *
     C     ISEN          IFEQ      1
     C     PR2           DIV(H)    1.20          PR2
     C                   END
     C*
     C** APLICA DESCONTO ESPECIAL
     C     DESCTO        IFGT      *ZEROS                                       DESCONTO ?
     C     PR2           MULT(H)   DESCTO        WORK72
     C                   SUB       WORK72        PR2
     C                   END
     C*
     C** DETERMINA VALOR DA FRANQUIA
     C     E800          TAG
     C                   Z-ADD     *ZEROS        VMINF             5 0
     C     CR2           IFNE      6                                            6=ROUBO
     C     @MESES        IFGT      96                                           +8 ANOS ?
     C     CODFRQ        LOOKUP    TABFRQ        TABPF2                   80
     C   80              Z-ADD     TABPF2        PFRANQ            3 3
     C** EUR               Z-ADD38        VMINF
     C                   EXSR      SRFRQ
     C     CODFRQ        IFNE      9
     C     WFRANQ        IFEQ      'V'
     C                   Z-ADD     75            VMINF
     C                   ELSE
     C                   Z-ADD     188           VMINF
     C                   END
     C                   END
     C                   ELSE                                                   ATÉ 8 ANOS
     C     CODFRQ        LOOKUP    TABFRQ        TABPF1                   80
     C   80              Z-ADD     TABPF1        PFRANQ
     C** EUR               Z-ADD25        VMINF
     C                   EXSR      SRFRQ
     C     CODFRQ        IFNE      9
     C     WFRANQ        IFEQ      'V'
     C                   Z-ADD     50            VMINF
     C                   ELSE
     C                   Z-ADD     125           VMINF
     C                   END
     C                   END
     C                   END
     C     VEXTRA        IFGT      *ZEROS
     C     ECAPDI        ADD       VEXTRA        WORK
     C                   ELSE
     C                   Z-ADD     ECAPDI        WORK
     C                   END
     C     WORK          MULT(H)   PFRANQ        FRANQ             5 0
     C     FRANQ         IFLT      VMINF
     C                   Z-ADD     VMINF         FRANQ
     C                   END
     C                   END
     C*
     C     CR3           IFEQ      *ZEROS
     C                   Z-ADD     *ZEROS        CAP3
     C                   Z-ADD     *ZEROS        PR3
     C                   END
     C*
     C     PR1           ADD       PR2           PRS
     C                   ADD       PR3           PRS
     C     S800          ENDSR
     C*************************************************************************
     C* CALCULO DO PRS-DP POR ART.5º
     C*************************************************************************
     C     ARTG5         BEGSR
     C     ART5          IFEQ      'S'
     C                   Z-ADD     EVART5        ECAPDP
     C                   END
     C                   Z-ADD     ECAPDP        ECAPDI
     C                   Z-ADD     ECAPDP        WORK
     C     VEXTRA        IFGT      *ZEROS
     C                   ADD       VEXTRA        WORK
     C                   END
     C                   Z-ADD     WORK          WCAPDP            6 0
     C*
     C**  TARIFAÇÃO
     C                   Z-ADD     CATV          D
     C     CR2           IFEQ      3                                            TODOS RISCOS
     C     WCAPDP        MULT(H)   TMED          PR2
     C                   ELSE
     C     CR2           IFEQ      4                                            CCC/IRE
     C     WCAPDP        MULT(H)   TCCC          PRCCC             9 2
     C     WCAPDP        MULT(H)   TI            PRI               9 2
     C     PRCCC         ADD       PRI           PR2
     C                   ELSE
     C     CR2           IFEQ      5                                            IRE
     C     WCAPDP        MULT(H)   TI            PR2
     C                   ELSE
     C     CR2           IFEQ      6                                            ROUBO
     C     WCAPDP        MULT(H)   TR            PR2
     C                   ELSE
     C     CR2           IFEQ      7                                            ROUBO + IRE
     C     WCAPDP        MULT(H)   TR            PRR               9 2
     C     WCAPDP        MULT(H)   TI            PRI               9 2
     C     PRR           ADD       PRI           PR2
     C                   ELSE
     C     CR2           IFEQ      8                                            TODOS RISCOS+RCSP
     C     WCAPDP        MULT(H)   TMED          PR2
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C*
     C** APLICA DESCONTO FRANQUIA
     C     CR2           IFNE      3
     C     CR2           IFNE      7
     C     CR2           IFNE      8
     C                   GOTO      TAGF2
     C                   END
     C                   END
     C                   END
     C*
     C     WCAPDP        MULT(H)   TR            ROUBO             9 2
     C                   SUB       ROUBO         PR2
     C*
     C     TAGF2         TAG
     C                   ADD       ROUBO         PR2
     C*
     C     ISEN          IFEQ      1
     C     PR2           DIV(H)    1.20          PR2
     C                   END
     C*
     C** APLICA DESCONTO ESPECIAL
     C     DESCTO        IFGT      *ZEROS                                       DESCONTO ?
     C     PR2           MULT(H)   DESCTO        WORK72
     C                   SUB       WORK72        PR2
     C                   END
     C                   ENDSR
     C*************************************************************************
     C* CARREGAMENTO DAS TABELAS DE DESVALORIZAÇÃO AUTOMÓVEL
     C*************************************************************************
     C     LOADD         BEGSR
      *
     C                   Z-ADD     0             MDES
     C                   Z-ADD     0             DESV
      *
     C                   Z-ADD     0             D
     C                   Z-ADD     1             KTAB1            12 0
     C     KTAB1         SETLL     TABDESV
     C     RDESV         TAG
     C                   READ      TABDESV                                80
     C  N80              DO
     C                   ADD       1             D
     C                   MOVEL     NTAB          MDES(D)
     C                   MOVE      TMDES         MDES(D)
     C     PERV          DIV       100           DESV(D)
     C                   GOTO      RDESV
     C                   END
     C                   ENDSR
     C*************************************************************************
     C* CARREGAMENTO DAS TABELAS DE DESVALORIZAÇÃO AUTOMÓVEL - VELHO
     C*************************************************************************
     C     LOADDV        BEGSR
      *
     C                   Z-ADD     0             MDES
     C                   Z-ADD     0             DESV
      *
     C                   Z-ADD     0             D
     C                   Z-ADD     1             KTAB2             8 0
     C     KTAB2         SETLL     DESVAUT
     C     RDESVV        TAG
     C                   READ      DESVAUT                                80
     C  N80              DO
     C                   ADD       1             D
     C                   MOVEL     NTAB          MDES(D)
     C                   MOVE      TMDES         MDES(D)
     C     PERV          DIV       100           DESV(D)
     C                   GOTO      RDESVV
     C                   END
     C                   ENDSR
     C*************************************************************************
     C* CARREGAMENTO DAS TABELAS DA TARIFAÇÃO AUTOMÓVEL PARA DP
     C*************************************************************************
     C     LOADT         BEGSR
     C     TALT          IFEQ      0                                            <----------
     C                   EXSR      SRCAL3                                                 |
     C                   ELSE                                                             *
     C     FRAC          IFGE      1                                            <-------- |
     C                   EXSR      SRCAL4                                               | |
     C                   END                                                    <-------- |
     C                   END                                                    <----------
     C     $TOTAL        IFNE      *BLANKS
     C     WTXM          DIV       100           TMED              4 4
     C     WCCC          DIV       100           TCCC              4 4
     C     WIRE          DIV       100           TI                4 4
     C     WROU          DIV       100           TR                4 4
     C                   END
     C                   ENDSR
     C****************************************************************
     C* DETERMINA ANTIGUIDADE DO VEÍCULO EM MESES.
     C****************************************************************
     C     ANTIG         BEGSR
     C     TALT          IFEQ      *ZERO
     C     AAINI         SUB       AAMAT         ANOS              2 0
     C     ANOS          MULT      12            #MESES            3 0
     C     MMINIC        SUB       MMMAT         MESES             2 0
     C                   ADD       MESES         #MESES
     C     DDINIC        SUB       DDMAT         DIAS              2 0
     C     DIAS          IFGT      *ZEROS
     C                   ADD       1             #MESES
     C                   ELSE
     C     DIAS          IFLT      *ZEROS
     C                   SUB       1             #MESES
     C                   END
     C                   END
     C                   GOTO      XANTIG
     C                   END
     C*
     C     AAALTX        SUB       AAMAT         ANOS
     C     ANOS          MULT      12            #MESES
     C     MMALT         SUB       MMMAT         MESES
     C                   ADD       MESES         #MESES
     C     DDALT         SUB       DDMAT         DIAS
     C     DIAS          IFGT      *ZEROS
     C                   ADD       1             #MESES
     C                   ELSE
     C     DIAS          IFLT      *ZEROS
     C                   SUB       1             #MESES
     C                   END
     C                   END
     C*
     C     XANTIG        TAG
     C     #MESES        IFEQ      *ZEROS
     C                   Z-ADD     1             #MESES
     C                   END
     C*
     C                   EXSR      MFRQ                                         - MESES FRQ
     C*
     C                   ENDSR
     C****************************************************************
     C* DETERMINA ANTIGUIDADE DO VEÍCULO EM MESES (AC)
     C****************************************************************
     C     MFRQ          BEGSR
     C     TALT          IFEQ      *ZERO
     C     AAINI         SUB       AC1X          ANOS
     C     ANOS          MULT      12            @MESES            3 0
     C     MMINIC        SUB       01            MESES
     C                   ADD       MESES         @MESES
     C     DDINIC        SUB       01            DIAS
     C     DIAS          IFGT      *ZEROS
     C                   ADD       1             @MESES
     C                   ELSE
     C     DIAS          IFLT      *ZEROS
     C                   SUB       1             @MESES
     C                   END
     C                   END
     C                   GOTO      YANTIG
     C                   END
     C*
     C     AAALTX        SUB       AC1X          ANOS
     C     ANOS          MULT      12            @MESES
     C     MMALT         SUB       01            MESES
     C                   ADD       MESES         @MESES
     C     DDALT         SUB       01            DIAS
     C     DIAS          IFGT      *ZEROS
     C                   ADD       1             @MESES
     C                   ELSE
     C     DIAS          IFLT      *ZEROS
     C                   SUB       1             @MESES
     C                   END
     C                   END
     C*
     C     YANTIG        TAG
     C     @MESES        IFEQ      *ZEROS
     C                   Z-ADD     1             @MESES
     C                   END
     C                   ENDSR
     C****************************************************************
     C* CONTROLE ALTERAÇÕES AGRAVAMENTOS SINISTROS AUTO
     C****************************************************************
     CSR   SRALT         BEGSR
     C                   Z-ADD     0             CTRL              1 0
     C     TB            IFNE      WTB
     C                   Z-ADD     1             CTRL
     C                   ELSE
     C     AGRAV         IFNE      WAGRAV
     C                   Z-ADD     1             CTRL
     C                   ELSE
     C     NSIN          IFNE      WNSIN
     C                   Z-ADD     1             CTRL
     C                   ELSE
     C     DTUSIN        IFNE      WDUSIN
     C                   Z-ADD     1             CTRL
     C                   ELSE
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
     C     FRANQ         IFNE      WFRQ
     C                   Z-ADD     1             CTRL
     C                   ELSE
     C     CODFRQ        IFNE      WCDFRQ
     C                   Z-ADD     1             CTRL
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
      *
     C     CTRL          IFEQ      1
     C                   Z-ADD     FRANQ         FRQ
     C                   Z-ADD     CODFRQ        CDFRQ
     C                   Z-ADD     UDATE         DATALT            6 0
     C                   TIME                    HORALT            6 0
     C                   EXCEPT    FALADD
     C                   END
     CSR                 ENDSR
     C****************************************************************
     C* CÁLCULO RC - DATA DE INICIO                                  *
     C****************************************************************
     CSR   SRCALL        BEGSR
     C                   MOVEL     *BLANKS       WDESC
     C                   MOVE      W4            $CHAVE                         chave
     C                   MOVE      DATINI        $DATA                          chave
     C*
     C     TALT          IFEQ      0
     C                   CALL      'TABRCN'
     C* parâmetros de input para o programa TABRCN
     C                   PARM                    $CHAVE            4            tipo veiculo
     C                   PARM                    $DATA             8            data inicio risco
     C* parâmetros devolvidos pelo programa TABRCN
     C                   PARM                    $DESCR           60            descrição
     C                   PARM                    $RC250            6            RC cap ¤1250.000
     C                   PARM                    $RC500            6            RC cap ¤2500.000
     C                   PARM                    $RC120            6            RC cap ¤1800.000
     C                   PARM                    $RCILI            6            RC cap ilimitado
     C                   PARM                    $ASVIA            6            Premio Assist Viagem
     C                   PARM                    $PRCSP            4            Premio RCSP
     C                   ELSE
     C                   MOVE      WDTALT        $DATA                          chave
     C     CAPRC         IFEQ      3250
     C     WCAPRC        IFNE      0
     C     CONTSN        IFEQ      ' '
     C                   MOVE      '00000000'    $DATA
     C                   END
     C                   END
     C                   END
     C                   CALL      'TABRCN'
     C* parâmetros de input para o programa TABRC
     C                   PARM                    $CHAVE            4            tipo veiculo
     C                   PARM                    $DATA             8            data inicio risco
     C* parâmetros devolvidos pelo programa TABRC
     C                   PARM                    $DESCR           60            descrição
     C                   PARM                    $RC250            6            RC cap ¤1250.000
     C                   PARM                    $RC500            6            RC cap ¤2500.000
     C                   PARM                    $RC120            6            RC cap ¤1800.000
     C                   PARM                    $RCILI            6            RC cap ilimitado
     C                   PARM                    $ASVIA            6            Premio Assist Viagem
     C                   PARM                    $PRCSP            4            Premio RCSP
     C                   END
     C*
     C                   MOVE      $DESCR        WDESC            60
     C                   MOVE      $RC250        RC250             6 2
     C                   MOVE      $RC500        RC500             6 2
     C                   MOVE      $RC120        RC120             6 2
     C                   MOVE      $RCILI        RCILI             6 2
     C                   MOVE      $ASVIA        ASVIAG            6 2
     C                   MOVE      $PRCSP        RCSP              4 2
     CSR                 ENDSR
     C****************************************************************
     C* CÁLCULO RC - DATA DE ALTERAÇÃO                               *
     C****************************************************************
     CSR   SRCAL2        BEGSR
     C                   MOVEL     *BLANKS       WDESC
     C                   MOVE      W4            $CHAVE                         chave
      *
     C                   Z-ADD     WDTALT        YDTALT            8 0
      *
     C     DATINI        IFGE      20091201
     C                   MOVE      '99999999'    $DATA
     C                   GOTO      FRENTE
     C                   END
      *
     C     CAPRC         IFLT      3250
     C                   MOVE      '00000000'    $DATA
     C                   GOTO      FRENTE
     C                   END
      *
     C     CAPRC         IFEQ      3250
     C     WCAPRC        IFNE      0
     C     CONTSN        IFEQ      ' '
     C                   MOVE      '00000000'    $DATA
     C                   GOTO      FRENTE
     C                   END
     C                   END
     C                   END
      *
     C     WCAPRC        IFNE      0
     C     CONTSN        IFEQ      'S'
     C                   MOVE      '99999999'    $DATA
     C                   GOTO      FRENTE
     C                   END
     C                   END
      *
     C     WAAALT        IFEQ      AACON
     C     WMMALT        IFLT      MES1
     C                   SUB       1             WAAALT
     C                   END
     C                   END
     C                   MOVE      WDTALT        $DATA                          chave
      *
     C                   MOVEL     *BLANKS       NOVA              1
     C     AMCONT        IFGE      201001
     C     MES1          IFEQ      MMCONT
     C     MMALT         IFEQ      MMCONT
     C                   MOVE      'S'           NOVA
     C                   END
     C                   END
     C                   END
      *
     C     NOVA          IFEQ      ' '
     C     AMCONT        IFGE      200907
     C     FRAC          IFEQ      2
     C                   MOVE      'S'           NOVA
     C                   END
     C                   END
     C                   END
      *
     C     NOVA          IFEQ      'N'
     C                   MOVE      '00000000'    $DATA
     C                   END
      *
     C     FRENTE        TAG
      *
     C                   CALL      'TABRCN'
     C* parâmetros de input para o programa TABRCN
     C                   PARM                    $CHAVE            4            tipo veiculo
     C                   PARM                    $DATA             8            data inicio risco
     C* parâmetros devolvidos pelo programa TABRCN
     C                   PARM                    $DESCR           60            descrição
     C                   PARM                    $RC250            6            RC cap ¤1250.000
     C                   PARM                    $RC500            6            RC cap ¤2500.000
     C                   PARM                    $RC120            6            RC cap ¤1800.000
     C                   PARM                    $RCILI            6            RC cap ilimitado
     C                   PARM                    $ASVIA            6            Premio Assist Viagem
     C                   PARM                    $PRCSP            4            Premio RCSP
      *
     C                   MOVE      $DESCR        WDESC            60
     C                   MOVE      $RC250        RC250             6 2
     C                   MOVE      $RC500        RC500             6 2
     C                   MOVE      $RC120        RC120             6 2
     C                   MOVE      $RCILI        RCILI             6 2
     C                   MOVE      $ASVIA        ASVIAG            6 2
     C                   MOVE      $PRCSP        RCSP              4 2
      *
     C                   Z-ADD     YDTALT        WDTALT
      *
     CSR                 ENDSR
     C****************************************************************
     C* CÁLCULO RC - ANTERIOR                                        *
     C****************************************************************
     C*R         SRRCA     BEGSR
     C*          CONTSN    IFEQ ' '
     C*                    MOVE ' '       TRATA   1
     C*          WCAPRC    IFEQ 1800
     C*                    MOVE 'S'       TRATA
     C*                    ELSE
     C*          WCAPRC    IFEQ 2500
     C*                    MOVE 'S'       TRATA
     C*                    END
     C*                    END
     C*          TRATA     IFEQ 'S'
     C*                    MOVE '00000000'$DATA
     C*                    END
     C*                    END
     C*R                   ENDSR
     C****************************************************************
     C* CÁLCULO DP - DATA DE INÍCIO                                  *
     C****************************************************************
     CSR   SRCAL3        BEGSR
     C                   MOVE      DATINI        $DATA                          chave
     C                   MOVEL     CATV          $CATV                          chave
     C     NMESES        LOOKUP    TABM                                   80
     C   80NMESES        ADD       1             WMESES            3 0
     C  N80              Z-ADD     NMESES        WMESES
     C                   MOVE      WMESES        $MESES                         chave
     C                   MOVE      EVNOVO        $VALOR
     C                   MOVE      CODFRQ        $FRANQ
     C     ART5          IFEQ      ' '
     C                   MOVE      'N'           ART5
     C                   END
     C                   MOVE      ART5          $ART5
      *
     C                   MOVEL     *BLANKS       $TOTAL
     C                   MOVEL     *BLANKS       $CCC                                       .000
     C                   MOVEL     *BLANKS       $IRE                                       .000
     C                   MOVEL     *BLANKS       $ROUBO                                     000
     C*
     C                   CALL      'TABDP'
     C* parâmetros de input para o programa TABDP
     C                   PARM                    $DATA             8            data cont.  risco
     C                   PARM                    $CATV             1            categoria   risco
     C                   PARM                    $MESES            3            n.meses     risco
     C                   PARM                    $VALOR            6            vl.novo     risco
     C                   PARM                    $FRANQ            2            cod.franq.  risco
     C                   PARM                    $ART5             1            art5(S/N)   risco
     C* parâmetros devolvidos pelo programa TABDP
     C                   PARM                    $TOTAL            3            tx.media
     C                   PARM                    $CCC              3            C.C.C.      .000
     C                   PARM                    $IRE              3            I.R.E.      .000
     C                   PARM                    $ROUBO            3            roubo       000
      *
     C                   MOVE      $TOTAL        WTXM              3 2
     C                   MOVE      $CCC          WCCC              3 2
     C                   MOVE      $IRE          WIRE              3 2
     C                   MOVE      $ROUBO        WROU              3 2
     CSR                 ENDSR
     C****************************************************************
     C* CÁLCULO DP - DATA DE ALTERAÇÃO                               *
     C****************************************************************
     CSR   SRCAL4        BEGSR
     C                   MOVE      WDTALT        $DATA                          chave
     C                   MOVEL     CATV          $CATV                          chave
     C     NMESES        LOOKUP    TABM                                   80
     C   80NMESES        ADD       1             WMESES            3 0
     C  N80              Z-ADD     NMESES        WMESES
     C                   MOVE      WMESES        $MESES                         chave
     C                   MOVE      EVNOVO        $VALOR
     C                   MOVE      CODFRQ        $FRANQ
     C     ART5          IFEQ      ' '
     C                   MOVE      'N'           ART5
     C                   END
     C                   MOVE      ART5          $ART5
      *
     C                   MOVEL     *BLANKS       $TOTAL
     C                   MOVEL     *BLANKS       $CCC                                       .000
     C                   MOVEL     *BLANKS       $IRE                                       .000
     C                   MOVEL     *BLANKS       $ROUBO                                     000
     C*
     C                   CALL      'TABDP'
     C* parâmetros de input para o programa TABDP
     C                   PARM                    $DATA             8            data cont.  risco
     C                   PARM                    $CATV             1            categoria   risco
     C                   PARM                    $MESES            3            n.meses     risco
     C                   PARM                    $VALOR            6            vl.novo     risco
     C                   PARM                    $FRANQ            2            cod.franq.  risco
     C                   PARM                    $ART5             1            art5(S/N)   risco
     C* parâmetros devolvidos pelo programa TABDP
     C                   PARM                    $TOTAL            3            tx.media
     C                   PARM                    $CCC              3            C.C.C.      .000
     C                   PARM                    $IRE              3            I.R.E.      .000
     C                   PARM                    $ROUBO            3            roubo       000
      *
     C                   MOVE      $TOTAL        WTXM              3 2
     C                   MOVE      $CCC          WCCC              3 2
     C                   MOVE      $IRE          WIRE              3 2
     C                   MOVE      $ROUBO        WROU              3 2
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
     C****************************************************************
     C* PRIVAÇÃO DE USO                                              *
     C****************************************************************
     CSR   SRPU          BEGSR
      *
     C                   SETOFF                                       80
     C     CILIND        IFLE      1900
     C     VSA           COMP      '2'                                    80
     C  N80VIA           COMP      '2'                                    80
     C   80              DO
     C                   MOVE      MSG(128)      MENSER
     C                   GOTO      ESRPU
     C                   END
     C                   END
      *
     C     VSA           IFNE      ' '
     C     VIAG          IFEQ      ' '
     C                   SETON                                        80
     C                   MOVE      MSG(129)      MENSER
     C                   GOTO      ESRPU
     C                   END
     C                   END
     C     VIA           IFNE      ' '
     C     VIAG          IFEQ      ' '
     C                   SETON                                        80
     C                   MOVE      MSG(129)      MENSER
     C                   GOTO      ESRPU
     C                   END
     C                   END
      *
     C                   Z-ADD     0             TVALOR            7 2
      *
     C                   MOVEL     *BLANKS       CDTAB
     C                   MOVE      'PU'          TPTAB
     C                   MOVEL     PRIV          CDTAB
     C     KTAB          CHAIN     FTABX                              80
     C  N80              DO
     C                   ADD       VALOR1        TVALOR
     C                   ADD       VALOR2        TVALOR
     C                   ADD       VALOR3        TVALOR
     C                   END
     C   80              DO
     C                   Z-ADD     0             TVALOR
     C                   MOVE      MSG(115)      MENSER
     C                   GOTO      ESRPU
     C                   END
      *
     C                   MOVEL     *BLANKS       CDTAB
     C                   MOVE      'VS'          TPTAB
     C                   MOVEL     VSA           CDTAB
     C     KTAB          CHAIN     FTABX                              80
     C  N80              DO
     C                   ADD       VALOR1        TVALOR
     C                   ADD       VALOR2        TVALOR
     C                   ADD       VALOR3        TVALOR
     C                   END
     C   80              DO
     C                   Z-ADD     0             TVALOR
     C                   MOVE      MSG(115)      MENSER
     C                   GOTO      ESRPU
     C                   END
      *
     C                   MOVEL     *BLANKS       CDTAB
     C                   MOVE      'VI'          TPTAB
     C                   MOVEL     VIA           CDTAB
     C     KTAB          CHAIN     FTABX                              80
     C  N80              DO
     C                   ADD       VALOR1        TVALOR
     C                   ADD       VALOR2        TVALOR
     C                   ADD       VALOR3        TVALOR
     C                   END
     C   80              DO
     C                   Z-ADD     0             TVALOR
     C                   MOVE      MSG(115)      MENSER
     C                   END
      *
     CSR   ESRPU         ENDSR
     C****************************************************************
     C* ASSISTÊNCIA EM VIAGEM                                        *
     C****************************************************************
     CSR   SRAV          BEGSR
      *
     C*    INDICE DIVISÃO FRAC
     C*    -------------------
     C                   Z-ADD     1             $FRAC             2 0
     C     FRAC          IFEQ      2
     C                   Z-ADD     2             $FRAC
     C     ADC           IFEQ      0
     C                   Z-ADD     1.04          $PFRAC            3 2
     C                   ELSE
     C                   Z-ADD     1.02          $PFRAC
     C                   END
     C                   ELSE
     C     FRAC          IFEQ      3
     C                   Z-ADD     4             $FRAC
     C     ADC           IFEQ      0
     C                   Z-ADD     1.06          $PFRAC
     C                   ELSE
     C                   Z-ADD     1.03          $PFRAC
     C                   END
     C                   ELSE
     C     FRAC          IFEQ      4
     C                   Z-ADD     12            $FRAC
     C                   Z-ADD     1.04          $PFRAC
     C                   END
     C                   END
     C                   END
     C*    -------------------
      *
     C                   MOVEL     *BLANKS       CDTAB
     C                   MOVE      'AV'          TPTAB
     C                   MOVEL     VIAG          CDTAB
     C     KTAB          CHAIN     FTABX                              80
     C   80              Z-ADD     0             VALOR1
     C   80              Z-ADD     0             VALOR2
     C   80              Z-ADD     0             VALOR3
     C   80              Z-ADD     0             WVALOR            7 2
      *
     C  N80              DO
     C     FLAGX         IFNE      'X'
     C     VALOR1        DIV(H)    $FRAC         VALOR1
     C     VALOR2        DIV(H)    $FRAC         VALOR2
     C     VALOR3        DIV(H)    $FRAC         VALOR3
     C                   END
     C     TALT          IFEQ      0
     C                   GOTO      CARECA
     C                   END
     C     RRAPOL        CHAIN     CLASSIC                            80
     C  N80ICCLAS        IFEQ      'S'
     C                   GOTO      CARECA
     C                   END
     C   80              SETOFF                                       80
     C     TALT          IFEQ      0
     C     VIAG          IFEQ      'N'
     C     DATINI        IFGE      20110201
     C                   SETON                                        8049
     C                   SETON                                        555657
     C                   END
     C                   END
     C                   END
     C                   END
     C     CARECA        TAG
      *
     C  N80              DO
     C     VIAG          IFEQ      'B'
     C     TALT          IFEQ      0
     C     DATINI        IFLT      20110201
     C                   SETON                                        8049
     C                   SETON                                        555657
     C                   END
     C                   END
     C                   END
     C                   END
      *
     C  N80              DO
     C                   Z-ADD     VALOR1        WVALOR
      *
     C     VIAG          IFEQ      'B'
      *
     C                   EXSR      ANTIG
      *
     C     @MESES        IFLE      96
     C     CATV          IFEQ      1
     C                   Z-ADD     VALOR1        WVALOR
     C                   ELSE
     C     CATV          IFEQ      6
     C                   Z-ADD     VALOR1        WVALOR
     C                   ELSE
     C     CATV          IFEQ      7
     C                   Z-ADD     VALOR1        WVALOR
     C                   ELSE
     C     WCATV         IFEQ      242
     C                   Z-ADD     VALOR1        WVALOR
     C                   ELSE
     C     WCATV         IFEQ      252
     C                   Z-ADD     VALOR1        WVALOR
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
      *
     C                   ELSE
     C     CATV          IFEQ      1
     C                   Z-ADD     VALOR2        WVALOR
     C                   ELSE
     C     CATV          IFEQ      6
     C                   Z-ADD     VALOR2        WVALOR
     C                   ELSE
     C     CATV          IFEQ      7
     C                   Z-ADD     VALOR2        WVALOR
     C                   ELSE
     C     WCATV         IFEQ      242
     C                   Z-ADD     VALOR2        WVALOR
     C                   ELSE
     C     WCATV         IFEQ      252
     C                   Z-ADD     VALOR2        WVALOR
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
      *
     C     CATV          IFEQ      4
     C                   Z-ADD     VALOR2        WVALOR
     C                   ELSE
     C     WCATV         IFEQ      243
     C                   Z-ADD     VALOR2        WVALOR
     C                   ELSE
     C     WCATV         IFEQ      253
     C                   Z-ADD     VALOR2        WVALOR
     C                   END
     C                   END
     C                   END
      *
     C                   END
      *
     C*    N16             DO
     C*          VIAG      IFEQ 'B'
     C*                    Z-ADDYDATA     XDATA   80
     C*                    MOVEL2011      XDATA
     C*                    MOVE DDINIC    XDATA
     C*          WDTALT    IFLT XDATA
     C*          WVALOR    IFNE PVGAGR
     C*                    SETON                     803334
     C*                    SETON                     232425
     C*                    SETON                     49
     C*                    END
     C*                    END
     C*                    END
     C*                    END
      *
     C                   END
      *
     CSR                 ENDSR
      *
     C****************************************************************
     C* FRANQUIA MINIMA                                              *
     C****************************************************************
     CSR   SRFRQ         BEGSR
     C                   MOVEL     *BLANKS       WFRANQ            1
     C     DATINI        IFGE      20091201
     C                   MOVEL     'N'           WFRANQ
     C                   ELSE
     C     WDTALT        IFLT      20100101
     C                   MOVEL     'V'           WFRANQ
     C                   ELSE
     C                   Z-ADD     AACON         YAACON
     C                   Z-ADD     DDINIC        YDDINI
     C     WDTALT        IFLE      20101231
     C     YDATA         IFGT      WDTALT
     C     WANO          IFLE      2010
     C                   MOVEL     'V'           WFRANQ
     C                   ELSE
     C                   MOVEL     'N'           WFRANQ
     C                   END
     C                   ELSE
     C                   MOVEL     'N'           WFRANQ
     C                   END
     C                   ELSE
     C                   MOVEL     'N'           WFRANQ
     C                   END
     C                   END
     C                   END
     CSR   EFRQ          ENDSR
     C
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
       // Subrotina.: RunPopUp
       // Objectivo.: Adicionar um novo registo de uma Apólice
       //==================================================================
       BEGSR RunPopUp;
          MONITOR;
             IF (ECR_CM#NMFLD = 'SIT');
                SIT = %DEC($runPopUp(54:%CHAR(SIT)):1:0);
             ELSEIF (ECR_CM#NMFLD = 'CONC');
                CONC = %DEC($runPopUp(31:%CHAR(CONC)):3:0);
             ELSEIF (ECR_CM#NMFLD = 'ISEN');
                ISEN = %DEC($runPopUp(64:%CHAR(ISEN)):1:0);
             ELSEIF (ECR_CM#NMFLD = 'MALT');
                MALT = %DEC($runPopUp(27:%CHAR(MALT)):2:0);
             ELSEIF (ECR_CM#NMFLD = 'TALT');
                TALT = %DEC($runPopUp(16:%CHAR(TALT)):1:0);
             ELSEIF (ECR_CM#NMFLD = 'DUR');
                DUR = %DEC($runPopUp(48:%CHAR(DUR)):1:0);
             ELSEIF (ECR_CM#NMFLD = 'FRAC');
                FRAC = %DEC($runPopUp(15:%CHAR(FRAC)):1:0);
             ELSEIF (ECR_CM#NMFLD = 'PRIV');
                PRIV = $runPopUp(61:PRIV);
             ELSEIF (ECR_CM#NMFLD = 'VSA');
                VSA = $runPopUp(62:VSA);
             ELSEIF (ECR_CM#NMFLD = 'VIA');
                VIA = $runPopUp(63:VIA);
             ELSEIF (ECR_CM#NMFLD = 'CRHIPOT') OR (ECR_CM#NMFLD = 'CRHIP');
                CRHIP = $RunCredPopUp(CRHIP);
             ELSEIF (ECR_CM#NMFLD = 'NSEG');
                NSEG = $RunEntPopUp(NSEG);
             ENDIF;
          ON-ERROR;
          ENDMON;
          ECR_CM#ROW = CM#CROW;
          ECR_CM#COL = CM#CCOL;
       ENDSR;
      /END-FREE
      *
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
     O                       MATR1               80
     O                       UYEAR               82
     O                       UMONTH              84
     O                       UDAY                86
     O*
     OREGAPO2   EADD         ADDR
     O                       K23                 23
     O*
     OGBASEDIA  EADD         GBAADD
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
     O                       VALMAT             204
     O                       AVISN              205
     O                       TALT               206
     O                       DDALT              208
     O                       MMALT              210
     O                       AAALT              212
     O                       MALT               214
     O                       ACTA               216P
     O                       CR1                217
     O                       CAPRC              220P
     O                       PR1                225P
     O                       CR2                226
     O                       ANOCON             229P
     O                       PR2                234P
     O                       PRS                239P
     O                       FRANQ              242P
     O                       BONUS              246P
     O                       PRSAGR             251P
     O                       MARC1              265
     O                       MATR1              276
     O                       AC1                278
     O                       CL                 279
     O                       PB                 280
     O                       NLUG1              281
     O                       CATV               282
     O                       MARC2              296
     O                       MATR2              307
     O                       TC                 308
     O                       NLUG2              310
     O                       NASC               312
     O                       CARTA              314
     O                       CS                 315
     O                       DULSIN             317
     O                       MULSIN             319
     O                       AULSIN             321
     O                       NSIN               322
     O                       AGRAV              324P
     O                       TB                 325
     O                       CL1                326
     O                       CL2                327
     O                       CL3                328
     O                       CL4                329
     O                       CL5                330
     O                       CL6                331
     O                       CL7                332
     O                       CL8                333
     O                       OBS1               378
     O                       OBS2               423
     O                       TOTREC             428P
     O                       DDFIM              433
     O                       MMFIM              435
     O                       AAFIM              437
     O                       TEST               438
     O                       TEXTO              440P
     O                       DDMATR             442
     O                       MMMATR             444
     O                       AAMATR             446
     O                       WCAPRC             449P
     O                       ART5               450
     O                       NSEG               453P
     O                       DTART5             459
     O                       VSA                460
     O                       VIA                461
     O                       DSCESP             464P
     O                       NSEG2              470
     O                       VIAG               471
     O                       DDINVG             473
     O                       MMINVG             475
     O                       AAINVG             477
     O                       DDEXVG             479
     O                       MMEXVG             481
     O                       AAEXVG             483
     O                       PRSVG              487P
     O                       PVGAGR             491P
     O                       TCALVG             492
     O                       TOTVG              496P
     O                       PRCSPA             503
     O                       PRCSP              510
     O                       VEXTRA             513P
     O                       CAPDPI             517P
     O                       CR3                518
     O                       CAP3               521P
     O                       PR3                526P
     O                       CATREB             529
     O                       CODFRQ             531
     O                       RECS               532
     O                       PJUR               533
     O                       DDINPJ             535
     O                       MMINPJ             537
     O                       AAINPJ             539
     O                       DDEXPJ             541
     O                       MMEXPJ             543
     O                       AAEXPJ             545
     O                       PRSPJ              549P
     O                       PPJAGR             553P
     O                       TCALPJ             554
     O                       TOTPJ              558P
     O                       MODELO             578
     O***                      APREF    587
     O                       QIV                588
     O                       CONTSN             593
     O                       PRQIV              667P
     O                       CILIND             591P
     O                       PRIV               592
     O                       PRPRIV             671P
     O                       CPMOR1             599P
     O                       CPLCB1             603P
     O                       CRHIP              608
     O                       JMM1               611
     O                       JMC1               614P
     O                       JMM2               617
     O                       JMC2               620P
     O                       CAPDP              624P
     O                       VLNOVO             628P
     O                       CAPREP             632P
     O                       DTCREP             638
     O                       VLART5             642P
     O                       QIVAGR             675P
     O                       PRVAGR             679P
     O                       PROCUA             653P
     O* inicio alteração - introduzir 3 campos novos, ASILVA, Março2006
     O                       FROTA              654
     O                       RRMAE              656
     O                       APMAE              663
     O* fim alteração
     O*--------------------------------------------------------
     O* campos novos adicionados em Maio 2007
     O                       ADC                648P
     O                       NIB                700
     O                       EMPREG             309
     O                       COMB               465
     O                       MEMO               431
     O*--------------------------------------------------------
     OGBASEDIA  E            GBAUPD
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
     O                       VALMAT             204
     O                       AVISN              205
     O                       TALT               206
     O                       DDALT              208
     O                       MMALT              210
     O                       AAALT              212
     O                       MALT               214
     O                       ACTA               216P
     O                       CR1                217
     O                       CAPRC              220P
     O                       PR1                225P
     O                       CR2                226
     O                       ANOCON             229P
     O                       PR2                234P
     O                       PRS                239P
     O                       FRANQ              242P
     O                       BONUS              246P
     O                       PRSAGR             251P
     O                       MARC1              265
     O                       MATR1              276
     O                       AC1                278
     O                       CL                 279
     O                       PB                 280
     O                       NLUG1              281
     O                       CATV               282
     O                       MARC2              296
     O                       MATR2              307
     O                       TC                 308
     O                       NLUG2              310
     O                       NASC               312
     O                       CARTA              314
     O                       CS                 315
     O                       DULSIN             317
     O                       MULSIN             319
     O                       AULSIN             321
     O                       NSIN               322
     O                       AGRAV              324P
     O                       TB                 325
     O                       CL1                326
     O                       CL2                327
     O                       CL3                328
     O                       CL4                329
     O                       CL5                330
     O                       CL6                331
     O                       CL7                332
     O                       CL8                333
     O                       OBS1               378
     O                       OBS2               423
     O                       TOTREC             428P
     O                       DDFIM              433
     O                       MMFIM              435
     O                       AAFIM              437
     O                       TEST               438
     O                       TEXTO              440P
     O                       DDMATR             442
     O                       MMMATR             444
     O                       AAMATR             446
     O                       ART5               450
     O                       NSEG               453P
     O                       DTART5             459
     O                       VSA                460
     O                       VIA                461
     O                       DSCESP             464P
     O                       NSEG2              470
     O                       VIAG               471
     O                       DDINVG             473
     O                       MMINVG             475
     O                       AAINVG             477
     O                       DDEXVG             479
     O                       MMEXVG             481
     O                       AAEXVG             483
     O                       PRSVG              487P
     O                       PVGAGR             491P
     O                       TCALVG             492
     O                       TOTVG              496P
     O                       PRCSPA             503
     O                       VEXTRA             513P
     O                       CAPDPI             517P
     O                       CR3                518
     O                       CAP3               521P
     O                       PR3                526P
     O                       CATREB             529
     O                       CODFRQ             531
     O                       RECS               532
     O                       PJUR               533
     O                       DDINPJ             535
     O                       MMINPJ             537
     O                       AAINPJ             539
     O                       DDEXPJ             541
     O                       MMEXPJ             543
     O                       AAEXPJ             545
     O                       PRSPJ              549P
     O                       PPJAGR             553P
     O                       TCALPJ             554
     O                       TOTPJ              558P
     O                       MODELO             578
     O***                      APREF    587
     O                       QIV                588
     O                       PRQIV              667P
     O                       CILIND             591P
     O                       PRIV               592
     O                       PRPRIV             671P
     O                       CPMOR1             599P
     O                       CPLCB1             603P
     O                       CRHIP              608
     O                       JMM1               611
     O                       JMC1               614P
     O                       JMM2               617
     O                       JMC2               620P
     O                       CAPDP              624P
     O                       VLNOVO             628P
     O                       CAPREP             632P
     O                       DTCREP             638
     O                       VLART5             642P
     O                       QIVAGR             675P
     O                       PRVAGR             679P
     O                       PROCUA             653P
     O*--------------------------------------------------------
     O* campos novos adicionados em Maio 2007
     O                       ADC                648P
     O                       NIB                700
     O                       EMPREG             309
     O                       COMB               465
     O                       MEMO               431
     O*--------------------------------------------------------
     OFALTER    EADD         FALADD
     O                       RRAPOL               9
     O                       WTB                 10
     O                       TB                  11
     O                       WAGRAV              13P
     O                       AGRAV               15P
     O                       WNSIN               16
     O                       NSIN                17
     O                       WDUSIN              23
     O                       DTUSIN              29
      *
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
     O                       FRQ                111P
     O                       WCDFRQ             113P
     O                       CDFRQ              115P
      *
     O                       DATALT             209
     O                       HORALT             215
     O                       USER               223
     O*
     OECRAN     E            MENU01
     O                                           K4 'MENU'
     O                       INDREG               1
     O                       OPCAO         Z      2
     O                       RRAPOL              11 '        0'
     O*
     O          E            INPU02
     O                                           K6 'INPUT1'
     O                       WOPCAO               9
     O                       INDREG              10
     O                       RRAPOL              19
     O                       REF                 20
     O                       NSEG          Z     25
     O                       SEG                 65
     O                       RSALDO              90
     O                       MOR                130
     O                       CPOSM         Z    134
     O                       ZPOSM         Z    137
     O                       CPMOR2             157
     O                       ECR_CM#ROW         160
     O                       ECR_CM#COL         163
     O                       ECR_CM#NMREC       173
     O                       ECR_CM#NMFLD       183
     O*
     O          E            INPU03
     O                                           K5 'INPUT'
     O                       INDREG               1
     O                       LCB                 41
     O                       CPOSL         Z     45
     O                       ZPOSL         Z     48
     O                       CPLCB2              68
     O                       SIT           Z     69
     O                       DDINIC        Z     71
     O                       MMINIC        Z     73
     O                       AAINI         Z     77
     O                       DUR           Z     78
     O                       DDFIM         Z     80
     O                       MMFIM         Z     82
     O                       AAFI          Z     86
     O                       FRAC          Z     87
     O                       MES1          Z     89
     O                       MMCONT        Z     91
     O                       AACON         Z     95
     O                       CEMIS               96
     O                       ISEN                97
     O                       CONC          Z    100
     O                       MED1          Z    103
     O                       COM1          Z    108
     O                       MED2          Z    113
     O                       TALT               114
     O                       DDALT         Z    116
     O                       MMALT         Z    118
     O                       AAALTX        Z    122
     O                       MALT          Z    124
     O                       ACTA          Z    127
     O                       CRHIP         Z    132
     O                       IM                 133
     O                       DSCESP        Z    136
     O                       EXCONT             137
     O                       CS                 138
     O                       CR1           Z    139
     O                       CAPRC         Z    144
     O                       PR1           Z    153
     O                       CR2           Z    154
     O                       ECAPDP        Z    160
     O                       PR2           Z    169
     O                       VEXTRA        Z    174
     O                       CODFRQ             176
     O                       FRANQ         Z    181
     O                       BONUS         Z    188
     O                       PRIV               189
     O                       VSA                190
     O                       VIA                191
     O                       PRPRIV        Z    198
     O                       QIV                199
     O                       PRQIV         Z    206
     O                       CR3           Z    207
     O                       CAP3          Z    212
     O                       PR3           Z    221
     O                       AVISN              222
     O                       PRS           Z    231
     O                       CAPRE         Z    237
     O                       DTCRE         Z    245
     O                       PRSAGR        Z    254
     O                       PRCSPA        Z    261
     O                       MATR1              272
     O                       DTMATX        Z    280
     O                       CATV               281
     O                       CL                 282
     O                       PB                 283
     O                       NLUG1         Z    284
     O* novo campo COMB, Maio 2007
     O                       COMB               285
     O*
     O                       MARC1              299
     O                       MODELO             319
     O                       CILIND        Z    324
     O                       AC1X          Z    328
     O                       EVNOVO        Z    334
     O                       VALMAT             335
     O                       ART5               336
     O                       EVART5        Z    342
     O                       DTAR5         Z    350
     O                       ECAPDI        Z    356
     O                       MATR2              367
     O                       MARC2              381
     O                       CATREB        Z    384
     O                       TC                 385
     O                       NASCX         Z    389
     O                       CARTAX        Z    393
     O                       TEST          Z    394
     O                       DULSIN        Z    396
     O                       MULSIN        Z    398
     O                       AULSI         Z    402
     O                       NSIN          Z    403
     O                       AGRAV         Z    406
     O                       TB            Z    407
     O                       TEXTO         Z    410
     O                       CL1                411
     O                       CL2                412
     O                       CL3                413
     O                       CL4                414
     O                       CL5                415
     O                       CL6                416
     O                       CL7                417
     O                       CL8                418
     O                       PILOTO             419
     O                       OBS1               464
     O                       JMM1          Z    467
     O                       JMC1          Z    472
     O                       OBS2               517
     O                       JMM2          Z    520
     O                       JMC2          Z    525
     O                       TOTREC             535 '       , 0'
     O* novo campo EMPREG, Maio 2007
     O                       EMPREG             536
     O*
     O                       VIAG               537
     O                       DTINV         Z    545
     O                       DTEXV         Z    553
     O                       PRSVG         Z    560
     O                       TCALVG        Z    561
     O                       PVGAGR        Z    568
     O                       TOTVG              576 '     , 0'
     O* novo campo ADC, Maio 2007
     O                       ADC           Z    587
     O*
     O                       PJUR               588
     O                       DTINPJ        Z    596
     O                       DTEXPJ        Z    604
     O                       PRSPJ         Z    611
     O                       TCALPJ        Z    612
     O                       PPJAGR        Z    619
     O                       TOTPJ              627 '     , 0'
     O* novo campo NIB, Maio 2007
     O                       NIB                648
     O                       ECR_CM#ROW         651
     O                       ECR_CM#COL         654
     O                       ECR_CM#NMREC       664
     O                       ECR_CM#NMFLD       674
     O*
     O          E            OUTP04
     O                                           K6 'OUTPUT'
     O                       WOPCAO               9
     O                       INDREG              10
     O                       RRAPOL              19
     O                       REF                 20
     O                       NSEG                25
     O                       SEG                 65
     O                       RSALDO              90
     O                       MOR                130
     O                       CPOSM         Z    134
     O                       ZPOSM         Z    137
     O                       CPMOR2             157
     O                       LCB                197
     O                       CPOSL         Z    201
     O                       ZPOSL         Z    204
     O                       CPLCB2             224
     O                       SIT                225
     O                       DDINIC        Z    227
     O                       MMINIC        Z    229
     O                       AAINI         Z    233
     O                       DUR                234
     O                       DDFIM         Z    236
     O                       MMFIM         Z    238
     O                       AAFI          Z    242
     O                       FRAC               243
     O                       MES1               245
     O                       MMCONT             247
     O                       AACON              251
     O                       CEMIS              252
     O                       ISEN               253
     O                       CONC               256
     O                       MED1          Z    259
     O                       COM1          Z    264
     O                       MED2          Z    269
     O                       TALT               270
     O                       DDALT         Z    272
     O                       MMALT         Z    274
     O                       AAALTX        Z    278
     O                       MALT          Z    280
     O                       ACTA          Z    283
     O                       CRHIP         Z    288
     O                       IM                 289
     O                       DSCESP        Z    292
     O                       EXCONT             293
     O                       CS                 294
     O                       CR1                295
     O                       CAPRC         Z    300
     O                       PR1           Z    309
     O                       CR2           Z    310
     O                       ECAPDP        Z    316
     O                       PR2           Z    325
     O                       VEXTRA        Z    330
     O                       CODFRQ             332
     O                       FRANQ         Z    337
     O                       BONUS         Z    344
     O                       PRIV               345
     O                       VSA                346
     O                       VIA                347
     O                       PRPRIV        Z    354
     O                       QIV                355
     O                       PRQIV         Z    362
     O                       CR3           Z    363
     O                       CAP3          Z    368
     O                       PR3           Z    377
     O                       AVISN              378
     O                       PRS           Z    387
     O                       CAPRE         Z    393
     O                       DTCRE         Z    401
     O                       PRSAGR        Z    410
     O                       PRCSPA        Z    417
     O                       MATR1              428
     O                       DTMATX        Z    436
     O                       CATV               437
     O                       CL                 438
     O                       PB                 439
     O                       NLUG1              440
     O* novo campo COMB, Maio 2007
     O                       COMB               441
     O*
     O                       MARC1              455
     O                       MODELO             475
     O                       CILIND        Z    480
     O                       AC1X          Z    484
     O                       EVNOVO        Z    490
     O                       VALMAT             491
     O                       ART5               492
     O                       EVART5        Z    498
     O                       DTAR5         Z    506
     O                       ECAPDI        Z    512
     O                       MATR2              523
     O                       MARC2              537
     O                       CATREB        Z    540
     O                       TC                 541
     O                       NASCX         Z    545
     O                       CARTAX        Z    549
     O                       TEST          Z    550
     O                       DULSIN        Z    552
     O                       MULSIN        Z    554
     O                       AULSI         Z    558
     O                       NSIN          Z    559
     O                       AGRAV         Z    562
     O                       TB            Z    563
     O                       TEXTO         Z    566
     O                       CL1                567
     O                       CL2                568
     O                       CL3                569
     O                       CL4                570
     O                       CL5                571
     O                       CL6                572
     O                       CL7                573
     O                       CL8                574
     O                       PILOTO             575
     O                       OBS1               620
     O                       JMM1          Z    623
     O                       JMC1          Z    628
     O                       OBS2               673
     O                       JMM2          Z    676
     O                       JMC2          Z    681
     O                       TOTREC             691 '       , 0'
     O* novo campo EMPREG, Maio 2007
     O                       EMPREG             692
     O*
     O                       VIAG               693
     O                       DTINV         Z    701
     O                       DTEXV         Z    709
     O                       PRSVG         Z    716
     O                       TCALVG        Z    717
     O                       PVGAGR        Z    724
     O                       TOTVG              732 '     , 0'
     O* novo campo ADC, Maio 2007
     O                       ADC           Z    743
     O*
     O                       PJUR               744
     O                       DTINPJ        Z    752
     O                       DTEXPJ        Z    760
     O                       PRSPJ         Z    767
     O                       TCALPJ        Z    768
     O                       PPJAGR        Z    775
     O                       TOTPJ              783 '     , 0'
     O* novo campo NIB, Maio 2007
     O                       NIB                804
     O                       ECR_CM#ROW         807
     O                       ECR_CM#COL         810
     O                       ECR_CM#NMREC       820
     O                       ECR_CM#NMFLD       830
     O*
     O          E            ECERRO
     O                                           K6 'ECERRO'
     O                       INDREG               1
     O                       MENSER              61
     O*
     O          E            OUTP05
     O                                           K6 'ECMENS'
     O                       MENSER              60
**
OPÇÃO INVÁLIDA                                                     1
RAMO INVÁLIDO                                                      2
APÓLICE JÁ EXISTENTE NO DIA                                        3
APÓLICE JÁ EXISTENTE NO GRANDE                                     4
APÓLICE INEXISTENTE                                                5
INVÁLIDO INDICADOR DE MORADA                                       6
SITUAÇÃO INVÁLIDA                                                  7
DATA INVÁLIDA                                                      8
DURACAO INVÁLIDA                                                   9
FRACCIONAMENTO INVÁLIDO                                            10
MÊS  INVÁLIDO                                                      11
DATA CONTINUADO INVÁLIDA                                           12
CÓD. EMISSÃO INVÁLIDO                                              13
CÓD. ISENÇÃO INVÁLIDO                                              14
CÓD. CONCELHO INVÁLIDO                                             15
MED  INVÁLIDO                                                      16
COM  INVÁLIDA                                                      17
CCOB TEM QUE SER = 0001, 0006 OU 0008 E JMM1 = 000                 18
COM  INVÁLIDA                                                      19
CR1 INVÁLIDO                                                       20
CAP  INVÁLIDO                                                      21
CR2 INVÁLIDO                                                       22
CAPDP INVÁLIDO (NÃO PODE SER SUPERIOR A  ¤ 225.000,00)             23
CAP INVÁLIDO                                                       24
PRC INVÁLIDO                                                       25
AC INVÁLIDO                                                        26
CL INVÁLIDO                                                        27
PB INVÁLIDO                                                        28
CATV INVÁLIDA                                                      29
TC INVALIDO                                                        30
PTG INVÁLIDO                                                       31
NASC INVÁLIDO                                                      32
CARTA INVÁLIDA                                                     33
CV INVÁLIDA                                                        34
TB INVÁLIDO                                                        35
CLA INVÁLIDA                                                       36
TALT INVÁLIDO                                                      37
MALT INVÁLIDO                                                      38
TEST INVÁLIDO                                                      39
NSIN INVÁLIDO                                                      40
AGRAV INVÁLIDO                                                     41
NSEG INVÁLIDO                                                      42
PR  INVÁLIDO                                                       43
MARCA VEÍCULO OMITIDA                                              44
MATRICULA INVÁLIDA                                                 45
NR LUGARES INVÁLIDO                                                46
CODIGO ASSISTÊNCIA EM VIAGEM INVÁLIDO. CONSULTE A AJUDA.           47
DATA DA INCLUSÃO DA ASSISTÊNCIA EM VIAGEM OMITIDA                  48
CATEGORIA DO VEÍCULO INVÁLIDA. (VERIFIQUE CAMPOS Catv/Pb/Cl)       49
DATA INCLUSÃO É OBRIGATÓRIA SE TEM ASSIST.VIAGEM.                  50
DATA EXCLUSÃO OBRIGATORIA SE ANULOU A COBERTURA ASSIST.VIAG.       51
DATA EXCLUSÃO NÃO PODE SER INFERIOR À DATA DE INCLUSÃO.            52
SE INCLUI ASSIST. EM VIAGEM NÃO PODE PREENCHER DATA EXCLUSÃO       53
SE NÃO INCLUI ASSIST.VIAGEM O CÓDIGO TCALC DEVE SER ZERO.          54
SE INCLUI ASSIST.VIAGEM E TALT=0 ENTÃO TCALC DEVE SER 1.           55
SE INCLUI ASSIST.VIAGEM E TALT=6 ENTÃO TCALC DEVE SER 2.           56
SE INCLUI ASSIST.VIAGEM E TALT=5 ENTÃO TCALC DEVE SER 0 OU 2       57
SE INCLUI ASSIST.VIAGEM O CÓDIGO TCALC DEVE SER 0, 1, OU 2.        58
SE INCLUI ASSIST.VIAGEM E TALT=1 ENTÃO TCALC DEVE SER 0 OU 1       59
SE INCLUI ASSIST.VIAGEM E TALT=2, 3, OU 4 TCALC  DEVE SER 0.       60
SE INCLUI ASSIST.VIAGEM E TALT=7 ENTÃO TCALC DEVE SER 1            61
O RAMO 51 = OCUPANTES NÃO PODE TER ASSISTÊNCIA EM VIAGEM.          62
NÃO PODE PREENCHER DATA DE INCLUSÃO P/ O RAMO 51 = OCUPANTES       63
NÃO PODE PREENCHER DATA DE EXCLUSÃO P/ O RAMO 51 = OCUPANTES       64
*** PRÉMIOS EM DÍVIDA ***                                          65
NÃO É POSSÍVEL ATRIBUIR Nº A ESTA APÓLICE.   CONTACTE O C.I.       66
A ESTA APÓLICE FOI ATRIBUÍDO O Nº XX-XXXXXXX.   "FAÇA ENTER"       67
SE CR2 DIFERE DE ZERO ENTÃO CAPDP DEVE SER DIFERENTE DE ZERO       68
SE CAPDP DIFERE DE ZERO ENTÃO CR2 DEVE SER DIFERENTE DE ZERO       69
CONSULTA À APÓLICE REFLETE ALTERAÇÃO EXISTENTE NO CARREG.          70
EXISTE UMA ALTERAÇÃO A ESTA APÓLICE NESTE CARREGAMENTO ...         71
%DSCTO INVÁLIDA.                                                   72
INVÁLIDA CATEGORIA DE REBOQUE.                                     73
NÃO INDICOU A MATRÍCULA DO VEÍCULO-2.                              74
VALOR DOS EXTRAS NÃO PODE SER NEGATIVO                             75
CÓD. FRANQUIA INVÁLIDO                                             76
VALOR EM NOVO NÃO PODE SER =< ZERO.                                77
INVÁLIDA INDICAÇÃO DE ART5º.  (S/N)                                78
VALOR DO ART5º INVÁLIDO.                                           79
INDIQUE SÓMENTE O RAMO. PARA APÓLICE NOVA O Nº É AUTOMÁTICO.       80
     FAÇA ENTER PARA CONFIRMAR OS VALORES CALCULADOS.              81
           O PRCAGR CALCULADO DEVE SER INFERIOR AO ANTERIOR.       82
           O PRCAGR CALCULADO DEVE SER SUPERIOR AO ANTERIOR.       83
CODIGO INVÁLIDO. S=INCLUI P.JURID.; " "=NÃO INCLUI P.JURID.        84
O RAMO 51 = OCUPANTES NÃO PODE TER PROTECÇÃO JURÍDICA.             85
A PROTECÇÃO JURÍDICA, SÓ É VÁLIDA A PARTIR DE 01031999.            86
DATA INCLUSÃO É OBRIGATÓRIA SE TEM P.JURÍDICA.    (PJUR=S).        87
DATA EXCLUSÃO OBRIGATORIA SE ANULOU A COBERTURA P.JURÍDICA.        88
SE NÃO INCLUI P.JURÍDICA O CÓDIGO TCALC DEVE SER ZERO.             89
SE INCLUI P.JURÍDICA E TALT=0 ENTÃO TCALC DEVE SER 1.              90
SE INCLUI P.JURÍDICA E TALT=8 ENTÃO TCALC DEVE SER 2.              91
SE INCLUI P.JURÍDICA E TALT=5 ENTÃO TCALC DEVE SER 0 OU 2          92
SE INCLUI P.JURÍDICA O CÓDIGO TCALC DEVE SER 0, 1, OU 2.           93
SE INCLUI P.JURÍDICA E TALT=1 ENTÃO TCALC DEVE SER 0 OU 1.         94
SE INCLUI P.JURÍDICA E TALT=2, 3, OU 4 TCALC  DEVE SER 0.          95
SE INCLUI P.JURÍDICA E TALT=9 ENTÃO TCALC DEVE SER 1.              96
SE INCLUI P.JURÍDICA NÃO PODE PREENCHER DATA EXCLUSÃO              97
DATA EXCLUSÃO OBRIGATORIA SE ANULOU A COBERTURA P.JURÍDICA.        98
INVÁLIDA INDICAÇÃO PARA Q.I.V.                                     99
A APÓLICE REFª, NÃO PODE TER O MESMO Nº DESTA APÓLICE.            100
APÓLICE DE REFERÊNCIA INEXISTENTE.                                101
INVÁLIDO MEDIADOR EM JMM1.                                        102
COMISSÃO EM JMC1 NÃO PODE SER MAIOR QUE COM1.                     103
INDIQUE A COMMISSÃO PARA JMC1.                                    104
INVÁLIDO MEDIADOR EM JMM2.                                        105
COMISSÃO EM JMC2 NÃO PODE SER MAIOR QUE COM1.                     106
INDIQUE A COMMISSÃO PARA JMC2.                                    107
SOMA COMISÃO (JMC1+JMC2) NÃO PODE SER MAIOR QUE COM1.             108
MEDIADOR SEM COBRANÇA NÃO PODE SER INTRODUZIDO EM JMM1.           109
JMC1 NÃO TEM A INDICAÇÃO DO MEDIADOR EM JMM1.                     110
JMC2 NÃO TEM A INDICAÇÃO DO MEDIADOR EM JMM2.                     111
CÓD. CR HIPOTECÁRIO INEXISTENTENTE.                               112
NÃO ESTÁ AUTORIZADO AO INTRODUZIDO CAP-DP OU CAP-INICIAL.         113
CÓDIGO POSTAL INEXISTENTE.                                        114
INVÁLIDA INDICAÇÃO DE PRIVAÇÃO DE USO. CONSULTE A AJUDA.          115
PRIVAÇÃO DE USO SÓ É ACEITE COM COBERTURA DE DANOS PRÓPRIOS.      116
DEVE PREENCHER AMBOS OS CAMPOS.                                   117
COBERTURA DE OCUPANTES NÃO É VÁLIDA PARA MOTOCICLOS.              118
INVÁLIDA INDICAÇÃO DE CO-SEGURO. 0=NORMAL 1=CEDIDO 2=ACEITE       119
INVÁLIDO IMPRIME AVISO COBRANÇA. Deve ser "S" ou "N".             120
VALIDA MATRICULA? Deve ser "S" ou "N".                            121
COMB: B.iodiesel,D.iesel,E.léctrico,G.asolina,H.íbrido,GP.L       122
ADC INVÁLIDO                                                      123
NIB INVÁLIDO                                                      124
CÓDIGO EMPREGADO INVÁLIDO: branco ou S (sim)                      125
Seguro Novo-não pode preencher ADC e NIB                          126
CAMPO CC (CILINDRADA) TEM DE SER PREENCHIDO.                      127
TIPO DE PRIVAÇÃO DE USO INCOERENTE COM A CILINDRADA.              128
ESTA COBERTURA SÓ É PERMITIDA EM CONJUNTO COM ASS.VIAGEM          129
CÓD. FRANQUIA INVÁLIDO COM CAPINI > 50.000,00                     130
CATV INVÁLIDA COM ESTA FRANQUIA                                   131
CÓD. FRANQUIA INVÁLIDO VIATURA COM + 8 ANOS                       132
CÓD. FRANQUIA INVÁLIDO SEGURADO < 25 ANOS                         133
CÓD. FRANQUIA INVÁLIDO CARTA < 2 ANOS                             134
** CAPITAL RC
01800
02500
06000
99999
** FRANQUIA
00
02
04
06
09
10
** PERC.FRANQ. ATÉ 8 ANOS
020
040
080
120
000
200
** PERC.FRANQ. +8 ANOS
030
060
120
180
000
300
** BONUS/PERCENTAGEM
1030
2000
3000
4000
5035
6040
7000
8045
9050
**      TAB PERCENTAGENS COMISSÕES DOS RAMOS
50104160625001250
51124990833301250
**      TABELA MES/DIAS
0131
0228
0331
0430
0531
0630
0731
0831
0930
1031
1130
1231
** RISCOS DP
3
4
5
6
7
8
**
S/MATRICULA
S/GARAGISTA
VARIAS
**
012
024
036
048
060
072
084
096
108
