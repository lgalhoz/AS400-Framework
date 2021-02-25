     H DFTNAME(TRSF05P2) DATEDIT(*DMY/) FIXNBR(*ZONED)
     H********************************************************************
     H*  EMPRESA    :  GRUPO MATA.                                       *
     H*  PROGRAMA   :  TRSF05P2                                          *
     H*  TIPO       :  SRT                                               *
     H*  APLICAÇÃO  :  PRODUÇAO GUARDIAN (EXPLORAÇAO TODOS OS RAMOS)     *
     H*  FUNÇÃO        - CRIAÇÃO DE FICHEIRO PARA TRANSFERÊNCIA DE       *
     H*                  INFORMAÇÃO (DADOS) PARA A LUSITANIA "VERDE"     *
     H*                                                                  *
     H*                  * FICHEIRO DE APÓLICES E COBERTURAS *           *
     H*                                                                  *
     H*  AUTOR      :  PAULO CORREIA                                     *
     H*  DATA       :  01/04/2000                                        *
     H*==================================================================*
     H*                    REGISTO DE MANUTENÇÃO                         *
     H*                                                                  *
     H*   DATA     ABREV.   DESCRIÇÃO DA ALTERAÇÃO                       *
     H* --------   ------   ----------------------                       *
     H* 2010-11-02 L.GALHOZ Limpar Coberturas e Prémios nas apólices mãe *
     H*                     para evitar duplicação nas frotas.           *
     H*                                                                  *
     H********************************************************************
     FGBASEDIO  IP   F  700     9AIDISK    KEYLOC(1)
     FGBASEDIA  IS   F  700     9AIDISK    KEYLOC(1)
     FGBASEGRO  IF   F  700     9AIDISK    KEYLOC(1) EXTFILE('GBASEGRO')
     FGBASEGRA  IF   F  700     9AIDISK    KEYLOC(1) EXTFILE('GBASEGRA')
     FFTABX     IF   F  274     7AIDISK    KEYLOC(1) EXTFILE('FTAB')
     FLFAPOL    IF A F  403    17AIDISK    KEYLOC(360)
     FLFCOBE    O  A F  384    17AIDISK    KEYLOC(360)
     F********************************************************************
     F*              RESUMO DA UTILIZAÇÃO DOS INDICADORES                *
     F********************************************************************
     F*                                                                  *
     F*  13    IDENTIFICA REGISTO DE APOLICES - AUTOMÓVEL                *
     F*  14    IDENTIFICA REGISTO DE APÓLICES - OUTROS RAMOS             *
     F*                                                                  *
     F*                                                                  *
     F********************************************************************
      *
T01  D RMG             S              2  0 DIM(24) CTDATA PERRCD(1)             RAMO/MOD/SMDO GAL VS
T01  D RML             S              6  0 DIM(24) ALT(RMG)
T02  D NTG             S              1  0 DIM(3) CTDATA PERRCD(1)              NATUREZA SEG. GAL VS
T02  D NTL             S              1  0 DIM(3) ALT(NTG)
T03  D FRG             S              1  0 DIM(5) CTDATA PERRCD(1)              FRACIONAMENTO GAL VS
T03  D FRL             S              1  0 DIM(5) ALT(FRG)
T04  D CUG             S              3  0 DIM(17) CTDATA PERRCD(1)             COD.ANULAÇÃO  GAL VS
T04  D CUL             S              2  0 DIM(17) ALT(CUG)
T05  D TABMM           S              2  0 DIM(12) CTDATA PERRCD(1) ASCEND      DIA/MES VENCIMENTO
T05  D TABDT           S              4  0 DIM(12) ALT(TABMM)
T06  D W1G             S             14    DIM(11) CTDATA PERRCD(1) ASCEND      MARCAS INVÁLIDAS
T06  D W1L             S             14    DIM(11) ALT(W1G)
T07  D W2G             S             11    DIM(11) CTDATA PERRCD(1) ASCEND      MATRICULAS INVÀLIDAS
T07  D W2L             S             11    DIM(11) ALT(W2G)
T08  D CVG             S              3  0 DIM(20) CTDATA PERRCD(1) ASCEND      CATEGORIAS DE VEÍCUL
T08  D CVL             S              4  0 DIM(20) ALT(CVG)
T09  D AIG             S              2  0 DIM(3) CTDATA PERRCD(1) ASCEND       AMBITO DE INCENDIO
T09  D AIL             S              1  0 DIM(3) ALT(AIG)
T10  D RIG             S              3  0 DIM(80) CTDATA PERRCD(1) ASCEND      RISCOS VS COBERTURAS
T10  D RIL             S              3  0 DIM(80) ALT(RIG)
T11  D C01G            S              4  0 DIM(60) CTDATA PERRCD(1) ASCEND      CLAUSULAS VS COBERTU
T11  D C01L            S              3  0 DIM(60) ALT(C01G)
T12  D C06G            S              4  0 DIM(42) CTDATA PERRCD(1) ASCEND      CLAUSULAS VS COBERTU
T12  D C06L            S              3  0 DIM(42) ALT(C06G)
T13  D C05G            S              4  0 DIM(30) CTDATA PERRCD(1) ASCEND      CLAUSULAS VS COBERTU
T13  D C05L            S              3  0 DIM(30) ALT(C05G)
T14  D C17G            S              4  0 DIM(24) CTDATA PERRCD(1) ASCEND      CLAUSULAS VS COBERTU
T14  D C17L            S              3  0 DIM(24) ALT(C17G)
T15  D C19G            S              4  0 DIM(6) CTDATA PERRCD(1) ASCEND       CLAUSULAS VS COBERTU
T15  D C19L            S              3  0 DIM(6) ALT(C19G)
T16  D C55G            S              4  0 DIM(36) CTDATA PERRCD(1) ASCEND      CLAUSULAS VS COBERTU
T16  D C55L            S              3  0 DIM(36) ALT(C55G)
T17  D OCUG            S              1  0 DIM(3) CTDATA PERRCD(1) ASCEND       TP PES. TRANSPORTADA
T17  D OCUL            S              1    DIM(3) ALT(OCUG)
T18  D MODG            S              5  0 DIM(3) CTDATA PERRCD(1) ASCEND       MODULO OCUPANTES
T18  D MODL            S              2    DIM(3) ALT(MODG)
T19  D CPRIV           S              1    DIM(6) CTDATA PERRCD(1) ASCEND       PRIVAÇÃO USO
T19  D DPRIV           S              2  0 DIM(6) ALT(CPRIV)
T20  D TABM            S              2  0 DIM(12) CTDATA PERRCD(12) ASCEND     MESES/DIAS
T20  D TABD            S              2  0 DIM(12) ALT(TABM)
T21  D PROFJ           S              3  0 DIM(91) CTDATA PERRCD(1) ASCEND      PROFISSÕES A.T.
T21  D PROFL           S              4  0 DIM(91) ALT(PROFJ)
      *
T22  D C05GR           S              4  0 DIM(9) CTDATA PERRCD(1) ASCEND       RESIDENTIA
T22  D C05LR           S              3  0 DIM(9) ALT(C05GR)
T23  D C05GC           S              4  0 DIM(9) CTDATA PERRCD(1) ASCEND       RESIDENTIA (CONDOMIN
T23  D C05LC           S              3  0 DIM(9) ALT(C05GC)
T24  D C55GM           S              4  0 DIM(13) CTDATA PERRCD(1) ASCEND      MERCANTILE
T24  D C55LM           S              3  0 DIM(13) ALT(C55GM)
T25  D CAT354          S              9  0 DIM(2) CTDATA PERRCD(1)
T28  D COMBG           S              1    DIM(2) CTDATA PERRCD(1)
T28  D COMBL           S              1    DIM(2) ALT(COMBG)
T29  D COMBG2          S              1    DIM(7) CTDATA PERRCD(1)
T29  D COMBL2          S              1    DIM(7) ALT(COMBG2)
     D********************************************************************
     D* ESTRUTURA DE DADOS - LOCAL DATA AREAS                            *
     D********************************************************************
     DJMLDA            DS                  DTAARA(*LDA)
     D  UDTHH                 15     26  0
     D  UHORA                 15     20  0
     D  UDATA                 21     26  0
     D  UDDDT                 21     22  0
     D  UMMDT                 23     24  0
     D  UAADT                 25     26  0
     D  UFICH                100    107
     D  UALTER               108    113  0
     D  UDDALT               108    109  0
     D  UMMALT               110    111  0
     D  UAAALT               112    113  0
     D  LDA$NUMMED           114    119  0
     D*
     D                 DS
     D  UDTTRF                15     26  0
     D  UATRF                 21     22  0
     D  UMTRF                 23     24  0
     D  UDTRF                 25     26  0
     D                 DS
     D  LURMS                  1      6
     D  LURR                   1      2
     D  LUMOD                  3      4
     D  LUSMOD                 5      6
     D                 DS
     D  WDTALT                 1      6  0
     D  AAALT                  1      2  0
     D  MMALT                  3      4  0
     D  DDALT                  5      6  0
     D                 DS
     D  WDTINI                 1      6  0
     D  AAINIC                 1      2  0
     D  MMINIC                 3      4  0
     D  DDINIC                 5      6  0
     D                 DS
     D  WDTFIM                 1      6  0
     D  AAFIM                  1      2  0
     D  MMFIM                  3      4  0
     D  DDFIM                  5      6  0
     D                 DS
     D  WDTVEN                 1      6  0
     D  AAVEN                  1      2  0
     D  MMVEN                  3      4  0
     D  DDVEN                  5      6  0
     D                 DS
     D  WDTMAT                 1      6  0
     D  AAMATR                 1      2  0
     D  MMMATR                 3      4  0
     D  DDMATR                 5      6  0
     D                 DS
     D  WALTER                 1      6  0
     D  AALTER                 1      2  0
     D  MALTER                 3      4  0
     D  DALTER                 5      6  0
     D                 DS
     D  WR12                   1      2  0
     D  WR1                    1      1  0
     D  WR2                    2      2  0
     D                 DS
     D  DATA$                  1      8  0
     D  AA$                    1      4  0
     D  MM$                    5      6  0
     D  DD$                    7      8  0
     D                 DS
     D  CF                     1     13
     D                                     DIM(13)
     D  CF01                   1      1
     D  CF02                   2      2
     D  CF03                   3      3
     D  CF04                   4      4
     D  CF05                   5      5
     D  CF06                   6      6
     D  CF07                   7      7
     D  CF08                   8      8
     D  CF09                   9      9
     D  CF10                  10     10
     D  CF11                  11     11
     D  CF12                  12     12
     D  CF13                  13     13
     D                 DS
     D  KTAB                   1      7
     D  TPTAB                  1      2
     D  CDTAB                  3      7
     I****************************************************
     I* FICHEIRO - DIA  DE APÓLICES ( OUTROS RAMOS )     *
     I****************************************************
     IGBASEDIO  AA  14   10 CO
     I                                  1    2 0RR
     I                                  3    9 0APOL
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
     I*                                     199 200 FILLER
     I                             P  201  203 0MED2
      *
     I                                206  206 0TALT
     I                                207  208 0DDALT
     I                                209  210 0MMALT
     I                                211  212 0AAALT
     I                                213  214 0MALT
     I                             P  215  216 0ACTA
     I                                217  217 0CR1
     I                             P  218  223 0CAP1
     I                             P  224  226 4TX1
     I                             P  227  231 2PR1
     I                                232  232 0CR2
     I                             P  233  238 0CAP2
     I                             P  239  241 4TX2
     I                             P  242  246 2PR2
     I                                247  247 0CR3
     I                             P  248  253 0CAP3
     I                             P  254  256 4TX3
     I                             P  257  261 2PR3
     I                             P  262  267 0WCAPT
     I                             P  268  270 4TXMED
     I                             P  271  275 2PRSACT
     I                             P  276  279 0FRANQO
     I                                280  324  LRPS
     I                             P  325  326 1TXIND
     I                             P  327  328 2TXSNB
     I                                329  330 0NASC
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
     I                                532  532  RECS                            RECIBO
     I*                                                                    'S' =
     I                                535  535 0CGRUPO
     I                             P  536  538 0NPESS
     I                                539  539  TPRISC
     I                                540  541  CLTAR
     I                                542  542  PERIOD
     I                                543  543  CONTR
     I                             P  544  545 0PCONTR
     I*                                                                    'N' =
     I                             P  596  599 0CPMOR1
     I                             P  600  603 0CPLCB1
     I* cedência de Comissões em JM
     I                                609  611 0JMM1                            CEDÊNC
     I                             P  612  614 3JMC1                            CEDÊNC
     I                                615  617 0JMM2                            CEDÊNC
     I                             P  618  620 3JMC2                            CEDÊNC
      *
     I* novos campos dos novos produtos RESIDENTIA, MERCANTILE e CONDOMINIO
     I                                204  204  CONDOM
     I                                205  205  NEWPRD
     I* coberturas facultativas
     I***                                   568 579 CF
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
     I* cláusulas diversas
     I                                587  587  CD103
     I                                588  588  CD130
     I                                589  589  CD131
     I                                590  590  CD132
     I                                591  591  CD133
     I                                592  592  CD134
     I                                593  593  CD135
     I                                594  594  CD136
     I                                595  595  CD137
      *
     I****************************************************
     I* FICHEIRO - BASE DE APÓLICES ( OUTROS RAMOS )     *
     I****************************************************
     IGBASEGRO  AA  24   10 CO
     I                                  1    2 0RR
     I                                  3    9 0APOL
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
     I                                178  178 0ZSIT
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
     I*                                     199 200 FILLER
     I                             P  201  203 0MED2
     I*                                     204 205 FILLER
     I                                206  206 0TALT
     I                                207  208 0DDALT
     I                                209  210 0MMALT
     I                                211  212 0AAALT
     I                                213  214 0MALT
     I                             P  215  216 0ACTA
     I                                217  217 0CR1
     I                             P  218  223 0CAP1
     I                             P  224  226 4TX1
     I                             P  227  231 2PR1
     I                                232  232 0CR2
     I                             P  233  238 0CAP2
     I                             P  239  241 4TX2
     I                             P  242  246 2PR2
     I                                247  247 0CR3
     I                             P  248  253 0CAP3
     I                             P  254  256 4TX3
     I                             P  257  261 2PR3
     I                             P  262  267 0WCAPT
     I                             P  268  270 4TXMED
     I                             P  271  275 2PRSACT
     I                             P  276  279 0FRANQO
     I                                280  324  LRPS
     I                             P  325  326 1TXIND
     I                             P  327  328 2TXSNB
     I                                329  330 0NASC
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
     I                                532  532  RECS                            RECIBO
     I*                                                                    'S' =
     I                                535  535 0CGRUPO
     I                             P  536  538 0NPESS
     I                                539  539  TPRISC
     I                                540  541  CLTAR
     I                                542  542  PERIOD
     I                                543  543  CONTR
     I                             P  544  545 0PCONTR
     I*
     I                             P  596  599 0CPMOR1
     I                             P  600  603 0CPLCB1
     I* cedência de Comissões em JM
     I                                609  611 0JMM1                            CEDÊNC
     I                             P  612  614 3JMC1                            CEDÊNC
     I                                615  617 0JMM2                            CEDÊNC
     I                             P  618  620 3JMC2                            CEDÊNC
      *
     I* novos campos dos novos produtos RESIDENTIA, MERCANTILE e CONDOMINIO
     I                                204  204  CONDOM
     I                                205  205  NEWPRD
     I* coberturas facultativas
     I***                                   568 579 CF
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
     I* cláusulas diversas
     I                                587  587  CD103
     I                                588  588  CD130
     I                                589  589  CD131
     I                                590  590  CD132
     I                                591  591  CD133
     I                                592  592  CD134
     I                                593  593  CD135
     I                                594  594  CD136
     I                                595  595  CD137
      *
     I****************************************************
     I* FICHEIRO - DIA  DE APÓLICES ( AUTOMOVEL )        *
     I****************************************************
     IGBASEDIA  BB  13   10 CA
     I                                  1    2 0RR
     I                                  3    9 0APOL
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
     I*                                     199 200 FILLER
     I                             P  201  203 0MED2
     I*                                     204 205 FILLER
     I                                206  206 0TALT
     I                                207  208 0DDALT
     I                                209  210 0MMALT
     I                                211  212 0AAALT
     I                                213  214 0MALT
     I                             P  215  216 0ACTA
     I                                217  217 0CR1
     I                             P  218  220 0CAPRC
     I                             P  221  225 2PR1
     I                                226  226 0CR2
     I*                                     227 229 FILLER
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
     I                                311  312 0NASC
     I                                313  314  CARTA
     I*                                     315 315 FILLER
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
     I*                                     429 431 FILLER
     I                                432  437 0DTFIM
     I                                432  433 0DDFIM
     I                                434  435 0MMFIM
     I                                436  437 0AAFIM
     I                                438  438 0TEST
     I                             P  439  440 0TEXTO
     I                                441  446 0DTMATR
     I                                441  442 0DDMATR
     I                                443  444 0MMMATR
     I                                445  446 0AAMATR
     I*                                     447 449 FILLER
     I                                450  450  ART5
     I                             P  451  453 0NSEG
     I                                454  459 0DTART5
     I                                460  460  VSA
     I*                                           1 - ¤  32,86 (AV.ATÉ 1900C
     I*                                           2 - ¤  45,72 (AV.ATÉ 2500C
     I                                461  461  VIA
     I*                                           1 - ¤  16,43 (AC.ATÉ 1900C
     I*                                           2 - ¤  23,58 (AC.ATÉ 2500C
     I*                               462  462  FILLER
     I                             P  463  464 1DSCESP
     I                                465  465  COMB
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
     I                                518  518 0CR3
     I                             P  519  521 0CAPOCU
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
     I                                579  587 0APREF
     I                                588  588  QIV
     I                             P  589  591 0CILIND
      * mudou posição                     P 589 5912PRQIV
     I                             P  639  642 0VLART5
     I*==> [LG] Incluir os campos FROTA e APMAE
     I                                654  654  FROTA
     I                                657  663 0APMAE
     I                             P  664  667 2PRQIV
     I                                592  592  PRIV
     I*                                               1 - ¤ Pr=28,50 (15D Cap=23
     I*                                               2 - ¤ "  44,53 (15D  "  37
     I*                                               3 - ¤ "  69,46 (15D  "  50
     I*                                               4 - ¤ "  44,53 (30D  "  23
     I*                                               5 - ¤ "  66,79 (30D  "  37
     I*                                               6 - ¤ " 111,32 (30D  "  50
      * mudou posição                     P 593 5952PRPRIV
     I                             P  668  671 2PRPRIV
     I                             P  596  599 0CPMOR1
     I                             P  600  603 0CPLCB1
     I* cedência de Comissões em JM
     I                                609  611 0JMM1                            CEDÊNC
     I                             P  612  614 3JMC1                            CEDÊNC
     I                                615  617 0JMM2                            CEDÊNC
     I                             P  618  620 3JMC2                            CEDÊNC
     I*
     I                             P  621  624 0CAPDP                           CEDÊNC
     I                             P  625  628 0VLNOVO
     I*
     I****************************************************
     I* FICHEIRO - BASE DE APÓLICES ( AUTOMOVEL )        *
     I****************************************************
     IGBASEGRA  AA  23   10 CA
     I                                  1    2 0RR
     I                                  3    9 0APOL
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
     I                                178  178 0ZSIT
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
     I*                                     199 200 FILLER
     I                             P  201  203 0MED2
     I*                                     204 205 FILLER
     I                                206  206 0TALT
     I                                207  208 0DDALT
     I                                209  210 0MMALT
     I                                211  212 0AAALT
     I                                213  214 0MALT
     I                             P  215  216 0ACTA
     I                                217  217 0CR1
     I                             P  218  220 0CAPRC
     I                             P  221  225 2PR1
     I                                226  226 0CR2
     I*                                     227 229 FILLER
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
     I                                311  312 0NASC
     I                                313  314  CARTA
     I*                                     315 315 FILLER
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
     I*                                     429 431 FILLER
     I                                432  437 0DTFIM
     I                                432  433 0DDFIM
     I                                434  435 0MMFIM
     I                                436  437 0AAFIM
     I                                438  438 0TEST
     I                             P  439  440 0TEXTO
     I                                441  446 0DTMATR
     I                                441  442 0DDMATR
     I                                443  444 0MMMATR
     I                                445  446 0AAMATR
     I*                                     447 449 FILLER
     I                                450  450  ART5
     I                             P  451  453 0NSEG
     I                                454  459 0DTART5
     I                                460  460  VSA
     I*                                           1 - ¤  32,86 (AV.ATÉ 1900C
     I*                                           2 - ¤  45,72 (AV.ATÉ 2500C
     I                                461  461  VIA
     I*                                           1 - ¤  16,43 (AC.ATÉ 1900C
     I*                                           2 - ¤  23,58 (AC.ATÉ 2500C
     I*                               462  462  FILLER
     I                             P  463  464 1DSCESP
     I                                465  465  COMB
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
     I                                579  587 0APREF
     I                                588  588  QIV
     I                             P  589  591 0CILIND
      * mudou posição                     P 589 5912PRQIV
     I                             P  639  642 0VLART5
     I*==> [LG] Incluir os campos FROTA e APMAE
     I                                654  654  FROTA
     I                                657  663 0APMAE
     I                             P  664  667 2PRQIV
     I                                592  592  PRIV
     I*                                               1 - ¤ Pr=28,50 (15D Cap=23
     I*                                               2 - ¤ "  44,53 (15D  "  37
     I*                                               3 - ¤ "  69,46 (15D  "  50
     I*                                               4 - ¤ "  44,53 (30D  "  23
     I*                                               5 - ¤ "  66,79 (30D  "  37
     I*                                               6 - ¤ " 111,32 (30D  "  50
      * mudou posição                     P 593 5952PRPRIV
     I                             P  668  671 2PRPRIV
     I                             P  596  599 0CPMOR1
     I                             P  600  603 0CPLCB1
     I* cedência de Comissões em JM
     I                                609  611 0JMM1                            CEDÊNC
     I                             P  612  614 3JMC1                            CEDÊNC
     I                                615  617 0JMM2                            CEDÊNC
     I                             P  618  620 3JMC2                            CEDÊNC
     I*
     I                             P  621  624 0CAPDP                           CEDÊNC
     I                             P  625  628 0VLNOVO
     I*
     I****************************************************
     I* FICHEIRO - APÓLICES ENVIADAS                     *
     I****************************************************
     ILFAPOL    NS
     I*
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
     I                             P  176  179 2CAPIT1
     I                             P  180  183 2CAPIT2
     I                             P  184  185 0NODIAS
     I                                186  186  SIT1
     I                                187  266  DCTAB3
     I                             P  267  270 2VALOR3
     I                             P  271  274 2CAPIT3
     C********************************************************************
     C* DETERMINA DATA DE TRANSFERÊNCIA                                  *
     C********************************************************************
     C*
     C                   IN        JMLDA
     C*
     C     FLAG          IFEQ      0                                            <-----------+
     C                   TIME                    UDTHH                                      I
     C                   Z-ADD     1             FLAG              1 0                      I
     C*                                                                I
     C* FORMATA DATA DA TRANSFERÊNCIA                                  I
     C*                                                                I
     C                   MOVE      UDDDT         UDTRF                                      I
     C                   MOVE      UMMDT         UMTRF                                      I
     C                   MOVE      UAADT         UATRF                                      I
     C*                                                                I
     C                   Z-ADD     *ZEROS        DTY2              8 0                      I
     C                   MOVE      UDTTRF        DTY2                                       I
     C                   EXSR      SRDTY2                                                   I
     C                   MOVE      DTY2          LDTTRF            8 0                      I
     C*                                                                I
     C                   MOVE      UFICH         LUFICH            8                        I
     C                   MOVE      UDDALT        DALTER                                     I
     C                   MOVE      UMMALT        MALTER                                     I
     C                   MOVE      UAAALT        AALTER                                     I
     C                   END                                                    <-----------+
     C*
     C********************************************************************
     C* CONTROLA INPUT PARA NAO SELECCIONAR OS REGITOS DOS RAMOS         *
     C*                                                                  *
     C*  10: VIAGENS  67: BAGAGEM  68: MARÍTIMO  69: POSTAL  70: AÉREO   *
     C*                                                                  *
     C********************************************************************
      *
     C     RR            IFEQ      10
     C                   GOTO      FIM
     C                   ELSE
     C     RR            IFEQ      67
     C                   GOTO      FIM
     C                   ELSE
     C     RR            IFEQ      68
     C                   GOTO      FIM
     C                   ELSE
     C     RR            IFEQ      69
     C                   GOTO      FIM
     C                   ELSE
     C     RR            IFEQ      70
     C                   GOTO      FIM
     C                   ELSE
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     F********************************************************************
     F* LIMPA CAMPOS DE TRABALHO  - FICHEIRO DE APÓLICES                 *
     F********************************************************************
     C*
     C* DADOS DA APÓLICE
     C*
     C                   MOVE      *BLANKS       LRAMO             2
     C                   MOVE      *BLANKS       LMOD              2
     C                   MOVE      *BLANKS       LSMOD             2
     C                   MOVE      *BLANKS       LAPOL             7
     C                   MOVE      *BLANKS       LNSEG             6 0
     C                   MOVE      *BLANKS       LNATS             1
     C                   MOVE      *ZEROS        LMMDDV            4 0
     C                   MOVE      *BLANKS       LFRAC             1
     C                   MOVE      *BLANKS       LCOBR             5
     C                   MOVE      *ZEROS        LDTUAT            8 0
     C                   MOVE      *BLANKS       LANUL             2
     C                   MOVE      *ZEROS        LDTANU            8 0
     C                   MOVE      *ZEROS        LDTINI            8 0
     C                   MOVE      *ZEROS        LDTFIM            8 0
     C                   MOVE      *BLANKS       LCONC             4
     C                   MOVE      *BLANKS       LLRPS            70
     C*
     C* ACIDENTES DE TRABALHO
     C*
     C                   MOVE      *BLANKS       LACTAT            6            * ACTIVIDADE (ACI.TR
     C                   MOVE      *BLANKS       LTPACT            1            * TIPO SEGURO(FIXO /
     C                   MOVE      *BLANKS       LPROF             4            * PROFISSÕES
     C*
     C* ACIDENTES PESSOAIS
     C                   MOVE      '0'           LTPACP            1            * TIPO DE SEGURO (AC
     C                   MOVE      '0'           LTRISC            1            *
     C                   MOVE      '00'          LCLTAR            2            *
     C                   MOVE      '0'           LPERIO            1            *
     C                   MOVE      'N'           LCONTR            1            *
     C                   MOVE      '0'           LPCONT            1            *
     C                   Z-ADD     0             LNPESS            6 0          *
     C                   Z-ADD     0             CAPTOT           13 2          *
     C*
     C* SEGURO AUTOMÓVEL
     C*
     C                   MOVE      *BLANKS       LTPAUT            2            * TIPO DE SEGURO  (A
     C                   MOVE      *BLANKS       LCATV             4            * CATEGORIA DO VEÍCU
     C                   MOVE      *BLANKS       LMARCA           20            * MARCA
     C                   MOVE      *BLANKS       LMATR            12            * MATRÍCULA
     C                   MOVE      *ZEROS        LDTMAT            8 0          * DATA MATRÍCULA
     C                   MOVE      *BLANKS       LCODCC            2
     C                   MOVE      *BLANKS       LCOMB             1
     C                   MOVE      *BLANKS       LFRQIV           11 2
     C*
     C* SEGURO INCENDIO
     C*
     C                   MOVE      *BLANKS       LFUNC             1            * FUNCAO
     C                   MOVE      *BLANKS       LACTIN            5            * ACTIVIDADE  (INCEN
     C                   MOVE      *BLANKS       LTPCON            1            * TIPO CONSTRUÇAO
     C                   MOVE      *BLANKS       LAMBIN            1            * AMBITO (INCÊNDIO)
     C*
     C* SEGURO MULTI-RISCOS HABITACAO
     C*
     C                   MOVE      *BLANKS       LAMBMH            1            * AMBITO (M.RISCOS H
     C*
     C* SEGURO MULTI-RISCOS COMERCIAL
     C*
     C                   MOVE      *BLANKS       LAMBMC            1            * AMBITO
     C                   MOVE      *BLANKS       LACTMC            4            * ACTIVIDADE
     C*
     C* SEGURO RC COMÉRCIO/INDÚSTRIA
     C*
     C                   MOVE      *BLANKS       LNISP             9            * AMBITO
     C*
     C* LIMPEZA DE CAMPOS ADICIONAIS GIL
     C*
     C                   MOVE      *ZEROS        LALTER            8 0          * DATA ALTERACAO (PR
     C                   MOVE      *BLANKS       LMATR2           12            * MATRÍCULA 2º VEÍCU
     C                   MOVE      *BLANKS       LMARC2           20            * MARCA     2º VEÍCU
     C*
     C* LIMPEZA DE CAMPOS DE CONTROLE GIL
     C*
     C                   MOVE      *BLANKS       LRRAP             9
     C*
     C* LIMPEZA DE CAMPOS DE TRABALHO
     C*
     C                   Z-ADD     *ZEROS        DTY2              8 0
     C                   Z-ADD     *ZEROS        WCONC             4 0
     C*
     C* SEGURO AUTOMÓVEL (INFORMAÇÃO OCUPANTES)
     C*
     C                   MOVE      *BLANKS       LTPOCU            1            * TIPO PES. TRANSPOR
     C                   MOVE      *BLANKS       LMODUL            2            * MODULO GARANTIAS
     F********************************************************************
     F* LIMPA CAMPOS DE TRABALHO  - FICHEIRO DE COBERTURAS               *
     F********************************************************************
     C*
     C                   MOVE      *BLANKS       LCOBER            3
     C                   Z-ADD     *ZEROS        LCAPT            13 2
     C                   Z-ADD     *ZEROS        LPRSI            11 2
     C*
     C                   Z-ADD     *ZEROS        LCAP1            13 2
     C                   Z-ADD     *ZEROS        LCAP2            13 2
     C                   Z-ADD     *ZEROS        LPRS1            11 2
     C                   Z-ADD     *ZEROS        LPRS2            11 2
     C                   Z-ADD     *ZEROS        LPRCOM           11 2          * PREMIO COMERCIAL
     C                   Z-ADD     *ZEROS        LCAPPI           13 2          * CAPITAL PERDAS IND
     C                   Z-ADD     *ZEROS        WPI               5 3          * CAPITAL PERDAS IND
     C                   MOVE      *BLANKS       LCOSEG            1
     C                   Z-ADD     *ZEROS        WCAP2            13 0
     C                   Z-ADD     *ZEROS        LPRS1C           11 2
     C                   Z-ADD     *ZEROS        LPRS2C           11 2
     C                   Z-ADD     *ZEROS        WPRSC            11 2
     C*
     C* SEGURO AUTOMÓVEL (PRIVAÇÃO DE USO )
     C*
     C                   MOVE      *BLANKS       LNDIAS            2            * N.DIAS    TRANSPOR
     C*
     F********************************************************************
     F* CONTROLA REGISTOS A CONVERTER E A ENVIAR                         *
     F********************************************************************
     C*
     C* FORMATA NOME DE FICHEIRO
     C*
     C*          REF       IFEQ 'O'
     C*                    ELSE
     C*                    MOVEL'GBDIA'   UFICH
     C*                    MOVE UFICH     LUFICH
     C*                    END
     C*
     C
     C*--- REF           IFEQ      'O'
     C*---               MOVE      'GBASEGRO'    LUFICH
     C*---               ELSE
     C*---               MOVE      'GBASEGRA'    LUFICH
     C*---               END
     C
     C                   MOVEL     RRAPOL        KEYAP            17
     C                   MOVE      LUFICH        KEYAP
     C     KEYAP         CHAIN     LFAPOL                             90
     C  N90              GOTO      FIM
     C   90
     CAN 14RRAPOL        CHAIN     GBASEGRO                           91
     C   90
     CAN 13RRAPOL        CHAIN     GBASEGRA                           91
      *
      * NÃO ESTÁ ANULADA NO GBASEGR
     C   U1              DO
     C  N91SIT           IFNE      3
     C                   GOTO      FIM
     C                   END
     C                   END
     C********************************************************************
     C* CONVERSAO DE CAMPOS - REGRAS                                     *
     C********************************************************************
     C*
     C* CONVERTE RAMOS  ( RAMO GUARDIAN VS LUSITANIA ).
     C*
     C                   Z-ADD     1             I                 2 0
     C     RR            IFEQ      15
     C     TPT           IFEQ      'P'
     C                   Z-ADD     13            I
     C                   END
     C                   END
     C*
     C     RR            LOOKUP    RMG(I)                                 20
     C   20              MOVE      RML(I)        LURMS
     C  N20              MOVE      *BLANKS       LURMS
     C*
     C     RR            IFEQ      04
     C     CL01          IFEQ      24
     C                   MOVE      130400        LURMS
     C                   END
     C                   END
     C*
     C     RR            IFEQ      05
     C     NEWPRD        IFNE      ' '
     C     CONDOM        IFNE      ' '
     C                   MOVE      340103        LURMS
     C                   END
     C                   END
     C                   END
     C*
     C                   MOVE      LURR          LRAMO
     C                   MOVE      LUMOD         LMOD
     C                   MOVE      LUSMOD        LSMOD
     C*
     C                   MOVE      APOL          LAPOL
     C*
     C* COMPOE CÓDIGO PESSOA
     C*
     C                   MOVEL     9             LNSEG
     C                   MOVE      NSEG          LNSEG
     C*
     C* CONVERTE NATUREZA DOS SEGUROS
     C*
     C                   Z-ADD     1             I                 2 0
     C     DUR           LOOKUP    NTG(I)                                 20
     C   20              MOVE      NTL(I)        LNATS
     C  N20              MOVE      *BLANKS       LNATS
     C*
     C* FORÇA A NATUREZA DO SEGURO SE APOLICE TEM INDEXAÇÃO (O.RAMOS)
     C*
     C   14DUR           IFEQ      1
     C     INDEX         IFEQ      'S'
     C                   MOVE      '2'           LNATS
     C                   END
     C                   END
     C*
     C* FORMATA VENCIMNENTO (DATA DE VENCIMENTO COMPLETA)
     C*
     C                   EXSR      SRDTVC
     C*
     C* FORMATA VENCIMENTO  (DIA E MES VENCIMENTO)
     C*
     C     MES1          IFEQ      MMINIC
     C                   MOVEL     MMINIC        LMMDDV
     C                   MOVE      DDINIC        LMMDDV
     C                   ELSE
     C                   MOVEL     MES1          LMMDDV
     C                   MOVE      DDINIC        LMMDDV
     C                   END
     C*
     C* FORMATA FRACCIONAMENTO
     C*
     C                   Z-ADD     1             I
     C     FRAC          LOOKUP    FRG(I)                                 20
     C   20              MOVE      FRL(I)        LFRAC
     C  N20              MOVE      *BLANKS       LFRAC
     C*
     C* FORÇA A FORMATAÇÃO DO VENCIMNENTO (DIA E MES VENCIMENTO)
     C* PARA AS APÓLICES TEMPORÁRIAS JÁ QUE ESTAS TÉM O MES1,
     C* MMCONT E AACONT PREENCHIDOS COM A CONSTANTE "99".
     C*
     C     LFRAC         IFEQ      '9'
     C                   MOVEL     MMINIC        LMMDDV
     C                   MOVE      DDINIC        LMMDDV
     C                   END
     C*
     C* FORMATA COBRADOR ( ESTE CAMPO VAI A BRANCOS )
     C*  - O COBRADOR SERÁ O NUMERO DE AGENTE JOSÈ MATA NA LUSITANIA
     C*  - ESTE CAMPO SERÁ PREENCHIDO PELA PROPRIA LUSITANIA
     C*
     C                   MOVE      *BLANKS       LCOBR
     C*
     C* FORMATA DATA DA ULTIMA ACTA.
     C*
     C     ACTA          IFGT      *ZEROS
     C                   Z-ADD     *ZEROS        DTY2
     C                   MOVE      WDTALT        DTY2
     C                   EXSR      SRDTY2
     C                   MOVE      DTY2          LDTUAT
     C     LDTUAT        IFGT      *ZEROS
     C                   MOVE      *ZEROS        DATA$
     C                   MOVE      LDTUAT        DATA$
     C                   EXSR      VDATA
     C                   MOVE      DATA$         LDTUAT
     C                   END
     C                   ELSE
     C                   Z-ADD     *ZEROS        LDTUAT
     C                   END
     C*
     C* FORMATA CÓDIGO DE ANULAÇÃO
     C*
     C                   MOVEL     SIT           WSIT              3 0
     C                   MOVE      MALT          WSIT
     C                   Z-ADD     1             I
     C     WSIT          LOOKUP    CUG(I)                                 20
     C   20              MOVE      CUL(I)        LANUL
     C  N20              DO
     C     SIT           IFEQ      3
     C                   MOVE      '09'          LANUL
     C                   ELSE
     C                   MOVE      '00'          LANUL
     C                   END
     C                   END
     C*
     C* FORMATA DATA DE ANULAÇÃO.
     C*
     C     SIT           IFEQ      3
     C                   Z-ADD     *ZEROS        DTY2
     C                   MOVE      WDTALT        DTY2
     C                   EXSR      SRDTY2
     C                   MOVE      DTY2          LDTANU
     C     LDTANU        IFGT      *ZEROS
     C                   MOVE      *ZEROS        DATA$
     C                   MOVE      LDTANU        DATA$
     C                   EXSR      VDATA
     C                   MOVE      DATA$         LDTANU
     C                   END
     C                   ELSE
     C                   Z-ADD     *ZEROS        LDTANU
     C                   END
     C*
     C* FORMATA DATA DE INICIO DO CONTRATO
     C*
     C* VALIDA SE ANO DE INICIO É ANTERIOR A 2000 E SE É FORÇA
     C* QUE O ANO SEJA 2000. ( INICIO NA LUSITANIA - CONTINUADOS GIL )
     C*
     C**         AAINIC    IFGT 05
     C**                   MOVE *ZEROS    AAINIC
     C**                   ELSE
     C**                   END
     C*
     C                   Z-ADD     *ZEROS        DTY2
     C                   MOVE      WDTINI        DTY2
     C                   EXSR      SRDTY2
     C                   MOVE      DTY2          LDTINI
     C     LDTINI        IFGT      *ZEROS
     C                   MOVE      *ZEROS        DATA$
     C                   MOVE      LDTINI        DATA$
     C                   EXSR      VDATA
     C                   MOVE      DATA$         LDTINI
     C                   END
     C*
     C* FORMATA DATA DE TERMO DO CONTRATO
     C*
     C                   Z-ADD     *ZEROS        DTY2
     C                   MOVE      WDTFIM        DTY2
     C                   EXSR      SRDTY2
     C                   MOVE      DTY2          LDTFIM
     C     LDTFIM        IFGT      *ZEROS
     C                   MOVE      *ZEROS        DATA$
     C                   MOVE      LDTFIM        DATA$
     C                   EXSR      VDATA
     C                   MOVE      DATA$         LDTFIM
     C                   END
     C*
     C     LDTFIM        IFEQ      *ZEROS
     C                   Z-ADD     99991231      LDTFIM
     C     LDTFIM        IFGT      *ZEROS
     C                   MOVE      *ZEROS        DATA$
     C                   MOVE      LDTFIM        DATA$
     C                   EXSR      VDATA
     C                   MOVE      DATA$         LDTFIM
     C                   END
     C*
     C*                    MOVE WDTVEN    DTY2
     C*                    EXSR SRDTY2
     C*                    MOVE DTY2      LDTFIM
     C                   END
     C*
     C* FORÇA DATA DE ANULAÇÃO A SER MAIOR QUE DATA DE INICIO PARA
     C* AS APÓLICES ANULADAS ANTES DOS CONTINUADOS POR TRANSFERÊNCIA.
     C* ISTO ACONTECE PELO FACTO DE SE FORCAR A DATA DE INICIO PARA A
     C* LUSITANIA COM O ANO 2000. SE O CONTINUADO POR TANSFERÊNCIA NÃO
     C* É PARA SAIR A APÓLICE É ANULADA NO DIA ANTERIOR AO CONTINUADO
     C* DANDO ORIGEM A UMA DATA DE ANULAÇÃO INFERIOR À DATA DE INICIO.
     C*
     C     SIT           IFEQ      3
     C     LDTINI        IFGT      LDTANU
     C                   MOVE      DDINIC        DDALT
     C                   Z-ADD     *ZEROS        DTY2
     C                   MOVE      WDTALT        DTY2
     C                   EXSR      SRDTY2
     C                   MOVE      DTY2          LDTANU
     C     LDTANU        IFGT      *ZEROS
     C                   MOVE      *ZEROS        DATA$
     C                   MOVE      LDTANU        DATA$
     C                   EXSR      VDATA
     C                   MOVE      DATA$         LDTANU
     C                   END
     C                   END
     C     LDTINI        IFGT      LDTANU
     C                   MOVE      LDTINI        LDTANU
     C     LDTANU        IFGT      *ZEROS
     C                   MOVE      *ZEROS        DATA$
     C                   MOVE      LDTANU        DATA$
     C                   EXSR      VDATA
     C                   MOVE      DATA$         LDTANU
     C                   END
     C                   END
     C                   END
     C*
     C* FORMATA DATA DE ALTERAÇAO EFECTIVA (CARREGAMENTO/PROCESAMENTO)
     C*
     C                   Z-ADD     *ZEROS        DTY2
     C                   MOVE      WALTER        DTY2
     C                   EXSR      SRDTY2
     C                   MOVE      DTY2          LALTER
     C     LALTER        IFGT      *ZEROS
     C                   MOVE      *ZEROS        DATA$
     C                   MOVE      LALTER        DATA$
     C                   EXSR      VDATA
     C                   MOVE      DATA$         LALTER
     C                   END
     C*
     C* FORMATA LOCAL DO RISCO (CONCELHO) E LOCAL RISCO (LUGAR)
     C*
     C                   MOVE      CONC          WCONC
     C                   MOVE      WCONC         LCONC
     C   14              MOVEL     LRPS          LLRPS
     C*
     C* SE ACIDENTES TRABALHO - DETERMINA MODALIDADE
     C*
     C     RR            IFEQ      15
     C                   MOVE      MOD           LTPACT
     C* FORMATA PROFISSÕES AC.TRABALHO
     C                   Z-ADD     1             I
     C     PROFSL        LOOKUP    PROFJ(I)                               20
     C   20              MOVE      PROFL(I)      LPROF
     C  N20              MOVE      9999          LPROF
     C                   END
     C*
      *
     C     RR            IFNE      50
     C     RR            IFNE      51
     C                   MOVEL     CDCAE         LACTAT
     C                   END
     C                   END
     C*
     C* SE GRUPO DE RAMOS AC.PESSOAIS - DETERMINA TIPO DE SEGURO
     C*
     C                   MOVE      0             LAPABE            1 0
      /FREE
           IF (LMOD = '02') AND *IN14;    // Se Acidentes Pessoais
              IF (RR = 51) AND (CR3 = 0); // Se Ocupantes
                 LTPACP = '5';            // Tipo de Seguro =
              ELSEIF (RR = 10);           // Se Viagens
                 LTPACP = '1';            // Tipo de Seguro =
              ELSEIF (RR = 15);           // Se Acidentes de Trabalho
                 EVAL  LTPACP = '1';      // Tipo de Seguro =
                 IF (TPT = 'O') AND (MOD = 'F');
                    LTPACP = '0';         // Tipo de Seguro =
                    LAPABE = 1;
                 ENDIF;
              ENDIF;
           ENDIF;

           IF (RR = 02);                  // Se Acidentes Pessoais
              IF (CGRUPO  <> 0);
                 LTPACP = %CHAR(CGRUPO);
                 LAPABE = 1;              // Apólice Nominativa
              ELSE;
                 LAPABE = 0;              // Sem valor
                 IF (NPESS = 1);
                    LTPACP = '1';         // Tipo de Seguro =
                 ELSEIF (NPESS = 2);
                    LTPACP = '2';         // Tipo de Seguro =
                    LAPABE = 1;           // Apólice Nominativa
                 ELSEIF (NPESS > 2);
                    LTPACP = '3';         // Tipo de Seguro =
                    LAPABE = 1;           // Apólice Nominativa
                 ENDIF;
              ENDIF;

              //-------------------------------------
              // Apólice não nomintativa na Clausula
              //-------------------------------------
              IF (CL01 = 07) OR (CL02 = 07) OR (CL03 = 07)
                 OR (CL04 = 07) OR (CL05 = 07) OR (CL06 = 07);
                 LAPABE = 2;  // Apólice Não Nominativa
              ENDIF;
           ENDIF;
      /END-FREE
     C*
     C   14              DO
     C     TPRISC        IFNE      *BLANKS
     C                   MOVE      TPRISC        LTRISC
     C                   END
     C     CLTAR         IFNE      *BLANKS
     C                   MOVE      CLTAR         LCLTAR
     C                   END
     C***                  MOVE PERIOD    LPERIO
     C***                  MOVE CONTR     LCONTR
     C***                  MOVE PCONTR    LPCONT
     C                   MONITOR
     C                   Z-ADD     NPESS         LNPESS
     C                   ON-ERROR
     C                   Z-ADD     *ZEROS        LNPESS
     C                   ENDMON
     C                   END
     C*
     C* SE AUTOMÓVEL  - DETERMINA CATEGORIA (COMPOE CATV GAL)
     C*
     C   13              DO
     C*
     C                   MOVEL     CATV          W2                2 0
     C                   MOVE      CL            W2
     C                   MOVE      PB            W3                3 0
     C                   MOVEL     W2            W3
      * CILINDRADA
     C                   MONITOR
     C                   Z-ADD     CILIND        LCILIN            5 0
     C                   ON-ERROR
     C                   Z-ADD     *ZEROS        LCILIN            5 0
     C                   ENDMON
     C*
     C* TIPO DE SEGURO AUTOMÓVEL ( DEFAULT = 10-VEICULO PARTICULAR )
     C*
     C                   MOVE      '10'          LTPAUT
     C*
     C     W3            IFEQ      501
     C                   MOVE      '14'          LTPAUT
     C                   ELSE
     C     W3            IFEQ      502
     C                   MOVE      '14'          LTPAUT
     C                   ELSE
     C     W3            IFEQ      503
     C                   END
     C                   END
     C                   END
     C*
     C     CL1           IFEQ      'I'
     C                   MOVE      '30'          LTPAUT
     C                   ELSE
     C     CL2           IFEQ      'I'
     C                   MOVE      '30'          LTPAUT
     C                   ELSE
     C     CL3           IFEQ      'I'
     C                   MOVE      '30'          LTPAUT
     C                   ELSE
     C     CL4           IFEQ      'I'
     C                   MOVE      '30'          LTPAUT
     C                   ELSE
     C     CL5           IFEQ      'I'
     C                   MOVE      '30'          LTPAUT
     C                   ELSE
     C     CL6           IFEQ      'I'
     C                   MOVE      '30'          LTPAUT
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C*
     C     LTPAUT        IFEQ      '30'
     C                   MOVEL     *BLANKS       VIAG
     C                   MOVEL     *BLANKS       MATR2
     C                   MOVEL     *BLANKS       MARC2
     C                   END
     C*
     C* PESQUISA COMBINAÇÃO NA MARCA
     C*
     C                   Z-ADD     1             I
     C     MARC1         LOOKUP    W1G(I)                                 20
     C   20              MOVE      '30'          LTPAUT
     C   20              MOVE      *BLANKS       MARC1
     C   20              MOVE      *BLANKS       MATR1
     C*
     C* PESQUISA COMBINAÇÃO NA MATRICULA
     C*
     C                   Z-ADD     1             I
     C     MATR1         LOOKUP    W2G(I)                                 20
     C   20              MOVE      '30'          LTPAUT
     C   20              MOVE      *BLANKS       MARC1
     C   20              MOVE      *BLANKS       MATR1
     C*
     C* ----------------------------------------------------
     C* PESQUISA COMBINAÇÃO NA MARCA
     C*
     C                   Z-ADD     1             I
     C     MARC2         LOOKUP    W1G(I)                                 20
     C   20              MOVE      '30'          LTPAUT
     C   20              MOVE      *BLANKS       MARC2
     C   20              MOVE      *BLANKS       MATR2
     C*
     C* PESQUISA COMBINAÇÃO NA MATRICULA
     C*
     C                   Z-ADD     1             I
     C     MATR2         LOOKUP    W2G(I)                                 20
     C   20              MOVE      '30'          LTPAUT
     C   20              MOVE      *BLANKS       MARC2
     C   20              MOVE      *BLANKS       MATR2
     C*
     C* ----------------------------------------------------
     C*
     C* PESQUISA CATEGORIA DE VEÍCULO
     C*    ( PARA CATEGORIAS INVÁLIDAS FORÇA CATV = 9990 NA LV. )
     C*
     C                   Z-ADD     1             I
     C     W3            LOOKUP    CVG(I)                                 20
     C   20              MOVE      CVL(I)        LCATV
     C  N20              MOVE      '9990'        LCATV
     C  N20              MOVE      '10'          LTPAUT
     C*
     C* SE TIPO DE SEGURO "30"=SEGURO DE CARTA FORÇA CATEGORIA VEICULO
     C* ( CATEGORIA EM JM É SEMPRE 500 ==> CATEGORIA LUSITANIA PASSOU
     C*   DE 3300 PARA 3450 )
     C*
     C     LTPAUT        IFEQ      '30'
     C                   MOVE      '3450'        LCATV
     C                   END
     C* CATV 354
     C     RRAPOL        LOOKUP    CAT354                                 20
     C   20              MOVE      '0800'        LCATV
     C                   END
     C*
     C                   MOVEL     MATR1         LMATR
     C                   MOVEL     MARC1         LMARCA
     C*
     C                   MOVEL     MATR2         LMATR2
     C                   MOVEL     MARC2         LMARC2
     C*
     C* FORMATA DATA DA MATRICULA
     C*
     C     RR            IFEQ      50
     C                   Z-ADD     *ZEROS        DTY2
     C                   MOVE      WDTMAT        DTY2
     C                   EXSR      SRDTY2
     C                   MOVE      DTY2          LDTMAT
     C     LDTMAT        IFGT      *ZEROS
     C                   MOVE      *ZEROS        DATA$
     C                   MOVE      LDTMAT        DATA$
     C                   EXSR      VDATA
     C                   MOVE      DATA$         LDTMAT
     C                   END
     C                   END
     C*
     C* FORMATA TIPO DE OCUPANTES INCLUIDOS NO AUTO
     C*
     C     RR            IFEQ      50                                             <-----+
     C     CR3           IFGT      0                                              <---+ I
     C                   Z-ADD     1             I                                    I I
     C     CR3           LOOKUP    OCUG(I)                                20          I I
     C   20              MOVE      OCUL(I)       LTPOCU                               I I
     C  N20              MOVE      '2'           LTPOCU                               I I
     C*                                                          I I
     C* FORMATA MÓDULO DE OCUPANTES DA LUSITANIA                 I I
     C*                                                          I I
     C                   Z-ADD     1             I                                    I I
     C     CAPOCU        LOOKUP    MODG(I)                                20          I I
     C   20              MOVE      MODL(I)       LMODUL                               I I
     C  N20              MOVE      MODL(1)       LMODUL                               I I
     C                   END                                                      <---+ I
     C*                                                            I
     C                   END                                                      <-----+
     C*
     C* FORMATA FUNÇÃO DE INCÊNDIO
     C*
     C   14RR            IFEQ      01
     C     CR1           IFEQ      3
     C                   MOVE      '2'           LFUNC
     C                   ELSE
     C     CR1           IFEQ      4
     C                   MOVE      '4'           LFUNC
     C                   ELSE
     C     NSEG          IFGT      9999
     C                   MOVE      '1'           LFUNC
     C                   ELSE
     C                   MOVE      '2'           LFUNC
     C                   END
     C                   END
     C                   END
     C                   END
     C*
     C   14RR            IFEQ      05
     C                   MOVE      '1'           LFUNC
     C                   END
     C*
     C   14RR            IFEQ      55
     C                   MOVE      '1'           LFUNC
     C                   END
     C*
     C* FORMATA ACTIVIDADE
     C*
     C                   MOVE      *BLANKS       LACTIN
     C*
     C* FORMATA TIPO DE CONSTRUÇÃO
     C*
     C                   MOVE      *BLANKS       LTPCON
     C*
     C* FORMATA AMBITO
     C*
     C   14RR            IFEQ      01
     C                   EXSR      SRAMB
     C                   MOVE      WAMBIT        LAMBIN
     C                   END
     C*
     C   14RR            IFEQ      05
     C                   EXSR      SRAMB
     C                   MOVE      WAMBIT        LAMBMH
     C                   END
     C*
     C   14RR            IFEQ      55
     C                   EXSR      SRAMB
     C                   MOVE      WAMBIT        LAMBMC
     C                   END
     C*
     C* FORMATA ACTIVIDADE COMERCIANTE
     C*
     C                   MOVE      *BLANKS       LACTMC
     C*
     C* FORMATA INFORMAÇÃO DE COSEGURO
     C*
     C   14CS            IFEQ      0
     C                   MOVE      '0'           LCOSEG
     C                   ELSE
     C     CS            IFEQ      1
     C                   MOVE      '2'           LCOSEG
     C                   ELSE
     C     CS            IFEQ      2
     C                   MOVE      '1'           LCOSEG
     C                   ELSE
     C                   MOVE      '0'           LCOSEG
     C                   END
     C                   END
     C                   END
     C*
     C* FORMATA VALOR EXTRAS
     C*
     C   14CS            IFEQ      0
     C                   MOVE      '0'           LCOSEG
     C                   ELSE
     C     CS            IFEQ      1
     C                   MOVE      '2'           LCOSEG
     C                   ELSE
     C     CS            IFEQ      2
     C                   MOVE      '1'           LCOSEG
     C                   ELSE
     C                   MOVE      '0'           LCOSEG
     C                   END
     C                   END
     C                   END
     C*
     C* FORMATA CAMPOS DE CONTROL
     C*
     C                   MOVEL     RR            LRRAP
     C                   MOVE      APOL          LRRAP
      *
     C                   Z-ADD     1             I
     C     COMB          LOOKUP    COMBG2(I)                              20
     C   20              MOVE      COMBL2(I)     LCOMB2            1
     C  N20              MOVE      '0'           LCOMB2
     C*
     C                   EXCEPT    REGAP
     C                   EXSR      SRISC
     C*
     C****************************************************
     C     FIM           TAG
     C****************************************************
     C*   FORMATA DATAS COM 8 DIGITOS                     *
     C********************************************    > < =
     C     SRDTY2        BEGSR
     C*
     C     DTY2          IFGT      *ZEROS
     C     DTY2          IFLT      00210000
     C                   MOVEL     20            DTY2
     C                   ELSE
     C                   MOVEL     19            DTY2
     C                   END
     C                   END
     C*
     C                   ENDSR
     C****************************************************
     C*   VALIDA DATAS - SE DATAS SÃO INCORRECTAS FORÇA  *
     C********************************************    > < =
     C     VDATA         BEGSR
     C*
     C     MM$           LOOKUP    TABM          TABD                     40
     C     MM$           IFEQ      02
     C     AA$           DIV       4             RES               3 0
     C                   MVR                     RES
     C     RES           IFEQ      *ZEROS
     C                   MOVE      29            TABD
     C                   END
     C                   END
     C*
     C     DD$           IFGT      TABD
     C                   MOVE      TABD          DD$
     C                   END
     C*
     C                   ENDSR
     C****************************************************
     C*   DETERMINA DATA VENCIMENTO                       *
     C********************************************    > < =
     C     SRDTVC        BEGSR
     C*
     C     DDINIC        IFEQ      01                                           <-------------------
     C     MES1          LOOKUP    TABMM         TABDT                    21
     C  N21              Z-ADD     *ZEROS        DDVEN
     C  N21              Z-ADD     *ZEROS        MMVEN
     C  N21              Z-ADD     *ZEROS        AAVEN
     C  N21              GOTO      EVREC
     C   21              MOVE      TABDT         MMVEN
     C   21              MOVEL     TABDT         DDVEN
     C*
     C     MES1          IFEQ      01                                           <---------------+
     C                   MOVE      AACONT        AAVEN                                          I
     C                   SUB       1             AAVEN                                          I
     C                   ELSE                                                                   *
     C                   MOVE      AACONT        AAVEN                                          I
     C                   END                                                    <---------------+
     C                   ELSE
     C                   MOVE      DDINIC        DDVEN
     C                   SUB       1             DDVEN
     C                   MOVE      MES1          MMVEN
     C                   MOVE      AACONT        AAVEN
     C                   END                                                    <-------------------
     C*
     C     EVREC         ENDSR
     C*
     C****************************************************
     C*   DETERMINA AMBITO DE INCENDIO                    *
     C********************************************    > < =
     C     SRAMB         BEGSR
     C*
     C                   Z-ADD     *ZEROS        WR1
     C                   Z-ADD     *ZEROS        WR2
     C                   MOVE      *BLANKS       WAMBIT            1
     C*
     C     CR1           IFEQ      1
     C                   MOVE      1             WR1
     C                   ELSE
     C     CR2           IFEQ      1
     C                   MOVE      1             WR1
     C                   ELSE
     C   14CR3           IFEQ      1
     C                   MOVE      1             WR1
     C                   END
     C                   END
     C                   END
     C*
     C     CR1           IFEQ      2
     C                   MOVE      2             WR2
     C                   ELSE
     C     CR2           IFEQ      2
     C                   MOVE      2             WR2
     C                   ELSE
     C   14CR3           IFEQ      2
     C                   MOVE      2             WR2
     C                   END
     C                   END
     C                   END
     C*
     C                   Z-ADD     1             I
     C     WR12          LOOKUP    AIG(I)                                 20
     C   20              MOVE      AIL(I)        WAMBIT
     C  N20              MOVE      '1'           WAMBIT
     C*
     C     ESRAMB        ENDSR
     C*
     C****************************************************
     C*   DETERMINA RISCOS  -  ( PARA COBERTURAS )        *
     C********************************************    > < =
     C     SRISC         BEGSR
     C*
     C                   MOVE      ' '           FCR0              1
     C                   Z-ADD     *ZEROS        WCR0              1 0
     C                   Z-ADD     *ZEROS        WCR1              1 0
     C                   Z-ADD     *ZEROS        WCR2              1 0
     C                   Z-ADD     *ZEROS        WCR3              1 0
     C                   Z-ADD     *ZEROS        WRCOB             3 0
     C*
     C     CR1           IFGT      0
     C                   MOVE      CR1           WCR1
     C                   END
     C     CR2           IFGT      0
     C                   MOVE      CR2           WCR2
     C                   END
     C     CR3           IFGT      0
     C     RR            IFEQ      02
     C                   MOVE      CR3           WCR3
     C                   ELSE
     C   14              MOVE      CR3           WCR2
     C                   END
     C   13              MOVE      CR3           WCR3
     C                   END
     C*
     C     WCR1          IFGT      0
     C                   MOVEL     RR            WRCOB
     C                   MOVE      WCR1          WRCOB
     C                   MOVE      WCR1          WCR0
     C                   Z-ADD     CAP1          CAPTOT
     C     RR            IFEQ      02
     C     NPESS         IFNE      0
     C     CAPTOT        DIV(H)    NPESS         CAPTOT
     C                   END
     C                   END
     C                   EXSR      SRCOB
     C   14              EXSR      SROCOB
     C                   END
     C*
     C     WCR2          IFGT      0
     C                   MOVEL     RR            WRCOB
     C                   MOVE      WCR2          WRCOB
     C                   MOVE      WCR2          WCR0
     C                   Z-ADD     CAP2          CAPTOT
     C     RR            IFEQ      02
     C     NPESS         IFNE      0
     C     CAPTOT        DIV(H)    NPESS         CAPTOT
     C                   END
     C                   END
     C                   EXSR      SRCOB
     C   14              EXSR      SROCOB
     C                   END
     C*
     C     WCR3          IFGT      0
     C                   MOVEL     RR            WRCOB
     C                   MOVE      WCR3          WRCOB
     C                   MOVE      WCR3          WCR0
     C                   Z-ADD     CAP3          CAPTOT
     C     RR            IFEQ      02
     C     NPESS         IFNE      0
     C     CAPTOT        DIV(H)    NPESS         CAPTOT
     C                   END
     C                   END
     C   14              EXSR      SRCOB
     C   13RR            IFEQ      50
     C                   EXSR      SROCUP
     C                   END
     C                   END
     C*
     C   13              EXSR      SRQIV
     C   13              EXSR      SRPRIV
     C   13              EXSR      SRVGPJ
     C*
     C     ERISC         ENDSR
     C*
     C****************************************************
     C*   PESQUISA ASSOCIAÇÕES DE COBERTURAS              *
     C********************************************    > < =
     C     SRCOB         BEGSR
     C*
     C                   Z-ADD     1             I
     C     WRCOB         LOOKUP    RIG(I)                                 20
     C   20              MOVE      RIL(I)        LCOBER
     C  N20              DO                                                     <------+
     C                   MOVE      0             WRCOB                                 I
     C     WRCOB         LOOKUP    RIG(I)                                 20           I
     C   20              MOVE      RIL(I)        LCOBER                                I
     C  N20              MOVE      *BLANKS       LCOBER                                I
     C                   END                                                    <------+
     C*
     C* FORMATA CAPITAIS  E PRS. IMPUTADO E (OUTROS RAMOS)
     C*
     C   14              DO                                                     <------+
     C                   Z-ADD     CAPTOT        LCAPT                                 I
     C     ISEN          IFNE      1                                                   I
     C     PRSACT        DIV(H)    1.2           LPRSI                                 I
     C                   ELSE                                                          I
     C                   Z-ADD     PRSACT        LPRSI                                 I
     C                   END                                                    <------+
     C                   Z-ADD     PRSACT        LPRCOM                                I
     C*                                                           I
     C                   EXSR      SRCP12                                              I
     C*                                                           I
     C     LCAPT         IFEQ      4992                                         <------+
     C                   Z-ADD     4993          LCAPT                                 I
     C                   END                                                    <------+
     C*                                                           I
[LG] C*-----------------------------------------
     C                   EXSR      ADDCOB
     C*                    EXCPTREGCOB                            I
[LG] C*-----------------------------------------
     C                   END                                                    <------+
     C*
     C* FORMATA CAPITAIS  E PRS. IMPUTADO (AUTO + OCUP.)
     C*
     C   13              DO                                                     <---------+
     C                   Z-ADD     *ZEROS        $RC               9 0                    I
     C                   Z-ADD     *ZEROS        $DP               9 0                    I
     C     CAPRC         IFEQ      99999                                        <-------+ I
     C***********          MOVE 9999999999LCAPT                    I I
     C***********          MOVEL999       LCAPT                    I I
     C                   Z-ADD     50000000      LCAPT                                  I I
     C* SE EXISTE ISENÇÃO PR.SIMPLES É IGUAL AO PR.COMERCIAL       I I
     C     ISEN          IFNE      1                                                    I I
     C     PRS           DIV(H)    1.2           LPRSI                                  I I
     C                   ELSE                                                           I I
     C                   Z-ADD     PRS           LPRSI                                  I I
     C                   END                                                            I I
     C                   Z-ADD     PRS           LPRCOM                                 I I
     C                   ELSE                                                           * I
     C*                                                            I I
     C* FORMATA DE FORMA DIFERENCIADA OS CAPITAIS DE AUTOMOVEL POR I I
     C* ESTES SEREM PREENCHIDOS DE FORMA DIFERENTE - EM MILHARES   I I
     C* EUROS PARA O RC AUTO E EM EUROS PARA DP AUTO E MIP/DT PARA I I
     C* OS OCUPANTES ENQUANTO RAMO ISOLADO.                        I I
     C*                                                            I I
     C     RR            IFEQ      50                                                   I I
     C     CAPRC         MULT      1000          $RC                                    I I
     C                   ELSE                                                           I I
     C                   Z-ADD     CAPRC         $RC                                    I I
     C                   END                                                            I I
     C                   Z-ADD     CAPDP         $DP                                    I I
     C     $RC           ADD       $DP           LCAPT                                  I I
     C     ISEN          IFNE      1                                                    I I
     C     PRS           DIV(H)    1.2           LPRSI                                  I I
     C                   ELSE                                                           I I
     C                   Z-ADD     PRS           LPRSI                                  I I
     C                   END                                                    <-------+ I
     C                   Z-ADD     PRS           LPRCOM                                 I I
     C                   END                                                    <-------+ I
     C*                                                              I
     C     RR            IFEQ      50                                           <-------+ I
     C                   Z-ADD     *ZEROS        $RC                                    I I
     C                   Z-ADD     *ZEROS        $DP                                    I I
     C*                                                            I I
     C* R.C. OBRIGATORIA                                           I I
     C     WCR0          IFGE      1                                            <-----+ I I
     C     WCR0          IFLE      2                                            <----+I I I
     C     CAPRC         IFGE      6000                                         <---+II I I
     C                   Z-ADD     6000          $RC                                III I I
     C     CAPRC         IFEQ      6000
     C                   Z-ADD     6000          $RC                                III I I
     C                   END
     C     $RC           MULT      1000          $RC                                III I I
     C                   ELSE                                                       *II I I
     C                   Z-ADD     CAPRC         $RC                                III I I
     C     $RC           MULT      1000          $RC                                III I I
     C                   END                                                    <---+II I I
     C*                                                         II I I
     C                   Z-ADD     $RC           LCAP1                               II I I
     C                   Z-ADD     $DP           LCAP2                               II I I
     C*                                                         II I I
     C     PRIV          IFNE      ' '                                          <--+ II I I
     C     ISEN          IFNE      1                                               I II I I
     C     PRPRIV        DIV(H)    1.2           WPRPRV            7 2             I II I I
     C                   ADD       WPRPRV        LPRSI                             I II I I
     C                   ELSE                                                      * II I I
     C                   ADD       PRPRIV        LPRSI                             I II I I
     C                   END                                                    <--+ II I I
     C                   ADD       PRPRIV        LPRCOM                            I II I I
     C                   END                                                    <--+ II I I
     C*                                                         II I I
     C     QIV           IFEQ      'S'                                          <--+ II I I
     C     ISEN          IFNE      1                                               I II I I
     C     PRQIV         DIV(H)    1.2           WPRQIV            7 2             I II I I
     C                   ADD       WPRQIV        LPRSI                             I II I I
     C                   ELSE                                                      * II I I
     C                   ADD       PRQIV         LPRSI                             I II I I
     C                   END                                                    <--+ II I I
     C                   ADD       PRQIV         LPRCOM                            I II I I
     C                   END                                                    <--+ II I I
     C*                                                         II I I
     C     ISEN          IFNE      1                                            <--+ II I I
     C     PR1           DIV(H)    1.2           LPRS1                             I II I I
     C                   ELSE                                                      * II I I
     C                   Z-ADD     PR1           LPRS1                             I II I I
     C                   END                                                    <--+ II I I
     C                   Z-ADD     PR1           LPRS1C                              II I I
     C                   Z-ADD     *ZEROS        LPRS2                               II I I
     C                   Z-ADD     *ZEROS        LPRS2C                              II I I
     C*                                                         II I I
[LG] C*-----------------------------------------
     C                   EXSR      ADDCOB
     C*                    EXCPTREGCOB                            I
[LG] C*-----------------------------------------
      *                                                         II I I
     C     LCOBER        IFEQ      '003'                                        <--+ II I I
     C                   MOVE      '004'         LCOBER                            I II I I
[LG] C*-----------------------------------------
     C                   EXSR      ADDCOB
     C*                    EXCPTREGCOB                            I
[LG] C*-----------------------------------------
     C                   MOVE      '003'         LCOBER                            I II I I
     C                   END                                                    <--+ II I I
      *                                                         II I I
     C                   END                                                    <----+I I I
     C                   END                                                    <-----+ I I
     C*                                                            I I
     C                   Z-ADD     *ZEROS        $RC                                    I I
     C                   Z-ADD     *ZEROS        $DP                                    I I
     C*                                                            I I
     C* DANOS PROPRIOS                                             I I
     C     WCR0          IFGT      2                                            <-----+ I I
     C                   Z-ADD     CAPDP         $DP                                  I I I
     C                   Z-ADD     $DP           LCAP2                                I I I
     C*                                                          I I I
     C                   Z-ADD     $RC           LCAP1                                I I I
     C                   Z-ADD     $DP           LCAP2                                I I I
     C                   Z-ADD     *ZEROS        LPRS1                                I I I
     C                   Z-ADD     *ZEROS        LPRS1C                               I I I
     C     ISEN          IFNE      1                                            <---+ I I I
     C     PR2           DIV(H)    1.2           LPRS2                              I I I I
     C                   ELSE                                                       * I I I
     C                   Z-ADD     PR2           LPRS2                              I I I I
     C                   END                                                    <---+ I I I
     C                   Z-ADD     PR2           LPRS2C                               I I I
     C*                                                          I I I
[LG] C*-----------------------------------------
     C                   EXSR      ADDCOB
     C*                    EXCPTREGCOB                            I
[LG] C*-----------------------------------------
     C                   END                                                    <-----+ I I
     C*                                                            I I
     C                   EXSR      SRCL50                                               I I
     C*                                                            I I
     C                   GOTO      XPTO01                                               I I
     C                   END                                                    <-------+ I
     C*                                                              I
     C*  FORÇA O PRÉMIO PR1 AOS DOIS CAMPOS DA LUSITANIA             I
     C*                                                              I
     C     RR            IFEQ      51                                           <-------+ I
     C                   Z-ADD     *ZEROS        $RC                                    I I
     C                   Z-ADD     *ZEROS        $DP                                    I I
     C*                                                            I I
     C     WCR0          IFEQ      1                                                    I I
     C                   Z-ADD     CAPRC         LCAP1                                  I I
     C                   Z-ADD     CAPDP         LCAP2                                  I I
     C     ISEN          IFNE      1                                                    I I
     C     PR1           DIV(H)    1.2           LPRS1                                  I I
     C                   ELSE                                                           I I
     C                   Z-ADD     PR1           LPRS1                                  I I
     C                   END                                                            I I
     C                   Z-ADD     PR1           LPRS1C                                 I I
     C     ISEN          IFNE      1                                                    I I
     C     PR1           DIV(H)    1.2           LPRS2                                  I I
     C                   ELSE                                                           I I
     C                   Z-ADD     PR1           LPRS2                                  I I
     C                   END                                                            I I
     C                   Z-ADD     PR1           LPRS2C                                 I I
[LG] C*-----------------------------------------
     C                   EXSR      ADDCOB
     C*                    EXCPTREGCOB                            I
[LG] C*-----------------------------------------
     C                   END                                                            I I
     C*                                                            I I
     C     WCR0          IFEQ      2                                            <----+  I I
     C                   Z-ADD     *ZEROS        LCAP1                               I  I I
     C                   Z-ADD     *ZEROS        LCAP2                               I  I I
     C                   Z-ADD     *ZEROS        LPRS1                               I  I I
     C                   Z-ADD     *ZEROS        LPRS1C                              I  I I
     C                   Z-ADD     *ZEROS        LPRS2                               I  I I
     C                   Z-ADD     *ZEROS        LPRS2C                              I  I I
[LG] C*-----------------------------------------
     C                   EXSR      ADDCOB
     C*                    EXCPTREGCOB                            I
[LG] C*-----------------------------------------
     C                   END                                                    <----+  I I
     C*                                                            I I
     C                   GOTO      XPTO01                                               I I
     C                   END                                                    <-------I I
     C                   END                                                    <---------+
     C*
     C     XPTO01        TAG
     C*          -----------------------------------------
     C*           LIMPEZA DE CAMPOS A PEDIDO DE LUSITANIA
     C*          -----------------------------------------
     C*
     C                   Z-ADD     LCAP2         WCAP2
     C                   Z-ADD     *ZEROS        LCAP1
     C                   Z-ADD     *ZEROS        LCAP2
     C                   Z-ADD     *ZEROS        LPRS1
     C                   Z-ADD     *ZEROS        LPRS1C
     C                   Z-ADD     *ZEROS        LPRS2
     C                   Z-ADD     *ZEROS        LPRS2C
     C*
     C     RR            IFEQ      02
     C     WCR0          IFEQ      1
     C                   MOVE      '002'         LCOBER
[LG] C*-----------------------------------------
     C                   EXSR      ADDCOB
     C*                    EXCPTREGCOB                            I
[LG] C*-----------------------------------------
     C                   END
     C                   MOVE      'X'           FCR0
     C     FCR0          IFEQ      ' '
     C     WCR0          IFEQ      9
     C                   MOVE      '002'         LCOBER
[LG] C*-----------------------------------------
     C                   EXSR      ADDCOB
     C*                    EXCPTREGCOB                            I
[LG] C*-----------------------------------------
     C                   END
     C                   END
     C                   END
     C*
     C     RR            IFEQ      06
     C                   MOVE      '002'         LCOBER
[LG] C*-----------------------------------------
     C                   EXSR      ADDCOB
     C*                    EXCPTREGCOB                            I
[LG] C*-----------------------------------------
     C                   MOVE      'X'           FCR0
     C     FCR0          IFEQ      ' '
     C                   MOVE      '002'         LCOBER
[LG] C*-----------------------------------------
     C                   EXSR      ADDCOB
     C*                    EXCPTREGCOB                            I
[LG] C*-----------------------------------------
     C                   END
     C                   END
     C*
     C     RR            IFEQ      05
     C****               MOVE      '003'         LCOBER
[LG] C*-----------------------------------------
     C****               EXSR      ADDCOB
     C*                    EXCPTREGCOB                            I
[LG] C*-----------------------------------------
     C                   MOVE      'X'           FCR0
     C     FCR0          IFEQ      ' '
     C                   MOVE      '003'         LCOBER
[LG] C*-----------------------------------------
     C                   EXSR      ADDCOB
     C*                    EXCPTREGCOB                            I
[LG] C*-----------------------------------------
     C                   END
     C                   END
     C*
     C     RR            IFEQ      55
     C*                    MOVE '002'     LCOBER
     C*                    EXCPTbEGCOB
     C                   MOVE      'X'           FCR0
     C     FCR0          IFEQ      ' '
     C*                    MOVE '002'     LCOBER
     C*                    EXCPTREGCOB
     C                   END
     C                   END
     C*
     C     RR            IFEQ      01
     C                   Z-ADD     1             CONT              2 0
     C     CONT          DOWLE     6
     C                   EXSR      SRCL
     C                   Z-ADD     1             I
     C     WRKCL         LOOKUP    C01G(I)                                20
     C* N20                MOVE *BLANKS   LCOBER
     C   20              MOVE      C01L(I)       LCOBER
     C   20              EXCEPT    REGCCL
     C                   ADD       1             CONT
     C                   END
     C                   GOTO      ESCOB
     C                   END
     C*
     C     RR            IFEQ      05
     C     NEWPRD        IFEQ      ' '
     C                   Z-ADD     1             CONT
     C     CONT          DOWLE     6
     C                   EXSR      SRCL
     C                   Z-ADD     1             I
     C     WRKCL         LOOKUP    C05G(I)                                20
     C* N20                MOVE *BLANKS   LCOBER
     C   20              MOVE      C05L(I)       LCOBER
     C   20              EXCEPT    REGCCL
     C                   ADD       1             CONT
     C                   END
     C                   ELSE
     C                   EXSR      SRCL05
     C                   END
     C                   GOTO      ESCOB
     C                   END
     C*
     C     RR            IFEQ      06
     C                   Z-ADD     1             CONT
     C     CONT          DOWLE     6
     C                   EXSR      SRCL
     C                   Z-ADD     1             I
     C     WRKCL         LOOKUP    C06G(I)                                20
     C* N20                MOVE *BLANKS   LCOBER
     C   20              MOVE      C06L(I)       LCOBER
     C   20              EXCEPT    REGCCL
     C                   ADD       1             CONT
     C                   END
     C                   GOTO      ESCOB
     C                   END
     C*
     C     RR            IFEQ      17
     C                   Z-ADD     1             CONT
     C     CONT          DOWLE     6
     C                   EXSR      SRCL
     C                   Z-ADD     1             I
     C     WRKCL         LOOKUP    C17G(I)                                20
     C* N20                MOVE *BLANKS   LCOBER
     C   20              MOVE      C17L(I)       LCOBER
     C   20              EXCEPT    REGCCL
     C                   ADD       1             CONT
     C                   END
     C                   GOTO      ESCOB
     C                   END
     C*
     C     RR            IFEQ      19
     C                   Z-ADD     1             CONT
     C     CONT          DOWLE     6
     C                   EXSR      SRCL
     C                   Z-ADD     1             I
     C     WRKCL         LOOKUP    C19G(I)                                20
     C* N20                MOVE *BLANKS   LCOBER
     C   20              MOVE      C19L(I)       LCOBER
     C   20              EXCEPT    REGCCL
     C                   ADD       1             CONT
     C                   END
     C                   GOTO      ESCOB
     C                   END
     C*
     C     RR            IFEQ      55
     C     NEWPRD        IFEQ      ' '
     C                   Z-ADD     1             CONT
     C     CONT          DOWLE     6
     C                   EXSR      SRCL
     C                   Z-ADD     1             I
     C     WRKCL         LOOKUP    C55G(I)                                20
     C* N20                MOVE *BLANKS   LCOBER
     C   20              DO                                                     <---------+
     C                   MOVE      C55L(I)       LCOBER                                   I
      *
     C     LCOBER        IFEQ      '106'                                        <-------+ I
     C     PPERDI        IFGT      0                                            <-----+ I I
     C     PPERDI        DIV       100           WPI               5 3                I I I
     C                   ELSE                                                         * I I
     C                   Z-ADD     30            PPERDI                               I I I
     C     PPERDI        DIV       100           WPI               5 3                I I I
     C                   END                                                    <-----+ I I
     C     WCAP2         MULT      WPI           WCAPPI           13 0                  I I
     C                   Z-ADD     WCAPPI        LCAPPI                                 I I
     C                   END                                                    <-------+ I
      *
     C                   EXCEPT    REGCCL                                                 I
     C                   END                                                    <---------+
     C                   ADD       1             CONT
     C                   END
     C                   ELSE
     C                   EXSR      SRCL55
     C                   END
     C                   GOTO      ESCOB
     C                   END
     C*
     C     ESCOB         ENDSR
     C*****************************************************
     C*   FORMATA COBERTURAS A.VIAGEM E PROTECÇÃO JURÍDICA*
     C********************************************    > < =
     C     SRVGPJ        BEGSR
     C*
     C* FORMATA COBERTURA ASSISTÊNCIA VIAGEM
     C     VIAG          IFNE      ' '                                          <---+
     C     VIAG          IFEQ      'N'                                          <---+
     C                   MOVE      '051'         LCOBER                             I
     C                   ELSE                                                       I
     C     VIAG          IFEQ      'V'                                          <---+
     C                   MOVE      '061'         LCOBER                             I
     C                   ELSE                                                       I
     C     VIAG          IFEQ      'P'                                          <---+
     C                   MOVE      '062'         LCOBER                             I
     C                   ELSE                                                       I
     C     VIAG          IFEQ      'A'                                          <---+
     C                   MOVE      '063'         LCOBER                             I
     C                   ELSE                                                       I
     C     VIAG          IFEQ      'B'                                          <---+
     C                   MOVE      '077'         LCOBER                             I
     C                   ELSE                                                       I
     C     VIAG          IFEQ      'T'                                          <---+
     C                   MOVE      '063'         LCOBER                             I
     C                   END                                                        I
     C                   END                                                        I
     C                   END                                                        I
     C                   END                                                        I
     C                   END                                                        I
     C                   END                                                        I
     C                   Z-ADD     *ZEROS        LCAPT                              I
     C     PRSVG         SUB       PVGAGR        WPRSVG            7 2              I
     C     ISEN          IFNE      1                                                I
     C     WPRSVG        DIV(H)    1.2           LPRSI                              I
     C                   ELSE                                                       I
     C                   Z-ADD     WPRSVG        LPRSI                              I
     C                   END                                                        I
     C                   Z-ADD     WPRSVG        LPRCOM                             I
     C*                                                        I
     C                   Z-ADD     *ZEROS        LCAP1                              I
     C                   Z-ADD     *ZEROS        LCAP2                              I
     C                   Z-ADD     LPRSI         LPRS1                              I
     C                   Z-ADD     *ZEROS        LPRS1C                             I
     C                   Z-ADD     *ZEROS        LPRS2                              I
     C                   Z-ADD     *ZEROS        LPRS2C                             I
     C*-----------------------------------------
     C                   EXSR      ADDCOB
      *
     C     VIAG          IFEQ      'B'
     C     VIAG          OREQ      'V'
     C     CATV          IFEQ      1
     C     CATV          OREQ      6
     C     CATV          OREQ      7
     C                   EXSR      SRPU2
     C                   ENDIF                                                  <---+
     C                   ENDIF                                                  <---+
[LG] C*-----------------------------------------
     C                   END                                                    <---+
     C* PROTECÇÃO JURÍDICA
     C     PJUR          IFEQ      'S'                                          <---+
     C                   MOVE      '058'         LCOBER                             I
     C                   Z-ADD     *ZEROS        LCAPT                              I
     C     ISEN          IFNE      1                                                I
     C     PRSPJ         DIV(H)    1.2           LPRSI                              I
     C                   ELSE                                                       I
     C                   Z-ADD     PRSPJ         LPRSI                              I
     C                   END                                                    <---+
     C                   Z-ADD     PRSPJ         LPRCOM                             I
     C*
     C                   Z-ADD     *ZEROS        LCAP1                              I
     C                   Z-ADD     *ZEROS        LCAP2                              I
     C                   Z-ADD     PRSPJ         LPRS1                              I
     C                   Z-ADD     *ZEROS        LPRS1C                             I
     C                   Z-ADD     *ZEROS        LPRS2                              I
     C                   Z-ADD     *ZEROS        LPRS2C                             I
[LG] C*-----------------------------------------
     C                   EXSR      ADDCOB
     C*                    EXCPTREGCOB                            I
[LG] C*-----------------------------------------
     C                   END                                                    <---+
     C*
     C     EVGPJ         ENDSR
     C*****************************************************
     C*   FORMATA COBERTURAS DE OCUPANTES                 *
     C********************************************    > < =
     C     SROCUP        BEGSR
     C*
     C* FORMATA COBERTURA OCUPANTES
     C                   MOVE      '052'         LCOBER
     C                   Z-ADD     *ZEROS        LCAPT
     C****                 Z-ADDCAPOCU    LCAP1
     C                   Z-ADD     *ZEROS        LCAP1
     C                   Z-ADD     *ZEROS        LCAP2
     C*
     C* FORMATA CAPITAL TOTAL SEGURO
     C     LMODUL        IFEQ      '01'
     C                   Z-ADD     9975.96       LCAPT
     C                   ELSE
     C     LMODUL        IFEQ      '02'
     C                   Z-ADD     14963.94      LCAPT
     C                   ELSE
     C     LMODUL        IFEQ      '03'
     C                   Z-ADD     24939.89      LCAPT
     C                   END
     C                   END
     C                   END
     C*
     C* FORMATA PREMIO SIMPLES
     C     ISEN          IFNE      1
     C     PR3           DIV(H)    1.2           LPRS1
     C                   ELSE
     C                   Z-ADD     PR3           LPRS1
     C                   END
     C                   Z-ADD     PR3           LPRS1C
     C                   Z-ADD     *ZEROS        LPRS2
     C                   Z-ADD     *ZEROS        LPRS2C
[LG] C*-----------------------------------------
     C                   EXSR      ADDCOB
     C*                    EXCPTREGCOB                            I
[LG] C*-----------------------------------------
     C*
     C     EOCUP         ENDSR
     C*****************************************************
     C*   FORMATA COBERTURAS DE QUEBRA ISOLADA DE VIDROS  *
     C*   QUANDO ESTA É CONTRATADA ISOLADAMENTE           *
     C********************************************    > < =
     C     SRQIV         BEGSR
     C*
     C     QIV           IFEQ      'S'
     C                   Z-ADD     *ZEROS        LCAP1
     C                   Z-ADD     1000          LCAP2
     C                   Z-ADD     *ZEROS        LPRS2
     C                   Z-ADD     *ZEROS        LPRS2C
     C     ISEN          IFNE      1
     C     PRQIV         DIV(H)    1.2           LPRS1
     C                   ELSE
     C                   Z-ADD     PRQIV         LPRS1
     C                   END
     C                   Z-ADD     PRQIV         LPRS1C
     C                   MOVE      '054'         LCOBER
      *
     C                   EXSR      SRQIV2
[LG] C*-----------------------------------------
     C                   EXSR      ADDCOB
     C*                    EXCPTREGCOB
[LG] C*-----------------------------------------
     C                   END
     C*
     C     EQIV          ENDSR
     C*****************************************************
     C*   QIV - PARTE 2                                   *
     C********************************************    > < =
     C     SRQIV2        BEGSR
     C     TALT          IFEQ      0
     C     WDTINI        ANDGE     110201
     C                   Z-ADD     50            LFRQIV
     C                   ENDIF
     C                   ENDSR
     C*****************************************************
     C*   FORMATA COBERTURAS DE PRIVAÇÃO DE USO           *
     C********************************************    > < =
     C     SRPRIV        BEGSR
     C*
     C     PRIV          IFNE      ' '
     C                   MOVE      *BLANKS       LNDIAS                                     TRANSPOR
     C                   MOVEL     *BLANKS       CDTAB
     C                   MOVE      'PU'          TPTAB
     C                   MOVEL     PRIV          CDTAB
     C     KTAB          CHAIN     FTABX                              69
     C  N69              DO
      *
     C                   Z-ADD     1             I
     C     COMB          LOOKUP    COMBG(I)                               20
     C   20              MOVE      COMBL(I)      LCOMB
     C  N20              MOVE      '2'           LCOMB
      *
     C     PRIV          IFGE      '1'
     C     PRIV          ANDLE     '9'
     C                   MOVE      '057'         LCOBER
     C                   MOVE      NODIAS        LNDIAS
     C                   MOVE      *BLANKS       LCOMB
     C                   EXSR      WRTCOB
     C                   ENDIF
      *
     C                   ENDDO
     C                   ENDIF
      * 01/03
     C     VSA           IFNE      ' '
     C                   MOVEL     *BLANKS       CDTAB
     C                   MOVE      'VS'          TPTAB
     C                   MOVEL     VSA           CDTAB
     C     KTAB          CHAIN     FTABX                              69
     C  N69              DO
     C                   MOVE      064           LCOBER
     C                   MOVE      *BLANKS       LNDIAS
     C     VSA           IFEQ      '1'
     C                   MOVE      01            LCODCC
     C                   ELSE
     C                   MOVE      03            LCODCC
     C                   ENDIF
      * 02
     C     CILIND        IFGT      1900
     C     VSA           ANDEQ     '1'
     C                   MOVE      02            LCODCC
     C                   ENDIF
     C                   EXSR      WRTCOB
     C                   ENDDO
     C                   ENDIF
      *---
      * 01/03
     C     VIA           IFNE      ' '
     C                   MOVEL     *BLANKS       CDTAB
     C                   MOVE      'VI'          TPTAB
     C                   MOVEL     VIA           CDTAB
     C     KTAB          CHAIN     FTABX                              69
     C  N69              DO
     C                   MOVE      065           LCOBER
     C                   MOVE      *BLANKS       LNDIAS
     C     VIA           IFEQ      '1'
     C                   MOVE      01            LCODCC
     C                   ELSE
     C                   MOVE      03            LCODCC
     C                   ENDIF
      * 02
     C     CILIND        IFGT      1900
     C     VIA           ANDEQ     '1'
     C                   MOVE      *BLANKS       LNDIAS
     C                   MOVE      02            LCODCC
     C                   ENDIF
     C                   EXSR      WRTCOB
     C                   ENDDO
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       LNDIAS
     C                   MOVE      *BLANKS       LCODCC
     C                   MOVE      *BLANKS       LCOMB
     C*
     C     EPRIV         ENDSR
     C*****************************************************
     C*   PESQUISA CLAUSULAS ESPECIAIS                    *
     C********************************************    > < =
     C     SRCL          BEGSR
     C*
     C     CONT          IFEQ      1
     C                   MOVEL     CONT          WRKCL             4 0
     C                   MOVE      CL01          WRKCL
     C                   ELSE
     C     CONT          IFEQ      2
     C                   MOVEL     CONT          WRKCL
     C                   MOVE      CL02          WRKCL
     C                   ELSE
     C     CONT          IFEQ      3
     C                   MOVEL     CONT          WRKCL
     C                   MOVE      CL03          WRKCL
     C                   ELSE
     C     CONT          IFEQ      4
     C                   MOVEL     CONT          WRKCL
     C                   MOVE      CL04          WRKCL
     C                   ELSE
     C     CONT          IFEQ      5
     C                   MOVEL     CONT          WRKCL
     C                   MOVE      CL05          WRKCL
     C                   ELSE
     C     CONT          IFEQ      6
     C                   MOVEL     CONT          WRKCL
     C                   MOVE      CL06          WRKCL
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C*
     C                   ENDSR
     C****************************************************
     C*   PESQUISA CLAUSULAS ESPECIAIS - INCENDIO         *
     C********************************************    > < =
     C     SRCP12        BEGSR
     C*
     C                   Z-ADD     *ZEROS        LCAP1
     C                   Z-ADD     *ZEROS        LPRS1
     C                   Z-ADD     *ZEROS        LPRS1C
     C                   Z-ADD     *ZEROS        LCAP2
     C                   Z-ADD     *ZEROS        LPRS2
     C                   Z-ADD     *ZEROS        LPRS2C
     C*
     C* DESDOBRAMENTO DE CAPITAIS E PRÉMIOS PARA RM=01 INCENDIO
     C*
     C     LAMBIN        IFGE      '1'
     C     LAMBIN        IFLE      '3'
     C*
     C     CR1           IFEQ      1
     C                   Z-ADD     CAP1          LCAP1
     C     ISEN          IFNE      1                                                   I
     C     PR1           DIV(H)    1.2           LPRS1
     C                   ELSE
     C                   Z-ADD     PR1           LPRS1
     C                   END
     C                   Z-ADD     PR1           LPRS1C
     C                   END
     C     CR2           IFEQ      1
     C                   ADD       CAP2          LCAP1
     C     ISEN          IFNE      1                                                   I
     C     PR2           DIV(H)    1.2           LPRS1
     C                   ELSE
     C                   Z-ADD     PR2           LPRS1
     C                   END
     C                   Z-ADD     PR2           WPRSC
     C                   ADD       WPRSC         LPRS1C
     C                   END
     C     CR3           IFEQ      1
     C                   ADD       CAP3          LCAP1
     C     ISEN          IFNE      1                                                   I
     C     PR3           DIV(H)    1.2           LPRS1
     C                   ELSE
     C                   Z-ADD     PR3           LPRS1
     C                   END
     C                   Z-ADD     PR3           WPRSC
     C                   ADD       WPRSC         LPRS1C
     C                   END
     C*
     C     CR1           IFEQ      2
     C                   Z-ADD     CAP1          LCAP2
     C     ISEN          IFNE      1                                                   I
     C     PR1           DIV(H)    1.2           LPRS2
     C                   ELSE
     C                   Z-ADD     PR1           LPRS2
     C                   END
     C                   Z-ADD     PR1           LPRS2C
     C                   END
     C     CR2           IFEQ      2
     C                   ADD       CAP2          LCAP2
     C     ISEN          IFNE      1                                                   I
     C     PR2           DIV(H)    1.2           LPRS2
     C                   ELSE
     C                   Z-ADD     PR2           LPRS2
     C                   END
     C                   Z-ADD     PR2           WPRSC
     C                   ADD       WPRSC         LPRS2C
     C                   END
     C     CR3           IFEQ      2
     C                   ADD       CAP3          LCAP2
     C     ISEN          IFNE      1                                                   I
     C     PR3           DIV(H)    1.2           LPRS2
     C                   ELSE
     C                   Z-ADD     PR3           LPRS2
     C                   END
     C                   Z-ADD     PR3           WPRSC
     C                   ADD       WPRSC         LPRS2C
     C                   END
     C*
     C                   END
     C                   END
     C*
     C* DESDOBRAMENTO DE CAPITAIS E PRÉMIOS PARA RM=05 M.RISCOS
     C*
     C     LAMBMH        IFGE      '1'
     C     LAMBMH        IFLE      '3'
     C*
     C     CR1           IFEQ      1
     C                   Z-ADD     CAP1          LCAP1
     C     ISEN          IFNE      1                                                   I
     C     PR1           DIV(H)    1.2           LPRS1
     C                   ELSE
     C                   Z-ADD     PR1           LPRS1
     C                   END
     C                   Z-ADD     PR1           LPRS1C
     C                   END
     C     CR2           IFEQ      1
     C                   ADD       CAP2          LCAP1
     C     ISEN          IFNE      1                                                   I
     C     PR2           DIV(H)    1.2           LPRS1
     C                   ELSE
     C                   Z-ADD     PR2           LPRS1
     C                   END
     C                   Z-ADD     PR2           WPRSC
     C                   ADD       WPRSC         LPRS1C
     C                   END
     C     CR3           IFEQ      1
     C                   ADD       CAP3          LCAP1
     C     ISEN          IFNE      1                                                   I
     C     PR3           DIV(H)    1.2           LPRS1
     C                   ELSE
     C                   Z-ADD     PR3           LPRS1
     C                   END
     C                   Z-ADD     PR3           WPRSC
     C                   ADD       WPRSC         LPRS1C
     C                   END
     C*
     C     CR1           IFEQ      2
     C                   Z-ADD     CAP1          LCAP2
     C     ISEN          IFNE      1                                                   I
     C     PR1           DIV(H)    1.2           LPRS2
     C                   ELSE
     C                   Z-ADD     PR1           LPRS2
     C                   END
     C                   Z-ADD     PR1           LPRS2C
     C                   END
     C     CR2           IFEQ      2
     C                   ADD       CAP2          LCAP2
     C     ISEN          IFNE      1                                                   I
     C     PR2           DIV(H)    1.2           LPRS2
     C                   ELSE
     C                   Z-ADD     PR2           LPRS2
     C                   END
     C                   Z-ADD     PR2           WPRSC
     C                   ADD       WPRSC         LPRS2C
     C                   END
     C     CR3           IFEQ      2
     C                   ADD       CAP3          LCAP2
     C     ISEN          IFNE      1                                                   I
     C     PR3           DIV(H)    1.2           LPRS2
     C                   ELSE
     C                   Z-ADD     PR3           LPRS2
     C                   END
     C                   Z-ADD     PR3           WPRSC
     C                   ADD       WPRSC         LPRS2C
     C                   END
     C*
     C                   END
     C                   END
     C*
     C* DESDOBRAMENTO DE CAPITAIS E PRÉMIOS PARA RM=55 M.RISCOS
     C*
     C     LAMBMC        IFGE      '1'
     C     LAMBMC        IFLE      '3'
     C*
     C     CR1           IFEQ      1
     C                   Z-ADD     CAP1          LCAP1
     C     ISEN          IFNE      1                                                   I
     C     PR1           DIV(H)    1.2           LPRS1
     C                   ELSE
     C                   Z-ADD     PR1           LPRS1
     C                   END
     C                   Z-ADD     PR1           LPRS1C
     C                   END
     C     CR2           IFEQ      1
     C                   ADD       CAP2          LCAP1
     C     ISEN          IFNE      1                                                   I
     C     PR2           DIV(H)    1.2           LPRS1
     C                   ELSE
     C                   Z-ADD     PR2           LPRS1
     C                   END
     C                   Z-ADD     PR2           WPRSC
     C                   ADD       WPRSC         LPRS1C
     C                   END
     C     CR3           IFEQ      1
     C                   ADD       CAP3          LCAP1
     C     ISEN          IFNE      1                                                   I
     C     PR3           DIV(H)    1.2           LPRS1
     C                   ELSE
     C                   Z-ADD     PR3           LPRS1
     C                   END
     C                   Z-ADD     PR3           WPRSC
     C                   ADD       WPRSC         LPRS1C
     C                   END
     C*
     C     CR1           IFEQ      2
     C                   Z-ADD     CAP1          LCAP2
     C     ISEN          IFNE      1                                                   I
     C     PR1           DIV(H)    1.2           LPRS2
     C                   ELSE
     C                   Z-ADD     PR1           LPRS2
     C                   END
     C                   Z-ADD     PR1           LPRS2C
     C                   END
     C     CR2           IFEQ      2
     C                   ADD       CAP2          LCAP2
     C     ISEN          IFNE      1                                                   I
     C     PR2           DIV(H)    1.2           LPRS2
     C                   ELSE
     C                   Z-ADD     PR2           LPRS2
     C                   END
     C                   Z-ADD     PR2           WPRSC
     C                   ADD       WPRSC         LPRS2C
     C                   END
     C     CR3           IFEQ      2
     C                   ADD       CAP3          LCAP2
     C     ISEN          IFNE      1                                                   I
     C     PR3           DIV(H)    1.2           LPRS2
     C                   ELSE
     C                   Z-ADD     PR3           LPRS2
     C                   END
     C                   Z-ADD     PR3           WPRSC
     C                   ADD       WPRSC         LPRS2C
     C                   END
     C*
     C                   END
     C                   END
     C*
     C                   ENDSR
     C*
     C*****************************************************
     C*   DETERMINA CLAUSULAS ESPECIAIS - AUTOMOVEL       *
     C********************************************    > < =
     C     SRCL50        BEGSR
     C*
     C* FORMATA COBERTURA RC FACULTATIVA
     C*
     C     WCR0          IFEQ      1                                            <--------+
     C                   Z-ADD     *ZEROS        $RC                                     I
     C                   Z-ADD     *ZEROS        $DP                                     I
     C*                                                             I
     C     CAPRC         IFGT      6000                                         <------+ I
     C     CAPRC         IFEQ      99999                                        <----+ I I
     C                   MOVE      9999999999    LCAP1                               I I I
     C                   MOVEL     999           LCAP1                               I I I
     C                   ELSE                                                        * I I
     C     CAPRC         IFEQ      6000
     C***  2750          MULT      1000          $RC                                 I I I
     C                   ELSE
     C     CAPRC         MULT      1000          $RC                                 I I I
     C                   END
     C                   Z-ADD     $RC           LCAP1                               I I I
     C                   Z-ADD     $DP           LCAP2                               I I I
     C                   END                                                    <----+ I I
     C                   Z-ADD     *ZEROS        LPRS1                                 I I
     C                   Z-ADD     *ZEROS        LPRS1C                                I I
     C                   Z-ADD     *ZEROS        LPRS2                                 I I
     C                   Z-ADD     *ZEROS        LPRS2C                                I I
     C                   MOVE      '002'         LCOBER                                I I
[LG] C*-----------------------------------------
     C                   EXSR      ADDCOB
     C*                    EXCPTREGCOB                            I
[LG] C*-----------------------------------------
     C                   END                                                    <------+ I
     C*                                                             I
     C* FORMATA COBERTURA DE QUEBRA ISOLADA DE VIDROS               I
     C*                                                             I
     C*      ------------------------------------------             I
     C*      - REFORÇA VALIDAÇAO-DUPLICAÇAO COBERTURAS              I
     C*                                                             I
     C     CR2           IFEQ      3                                                     I
     C                   GOTO      FORCA1                                                I
     C                   ELSE                                                            I
     C     CR2           IFEQ      8                                                     I
     C                   GOTO      FORCA1                                                I
     C                   ELSE                                                            I
     C                   END                                                             I
     C                   END                                                             I
     C*                                                             I
     C*      ------------------------------------------             I
     C*      - CAMPO  SUBSTITUIDO A PEDIDO DE LUSITANIA             I
     C*                                                             I
     C*                    Z-ADD150000    LCAP1                     I
     C*  PASSOU A EUROS    Z-ADD1000      LCAP1                     I
     C*                    Z-ADD$DP       LCAP2                     I
     C*      ------------------------------------------             I
     C                   Z-ADD     $RC           LCAP1                                   I
     C                   Z-ADD     1000          LCAP2                                   I
     C                   Z-ADD     *ZEROS        LPRS1                                   I
     C                   Z-ADD     *ZEROS        LPRS1C                                  I
     C                   Z-ADD     *ZEROS        LPRS2                                   I
     C                   Z-ADD     *ZEROS        LPRS2C                                  I
     C                   MOVE      '054'         LCOBER                                  I
      *
     C                   EXSR      SRQIV2
[LG] C*-----------------------------------------
     C                   EXSR      ADDCOB
     C*                    EXCPTREGCOB                            I
[LG] C*-----------------------------------------
     C     FORCA1        TAG                                                             I
     C                   END                                                    <--------+
     C*
     C     WCR0          IFEQ      2                                            <--------+
     C                   Z-ADD     *ZEROS        $RC                                     I
     C                   Z-ADD     *ZEROS        $DP                                     I
     C*                                                             I
     C     CAPRC         IFGT      6000                                         <------+ I
     C     CAPRC         IFEQ      99999                                        <----+ I I
     C                   MOVE      9999999999    LCAP1                               I I I
     C                   MOVEL     999           LCAP1                               I I I
     C                   ELSE                                                        * I I
     C     CAPRC         IFEQ      6000
     C***  2750          MULT      1000          $RC                                 I I I
     C                   ELSE
     C     CAPRC         MULT      1000          $RC                                 I I I
     C                   END                                                    <----+ I I
     C                   Z-ADD     $RC           LCAP1                               I I I
     C                   Z-ADD     $DP           LCAP2                               I I I
     C                   END                                                    <----+ I I
     C                   Z-ADD     *ZEROS        LPRS1                                 I I
     C                   Z-ADD     *ZEROS        LPRS1C                                I I
     C                   Z-ADD     *ZEROS        LPRS2                                 I I
     C                   Z-ADD     *ZEROS        LPRS2C                                I I
     C                   MOVE      '002'         LCOBER                                I I
[LG] C*-----------------------------------------
     C                   EXSR      ADDCOB
     C*                    EXCPTREGCOB                            I
[LG] C*-----------------------------------------
     C                   END                                                    <------+ I
     C                   END                                                    <--------+
     C*
     C     WCR0          IFEQ      3                                            <--------+
     C                   Z-ADD     *ZEROS        $RC                                     I
     C                   Z-ADD     *ZEROS        $DP                                     I
     C*                                                             I
     C* I.R.E.                                                      I
     C                   Z-ADD     *ZEROS        LCAP1                                   I
     C                   Z-ADD     *ZEROS        LCAP2                                   I
     C                   Z-ADD     *ZEROS        LPRS1                                   I
     C                   Z-ADD     *ZEROS        LPRS1C                                  I
     C                   Z-ADD     *ZEROS        LPRS2                                   I
     C                   Z-ADD     *ZEROS        LPRS2C                                  I
     C                   MOVE      '004'         LCOBER                                  I
[LG] C*-----------------------------------------
     C                   EXSR      ADDCOB
     C*                    EXCPTREGCOB                            I
[LG] C*-----------------------------------------
     C* F.R.                                                        I
     C                   Z-ADD     *ZEROS        LCAP1                                   I
     C                   Z-ADD     *ZEROS        LCAP2                                   I
     C                   Z-ADD     *ZEROS        LPRS1                                   I
     C                   Z-ADD     *ZEROS        LPRS1C                                  I
     C                   Z-ADD     *ZEROS        LPRS2                                   I
     C                   Z-ADD     *ZEROS        LPRS2C                                  I
     C                   MOVE      '005'         LCOBER                                  I
[LG] C*-----------------------------------------
     C                   EXSR      ADDCOB
     C*                    EXCPTREGCOB                            I
[LG] C*-----------------------------------------
     C* Q.I.V.                                                      I
     C                   Z-ADD     *ZEROS        LCAP1                                   I
     C                   Z-ADD     1000          LCAP2                                   I
     C                   Z-ADD     *ZEROS        LPRS1                                   I
     C                   Z-ADD     *ZEROS        LPRS1C                                  I
     C                   Z-ADD     *ZEROS        LPRS2                                   I
     C                   Z-ADD     *ZEROS        LPRS2C                                  I
     C                   MOVE      '054'         LCOBER                                  I
      *
     C                   EXSR      SRQIV2
[LG] C*-----------------------------------------
     C                   EXSR      ADDCOB
     C*                    EXCPTREGCOB                            I
[LG] C*-----------------------------------------
     C                   END                                                    <--------+
     C*
     C     WCR0          IFEQ      4                                            <--------+
     C                   Z-ADD     *ZEROS        $RC                                     I
     C                   Z-ADD     *ZEROS        $DP                                     I
     C* Q.I.V.                                                      I
     C                   Z-ADD     *ZEROS        LCAP1                                   I
     C                   Z-ADD     1000          LCAP2                                   I
     C                   Z-ADD     *ZEROS        LPRS1                                   I
     C                   Z-ADD     *ZEROS        LPRS1C                                  I
     C                   Z-ADD     *ZEROS        LPRS2                                   I
     C                   Z-ADD     *ZEROS        LPRS2C                                  I
     C                   MOVE      '054'         LCOBER                                  I
      *
     C                   EXSR      SRQIV2
[LG] C*-----------------------------------------
     C                   EXSR      ADDCOB
     C*                    EXCPTREGCOB                            I
[LG] C*-----------------------------------------
     C                   END                                                    <--------+
     C*
     C     WCR0          IFEQ      8                                            <--------+
     C                   Z-ADD     *ZEROS        $RC                                     I
     C                   Z-ADD     *ZEROS        $DP                                     I
     C* I.R.E.                                                      I
     C                   Z-ADD     $RC           LCAP1                                   I
     C                   Z-ADD     $DP           LCAP2                                   I
     C                   Z-ADD     *ZEROS        LPRS1                                   I
     C                   Z-ADD     *ZEROS        LPRS1C                                  I
     C                   Z-ADD     *ZEROS        LPRS2                                   I
     C                   Z-ADD     *ZEROS        LPRS2C                                  I
     C                   MOVE      '004'         LCOBER                                  I
[LG] C*-----------------------------------------
     C                   EXSR      ADDCOB
     C*                    EXCPTREGCOB                            I
[LG] C*-----------------------------------------
     C* F.R.                                                        I
     C                   Z-ADD     $RC           LCAP1                                   I
     C                   Z-ADD     $DP           LCAP2                                   I
     C                   Z-ADD     *ZEROS        LPRS1                                   I
     C                   Z-ADD     *ZEROS        LPRS1C                                  I
     C                   Z-ADD     *ZEROS        LPRS2                                   I
     C                   Z-ADD     *ZEROS        LPRS2C                                  I
     C                   MOVE      '005'         LCOBER                                  I
[LG] C*-----------------------------------------
     C                   EXSR      ADDCOB
     C*                    EXCPTREGCOB                            I
[LG] C*-----------------------------------------
     C* F.NATUREZA                                                  I
     C                   Z-ADD     $RC           LCAP1                                   I
     C                   Z-ADD     $DP           LCAP2                                   I
     C                   Z-ADD     *ZEROS        LPRS1                                   I
     C                   Z-ADD     *ZEROS        LPRS1C                                  I
     C                   Z-ADD     *ZEROS        LPRS2                                   I
     C                   Z-ADD     *ZEROS        LPRS2C                                  I
     C                   MOVE      '053'         LCOBER                                  I
[LG] C*-----------------------------------------
     C                   EXSR      ADDCOB
     C*                    EXCPTREGCOB                            I
[LG] C*-----------------------------------------
     C* Q.I.V.                                                      I
     C                   Z-ADD     $RC           LCAP1                                   I
     C                   Z-ADD     1000          LCAP2                                   I
     C                   Z-ADD     *ZEROS        LPRS1                                   I
     C                   Z-ADD     *ZEROS        LPRS1C                                  I
     C                   Z-ADD     *ZEROS        LPRS2                                   I
     C                   Z-ADD     *ZEROS        LPRS2C                                  I
     C                   MOVE      '054'         LCOBER                                  I
      *
     C                   EXSR      SRQIV2
[LG] C*-----------------------------------------
     C                   EXSR      ADDCOB
     C*                    EXCPTREGCOB                            I
[LG] C*-----------------------------------------
     C* A.MALICIOSOS                                                I
     C                   Z-ADD     $RC           LCAP1                                   I
     C                   Z-ADD     $DP           LCAP2                                   I
     C                   Z-ADD     *ZEROS        LPRS1                                   I
     C                   Z-ADD     *ZEROS        LPRS1C                                  I
     C                   Z-ADD     *ZEROS        LPRS2                                   I
     C                   Z-ADD     *ZEROS        LPRS2C                                  I
     C                   MOVE      '056'         LCOBER                                  I
[LG] C*-----------------------------------------
     C                   EXSR      ADDCOB
     C*                    EXCPTREGCOB                            I
[LG] C*-----------------------------------------
     C                   END                                                    <--------+
     C*
     C                   ENDSR
     C****************************************************
     C*   VALIDA A CRIAÇAO DE OUTRAS COBERTURAS           *
     C********************************************    > < =
     C     SROCOB        BEGSR
     C*
     C     RR            IFEQ      01
     C                   Z-ADD     *ZEROS        WCR1
     C                   Z-ADD     *ZEROS        WCR2
     C                   Z-ADD     *ZEROS        WCR3
     C                   END
     C     RR            IFEQ      04
     C                   Z-ADD     *ZEROS        WCR1
     C                   Z-ADD     *ZEROS        WCR2
     C                   Z-ADD     *ZEROS        WCR3
     C                   END
     C     RR            IFEQ      05
     C                   Z-ADD     *ZEROS        WCR1
     C                   Z-ADD     *ZEROS        WCR2
     C                   Z-ADD     *ZEROS        WCR3
     C                   END
     C     RR            IFEQ      06
     C                   Z-ADD     *ZEROS        WCR1
     C                   Z-ADD     *ZEROS        WCR2
     C                   Z-ADD     *ZEROS        WCR3
     C                   END
     C     RR            IFEQ      07
     C                   Z-ADD     *ZEROS        WCR1
     C                   Z-ADD     *ZEROS        WCR2
     C                   Z-ADD     *ZEROS        WCR3
     C                   END
     C     RR            IFEQ      08
     C                   Z-ADD     *ZEROS        WCR1
     C                   Z-ADD     *ZEROS        WCR2
     C                   Z-ADD     *ZEROS        WCR3
     C                   END
     C     RR            IFEQ      09
     C                   Z-ADD     *ZEROS        WCR1
     C                   Z-ADD     *ZEROS        WCR2
     C                   Z-ADD     *ZEROS        WCR3
     C                   END
     C     RR            IFEQ      17
     C                   Z-ADD     *ZEROS        WCR1
     C                   Z-ADD     *ZEROS        WCR2
     C                   Z-ADD     *ZEROS        WCR3
     C                   END
     C     RR            IFEQ      18
     C                   Z-ADD     *ZEROS        WCR1
     C                   Z-ADD     *ZEROS        WCR2
     C                   Z-ADD     *ZEROS        WCR3
     C                   END
     C     RR            IFEQ      55
     C                   Z-ADD     *ZEROS        WCR1
     C                   Z-ADD     *ZEROS        WCR2
     C                   Z-ADD     *ZEROS        WCR3
     C                   END
     C                   ENDSR
     C*
     C*****************************************************
     C*   COBERTURAS RESIDENTIA / CONDOMINIO              *
     C********************************************    > < =
     C     SRCL05        BEGSR
     C*
     C                   MOVEL     10            WRKCL
     C                   Z-ADD     1             CONT
     C     CONT          DOWLE     13
     C     CF(CONT)      IFNE      ' '
     C                   MOVE      CONT          WRKCL
     C                   Z-ADD     1             I
     C     CONDOM        IFEQ      ' '
     C     WRKCL         LOOKUP    C05GR(I)                               20
     C   20              MOVE      C05LR(I)      LCOBER
     C                   ELSE
     C     WRKCL         LOOKUP    C05GC(I)                               20
     C   20              MOVE      C05LC(I)      LCOBER
     C                   END
     C   20              EXCEPT    REGCCL
     C                   END
     C                   ADD       1             CONT
     C                   END
      *
     C***  CE001         IFNE      ' '
     C***                MOVE      099           LCOBER
     C***                EXCEPT    REGCCL
     C***                END
      *
     C                   ENDSR
     C*****************************************************
     C*   COBERTURAS MERCANTILE                           *
     C********************************************    > < =
     C     SRCL55        BEGSR
     C*
     C                   MOVEL     10            WRKCL
     C                   Z-ADD     1             CONT
     C     CONT          DOWLE     13
     C     CF(CONT)      IFNE      ' '
     C                   MOVE      CONT          WRKCL
     C                   Z-ADD     1             I
     C     WRKCL         LOOKUP    C55GM(I)                               20
     C   20              DO
     C                   MOVE      C55LM(I)      LCOBER
      *
     C     LCOBER        IFEQ      '106'                                        <-------+ I
     C     PPERDI        IFGT      0                                            <-----+ I I
     C     PPERDI        DIV       100           WPI               5 3                I I I
     C                   ELSE                                                         * I I
     C                   Z-ADD     30            PPERDI                               I I I
     C     PPERDI        DIV       100           WPI               5 3                I I I
     C                   END                                                    <-----+ I I
     C     WCAP2         MULT      WPI           WCAPPI           13 0                  I I
     C                   Z-ADD     WCAPPI        LCAPPI                                 I I
     C                   END                                                    <-------+ I
      *
     C   20              EXCEPT    REGCCL
     C                   END
     C                   END
     C                   ADD       1             CONT
     C                   END
     C*
     C     CD103         IFNE      ' '
     C                   MOVE      111           LCOBER
     C                   EXCEPT    REGCCL
     C                   END
     C*
     C                   ENDSR
     C****************************************************************
     C* PRIVAÇÃO DE USO ASSOCIADO À AV "B" E "V"                     *
     C****************************************************************
     CSR   SRPU2         BEGSR
      *
     C                   MOVE      '078'         LCOBER
     C                   Z-ADD     0             LPRS1
     C                   Z-ADD     0             LPRS1C
     C                   Z-ADD     CAPIT1        LCAP1
     C     CILIND        IFLE      1900
     C                   MOVE      01            LCODCC
     C                   ELSE
     C                   MOVE      03            LCODCC
     C                   ENDIF
     C                   MOVE      05            LNDIAS
     C                   MOVEL     *BLANKS       LCODCC
     C                   MOVEL     *BLANKS       LCOMB
     C*-----
     C                   EXSR      ADDCOB
      *
     CSR                 ENDSR
     O********************************************************************
     O* OUTPUT - APÓLICES                                                *
     O********************************************************************
     CSR   WRTCOB        BEGSR
     C                   Z-ADD     *ZEROS        LCAP1
     C                   Z-ADD     *ZEROS        LPRS1
     C                   Z-ADD     *ZEROS        LPRS1C
     C     ISEN          IFNE      1
     C     VALOR1        DIV(H)    1.2           LPRS1
     C                   ELSE
     C                   Z-ADD     VALOR1        LPRS1
     C                   END
     C                   Z-ADD     VALOR1        LPRS1C
     C                   Z-ADD     CAPIT1        LCAP1
[LG] C*-----------------------------------------
     C                   EXSR      ADDCOB
     C*                    EXCPTREGCOB                            I
[LG] C*-----------------------------------------
     CSR                 ENDSR
     C********************************************************************
     C* ROTINA....: ADDCOB                                               *
     C* OBJECTIVO.: Adicionar um registo no Interface de Coberturas.     *
     C*             Aplicar as excepções solicitadas pela Lusitânia.     *
     C*                                                                  *
     C* Observação: Nas apólices frota os prémios são os únicos que      *
     C*             passam a ir sem valores. Os Capitais vão manter-se   *
     C*             Com valores, como originalmente.                     *
     C********************************************************************
     C     ADDCOB        BEGSR
     C*          -----------------------------------------
     C*           LIMPEZA DE CAMPOS A PEDIDO DE LUSITANIA
     C*          -----------------------------------------
     C*==> Se o ramo for Automóvel e apólice mãe então limpar
      *
     C                   IF        (RR = 50) AND (APMAE = 0) AND (FROTA = 'F')
     C                   Z-ADD     *ZEROS        LCAPT
     C*                  Z-ADD     *ZEROS        LPRSI
     C                   Z-ADD     *ZEROS        LCAP1
     C                   Z-ADD     *ZEROS        LCAP2
     C*                  Z-ADD     *ZEROS        LPRS1C
     C*                  Z-ADD     *ZEROS        LPRS2C
     C                   Z-ADD     *ZEROS        LCAPPI
     C                   ENDIF
      *
     C     LPRCOM        IFLT      0
     C                   Z-SUB     LPRCOM        LPRCOM
     C                   ENDIF
     C     LPRS1         IFLT      0
     C                   Z-SUB     LPRS1         LPRS1
     C                   ENDIF
     C     LPRS2         IFLT      0
     C                   Z-SUB     LPRS2         LPRS2
     C                   ENDIF
     C     LPRSI         IFLT      0
     C                   Z-SUB     LPRSI         LPRSI
     C                   ENDIF
      *
     C                   EXCEPT    REGCOB
     C*
[LG] C                   ENDSR
     O********************************************************************
     O* OUTPUT - APÓLICES                                                *
     O********************************************************************
     OLFAPOL    EADD         REGAP
     O                       LRAMO                2
     O                       LMOD                 4
     O                       LSMOD                6
     O                       LAPOL               13
     O                       LNSEG               19
     O                       LNATS               20
     O                       LMMDDV              24
     O                       LFRAC               25
     O                       LCOBR               30
     O                       LDTUAT              38
     O                       LANUL               40
     O                       LDTANU              48
     O                       LDTINI              56
     O                       LDTFIM              64
     O                       LCONC               68
     O                       LLRPS              138
     O*
     O                       LACTAT             144
     O                       LTPACP             145
     O*
     O                       LTPAUT             147
     O                       LCATV              151
     O                       LMATR              163
     O                       LMARCA             183
     O                       LDTMAT             191
     O*
     O                       LFUNC              192
     O                       LACTIN             197
     O                       LTPCON             198
     O                       LAMBIN             199
     O*
     O                       LAMBMH             200
     O*
     O                       LAMBMC             201
     O                       LACTMC             205
     O                       LPROF              209
     O*
     O                       LCOMB2             210
     O                       LAPABE             211
     O*
     O                       LTPOCU             315
     O                       LMODUL             317
     O*
     O                       LCOSEG             318
     O                       LALTER             326
     O                       LTPACT             327
     O                       LMATR2             339
     O                       LMARC2             359
     O*
     O                       LRRAP              368
     O                       LUFICH             376
     O                       LDTTRF             384
     O* CAMPOS NOVOS
     O                       LTRISC             385
     O                       LCLTAR             387
     O                       LPERIO             388
     O                       LCONTR             389
     O                       LPCONT             392
     O                       LNPESS             398
     O                       LCILIN             403
     O*
     OLFCOBE    EADD         REGCOB
     O                       LRAMO                2
     O                       LMOD                 4
     O                       LSMOD                6
     O                       LAPOL               13
     O                       LCOBER              16
     O                       LCAPT               29
     O                       LPRSI               40
     O*
     O                       LCAP1               77
     O                       LCAP2               90
     O                       LPRS1C             101
     O                       LPRS2C             112
     O                       LCAPPI             125
     O                       WPI                130
     O                       LPRCOM             141
     O                       LPRS1              152
     O                       LPRS2              163
     O                       LNDIAS             165
     O                       LCODCC             167
     O                       LCOMB              168
     O                       LFRQIV             179
     O*
     O                       LALTER             359
     O                       LRRAP              368
     O                       LUFICH             376
     O                       LDTTRF             384
     O*
     O          EADD         REGCCL
     O                       LRAMO                2
     O                       LMOD                 4
     O                       LSMOD                6
     O                       LAPOL               13
     O                       LCOBER              16
     O                       LCAPT               29
     O                       LPRSI               40
     O*
     O                       LCAP1               77
     O                       LCAP2               90
     O                       LPRS1C             101
     O                       LPRS2C             112
     O                       LCAPPI             125
     O                       WPI                130
     O                       LPRCOM             141
     O                       LPRS1              152
     O                       LPRS2              163
     O                       LNDIAS             165
     O                       LCODCC             167
     O                       LCOMB              168
     O                       LFRQIV             179
     O*
     O                       LALTER             359
     O                       LRRAP              368
     O                       LUFICH             376
     O                       LDTTRF             384
     O*
** 01-TABELA DE CONVERSAO DE RAMOS  (LA VERSUS LV - RAMO/MODAL/SUMODAL)
01080000
02010200
03130300
04130500
05340100
06160100
07090600
08090600
09090600
10010205
14300100
15010101
15010102
17340700
18090304
19340600
50310000
51010301
55340200
65090400
67070002
68070002
69070002
70070003
** 02-TABELA DE CONVERSAO DA NATUREZA DE SEGUROS
11     ANO E SEGUINTES + ANO E SEGUINTES C/INDEXAÇÃO
25     TEMPORÁRIO (PERIODO CURTO)
33     APOLICE ABERTA
** 03-TABELA DE CONVERSAO DE FRACCIONAMENTO
11     ANUAL
22     SEMESTRAL
33     TRIMESTRAL
44     MENSAL
59     UNICO
** 04-TABELA DE CONVERSAO DE COD. ANULAÇÃO
10000  EM VIGOR
20000  EM VIGOR (SUSPENSA NA GAL SEM CORRESPONDENCIA NA LUSITANIA)
30009  P/DECISÃO DA COMPANHIA
36104  PEDIDO DO SEGURADO (TRANSFERENCIA)
36202  FALTA DE PAGAMENTO
36308  SUBSTITUIÇÃO DA APOLICE
36409  ANULAÇÃO P/DECISÃO DA COMPANHIA
36503  PEDIDO DO SEGURADO (EXTINÇÃO DO RISCO)
38105  FALSAS DECLARAÇÕES
38204  PEDIDO DO SEGURADO (TRANSFERENCIA)
38314  EMISSÃO INVÁLIDA
38409  ANULAÇÃO P/DECISÃO DA COMPANHIA
38511  CADUCIDADE P/CONDIÇÕES PRÓPRIAS
38603  PEDIDO DO SEGURADO (EXTINÇÃO DO RISCO)
38706  EXCESSO DE SINISTRALIDADE
38810  EXTINÇÃO RISCO POR SINISTRO
38912  FALTA PAGAMENTO - DEVOLUÇÃO CHEQUE
** 05-DIA LIMITE DO SEGURO PARA DATA TERMO
013112
023101
032802
043103
053004
063105
073006
083107
093108
103009
113110
123011
** 06-MARCAS INVÁLIDAS (SEGURO GARAGISTA)
A INDICAR
AUTOMOBIL
GARAGISTA
S/CLIENTE
S/GARAGISTA
SEG GARAGI
SEG GARAGIS
SEG.GARAG
SEG.GARAGI
SEG.GARAGIS
SEG.GARAGISTA
** 07-MATRICULAS INVÁLIDAS (SEGURO GARAGISTA)
A INDICAR
AUTOMOBIL
GARAGISTA
S/CLIENTE
S/GARAGISTA
SEG GARAGI
SEG GARAGIS
SEG.GARAG
SEG.GARAGI
SEG.GARAGIS
SEG.GARAGISTA
** 08-CATEGORIAS DE VEÍCULOS
1300100
1400100
1500100
2420500
2430600
2520500
2530600
3540800
3550800
3560800
4101300
4201200
4301200
5009990
5011400
5021400
5031400
5041400
6400100
7400100
** 09-AMBITO DE INCENDIO
022
101
123
** 10-TABELA DE ASSOCIAÇOES DE RISCOS E COBERTURAS
010001
011001
012001
013001
014001
019001
020001
021001
022013
023012
024016
025001
026002
027003
029001
030001
031001
040001
041001
042001
049001
050001
051001
052001
053001
059001
060001
061001
062001
063001
069001
070001
071001
072001
079001
080001
081001
082001
089001
090001
091001
092001
099001
100000
140000
140000
150001
151001
152001
170001
171001
172001
173001
180001
181001
182001
183001
190001
191001
199001
500001       ( INICIALMENTE ERA 006 )
501001
502001
503003
504003
505004
506005
507004
508003       ( INICIALMENTE ERA 006 )
509005
510003
511003
512015
550001
551001
552001
553001
558001
650001
651001
** 11-TABELA DE CLAUSULA VERSUS COBERTURAS RAMO = 01
0114100
0115101
0116102
0117103
0118120
0119121
0120122
0121123
0122124
0123125
0214100
0215101
0216102
0217103
0218120
0219121
0220122
0221123
0222124
0223125
0314100
0315101
0316102
0317103
0318120
0319121
0320122
0321123
0322124
0323125
0414100
0415101
0416102
0417103
0418120
0419121
0420122
0421123
0422124
0423125
0514100
0515101
0516102
0517103
0518120
0519121
0520122
0521123
0522124
0523125
0614100
0615101
0616102
0617103
0618120
0619121
0620122
0621123
0622124
0623125
** 12-TABELA DE CLAUSULA VERSUS COBERTURAS RAMO = 06
0114004
0115004
0116004
0117004
0127005
0128005
0131003
0214004
0215004
0216004
0217004
0227005
0228005
0231003
0314004
0315004
0316004
0317004
0327005
0328005
0331003
0414004
0415004
0416004
0417004
0427005
0428005
0431003
0514004
0515004
0516004
0517004
0527005
0528005
0531003
0614004
0615004
0616004
0617004
0627005
0628005
0631003
** 13-TABELA DE CLAUSULA VERSUS COBERTURAS RAMO = 05
0101080
0102083
0103084
0104100
0199020
0201080
0202083
0203084
0204100
0299020
0301080
0302083
0303084
0304100
0399020
0401080
0402083
0403084
0404100
0499020
0501080
0502083
0503084
0504100
0599020
0601080
0602083
0603084
0604100
0699020
** 14-TABELA DE CLAUSULA VERSUS COBERTURAS RAMO = 17
0101002
0102004
0103006
0104005
0201002
0202004
0203006
0204005
0301002
0302004
0303006
0304005
0401002
0402004
0403006
0404005
0501002
0502004
0503006
0504005
0601002
0602004
0603006
0604005
** 15-TABELA DE CLAUSULA VERSUS COBERTURAS RAMO = 19
0110007
0210007
0310007
0410007
0510007
0610007
** 16-TABELA DE CLAUSULA VERSUS COBERTURAS RAMO = 55
0101101
0102102
0103103
0104100
0105106
0106107
0201101
0202102
0203103
0204100
0205106
0206107
0301101
0302102
0303103
0304100
0305106
0306107
0401101
0402102
0403103
0404100
0405106
0406107
0501101
0502102
0503103
0504100
0505106
0506107
0601101
0602102
0603103
0604100
0605106
0606107
** 17-TABELA DE RISCO OCUPANTES(AUTO) VERSUS TIPO PESSOA SEGURA
11
21
32
** 18-TABELA DE RISCO OCUPANTES(AUTO) VERSUS TIPO PESSOA SEGURA
1000001
1500002
2500003
** 19-TABELA DE PRIVAÇÃO DE USO
115
215
315
430
530
630
** 20-MESES/DIAS
013102280331043005310630073108310930103111301231
** 21-PROFISSÕES AC.TRABALHO
2002701
2021001
2041201
2063002
2073601
2101102
2133717
2141901
2152001
2161002
2171103
2182705
2202102
2283005
2283006
2303712
2311004
2351902
2363007
2381903
2441005
2451404
2462002
2473704
2482406
2592901
2601104
2611906
2621405
2632711
2651105
2682601
2692405
2722602
2743107
2762603
2792105
2803203
2811110
2823801
2853103
2871302
2881113
2893110
2903708
2932005
2951301
2962007
2981006
3001701
3011407
3023014
3043012
3113706
3163009
3182715
3193001
3203608
3213709
3222012
3232704
3243707
3253206
3262202
3272402
3293002
3313201
3343301
3361106
3373702
3382205
3433109
3441905
3492716
3512901
3561203
3583502
3592902
3662302
3702502
3711003
3792009
3801915
3812703
3862702
3903012
3923103
3941801
3951202
3983602
3992406
** 22-TABELA DE CLAUSULA VERSUS COBERTURAS RAMO = 05 (RESIDENTIA)
1001083
1002084
1003020
1004081
1005082
1006100
1007080
1013500
1999999
** 23-TABELA DE CLAUSULA VERSUS COBERTURAS RAMO = 05 (CONDOMINIO)
1001005
1002009
1003008
1004003
1005002
1006004
1007080
1008007
1013500
** 24-TABELA DE CLAUSULA VERSUS COBERTURAS RAMO = 55 (MERCANTILE)
1001102
1002122
1003106
1004109
1005104
1006100
1007101
1008107
1009120
1010113
1011114
1012110
1013500
** 25-
509941338
509941820
** 28-COMBUSTIVEL (LFCOBE)
D2
G1
** 29-COMBUSTIVEL (LFAPOL)
 0
G1
D2
L3
E4
H5
B6
