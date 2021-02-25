     H CURSYM('¤') DECEDIT('0,') DATEDIT(*YMD.)
     FGBASEGRT  IP   E           K DISK
     FFAPLDIT   IF   E           K DISK    EXTFILE('FAPLXXT')
     FLFAPOLT   IF A F  384    17AIDISK    KEYLOC(360) EXTFILE('LFAPOL')
     FLFCOBET   O  A F  384    17AIDISK    KEYLOC(360) EXTFILE('LFCOBE')
      *
     D RMG             S              2  0 DIM(4) CTDATA PERRCD(1)              RAMO/MOD/SMDO GAL VS LU
     D RML             S              6  0 DIM(4) ALT(RMG)
     D NTG             S              1  0 DIM(4) CTDATA PERRCD(1)              NATUREZA SEG. GAL VS LU
     D NTL             S              1  0 DIM(4) ALT(NTG)
     D CUG             S              3  0 DIM(17) CTDATA PERRCD(1)             COD.ANULAÇÃO  GAL VS LU
     D CUL             S              2  0 DIM(17) ALT(CUG)
     D TABMM           S              2  0 DIM(12) CTDATA PERRCD(1) ASCEND      DIA/MES VENCIMENTO
     D TABDT           S              4  0 DIM(12) ALT(TABMM)
     D TABM            S              2  0 DIM(12) CTDATA PERRCD(12) ASCEND     MESES/DIAS
     D TABD            S              2  0 DIM(12) ALT(TABM)
     D*
     D********************************************************************
     D* ESTRUTURA DE DADOS - LOCAL DATA AREAS                            *
     D********************************************************************
     D                UDS
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
     D                 DS
     D  RRAPOL                 1      9  0
     D  RAMO                   1      2  0
     D  APOL                   3      9  0
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
     D  WDTINI                 1      8  0
     D  AAINIC                 1      4  0
     D  MMINIC                 5      6  0
     D  DDINIC                 7      8  0
     D                 DS
     D  WDTALT                 1      8  0
     D  AAALT                  1      4  0
     D  MMALT                  5      6  0
     D  DDALT                  7      8  0
     D                 DS
     D  WDTVEN                 1      8  0
     D  AAVEN                  1      4  0
     D  MMVEN                  5      6  0
     D  DDVEN                  7      8  0
     D                 DS
     D  WALTER                 1      6  0
     D  AALTER                 1      2  0
     D  MALTER                 3      4  0
     D  DALTER                 5      6  0
     D                 DS
     D  DATA$                  1      8  0
     D  AA$                    1      4  0
     D  MM$                    5      6  0
     D  DD$                    7      8  0
      *
     I****************************************************
     I* FICHEIRO - APÓLICES ENVIADAS ( TRANSPORTES )     *
     I****************************************************
     ILFAPOLT   NS
     F********************************************************************
     F* DETERMINA DATA DE TRANSFERÊNCIA                                  *
     F********************************************************************
     C*
     C     FLAG          IFEQ      0                                            <-----------+
     C                   Z-ADD     1             FLAG              1 0                      I
     C*                                                                I
     C     KGRT          KLIST
     C                   KFLD                    RAMO
     C                   KFLD                    APOL
     C*                                                                I
     C* FORMATA DATA DA ALTERAÇÃO                                      I
     C*                                                                I
     C                   MOVE      UFICH         LUFICH            8                        I
     C                   MOVE      UDDALT        DALTER                                     I
     C                   MOVE      UMMALT        MALTER                                     I
     C                   MOVE      UAAALT        AALTER                                     I
     C                   END                                                    <-----------+
     C*
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
     C                   Z-ADD     0             LMMDDV            4 0
     C                   MOVE      *BLANKS       LFRAC             1
     C                   MOVE      *BLANKS       LCOBR             5
     C                   Z-ADD     0             LDTUAT            8 0
     C                   MOVE      *BLANKS       LANUL             2
     C                   Z-ADD     0             LDTANU            8 0
     C                   Z-ADD     0             LDTINI            8 0
     C                   Z-ADD     0             LDTFIM            8 0
     C                   MOVE      *ZEROS        LCONC             4
     C                   MOVE      *BLANKS       LLRPS            70
     C                   Z-ADD     0             LCOSEG            1 0
     C* ACIDENTES DE TRABALHO
     C*
     C                   MOVE      *BLANKS       LACTAT            6            * ACTIVIDADE (ACI.TRA
     C                   MOVE      *BLANKS       LTPACT            1            * TIPO SEGURO(FIXO /
     C                   MOVE      *BLANKS       LPROF             4            * PROFISSÕES
     C*
     C* ACIDENTES PESSOAIS
     C                   MOVE      *BLANKS       LTPACP            1            * TIPO DE SEGURO (ACP
     C*
     C* SEGURO AUTOMÓVEL
     C*
     C                   MOVE      *BLANKS       LTPAUT            2            * TIPO DE SEGURO  (AU
     C                   MOVE      *BLANKS       LCATV             4            * CATEGORIA DO VEÍCUL
     C                   MOVE      *BLANKS       LMARCA           20            * MARCA
     C                   MOVE      *BLANKS       LMATR            12            * MATRÍCULA
     C                   Z-ADD     0             LDTMAT            8 0          * DATA MATRÍCULA
     C*
     C* SEGURO INCENDIO
     C*
     C                   MOVE      *BLANKS       LFUNC             1            * FUNCAO
     C                   MOVE      *BLANKS       LACTIN            5            * ACTIVIDADE  (INCEND
     C                   MOVE      *BLANKS       LTPCON            1            * TIPO CONSTRUÇAO
     C                   MOVE      *BLANKS       LAMBIN            1            * AMBITO (INCÊNDIO)
     C*
     C* SEGURO MULTI-RISCOS HABITACAO
     C*
     C                   MOVE      *BLANKS       LAMBMH            1            * AMBITO (M.RISCOS HA
     C*
     C* SEGURO MULTI-RISCOS COMERCIAL
     C*
     C                   MOVE      *BLANKS       LAMBMC            1            * AMBITO
     C                   MOVE      *BLANKS       LACTMC            4            * ACTIVIDADE
     C*
     C* LIMPEZA DE CAMPOS ADICIONAIS GIL
     C*
     C                   Z-ADD     0             LALTER            8 0          * DATA ALTERACAO (PRO
     C                   MOVE      *BLANKS       LMATR2           12            * MATRÍCULA 2º VEÍCUL
     C                   MOVE      *BLANKS       LMARC2           20            * MARCA     2º VEÍCUL
     C*
     C* LIMPEZA DE CAMPOS DE CONTROLE GIL
     C*
     C                   MOVE      *BLANKS       LRRAP             9
     C*
     C* SEGURO AUTOMÓVEL (INFORMAÇÃO OCUPANTES)
     C*
     C                   MOVE      *BLANKS       LTPOCU            1            * TIPO PES. TRANSPORT
     C                   MOVE      *BLANKS       LMODUL            2            * MODULO GARANTIAS
     F********************************************************************
     F* LIMPA CAMPOS DE TRABALHO  - FICHEIRO DE COBERTURAS               *
     F********************************************************************
     C*
     C                   MOVE      *BLANKS       LCOBER            3
     C                   Z-ADD     *ZEROS        LCAPT            13 2
     C                   Z-ADD     *ZEROS        WPRSI            11 2
     C                   Z-ADD     *ZEROS        LPRSI            11 2
     C*
     C                   Z-ADD     *ZEROS        LCAP1            13 2
     C                   Z-ADD     *ZEROS        LCAP2            13 2
     C                   Z-ADD     *ZEROS        LPRS1            11 2
     C                   Z-ADD     *ZEROS        LPRS2            11 2
     C                   Z-ADD     *ZEROS        LPRCOM           11 2          * PREMIO COMERCIAL
     C                   Z-ADD     *ZEROS        LCAPPI           13 2          * CAPITAL PERDAS IND.
     C                   Z-ADD     *ZEROS        WPI               5 3          * CAPITAL PERDAS IND.
     C                   Z-ADD     *ZEROS        WCAP2            13 0
     C                   Z-ADD     *ZEROS        LPRS1C           11 2
     C                   Z-ADD     *ZEROS        LPRS2C           11 2
     C                   Z-ADD     *ZEROS        WPRSC            11 2
     C*
     C* SEGURO AUTOMÓVEL (PRIVAÇÃO DE USO )
     C*
     C                   MOVE      *BLANKS       LNDIAS            2            * N.DIAS    TRANSPORT
     C*
     F********************************************************************
     F* CONTROLA REGISTOS A CONVERTER E A ENVIAR                         *
     F********************************************************************
     C*
     C* FORMATA NOME DE FICHEIRO
     C*
     C                   MOVE      'GBASEGRT'    LUFICH
     C*
     C                   MOVEL     RRAPOL        KEYAP            17
     C                   MOVE      LUFICH        KEYAP
     C     KEYAP         CHAIN     LFAPOLT                            90
     C  N90              GOTO      FIM
     C*
     C********************************************************************
     C* CONVERSAO DE CAMPOS - APÓLICES                                   *
     C********************************************************************
     C*
     C                   Z-ADD     DTINI         WDTINI
     C                   Z-ADD     DTALT         WDTALT
     C                   Z-ADD     MACONT        AACONT            4 0
     C*
     C* CONVERTE RAMOS  ( RAMO GUARDIAN VS LUSITANIA ).
     C*
     C                   Z-ADD     1             I                 2 0
     C     RAMO          LOOKUP    RMG(I)                                 20
     C   20              MOVE      RML(I)        LURMS
     C  N20              MOVE      *BLANKS       LURMS
     C*
     C                   MOVE      LURR          LRAMO
     C                   MOVE      LUMOD         LMOD
     C                   MOVE      LUSMOD        LSMOD
     C*
     C                   MOVE      APOL          LAPOL
     C*
     C* COMPÕE CÓDIGO PESSOA
     C*
     C                   MOVEL     9             LNSEG
     C                   MOVE      NSEG          LNSEG
     C*
     C* CONVERTE NATUREZA DOS SEGUROS
     C*
     C                   Z-ADD     1             I
     C     TIPO          LOOKUP    NTG(I)                                 20
     C   20              MOVE      NTL(I)        LNATS
     C  N20              MOVE      *BLANKS       LNATS
     C*
     C* FORMATA VENCIMENTO  (DIA E MES VENCIMENTO)
     C*
     C     MES1          IFEQ      MMINIC
     C                   MOVEL     MMINIC        LMMDDV
     C                   MOVE      DDINIC        LMMDDV
     C                   ELSE
     C     MES1          IFEQ      0
     C                   MOVEL     MMINIC        LMMDDV
     C                   ELSE
     C                   MOVEL     MES1          LMMDDV
     C                   END
     C                   MOVE      DDINIC        LMMDDV
     C                   END
     C*
     C* FORMATA FRACCIONAMENTO
     C*
     C                   MOVE      '9'           LFRAC
     C*
     C* FORMATA DATA DA ULTIMA ACTA.
     C*
     C     ACTA          IFGT      *ZEROS
     C                   MOVE      DTALT         LDTUAT
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
     C                   MOVE      DTALT         LDTANU
     C                   ELSE
     C                   Z-ADD     *ZEROS        LDTANU
     C                   END
     C*
     C* FORMATA DATA DE INICIO DO CONTRATO
     C*
     C                   MOVE      DTINI         LDTINI
     C*
     C* FORMATA DATA DE TERMO DO CONTRATO
     C*
     C     DTFIM         IFNE      0
     C                   MOVE      DTFIM         LDTFIM
     C                   ELSE
     C                   EXSR      SRDTVC
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
     C                   Z-ADD     DDINIC        DDALT
     C                   Z-ADD     WDTALT        LDTANU
     C                   END
     C     LDTINI        IFGT      LDTANU
     C                   Z-ADD     LDTINI        LDTANU
     C                   END
     C                   END
     C*
     C* FORMATA LOCAL DO RISCO (CONCELHO) E LOCAL RISCO (LUGAR)
     C*
     C                   MOVE      CONC          LCONC
     C     TIPO          IFEQ      4
     C                   MOVEL     'APLICAÇÕ'    WORK             10
     C                   MOVE      'ES'          WORK
     C                   MOVEL     WORK          LLRPS
     C                   ELSE
     C                   MOVEL     DESCA         LLRPS
     C                   END
     C*
     C* FORMATA INFORMAÇÃO DE COSEGURO
     C*
     C     CS            IFEQ      0
     C                   Z-ADD     0             LCOSEG
     C                   ELSE
     C     CS            IFEQ      1
     C                   Z-ADD     2             LCOSEG
     C                   ELSE
     C     CS            IFEQ      2
     C                   Z-ADD     1             LCOSEG
     C                   ELSE
     C                   Z-ADD     0             LCOSEG
     C                   END
     C                   END
     C                   END
     C*
     C* FORMATA DATA DE ALTERAÇAO EFECTIVA (CARREGAMENTO/PROCESSAMENTO)
     C*
     C                   Z-ADD     *ZEROS        DTY2              8 0
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
     C* FORMATA CAMPOS DE CONTROL
     C*
     C                   MOVEL     RAMO          LRRAP
     C                   MOVE      APOL          LRRAP
     C                   Z-ADD     *DATE         LDTTRF            8 0
     C*
     C                   EXCEPT    REGAP
     C********************************************************************
     C* CONVERSAO DE CAMPOS - COBERTURAS                                 *
     C********************************************************************
      *
     C     TIPO          IFEQ      4
      *
     C     KGRT          SETLL     APLDIFT
     C     KGRT          READE     APLDIFT                                80
     C     *IN80         DOWEQ     *OFF
      * COBERTURA
     C                   MOVE      '001'         LCOBER
      * CAPITAL
     C                   ADD       ACAP1         LCAPT
      * PRÉMIO IMPUTADO
     C     ISEN          IFNE      1
     C     APR1          DIV       1,2           WPRSI
     C                   ADD       WPRSI         LPRSI
     C                   ELSE
     C                   ADD       APR1          LPRSI
     C                   END
      * PRÉMIO COMERCIAL
     C                   ADD       APR1          LPRCOM
      *
     C     *IN69         IFEQ      *OFF
     C                   MOVE      *ON           *IN69
     C                   END
      *
     C     KGRT          READE     APLDIFT                                80
     C                   END
      *
     C     *IN69         IFEQ      *ON
     C                   MOVE      *OFF          *IN69
     C                   EXCEPT    REGCCL
     C                   END
      *
     C                   ELSE
      * COBERTURA
     C                   MOVE      '001'         LCOBER
      * CAPITAL
     C                   Z-ADD     CAP1          LCAPT
      * PRÉMIO IMPUTADO
     C     ISEN          IFNE      1
     C     PR1           DIV       1,2           LPRSI
     C                   ELSE
     C                   Z-ADD     PR1           LPRSI
     C                   END
      * PRÉMIO COMERCIAL
     C                   Z-ADD     PR1           LPRCOM
     C                   EXCEPT    REGCCL
     C                   END
      *
      *
     C     FIM           TAG
      *
     C****************************************************
     C*   DETERMINA DATA VENCIMENTO                       *
     C********************************************    > < =
     C     SRDTVC        BEGSR
      *
     C     DDINIC        IFEQ      01                                           <--------------------
     C     MES1          LOOKUP    TABMM         TABDT                    21
     C  N21              Z-ADD     *ZEROS        DDVEN
     C  N21              Z-ADD     *ZEROS        MMVEN
     C  N21              Z-ADD     *ZEROS        AAVEN
     C  N21              GOTO      EVREC
     C   21              MOVE      TABDT         MMVEN
     C   21              MOVEL     TABDT         DDVEN
      *
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
     C                   END                                                    <--------------------
      *
     C                   Z-ADD     WDTVEN        LDTFIM
      *
     C     EVREC         ENDSR
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
     O********************************************************************
     O* OUTPUT - APÓLICES                                                *
     O********************************************************************
     OLFAPOLT   EADD         REGAP
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
      *
     O                       LACTAT             144
     O                       LTPACP             145
      *
     O                       LTPAUT             147
     O                       LCATV              151
     O                       LMATR              163
     O                       LMARCA             183
     O                       LDTMAT             191
      *
     O                       LFUNC              192
     O                       LACTIN             197
     O                       LTPCON             198
     O                       LAMBIN             199
      *
     O                       LAMBMH             200
      *
     O                       LAMBMC             201
     O                       LACTMC             205
     O                       LPROF              209
      *
     O                       LTPOCU             315
     O                       LMODUL             317
      *
     O                       LCOSEG             318
     O                       LALTER             326
     O                       LTPACT             327
     O                       LMATR2             339
     O                       LMARC2             359
      *
     O                       LRRAP              368
     O                       LUFICH             376
     O                       LDTTRF             384
      *
     OLFCOBET   EADD         REGCCL
     O                       LRAMO                2
     O                       LMOD                 4
     O                       LSMOD                6
     O                       LAPOL               13
     O                       LCOBER              16
     O                       LCAPT               29
     O                       LPRSI               40
      *
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
      *
     O                       LALTER             359
     O                       LRRAP              368
     O                       LUFICH             376
     O                       LDTTRF             384
      *
**  TABELA DE CONVERSAO DE RAMOS  (LA VERSUS LV - RAMO/MODAL/SUMODAL)
67070002
68070001
69070002
70070003
**  TABELA DE CONVERSAO DA NATUREZA DE SEGUROS
15     TEMPORÁRIO (PERIODO CURTO)
24     FLUTUANTES
32     ANO E SEGUINTES + ANO E SEGUINTES C/INDEXAÇÃO
43     APOLICE ABERTA
**  TABELA DE CONVERSAO DE COD. ANULAÇÃO
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
** DIA LIMITE DO SEGURO PARA DATA TERMO
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
** MESES/DIAS
013102280331043005310630073108310930103111301231
