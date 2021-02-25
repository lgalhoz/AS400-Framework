     H DFTNAME(TRSF06) DATEDIT(*DMY/)
     H********************************************************************
     H*  EMPRESA    :  GRUPO MATA.                                       *
     H*  PROGRAMA   :  TRSF06                                            *
     H*  TIPO       :  SRT                                               *
     H*  APLICAÇÃO  :  PRODUÇAO GUARDIAN (EXPLORAÇAO TODOS OS RAMOS)     *
     H*  FUNÇÃO        - CRIAÇÃO DE FICHEIRO PARA TRANSFERÊNCIA DE       *
     H*                  INFORMAÇÃO (DADOS) PARA A LUSITANIA "VERDE"     *
     H*                                                                  *
     H*                  * FICHEIRO DE APÓLICES E COBERTURAS *           *
     H*                        SÓ RAMOS DE TRANSPORTES                   *
     H*                                                                  *
     H*  AUTOR      :  PAULO CORREIA                                     *
     H*  DATA       :  01/04/2000                                        *
     H*==================================================================*
     H*                    REGISTO DE MANUTENÇÃO                         *
     H*                                                                  *
     H*   DATA   ABREV.  DESCRIÇÃO DA ALTERAÇÃO                          *
     H* -------- ------  ----------------------                          *
     H*                                                                  *
     H*                                                                  *
     H********************************************************************
     FGREGA     IP   F  448        DISK
     FGREGAPOL  IF   F  100     9AIDISK    KEYLOC(3)
     FLFAPOL    UF A F  384    17AIDISK    KEYLOC(360) EXTFILE('LFAPOLT')
     FEFAPOL    O  A F  384    17AIDISK    KEYLOC(360) EXTFILE('LFAPOL')
     FLFCOBERT  O  A F  384    17AIDISK    KEYLOC(360) EXTFILE('LFCOBET')
     FEFCOBERT  O  A F  384    17AIDISK    KEYLOC(360) EXTFILE('LFCOBE')
     F********************************************************************
     F*              RESUMO DA UTILIZAÇÃO DOS INDICADORES                *
     F********************************************************************
     F*                                                                  *
     F*  01 -  IDENTIFICA REGISTO DE TRANSPORTES                         *
     F*  02 -  IDENTIFICA REGISTO DE APOLICES LUSITANIA (ENVIADAS)       *
     F*  04 -  IDENTIFICA REGISTO DE "REGISTO" DE APÒLICES(ENVIADAS)     *
     F*                                                                  *
     F*                                                                  *
     F********************************************************************
     D RMG             S              2  0 DIM(23) CTDATA PERRCD(1)             RAMO/MOD/SMDO GAL VS LUSITANIA
     D RML             S              6  0 DIM(23) ALT(RMG)
     D NTG             S              1  0 DIM(3) CTDATA PERRCD(1)              NATUREZA SEG. GAL VS LUSITANIA
     D NTL             S              1  0 DIM(3) ALT(NTG)
     D FRG             S              1  0 DIM(5) CTDATA PERRCD(1)              FRACIONAMENTO GAL VS LUSITANIA
     D FRL             S              1  0 DIM(5) ALT(FRG)
     D CUG             S              3  0 DIM(7) CTDATA PERRCD(1)              COD.ANULAÇÃO  GAL VS LUSITANIA
     D CUL             S              2  0 DIM(7) ALT(CUG)
     D TABMM           S              2  0 DIM(12) CTDATA PERRCD(1) ASCEND      DIA/MES VENCIMENTO
     D TABDT           S              4  0 DIM(12) ALT(TABMM)
     D W1G             S             14    DIM(11) CTDATA PERRCD(1) ASCEND      MARCAS INVÁLIDAS
     D W1L             S             14    DIM(11) ALT(W1G)
     D W2G             S             11    DIM(11) CTDATA PERRCD(1) ASCEND      MATRICULAS INVÀLIDAS
     D W2L             S             11    DIM(11) ALT(W2G)
     D CVG             S              3  0 DIM(20) CTDATA PERRCD(1) ASCEND      MATRICULAS INVÀLIDAS
     D CVL             S              4  0 DIM(20) ALT(CVG)
     D AIG             S              2  0 DIM(3) CTDATA PERRCD(1) ASCEND       AMBITO DE INCENDIO
     D AIL             S              1  0 DIM(3) ALT(AIG)
     D RIG             S              3  0 DIM(84) CTDATA PERRCD(1) ASCEND      RISCOS VS COBERTURAS
     D RIL             S              3  0 DIM(84) ALT(RIG)
     D C01G            S              4  0 DIM(60) CTDATA PERRCD(1) ASCEND      CLAUSULAS VS COBERTURAS RM=01
     D C01L            S              3  0 DIM(60) ALT(C01G)
     D C06G            S              4  0 DIM(42) CTDATA PERRCD(1) ASCEND      CLAUSULAS VS COBERTURAS RM=06
     D C06L            S              3  0 DIM(42) ALT(C06G)
     D C05G            S              4  0 DIM(30) CTDATA PERRCD(1) ASCEND      CLAUSULAS VS COBERTURAS RM=05
     D C05L            S              3  0 DIM(30) ALT(C05G)
     D C17G            S              4  0 DIM(24) CTDATA PERRCD(1) ASCEND      CLAUSULAS VS COBERTURAS RM=17
     D C17L            S              3  0 DIM(24) ALT(C17G)
     D C19G            S              4  0 DIM(6) CTDATA PERRCD(1) ASCEND       CLAUSULAS VS COBERTURAS RM=19
     D C19L            S              3  0 DIM(6) ALT(C19G)
     D C55G            S              4  0 DIM(36) CTDATA PERRCD(1) ASCEND      CLAUSULAS VS COBERTURAS RM=55
     D C55L            S              3  0 DIM(36) ALT(C55G)
     D TMD             S              2  0 DIM(12) CTDATA PERRCD(1) ASCEND      DIA/MES DATA TERMO
     D TDM             S              2  0 DIM(12) ALT(TMD)
     D TABM            S              2  0 DIM(12) CTDATA PERRCD(12) ASCEND     MESES/DIAS       GS0100
     D TABD            S              2  0 DIM(12) ALT(TABM)
     D*                                      99 100 FILLER                DISPONÍVEL
     D********************************************************************
     D* ESTRUTURA DE DADOS - LOCAL DATA AREAS                            *
     D********************************************************************
     D                UDS
     D  UAAMOV                 1      2  0
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
     D  AAINI                  1      2  0
     D  MMINI                  3      4  0
     D  DDINI                  5      6  0
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
     D  MMMAAR                 3      4  0
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
     D                 DS                                                       GS0100
     D  DATA$                  1      8  0                                      GS0100
     D  AA$                    1      4  0                                      GS0100
     D  MM$                    5      6  0                                      GS0100
     D  DD$                    7      8  0                                      GS0100
     I*
     I********************************************************************
     I* FICHEIRO HISTÓRICO DE RECIBOS - GUARDIAN                         *
     I* CHAVE = GHNREC                                                   *
     I********************************************************************
     IGREGA     NS  01
     I                                  1    9 0GHNREC                          NUMERO RECIBO  (AAMM99999)
     I                                  1    2 0GHAREC
     I                                  3    4 0GHMREC
     I                                  5    9 0GHSREC
     I*                                                                      AA = ANO MOVIMENTO
     I*                                                                      MM = MES MOVIMENTO
     I*                                                                   99999 = NÚMERO SEQUENCIAL
     I                                 10   18 0GHRRAP                          RAMO/APOLICE
     I                                 10   11 0GHRR                            RAMO
     I                                 12   18 0GHAPOL                          APÓLICE
     I                                 19   58  GHSEG                           NOME DO SEGURADO
     I                                 59   98  GHMOR                           MORADA DO SEGURADO
     I*                                      99 101 GHFILL                C.POSTAL MORADA
     I                                102  121  GHCPM2                          LOCALIDADE
     I                                122  161  GHLCB                           LOCAL DE COBRANÇA
     I*                                     162 164 GHFILL                C.POSTAL LOCAL DE COBRANÇA
     I                                165  181  GHCPC2                          LOCALIDADE (LOC. COBRANÇA)
     I                             P  182  184 0GHNCL                           NÚMERO CLIENTE (SEGURADO)
     I                                185  185 0GHTREG                          TIPO RECIBO:
     I*                                                                     0 = NOVO
     I*                                                                     1 = ADICIONAL
     I*                                                                     2 = CONTINUADO
     I*                                                                     3 = ANULAÇÃO DE ESTORNO
     I*                                                                     4 = ANULADO
     I*                                                                     5 = ESTORNO
     I                                186  191 0GHDTI                           DT INICIO RECIBO (DDMMAA)
     I                                186  187 0DDINI                           DIA INICIO RECIBO
     I                                188  189 0MMINI                           MES INICIO RECIBO
     I                                190  191 0AAINI                           ANO INICIO RECIBO
     I                                192  192 0GHFRAC                          FRACCIONAMENTO
     I                                193  198 0GHDTF                           DATA FIM RECIBO  (DDMMAA)
     I                                193  194 0DDFIM                           DIA  FIM RECIBO
     I                                195  196 0MMFIM                           MES  FIM RECIBO
     I                                197  198 0AAFIM                           ANO  FIM RECIBO
     I                             P  199  204 0GHCTDP                          CAPITAL DANOS PRÓPRIOS OU
     I*                                                                   CAPITAL TOT.SEGURO (VALOR)
     I                             P  205  207 0GHCTRC                          CAPITAL R.CIVIL    (VALOR)
     I                             P  208  212 2GHPRS                           PRÉMIO SIMPLES     (VALOR)
     I                             P  213  216 2GHBONU                          BÓNUS              (VALOR)
     I                             P  217  220 2GHENC7                          ENCARGOS           (VALOR)
     I                             P  221  222 2GHCSTA                          CUSTO DA APÓLICE   (VALOR)
     I                             P  223  224 2GHCRC                           C.R.C.             (VALOR)
     I* DESCONTINUADO                     P 225 2260GHCV                  CUSTO C.I.S.A      (VALOR)
     I                                225  225  GHPJUR                          FLAG P.JURIDICA
     I                                226  226  GHCR1                           CÒDIGO RISCO (SE CR1=2 QIV)
     I                             P  227  230 2GHSELO                          SELOS              (VALOR)
     I                             P  231  234 2GHSNA                           SNA  (I.N.E.M.)    (VALOR)
     I                             P  235  238 2GHSNB                           S.N.B.             (VALOR)
     I                             P  239  243 2GHTREC                          TOTAL DO RECIBO    (VALOR)
     I                                244  244 0GHISEN                          CODIGO DE ISENÇÃO:
     I*                                                                     0 = SEM ISENÇÃO
     I*                                                                     1 = ISENTO DE ENCARGOS
     I*                                                                     2 = ISENTO DE SELOS
     I*                                                                     3 = ISENTO DE C.APÓLICE
     I*                                                                     4 = ISENTO DE C.I.S.A
     I                                245  245 0GHCEMI                          CÓDIGO DE EMISSÃO
     I*                                                                     1 = UMA  CÓPIA
     I*                                                                     2 = DUAS CÓPIAS
     I*                                                                     3 = TRÊS COPIAS
     I                             P  246  247 0GHCONC                          CONCELHO
     I                                248  250 0GHMEDC                          MEDIADOR COM COBRANÇA
     I                             P  251  255 2GHCOM1                          COMISSÃO MED. C/COBRANÇA
     I                             P  256  258 0GHMED2                          MEDIADOR SEM COBRAMNÇA
     I                             P  259  263 2GHCOM2                          COMISSÃO MED. S/COBRANÇA
     I                                264  277  GHMRC1                          MARCA 1º VEÍCULO
     I                                278  288  GHMAT1                          MATRICULA 1º VEÍCULO
     I                                289  302  GHMRC2                          MARCA 2º VEICULO
     I                                303  313  GHMAT2                          MATRICULA 2º VEÍCULO
     I                                314  314  GHLH1                           RISCO 1
     I                                315  359  GHRISC                          OBJECTO OU PESSOA SEGURA
     I*                                                                   OU LOCAL DO RISCO
     I                             P  360  364 2GHPRRC                          PRÉMIO R.CIVIL
     I                             P  365  369 2GHPRDP                          PRÉMIO D.PRÓPRIOS
     I                                370  370 0GHCATV                          CATEGORIA DO VEÍCULO
     I                                371  371  GHVIAG                          ASSISTENCIA VIAGEM
     I                                372  372  GHFIX                           SE     GHFIX = '*'
     I*                                                                   ENTÃO  REG.TRNSFERIDO
     I*                                                                          PARA COBRANCAS
     I                             P  373  376 2GHVFRC                          FRACCIONAMENTO     (VALOR)
     I                             P  377  380 2GHFGA                           FGA/FAT            (VALOR)
     I                             P  381  386 0GHCAPF                          CAPITAL PARA FAT
     I*
     I                             P  399  402 0GHCPM1                          C.POSTAL MORADA
     I                             P  403  406 0GHCPC1                          C.POSTAL LOCAL DE COBRANÇA
     I*
     I                                426  430 0GHNSEQ                          N. SEQUENCIAL
     I                                431  432 0GHAAMV                          ANO MOVIMENTO
     I                                433  434 0GHMMMV                          MÊS MOVIMENTO
     I                                435  440 0GHDALT                          DATA ALTERAÇÃO
     I                                435  436 0GHDDA
     I                                437  438 0GHMMA
     I                                439  440 0GHAAA
     I                                441  448  GHUSR                           UTILIZADOR
     I*
     I********************************************************************
     I* FICHEIRO DE APOLICES CONVERTIDAS - LUSITANIA                     *
     I********************************************************************
     ILFAPOL    NS  02
     I*
     I********************************************************************
     I* FICHEIRO DE REGISTO DE APÒLICES  - GUARDIAN                      *
     I********************************************************************
     I*
     IGREGAPOL  NS  04
     I                                  1    1  RACTIV                          ACTIVIDADE REGISTO
     I*                                                                    ' '= activo
     I*                                                                     E = eliminado
     I                                  2    2 0RSIT                            SITUAÇÃO REG. APOL.
     I*                                                                     1 = alocada
     I*                                                                     2 = processada
     I*                                                                     3 = anulada
     I                                  3   11  RA
     I                                  3    4 0RRR                             RAMO
     I                                  5   11 0RAPOL                           APOLICE
     I                                 12   12 0RTIPO                           TIPO
     I*                                                                     1 = 1 ano/seguintes
     I*                                                                     2 = temporária
     I*                                                                     3 = aberta/flutuante
     I                                 13   18 0RDTINI                          DATA INIC.SEGURO (AMD)
     I                                 19   24 0RDTFIM                          DATA FIM CONTRACTO (AMD)
     I                                 25   29 0RNSEG                           N. SEGURADO
     I                                 30   69  RSEG                            NOME SEGURADO
     I                                 70   80  RMATR1                          MATRICULA
     I                                 81   86 0RDTREG                          DATA REGISTO  (AMD)
     I                                 87   92 0RDTANU                          DATA ANULAÇÃO (AMD)
     I                                 87   88 0RAAANU                             ANO
     I                                 89   90 0RMMANU                             MES (ANULAÇÃO)
     I                                 91   92 0RDDANU                             DIA
     I                                 93   98 0RDTPRO                          DATA PROCESS. (AMD)
     I                                 93   94 0RAAPRO                             ANO
     I                                 95   96 0RMMPRO                             MES (ANULAÇÃO9
     I                                 97   98 0RDDPRO                             DIA
     I*
     F********************************************************************
     F* DETERMINA DATA DE TRANSFERÊNCIA                                  *
     F********************************************************************
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
     C                   MOVE      *BLANKS       LUFICH                                     I
     C*                                                                I
     C                   END                                                    <-----------+
     C********************************************************************
     C* CONTROLA O INPUT PARA SELECCIONAR APENAS OS REGISTOS DOS RAMOS   *
     C*                                                                  *
     C*  10: VIAGENS  67: BAGAGEM  68: MARÍTIMO  69: POSTAL  70: AÉREO   *
     C*
     C********************************************************************
     C*
     C     GHRR          IFNE      10
     C     GHRR          IFNE      67
     C     GHRR          IFNE      68
     C     GHRR          IFNE      69
     C     GHRR          IFNE      70
     C                   GOTO      FIM
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
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
     C                   MOVE      *BLANKS       LACTAT            6            * ACTIVIDADE (ACI.TRABALHO)
     C                   MOVE      *BLANKS       LTPACT            1            * TIPO SEGURADO (FIXO/VARIAVEL)
     C*
     C* ACIDENTES PESSOAIS
     C                   MOVE      *BLANKS       LTPACP            1            * TIPO DE SEGURO (ACP)
     C*
     C* SEGURO AUTOMÓVEL
     C*
     C                   MOVE      *BLANKS       LTPAUT            2            * TIPO DE SEGURO  (AUT)
     C                   MOVE      *BLANKS       LCATV             4            * CATEGORIA DO VEÍCULO
     C                   MOVE      *BLANKS       LMATR            12            * MATRÍCULA
     C                   MOVE      *BLANKS       LMARCA           20            * MARCA
     C                   MOVE      *ZEROS        LDTMAT            8 0          * DATA MATRÍCULA
     C*
     C* SEGURO INCÊNDIO
     C*
     C                   MOVE      *BLANKS       LFUNC             1            * FUNCAO
     C                   MOVE      *BLANKS       LACTIN            5            * ACTIVIDADE  (INCEND.)
     C                   MOVE      *BLANKS       LTPCON            1            * TIPO CONSTRUÇAO
     C                   MOVE      *BLANKS       LAMBIN            1            * AMBITO (INCÊNDIO)
     C*
     C* SEGURO MULTI-RISCOS HABITACAO
     C*
     C                   MOVE      *BLANKS       LAMBMH            1            * AMBITO (M.RISCOS HABIT.)
     C*
     C* SEGURO MULTI-RISCOS COMERCIAL
     C*
     C                   MOVE      *BLANKS       LAMBMC            1            * AMBITO
     C                   MOVE      *BLANKS       LACTMC            4            * ACTIVIDADE
     C*
     C* LIMPEZA DE CAMPOS ADICIONAIS GIL
     C*
     C                   MOVE      *ZEROS        LALTER            8 0          * DATA ALTERACAO (PROC)
     C                   MOVE      *BLANKS       LMATR2           12            * MATRÍCULA 2º VEÍCULO
     C                   MOVE      *BLANKS       LMARC2           20            * MARCA     2º VEÍCULO
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
     C                   Z-ADD     *ZEROS        LCAPPI           13 2          * CAPITAL PERDAS IND.
     C                   Z-ADD     *ZEROS        WPI               5 3          * CAPITAL PERDAS IND.
     C                   MOVE      *BLANKS       LCOSEG            1
     C                   Z-ADD     *ZEROS        LPRS1C           11 2
     C                   Z-ADD     *ZEROS        LPRS2C           11 2
     C                   Z-ADD     *ZEROS        WPRSC            11 2
     C*
     F********************************************************************
     F* CONVERSAO DE CAMPOS - REGRAS                                     *
     F********************************************************************
     C*
     C* CONVERTE RAMOS  ( RAMO GUARDIAN VS LUSITANIA ).
     C*
     C                   Z-ADD     1             I                 2 0
     C     GHRR          LOOKUP    RMG(I)                                 20
     C   20              MOVE      RML(I)        LURMS
     C  N20              MOVE      *BLANKS       LURMS
     C*
     C                   MOVE      LURR          LRAMO
     C                   MOVE      LUMOD         LMOD
     C                   MOVE      LUSMOD        LSMOD
     C*
     C                   MOVE      GHAPOL        LAPOL
     C*
     C* COMPÕE CÓDIGO PESSOA
     C*
     C                   MOVEL     9             LNSEG
     C                   MOVE      GHNCL         LNSEG
     C*
     C* ACEDE A REGISTO DE APÓLICES (LIVRO)
     C*
     C     GHRRAP        CHAIN     GREGAPOL                           90
     C   90              MOVE      2             DUR
     C  N90              DO
     C                   MOVE      RTIPO         DUR
     C     GHDTI         IFEQ      0
     C                   MOVE      RDTINI        GHDTI
     C                   END
     C     GHDTF         IFEQ      0
     C                   MOVE      RDTFIM        GHDTF
     C                   END
     C                   END
     C*
     C* CONVERTE NATUREZA DOS SEGUROS  (FORÇA DURAÇÃO SE É INVÁLIDO)
     C*
     C     RTIPO         IFNE      1
     C     RTIPO         IFNE      2
     C     RTIPO         IFNE      3
     C                   MOVE      2             DUR               1 0
     C                   END
     C                   END
     C                   END
     C*
     C     GHRR          IFEQ      10
     C                   MOVE      2             DUR
     C                   END
     C*
     C                   Z-ADD     1             I                 2 0
     C     DUR           LOOKUP    NTG(I)                                 20
     C   20              MOVE      NTL(I)        LNATS
     C  N20              MOVE      *BLANKS       LNATS
     C*
     C* FORÇA A NATUREZA DO SEGURO SE APOLICE TEM INDEXAÇÃO (O.RAMOS)
     C*
     C*  14      DUR       IFEQ 1
     C*          INDEX     IFEQ 'S'
     C*                    MOVE '2'       LNATS
     C*                    END
     C*                    END
     C*
     C* FORMATA VENCIMNENTO (DIA E MES VENCIMENTO - FORÇA CAMPOS)
     C*
     C     GHMREC        IFEQ      *ZEROS
     C                   MOVE      GHMMMV        MES1              2 0
     C                   ELSE
     C                   MOVE      GHMREC        MES1
     C                   END
     C*
     C     MES1          IFEQ      02
     C                   MOVEL     MES1          LMMDDV
     C                   MOVE      28            LMMDDV
     C                   ELSE
     C                   MOVEL     MES1          LMMDDV
     C                   MOVE      30            LMMDDV
     C                   END
     C*
     C* FORMATA VENCIMENTO (FORÇA CAMPOS-DT.VENCIMENTO COMPLETA)
     C*
     C                   EXSR      SRDTVC
     C*
     C* FORMATA FRACCIONAMENTO (FORÇA TEMPORARIO NO FRACCIONAMENTO)
     C*
     C                   Z-ADD     1             I
     C     GHFRAC        LOOKUP    FRG(I)                                 20
     C   20              MOVE      FRL(I)        LFRAC
     C  N20              MOVE      '9'           LFRAC
     C*
     C* FORMATA COBRADOR ( ESTE CAMPO VAI A BRANCOS )
     C*  - O COBRADOR SERÁ O NUMERO DE AGENTE JOSÈ MATA NA LUSITANIA
     C*  - ESTE CAMPO SERÁ PREENCHIDO PELA PROPRIA LUSITANIA
     C*
     C                   MOVE      *BLANKS       LCOBR
     C*
     C* FORMATA DATA DA ULTIMA ACTA.  (VERIFICAR REGAPOL)
     C*
     C                   Z-ADD     *ZEROS        ACTA              6 0
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
     C* FORMATA CODIGO DE ANULAÇÃO (REVER ACESSO RGAPOL)
     C*
     C     RSIT          IFEQ      3
     C                   MOVEL     RSIT          WSIT              3 0
     C                   MOVE      '00'          WSIT
     C                   ELSE
     C     RSIT          IFEQ      2
     C                   MOVEL     '1'           WSIT              3 0
     C                   MOVE      '00'          WSIT
     C                   ELSE
     C                   MOVEL     '1'           WSIT              3 0
     C                   MOVE      '00'          WSIT
     C                   END
     C                   END
     C                   Z-ADD     1             I
     C     WSIT          LOOKUP    CUG(I)                                 20
     C   20              MOVE      CUL(I)        LANUL
     C  N20              DO
     C     RSIT          IFEQ      3
     C                   MOVE      '09'          LANUL
     C                   ELSE
     C                   MOVE      '00'          LANUL
     C                   END
     C                   END
     C*
     C* FORMATA DATA DE ANULAÇAO (FORÇA DATA DO REGISTO (LIVRO)
     C*
     C     RSIT          IFEQ      3
     C                   Z-ADD     *ZEROS        DTY2
     C*
     C* FORÇA CAMPO DE DATA DE ALTERAÇÃO (C/DATA DE ANULAÇÃO)
     C                   MOVE      RAAANU        AAALT
     C                   MOVE      RMMANU        MMALT
     C                   MOVE      RDDANU        DDALT
     C*
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
     C*    1º FORÇA DATA DE INICIO DO CONTRATO
     C*       (ESTE CAMPO PARA OS TRANSPORTES ENCONTRA-SE A ZEROS)
     C*
     C     GHDTI         IFEQ      *ZEROS
     C                   MOVE      RAAPRO        AAINI
     C                   MOVE      RMMPRO        MMINI
     C                   MOVE      RDDPRO        DDINI
     C                   END
     C*
     C**         AAINI     IFGT 05
     C**                   MOVE *ZEROS    AAINI
     C**                   ELSE
     C**                   END
     C*
     C     WDTINI        IFEQ      *ZEROS
     C* ANO
     C                   MOVE      GHAREC        AAINI             2 0
     C* MES
     C     GHMREC        IFEQ      *ZEROS
     C                   MOVE      GHMMMV        MMINI             2 0
     C                   ELSE
     C                   MOVE      GHMREC        MMINI
     C                   END
     C* DIA
     C                   MOVE      01            DDINI             2 0
     C                   END
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
     C* FORMATA DATA DE TERMO DO CONTRATO SE APOLICE NÃO ABERTA
     C*    1º FORÇA DATA DE TERMO DO CONTRATO
     C*       (ESTE CAMPO PARA OS TRANSPORTES ENCONTRA-SE A ZEROS)
     C*
     C     GHDTF         IFEQ      *ZEROS                                       <-------------+
     C     LNATS         IFNE      '3'                                          <--------+    I
     C* ANO                                                         I    I
     C                   MOVE      GHAREC        AAFIM             2 0                   I    I
     C* MES                                                         I    I
     C     GHMREC        IFEQ      *ZEROS                                                I    I
     C                   MOVE      GHMMMV        MMFIM             2 0                   I    I
     C                   ELSE                                                            I    I
     C                   MOVE      GHMREC        MMFIM                                   I    I
     C                   END                                                             I    I
     C* DIA                                                         I    I
     C                   Z-ADD     1             I                 2 0                   I    I
     C     MMFIM         LOOKUP    TMD(I)                                 21             I    I
     C  N21              Z-ADD     *ZEROS        DDFIM                                   I    I
     C  N21              Z-ADD     *ZEROS        MMFIM                                   I    I
     C  N21              Z-ADD     *ZEROS        AAFIM                                   I    I
     C   21              MOVEL     TDM(I)        DDFIM             2 0                   I    I
     C*                                                             I    I
     C                   END                                                    <--------+    I
     C                   END                                                    <-------------+
     C*
     C     LNATS         IFNE      '3'                                          <-------------+
     C                   Z-ADD     *ZEROS        DTY2                                         I
     C                   MOVE      WDTFIM        DTY2                                         I
     C                   EXSR      SRDTY2                                                     I
     C                   MOVE      DTY2          LDTFIM                                       I
     C     LDTFIM        IFGT      *ZEROS                                                     I
     C                   MOVE      *ZEROS        DATA$                                        I
     C                   MOVE      LDTFIM        DATA$                                        I
     C                   EXSR      VDATA                                                      I
     C                   MOVE      DATA$         LDTFIM                                       I
     C                   END                                                                  I
     C                   END                                                    <-------------+
     C*
     C     LDTFIM        IFEQ      *ZEROS
     C     LNATS         IFNE      '3'
     C                   MOVE      WDTVEN        LDTFIM
     C     LDTFIM        IFGT      *ZEROS
     C                   MOVE      *ZEROS        DATA$
     C                   MOVE      LDTFIM        DATA$
     C                   EXSR      VDATA
     C                   MOVE      DATA$         LDTFIM
     C                   END
     C                   END
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
     C     RSIT          IFEQ      3
     C     LDTINI        IFGT      LDTANU
     C                   MOVE      DDINI         DDALT
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
     C*
     C* FORMATA DATA DE ALTERAÇAO EFECTIVA (CARREGAMENTO/PROCESSAMENTO)
     C*
     C                   Z-ADD     *ZEROS        DTY2
     C                   Z-ADD     GHDDA         DALTER
     C                   Z-ADD     GHMMA         MALTER
     C                   Z-ADD     GHAAA         AALTER
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
     C                   MOVE      GHCONC        WCONC
     C                   MOVE      WCONC         LCONC
     C                   MOVEL     GHRISC        LLRPS
     C*
     C* SE ACIDENTES TRABALHO - DETERMINA ACTIVIDADE
     C*
     C*          GHRR      IFEQ 15
     C*                    MOVE CDCAE     LACTAT
     C*                    END
     C*
     C* SE GRUPO DE RAMOS AC.PESSOAIS - DETERMINA TIPO DE SEGURO
     C*
     C     LMOD          IFEQ      '02'
     C*          GHRR      IFEQ 51
     C*                    MOVE 5         LTPACP
     C*                    END
     C     GHRR          IFEQ      10
     C                   MOVE      1             LTPACP
     C                   END
     C*          GHRR      IFEQ 15
     C*          TPT       IFEQ 'O'
     C*          MOD       IFEQ 'F'
     C*                    MOVE '3'       LTPACP
     C*                    ELSE
     C*                    MOVE '1'       LTPACP
     C*                    END
     C*                    ELSE
     C*                    MOVE '1'       LTPACP
     C*                    END
     C*                    END
     C                   END
     C*
     C* SE AUTOMÓVEL  - DETERMINA CATEGORIA (COMPOE CATV GAL)
     C*
     C*  13                DO
     C*
     C*                    MOVELCATV      W2      20
     C*                    MOVE CL        W2
     C*                    MOVE PB        W3      30
     C*                    MOVELW2        W3
     C*
     C* TIPO DE SEGURO AUTOMOVEL ( DEFAULT = 10-VEICULO PARTICULAR)
     C*
     C*                    MOVE '10'      LTPAUT
     C*
     C*          W3        IFEQ 501
     C*                    MOVE '14'      LTPAUT
     C*                    ELSE
     C*          W3        IFEQ 502
     C*                    MOVE '14'      LTPAUT
     C*                    ELSE
     C*          W3        IFEQ 503
     C*                    END
     C*                    END
     C*                    END
     C*
     C*          CL1       IFEQ 'I'
     C*                    MOVE '30'      LTPAUT
     C*                    ELSE
     C*          CL2       IFEQ 'I'
     C*                    MOVE '30'      LTPAUT
     C*                    ELSE
     C*          CL3       IFEQ 'I'
     C*                    MOVE '30'      LTPAUT
     C*                    ELSE
     C*          CL4       IFEQ 'I'
     C*                    MOVE '30'      LTPAUT
     C*                    ELSE
     C*          CL5       IFEQ 'I'
     C*                    MOVE '30'      LTPAUT
     C*                    ELSE
     C*          CL6       IFEQ 'I'
     C*                    MOVE '30'      LTPAUT
     C*                    END
     C*                    END
     C*                    END
     C*                    END
     C*                    END
     C*                    END
     C*
     C* PESQUISA COMBINAÇÃO NA MARCA
     C*
     C*                    Z-ADD1         I
     C*          MARC1     LOKUPW1G,I                    20
     C*  20                MOVE '30'      LTPAUT
     C*                    MOVE *BLANKS   MARC1
     C*                    MOVE *BLANKS   MATR1
     C*
     C* PESQUISA COMBINAÇÃO NA MATRICULA
     C*
     C*                    Z-ADD1         I
     C*          MATR1     LOKUPW2G,I                    20
     C*  20                MOVE '30'      LTPAUT
     C*                    MOVE *BLANKS   MARC1
     C*                    MOVE *BLANKS   MATR1
     C*
     C* PESQUISA CATEGORIA DE VEÌCULO
     C*
     C*                    Z-ADD1         I
     C*          W3        LOKUPCVG,I                    20
     C*  20                MOVE CVL,I     LCATV
     C* N20                MOVE *BLANKS   LCATV
     C*
     C*                    MOVELMATR1     LMATR
     C*                    MOVELMARC1     LMARCA
     C*
     C* FORMATA DATA DA MATRICULA
     C*
     C*                    Z-ADD*ZEROS    DTY2
     C*                    MOVE WDTMAT    DTY2
     C*                    EXSR SRDTY2
     C*                    MOVE DTY2      LDTMAT
     C*
     C*                    END
     C*
     C* FORMATA FUNÇÃO DE INCÊNDIO
     C*
     C*  14      RR        IFEQ 01
     C*          CR1       IFEQ 3
     C*                    MOVE '2'       LFUNC
     C*                    ELSE
     C*          CR1       IFEQ 4
     C*                    MOVE '4'       LFUNC
     C*                    ELSE
     C*          NSEG      IFGT 9999
     C*                    MOVE '1'       LFUNC
     C*                    ELSE
     C*                    MOVE '2'       LFUNC
     C*                    END
     C*                    END
     C*                    END
     C*                    END
     C*
     C*  14      RR        IFEQ 05
     C*                    MOVE '1'       LFUNC
     C*                    END
     C*
     C*  14      RR        IFEQ 55
     C*                    MOVE '1'       LFUNC
     C*                    END
     C*
     C* FORMATA ACTIVIDADE
     C*
     C*                    MOVE *BLANKS   LACTIN
     C*
     C* FORMATA TIPO DE CONSTRUÇÃO
     C*
     C*                    MOVE *BLANKS   LTPCON
     C*
     C* FORMATA AMBITO
     C*
     C*  14      RR        IFEQ 01
     C*                    EXSR SRAMB
     C*                    END
     C*
     C*  14      RR        IFEQ 05
     C*                    EXSR SRAMB
     C*                    END
     C*
     C*  14      RR        IFEQ 55
     C*                    EXSR SRAMB
     C*                    END
     C*
     C* FORMATA ACTIVIDADE COMERCIANTE
     C*
     C*                    MOVE *BLANKS   LAMBMH
     C*
     C* FORMATA INFORMAÇÃO DE COSEGURO
     C*
     C                   MOVE      '0'           LCOSEG
     C*
     C* FORMATA COBERTURAS / CAPITAIS / PREMIOS
     C*
     C* FORMATA CAMPOS DE CONTROL
     C*
     C                   MOVEL     GHRR          LRRAP
     C                   MOVE      GHAPOL        LRRAP
     C*
     C*DETERMINA SE APOLICE JÁ FOI ALGUMA VEZ CRIADA
     C*
     C                   MOVEL     LRRAP         KREGA
     C                   MOVE      LUFICH        KREGA            17
     C     KREGA         CHAIN     LFAPOL                             90
     C  N90              GOTO      FIM
     C*
     C                   EXCEPT    REGAP
     C*
     C                   EXSR      SRISC
     C*
     C     FIM           TAG
     C****************************************************
     C*   FORMATA DATAS COM 8 DIGITOS                     *
     C********************************************    > < =
     C     SRDTY2        BEGSR
     C*
     C     DTY2          IFGT      *ZEROS
     C     DTY2          IFLT      400000
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
     C     MM$           LOOKUP    TABM          TABD                     40                   GS0100
     C*  40                DO
     C*          MM$       IFEQ 02                                        GS0100
     C*          AA$       DIV  4         RES     30                      GS0100
     C*                    MVR            RES                             GS0100
     C*          RES       IFEQ *ZEROS                                    GS0100
     C*                    MOVE 29        TABD                            GS0100
     C*                    END                                            GS0100
     C*                    END                                            GS0100
     C*                                                                   GS0100
     C     DD$           IFGT      TABD                                                        GS0100
     C                   MOVE      TABD          DD$                                           GS0100
     C                   END                                                                   GS0100
     C*                    END
     C*
     C                   ENDSR
     C****************************************************
     C*   DETERMINA DATA VENCIMENTO                       *
     C********************************************    > < =
     C     SRDTVC        BEGSR
     C*
     C     MES1          IFEQ      02
     C                   MOVE      29            DDVEN
     C                   MOVE      MES1          MMVEN
     C                   MOVE      GHAREC        AAVEN
     C                   ELSE
     C                   MOVE      30            DDVEN
     C                   MOVE      MES1          MMVEN
     C                   MOVE      GHAREC        AAVEN
     C                   END
     C*
     C                   ENDSR
     C*
     C****************************************************
     C*   DETERMINA AMBITO DE INCENDIO                    *
     C********************************************    > < =
     C*          SRAMB     BEGSR
     C*
     C*                    Z-ADD*ZEROS    WR1
     C*                    Z-ADD*ZEROS    WR2
     C*
     C*          CR1       IFEQ 1
     C*                    MOVE 1         WR1
     C*                    ELSE
     C*          CR2       IFEQ 1
     C*                    MOVE 1         WR1
     C*                    ELSE
     C*  14      CR3       IFEQ 1
     C*                    MOVE 1         WR1
     C*                    END
     C*                    END
     C*                    END
     C*
     C*          CR1       IFEQ 2
     C*                    MOVE 2         WR2
     C*                    ELSE
     C*          CR2       IFEQ 2
     C*                    MOVE 2         WR2
     C*                    ELSE
     C*  14      CR3       IFEQ 2
     C*                    MOVE 2         WR2
     C*                    END
     C*                    END
     C*                    END
     C*
     C*                    Z-ADD1         I
     C*          WR12      LOKUPAIG,I                    20
     C*  20                MOVE AIL,I     LAMBIN
     C* N20                MOVE '1'       LAMBIN
     C*
     C*          ESRAMB    ENDSR
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
     C* FORÇA RISCOS COM VALOR "1" E "0".
     C*
     C                   Z-ADD     1             CR1               1 0
     C                   Z-ADD     *ZEROS        CR2               1 0
     C                   Z-ADD     *ZEROS        CR3               1 0
     C*
     C     CR1           IFGT      0
     C                   MOVE      CR1           WCR1
     C                   END
     C     CR2           IFGT      0
     C                   MOVE      CR2           WCR2
     C                   END
     C     CR3           IFGT      0
     C                   MOVE      CR3           WCR2
     C                   END
     C*
     C     WCR1          IFGT      0
     C                   MOVEL     GHRR          WRCOB
     C                   MOVE      WCR1          WRCOB
     C                   MOVE      WCR1          WCR0
     C                   EXSR      SRCOB
     C                   END
     C*
     C     WCR2          IFGT      0
     C                   MOVEL     GHRR          WRCOB
     C                   MOVE      WCR2          WRCOB
     C                   MOVE      WCR2          WCR0
     C                   EXSR      SRCOB
     C                   END
     C*
     C     WCR3          IFGT      0
     C                   MOVEL     GHRR          WRCOB
     C                   MOVE      WCR3          WRCOB
     C                   MOVE      WCR3          WCR0
     C                   EXSR      SRCOB
     C                   END
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
     C* FORMATA CAPITAIS  E PRS. IMPUTADO  (OUTROS RAMOS)
     C*
     C*  14                DO
     C                   Z-ADD     GHCTDP        LCAPT
     C     GHISEN        IFNE      1
     C     GHPRS         DIV(H)    1.2           LPRSI
     C                   ELSE
     C                   Z-ADD     GHPRS         LPRSI
     C                   END
     C                   Z-ADD     GHPRS         LPRCOM
     C*                    END
     C*
     C* FORMATA CAPITAIS  E PRS. IMPUTADO (AUTO + OCUP)
     C*
     C*  13                DO
     C*                    Z-ADD*ZEROS    $RC     90
     C*                    Z-ADD*ZEROS    $DP     90
     C*          CAPRC     IFEQ 99999
     C*                    MOVE 9999999999LCAPT
     C*                    MOVEL999       LCAPT
     C*                    Z-ADDPRS       LPRSI
     C*                    ELSE
     C*          CAPRC     MULT 1000      $RC
     C*                    Z-ADDCAPDP     $DP
     C*          $RC       ADD  $DP       LCAPT
     C*                    Z-ADDPRS       LPRSI
     C*                    END
     C*                    END
     C*
     C*          RR        IFEQ 50
     C*                    Z-ADD*ZEROS    $RC
     C*                    Z-ADD*ZEROS    $DP
     C*          CAP1      MULT 1000      LCAP1
     C*                    Z-ADDCAP2      LCAP2
     C*                    Z-ADDPR1       LPRS1
     C*                    Z-ADDPR2       LPRS2
     C*                    EXCPTREGCOB
     C*                    EXSR SRCL50
     C*                    GOTO XPTO01
     C*                    END
     C*
     C*          RR        IFEQ 51
     C*                    Z-ADD*ZEROS    $RC
     C*                    Z-ADD*ZEROS    $DP
     C*          CAP1      MULT 1000      LCAP1
     C*                    Z-ADDCAP2      LCAP2
     C*                    Z-ADDPR1       LPRS1
     C*                    Z-ADDPR2       LPRS2
     C*                    EXCPTREGCOB
     C*                    GOTO XPTO01
     C*                    END
     C*
     C                   EXCEPT    REGCOB
     C*
     C     GHRR          IFEQ      10
     C     GHVIAG        ANDEQ     'S'
     C                   MOVE      '014'         LCOBER
     C                   Z-ADD     0             LCAP1
     C                   Z-ADD     0             LCAP2
     C                   Z-ADD     0             LPRS1C
     C                   Z-ADD     0             LPRS2C
     C                   Z-ADD     0             LCAPPI
     C                   Z-ADD     0             WPI
     C                   EXCEPT    REGCOB
     C                   ENDIF
     C     XPTO01        TAG
     C*
     C*          RR        IFEQ 01
     C*          WRCOB     IFEQ 1
     C*                    MOVE '002'     LCOBER
     C*                    EXCPTREGCOB
     C*                    END
     C*          WRCOB     IFEQ 9
     C*                    MOVE '002'     LCOBER
     C*                    EXCPTREGCOB
     C*                    END
     C*                    END
     C*
     C*          RR        IFEQ 06
     C*          WRCOB     IFEQ 1
     C*                    MOVE '002'     LCOBER
     C*                    EXCPTREGCOB
     C*                    END
     C*          WRCOB     IFEQ 9
     C*                    MOVE '002'     LCOBER
     C*                    EXCPTREGCOB
     C*                    END
     C*                    END
     C*
     C*          RR        IFEQ 05
     C*          WRCOB     IFEQ 1
     C*                    MOVE '002'     LCOBER
     C*                    EXCPTREGCOB
     C*                    END
     C*          WRCOB     IFEQ 9
     C*                    MOVE '002'     LCOBER
     C*                    EXCPTREGCOB
     C*                    END
     C*                    END
     C*
     C*          RR        IFEQ 55
     C*          WRCOB     IFEQ 1
     C*                    MOVE '002'     LCOBER
     C*                    EXCPTREGCOB
     C*                    END
     C*          WRCOB     IFEQ 9
     C*                    MOVE '002'     LCOBER
     C*                    EXCPTREGCOB
     C*                    END
     C*                    END
     C*
     C*                    EXSR SRCP12
     C*
     C*          RR        IFEQ 01
     C*                    Z-ADDCAP1      LCAP1
     C*                    Z-ADDPR1       LPRS1
     C*                    Z-ADDCAP2      LCAP2
     C*                    Z-ADDPR2       LPRS2
     C*                    EXCPTREGCCL
     C*                    EXSR SRCL
     C*                    Z-ADD1         I
     C*          WRKCL     LOKUPC01G,I                   20
     C*  20                MOVE C01L,I    LCOBER
     C*  20                EXCPTREGCCL
     C*                    GOTO ESCOB
     C*                    END
     C*
     C*          RR        IFEQ 05
     C*                    Z-ADDCAP1      LCAP1
     C*                    Z-ADDPR1       LPRS1
     C*                    Z-ADDCAP2      LCAP2
     C*                    Z-ADDPR2       LPRS2
     C*                    EXCPTREGCCL
     C*                    EXSR SRCL
     C*                    Z-ADD1         I
     C*          WRKCL     LOKUPC05G,I                   20
     C*  20                MOVE C05L,I    LCOBER
     C*  20                EXCPTREGCCL
     C*                    GOTO ESCOB
     C*                    END
     C*
     C*          RR        IFEQ 06
     C*                    Z-ADDCAP1      LCAP1
     C*                    Z-ADDPR1       LPRS1
     C*                    Z-ADDCAP2      LCAP2
     C*                    Z-ADDPR2       LPRS2
     C*                    EXCPTREGCCL
     C*                    EXSR SRCL
     C*                    Z-ADD1         I
     C*          WRKCL     LOKUPC06G,I                   20
     C*  20                MOVE C06L,I    LCOBER
     C*  20                EXCPTREGCCL
     C*                    GOTO ESCOB
     C*                    END
     C*
     C*          RR        IFEQ 17
     C*                    Z-ADDCAP1      LCAP1
     C*                    Z-ADDPR1       LPRS1
     C*                    Z-ADDCAP2      LCAP2
     C*                    Z-ADDPR2       LPRS2
     C*                    EXCPTREGCOB
     C*                    EXSR SRCL
     C*                    Z-ADD1         I
     C*          WRKCL     LOKUPC17G,I                   20
     C*  20                MOVE C17L,I    LCOBER
     C*  20                EXCPTREGCCL
     C*                    GOTO ESCOB
     C*                    END
     C*
     C*          RR        IFEQ 19
     C*                    Z-ADDCAP1      LCAP1
     C*                    Z-ADDPR1       LPRS1
     C*                    Z-ADDCAP2      LCAP2
     C*                    Z-ADDPR2       LPRS2
     C*                    EXCPTREGCCL
     C*                    EXSR SRCL
     C*                    Z-ADD1         I
     C*          WRKCL     LOKUPC19G,I                   20
     C*  20                MOVE C19L,I    LCOBER
     C*  20                EXCPTREGCCL
     C*                    GOTO ESCOB
     C*                    END
     C*
     C     ESCOB         ENDSR
     C****************************************************
     C*   PESQUISA CLAUSULAS ESPECIAIS - INCENDIO         *
     C********************************************    > < =
     C*          SRCL      BEGSR
     C*
     C*          CL1       IFNE '  '
     C*                    MOVEL'01'      WRKCL   40
     C*                    MOVE CL1       WRKCL   40
     C*                    ELSE
     C*          CL2       IFNE '  '
     C*                    MOVEL'02'      WRKCL   40
     C*                    MOVE CL1       WRKCL   40
     C*                    ELSE
     C*          CL3       IFEQ '  '
     C*                    MOVEL'03'      WRKCL   40
     C*                    MOVE CL1       WRKCL   40
     C*                    ELSE
     C*          CL4       IFEQ '  '
     C*                    MOVEL'04'      WRKCL   40
     C*                    MOVE CL1       WRKCL   40
     C*                    ELSE
     C*          CL5       IFEQ '  '
     C*                    MOVEL'05'      WRKCL   40
     C*                    MOVE CL1       WRKCL   40
     C*                    ELSE
     C*          CL6       IFEQ '  '
     C*                    MOVEL'06'      WRKCL   40
     C*                    MOVE CL1       WRKCL   40
     C*                    END
     C*                    END
     C*                    END
     C*                    END
     C*                    END
     C*                    END
     C*
     C*                    ENDSR
     C****************************************************
     C*   PESQUISA CLAUSULAS ESPECIAIS - INCENDIO         *
     C********************************************    > < =
     C*          SRCP12    BEGSR
     C*
     C*                    Z-ADD*ZEROS    LCAP1
     C*                    Z-ADD*ZEROS    LPRS1
     C*                    Z-ADD*ZEROS    LCAP2
     C*                    Z-ADD*ZEROS    LPRS2
     C*
     C*          LAMBIN    IFEQ '3'
     C*          CR1       IFEQ 1
     C*                    Z-ADDCAP1      LCAP1
     C*                    Z-ADDPR1       LPRS1
     C*                    END
     C*          CR2       IFEQ 1
     C*                    Z-ADDCAP1      LCAP1
     C*                    Z-ADDPR1       LPRS1
     C*                    END
     C*          CR3       IFEQ 1
     C*                    Z-ADDCAP1      LCAP1
     C*                    Z-ADDPR1       LPRS1
     C*                    END
     C*
     C*          CR1       IFEQ 2
     C*                    Z-ADDCAP2      LCAP2
     C*                    Z-ADDPR2       LPRS2
     C*                    END
     C*          CR2       IFEQ 2
     C*                    Z-ADDCAP2      LCAP2
     C*                    Z-ADDPR2       LPRS2
     C*                    END
     C*          CR3       IFEQ 2
     C*                    Z-ADDCAP2      LCAP2
     C*                    Z-ADDPR2       LPRS2
     C*                    END
     C*
     C*                    END
     C*
     C*                    ENDSR
     C*
     C****************************************************
     C*   DETERMINA CLAUSULAS ESPECIAIS - AUTOMOVEL       *
     C********************************************    > < =
     C*          SRCL50    BEGSR
     C*
     C*          WRCOB     IFEQ 1
     C*          CAP1      IFGT 600
     C*                    MOVE '002'     LCOBER
     C*                    EXCPTREGCOB
     C*                    END
     C*                    MOVE '054'     LCOBER
     C*                    EXCPTREGCOB
     C*                    END
     C*
     C*          WRCOB     IFEQ 2
     C*          CAP1      IFGT 600
     C*                    MOVE '002'     LCOBER
     C*                    EXCPTREGCOB
     C*                    END
     C*                    END
     C*
     C*          WRCOB     IFEQ 3
     C*                    MOVE '054'     LCOBER
     C*                    EXCPTREGCOB
     C*                    END
     C*
     C*          WRCOB     IFEQ 4
     C*                    MOVE '004'     LCOBER
     C*                    EXCPTREGCOB
     C*                    MOVE '054'     LCOBER
     C*                    EXCPTREGCOB
     C*                    END
     C*
     C*          WRCOB     IFEQ 8
     C*                    MOVE '053'     LCOBER
     C*                    EXCPTREGCOB
     C*                    MOVE '056'     LCOBER
     C*                    EXCPTREGCOB
     C*                    MOVE '054'     LCOBER
     C*                    EXCPTREGCOB
     C*                    END
     C*
     C*                    ENDSR
     F********************************************************************
     F* OUTPUT - APÒLICES FICHEIRO DE CONTROLE                           *
     F********************************************************************
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
     O*
     F********************************************************************
     F* OUTPUT - APÒLICES FICHEIRO PARA ENVIO                            *
     F********************************************************************
     OEFAPOL    EADD         REGAP
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
     O*
     F********************************************************************
     F* OUTPUT - COBERTURAS FICHEIRO DE CONTROLE                         *
     F********************************************************************
     OLFCOBERT  EADD         REGCOB
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
     O*
     O                       LALTER             359
     O                       LRRAP              368
     O                       LUFICH             376
     O                       LDTTRF             384
     F********************************************************************
     F* OUTPUT - COBERTURAS FICHEIRO PARA ENVIO                          *
     F********************************************************************
     OEFCOBERT  EADD         REGCOB
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
     O*
     O                       LALTER             359
     O                       LRRAP              368
     O                       LUFICH             376
     O                       LDTTRF             384
     O*
     O*       EADD             REGCCL
     O*                        LRAMO      2
     O*                        LMOD       4
     O*                        LSMOD      6
     O*                        LAPOL     13
     O*                        LCOBER    16
     O*                        LCAPT     29
     O*                        LPRSI     40
**  TABELA DE CONVERSAO DE RAMOS  (LA VERSUS LV)
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
17340700
18090304
19340600
50310000
51010301
55340200
65090400
67070002
68070001
69070002
70070003
**  TABELA DE CONVERSAO DA NATUREZA DE SEGUROS
11     ANO E SEGUINTES + ANO E SEGUINTES C/INDEXAÇÃO
25     TEMPORÀRIO (PERIODO CURTO)
33     APOLICE ABERTA
**  TABELA DE CONVERSAO DE FRACCIONAMENTO
11     ANUAL
22     SEMESTRAL
33     TRIMESTRAL
44     MENSAL
59     UNICO
**  TABELA DE CONVERSAO DE COD. ANULAÇÃO
10000  EM VIGOR
20000  EM VIGOR (SUSPENSA NA GAL SEM CORRESPONDENCIA NA LUSITANIA)
30009  P/DECISÃO DA COMPANHIA
36103  PEDIDO DO SEGURADO (EXTINÇÃO DOO RISCO)
36202  FALTA DE PAGAMENTO (ANULADA)
36308  SUBSTITUIÇÃO DA APOLICE
36409  P/DECISÃO DA COMPANHIA
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
** MARCAS INVÀLIDAS (SEGURO GARAGISTA)
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
** MATRICULAS INVÀLIDAS (SEGURO GARAGISTA)
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
** CATEGORIAS DE VEÌCULOS
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
5003450
5011400
5021400
5031400
5041400
6400100
7400100
** AMBITO DE INCENDIO
022
101
123
** TABELA DE ASSOCIAÇOES DE RISCOS E COBERTURAS
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
100007
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
670001
680001
690001
700001
**  TABELA DE CLAUSULA VERSUS COBERTURAS RAMO = 01
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
**  TABELA DE CLAUSULA VERSUS COBERTURAS RAMO = 06
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
**  TABELA DE CLAUSULA VERSUS COBERTURAS RAMO = 05
0101080
0102083
0103084
0104028
0199020
0201080
0202083
0203084
0204028
0299020
0301080
0302083
0303084
0304028
0399020
0401080
0402083
0403084
0404028
0499020
0501080
0502083
0503084
0504028
0599020
0601080
0602083
0603084
0604028
0699020
**  TABELA DE CLAUSULA VERSUS COBERTURAS RAMO = 17
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
**  TABELA DE CLAUSULA VERSUS COBERTURAS RAMO = 19
0110007
0210007
0310007
0410007
0510007
0510007
**  TABELA DE CLAUSULA VERSUS COBERTURAS RAMO = 55
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
** ULTIMO DIA DO SEGURO PARA DATA TERMO
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
** MESES/DIAS
013102280331043005310630073108310930103111301231
