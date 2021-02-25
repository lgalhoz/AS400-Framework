     H DECEDIT(',') DFTNAME(TRSF20) DATEDIT(*DMY/)
     H********************************************************************
     H*  EMPRESA    :  GRUPO MATA.                                       *
     H*  PROGRAMA   :  TRSF20                                            *
     H*  TIPO       :  SRT                                               *
     H*  APLICAÇÃO  :  PRODUÇAO GUARDIAN (EXPLORAÇAO TODOS OS RAMOS)     *
     H*  FUNÇÃO        - CRIAÇÃO DE FICHEIRO PARA TRANSFERÊNCIA DE       *
     H*                  INFORMAÇÃO (DADOS) PARA A LUSITANIA "VERDE"     *
     H*                                                                  *
     H*  DADOS      :  FICHEIRO DE SINISTROS - AUTOMÓVEL                 *
     H*                SINISTROS / PROVISOES / PAGAMENTOS                *
     H*                                                                  *
     H*  AUTOR      :  PAULO CORREIA                                     *
     H*  DATA       :  01/06/2000                                        *
     H*==================================================================*
     H*                    REGISTO DE MANUTENÇÃO                         *
     H*                                                                  *
     H*   DATA   ABREV.  DESCRIÇÃO DA ALTERAÇÃO                          *
     H* -------- ------  ----------------------                          *
     H*                                                                  *
     H*                                                                  *
     F********************************************************************
     F*              RESUMO DA UTILIZAÇÃO DOS INDICADORES                *
     F********************************************************************
     F*                                                                  *
     F*  01 - IDENTIFICA REGISTO FICHEIRO DE PROCESSOS DE SINISTROS      *
     F*  03 - IDENTIFICA REGISTO DO FICHEIRO DE INTERVENIENTES SINISTRO  *
     F*  04 - IDENTIFICA REGISTO FICHEIRO DE PROVISOES PARA SINISTROS    *
     F*  05 - IDENTIFICA REGISTO DO FICHEIRO PAGAMENTOS / REEMBOLSOS     *
     F*                                                                  *
     F*  20 - LOKUP TABELA DE RAMOS (CONVERSAO)                          *
     F*  21 - LOKUP TABELA DE CASOS IDS (CONVERSAO)                      *
     F*                                                                  *
     F********************************************************************
     FSINPRO    IP   F  384     7AIDISK    KEYLOC(2)
     FSININT    IF   F  448     8AIDISK    KEYLOC(2)
     FSINPRV    IF   F  128     8AIDISK    KEYLOC(2)
     FSINPAG    IF   F  300     8AIDISK    KEYLOC(2)
     FGBASEGRA  IF   F  700     9AIDISK    KEYLOC(1)
     FLSINAUT   UF A F  384    23AIDISK    KEYLOC(354)
     FLPRVAUT   UF A F  384    10AIDISK    KEYLOC(367)
     FLPAGAUT   UF A F  384    10AIDISK    KEYLOC(367)
     FESINAUT   O  A F  384    23AIDISK    KEYLOC(354)
     FEPRVAUT   O  A F  384    10AIDISK    KEYLOC(367)
     FEPAGAUT   O  A F  384    10AIDISK    KEYLOC(367)
     D********************************************************************
     D RMG             S              2  0 DIM(23) CTDATA PERRCD(1)             RAMOS L.AZUL VS L.VERDE
     D RML             S              6  0 DIM(23) ALT(RMG)
     D CASG            S              2  0 DIM(25) CTDATA PERRCD(1)             CASO IDS L.AZUL VS L.VERDE
     D CASL            S              2  0 DIM(25) ALT(CASG)
     D COB1G           S              4  0 DIM(7) CTDATA PERRCD(1)              COBERT. L.AZUL VS L.VERDE
     D COB1L           S              3  0 DIM(7) ALT(COB1G)
     D COB3G           S              4  0 DIM(23) CTDATA PERRCD(1)             COBERT. L.AZUL VS L.VERDE
     D COB3L           S              3  0 DIM(23) ALT(COB3G)
     D COB5G           S              4  0 DIM(24) CTDATA PERRCD(1)             COBERT. L.AZUL VS L.VERDE
     D COB5L           S              3  0 DIM(24) ALT(COB5G)
     D TPAGG           S              9  0 DIM(3) CTDATA PERRCD(1)              TP.PAG. LA VS COB.DESP LV
     D COBDL           S              3  0 DIM(3) ALT(TPAGG)
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
     D*
     D* ANO E MES E DIA DE MOVIMENTO (SELECCAO)
     D  UDTMOV                 1      6  0
     D  UAAMOV                 1      2  0
     D  UMMMOV                 3      4  0
     D  UDDMOV                 5      6  0
     D  UDTINI                 7     12  0
     D*
     D                 DS
     D  KEYPRO                 1     23
     D  IPROC                  1      1
     D  AAPROC                 2      3  0
     D  NSEQ                   4      7  0
     D  AA1                    8      9  0
     D  AAFIM                 10     11  0
     D  DTFIM                 10     15  0
     D  AA2                   16     17  0
     D  AARINI                18     19  0
     D  DTRINI                18     23  0
     D                 DS
     D  RMAPOL                 1      9  0
     D  RM                     1      2  0
     D  AAPOL                  2      9  0
     D                 DS
     D  CHINT                  1      8
     D  NPROCI                 1      6  0
     D  CODINT                 7      7
     D  NREG                   8      8  0
     D                 DS
     D  UDTTRF                15     26  0
     D  UATRF                 21     22  0
     D  UMTRF                 23     24  0
     D  UDTRF                 25     26  0
     D                 DS
     D  TIPMOV                 1      3  0
     D  TPMOV1                 1      1  0
     D
     D WCodPostal      DS
     D  WCplSin                1      7  0
     D  CODPOST                1      4  0
     D  CODFREG                5      7  0
     D
     D*==> Informação CPRS
     D LDATENTDOC      S              8S 0 INZ(*ZEROS)
     D ICREGESTR       S              1A   INZ(*BLANKS)
     D ICFACEXC        S              1A   INZ(*BLANKS)
     D ICSUSFRA        S              1A   INZ(*BLANKS)
     D ICDECLEI        S              1A   INZ(*BLANKS)
     D
     D*==> Constantes
     D KTE_MIP         C                   '851'
     D KTE_DTR         C                   '854'
     I********************************************************************
     I* FICHEIRO DE SINISTROS - GUARDIAN AUTOMÓVEL                       *
     I* CHAVE = KEYPRO                                                   *
     I********************************************************************
     ISINPRO    NS  01    2 CP
     I                                  1    1  ACTREG
     I                                  2    2  IPROC
     I                                  2    8  NPROC
     I                                  3    4 0AAPROC
     I                                  3    8 0ANPROC
     I                                  5    8 0NSEQ
     I                                  9    9 0SPROC
     I                                 10   11 0AAINI
     I                                 10   15 0DTABER
     I                                 16   21 0DTRINI
     I                                 22   27 0DTFIM
     I                             P   28   32 2TOTEST
     I                             P   33   37 2PROV
     I                             P   38   42 2PAGAM
     I                             P   43   47 2RECUP
     I                             P   48   52 2DESPQP
     I                             P   53   57 2RECUPQ
     I                                 58   59  CAUSA
     I                                 60   65 0DTSIN
     I                                 66  115  LOCSIN
     I                                116  118 0CODCON
     I                                119  119  FERID
     I                                120  120  TESTEM
     I                                121  121  AUTORI
     I                                122  127 0DTPART
     I                                128  128  FPART
     I                                129  168  NOMPAR
     I                                169  173 0NSEG
     I                                174  174 0TIPOAP
     I                                175  175 0SITAP
     I* REDEFINIDO ( SO  2 POSIÇÕES)        176 181 RM
     I                                176  177 0RM
     I                             P  178  181 0CPLSIN
     I                                182  188 0AP
     I                                189  194 0DTAPOL
     I                                195  200 0DATMAT
     I                             P  201  205 0CAPINI
     I                                207  207  LEADER
     I                                208  211 2QPCOS
     I                                212  215 2PACEIT
     I                                216  219 2PCEDID
     I                                220  221 0R1
     I                             P  222  226 0CAP1
     I                             P  227  231 0FRQ1
     I                                232  233 0R2
     I                             P  234  238 0CAP2
     I                             P  239  243 0FRQ2
     I                                244  245 0R3
     I                             P  246  250 0CAP3
     I                             P  251  255 0FRQ3
     I                                256  256 0TIPBON
     I                             P  257  261 2VALBON
     I                                262  262  TIPOPR
     I                                263  263  ACORDO
     I                                264  264  REGSIN
     I                                265  265  AGRAVA
     I                                266  271 0NNPROC
     I                                289  289  AJUDIC
     I                                290  295 0DTRJUD
     I                                301  302 0CODFRQ
     I                                303  316  MARCA
     I                                317  327  MATRIC
     I                                328  331 0CATV1
     I                                332  333 0AAV1
     I                                334  347  MARC2
     I                                348  358  MATR2
     I                                359  362 0CATV2
     I                                363  364 0AAV2
     I                                365  365  IDS                             IDS = S/N
     I                                366  366  TIPIDS                          TIPO DE IDS
     I*                                                                     D = IDS DEVEDOR
     I*                                                                     C = IDS CREDOR
     I                                367  368 0TPRESP                          TABELA PRATICA RESPONSAB.
     I                             P  369  370 0PRESP                           PRECENTAGEM RESPONSAB.
     I*
     I                                365  365 0EVOL1
     I                                366  366 0EVOL2
     I                                367  367 0EVOL3
     I                                368  368 0EVOL4
     I                                369  369 0EVOL5
     I                                370  370 0EVOL6
     I                                371  376 0DTUACT
     I                                377  384  USERID
     I          NS  02    2 C
     I********************************************************************
     I*        *** FICHEIRO DE INTERVENIENTES ***
     I********************************************************************
     I/SPACE 3
     ISININT    NS  03
     I                                  1    1  ACTREG                          Actividade Registo
     I                                  2    7 0NPROCI
     I                                  2    3 0APROCI                          Ano Processo
     I                                  4    7 0SEQINT                          N.sequencia
     I                                  8    8  CODINT                          Cod. Identificação
     I                                  9    9 0NREG                            N.Registo
     I                                 10   10  CODREG                          Codigo Registo
     I                                 11   11  TPDN                            Tipo de Danos
     I                                 12   51  NOMSIN                          Nome Segurado
     I                                 52   91  MORSIN                          Morada Segurado
     I                                 92   95 0CPSIN                           Cod.Postal Sinistrado
     I                                 96  101 0DTNSIN                          Data Nascimento Sinist
     I                                102  107 0DTCART                          Data Carta Condução
     I                                108  147  NOMSEG                          Nome Segurado
     I                                148  187  MORSEG                          Morada Segurado
     I                             P  188  191 0CPSEG                           Cod.Postal Segurado
     I                                192  205  MARC1                           Marca do Veiculo
     I                                206  216  MATR1                           Matricula do Veiculo
     I                                217  221  CP3                             Cod.Companhia Terceiro
     I                                222  231  APOL3                           Apólice do Terceiro
     I                                232  233  TPSEG                           Tipo Apolice (RC/DP)
     I                                234  283  EXTDAN                          Ext. dos Danos fisicos
     I                                284  284  HOSPSN                          Hospitalização:Sim/Não
     I                                285  314  HOSPLC                          Local da Hospitalizaç.
     I                                315  315  TPSINS                          Tipo Sinistrado:
     I                                316  345  LSOCOR
     I                                346  346  TRNSPU
     I                                347  348 0FSANC
     I                                349  356 0DRECUP
     I                                357  358 0FSATD
     I                                359  367 0FSAPE
     I*---------                            368 369 FILLER
     I                                370  370  DANOS                           Danos
     I                                371  376 0DTUACT                          Data Ultima Actualiza.
     I                                377  384  USERID                          User ID
     I                                385  404  PROCT
     I                                405  410 0DTNAV
     I********************************************************************
     I*        *** FICHEIRO DE PROVISOES ***
     I********************************************************************
     ISINPRV    NS  04
     I                                  1    1  ACTREG
     I                                  2    3 0AAPPRV
     I                                  4    7 0SEQPRV
     I                                  2    7 0PROPRV
     I                                  8    9 0NLIPRV
     I                             P   10   11 0TIPMOV
     I                                 12   17 0DTMOV
     I                                 12   17 0PRVDTM
     I                                 18   47  DESC
     I                             P   48   52 2VALOR
     I                             P   53   57 2VALR1
     I                             P   58   62 2VALR3
     I                             P   63   67 2VALR45
     I* REDEFINIDO ( SO 2 POSIÇÕES )         68  73 RM
     I                                 68   69 0RMPRV
     I                             P   74   78 2VALMIP
     I                             P   79   83 2VALDTRA
     I                                 74   83  CTAPRV
     I                                 84   93  CTAIND
     I                                 94   94 0SIT1
     I                                 95   95 0SIT2
     I                                 96   96 0SIT3
     I*                                      97 100 FILLER
     I                             P  101  105 2DMATER
     I                             P  106  110 2DFISIC
     I*                                     117 114 FILLER
     I                                115  120 0DTUACT
     I                                121  128  USERID
     I********************************************************************
     I*        *** FICHEIRO DE PAGAMENTOS E REEMBOLSOS ***
     I********************************************************************
     ISINPAG    NS  05
     I                                  1    1  ACTREG
     I                                  2    3 0AAPPAG
     I                                  4    7 0SEQPAG
     I                                  2    7 0PROPAG
     I                                  8    9 0NLIPAG
     I                             P   10   11 0TIPMOV
     I                                 12   17 0DTMOV
     I                                 12   17 0PAGDTM
     I                                 18   47  DESC
     I                             P   48   52 2VALOR
     I                             P   53   57 2VALR1
     I                             P   58   62 2VALR3
     I                             P   63   67 2VALR45
     I* REDEFINIDO ( SO 2 POSIÇÕES )         68  73 RM
     I                                 68   69 0RMPAG
     I                             P   74   78 2VALMIP
     I                             P   79   83 2VALDTRA
     I                                 74   83  CTAPRV
     I                                 84   93  CTAIND
     I                                 94   94  PF
     I                                 95   95  I100
     I                                 96   96 0FORMAP
     I*
     I*
     I*
     I*
     I                                 97  102 0DT
     I                                103  112  DOCCH
     I                                115  116 0BANCO
     I                                117  119 0DEPBAN
     I                                120  121 0ENTPAG
     I                                122  126 0PAGENT
     I******                                127 166 NOMPAG
     I                                127  153  NOMPAG
     I                                167  206  MORPAG
     I                                207  226  LOCPAG
     I                             P  227  230 0POSPAG
     I                                231  236 0DTFACT                          Data Fact/Doc.
     I                                241  255  NODOC
     I*                                     256 268 FILLER
     I                             P  269  273 2DMATER
     I                             P  274  278 2DFISIC
     I                                284  284  RECEMI
     I                                285  285  ENVIO
     I                                286  286  SIT
     I                                287  292 0DTUACT
     I                                293  300  USERID
     I*
     I****************************************************
     I* FICHEIRO - BASE DE APÓLICES ( OUTROS RAMOS )     *
     I****************************************************
     IGBASEGRA  AA
     I*                                                                     V=PREMIO VARIAVEL
     I                                315  315  CS                              TIPO DE TRABALHADOR
     I********************************************************************
     I* FICHEIROS DE CONTROL *                                          *
     I********************************************************************
     ILSINAUT   NS
     I*
     ILPRVAUT   NS
     I*
     ILPAGAUT   NS
     F********************************************************************
     F* DETERMINA DATA DE TRANSFERÊNCIA                                  *
     F********************************************************************
     C*
     C* CONTROLA REGISTOS DE PROCESSO A PROCESSAR
     C*
     C   02              GOTO      FIM
     C*
     C* CONTROLA PROCESSOS CUJO RAMO NAO É AUTOMÓVEL (REG. ERRADOS)
     C*
+----C     RM            IFNE      50
|    C                   GOTO      FIM
+----C                   END
     C
     C*==> Formatar Código Postal e Localidade Postal
     C                   MOVE      CPLSIN        WCplSin
     C                   MOVE      ACORDO        ICDECLEI
     C
     C*
     C* CONTROLA PROCESSOS IDS (SE PROC.É IDS NÃO CRIA REGISTO P/ENVIO)
     C*
+----C     IDS           IFEQ      'S'
|    C                   GOTO      FIM
+----C                   ELSE
|+---C     NSEQ          IFGT      8000
||   C                   GOTO      FIM
|+---C                   END
+----C                   END
     C*
+----C     FLAGU         IFEQ      0                                            <-----------+
|    C                   TIME                    UDTHH                                      I
|    C                   Z-ADD     1             FLAGU             1 0                      I
|    C*                                                                I
|    C* FORMATA DATA DO MOVIMENTO - DATA DE SELECÇÃO                   I
|    C*                                                                I
|    C                   Z-ADD     *ZEROS        DTY2                                       I
|    C                   Z-ADD     UDTMOV        DTY2                                       I
|    C                   EXSR      SRDTY2                                                   I
|    C                   MOVE      DTY2          LDTMOV            8 0                      I
|    C*                                                                I
|    C* FORMATA DATA DA TRANSFERÊNCIA                                  I
|    C*                                                                I
|    C                   MOVE      UDDDT         UDTRF                                      I
|    C                   MOVE      UMMDT         UMTRF                                      I
|    C                   MOVE      UAADT         UATRF                                      I
|    C*                                                                I
|    C                   Z-ADD     *ZEROS        DTY2              8 0                      I
|    C                   MOVE      UDTTRF        DTY2                                       I
|    C                   EXSR      SRDTY2                                                   I
|    C                   MOVE      DTY2          LDTTRF            8 0                      I
|    C*                                                                I
+----C                   END                                                    <-----------+
     C*
     C* LIMPA CAMPOS DE OUTPUT ( NOVA DEFINIÇAO  -  PROCESSOS )
     C*
     C                   MOVE      *BLANKS       LRAMO             2            * CODIGO DE RAMO
     C                   MOVE      *BLANKS       LMOD              2            * MODALIDADE
     C                   MOVE      *BLANKS       LSMOD             2            * SUB-MODALIDADE
     C                   MOVE      *BLANKS       LAPOL             7            * APOLICE
     C                   Z-ADD     *ZEROS        LANO              4 0          * ANO
     C                   MOVE      *BLANKS       LNPROC            5            * NUMERO DE SINISTRO (PROCESSO)
     C*
     C                   Z-ADD     *ZEROS        LDTSIN            8 0          * DATA DO SINISTROS
     C                   Z-ADD     *ZEROS        LDTABE            8 0          * DATA DE ABERTURA
     C                   MOVE      *BLANKS       LCENC             2            * CÓDIGO DE ENCERRAMENTO
     C                   Z-ADD     *ZEROS        LDTENC            8 0          * DATA DE ENCERRAMENTO
     C                   Z-ADD     *ZEROS        LDTRAB            8 0          * DATA DE REABERTURA
     C*
     C                   MOVE      *BLANKS       LIDS              1            * INDICADOR DE IDS
     C*
     C                   MOVE      *BLANKS       LNCASO            2            * NUMERO DO CASO IDS
     C                   Z-ADD     *ZEROS        LDTNAV            8 0          * DATA DE AVISO
     C                   MOVE      *BLANKS       LMARCT           18            * MARCA VEÍCULO TERCEIRO
     C                   MOVE      *BLANKS       LMATRT           12            * MATRICULA DO TERCEIRO
     C                   MOVE      *BLANKS       LCOMPT            4            * COMPANHIA DO TERCEIRO
     C                   MOVE      *BLANKS       LCIDST            3            * CENTRO IDS DO TERCEIRO
     C                   MOVE      *BLANKS       LAPOLT           10            * APÓLICE DO TERCEIRO
     C                   MOVE      *BLANKS       LPROCT           20            * PROCESSO SIN. TERCEIRO
     C                   MOVE      *ZEROS        LNRESP            3 0          * NOSSA RESPONSABILIDADE
     C                   MOVE      *ZEROS        LRESP             3 0          * RESPONSABILIDADE OUTROS
     C                   MOVE      *BLANKS       LLIPAG            1            * COSEGURO PAGA?(S/N)
     C*
     C* LIMPA CAMPOS DE OUTPUT ( NOVA DEFINIÇAO  -  PROVISÕES )
     C*
     C                   MOVE      *BLANKS       LCOBPV            3            * COBERTURA AFECTADA
     C                   Z-ADD     *ZEROS        LVLPRV           11 2          * PROVISÃO AFECTADA
     C*
     C* LIMPA CAMPOS DE OUTPUT ( NOVA DEFINIÇAO  -  PAGAMENTOS / REEMBOLSOS )
     C*
     C                   MOVE      *BLANKS       LNRECS            5            * NR. RECIBO_SINISTRO
     C                   MOVE      *BLANKS       LEREC             1            * ESPECIE DE RECIBO
     C                   Z-ADD     *ZEROS        LDTEMI            8 0          * DATA DE EMISSAO RECIBO
     C                   Z-ADD     *ZEROS        LTREC            13 2          * TOTAL DO RECIBO (PAG/REMB)
     C                   MOVE      *BLANKS       LCANUL            1            * CODIGO DE ANULAÇÃO
     C                   Z-ADD     *ZEROS        LDTANU            8 0          * DATA DE ANULAÇÃO
     C                   MOVE      *BLANKS       LCMOV             1            * CODIGO MOVIMENTO
     C                   MOVE      *BLANKS       LCOBPG            3            * COBERTURA AFECTADA
     C                   MOVE      *BLANKS       LCDESP            3            * DESPESA
     C*
     C* LIMPA CAMPOS DE TRABALHO
     C*
     C                   MOVE      *BLANKS       W1RAMO            6
     C                   MOVE      *BLANKS       W2RAMO            4
     C*
     C                   Z-ADD     *ZEROS        Y2000             4 0
     C                   Z-ADD     *ZEROS        DTY2              8 0
     C*
     C********************************************************************
     C*
     C* FORMATA DATA ABERTURA PROCESOS DE SINISTRO EM JOSÉ MATA
     C*
     C                   Z-ADD     *ZEROS        DTY2
     C                   MOVE      DTABER        DTY2
     C                   EXSR      SRDTY2
     C                   MOVE      DTY2          JMDTP             8 0
     C*
+----C     JMDTP         IFGT      LDTMOV
|    C                   GOTO      FIM
+----C                   END
     C*
     C                   Z-ADD     0             AA1
+----C     DTFIM         IFNE      0
|+---C     AAFIM         IFGE      50
||   C                   Z-ADD     19            AA1
|+---C                   ELSE
||   C                   Z-ADD     20            AA1
|+---C                   END
+----C                   END
     C                   Z-ADD     0             AA2
+----C     DTRINI        IFNE      0
|+---C     AARINI        IFGE      50
||   C                   Z-ADD     19            AA2
|+---C                   ELSE
||   C                   Z-ADD     20            AA2
|+---C                   END
+----C                   END
     C*
     C* DETERMINA O ENVIO ANTERIOR DO PROCESSO DE SINISTRO
     C*
     C                   SETOFF                                       98
     C     KEYPRO        CHAIN     LSINAUT                            99
     C   99              SETON                                        98
     C*
     C* CONVERTE RAMOS  ( RAMO GUARDIAN VS LUSITANIA ).
     C*
     C                   Z-ADD     1             I                 2 0
     C     RM            LOOKUP    RMG(I)                                 20
     C   20              MOVE      RML(I)        W1RAMO
     C  N20              MOVE      *BLANKS       W1RAMO
     C                   MOVE      W1RAMO        W2RAMO
     C*
     C                   MOVEL     W1RAMO        LRAMO
     C                   MOVEL     W2RAMO        LMOD
     C                   MOVE      W2RAMO        LSMOD
     C*
     C                   MOVE      AP            LAPOL
     C*
     C                   MOVEL     RM            RRAPOL            9 0
     C                   MOVE      AP            RRAPOL
     C     RRAPOL        CHAIN     GBASEGRA                           99
+----C  N99CS            IFEQ      '2'
|    C                   MOVE      'S'           LLIPAG
+----C                   END
     C*
     C* FORMATA ANO DO PROCESSO
     C*
     C                   Z-ADD     *ZEROS        Y2000
     C                   MOVE      AAPROC        Y2000
     C                   EXSR      SR2000
     C                   MOVE      Y2000         LANO
     C*
     C                   MOVE      ANPROC        LNPROC
     C*
+----C     DTABER        IFGE      050101                                       - A PARTIR
|    C                   MOVEL     '9'           LNPROC                           DE 01/2005
+----C                   ELSE
|    C                   MOVEL     '7'           LNPROC
+----C                   END
     C*
     C* FORMATA DATA OCORRÊNCIA DO SINISTRO
     C*
     C                   Z-ADD     *ZEROS        DTY2
     C                   MOVE      DTSIN         DTY2
     C                   EXSR      SRDTY2
     C                   MOVE      DTY2          LDTSIN
     C*
     C* FORMATA DATA ABERTURA SINISTRO
     C*
     C                   Z-ADD     *ZEROS        DTY2
     C                   MOVE      DTABER        DTY2
     C                   EXSR      SRDTY2
     C                   MOVE      DTY2          LDTABE
     C*
     C* CONVERSAO CÓDIGO ENCERRAMENTO (É UM VALOR DEFAULT)
     C*
+----C     DTFIM         IFGT      *ZEROS
|+---C     IDS           IFEQ      'S'
||   C                   MOVE      '08'          LCENC
|+---C                   ELSE
||   C                   MOVE      '02'          LCENC
|+---C                   END
+----C                   ELSE
|    C                   MOVE      '00'          LCENC
+----C                   END
     C*
     C* FORMATA DATA ENCERRAMENTO SINISTRO
     C*
     C                   Z-ADD     *ZEROS        DTY2
     C                   MOVE      DTFIM         DTY2
     C                   EXSR      SRDTY2
     C                   MOVE      DTY2          LDTENC
     C*
     C* FORMATA DATA DE REABERTURA DO PROCESSO
     C*
     C                   Z-ADD     *ZEROS        DTY2
     C                   MOVE      DTRINI        DTY2
     C                   EXSR      SRDTY2
     C                   MOVE      DTY2          LDTRAB
     C*
     C* FORÇA CAMPO DE IDS A NORMAL SE ESTÁ A BRANCO
     C*
+----C     IDS           IFEQ      ' '
|    C                   MOVE      'N'           IDS
+----C                   END
     C*
     C* FORMATA INDICADOR DE IDS
     C*
+----C     IDS           IFEQ      'N'
|    C                   MOVE      'N'           LIDS
+----C                   ELSE
|    C                   MOVE      TIPIDS        LIDS
+----C                   END
     C*
+----C     IDS           IFEQ      'S'
|    C*
|    C* CONVERTE CASO IDS (NAO EXISTE COINCIDÊNCIA DE TABELAS)
|    C*
|    C                   Z-ADD     1             I
|    C     TPRESP        LOOKUP    CASG(I)                                21
|    C   21              MOVE      CASG(I)       LNCASO
|    C  N21              MOVE      '  '          LNCASO
|    C*                    MOVE TPRESP    LNCASO
|    C*
|    C* ACEDE À INFORMAÇAO DO INTERVERNIENTE (TERCEIRO / SEGURADO)
|    C*
|    C                   MOVE      *BLANKS       CHINT
|    C                   MOVE      ANPROC        NPROCI
|    C     CHINT         SETLL     SININT
|    C                   EXSR      SRINT
|    C*
|    C* FORMATA PROCESSO DO TERCEIRO
|    C*
|    C                   MOVE      PROCT         LPROCT
|    C*
|    C* DETERMINA AS RESPONSABILIDADES
|    C*
|    C                   Z-ADD     *ZEROS        NRESP             3 0
|    C                   Z-ADD     *ZEROS        RESP              3 0
|    C*
|+---C     PRESP         IFEQ      0
||   C                   MOVE      100           PRESP
|+---C                   END
|    C*
|+---C     TIPIDS        IFEQ      'D'
||   C                   Z-ADD     PRESP         NRESP
||   C     100           SUB       PRESP         RESP
|+---C                   ELSE
||   C     100           SUB       PRESP         NRESP
||   C                   Z-ADD     PRESP         RESP
|+---C                   END
|    C*
|    C                   MOVE      NRESP         LNRESP
|    C                   MOVE      RESP          LRESP
|    C*
+----C                   END
     C*
     C   98              EXCEPT    REGPRO
     C*
     C     SALTA         TAG
     C*
     C****************************************************
     C*   FORMATA FICHEIRO DE PROVISOES                   *
     C********************************************    > < =
     C*
     C* ACEDE AS PROVISOES DO PROCESSOS
     C*
     C                   MOVE      *BLANKS       C8                8
     C                   MOVEL     ANPROC        C8
     C                   MOVE      '00'          C8
     C     C8            SETLL     SINPRV
     C*
     C*  LÊ PROVISOES PARA PROCESSO E FORMATA CAMPOS PARA OUTPUT
     C*
     C     LOOPRV        TAG
     C                   READ      SINPRV                                 50
     C   50              GOTO      SEGUE1
     C*
     C     PROPRV        COMP      ANPROC                             5151
     C   51              GOTO      SEGUE1
     C*
     C                   Z-ADD     *ZEROS        DTY2                                       I
     C                   Z-ADD     PRVDTM        DTY2
     C                   EXSR      SRDTY2
     C                   MOVE      DTY2          LPRVDT            8 0
     C*
     C     LPRVDT        IFLE      LDTMOV
     C                   MOVE      *BLANKS       KEYPRV           10 0
     C                   MOVEL     ANPROC        WKPRV1            8 0
     C                   MOVE      NLIPRV        WKPRV1
     C                   MOVEL     WKPRV1        KEYPRV
     C*
     C                   EXSR      SRFPRV
     C                   END
     C*
     C                   GOTO      LOOPRV
     C*
     C****************************************************
     C     SEGUE1        TAG
     C****************************************************
     C*
     C     C8            SETLL     SINPAG
     C     LOOPAG        TAG
     C                   READ      SINPAG                                 50
     C   50              GOTO      SEGUE2
     C*
     C     PROPAG        COMP      ANPROC                             5252
     C   52              GOTO      SEGUE2
     C*
     C                   Z-ADD     *ZEROS        DTY2
     C                   Z-ADD     PAGDTM        DTY2
     C                   EXSR      SRDTY2
     C                   MOVE      DTY2          LPAGDT            8 0
     C*
     C     LPAGDT        IFLE      LDTMOV
     C                   MOVE      *BLANKS       KEYPAG           10 0
     C                   MOVEL     ANPROC        WKPAG1            8 0
     C                   MOVE      NLIPAG        WKPAG1
     C                   MOVEL     WKPAG1        KEYPAG
     C*
     C     TPMOV1        IFEQ      1                                            PAGAMENTOS/REEMBOLSOS
     C*
     C     TIPMOV        IFEQ      112
     C                   EXSR      SRCIND
     C                   ELSE
     C     TIPMOV        IFEQ      113
     C                   EXSR      SRCIND
     C                   ELSE
     C     TIPMOV        IFEQ      114
     C                   EXSR      SRCIND
     C                   ELSE
     C     TIPMOV        IFEQ      115
     C                   EXSR      SRCIND
     C                   ELSE
     C                   EXSR      SRFPAG
     C                   END
     C                   END
     C                   END
     C                   END
     C*
     C                   END
     C                   END
     C*
     C     TPMOV1        IFEQ      2                                            RECUPERAÇÕES
     C                   EXSR      SRFREE
     C                   END
  LG C*                  END
     C*
  LG C*                  END
     C*
     C                   GOTO      LOOPAG
     C*
     C****************************************************
     C     SEGUE2        TAG
     C****************************************************
     C     FIM           TAG
     C/EJECT
     C*==========================================================
     C* Subrotina.: SR2000
     C* Objectivo.: Formata o ano da data com 4 digitos
     C*==========================================================
+----C     SR2000        BEGSR
|    C*
|    C     Y2000         IFLT      40
|    C                   MOVEL     20            Y2000
|    C                   ELSE
|    C                   MOVEL     19            Y2000
|    C                   END
|    C*
+----C                   ENDSR
     C*==========================================================
     C* Subrotina.: SRDTY2
     C* Objectivo.: Formata a data com 8 digitos
     C*==========================================================
+----C     SRDTY2        BEGSR
|    C*
|    C     DTY2          IFGT      *ZEROS
|    C     DTY2          IFLT      400000
|    C                   MOVEL     20            DTY2
|    C                   ELSE
|    C                   MOVEL     19            DTY2
|    C                   END
|    C                   END
|    C*
+----C                   ENDSR
     C*==========================================================
     C* Subrotina.: SPRINT
     C* Objectivo.: Procurar Intervenientes
     C*==========================================================
+----C     SRINT         BEGSR
|    C*
|    C                   MOVE      *BLANK        FLAG              1
|    C*
|    C     FLAG          DOWEQ     *BLANK
|    C                   READ      SININT                                 50
|    C*                                                                      I
|    C   50              MOVE      'X'           FLAG
|    C  N50ANPROC        IFEQ      NPROCI
|    C     CODINT        IFEQ      'S'
|    C                   ELSE
|    C*                                                                I  I  I
|    C* FORMATA MARCA/MATRICULA/COMP./APÓLICE DO TERCEIRO              I  I  I
|    C*                                                                I  I  I
|    C                   MOVEL     MARC1         LMARCT
|    C                   MOVEL     MATR1         LMATRT
|    C                   MOVE      CP3           LCOMPT
|    C                   MOVEL     APOL3         LAPOLT
|    C*                                                                I  I  I
|    C* FORMATA PROCESSO DO TERCEIRO ( NÃO PODE SER ACTIVADO )         I  I  I
|    C*                                                                I  I  I
|    C                   MOVE      PROCT         LPROCT
|    C*                                                                I  I  I
|    C* FORMATA DATA DE AVISO ( NAO PODE SER ACTIVADO AINDA)           I  I  I
|    C*                                                                I  I  I
|    C                   Z-ADD     *ZEROS        DTY2
|    C                   MOVE      DTNAV         DTY2
|    C                   EXSR      SRDTY2
|    C                   MOVE      DTY2          LDTNAV
|    C                   END
|    C                   ELSE
|    C                   MOVE      'X'           FLAG
|    C                   END
|    C                   END
|    C*
+----C                   ENDSR
     C/EJECT
     C*==========================================================
     C* Subrotina.: SRFPRV
     C* Objectivo.: Formatar Provisões
     C*
     C*==========================================================
+----C     SRFPRV        BEGSR
|    C*
|    C* LIMPA CAMPOS DE TRABALHO
|    C*
|    C                   Z-ADD     *ZEROS        RI                1 0
|    C                   Z-ADD     *ZEROS        RIII              1 0
|    C                   Z-ADD     *ZEROS        RIVV              1 0
|    C                   Z-ADD     *ZEROS        WCOB1             2 0
|    C                   Z-ADD     *ZEROS        PCOB1             4 0
|    C                   Z-ADD     *ZEROS        WCOB3             2 0
|    C                   Z-ADD     *ZEROS        PCOB3             4 0
|    C                   Z-ADD     *ZEROS        WCOB5             2 0
|    C                   Z-ADD     *ZEROS        PCOB5             4 0
|    C*
|    C* =================================================================================
|    C*  RISCO I - ( DANOS MATERIAIS ) - PROVISÕES
|    C* =================================================================================
|    C*
|    C* DETERMINA O ENVIO ANTERIOR DA PROVISÃO DO SINISTRO
|    C*
|    C                   MOVE      *BLANKS       WKPRV            10
|    C                   MOVE      *BLANKS       WKR1              2
|    C                   MOVE      R1            WKR1
|    C                   MOVEL     'M'           WKR1
|    C                   MOVEL     KEYPRV        WKPRV
|    C                   MOVE      WKR1          WKPRV
|    C     WKPRV         CHAIN     LPRVAUT                            99
|    C  N99              GOTO      PRVF01
|    C*
|+---C     DMATER        IFNE      *ZEROS
||   C*                                                           I
||   C                   MOVE      1             RI
||   C                   MOVE      R1            WCOB1
||   C                   MOVEL     RI            WCOB1
||   C                   MOVEL     WCOB1         PCOB1
||   C                   MOVE      CAUSA         PCOB1
||   C*                                                           I
||   C* SE PROCESSO É IDS CONVERTE COBERTURAS ESPECIFICAS DE IDS  I
||   C*                                                           I
||+--C     IDS           IFEQ      'S'
|||+-C     TIPIDS        IFEQ      'D'
|||| C                   MOVE      9             RI
|||| C                   MOVE      9             WCOB1
|||| C                   MOVEL     RI            WCOB1
|||| C                   MOVEL     WCOB1         PCOB1
|||| C                   MOVE      92            PCOB1
|||+-C                   ELSE
||||+C     TIPIDS        IFEQ      'C'
|||||C                   MOVE      9             RI
|||||C                   MOVE      9             WCOB1
|||||C                   MOVEL     RI            WCOB1
|||||C                   MOVEL     WCOB1         PCOB1
|||||C                   MOVE      91            PCOB1
||||+C                   ELSE
||||+C                   END
|||+-C                   END
||+--C                   END
||   C*                                                           I
||   C* PESQUISA/CONVERTE A COBERTURA AFECTA PELA PROVISAO        I
||   C*                                                           I
||   C                   Z-ADD     1             I                 2 0
||   C     PCOB1         LOOKUP    COB1G(I)                               30
||   C   30              MOVE      COB1L(I)      LCOBPV
||+--C  N30              DO
|||  C                   MOVE      '00'          PCOB1
|||  C                   Z-ADD     1             I                 2 0
|||  C     PCOB1         LOOKUP    COB1G(I)                               30
|||  C   30              MOVE      COB1L(I)      LCOBPV                         bot
|||  C  N30              MOVE      *BLANKS       LCOBPV
||+--C                   END
||   C*                                                           I
||   C                   MOVE      LCOBPV        LCDESP
||   C                   MOVE      DMATER        LVLPRV
||   C     LVLPRV        COMP      0                                    22
||   C   22              Z-SUB     LVLPRV        LVLPRV
||   C                   EXCEPT    REGPRV
|+---C                   END
|    C*
|    C* =================================================================================
|    C*  RISCO I - ( DANOS FISICOS   ) - PROVISÕES
|    C* =================================================================================
|    C     PRVF01        TAG
|    C*
|    C*
|    C* DETERMINA O ENVIO ANTERIOR DA PROVISÃO DO SINISTRO
|    C*
|    C                   MOVE      *BLANKS       WKPRV            10
|    C                   MOVE      *BLANKS       WKR1
|    C                   MOVE      R1            WKR1
|    C                   MOVEL     'F'           WKR1
|    C                   MOVEL     KEYPRV        WKPRV
|    C                   MOVE      WKR1          WKPRV
|    C     WKPRV         CHAIN     LPRVAUT                            99
|    C  N99              GOTO      PRV001
|    C*
|+---C     DFISIC        IFNE      *ZEROS
||   C*                                                           I
||   C* SE PROCESSO É IDS IGNORA REGISTO MOVIMENTO DANOS FISICOS  I
||   C* NOTA: PROCESSOS IDS NÃO PODEM TER DANOS FISICOS           I
||   C*                                                           I
||+--C     IDS           IFEQ      'S'
|||  C                   GOTO      PRV001
||+--C                   END
||   C*                                                           I
||   C                   MOVE      9             RI
||   C                   MOVE      9             WCOB1
||   C                   MOVEL     RI            WCOB1
||   C                   MOVEL     WCOB1         PCOB1
||   C                   MOVE      '90'          PCOB1
||   C*                                                           I
||   C* PESQUISA/CONVERTE A COBERTURA AFECTA PELA PROVISAO        I
||   C*                                                           I
||   C                   Z-ADD     1             I                 2 0
||   C     PCOB1         LOOKUP    COB1G(I)                               30
||   C   30              MOVE      COB1L(I)      LCOBPV
||+--C  N30              DO
|||  C                   MOVE      '00'          PCOB1
|||  C                   Z-ADD     1             I                 2 0
|||  C     PCOB1         LOOKUP    COB1G(I)                               30
|||  C   30              MOVE      COB1L(I)      LCOBPV
|||  C  N30              MOVE      *BLANKS       LCOBPV
||+--C                   END
||   C*                                                           I
||   C                   MOVE      LCOBPV        LCDESP
||   C                   MOVE      DFISIC        LVLPRV
||   C     LVLPRV        COMP      0                                    22
||   C   22              Z-SUB     LVLPRV        LVLPRV
||   C                   EXCEPT    REGPRV
|+---C                   END
|    C*
|    C* =================================================================================
|    C*  RISCO III - PROVISÕES
|    C* =================================================================================
|    C     PRV001        TAG
|    C*
|    C* DETERMINA O ENVIO ANTERIOR DA PROVISÃO DO SINISTRO
|    C*
|    C                   MOVE      *BLANKS       WKPRV            10
|    C                   MOVE      *BLANKS       WKR1
|    C                   MOVE      R2            WKR1
|    C                   MOVEL     3             WKR1
|    C                   MOVEL     KEYPRV        WKPRV
|    C                   MOVE      WKR1          WKPRV
|    C     WKPRV         CHAIN     LPRVAUT                            99
|    C  N99              GOTO      PRV002
|    C*
|+---C     VALR3         IFNE      *ZEROS
||   C*                                                            I
||   C* SE PROCESSO É IDS IGNORA REGISTO - MOVIMENTO INCORRECTO    I
||   C* NOTA: PROCESSOS IDS SÓ PODEM SER DE R.C.                   I
||   C*                                                            I
||+--C     IDS           IFEQ      'S'
|||  C                   GOTO      PRV002
||+--C                   END
||   C*                                                            I
||   C                   MOVE      3             RIII              1 0
||   C                   MOVE      R2            WCOB3
||   C                   MOVEL     RIII          WCOB3
||   C                   MOVEL     WCOB3         PCOB3
||   C                   MOVE      CAUSA         PCOB3
||   C*                                                            I
||   C* PESQUISA/CONVERTE A COBERTURA AFECTA PELA PROVISAO         I
||   C*                                                            I
||   C                   Z-ADD     1             I                 2 0
||   C     PCOB3         LOOKUP    COB3G(I)                               30
||   C   30              MOVE      COB3L(I)      LCOBPV
||+--C  N30              DO
|||  C                   MOVE      '00'          PCOB3
|||  C                   Z-ADD     1             I                 2 0
|||  C     PCOB3         LOOKUP    COB3G(I)                               30
|||  C   30              MOVE      COB3L(I)      LCOBPV
|||  C  N30              MOVE      *BLANKS       LCOBPV
||+--C                   END
||   C*                                                            I
||   C                   MOVE      LCOBPV        LCDESP
||   C                   MOVE      VALR3         LVLPRV
||   C     LVLPRV        COMP      0                                    22
||   C   22              Z-SUB     LVLPRV        LVLPRV
||   C                   EXCEPT    REGPRV
|+---C                   END
|    C*
|    C* =================================================================================
|    C*  RISCO IV+V - PROVISÕES
|    C* =================================================================================
|    C     PRV002        TAG
|    C*
|    C* DETERMINA O ENVIO ANTERIOR DA PROVISÃO DO SINISTRO
|    C*
|    C                   MOVE      *BLANKS       WKPRV            10
|    C                   MOVE      *BLANKS       WKR1
|    C                   MOVE      R2            WKR1
|    C                   MOVEL     5             WKR1
|    C                   MOVEL     KEYPRV        WKPRV
|    C                   MOVE      WKR1          WKPRV
|    C     WKPRV         CHAIN     LPRVAUT                            99
|    C  N99              GOTO      PRV003
|    C*
|+---C     VALR45        IFNE      *ZEROS
||   C*                                                            I
||   C* SE PROCESSO É IDS IGNORA REGISTO - MOVIMENTO INCORRECTO    I
||   C* NOTA: PROCESSOS IDS SÓ PODEM SER DE R.C.                   I
||   C*                                                            I
||+--C     IDS           IFEQ      'S'
|||  C                   GOTO      PRV003
||+--C                   END
||   C*                                                            I
||   C                   MOVE      5             RIVV              1 0
||   C                   MOVE      R2            WCOB5
||   C                   MOVEL     RIVV          WCOB5
||   C                   MOVEL     WCOB5         PCOB5
||   C                   MOVE      CAUSA         PCOB5
||   C*                                                            I
||   C* PESQUISA/CONVERTE A COBERTURA AFECTA PELA PROVISAO         I
||   C*                                                            I
||   C                   Z-ADD     1             I                 2 0
||   C     PCOB5         LOOKUP    COB5G(I)                               30
||   C   30              MOVE      COB5L(I)      LCOBPV
||+--C  N30              DO
|||  C                   MOVE      '00'          PCOB5
|||  C                   Z-ADD     1             I                 2 0
|||  C     PCOB5         LOOKUP    COB5G(I)                               30
|||  C   30              MOVE      COB5L(I)      LCOBPV
|||  C  N30              MOVE      *BLANKS       LCOBPV
||+--C                   END
||   C*                                                            I
||   C                   MOVE      LCOBPV        LCDESP
||   C                   MOVE      VALR45        LVLPRV
||   C     LVLPRV        COMP      0                                    22
||   C   22              Z-SUB     LVLPRV        LVLPRV
||   C                   EXCEPT    REGPRV
|+---C                   END
|    C
|    C* =================================================================================
|    C*  Cobertura MIP - PROVISÕES
|    C* =================================================================================
|    C     PRV003        TAG
     C                   IF        (DTFIM > *ZEROS) AND (DTFIM < 130101)
     C                   GOTO      ESRPRV
     C                   ENDIF
|    C*
|    C* DETERMINA O ENVIO ANTERIOR DA PROVISÃO DO SINISTRO
|    C*
|    C                   MOVE      *BLANKS       WKPRV            10
|    C                   MOVE      *BLANKS       WKR1
|    C                   MOVE      'MP'          WKR1
|    C                   MOVEL     KEYPRV        WKPRV
|    C                   MOVE      WKR1          WKPRV
|    C     WKPRV         CHAIN     LPRVAUT                            99
|    C  N99              GOTO      PRV004
|    C*
|+---C                   IF        (VALMIP > *ZEROS)
||   C*                                                            I
||   C* SE PROCESSO É IDS IGNORA REGISTO - MOVIMENTO INCORRECTO    I
||   C* NOTA: PROCESSOS IDS SÓ PODEM SER DE R.C.                   I
||   C*                                                            I
||+--C     IDS           IFEQ      'S'
|||  C                   GOTO      PRV004
||+--C                   END
||   C*
||   C                   MOVE      KTE_MIP       LCDESP
||   C                   MOVE      '052'         LCOBPV
||   C                   MOVE      VALMIP        LVLPRV
||   C     LVLPRV        COMP      0                                    22
||   C   22              Z-SUB     LVLPRV        LVLPRV
||   C                   EXCEPT    REGPRV
|+---C                   END
|    C
|    C* =================================================================================
|    C*  Cobertura Despesas de Tratamento
|    C* =================================================================================
|    C     PRV004        TAG
|    C* DETERMINA O ENVIO ANTERIOR DA PROVISÃO DO SINISTRO
|    C*
|    C                   MOVE      *BLANKS       WKPRV            10
|    C                   MOVE      *BLANKS       WKR1
|    C                   MOVE      'DT'          WKR1
|    C                   MOVEL     KEYPRV        WKPRV
|    C                   MOVE      WKR1          WKPRV
|    C     WKPRV         CHAIN     LPRVAUT                            99
|    C  N99              GOTO      ESRPRV
|    C*
|+---C                   IF        (VALDTRA > *ZEROS)
||   C*                                                            I
||   C* SE PROCESSO É IDS IGNORA REGISTO - MOVIMENTO INCORRECTO    I
||   C* NOTA: PROCESSOS IDS SÓ PODEM SER DE R.C.                   I
||   C*                                                            I
||+--C     IDS           IFEQ      'S'
|||  C                   GOTO      ESRPRV
||+--C                   END
||   C*                                                            I
||   C                   MOVE      KTE_DTR       LCDESP
||   C                   MOVE      '052'         LCOBPV
||   C                   MOVE      VALDTRA       LVLPRV
||   C     LVLPRV        COMP      0                                    22
||   C   22              Z-SUB     LVLPRV        LVLPRV
||   C                   EXCEPT    REGPRV
|+---C                   END
+----C     ESRPRV        ENDSR
     C/EJECT
     C*==========================================================
     C* Subrotina.: SRCIND
     C* Objectivo.: Formatar Pagamentos de Indemnizações
     C*==========================================================
+----C     SRCIND        BEGSR
|    C*
|    C* LIMPA CAMPOS DE TRABALHO
|    C*
|    C                   Z-ADD     *ZEROS        RI                1 0
|    C                   Z-ADD     *ZEROS        RIII              1 0
|    C                   Z-ADD     *ZEROS        RIVV              1 0
|    C                   Z-ADD     *ZEROS        WCOB1             2 0
|    C                   Z-ADD     *ZEROS        PCOB1             4 0
|    C                   Z-ADD     *ZEROS        WCOB3             2 0
|    C                   Z-ADD     *ZEROS        PCOB3             4 0
|    C                   Z-ADD     *ZEROS        WCOB5             2 0
|    C                   Z-ADD     *ZEROS        PCOB5             4 0
|    C*
|    C* FORMATA NÚMERO E ESPECIE DE RECIBO
|    C*
|    C                   MOVE      *BLANKS       LNRECS                         ( ATÉ NOVA REGRA )
|    C                   MOVE      '1'           LEREC
|    C*
|    C* =================================================================================
|    C*  RISCO I - ( DANOS MATERAIS ) Pagamentos de Indemnizações
|    C* =================================================================================
|    C*
|    C* DETERMINA O ENVIO ANTERIOR DO PAGAMENTO A SINISTROS
|    C*
|    C                   MOVE      *BLANKS       WKPAG            10
|    C                   MOVE      *BLANKS       WKR1
|    C                   MOVE      R1            WKR1
|    C                   MOVEL     'M'           WKR1
|    C                   MOVEL     KEYPAG        WKPAG
|    C                   MOVE      WKR1          WKPAG
|    C     WKPAG         CHAIN     LPAGAUT                            99
|    C  N99              GOTO      INDF01
|    C*
|+---C     DMATER        IFNE      *ZEROS
||   C*                                                            I
||   C*                                                            I
||   C* FORMATA CÓDIGO MOVIMENTO                                   I
||   C*                                                            I
||   C                   MOVE      '4'           LCMOV
||   C*                                                            I
||   C* FORMATA DATA DE MOVIMENTO DO PAGAMENTO  (DATA EMISSÃO)     I
||   C*                                                            I
||   C                   Z-ADD     *ZEROS        DTY2
||   C                   MOVE      DTMOV         DTY2
||   C                   EXSR      SRDTY2
||   C                   MOVE      DTY2          LDTEMI
||   C*                                                            I
||   C* INICIALIZA CAMPOS                                          I
||   C*                                                            I
||   C                   Z-ADD     *ZEROS        LDTANU
||   C                   MOVE      *BLANKS       LCANUL
||   C*                                                            I
||   C                   MOVE      1             RI
||   C                   MOVE      R1            WCOB1
||   C                   MOVEL     RI            WCOB1
||   C                   MOVEL     WCOB1         PCOB1
||   C                   MOVE      CAUSA         PCOB1
||   C*                                                            I
||   C* SE PROCESSO É IDS CONVERTE COBERTURAS ESPECIFICAS DE IDS   I
||   C* NOTA: PROCESSOS IDS = D SÓ PODEM TER PROVISÕES             I
||   C*                                                            I
||+--C     IDS           IFEQ      'S'
|||+-C     TIPIDS        IFEQ      'D'
|||| C                   GOTO      INDF01
|||+-C                   ELSE
||||+C     TIPIDS        IFEQ      'C'
|||||C                   MOVE      9             RI
|||||C                   MOVE      9             WCOB1
|||||C                   MOVEL     RI            WCOB1
|||||C                   MOVEL     WCOB1         PCOB1
|||||C                   MOVE      91            PCOB1
||||+C                   ELSE
||||+C                   END
|||+-C                   END
||+--C                   END
||   C*                                                            I
||   C* PESQUISA/CONVERTE A COBERTURA AFECTA PELA PROVISAO         I
||   C*                                                            I
||   C                   Z-ADD     1             I                 2 0
||   C     PCOB1         LOOKUP    COB1G(I)                               30
||   C   30              MOVE      COB1L(I)      LCOBPG
||+--C  N30              DO
|||  C                   MOVE      '00'          PCOB1
|||  C                   Z-ADD     1             I                 2 0
|||  C     PCOB1         LOOKUP    COB1G(I)                               30
|||  C   30              MOVE      COB1L(I)      LCOBPG
|||  C  N30              MOVE      *BLANKS       LCOBPG
||+--C                   END
||   C*                                                            I
||   C                   MOVE      DMATER        LTREC
||+--C     LTREC         IFLT      *ZEROS
|||  C                   MULT      -1            LTREC
|||  C                   MOVE      '2'           LCANUL
|||  C                   MOVE      LDTEMI        LDTANU
|||  C                   MOVE      *BLANKS       LDTEMI
|||  C                   MOVE      '5'           LCMOV
|||  C******               MOVE '   '     LCOBPG               I   I
||+--C                   END
||   C*                                                            I
||   C                   MOVE      LCOBPG        LCDESP
||   C     LTREC         COMP      0                                    23
||   C   23              Z-SUB     LTREC         LTREC
||   C                   EXCEPT    REGPAG
|+---C                   END
|    C*
|    C* =================================================================================
|    C*  RISCO I - ( DANOS FISICOS  ) Pagamentos de Indemnizações
|    C* =================================================================================
|    C     INDF01        TAG
|    C*
|    C* DETERMINA O ENVIO ANTERIOR DO PAGAMENTO A SINISTROS
|    C*
|    C                   MOVE      *BLANKS       WKPAG
|    C                   MOVE      *BLANKS       WKR1
|    C                   MOVE      R1            WKR1
|    C                   MOVEL     'F'           WKR1
|    C                   MOVEL     KEYPAG        WKPAG
|    C                   MOVE      WKR1          WKPAG
|    C     WKPAG         CHAIN     LPAGAUT                            99
|    C  N99              GOTO      IND001
|    C*
|+---C     DFISIC        IFNE      *ZEROS
||   C*                                                            I
||   C* SE PROCESSO É IDS IGNORA REGISTO MOVIMENTO DANOS FISICOS   I
||   C* NOTA: PROCESSOS IDS NÃO PODEM TER DANOS FISICOS            I
||   C*                                                            I
||+--C     IDS           IFEQ      'S'
|||  C                   GOTO      IND001
||+--C                   END
||   C*                                                            I
||   C* FORMATA CÓDIGO MOVIMENTO                                   I
||   C*                                                            I
||   C                   MOVE      '4'           LCMOV
||   C*                                                            I
||   C* FORMATA DATA DE MOVIMENTO DO PAGAMENTO  (DATA EMISSÃO)     I
||   C*                                                            I
||   C                   Z-ADD     *ZEROS        DTY2
||   C                   MOVE      DTMOV         DTY2
||   C                   EXSR      SRDTY2
||   C                   MOVE      DTY2          LDTEMI
||   C*                                                            I
||   C* INICIALIZA CAMPOS                                          I
||   C*                                                            I
||   C                   Z-ADD     *ZEROS        LDTANU
||   C                   MOVE      *BLANKS       LCANUL
||   C*                                                           I
||   C                   MOVE      9             RI
||   C                   MOVE      9             WCOB1
||   C                   MOVEL     RI            WCOB1
||   C                   MOVEL     WCOB1         PCOB1
||   C                   MOVE      '90'          PCOB1
||   C*                                                            I
||   C* PESQUISA/CONVERTE A COBERTURA AFECTA PELA PROVISAO         I
||   C*                                                            I
||   C                   Z-ADD     1             I                 2 0
||   C     PCOB1         LOOKUP    COB1G(I)                               30
||   C   30              MOVE      COB1L(I)      LCOBPG
||+--C  N30              DO
|||  C                   MOVE      '00'          PCOB1
|||  C                   Z-ADD     1             I                 2 0
|||  C     PCOB1         LOOKUP    COB1G(I)                               30
|||  C   30              MOVE      COB1L(I)      LCOBPG
|||  C  N30              MOVE      *BLANKS       LCOBPG
||+--C                   END
||   C*                                                            I
||   C                   MOVE      DFISIC        LTREC
||+--C     LTREC         IFLT      *ZEROS
|||  C                   MULT      -1            LTREC
|||  C                   MOVE      '2'           LCANUL
|||  C                   MOVE      LDTEMI        LDTANU
|||  C                   MOVE      *BLANKS       LDTEMI
|||  C                   MOVE      '5'           LCMOV
|||  C********             MOVE '   '     LCOBPG               I   I
||+--C                   END
||   C*                                                            I
||   C                   MOVE      LCOBPG        LCDESP
||   C     LTREC         COMP      0                                    23
||   C   23              Z-SUB     LTREC         LTREC
||   C                   EXCEPT    REGPAG
|+---C                   END
|    C*
|    C* =================================================================================
|    C*  RISCO III    Pagamentos de Indemnizações
|    C* =================================================================================
|    C     IND001        TAG
|    C*
|    C* DETERMINA O ENVIO ANTERIOR DO PAGAMENTO A SINISTROS
|    C*
|    C                   MOVE      *BLANKS       WKPAG
|    C                   MOVE      *BLANKS       WKR1
|    C                   MOVE      R2            WKR1
|    C                   MOVEL     KEYPAG        WKPAG
|    C                   MOVE      WKR1          WKPAG
|    C     WKPAG         CHAIN     LPAGAUT                            99
|    C  N99              GOTO      IND002
|    C*
|+---C     VALR3         IFNE      *ZEROS
||   C*                                                            I
||   C* SE PROCESSO É IDS IGNORA REGISTO - MOVIMENTO INCORRECTO    I
||   C* NOTA: PROCESSOS IDS SÓ PODEM SER DE R.C.                   I
||   C*                                                            I
||+--C     IDS           IFEQ      'S'
|||  C                   GOTO      IND002
||+--C                   END
||   C*                                                            I
||   C* FORMATA CÓDIGO MOVIMENTO                                   I
||   C*                                                            I
||   C                   MOVE      '4'           LCMOV
||   C*                                                            I
||   C* FORMATA DATA DE MOVIMENTO DO PAGAMENTO  (DATA EMISSÃO)     I
||   C*                                                            I
||   C                   Z-ADD     *ZEROS        DTY2
||   C                   MOVE      DTMOV         DTY2
||   C                   EXSR      SRDTY2
||   C                   MOVE      DTY2          LDTEMI
||   C*                                                            I
||   C* INICIALIZA CAMPOS                                          I
||   C*                                                            I
||   C                   Z-ADD     *ZEROS        LDTANU
||   C                   MOVE      *BLANKS       LCANUL
||   C*                                                            I
||   C                   MOVE      3             RIII              1 0
||   C                   MOVE      R2            WCOB3
||   C                   MOVEL     RIII          WCOB3
||   C                   MOVEL     WCOB3         PCOB3
||   C                   MOVE      CAUSA         PCOB3
||   C*                                                            I
||   C* PESQUISA/CONVERTE A COBERTURA AFECTA PELA PROVISAO         I
||   C*                                                            I
||   C                   Z-ADD     1             I                 2 0
||   C     PCOB3         LOOKUP    COB3G(I)                               30
||   C   30              MOVE      COB3L(I)      LCOBPG
||+--C  N30              DO
|||  C                   MOVE      '00'          PCOB3
|||  C                   Z-ADD     1             I                 2 0
|||  C     PCOB3         LOOKUP    COB3G(I)                               30
|||  C   30              MOVE      COB3L(I)      LCOBPG
|||  C  N30              MOVE      *BLANKS       LCOBPG
||+--C                   END
||   C*                                                            I
||   C                   MOVE      VALR3         LTREC
||+--C     LTREC         IFLT      *ZEROS
|||  C                   MULT      -1            LTREC
|||  C                   MOVE      '2'           LCANUL
|||  C                   MOVE      LDTEMI        LDTANU
|||  C                   MOVE      *BLANKS       LDTEMI
|||  C                   MOVE      '5'           LCMOV
|||  C*******              MOVE '   '     LCOBPG               I   I
||+--C                   END
||   C*                                                            I
||   C                   MOVE      LCOBPG        LCDESP
||   C     LTREC         COMP      0                                    23
||   C   23              Z-SUB     LTREC         LTREC
||   C                   EXCEPT    REGPAG
|+---C                   END
|    C*
|    C* =================================================================================
|    C*  RISCO IV + V    Pagamentos de Indemnizações
|    C* =================================================================================
|    C     IND002        TAG
|    C*
|    C* DETERMINA O ENVIO ANTERIOR DO PAGAMENTO A SINISTROS
|    C*
|    C                   MOVE      *BLANKS       WKPAG
|    C                   MOVE      *BLANKS       WKR1
|    C                   MOVE      R2            WKR1
|    C                   MOVEL     KEYPAG        WKPAG
|    C                   MOVE      WKR1          WKPAG
|    C     WKPAG         CHAIN     LPAGAUT                            99
|    C  N99              GOTO      IND003
|    C*
|+---C     VALR45        IFNE      *ZEROS
||   C*                                                            I
||   C* SE PROCESSO É IDS IGNORA REGISTO - MOVIMENTO INCORRECTO    I
||   C* NOTA: PROCESSOS IDS SÓ PODEM SER DE R.C.                   I
||   C*                                                            I
||+--C     IDS           IFEQ      'S'
|||  C                   GOTO      IND003
||+--C                   END
||   C*                                                            I
||   C* FORMATA CÓDIGO MOVIMENTO                                   I
||   C*                                                            I
||   C                   MOVE      '4'           LCMOV
||   C*                                                            I
||   C* FORMATA DATA DE MOVIMENTO DO PAGAMENTO  (DATA EMISSÃO)     I
||   C*                                                            I
||   C                   Z-ADD     *ZEROS        DTY2
||   C                   MOVE      DTMOV         DTY2
||   C                   EXSR      SRDTY2
||   C                   MOVE      DTY2          LDTEMI
||   C*                                                            I
||   C* INICIALIZA CAMPOS                                          I
||   C*                                                            I
||   C                   Z-ADD     *ZEROS        LDTANU
||   C                   MOVE      *BLANKS       LCANUL
||   C*                                                            I
||   C                   MOVE      5             RIVV              1 0
||   C                   MOVE      R2            WCOB5
||   C                   MOVEL     RIVV          WCOB5
||   C                   MOVEL     WCOB5         PCOB5
||   C                   MOVE      CAUSA         PCOB5
||   C*                                                            I
||   C* PESQUISA/CONVERTE A COBERTURA AFECTA PELA PROVISAO         I
||   C*                                                            I
||   C                   Z-ADD     1             I                 2 0
||   C     PCOB5         LOOKUP    COB5G(I)                               30
||   C   30              MOVE      COB5L(I)      LCOBPG
||+--C  N30              DO
|||  C                   MOVE      '00'          PCOB5
|||  C                   Z-ADD     1             I                 2 0
|||  C     PCOB5         LOOKUP    COB5G(I)                               30
|||  C   30              MOVE      COB5L(I)      LCOBPG
|||  C  N30              MOVE      *BLANKS       LCOBPG
||+--C                   END
||   C*                                                            I
||   C                   MOVE      VALR45        LTREC
||+--C     LTREC         IFLT      *ZEROS
|||  C                   MULT      -1            LTREC
|||  C                   MOVE      '2'           LCANUL
|||  C                   MOVE      LDTEMI        LDTANU
|||  C                   MOVE      *BLANKS       LDTEMI
|||  C                   MOVE      '5'           LCMOV
|||  C********             MOVE '   '     LCOBPG               I   I
||+--C                   END
||   C*                                                            I
||   C                   MOVE      LCOBPG        LCDESP
||   C     LTREC         COMP      0                                    23
||   C   23              Z-SUB     LTREC         LTREC
||   C                   EXCEPT    REGPAG
|+---C                   END
|    C*
|    C* =================================================================================
|    C*  Cobertura MIP - Pagamentos de Indemnizações
|    C* =================================================================================
|    C     IND003        TAG
|    C                   IF        (DTFIM > *ZEROS) AND (DTFIM < 130101)
|    C                   GOTO      ESRIND
|    C                   ENDIF
|    C*
|    C* DETERMINA O ENVIO ANTERIOR DO PAGAMENTO A SINISTROS
|    C*
|    C                   MOVE      *BLANKS       WKPAG
|    C                   MOVE      *BLANKS       WKR1
|    C                   MOVE      'M4'          WKR1
|    C                   MOVEL     KEYPAG        WKPAG
|    C                   MOVE      WKR1          WKPAG
|    C     WKPAG         CHAIN     LPAGAUT                            99
|    C  N99              GOTO      IND004
|    C*
|+---C     VALMIP        IFNE      *ZEROS
||   C*                                                            I
||   C* SE PROCESSO É IDS IGNORA REGISTO - MOVIMENTO INCORRECTO    I
||   C* NOTA: PROCESSOS IDS SÓ PODEM SER DE R.C.                   I
||   C*                                                            I
||+--C     IDS           IFEQ      'S'
|||  C                   GOTO      IND004
||+--C                   END
||   C*                                                            I
||   C* FORMATA CÓDIGO MOVIMENTO                                   I
||   C*                                                            I
||   C                   MOVE      '4'           LCMOV
||   C*                                                            I
||   C* FORMATA DATA DE MOVIMENTO DO PAGAMENTO  (DATA EMISSÃO)     I
||   C*                                                            I
||   C                   Z-ADD     *ZEROS        DTY2
||   C                   MOVE      DTMOV         DTY2
||   C                   EXSR      SRDTY2
||   C                   MOVE      DTY2          LDTEMI
||   C*                                                            I
||   C* INICIALIZA CAMPOS                                          I
||   C*                                                            I
||   C                   Z-ADD     *ZEROS        LDTANU
||   C                   MOVE      *BLANKS       LCANUL
||   C                   MOVE      VALMIP        LTREC
||+--C     LTREC         IFLT      *ZEROS
|||  C                   MULT      -1            LTREC
|||  C                   MOVE      '2'           LCANUL
|||  C                   MOVE      LDTEMI        LDTANU
|||  C                   MOVE      *BLANKS       LDTEMI
|||  C                   MOVE      '5'           LCMOV
||+--C                   END
||   C*                                                            I
||   C                   MOVE      KTE_MIP       LCDESP
||   C                   MOVE      '052'         LCDESP
||   C     LTREC         COMP      0                                    23
||   C   23              Z-SUB     LTREC         LTREC
||   C                   EXCEPT    REGPAG
|+---C                   END
|    C*
|    C* =================================================================================
|    C*  Cobertura VALDTR - Pagamentos de Indemnizações
|    C* =================================================================================
|    C     IND004        TAG
|    C*
|    C* DETERMINA O ENVIO ANTERIOR DO PAGAMENTO A SINISTROS
|    C*
|    C                   MOVE      *BLANKS       WKPAG
|    C                   MOVE      *BLANKS       WKR1
|    C                   MOVE      'D4'          WKR1
|    C                   MOVEL     KEYPAG        WKPAG
|    C                   MOVE      WKR1          WKPAG
|    C     WKPAG         CHAIN     LPAGAUT                            99
|    C  N99              GOTO      ESRIND
|    C*
|+---C     VALDTRA       IFNE      *ZEROS
||   C*                                                            I
||   C* SE PROCESSO É IDS IGNORA REGISTO - MOVIMENTO INCORRECTO    I
||   C* NOTA: PROCESSOS IDS SÓ PODEM SER DE R.C.                   I
||   C*                                                            I
||+--C     IDS           IFEQ      'S'
|||  C                   GOTO      ESRIND
||+--C                   END
||   C*                                                            I
||   C* FORMATA CÓDIGO MOVIMENTO                                   I
||   C*                                                            I
||   C                   MOVE      '4'           LCMOV
||   C*                                                            I
||   C* FORMATA DATA DE MOVIMENTO DO PAGAMENTO  (DATA EMISSÃO)     I
||   C*                                                            I
||   C                   Z-ADD     *ZEROS        DTY2
||   C                   MOVE      DTMOV         DTY2
||   C                   EXSR      SRDTY2
||   C                   MOVE      DTY2          LDTEMI
||   C*                                                            I
||   C* INICIALIZA CAMPOS                                          I
||   C*                                                            I
||   C                   Z-ADD     *ZEROS        LDTANU
||   C                   MOVE      *BLANKS       LCANUL
||   C                   MOVE      VALDTRA       LTREC
||+--C     LTREC         IFLT      *ZEROS
|||  C                   MULT      -1            LTREC
|||  C                   MOVE      '2'           LCANUL
|||  C                   MOVE      LDTEMI        LDTANU
|||  C                   MOVE      *BLANKS       LDTEMI
|||  C                   MOVE      '5'           LCMOV
||+--C                   END
||   C*                                                            I
||   C                   MOVE      KTE_DTR       LCDESP
||   C                   MOVE      '052'         LCOBPG
||   C     LTREC         COMP      0                                    23
||   C   23              Z-SUB     LTREC         LTREC
||   C                   EXCEPT    REGPAG
|+---C                   END
+----C     ESRIND        ENDSR
     C/EJECT
     C*==========================================================
     C* Subrotina.: SRFPAG
     C* Objectivo.: Formatar Pagamentos de Despesas
     C*==========================================================
+----C     SRFPAG        BEGSR
|    C*
|    C* FORMATA NÚMERO E ESPECIE DE RECIBO
|    C*
|    C                   MOVE      *BLANKS       LNRECS                         ( ATÉ NOVA REGRA )
|    C                   MOVE      '1'           LEREC
|    C*
|    C* =====================================================================================
|    C*  RISCO I      Pagamentos de Despesas
|    C* =====================================================================================
|    C*
|    C* DETERMINA O ENVIO ANTERIOR DO PAGAMENTO A SINISTROS
|    C*
|    C                   MOVE      *BLANKS       WKPAG
|    C                   MOVE      *BLANKS       WKR1
|    C                   MOVE      R1            WKR1
|    C                   MOVEL     KEYPAG        WKPAG
|    C                   MOVE      WKR1          WKPAG
|    C     WKPAG         CHAIN     LPAGAUT                            99
|    C  N99              GOTO      PAG001
|    C*
|+---C     VALR1         IFNE      *ZEROS
||   C*                                                            I
||   C* FORMATA CÓDIGO MOVIMENTO                                   I
||   C*                                                            I
||   C                   MOVE      '4'           LCMOV
||   C*                                                            I
||   C* FORMATA DATA DE MOVIMENTO DO PAGAMENTO  (DATA EMISSÃO)     I
||   C*                                                            I
||   C                   Z-ADD     *ZEROS        DTY2
||   C                   MOVE      DTMOV         DTY2
||   C                   EXSR      SRDTY2
||   C                   MOVE      DTY2          LDTEMI
||   C*                                                            I
||   C* INICIALIZA CAMPOS                                          I
||   C*                                                            I
||   C                   Z-ADD     *ZEROS        LDTANU
||   C                   MOVE      *BLANKS       LCANUL
||   C*                                                            I
||   C                   MOVE      *ZEROS        WRMSP1            9 0
||   C                   MOVEL     LRAMO         WRMSP1
||   C                   MOVEL     LMOD          WRMSP2            4 0
||   C                   MOVE      LSMOD         WRMSP2
||   C                   MOVE      TIPMOV        WRMSP3            7 0
||   C                   MOVEL     WRMSP2        WRMSP3
||   C                   MOVE      WRMSP3        WRMSP1
||   C*                                                            I
||   C* PESQUISA/CONVERTE A COBERTURA/DESPESA AFECTA AO PAGAM.     I
||   C*                                                            I
||   C                   Z-ADD     1             I                 2 0
||   C     WRMSP1        LOOKUP    TPAGG(I)                               30
||   C   30              MOVE      COBDL(I)      LCOBPG
||+--C  N30              DO
|||  C*                                                            I
|||  C* FORÇA TIPO MOV = 000                                       I
|||  C                   MOVEL     LRAMO         WRMSP1
|||  C                   MOVEL     LMOD          WRMSP2
|||  C                   MOVE      LSMOD         WRMSP2
|||  C                   MOVE      000           WRMSP3
|||  C                   MOVEL     WRMSP2        WRMSP3
|||  C                   MOVE      WRMSP3        WRMSP1
|||  C*                                                            I
|||  C                   Z-ADD     1             I                 2 0
|||  C     WRMSP1        LOOKUP    TPAGG(I)                               35
|||  C   35              MOVE      COBDL(I)      LCOBPG
|||  C  N35              MOVE      *BLANKS       LCOBPG
||+--C                   END
||   C*                                                            I
||   C                   MOVE      VALR1         LTREC
||+--C     LTREC         IFLT      *ZEROS
|||  C                   MULT      -1            LTREC
|||  C                   MOVE      '2'           LCANUL
|||  C                   MOVE      LDTEMI        LDTANU
|||  C                   MOVE      *BLANKS       LDTEMI
|||  C                   MOVE      '5'           LCMOV
|||  C********             MOVE '   '     LCOBPG                   I
||+--C                   END
||   C*                                                            I
||   C                   MOVE      LCOBPG        LCDESP
||   C     LTREC         COMP      0                                    23
||   C   23              Z-SUB     LTREC         LTREC
||   C                   EXCEPT    REGPAG
|+---C                   END
|    C*
|    C* =================================================================================
|    C*  RISCO III     Pagamentos de Despesas
|    C* =================================================================================
|    C     PAG001        TAG
|    C*
|    C* DETERMINA O ENVIO ANTERIOR DO PAGAMENTO A SINISTROS
|    C*
|    C                   MOVE      *BLANKS       WKPAG
|    C                   MOVE      *BLANKS       WKR1
|    C                   MOVE      R2            WKR1
|    C                   MOVEL     KEYPAG        WKPAG
|    C                   MOVE      WKR1          WKPAG
|    C     WKPAG         CHAIN     LPAGAUT                            99
|    C  N99              GOTO      PAG002
|    C*
|+---C     VALR3         IFNE      *ZEROS
||   C*                                                            I
||   C* FORMATA CÓDIGO MOVIMENTO                                   I
||   C*                                                            I
||   C                   MOVE      '4'           LCMOV
||   C*                                                            I
||   C* FORMATA DATA DE MOVIMENTO DO PAGAMENTO  (DATA EMISSÃO)     I
||   C*                                                            I
||   C                   Z-ADD     *ZEROS        DTY2
||   C                   MOVE      DTMOV         DTY2
||   C                   EXSR      SRDTY2
||   C                   MOVE      DTY2          LDTEMI
||   C*                                                            I
||   C* INICIALIZA CAMPOS                                          I
||   C*                                                            I
||   C                   Z-ADD     *ZEROS        LDTANU
||   C                   MOVE      *BLANKS       LCANUL
||   C*                                                            I
||   C                   MOVE      *ZEROS        WRMSP1
||   C                   MOVEL     LRAMO         WRMSP1
||   C                   MOVEL     LMOD          WRMSP2
||   C                   MOVE      LSMOD         WRMSP2
||   C                   MOVE      TIPMOV        WRMSP3
||   C                   MOVEL     WRMSP2        WRMSP3
||   C                   MOVE      WRMSP3        WRMSP1
||   C*                                                            I
||   C* PESQUISA/CONVERTE A COBERTURA/DESPESA AFECTA AO PAGAM.     I
||   C*                                                            I
||   C                   Z-ADD     1             I                 2 0
||   C     WRMSP1        LOOKUP    TPAGG(I)                               30
||   C   30              MOVE      COBDL(I)      LCOBPG
||+--C  N30              DO
|||  C*                                                            I
|||  C* FORÇA TIPO MOV = 000                                       I
|||  C                   MOVEL     LRAMO         WRMSP1
|||  C                   MOVEL     LMOD          WRMSP2
|||  C                   MOVE      LSMOD         WRMSP2
|||  C                   MOVE      000           WRMSP3
|||  C                   MOVEL     WRMSP2        WRMSP3
|||  C                   MOVE      WRMSP3        WRMSP1
|||  C*                                                            I
|||  C                   Z-ADD     1             I                 2 0
|||  C     WRMSP1        LOOKUP    TPAGG(I)                               35
|||  C   35              MOVE      COBDL(I)      LCOBPG
|||  C  N35              MOVE      *BLANKS       LCOBPG
||+--C                   END
||   C*                                                            I
||   C                   MOVE      VALR3         LTREC
||+--C     LTREC         IFLT      *ZEROS
|||  C                   MULT      -1            LTREC
|||  C                   MOVE      '2'           LCANUL
|||  C                   MOVE      LDTEMI        LDTANU
|||  C                   MOVE      *BLANKS       LDTEMI
|||  C                   MOVE      '5'           LCMOV
|||  C********             MOVE '   '     LCOBPG                   I
||+--C                   END
||   C*                                                            I
||   C                   MOVE      LCOBPG        LCDESP
||   C     LTREC         COMP      0                                    23
||   C   23              Z-SUB     LTREC         LTREC
||   C                   EXCEPT    REGPAG
|+---C                   END
|    C*
|    C*
|    C* =================================================================================
|    C*  RISCO IV + V    Pagamentos de Despesas
|    C* =================================================================================
|    C     PAG002        TAG
|    C*
|    C* DETERMINA O ENVIO ANTERIOR DO PAGAMENTO A SINISTROS
|    C*
|    C                   MOVE      *BLANKS       WKPAG
|    C                   MOVE      *BLANKS       WKR1
|    C                   MOVE      R2            WKR1
|    C                   MOVEL     KEYPAG        WKPAG
|    C                   MOVE      WKR1          WKPAG
|    C     WKPAG         CHAIN     LPAGAUT                            99
|    C  N99              GOTO      PAG003
|    C*
|+---C     VALR45        IFNE      *ZEROS
||   C*                                                            I
||   C* FORMATA CÓDIGO MOVIMENTO                                   I
||   C*                                                            I
||   C                   MOVE      '4'           LCMOV
||   C*                                                            I
||   C* FORMATA DATA DE MOVIMENTO DO PAGAMENTO  (DATA EMISSÃO)     I
||   C*                                                            I
||   C                   Z-ADD     *ZEROS        DTY2
||   C                   MOVE      DTMOV         DTY2
||   C                   EXSR      SRDTY2
||   C                   MOVE      DTY2          LDTEMI
||   C*                                                            I
||   C* INICIALIZA CAMPOS                                          I
||   C*                                                            I
||   C                   Z-ADD     *ZEROS        LDTANU
||   C                   MOVE      *BLANKS       LCANUL
||   C*                                                            I
||   C                   MOVE      *ZEROS        WRMSP1
||   C                   MOVEL     LRAMO         WRMSP1
||   C                   MOVEL     LMOD          WRMSP2
||   C                   MOVE      LSMOD         WRMSP2
||   C                   MOVE      TIPMOV        WRMSP3
||   C                   MOVEL     WRMSP2        WRMSP3
||   C                   MOVE      WRMSP3        WRMSP1
||   C*                                                            I
||   C* PESQUISA/CONVERTE A COBERTURA/DESPESA AFECTA AO PAGAM.     I
||   C*                                                            I
||   C                   Z-ADD     1             I                 2 0
||   C     WRMSP1        LOOKUP    TPAGG(I)                               30
||   C   30              MOVE      COBDL(I)      LCOBPG
||+--C  N30              DO
|||  C*                                                            I
|||  C* FORÇA TIPO MOV = 000                                       I
|||  C                   MOVEL     LRAMO         WRMSP1
|||  C                   MOVEL     LMOD          WRMSP2
|||  C                   MOVE      LSMOD         WRMSP2
|||  C                   MOVE      000           WRMSP3
|||  C                   MOVEL     WRMSP2        WRMSP3
|||  C                   MOVE      WRMSP3        WRMSP1
|||  C*                                                            I
|||  C                   Z-ADD     1             I                 2 0
|||  C     WRMSP1        LOOKUP    TPAGG(I)                               35
|||  C   35              MOVE      COBDL(I)      LCOBPG
|||  C  N35              MOVE      *BLANKS       LCOBPG
||+--C                   END
||   C*                                                            I
||   C                   MOVE      VALR45        LTREC
||+--C     LTREC         IFLT      *ZEROS
|||  C                   MULT      -1            LTREC
|||  C                   MOVE      '2'           LCANUL
|||  C                   MOVE      LDTEMI        LDTANU
|||  C                   MOVE      *BLANKS       LDTEMI
|||  C                   MOVE      '5'           LCMOV
||+--C                   END
||   C*                                                            I
||   C                   MOVE      LCOBPG        LCDESP
||   C     LTREC         COMP      0                                    23
||   C   23              Z-SUB     LTREC         LTREC
||   C                   EXCEPT    REGPAG
|+---C                   END
|    C* =================================================================================
|    C*  COBERTURA MIP - Pagamentos de Despesas
|    C* =================================================================================
|    C     PAG003        TAG
     C                   IF        (DTFIM > *ZEROS) AND (DTFIM < 130101)
     C                   GOTO      ESRPAG
     C                   ENDIF
|    C*
|    C* DETERMINA O ENVIO ANTERIOR DO PAGAMENTO A SINISTROS
|    C*
|    C                   MOVE      *BLANKS       WKPAG
|    C                   MOVE      *BLANKS       WKR1
|    C                   MOVE      'M4'          WKR1
|    C                   MOVEL     KEYPAG        WKPAG
|    C                   MOVE      WKR1          WKPAG
|    C     WKPAG         CHAIN     LPAGAUT                            99
|    C  N99              GOTO      PAG004
|    C*
|+---C     VALMIP        IFNE      *ZEROS
||   C*                                                            I
||   C* FORMATA CÓDIGO MOVIMENTO                                   I
||   C*                                                            I
||   C                   MOVE      '4'           LCMOV
||   C*                                                            I
||   C* FORMATA DATA DE MOVIMENTO DO PAGAMENTO  (DATA EMISSÃO)     I
||   C*                                                            I
||   C                   Z-ADD     *ZEROS        DTY2
||   C                   MOVE      DTMOV         DTY2
||   C                   EXSR      SRDTY2
||   C                   MOVE      DTY2          LDTEMI
||   C*                                                            I
||   C* INICIALIZA CAMPOS                                          I
||   C*                                                            I
||   C                   Z-ADD     *ZEROS        LDTANU
||   C                   MOVE      *BLANKS       LCANUL
||   C                   MOVE      VALMIP        LTREC
||+--C     LTREC         IFLT      *ZEROS
|||  C                   MULT      -1            LTREC
|||  C                   MOVE      '2'           LCANUL
|||  C                   MOVE      LDTEMI        LDTANU
|||  C                   MOVE      *BLANKS       LDTEMI
|||  C                   MOVE      '5'           LCMOV
||+--C                   END
||   C*                                                            I
||   C                   MOVE      '052'         LCOBPG
||   C                   MOVE      KTE_MIP       LCDESP
||   C     LTREC         COMP      0                                    23
||   C   23              Z-SUB     LTREC         LTREC
||   C                   EXCEPT    REGPAG
|+---C                   END
|    C* =================================================================================
|    C*  COBERTURA DTR - Pagamentos de Despesas
|    C* =================================================================================
|    C     PAG004        TAG
|    C*
|    C* DETERMINA O ENVIO ANTERIOR DO PAGAMENTO A SINISTROS
|    C*
|    C                   MOVE      *BLANKS       WKPAG
|    C                   MOVE      *BLANKS       WKR1
|    C                   MOVE      'D4'          WKR1
|    C                   MOVEL     KEYPAG        WKPAG
|    C                   MOVE      WKR1          WKPAG
|    C     WKPAG         CHAIN     LPAGAUT                            99
|    C  N99              GOTO      ESRPAG
|    C*
|+---C     VALDTRA       IFNE      *ZEROS
||   C*                                                            I
||   C* FORMATA CÓDIGO MOVIMENTO                                   I
||   C*                                                            I
||   C                   MOVE      '4'           LCMOV
||   C*                                                            I
||   C* FORMATA DATA DE MOVIMENTO DO PAGAMENTO  (DATA EMISSÃO)     I
||   C*                                                            I
||   C                   Z-ADD     *ZEROS        DTY2
||   C                   MOVE      DTMOV         DTY2
||   C                   EXSR      SRDTY2
||   C                   MOVE      DTY2          LDTEMI
||   C*                                                            I
||   C* INICIALIZA CAMPOS                                          I
||   C*                                                            I
||   C                   Z-ADD     *ZEROS        LDTANU
||   C                   MOVE      *BLANKS       LCANUL
||   C                   MOVE      VALDTRA       LTREC
||+--C     LTREC         IFLT      *ZEROS
|||  C                   MULT      -1            LTREC
|||  C                   MOVE      '2'           LCANUL
|||  C                   MOVE      LDTEMI        LDTANU
|||  C                   MOVE      *BLANKS       LDTEMI
|||  C                   MOVE      '5'           LCMOV
||+--C                   END
||   C*                                                            I
||   C                   MOVE      '052'         LCOBPG
||   C                   MOVE      KTE_DTR       LCDESP
||   C     LTREC         COMP      0                                    23
||   C   23              Z-SUB     LTREC         LTREC
||   C                   EXCEPT    REGPAG
|+---C                   END
+----C     ESRPAG        ENDSR
     C/EJECT
     C*==========================================================
     C* Subrotina.: SRFREE
     C* Objectivo.: Formatar Reembolsos
     C*==========================================================
+----C     SRFREE        BEGSR
|    C*
|    C* LIMPA CAMPOS DE TRABALHO
|    C*
|    C                   Z-ADD     *ZEROS        RI                1 0
|    C                   Z-ADD     *ZEROS        RIII              1 0
|    C                   Z-ADD     *ZEROS        RIVV              1 0
|    C                   Z-ADD     *ZEROS        WCOB1             2 0
|    C                   Z-ADD     *ZEROS        PCOB1             4 0
|    C                   Z-ADD     *ZEROS        WCOB3             2 0
|    C                   Z-ADD     *ZEROS        PCOB3             4 0
|    C                   Z-ADD     *ZEROS        WCOB5             2 0
|    C                   Z-ADD     *ZEROS        PCOB5             4 0
|    C*
|    C                   MOVE      *BLANKS       LNRECS                         ATÉ NOVA REGRA
|    C*
|    C     TIPMOV        IFEQ      220
|    C*                    MOVE '8'       LEREC            SALVADOS (A PEDIDO)
|    C                   MOVE      '9'           LEREC                          REEMBOLSOS
|    C                   ELSE
|    C                   MOVE      '9'           LEREC                          REEMBOLSOS
|    C                   END
|    C*
|    C* =================================================================================
|    C*  RISCO I - ( DANOS MATERIAIS )   Reembolsos
|    C* =================================================================================
|    C*
|    C* DETERMINA O ENVIO ANTERIOR DA PROVISÃO DO SINISTRO
|    C*
|    C                   MOVE      *BLANKS       WKPAG
|    C                   MOVE      *BLANKS       WKR1
|    C                   MOVE      R1            WKR1
|    C                   MOVEL     'M'           WKR1
|    C                   MOVEL     KEYPAG        WKPAG
|    C                   MOVE      WKR1          WKPAG
|    C     WKPAG         CHAIN     LPAGAUT                            99
|    C  N99              GOTO      REEF01
|    C*
|+---C     DMATER        IFNE      *ZEROS
||   C*                                                             I
||   C* FORMATA CÓDIGO MOVIMENTO                                    I
||   C*                                                             I
||   C                   MOVE      '4'           LCMOV
||   C*                                                             I
||   C* FORMATA DATA DE MOVIMENTO DO PAGAMENTO  (DATA EMISSÃO)      I
||   C*                                                             I
||   C                   Z-ADD     *ZEROS        DTY2
||   C                   MOVE      DTMOV         DTY2
||   C                   EXSR      SRDTY2
||   C                   MOVE      DTY2          LDTEMI
||   C*                                                             I
||   C* INICIALIZA CAMPOS                                           I
||   C*                                                             I
||   C                   Z-ADD     *ZEROS        LDTANU
||   C                   MOVE      *BLANKS       LCANUL
||   C*                                                             I
||   C                   MOVE      1             RI
||   C                   MOVE      R1            WCOB1
||   C                   MOVEL     RI            WCOB1
||   C                   MOVEL     WCOB1         PCOB1
||   C                   MOVE      CAUSA         PCOB1
||   C*                                                             I
||   C* SE PROCESSO É IDS CONVERTE COBERTURAS ESPECIFICAS DE IDS    I
||   C* NOTA: PROCESSOS IDS = D SÓ PODEM TER PROVISÕES              I
||   C*                                                             I
||+--C     IDS           IFEQ      'S'
|||+-C     TIPIDS        IFEQ      'D'
|||| C                   GOTO      REEF01
|||+-C                   ELSE
||||+C     TIPIDS        IFEQ      'C'
|||||C                   MOVE      9             RI
|||||C                   MOVE      9             WCOB1
|||||C                   MOVEL     RI            WCOB1
|||||C                   MOVEL     WCOB1         PCOB1
|||||C                   MOVE      91            PCOB1
||||+C                   ELSE
||||+C                   END
|||+-C                   END
||+--C                   END
||   C*                                                             I
||   C* PESQUISA/CONVERTE A COBERTURA AFECTA PELA PROVISAO          I
||   C*                                                             I
||   C                   Z-ADD     1             I                 2 0
||   C     PCOB1         LOOKUP    COB1G(I)                               30
||   C   30              MOVE      COB1L(I)      LCOBPG
||+--C  N30              DO
|||  C                   MOVE      '00'          PCOB1
|||  C                   Z-ADD     1             I                 2 0
|||  C     PCOB1         LOOKUP    COB1G(I)                               30
|||  C   30              MOVE      COB1L(I)      LCOBPG
|||  C  N30              MOVE      *BLANKS       LCOBPG
||+--C                   END
||   C*                                                             I
||   C                   MOVE      DMATER        LTREC
||+--C     LTREC         IFLT      *ZEROS
|||  C                   MULT      -1            LTREC
|||  C                   MOVE      '2'           LCANUL
|||  C                   MOVE      LDTEMI        LDTANU
|||  C                   MOVE      *BLANKS       LDTEMI
|||  C                   MOVE      '5'           LCMOV
|||  C*********            MOVE '   '     LCOBPG                    I
||+--C                   END
||   C*                                                             I
||   C                   MOVE      LCOBPG        LCDESP
||   C     LTREC         COMP      0                                    23
||   C   23              Z-SUB     LTREC         LTREC
||   C                   EXCEPT    REGPAG
|+---C                   END
|    C*
|    C* =================================================================================
|    C*  RISCO I - ( DANOS FISICOS )  Reembolsos
|    C* =================================================================================
|    C     REEF01        TAG
|    C*
|    C* DETERMINA O ENVIO ANTERIOR DA PROVISÃO DO SINISTRO
|    C*
|    C                   MOVE      *BLANKS       WKPAG
|    C                   MOVE      *BLANKS       WKR1
|    C                   MOVE      R1            WKR1
|    C                   MOVEL     'F'           WKR1
|    C                   MOVEL     KEYPAG        WKPAG
|    C                   MOVE      WKR1          WKPAG
|    C     WKPAG         CHAIN     LPAGAUT                            99
|    C  N99              GOTO      REE001
|    C*
|+---C     DFISIC        IFNE      *ZEROS
||   C*                                                             I
||   C* SE PROCESSO É IDS IGNORA REGISTO MOVIMENTO DANOS FISICOS    I
||   C* NOTA: PROCESSOS IDS NÃO PODEM TER DANOS FISICOS             I
||   C*                                                             I
||+--C     IDS           IFEQ      'S'
|||  C                   GOTO      REE001
||+--C                   END
||   C*                                                             I
||   C* FORMATA CÓDIGO MOVIMENTO                                    I
||   C*                                                             I
||   C                   MOVE      '4'           LCMOV
||   C*                                                             I
||   C* FORMATA DATA DE MOVIMENTO DO PAGAMENTO  (DATA EMISSÃO)      I
||   C*                                                             I
||   C                   Z-ADD     *ZEROS        DTY2
||   C                   MOVE      DTMOV         DTY2
||   C                   EXSR      SRDTY2
||   C                   MOVE      DTY2          LDTEMI
||   C*                                                             I
||   C* INICIALIZA CAMPOS                                           I
||   C*                                                             I
||   C                   Z-ADD     *ZEROS        LDTANU
||   C                   MOVE      *BLANKS       LCANUL
||   C*                                                           I
||   C                   MOVE      9             RI
||   C                   MOVE      9             WCOB1
||   C                   MOVEL     RI            WCOB1
||   C                   MOVEL     WCOB1         PCOB1
||   C                   MOVE      '90'          PCOB1
||   C*                                                             I
||   C* PESQUISA/CONVERTE A COBERTURA AFECTA PELA PROVISAO          I
||   C*                                                             I
||   C                   Z-ADD     1             I                 2 0
||   C     PCOB1         LOOKUP    COB1G(I)                               30
||   C   30              MOVE      COB1L(I)      LCOBPG
||+--C  N30              DO
|||  C                   MOVE      '00'          PCOB1
|||  C                   Z-ADD     1             I                 2 0
|||  C     PCOB1         LOOKUP    COB1G(I)                               30
|||  C   30              MOVE      COB1L(I)      LCOBPG
|||  C  N30              MOVE      *BLANKS       LCOBPG
||+--C                   END
||   C*                                                             I
||   C                   MOVE      DFISIC        LTREC
||+--C     LTREC         IFLT      *ZEROS
|||  C                   MULT      -1            LTREC
|||  C                   MOVE      '2'           LCANUL
|||  C                   MOVE      LDTEMI        LDTANU
|||  C                   MOVE      *BLANKS       LDTEMI
|||  C                   MOVE      '5'           LCMOV
|||  C******               MOVE '   '     LCOBPG                    I
||+--C                   END
||   C*                                                             I
||   C                   MOVE      LCOBPG        LCDESP
||   C     LTREC         COMP      0                                    23
||   C   23              Z-SUB     LTREC         LTREC
||   C                   EXCEPT    REGPAG
|+---C                   END
|    C*
|    C* =================================================================================
|    C*  RISCO III           Reembolsos
|    C* =================================================================================
|    C     REE001        TAG
|    C*
|    C* DETERMINA O ENVIO ANTERIOR DO PAGAMENTO A SINISTROS
|    C*
|    C                   MOVE      *BLANKS       WKPAG
|    C                   MOVE      *BLANKS       WKR1
|    C                   MOVE      R2            WKR1
|    C                   MOVEL     KEYPAG        WKPAG
|    C                   MOVE      WKR1          WKPAG
|    C     WKPAG         CHAIN     LPAGAUT                            99
|    C  N99              GOTO      REE002
|    C*
|+---C     VALR3         IFNE      *ZEROS
||   C*                                                            I
||   C* SE PROCESSO É IDS IGNORA REGISTO - MOVIMENTO INCORRECTO    I
||   C* NOTA: PROCESSOS IDS SÓ PODEM SER DE R.C.                   I
||   C*                                                            I
||+--C     IDS           IFEQ      'S'
|||  C                   GOTO      REE002
||+--C                   END
||   C*                                                            I
||   C* FORMATA CÓDIGO MOVIMENTO                                   I
||   C*                                                            I
||   C                   MOVE      '4'           LCMOV
||   C*                                                            I
||   C* FORMATA DATA DE MOVIMENTO DO PAGAMENTO  (DATA EMISSÃO)     I
||   C*                                                            I
||   C                   Z-ADD     *ZEROS        DTY2
||   C                   MOVE      DTMOV         DTY2
||   C                   EXSR      SRDTY2
||   C                   MOVE      DTY2          LDTEMI
||   C*                                                            I
||   C* INICIALIZA CAMPOS                                          I
||   C*                                                            I
||   C                   Z-ADD     *ZEROS        LDTANU
||   C                   MOVE      *BLANKS       LCANUL
||   C*                                                            I
||   C                   MOVE      3             RIII              1 0
||   C                   MOVE      R2            WCOB3
||   C                   MOVEL     RIII          WCOB3
||   C                   MOVEL     WCOB3         PCOB3
||   C                   MOVE      CAUSA         PCOB3
||   C*                                                            I
||   C* PESQUISA/CONVERTE A COBERTURA AFECTA PELA PROVISAO         I
||   C*                                                            I
||   C                   Z-ADD     1             I                 2 0
||   C     PCOB3         LOOKUP    COB3G(I)                               30
||   C   30              MOVE      COB3L(I)      LCOBPG
||+--C  N30              DO
|||  C                   MOVE      '00'          PCOB3
|||  C                   Z-ADD     1             I                 2 0
|||  C     PCOB3         LOOKUP    COB3G(I)                               30
|||  C   30              MOVE      COB3L(I)      LCOBPG
|||  C  N30              MOVE      *BLANKS       LCOBPG
||+--C                   END
||   C*                                                            I
||   C                   MOVE      VALR3         LTREC
||+--C     LTREC         IFLT      *ZEROS
|||  C                   MULT      -1            LTREC
|||  C                   MOVE      '2'           LCANUL
|||  C                   MOVE      LDTEMI        LDTANU
|||  C                   MOVE      *BLANKS       LDTEMI
|||  C                   MOVE      '5'           LCMOV
|||  C********              MOVE '   '     LCOBPG                   I
||+--C                   END
||   C*                                                            I
||   C                   MOVE      LCOBPG        LCDESP
||   C     LTREC         COMP      0                                    23
||   C   23              Z-SUB     LTREC         LTREC
||   C                   EXCEPT    REGPAG
|+---C                   END
|    C*
|    C*
|    C* =================================================================================
|    C*  RISCO IV + V    Reembolsos
|    C* =================================================================================
|    C     REE002        TAG
|    C*
|    C* DETERMINA O ENVIO ANTERIOR DO PAGAMENTO A SINISTROS
|    C*
|    C                   MOVE      *BLANKS       WKPAG
|    C                   MOVE      *BLANKS       WKR1
|    C                   MOVE      R2            WKR1
|    C                   MOVEL     KEYPAG        WKPAG
|    C                   MOVE      WKR1          WKPAG
|    C     WKPAG         CHAIN     LPAGAUT                            99
|    C  N99              GOTO      REE003
|    C*
|+---C     VALR45        IFNE      *ZEROS
||   C*                                                            I
||   C* SE PROCESSO É IDS IGNORA REGISTO - MOVIMENTO INCORRECTO    I
||   C* NOTA: PROCESSOS IDS SÓ PODEM SER DE R.C.                   I
||   C*                                                            I
||+--C     IDS           IFEQ      'S'
|||  C                   GOTO      REE003
||+--C                   END
||   C*                                                            I
||   C* FORMATA CÓDIGO MOVIMENTO                                   I
||   C*                                                            I
||   C                   MOVE      '4'           LCMOV
||   C*                                                            I
||   C* FORMATA DATA DE MOVIMENTO DO PAGAMENTO  (DATA EMISSÃO)     I
||   C*                                                            I
||   C                   Z-ADD     *ZEROS        DTY2
||   C                   MOVE      DTMOV         DTY2
||   C                   EXSR      SRDTY2
||   C                   MOVE      DTY2          LDTEMI
||   C*                                                            I
||   C* INICIALIZA CAMPOS                                          I
||   C*                                                            I
||   C                   Z-ADD     *ZEROS        LDTANU
||   C                   MOVE      *BLANKS       LCANUL
||   C*                                                            I
||   C                   MOVE      5             RIVV              1 0
||   C                   MOVE      R2            WCOB5
||   C                   MOVEL     RIVV          WCOB5
||   C                   MOVEL     WCOB5         PCOB5
||   C                   MOVE      CAUSA         PCOB5
||   C*                                                            I
||   C* PESQUISA/CONVERTE A COBERTURA AFECTA PELA PROVISAO         I
||   C*                                                            I
||   C                   Z-ADD     1             I                 2 0
||   C     PCOB5         LOOKUP    COB5G(I)                               30
||   C   30              MOVE      COB5L(I)      LCOBPG
||+--C  N30              DO
|||  C                   MOVE      '00'          PCOB5
|||  C                   Z-ADD     1             I                 2 0
|||  C     PCOB5         LOOKUP    COB5G(I)                               30
|||  C   30              MOVE      COB5L(I)      LCOBPG
||+--C  N30              MOVE      *BLANKS       LCOBPG
||   C                   END
||   C*                                                            I
||   C                   MOVE      VALR45        LTREC
||+--C     LTREC         IFLT      *ZEROS
|||  C                   MULT      -1            LTREC
|||  C                   MOVE      '2'           LCANUL
|||  C                   MOVE      LDTEMI        LDTANU
|||  C                   MOVE      *BLANKS       LDTEMI
|||  C                   MOVE      '5'           LCMOV
|||  C********             MOVE '   '     LCOBPG                   I
||+--C                   END
||   C*                                                            I
||   C                   MOVE      LCOBPG        LCDESP
||   C     LTREC         COMP      0                                    23
||   C   23              Z-SUB     LTREC         LTREC
||   C                   EXCEPT    REGPAG
|+---C                   END
|    C*
|    C* =================================================================================
|    C*  COBERTURA MIP   Reembolsos
|    C* =================================================================================
|    C     REE003        TAG
     C                   IF        (DTFIM > *ZEROS) AND (DTFIM < 130101)
     C                   GOTO      ESRREE
     C                   ENDIF
|    C*
|    C* DETERMINA O ENVIO ANTERIOR DO PAGAMENTO A SINISTROS
|    C*
|    C                   MOVE      *BLANKS       WKPAG
|    C                   MOVE      *BLANKS       WKR1
|    C                   MOVE      'M4'          WKR1
|    C                   MOVEL     KEYPAG        WKPAG
|    C                   MOVE      WKR1          WKPAG
|    C     WKPAG         CHAIN     LPAGAUT                            99
|    C  N99              GOTO      REE004
|    C*
|+---C     VALMIP        IFNE      *ZEROS
||   C*                                                            I
||   C* SE PROCESSO É IDS IGNORA REGISTO - MOVIMENTO INCORRECTO    I
||   C* NOTA: PROCESSOS IDS SÓ PODEM SER DE R.C.                   I
||   C*                                                            I
||+--C     IDS           IFEQ      'S'
|||  C                   GOTO      REE004
||+--C                   END
||   C*                                                            I
||   C* FORMATA CÓDIGO MOVIMENTO                                   I
||   C*                                                            I
||   C                   MOVE      '4'           LCMOV
||   C*                                                            I
||   C* FORMATA DATA DE MOVIMENTO DO PAGAMENTO  (DATA EMISSÃO)     I
||   C*                                                            I
||   C                   Z-ADD     *ZEROS        DTY2
||   C                   MOVE      DTMOV         DTY2
||   C                   EXSR      SRDTY2
||   C                   MOVE      DTY2          LDTEMI
||   C*                                                            I
||   C* INICIALIZA CAMPOS                                          I
||   C*                                                            I
||   C                   Z-ADD     *ZEROS        LDTANU
||   C                   MOVE      *BLANKS       LCANUL
||   C                   MOVE      VALMIP        LTREC
||+--C     LTREC         IFLT      *ZEROS
|||  C                   MULT      -1            LTREC
|||  C                   MOVE      '2'           LCANUL
|||  C                   MOVE      LDTEMI        LDTANU
|||  C                   MOVE      *BLANKS       LDTEMI
|||  C                   MOVE      '5'           LCMOV
||+--C                   END
||   C*                                                            I
||   C                   MOVE      '052'         LCOBPG
||   C                   MOVE      KTE_MIP       LCDESP
||   C     LTREC         COMP      0                                    23
||   C   23              Z-SUB     LTREC         LTREC
||   C                   EXCEPT    REGPAG
|+---C                   END
|    C*
|    C* =================================================================================
|    C*  COBERTURA DTRAT Reembolsos
|    C* =================================================================================
|    C     REE004        TAG
|    C*
|    C* DETERMINA O ENVIO ANTERIOR DO PAGAMENTO A SINISTROS
|    C*
|    C                   MOVE      *BLANKS       WKPAG
|    C                   MOVE      *BLANKS       WKR1
|    C                   MOVE      'D4'          WKR1
|    C                   MOVEL     KEYPAG        WKPAG
|    C                   MOVE      WKR1          WKPAG
|    C     WKPAG         CHAIN     LPAGAUT                            99
|    C  N99              GOTO      ESRREE
|    C*
|+---C     VALDTRA       IFNE      *ZEROS
||   C*                                                            I
||   C* SE PROCESSO É IDS IGNORA REGISTO - MOVIMENTO INCORRECTO    I
||   C* NOTA: PROCESSOS IDS SÓ PODEM SER DE R.C.                   I
||   C*                                                            I
||+--C     IDS           IFEQ      'S'
|||  C                   GOTO      ESRREE
||+--C                   END
||   C*                                                            I
||   C* FORMATA CÓDIGO MOVIMENTO                                   I
||   C*                                                            I
||   C                   MOVE      '4'           LCMOV
||   C*                                                            I
||   C* FORMATA DATA DE MOVIMENTO DO PAGAMENTO  (DATA EMISSÃO)     I
||   C*                                                            I
||   C                   Z-ADD     *ZEROS        DTY2
||   C                   MOVE      DTMOV         DTY2
||   C                   EXSR      SRDTY2
||   C                   MOVE      DTY2          LDTEMI
||   C*                                                            I
||   C* INICIALIZA CAMPOS                                          I
||   C*                                                            I
||   C                   Z-ADD     *ZEROS        LDTANU
||   C                   MOVE      *BLANKS       LCANUL
||   C                   MOVE      VALDTRA       LTREC
||+--C     LTREC         IFLT      *ZEROS
|||  C                   MULT      -1            LTREC
|||  C                   MOVE      '2'           LCANUL
|||  C                   MOVE      LDTEMI        LDTANU
|||  C                   MOVE      *BLANKS       LDTEMI
|||  C                   MOVE      '5'           LCMOV
||+--C                   END
||   C*                                                            I
||   C                   MOVE      '052'         LCOBPG
||   C                   MOVE      KTE_DTR       LCDESP
||   C     LTREC         COMP      0                                    23
||   C   23              Z-SUB     LTREC         LTREC
||   C                   EXCEPT    REGPAG
|+---C                   END
+----C     ESRREE        ENDSR
     O****************************************************
     O*  OUTPUT DE REGISTO DE PROCESSO DE REGISTO        *
     O****************************************************
     OLSINAUT   EADD         REGPRO
     O                       LRAMO                2                             RAMO
     O                       LMOD                 4                             MODALIDADE
     O                       LSMOD                6                             SUBMODADLIDADE
     O                       LAPOL               13                             APOLICE
     O                       LANO                17                             ANO DO PROCESSO
     O                       LNPROC              22                             NUMERO DO PROCESSO
     O                       LDTSIN              30                             DATA DO SINISTRO
     O                       LDTABE              38                             DATA DE ABERTURA
     O                       LCENC               40                             CODIGO ENCERRAMENTO
     O                       LDTENC              48                             DATA ENCERRAMENTO
     O                       LDTRAB              56                             DATA REABERTURA
     O                       LIDS                57                             INDICADOR DE IDS
     O*
     O*---> INFORMAÇÃO IDS
     O                       LNCASO              59                             NOSSO CASO (TAB. PRÁTICA)
     O                       LDTNAV              67                             DATA DE AVISO CASO IDS
     O                       LMARCT              85                             MARCA DO TERCEIRO
     O                       LMATRT              97                             MATRICULA DO TERCEIRO
     O                       LCOMPT             101                             COMPANHIA DO TERCEIRO
     O                       LCIDST             104                             CASO IDS DO TERCEIRO
     O*
     O*---> INFORMAÇÃO CPRS
     O                       LDATENTDOC         112                             Data de Entrada do Documento que deu
     O                       ICREGESTR          113                             regularização no estrangeiro (S/N)
     O                       ICFACEXC           114                             Factores Exceptionais (informar S/N)
     O                       ICSUSFRA           115                             Suspeita de Fraude (informar S/N)
     O                       ICDECLEI           116                             Decreto Lei de 2006
     O*
     O*---> INFORMAÇÃO FSA
     O                       CODPOST            120                             Código Postal do Sinistro (FSA)
     O                       CODFREG            123                             Código Freguesia (FSA)
     O*
     O*---> INFORMAÇÃO Apólice do Terceiro
     O                       LAPOLT             133                             APOLICE DO TERCEIRO
     O                       LPROCT             153                             NR. PROCESSO DO TERCEIRO
     O                       LNRESP             156                             NOSSA RESPONSABILIDADE
     O                       LRESP              160                             RESPONSABILIDADE (DIFERENÇA)
     O                       LLIPAG             161                             RESPONSABILIDADE (DIFERENÇA)
     O*
     O*---> INFORMAÇÃO CONTROLO
     O                       KEYPRO             376                             NUMERO PROCESSO (CHAVE)
     O                       LDTTRF             384                             DATA DA TRANSFERÊRECIA
     O****************************************************
     O*  OUTPUT DE REGISTO DE PROVISAO PARA SINISTRO     *
     O****************************************************
     OLPRVAUT   EADD         REGPRV
     O                       LRAMO                2                             RAMO
     O                       LMOD                 4                             MODALIDADE
     O                       LSMOD                6                             SUBMODADLIDADE
     O                       LANO                10                             ANO SINISNTRO
     O                       LNPROC              15                             NR. RECIBO
     O                       LCOBPV              18                             COBERTURA AFECTADA
     O                       LVLPRV              29                             VALOR DA PROVISÃO
     O                     22                    30 '-'
     O                       LCDESP              33                             Código de Despesa
     O*                        FILLER    64                               FILLER
     O                       PROPRV              70                             N/PROCESSO SINISTRO
     O                       NLIPRV              72                             NUMERO LINHA
     O*                        FILLER    64
     O                       LDTMOV             356
     O                       LPRVDT             364
     O                       WKPRV              376                             NUMERO PROCESSO (CHAVE)
     O                       LDTTRF             384                             DATA DA TRANSFERÊRECIA
     O****************************************************
     O*  OUTPUT DE REGISTO DE PAGAMENTO A SINISTRO       *
     O****************************************************
     OLPAGAUT   EADD         REGPAG
     O                       LRAMO                2                             RAMO
     O                       LMOD                 4                             MODALIDADE
     O                       LSMOD                6                             SUBMODADLIDADE
     O                       LANO                10                             ANO DO PROCESSO
     O                       LNPROC              15                             NUMERO DO PROCESSO
     O                       LNRECS              20                             NR.REC.SINISTRO (ID)
     O                       LEREC               21                             ESPECIE DE RECIBO
     O                       LDTEMI              29                             DATA DE EMISSAO
     O                       LTREC               42                             TOTAL RECIBO (MOVIM.)
     O                     23                    43 '-'
     O                       LCANUL              44                             CODIGO ANULACAO
     O                       LDTANU              52                             DATA DE ANULAÇÃO
     O                       LCMOV               53                             CODIGO DE MOVIMENTO
     O                       LCOBPG              56                             COBERTURA AFECTADA
     O                       LCDESP              59                             Código de Despesa
     O*                        FILLER    64                               FILLER
     O                       PROPAG              70
     O                       NLIPAG              72
     O                       NODOC               87
     O*                        FILLER   348
     O                       LDTMOV             356
     O                       LPAGDT             364
     O                       WKPAG              376                             NUMERO PROCESSO (CHAVE)
     O                       LDTTRF             384                             DATA DA TRANSFERÊRECIA
     O****************************************************
     O*  OUTPUT DE REGISTO DE PROCESSO DE REGISTO        *
     O****************************************************
     OESINAUT   EADD         REGPRO
     O                       LRAMO                2                             RAMO
     O                       LMOD                 4                             MODALIDADE
     O                       LSMOD                6                             SUBMODADLIDADE
     O                       LAPOL               13                             APOLICE
     O                       LANO                17                             ANO DO PROCESSO
     O                       LNPROC              22                             NUMERO DO PROCESSO
     O                       LDTSIN              30                             DATA DO SINISTRO
     O                       LDTABE              38                             DATA DE ABERTURA
     O                       LCENC               40                             CODIGO ENCERRAMENTO
     O                       LDTENC              48                             DATA ENCERRAMENTO
     O                       LDTRAB              56                             DATA REABERTURA
     O                       LIDS                57                             INDICADOR DE IDS
     O*
     O*---> INFORMAÇÃO IDS
     O                       LNCASO              59                             NOSSO CASO (TAB. PRÁTICA)
     O                       LDTNAV              67                             DATA DE AVISO CASO IDS
     O                       LMARCT              85                             MARCA DO TERCEIRO
     O                       LMATRT              97                             MATRICULA DO TERCEIRO
     O                       LCOMPT             101                             COMPANHIA DO TERCEIRO
     O                       LCIDST             104                             CASO IDS DO TERCEIRO
     O*
     O*---> INFORMAÇÃO CPRS
     O                       LDATENTDOC         112                             Data de Entrada do Documento que deu
     O                       ICREGESTR          113                             regularização no estrangeiro (S/N)
     O                       ICFACEXC           114                             Factores Exceptionais (informar S/N)
     O                       ICSUSFRA           115                             Suspeita de Fraude (informar S/N)
     O                       ICDECLEI           116                             Decreto Lei de 2006
     O*
     O*---> INFORMAÇÃO FSA
     O                       CODPOST            120                             Código Postal do Sinistro (FSA)
     O                       CODFREG            123                             Código Freguesia (FSA)
     O*
     O*---> INFORMAÇÃO Apólice do Terceiro
     O                       LAPOLT             133                             APOLICE DO TERCEIRO
     O                       LPROCT             153                             NR. PROCESSO DO TERCEIRO
     O                       LNRESP             156                             NOSSA RESPONSABILIDADE
     O                       LRESP              160                             RESPONSABILIDADE (DIFERENÇA)
     O                       LLIPAG             161                             RESPONSABILIDADE (DIFERENÇA)
     O*
     O*---> INFORMAÇÃO CONTROLO
     O                       KEYPRO             376                             NUMERO PROCESSO (CHAVE)
     O                       LDTTRF             384                             DATA DA TRANSFERÊRECIA
     O****************************************************
     O*  OUTPUT DE REGISTO DE PROVISAO PARA SINISTRO     *
     O****************************************************
     OEPRVAUT   EADD         REGPRV
     O                       LRAMO                2                             RAMO
     O                       LMOD                 4                             MODALIDADE
     O                       LSMOD                6                             SUBMODADLIDADE
     O                       LANO                10                             ANO SINISNTRO
     O                       LNPROC              15                             NR. RECIBO
     O                       LCOBPV              18                             COBERTURA AFECTADA
     O                       LVLPRV              29                             VALOR DA PROVISÃO
     O                     22                    30 '-'
     O                       LCDESP              33                             Código de Despesa
     O*                        FILLER    64                               FILLER
     O                       PROPRV              70                             N/PROCESSO SINISTRO
     O                       NLIPRV              72                             NUMERO LINHA
     O*                        FILLER    64
     O                       LDTMOV             356
     O                       LPRVDT             364
     O                       WKPRV              376                             NUMERO PROCESSO (CHAVE)
     O                       LDTTRF             384                             DATA DA TRANSFERÊRECIA
     O****************************************************
     O*  OUTPUT DE REGISTO DE PAGAMENTO A SINISTRO       *
     O****************************************************
     OEPAGAUT   EADD         REGPAG
     O                       LRAMO                2                             RAMO
     O                       LMOD                 4                             MODALIDADE
     O                       LSMOD                6                             SUBMODADLIDADE
     O                       LANO                10                             ANO DO PROCESSO
     O                       LNPROC              15                             NUMERO DO PROCESSO
     O                       LNRECS              20                             NR.REC.SINISTRO (ID)
     O                       LEREC               21                             ESPECIE DE RECIBO
     O                       LDTEMI              29                             DATA DE EMISSAO
     O                       LTREC               42                             TOTAL RECIBO (MOVIM.)
     O                     23                    43 '-'
     O                       LCANUL              44                             CODIGO ANULACAO
     O                       LDTANU              52                             DATA DE ANULAÇÃO
     O                       LCMOV               53                             CODIGO DE MOVIMENTO
     O                       LCOBPG              56                             COBERTURA AFECTADA
     O                       LCDESP              59                             Código de Despesa
     O*                        FILLER    64                               FILLER
     O                       PROPAG              70
     O                       NLIPAG              72
     O                       NODOC               87
     O*                        FILLER   348
     O                       LDTMOV             356
     O                       LPAGDT             364
     O                       WKPAG              376                             NUMERO PROCESSO (CHAVE)
     O                       LDTTRF             384                             DATA DA TRANSFERÊRECIA
     O*******************************************************
**              *** TABELA DE CONVERSAO DE RAMOS      RMG()/ RML ()
01080000               * INCÊNDIO E E.NATUREZA      1  01 => 080000
02010200               * ACIDENTES PESSOAIS         2  02 => 010200
03130300               * RESP. CIVIL PESSOAL        3  03 => 130300
04130500               * RESP. CIVIL COM./IND.      4  04 => 130500
05340100               * RISCOS M.HABITAÇÃO         5  05 => 340100
06160100               * PERDAS DE LUCROS           6  06 => 160100
07090600               * ROUBO VALORES              7  07 => 090600
08090600               * ROUBO RESIDÊNCIAS          8  08 => 090600
09090600               * ROUBO ESTABELECIMENTOS     9  09 => 090600
00010205               * VIAGENS                   10  10 => 010205
14300100               * DOENÇA                    11  14 => 300100
15010101               * ACIDENTES DE TRABALHO     12  15 => 010101
17340700               * EQUIPAMENTO ELECTRÓNICO   13  17 => 340700
18090304               * AVARIA DE MÁQUINAS        14  18 => 090304
19340600               * OBRAS E MONTAGENS         15  19 => 340600
51010301               * OCUPANTES                 16  51 => 010301
50310000               * AUTOMÓVEL                 17  50 => 310000
55340200               * RISCOS M.ESTABELECIMENTOS 18  55 => 340200
65090400               * CRISTAIS                  19  65 => 090400
67070002               * BAGAGEM                   20  67 => 070002
68070002               * MARÍTIMO TERRESTRE        21  68 => 070002
69070002               * POSTAL                    22  69 => 070002
70070003               * AÉREO                     23  70 => 070003
**              *** TABELA DE CONVERSAO DE CASOS IDS ***  CASG() / CASL()
1010                                                  1    10 => 10
1111                                                  2    11 => 11
1212                                                  3    12 => 12
1313                                                  4    13 => 13
1414                                                  5    14 => 14
1515                                                  6    15 => 15
1616                                                  7    16 => 16
1717                                                  8    17 => 17
2020                                                  9    20 => 20
2121                                                 10    21 => 21
3030                                                 11    30 => 30
3131                                                 12    31 => 31
4040                                                 13    40 => 40
5050                                                 14    50 => 50
5151                                                 15    51 => 51
5252                                                 16    52 => 52
5353                                                 17    53 => 53
6060                                                 18    60 => 60
6161                                                 19    61 => 61
6262                                                 20    62 => 62
6363                                                 21    63 => 63
6464                                                 22    64 => 64
7070                                                 23    70 => 70
7171                                                 24    71 => 71
7272                                                 25    72 => 72
**              *** TAB.CONVERSAO COD.COBERTURA PROVISAO ( RI = 1 ) ***  COB1G() / COB1L/()
1100001                                               1   1100 => 001
1105054                                               2   1105 => 054
1200001                                               3   1200 => 001
9900001         0 ASSUME RC OBRIGATORIO               4   9900 => 001
9990111         1 DANOS CORPORAIS                     5   9990 => 111
9991001         2 IDS CREDOR                          6   9991 => 001
9992001         3 IDS DEVEDOR                         7   9992 => 001
**              *** TAB.CONVERSAO COD.COBERTURA PROVISAO ( RIII = 3 ) ***  COB3G() / COB3L()
3300003                                               1   3300 => 003
3301003                                               2   3301 => 003
3302003                                               3   3302 => 003
3305054                                               4   3305 => 054
3306003                                               5   3306 => 003
3309003                                               6   3309 => 003
3400003                                               7   3400 => 003
3401003                                               8   3401 => 003
3402003                                               9   3402 => 003
3405054                                              10   3405 => 054
3406003                                              11   3406 => 003
3700054                                              12   3700 => 054
3705054                                              13   3705 => 054
3800003                                              14   3800 => 003
3801003                                              15   3801 => 003
3802003                                              16   3802 => 003
3805054                                              17   3805 => 054
3806003                                              18   3806 => 003
3809003                                              19   3809 => 003
3811053                                              20   3811 => 053
3812052                                              21   3812 => 052
3900054                                              22   3900 => 054
3905054                                              23   3905 => 054
**              *** TAB.CONVERSAO COD.COBERTURA PROVISAO ( RIV+V = 5) ***    COB5G() / COB5L()
5300005                                               1   5300 => 005
5304005                                               2   5304 => 005
5307004                                               3   5307 => 004
5308005                                               4   5308 => 005
5310005                                               5   5310 => 005
5400004                                               6   5400 => 004
5407004                                               7   5407 => 004
5500004                                               8   5500 => 004
5507004                                               9   5507 => 004
5604005                                              10   5604 => 005
5600005                                              11   5600 => 005
5608005                                              12   5608 => 005
5610005                                              13   5610 => 005
5700004                                              14   5700 => 004
5707004                                              15   5707 => 004
5800005                                              16   5800 => 005
5804005                                              17   5804 => 005
5807004                                              18   5807 => 004
5808005                                              19   5808 => 005
5810005                                              20   5810 => 005
5900005                                              21   5900 => 005
5904005                                              22   5904 => 005
5908005                                              23   5908 => 005
5910005                                              24   5910 => 005
**              *** TABELA DE CONVERSAO COD. COBERTURA DESPESAS ***   TPAGG() / COBDL()
310000000933                                          1  310000000 => 933
310000110930                                          2  310000110 => 930
310000116909                                          3  310000116 => 909
