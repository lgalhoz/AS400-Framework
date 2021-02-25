     H DECEDIT(',') DFTNAME(TRSF30) DATEDIT(*DMY/)
     H********************************************************************
     H*  EMPRESA    :  GRUPO MATA.                                       *
     H*  PROGRAMA   :  TRSF30                                            *
     H*  TIPO       :  SRT                                               *
     H*  APLICAÇÃO  :  PRODUÇAO GUARDIAN (EXPLORAÇAO TODOS OS RAMOS)     *
     H*  FUNÇÃO        - CRIAÇÃO DE FICHEIRO PARA TRANSFERÊNCIA DE       *
     H*                  INFORMAÇÃO (DADOS) PARA A LUSITANIA "VERDE"     *
     H*                                                                  *
     H*  DADOS      :  FICHEIRO DE SINISTROS - OUTROS RAMOS              *
     H*                SINISTROS / PROVISÕES / PAGAMENTOS                *
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
     F*  04 - IDENTIFICA REGISTO FICHEIRO DE PROVISOES PARA SINISTROS    *
     F*  05 - IDENTIFICA REGISTO DO FICHEIRO PAGAMENTOS / REEMBOLSOS     *
     F*                                                                  *
     F*  20 - LOKUP TABELA DE RAMOS (CONVERSAO)                          *
     F*                                                                  *
     F********************************************************************
     FSINPRO    IP   F  384     9AIDISK                                         KEYLOC(EXTK)
     FSINPRV    IF   F  128    10AIDISK                                         KEYLOC(EXTK)
     FSINPAG    IF   F  300    10AIDISK                                         KEYLOC(EXTK)
     FGBASEGRO  IF   F  700     9AIDISK    KEYLOC(1)
     FLSINORM   UF A F  384    25AIDISK    KEYLOC(352)
     FLPRVORM   UF A F  384    12AIDISK    KEYLOC(365)
     FLPAGORM   UF A F  384    12AIDISK    KEYLOC(365)
     FESINORM   O  A F  384    25AIDISK    KEYLOC(352)
     FEPRVORM   O  A F  384    12AIDISK    KEYLOC(365)
     FEPAGORM   O  A F  384    12AIDISK    KEYLOC(365)
     D********************************************************************
     D RMG             S              2  0 DIM(24) CTDATA PERRCD(1)             RAMOS L.AZUL VS L.VERDE
     D RML             S              6  0 DIM(24) ALT(RMG)
     D TPAGG           S              9  0 DIM(48) CTDATA PERRCD(1)             TP.PAG. LA VS COB.DESP LV
     D COBDL           S              3  0 DIM(48) ALT(TPAGG)
     D RIG             S              3  0 DIM(71) CTDATA PERRCD(1) ASCEND      RICOS VS COBERTURAS
     D RIL             S              3  0 DIM(71) ALT(RIG)
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
     D*
     D                 DS
     D  KEYPRO                 1     25
     D  RM                     1      2  0
     D  IPROC                  3      3
     D  AAPROC                 4      5  0
     D  NSEQ                   6      9  0
     D  AA1                   10     11  0
     D  AAFIM                 12     13  0
     D  DTFIM                 12     17  0
     D  AA2                   18     19  0
     D  AARINI                20     21  0
     D  DTRINI                20     25  0
     D*
     D                 DS
     D  UDTTRF                21     26  0
     D  UATRF                 21     22  0
     D  UMTRF                 23     24  0
     D  UDTRF                 25     26  0
     D                 DS
     D  TIPMOV                 1      3  0
     D  TPMOV1                 1      1  0
     I********************************************************************
     I* FICHEIRO DE SINISTROS - GUARDIAN AUTOMÓVEL                       *
     I* CHAVE = KEYPRO                                                   *
     I********************************************************************
     ISINPRO    NS  01
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
     IGBASEGRO  AA
     I                                204  204  CONDOM
     I                                463  463  CS                              TIPO DE TRABALHADOR
     I                                518  518  TPT                             TIPO DE TRABALHADOR
     I********************************************************************
     I* FICHEIROS DE CONTROL *                                          *
     I********************************************************************
     ILSINORM   NS
     I*
     ILPRVORM   NS
     I*
     ILPAGORM   NS
     I********************************************************************
     C*
     C     FLAGU         IFEQ      0                                            <-----------+
     C                   TIME                    UDTHH                                      I
     C                   Z-ADD     1             FLAGU             1 0                      I
     C*                                                                I
     C* FORMATA DATA DO MOVIMENTO - DATA DE SELECÇÃO                   I
     C*                                                                I
     C                   Z-ADD     *ZEROS        DTY2                                       I
     C                   Z-ADD     UDTMOV        DTY2                                       I
     C                   EXSR      SRDTY2                                                   I
     C                   MOVE      DTY2          LDTMOV            8 0                      I
     C*                                                                I
     C* FORMATA DATA DA TRANSFERÊNCIA DOS DADOS                        I
     C*                                                                I
     C                   Z-ADD     UDDDT         UDTRF                                      I
     C                   Z-ADD     UMMDT         UMTRF                                      I
     C                   Z-ADD     UAADT         UATRF                                      I
     C*                                                                I
     C                   Z-ADD     *ZEROS        DTY2              8 0                      I
     C                   MOVE      UDTTRF        DTY2                                       I
     C                   EXSR      SRDTY2                                                   I
     C                   MOVE      DTY2          LDTTRF            8 0                      I
     C*                                                                I
     C                   END                                                    <-----------+
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
     C     JMDTP         IFGT      LDTMOV
     C                   GOTO      FIM
     C                   END
     C*
     C                   Z-ADD     0             AA1
     C     DTFIM         IFNE      0
     C     AAFIM         IFGE      50
     C                   Z-ADD     19            AA1
     C                   ELSE
     C                   Z-ADD     20            AA1
     C                   END
     C                   END
     C                   Z-ADD     0             AA2
     C     DTRINI        IFNE      0
     C     AARINI        IFGE      50
     C                   Z-ADD     19            AA2
     C                   ELSE
     C                   Z-ADD     20            AA2
     C                   END
     C                   END
     C*
     C* DETERMINA O ENVIO ANTERIOR DO PROCESSO DE SINISTRO
     C*
     C                   SETOFF                                       98
     C     KEYPRO        CHAIN     LSINORM                            99
     C   99              SETON                                        98
     C*
     C* CONVERTE RAMOS  ( RAMO GUARDIAN VS LUSITANIA ).
     C*
     C                   MOVEL     RM            RRAPOL            9 0
     C                   MOVE      AP            RRAPOL
     C     RRAPOL        CHAIN     GBASEGRO                           99
     C   99              MOVE      'O'           TPT
     C  N99CS            IFEQ      '2'
     C                   MOVE      'S'           LLIPAG
     C                   END
     C*
     C                   Z-ADD     1             I                 2 0
     C     RM            IFEQ      15
     C     TPT           IFEQ      'P'
     C                   Z-ADD     13            I
     C                   END
     C                   END
     C     RM            LOOKUP    RMG(I)                                 20
     C   20              MOVE      RML(I)        W1RAMO
     C  N20              MOVE      *BLANKS       W1RAMO
     C                   MOVE      W1RAMO        W2RAMO
     C*
     C                   MOVEL     W1RAMO        LRAMO
     C                   MOVEL     W2RAMO        LMOD
     C                   MOVE      W2RAMO        LSMOD
      *
     C     RM            IFEQ      05
     C     CONDOM        IFNE      ' '
     C                   MOVE      34            LRAMO
     C                   MOVE      01            LMOD
     C                   MOVE      03            LSMOD
     C                   END
     C                   END
      *
     C                   MOVE      AP            LAPOL
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
     C     DTABER        IFGE      050101                                       - A PARTIR
     C                   MOVEL     '9'           LNPROC                           DE 01/2005
     C                   ELSE
     C                   MOVEL     '7'           LNPROC
     C                   END
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
     C     DTFIM         IFGT      *ZEROS
     C                   MOVE      '02'          LCENC
     C                   ELSE
     C                   MOVE      '00'          LCENC
     C                   END
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
     C                   MOVE      *BLANKS       C10              10
     C                   MOVEL     ANPROC        C8                8 0
     C                   MOVE      '00'          C8
     C                   MOVEL     RM            C10
     C                   MOVE      C8            C10
     C     C10           SETLL     SINPRV
     C*
     C*  LÊ PROVISOES PARA PROCESSO E FORMATA CAMPOS PARA OUTPUT
     C*
     C     LOOPRV        TAG
     C                   READ      SINPRV                                 50
     C   50              GOTO      SEGUE1
     C*
     C     RMPRV         COMP      RM                                 5151
     C  N51PROPRV        COMP      ANPROC                             5151
     C   51              GOTO      SEGUE1
     C*
     C                   Z-ADD     *ZEROS        DTY2
     C                   Z-ADD     PRVDTM        DTY2
     C                   EXSR      SRDTY2
     C                   MOVE      DTY2          LPRVDT            8 0
     C*
     C     LPRVDT        IFLE      LDTMOV
     C                   MOVE      *BLANKS       KEYPRV           12 0
     C                   MOVEL     ANPROC        WKPRV1            8 0
     C                   MOVE      NLIPRV        WKPRV1
     C                   MOVEL     RMPRV         WKPRV2           10 0
     C                   MOVE      WKPRV1        WKPRV2
     C                   MOVEL     WKPRV2        KEYPRV
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
     C     C10           SETLL     SINPAG
     C     LOOPAG        TAG
     C                   READ      SINPAG                                 50
     C   50              GOTO      SEGUE2
     C*
     C     RMPAG         COMP      RM                                 5252
     C  N52PROPAG        COMP      ANPROC                             5252
     C   52              GOTO      SEGUE2
     C*
     C                   Z-ADD     *ZEROS        DTY2
     C                   Z-ADD     PAGDTM        DTY2
     C                   EXSR      SRDTY2
     C                   MOVE      DTY2          LPAGDT            8 0
     C*
     C     LPAGDT        IFLE      LDTMOV
     C                   MOVE      *BLANKS       KEYPAG           12 0
     C                   MOVEL     ANPROC        WKPAG1            8 0
     C                   MOVE      NLIPAG        WKPAG1
     C                   MOVEL     RMPAG         WKPAG2           10 0
     C                   MOVE      WKPAG1        WKPAG2
     C                   MOVEL     WKPAG2        KEYPAG
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
     C*---               END
     C*
     C*---               END
     C*
     C                   GOTO      LOOPAG
     C*
     C****************************************************
     C     SEGUE2        TAG
     C****************************************************
     C     FIM           TAG
     C/EJECT
     C****************************************************
     C*   FORMATA ANO COM 4 DIGITOS  ( REGRA ANO 2000 )   *
     C********************************************    > < =
     C     SR2000        BEGSR
     C*
     C     Y2000         IFLT      40
     C                   MOVEL     20            Y2000
     C                   ELSE
     C                   MOVEL     19            Y2000
     C                   END
     C*
     C                   ENDSR
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
     C/EJECT
     C*****************************************************
     C*   FORMATA PROVISOES                               *
     C********************************************    > < =
     C     SRFPRV        BEGSR
     C*
     C* LIMPA CAMPOS DE TRABALHO
     C*
     C                   Z-ADD     *ZEROS        RI                1 0
     C                   Z-ADD     *ZEROS        RII               1 0
     C                   Z-ADD     *ZEROS        RIII              1 0
     C                   Z-ADD     *ZEROS        WCOB1             3 0
     C                   Z-ADD     *ZEROS        WCOB2             3 0
     C                   Z-ADD     *ZEROS        WCOB3             3 0
     C* ---
     C* - RISCO I ------------------------------------------
     C* ---
     C*
     C* DETERMINA O ENVIO ANTERIOR DA PROVISÃO DO SINISTRO
     C*
     C                   MOVE      R1            KEYPRV
     C     KEYPRV        CHAIN     LPRVORM                            99
     C  N99              GOTO      ESR001
     C*
     C     VALR1         IFNE      *ZEROS                                       <---------+
     C                   MOVE      R1            RI                                       I
     C                   MOVE      RI            WCOB1                                    I
     C                   MOVEL     RM            WCOB1                                    I
     C*                                                              I
     C* PESQUISA/CONVERTE A COBERTURA AFECTADA PELA PROVISAO         I
     C*                                                              I
     C                   Z-ADD     1             I                 2 0                    I
     C     WCOB1         LOOKUP    RIG(I)                                 30              I
     C   30              MOVE      RIL(I)        LCOBPV                                   I
     C  N30              DO                                                               I
     C                   MOVE      '9'           RI                                       I
     C                   MOVE      RI            WCOB1                                    I
     C                   Z-ADD     1             I                 2 0                    I
     C     WCOB1         LOOKUP    RIG(I)                                 30              I
     C   30              MOVE      RIL(I)        LCOBPV                                   I
     C  N30              MOVE      *BLANKS       LCOBPV                                   I
     C                   END                                                              I
     C*                                                              I
     C                   MOVE      VALR1         LVLPRV                                   I
     C                   EXCEPT    REGPRV                                                 I
     C                   END                                                    <---------+
     C*
     C     ESR001        TAG
     C* ---
     C* - RISCO II  ----------------------------------------
     C* ---
     C*
     C* DETERMINA O ENVIO ANTERIOR DA PROVISÃO DO SINISTRO
     C*
     C                   MOVE      R2            KEYPRV
     C     KEYPRV        CHAIN     LPRVORM                            99
     C  N99              GOTO      ESR002
     C*
     C     VALR3         IFNE      *ZEROS                                       <---------+
     C                   MOVE      R2            RII               1 0                    I
     C                   MOVE      RII           WCOB2                                    I
     C                   MOVEL     RM            WCOB2                                    I
     C*                                                              I
     C* PESQUISA/CONVERTE A COBERTURA AFECTADA PELA PROVISAO         I
     C*                                                              I
     C                   Z-ADD     1             I                 2 0                    I
     C     WCOB2         LOOKUP    RIG(I)                                 30              I
     C   30              MOVE      RIL(I)        LCOBPV                                   I
     C  N30              DO                                                               I
     C                   MOVE      '9'           RII                                      I
     C                   MOVE      RII           WCOB2                                    I
     C                   Z-ADD     1             I                 2 0                    I
     C     WCOB2         LOOKUP    RIG(I)                                 30              I
     C   30              MOVE      RIL(I)        LCOBPV                                   I
     C  N30              MOVE      *BLANKS       LCOBPV                                   I
     C                   END                                                              I
     C*                                                              I
     C                   MOVE      VALR3         LVLPRV                                   I
     C                   EXCEPT    REGPRV                                                 I
     C                   END                                                    <---------+
     C*
     C     ESR002        TAG
     C* ---
     C* - RISCO III  ---------------------------------------
     C* ---
     C*
     C* DETERMINA O ENVIO ANTERIOR DA PROVISÃO DO SINISTRO
     C*
     C                   MOVE      R3            KEYPRV
     C     KEYPRV        CHAIN     LPRVORM                            99
     C  N99              GOTO      ESRPRV
     C*
     C     VALR45        IFNE      *ZEROS                                       <---------+
     C                   MOVE      R3            RIII              1 0                    I
     C                   MOVE      RIII          WCOB3                                    I
     C                   MOVEL     RM            WCOB3                                    I
     C*                                                              I
     C* PESQUISA/CONVERTE A COBERTURA AFECTADA PELA PROVISAO         I
     C*                                                              I
     C                   Z-ADD     1             I                 2 0                    I
     C     WCOB3         LOOKUP    RIG(I)                                 30              I
     C   30              MOVE      RIL(I)        LCOBPV                                   I
     C  N30              DO                                                               I
     C                   MOVE      '9'           RIII                                     I
     C                   MOVE      RIII          WCOB3                                    I
     C                   Z-ADD     1             I                 2 0                    I
     C     WCOB3         LOOKUP    RIG(I)                                 30              I
     C   30              MOVE      RIL(I)        LCOBPV                                   I
     C  N30              MOVE      *BLANKS       LCOBPV                                   I
     C                   END                                                              I
     C*                                                              I
     C                   MOVE      VALR45        LVLPRV                                   I
     C                   EXCEPT    REGPRV                                                 I
     C                   END                                                    <---------+
     C*
     C     ESRPRV        ENDSR
     C/EJECT
     C*****************************************************
     C*  FORMATA COBERTURAS PARA PAGAMENTOS/INDEMNIZAÇÕES *
     C********************************************    > < =
     C     SRCIND        BEGSR
     C*
     C* LIMPA CAMPOS DE TRABALHO
     C*
     C                   Z-ADD     *ZEROS        RI
     C                   Z-ADD     *ZEROS        RII
     C                   Z-ADD     *ZEROS        RIII
     C                   Z-ADD     *ZEROS        WCOB1
     C                   Z-ADD     *ZEROS        WCOB2
     C                   Z-ADD     *ZEROS        WCOB3
     C*
     C* FORMATA NÚMERO E ESPECIE DE RECIBO
     C*
     C                   MOVE      *BLANKS       LNRECS                         ( ATÉ NOVA REGRA )
     C                   MOVE      '1'           LEREC
     C* ---
     C* - RISCO I ------------------------------------------
     C* ---
     C*
     C* DETERMINA O ENVIO ANTERIOR DO PAGAMENTO (INDEMNIZAÇÃO)
     C*
     C                   MOVE      R1            KEYPAG
     C     KEYPAG        CHAIN     LPAGORM                            99
     C  N99              GOTO      IND001
     C*
     C     VALR1         IFNE      *ZEROS                                       <---------+
     C*                                                              I
     C* FORMATA CÓDIGO MOVIMENTO                                     I
     C*                                                              I
     C                   MOVE      '4'           LCMOV                                    I
     C*                                                              I
     C* FORMATA DATA DE MOVIMENTO DO PAGAMENTO  (DATA EMISSÃO)       I
     C*                                                              I
     C                   Z-ADD     *ZEROS        DTY2                                     I
     C                   MOVE      DTMOV         DTY2                                     I
     C                   EXSR      SRDTY2                                                 I
     C                   MOVE      DTY2          LDTEMI                                   I
     C*                                                              I
     C* INICIALIZA CAMPOS                                            I
     C*                                                              I
     C                   Z-ADD     *ZEROS        LDTANU                                   I
     C                   MOVE      *BLANKS       LCANUL                                   I
     C*                                                              I
     C                   MOVE      R1            RI                                       I
     C                   MOVE      RI            WCOB1                                    I
     C                   MOVEL     RM            WCOB1                                    I
     C*                                                              I
     C* PESQUISA/CONVERTE A COBERTURA AFECTADA PELA INDEMNIZAÇÃO     I
     C*                                                              I
     C                   Z-ADD     1             I                 2 0                    I
     C     WCOB1         LOOKUP    RIG(I)                                 30              I
     C   30              MOVE      RIL(I)        LCOBPG                                   I
     C  N30              DO                                                               I
     C                   MOVE      '9'           RI                                       I
     C                   MOVE      RI            WCOB1                                    I
     C                   Z-ADD     1             I                 2 0                    I
     C     WCOB1         LOOKUP    RIG(I)                                 30              I
     C   30              MOVE      RIL(I)        LCOBPG                                   I
     C  N30              MOVE      *BLANKS       LCOBPG                                   I
     C                   END                                                              I
     C*                                                              I
     C                   MOVE      VALR1         LTREC                                    I
     C     LTREC         IFLT      *ZEROS                                                 I
     C                   MULT      -1            LTREC                                    I
     C                   MOVE      '2'           LCANUL                                   I
     C                   MOVE      LDTEMI        LDTANU                                   I
     C                   MOVE      *BLANKS       LDTEMI                                   I
     C                   MOVE      '5'           LCMOV                                    I
     C********             MOVE '   '     LCOBPG                     I
     C                   END                                                              I
     C*                                                              I
     C                   EXCEPT    REGPAG                                                 I
     C                   END                                                    <---------+
     C*
     C     IND001        TAG
     C* ---
     C* - RISCO II  ----------------------------------------
     C* ---
     C*
     C* DETERMINA O ENVIO ANTERIOR DA PROVISÃO DO SINISTRO
     C*
     C                   MOVE      R2            KEYPAG
     C     KEYPAG        CHAIN     LPAGORM                            99
     C  N99              GOTO      IND002
     C*
     C     VALR3         IFNE      *ZEROS                                       <---------+
     C*                                                              I
     C* FORMATA CÓDIGO MOVIMENTO                                     I
     C*                                                              I
     C                   MOVE      '4'           LCMOV                                    I
     C*                                                              I
     C* FORMATA DATA DE MOVIMENTO DO PAGAMENTO  (DATA EMISSÃO)       I
     C*                                                              I
     C                   Z-ADD     *ZEROS        DTY2                                     I
     C                   MOVE      DTMOV         DTY2                                     I
     C                   EXSR      SRDTY2                                                 I
     C                   MOVE      DTY2          LDTEMI                                   I
     C*                                                              I
     C* INICIALIZA CAMPOS                                            I
     C*                                                              I
     C                   Z-ADD     *ZEROS        LDTANU                                   I
     C                   MOVE      *BLANKS       LCANUL                                   I
     C*                                                              I
     C                   MOVE      R2            RII               1 0                    I
     C                   MOVE      RII           WCOB2                                    I
     C                   MOVEL     RM            WCOB2                                    I
     C*                                                              I
     C* PESQUISA/CONVERTE A COBERTURA AFECTADA PELA PROVISAO         I
     C*                                                              I
     C                   Z-ADD     1             I                 2 0                    I
     C     WCOB2         LOOKUP    RIG(I)                                 30              I
     C   30              MOVE      RIL(I)        LCOBPG                                   I
     C  N30              DO                                                               I
     C                   MOVE      '9'           RII                                      I
     C                   MOVE      RII           WCOB2                                    I
     C                   Z-ADD     1             I                 2 0                    I
     C     WCOB2         LOOKUP    RIG(I)                                 30              I
     C   30              MOVE      RIL(I)        LCOBPG                                   I
     C  N30              MOVE      *BLANKS       LCOBPG                                   I
     C                   END                                                              I
     C*                                                              I
     C                   MOVE      VALR3         LTREC                                    I
     C     LTREC         IFLT      *ZEROS                                                 I
     C                   MULT      -1            LTREC                                    I
     C                   MOVE      '2'           LCANUL                                   I
     C                   MOVE      LDTEMI        LDTANU                                   I
     C                   MOVE      *BLANKS       LDTEMI                                   I
     C                   MOVE      '5'           LCMOV                                    I
     C*********            MOVE '   '     LCOBPG                     I
     C                   END                                                              I
     C*                                                              I
     C                   EXCEPT    REGPAG                                                 I
     C                   END                                                    <---------+
     C*
     C     IND002        TAG
     C* ---
     C* - RISCO III  ---------------------------------------
     C* ---
     C*
     C* DETERMINA O ENVIO ANTERIOR DA PROVISÃO DO SINISTRO
     C*
     C                   MOVE      R3            KEYPAG
     C     KEYPAG        CHAIN     LPAGORM                            99
     C  N99              GOTO      ESRIND
     C*
     C     VALR45        IFNE      *ZEROS                                       <---------+
     C*                                                              I
     C* FORMATA CÓDIGO MOVIMENTO                                     I
     C*                                                              I
     C                   MOVE      '4'           LCMOV                                    I
     C*                                                              I
     C* FORMATA DATA DE MOVIMENTO DO PAGAMENTO  (DATA EMISSÃO)       I
     C*                                                              I
     C                   Z-ADD     *ZEROS        DTY2                                     I
     C                   MOVE      DTMOV         DTY2                                     I
     C                   EXSR      SRDTY2                                                 I
     C                   MOVE      DTY2          LDTEMI                                   I
     C*                                                              I
     C* INICIALIZA CAMPOS                                            I
     C*                                                              I
     C                   Z-ADD     *ZEROS        LDTANU                                   I
     C                   MOVE      *BLANKS       LCANUL                                   I
     C*                                                              I
     C                   MOVE      R3            RIII              1 0                    I
     C                   MOVE      RIII          WCOB3                                    I
     C                   MOVEL     RM            WCOB3                                    I
     C*                                                              I
     C* PESQUISA/CONVERTE A COBERTURA AFECTADA PELA PROVISAO         I
     C*                                                              I
     C                   Z-ADD     1             I                 2 0                    I
     C     WCOB3         LOOKUP    RIG(I)                                 30              I
     C   30              MOVE      RIL(I)        LCOBPG                                   I
     C  N30              DO                                                               I
     C                   MOVE      '9'           RIII                                     I
     C                   MOVE      RIII          WCOB3                                    I
     C                   Z-ADD     1             I                 2 0                    I
     C     WCOB3         LOOKUP    RIG(I)                                 30              I
     C   30              MOVE      RIL(I)        LCOBPG                                   I
     C  N30              MOVE      *BLANKS       LCOBPG                                   I
     C                   END                                                              I
     C*                                                              I
     C                   MOVE      VALR45        LTREC                                    I
     C     LTREC         IFLT      *ZEROS                                                 I
     C                   MULT      -1            LTREC                                    I
     C                   MOVE      '2'           LCANUL                                   I
     C                   MOVE      LDTEMI        LDTANU                                   I
     C                   MOVE      *BLANKS       LDTEMI                                   I
     C                   MOVE      '5'           LCMOV                                    I
     C*********            MOVE '   '     LCOBPG                     I
     C                   END                                                              I
     C*                                                              I
     C                   EXCEPT    REGPAG                                                 I
     C                   END                                                    <---------+
     C*
     C     ESRIND        ENDSR
     C*
     C/EJECT
     C*****************************************************
     C*   FORMATA COBERTURAS PARA PAGAMENTOS/DESPESAS     *
     C********************************************    > < =
     C     SRFPAG        BEGSR
     C*
     C* FORMATA NÚMERO E ESPECIE DE RECIBO E CÓDIGO MOVIMENTO
     C*
     C                   MOVE      *BLANKS       LNRECS                         ( ATÉ NOVA REGRA )
     C                   MOVE      '1'           LEREC
     C* ---
     C* - RISCO I ------------------------------------------
     C* ---
     C*
     C* DETERMINA O ENVIO ANTERIOR DO PAGAMENTO A SINISTROS
     C*
     C                   MOVE      R1            KEYPAG
     C     KEYPAG        CHAIN     LPAGORM                            99
     C  N99              GOTO      PAG001
     C*
     C     VALR1         IFNE      *ZEROS                                       <---------+
     C*                                                              I
     C* FORMATA CÓDIGO MOVIMENTO                                     I
     C*                                                              I
     C                   MOVE      '4'           LCMOV                                    I
     C*                                                              I
     C* FORMATA DATA DE MOVIMENTO DO PAGAMENTO  (DATA EMISSÃO)       I
     C*                                                              I
     C                   Z-ADD     *ZEROS        DTY2                                     I
     C                   MOVE      DTMOV         DTY2                                     I
     C                   EXSR      SRDTY2                                                 I
     C                   MOVE      DTY2          LDTEMI                                   I
     C*                                                              I
     C* INICIALIZA CAMPOS                                            I
     C*                                                              I
     C                   Z-ADD     *ZEROS        LDTANU                                   I
     C                   MOVE      *BLANKS       LCANUL                                   I
     C*                                                              I
     C                   MOVE      *ZEROS        WRMSP1            9 0                    I EGRA )
     C                   MOVEL     LRAMO         WRMSP1                                   I
     C                   MOVEL     LMOD          WRMSP2            4 0                    I
     C                   MOVE      LSMOD         WRMSP2                                   I
     C                   MOVE      TIPMOV        WRMSP3            7 0                    I
     C                   MOVEL     WRMSP2        WRMSP3                                   I
     C                   MOVE      WRMSP3        WRMSP1                                   I
     C*                                                              I
     C* PESQUISA/CONVERTE A COBERTURA/DESPESA AFECTA AO PAGAM.       I
     C*                                                              I
     C                   Z-ADD     1             I                 2 0                    I
     C     WRMSP1        LOOKUP    TPAGG(I)                               30              I
     C   30              MOVE      COBDL(I)      LCOBPG                                   I
     C  N30              DO                                                     <---+     I
     C* FORÇA TIPO MOV = 000                                   I     I
     C                   MOVEL     LRAMO         WRMSP1                             I     I
     C                   MOVEL     LMOD          WRMSP2                             I     I
     C                   MOVE      LSMOD         WRMSP2                             I     I
     C                   MOVE      000           WRMSP3                             I     I
     C                   MOVEL     WRMSP2        WRMSP3                             I     I
     C                   MOVE      WRMSP3        WRMSP1                             I     I
     C*                                                        I     I
     C                   Z-ADD     1             I                 2 0              I     I
     C     WRMSP1        LOOKUP    TPAGG(I)                               35        I     I
     C   35              MOVE      COBDL(I)      LCOBPG                             I     I
     C  N35              MOVE      *BLANKS       LCOBPG                             I     I
     C                   END                                                    <---+     I
     C*                                                              I
     C                   MOVE      VALR1         LTREC                                    I
     C     LTREC         IFLT      *ZEROS                                       <---+     I
     C                   MULT      -1            LTREC                              I     I
     C                   MOVE      '2'           LCANUL                             I     I
     C                   MOVE      LDTEMI        LDTANU                             I     I
     C                   MOVE      *BLANKS       LDTEMI                             I     I
     C                   MOVE      '5'           LCMOV                              I     I
     C*********            MOVE '   '     LCOBPG               I     I
     C                   END                                                    <---+     I
     C*                                                              I
     C                   EXCEPT    REGPAG                                                 I
     C                   END                                                    <---------+
     C*
     C     PAG001        TAG
     C* ---
     C* - RISCO II -----------------------------------------
     C* ---
     C*
     C* DETERMINA O ENVIO ANTERIOR DO PAGAMENTO A SINISTROS
     C*
     C                   MOVE      R2            KEYPAG
     C     KEYPAG        CHAIN     LPAGORM                            99
     C  N99              GOTO      PAG002
     C*
     C     VALR3         IFNE      *ZEROS                                       <---------+
     C*                                                              I
     C* FORMATA CÓDIGO MOVIMENTO                                     I
     C*                                                              I
     C                   MOVE      '4'           LCMOV                                    I
     C*                                                              I
     C* FORMATA DATA DE MOVIMENTO DO PAGAMENTO  (DATA EMISSÃO)       I
     C*                                                              I
     C                   Z-ADD     *ZEROS        DTY2                                     I
     C                   MOVE      DTMOV         DTY2                                     I
     C                   EXSR      SRDTY2                                                 I
     C                   MOVE      DTY2          LDTEMI                                   I
     C*                                                              I
     C* INICIALIZA CAMPOS                                            I
     C*                                                              I
     C                   Z-ADD     *ZEROS        LDTANU                                   I
     C                   MOVE      *BLANKS       LCANUL                                   I
     C*                                                              I
     C                   MOVE      *ZEROS        WRMSP1                                   I EGRA )
     C                   MOVEL     LRAMO         WRMSP1                                   I
     C                   MOVEL     LMOD          WRMSP2                                   I
     C                   MOVE      LSMOD         WRMSP2                                   I
     C                   MOVE      TIPMOV        WRMSP3                                   I
     C                   MOVEL     WRMSP2        WRMSP3                                   I
     C                   MOVE      WRMSP3        WRMSP1                                   I
     C*                                                              I
     C* PESQUISA/CONVERTE A COBERTURA/DESPESA AFECTA AO PAGAM.       I
     C*                                                              I
     C                   Z-ADD     1             I                 2 0                    I
     C     WRMSP1        LOOKUP    TPAGG(I)                               30              I
     C   30              MOVE      COBDL(I)      LCOBPG                                   I
     C  N30              DO                                                     <---+     I
     C* FORÇA TIPO MOV = 00                                    I     I
     C                   MOVEL     LRAMO         WRMSP1                             I     I
     C                   MOVEL     LMOD          WRMSP2                             I     I
     C                   MOVE      LSMOD         WRMSP2                             I     I
     C                   MOVE      000           WRMSP3                             I     I
     C                   MOVEL     WRMSP2        WRMSP3                             I     I
     C                   MOVE      WRMSP3        WRMSP1                             I     I
     C*                                                        I     I
     C                   Z-ADD     1             I                 2 0              I     I
     C     WRMSP1        LOOKUP    TPAGG(I)                               35        I     I
     C   35              MOVE      COBDL(I)      LCOBPG                             I     I
     C  N35              MOVE      *BLANKS       LCOBPG                             I     I
     C                   END                                                    <---+     I
     C*                                                              I
     C                   MOVE      VALR3         LTREC                          <---+     I
     C     LTREC         IFLT      *ZEROS                                           I     I
     C                   MULT      -1            LTREC                              I     I
     C                   MOVE      '2'           LCANUL                             I     I
     C                   MOVE      LDTEMI        LDTANU                             I     I
     C                   MOVE      *BLANKS       LDTEMI                             I     I
     C                   MOVE      '5'           LCMOV                              I     I
     C********             MOVE '   '     LCOBPG               I     I
     C                   END                                                    <---+     I
     C*                                                              I
     C                   EXCEPT    REGPAG                                                 I
     C                   END                                                    <---------+
     C*
     C     PAG002        TAG
     C* ---
     C* - RISCO III ----------------------------------------
     C* ---
     C*
     C* DETERMINA O ENVIO ANTERIOR DO PAGAMENTO A SINISTROS
     C*
     C                   MOVE      R3            KEYPAG
     C     KEYPAG        CHAIN     LPAGORM                            99
     C  N99              GOTO      ESRPAG
     C*
     C     VALR45        IFNE      *ZEROS                                       <---------+
     C*                                                              I
     C* FORMATA CÓDIGO MOVIMENTO                                     I
     C*                                                              I
     C                   MOVE      '4'           LCMOV                                    I
     C*                                                              I
     C* FORMATA DATA DE MOVIMENTO DO PAGAMENTO  (DATA EMISSÃO)       I
     C*                                                              I
     C                   Z-ADD     *ZEROS        DTY2                                     I
     C                   MOVE      DTMOV         DTY2                                     I
     C                   EXSR      SRDTY2                                                 I
     C                   MOVE      DTY2          LDTEMI                                   I
     C*                                                              I
     C* INICIALIZA CAMPOS                                            I
     C*                                                              I
     C                   Z-ADD     *ZEROS        LDTANU                                   I
     C                   MOVE      *BLANKS       LCANUL                                   I
     C*                                                              I
     C                   MOVE      *ZEROS        WRMSP1                                   I EGRA )
     C                   MOVEL     LRAMO         WRMSP1                                   I
     C                   MOVEL     LMOD          WRMSP2                                   I
     C                   MOVE      LSMOD         WRMSP2                                   I
     C                   MOVE      TIPMOV        WRMSP3                                   I
     C                   MOVEL     WRMSP2        WRMSP3                                   I
     C                   MOVE      WRMSP3        WRMSP1                                   I
     C*                                                              I
     C* PESQUISA/CONVERTE A COBERTURA/DESPESA AFECTA AO PAGAM.       I
     C*                                                              I
     C                   Z-ADD     1             I                 2 0                    I
     C     WRMSP1        LOOKUP    TPAGG(I)                               30              I
     C   30              MOVE      COBDL(I)      LCOBPG                         <---+     I
     C  N30              DO                                                         I     I
     C* FORÇA TIPO MOV = 00                                    I     I
     C                   MOVEL     LRAMO         WRMSP1                             I     I
     C                   MOVEL     LMOD          WRMSP2                             I     I
     C                   MOVE      LSMOD         WRMSP2                             I     I
     C                   MOVE      000           WRMSP3                             I     I
     C                   MOVEL     WRMSP2        WRMSP3                             I     I
     C                   MOVE      WRMSP3        WRMSP1                             I     I
     C*                                                        I     I
     C                   Z-ADD     1             I                 2 0              I     I
     C     WRMSP1        LOOKUP    TPAGG(I)                               35        I     I
     C   35              MOVE      COBDL(I)      LCOBPG                             I     I
     C  N35              MOVE      *BLANKS       LCOBPG                             I     I
     C                   END                                                    <---+     I
     C*                                                              I
     C                   MOVE      VALR45        LTREC                          <---+     I
     C     LTREC         IFLT      *ZEROS                                           I     I
     C                   MULT      -1            LTREC                              I     I
     C                   MOVE      '2'           LCANUL                             I     I
     C                   MOVE      LDTEMI        LDTANU                             I     I
     C                   MOVE      *BLANKS       LDTEMI                             I     I
     C                   MOVE      '5'           LCMOV                              I     I
     C*********            MOVE '   '     LCOBPG               I     I
     C                   END                                                    <---+     I
     C*                                                              I
     C                   EXCEPT    REGPAG                                                 I
     C                   END                                                    <---------+
     C*
     C     ESRPAG        ENDSR
     C*
     C/EJECT
     C*****************************************************
     C*   FORMATA REEMBOLSOS                              *
     C********************************************    > < =
     C     SRFREE        BEGSR
     C*
     C* LIMPA CAMPOS DE TRABALHO
     C*
     C                   Z-ADD     *ZEROS        RI
     C                   Z-ADD     *ZEROS        RII
     C                   Z-ADD     *ZEROS        RIII
     C                   Z-ADD     *ZEROS        WCOB1
     C                   Z-ADD     *ZEROS        WCOB2
     C                   Z-ADD     *ZEROS        WCOB3
     C*
     C* FORMATA NÚMERO E ESPECIE DE RECIBO
     C*
     C                   MOVE      *BLANKS       LNRECS                         ( ATÉ NOVA REGRA )
     C*
     C     TIPMOV        IFEQ      220
     C*                    MOVE '8'       LEREC            SALVADOS (A PEDIDO)
     C                   MOVE      '9'           LEREC                          REEMBOLSOS
     C                   ELSE
     C                   MOVE      '9'           LEREC                          REEMBOLSOS
     C                   END
     C* ---
     C* - RISCO I ------------------------------------------
     C* ---
     C*
     C* DETERMINA O ENVIO ANTERIOR DA PROVISÃO DO SINISTRO
     C*
     C                   MOVE      R1            KEYPAG
     C     KEYPAG        CHAIN     LPAGORM                            99
     C  N99              GOTO      REE001
     C*
     C     VALR1         IFNE      *ZEROS                                       <--------+
     C*                                                             I
     C* FORMATA CÓDIGO MOVIMENTO                                    I
     C*                                                             I
     C                   MOVE      '4'           LCMOV                                   I
     C*                                                             I
     C* FORMATA DATA DE MOVIMENTO DO PAGAMENTO  (DATA EMISSÃO)      I
     C*                                                             I
     C                   Z-ADD     *ZEROS        DTY2                                    I
     C                   MOVE      DTMOV         DTY2                                    I
     C                   EXSR      SRDTY2                                                I
     C                   MOVE      DTY2          LDTEMI                                  I
     C*                                                             I
     C* INICIALIZA CAMPOS                                           I
     C*                                                             I
     C                   Z-ADD     *ZEROS        LDTANU                                  I
     C                   MOVE      *BLANKS       LCANUL                                  I
     C*                                                             I
     C                   MOVE      R1            RI                                      I
     C                   MOVE      RI            WCOB1                                   I
     C                   MOVEL     RM            WCOB1                                   I
     C*                                                             I
     C* PESQUISA/CONVERTE A COBERTURA AFECTADA PELA PROVISAO        I
     C*                                                             I
     C                   Z-ADD     1             I                 2 0                   I
     C     WCOB1         LOOKUP    RIG(I)                                 30             I
     C   30              MOVE      RIL(I)        LCOBPG                                  I
     C  N30              DO                                                              I
     C                   MOVE      '9'           RI                                      I
     C                   MOVE      RI            WCOB1                                   I
     C                   Z-ADD     1             I                 2 0                   I
     C     WCOB1         LOOKUP    RIG(I)                                 30             I
     C   30              MOVE      RIL(I)        LCOBPG                                  I
     C  N30              MOVE      *BLANKS       LCOBPG                                  I
     C                   END                                                             I
     C*                                                             I
     C                   MOVE      VALR1         LTREC                                   I
     C     LTREC         IFLT      *ZEROS                                                I
     C                   MULT      -1            LTREC                                   I
     C                   MOVE      '2'           LCANUL                                  I
     C                   MOVE      LDTEMI        LDTANU                                  I
     C                   MOVE      *BLANKS       LDTEMI                                  I
     C                   MOVE      '5'           LCMOV                                   I
     C**********           MOVE '   '     LCOBPG                    I
     C                   END                                                             I
     C*                                                             I
     C                   EXCEPT    REGPAG                                                I
     C                   END                                                    <--------+
     C*
     C     REE001        TAG
     C* ---
     C* - RISCO II -----------------------------------------
     C* ---
     C*
     C* DETERMINA O ENVIO ANTERIOR DA PROVISÃO DO SINISTRO
     C*
     C                   MOVE      R2            KEYPAG
     C     KEYPAG        CHAIN     LPAGORM                            99
     C  N99              GOTO      REE002
     C*
     C     VALR3         IFNE      *ZEROS                                       <---------+
     C*                                                              I
     C* FORMATA CÓDIGO MOVIMENTO                                     I
     C*                                                              I
     C                   MOVE      '4'           LCMOV                                    I
     C*                                                              I
     C* FORMATA DATA DE MOVIMENTO DO PAGAMENTO  (DATA EMISSÃO)       I
     C*                                                              I
     C                   Z-ADD     *ZEROS        DTY2                                     I
     C                   MOVE      DTMOV         DTY2                                     I
     C                   EXSR      SRDTY2                                                 I
     C                   MOVE      DTY2          LDTEMI                                   I
     C*                                                              I
     C* INICIALIZA CAMPOS                                            I
     C*                                                              I
     C                   Z-ADD     *ZEROS        LDTANU                                   I
     C                   MOVE      *BLANKS       LCANUL                                   I
     C*                                                              I
     C                   MOVE      R2            RII                                      I
     C                   MOVE      RII           WCOB2                                    I
     C                   MOVEL     RM            WCOB2                                    I
     C*                                                              I
     C* PESQUISA/CONVERTE A COBERTURA AFECTADA PELA PROVISAO         I
     C*                                                              I
     C                   Z-ADD     1             I                 2 0                    I
     C     WCOB2         LOOKUP    RIG(I)                                 30              I
     C   30              MOVE      RIL(I)        LCOBPG                                   I
     C  N30              DO                                                               I
     C                   MOVE      '9'           RI                                       I
     C                   MOVE      RI            WCOB2                                    I
     C                   Z-ADD     1             I                 2 0                    I
     C     WCOB2         LOOKUP    RIG(I)                                 30              I
     C   30              MOVE      RIL(I)        LCOBPG                                   I
     C  N30              MOVE      *BLANKS       LCOBPG                                   I
     C                   END                                                              I
     C*                                                              I
     C                   MOVE      VALR3         LTREC                                    I
     C     LTREC         IFLT      *ZEROS                                                 I
     C                   MULT      -1            LTREC                                    I
     C                   MOVE      '2'           LCANUL                                   I
     C                   MOVE      LDTEMI        LDTANU                                   I
     C                   MOVE      *BLANKS       LDTEMI                                   I
     C                   MOVE      '5'           LCMOV                                    I
     C**********           MOVE '   '     LCOBPG                     I
     C                   END                                                              I
     C*                                                              I
     C                   EXCEPT    REGPAG                                                 I
     C                   END                                                    <---------+
     C*
     C     REE002        TAG
     C* ---
     C* - RISCO III ----------------------------------------
     C* ---
     C*
     C* DETERMINA O ENVIO ANTERIOR DA PROVISÃO DO SINISTRO
     C*
     C                   MOVE      R3            KEYPAG
     C     KEYPAG        CHAIN     LPAGORM                            99
     C  N99              GOTO      ESRREE
     C*
     C     VALR45        IFNE      *ZEROS                                       <---------+
     C*                                                              I
     C* FORMATA CÓDIGO MOVIMENTO                                     I
     C*                                                              I
     C                   MOVE      '4'           LCMOV                                    I
     C*                                                              I
     C* FORMATA DATA DE MOVIMENTO DO PAGAMENTO  (DATA EMISSÃO)       I
     C*                                                              I
     C                   Z-ADD     *ZEROS        DTY2                                     I
     C                   MOVE      DTMOV         DTY2                                     I
     C                   EXSR      SRDTY2                                                 I
     C                   MOVE      DTY2          LDTEMI                                   I
     C*                                                              I
     C* INICIALIZA CAMPOS                                            I
     C*                                                              I
     C                   Z-ADD     *ZEROS        LDTANU                                   I
     C                   MOVE      *BLANKS       LCANUL                                   I
     C*                                                              I
     C                   MOVE      R3            RIII                                     I
     C                   MOVE      RIII          WCOB3                                    I
     C                   MOVEL     RM            WCOB3                                    I
     C*                                                              I
     C* PESQUISA/CONVERTE A COBERTURA AFECTADA PELA PROVISAO         I
     C*                                                              I
     C                   Z-ADD     1             I                 2 0                    I
     C     WCOB3         LOOKUP    RIG(I)                                 30              I
     C   30              MOVE      RIL(I)        LCOBPG                                   I
     C  N30              DO                                                               I
     C                   MOVE      '9'           RI                                       I
     C                   MOVE      RI            WCOB2                                    I
     C                   Z-ADD     1             I                 2 0                    I
     C     WCOB3         LOOKUP    RIG(I)                                 30              I
     C   30              MOVE      RIL(I)        LCOBPG                                   I
     C  N30              MOVE      *BLANKS       LCOBPG                                   I
     C                   END                                                              I
     C*                                                              I
     C                   MOVE      VALR45        LTREC                                    I
     C     LTREC         IFLT      *ZEROS                                                 I
     C                   MULT      -1            LTREC                                    I
     C                   MOVE      '2'           LCANUL                                   I
     C                   MOVE      LDTEMI        LDTANU                                   I
     C                   MOVE      *BLANKS       LDTEMI                                   I
     C                   MOVE      '5'           LCMOV                                    I
     C********             MOVE '   '     LCOBPG                     I
     C                   END                                                              I
     C*                                                              I
     C                   EXCEPT    REGPAG                                                 I
     C                   END                                                    <---------+
     C*
     C     ESRREE        ENDSR
     C*
     C/EJECT
     O****************************************************
     O*  OUTPUT DE REGISTO DE PROCESSO DE REGISTO        *
     O****************************************************
     OLSINORM   EADD         REGPRO
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
     O                       LLIPAG             142                             DATA REABERTURA
     O*
     O                       KEYPRO             376                             NUMERO PROCESSO (CHAVE)
     O                       LDTTRF             384                             DATA DA TRANSFERÊRECIA
     O****************************************************
     O*  OUTPUT DE REGISTO DE PROVISAO PARA SINISTRO     *
     O****************************************************
     OLPRVORM   EADD         REGPRV
     O                       LRAMO                2                             RAMO
     O                       LMOD                 4                             MODALIDADE
     O                       LSMOD                6                             SUBMODADLIDADE
     O                       LANO                10                             ANO SINISNTRO
     O                       LNPROC              15                             NR. RECIBO
     O                       LCOBPV              18                             COBERTURA AFECTADA
     O                       LVLPRV              30 '0           -'             VALOR DA PROVISÃO
     O*                        FILLER    64                               FILLER
     O                       PROPRV              70                             N/PROCESSO SINISTRO
     O                       NLIPRV              72                             NUMERO LINHA
     O*                        FILLER    64                               FILLER
     O                       LDTMOV             356
     O                       LPRVDT             364
     O                       KEYPRV             376                             NUMERO PROCESSO (CHAVE)
     O                       LDTTRF             384                             DATA DA TRANSFERÊRECIA
     O****************************************************
     O*  OUTPUT DE REGISTO DE PAGAMENTO A SINISTRO       *
     O****************************************************
     OLPAGORM   EADD         REGPAG
     O                       LRAMO                2                             RAMO
     O                       LMOD                 4                             MODALIDADE
     O                       LSMOD                6                             SUBMODADLIDADE
     O                       LANO                10                             ANO DO PROCESSO
     O                       LNPROC              15                             NUMERO DO PROCESSO
     O                       LNRECS              20                             NR.REC.SINISTRO (ID)
     O                       LEREC               21                             ESPECIE DE RECIBO
     O                       LDTEMI              29                             DATA DE EMISSAO
     O                       LTREC               43 '0             -'           TOTAL RECIBO (MOVIM.)
     O                       LCANUL              44                             CODIGO ANULACAO
     O                       LDTANU              52                             DATA DE ANULAÇÃO
     O                       LCMOV               53                             CODIGO DE MOVIMENTO
     O                       LCOBPG              56                             COBERTURA AFECTADA
     O*                        FILLER    64                               FILLER
     O                       PROPAG              70                             N/PROCESSO SINISTRO
     O                       NLIPAG              72                             NUMERO LINHA
     O                       NODOC               87
     O*                        FILLER   348                               FILLER
     O                       LDTMOV             356
     O                       LPAGDT             364
     O                       KEYPAG             376                             NUMERO PROCESSO (CHAVE)
     O                       LDTTRF             384                             DATA DA TRANSFERÊRECIA
     O****************************************************
     O*  OUTPUT DE REGISTO DE PROCESSO DE REGISTO        *
     O****************************************************
     OESINORM   EADD         REGPRO
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
     O                       LLIPAG             142                             DATA REABERTURA
     O*
     O                       KEYPRO             376                             NUMERO PROCESSO (CHAVE)
     O                       LDTTRF             384                             DATA DA TRANSFERÊRECIA
     O****************************************************
     O*  OUTPUT DE REGISTO DE PROVISAO PARA SINISTRO     *
     O****************************************************
     OEPRVORM   EADD         REGPRV
     O                       LRAMO                2                             RAMO
     O                       LMOD                 4                             MODALIDADE
     O                       LSMOD                6                             SUBMODADLIDADE
     O                       LANO                10                             ANO SINISNTRO
     O                       LNPROC              15                             NR. RECIBO
     O                       LCOBPV              18                             COBERTURA AFECTADA
     O                       LVLPRV              30 '0           -'             VALOR DA PROVISÃO
     O*                        FILLER    64                               FILLER
     O                       PROPRV              70                             N/PROCESSO SINISTRO
     O                       NLIPRV              72                             NUMERO LINHA
     O*                        FILLER    64                               FILLER
     O                       LDTMOV             356
     O                       LPRVDT             364
     O                       KEYPRV             376                             NUMERO PROCESSO (CHAVE)
     O                       LDTTRF             384                             DATA DA TRANSFERÊRECIA
     O****************************************************
     O*  OUTPUT DE REGISTO DE PAGAMENTO A SINISTRO       *
     O****************************************************
     OEPAGORM   EADD         REGPAG
     O                       LRAMO                2                             RAMO
     O                       LMOD                 4                             MODALIDADE
     O                       LSMOD                6                             SUBMODADLIDADE
     O                       LANO                10                             ANO DO PROCESSO
     O                       LNPROC              15                             NUMERO DO PROCESSO
     O                       LNRECS              20                             NR.REC.SINISTRO (ID)
     O                       LEREC               21                             ESPECIE DE RECIBO
     O                       LDTEMI              29                             DATA DE EMISSAO
     O                       LTREC               43 '0             -'           TOTAL RECIBO (MOVIM.)
     O                       LCANUL              44                             CODIGO ANULACAO
     O                       LDTANU              52                             DATA DE ANULAÇÃO
     O                       LCMOV               53                             CODIGO DE MOVIMENTO
     O                       LCOBPG              56                             COBERTURA AFECTADA
     O*                        FILLER    64                               FILLER
     O                       PROPAG              70                             N/PROCESSO SINISTRO
     O                       NLIPAG              72                             NUMERO LINHA
     O                       NODOC               87
     O*                        FILLER   348                               FILLER
     O                       LDTMOV             356
     O                       LPAGDT             364
     O                       KEYPAG             376                             NUMERO PROCESSO (CHAVE)
     O                       LDTTRF             384                             DATA DA TRANSFERÊRECIA
     O*******************************************************
**              *** TABELA DE CONVERSAO DE RAMOS
01080000               * INCÊNDIO E E.NATUREZA
02010200               * ACIDENTES PESSOAIS
03130300               * RESP. CIVIL PESSOAL
04130500               * RESP. CIVIL COM./IND.
05340100               * RISCOS M.HABITAÇÃO
06160100               * PERDAS DE LUCROS
07090600               * ROUBO VALORES
08090600               * ROUBO RESIDÊNCIAS
09090600               * ROUBO ESTABELECIMENTOS
10010205               * VIAGENS
14300100               * DOENÇA
15010101               * ACIDENTES DE TRABALHO CONTA OUTRÉM
15010102               * ACIDENTES DE TRABALHO CONTA PRÓPRIO
17340700               * EQUIPAMENTO ELECTRÓNICO
18090304               * AVARIA DE MÁQUINAS
19340600               * OBRAS E MONTAGENS
51010301               * OCUPANTES
50310000               * AUTOMÓVEL
55340200               * RISCOS M.ESTABELECIMENTOS
65090400               * CRISTAIS
67070002               * BAGAGEM
68070002               * MARÍTIMO TERRESTRE
69070002               * POSTAL
70070003               * AÉREO
**              *** TABELA DE CONVERSAO COD. COBERTURA DESPESAS ***
010101000911
010101110917
010101116909
010101119918
010200000911
010200116909
010200000911
010301000911
070002000911
070002110930
070002116910
070003000911
070003110930
070003116910
080000000933
080000110930
080000116909
090304000933
090304110930
090400000933
090400110931
090600000933
090600110930
090600116909
130300000933
130300110930
130300116909
130500000933
130500110930
130500116909
160100000933
160100110930
160100116909
320000000911
320000110930
320000116910
340100000933
340100110930
340100116909
340103000933
340103110930
340103116909
340200000933
340200116909
340600000933
340600110930
340700000933
340700110930
**              *** TABELA DE ASSOCIAÇOES DE RISCOS E COBERTURAS ***
011001
012001
013001
014001
019001
021001
022013
023012
024016
025001
026002
027003
029001
031001
039001
041001
042001
049001
051001
052001
053001
059001
061001
062001
063001
069001
071001
072001
079001
081001
082001
089001
091001
092001
099001
100000
109000
140000
140000
149000
151001
152001
159001
171001
172001
173001
179001
181001
182001
183001
189001
191001
199001
501001
502001
503006
504003
505004
506005
507004
508006
509005
511003
512015
512013
551001
552001
553001
559001
651001
659001
