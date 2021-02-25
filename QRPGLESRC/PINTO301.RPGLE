     H DECEDIT(',') DFTNAME(TRSF10) DATEDIT(*DMY/)
     H********************************************************************
     H*  EMPRESA    :  GRUPO MATA.                                       *
     H*  PROGRAMA   :  TRSF10                                            *
     H*  TIPO       :  SRT                                               *
     H*  APLICAÇÃO  :  PRODUÇAO GUARDIAN (EXPLORAÇÃO TODOS OS RAMOS)     *
     H*  FUNÇÃO        - CRIAÇÃO DE FICHEIRO PARA TRANSFERÊNCIA DE       *
     H*                  INFORMAÇÃO (DADOS) PARA A LUSITANIA "VERDE"     *
     H*                                                                  *
     H*  DADOS      :  FICHEIRO DE RECIBOS ( PRODUÇAO + COBRANÇA )       *
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
     F********************************************************************
     F*              RESUMO DA UTILIZAÇÃO DOS INDICADORES                *
     F********************************************************************
     F*                                                                  *
     F*  01 - IDENTIFICA REGISTO DE HISTORICO DE RECIBOS  - (GREGA)      *
     F*  02 - IDENTIFICA REGISTO DE FICHEIRO DE COBRANÇAS - (GCOBGR)     *
     F*  01 - IDENTIFICA REGISTO DE HISTORICO DE RECIBOS  - (GREGA)      *
     F*                                                                  *
     F*  20 - LOKUP TABELA DE RAMOS (CONVERSAO)                          *
     F*  21 - LOKUP TABELA DE ESPECIE DE RECIBOS (CONVERSAO)             *
     F*                                                                  *
     F********************************************************************
     FGREGA     IP   F  448        DISK
     FGREGAX    IF   F  448    24AIDISK                                         KEYLOC(EXTK)
     FGFCOBGR   IF   F  250     9AIDISK    KEYLOC(2)
     FGCOBCOLF  IF   F  350     9AIDISK    KEYLOC(322)
     FGBASEGRL  IF   F  700     9AIDISK    KEYLOC(1)
     FLFRECX    IF   F  384    25AIDISK    KEYLOC(352)
     FLFRECCX   IF   F  384    25AIDISK    KEYLOC(352)
     FGREGAPOL  IF   F  100     9AIDISK    KEYLOC(3)
     FCSFTAB    IF   F   37     3AIDISK    KEYLOC(1)
     FLFREC     O  A F  384    25AIDISK    KEYLOC(352)
     FLFRECC    O  A F  384    25AIDISK    KEYLOC(352)
     FEFREC     O  A F  384    25AIDISK    KEYLOC(352)
     FEFRECC    O  A F  384    25AIDISK    KEYLOC(352)
     FEFRECT    O  A F  384    25AIDISK    KEYLOC(352)
     FEFRECCT   O  A F  384    25AIDISK    KEYLOC(352)
     FMERROS    O    F   80        PRINTER OFLIND(*INOF)
     D********************************************************************
     D RMG             S              2  0 DIM(23) CTDATA PERRCD(1)             RAMOS GUARDIAN VS LUSITANIA
     D RML             S              2  0 DIM(23) ALT(RMG)
     D ERG             S              1  0 DIM(6) CTDATA PERRCD(1)              TIPO RECIBO GA VS LUSITANIA
     D ERL             S              1  0 DIM(6) ALT(ERG)
     D ARG             S              1  0 DIM(2) CTDATA PERRCD(1)              ANUL.RECIBO GA VS LUSITANIA
     D ARL             S              2  0 DIM(2) ALT(ARG)
     D RJM             S              9  0 DIM(23) CTDATA PERRCD(1)
     D TMD             S              2  0 DIM(12) CTDATA PERRCD(1) ASCEND      DIA/MES DATA TERMO
     D TDM             S              2  0 DIM(12) ALT(TMD)
     D TABM            S              2  0 DIM(12) CTDATA PERRCD(12) ASCEND     MESES/DIAS       GS0100
     D TABD            S              2  0 DIM(12) ALT(TABM)
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
     D* ANO E MES E DIA DE MOVIMENTO (SELECÇAO)
     D  UDTMOV                 1      6  0
     D  UAMMOV                 1      4  0
     D  UAAMOV                 1      2  0
     D  UMMMOV                 3      4  0
     D  UDDMOV                 5      6  0
     D********************************************************************
     D                 DS
     D  DTCRI                  1      6  0
     D  DDCRI                  1      2  0
     D  MMCRI                  3      4  0
     D  AACRI                  5      6  0
     D********************************************************************
     D                 DS
     D  WDTCRI                 1      6  0
     D  WAACRI                 1      2  0
     D  WMMCRI                 3      4  0
     D  WDDCRI                 5      6  0
     D********************************************************************
     D                 DS
     D  UDTTRF                15     26  0
     D  UATRF                 21     22  0
     D  UMTRF                 23     24  0
     D  UDTRF                 25     26  0
     D********************************************************************
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
     D  LKEY                   1     25
     D  GHWRR                  1      2  0
     D  GHAPOL                 3      9  0
     D  GHTREG                10     10  0
     D  GHNREC                11     19  0
     D  GHNSEQ                20     24  0
     D  LCDMOV                25     25  0
     D                 DS
     D  DATA$                  1      8  0
     D  AA$                    1      4  0
     D  MM$                    5      6  0
     D  DD$                    7      8  0
     I********************************************************************
     I* FICHEIRO HISTÓRICO DE RECIBOS - GUARDIAN                         *
     I*                                                                  *
     I* CHAVE = NREC/TREG/RR/APOL/NSEQ  (NSEQ=00000)                     *
     I*         (NSEQ=00000) SE RECIBO EXISTE NA COBRANÇA*               *
     I********************************************************************
     IGREGA     NS  01
     I                                  1    9 0GHNREC                          NUMERO RECIBO  (AAMM99999)
     I                                  1    4 0GHAMR                           ANO E MES DO RECIBO
     I                                  1    2 0GHAREC
     I                                  3    4 0GHMREC
     I                                  5    5 0CTLREC
     I                                  5    9 0GHSREC
     I*                                                                      AA = ANO MOVIMENTO
     I*                                                                      MM = MES MOVIMENTO
     I*                                                                   99999 = NÚMERO SEQUENCIAL
     I                                 10   11 0GHRR                            RAMO
     I                                 10   18 0GHRRAP                          RAMO / APÓLICE
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
     I                                192  192 0GHFRAC                          FRACCIONAMENTO
     I                                193  198 0GHDTF                           DATA FIM RECIBO  (DDMMAA)
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
     I                                407  407 0GHOCU                           N. SEQUENCIAL
     I                             P  408  410 0GHCAOC                          N. SEQUENCIAL
     I                             P  411  415 2GHPROC                          N. SEQUENCIAL
     I                                416  416 0GHQIV                           N. SEQUENCIAL
     I                             P  417  419 2GHPR                            N. SEQUENCIAL
     I                                420  420 0GHPRIV                          N. SEQUENCIAL
     I****                                P 421 4232GHPRPU                N. SEQUENCIAL
      *
     I                                421  421  TIPCS                           COSEGURO=2
      *
     I                                426  430 0GHNSEQ                          N. SEQUENCIAL
     I                                431  434 0AMMOV                           ANO MOVIMENTO
     I                                431  432 0GHAAMV                          ANO MOVIMENTO
     I                                433  434 0GHMMMV                          MÊS MOVIMENTO
     I                                435  440 0DTCRI                           DATA ALTERAÇÃO
     I                                441  448  GHUSR                           UTILIZADOR
     I*
     I****************************************************
     I* FICHEIRO DE COBRANÇAS - GUARDIAN                 *
     I* CHAVE = GCNREC                                   *
     I****************************************************
     IGFCOBGR   NS  02
     I                                  1    1 0SIT                             SITUAÇÃO RECIBO:
     I*                                                                     0 = ABERTO
     I*                                                                     1 = COBRADO
     I*                                                                     2 = SUSPENSO
     I*                                                                     3 = ANULADO
     I                                  2   10 0NUMREC                          NUMERO RECIBO
     I                                 11   11 0TR                              TIPO RECIBO:
     I*                                                                     0 = NOVO
     I*                                                                     1 = ADICIONAL
     I*                                                                     2 = CONTINUADO
     I*                                                                     3 = ANULAÇÃO
     I*                                                                     5 = ESTORNO
     I                             P   12   16 2TREC                            VALOR TOTAL RECIBO
     I                                 17   22 0DTEMIS                          DATA EMISSÃO AAMMDD
     I                                 23   28 0DTI                             DATA INICIO  AAMMDD
     I                                 29   34 0DTF                             DATA FIM     AAMMDD
     I                                 35   37 0GCJMM1                          DATA AVISO SUSPENSÃO
     I                                 38   40 0GCJMM2                          DATA AVISO SUSPENSÃO
     I                                 41   41  TIPCOB                          TIPO COBRANÇA:
     I*                                                                     C = CAIXA
     I*                                                                     M = MEDIADOR
     I                                 42   42 0FORCOB                          FORMA COBRANÇA:
     I*                                                                     1 = CHEQUE
     I*                                                                     2 = TRANSF BANCARIA
     I*                                                                     3 = VALE POSTAL
     I*                                                                     4 = DINHEIRO
     I*                                                                     5 = DIVERSA
     I*                                                                     6 = ENCONTRO CONTAS
     I*                                                                     8 = PAG.ESTOR.DINHEI
     I*                                                                     9 = PAG.ESTOR.CHEQUE
     I                                 43   48 0DTPAG                           DATA PAGAMTO AAMMDD
     I                                 49   55 0REFA                            REFª COBRANÇA (AANNNNN
     I*                                                                        AA = ANO COBRANÇA
     I*                                                                     NNNNN = Nº ORDEM
     I                                 56   59 0GCNDIA
     I                                 60   65 0DTLIM                           DATA LIMITE COBRANÇA
     I                                 66   69 0MED1GC                          MEDIADOR C/ COBRANÇA
     I                                 70   73 0MED2GC                          MEDIADOR S/ COBRANÇA
     I                             P   74   78 2CMED                            COMISSÃO MEDIAÇÃO
      *
     I                                 79   79  GCTCS
      *
     I                                 80   81 0RM                              RAMO
     I                                 82   88 0AP                              APOLICE
     I                                 89   93 0NSEG                            NUMERO SEGURADO
     I                                 94  133  SEG                             NOME SEGURADO
     I                                134  173  MOR                             MORADA
     I                             P  174  177 0CMOR1                           CODIGO POSTAL
     I                                178  197  CMOR2                           LOCALIDADE
     I                                198  198  FLISTA                          FLAG LISTAGEM MOVIMENT
     I*                                                                     * = LISTADO (NO DIA
     I                                200  200  FEMIS                           FLAG CARTA AVS-SUSP.
     I*                                                                     E = CARTA EMITIDA
     I                                201  206 0GCDCM                           DATA EMISSÃO CARTA MED.
     I*                                                                   DE RECIBOS +30 DIAS
     I                                207  207 0GCFL3                           FLAG EMISSÃO CARTA SEG.
     I*                                                                     S = EMITE CARTA
     I*                                                                     N = NÃO EMITE CARTA
     I                                208  213 0GCDCS                           DATA EMISSÃO CARTA SEG.
     I*                                                                   DE RECIBOS +45 DIAS
     I                                214  214 0GCFL4                           FLAG ANULAÇÃO CONTRATO
     I*                                                                     S = ANULA CONTRATO
     I*                                                                     N = NÃO ANULA CONTRATO
     I                                215  220 0GCDCA                           DATA ANULAÇÃO CONTRATO
     I                                221  222  GCCANC                          CANAL DE COBRANÇA
     I*                                                                       01 = TESOURARIA
     I*                                                                       99 = AGENTE
     I                                223  228 0GCDACC                          DATA ALTER. CANAL COB.
     I                                229  236  GCUACC                          USERID "      "    "
     I*
     I                                250  250  GCTCOM
     I********************************************************************
     I* CHAVE = NREC/TREG/RR/APOL/NSEQ                   (NSEQ=00000)    *
     I*         (NSEQ=00000) SE RECIBO EXISTE NA COBRANÇA                *
     I********************************************************************
     IGREGAX    NS  03
     I                                  1    9 0GXNREC                          NUMERO RECIBO  (AAMM99999)
     I                                  1    4 0GXAMR                           ANO E MES DO RECIBO
     I                                  1    2 0GXAREC
     I                                  3    4 0GXMREC
     I                                  5    9 0GXSREC
     I*                                                                      AA = ANO MOVIMENTO
     I*                                                                      MM = MES MOVIMENTO
     I*                                                                   99999 = NÚMERO SEQUENCIAL
     I                                 10   11 0GXRR                            RAMO
     I                                 10   18 0GXRRAP                          RAMO / APÓLICE
     I                                 12   18 0GXAPOL                          APÓLICE
     I                                 19   58  GXSEG                           NOME DO SEGURADO
     I                                 59   98  GXMOR                           MORADA DO SEGURADO
     I*                                      99 101 FILLER                C.POSTAL MORADA
     I                                102  121  GXCPM2                          LOCALIDADE
     I                                122  161  GXLCB                           LOCAL DE COBRANÇA
     I*                                     162 164 FILLER                C.POSTAL LOCAL DE COBRANÇA
     I                                165  181  GXCPC2                          LOCALIDADE (LOC. COBRANÇA)
     I                             P  182  184 0GXNCL                           NÚMERO CLIENTE (SEGURADO)
     I                                185  185 0GXTREG                          TIPO RECIBO:
     I*                                                                     0 = NOVO
     I*                                                                     1 = ADICIONAL
     I*                                                                     2 = CONTINUADO
     I*                                                                     3 = ANULAÇÃO DE ESTORNO
     I*                                                                     4 = ANULADO
     I*                                                                     5 = ESTORNO
     I                                186  191 0GXDTI                           DT INICIO RECIBO (DDMMAA)
     I                                192  192 0GXFRAC                          FRACCIONAMENTO
     I                                193  198 0GXDTF                           DATA FIM RECIBO  (DDMMAA)
     I                             P  199  204 0GXCTDP                          CAPITAL DANOS PRÓPRIOS OU
     I*                                                                   CAPITAL TOT.SEGURO (VALOR)
     I                             P  205  207 0GXCTRC                          CAPITAL R.CIVIL    (VALOR)
     I                             P  208  212 2GXPRS                           PRÉMIO SIMPLES     (VALOR)
     I                             P  213  216 2GXBONU                          BÓNUS              (VALOR)
     I                             P  217  220 2GXENC7                          ENCARGOS           (VALOR)
     I                             P  221  222 2GXCSTA                          CUSTO DA APÓLICE   (VALOR)
     I                             P  223  224 2GXCRC                           C.R.C.             (VALOR)
     I* DESCONTINUADO                     P 225 2260GXCV                  CUSTO C.I.S.A      (VALOR)
     I                                225  225  GXPJUR                          FLAG P.JURIDICA
     I                                226  226  GXCR1                           CÒDIGO RISCO (SE CR1=2 QIV)
     I                             P  227  230 2GXSELO                          SELOS              (VALOR)
     I                             P  231  234 2GXSNA                           SNA  (I.N.E.M.)    (VALOR)
     I                             P  235  238 2GXSNB                           S.N.B.             (VALOR)
     I                             P  239  243 2GXTREC                          TOTAL DO RECIBO    (VALOR)
     I                                244  244 0GXISEN                          CODIGO DE ISENÇÃO:
     I*                                                                     0 = SEM ISENÇÃO
     I*                                                                     1 = ISENTO DE ENCARGOS
     I*                                                                     2 = ISENTO DE SELOS
     I*                                                                     3 = ISENTO DE C.APÓLICE
     I*                                                                     4 = ISENTO DE C.I.S.A
     I                                245  245 0GXCEMI                          CÓDIGO DE EMISSÃO
     I*                                                                     1 = UMA  CÓPIA
     I*                                                                     2 = DUAS CÓPIAS
     I*                                                                     3 = TRÊS COPIAS
     I                             P  246  247 0GXCONC                          CONCELHO
     I                                248  250 0GXMEDC                          MEDIADOR COM COBRANÇA
     I                             P  251  255 2GXCOM1                          COMISSÃO MED. C/COBRANÇA
     I                             P  256  258 0GXMED2                          MEDIADOR SEM COBRAMNÇA
     I                             P  259  263 2GXCOM2                          COMISSÃO MED. S/COBRANÇA
     I                                264  277  GXMRC1                          MARCA 1º VEÍCULO
     I                                278  288  GXMAT1                          MATRICULA 1º VEÍCULO
     I                                289  302  GXMRC2                          MARCA 2º VEICULO
     I                                303  313  GXMAT2                          MATRICULA 2º VEÍCULO
     I                                314  314  GXLH1                           RISCO 1
     I                                315  359  GXRISC                          OBJECTO OU PESSOA SEGURA
     I*                                                                   OU LOCAL DO RISCO
     I                             P  360  364 2GXPRRC                          PRÉMIO R.CIVIL
     I                             P  365  369 2GXPRDP                          PRÉMIO D.PRÓPRIOS
     I                                370  370 0GXCATV                          CATEGORIA DO VEÍCULO
     I                                371  371  GXVIAG                          ASSISTENCIA VIAGEM
     I                                372  372  GXFIX                           SE     GHFIX = '*'
     I*                                                                   ENTÃO  REG.TRNSFERIDO
     I*                                                                          PARA COBRANCAS
     I                             P  373  376 2GXVFRC                          FRACCIONAMENTO     (VALOR)
     I                             P  377  380 2GXFGA                           FGA/FAT            (VALOR)
     I                             P  381  386 0GXCAPF                          CAPITAL PARA FAT
     I*
     I                             P  399  402 0GXCPM1                          C.POSTAL MORADA
     I                             P  403  406 0GXCPC1                          C.POSTAL LOCAL DE COBRANÇA
     I*
     I                                407  407 0GXOCU                           N. SEQUENCIAL
     I                             P  408  410 0GXCAOC                          N. SEQUENCIAL
     I                             P  411  415 2GXPROC                          N. SEQUENCIAL
     I                                416  416 0GXQIV                           N. SEQUENCIAL
     I                             P  417  419 2GXPR                            N. SEQUENCIAL
     I                                420  420 0GXPRIV                          N. SEQUENCIAL
     I                             P  421  423 2GXPRPU                          N. SEQUENCIAL
     I*                                                                   FILLER RESERV.INFORM.NOVA
     I                                426  430 0GXNSEQ                          N. SEQUENCIAL
     I                                431  432 0GXAAMV                          ANO MOVIMENTO
     I                                433  434 0GXMMMV                          MÊS MOVIMENTO
     I                                435  440 0GXDALT                          DATA ALTERAÇÃO
     I                                441  448  GXUSR                           UTILIZADOR
     I********************************************************************
     IGCOBCOLF  NS
     I                                322  330 0ANREC                           DATA ANULAÇÃO CONTRATO
     I********************************************************************
     IGBASEGRL  NS
     I                                  1    9 0RRAPOL                          APOLICE
     I                                 10   10  REF
     I                                178  178 0SITAP                           SITUAÇÃO APÓLICE
     I                                206  206 0TALT                            TIPO ALTERAÇÃO
     I                                207  208 0DDALT                           DIA ALTERAÇÃO
     I                                209  210 0MMALT                           MES ALTERAÇÃO
     I                                211  212 0AAALT                           ANO ALTERAÇÃO
     I                                213  214 0MALT                            MOTIVO ALTERAÇÃO
     I*
     I********************************************************************
     I* FICHEIRO HISTÓRICO DE RECIBOS - A ENVIAR À LUSITANIA             *
     I* CHAVE = NREC/TREG/RR/APOL/NSEQ/CDMOV             (NSEQ=00000)    *
     I*         (NSEQ=00000) SE RECIBO EXISTE NA COBRANÇA                *
     I********************************************************************
     ILFRECX    NS
     I*
     I********************************************************************
     I* FICHEIRO HISTÓRICO DE RECIBOS - A ENVIAR À LUSITANIA             *
     I* CHAVE = NREC/TREG/RR/APOL/NSEQ/CDMOV             (NSEQ=00000)    *
     I*         (NSEQ=00000) SE RECIBO EXISTE NA COBRANÇA                *
     I********************************************************************
     ILFRECCX   NS
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
     I                                 81   82 0RAAREG                          DATA REGISTO  (AMD)
     I                                 83   84 0RMMREG                          DATA REGISTO  (AMD)
     I                                 85   86 0RDDREG                          DATA REGISTO  (AMD)
     I                                 87   92 0RDTANU                          DATA ANULAÇÃO (AMD)
     I                                 87   88 0RAAANU                             ANO
     I                                 89   90 0RMMANU                             MES (ANULAÇÃO)
     I                                 91   92 0RDDANU                             DIA
     I                                 93   98 0RDTPRO                          DATA PROCESS. (AMD)
     I                                 93   94 0RAAPRO                             ANO
     I                                 95   96 0RMMPRO                             MES (ANULAÇÃO9
     I                                 97   98 0RDDPRO                             DIA
     I*                                      99 100 FILLER                DISPONÍVEL
     I********************************************************************
     I* FICHEIRO DE MEDIADORES - COSEGURO                                *
     I********************************************************************
     I*
     ICSFTAB    NS
     I                                  4    7 0NUMCS                           ACTIVIDADE REGISTO
     C*
     C* IGNORA REGISTO APENAS A PARTIR DO ANO 2002.
     C* (IGNORA RECIBOS DE MOVIMENTO DE ACERTO DE COMISSÕES AOS MED.)
     C*
      *
     C                   Z-ADD     DDCRI         WDDCRI
     C                   Z-ADD     MMCRI         WMMCRI
     C                   Z-ADD     AACRI         WAACRI
      *
     C     WDTCRI        IFGT      UDTMOV
     C                   GOTO      FIM
     C                   END
      *
     C**         GHAMR     IFGT UAMMOV
     C**                   GOTO FIM
     C**                   END
      *
     C**         GHSREC    IFGE 80000
     C**         WDTCRI    IFGE 040101
     C**                   GOTO FIM
     C**                   END
     C**                   END
      *
     C**         CTLREC    IFEQ 8
     C**                   GOTO FIM
     C**                   ELSE
     C**         CTLREC    IFEQ 9
     C**                   GOTO FIM
     C**                   END
     C**                   END
      *
     C     GHTREC        IFLE      .01
     C***        GHCOM1    IFLE 0
     C     GHRRAP        LOOKUP    RJM                                    88
     C  N88              GOTO      FIM
     C***                  END
     C                   END
      *
     C**         GHTREC    IFGT 0
     C**         GHPRS     IFLE .01
     C**         GHCOM1    IFLE 0
     C**         GHAMR     IFGE 0201
     C**                   GOTO FIM
     C**                   END
     C**                   END
     C**                   END
     C**                   END
      *
     C*
     C* REJEITA OS RECIBOS DE CO-SEGURO COBRADOS.
     C*
     C     GHNREC        CHAIN     GFCOBGR                            90
     C  N90              DO
     C     GCTCS         IFNE      ' '
     C     SIT           IFEQ      1
     C                   GOTO      FIM
     C                   END
     C                   END
     C                   END
     C*
     C* REJEITA OS RECIBOS DE ANULAÇÃO E DE ANULAÇÃO ESTORNO.
     C*
     C     GHTREG        IFEQ      3
     C                   GOTO      FIM
     C                   ELSE
     C     GHTREG        IFEQ      4
     C                   GOTO      FIM
     C                   ELSE
     C                   END
     C                   END
     C*
     C* REJEITA OS RECIBOS DE OCUPANTES INCLUIDOS NO AUTOMÓVEL.
     C* REJEITA OS RECIBOS DE RC+DP ; A.VIAGEM ; P.JURIDICA.
     C*
     C     GHRR          IFEQ      51
     C     GHOCU         IFGT      0
     C                   GOTO      FIM
     C                   ELSE
     C     GHAPOL        IFLT      9940000
     C                   GOTO      FIM
     C                   END
     C                   END
     C                   END
      *
     C     GHRR          IFEQ      52
     C                   GOTO      FIM
     C                   END
     C     GHRR          IFEQ      53
     C                   GOTO      FIM
     C                   END
     C     GHRR          IFEQ      54
     C                   GOTO      FIM
     C                   END
      *----
      * CAMPOS COM VALOR NEGATIVO NÃO SÃO PROCESSADOS
      *----
     C                   SETOFF                                       60
     C                   MOVEL     *BLANKS       CAMPO
     C                   MOVEL     *BLANKS       DCAMPO
     C     GHPRS         IFLT      0                                            - PREMIO
     C                   SETON                                        60          SIMPLES
     C                   ELSE
     C     GHFGA         IFLT      0                                            - FGA
     C                   SETON                                        60
     C                   ELSE
     C     GHCRC         IFLT      0                                            - CUSTO
     C                   SETON                                        60          C.V.
     C                   ELSE
     C     GHCSTA        IFLT      0                                            - CUSTO
     C                   SETON                                        60          APÓLICE
     C                   ELSE
     C     GHSELO        IFLT      0                                            - SELOS
     C                   SETON                                        60
     C                   ELSE
     C     GHSNA         IFLT      0                                            - I.N.E.M.
     C                   SETON                                        60
     C                   ELSE
     C     GHSNB         IFLT      0                                            - S.N.B.
     C                   SETON                                        60
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
      *
     C   60              DO
     C                   Z-ADD     1             LCDMOV
     C     LKEY          CHAIN     LFRECX                             90
     C  N90              GOTO      FIM
     C                   Z-ADD     3             LCDMOV
     C     LKEY          CHAIN     LFRECX                             90
     C  N90              GOTO      FIM
     C                   Z-ADD     4             LCDMOV
     C     LKEY          CHAIN     LFRECX                             90
     C  N90              GOTO      FIM
      *
     C     FLAGC         IFEQ      0
     C                   EXCEPT    CAB1
     C                   Z-ADD     1             FLAGC
     C                   END
     C   OF              EXCEPT    CAB1
     C                   EXCEPT    ERRO01
     C                   GOTO      FIM
     C                   END
      *
     C********************************************************************
     C* DETERMINA DATA DE TRANSFERÊNCIA                                  *
     C********************************************************************
     C*
     C     FLAG          IFEQ      0                                            <-----------+
     C                   TIME                    UDTHH                                      I
     C                   Z-ADD     1             FLAG              1 0                      I
     C*                                                                I
     C* FORMATA DATA DO MOVIMENTO - DATA DE SELECÇÃO                   I
     C*                                                                I
     C**                   MOVE UDDMOV    UDMOV                        I
     C**                   MOVE UMMMOV    UMMOV                        I
     C**                   MOVE UAAMOV    UAMOV                        I
     C*                                                                I
     C                   Z-ADD     *ZEROS        DTY2              8 0                      I
     C                   MOVE      UDTMOV        DTY2                                       I
     C                   EXSR      SRDTY2                                                   I
     C                   MOVE      DTY2          LDTMOV            8 0                      I
     C     LDTMOV        IFGT      *ZEROS                                                   I
     C                   MOVE      *ZEROS        DATA$                                      I
     C                   MOVE      LDTMOV        DATA$                                      I
     C                   EXSR      VDATA                                                    I
     C                   MOVE      DATA$         LDTMOV                                     I
     C                   END                                                                I
     C*                                                                I
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
     C     LDTTRF        IFGT      *ZEROS                                                   I
     C                   MOVE      *ZEROS        DATA$                                      I
     C                   MOVE      LDTTRF        DATA$                                      I
     C                   EXSR      VDATA                                                    I
     C                   MOVE      DATA$         LDTTRF                                     I
     C                   END                                                                I
     C*                                                                I
     C                   END                                                    <-----------+
     C*
     C* IMPRIME CABEÇALHO DE MAPA DE ERROS
     C*
     C     FLAGC         IFEQ      0
     C                   EXCEPT    CAB1
     C                   Z-ADD     1             FLAGC             1 0
     C                   END
     C   OF              EXCEPT    CAB1
     C*
     C* LIMPA CAMPOS DE OUTPUT ( NOVA DEFINIÇAO )
     C*
     C                   MOVE      *BLANKS       LRAMO             2            * CODIGO DE RAMO
     C                   MOVE      *BLANKS       LAPOL             7            * NR. APOLICE
     C                   MOVE      *BLANKS       LSEG              6            * NR. SEGURADO
     C                   MOVE      *BLANKS       LNREC             6            * NR. RECIBO
     C                   MOVE      *BLANKS       LEREC             1            * ESPECIE DE RECIBO
     C*
     C                   Z-ADD     *ZEROS        LCAP             13 2          * CAPITAL
     C                   Z-ADD     *ZEROS        LCAPF            13 2          * CAPITAL PARA FAT
     C                   Z-ADD     *ZEROS        LPCOM            11 2          * PREMIO COMERCIAL
     C                   Z-ADD     *ZEROS        LFGA             11 2          * FUNDO GARANTIA AUTOMÓVEL
     C                   Z-ADD     *ZEROS        LPR               5 2          * PREVENÇÃO RODOVIÁRIA
     C                   Z-ADD     *ZEROS        LFFAT            11 2          * FUNDOS/FAT
     C                   Z-ADD     *ZEROS        LCRC             11 2          * VALOR CARTA/CARTAO
     C                   Z-ADD     *ZEROS        LCUSTA           11 2          * VALOR APOL/ACTA
     C                   Z-ADD     *ZEROS        LSELO            11 2          * VALOR I. SELO
     C                   Z-ADD     *ZEROS        LINEM            11 2          * VALOR I.N.E.M
     C                   Z-ADD     *ZEROS        LSNB             11 2          * VALOR S.N.B.
     C                   Z-ADD     *ZEROS        LTREC            11 2          * VALOR TOTAL RECIBO
     C                   Z-ADD     *ZEROS        LPCOMI           11 2          * PRÉMIO PARA COMISSÕES
     C*
     C                   Z-ADD     *ZEROS        LPFRAC           11 2          * VALOR PREMIO FRACIONAM.
     C                   Z-ADD     *ZEROS        LCOMI            11 2          * VALOR COMISSAO JOSÉ MATA
     C                   Z-ADD     *ZEROS        LVFRAC           11 2          * VALOR DO FRACIONAMENTO
     C                   Z-ADD     *ZEROS        LCOMLI           11 2          * COM.LIDER
     C                   MOVE      *BLANKS       LCOSLI            4            * N.COS.LIDER
     C*
     C                   Z-ADD     *ZEROS        LDTEMI            8 0          * DATA DE EMISSÃO
     C                   Z-ADD     *ZEROS        LDTINI            8 0          * DATA INICIO
     C                   Z-ADD     *ZEROS        LDTERM            8 0          * DATA TERMO
     C                   Z-ADD     *ZEROS        LCANUL            2 0          * CÓDIGO DE ANULAÇÃO
     C                   Z-ADD     *ZEROS        LDTANU            8 0          * DATA DE ANULAÇÃO
     C*
     C                   MOVE      *BLANKS       LNCV1             6            * NÚMERO DE CARTÃO - 01
     C                   MOVE      *BLANKS       LNCV2             6            * NÚMERO DE CARTAO - 02
     C*
     C                   MOVE      *BLANKS       LCDMOV            1 0          * CODIGO MOVIMENTO
     C*
     C* LIMPA CAMPOS DE NOVOS NO LAYOUT
     C*   (A.VIAGEM)
     C                   MOVE      *BLANKS       LVIAG             1
     C                   MOVE      *ZEROS        LPRSVG           11 2
     C                   MOVE      *ZEROS        LRESVG           11 2
     C                   MOVE      *ZEROS        LPRTVG           11 2
     C                   MOVE      *ZEROS        LPRVGS           11 2
     C*
     C* LIMPA CAMPOS DE NOVOS NO LAYOUT
     C*   (P.JURIDICA)
     C                   MOVE      *BLANKS       LPJUR             1
     C                   MOVE      *ZEROS        LPRSPJ           11 2
     C                   MOVE      *ZEROS        LRESPJ           11 2
     C                   MOVE      *ZEROS        LPRTPJ           11 2
     C                   MOVE      *ZEROS        LPRPJS           11 2
     C*
     C* LIMPA CAMPOS DE TRABLHO
     C*
     C                   MOVE      *ZEROS        DP                9 0
     C                   MOVE      *ZEROS        RC                9 0
     C                   MOVE      *ZEROS        KEY              24
     C*
     C                   Z-ADD     *ZEROS        WDTI
     C                   Z-ADD     *ZEROS        WDTF
     C                   Z-ADD     *ZEROS        WDTEMI
     C                   Z-ADD     *ZEROS        WDTPAG
     C                   Z-ADD     *ZEROS        WDTANU
     C                   Z-ADD     *ZEROS        WDTINI
     C*
     C* ------------------------------------------------------------------
     C*  ROTINA DE VALIDAÇÃO DE CONTEÚDO DO VALOR DOS CAMPOS             -
     C* ------------------------------------------------------------------
     C*                                                                ---
     C                   EXSR      SRVVL
     C   60              EXCEPT    ERRO01
     C*
     C* ------------------------------------------------------------------
     C*  VALIDA SE DATA DE TRANSFERÊNCIA PARA PRODUÇÃO > DATA SELECÇÃO   -
     C* ------------------------------------------------------------------
     C*                                                                ---
     C*          GHMMMV    IFGT UMMMOV                                   -
     C*                    GOTO FIM                                      -
     C*                    END
     C*
     C* ------------------------------------------------------------------
     C*  REGRA DE COMPOSIÇAO DE NÚMERO DE SEGURADO                       -
     C* ----                                                           ---
     C*   *) -  ANTECEDER O NUMERO DE SEGURADO COM UM "9"                -
     C*                                                                  -
     C* ------------------------------------------------------------------
     C*  REGRA DE COMPOSIÇAO DE NÚMERO DE RECIBO                         -
     C* ----                                                           ---
     C*   *) -  ANTECEDER O NUMERO DE SEQUENCIAL "AAMMNSEQ" DA NUMERAÇAO -
     C*         DO RECIBO COM UM 8.                                      -
     C*                                                                  -
     C* ------------------------------------------------------------------
     C*  REGRA A APLICAR NA ATRIBUIÇÃO DA NUMERAÇÃO ÀS CARTAS VERDES     -
     C* ----                                                           ---
     C*   SE MATRICULA 1º VEÍCULO ESTÁ PREENCHIDA CAMPO LNCV1 = 999999   -
     C*   SE MATRICULA 2º VEÍCULO ESTÁ PREENCHIDA CAMPO LNCV1 = 000000   -
     C* ----                                                           ---
     C*                                                                  -
     C* ------------------------------------------------------------------
     C*
     C* CONVERTE RAMOS  ( RAMO GUARDIAN VS LUSITANIA ).
     C*
     C                   Z-ADD     GHRR          GHWRR
     C*
     C* CONVERTE RAMOS  ( RAMO GUARDIAN VS LUSITANIA ).
     C*
     C                   Z-ADD     1             I                 2 0
     C     GHRR          LOOKUP    RMG(I)                                 20
     C   20              MOVE      RML(I)        LRAMO
     C  N20              MOVE      *BLANKS       LRAMO
     C*
     C                   MOVE      GHAPOL        LAPOL
     C*
     C* COMPOE CÓDIGO PESSOA
     C*
     C                   MOVEL     9             LSEG
     C                   MOVE      GHNCL         LSEG
     C*
     C* COMPOE NÚMERO DO RECIBO
     C*
     C                   MOVEL     8             LNREC
     C                   MOVE      GHSREC        LNREC
     C*
     C* PESQUISA SE RECIBO É APENAS RECIBO DE COMISSAO JM
     C* << RECIBO SÓ DE COMISSÃO FORMATAÇÃO É ESPECIFICA >>
     C*
     C                   Z-ADD     1             I
     C     GHRRAP        LOOKUP    RJM(I)                                 21
     C   21              EXSR      SRJMC
     C   21              GOTO      FIM
     C*
     C* CONVERTE ESPECIE DE RECIBO ( GUARDIAN VS LUSITANIA ).
     C*
     C                   Z-ADD     1             I
     C     GHTREG        LOOKUP    ERG(I)                                 21
     C   21              MOVE      ERL(I)        LEREC
     C  N21              MOVE      *BLANKS       LEREC
     C*
     C* VALIDA SE RECIBO É DE PRÉMIO ÚNICO. SE É FORÇA ESPECIE DE RECIBO.
     C*
     C     GHFRAC        IFEQ      5
     C                   MOVE      2             LEREC
     C                   END
     C*
     C* FORMATA CAPITAL TOTAL
     C*
     C                   Z-ADD     GHCTDP        LCAP
     C*
     C     GHRR          IFEQ      50                                           <-----------------+
     C                   Z-ADD     GHCTDP        DP                                               I
     C*                                                                      I
     C     GHCTRC        IFEQ      99999                                        <----------+      I
     C                   Z-ADD     *ZEROS        RC                                        I      I
     C                   ELSE                                                              I      I
     C     GHCTRC        MULT      1000          RC                                        I      I
     C                   END                                                    <----------+      I
     C     RC            ADD       DP            LCAP                                             I
     C*                                                                      I
     C                   ELSE                                                                     *
     C*                                                                      I
     C     RM            IFEQ      51                                           <-----------+     I
     C     GHCTRC        MULT      1000          RC                                         I     I
     C                   Z-ADD     GHCTDP        DP                                         I     I
     C     RC            ADD       DP            LCAP                                       I     I
     C                   END                                                    <-----------+     I
     C*                                                                      I
     C                   END                                                    <-----------------+
     C*
     C* FORMATA CAPITAL PARA FAT.
     C*
     C     GHRR          IFEQ      15
     C                   Z-ADD     GHCAPF        LCAPF
     C                   END
     C*
     C* FORMATA PRÉMIO COMERCIAL.
     C*
     C                   Z-ADD     *ZEROS        WPRS              9 2
     C                   Z-ADD     *ZEROS        WENC7             9 2
     C     GHPRS         IFGE      GHVFRC
     C     GHPRS         SUB       GHVFRC        WPRS
     C                   ELSE
     C                   Z-ADD     GHPRS         WPRS
     C                   END
     C     GHENC7        IFGE      GHFGA
     C     GHENC7        SUB       GHFGA         WENC7
     C                   ELSE
     C                   Z-ADD     GHENC7        WENC7
     C                   END
     C     WPRS          ADD       WENC7         LPCOM
     C*
     C* MOVE CAMPOS DIVERSOS
     C*
     C     GHRR          IFEQ      15                                           - AC.TRAB.
     C                   Z-ADD     *ZEROS        LFGA
     C                   Z-ADD     GHFGA         LFFAT
     C                   ELSE
     C     GHRR          IFEQ      10                                           - VIAGENS
     C                   Z-ADD     *ZEROS        LFGA
     C                   Z-ADD     *ZEROS        LFFAT
     C                   ELSE
     C     AMMOV         IFLT      0801
     C                   Z-ADD     GHFGA         LFGA
     C                   ELSE
     C                   Z-ADD     GHFGA         LPR
     C                   Z-ADD     GHPR          LFGA
     C                   END
     C                   Z-ADD     *ZEROS        LFFAT
     C                   END
     C                   END
      *
     C     GHRR          IFEQ      50
     C     GHTREG        IFNE      5
     C                   Z-ADD     GHCRC         LCRC
     C                   ELSE
     C                   ADD       GHCRC         LPCOM
     C                   END
     C                   END
      *
     C                   Z-ADD     GHCSTA        LCUSTA
     C                   Z-ADD     GHSELO        LSELO
     C                   Z-ADD     GHSNA         LINEM
     C                   Z-ADD     GHSNB         LSNB
     C                   Z-ADD     GHTREC        LTREC
     C*
     C* NOTA : - CAMPO GHPRS TEM O PR. SIMPLES + VAL. FRACCIONAMENTO
     C* ASSIM: - MOVE PRÉMIO SIMPLES SEM O VALOR DO FRACCIONAMENTO
     C*          PARA O CAMPO PREMIO DE COMISSIONAMETO.
     C*
     C* ALTERAÇÃO EM 2002/04/04
     C* SE EXISTE ISENÇÃO PR.SIMPLES É IGUAL AO PR.COMERCIAL
     C     GHISEN        IFNE      1
     C     WPRS          DIV(H)    1.2           LPCOMI
     C                   ELSE
     C                   Z-ADD     WPRS          LPCOMI
     C                   END
     C*
     C                   Z-ADD     GHPRS         LPFRAC
     C* _____________________________________________________________:
     C* REGRA INICIAL DA INFORMAÇÃO DAS COMISSÕES:
     C*
     C*  - NAO SE INFORMA AS COMISSOES CEDIDAS POR JOSE MATA
     C*  - O CAMPO LCOMI APENAS GUARDA OS VALORES DAS COMISSOES
     C*    PARA OS RECIBOS SÓ DE COMISSAO DE JOSE MATA (RAPPEL)
     C*
     C* _____________________________________________________________:
     C* NOTA:
     C*
     C* INICIALMENTE APLICAVA-SE A REGRA ATRÁS REFERIDA. FOI
     C* SOLICITADO A INFORMAÇÃO DA COMISSÃO CALCULADA E ATRIBUIDA
     C* A JOSÉ MATA. (15.11.2000)
     C*
     C     TIPCS         IFEQ      '2'
     C                   Z-ADD     GHMED2        KEYCSF            3 0
     C     KEYCSF        CHAIN     CSFTAB                             90
     C  N90              MOVEL     NUMCS         LCOSLI
     C  N90              Z-ADD     GHCOM2        LCOMI
     C  N90              Z-ADD     GHCOM1        LCOMLI
     C   90GHCOM1        ADD       GHCOM2        LCOMI
      *
     C                   ELSE
     C     GHCOM1        ADD       GHCOM2        LCOMI
     C                   END
     C*
     C                   Z-ADD     GHVFRC        LVFRAC
     C*
     C* FORÇA CAMPOS DE CARTA VERDE A ZEROS ( ATÉ ALTERAÇÃO DO PROG. )
     C*
     C                   MOVE      *BLANKS       LNCV1
     C                   MOVE      *BLANKS       LNCV2
     C*
     C* DETERMINA VALORES DE A.VIAGEM SE RECIBO TEM A.VIAGEM
     C*
     C     GHVIAG        IFNE      ' '                                          <----------+
     C***                  MOVE GHVIAG    LVIAG                       I
     C                   MOVE      'S'           LVIAG                                     I
     C                   EXSR      SRVIAG                                                  I
     C*                                                               I
     C* MOVE CAMPOS DA A.VIAGEM PARA CAMPOS A.VIAGEM LUSITANIA        I
     C*                                                               I
     C*                    Z-ADDGXPRS     LPRSVG                      I
     C*                    Z-ADDPRSRVG    LRESVG                      I
     C*                    Z-ADDGXTREC    LPRTVG                      I
     C*                                                               I
     C                   END                                                    <----------+
     C*
     C* DETERMINA VALORES DE A.VIAGEM SE RECIBO TEM A.VIAGEM
     C*
     C     GHPJUR        IFEQ      'S'                                          <-----------+
     C                   MOVE      GHPJUR        LPJUR                                      I
     C                   EXSR      SRPJUR                                                   I
     C*                                                                I
     C* MOVE CAMPOS DA P.JURIDICA P/CAMPOS P.JURIDICA DA LUSITANIA     I
     C*                                                                I
     C*                    Z-ADDGXPRS     LPRSPJ                       I
     C*                    Z-ADDPRSRPJ    LRESPJ                       I
     C*                    Z-ADDGXTREC    LPRTPJ                       I
     C*                                                                I
     C                   END                                                    <-----------+
     C*
     C* ACEDE AO REGISTO DE COBRANÇAS PARA COMPLETAR
     C* INFORMAÇAO EM FALTA NO REGISTO DO GREGA.
     C*
     C****       GHSREC    IFGE 80000
     C     GHNREC        CHAIN     GCOBCOLF                           90
     C****                 ELSE
     C   90GHNREC        CHAIN     GFCOBGR                            90
     C****                 END
     C   90              SETON                                        50
     C   90              EXCEPT    ERRO00
     C   90              GOTO      FIM
     C  N90              DO                                                     <--------------+
     C*                                                                   I
     C* FORMATA DATA DE EMISSAO DO RECIBO NO GCOBGR                       I
     C*                                                                   I
     C                   MOVE      *ZEROS        DTY2              8 0                         I
     C                   MOVE      *ZEROS        WDTEMI            8 0                         I
     C                   MOVE      DTEMIS        DTY2                                          I
     C                   EXSR      SRDTY2                                                      I
     C                   MOVE      DTY2          WDTEMI                                        I
     C     WDTEMI        IFGT      *ZEROS                                                      I
     C                   MOVE      *ZEROS        DATA$                                         I
     C                   MOVE      WDTEMI        DATA$                                         I
     C                   EXSR      VDATA                                                       I
     C                   MOVE      DATA$         WDTEMI                                        I
     C                   MOVE      DATA$         LDTEMI                                        I
     C                   END                                                                   I
     C*                                                                   I
     C* FORMATA DATA DE INICIO DO RECIBO NO GCOBGR                        I
     C*    1º FORÇA DATA DE INICIO DO RECIBO                              I
     C*       (ESTE CAMPO PARA OS TRANSPORTES ENCONTRA-SE A ZEROS)        I
     C*                                                                   I
     C     DTI           IFEQ      *ZEROS                                       <--------+     I
     C     GHRRAP        CHAIN     GREGAPOL                           90                 I     I
     C  N90              DO                                                     <-----+  I     I
     C                   Z-ADD     RDTINI        DTI                                  I  I     I
     C                   Z-ADD     RDTFIM        DTF                                  I  I     I
     C                   END                                                    <-----+  I     I
     C                   END                                                    <--------+     I
     C*                                                                   I
     C     DTI           IFEQ      *ZEROS                                       <-----+        I
     C                   MOVE      RAAPRO        AAINI                                I        I
     C                   MOVE      RMMPRO        MMINI                                I        I
     C                   MOVE      RDDPRO        DDINI                                I        I
     C                   END                                                    <-----+        I
     C*                                                                   I
     C     DTI           IFEQ      *ZEROS                                       <-----+        I
     C                   MOVE      RAAREG        AAINI                                I        I
     C                   MOVE      RMMREG        MMINI                                I        I
     C                   MOVE      RDDREG        DDINI                                I        I
     C                   END                                                    <-----+        I
     C*                                                                   I
     C**         AAINI     IFGT 05                         <-----+        I
     C**                   MOVE *ZEROS    AAINI                  I        I
     C**                   ELSE                                  *        I
     C**                   END                             <-----+        I
     C*                                                                   I
     C     DTI           IFEQ      *ZEROS                                       <-------+      I
     C     WDTINI        IFEQ      *ZEROS                                       <-----+ I      I
     C* ANO                                                      I I      I
     C                   MOVE      GHAREC        AAINI             2 0                I I      I
     C* MES                                                      I I      I
     C     GHMREC        IFEQ      *ZEROS                                       <---+ I I      I
     C                   MOVE      GHMMMV        MMINI             2 0              I I I      I
     C                   ELSE                                                       * I I      I
     C                   MOVE      GHMREC        MMINI                              I I I      I
     C                   END                                                    <---+ I I      I
     C* DIA                                                      I I      I
     C                   MOVE      01            DDINI             2 0                I I      I
     C                   END                                                    <-----+ I      I
     C*                                                            I      I
     C                   MOVE      WDTINI        DTI                                    I      I
     C                   END                                                    <-------+      I
     C*                                                                   I
     C* REFORMATA DATA DE INICIO                                          I
     C*                                                                   I
     C                   MOVE      *ZEROS        DTY2              8 0                         I
     C                   MOVE      *ZEROS        WDTI              8 0                         I
     C                   MOVE      DTI           DTY2                                          I
     C                   EXSR      SRDTY2                                                      I
     C                   MOVE      DTY2          WDTI                                          I
     C     WDTI          IFGT      *ZEROS                                                      I
     C                   MOVE      *ZEROS        DATA$                                         I
     C                   MOVE      WDTI          DATA$                                         I
     C                   EXSR      VDATA                                                       I
     C                   MOVE      DATA$         WDTI                                          I
     C                   END                                                                   I
     C*                                                                   I
     C* FORMATA DATA DE TERMO DO RECIBO NO GCOBGR                         I
     C*    1º FORÇA DATA DE TERMO DO RECIBO                               I
     C*       (ESTE CAMPO PARA OS TRANSPORTES ENCONTRA-SE A ZEROS)        I
     C*                                                                   I
     C*                                                                   I
     C     DTF           IFEQ      *ZEROS                                       <-----+        I
     C                   MOVE      RAAPRO        AAFIM                                I        I
     C                   MOVE      RMMPRO        MMFIM                                I        I
     C                   MOVE      RDDPRO        DDFIM                                I        I
     C                   END                                                    <-----+        I
     C*                                                                   I
     C     DTF           IFEQ      *ZEROS                                       <-----+        I
     C                   MOVE      RAAREG        AAFIM                                I        I
     C                   MOVE      RMMREG        MMFIM                                I        I
     C                   MOVE      RDDREG        DDFIM                                I        I
     C                   END                                                    <-----+        I
     C*                                                                   I
     C**         AAFIM     IFGT 05                         <-----+        I
     C**                   MOVE *ZEROS    AAFIM                  I        I
     C**                   ELSE                                  *        I
     C**                   END                             <-----+        I
     C*                                                                   I
     C     DTF           IFEQ      *ZEROS                                       <--------+     I
     C     WDTFIM        IFEQ      *ZEROS                                       <-------+I     I
     C* ANO                                                        II     I
     C                   MOVE      GHAREC        AAFIM             2 0                  II     I
     C* MES                                                        II     I
     C     GHMREC        IFEQ      *ZEROS                                       <-----+ II     I
     C                   MOVE      GHMMMV        MMFIM             2 0                I II     I
     C                   ELSE                                                         * II     I
     C                   MOVE      GHMREC        MMFIM                                I II     I
     C                   END                                                    <-----+ II     I
     C* DIA                                                        II     I
     C                   Z-ADD     1             I                 2 0                  II     I
     C     MMFIM         LOOKUP    TMD(I)                                 21            II     I
     C  N21              Z-ADD     *ZEROS        DDFIM                                  II     I
     C  N21              Z-ADD     *ZEROS        MMFIM                                  II     I
     C  N21              Z-ADD     *ZEROS        AAFIM                                  II     I
     C   21              MOVEL     TDM(I)        DDFIM             2 0                  II     I
     C*                                                            II     I
     C                   END                                                    <-------+I     I
     C*                                                             I     I
     C                   MOVE      WDTFIM        DTF                                     I     I
     C                   END                                                    <--------+     I
     C*                                                                   I
     C* REFORMATA DATA DE FIM DO RECIBO NO GCOBGR                         I
     C*                                                                   I
     C                   MOVE      *ZEROS        DTY2              8 0                         I
     C                   MOVE      *ZEROS        WDTF              8 0                         I
     C                   MOVE      DTF           DTY2                                          I
     C                   EXSR      SRDTY2                                                      I
     C                   MOVE      DTY2          WDTF                                          I
     C     WDTF          IFGT      *ZEROS                                                      I
     C                   MOVE      *ZEROS        DATA$                                         I
     C                   MOVE      WDTF          DATA$                                         I
     C                   EXSR      VDATA                                                       I
     C                   MOVE      DATA$         WDTF                                          I
     C                   END                                                                   I
     C*                                                                   I
     C* REFORÇA A DATA DE INICIO DO RECIBO A SER SUPERIOR OU IGUAL        I
     C* AO ANO 2000. ( EM CASO NEGATIVO, DATA INICIO=DATA TERMO )         I
     C* ISTO ACONTECE PARA RECIBOS DE ESTORNO E ADICIONAIS.               I
     C*                                                                   I
     C     WDTI          IFLE      20000000                                                    I
     C                   MOVEL     WDTF          WDTI
     C     GHAREC        IFEQ      0                                                           I
     C                   MOVEL     2000          WDTI                                          I
     C                   ELSE                                                                  I
     C                   MOVEL     20            WGHANO            4 0                         I
     C                   MOVE      GHAREC        WGHANO                                        I
     C                   MOVEL     WGHANO        WDTI                                          I
     C                   END                                                                   I
     C                   END                                                                   I
     C*                                                                   I
     C* REFORÇA A DATA DE INICIO DO RECIBO A SER SUPERIOR OU IGUAL        I
     C* À DATA DE FIM. ( EM CASO NEGATIVO, DATA TERMO=DATA INICIO )       I
     C* ISTO ACONTECE PARA RECIBOS DE ESTORNO E ADICIONAIS.               I
     C*                                                                   I
     C     WDTF          IFLT      WDTI                                                        I
     C                   Z-ADD     WDTI          WDTF                                          I
     C                   END                                                                   I
     C*                                                                   I
     C* FORMATA DATA LIMITE COBRANÇA DO RECIBO NO GCOBGR                  I
     C*                                                                   I
     C*                    MOVE TZEROS    DTY2    80                      I
     C*                    MOVE *ZEROS    WDTLIM  80                      I
     C*                    MOVE DTLIM     DTY2                            I
     C*                    EXSR SRDTY2                                    I
     C*                    MOVE DTY2      WDTLIM                          I
     C*                                                                   I
     C* FORMATA DATA DA COBRANÇA SE RECIBO COBRADO (SIT = 1)              I
     C*                                                                   I
     C     SIT           IFEQ      1                                            <---------+    I
     C                   MOVE      *ZEROS        DTY2              8 0                    I    I
     C                   MOVE      *ZEROS        WDTPAG            8 0                    I    I
     C                   MOVE      DTPAG         DTY2                                     I    I
     C                   EXSR      SRDTY2                                                 I    I
     C                   MOVE      DTY2          WDTPAG                                   I    I
     C                   ELSE                                                             I    I
     C                   MOVE      *ZEROS        WDTPAG                                   I    I
     C     WDTPAG        IFGT      *ZEROS                                                 I    I
     C                   MOVE      *ZEROS        DATA$                                    I    I
     C                   MOVE      WDTPAG        DATA$                                    I    I
     C                   EXSR      VDATA                                                  I    I
     C                   MOVE      DATA$         WDTPAG                                   I    I
     C                   END                                                              I    I
     C                   END                                                    <---------*    I
     C*                                                                   I
     C* FORMATA DATA DE ANULAÇAO DO RECIBO NO GCOBGR                      I
     C*                                                                   I
     C     SIT           IFEQ      3                                            <---------+    I
     C                   MOVE      *ZEROS        DTY2              8 0                    I    I
     C                   MOVE      *ZEROS        WDTANU            8 0                    I    I
     C                   MOVE      DTPAG         DTY2                                     I    I
     C                   EXSR      SRDTY2                                                 I    I
     C                   MOVE      DTY2          WDTANU                                   I    I
     C     WDTANU        IFGT      *ZEROS                                                 I    I
     C                   MOVE      *ZEROS        DATA$                                    I    I
     C                   MOVE      WDTANU        DATA$                                    I    I
     C                   EXSR      VDATA                                                  I    I
     C                   MOVE      DATA$         WDTANU                                   I    I
     C                   END                                                              I    I
     C*                                                              I    I
     C* FORMATA CODIGO DE ANULAÇAO DO RECIBO NO GCOBGR               I    I
     C*                                                              I    I
     C                   MOVE      1             WANUL             1 0                    I    I
     C*                                                              I    I
     C                   Z-ADD     1             I                 2 0                    I    I
     C     WANUL         LOOKUP    ARG(I)                                 20              I    I
     C   20              MOVE      ARL(I)        WLCANU            1                      I    I
     C  N20              MOVE      *BLANKS       WLCANU                                   I    I
     C*                                                              I    I
     C* FORÇA CÒDIGO SE APÓLICE FOI ANULADA POR FALTA PAGAMENTO      I    I
     C*                                                              I    I
     C                   Z-ADD     *ZEROS        CHBASE            9 0                    I    I
     C                   MOVEL     GHRR          CHBASE                                   I    I
     C                   MOVE      GHAPOL        CHBASE                                   I    I
     C*                                                              I    I
     C     CHBASE        CHAIN     GBASEGRL                           99                  I    I
     C  N99              DO                                                     <------+  I    I
     C     SITAP         IFEQ      3                                            <----+ I  I    I
     C     MALT          IFEQ      62                                           <--+ I I  I    I
     C*                                                       I I I  I    I
     C                   MOVE      2             WANUL                             I I I  I    I
     C*                                                       I I I  I    I
     C                   Z-ADD     1             I                                 I I I  I    I
     C     WANUL         LOOKUP    ARG(I)                                 20       I I I  I    I
     C   20              MOVE      ARL(I)        WLCANU                            I I I  I    I
     C  N20              MOVE      *BLANKS       WLCANU                            I I I  I    I
     C*                                                       I I I  I    I
     C                   END                                                    <--+ I I  I    I
     C                   END                                                    <----+ I  I    I
     C                   END                                                    <------+  I    I
     C                   END                                                    <---------+    I
     C                   END                                                    <--------------+
     C*
     C* DETERMINA TIPO DE REGISTO A COLOCAR NO FICHEIRO DE OUTPUT
     C* POR ANÁLISE DOS CAMPOS DESCODIFICADOS EM ESPECIAL NAS COBRANÇAS
     C*
     C     WDTEMI        IFGT      *ZEROS
     C                   MOVE      WDTI          LDTINI
     C                   MOVE      WDTF          LDTERM
     C                   MOVE      '1'           LCDMOV                         ( EMISSÃO )
     C*
     C* CONTROLA O ENVIO DE REGISTOS ANTERIORES (JÁ ENVIADOS)
     C*
     C     LKEY          CHAIN     LFRECX                             90
     C**N90                DO                              <-----+
     C**         GHRR      IFEQ 68                         <---+ I
     C**         GHAPOL    IFLT 99200309                       I I
     C**                   MOVE '32'      LRAMO                I I
     C**                   END                             <---+ I
     C**                   END                                   I
     C**                   END                             <-----+
     C   90              DO                                                     <-----+
     C     GHRR          IFNE      68                                                 I
     C                   EXCEPT    REGEMI                                             I
     C                   ELSE                                                         I
     C                   EXCEPT    REGE32                                             I
     C                   END                                                          I
     C                   END                                                    <-----+
     C                   END
     C*
     C     WDTPAG        IFGT      *ZEROS
     C     WDTPAG        IFLE      LDTMOV
     C                   MOVE      WDTI          LDTINI
     C                   MOVE      WDTF          LDTERM
     C                   MOVE      WDTPAG        LDTANU                         ( DATA DA COBRANÇA )
     C                   MOVE      '4'           LCDMOV                         ( COBRADO )
     C*
     C* CONTROLA O O ENVIO DE REGISTOS ANTERIORES (JÁ ENVIADOS)
     C*
     C     LKEY          CHAIN     LFRECX                             90
     C* N90                GOTO FIM
     C   90              DO                                                     <-----+
     C     GHRR          IFNE      68                                                 I
     C                   EXCEPT    REGANU                                             I
     C                   ELSE                                                         I
     C****                 EXSR SRRAMO
     C                   EXCEPT    REGA32                                             I
     C                   END                                                          I
     C                   END                                                    <-----+
     C                   END
     C                   END
     C*
     C     WDTANU        IFGT      *ZEROS
     C     WDTANU        IFLE      LDTMOV
     C                   MOVE      WDTI          LDTINI
     C                   MOVE      WDTF          LDTERM
     C                   MOVE      WLCANU        LCANUL
     C                   MOVE      WDTANU        LDTANU                         ( DATA ANULAÇÃO )
     C                   MOVE      '3'           LCDMOV                         ( ANULADO )
     C*
     C* CONTROLA O O ENVIO DE REGISTOS ANTERIORES (JÁ ENVIADOS)
     C*
     C     LKEY          CHAIN     LFRECX                             90
     C* N90                GOTO FIM
     C   90              DO                                                     <-----+
     C     GHRR          IFNE      68                                                 I
     C                   EXCEPT    REGANU                                             I
     C                   ELSE                                                         I
     C****                 EXSR SRRAMO
     C                   EXCEPT    REGA32                                             I
     C                   END                                                          I
     C                   END                                                    <-----+
     C                   END
     C                   END
     C*
     C     FIM           TAG
     C****************************************************
     C*   FORMATA RECIBOS SÓ DE COMISSAO JOSÉ MATA        *
     C********************************************    > < =
     C     SRJMC         BEGSR
     C*
     C* ACEDE AO REGISTO DE COBRANÇAS PARA COMPLETAR
     C* INFORMAÇAO EM FALTA NO REGISTO DO GREGA.
     C*
     C****       GHSREC    IFGE 80000
     C     GHNREC        CHAIN     GCOBCOLF                           90
     C****                 ELSE
     C   90GHNREC        CHAIN     GFCOBGR                            90
     C****                 END
     C   90              SETON                                        50
     C   90              EXCEPT    ERRO00
     C   90              GOTO      ESRJMC
     C*
     C* VALIDA SE RECIBO DE COMISSÃO ESTÁ OU NÃO ANULADO
     C*
     C     SIT           IFEQ      3
     C                   GOTO      ESRJMC
     C                   END
     C*
     C* CONVERTE RAMOS  ( RAMO GUARDIAN VS LUSITANIA ).
     C*
     C                   Z-ADD     1             I                 2 0
     C     GHRR          LOOKUP    RMG(I)                                 20
     C   20              MOVE      RML(I)        LRAMO
     C  N20              MOVE      *BLANKS       LRAMO
     C*
     C                   MOVE      GHAPOL        LAPOL
     C*
     C     GCTCOM        IFEQ      'R'
     C                   MOVE      '6'           LCDMOV
     C                   ELSE
     C     GCTCOM        IFEQ      'G'
     C                   MOVE      '8'           LCDMOV
     C                   ELSE
     C                   MOVE      '6'           LCDMOV
     C                   END
     C                   END
     C*
     C     TIPCS         IFEQ      '2'
     C                   Z-ADD     GHMED2        KEYCSF
     C     KEYCSF        CHAIN     CSFTAB                             90
     C  N90              MOVEL     NUMCS         LCOSLI
     C  N90              Z-ADD     GHCOM2        LCOMI
     C  N90              Z-ADD     GHCOM1        LCOMLI
     C   90GHCOM1        ADD       GHCOM2        LCOMI
      *
     C                   ELSE
     C     GHCOM1        ADD       GHCOM2        LCOMI
     C                   END
     C*
     C     LCOMI         IFLE      *ZEROS
     C                   GOTO      ESRJMC
     C                   END
     C*
     C* REFORMATA DATA DE INICIO
     C*
     C                   MOVE      *ZEROS        DTY2              8 0
     C                   MOVE      *ZEROS        WDTI
     C                   MOVE      DTI           DTY2
     C                   EXSR      SRDTY2
     C                   MOVE      DTY2          WDTI
     C*
     C* REFORMATA DATA DE FIM
     C*
     C                   MOVE      *ZEROS        DTY2              8 0
     C                   MOVE      *ZEROS        WDTF              8 0
     C                   MOVE      DTF           DTY2
     C                   EXSR      SRDTY2
     C                   MOVE      DTY2          WDTF
     C*
     C* REFORMATA DATA DA COBRANÇA SE RECIBO COBRADO (SIT = 1)
     C*
     C     SIT           IFEQ      1                                            <---------+
     C                   MOVE      *ZEROS        DTY2              8 0                    I
     C                   MOVE      *ZEROS        WDTPAG            8 0                    I
     C                   MOVE      DTPAG         DTY2                                     I
     C                   EXSR      SRDTY2                                                 I
     C                   MOVE      DTY2          WDTPAG                                   I
     C     WDTPAG        IFGT      *ZEROS                                                 I
     C                   MOVE      *ZEROS        DATA$                                    I
     C                   MOVE      WDTPAG        DATA$                                    I
     C                   EXSR      VDATA                                                  I
     C                   MOVE      DATA$         WDTPAG                                   I
     C                   END                                                              I
     C                   ELSE                                                             I
     C                   MOVE      *ZEROS        WDTPAG                                   I
     C                   END                                                    <---------*
     C*
     C* CONTROLA O ENVIO DE REGISTOS PELA DATA DE COBRANÇA
     C*
     C     WDTPAG        IFLE      LDTMOV
     C     LKEY          CHAIN     LFRECCX                            90
     C   90              DO                                                     <-----+
     C     GHRR          IFNE      68                                                 I
     C                   EXCEPT    REGCC                                              I
     C                   ELSE                                                         I
     C                   EXCEPT    RGCC32                                             I
     C                   END                                                          I
     C                   END                                                    <-----+
     C                   END
     C*
     C     ESRJMC        ENDSR
     C****************************************************
     C*   FORMATA DATAS COM 8 DIGITOS                     *
     C********************************************    > < =
     C     SRDTY2        BEGSR
     C*
     C     DTY2          IFGT      *ZEROS
     C     DTY2          IFLT      00400000
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
     C* ********************************************************
     C* *  SUBROTINA DE CÁLCULO DE PREMIOS A.VIAGEM            *
     C* ********************************************************
     C     SRVIAG        BEGSR
     C*
     C                   MOVEL     LKEY          KEY
     C                   MOVEL     '52'          KEY
     C     KEY           CHAIN     GREGAX                             10
     C   10              DO
     C                   Z-ADD     *ZEROS        LPRSVG
     C                   Z-ADD     *ZEROS        LRESVG
     C                   Z-ADD     *ZEROS        LPRTVG
     C                   Z-ADD     *ZEROS        LPRVGS
     C                   GOTO      ESRVG
     C                   END
     C*                                                                ---
     C                   EXSR      SRVVL0
     C   70              EXCEPT    ERRO02
     C*
     C     GXPRS         IFEQ      12,61                                        <-----
     C                   Z-ADD     8,73          PRSRVG            9 2               |
     C                   ELSE                                                        *
     C     GXPRS         IFEQ      12,90                                        <--- |
     C                   Z-ADD     8,98          PRSRVG                            | |
     C                   END                                                    <--- |
     C                   END                                                    <-----
     C*
     C     ESRVGM        TAG
     C*
     C*  LPRSVG CONTEM VALOR DO PREMIO COMERCIAL
     C                   Z-ADD     GXPRS         LPRSVG
     C                   Z-ADD     PRSRVG        LRESVG
     C                   Z-ADD     GXTREC        LPRTVG
     C*
     C*  LPRVGS CONTEM VALOR DO PREMIO SIMPLES
     C*  SE EXISTE ISENÇÃO PR.SIMPLES É IGUAL AO PR.COMERCIAL
     C*
     C     GXISEN        IFNE      1
     C     GXPRS         DIV(H)    1.2           LPRVGS
     C                   ELSE
     C                   Z-ADD     GXPRS         LPRVGS
     C                   END
     C*
     C     ESRVG         ENDSR
     C*
     C* **************************************************
     C* *  SUBROTINA DE CALCULO DE PREMIOS P.JURIDICA    *
     C* **************************************************
     C     SRPJUR        BEGSR
     C*
     C                   MOVEL     LKEY          KEY
     C                   MOVEL     '54'          KEY
     C     KEY           CHAIN     GREGAX                             10
     C   10              DO
     C                   Z-ADD     *ZEROS        LPRSPJ
     C                   Z-ADD     *ZEROS        LRESPJ
     C                   Z-ADD     *ZEROS        LPRTPJ
     C                   GOTO      ESRPJ
     C                   END
     C*
     C                   EXSR      SRVVL0
     C   70              EXCEPT    ERRO02
     C*
     C     GXPRS         IFEQ      6,66
     C                   Z-ADD     3,49          PRSRPJ            9 2
     C                   ELSE
     C                   Z-ADD     *ZEROS        PRSRPJ
     C                   END
     C*
     C* MOVE CAMPOS DA P.JURIDICA P/CAMPOS P.JURIDICA DA LUSITANIA
     C*
     C                   Z-ADD     GXPRS         LPRSPJ
     C                   Z-ADD     PRSRPJ        LRESPJ
     C                   Z-ADD     GXTREC        LPRTPJ
     C*
     C*  LPRPJS CONTEM VALOR DO PREMIO SIMPLES
     C*  SE EXISTE ISENÇÃO PR.SIMPLES É IGUAL AO PR.COMERCIAL
     C*
     C     GXISEN        IFNE      1
     C     GXPRS         DIV(H)    1.2           LPRPJS
     C                   ELSE
     C                   Z-ADD     GXPRS         LPRPJS
     C                   END
     C*
     C     ESRPJ         ENDSR
     C*
     C* **************************************************
     C* *  SUBROTINA DE CALCULO DE PREMIOS P.JURIDICA    *
     C* **************************************************
     C     SRVVL         BEGSR
     C*
     C                   SETOFF                                       60
     C     GHPRS         IFLT      *ZEROS
     C                   MOVE      'GHPRS '      CAMPO             6
     C                   MOVEL     'PREMIO S'    DCAMPO           16
     C                   MOVE      'IMPLES  '    DCAMPO
     C                   SETON                                        60
     C                   Z-ADD     GHPRS         WVALOR           11 2
     C                   EXSR      SRINV
     C                   Z-ADD     WVALOR        GHPRS
     C                   END
     C                   SETOFF                                       60
     C     GHENC7        IFLT      *ZEROS
     C                   MOVE      'GHENC7'      CAMPO
     C                   MOVEL     'ENCARGOS'    DCAMPO
     C                   MOVE      '        '    DCAMPO
     C                   SETON                                        60
     C                   Z-ADD     GHENC7        WVALOR
     C                   EXSR      SRINV
     C                   Z-ADD     WVALOR        GHENC7
     C                   END
     C                   SETOFF                                       60
     C     GHCSTA        IFLT      *ZEROS
     C                   MOVE      'GHCSTA'      CAMPO
     C                   MOVEL     'CUSTO A'     DCAMPO
     C                   MOVE      'PÓLICE  '    DCAMPO
     C                   SETON                                        60
     C                   Z-ADD     GHCSTA        WVALOR
     C                   EXSR      SRINV
     C                   Z-ADD     WVALOR        GHCSTA
     C                   END
     C                   SETOFF                                       60
     C     GHCRC         IFLT      *ZEROS
     C                   MOVE      'GHCRC '      CAMPO
     C                   MOVEL     'C.R.C.  '    DCAMPO
     C                   MOVE      '        '    DCAMPO
     C                   SETON                                        60
     C                   Z-ADD     GHCRC         WVALOR
     C                   EXSR      SRINV
     C                   Z-ADD     WVALOR        GHCRC
     C                   END
     C                   SETOFF                                       60
     C     GHSELO        IFLT      *ZEROS
     C                   MOVE      'GHSELO'      CAMPO
     C                   MOVEL     'IMPOSTO '    DCAMPO
     C                   MOVE      'SELO    '    DCAMPO
     C                   SETON                                        60
     C                   Z-ADD     GHSELO        WVALOR
     C                   EXSR      SRINV
     C                   Z-ADD     WVALOR        GHSELO
     C                   END
     C                   SETOFF                                       60
     C     GHSNA         IFLT      *ZEROS
     C                   MOVE      'GHSNA '      CAMPO
     C                   MOVEL     'S.N.AMBU'    DCAMPO
     C                   MOVE      'LÂNCIAS '    DCAMPO
     C                   SETON                                        60
     C                   Z-ADD     GHSNA         WVALOR
     C                   EXSR      SRINV
     C                   Z-ADD     WVALOR        GHSNA
     C                   END
     C                   SETOFF                                       60
     C     GHSNB         IFLT      *ZEROS
     C                   MOVE      'GHSNB   '    CAMPO
     C                   MOVEL     'S.N.BOMB'    DCAMPO
     C                   MOVE      'EIROS   '    DCAMPO
     C                   SETON                                        60
     C                   Z-ADD     GHSNB         WVALOR
     C                   EXSR      SRINV
     C                   Z-ADD     WVALOR        GHSNB
     C                   END
     C                   SETOFF                                       60
     C     GHTREC        IFLT      *ZEROS
     C                   MOVE      'GHTREC'      CAMPO
     C                   MOVEL     'TOTAL DE'    DCAMPO
     C                   MOVE      ' RECIBO '    DCAMPO
     C                   SETON                                        60
     C                   Z-ADD     GHTREC        WVALOR
     C                   EXSR      SRINV
     C                   Z-ADD     WVALOR        GHTREC
     C                   END
     C                   SETOFF                                       60
     C     GHFGA         IFLT      *ZEROS
     C                   MOVE      'GHFGA'       CAMPO
     C                   MOVEL     'F.G.AUTO'    DCAMPO
     C                   MOVE      'MÓVEL   '    DCAMPO
     C                   SETON                                        60
     C                   Z-ADD     GHFGA         WVALOR
     C                   EXSR      SRINV
     C                   Z-ADD     WVALOR        GHFGA
     C                   END
     C                   SETOFF                                       60
     C     GHVFRC        IFLT      *ZEROS
     C                   MOVE      'GHVFRC'      CAMPO
     C                   MOVEL     'V.FRACCI'    DCAMPO
     C                   MOVE      'ONAMENTO'    DCAMPO
     C                   SETON                                        60
     C                   Z-ADD     GHVFRC        WVALOR
     C                   EXSR      SRINV
     C                   Z-ADD     WVALOR        GHVFRC
     C                   END
     C                   SETOFF                                       60
     C     GHCOM1        IFLT      *ZEROS
     C                   MOVE      'GHCOM1'      CAMPO
     C                   MOVEL     'COMISSAO'    DCAMPO
     C                   MOVE      ' MED-1  '    DCAMPO
     C                   SETON                                        60
     C                   Z-ADD     GHCOM1        WVALOR
     C                   EXSR      SRINV
     C                   Z-ADD     WVALOR        GHCOM1
     C                   END
     C                   SETOFF                                       60
     C     GHCOM2        IFLT      *ZEROS
     C                   MOVE      'GHCOM2'      CAMPO
     C                   MOVEL     'COMISSAO'    DCAMPO
     C                   MOVE      ' MED-2  '    DCAMPO
     C                   SETON                                        60
     C                   Z-ADD     GHCOM2        WVALOR
     C                   EXSR      SRINV
     C                   Z-ADD     WVALOR        GHCOM2
     C                   END
     C*
     C     ESRVVL        ENDSR
     C*
     C* **************************************************
     C* *  VALIDA VALORES                                *
     C* **************************************************
     C     SRVVL0        BEGSR
     C*
     C*
     C                   SETOFF                                       70
     C     GXPRS         IFLT      *ZEROS
     C                   MOVE      'GXPRS '      CAMPO             6
     C                   MOVEL     'PREMIO S'    DCAMPO           16
     C                   MOVE      'IMPLES  '    DCAMPO
     C                   SETON                                        70
     C                   Z-ADD     GXPRS         WVALOR           11 2
     C                   EXSR      SRINV
     C                   Z-ADD     WVALOR        GXPRS
     C                   END
     C                   SETOFF                                       70
     C     GXTREC        IFLT      *ZEROS
     C                   MOVE      'GXTREC'      CAMPO
     C                   MOVEL     'TOTAL DE'    DCAMPO
     C                   MOVE      ' RECIBO '    DCAMPO
     C                   SETON                                        70
     C                   Z-ADD     GXTREC        WVALOR
     C                   EXSR      SRINV
     C                   Z-ADD     WVALOR        GXTREC
     C                   END
     C*
     C     ESRVL0        ENDSR
     C*
     C* **************************************************
     C* *  INVERTE VALOR   NEGATIVO  A POSITIVO          *
     C* **************************************************
     C     SRINV         BEGSR
     C*
     C                   Z-SUB     WVALOR        WVALOR
     C*
     C     ESRINV        ENDSR
     C*
     C* *****************************************************
     C* *  CONTROLA ENVIO RAMO 07 OU 32 COBRADOS E ANULADOS *
     C* *****************************************************
     C*          SRRAMO    BEGSR
      *
     C*                    Z-ADDGHWRR     WGHRR   20
     C*                    Z-ADDLCDMOV    WLCDMV  10
      *
     C*                    Z-ADD32        GHWRR
     C*                    Z-ADD1         LCDMOV
      *
     C*          LKEY      CHAINLFRECX               90
     C* N90                MOVE '32'      LRAMO
      *
     C*                    Z-ADDWGHRR     GHWRR
     C*                    Z-ADDWLCDMV    LCDMOV
      *
     C*                    ENDSR
      *
     O****************************************************
     O*  ADICIONA REGISTOS - RECIBOS NORMAIS             *
     O****************************************************
     OLFREC     EADD         REGEMI
     O                       LRAMO                2                             RAMO
     O                       LAPOL                9                             APÓLICE
     O                       LSEG                15                             NR. SEGURADO
     O                       LNREC               21                             NUMERO RECIBO
     O                       LEREC               22                             ESPECIE DE RECIBO
     O                       LCAP                35                             CAPITAL
     O                       LCAPF               48                             CAPITAL  PARA FAT
     O                       LPCOM               59                             PREMIO COMERCIAL
     O                       LFGA                70                             FUNDO GARANTIA AUTO (FGA)
     O                       LFFAT               81                             FUNDOS / FAT
     O                       LCRC                92                             VALOR CARTA VERDE (CARTAO)
     O                       LCUSTA             103                             VALOR CUSTO DE APOLICE
     O                       LSELO              114                             VALOR IMPOSTO SELO
     O                       LINEM              125                             VALOR DO INEM
     O                       LSNB               136                             VALOR DO SBB
     O                       LTREC              147                             VALOR TOTAL RECIBO
     O                       LPCOMI             158                             PREMIO PARA COMISSÕES
     O                       LDTEMI             166                             DATA DE EMISSÃO
     O                       LDTINI             174                             DATA DE INICIO
     O                       LDTERM             182                             DATA TERMO
     O                       LCANUL             184                             CODIGO ANULAÇÃO RECIBO
     O                       LDTANU             192                             DATA ANULAÇÃO RECIBO
     O                       LNCV1              198                             NR CARTA VERDE 1.VEICULO
     O                       LNCV2              204                             NR CARTA VERDE 2.VEICULO
     O                       LCDMOV             205                             CÒDIGO DO MOVIMENTO
     O                       LVFRAC             216                             PREMIO DA FRACÇÃO
     O                       LCOMI              227                             COMISSÃO (JOSÉ MATA)
     O                       LPR                232                             COMISSÃO (JOSÉ MATA)
     O*
     O                       LVIAG              233                             A.VIAGEM SIM/NAO
     O                       LPRVGS             244                             P.SIMPLES A.VIAGEM
     O                       LRESVG             255                             P.RESSEGURO A.VIAGEM
     O                       LPRTVG             266                             P.TOTAL A.VIAGEM
     O                       LPJUR              267                             P.JURIDICA SIM/NAO
     O                       LPRPJS             278                             P.SIMPLES P.JURIDICA
     O                       LRESPJ             289                             P.RESSEGURO P.JURIDICA
     O                       LPRTPJ             300                             P.TOTAL P.JURIDICA
     O                       LPRSVG             311
     O                       LPRSPJ             322
     O*
     O                       LCOMLI             333
     O                       LCOSLI             337
     O*
     O                       LKEY               376                             CHAVE DO REGISTO
     O                       LDTTRF             384                             DATA DA TRANFERÊNCIA
     O****************************************************
     OLFREC     EADD         REGANU
     O                       LRAMO                2                             RAMO
     O                       LAPOL                9                             APÓLICE
     O                       LSEG                15                             NR. SEGURADO
     O                       LNREC               21                             NUMERO RECIBO
     O                       LEREC               22                             ESPECIE DE RECIBO
     O                       LCAP                35                             CAPITAL
     O                       LCAPF               48                             CAPITAL  PARA FAT
     O                       LPCOM               59                             PREMIO COMERCIAL
     O                       LFGA                70                             FUNDO GARANTIA AUTO (FGA)
     O                       LFFAT               81                             FUNDOS / FAT
     O                       LCRC                92                             VALOR CARTA VERDE (CARTAO)
     O                       LCUSTA             103                             VALOR CUSTO DE APOLICE
     O                       LSELO              114                             VALOR IMPOSTO SELO
     O                       LINEM              125                             VALOR DO INEM
     O                       LSNB               136                             VALOR DO SBB
     O                       LTREC              147                             VALOR TOTAL RECIBO
     O                       LPCOMI             158                             PREMIO PARA COMISSÕES
     O                       LDTEMI             166                             DATA DE EMISSÃO
     O                       LDTINI             174                             DATA DE INICIO
     O                       LDTERM             182                             DATA TERMO
     O                       LCANUL             184                             CODIGO ANULAÇÃO RECIBO
     O                       LDTANU             192                             DATA ANULAÇÃO RECIBO
     O                       LNCV1              198                             NR CARTA VERDE 1.VEICULO
     O                       LNCV2              204                             NR CARTA VERDE 2.VEICULO
     O                       LCDMOV             205                             CÒDIGO DO MOVIMENTO
     O                       LVFRAC             216                             PREMIO DA FRACÇÃO
     O                       LCOMI              227                             COMISSÃO (JOSÉ MATA)
     O                       LPR                232                             COMISSÃO (JOSÉ MATA)
     O*
     O                       LVIAG              233                             A.VIAGEM SIM/NAO
     O                       LPRVGS             244                             P.SIMPLES A.VIAGEM
     O                       LRESVG             255                             P.RESSEGURO A.VIAGEM
     O                       LPRTVG             266                             P.TOTAL A.VIAGEM
     O                       LPJUR              267                             P.JURIDICA SIM/NAO
     O                       LPRPJS             278                             P.SIMPLES P.JURIDICA
     O                       LRESPJ             289                             P.RESSEGURO P.JURIDICA
     O                       LPRTPJ             300                             P.TOTAL P.JURIDICA
     O                       LPRSVG             311
     O                       LPRSPJ             322
     O*
     O                       LCOMLI             333
     O                       LCOSLI             337
     O*
     O                       LKEY               376                             CHAVE DO REGISTO
     O                       LDTTRF             384                             DATA DA TRANFERÊNCIA
     O*
     OLFREC     EADD         REGE32
     O                       LRAMO                2                             RAMO
     O                       LAPOL                9                             APÓLICE
     O                       LSEG                15                             NR. SEGURADO
     O                       LNREC               21                             NUMERO RECIBO
     O                       LEREC               22                             ESPECIE DE RECIBO
     O                       LCAP                35                             CAPITAL
     O                       LCAPF               48                             CAPITAL  PARA FAT
     O                       LPCOM               59                             PREMIO COMERCIAL
     O                       LFGA                70                             FUNDO GARANTIA AUTO (FGA)
     O                       LFFAT               81                             FUNDOS / FAT
     O                       LCRC                92                             VALOR CARTA VERDE (CARTAO)
     O                       LCUSTA             103                             VALOR CUSTO DE APOLICE
     O                       LSELO              114                             VALOR IMPOSTO SELO
     O                       LINEM              125                             VALOR DO INEM
     O                       LSNB               136                             VALOR DO SBB
     O                       LTREC              147                             VALOR TOTAL RECIBO
     O                       LPCOMI             158                             PREMIO PARA COMISSÕES
     O                       LDTEMI             166                             DATA DE EMISSÃO
     O                       LDTINI             174                             DATA DE INICIO
     O                       LDTERM             182                             DATA TERMO
     O                       LCANUL             184                             CODIGO ANULAÇÃO RECIBO
     O                       LDTANU             192                             DATA ANULAÇÃO RECIBO
     O                       LNCV1              198                             NR CARTA VERDE 1.VEICULO
     O                       LNCV2              204                             NR CARTA VERDE 2.VEICULO
     O                       LCDMOV             205                             CÒDIGO DO MOVIMENTO
     O                       LVFRAC             216                             PREMIO DA FRACÇÃO
     O                       LCOMI              227                             COMISSÃO (JOSÉ MATA)
     O                       LPR                232                             COMISSÃO (JOSÉ MATA)
     O*
     O                       LVIAG              233                             A.VIAGEM SIM/NAO
     O                       LPRVGS             244                             P.SIMPLES A.VIAGEM
     O                       LRESVG             255                             P.RESSEGURO A.VIAGEM
     O                       LPRTVG             266                             P.TOTAL A.VIAGEM
     O                       LPJUR              267                             P.JURIDICA SIM/NAO
     O                       LPRPJS             278                             P.SIMPLES P.JURIDICA
     O                       LRESPJ             289                             P.RESSEGURO P.JURIDICA
     O                       LPRTPJ             300                             P.TOTAL P.JURIDICA
     O                       LPRSVG             311
     O                       LPRSPJ             322
     O*
     O                       LCOMLI             333
     O                       LCOSLI             337
     O*
     O                       LKEY               376                             CHAVE DO REGISTO
     O                       LDTTRF             384                             DATA DA TRANFERÊNCIA
     O*
     OLFREC     EADD         REGA32
     O                       LRAMO                2                             RAMO
     O                       LAPOL                9                             APÓLICE
     O                       LSEG                15                             NR. SEGURADO
     O                       LNREC               21                             NUMERO RECIBO
     O                       LEREC               22                             ESPECIE DE RECIBO
     O                       LCAP                35                             CAPITAL
     O                       LCAPF               48                             CAPITAL  PARA FAT
     O                       LPCOM               59                             PREMIO COMERCIAL
     O                       LFGA                70                             FUNDO GARANTIA AUTO (FGA)
     O                       LFFAT               81                             FUNDOS / FAT
     O                       LCRC                92                             VALOR CARTA VERDE (CARTAO)
     O                       LCUSTA             103                             VALOR CUSTO DE APOLICE
     O                       LSELO              114                             VALOR IMPOSTO SELO
     O                       LINEM              125                             VALOR DO INEM
     O                       LSNB               136                             VALOR DO SBB
     O                       LTREC              147                             VALOR TOTAL RECIBO
     O                       LPCOMI             158                             PREMIO PARA COMISSÕES
     O                       LDTEMI             166                             DATA DE EMISSÃO
     O                       LDTINI             174                             DATA DE INICIO
     O                       LDTERM             182                             DATA TERMO
     O                       LCANUL             184                             CODIGO ANULAÇÃO RECIBO
     O                       LDTANU             192                             DATA ANULAÇÃO RECIBO
     O                       LNCV1              198                             NR CARTA VERDE 1.VEICULO
     O                       LNCV2              204                             NR CARTA VERDE 2.VEICULO
     O                       LCDMOV             205                             CÒDIGO DO MOVIMENTO
     O                       LVFRAC             216                             PREMIO DA FRACÇÃO
     O                       LCOMI              227                             COMISSÃO (JOSÉ MATA)
     O                       LPR                232                             COMISSÃO (JOSÉ MATA)
     O*
     O                       LVIAG              233                             A.VIAGEM SIM/NAO
     O                       LPRVGS             244                             P.SIMPLES A.VIAGEM
     O                       LRESVG             255                             P.RESSEGURO A.VIAGEM
     O                       LPRTVG             266                             P.TOTAL A.VIAGEM
     O                       LPJUR              267                             P.JURIDICA SIM/NAO
     O                       LPRPJS             278                             P.SIMPLES P.JURIDICA
     O                       LRESPJ             289                             P.RESSEGURO P.JURIDICA
     O                       LPRTPJ             300                             P.TOTAL P.JURIDICA
     O                       LPRSVG             311
     O                       LPRSPJ             322
     O*
     O                       LCOMLI             333
     O                       LCOSLI             337
     O*
     O                       LKEY               376                             CHAVE DO REGISTO
     O                       LDTTRF             384                             DATA DA TRANFERÊNCIA
     O****************************************************
     O*  ADICIONA REGISTOS - RECIBOS COMISSÃO DE RAPPEL  *
     O****************************************************
     OLFRECC    EADD         REGCC
     O                       LRAMO                2                             RAMO
     O                       LAPOL                9                             APÓLICE
     O                       LSEG                15                             NR. SEGURADO
     O                       LNREC               21                             NUMERO RECIBO
     O                       LEREC               22                             ESPECIE DE RECIBO
     O                       LCAP                35                             CAPITAL
     O                       LCAPF               48                             CAPITAL  PARA FAT
     O                       LPCOM               59                             PREMIO COMERCIAL
     O                       LFGA                70                             FUNDO GARANTIA AUTO (FGA)
     O                       LFFAT               81                             FUNDOS / FAT
     O                       LCRC                92                             VALOR CARTA VERDE (CARTAO)
     O                       LCUSTA             103                             VALOR CUSTO DE APOLICE
     O                       LSELO              114                             VALOR IMPOSTO SELO
     O                       LINEM              125                             VALOR DO INEM
     O                       LSNB               136                             VALOR DO SBB
     O                       LTREC              147                             VALOR TOTAL RECIBO
     O                       LPCOMI             158                             PREMIO PARA COMISSÕES
     O                       LDTEMI             166                             DATA DE EMISSÃO
     O                       LDTINI             174                             DATA DE INICIO
     O                       LDTERM             182                             DATA TERMO
     O                       LCANUL             184                             CODIGO ANULAÇÃO RECIBO
     O                       LDTANU             192                             DATA ANULAÇÃO RECIBO
     O                       LNCV1              198                             NR CARTA VERDE 1.VEICULO
     O                       LNCV2              204                             NR CARTA VERDE 2.VEICULO
     O                       LCDMOV             205                             CÒDIGO DO MOVIMENTO
     O                       LVFRAC             216                             PREMIO DA FRACÇÃO
     O                       LCOMI              227                             COMISSÃO (JOSÉ MATA)
     O                       LPR                232                             COMISSÃO (JOSÉ MATA)
     O*
     O                       LVIAG              233                             A.VIAGEM SIM/NAO
     O                       LPRVGS             244                             P.SIMPLES A.VIAGEM
     O                       LRESVG             255                             P.RESSEGURO A.VIAGEM
     O                       LPRTVG             266                             P.TOTAL A.VIAGEM
     O                       LPJUR              267                             P.JURIDICA SIM/NAO
     O                       LPRPJS             278                             P.SIMPLES P.JURIDICA
     O                       LRESPJ             289                             P.RESSEGURO P.JURIDICA
     O                       LPRTPJ             300                             P.TOTAL P.JURIDICA
     O                       LPRSVG             311
     O                       LPRSPJ             322
     O*
     O                       LCOMLI             333
     O                       LCOSLI             337
     O*
     O                       LKEY               376                             CHAVE DO REGISTO
     O                       LDTTRF             384                             DATA DA TRANFERÊNCIA
     OLFRECC    EADD         RGCC32
     O                       LRAMO                2                             RAMO
     O                       LAPOL                9                             APÓLICE
     O                       LSEG                15                             NR. SEGURADO
     O                       LNREC               21                             NUMERO RECIBO
     O                       LEREC               22                             ESPECIE DE RECIBO
     O                       LCAP                35                             CAPITAL
     O                       LCAPF               48                             CAPITAL  PARA FAT
     O                       LPCOM               59                             PREMIO COMERCIAL
     O                       LFGA                70                             FUNDO GARANTIA AUTO (FGA)
     O                       LFFAT               81                             FUNDOS / FAT
     O                       LCRC                92                             VALOR CARTA VERDE (CARTAO)
     O                       LCUSTA             103                             VALOR CUSTO DE APOLICE
     O                       LSELO              114                             VALOR IMPOSTO SELO
     O                       LINEM              125                             VALOR DO INEM
     O                       LSNB               136                             VALOR DO SBB
     O                       LTREC              147                             VALOR TOTAL RECIBO
     O                       LPCOMI             158                             PREMIO PARA COMISSÕES
     O                       LDTEMI             166                             DATA DE EMISSÃO
     O                       LDTINI             174                             DATA DE INICIO
     O                       LDTERM             182                             DATA TERMO
     O                       LCANUL             184                             CODIGO ANULAÇÃO RECIBO
     O                       LDTANU             192                             DATA ANULAÇÃO RECIBO
     O                       LNCV1              198                             NR CARTA VERDE 1.VEICULO
     O                       LNCV2              204                             NR CARTA VERDE 2.VEICULO
     O                       LCDMOV             205                             CÒDIGO DO MOVIMENTO
     O                       LVFRAC             216                             PREMIO DA FRACÇÃO
     O                       LCOMI              227                             COMISSÃO (JOSÉ MATA)
     O                       LPR                232                             COMISSÃO (JOSÉ MATA)
     O*
     O                       LVIAG              233                             A.VIAGEM SIM/NAO
     O                       LPRVGS             244                             P.SIMPLES A.VIAGEM
     O                       LRESVG             255                             P.RESSEGURO A.VIAGEM
     O                       LPRTVG             266                             P.TOTAL A.VIAGEM
     O                       LPJUR              267                             P.JURIDICA SIM/NAO
     O                       LPRPJS             278                             P.SIMPLES P.JURIDICA
     O                       LRESPJ             289                             P.RESSEGURO P.JURIDICA
     O                       LPRTPJ             300                             P.TOTAL P.JURIDICA
     O                       LPRSVG             311
     O                       LPRSPJ             322
     O*
     O                       LCOMLI             333
     O                       LCOSLI             337
     O*
     O                       LKEY               376                             CHAVE DO REGISTO
     O                       LDTTRF             384                             DATA DA TRANFERÊNCIA
     O****************************************************
     O*  ADICIONA REGISTOS - RECIBOS NORMAIS             *
     O****************************************************
     OEFREC     EADD         REGEMI
     O                       LRAMO                2                             RAMO
     O                       LAPOL                9                             APÓLICE
     O                       LSEG                15                             NR. SEGURADO
     O                       LNREC               21                             NUMERO RECIBO
     O                       LEREC               22                             ESPECIE DE RECIBO
     O                       LCAP                35                             CAPITAL
     O                       LCAPF               48                             CAPITAL  PARA FAT
     O                       LPCOM               59                             PREMIO COMERCIAL
     O                       LFGA                70                             FUNDO GARANTIA AUTO (FGA)
     O                       LFFAT               81                             FUNDOS / FAT
     O                       LCRC                92                             VALOR CARTA VERDE (CARTAO)
     O                       LCUSTA             103                             VALOR CUSTO DE APOLICE
     O                       LSELO              114                             VALOR IMPOSTO SELO
     O                       LINEM              125                             VALOR DO INEM
     O                       LSNB               136                             VALOR DO SBB
     O                       LTREC              147                             VALOR TOTAL RECIBO
     O                       LPCOMI             158                             PREMIO PARA COMISSÕES
     O                       LDTEMI             166                             DATA DE EMISSÃO
     O                       LDTINI             174                             DATA DE INICIO
     O                       LDTERM             182                             DATA TERMO
     O                       LCANUL             184                             CODIGO ANULAÇÃO RECIBO
     O                       LDTANU             192                             DATA ANULAÇÃO RECIBO
     O                       LNCV1              198                             NR CARTA VERDE 1.VEICULO
     O                       LNCV2              204                             NR CARTA VERDE 2.VEICULO
     O                       LCDMOV             205                             CÒDIGO DO MOVIMENTO
     O                       LVFRAC             216                             PREMIO DA FRACÇÃO
     O                       LCOMI              227                             COMISSÃO (JOSÉ MATA)
     O                       LPR                232                             COMISSÃO (JOSÉ MATA)
     O*
     O                       LVIAG              233                             A.VIAGEM SIM/NAO
     O                       LPRVGS             244                             P.SIMPLES A.VIAGEM
     O                       LRESVG             255                             P.RESSEGURO A.VIAGEM
     O                       LPRTVG             266                             P.TOTAL A.VIAGEM
     O                       LPJUR              267                             P.JURIDICA SIM/NAO
     O                       LPRPJS             278                             P.SIMPLES P.JURIDICA
     O                       LRESPJ             289                             P.RESSEGURO P.JURIDICA
     O                       LPRTPJ             300                             P.TOTAL P.JURIDICA
     O                       LPRSVG             311
     O                       LPRSPJ             322
     O*
     O                       LCOMLI             333
     O                       LCOSLI             337
     O*
     O                       LKEY               376                             CHAVE DO REGISTO
     O                       LDTTRF             384                             DATA DA TRANFERÊNCIA
     O****************************************************
     OEFREC     EADD         REGANU
     O                       LRAMO                2                             RAMO
     O                       LAPOL                9                             APÓLICE
     O                       LSEG                15                             NR. SEGURADO
     O                       LNREC               21                             NUMERO RECIBO
     O                       LEREC               22                             ESPECIE DE RECIBO
     O                       LCAP                35                             CAPITAL
     O                       LCAPF               48                             CAPITAL  PARA FAT
     O                       LPCOM               59                             PREMIO COMERCIAL
     O                       LFGA                70                             FUNDO GARANTIA AUTO (FGA)
     O                       LFFAT               81                             FUNDOS / FAT
     O                       LCRC                92                             VALOR CARTA VERDE (CARTAO)
     O                       LCUSTA             103                             VALOR CUSTO DE APOLICE
     O                       LSELO              114                             VALOR IMPOSTO SELO
     O                       LINEM              125                             VALOR DO INEM
     O                       LSNB               136                             VALOR DO SBB
     O                       LTREC              147                             VALOR TOTAL RECIBO
     O                       LPCOMI             158                             PREMIO PARA COMISSÕES
     O                       LDTEMI             166                             DATA DE EMISSÃO
     O                       LDTINI             174                             DATA DE INICIO
     O                       LDTERM             182                             DATA TERMO
     O                       LCANUL             184                             CODIGO ANULAÇÃO RECIBO
     O                       LDTANU             192                             DATA ANULAÇÃO RECIBO
     O                       LNCV1              198                             NR CARTA VERDE 1.VEICULO
     O                       LNCV2              204                             NR CARTA VERDE 2.VEICULO
     O                       LCDMOV             205                             CÒDIGO DO MOVIMENTO
     O                       LVFRAC             216                             PREMIO DA FRACÇÃO
     O                       LCOMI              227                             COMISSÃO (JOSÉ MATA)
     O                       LPR                232                             COMISSÃO (JOSÉ MATA)
     O*
     O                       LVIAG              233                             A.VIAGEM SIM/NAO
     O                       LPRVGS             244                             P.SIMPLES A.VIAGEM
     O                       LRESVG             255                             P.RESSEGURO A.VIAGEM
     O                       LPRTVG             266                             P.TOTAL A.VIAGEM
     O                       LPJUR              267                             P.JURIDICA SIM/NAO
     O                       LPRPJS             278                             P.SIMPLES P.JURIDICA
     O                       LRESPJ             289                             P.RESSEGURO P.JURIDICA
     O                       LPRTPJ             300                             P.TOTAL P.JURIDICA
     O                       LPRSVG             311
     O                       LPRSPJ             322
     O*
     O                       LCOMLI             333
     O                       LCOSLI             337
     O*
     O                       LKEY               376                             CHAVE DO REGISTO
     O                       LDTTRF             384                             DATA DA TRANFERÊNCIA
     O*
     O****************************************************
     O*  ADICIONA REGISTOS - RECIBOS DOMISSÃO DE RAPPEL  *
     O****************************************************
     OEFRECC    EADD         REGCC
     O                       LRAMO                2                             RAMO
     O                       LAPOL                9                             APÓLICE
     O                       LSEG                15                             NR. SEGURADO
     O                       LNREC               21                             NUMERO RECIBO
     O                       LEREC               22                             ESPECIE DE RECIBO
     O                       LCAP                35                             CAPITAL
     O                       LCAPF               48                             CAPITAL  PARA FAT
     O                       LPCOM               59                             PREMIO COMERCIAL
     O                       LFGA                70                             FUNDO GARANTIA AUTO (FGA)
     O                       LFFAT               81                             FUNDOS / FAT
     O                       LCRC                92                             VALOR CARTA VERDE (CARTAO)
     O                       LCUSTA             103                             VALOR CUSTO DE APOLICE
     O                       LSELO              114                             VALOR IMPOSTO SELO
     O                       LINEM              125                             VALOR DO INEM
     O                       LSNB               136                             VALOR DO SBB
     O                       LTREC              147                             VALOR TOTAL RECIBO
     O                       LPCOMI             158                             PREMIO PARA COMISSÕES
     O                       LDTEMI             166                             DATA DE EMISSÃO
     O                       LDTINI             174                             DATA DE INICIO
     O                       LDTERM             182                             DATA TERMO
     O                       LCANUL             184                             CODIGO ANULAÇÃO RECIBO
     O                       LDTANU             192                             DATA ANULAÇÃO RECIBO
     O                       LNCV1              198                             NR CARTA VERDE 1.VEICULO
     O                       LNCV2              204                             NR CARTA VERDE 2.VEICULO
     O                       LCDMOV             205                             CÒDIGO DO MOVIMENTO
     O                       LVFRAC             216                             PREMIO DA FRACÇÃO
     O                       LCOMI              227                             COMISSÃO (JOSÉ MATA)
     O                       LPR                232                             COMISSÃO (JOSÉ MATA)
     O*
     O                       LVIAG              233                             A.VIAGEM SIM/NAO
     O                       LPRVGS             244                             P.SIMPLES A.VIAGEM
     O                       LRESVG             255                             P.RESSEGURO A.VIAGEM
     O                       LPRTVG             266                             P.TOTAL A.VIAGEM
     O                       LPJUR              267                             P.JURIDICA SIM/NAO
     O                       LPRPJS             278                             P.SIMPLES P.JURIDICA
     O                       LRESPJ             289                             P.RESSEGURO P.JURIDICA
     O                       LPRTPJ             300                             P.TOTAL P.JURIDICA
     O                       LPRSVG             311
     O                       LPRSPJ             322
     O*
     O                       LCOMLI             333
     O                       LCOSLI             337
     O*
     O                       LKEY               376                             CHAVE DO REGISTO
     O                       LDTTRF             384                             DATA DA TRANFERÊNCIA
     O****************************************************
     O*  ADICIONA REG.TRANSP-RECIBOS NORMAIS             *
     O****************************************************
     OEFRECT    EADD         REGE32
     O                       LRAMO                2                             RAMO
     O                       LAPOL                9                             APÓLICE
     O                       LSEG                15                             NR. SEGURADO
     O                       LNREC               21                             NUMERO RECIBO
     O                       LEREC               22                             ESPECIE DE RECIBO
     O                       LCAP                35                             CAPITAL
     O                       LCAPF               48                             CAPITAL  PARA FAT
     O                       LPCOM               59                             PREMIO COMERCIAL
     O                       LFGA                70                             FUNDO GARANTIA AUTO (FGA)
     O                       LFFAT               81                             FUNDOS / FAT
     O                       LCRC                92                             VALOR CARTA VERDE (CARTAO)
     O                       LCUSTA             103                             VALOR CUSTO DE APOLICE
     O                       LSELO              114                             VALOR IMPOSTO SELO
     O                       LINEM              125                             VALOR DO INEM
     O                       LSNB               136                             VALOR DO SBB
     O                       LTREC              147                             VALOR TOTAL RECIBO
     O                       LPCOMI             158                             PREMIO PARA COMISSÕES
     O                       LDTEMI             166                             DATA DE EMISSÃO
     O                       LDTINI             174                             DATA DE INICIO
     O                       LDTERM             182                             DATA TERMO
     O                       LCANUL             184                             CODIGO ANULAÇÃO RECIBO
     O                       LDTANU             192                             DATA ANULAÇÃO RECIBO
     O                       LNCV1              198                             NR CARTA VERDE 1.VEICULO
     O                       LNCV2              204                             NR CARTA VERDE 2.VEICULO
     O                       LCDMOV             205                             CÒDIGO DO MOVIMENTO
     O                       LVFRAC             216                             PREMIO DA FRACÇÃO
     O                       LCOMI              227                             COMISSÃO (JOSÉ MATA)
     O                       LPR                232                             COMISSÃO (JOSÉ MATA)
     O*
     O                       LVIAG              233                             A.VIAGEM SIM/NAO
     O                       LPRVGS             244                             P.SIMPLES A.VIAGEM
     O                       LRESVG             255                             P.RESSEGURO A.VIAGEM
     O                       LPRTVG             266                             P.TOTAL A.VIAGEM
     O                       LPJUR              267                             P.JURIDICA SIM/NAO
     O                       LPRPJS             278                             P.SIMPLES P.JURIDICA
     O                       LRESPJ             289                             P.RESSEGURO P.JURIDICA
     O                       LPRTPJ             300                             P.TOTAL P.JURIDICA
     O                       LPRSVG             311
     O                       LPRSPJ             322
     O*
     O                       LCOMLI             333
     O                       LCOSLI             337
     O*
     O                       LKEY               376                             CHAVE DO REGISTO
     O                       LDTTRF             384                             DATA DA TRANFERÊNCIA
     O****************************************************
     OEFRECT    EADD         REGA32
     O                       LRAMO                2                             RAMO
     O                       LAPOL                9                             APÓLICE
     O                       LSEG                15                             NR. SEGURADO
     O                       LNREC               21                             NUMERO RECIBO
     O                       LEREC               22                             ESPECIE DE RECIBO
     O                       LCAP                35                             CAPITAL
     O                       LCAPF               48                             CAPITAL  PARA FAT
     O                       LPCOM               59                             PREMIO COMERCIAL
     O                       LFGA                70                             FUNDO GARANTIA AUTO (FGA)
     O                       LFFAT               81                             FUNDOS / FAT
     O                       LCRC                92                             VALOR CARTA VERDE (CARTAO)
     O                       LCUSTA             103                             VALOR CUSTO DE APOLICE
     O                       LSELO              114                             VALOR IMPOSTO SELO
     O                       LINEM              125                             VALOR DO INEM
     O                       LSNB               136                             VALOR DO SBB
     O                       LTREC              147                             VALOR TOTAL RECIBO
     O                       LPCOMI             158                             PREMIO PARA COMISSÕES
     O                       LDTEMI             166                             DATA DE EMISSÃO
     O                       LDTINI             174                             DATA DE INICIO
     O                       LDTERM             182                             DATA TERMO
     O                       LCANUL             184                             CODIGO ANULAÇÃO RECIBO
     O                       LDTANU             192                             DATA ANULAÇÃO RECIBO
     O                       LNCV1              198                             NR CARTA VERDE 1.VEICULO
     O                       LNCV2              204                             NR CARTA VERDE 2.VEICULO
     O                       LCDMOV             205                             CÒDIGO DO MOVIMENTO
     O                       LVFRAC             216                             PREMIO DA FRACÇÃO
     O                       LCOMI              227                             COMISSÃO (JOSÉ MATA)
     O                       LPR                232                             COMISSÃO (JOSÉ MATA)
     O*
     O                       LVIAG              233                             A.VIAGEM SIM/NAO
     O                       LPRVGS             244                             P.SIMPLES A.VIAGEM
     O                       LRESVG             255                             P.RESSEGURO A.VIAGEM
     O                       LPRTVG             266                             P.TOTAL A.VIAGEM
     O                       LPJUR              267                             P.JURIDICA SIM/NAO
     O                       LPRPJS             278                             P.SIMPLES P.JURIDICA
     O                       LRESPJ             289                             P.RESSEGURO P.JURIDICA
     O                       LPRTPJ             300                             P.TOTAL P.JURIDICA
     O                       LPRSVG             311
     O                       LPRSPJ             322
     O*
     O                       LCOMLI             333
     O                       LCOSLI             337
     O*
     O                       LKEY               376                             CHAVE DO REGISTO
     O                       LDTTRF             384                             DATA DA TRANFERÊNCIA
     O*
     O****************************************************
     O*  ADICIONA REG.TRANSP-RECIBOS COMISSÃO DE RAPPEL  *
     O****************************************************
     OEFRECCT   EADD         RGCC32
     O                       LRAMO                2                             RAMO
     O                       LAPOL                9                             APÓLICE
     O                       LSEG                15                             NR. SEGURADO
     O                       LNREC               21                             NUMERO RECIBO
     O                       LEREC               22                             ESPECIE DE RECIBO
     O                       LCAP                35                             CAPITAL
     O                       LCAPF               48                             CAPITAL  PARA FAT
     O                       LPCOM               59                             PREMIO COMERCIAL
     O                       LFGA                70                             FUNDO GARANTIA AUTO (FGA)
     O                       LFFAT               81                             FUNDOS / FAT
     O                       LCRC                92                             VALOR CARTA VERDE (CARTAO)
     O                       LCUSTA             103                             VALOR CUSTO DE APOLICE
     O                       LSELO              114                             VALOR IMPOSTO SELO
     O                       LINEM              125                             VALOR DO INEM
     O                       LSNB               136                             VALOR DO SBB
     O                       LTREC              147                             VALOR TOTAL RECIBO
     O                       LPCOMI             158                             PREMIO PARA COMISSÕES
     O                       LDTEMI             166                             DATA DE EMISSÃO
     O                       LDTINI             174                             DATA DE INICIO
     O                       LDTERM             182                             DATA TERMO
     O                       LCANUL             184                             CODIGO ANULAÇÃO RECIBO
     O                       LDTANU             192                             DATA ANULAÇÃO RECIBO
     O                       LNCV1              198                             NR CARTA VERDE 1.VEICULO
     O                       LNCV2              204                             NR CARTA VERDE 2.VEICULO
     O                       LCDMOV             205                             CÒDIGO DO MOVIMENTO
     O                       LVFRAC             216                             PREMIO DA FRACÇÃO
     O                       LCOMI              227                             COMISSÃO (JOSÉ MATA)
     O                       LPR                232                             COMISSÃO (JOSÉ MATA)
     O*
     O                       LVIAG              233                             A.VIAGEM SIM/NAO
     O                       LPRVGS             244                             P.SIMPLES A.VIAGEM
     O                       LRESVG             255                             P.RESSEGURO A.VIAGEM
     O                       LPRTVG             266                             P.TOTAL A.VIAGEM
     O                       LPJUR              267                             P.JURIDICA SIM/NAO
     O                       LPRPJS             278                             P.SIMPLES P.JURIDICA
     O                       LRESPJ             289                             P.RESSEGURO P.JURIDICA
     O                       LPRTPJ             300                             P.TOTAL P.JURIDICA
     O                       LPRSVG             311
     O                       LPRSPJ             322
     O*
     O                       LCOMLI             333
     O                       LCOSLI             337
     O*
     O                       LKEY               376                             CHAVE DO REGISTO
     O                       LDTTRF             384                             DATA DA TRANFERÊNCIA
     O****************************************************
     O* IMPRIME MAPA ERROS (REG. INEXISTENTES NO GCOBGR  *
     O****************************************************
     OMERROS    E            CAB1             04
     O                                          027 'LUSITANIA-JOSE MATA, LDA'
     O                                          070 'DATA'
     O                       UDATE              080 '  /  /  '
     O*
     O          E            CAB1             05
     O                       PAGE               080
     O*
     O          E            CAB1             07
     O                                          027 '*** MAPA DE ERROS - TRAN'
     O                                          048 'SFERÊNCIA RECIBOS ***'
     O*
     O          E            ERRO00      1
     O                       GHNREC             010
     O                       GHRRAP             021
     O                                          071 'INEXISTENTE NO GCOBGR/CO'
     O*
     O          E    60      ERRO01      1
     O               60      GHNREC             010
     O               60      GHRRAP             021
     O               60      CAMPO              031
     O               60      DCAMPO             045
     O               60                         065 'COM VALOR NEGATIVO'
     O*
     O          E    70      ERRO02      1
     O               70      GXPRS              010
     O               70      GXTREC             020
     O               70      GXRRAP             031
     O               70                         065 'COM VALOR NEGATIVO'
     O****************************************************
**              *** TABELA DE CONVERSAO DE RAMOS
0108                   * INCÊNDIO E E.NATUREZA
0201                   * ACIDENTES PESSOAIS
0313                   * RESP. CIVIL PESSOAL
0413                   * RESP. CIVIL COM./IND.
0534                   * RISCOS M.HABITAÇÃO
0616                   * PERDAS DE LUCROS
0709                   * ROUBO VALORES
0809                   * ROUBO RESIDÊNCIAS
0909                   * ROUBO ESTABELECIMENTOS
1001                   * VIAGENS
1430                   * DOENÇA
1501                   * ACIDENTES DE TRABALHO
1734                   * EQUIPAMENTO ELECTRÓNICO
1809                   * AVARIA DE MÁQUINAS
1934                   * OBRAS E MONTAGENS
5101                   * OCUPANTES
5031                   * AUTOMÓVEL
5534                   * RISCOS M.ESTABELECIMENTOS
6509                   * CRISTAIS
6707                   * BAGAGEM
6807                   * MARÍTIMO TERRESTRE
6907                   * POSTAL
7007                   * AÉREO
**              *** TABELA DE CONVERSAO DE ESPECIE DE RECIBO
01                     * NOVO VS PRÉMIO FIXO (1º RECIBO)
16                     * ADICIONAL VS PREMIO ACTA ADICIONAL
23                     * CONTINUADO VS CONTINUADO FIXO
30                     * ANULAÇAO DE ESTORNO VS SIT. RECIBO ESTORNO = ANULADO
40                     * ANULADO VS SIT. RECIBO PRÉMIO = ANULADO
59                     * ESTORNO VS ESTORNO
**              *** TABELA DE CONVERSAO DE CÓDIGO DE ANULAÇAO DE RECIBO
101
202
**              *** TABELA DE IDENTIFICAÇAO DE RECIBOS SÓ DE COMISSAO JM.
019999999
029919999
039919999
049999999
059929999
069999999
079909999
089919999
099929999
109939999
149999999
159999999
179949999
189949999
199959999
509999999
519979999
559999999
659999999
679999999
689999999
699919999
709939999
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
