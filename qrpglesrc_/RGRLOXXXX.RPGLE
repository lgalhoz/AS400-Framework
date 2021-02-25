    **
      ** SERVIDORES DO i5/OS - PROGRAMA DE SAÍDA DE UTILIZADOR EXEMPLO
      **
      ** O PROGRAMA RPG QUE SE SEGUE ACEITA INCONDICIONALMENTE
      ** ACEITA TODOS OS PEDIDOS. PODE SER UTILIZADO COMO INTERFACE
      ** PARA APLICAÇÕES ESPECÍFICAS. NOTA: REMOVA AS
      ** SUBROTINAS E AS INSTRUÇÕES EM MAIÚSCULAS PARA OS SERVIDORES
      ** QUE NÃO REQUEREM O TRATAMENTO ESPECÍFICO DE PROGRAMAS
      ** DE SAÍDA, PARA OBTER UM MELHOR DESEMPENHO.       **
     E*
     E* DEFINIÇÕES DE MATRIZES NECESSÁRIAS PARA A FUNÇÃO DE TRANSFERÊNCIA
     E* E SQL REMOTA
     E*
     E                    TFREQ    4096  1
     E                    RSREQ    4107  1
     I*
     I*
     IPCSDTA      DS
     I                                        1  10 USERID
     I                                       11  20 APPLID
     I*
     I* PARÂMETROS ESPECÍFICOS PARA IMPRESSORA VIRTUAL
     I*
     I                                       21  30 VPFUNC
     I                                       31  40 VPOBJ
     I                                       41  50 VPLIB
     I                                       71  750VPIFN
     I                                       76  85 VPOUTQ
     I                                       86  95 VPQLIB
     I*
     I* PARÂMETROS ESPECÍFICOS PARA A FUNÇÃO DE MENSAGENS
     I                                       21  30 MFFUNC
     I*
     I* PARÂMETROS ESPECÍFICOS PARA A FUNÇÃO DE TRANSFERÊNCIA
     I*
     I                                       21  30 TFFUNC
     I                                       31  40 TFOBJ
     I                                       41  50 TFLIB
     I                                       51  60 TFMBR
     I                                       61  70 TFFMT
     I                                       71  750TFLEN
     I                                       764171 TFREQ
     I*
     I* PARÂMETROS ESPECÍFICOS PARA O SERVIDOR DE FICHEIROS
     I*
     I* NOTA: FSNAME PODE TER ATÉ l6MB.
     I* FSNLEN CONTERÁ O TAMANHO REAL DE FSNAME.
     I*
     I                                    B  21  240FSFID
     I                                       25  32 FSFMT
     I                                       33  33 FSREAD
     I                                       34  34 FSWRIT
     I                                       35  35 FSRDWR
     I                                       36  36 FSDLT
     I                                    B  37  400FSNLEN
     I                                       41 296 FSNAME
     I*
     I* PARÂMETROS ESPECÍFICOS PARA FILAS DE DADOS
     I*
     I                                       21  30 DQFUNC
     I                                       31  40 DQQ
     I                                       41  50 DQLIB
     I                                       70  750DQLEN
     I                                       76  77 DQROP
     I                                       78  820DQKLEN
     I                                       83 338 DQKEY
     I*
     I* PARÂMETROS ESPECÍFICOS PARA SQL REMOTA
     I*
     I                                       21  30 RSFUNC
     I                                       31  40 RSOBJ
     I                                       41  50 RSLIB
     I                                       51  51 RSCMT
     I                                       52  52 RSMODE
     I                                       53  53 RSCID
     I                                       54  71 RSSTN
     I                                       72  75 RSRSV
     I                                       764182 RSREQ
     I*
     I* PARÂMETROS ESPECÍFICOS PARA O SERVIDOR DE IMPRESSÃO NA REDE
     I*
     I                                       21  28 NPFT
     I                                    B  29  320NPFID
     I* OS SEGUINTES PARÂMETROS SÃO ADICIONAIS PARA O FORMATO SPLF0l00
     I                                       33  42 NPJOBN
     I                                       43  52 NPUSRN
     I                                       53  58 NPJOB#
     I                                       59  68 NPFILE
     I                                    B  69  720NPFIL#
     I                                    B  73  760NPLEN
     I                                       77 332 NPDATA
     I*
     I* Servidor de fila de dados:
     I*
     I* QIBM_QZHQ_DATA_QUEUE  formato ZHQ00100
     I*
     I                                       21  28 DQOFMT
     I                                    B  29  320DQOFID
     I                                       33  42 DQOOBJ
     I                                       43  52 DQOLIB
     I                                       53  54 DQOROP
     I                                    B  55  580DQOLEN
     I                                       59 314 DQOKEY
     I*
     I* PARÂMETROS ESPECÍFICOS PARA O SERVIDOR CENTRAL
     I*
     I                                       21  28 CSFMT
     I                                    B  29  320CSFID
     I* Servidor central:
     I*
     I* QIBM_QZSC_LM formato ZSCL0l00 para chamadas de gestão de licenças
     I*
     I*
     I                                       33 287 CSLCNM
     I                                      288 295 CSLUSR
     I                                      296 302 CSLPID
     I                                      303 306 CSLFID
     I                                      307 312 CSLRID
     I                                    B 313 3140CSLTYP
     I*
     I* Servidor central:
     I*
     I* QIBM_QZSC_LM formato ZSCS0l00 para chamadas de gestão de sistema
     I*
     I*
     I                                       33 287 CSSCNM
     I                                      288 542 CSSCMY
     I                                      543 543 CSSNDE
     I                                      544 798 CSSNNM
     I*
     I* Servidor central:
     I*
     I* QIBM_QZSC_LM formato ZSCN0l00 para obter chamadas de definição de
     I* conversão
     I*
     I*
     I                                       21  30 CSNXFM
     I                                       29  320CSNFNC
     I                                    B  33  360CSNFRM
     I                                    B  37  400CSNTO
     I                                    B  41  420CSNCNT
     I*
     I* PARÂMETROS ESPECÍFICOS PARA O SERVIDOR DE BASES DE DADOS
     I*
     I                                       21  28 DBFMT
     I                                    B  29  320DBFID
     I*
     I* OS SEGUINTES PARÂMETROS SÃO ADICIONAIS PARA O FORMATO ZDAD0l00
     I                                       33 160 DBDFIL
     I                                      161 170 DBDLIB
     I                                      171 180 DBDMBR
     I                                      181 190 DBDAUT
     I                                      191 318 DBDBFL
     I                                      319 328 DBDBLB
     I                                      329 338 DBDOFL
     I                                      339 348 DBDOLB
     I                                      349 358 DBDOMB
     I*
     I* OS SEGUINTES PARÂMETROS SÃO ADICIONAIS PARA O FORMATO ZDAD0200
     I                                    B  33  360DBNUM
     I                                       37  46 DBLIB2
     I*
     I* OS SEGUINTES PARÂMETROS SÃO ADICIONAIS PARA O FORMATO ZDAQ0l00
     I                                       33  50 DBSTMT
     I                                       51  68 DBCRSR
     I                                       69  70 DBOPI
     I                                       71  72 DBATTR
     I                                       73  82 DBPKG
     I                                       83  92 DBPLIB
     I                                    B  93  940DBDRDA
     I                                       95  95 DBCMT
     I                                       96 351 DBTEXT
     I* OS SEGUINTES PARÂMETROS SUBSTITUEM DBTEXT PARA O FORMATO ZDAQ0200
     I                                       96 105 DBSQCL
     I                                    B 133 1360DBSQLN
     I                                      137 392 DBSQTX
     I* OS SEGUINTES PARÂMETROS SÃO ADICIONAIS PARA O FORMATO ZDAR0l00
     I                                       33  52 DBLIBR
     I                                       53  88 DBRDBN
     I                                       89 108 DBPKGR
     I                                      109 364 DBFILR
     I                                      365 384 DBMBRR
     I                                      385 404 DBFFT
     I* OS SEGUINTES PARÂMETROS SÃO ADICIONAIS PARA O FORMATO ZDAR0200
     I                                       33  42 DBRPLB
     I                                       43 170 DBRPTB
     I                                      171 180 DBRFLB
     I                                      181 308 DBRFTB
     I*
     I* Servidor de comandos remotos e chamadas de programa distribuídas:
     I*
     I* QIBM_QZRC_RMT  formato CZRC0100
     I*    RCPGM E RCLIB NÃO SÃO UTILIZADOS PARA CHAMADAS DE COMANDOS REMOTOS
     I*
     I                                       21  28 RCFMT
     I                                    B  29  320RCFID
     I                                       33  42 RCPGM
     I                                       43  52 RCLIB
     I                                    B  53  560RCNUM
     I                                       57 312 RCDATA
     I*
     I* servidor de início de sessão
     I*
     I* QIBM_QZSO_SIGNONSRV formato ZSOY0l00 para o servidor de início de sessão de TCP/IP
     I*
     I                                       21  28 SOXFMT
     I                                    B  29  320SOFID
     I*

     I***************************************************************
     I*
     I              '*VPRT     '          C         #VPRT
     I              '*TFRFCL   '          C         #TRFCL
     I              '*FILESRV  '          C         #FILE
     I              '*MSGFCL   '          C         #MSGF
     I              '*DQSRV    '          C         #DQSRV
     I              '*RQSRV    '          C         #RQSRV
     I              '*SQL      '          C         #SQL
     I              '*NDB      '          C         #NDBSV
     I              '*SQLSRV   '          C         #SQLSV
     I              '*RTVOBJINF'          C         #RTVOB
     I              '*DATAQSRV '          C         #DATAQ
     I              'QNPSERVR  '          C         #QNPSV
     I              '*CNTRLSRV '          C         #CNTRL
     I              '*RMTSRV   '          C         #RMTSV
     I              '*SIGNON   '          C         #SIGN
     I*
     C*
     C* PARÂMETROS DE CHAMADAS DE PROGRAMAS DE SAÍDA
     C*
     C           *ENTRY    PLIST
     C                     PARM           RTNCD   1
     C                     PARM           PCSDTA
     C*
     C* INICIALIZAR VALOR DE RETORNO PARA ACEITAR PEDIDO
     C*
     C                     MOVE '1'       RTNCD
     C*
     C* PROCESSAMENTO COMUM
     C*
     C*              LÓGICA COMUM INDICADA AQUI
     C*
     C* PROCESSO BASEADO NO ID DO SERVIDOR
     C*
     C           APPLID    CASEQ#VPRT     VPRT
     C           APPLID    CASEQ#TRFCL    TFR
     C           APPLID    CASEQ#FILE     FILE
     C           APPLID    CASEQ#MSGF     MSG
     C           APPLID    CASEQ#DQSRV    DATAQ
     C           APPLID    CASEQ#RQSRV    RSQL
     C           APPLID    CASEQ#SQL      SQLINT
     C           APPLID    CASEQ#NDBSV    NDB
     C           APPLID    CASEQ#SQLSV    SQLSRV
     C           APPLID    CASEQ#RTVOB    RTVOBJ
     C           APPLID    CASEQ#DATAQ    ODATAQ
     C           APPLID    CASEQ#QNPSV    NETPRT
     C           APPLID    CASEQ#CNTRL    CENTRL
     C           APPLID    CASEQ#RMTSV    RMTCMD
     C           APPLID    CASEQ#SIGN     SIGNON
     C                     END
     C                     SETON                     LR
     C                     RETRN
     C*
     C* SUBROTINAS
     C*
     C*
     C* IMPRESSÃO VIRTUAL
     C*
     C           VPRT      BEGSR
     C*                   LÓGICA ESPECÍFICA INDICADA AQUI
     C                     ENDSR
     C*
     C* FUNÇÃO DE TRANSFERÊNCIA
     C*
     C* SEGUE-SE UM EXEMPLO DO PROCESSAMENTO ESPECÍFICO
     C* QUE O PROGRAMA DE SAÍDA PODERIA EXECUTAR PARA A FUNÇÃO DE TRANSFERÊNCIA.
     C*
     C* NESTE CASO, OS UTILIZADORES NÃO PODEM SELECCIONAR
     C* DADOS A PARTIR DOS FICHEIROS INCLUÍDOS NA BIBLIOTECA QIWS.
     C*
     C           TFR       BEGSR
     C           TFFUNC    IFEQ 'SELECT'
     C           TFLIB     ANDEQ'QIWS'
     C                     MOVE '0'       RTNCD
     C                     END
     C                     ENDSR
     C*
     C*
     C* SERVIDOR DE FICHEIROS
     C*
     C           FICH      BEGSR
     C*                   LÓGICA ESPECÍFICA INDICADA AQUI
     C                     ENDSR
     C*
     C* FUNÇÃO DE MENSAGENS
     C*
     C           MSG       BEGSR
     C*             LÓGICA ESPECÍFICA INDICADA AQUI
     C                     ENDSR
     C* FILAS DE DADOS
     C*
     C          FILADADOS  BEGSR
     C*                   LÓGICA ESPECÍFICA INDICADA AQUI
     C                     ENDSR
     C*
     C* SQL REMOTA
     C*
     C           RSQL      BEGSR
     C*                   LÓGICA ESPECÍFICA INDICADA AQUI
     C                     ENDSR
     C*
     C* SERVIDORES
     C*
     C*
     C* INIC DA BASE DE DADOS
     C*
     C           INISQL    BEGSR
     C*                   LÓGICA ESPECÍFICA INDICADA AQUI
     C                     ENDSR
     C*
     C* BASE DE DADOS BDN (BASE DE DADOS NATIVA)
     C*
     C           BDN       BEGSR
     C*             LÓGICA ESPECÍFICA INDICADA AQUI
     C                     ENDSR
     C*
     C* SQL DE BASE DE DADOS
     C*
     C           SRVSQL    BEGSR
     C*                   LÓGICA ESPECÍFICA INDICADA AQUI
     C                     ENDSR
     C*
     C* INFORMAÇÕES DE OBJECTOS DE OBTENÇÃO DA BASE DE DADOS
     C*
     C           OBTOBJ    BEGSR
     C*                   LÓGICA ESPECÍFICA INDICADA AQUI
     C                     ENDSR
     C*
     C* SERVIDOR DE FILAS DE DADOS
     C*
     C           FILADADOSO    BEGSR
     C*                   LÓGICA ESPECÍFICA INDICADA AQUI
     C                     ENDSR
     C*
     C* IMPRESSÃO NA REDE
     C*
     C           IMPREDE   BEGSR
     C*                   LÓGICA ESPECÍFICA INDICADA AQUI
     C                     ENDSR
     C*
     C* SERVIDOR CENTRAL
     C*
     C*
     C* SEGUE-SE UM EXEMPLO DO PROCESSAMENTO ESPECÍFICO
     C* DO QUE O PROGRAMA DE SAÍDA PODERIA EXECUTAR PARA A GESTÃO DE LICENÇAS.
     C*
     C* NESTE CASO, O UTILIZADOR "USERALL" NÃO PODERÁ EXECUTAR
     C* NENHUMA DAS FUNÇÕES FORNECIDAS PELO SERVIDOR
     C* CENTRAL PARA O QUAL ESTE PROGRAMA É UM PROGRAMA DE
     C* SAÍDA REGISTADO - INFORMAÇÕES SOBRE LICENÇAS, GESTÃO DE SISTEMA
     C* OU OBTER UMA DEFINIÇÃO DE CONVERSÃO.
     C*
     C           CENTRL    BEGSR
     C           USERID    IFEQ 'USERALL'
     C                     MOVE '0'       RTNCD
     C                     ENDIF
     C*                   LÓGICA ESPECÍFICA INDICADA AQUI
     C                     ENDSR
     C*
     C* COMANDO REMOTO E CHAMADA DE PROGRAMA DISTRIBUÍDA
     C*
     C* NESTE CASO, O UTILIZADOR "USERALL" NÃO PODERÁ EXECUTAR
     C* COMANDOS REMOTOS NEM CHAMADAS DE PROGRAMA REMOTAS
     C*
     C           RMTCMD    BEGSR
     C           USERID    IFEQ 'USERALL'
     C                     MOVE '0'       RTNCD
     C                     ENDIF
     C                     ENDSR
     C*
     C* SERVIDOR DE INÍCIO DE SESSÃO
     C*
     C           SIGNON    BEGSR
     C*                   LÓGICA ESPECÍFICA INDICADA AQUI
     C                     ENDSR
