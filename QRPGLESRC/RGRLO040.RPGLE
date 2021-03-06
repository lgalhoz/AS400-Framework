     H*===================================================================================
     H* Servidores i5/OS - Estrutura do Programa de Sa�da
     H* Activar com o comando WRKREGINF
     H*-----------------------------------------------------------------------------------
     H* Estes programas, s�o um incremento � seguran�a e controlo do Sistema
     H* a chamadas ODBC, e acessos FTP, entre outros ao AS400.
     H*
     H* Com esta estrutura torna-se poss�vel condicionar os acessos e detectar
     H* sinais de tentaiva de acessos n�o autorizados aos dados.
     H*===================================================================================
     D*==> Estrurua Standard
     D PCSDTA          DS
     D  USERID                 1     10
     D  APPLID                11     20
     D
     D*=> Par�metros Espec�ficos da Impressora Virtual
     D  VPFUNC                21     30
     D  VPOBJ                 31     40
     D  VPLIB                 41     50
     D  VPIFN                 71     75  0
     D  VPOUTQ                76     85
     D  VPQLIB                86     95
     D
     D*=> Par�metros espec�ficos para a fun��o demensagens
     D  MFFUNC                21     30
     D
     D*=> Par�metros Espec�ficos para a Transfer�ncia de ficheiros
     D  TFFUNC                21     30
     D  TFOBJ                 31     40
     D  TFLIB                 41     50
     D  TFMBR                 51     60
     D  TFFMT                 61     70
     D  TFLEN                 71     75  0
     D  TFREQ                 76   4171    DIM(4096)
     D
     D*-------------------------------------------------
     D* Par�metros Espec�ficos para o SGBD
     D* Nota: FSNAME -> pode ter at� l6MB.
     D*       FSNLEN -> Contem o Tamanho real do FSNAME.
     D*-------------------------------------------------
     D  FSFID                 21     24B 0
     D  FSFMT                 25     32
     D  FSREAD                33     33
     D  FSWRIT                34     34
     D  FSRDWR                35     35
     D  FSDLT                 36     36
     D  FSNLEN                37     40B 0
     D  FSNAME                41    296
     D
     D*=> Par�metro Espec�dicos para as Filas de dados (MSQ)
     D  DQFUNC                21     30
     D  DQQ                   31     40
     D  DQLIB                 41     50
     D  DQLEN                 70     75  0
     D  DQROP                 76     77
     D  DQKLEN                78     82  0
     D  DQKEY                 83    338
     D
     D*=> Par�metros Espec�ficos para SQL remota (ODBC)
     D  RSFUNC                21     30
     D  RSOBJ                 31     40
     D  RSLIB                 41     50
     D  RSCMT                 51     51
     D  RSMODE                52     52
     D  RSCID                 53     53
     D  RSSTN                 54     71
     D  RSRSV                 72     75
     D  RSREQ                 76   4182    DIM(4107)
     D
     D*=> Par�metros Espec�ficos para o Servidor de Impress�o em rede
     D  NPFT                  21     28
     D  NPFID                 29     32B 0
     D
     D*=> FORMATO SPLF0l00
     D  NPJOBN                33     42
     D  NPUSRN                43     52
     D  NPJOB#                53     58
     D  NPFILE                59     68
     D  NPFIL#                69     72B 0
     D  NPLEN                 73     76B 0
     D  NPDATA                77    332
     D
     D*---------------------------------------------------
     D* Servidor de fila de dados:
     D*     QIBM_QZHQ_DATA_QUEUE  formato ZHQ00100
     D*---------------------------------------------------
     D  DQOFMT                21     28
     D  DQOFID                29     32B 0
     D  DQOOBJ                33     42
     D  DQOLIB                43     52
     D  DQOROP                53     54
     D  DQOLEN                55     58B 0
     D  DQOKEY                59    314
     D
     D*---------------------------------------------------
     D* Par�metros Espec�ficos para o Servidor Central
     D*      QIBM_QZSC_LM formato ZSCL0l00 para chamadas
     D*      de gest�o de licen�as
     D*---------------------------------------------------
     D  CSFMT                 21     28
     D  CSFID                 29     32B 0
     D  CSLCNM                33    287
     D  CSLUSR               288    295
     D  CSLPID               296    302
     D  CSLFID               303    306
     D  CSLRID               307    312
     D  CSLTYP               313    314B 0
     D*
     D* Servidor central:
     D*
     D* QIBM_QZSC_LM formato ZSCS0l00 para chamadas de gest�o de sistema
     D*
     D*
     D  CSSCNM                33    287
     D  CSSCMY               288    542
     D  CSSNDE               543    543
     D  CSSNNM               544    798
     D*
     D* Servidor central:
     D*
     D* QIBM_QZSC_LM formato ZSCN0l00 para obter chamadas de defini��o de
     D* convers�o
     D*
     D*
     D  CSNXFM                21     30
     D  CSNFNC                29     32  0
     D  CSNFRM                33     36B 0
     D  CSNTO                 37     40B 0
     D  CSNCNT                41     42B 0
     D*
     D* PAR�METROS ESPEC�FICOS PARA O SERVIDOR DE BASES DE DADOS
     D*
     D  DBFMT                 21     28
     D  DBFID                 29     32B 0
     D*
     D* OS SEGUINTES PAR�METROS S�O ADICIONAIS PARA O FORMATO ZDAD0l00
     D  DBDFIL                33    160
     D  DBDLIB               161    170
     D  DBDMBR               171    180
     D  DBDAUT               181    190
     D  DBDBFL               191    318
     D  DBDBLB               319    328
     D  DBDOFL               329    338
     D  DBDOLB               339    348
     D  DBDOMB               349    358
     D*
     D* OS SEGUINTES PAR�METROS S�O ADICIONAIS PARA O FORMATO ZDAD0200
     D  DBNUM                 33     36B 0
     D  DBLIB2                37     46
     D*
     D* OS SEGUINTES PAR�METROS S�O ADICIONAIS PARA O FORMATO ZDAQ0l00
     D  DBSTMT                33     50
     D  DBCRSR                51     68
     D  DBOPI                 69     70
     D  DBATTR                71     72
     D  DBPKG                 73     82
     D  DBPLIB                83     92
     D  DBDRDA                93     94B 0
     D  DBCMT                 95     95
     D  DBTEXT                96    351
     D* OS SEGUINTES PAR�METROS SUBSTITUEM DBTEXT PARA O FORMATO ZDAQ0200
     D  DBSQCL                96    105
     D  DBSQLN               133    136B 0
     D  DBSQTX               137    392
     D* OS SEGUINTES PAR�METROS S�O ADICIONAIS PARA O FORMATO ZDAR0l00
     D  DBLIBR                33     52
     D  DBRDBN                53     88
     D  DBPKGR                89    108
     D  DBFILR               109    364
     D  DBMBRR               365    384
     D  DBFFT                385    404
     D* OS SEGUINTES PAR�METROS S�O ADICIONAIS PARA O FORMATO ZDAR0200
     D  DBRPLB                33     42
     D  DBRPTB                43    170
     D  DBRFLB               171    180
     D  DBRFTB               181    308
     D*
     D* Servidor de comandos remotos e chamadas de programa distribu�das:
     D*
     D* QIBM_QZRC_RMT  formato CZRC0100
     D*    RCPGM E RCLIB N�O S�O UTILIZADOS PARA CHAMADAS DE COMANDOS REMOTOS
     D*
     D  RCFMT                 21     28
     D  RCFID                 29     32B 0
     D  RCPGM                 33     42
     D  RCLIB                 43     52
     D  RCNUM                 53     56B 0
     D  RCDATA                57    312
     D*
     D* servidor de in�cio de sess�o
     D*
     D* QIBM_QZSO_SIGNONSRV formato ZSOY0l00 para o servidor de in�cio de sess�o de TCP/IP
     D*
     D  SOXFMT                21     28
     D  SOFID                 29     32B 0
     I*

     D***************************************************************
     D*
     D #VPRT           C                   CONST('*VPRT     ')
     D #TRFCL          C                   CONST('*TFRFCL   ')
     D #FILE           C                   CONST('*FILESRV  ')
     D #MSGF           C                   CONST('*MSGFCL   ')
     D #DQSRV          C                   CONST('*DQSRV    ')
     D #RQSRV          C                   CONST('*RQSRV    ')
     D #SQL            C                   CONST('*SQL      ')
     D #NDBSV          C                   CONST('*NDB      ')
     D #SQLSV          C                   CONST('*SQLSRV   ')
     D #RTVOB          C                   CONST('*RTVOBJINF')
     D #DATAQ          C                   CONST('*DATAQSRV ')
     D #QNPSV          C                   CONST('QNPSERVR  ')
     D #CNTRL          C                   CONST('*CNTRLSRV ')
     D #RMTSV          C                   CONST('*RMTSRV   ')
     D #SIGN           C                   CONST('*SIGNON   ')
     I*
     C*
     C* PAR�METROS DE CHAMADAS DE PROGRAMAS DE SA�DA
     C*
     C     *ENTRY        PLIST
     C                   PARM                    RTNCD             1
     C                   PARM                    PCSDTA
     C*
     C* INICIALIZAR VALOR DE RETORNO PARA ACEITAR PEDIDO
     C*
     C                   MOVE      '1'           RTNCD
     C*
     C* PROCESSAMENTO COMUM
     C*
     C*              L�GICA COMUM INDICADA AQUI
     C*
     C* PROCESSO BASEADO NO ID DO SERVIDOR
     C*
     C     APPLID        CASEQ     #VPRT         VPRT
     C     APPLID        CASEQ     #TRFCL        TFR
     C     APPLID        CASEQ     #FILE         FILE
     C     APPLID        CASEQ     #MSGF         MSG
     C     APPLID        CASEQ     #DQSRV        DATAQ
     C     APPLID        CASEQ     #RQSRV        RSQL
     C     APPLID        CASEQ     #SQL          SQLINT
     C     APPLID        CASEQ     #NDBSV        NDB
     C     APPLID        CASEQ     #SQLSV        SQLSRV
     C     APPLID        CASEQ     #RTVOB        RTVOBJ
     C     APPLID        CASEQ     #DATAQ        ODATAQ
     C     APPLID        CASEQ     #QNPSV        NETPRT
     C     APPLID        CASEQ     #CNTRL        CENTRL
     C     APPLID        CASEQ     #RMTSV        RMTCMD
     C     APPLID        CASEQ     #SIGN         SIGNON
     C                   END
     C                   SETON                                        LR
     C                   RETURN
     C*
     C* SUBROTINAS
     C*
     C*
     C* IMPRESS�O VIRTUAL
     C*
     C     VPRT          BEGSR
     C*                   L�GICA ESPEC�FICA INDICADA AQUI
     C                   ENDSR
     C*
     C* FUN��O DE TRANSFER�NCIA
     C*
     C* SEGUE-SE UM EXEMPLO DO PROCESSAMENTO ESPEC�FICO
     C* QUE O PROGRAMA DE SA�DA PODERIA EXECUTAR PARA A FUN��O DE TRANSFER�NCIA.
     C*
     C* NESTE CASO, OS UTILIZADORES N�O PODEM SELECCIONAR
     C* DADOS A PARTIR DOS FICHEIROS INCLU�DOS NA BIBLIOTECA QIWS.
     C*
     C     TFR           BEGSR
     C     TFFUNC        IFEQ      'SELECT'
     C     TFLIB         ANDEQ     'QIWS'
     C                   MOVE      '0'           RTNCD
     C                   END
     C                   ENDSR
     C*
     C*
     C* SERVIDOR DE FICHEIROS
     C*
     C     FICH          BEGSR
     C*                   L�GICA ESPEC�FICA INDICADA AQUI
     C                   ENDSR
     C*
     C* FUN��O DE MENSAGENS
     C*
     C     MSG           BEGSR
     C*             L�GICA ESPEC�FICA INDICADA AQUI
     C                   ENDSR
     C* FILAS DE DADOS
     C*
     C     ILADADOS      BEGSR
     C*                   L�GICA ESPEC�FICA INDICADA AQUI
     C                   ENDSR
     C*
     C* SQL REMOTA
     C*
     C     RSQL          BEGSR
     C*                   L�GICA ESPEC�FICA INDICADA AQUI
     C                   ENDSR
     C*
     C* SERVIDORES
     C*
     C*
     C* INIC DA BASE DE DADOS
     C*
     C     INISQL        BEGSR
     C*                   L�GICA ESPEC�FICA INDICADA AQUI
     C                   ENDSR
     C*
     C* BASE DE DADOS BDN (BASE DE DADOS NATIVA)
     C*
     C     BDN           BEGSR
     C*             L�GICA ESPEC�FICA INDICADA AQUI
     C                   ENDSR
     C*
     C* SQL DE BASE DE DADOS
     C*
     C     SRVSQL        BEGSR
     C*                   L�GICA ESPEC�FICA INDICADA AQUI
     C                   ENDSR
     C*
     C* INFORMA��ES DE OBJECTOS DE OBTEN��O DA BASE DE DADOS
     C*
     C     OBTOBJ        BEGSR
     C*                   L�GICA ESPEC�FICA INDICADA AQUI
     C                   ENDSR
     C*
     C* SERVIDOR DE FILAS DE DADOS
     C*
     C     FILADADOSO    BEGSR
     C*                   L�GICA ESPEC�FICA INDICADA AQUI
     C                   ENDSR
     C*
     C* IMPRESS�O NA REDE
     C*
     C     IMPREDE       BEGSR
     C*                   L�GICA ESPEC�FICA INDICADA AQUI
     C                   ENDSR
     C*
     C* SERVIDOR CENTRAL
     C*
     C*
     C* SEGUE-SE UM EXEMPLO DO PROCESSAMENTO ESPEC�FICO
     C* DO QUE O PROGRAMA DE SA�DA PODERIA EXECUTAR PARA A GEST�O DE LICEN�AS.
     C*
     C* NESTE CASO, O UTILIZADOR "USERALL" N�O PODER� EXECUTAR
     C* NENHUMA DAS FUN��ES FORNECIDAS PELO SERVIDOR
     C* CENTRAL PARA O QUAL ESTE PROGRAMA � UM PROGRAMA DE
     C* SA�DA REGISTADO - INFORMA��ES SOBRE LICEN�AS, GEST�O DE SISTEMA
     C* OU OBTER UMA DEFINI��O DE CONVERS�O.
     C*
     C     CENTRL        BEGSR
     C     USERID        IFEQ      'USERALL'
     C                   MOVE      '0'           RTNCD
     C                   ENDIF
     C*                   L�GICA ESPEC�FICA INDICADA AQUI
     C                   ENDSR
     C*
     C* COMANDO REMOTO E CHAMADA DE PROGRAMA DISTRIBU�DA
     C*
     C* NESTE CASO, O UTILIZADOR "USERALL" N�O PODER� EXECUTAR
     C* COMANDOS REMOTOS NEM CHAMADAS DE PROGRAMA REMOTAS
     C*
     C     RMTCMD        BEGSR
     C     USERID        IFEQ      'USERALL'
     C                   MOVE      '0'           RTNCD
     C                   ENDIF
     C                   ENDSR
     C*
     C* SERVIDOR DE IN�CIO DE SESS�O
     C*
     C     SIGNON        BEGSR
     C*                   L�GICA ESPEC�FICA INDICADA AQUI
     C                   ENDSR
