      **************************************************************************
      * JOSÉ MATA - CORRECTORES E CONSULTORES DE SEGUROS                       *
      **************************************************************************
      * DESCRIÇÃO: PROGRAMA DE MANUTENÇÃO DE PESSOAS                           *
      * AUTOR:     PAULO FONSECA                                               *
      * DATA:      15.05.2008                                                  *
      **************************************************************************
      *ALTERAÇÕES
      *
      * 15.05.2008 PF CRIAÇÃO DO PGM
      **************************************************************************
      * Alterações
      * 26.04.2011 Luis Galhoz
      * Conversão do código para ILE, expansão do campo de EMAIL
      * Inclusão do Campo Código de Empregado
      **************************************************************************
     FGFSEG     UF   E           K DISK
     FSD1PE     UF A E           K DISK
     FGFSEGEXT  UF A E           K DISK
     FSD1CP     IF   F   64     7AIDISK    KEYLOC(2)
     FAUESPEF   IF   E           K DISK
     FHAGSEGF   O  A F   50        DISK
     FOP0300E   CF   E             WORKSTN
      **************************************************************************
     D MSGD            S             40    DIM(15) CTDATA PERRCD(1)
      *
     D PSTAT          SDS
     D  USR                  254    263
      *
     D                 DS
     D  DATALT                 1      8  0
     D  AA                     1      4  0
     D  MM                     5      6  0
     D  DD                     7      8  0
      *
     D                 DS
     D  DUA                    1      6  0
     D  D                      1      2  0
     D  M                      3      4  0
     D  A                      5      6  0
      *
     D                 DS
     D  WKDATA                 1      8  0
     D  WMM                    1      2  0
     D  WDD                    3      4  0
     D  WAA                    5      8  0
     D  WIcLoaded      S               N
      *==================================================================*
      * INDICADORES
      * 03-Sair
      * 04-Consulta
      * 05-Mudar Ordenação
      * 06-Adicionar
      * 10-Controle de erros
      * 11-Controle de erros
      * 12-Tecla de voltar
      * 13-CHAIN ao fx CPostal
      * 15-Controle de erros
      * 20-SFLDSP
      * 21-SFLCLR
      * 25-READ SFL
      * 26-Indicador do 1º registo seleccionado
      * 30-CHAIN ao fx GFSEG
      * 32-CHAIN ao fx GFSEGEXT
      * 61-Ordena por Numero
      * 62-Ordena por Apelido
      * 63-Ordena por Nome
      * 70-Erro Nome
      * 71-Erro Apelido
      * 72-Erro NIF
      * 73-Erro BI
      * 74-Erro Morada
      * 75-Erro CPostal
      * 76-Erro Tipo
      * 77-Erro Gestor
      * 78-Erro Grupo
      * 79-Erro Contencioso
      * 80-Erro CORSEL
      * 81-Erro Piloto
      * 82-NIF alterado
      * 83-Erro Especial
      * 84-Erro Data Nascimento
      * 85-Tipo Sociedade
      * 86-Sem autoridade a alterar Contencioso
      * 87-Sem autoridade a alterar Especial
      **************************************************************************
     IMPESNPF1
     I              NSEG                        PNSEG
     I              APELID                      APEL
     I              CPOST                       CPOS
     I              TLM                         TELM
     I              NFAX                        FAX
     I              EMAIL                       MAIL
     I              TELEF                       TEL
      *
     ISD1CP     AA
     I                                  9   38  DESC
      **************************************************************************
      *
     C     AU            KLIST
     C                   KFLD                    PGM              10
     C                   KFLD                    FLD              10
     C                   KFLD                    USR
      *
     C                   MOVEL     'OP0100R'     PGM
     C                   MOVEL     'UPD'         HAOPR             3
     C                   SETOFF                                       868782
      * Verifica acesso a alterar contencioso
     C                   MOVEL     'CONTEN'      FLD
     C     AU            CHAIN     AUESPEF                            86
      * Verifica acesso a alterar Especial
     C                   MOVEL     'STESP '      FLD
     C     AU            CHAIN     AUESPEF                            87
      * Segurado a ler
     C                   SETOFF                                       616263
     C                   SETOFF                                       1112
      *
     C     *IN12         DOWEQ     *OFF
     C     *IN11         ANDEQ     *OFF
     C     *IN06         ANDEQ     *OFF
     C                   EXFMT     FMTNUMC
      *
     C     WSEG          IFNE      0
     C     WSEG          CHAIN     GFSEG                              30
     C   30              MOVE      MSGD(10)      WKMSG
     C  N30              SETON                                        11
      * Guarda valor inicial do NIF
     C  N30              Z-ADD     NIF           INNIF             9 0
     C                   ELSE
     C                   MOVE      MSGD(11)      WKMSG
     C                   ENDIF
      *
     C*  04              EXSR      F4
     C                   IF        *IN04
     C                   CALL      'OP0201R'
     C                   PARM                    WSEG
     C                   PARM                    NOM
     C                   ENDIF
     C
     C   06              CALL      'OP0100J'
     C   06              SETOFF                                       LR1203
     C                   ENDDO
      *
      *
     C                   SETON                                        15
     C                   EVAL      WICLoaded = *OFF
     C   11
     CANN12*IN15         DOWEQ     *ON
     C     *IN03         ANDEQ     *OFF
     C     *IN06         ANDEQ     *OFF
     C                   MOVEL     NOM           NOME
      *
     C     TIPO          IFEQ      ' '
     C     NSEG          IFLT      10000
     C                   MOVE      'S'           TIPO
     C                   ELSE
     C                   MOVE      'I'           TIPO
     C                   ENDIF
     C                   ENDIF
      *
     C                   IF        (NOT WIcLoaded)
     C                   EXSR      GetExtFields
     C                   EVAL      WICLoaded = *ON
     C                   ENDIF
     C                   EXFMT     FMTUPD
     C                   IF        (NOT *IN03)
     C                   EXSR      VAL
      *
     C                   EXSR      VALFIM
      *
     C                   ENDIF
     C                   ENDDO
      *
     C     *IN03         IFEQ      *ON
     C     *IN06         OREQ      *ON
     C                   SETON                                        LR
     C                   ELSE
     C  N12              EXSR      GRAVA
     C                   ENDIF
      *
      **************************************************************************
     C                   SETON                                        LR
      **************************************************************************
     C     VAL           BEGSR
      *
     C                   SETOFF                                       15
     C                   MOVE      *BLANKS       MSGT
      * Especial
     C     STESP         SCAN      ' S'          WK1
     C     WK1           IFGT      0
     C                   SETOFF                                       83
     C                   ELSE
     C                   SETON                                        8315
     C                   MOVE      MSGD(9)       MSGT
     C                   ENDIF
      * Piloto
     C     CORSEL        IFEQ      'S'
     C     PILOTO        SCAN      'SN'          WK1
     C     WK1           IFGT      0
     C                   SETOFF                                       81
     C                   ELSE
     C                   SETON                                        8115
     C                   MOVE      MSGD(9)       MSGT
     C                   ENDIF
     C                   ELSE
     C     PILOTO        SCAN      ' '           WK1
     C     WK1           IFEQ      0
     C                   SETON                                        8115
     C                   MOVE      MSGD(13)      MSGT
     C                   ENDIF
     C                   ENDIF
      * CORSEL
     C     CORSEL        SCAN      'SN'          WK1
     C     WK1           IFGT      0
     C                   SETOFF                                       80
     C                   ELSE
     C                   SETON                                        8015
     C                   MOVE      MSGD(9)       MSGT
     C                   ENDIF
      * Contencioso
     C     CONTEN        SCAN      'SN'          WK1
     C     WK1           IFGT      0
     C                   SETOFF                                       79
     C                   ELSE
     C                   SETON                                        7915
     C                   MOVE      MSGD(9)       MSGT
     C                   ENDIF
      * Chave do Colaborador
     C                   IF        (WICDVEN = 'S') AND (WCODEMP = '')
     C                   MOVE      MSGD(14)      MSGT
     C                   SETON                                        8915
     C                   ELSE
     C                   SETOFF                                       89
     C                   ENDIF
     C                   IF        (WICDVEN <> 'N') AND (WICDVEN <> 'S')
     C                   MOVE      MSGD(9)       MSGT
     C                   SETON                                        8815
     C                   ELSE
     C                   SETOFF                                       88
     C                   ENDIF
      * Tipo
     C     TIPO          SCAN      'SIE'         WK1               1 0
     C     WK1           IFGT      0
     C                   SETOFF                                       76
     C                   ELSE
     C                   SETON                                        7615
     C                   MOVE      MSGD(9)       MSGT
     C                   ENDIF
      * CPostal
     C     CPOST         IFNE      0
     C     CPOST         CHAIN     SD1CP                              13
     C     *IN13         IFEQ      *OFF
     C                   MOVEL     DESC          DESCP
     C                   SETOFF                                       75
     C                   ELSE
     C                   SETON                                        7515
     C                   MOVE      MSGD(7)       MSGT
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     MSGD(8)       DESCP
     C                   ENDIF
      * Morada
     C     MORAD         IFEQ      *BLANKS
     C                   SETON                                        7415
     C                   MOVE      MSGD(6)       MSGT
     C                   ELSE
     C                   SETOFF                                       74
     C                   ENDIF
      * Data Nascimento
     C     DTNASC        IFGT      9999
     C                   CALL      'ISDATE'
     C                   PARM                    DTNASC
     C                   PARM                    RET               1
     C     RET           IFEQ      '0'
     C                   SETOFF                                       84
     C                   ELSE
     C                   SETON                                        84  15
     C                   MOVE      MSGD(12)      MSGT
     C                   ENDIF
     C                   ELSE
     C                   SETOFF                                       84
     C                   ENDIF
      * BI
     C     CDBI          IFNE      *BLANKS
     C                   MOVE      CDBI          WKNIF
     C                   MOVEL     BI            WKNIF
     C                   CALL      'CHKD'
     C                   PARM                    WKNIF
     C                   PARM                    RES               1
     C     RES           IFEQ      '1'
     C                   SETON                                        7315
     C                   MOVE      MSGD(5)       MSGT
     C                   ELSE
     C                   SETOFF                                       73
     C                   ENDIF
     C                   ENDIF
     C*--------------------------
     C* Validação do NIF
     C*--------------------------
     C*    *IN72         IFEQ      *ON
     C*    *IN69         ANDEQ     *ON
     C*                  SETOFF                                       72
     C*                  ELSE
     C     NIF           IFGT      99999999
     C     NIF           ANDNE     123456789
     C                   MOVE      NIF           WKNIF             9
     C                   CALL      'CHKD'
     C                   PARM                    WKNIF
     C                   PARM                    RES               1
     C     RES           IFEQ      '1'
     C                   SETON                                        7215
     C                   MOVE      MSGD(4)       MSGT
     C                   ELSE
     C                   SETOFF                                       72
     C                   ENDIF
     C                   ELSE
     C     NIF           IFNE      0
     C                   SETON                                        7215
     C                   MOVE      MSGD(3)       MSGT
     C                   ENDIF
     C                   ENDIF
     C*                  ENDIF
      * Apelido
     C     APELID        IFEQ      *BLANKS
     C                   SETON                                        7115
     C                   MOVE      MSGD(2)       MSGT
     C                   ELSE
     C                   SETOFF                                       71
     C                   ENDIF
      * Nome
     C     NOME          IFEQ      *BLANKS
     C                   SETON                                        7015
     C                   MOVE      MSGD(1)       MSGT
     C                   ELSE
     C                   SETOFF                                       70
     C                   ENDIF
      *
     C                   ENDSR
     C*==================================================================*
     C* Subrotina..: GRAVA
     C* Objectivo..: Gravar os dados dos Segurados no ficheiro
     C*              GFSEG. Valida também o NIF
     C*==================================================================*
     C     GRAVA         BEGSR
      *
     C     NIF           IFNE      INNIF
     C                   SETON                                        82
     C                   ENDIF
      *
     C                   MOVEL     *BLANKS       NOM
     C                   MOVEL     NOME          NOM
     C     PILOTO        IFEQ      'S'
     C                   MOVE      'S'           STESP
     C                   ENDIF
     C                   MOVE      *DATE         WKDATA
     C                   Z-ADD     WDD           DD
     C                   Z-ADD     WMM           MM
     C                   Z-ADD     WAA           AA
     C                   Z-ADD     WDD           D
     C                   Z-ADD     WMM           M
     C                   Z-ADD     WAA           A
     C                   TIME                    TIMALT
     C                   MOVEL     USR           USERID
     C                   MOVEL     NOME          NOM
     C  N30              UPDATE    FSEGF1
     C  N30              EXSR      UpdExtFields
     C  N30
     CAN 82              EXCEPT    HST
     C
     C* Sincronizar alterações com no ficheiro SD1PE
     C  N30              EXSR      SRPE
      *
     C                   CALL      'OP0050R'
     C                   PARM                    NSEG
      *
     C                   ENDSR
     ***********************************************
     C     VALFIM        BEGSR
      * Aviso de NIF não preenchido
     C     *IN15         IFEQ      *OFF
     C     NIF           ANDEQ     0
     C     *IN10         DOWEQ     *OFF
     C     *IN05         ANDEQ     *OFF
     C     *IN12         ANDEQ     *OFF
     C                   EXFMT     FMTINFO
     C                   ENDDO
     C     *IN10         IFEQ      *ON
     C                   SETON                                        1572
     C                   ENDIF
     C                   ENDIF
     C                   SETOFF                                       1005
      * Valida se já existe o NIF
      * Valida se já existe o BI
     C                   ENDSR
     C*==================================================================*
     C* Subrotina..: SRPE
     C* Objectivo..: Actualizar ou criar dados no ficheiro de Pessoas
     C*              e Empresas SD1PE, se não existir.
     C*==================================================================*
     C     SRPE          BEGSR
     C                   Z-ADD     NSEG          PNSEG
     C     PNSEG         CHAIN     SD1PE                              21
      * se não existe
     C                   MOVEL     NOM           NOME
     C                   MOVEL     APELID        APEL
     C                   MOVEL     MORAD         MORADA
     C                   MOVEL     DESCP         LOCAL
     C                   Z-ADD     CPOST         CPOS
     C                   Z-ADD     NIF           NRC
     C                   Z-ADD     TLM           TELM
     C                   MOVE      NFAX          FAX
     C                   MOVEL     WEMAIL        MAIL
     C                   MOVEL     TELEF         TEL
     C                   MOVEL     USERID        UUA
      * Valores c/ regras
     C     NSEG          IFLE      9999
     C                   MOVE      'F'           IDENT
     C                   MOVEL     '0A'          CGEST
     C                   ELSE
     C                   MOVE      'I'           IDENT
     C                   MOVEL     '0B'          CGEST
     C                   ENDIF
      *
     C     CORSEL        IFEQ      'S'
     C                   Z-ADD     272           CDINT
     C                   ELSE
     C     *IN21         IFEQ      *ON
     C                   Z-ADD     0             CDINT
     C                   ENDIF
     C                   ENDIF
      * Valores Fixos
     C     *IN21         IFEQ      *ON
     C                   Z-ADD     10            CREG
     C                   Z-ADD     0             CSIT
     C                   MOVEL     'P'           NACION
     C                   Z-ADD     1             NUA
     C*                    MOVELGEST      CGEST
     C*                    MOVE TECN      CGEST
     C                   MOVEL     '**'          FILL6
     C                   MOVEL     '000'         CDAGR1
     C                   Z-ADD     0             PAS
     C                   Z-ADD     0             VAS
     C                   MOVE      *ZEROS        FILL11
     C                   WRITE     MPESNPF1
     C                   ELSE
     C                   ADD       1             NUA
     C                   UPDATE    MPESNPF1
     C                   ENDIF
     C                   ENDSR
     C*==================================================================*
     C* Subrotina..: GetExtFields
     C* Objectivo..: Obter campos extra para as Pessoas e Empersas
     C*              Se o registo não existir então criar.
     C*==================================================================*
     C     GetExtFields  BEGSR
     C     NSEG          CHAIN     FSEGEXT                            32
     C                   IF        (NOT *IN32)
     C                   EVAL      WEMAIL  = EMAIL01
     C                   EVAL      WCodEmp = CODEMP
     C                   EVAL      WIcDVen = ICDVEN
     C                   ELSE
     C                   EVAL      EMAIL01 = ''
     C                   EVAL      CODEMP  = ''
     C                   EVAL      WICDVEN = 'N'
     C                   EVAL      WEMAIL  = EMAIL01
     C                   EVAL      WCodEmp = CODEMP
     C                   WRITE     FSEGEXT
     C                   ENDIF
     C                   ENDSR
     C*
     C*==================================================================*
     C* Subrotina..: UpdExtFields
     C* Objsectivo.: Actualizar Campos no ficheiro de Pessoas e Empresas.
     C*==================================================================*
     C     UpdExtFields  BEGSR
     C                   EVAL      EMAIL01 = WEMAIL
     C                   EVAL      CODEMP  = WCodEmp
     C                   EVAL      ICDVEN  = WICDVEN
     C                   UPDATE    FSEGEXT
     C                   ENDSR
     C*==================================================================*
     ***********************************************
     OHAGSEGF   EADD         HST
     O                       DATALT               8
     O                       TIMALT              14
     O                       USERID              24
     O                       NSEG                29
     O                       HAOPR               32
     O                       INNIF               41
     O                       NIF                 50
     ***********************************************
**
Nome Obrigatório                                     #01
Apelido Obrigatório                                  #02
Nº Contrib. inválido                                 #03
Nº Contrib. inválido-CheckDigit                      #04
Nº BI errado                                         #05
Morada Obrigatória                                   #06
C.Postal não existe no ficheiro                      #07
C.P. Desconhecido                                    #08
Preencha com um dos valores possiveis                #09
Código não existe                                    #10
Preencha o campo                                     #11
Data Inválida                                        #12
O campo não pode estar preenchido                    #13
Obrigatorio preencher a Chave Empregado              #14
Limpar a Chave do Empregado                          #15
