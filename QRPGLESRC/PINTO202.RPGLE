     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL) FIXNBR(*ZONED)
     H*=====================================================================
     H* Programa...: PINTO101
     H* Objectivo..: Carregar Interface de Intervenientes das Apólices
     H*              de Acidentes Pessoais e Viagem.
     H*
     H* Observações: Este programa é invocado na cadeia de geração
     H*              automática de Interfaces.
     H*=====================================================================
     F*              RESUMO DA UTILIZAÇÃO DOS INDICADORES                *
     F********************************************************************
     F*                                                                  *
     F*  01 -  IDENTIFICA REGISTO DE PESSOAS/EMPRESAS - SEGURADOS        *
     F*  01 -  IDENTIFICA REGISTO DE PESSOAS/EMPRESAS - SEG. LUSITANIA   *
     F*                                                                  *
     F********************************************************************
     FOPLSEGF   IP   F  135     5AIDISK    KEYLOC(1)
     FOFSEGX    IF   F  211     6AIDISK    KEYLOC(1)
     FLFSEG     UF A F  211     6AIDISK    KEYLOC(1)
     FOFSEG     O  A F  211     6AIDISK    KEYLOC(1)
     FSTP1Q011  O    F   80        PRINTER OFLIND(*INOF)
      *
     D S1              S              1    DIM(52) CTDATA PERRCD(52)
     D S2              S              1    DIM(52) CTDATA PERRCD(52)
     D TABMES          S              2  0 DIM(12) CTDATA PERRCD(1) ASCEND
     D TABDIA          S              2  0 DIM(12) ALT(TABMES)
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
     D                 DS
     D  UDTTRF                15     26  0
     D  UATRF                 21     22  0
     D  UMTRF                 23     24  0
     D  UDTRF                 25     26  0
     D*
     D                 DS
     D  A1                     1     40                                         APELID
     D                                     DIM(40)
     D  NOME                   1     40                                         NOME
     D                 DS
     D  A2                     1     40                                         APELID
     D                                     DIM(40)
     D  MORA                   1     40                                         MORADA
     D                 DS
     D  A3                     1     20                                         APELID
     D                                     DIM(20)
     D  LOCAL                  1     20                                         LOCALI
     D                 DS
     D  DTNAS                  1      8  0                                      APELID
     D  DDNASC                 1      2  0                                      APELID
     D  MMNASC                 3      4  0                                      APELID
     D  AANASC                 5      8  0                                      APELID
      *
     D                 DS
     D  REG1                   1    211
     D  REG1A                  1    186
     D  REG1B                195    211
     D                 DS
     D  REG2                   1    211
     D  LNSEG                  1      6  0
     D  LNOME                  7     56
     D  LTITUL                57     58
     D  LTIPO                 59     59
     D  LPROF                 60     65
     D  LMORA                 66    105
     D  LLOCAL               106    135
     D  LCPOST               136    142
     D  LNISP                143    151
     D**LDTTRF               187    194
     D  LNIF                 195    203
     D  LDTNAS               204    211
     F********************************************************************
     F* FICHEIRO DE PESSOAS E EMPRESAS (SEGURADOS)                       *
     F********************************************************************
     IOPLSEGF   NS  01
     I                                  1    5 0NSEG                            NÚMERO
     I                                  6   14 0RRAPOL                          NÚMERO
     I                                 15   54  NOME                            NOME
     I                                 55   94  MORA                            MORADA
     I                                 99  118  LOCAL                           LOCALI
     I                             P   95   98 0CPOST                           CÓDIGO
     I                                119  127 0NIF                             CÓDIGO
     I                                128  135 0DTNAS                           CÓDIGO
     I********************************************************************
     I* FICHEIRO DE PESSOAS E EMPRESAS (SEGURADOS)                       *
     I********************************************************************
     IOFSEGX    NS  02
      *
     ILFSEG     NS
     I                                  1  186  REG1A
     I                                195  211  REG1B
      *
     F********************************************************************
     F* DETERMINA DATA DE TRANSFERÊNCIA                                  *
     F********************************************************************
     C*
     C     FLAG          IFEQ      0                                            <-----------+
     C                   TIME                    UDTHH                                      I
     C                   Z-ADD     1             FLAG              1 0                      I
     C                   MOVE      *ON           *IN69
     C*
     C* FORMATA DATA DA TRANSFERÊNCIA
     C*
     C                   MOVE      UDDDT         UDTRF                                      I
     C                   MOVE      UMMDT         UMTRF                                      I
     C                   MOVE      UAADT         UATRF                                      I
     C*
     C                   Z-ADD     *ZEROS        DTY2              8 0                      I
     C                   MOVE      UDTTRF        DTY2                                       I
     C                   EXSR      SRDTY2                                                   I
     C                   MOVE      DTY2          LDTTRF            8 0                      I
     C*
     C                   END                                                    <-----------+
     C*
     C********************************************************************
     C* LIMPEZA DE CAMPOS DE CONVERSAO                                   *
     C********************************************************************
     C*
     C                   MOVE      *ZEROS        LNSEG             6 0
     C                   MOVEL     '9'           LNSEG
     C                   MOVE      *BLANK        LNOME            50
     C                   MOVE      *BLANK        LTITUL            2
     C                   MOVE      *BLANK        LTIPO             1
     C                   MOVE      *BLANK        LPROF             6
     C                   MOVE      *BLANK        LMORA            40
     C                   MOVE      *BLANK        LLOCAL           30
     C                   MOVE      *BLANK        LCPOST            7
     C                   MOVE      *BLANK        LSUFCP            3
     C                   MOVE      *BLANK        LNIF              9
     C                   MOVE      *BLANK        LDTNAS            8
     C*
     C********************************************************************
     C* CONVERSAO DE CAMPOS                                              *
     C******************************************************
     C                   MOVE      NSEG          LNSEG
     C*
     C     DTNAS         IFNE      0
     C     DDNASC        IFGT      31
     C                   MOVE      *OFF          *IN69
     C                   EXCEPT    ERRO
     C                   GOTO      FIM
     C                   ENDIF
     C*
     C     MMNASC        IFGT      12
     C                   MOVE      *OFF          *IN69
     C                   EXCEPT    ERRO
     C                   GOTO      FIM
     C                   ENDIF
     C*
     C     MMNASC        LOOKUP    TABMES        TABDIA                   88
     C     *IN88         IFEQ      *ON
     C     DDNASC        ANDGT     TABDIA
     C                   MOVE      *OFF          *IN69
     C                   EXCEPT    ERRO
     C                   GOTO      FIM
     C                   ENDIF
     C                   ENDIF
     C*
     C* DETERMINA O ENVIO ANTERIOR DO SEGURADO
     C*
     C     LNSEG         CHAIN     LFSEG                              98
     C  N98              GOTO      FIM
     C*
     C     LNSEG         CHAIN     OFSEGX                             99
     C  N99              GOTO      FIM
     C*
     C                   MOVE      '99'          LTITUL
     C     NSEG          IFGT      9999
     C                   MOVE      '1'           LTIPO
     C                   ELSE
     C                   MOVE      '3'           LTIPO
     C                   END
     C*****                MOVE '2046'    LPROF  (DESACTIVADO 07/10/03)
     C                   MOVE      '999999'      LPROF
     C                   MOVE      CPOST         LCPOST
     C*****                MOVE '000'     LSUFCP (DESACTIVADO 03/10/01)
     C********************************************************************
     C* CONVERSÃO DE CARACTERES
     C********************************************************************
      * NOME
     C                   DO        40            Y2                2 0
     C                   Z-ADD     1             Y1                2 0
     C     A1(Y2)        LOOKUP    S1(Y1)                                 88
     C   88              MOVE      S2(Y1)        A1(Y2)
     C                   END
     C                   MOVEL     NOME          LNOME
      * MORADA
     C                   DO        40            Y2
     C                   Z-ADD     1             Y1
     C     A2(Y2)        LOOKUP    S1(Y1)                                 88
     C   88              MOVE      S2(Y1)        A2(Y2)
     C                   END
     C                   MOVEL     MORA          LMORA
      * D.POSTAL
     C                   DO        20            Y2
     C                   Z-ADD     1             Y1
     C     A3(Y2)        LOOKUP    S1(Y1)                                 88
     C   88              MOVE      S2(Y1)        A3(Y2)
     C                   END
     C                   MOVEL     LOCAL         LLOCAL
      * NIF
     C     NIF           IFNE      0
     C                   MOVE      NIF           LNIF
     C                   END
      * DATA NASCIMENTO
     C     DTNAS         IFNE      0
     C                   MOVE      DTNAS         LDTNAS
     C                   END
      *
     C                   MOVE      *OFF          *IN69
     C  N98REG1          COMP      REG2                               6969
     C                   EXCEPT
      *
     C     FIM           TAG
     C****************************************************
     C*   FORMATA DATAS COM 8 DIGITOS                     *
     C********************************************    > < =
     C     SRDTY2        BEGSR
     C*
     C     DTY2          IFLT      400000
     C                   MOVEL     20            DTY2
     C                   ELSE
     C                   MOVEL     19            DTY2
     C                   END
     C*
     C                   ENDSR
     C****************************************************
     OOFSEG     EADD 98
     O         OR    69
     O                       LNSEG                6
     O                       LNOME               56
     O                       LTITUL              58
     O                       LTIPO               59
     O                       LPROF               65
     O                       LMORA              105
     O                       LLOCAL             135
     O                       LCPOST             142
     O*                        LSUFCP   142
     O                       LDTTRF             194
     O                       LNIF               203
     O                       LDTNAS             211
      *
     OLFSEG     EADD    98
     O                       LNSEG                6
     O                       LNOME               56
     O                       LTITUL              58
     O                       LTIPO               59
     O                       LPROF               65
     O                       LMORA              105
     O                       LLOCAL             135
     O                       LCPOST             142
     O*                        LSUFCP   142
     O                       LDTTRF             194
     O                       LNIF               203
     O                       LDTNAS             211
      *
     OLFSEG     E      N98 69
     O                       LNSEG                6
     O                       LNOME               56
     O                       LTITUL              58
     O                       LTIPO               59
     O                       LPROF               65
     O                       LMORA              105
     O                       LLOCAL             135
     O                       LCPOST             142
     O*                        LSUFCP   142
     O                       LDTTRF             194
     O                       LNIF               203
     O                       LDTNAS             211
      *
     OSTP1Q011  E            ERRO        1
     O                                           15 'Apólice:'
     O                       RRAPOL            +  1 '0  -       '
     O                                         +  2 'Segurado:'
     O                       NSEG              +  1
     O                                         +  2 'Dt.Nasc:'
     O                       DTNAS             +  1 '  /  /    '
**
ÀÁÈÉÌÍÒÓÙÚÇÃÕàáèéìíòóùúçãõÄËÏÖÜÂÊÔÛ`´ÎÏâêîôûäëïöüºªÅ
**
AAEEIIOOUUCAOaaeeiioouucaoAEIOUAEOU  IIaeiouaeiou..A
** MESES VS NºDIAS NO MES
0131
0229
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
