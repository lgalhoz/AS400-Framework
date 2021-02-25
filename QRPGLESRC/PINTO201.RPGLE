     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL) FIXNBR(*ZONED)
     H*=====================================================================
     H* Programa...: PINTO100
     H* Objectivo..: Carregar Interface de Pessoas e Empresas
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
     FGFSEG     IP   F  267     5AIDISK    KEYLOC(1)
     FOFSEGX    IF   F  211     6AIDISK    KEYLOC(1)
     FLFSEG     UF A F  211     6AIDISK    KEYLOC(1)
     FMPESNP    IF   F  512     6AIDISK    KEYLOC(5)
     FGBASEGX3  IF   F  700     5AIDISK    KEYLOC(466)
     FGBASEGX4  IF   F  700     5AIDISK    KEYLOC(466)
     FGCOBGRX6  IF   F  250     5AIDISK    KEYLOC(89)
     FOFSEG     O  A F  211     6AIDISK    KEYLOC(1)
      *
     D S1              S              1    DIM(52) CTDATA PERRCD(52)
     D S2              S              1    DIM(52) CTDATA PERRCD(52)
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
     D*
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
     D****                                  187 194 LDTTRF
     D  LNIF                 195    203
     D  LDTNAS               204    211
     D*
     D                 DS
     D  UDTTRF                15     26  0
     D  UATRF                 21     22  0
     D  UMTRF                 23     24  0
     D  UDTRF                 25     26  0
     D*
     D                 DS
     D  A1                     1     40                                         APELIDO
     D                                     DIM(40)
     D  NOME                   1     40                                         NOME
     D                 DS
     D  A2                     1     40                                         APELIDO
     D                                     DIM(40)
     D  MORA                   1     40                                         MORADA
     D                 DS
     D  A3                     1     20                                         APELIDO
     D                                     DIM(20)
     D  LOCAL                  1     20                                         LOCALIDADE
     F********************************************************************
     F* FICHEIRO DE PESSOAS E EMPRESAS (SEGURADOS)                       *
     F********************************************************************
     IGFSEG     NS  01
     I                                  1    5 0NSEG                            NÚMERO SEGURADO
     I                                  6   25  APEL                            APELIDO
     I                                 26   65  NOME                            NOME
     I                                 66  105  MORA                            MORADA
     I                                106  125  LOCAL                           LOCALIDADE
     I                             P  126  129 0CPOST                           CÓDIGO POSTAL
     I                                170  178 0NIF                             CÓDIGO POSTAL
     I                                179  186 0DTNAS                           CÓDIGO POSTAL
     I********************************************************************
     I* FICHEIRO DE PESSOAS E EMPRESAS (SEGURADOS)                       *
     I********************************************************************
     IOFSEGX    NS  02
     I*
     ILFSEG     NS
     I                                  1  186  REG1A
     I                                195  211  REG1B
     I*
     I********************************************************************
     I* FICHEIRO DE PESSOAS E EMPRESAS (SEGURADOS)                       *
     I********************************************************************
     IMPESNP    NS  03
     I                                292  300  NISP
     I*
     I********************************************************************
     I* FICHEIRO DE APÓLICES                                             *
     I********************************************************************
     IGBASEGX3  NS
     I*
     I********************************************************************
     I* FICHEIRO DE APÓLICES                                             *
     I********************************************************************
     IGBASEGX4  NS
     I*
     I********************************************************************
     I* FICHEIRO DE RECIBOS                                              *
     I********************************************************************
     IGCOBGRX6  NS
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
     C                   END                                                    <-----------+
     C*
     C     NSEG          CHAIN     GBASEGX3                           88
     C   88NSEG          CHAIN     GBASEGX4                           88
     C   88NSEG          CHAIN     GCOBGRX6                           88
     C   88              GOTO      FIM
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
     C                   MOVE      *BLANK        LNISP             9
     C                   MOVE      *BLANK        LDTNAS            8
     C*
     C********************************************************************
     C* CONVERSAO DE CAMPOS                                              *
     C******************************************************
     C                   MOVE      NSEG          LNSEG
      *
      * DETERMINA O ENVIO ANTERIOR DO SEGURADO
      *
     C     LNSEG         CHAIN     OFSEGX                             99
     C  N99              GOTO      FIM
      *
     C                   Z-ADD     NSEG          KNSEG             6 0
     C     KNSEG         CHAIN     MPESNP                             88
     C  N88              MOVEL     NISP          LNISP             9
      *
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
     C                   SETOFF                                       69
     C     LNSEG         CHAIN     LFSEG                              99
     C  N99REG1          COMP      REG2                               6969
     C                   EXCEPT
      *
     C     FIM           TAG
     C****************************************************
     C*   FORMATA DATAS COM 8 DIGITOS                     *
     C********************************************    > < =
     C     SRDTY2        BEGSR
      *
     C     DTY2          IFLT      400000
     C                   MOVEL     20            DTY2
     C                   ELSE
     C                   MOVEL     19            DTY2
     C                   END
      *
     C                   ENDSR
     C****************************************************
     OOFSEG     EADD 99
     O         OR    69
     O                       LNSEG                6
     O                       LNOME               56
     O                       LTITUL              58
     O                       LTIPO               59
     O                       LPROF               65
     O                       LMORA              105
     O                       LLOCAL             135
     O                       LCPOST             142
     O                       LNISP              151
     O                       LDTTRF             194
     O                       LNIF               203
     O                       LDTNAS             211
      *
     OLFSEG     EADD 99
     O                       LNSEG                6
     O                       LNOME               56
     O                       LTITUL              58
     O                       LTIPO               59
     O                       LPROF               65
     O                       LMORA              105
     O                       LLOCAL             135
     O                       LCPOST             142
     O                       LNISP              151
     O                       LDTTRF             194
     O                       LNIF               203
     O                       LDTNAS             211
      *
     OLFSEG     E   N99 69
     O                       LNSEG                6
     O                       LNOME               56
     O                       LTITUL              58
     O                       LTIPO               59
     O                       LPROF               65
     O                       LMORA              105
     O                       LLOCAL             135
     O                       LCPOST             142
     O                       LNISP              151
     O                       LDTTRF             194
     O                       LNIF               203
     O                       LDTNAS             211
**
ÀÁÈÉÌÍÒÓÙÚÇÃÕàáèéìíòóùúçãõÄËÏÖÜÂÊÔÛ`´ÎÏâêîôûäëïöüºªÅ
**
AAEEIIOOUUCAOaaeeiioouucaoAEIOUAEOU  IIaeiouaeiou..A
