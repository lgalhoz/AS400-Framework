     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL) FIXNBR(*ZONED)
     H*=====================================================================
     H* Programa...: PINTO102
     H* Objectivo..: Carregar Interface de Pessoas e Empresas (Entidades)
     H*
     H* Observações: Este programa é invocado na cadeia de geração
     H*              automática de Interfaces.
     H*=====================================================================
     F*              RESUMO DA UTILIZAÇÃO DOS INDICADORES                *
     F********************************************************************
     F*                                                                  *
     F*  01 -  IDENTIFICA REGISTO DE ENTIDADES-PESSOAS                   *
     F*                                                                  *
     F********************************************************************
     FGSINENT   IP   F  400     7AIDISK    KEYLOC(1)
(LG) FLFENTX    IF   F  208     9AIDISK    KEYLOC(143)                          Antes era 194
(LG) FLFENT     O  A F  217        DISK                                         Antes era 194
      *
     D CE              S              2  0 DIM(12) CTDATA PERRCD(1)             ENTIDADES/DESCRIÇÃO
     D DE              S             30    DIM(12) ALT(CE)
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
     D  UDTTRF                15     26  0
     D  UATRF                 21     22  0
     D  UMTRF                 23     24  0
     D  UDTRF                 25     26  0
     D*
     D                 DS
     D  A1                     1     40    DIM(40)                              APELIDO
     D  NOMENT                 1     40                                         NOME
     D                 DS
     D  A2                     1     40    DIM(40)                              APELIDO
     D  MORENT                 1     40                                         MORADA
     D                 DS
     D  A3                     1     20    DIM(20)                              APELIDO
     D  LOCENT                 1     20                                         LOCALIDADE
     D  LNISP          S              9S 0
     F********************************************************************
     F* FICHEIRO DE PESSOAS E EMPRESAS (SEGURADOS)                       *
     F********************************************************************
     IGSINENT   NS  01
     I                                  1    2 0TIPENT
     I                                  3    7 0NUMENT
     I                                  8   47  NOMENT
     I                                 88  127  MORENT
     I                                128  147  LOCENT
     I                             P  153  156 0POSENT
     I                                166  174 0NCONTR
     I                                201  201  EMPIND
(LG) I                                220  230 0NISS
(LG) I                                231  233 0CDSS
     I                                400  400  ELIM
     I********************************************************************
     I* FICHEIRO DE ENTIDADES-PESSOAS                                    *
     I********************************************************************
     ILFENTX    NS  02
      *
     C     ELIM          IFEQ      '*'
     C                   SETOFF                                       99
     C                   GOTO      FIM
     C                   END
     C********************************************************************
     C* DETERMINA DATA DE TRANSFERÊNCIA                                  *
     C********************************************************************
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
     C********************************************************************
     C* LIMPEZA DE CAMPOS DE CONVERSAO                                   *
     C********************************************************************
     C*
     C                   Z-ADD     0             LNENT             6 0
     C                   MOVE      *BLANK        LNOME            50
     C                   MOVE      *BLANK        LTITUL            2
     C                   MOVE      *BLANK        LTIPO             1
     C                   MOVE      *BLANK        LPROF             6
     C                   MOVE      *BLANK        LMORA            40
     C                   MOVE      *BLANK        LLOCAL           30
     C                   MOVE      *BLANK        LCPOST            7
     C                   Z-ADD     0             LCONTR            9 0
     C                   Z-ADD     0             LTPENT            2 0
     C                   MOVE      *BLANK        LDTENT           30
(LG) C                   Z-ADD     0             NISS             11 0
(LG) C                   Z-ADD     0             CDSS              3 0
     C*
     C********************************************************************
     C* CONVERSAO DE CAMPOS                                              *
     C******************************************************
     C                   Z-ADD     NCONTR        LCONTR
     C*
     C* DETERMINA O ENVIO ANTERIOR DA ENTIDADE
     C*
     C     LCONTR        CHAIN     LFENTX                             99
     C  N99              GOTO      FIM
     C*
     C                   MOVE      '99'          LTITUL
     C     EMPIND        IFEQ      'I'
     C                   MOVE      '1'           LTIPO
     C                   ELSE
     C                   MOVE      '3'           LTIPO
     C                   END
     C******               MOVE '2046'    LPROF  (DESACTIVADO 07/10/03)
     C                   MOVE      '999999'      LPROF
     C                   MOVE      POSENT        LCPOST
     C                   Z-ADD     TIPENT        LTPENT
     C* DESCRIÇÃO ENTIDADE
     C                   Z-ADD     1             I                 2 0
     C     LTPENT        LOOKUP    CE(I)                                  20
     C  N20              DO
     C                   SETOFF                                       99
     C                   GOTO      FIM
     C                   END
     C   20              MOVE      DE(I)         LDTENT
     C********************************************************************
     C* CONVERSÃO DE CARACTERES
     C********************************************************************
      * NOME
     C                   DO        40            Y2                2 0
     C                   Z-ADD     1             Y1                2 0
     C     A1(Y2)        LOOKUP    S1(Y1)                                 88
     C   88              MOVE      S2(Y1)        A1(Y2)
     C                   END
     C                   MOVEL     NOMENT        LNOME
      * MORADA
     C                   DO        40            Y2
     C                   Z-ADD     1             Y1
     C     A2(Y2)        LOOKUP    S1(Y1)                                 88
     C   88              MOVE      S2(Y1)        A2(Y2)
     C                   END
     C                   MOVEL     MORENT        LMORA
      * D.POSTAL
     C                   DO        20            Y2
     C                   Z-ADD     1             Y1
     C     A3(Y2)        LOOKUP    S1(Y1)                                 88
     C   88              MOVE      S2(Y1)        A3(Y2)
     C                   END
     C                   MOVEL     LOCENT        LLOCAL
     C*
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
     OLFENT     DADD 99
     O                       LNENT                6
     O                       LNOME               56
     O                       LTITUL              58
     O                       LTIPO               59
     O                       LPROF               65
     O                       LMORA              105
     O                       LLOCAL             135
     O                       LCPOST             142
     O                       LCONTR             151
     O                       LTPENT             153
     O                       LDTENT             183
     O                       LDTTRF             194
(LG) O                       NISS               205
(LG) O                       CDSS               208
     O                       LNISP              217
**
01PERITOS
02ADVOGADOS
03HOSPITAIS E CLINICAS
04GARAGENS E OFICINAS
05TRIBUNAIS
06MEDICOS E PARAMEDICOS
07RENT-A-CAR
12BOMBEIROS E AUTOR. CIVIS
13EMPREITEIROS
14AG VIAGEM E HOTELARIA
15FARMACIAS E ORTOPEDICAS
21JUROS MORA-RETENÇÃO IRS
**
ÀÁÈÉÌÍÒÓÙÚÇÃÕàáèéìíòóùúçãõÄËÏÖÜÂÊÔÛ`´ÎÏâêîôûäëïöüºªÅ
**
AAEEIIOOUUCAOaaeeiioouucaoAEIOUAEOU  IIaeiouaeiou..A
