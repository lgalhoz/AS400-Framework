     H********************************************************************
     H*  EMPRESA    :  LUSITANIA COMPANHIA DE SEGUROS, SA                *
     H*  PROGRAMA   :  CSP003                                            *
     H*  TIPO       :  NRT                                               *
     H*  APLICAÇÃO  :  COSEGURO CEDIDO - PRÉMIOS A PAGAR                 *
     H*  FUNÇÃO     :  CRIA FICHEIRO FORMATADO DO PLANO COSEGURO CEDIDO  *
     H*                PARA ENVIO À LUSITANIA.                           *
     H*  AUTOR      :  RUI GUERREIRO                                     *
     H*  DATA       :  11/11/2004                                        *
     H*==================================================================*
     H*                    REGISTO DE MANUTENÇÃO                         *
     H*                                                                  *
     H*   DATA   ABREV.  DESCRIÇÃO DA ALTERAÇÃO                          *
     H* -------- ------  ----------------------                          *
     H*                                                                  *
     H********************************************************************
     FCSF02     IP   E           K DISK
     FGBASEDIX  IF   E           K DISK
     FGBASEGRL  IF   E           K DISK
     FCSF02C    O  A E             DISK
     D TABR            S              2  0 DIM(23) CTDATA PERRCD(1) ASCEND
     D TABL            S              2  0 DIM(23) ALT(TABR)
     D TABS1           S              4  0 DIM(11) CTDATA PERRCD(1) ASCEND
     D TABS2           S              5  0 DIM(11) ALT(TABS1)
     D TABAP           S              9  0 DIM(3) CTDATA PERRCD(1)
     D                 DS
     D  RRAPOL                 1      9  0
     D  RAMO                   1      2  0
     D  APOL                   3      9  0
     IBASEGRFA
     I              CS                          XX1                             NOME S
     IBASEGRFO
     I              CS                          XX1                             NOME S
     IBASEDIFA
     I              CS                          XX1                             NOME S
     IBASEDIFO
     I              CS                          XX1                             NOME S
      *
     C     KAPOL         KLIST
     C                   KFLD                    RAMO
     C                   KFLD                    APOL
      *
     C     KAPOL         CHAIN     GBASEDIX                           88
     C     *IN88         IFEQ      *ON
     C     KAPOL         CHAIN     GBASEGRL                           88
     C                   ENDIF
     C     *IN88         IFEQ      *OFF
     C     SIT           ANDNE     3
      *
     C     CS            IFEQ      1                                            - CEDIDO
     C     RAMO          LOOKUP    TABR          TABL                     10
     C   10              MOVE      TABL          PRAMO
     C                   MOVE      APOL          PAPOL
     C                   MOVE      DATA          PDATA
     C                   Z-ADD     NUMSEG        PNSEG
      *
     C     RRAPOL        LOOKUP    TABAP                                  70
     C     *IN70         IFEQ      *ON
     C     NUMSEG        LOOKUP    TABS1         TABS2                    10
     C   10              MOVE      TABS2         PNSEG
     C                   ENDIF
      *
     C                   Z-ADD     QPER          PQPER
     C                   MOVE      LIDER         PLIDER
     C                   Z-ADD     TXGEST        PTGEST
     C                   Z-ADD     TXGESS        PTGESS
     C                   MOVEL     APLDR         PAPLDR
     C                   WRITE     CSF02CF
     C                   ENDIF
      *
     C                   ENDIF
**
0108
0201
0313
0413
0534
0616
0709
0809
0909
1001
1430
1501
1734
1809
1934
5031
5101
5534
6509
6707
6807
6907
7007
**
029440141
030340111
035040371
056440231
056540431
066941841
107740011
191341451
481341291
545940281
634941601
**
069900035
559961010
049920426
