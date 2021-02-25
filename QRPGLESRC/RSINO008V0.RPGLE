     H CURSYM('¤') DECEDIT(',') DFTNAME(GS0998) DATEDIT(*DMY.)
     H********************************************************************
     H*  EMPRESA    :  LUSITANIA COMPANHIA DE SEGUROS, S.A.              *
     H*  PROGRAMA   :  GS0998                                            *
     H*  TIPO       :  SRT                                               *
     H*  APLICAÇÃO  :  SINISTROS AUTOMÓVEL / OUTROS RAMOS                *
     H*  FUNÇÃO     :  LISTA VALORES DE SINISTROS POR MEDIADOR           *
     H*  AUTOR      :  RUI GUERREIRO                                     *
     H*  DATA       :  09/10/2000                                        *
     H*==================================================================*
     H*                    REGISTO DE MANUTENÇÃO                         *
     H*                                                                  *
     H*    DATA     ABREV.  DESCRIÇÃO DA ALTERAÇÃO                       *
     H* ---------- -------  ----------------------                       *
     H* 27-09-2010 LGALHOZ  Actualizar um ficheiro temporário            *
     H*                     a converter em Excel
     H*                     Incluir campo Familiar para Corsel
     H********************************************************************
     FSINPRO    IP   F  384        DISK
     FGSINPRV   IF   E           K DISK    PREFIX(APV_) EXTFILE('G.SINPRV')
     FGSINPAG   IF   E           K DISK    PREFIX(APG_)
     FIDSPRV    IF   E           K DISK    PREFIX(IPV_)
     FIDSPAG    IF   E           K DISK    PREFIX(IPG_)
     FOSINPRV   IF   E           K DISK    PREFIX(OPC_)
     FOSINPAG   IF   E           K DISK    PREFIX(OPG_)
     FTABELAS   IF   F   64    12AIDISK    KEYLOC(2)
     FGFSEG     IF   E           K DISK    PREFIX(SEG_)
     FSD1PE     IF   E           K DISK    PREFIX(PE_)
     FGS0998R   O    F  198        PRINTER OFLIND(*INOF)
      *
     D VPR             S              9  2 DIM(4)
     D VPG             S              9  2 DIM(4)
     D VRE             S              9  2 DIM(4)
     D CUS             S              9  2 DIM(4)
     D PEN             S              9  2 DIM(4)
      *
     D RAMO            S              2    DIM(99)
     D RVPR            S              9  2 DIM(99)
     D RVPG            S              9  2 DIM(99)
     D RVRE            S              9  2 DIM(99)
     D RCUS            S              9  2 DIM(99)
     D RPEN            S              9  2 DIM(99)
     D                 DS
     D  TIPMOV                 1      3  0
     D  TM11                   1      1  0
     D                UDS
     D***************************************************
     D* D/S DEFENINDO CAMPOS DO 'LDA' DA WKSTN          *
     D***************************************************
     D  UWSID                  1      2                                         WKSTN-
     D  UWSTY                  3      3                                         WKSTN-
     D  UPRID                  4      5                                         PRINTE
     D  UPRTY                  6      6                                         PRINTE
     D  USRID                  7     14                                         USER-I
     D  UDTHH                 15     26  0
     D  UTIME                 15     20  0                                      HORA A
     D  UDATA                 21     26  0                                      DATA A
     D  UTPRO                 27     27                                         TIPO P
     D  DOANOM                28     33  0                                      DATA P
     D  AOANOM                34     39  0                                      DATA P
     D*--------------------------------------------------
     D                 DS
     D  CHAVEA                 1      8
     D  NUMPRA                 1      6  0
     D*
     D  KYTAB                 10     21
     D  FIX                   10     11  0
     D  REST                  12     21
     D                 DS
     D  CHAVEO                 1     10
     D  CHAVO                  1      8
     D  RM                     1      2
     D  NUMPRO                 3      8  0
     D                 DS
     D  CHAVOW                 1      8
     D  RMO                    1      2
     D  NPROCO                 3      8  0
     I*====================================================================
     ISINPRO    NS  03
     I                                  3    4 0AAPROC                          Ano Pr
     I                                  3    8 0NUMPR                           N.Seq.
     I                                  9    9 0SPROC                           Situac
     I*                                                                     1-Ac
     I*                                                                     6-Pa
     I                                 10   15 0DTPROC                          Data a
     I                                 10   11 0ANOPRO                          Data a
     I                                 22   27 0DTFIM                           Data d
     I                                 58   59  CAUSA
     I                                 60   65 0DTSIN                           Data d
     I                                 60   61 0ANOSIN
     I                                119  119  FERID                           Ferido
     I                                169  173 0NSEG                            GS0220
     I                                176  177  RM            L1                Ramo
     I                                182  188 0AP                              Apolic
     I                                297  299 0NMED          L2                GS0220
     I                                317  327  MATR1                           Matric
     I                                365  365  IDS                             Sinist
     I                                366  366  CRDR                            Credor
     I                                377  384  USERID                          GS0220
     I*                                                                      C=C
     I*                                                                      D=D
     IGSINPAG   NS
     I                                  1    1  ACTREG
     I                                  2    7 0NPROC                           Ano do
     I                                  2    3 0AAPROC
     I                                  4    7 0NSEQ
     I                                  8    9 0NLINHA
     I                             P   10   11 0TIPMOV
     I                                 12   17 0DTMOV
     I                                 18   47  DESC
     I                             P   48   52 2VALOR
     I                             P   53   57 2VALR1
     I                             P   58   62 2VALR3
     I                             P   63   67 2VALR45
     I                                 68   69  RM
     I                             P   74   78 2PAGMIP
     I                             P   79   83 2PAGTRA
     I                                 84   93  CTAIND
     I                                 94   94  PF
     I                                 95   95  I100
     I                                 96   96 0FORMAP
     I*
     I*
     I                                 97  102 0DT
     I                                103  112  DOCCH
     I                                115  116 0BANCO
     I                                117  119 0DEPBAN
     I                                120  121 0ENTPAG
     I                                122  126 0PAGENT
     I                                127  166  NOMPAG
     I                                167  206  MORPAG
     I                                207  226  LOCPAG
     I                             P  227  230 0POSPAG
     I                             P  269  273 2DMATER
     I                             P  274  278 2DFISIC
     I                                285  285  ENVIO
     I                                286  286  SIT
     I                                287  292 0DTUACT
     I                                293  300  USERID
     IIDSPAG    NS
     I                                  1    1  ACTREG
     I                                  2    7 0NPROC                           Ano do
     I                                  2    3 0APROCX
     I                                  4    7 0NSEQ
     I                                  8    9 0NLINHA
     I                             P   10   11 0TIPMOV
     I                                 12   17 0DTMOV
     I                                 18   47  DESC
     I                             P   48   52 2VALOR
     I                             P   53   57 2VALR1
     I                             P   58   62 2VALR3
     I                             P   63   67 2VALR45
     I                                 68   69  RM
     I                             P   74   78 2PAGMIP
     I                             P   79   83 2PAGTRA
     I                                 84   93  CTAIND
     I                                 94   94  PF
     I                                 95   95  I100
     I                                 96   96 0FORMAP
     I*
     I*
     I                                 97  102 0DT
     I                                103  112  DOCCH
     I                                115  116 0BANCO
     I                                117  119 0DEPBAN
     I                                120  121 0ENTPAG
     I                                122  126 0PAGENT
     I                                127  166  NOMPAG
     I                                167  206  MORPAG
     I                                207  226  LOCPAG
     I                             P  227  230 0POSPAG
     I                             P  269  273 2DMATER
     I                             P  274  278 2DFISIC
     I                                285  285  ENVIO
     I                                286  286  SIT
     I                                287  292 0DTUACT
     I                                293  300  USERID
     IOSINPAG   NS
     I                                  1    1  ACTREG
     I                                  2    7 0NPROCO                          Ano do
     I                                  2    3 0AAPROC
     I                                  4    7 0NSEQ
     I                                  8    9 0NLINHA
     I                             P   10   11 0TIPMOV
     I                                 12   17 0DTMOV
     I                                 18   47  DESC
     I                             P   48   52 2VALOR
     I                             P   53   57 2VALR1
     I                             P   58   62 2VALR3
     I                             P   63   67 2VALR45
     I                                 68   69  RMO
     I                                 74   83  CTAPRV
     I                                 84   93  CTAIND
     I                                 94   94  PF
     I                                 95   95  I100
     I                                 96   96 0FORMAP
     I*
     I*
     I                                 97  102 0DT
     I                                103  112  DOCCH
     I                                115  116 0BANCO
     I                                117  119 0DEPBAN
     I                                120  121 0ENTPAG
     I                                122  126 0PAGENT
     I                                127  166  NOMPAG
     I                                167  206  MORPAG
     I                                207  226  LOCPAG
     I                             P  227  230 0POSPAG
     I                             P  269  273 2DMATER
     I                             P  274  278 2DFISIC
     I                                285  285  ENVIO
     I                                286  286  SIT
     I                                287  292 0DTUACT
     I                                293  300  USERID
     IGSINPRV   NS  05
     I                                  1    1  ACTREG                          Activ.
     I                                  2    7 0NPROC                           Ano do
     I                                  2    3 0AAPROC                          Ano do
     I                                  4    7 0NSEQ                            N.Sequ
     I                                  8    9 0NLINHA                          Numero
     I                             P   10   11 0TIPMOV                          Tipo d
     I                                 12   17 0DTMOV                           Data d
     I                                 18   47  DESC                            Descri
     I                             P   48   52 2VALOR                           Valor
     I                             P   53   57 2VALR1                           Valor
     I                             P   58   62 2VALR3                           Valor
     I                             P   63   67 2VALR45                          Valor
     I                                 68   69  RM                              Codigo
     I                             P   74   78 2PRVMIP
     I                             P   79   83 2PRVTRA
     I                                 84   93  CTAIND                          Conta
     I                                 94   94 0SIT1
     I                                 95   95 0SIT2
     I                                 96   96 0SIT3
     I*                                      97 100 FILLER
     I                             P  101  105 2DMATER                          Valor
     I                             P  106  110 2DFISIC                          Valor
     I*                                     111 114 FILLER
     I                                115  120 0DTUACT                          Data U
     I                                121  128  USERID                          User-i
     IIDSPRV    NS
     I                                  1    1  ACTREG                          Activ.
     I                                  2    7 0NPROC                           Ano do
     I                                  2    3 0APROCX                          Ano do
     I                                  4    7 0NSEQ                            N.Sequ
     I                                  8    9 0NLINHA                          Numero
     I                             P   10   11 0TIPMOV                          Tipo d
     I                                 12   17 0DTMOV                           Data d
     I                                 18   47  DESC                            Descri
     I                             P   48   52 2VALOR                           Valor
     I                             P   53   57 2VALR1                           Valor
     I                             P   58   62 2VALR3                           Valor
     I                             P   63   67 2VALR45                          Valor
     I                                 68   69  RM                              Codigo
     I                             P   74   78 2PRVMIP
     I                             P   79   83 2PRVTRA
     I                                 84   93  CTAIND                          Conta
     I                                 94   94 0SIT1
     I                                 95   95 0SIT2
     I                                 96   96 0SIT3
     I*                                      97 100 FILLER
     I                             P  101  105 2DMATER                          Valor
     I                             P  106  110 2DFISIC                          Valor
     I*                                     111 114 FILLER
     I                                115  120 0DTUACT                          Data U
     I                                121  128  USERID                          User-i
     IOSINPRV   NS  05
     I                                  1    1  ACTREG                          Activ.
     I                                  2    7 0NPROCO                          Ano do
     I                                  2    3 0AAPROC                          Ano do
     I                                  4    7 0NSEQ                            N.Sequ
     I                                  8    9 0NLINHA                          Numero
     I                             P   10   11 0TIPMOV                          Tipo d
     I                                 12   17 0DTMOV                           Data d
     I                                 18   47  DESC                            Descri
     I                             P   48   52 2VALOR                           Valor
     I                             P   53   57 2VALR1                           Valor
     I                             P   58   62 2VALR3                           Valor
     I                             P   63   67 2VALR45                          Valor
     I                                 68   69  RMO                             Codigo
     I                                 74   83  CTAPRV                          Conta
     I                                 84   93  CTAIND                          Conta
     I                                 94   94 0SIT1
     I                                 95   95 0SIT2
     I                                 96   96 0SIT3
     I*                                      97 100 FILLER
     I                             P  101  105 2DMATER                          Valor
     I                             P  106  110 2DFISIC                          Valor
     I*                                     111 114 FILLER
     I                                115  120 0DTUACT                          Data U
     I                                121  128  USERID                          User-i
     ITABELAS   NS  04
     I                                 14   33  DESTAB
     IGFSEG     NS
     I                                 26   60  NOMSEG
     ISD1PE     NS
     I                                 15   49  NOMMED
     C*
     C     *ENTRY        PLIST
     C                   PARM                    $DCLUBE          40
     C*
      /FREE
      /END-FREE
     C   L2              DO
     C*
     C                   MOVEL     060           CHPE              6 0
     C                   MOVE      NMED          CHPE
     C     CHPE          CHAIN     SD1PE                              99
     C   99              MOVE      *BLANK        NOMMED
     C     NMED          IFEQ      0
     C                   MOVEL     'TESOURAR'    WORK             10
     C                   MOVE      'IA'          WORK
     C                   MOVEL     WORK          NOMMED
     C                   END
     C*
     C                   END
     C*
     C   L1              DO
     C                   SETOFF                                       50
     C     RM            IFEQ      '50'
     C                   SETON                                        50
     C                   ELSE
     C     RM            IFEQ      '51'
     C                   SETON                                        50
     C                   END
     C                   END
     C                   Z-ADD     1             R                 2 0
     C     RM            LOOKUP    RAMO(R)                                99
     C  N99              MOVE      RM            R
     C                   MOVE      RM            RAMO(R)
     C                   MOVE      RM            WRM               2
     C                   Z-ADD     01            FIX
     C                   MOVEL     RM            REST
     C     KYTAB         CHAIN     TABELAS                            99
     C                   MOVE      DESTAB        NRAM             20
     C                   END
     C*
     C     CAUSA         IFNE      '88'
     C                   MOVE      *BLANK        C12              12
     C                   MOVEL     '02'          C4                4
     C                   MOVE      CAUSA         C4
     C                   MOVEL     C4            C12
     C     C12           CHAIN     TABELAS                            99
     C   99              MOVE      *BLANK        DESTAB
     C                   MOVEL     DESTAB        CAUSIN           20
      *
     C                   ELSE
     C                   MOVEL     *BLANKS       CAUSIN
     C                   END
     C*
     C                   MOVE      *BLANK        WIDS              5
     C     RM            IFEQ      '50'
     C     IDS           IFEQ      'S'
     C                   MOVEL     'IDS'         WIDS
     C                   MOVE      CRDR          WIDS
     C                   END
     C                   END
     C*
     C     NSEG          CHAIN     GFSEG                              99
     C                   MOVE      DTSIN         WDTSIN            8 0
     C     ANOSIN        IFGE      40
     C                   MOVEL     19            WDTSIN
     C                   ELSE
     C                   MOVEL     20            WDTSIN
     C                   END
     C*
     C                   SETOFF                                       69
     C*
     C     RM            IFEQ      '50'
     C     USERID        IFNE      'IDS     '
     C                   MOVE      NUMPR         NUMPRA
     C                   EXSR      SR01A
     C                   EXSR      SR02A
     C                   ELSE
     C     USERID        IFEQ      'IDS     '
     C                   SETON                                        69
     C                   Z-ADD     NUMPR         INUMPR            5 0
     C                   MOVEL     '9'           INUMPR
     C                   MOVE      NUMPR         NUMPRA
     C     DTFIM         IFEQ      0
     C                   EXSR      SR01I                                        - PROV
     C                   ELSE
     C                   EXSR      SR02I                                        - PAG
     C                   END
     C                   END
     C                   END
     C                   ELSE
     C     USERID        IFNE      'AC.TRAB.'
     C                   MOVE      NUMPR         NUMPRO
     C                   EXSR      SR01O
     C                   EXSR      SR02O
     C                   ELSE
     C     USERID        IFEQ      'AC.TRAB.'
     C                   SETON                                        69
     C                   Z-ADD     NUMPR         INUMPR
     C                   MOVE      NUMPR         NUMPRA
     C     DTFIM         IFEQ      0
     C                   EXSR      SR01T                                        - PROV
     C                   ELSE
     C                   EXSR      SR02T                                        - PAG
     C                   END
     C                   MOVEL     DTPROC        AAPROC
     C                   END
     C                   END
     C                   END
      *
     C     VPR(1)        IFEQ      0
     C                   Z-ADD     VPG(1)        VPR(1)
     C                   ADD       VPG(1)        VPR(2)
     C                   ADD       VPG(1)        VPR(3)
     C                   ADD       VPG(1)        VPR(4)
     C                   END
      *
     C     VPR           SUB       VRE           CUS
     C     VPR           SUB       VPG           PEN
      *
     CL1                 ADD       VPR(2)        RVPR(R)
     CL1                 ADD       VPG(2)        RVPG(R)
     CL1                 ADD       VRE(2)        RVRE(R)
     CL1                 ADD       CUS(2)        RCUS(R)
     CL1                 ADD       PEN(2)        RPEN(R)
     CL1                 EXCEPT    TOTL1
     C*
     CL2                 EXCEPT    TOTL2
     C*
     CLR                 EXCEPT    TOTGER
     CLR                 Z-ADD     1             R
     CLR   R             DOWLT     99
     CLR   RAMO(R)       IFNE      *BLANK
     CLR                 Z-ADD     01            FIX
     CLR                 MOVEL     RAMO(R)       REST
     CLR   KYTAB         CHAIN     TABELAS                            99
     CLR                 MOVE      DESTAB        NRAM             20
     CLR                 EXCEPT    LINRES
     CLR                 END
     CLR                 ADD       1             R
     CLR                 END
     CLR                 XFOOT     RVPR          TRVPR             9 2
     CLR                 XFOOT     RVPG          TRVPG             9 2
     CLR                 XFOOT     RVRE          TRVRE             9 2
     CLR                 XFOOT     RPEN          TRPEN             9 2
     CLR                 XFOOT     RCUS          TRCUS             9 2
     CLR                 EXCEPT    TOTRES
      /FREE
       //============================================================
       // Subrotina.: SR01A
       // Objectivo.: Obter o valor das Provisões
       //============================================================
       BEGSR SR01A;
           SETLL CHAVEA GSINPRV;
           READE CHAVEA GSINPRV;
     C     CHAVEA        SETLL     GSINPRV
     C                   MOVE      NUMPRA        NPROC
     C     NPROC         DOWEQ     NUMPRA
     C                   SETOFF                                       99
     C                   READ      GSINPRV                                99
     C   99              MOVE      *BLANK        NPROC
     C     NPROC         IFEQ      NUMPRA
     C                   ADD       VALOR         VPR
     C                   END
     C                   END
       ENDSR;
      /END-FREE
     C*----------------------------------------------------------*
     C     SR02A         BEGSR
     C     CHAVEA        SETLL     GSINPAG
     C                   MOVE      NUMPRA        NPROC
     C     NPROC         DOWEQ     NUMPRA
     C                   SETOFF                                       99
     C                   READ      GSINPAG                                99
     C   99              MOVE      *BLANK        NPROC
     C     NPROC         IFEQ      NUMPRA
     C     TM11          IFEQ      1
     C                   ADD       VALOR         VPG
     C                   ELSE
     C     TM11          IFEQ      2
     C                   ADD       VALOR         VRE
     C                   END
     C                   END
     C                   END
     C                   END
     C                   ENDSR
     C*----------------------------------------------------------*
     C     SR01I         BEGSR
     C     CHAVEA        SETLL     IDSPRV
     C                   MOVE      NUMPRA        NPROC
     C     NPROC         DOWEQ     NUMPRA
     C                   SETOFF                                       99
     C                   READ      IDSPRV                                 99
     C   99              MOVE      *BLANK        NPROC
     C     NPROC         IFEQ      NUMPRA
     C                   ADD       VALOR         VPR
     C                   END
     C                   END
     C                   ENDSR
     C*----------------------------------------------------------*
     C     SR02I         BEGSR
     C     CHAVEA        SETLL     IDSPAG
     C                   MOVE      NUMPRA        NPROC
     C     NPROC         DOWEQ     NUMPRA
     C                   SETOFF                                       99
     C                   READ      IDSPAG                                 99
     C   99              MOVE      *BLANK        NPROC
     C     NPROC         IFEQ      NUMPRA
     C     TM11          IFEQ      1
     C                   ADD       VALOR         VPG
     C                   ELSE
     C     TM11          IFEQ      2
     C                   ADD       VALOR         VRE
     C                   END
     C                   END
     C                   END
     C                   END
      *
     C                   ENDSR
     C*----------------------------------------------------------*
     C     SR01O         BEGSR
     C                   MOVE      RM            RMO
     C                   MOVE      NUMPRO        NPROCO
     C     CHAVEO        SETLL     OSINPRV
     C     CHAVO         DOWEQ     CHAVOW
     C                   SETOFF                                       99
     C                   READ      OSINPRV                                99
     C   99              MOVE      *BLANK        CHAVOW
     C     CHAVO         IFEQ      CHAVOW
     C                   ADD       VALOR         VPR
     C                   END
     C                   END
     C                   ENDSR
     C*----------------------------------------------------------*
     C     SR02O         BEGSR
     C                   MOVE      RM            RMO
     C                   MOVE      NUMPRO        NPROCO
     C     CHAVEO        SETLL     OSINPAG
     C     CHAVO         DOWEQ     CHAVOW
     C                   SETOFF                                       99
     C                   READ      OSINPAG                                99
     C   99              MOVE      *BLANK        CHAVOW
     C     CHAVO         IFEQ      CHAVOW
     C     TM11          IFEQ      1
     C                   ADD       VALOR         VPG
     C                   ELSE
     C     TM11          IFEQ      2
     C                   ADD       VALOR         VRE
     C                   END
     C                   END
     C                   END
     C                   END
     C                   ENDSR
     C*----------------------------------------------------------*
     C     SR01T         BEGSR
     C     CHAVEA        SETLL     IDSPRV
     C                   MOVE      NUMPRA        NPROC
     C     NPROC         DOWEQ     NUMPRA
     C                   SETOFF                                       99
     C                   READ      IDSPRV                                 99
     C   99              MOVE      *BLANK        NPROC
     C     NPROC         IFEQ      NUMPRA
     C                   ADD       VALOR         VPR
     C                   END
     C                   END
     C                   ENDSR
     C*----------------------------------------------------------*
     C     SR02T         BEGSR
     C     CHAVEA        SETLL     IDSPAG
     C                   MOVE      NUMPRA        NPROC
     C     NPROC         DOWEQ     NUMPRA
     C                   SETOFF                                       99
     C                   READ      IDSPAG                                 99
     C   99              MOVE      *BLANK        NPROC
     C     NPROC         IFEQ      NUMPRA
     C     TM11          IFEQ      1
     C                   ADD       VALOR         VPG
     C                   ELSE
     C     TM11          IFEQ      2
     C                   ADD       VALOR         VRE
     C                   END
     C                   END
     C                   END
     C                   END
      *
     C                   ENDSR
     OGS0998R   H    L2                     1 01
     O         OR    OFNL2
     O                                           24 'LUSITANIA COMPANHIA DE S'
     O                                           36 'EGUROS, S.A.'
     O                                           95 'SINISTROS DO ANO/MÊS:'
     O                       DOANOM             103 '    /  '
     O                                          117 'AO  ANO/MÊS:'
     O                       AOANOM             125 '    /  '
     O                       $DCLUBE           +003
     O                                          185 'Data:'
     O                       UDATA         Y    194
     O          H    L2                     1
     O         OR    OFNL2
     O                                           10 'REP-GS0998'
     O                                           83 'MEDIADOR:'
     O                       NMED                87
     O                       NOMMED             124
     O                                          185 'Hora:'
     O                       UTIME              194 '  :  :  '
     O          H    L2                     1
     O         OR    OFNL2
     O                                          185 'Pág.:'
     O                       PAGE          Z    194
     O          H    L2                     1
     O         OR    OFNL2
     O                                          105 'VALORES EM EUROS'
     O          H    L2                     1
     O         OR    OFNL2
     O                                           49 'DATA DO'
     O                                          163 'CUSTO DO  '
     O                                          175 'PAGAMENTOS'
     O          H    L2                     0
     O         OR    OFNL2
     O                                           38 'Nº PROC'
     O                                           49 'SINIST.'
     O                                           59 'APÓLICE'
     O                                           70 'MATRÍCULA'
     O                                           82 'SEGURADO'
     O                                          128 'PROVISÕES '
     O                                          139 'PAGAMENTOS'
     O                                          151 'RECUPERAÇ.'
     O                                          163 'PROCESSO  '
     O                                          175 'PENDENTES '
     O                                          194 'CAUSA DO SINISTRO'
     O          H    L2                     1
     O         OR    OFNL2
     O                                           38 '_______'
     O                                           49 '_______'
     O                                           59 '_______'
     O                                           70 '_________'
     O                                           91 '_________________'
     O                                          114 '_______________________'
     O                                          127 '_________'
     O                                          139 '__________'
     O                                          151 '__________'
     O                                          162 '_________'
     O                                          175 '__________'
     O                                          194 '_________________'
     O          H    L1                     0
     O                                            5 'RAMO:'
     O                       WRM                  8
     O                       NRAM                29
     O          D    03                     1
     O                       WIDS                28
     O                 N69   NUMPR               38 '0  /    '
     O                  69   INUMPR              39
     O                  69   AAPROC              33
     O                  69                       34 '/'
     O                       WDTSIN              50 '    /  /  '
     O                       AP                  59
     O                  50   MATR1               72
     O                       NSEG                79
     O                       NOMSEG             115
     O                       VPR(1)         B   126 '    . 0 ,  '
     O                       VPG(1)         B   138 '    . 0 ,  '
     O                       VRE(1)         B   150 '    . 0 ,  '
     O                       CUS(1)         B   162 '    . 0 ,  '
     O                       PEN(1)         B   175 '    . 0 ,  -'
     O                       CAUSIN             197
     O          E            TOTL1          2
     O                                          108 '* TOTAL DO RAMO    '
     O                       VPR(2)         B   126 '    . 0 ,  '
     O                       VPG(2)         B   138 '    . 0 ,  '
     O                       VRE(2)         B   150 '    . 0 ,  '
     O                       CUS(2)         B   162 '    . 0 ,  '
     O                       PEN(2)         B   175 '    . 0 ,  -'
     O          E            TOTL2          1
     O                                          108 '** TOTAL DO MEDIADOR'
     O                       VPR(3)         B   126 '    . 0 ,  '
     O                       VPG(3)         B   138 '    . 0 ,  '
     O                       VRE(3)         B   150 '    . 0 ,  '
     O                       CUS(3)         B   162 '    . 0 ,  '
     O                       PEN(3)         B   175 '    . 0 ,  -'
     O          E            TOTGER         1 01
     O                                           24 'LUSITANIA COMPANHIA DE S'
     O                                           36 'EGUROS, S.A.'
     O                                           95 'SINISTROS DO ANO/MÊS:'
     O                       DOANOM             103 '    /  '
     O                                          117 'AO  ANO/MÊS:'
     O                       AOANOM             125 '    /  '
     O                       $DCLUBE           +003
     O                                          185 'Data:'
     O                       UDATA         Y    194
     O          E            TOTGER         1
     O                                           10 'REP-GS0998'
     O                                          185 'Hora:'
     O                       UTIME              194 '  :  :  '
     O          E            TOTGER         1
     O                                          185 'Pág.:'
     O                       PAGE          Z    194
     O          E            TOTGER         1
     O                                          138 'CUSTO DO  '
     O                                          150 'PAGAMENTOS'
     O          E            TOTGER         0
     O                                          100 'PROVISÕES '
     O                                          112 'PAGAMENTOS'
     O                                          124 'RECUPERAÇ.'
     O                                          138 'PROCESSO  '
     O                                          150 'PENDENTES '
     O          E            TOTGER         2
     O                                           99 '_________'
     O                                          112 '__________'
     O                                          124 '__________'
     O                                          137 '_________'
     O                                          150 '__________'
     O          E            TOTGER         3
     O                                           83 '*** TOTAL GERAL'
     O                       VPR(4)         B    99 '    . 0 ,  '
     O                       VPG(4)         B   112 '    . 0 ,  '
     O                       VRE(4)         B   124 '    . 0 ,  '
     O                       CUS(4)         B   137 '    . 0 ,  '
     O                       PEN(4)         B   151 '    . 0 ,  -'
     O          E            TOTGER      3  0
     O                                           79 'RESUMO DOS RAMOS'
     O          E            TOTGER         2
     O                                           79 '________________'
     O          E            LINRES         1
     O                       RAMO(R)             60
     O                       NRAM                81
     O                       RVPR(R)             99 '    . 0 ,  '
     O                       RVPG(R)            112 '    . 0 ,  '
     O                       RVRE(R)            124 '    . 0 ,  '
     O                       RCUS(R)            137 '    . 0 ,  '
     O                       RPEN(R)            151 '    . 0 ,  -'
     O          E            TOTRES         1
     O                                           99 '-----------'
     O                                          112 '-----------'
     O                                          124 '-----------'
     O                                          137 '-----------'
     O                                          150 '-----------'
     O          E            TOTRES         1
     O                                           83 '*** TOTAL GERAL'
     O                       TRVPR               99 '    . 0 ,  '
     O                       TRVPG              112 '    . 0 ,  '
     O                       TRVRE              124 '    . 0 ,  '
     O                       TRCUS              137 '    . 0 ,  '
     O                       TRPEN              151 '    . 0 ,  -'
