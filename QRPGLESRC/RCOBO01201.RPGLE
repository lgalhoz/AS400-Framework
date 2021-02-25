     H DFTNAME(RCOBO01401) DATEDIT(*DMY/)
     H*======================================================================
     H*
     H*    RECOLHA DE ELEMENTOS PARA FACTURACAO DE RECIBOS DE PREMIO
     H*
     H*======================================================================
     FWS        CP   F  500        WORKSTN
     FFRECNP    UF   F  326     6AIDISK    KEYLOC(5) EXTFILE('SD.1.RCA')
     FFSAPOL    IF   F  320    16AIDISK    KEYLOC(8) EXTFILE('SD.1.AP1')
     FMCOMPNP   IF   F  128     3AIDISK    KEYLOC(5) EXTFILE('SD.1.CS')
     FMCORNP    IF   F  128     3AIDISK    KEYLOC(4) EXTFILE('SD.1.CR')
     FMDESCR    IF   F  256     3AIDISK    KEYLOC(3) EXTFILE('SD.1.DF')
     FMPESNP    IF   F  512     6AIDISK    KEYLOC(5) EXTFILE('SD.1.PE')
     FMRNP      IF   F  128     3AIDISK    KEYLOC(5) EXTFILE('SD.1.RM')
     FSDCTL     IF   F  512    10AIDISK    KEYLOC(1) EXTFILE('SD.1.CTL')
     FRECAVD    O  A F  350        DISK    EXTFILE('SD.1.HAV')
     FSD101     O    F  198        PRINTER OFLIND(PrintOf) EXTFILE('QSYSPRT')
     F                                     FORMLEN(72)
     F                                     FORMOFL(64)
     F*======================================================================
     D AR              S             40    DIM(38) CTDATA PERRCD(1)
     D A               S             74    DIM(1) CTDATA PERRCD(1)
     D ARR             S              1    DIM(198)
     D                UDS
     D  USER                   1      8
     D  CA                    11     12
     D  PROC                  20     27
     D  PROG                  28     35
     D  CLIE                  86     87
     D  LISTA                 92     92  0
     D  APOLIC               102    111
     D  CSEG                 112    117  0
     D  CCOMP                118    120  0
     D  CRAMO                121    123  0
     D  NUMREC               137    144
     D  DTINIC               145    150  0
     D  DTFIM                151    156  0
     D  DS2                  101    142
     D  CCORL                244    246  0
     D  CF                   256    256
     D*
     D*** - 1ª Estrutura de dados
     D*
     D                 DS
     D  CHV16                  1     16
     D  APOL2                  1     10
     D  CCOMP2                11     13  0
     D  CRAMO2                14     16  0
     D  CCOM1                 20     22  0
     D  CCOM2                 23     25  0
     D  CCOM3                 26     28  0
     D* Campos diversos
     D                 DS
     D  KEYCTL                 1     10
     D  KEY9                  11     19
     D                 DS
     D  TIMEDT                 1     12  0
     D  TIMSYS                 1      6  0
     D  DATSYS                 7     12  0
     D PrintOf         S               N
     IWS        NS  04    1 C
     I          NS  01    1 C1
     I                                  2    7 0NO
     I                                  8   17  APOLIC
     I          NS  02    1 C2
     I                                  2    4 0CFIX
     I                                  5   39  DVAR
     I                                 40   42 0CCOMIS
     I                                 43   51 2NCOMIS
     I                                 52   54 0CC1
     I                                 55   63 2VCC1
     I                                 64   66 0CC2
     I                                 67   75 2VCC2
     I                                 76   76 0CFACT
     I          NS  03    1 C3
     IFRECNP    NS
     I                                  3    3 0TR
     I                                 11   20  APOL
     I                                 21   28  NUMRE1
     I                                 29   31 0CCOMP1
     I                                 32   37 0CSEG1
     I                                 38   40 0CRAMO1
     I                                 41   46 0DTINI1
     I                                 47   52 0DTFIM1
     I                                 11   52  DS1
     I                             P   53   59 2VPREM1
     I                             P   60   64 2VCOMI1
     I                                 65   70 0DTRECX
     I                                 71   78  NUMGRX
     I                                 79   84 0DTGRX
     I                                 85   90 0DTENTX
     I                             P   97  100 0NAVD
     I                                101  106 0DTAVD
     I                                107  109 0CFIX1
     I                                110  144  DVAR1
     I                             P  147  151 2NCOMI1
     I                             P  154  158 2VCC11
     I                             P  161  165 2VCC21
     I                                304  304 0CFACT1
     I                                300  300 0CD0
     I                                302  302  CALT
     I                                309  309 0CSIT
     I                                315  315  AST
     IMPESNP    NS
     I                                 15   50  DSEG
     I                                129  129 0CSITMP
     I                                245  245  CODALT
     IMCOMPNP   NS
     I                                  8   24  DCOMP
     I                                 41   49  DCOMP1
     IMRNP      NS
     I                                  8   30  DRAMO
     I                                 32   40  DRAMO1
     IMDESCR    NS
     I                                  6   40  DFIX1
     I                                 41   75  DFIX2
     IMCORNP    NS
     I*
     IFSAPOL    NS
     I                             P  154  155 0CCOMI1
     I                             P  156  157 0CC11
     I                             P  158  159 0CC21
     I                                225  225 0CDBLQF
     I                                320  320  CDSUPF
     ISDCTL     NS
     I                                 11   40  CT04
     C*---------------------------------------------------------
     C*  ROTINA PRINCIPAL
     C*---------------------------------------------------------
     C                   SETOFF                                       919293
     C                   SETOFF                                       9499
     C   01              SETON                                        47
     C   KG
     CAN 01              SETON                                        LR
     C   LR              GOTO      END
     C   KB
     CAN 01              SETON                                        91
     C   01
     CANNKB
     CANNKL              EXSR      INICIO
     C   KL
     CANN47              SETON                                        91
     C   KL              Z-ADD     0             NCOMIS
     C   KL              Z-ADD     0             VCC1
     C   KL              Z-ADD     0             VCC2
     C   KL              Z-ADD     0             NAVD
     C   KL              EXCEPT
     C* So lista recibos anulados se a Pos. 92 da LDA for = '1'
     C   KL
     CANN47
     CANN78LISTA         IFEQ      1
     C                   SETON                                        78
     C                   END
     C*
     C   KL
     CANN47              SETON                                        49
     C   KL
     CANN47              EXSR      TOTAIS
     C   KL              GOTO      END
     C   KA
     CAN 02              SETON                                        91
     C   02
     CANNKA              EXSR      VALID
     C   KA
     CAN 03              SETON                                        92
     C   KB
     CAN 03              SETON                                        93
     C   03
     CANNKA
     CANNKB              SETON                                        91
     C   03
     CANNKA
     CANNKB              EXSR      TOTAIS
     C   04              EXSR      SR99
     C   04              GOTO      END
     C   KA
     COR KB              GOTO      END
     C     FIM           TAG
     C  NKB
     CANN91
     CANN92              SETON                                        93
     C     END           TAG
     C*-------------------------------------------------------------------
     C* SUB-ROTINA - SR99  Inicio do Programa
     C*-------------------------------------------------------------------
     C     SR99          BEGSR
     C                   MOVEL     CF            KEYCTL
     C                   MOVEL     CA            KEY9
     C                   MOVE      KEY9          KEYCTL
     C     KEYCTL        CHAIN     SDCTL                              LR
     C   LR              GOTO      END99
     C                   MOVE      '-'           ARR
     C                   TIME                    TIMEDT
     C                   SETON                                        916667
     C     CLIE          COMP      '05'                                   90
     C  N90CLIE          COMP      '12'                                   90
     C*
     C     END99         ENDSR
     C*-------------------------------------------------------------------
     C*    SUBROTINA - INICIO - DE TESTE AO Nº DE ORDEM
     C*-------------------------------------------------------------------
     C     INICIO        BEGSR
     C                   SETOFF                                           07
     C                   SETOFF                                           21
     C                   SETOFF                                           50
     C     NO            CHAIN     FRECNP                             09
     C*
     C                   MOVE      APOL          APOL2
     C                   Z-ADD     CCOMP1        CCOMP2
     C                   Z-ADD     CRAMO1        CRAMO2
     C*
     C   09              MOVE      AR(3)         ERRO             40
     C   09              GOTO      FIM1
     C     CALT          COMP      'S'                                    08
     C  N08CALT          COMP      'M'                                    08
     C   08              MOVE      AR(2)         ERRO
     C   08              GOTO      FIM1
     C     TR            COMP      1                                  0808
     C   08              MOVE      AR(30)        ERRO
     C   08              GOTO      FIM1
     C     VCOMI1        COMP      0                                      64
     C   64AST           COMP      '*'                                6464
     C   64              MOVE      AR(38)        ERRO
     C   64              GOTO      FIM1
     C     NAVD          COMP      0                                  0707
     C  N07DTAVD         COMP      0                                  0707
     C   07              MOVE      AR(1)         ERRO
     C   07              SETOFF                                       47
     C     CSEG1         CHAIN     MPESNP                             21
     C   21              MOVE      *BLANKS       DSEG1            36
     C  N21              MOVE      DSEG          DSEG1
     C     CSITMP        COMP      1                                  12  12
     C  N12CODALT        COMP      'S'                                    34
     C  N12
     CAN 34              MOVE      AR(32)        ERRO
     C   12              MOVE      AR(4)         ERRO
     C   34
     COR 12              GOTO      FIM1
     C     CODALT        COMP      'C'                                    87
     C     APOLIC        COMP      APOL                               2323
     C   23              MOVE      AR(5)         ERRO
     C   23              MOVE      APOL          ERRO
     C   23              GOTO      FIM1
     C*
     C     CHV16         CHAIN     FSAPOL                             80
     C   80              MOVE      AR(35)        ERRO
     C   80              GOTO      FIM1
     C  NKDCDSUPF        COMP      'S'                                    80
     C   80
     CANNKD              MOVE      AR(37)        ERRO
     C   80
     CANNKD              GOTO      FIM1
     C     CDBLQF        COMP      1                                      50
     C   50              MOVE      AR(33)        ERRO
     C   50              GOTO      FIM1
     C*
     C     CDBLQF        COMP      2                                      50
     C   50              MOVE      AR(34)        ERRO
     C   50              GOTO      FIM1
     C*        87          GOTO S1
     C*                    Z-ADD0         CCOMI1
     C*                    Z-ADD0         CC11
     C*                    Z-ADD0         CC21
     C*          S1        TAG
     C                   EXSR      MOVE
     C     FIM1          TAG
     C   08
     COR 09
     COR 12
     COR 23
     COR 34
     COR 50
     COR 64
     COR 80              SETON                                        91
     C                   SETOFF                                       50
     C                   ENDSR
     C*-------------------------------------------------------------------
     C*     SUBROTINA -MOVE- GUARDA DADOS DE INPUT (VER CMD 1)
     C*-------------------------------------------------------------------
     C     MOVE          BEGSR
     C                   SETOFF                                       202224
     C                   SETOFF                                       252627
     C                   SETOFF                                       49
     C  N01
     CANNKA              GOTO      S0
     C                   MOVE      NUMRE1        NUMREC            8
     C                   Z-ADD     CCOMP1        CCOMP             3 0
     C                   Z-ADD     CSEG1         CSEG              6 0
     C                   Z-ADD     CRAMO1        CRAMO             3 0
     C                   Z-ADD     DTINI1        DTINIC            6 0
     C                   Z-ADD     DTFIM1        DTFIM             6 0
     C                   Z-ADD     VPREM1        VPREM            13 2
     C                   Z-ADD     VCOMI1        VCOMIS            9 2
     C                   Z-ADD     NAVD          NAVDX             7 0
     C                   Z-ADD     DTAVD         DTAVDX            6 0
     C                   MOVE      DTRECX        DTREC             6 0
     C                   MOVE      NUMGRX        NUMGR             8
     C                   MOVE      DTGRX         DTGR              6 0
     C                   MOVE      DTENTX        DTENTR            6 0
     C  NKA
     CAN 01              Z-ADD     CFIX1         CFIX              3 0
     C  NKA
     CAN 01              Z-ADD     CCOMI1        CCOMIS            3 0
     C  NKA
     CAN 01              Z-ADD     NCOMI1        NCOMIS            9 2
     C  NKA
     CAN 01              Z-ADD     CC11          CC1               3 0
     C  NKA
     CAN 01              Z-ADD     VCC11         VCC1              9 2
     C  NKA
     CAN 01              Z-ADD     CC21          CC2               3 0
     C  NKA
     CAN 01              Z-ADD     VCC21         VCC2              9 2
     C  NKA
     CAN 01              MOVE      DVAR1         DVAR             35
     C  NKA
     CAN 01              Z-ADD     CFACT1        CFACT             1 0
     C  NKA
     CAN 01CFIX          COMP      0                                      18
     C  NKA
     CAN 01
     CANN18CFIX          CHAIN     MDESCR                             18
     C  NKA
     CAN 01
     CAN 18              MOVE      *BLANKS       DFIX1
     C  NKA
     CAN 01
     CAN 18              MOVE      *BLANKS       DFIX2
     C                   MOVE      APOL          APOLIC
     C                   SETON                                        92
     C                   SETOFF                                       49
     C     S0            TAG
     C*          CD0       COMP 0                    49
     C     CFACT         COMP      0                                  49
     C   49
     CANN07              SETOFF                                       47
     C  N47
     CANN07              MOVE      AR(31)        ERRO
     C   07              SETON                                        49
     C  N49              GOTO      S3
     C     CSIT          COMP      0                                  27
     C   27CSIT          COMP      2                                  262425
     C   24              MOVE      AR(8)         MSG              25
     C   25              MOVE      AR(9)         MSG
     C   26              MOVE      AR(10)        MSG
     C     S3            TAG
     C     CCOMP         CHAIN     MCOMPNP                            20
     C     CRAMO         CHAIN     MRNP                               22
     C     FIM4          TAG
     C   27              SETON                                        92
     C                   ENDSR
     C*-------------------------------------------------------------------
     C*    SUBROTINA  -VALID- QUE VALIDA OS ELEMENTOS ENTRADOS
     C*-------------------------------------------------------------------
     C     VALID         BEGSR
     C                   SETOFF                                       1011
     C                   SETOFF                                       2829
     C                   SETOFF                                       303132
     C                   SETOFF                                       3335
     C                   SETOFF                                       414243
     C                   SETOFF                                       535456
     C                   SETOFF                                       5859
     C                   SETOFF                                       606162
     C                   SETOFF                                       6388
     C                   SETOFF                                       707172
     C*          S1        TAG
     C     CFIX          COMP      0                                  69  19
     C   19              MOVE      AR(19)        ERRO
     C  N19CFIX          COMP      899                                19
     C   19
     CAN 69              MOVE      AR(29)        ERRO
     C   19              SETON                                        28
     C   19              MOVE      *BLANKS       DFIX1
     C   19              MOVE      *BLANKS       DFIX2
     C   19              GOTO      FIM3
     C  N19CFIX          CHAIN     MDESCR                             28
     C   28              MOVE      AR(6)         ERRO
     C   28              MOVE      *BLANKS       DFIX1
     C   28              MOVE      *BLANKS       DFIX2
     C   28              GOTO      FIM3
     C     DVAR          COMP      *BLANKS                                11
     C   11              GOTO      S2
     C                   MOVEL     DVAR          D                 1
     C     D             COMP      *BLANK                                 10
     C   10              MOVE      AR(12)        ERRO
     C   10              GOTO      FIM3
     C     S2            TAG
     C     CCOMIS        COMP      0                                  31
     C   31CCOMIS        COMP      CCORL                                  56
     C  N56
     CAN 31
     CANN90              SETON                                        5841
     C   58
     CANN90              MOVE      AR(22)        ERRO
     C   58
     CANN90              MOVE      CCORL         ERRO
     C   58
     CANN90              GOTO      FIM3
     C     CC1           COMP      0                                  32
     C     CC2           COMP      0                                  33
     C   32
     CANN31
     CANN90
     COR 33
     CANN31
     CANN90              SETON                                        4158
     C   58
     CANN90              MOVE      AR(20)        ERRO
     C   58
     CANN90              GOTO      FIM3
     C     CC1           COMP      CCORL                                  29
     C   29
     CANN90              MOVE      AR(28)        ERRO
     C   29
     CANN90              MOVE      CCORL         ERRO
     C   29
     CAN 32
     CANN90              SETON                                        4230
     C   29
     CANN90              GOTO      FIM3
     C     CC2           COMP      CCORL                                  29
     C   29
     CANN90              MOVE      AR(28)        ERRO
     C   29
     CANN90              MOVE      CCORL         ERRO
     C   29
     CAN 33
     CANN90              SETON                                        4330
     C   29
     CANN90              GOTO      FIM3
     C   31CCOMIS        CHAIN     MCORNP                             41
     C   32CC1           CHAIN     MCORNP                             42
     C   33CC2           CHAIN     MCORNP                             43
     C   41
     COR 42
     COR 43              SETON                                        30
     C   30              MOVE      AR(7)         ERRO
     C   30              GOTO      FIM3
     C     NCOMIS        COMP      0                                  61
     C     VCC1          COMP      0                                  62
     C     VCC2          COMP      0                                  63
     C     AST           COMP      '*'                                    88
     C   88              GOTO      FIM88
     C  N31
     CAN 61
     COR 31
     CANN61              MOVE      AR(21)        ERRO
     C   31
     CANN61
     CORN31
     CAN 61              SETON                                        7060
     C   70              GOTO      FIM3
     C  N32
     CAN 62
     COR 32
     CANN62              MOVE      AR(21)        ERRO
     C   32
     CANN62
     CORN32
     CAN 62              SETON                                        7160
     C   71              GOTO      FIM3
     C  N33
     CAN 63
     COR 33
     CANN63              MOVE      AR(21)        ERRO
     C   33
     CANN63
     CORN33
     CAN 63              SETON                                        7260
     C   72              GOTO      FIM3
     C                   GOTO      FIM88A
     C     FIM88         TAG
     C   61
     COR 62
     COR 63              MOVE      AR(36)        ERRO
     C   61              SETON                                        7060
     C   62              SETON                                        7160
     C   63              SETON                                        7260
     C   61
     COR 62
     COR 63              GOTO      FIM3
     C     FIM88A        TAG
     C   31VCC1          ADD       VCC2          TCC              10 2
     C   31NCOMIS        COMP      TCC                                  53
     C   53
     CANN90              MOVE      AR(25)        ERRO
     C   53
     CANN90              SETON                                          7172
     C   53
     CANN90              SETON                                        60
     C   53
     CANN90              GOTO      FIM3
     C   31NCOMIS        COMP      VCOMIS                             54
     C   54              MOVE      AR(26)        ERRO
     C   54              SETON                                        6070
     C   54              GOTO      FIM3
     C   31
     CANN32
     CANN33              SETON                                        593233
     C   59
     CANN90              MOVE      AR(27)        ERRO
     C   59
     CANN90              SETON                                        607172
     C   59
     CANN90              GOTO      FIM3
     C     CFACT         COMP      0                                      35
     C  N35CFACT         COMP      9                                  35
     C   35              MOVE      AR(11)        ERRO
     C     FIM3          TAG
     C   10
     COR 28
     COR 35
     COR 30
     COR 58
     COR 60              SETON                                        92
     C                   ENDSR
     C*-------------------------------------------------------------------
     C*   SUBROTINA -TOTAIS- CALCULO DOS TOTAIS ENTRADOS
     C*-------------------------------------------------------------------
     C     TOTAIS        BEGSR
     C   49TTPREM        SUB       VPREM1        TTPREM
     C   49
     CANNKLTTPREM        ADD       VPREM         TTPREM
     C   49TTCOM         SUB       VCOMI1        TTCOM
     C   49
     CANNKLTTCOM         ADD       VCOMIS        TTCOM
     C   49
     CAN KLTTREG         SUB       1             TTREG
     C  N49VPREM         ADD       TTPREM        TTPREM           13 2
     C  N49VCOMIS        ADD       TTCOM         TTCOM            10 2
     C  N49TTREG         ADD       1             TTREG             4 0
     C  NKL              EXCEPT
     C                   ENDSR
     C*-------------------------------------------------------------------
     OWS        D    91NKG
     O                                           K8 'EC01    '
     O                       NO            Z   0006
     O                       APOLIC            0016
     O                       ERRO           B  0056
     O                       TTPREM            0073 '  .   .   . 0 ,  '
     O                       TTCOM             0085 '     . 0 ,  '
     O                       TTREG         Z   0089
     O          D    92NKG
     O                                           K8 'EC02    '
     O                       NO            Z   0006
     O                       APOLIC            0016
     O                       CSEG          Z   0022
     O                       DSEG              0058
     O                       CCOMP         Z   0061
     O                       DCOMP             0078
     O                       CRAMO         Z   0081
     O                       DRAMO             0104
     O                       VPREM              121 '  .   .   . 0 ,  '
     O                       VCOMIS             133 ' .   . 0 ,  '
     O                       DTINIC             141 '  /  /  '
     O                       DTFIM              149 '  /  /  '
     O                       CFIX          Z    152
     O                       DFIX1              187
     O                       DFIX2              222
     O                       DVAR               257
     O                       CCOMIS        Z    260
     O                       NCOMIS        Z    269
     O                       CC1           Z    272
     O                       VCC1          Z    281
     O                       CC2           Z    284
     O                       VCC2          Z    293
     O                       CFACT              294
     O                       ERRO           B   334
     O          D    93NKGNKA
     O                                           K8 'EC03    '
     O                       NO            Z   0006
     O                       APOLIC            0016
     O                       CSEG          Z   0022
     O                       DSEG              0058
     O                       CCOMP         Z   0061
     O                       DCOMP             0078
     O                       CRAMO         Z   0081
     O                       DRAMO             0104
     O                       VPREM              121 '  .   .   . 0 ,  '
     O                       VCOMIS             133 ' .   . 0 ,  '
     O                       DTINIC             141 '  /  /  '
     O                       DTFIM              149 '  /  /  '
     O                       CFIX          Z    152
     O                       DFIX1              187
     O                       DFIX2              222
     O                       DVAR               257
     O                       CCOMIS        Z    260
     O                       NCOMIS             272 ' .   . 0 ,  '
     O                       CC1           Z    275
     O                       VCC1               287 ' .   . 0 ,  '
     O                       CC2           Z    290
     O                       VCC2               302 ' .   . 0 ,  '
     O                       CFACT              303
     O                       A                  379
     O*         DR   LR
     ORECAVD    EADD 99
     O         OR    91 KL
     O                                            4 '5302'
     O                       NO                  10
     O                       NUMREC              28
     O                       APOLIC              20
     O                       CCOMP               31
     O                       CSEG                37
     O                       CRAMO               40
     O                       DTINIC              46
     O                       DTFIM               52
     O                       VPREM               59P
     O                       VCOMIS              64P
     O                       DTREC               70
     O                       NUMGR               78
     O                       DTGR                84
     O                       DTENTR              90
     O                       NAVD               100P
     O              NKL      DTAVD              106
     O              NKL      CFIX               109
     O              NKL      DVAR               144
     O              NKL      CCOMIS             146P
     O                       NCOMIS             151P
     O              NKL      CC1                153P
     O                       VCC1               158P
     O              NKL      CC2                160P
     O                       VCC2               165P
     O              NKL      CALT               302
     O              NKL      CFACT              304
     O               KL                         106 '000000'
     O               KL                         109 '000'
     O               KL                         129 '                    '
     O               KL                         144 '               '
     O               KL      CCOM1              146P
     O               KL      CCOM2              153P
     O               KL      CCOM3              160P
     O               KL                         302 'R'
     O               KL                         304 '0'
     O                       UDATE              332
     O                       USER               347
     OFRECNP    E    91NKGNKA
     O                       NUMREC         B    28
     O                       APOLIC         B    20
     O                       CCOMP          B    31
     O                       CSEG           B    37
     O                       CRAMO          B    40
     O                       DTINIC         B    46
     O                       DTFIM          B    52
     O                       VPREM          B    59P
     O                       NAVD           B   100P
     O              NKL      DTAVD          B   106
     O              NKL      CFIX           B   109
     O              NKL      DVAR           B   144
     O              NKL      CCOMIS         B   146P
     O                       NCOMIS         B   151P
     O              NKL      CC1            B   153P
     O                       VCC1           B   158P
     O              NKL      CC2            B   160P
     O                       VCC2           B   165P
     O              NKL                         300 '1'
     O              NKL      CFACT          B   304
     O               KL                         106 '000000'
     O               KL                         109 '000'
     O               KL                         129 '                    '
     O               KL                         144 '               '
     O               KL      CCOM1              146P
     O               KL      CCOM2              153P
     O               KL      CCOM3              160P
     O               KL                         300 '0'
     O               KL                         304 '0'
     O                       USER               207
     O*---------------------------------------------------------
     O* MAPA PARA REGISTO DE ANULACOES DE RECIBOS POR AV. DEBITO
     O*---------------------------------------------------------
     OSD101     D    91 KL 78               1 01
     O*--> lg  OR    OF
     O                       CT04                30
     O                                          189 'Pág.'
     O                       PAGE               198
     O          D    91 KL 78               1
     O*--> lg  OR    OF
     O                       PROC                 8
     O                                           10 '*'
     O                       PROG                19
     O                                           99 '  DIÁRIO ANALITICO D'
     O                                          119 'E RECIBOS ANULADOS P'
     O                                          139 'OR AVISO DE DÉBITO  '
     O                                          189 'Data'
     O                       DATSYS             198 '  /  /  '
     O          D    91 KL 78               1
     O*--> lg  OR    OF
     O                                            6 'User :'
     O                       USER                15
     O                                          189 'Hora'
     O                       TIMSYS             198 '  :  :  '
     O          D    91 KL 78               1
     O*--> lg  OR    OF
     O                       ARR                198
     O          D    91 KL 78               1
     O*--> lg  OR    OF
     O                                            8 'NúMERO'
     O                                           27 'NÚMERO'
     O                                          134 'VALOR'
     O                                          148 'VALOR'
     O                                          159 'DATA'
     O                                          186 'DATA'
     O          D    91 KL 78               1
     O*--> lg  OR    OF
     O                                           25 'DE     APÓLICE     DO'
     O                                           41 'COMPANHIA'
     O                                           68 'S E G U R A D O'
     O                                           98 'R A M O'
     O                                          113 'PRAZO'
     O                                          133 'DO'
     O                                          146 'DA'
     O                                          158 'DO'
     O                                          179 'AVISO DE DÉBITO'
     O                                          185 'DE'
     O          D    91 KL 78               1
     O*--> lg  OR    OF
     O                                            7 'ORDEM'
     O                                           43 'RECIBO                '
     O                                          108 'INÍCIO'
     O                                          117 'FIM'
     O                                          149 'RECIBO      COMISSÃO'
     O                                          160 'RECIBO'
     O                                          188 'NO.      DATA   ENTRADA'
     O          D    91 KL 78               2
     O*--> lg  OR    OF
     O                       ARR                198
     O*--> lg   DF   KLN47 78               1
     O                       NO            Z      8
     O                       APOL                19
     O                       NUMRE1              28
     O                       CCOMP1        Z     32
     O                                           33 '-'
     O                       DCOMP1         B    42
     O                       CSEG1         Z     49
     O                                           50 '-'
     O                       DSEG1          B    86
     O                       CRAMO1        Z     90
     O                                           91 '-'
     O                       DRAMO1         B   100
     O                       DTINI1             109 '  /  /  '
     O                                          111 'a'
     O                       DTFIM1             120 '  /  /  '
     O                       VPREM1             138 '  .   .   . 0 ,  -'
     O                       VCOMI1             152 ' .   . 0 ,  -'
     O                       DTRECX             161 '  /  /  '
     O                       NAVDX         Z    170
     O                       DTAVDX             179 '  /  /  '
     O                       DTENTX             188 '  /  /  '
     O*---------------------------------------------------------
**
Rectific.Recibo já c/AVISO-DÉBITO EMIT.     1
     Registo  Suprimido  ou  Morto          2
     Número  de  Ordem  Inexistente         3
 Segurado  sem  ordem  p/ser  facturado     4
Apólice Incompativel c/N.Ord.               5
    Código de Descrição Incorrecto          6
 Codigo  de  Correspondente  Incorrecto     7
      Recibo  Devolvido  Por  Errado        8
           Recibo  já  Facturado            9
 Recibo afim de ser tratado directamente   10
Cód.Fact. deve ser compreendido (1 a 9)    11
A 1ªPosicao da descrição está em branco    12
        Falta  de  pagamento               13
        Tratado directamente               14
  *** Código de segurado por criar ***     15
        ***  Data  inválida  ***           16
*** Código de Ramo está por defenir ***    17
  Código de Companhia está por defenir     18
É um campo de preenchimento obrigatório    19
Se há Correspondente falta a N/Comissão    20
 Cód.preenchido valor preench/igualmente   21
  O código N/Comissão deve de ser : XXX    22
Comissão excede valor previsto (KN-Cont)   23
 O cód. Alteração deve ser um (R)          24
N/Comis. c/valor inf.a soma dos corresp.   25
N/Comissão ultrap.o valor/comissão total   26
N/Comissão especif.falta correspondentes   27
Cód.de corresp.deve ser diferente de XXX   28
Código  Descrição  Fixa  ultrapassa  899   29
Número Ordem não é um recibo de Prémio     30
        R E C T I F I C A Ç Ã O            31
Segurado Suprimido no Ficheiro             32
Apólice sem ordem p/ser facturada.Sit 1    33
Apólice sem ordem p/ser facturada.Sit 2    34
Apólice não encontrada. Veja nas Mortas    35
Recibo sem Comissão - Comissão Inválida    36
Apólice Suprimida. Use CMD-4.              37
Recibo com Comissão igual a zero           38
**
      Confirme os dados agora vizualisados, NÃO há nova validação
