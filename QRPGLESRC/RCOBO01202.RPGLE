     H DFTNAME(SD101E) DATEDIT(*DMY/)
     FWS        CP   F  500        WORKSTN
     FFRECNP    UC   F  326R    6AIDISK    KEYLOC(5)
     FFRECNPO   IC   F  326R    6AIDISK    KEYLOC(5)
     FMRONP     IC   F  326R    6AIDISK    KEYLOC(5)
     FMCOMPNP   IC   F  128R    3AIDISK    KEYLOC(5)
     FMCORNP    IC   F  128R    3AIDISK    KEYLOC(4)
     FMDESCR    IC   F  256R    3AIDISK    KEYLOC(3)
     FMPESNP    IC   F  512R    6AIDISK    KEYLOC(5)
     FMRNP      IC   F  128R    3AIDISK    KEYLOC(5)
     FFSAPOL1   IC   F  320R   16AIDISK    KEYLOC(8)
     FRECAVD    O  A F  350        DISK
     D AR              S             40    DIM(31) CTDATA PERRCD(1)
     D A               S             74    DIM(1) CTDATA PERRCD(1)
     D C               S             14    DIM(3) CTDATA PERRCD(1)
     D R               S             23    DIM(3) CTDATA PERRCD(1)
     D                 DS
     D  CCOM1                  1      3  0
     D  CCOM2                  4      6  0
     D  CCOM3                  7      9  0
     D  APOL                  11     20
     D  CCOMP                 21     23  0
     D  CRAMO                 24     26  0
     D  ACR                   11     26
     D                UDS
     D  USER                   1      8
     D  NUMCLI                86     87  0
     D  CCORL                244    246  0
     IWS        NS  04    1 C
     I          NS  01    1 C1
     I                                  2    7 0NO
     I                                  8   17  APOLIC
     I          NS  02    1 C2
     I                                  2    4 0CFIX
     I                                  5    7 0CFIX2
     I                                  8   42  DVAR
     I                                 43   48 0ORIGEM
     I                                 49   51 0CCOMIS
     I                                 52   60 2NCOMIS
     I                                 61   63 0CC1
     I                                 64   72 2VCC1
     I                                 73   75 0CC2
     I                                 76   84 2VCC2
     I                                 85   85 0CDENV
     I          NS  03    1 C3
     IMRONP     NS
     I                                  3    4 0CREG
     I                                 11   20  APOLX
     I                                 29   31 0COMPX
     I                                309  309 0CSITX
     IFRECNPO   NS
     I                                  3    4 0CREG
     I                                 11   20  APOLX
     I                                 29   31 0COMPX
     I                                309  309 0CSITX
     IFRECNP    NS
     I                                  3    4 0CODMOV
     I                                 11   20  APOL
     I                                 21   28  NUMREC
     I                                 29   31 0CCOMP
     I                                 32   37 0CSEG
     I                                 38   40 0CRAMO
     I                                 41   46 0DTINIC
     I                                 47   52 0DTFIM
     I                             P   53   59 2VPREM
     I                             P   60   64 2VCOMIS
     I                                 65   70 0DTREC
     I                                 71   78  NUMGR
     I                                 79   84 0DTGR
     I                                 85   90 0DTENTR
     I                             P   97  100 0NAVC
     I                                101  106 0DTAVC
     I                                107  109 0CFIX
     I                                110  144  DVAR
     I                             P  145  146 0CCOMIS
     I                             P  147  151 2NCOMIS
     I                             P  152  153 0CC1
     I                             P  154  158 2VCC1
     I                             P  159  160 0CC2
     I                             P  161  165 2VCC2
     I                                184  189 0ORIGEM
     I                                297  299 0CFIX2
     I                                304  304 0CDENV
     I                                302  302  CALT
     I                                309  309 0CSIT
     I                                315  315  AST
     IMPESNP    NS
     I                                 15   50  DSEG
     I                                129  129 0CSITMP
     IMCOMPNP   NS
     I                                  8   24  DCOMP
     IMRNP      NS
     I                                  8   30  DRAMO
     IMDESCR    NS
     I                                  6   40  DFIXA
     I                                 41   75  DFIXB
     IMCORNP    NS
     IFSAPOL1   NS
     I                             P  154  155 0CORR1
     I                             P  156  157 0CORR2
     I                             P  158  159 0CORR3
     I                                320  320  CDSUPR
     C                   SETOFF                                       091126
     C                   SETOFF                                       2728
     C                   SETOFF                                       313233
     C                   SETOFF                                       353637
     C                   SETOFF                                       383941
     C                   SETOFF                                       424369
     C                   SETOFF                                       707172
     C                   SETOFF                                       919293
     C                   SETOFF                                       99
     C     NUMCLI        COMP      5                                      80
     C  N80NUMCLI        COMP      12                                     80
     C   01              EXSR      ORDEM
     C   KG
     CAN 01              SETON                                        LR
     C   LR              GOTO      FIMX
     C   KL
     CANN47              SETON                                        91
     C   KL              Z-ADD     0             NCOMIS
     C   KL              Z-ADD     0             VCC1
     C   KL              Z-ADD     0             VCC2
     C   KL              EXCEPT
     C   KL              GOTO      FIMX
     C   KA
     CAN 01
     COR KA
     CAN 02              SETON                                        91
     C   91              GOTO      FIMX
     C   KA
     CAN 03              SETON                                        92
     C   92              GOTO      FIMX
     C   02              EXSR      VALID
     C   04              SETON                                        91
     C   04              GOTO      FIMX
     C   03              SETON                                        91
     C   03              EXSR      TOTAIS
     C   03              GOTO      FIMX
     C     FIMX          TAG
     C*-------------------------------------------------------------------
     C*    TESTE AO Nº DE ORDEM E TIPO DE RECIBO
     C*-------------------------------------------------------------------
     C     ORDEM         BEGSR
     C                   SETOFF                                       070809
     C                   SETOFF                                       1288
     C                   SETOFF                                       212223
     C                   SETOFF                                       25
     C                   SETOFF                                       47
     C     NO            COMP      0                                      09
     C   09              MOVE      AR(8)         ERRO
     C   09              GOTO      FIM1
     C     NO            CHAIN     FRECNP                             09
     C   09              MOVE      AR(3)         ERRO             40
     C   09              GOTO      FIM1
     C     CALT          COMP      'S'                                    08
     C  N08CALT          COMP      'M'                                    08
     C   08              MOVE      AR(2)         ERRO
     C   08              GOTO      FIM1
     C     CODMOV        COMP      21                                     64
     C   64VCOMIS        COMP      0                                      64
     C   64AST           COMP      '*'                                6464
     C   64              MOVE      AR(30)        ERRO
     C   64              GOTO      FIM1
     C     NAVC          COMP      0                                  0707
     C  N07DTAVC         COMP      0                                  0707
     C   07              MOVE      AR(1)         ERRO
     C   07              GOTO      FIM1
     C     CSEG          CHAIN     MPESNP                             25
     C  N25CSITMP        COMP      1                                  12  12
     C   12              MOVE      AR(10)        ERRO
     C   12              MOVE      APOL          ERRO
     C   12              GOTO      FIM1
     C     APOLIC        COMP      APOL                               1212
     C   12              MOVE      AR(5)         ERRO
     C   12              MOVE      APOL          ERRO
     C   12              GOTO      FIM1
     C     CODMOV        COMP      21                                     21
     C     CODMOV        COMP      32                                     22
     C     CODMOV        COMP      43                                     23
     C     ACR           CHAIN     FSAPOL1                            12
     C   12              MOVE      AR(23)        ERRO
     C   12              GOTO      FIM1
     C  NKDCDSUPR        COMP      'S'                                    12
     C   12
     CANNKD              MOVE      AR(24)        ERRO
     C   12
     CANNKD              GOTO      FIM1
     C                   SETOFF                                       90
     C  N21              SETON                                        90
     C   21AST           COMP      '*'                                    90
     C     NUMCLI        IFNE      1
     C  N21
     CANN22
     CANN23              MOVE      AR(13)        ERRO
     C  N21
     CANN22
     CANN23              SETON                                        09
     C                   ELSE
     C  N21
     CANN22              MOVE      AR(17)        ERRO
     C  N21
     CANN22              SETON                                        09
     C                   END
     C     FIM1          TAG
     C  N09
     CANN08
     CANN07
     CANN12
     CANN64              SETON                                        92
     C  N92              SETON                                        91
     C   92CDENV         COMP      0                                      47
     C   92              EXSR      KEY
     C                   ENDSR
     C*-------------------------------------------------------------------
     C* SUB-ROTINA VALIDAÇÃO DOS CAMPOS DE INPUT
     C*-------------------------------------------------------------------
     C     VALID         BEGSR
     C     CFIX          COMP      0                                  69  28
     C   28              MOVE      AR(19)        ERRO
     C   69CFIX          COMP      399                                28
     C   69CFIX          COMP      300                                  28
     C   69
     CAN 28              MOVE      AR(29)        ERRO
     C  N28CFIX          CHAIN     MDESCR                             28
     C   28              MOVE      AR(6)         ERRO
     C   28              MOVE      *BLANKS       DFIX1            35
     C   28              MOVE      *BLANKS       DFIX2            35
     C   28              GOTO      FIM
     C                   MOVE      DFIXA         DFIX1
     C                   MOVE      DFIXB         DFIX2
     C     CFIX2         COMP      0                                  26
     C   26CFIX2         CHAIN     MDESCR                             27
     C   26
     CANN27              MOVE      DFIXA         DFIX3            35
     C   26
     CANN27              MOVE      DFIXB         DFIX4            35
     C   27              MOVE      AR(6)         ERRO
     C   27              MOVE      *BLANKS       DFIX3
     C   27              MOVE      *BLANKS       DFIX4
     C   27              GOTO      FIM
     C     DVAR          COMP      *BLANKS                            1111
     C   11              MOVEL     DVAR          D                 1
     C   11D             COMP      *BLANK                                 11
     C   11              MOVE      AR(12)        ERRO
     C   11              GOTO      FIM
     C*
     C     NUMCLI        IFEQ      1
     C   KI              Z-ADD     999999        ORIGEM
     C   KI              SETOFF                                       09
     C   KI              GOTO      SALT09
     C     ORIGEM        COMP      0                                    0909
     C   09              MOVE      AR(4)         ERRO
     C   09              GOTO      FIM
     C                   GOTO      CLI01
     C                   END
     C*
     C     CRAMO         COMP      002                                0909
     C   09CRAMO         COMP      013                                0909
     C   09CRAMO         COMP      502                                0909
     C   09CRAMO         COMP      513                                0909
     C  N09ORIGEM        COMP      0                                  09
     C   09ORIGEM        COMP      998789                               09
     C  N09              GOTO      SALT09
     C     CLI01         TAG
     C     ORIGEM        CHAIN     FRECNPO                            09
     C   09ORIGEM        CHAIN     MRONP                              09
     C   09              MOVE      AR(4)         ERRO
     C   09              GOTO      FIM
     C     CSITX         COMP      1                                      09
     C  N09CSITX         COMP      3                                      09
     C   09              MOVE      AR(9)         ERRO
     C   09              GOTO      FIM
     C     CREG          COMP      15                                 3737
     C  N37APOLX         COMP      APOL                               3838
     C  N37
     CANN38COMPX         COMP      CCOMP                              3939
     C   37              MOVE      AR(14)        ERRO
     C   38              MOVE      AR(15)        ERRO
     C   38              MOVE      APOLX         ERRO
     C   39              MOVE      AR(16)        ERRO
     C   39              MOVE      COMPX         ERRO
     C   37
     COR 38
     COR 39              SETON                                        09
     C   09              GOTO      FIM
     C     SALT09        TAG
     C     CCOMIS        COMP      0                                  31
     C   31CCOMIS        COMP      CCORL                              4141
     C   41
     CANN80              MOVE      AR(22)        ERRO
     C   41
     CANN80              MOVE      CCORL         ERRO
     C   41
     CANN80              GOTO      FIM
     C   41
     CAN 80              SETOFF                                       41
     C  N31NCOMIS        COMP      0                                  7070
     C   31NCOMIS        COMP      0                                      70
     C   70              MOVE      AR(21)        ERRO
     C   70              GOTO      FIM
     C     CC1           COMP      0                                  32
     C     CC2           COMP      0                                  33
     C   21AST           COMP      '*'                                    88
     C   32
     CANN31
     CANN80
     COR 33
     CANN31
     CANN80              SETON                                        41
     C   88              GOTO      FIM88
     C   32CC1           COMP      CCORL                                  42
     C   42
     CANN80              MOVE      AR(28)        ERRO
     C   42
     CANN80              MOVE      CCORL         ERRO
     C   42
     CANN80              GOTO      FIM
     C   33
     CANN80CC2           COMP      CCORL                                  43
     C   43
     CANN80              MOVE      AR(28)        ERRO
     C   43
     CANN80              MOVE      CCORL         ERRO
     C   43
     CANN80              GOTO      FIM
     C   32CC1           CHAIN     MCORNP                             42
     C   33
     CANN42CC2           CHAIN     MCORNP                             43
     C   42
     COR 43              MOVE      AR(7)         ERRO
     C   42
     COR 43              GOTO      FIM
     C   32VCC1          COMP      0                                      71
     C  N32VCC1          COMP      0                                  71
     C   32VCC2          COMP      0                                      72
     C  N33VCC2          COMP      0                                  72
     C   71
     COR 72              MOVE      AR(21)        ERRO
     C   71
     COR 72              GOTO      FIM
     C                   GOTO      FIM88A
     C     FIM88         TAG
     C   31
     COR 32
     COR 33              MOVE      AR(31)        ERRO
     C   31              SETON                                        70
     C   32              SETON                                        71
     C   33              SETON                                        72
     C   31
     COR 32
     COR 33              GOTO      FIM
     C     FIM88A        TAG
     C   31VCC1          ADD       VCC2          TCC              10 2
     C   31NCOMIS        COMP      TCC                                  70
     C   70
     CANN80              MOVE      AR(25)        ERRO
     C   31
     CANN70NCOMIS        COMP      VCOMIS                             70
     C   70
     CANN80              MOVE      AR(26)        ERRO
     C   70
     CANN80              GOTO      FIM
     C   70
     CAN 80              SETOFF                                       70
     C  N80
     CAN 31
     CANN32
     CANN33              SETON                                        42
     C  N80
     CAN 31
     CANN32
     CANN33              MOVE      AR(27)        ERRO
     C  N80
     CAN 31
     CANN32
     CANN33              GOTO      FIM
     C     CDENV         COMP      0                                      35
     C  N35CDENV         COMP      9                                  35
     C   35              MOVE      AR(11)        ERRO
     C   35              GOTO      FIM
     C     ERRO          COMP      *BLANKS                            3636
     C     FIM           TAG
     C   09
     COR 11
     COR 27
     COR 28
     COR 41
     COR 42              SETON                                        92
     C   43
     CANN92
     COR 70
     CANN92
     COR 71
     CANN92
     COR 72
     CANN92
     COR 35
     CANN92              SETON                                        92
     C*** VER A FUNÇÃO DO (KB)
     C  N91
     CANN92              SETON                                        93
     C     END           ENDSR
     C*-------------------------------------------------------------------
     C*  SUB-ROTINA DE CALCULO DE TOTAIS
     C*-------------------------------------------------------------------
     C     TOTAIS        BEGSR
     C     TTREG         ADD       1             TTREG             4 0
     C     TPREM         ADD       VPREM         TPREM            13 2
     C     TCOMIS        ADD       VCOMIS        TCOMIS           13 2
     C                   TIME                    TIMEX             6 0
     C                   EXCEPT
     C                   MOVE      *BLANKS       DFIX1
     C                   MOVE      *BLANKS       DFIX2
     C                   MOVE      *BLANKS       DFIX3
     C                   MOVE      *BLANKS       DFIX4
     C                   ENDSR
     C*-------------------------------------------------------------------
     C*  SUB-ROTINA DE CONSTANTES DE OUTPUT
     C*-------------------------------------------------------------------
     C     KEY           BEGSR
     C   21              MOVE      R(1)          CONST0           23
     C   22              MOVE      R(2)          CONST0
     C   23              MOVE      R(3)          CONST0
     C   21              MOVE      C(1)          CONST1           14
     C   22              MOVE      C(2)          CONST1
     C   23              MOVE      C(3)          CONST1
     C     CCOMP         CHAIN     MCOMPNP                            25
     C     CRAMO         CHAIN     MRNP                               25
     C     FIMK          ENDSR
     OWS        D    91NKG
     O                                           K8 'EC01    '
     O                       NO            Z      6
     O                       APOLIC              16
     O                       ERRO           B    56
     O                       TTREG         Z     60
     O                       TPREM               77 '  .   .   .  0$  '
     O                       TCOMIS              94 '  .   .   .  0$  '
     O          D    92NKG
     O                                           K8 'EC02    '
     O                       CONST0              23
     O                       NO            Z     29
     O                       APOLIC              39
     O                       CSEG          Z     45
     O                       DSEG                81
     O                       CCOMP         Z     84
     O                       DCOMP              101
     O                       CRAMO         Z    104
     O                       DRAMO              127
     O                       CONST1             141
     O                       VPREM              159 '  .   .   .  0$  '
     O                       VCOMIS             171 ' .   .  0$  '
     O                       DTINIC             179 '  /  /  '
     O                       DTFIM              187 '  /  /  '
     O                       CFIX          Z    190
     O                       DFIX1              225
     O                       DFIX2              260
     O                       CFIX2         Z    263
     O                       DFIX3              298
     O                       DFIX4              333
     O                       DVAR               368
     O                       ORIGEM        Z    374
     O                 N90   CORR1              377
     O                 N90   NCOMIS        Z    386
     O                 N90   CORR2              389
     O                 N90   VCC1          Z    398
     O                 N90   CORR3              401
     O                 N90   VCC2          Z    410
     O                       CDENV              411
     O                       ERRO           B   451
     O          D    93NKGNKA
     O                                           K8 'EC03    '
     O                       CONST0              23
     O                       NO            Z     29
     O                       APOLIC              39
     O                       CSEG          Z     45
     O                       DSEG                81
     O                       CCOMP         Z     84
     O                       DCOMP              101
     O                       CRAMO         Z    104
     O                       DRAMO              127
     O                       CONST1             141
     O                       VPREM              159 '  .   .   .  0$  '
     O                       VCOMIS             171 ' .   .  0$  '
     O                       DTINIC             179 '  /  /  '
     O                       DTFIM              187 '  /  /  '
     O                       CFIX          Z    190
     O                       DFIX1              225
     O                       DFIX2              260
     O                       CFIX2         Z    263
     O                       DFIX3              298
     O                       DFIX4              333
     O                       DVAR               368
     O                       ORIGEM        Z    374
     O                 N90   CORR1              377
     O                 N90   NCOMIS             389 ' .   .  0$  '
     O                 N90   CORR2              392
     O                 N90   VCC1               404 ' .   .  0$  '
     O                 N90   CORR3              407
     O                 N90   VCC2               419 ' .   .  0$  '
     O                       CDENV              420
     O                       A                  496
     O          DR   LR
     ORECAVD    EADD 99
     O         OR    91 KL
     O*                                   2 '53'
     O                       CODMOV               4
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
     O                       NAVC               100P
     O                       DTAVC              106
     O              NKL      CFIX               109
     O              NKL      DVAR               144
     O              NKL      CCOMIS             146P
     O              NKL      NCOMIS             151P
     O              NKL      CC1                153P
     O              NKL      VCC1               158P
     O              NKL      CC2                160P
     O              NKL      VCC2               165P
     O              NKL      ORIGEM             189
     O              NKL      CFIX2              299
     O              NKL      CALT               302
     O              NKL      CDENV              304
     O               KL                         109 '000'
     O               KL                         129 '                    '
     O               KL                         144 '               '
     O               KL      CCOM1              146P
     O               KL      NCOMIS             151P
     O               KL      CCOM2              153P
     O               KL      VCC1               158P
     O               KL      CCOM3              160P
     O               KL      VCC2               165P
     O               KL                         189 '000000'
     O               KL                         299 '000'
     O               KL                         302 'R'
     O               KL                         304 '0'
     O                       UDATE              332
     O                       TIMEX              339
     O                       USER               347
     OFRECNP    E    91NKGNKA
     O                       NAVC               100P
     O                       DTAVC              106
     O              NKL      CFIX           B   109
     O              NKL      DVAR           B   144
     O              NKL      CCOMIS         B   146P
     O              NKL      NCOMIS         B   151P
     O              NKL      CC1            B   153P
     O              NKL      VCC1           B   158P
     O              NKL      CC2            B   160P
     O              NKL      VCC2           B   165P
     O              NKL      ORIGEM         B   189
     O              NKL      CFIX2          B   299
     O              NKL                         300 '1'
     O              NKL      CDENV          B   304
     O               KL                         109 '000'
     O               KL                         129 '                    '
     O               KL                         144 '               '
     O               KL      CCOM1              146P
     O               KL      NCOMIS             151P
     O               KL      CCOM2              153P
     O               KL      VCC1               158P
     O               KL      CCOM3              160P
     O               KL      VCC2               165P
     O               KL                         189 '000000'
     O               KL                         299 '000'
     O               KL                         300 '0'
     O               KL                         304 '0'
     O                       USER               207
**
       Nota de Envio ja  Emitida            1
     Registo  Suprimido  ou  Morto          2
     Numero  de  Ordem  Inexistente         3
        - N. Ord.Origem invalido -          4
Apolice nao e a mesma --->    ..........    5
    Codigo de Descricao Incorrecto          6
 Codigo  de  Correspondente  Incorrecto     7
   Nao foi indicado o Numero de Ordem       8
  O Numero Ordem Ordigem foi devolvido      9
Seg.Tratado directamente ou c/Rec.n/Cobr   10
     Codigo Envio Deve Ser de 1 a 9        11
A 1.Posicao da descricao esta em branco    12
Recibo nao e de Estorno/Indemniz/Compart   13
N. Origem nao corresp.a recibo de Premio   14
N. Origem c/Apolice diferente ..........   15
N. Origem c/Cod.Companhia diferente  ...   16
Recibo nao e de Estorno/Comparticipacao    17
----------------------------------------   18
E um campo de preenchimento obrigatorio    19
Se ha Correspondente falta a N/Comissao    20
  Codigo sem valor                         21
  O codigo da Comissao deve de ser : XXX   22
Apolice nao encontrada. Veja nas Mortas    23
Apolice Suprimida. Use CMD-4.              24
N/Comis. c/valor inf.a soma dos corresp.   25
N/Comissao ultrap.o valor/comissao total   26
N/Comissao especif.falta correspondentes   27
Cod.de corresp.deve ser diferente de XXX   28
Cod.Descr.ultrapassa limites (300 a 399)   29
Recibo com Comissao igual a zero           30
Recibo sem Comissao - Comissao Invalida    31
**
      Confirme os dados agora vizualisados, NAO ha nova validacao
**
Valor Estorno.                                   1
Valor Compart.                                   2
Valor Indemniz                                   3
**
          E S T O R N O
        COMPARTICIPACAO
           INDEMNIZACAO
