     FGUIA      IP   F  550        DISK
     FGCOBGR    IF   E           K DISK
     FINRECIF   UF A E           K DISK
     FRECI811   O  A F   72        DISK
     D*===========================================================
     D                 DS
     D  DTINI                  1      8  0
     D  AAINIC                 3      4  0
     D  MMINIC                 5      6  0
     D  DDINIC                 7      8  0
     D*
     D                 DS
     D  DTFIM                  1      8  0
     D  AAFIMC                 3      4  0
     D  MMFIMC                 5      6  0
     D  DDFIMC                 7      8  0
     D*
     D                 DS
     D  DTENV                  1      8  0
     D  WYEAR                  3      4  0
     D  WMONTH                 5      6  0
     D  WDAY                   7      8  0
     I****************************************************************
     IGUIA      AA  01
     I                                  1    9 0NREC
     I                                 10   11 0RAMO
     I                                 12   18 0APOL
     I                             P  182  184 0SEG
     I                             P  239  243 2TOTREC
     I                             P  251  255 2COM
     I                             P  465  468 2JMCOM1
     I                             P  469  472 2JMCOM2
     I                                503  505 0JMM1
     I*
     I                                186  191 0DTINIC
     I                                186  187 0DDINIC
     I                                188  189 0MMINIC
     I                                190  191 0AAINIC
     I                                193  198 0DTFIMC
     I                                193  194 0DDFIMC
     I                                195  196 0MMFIMC
     I                                197  198 0AAFIMC
     *****************************************************************
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     UYEAR         WYEAR
     C                   Z-ADD     UMONTH        WMONTH
     C                   Z-ADD     UDAY          WDAY
     C                   MOVEL     20            DTENV
     C                   SETON                                        90
     C                   ENDIF
     C*
     C     KLST          KLIST
     C                   KFLD                    ENT
     C                   KFLD                    RAMO
     C                   KFLD                    APOL
     C                   KFLD                    NREC
     C*
     C     JMM1          IFEQ      811
     C                   Z-ADD     JMM1          ENT
     C                   Z-ADD     COM           COM1
     C     JMCOM1        IFGT      0
     C                   Z-ADD     JMCOM1        COM1
     C                   ELSE
     C                   Z-ADD     JMCOM2        COM1
     C                   ENDIF
     C                   MOVEL     20            DTINI
     C                   MOVEL     20            DTFIM
     C                   MOVEL     0             WSIT              1
     C     NREC          CHAIN     GCOBGR                             10
     C   10              Z-ADD     GCDPAG        DTPAG
     C     KLST          CHAIN     INRECIF                            11
     C   11              WRITE     INRECIG
     C   11              EXCEPT    REC811
     C                   ENDIF
     *****************************************************************
     ORECI811   EADD         REC811
     O                       RAMO                 2
     O                       APOL                 9
     O                       SEG                 14
     O                       NREC                23
     O                       TOTREC              36 ' .   . 0 ,  -'
     O                       COM1                47 '   .   ,  -'
     O                       DTPAG               55
     O                       DTINI               63
     O                       DTFIM               71
     O                       WSIT                72
