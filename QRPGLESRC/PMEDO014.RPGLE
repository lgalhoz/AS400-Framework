     FGBASEGRO  IP   E           K DISK
     FGFSEG     IF   E           K DISK
     FSD1PE     IF   F  512     6AIDISK    KEYLOC(5)
     FAPOL811O  IF A F  256    14AIDISK    KEYLOC(1)
     FSEGU811   IF A F  200     5AIDISK    KEYLOC(1)
      *
     D                 DS
     D  DTINI                  1      6  0
     D  APD                    1      2  0
     D  APM                    3      4  0
     D  APA                    5      6  0
      *
     D                 DS
     D  APINI                  1      8  0
     D  APAA                   3      4  0
     D  APMM                   5      6  0
     D  APDD                   7      8  0
      *
     D                 DS
     D  DTMATR                 1      6  0
     D  APDM                   1      2  0
     D  APMMX                  3      4  0
     D  APAM                   5      6  0
      *
     D                 DS
     D  APMAT                  1      8  0
     D  APAAM                  3      4  0
     D  APMMM                  5      6  0
     D  APDDM                  7      8  0
      *
      *
     D                 DS
     D  KOUT                   1     14
     D  NSEG                   1      5  0
     D  APOL                   6     12  0
     D  RAMO                  13     14  0
      **************************************************************************
     IAPOL811O  NS
     ISEGU811   NS
      *
     ISD1PE     NS  01
     I                                135  149  TELEF
     I                                150  161  NFAX
     I                                236  244 0NRC
     I                                417  444  EMAIL
      *
      **************************************************************************
      *  MAIN LINE
      **************************************************************************
     C     RRAP          KLIST
     C                   KFLD                    RAMO
     C                   KFLD                    APOL
      *
     C     JMM1          IFEQ      811
     C     SIT           IFEQ      1
     C     SIT           OREQ      4
      *
      *
     C     KOUT          CHAIN     APOL811O                           13
     C     *IN13         IFEQ      *ON
      *
     C                   Z-ADD     APA           APAA
     C                   Z-ADD     APM           APMM
     C                   Z-ADD     APD           APDD
     C     APA           IFGT      50
     C                   MOVEL     19            APINI
     C                   ELSE
     C                   MOVEL     20            APINI
     C                   ENDIF
      *
     C                   Z-ADD     APAM          APAAM
     C                   Z-ADD     APMMX         APMMM
     C                   Z-ADD     APDM          APDDM
     C     APAM          IFGT      50
     C                   MOVEL     19            APMAT
     C                   ELSE
     C                   MOVEL     20            APMAT
     C                   ENDIF
      *
     C                   EXCEPT    APOO
      * SEGURADO
     C     NSEG          CHAIN     GFSEG                              14
     C     NSEG          CHAIN     SEGU811                            15
     C                   Z-ADD     NSEG          PENSEG            6 0
     C     PENSEG        CHAIN     SD1PE                              16
     C     *IN16         IFEQ      *ON
     C                   MOVEL     *BLANKS       EMAIL
     C                   MOVEL     *BLANKS       NFAX
     C                   MOVEL     *BLANKS       TELEF
     C                   Z-ADD     *ZEROS        NRC
     C                   ENDIF
     C  N14
     CAN 15              EXCEPT    SEGU
      *
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
      **************************************************************************
      *
     OAPOL811O  EADD         APOO
     O                       NSEG                 5
     O                       APOL                12
     O                       RAMO                14
     O                       APINI               22
     O                       MES1                24
     O                       MACONT              28
     O                       FRAC                29
     O                       NASC                31
     O                       FRANQO              40 ' .   .   '
     O                       LRPS                85
     O                       CR1                 86
     O                       CAP1               100 '  .   .   .   '
     O                       PR1                112 ' .   .   ,  '
     O                       CR2                113
     O                       CAP2               127 '  .   .   .   '
     O                       PR2                139 ' .   .   ,  '
     O                       CR3                140
     O                       CAP3               154 '  .   .   .   '
     O                       PR3                166 ' .   .   ,  '
     O                       OBS1               211
     O                       OBS2               256
      *
     OSEGU811   EADD         SEGU
     O                       NSEG                 5
     O                       NOM                 45
     O                       MORAD               85
     O                       CPOST               92
     O                       DESCP              112
     O                       NIB                133
     O                       TELEF              148
     O                       NFAX               160
     O                       NRC                169
     O                       EMAIL              197
