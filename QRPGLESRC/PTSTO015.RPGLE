     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H DECEDIT(',') DFTNAME(GP015) DATEDIT(*DMY.) FIXNBR(*ZONED) AUT(*ALL)
     H*===================================================================
     FFPRDT001  UF A E           K DISK
     FGFSEG     IF   F  267     5AIDISK    KEYLOC(1)
     F*===================================================================
     IGFSEG     AA
     I                                  1    5 0NSEG
     I                                  6   25  APEL
     I                                 26   65  SEG
     I                                 66  105  MOR
     I                                106  125  CPMOR2
     I                             P  126  129 0CPMOR1
     I                                130  130  GEST
     I                                131  131  TECN
     I                                132  132  EXCONT
     I                                170  178 0NIF
     C*===================================================================
     C                   EXSR      SRFIC
||   C                   EVAL      PRD00101 = %DATE('2000-01-01':*ISO)
||   C                   EVAL      PRD00102 = %DATE('9999-12-31':*ISO)
||   C                   WRITE     RPRDP001
     C                   SETON                                        LR
     C
     C*===================================================================
     C* Subrotina.: SRFIC
     C* Descrição.: ForCompr Valores por defeito
     C*===================================================================
     CSR   SRFIC         BEGSR
     C                   EVAL      NSEG = 84471
     C     NSEG          CHAIN     GFSEG                              88
     C     *IN88         IFEQ      *ON
     C                   MOVEL     *BLANKS       APEL
     C                   MOVEL     *BLANKS       NIF
     C                   ENDIF
      *
     C                   Z-ADD     92            CDRAMO
     C                   Z-ADD     9900001       NUAPOL
     C                   Z-ADD     NSEG          NUSEG
     C                   MOVEL     SEG           NOSEG
     C                   MOVEL     APEL          APELIDO
     C                   Z-ADD     NIF           NUNIF
     C******             MOVEL     *BLANKS       MATRIC
     C                   Z-ADD     5             STATUS
     C                   MOVEL     'O local'     LCRISK
     C                   Z-ADD     100           CDMED1
     C                   Z-ADD     200           CDJMM1
     C                   Z-ADD     300           CDJMM2
     C                   MOVEL     'S'           ICCARR
     CSR                 ENDSR
