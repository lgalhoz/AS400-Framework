     H********************************************************************
     H*  EMPRESA    :  JOSE MATA LDA                                     *
     H*  PROGRAMA   :  TRSAP                                             *
     H*  APLICA플O  :  PESSOAS SEGURAS AP                                *
     H*  FUN플O     :  CRIA FICHEIRO FORMATADO DE PESSOAS SEGURAS AP     *
     H*                PARA ENVIO � LUSITANIA.                           *
     H*  AUTOR      :  PAULO FONSECA                                     *
     H*  DATA       :  12/11/2008                                        *
     H*==================================================================*
     H*                    REGISTO DE MANUTEN플O                         *
     H*                                                                  *
     H*   DATA   ABREV.  DESCRI플O DA ALTERA플O                          *
     H* -------- ------  ----------------------                          *
     H*                                                                  *
     H********************************************************************
     FOPLAPOF   IF   E             DISK
     FJMRAMOF   IF   E             DISK
     FLFOPSAP   O  A F  549        DISK
     I****************************************************
      *
     C                   READ      OPLAPOF                                30
      *
     C     *IN30         DOUEQ     *ON
      * Converte ramo
     C     LARAMO        CHAIN     JMRAMOF                            31
     C  N31              MOVE      RMLS          LRAMO             2
     C   31              MOVE      '  '          LRAMO
      *
     C                   MOVE      LAAPOL        LAPOL             7
     C                   MOVE      LANSEG        LNSEG             6
     C                   MOVEL     '9'           LNSEG
      *
     C     LADTAD        IFEQ      *ZEROS
     C                   MOVE      '99999999'    LDATA             8
     C                   ELSE
     C                   MOVE      LADTAD        LDATA
     C                   ENDIF
      *
     C     LAPARE        IFEQ      *BLANKS
     C                   MOVE      '00'          PAREN             2
     C                   ELSE
     C                   MOVE      LAPARE        PAREN             2
     C                   ENDIF
      *
     C                   MOVE      LAPSEG        NPARE             6
     C                   MOVE      LASIT         SIT               1
      *
     C     LASITD        IFEQ      *ZEROS
     C                   MOVE      '99999999'    DTSIT             8
     C                   ELSE
     C                   MOVE      LASITD        DTSIT             8
     C                   ENDIF
      *
     C                   MOVE      LATPRI        TPRIS             1
     C                   MOVE      LACLTA        CLTAR             2
     C                   MOVE      'N'           CONTR             1
     C                   MOVE      '0'           CONTP             1
     C                   MOVE      LABENE        BENEF             1
     C                   MOVE      'N'           ICCONF            1
     C                   MOVE      *BLANKS       GENER           500
      *
     C                   EXCEPT    ADDREG
      *
     C                   READ      OPLAPOF                                30
      *
     C   30              SETON                                        LR
     C                   ENDDO
      ********************************************************************
     OLFOPSAP   EADD         ADDREG
     O                       LRAMO                2
     O                       LAPOL                9
     O                       LNSEG               15
     O                       LDATA               23
     O                       PAREN               25
     O                       NPARE               31
     O                       SIT                 32
     O                       DTSIT               40
     O                       TPRIS               41
     O                       CLTAR               43
     O                       CONTR               44
     O                       CONTP               47
     O                       BENEF               48
     O                       ICCONF              49
     O                       GENER              549
