     H********************************************************************
     H*  EMPRESA    :  JOSE MATA LDA                                     *
     H*  PROGRAMA   :  TRBEN                                             *
     H*  APLICAÇÃO  :  PESSOAS SEGURAS AP                                *
     H*  FUNÇÃO     :  CRIA FICHEIRO FORMATADO DOS BENEFICIÁRIOS AP      *
     H*                PARA ENVIO À LUSITANIA.                           *
     H*  AUTOR      :  PAULO FONSECA                                     *
     H*  DATA       :  12/11/2008                                        *
     H*==================================================================*
     H*                    REGISTO DE MANUTENÇÃO                         *
     H*                                                                  *
     H*   DATA   ABREV.  DESCRIÇÃO DA ALTERAÇÃO                          *
     H* -------- ------  ----------------------                          *
     H*                                                                  *
     H********************************************************************
     FOPLAPHF   IF   E             DISK
     FJMRAMOF   IF   E             DISK
     FLFOPBEN   O  A F   43        DISK
     I****************************************************
      *
     C                   READ      OPLAPHF                                30
      *
     C     *IN30         DOUEQ     *ON
      * Converte ramo
     C     HRRAMO        CHAIN     JMRAMOF                            31
     C  N31              MOVE      RMLS          LRAMO             2
     C   31              MOVE      '  '          LRAMO
      *
     C                   MOVE      HRAPOL        LAPOL             7
     C                   MOVE      HRNSEG        LNSEG             6
     C                   MOVEL     '9'           LNSEG
      *
     C                   MOVE      HRNHER        LNHER             6
     C                   MOVEL     '9'           LNHER
      *
     C                   MOVEL     HRPARE        LNPARE            2
      *
     C     HRDTAD        IFEQ      *ZEROS
     C                   MOVE      '99999999'    LDATA             8
     C                   ELSE
     C                   MOVE      HRDTAD        LDATA
     C                   ENDIF
      *
     C                   MOVE      HRSIT         LNSIT             1
      *
     C     HRSITD        IFEQ      *ZEROS
     C                   MOVE      '99999999'    LSITD             8
     C                   ELSE
     C                   MOVE      HRSITD        LSITD
     C                   ENDIF
      *
     C     HRQUOP        IFEQ      *ZEROS
     C                   MOVE      '000'         LQUOP             3
     C                   ELSE
     C                   MOVE      HRQUOP        LQUOP
     C                   ENDIF
      *
     C                   EXCEPT    ADDREG
      *
     C                   READ      OPLAPHF                                30
      *
     C   30              SETON                                        LR
     C                   ENDDO
      ********************************************************************
     OLFOPBEN   EADD         ADDREG
     O                       LRAMO                2
     O                       LAPOL                9
     O                       LNSEG               15
     O                       LNHER               21
     O                       LNPARE              23
     O                       LDATA               31
     O                       LNSIT               32
     O                       LSITD               40
     O                       LQUOP               43
