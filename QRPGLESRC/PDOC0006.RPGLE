     H DECEDIT(',') DATEDIT(*DMY.)
     H*
     H* EMISSÃO DE CARTAS-VERDES
     H*
     FMEDSAT    IT   F    4        DISK
     FGUIAORD   IF   F  550        DISK
     FFDOCP060  UF A E           K DISK
     FFDOCP06001IF   E           K DISK    RENAME(RDOCP060:RDOCP06001)
     FMGP080    O    F   99        PRINTER OFLIND(*INOF)
     F                                     FORMLEN(70)
     F                                     FORMOFL(70)
*****F* INICIO ALTERAÇÃO - ASILVA - ABRIL 2006
     FGBASEGRA  IF   F  700     9AIDISK    KEYLOC(1)
*****D* FIM ALTERAÇÃO    - ASILVA - ABRIL 2006
     D TCATV           S              1  0 DIM(7) CTDATA PERRCD(1)
     D TCODV           S              1    DIM(7) ALT(TCATV)
     D TABMS           S              4  0 DIM(40) FROMFILE(MEDSAT) PERRCD(1)   MED SATELITES
     D                                     ASCEND
     D WNCV            S                   LIKE(NCV)
     D WANNCV          S                   LIKE(ANNCV)
*****D* FIM ALTERAÇÃO    - ASILVA - ABRIL 2006
     D                UDS
     D  WAnoMovim              1      4  0
     D  WMesMovim              5      6  0
     D  NCIS                  38     43  0
     D  ANOCIS                44     45  0
     D  WDiaEmis              46     47  0
     D  WMesEmis              48     49  0
     D  WAnoEmis              50     51  0
     D  WNomFile              52     62
     D**************************************************************************
     D*****   NÃO ESQUECER DA LOCAL OFFSET A PEDIR OS CAMPOS "NCISA E ANOCISA"
     D**************************************************************************
     D                 DS
     D  WDTINI                 1      8  0
     D  DDINIC                 1      2  0
     D  MMINIC                 3      4  0
     D  MCINIC                 5      6  0
     D  AAINIC                 7      8  0
     D  WAAINI                 5      8  0
     D                 DS
     D  WDTFIM                 1      8  0
     D  DDFIM                  1      2  0
     D  MMFIM                  3      4  0
     D  MCFIM                  5      6  0
     D  AAFIM                  7      8  0
     D  WAAFIM                 5      8  0
     D*====================================================================
     IGUIAORD   AA  02
     I                                 10   11 0RM
     I                                 12   18 0APOL
*****I* INICIO ALTERAÇÃO - ASILVA - ABRIL 2006
     I                                 10   18 0RMAPOL
*****I* FIM ALTERAÇÃO    - ASILVA - ABRIL 2006
     I                                 19   58  SEG
     I                                 59   98  MOR
     I                                102  121  CPMOR2
     I                             P  182  184 0NCL
     I                                185  185 0TREG
     I                                186  191 0DTINI
     I                                186  187 0DDINIC
     I                                188  189 0MMINIC
     I                                190  191 0AAINIC
     I                                193  198 0DTFIM
     I                                193  194 0DDFIM
     I                                195  196 0MMFIM
     I                                197  198 0AAFIM
     I                             P  208  212 2PRS
     I                             P  213  216 2BONUS
     I                             P  221  222 2CUSTAA
     I                             P  223  224 2CRC
     I                                244  244 0ISEN
     I                                248  250 0MED1
     I                             P  256  258 0MED2
     I                                264  277  MARCA1
     I                                278  288  MATR1
     I                                289  302  MARCA2                  43
     I                                303  313  MATR2
     I                                371  371  VIAG
     I*
     I                                424  424 0CATV
     I                             P  465  468 2JMCOM1
     I                             P  469  472 2JMCOM2
     I                             P  473  476 2JMAVC1
     I                             P  477  480 2JMAVC2
     I                             P  481  484 2JMPJC1
     I                             P  485  488 2JMPJC2
     I                                503  505 0JMM1
     I                             P  506  508 3JMC1
     I                                509  511 0JMM2
     I                             P  512  514 3JMC2
     I                             P  543  546 0CPMOR1
*****I* INICIO ALTERAÇÃO - ASILVA - ABRIL 2006
     IGBASEGRA  BB
     I                                654  654  FROTA
     I*=====================================================================
     C     KDOC060       KLIST
     C                   KFLD                    WNCV
     C                   KFLD                    WANNCV
     C
     C     LER           TAG
     C                   READ      GUIAORD                                99     fim do ficheiro
     C   99              GOTO      FIM
     C*
     C     RM            COMP      50                                     50     ramo auto
     C  N50              GOTO      LER                                           ler outro reg
     C*
     C     TREG          COMP      5                                      41     estorno
     C   41              GOTO      LER                                           ler outro reg
     C*
     C     TREG          COMP      1                                      01     adicional
     C   01ISEN          COMP      4                                      41     isento cisa
     C   41              GOTO      LER                                           ler outro reg
     C*
     C   01ISEN          COMP      5                                      41     isento apol+cisa
     C   41              GOTO      LER                                           ler outro reg
     C*
******* INICIO ALTERAÇÃO - ASILVA - ABRIL 2006
     C*Alteração realizada para prever a reemissão das Apólices
     C*Temporárias.
     C                   SETOFF                                       69
[LG] C                   EVAL      FROTA = ' '
     C     RMAPOL        CHAIN     GBASEGRA                           69         procura apol
     C     FROTA         IFEQ      'F'                                           existe e é frota
     C                   GOTO      LER                                           lê próximo reg
     C                   END                                                     não emite CISA
******* FIM ALTERAÇÃO    - ASILVA - ABRIL 2006
     C*
     C* inicio da preparação dos campos para output das cartas-verdes
     C* calcular data inicio do seguro
     C     AAINIC        IFLT      40                                            ano inicio<40
     C                   MOVEL     20            MCINIC                          ano>=2000
     C                   ELSE                                                    senão
     C                   MOVEL     19            MCINIC                          ano<2000
     C                   END
     C* calcular data fim do seguro
     C     AAFIM         IFLT      40                                            ano fim<40
     C                   MOVEL     20            MCFIM                           ano>=2000
     C                   ELSE                                                    senão
     C                   MOVEL     19            MCFIM                           ano<2000
     C                   END
     C* procurar o nome do mediador
     C     COMUM         TAG
     C     MED1          IFGE      900                                           med c/cob >=900
     C                   Z-ADD     0             JMM1                            med c/cob
     C                   Z-ADD     0             JMC1                            comissão med c/cob
     C                   END
     C*
     C                   Z-ADD     0             WMED1             4 0
     C                   Z-ADD     0             WMED2             4 0
     C     JMM1          IFEQ      *ZEROS                                        med c/cob=0
     C     JMM2          IFEQ      *ZEROS                                        med s/cob=0
     C                   Z-ADD     MED1          WMED1                           med c/cob
     C                   ELSE
     C                   Z-ADD     JMM2          WMED2                           med s/cob
     C                   END
     C                   ELSE
     C                   Z-ADD     JMM1          WMED1                           med c/cob
     C                   END
     C*
     C                   MOVE      MED1          WMED              4 0           med c/cob
     C     WMED          LOOKUP    TABMS                                  10     encontra med
     C   10              DO
     C     JMM1          IFGT      *ZEROS                                        med c/cob >0
     C                   MOVE      MED1          WMED1                           med c/cob
     C                   END
     C     JMM2          IFGT      *ZEROS                                        med s/cob >0
     C                   MOVE      MED1          WMED2
     C                   END
     C                   END
     C* procurar a categoria do veículo
     C                   Z-ADD     1             X                 1 0
     C     CATV          LOOKUP    TCATV(X)                               35
     C   35              MOVE      TCODV(X)      CODCV             1             encontou
     C  N35              MOVE      ' '           CODCV             1             não encontrou
     C* escreve linhas de output da carta-verde
     C                   EXCEPT    LH07
     C                   EXCEPT    LH10
     C  N43              EXCEPT    LH11
     C*
     C* alteração em 31.7.06, ver memo de 20.7.06 de Ana Paula Craveiro
     C     WMED1         IFEQ      817
     C     APOL          IFEQ      9908553
     C                   MOVE      *BLANK        WMED1
     C                   END
     C     APOL          IFEQ      9983107
     C                   MOVE      *BLANK        JMM1
     C                   END
     C                   END
     C* alteração em 31.7.06, ver memo de 20.7.06 de Ana Paula Craveiro
     C*
     C                   EXCEPT    LH14
     C                   EXCEPT    LH15
     C                   EXCEPT    LH17
     C                   EXCEPT    LH18
     C                   EXCEPT    LH19
     C                   EXCEPT    LH20
     C* saber se tem assistencia em viagem - escreve linha de output
     C     VIAG          COMP      ' '                                3333       assist vgm=SIM
     C   33              EXCEPT    LH21
     C* continua a escrever linhas de output
     C                   EXCEPT    LH49
     C                   EXCEPT    LH51
     C                   EXCEPT    LH52
     C                   EXCEPT    LH54
     C                   EXCEPT    LH55
     C                   EXCEPT    LH58
     C                   EXCEPT    LH59
     C                   EXCEPT    LH61
     C                   EXCEPT    LH62
     C                   EXCEPT    LH63
     C                   EXCEPT    LH64
     C                   EXCEPT    LH65
     C                   EXCEPT    LH66
     C                   EXCEPT    LH68
     C                   EXCEPT    LH69
     C                   EXCEPT    LH70
     C* incremento nº CISA
     C                   EXSR      SavData
     C                   ADD       1             NCIS                           incremento nºcisa
     C                   GOTO      LER                                          ler próximo reg
     C* FIM do programa
     C     FIM           TAG
     C                   SETON                                        LR        fim do programa
     C
     C*====================================================================
     C* Subrotina...: SavData
     C* Objectivo...: Guardar os dados das Cartas Verdes numa Tabela
     C*====================================================================
     C     SavData       BEGSR
     C*==> Validar a Existência do Registo
     C                   EVAL      WANNCV = 2000 + ANOCIS
     C                   EVAL      WNCV = NCIS
     C     KDOC060       CHAIN     RDOCP060                           40
     C                   IF        *IN40
     C                   EVAL      NCV = NCIS
     C                   EVAL      ANNCV = 2000 + ANOCIS
     C                   EVAL      CRAMO = RM
     C                   EVAL      NUAPOL = APOL
     C                   EVAL      DTINICIO = 20000000 + (AAINIC * 10000) +
     C                                        (MMINIC * 100) + DDINIC
     C                   EVAL      DTTERMO  = 20000000 + (AAFIM  * 10000) +
     C                                        (MMFIM  * 100) + DDFIM
     C                   EVAL      NUMATR1 =  MATR1
     C                   EVAL      MARCA2 = MARCA2
     C                   EVAL      NUMATR2 =  MATR2
     C                   EVAL      MARCA1 = MARCA1
     C                   EVAL      CATVIA = CODCV
     C                   EVAL      NUMSEG = NCL
     C                   EVAL      NOMSEG = SEG
     C                   EVAL      MORSEG = MOR
     C                   EVAL      CODPOS = CPMOR1
     C                   EVAL      LOCPOS = CPMOR2
     C                   EVAL      PAIMOR = 'Portugal'
     C                   EVAL      NUMED1 = WMED1
     C                   EVAL      NUMED2 = WMED2
     C                   EVAL      FILORG = WNomFile
     C                   EVAL      ASTVIAG = VIAG
     C                   WRITE     RDOCP060
     C                   ELSE
     C*==> Validar se os dados estão coerentes
     C                   IF        (CRAMO <> RM) OR (NUAPOL <> APOL)
     C     'ERRO'        DSPLY
     C                   SETON                                        LR        fim do programa
     C                   RETURN
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR
     C*=================================================================
     OMGP080    E            LH07             07
     O                       DDINIC              33
     O                       MMINIC              37
     O                       WAAINI              43
     O                       DDFIM               47
     O                       MMFIM               51
     O                       WAAFIM              57
     O                                           66 '1.'
     O                       NCIS                72
     O                                           73 '/'
     O                       ANOCIS              75
     O                       APOL          4     90
     O*
     O          E            LH10             10
     O                       MATR1               44
     O                       CODCV               64
     O                       MARCA1              85
     O*
     O          E            LH11             11
     O                       MATR2               44
     O                                           64 'F'
     O                       MARCA2              85
     O*
     O          E            LH14             14
     O                                            9 'MED1:'
     O                       WMED1         Z     13
     O                                           32 '  '
     O                                           36 '  '
     O                                           44 '  '
     O                                           49 '  '
     O                                           53 '  '
     O                                           58 '  '
     O                                           62 '  '
     O                                           66 '  '
     O                                           70 '  '
     O                                           74 '  '
     O                                           79 'XX'
     O                                           83 '  '
     O                                           87 '  '
     O                                           91 'XX'
     O*
     O          E            LH15             15
     O                                            9 'MED2:'
     O                       WMED2         Z     13
     O                                           33 'XX'
     O                                           38 '  '
     O                                           43 'XX'
     O                                           48 'XX'
     O                                           54 'XX'
     O                                           59 'XX'
     O                                           64 'XX'
     O                                           70 '  '
     O                                           75 'XX'
     O                                           80 'XX'
     O                                           86 'XX'
     O                                           91 'XX'
     O*
     O          E            LH17             17
     O                       SEG                 72
     O*
     O          E            LH18             18
     O                       MOR                 72
     O*
     O          E            LH19             19
     O                       CPMOR1              40 '    -   '
     O                       CPMOR2              62
     O*
     O          E            LH20             20
     O                                           40 'PORTUGAL'
     O*
     O          E            LH21             21
     O                                           64 'ASSISTÊNCIA EM VIAGEM 24'
     O                                           82 'H-EUROP ASSISTANCE'
     O*
     O          E            LH49             49
     O                       APOL          4     15
     O*
     O          E            LH51             51
     O                       DDINIC              33
     O                       MMINIC              37
     O                       WAAINI              43
     O                       DDFIM               47
     O                       MMFIM               51
     O                       WAAFIM              57
     O                                           66 '1.'
     O                       NCIS                72
     O                                           73 '/'
     O                       ANOCIS              75
     O                       APOL          4     90
     O*
     O          E            LH52             52
     O                       MATR1               18
     O*
     O          E            LH54             54
     O                       WDTINI              18 '  /  /   0'
     O                       MATR1               44
     O                       CODCV               64
     O                       MARCA1              85
     O*
     O          E            LH55             55
     O                       WDTFIM              18 '  /  /   0'
     O              N43      MATR2               44
     O              N43                          64 'F'
     O              N43      MARCA2              85
     O*
     O          E            LH58             58
     O                                           32 '  '
     O                                           36 '  '
     O                                           44 '  '
     O                                           49 '  '
     O                                           53 '  '
     O                                           58 '  '
     O                                           62 '  '
     O                                           66 '  '
     O                                           70 '  '
     O                                           74 '  '
     O                                           79 'XX'
     O                                           83 '  '
     O                                           87 '  '
     O                                           91 'XX'
     O          E            LH59             59
     O                                           33 'XX'
     O                                           38 '  '
     O                                           43 'XX'
     O                                           48 'XX'
     O                                           54 'XX'
     O                                           59 'XX'
     O                                           64 'XX'
     O                                           70 '  '
     O                                           75 'XX'
     O                                           80 'XX'
     O                                           86 'XX'
     O                                           91 'XX'
     O          E            LH61             61
     O                       SEG                 72
     O*
     O          E            LH62             62
     O                       MOR                 72
     O*
     O          E            LH63             63
     O                  33                       13 ' EUROP '
     O                       CPMOR1              40 '    -   '
     O                       CPMOR2              62
     O*
     O          E            LH64             64
     O                                           15 'ASSISTANCE'
     O                                           40 'PORTUGAL'
     O*
     O          E            LH65             65
     O                  33                       14 'EM PORTUGAL:'
     O                 N33                       16 'EXCLUÍDA'
     O*
     O          E       33   LH66             66
     O                                           14 '808 202 141'
     O          E       33   LH68             67
     O                                           17 'NO ESTRANGEIRO:'
     O*
     O          E       33   LH69             68
     O                                           18 '351 217 806 271'
     O*
     O          E       33   LH70             69
     O                       APOL          4     15
**                                                         FTCATV
1A
2A
3C
4B
5F
6A
7A
