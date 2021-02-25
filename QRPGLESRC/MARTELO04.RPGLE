     H*=======================================================
     H* Programa...: UPDREC
     H* Objectivo..: Corrigir campos de Observações
     H*
     H* Descrição..: Para correr esta função deverá para o efeito
     H*              realizar o seguinte:
     H*  1. Corrigir o texto de Observações:
     H*  1.1. Corrigir no Original (GBASGRO) os campos de Observações
     H*  1.2. Localizar onde foi gerada a Acta (Continuados ou Carregamento)
     H*  1.3. Executar este programa:
     H*          CALL LGALHOZ/MARTELO03 (<FF> <RR> <AA>)
     H*             <FF> = Nome do ficheiro
     H*             <RR> = Código do Ramo
     H*             <AA> = Número da Apólice
     H*  1.4. Reemitir a Acta (SOURCE/QS36PRC.GP045...)
     H*
     H*  2. Corrigir Motivo de Alteração
     H*  2.1. Corrigir no Original (GBASEDIO) o campo Motivo Alteração
     H*  2.2. Localizar onde foi gerada a Acta (Continuados ou Carregamento)
     H*  2.3. Executar este programa:
     H*          CALL LGALHOZ/MARTELO03 (<FF> <RR> <AA>)
     H*             <FF> = Nome do ficheiro
     H*             <RR> = Código do Ramo
     H*             <AA> = Número da Apólice
     H*  2.4. Reemitir a Acta (SOURCE/QS36PRC.GP045...)
     H*
     H* Observação.:
     H*   Carregamentos = GEXAP[MM][N]
     H*   Continuados   = GEAP[AA][MM]
     H*=======================================================
     FGBASEDIO  IF   E           K DISK    PREFIX('O_')
     FTEMPORAR  UF   F 1000        DISK    USROPN
     D*=======================================================
     D shell           PR                  EXTPGM('QCMDEXC')
     D   StdCmd                     200A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5
     D
     D WStdCmd         S            200A
     D WStdLen         S             15P 5
     D WNomLib         S             10A
     D WNomFile        S             10A
     D WIcErro         S               N
     D PRamo           S              2S 0
     D WRamo           S              2S 0
     D PApol           S              7S 0
     D WApol           S              7S 0
     D WTexto          S             52A
     D PALT            S              2S 0
     D WPALT           S              2S 0
     D WAlt            S              2S 0
     I*=======================================================
     ITEMPORAR  AA  11
     I                                  1    3 0MEDCOB
     I                                  4    5 0RR
     I                                  6   12 0APOL
     I                                  4   12 0RRAPOL
     I                                 13   21 0NREC
     I                                 22   61  SEG
     I                                 62  101  MOR
     I                                105  124  CPMOR2
     I                                125  164  LCB
     I                                168  187  CPLCB2
     I                                188  193 0DTINIC
     I                                194  194 0DUR
     I                                195  195 0CE
     I                                198  203 0DTALT
     I                             P  204  205 0ACTA
     I                                206  206 0CR1
     I                             P  207  212 0CAP1
     I                             P  213  215 4TX1
     I                             P  213  215 3TX1AT
     I                                216  216 0CR2
     I                             P  217  222 0CAP2
     I                             P  223  225 4TX2
     I                                226  226 0CR3
     I                             P  227  232 0CAP3
     I                             P  233  235 4TX3
     I                                236  280  LRPS
     I                                281  282 0NASC
     I                             P  283  284 0PROF
     I                             P  285  286 0TEXTO
     I                                287  298 0WCL
     I                                287  288 0CL1
     I                                289  290 0CL2
     I                                291  292 0CL3
     I                                293  294 0CL4
     I                                295  296 0CL5
     I                                297  298 0CL6
     I                                299  312  MARC1
     I                                313  323  MATR1
     I                                324  325 0AC
     I                                326  326 0CIL
     I                                327  327 0PB
     I                                328  328 0NLUG1
     I                                329  342  MARC2
     I                                343  353  MATR2
     I                                354  354 0TC
     I                                355  355 0NLUG2
     I                                356  400  OBS1
     I                                401  445  OBS2
     I                                446  446 0TREG
     I                                447  452 0DTFIM
     I                             P  453  457 2PRS
     I                             P  458  461 2BONUS
     I                             P  462  466 2ENCP
     I                             P  467  468 2CUSTAA
     I                             P  469  470 2CRC
     I                             P  471  474 2SELO
     I                             P  475  478 2SNA
     I                             P  479  482 2SNB
     I                             P  483  487 2TOTREC
     I                             P  488  491 2COM1
     I                             P  492  494 0MED2
     I                             P  495  498 2COM2
     I                             P  499  501 0FRANQ
     I                                502  502 0CATV
     I                                503  503  C1
     I                                504  504  C2
     I                                505  505  C3
     I                                506  506  C4
     I                                507  507  C5
     I                                508  508  C6
     I                                509  509  C7
     I                                510  510  C8
     I                                511  512 0MALT
     I                                513  513 0ISEN
     I                             P  514  518 2PRRC
     I                                524  524 0FRAC
     I                             P  525  527 0NSEG
     I                                528  557  NATUR                           NATURE
     I                                558  558  CLASS                           CLASSE
     I                                559  559  PERDCQ                          PERDAS
     I                                560  560  TPERD                           TIPO D
     I                                561  562 0PPERDI                          PERCEN
     I                                563  571 0ENCFIX                          VALOR
     I                                572  573 0PINDMN                          PERIOD
     I                                577  577  TCALVG
     I                             P  578  581 2PRSVG
     I                             P  582  585 2PVGAGR
     I                             P  586  589 2TOTVG
     I                             P  590  593 2AVENC
     I                             P  594  597 2AVSELO
     I                             P  598  601 2AVSNA
     I                             P  602  605 2AVCOM1
     I                             P  606  609 2AVCOM2
     I                                610  610  VIAG
     I                             P  611  614 2AVFRAC
     I                             P  615  618 2AVFGA
     I                             P  619  622 2VFRAC                   47
     I                             P  623  626 2FGA                     48
     I                             P  627  630 2VFRCSP
     I                             P  631  634 0VLNOVO
     I                             P  635  637 0VLEXTR
     I                             P  638  641 0CAPDPI
     I                                642  642  ARTG5
     I                                643  644  CODFRQ
     I                             P  650  651 1DESCTO
     I                                652  654  CATREB
     I                             P  655  659 2PRSAGR
     I                                660  660 0TCALPJ
     I                             P  661  664 2WPRSPJ
     I                             P  665  668 2PPJAGR
     I                             P  669  672 2TOTPJ
     I                             P  673  676 2PJENC
     I                             P  677  680 2PJSELO
     I                             P  681  684 2PJSNA
     I                             P  685  688 2PJCOM1
     I                             P  689  692 2PJCOM2
     I                                693  693  PJUR
     I                             P  694  697 2PJFRAC
     I                             P  698  701 2PJFGA
     I                                702  721  MODELO
     I                                722  730 0APREF
     I                                731  736  CDCAE
     I                                737  739 0JMM1
     I                             P  740  742 3JMC1
     I                                743  745 0JMM2
     I                             P  746  748 3JMC2
     I                                749  749  MOD
     I                                750  750  TPT
     I                             P  751  756 0CAPFAT
     I                             P  803  806 2JMCOM1
     I                             P  807  810 2JMCOM2
     I                             P  811  814 2JMAVC1
     I                             P  815  818 2JMAVC2
     I                             P  819  822 2JMPJC1
     I                             P  823  826 2JMPJC2
     I                                827  831 0CRHIP
     I                             P  832  835 0CPMOR1
     I                             P  836  839 0CPLCB1
     I                             P  840  843 0CAPREP
     I                             P  844  849 0DTCREP
     I                                850  850  QIV
     I                                854  854  PRIV
     I                             P  858  862 2PROCU
     I                             P  863  867 2PROCUA
     I                             P  868  870 2SELOCU
     I                             P  871  873 2SNAOCU
     I                             P  875  877 2COM1OC
     I                             P  878  881 2COM2OC
     I                             P  882  885 2JMOCC1
     I                             P  886  889 2JMOCC2
     I                             P  890  893 2OCFRAC
     I                                894  895 0PBONUS
     I                                896  898 0PAGSIN
+    I                                907  907  CONDOM
+    I                                908  908  NEWPRD
+    I                                909  909  CF01
+    I                                910  910  CF02
+    I                                911  911  CF03
+    I                                912  912  CF04
+    I                                913  913  CF05
+    I                                914  914  CF06
+    I                                915  915  CF07
+    I                                916  916  CF08
+    I                                917  917  CF09
+    I                                918  918  CF10
+    I                                919  919  CF11
+    I                                920  920  CF12
+    I                                937  937  CF13
+    I                                909  920  CF999
+    I                                921  921  CE001
+    I                                938  938  CE002
+    I                                922  922  CE003
+    I                                923  923  CA030
+    I                                924  924  CA031
+    I                                925  925  CA100
+    I                                926  926  CA101
+    I                                927  927  CA102
+    I                                928  928  CD103
+    I                                929  929  CD130
+    I                                930  930  CD131
+    I                                931  931  CD132
+    I                                932  932  CD133
+    I                                933  933  CD134
+    I                                934  934  CD135
+    I                                935  935  CD136
+    I                                936  936  CD137
     I                                921  936  CEAD99
     C*=====================================================================
     C     *ENTRY        PLIST
     C                   PARM                    WNomFile
     C                   PARM                    PRAMO
     C                   PARM                    PAPOL
     C
      /FREE
         *IN22 = *OFF;    // Não actualizar o motivo de Alteração
         WRamo = *ZEROS;
         WApol = *ZEROS;

         MONITOR;
            WRamo = PRamo; // Ramo da Apólice
            WApol = PApol; // Número da Apólice
         ON-ERROR;
         ENDMON;

         EXSR OpenFile;
         IF (NOT WIcErro);
            READ TEMPORAR;
            DOW (NOT %EOF());
               IF ((RR = WRamo) AND (APOL = WApol) AND (WApol <> *ZEROS));
                  CHAIN (WRamo:WApol) GBASEDIO;
                  IF (%FOUND());
                     OBS1 = O_OBS1;
                     OBS2 = O_OBS2;
                     MALT  = O_MALT;
                     DTALT = O_DALT;
                     EXCEPT ALTERA;
                  ENDIF;
               ENDIF;
               READ TEMPORAR;
            ENDDO;
            CLOSE TEMPORAR;
         ENDIF;

         *INLR = *ON;
         RETURN;

        //=================================================================
        // Subrotina.: OpenFile
        // Objectivo.: Abrir ficheiro temporário a rectificar
        //=================================================================
         BEGSR OpenFile;
            MONITOR;
               WIcErro = *OFF;
               WNomLib = 'QS36F';
               WStdCmd = 'OVRDBF FILE(TEMPORAR) TOFILE('+%TRIM(WNomLib)     +
                         '/' + %TRIM(WNomFile) + ')';
               WStdLen = %LEN(WStdCmd);
               shell(WStdCmd:WStdLen);
               OPEN TEMPORAR;
            ON-ERROR;
               WIcErro = *ON;
            ENDMON;
         ENDSR;
      /END-FREE
     O*=================================================================
     OTEMPORAR  E            ALTERA
     O                       OBS1               400
     O                       OBS2               445
     O                       MALT               512
     O                       DTALT              203
