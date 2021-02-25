     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H DECEDIT(',') DFTNAME(GP015) DATEDIT(*DMY.) FIXNBR(*ZONED) AUT(*ALL)
     H*==================================================================
     H* Programa..: PTESO001
     H* Descrição.: Teste às subh-opções de Menu
     H*==================================================================
     FFTSTE001  CF   E             WORKSTN PREFIX(ECR_) INFDS(INFD#KEYS)
     FGBASEDIO  IF   F  700     9AIDISK    KEYLOC(1) EXTFILE('LGALHOZ/GBASEDIO')
     FGBASEGRO  IF   F  700     9AIDISK    KEYLOC(1)
     F********************************************************************
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SSECO010

     D EXPORTDTA       DS
     D RBASEBK                 1    700
     D RAMO                    1      2  0
     D APOLICE                 3      9  0
     D SEG                    11     50
     D CR1_                  217    217  0
     D CAP1                  218    223P 0
     D TX1                   224    226P 4
     D TX1AT                 224    226P 3
     D PR1                   227    231P 2
     D CR2_                  232    232  0
     D CAP2                  233    238P 0
     D TX2                   239    241P 4
     D PR2                   242    246P 2
     D CR3_                  247    247  0
     D CAP3                  248    253P 0
     D TX3                   254    256P 4
     D PR3                   257    261P 2
     D CAPTOT                262    267P 0
     D PRSACT                271    275P 2
     D CF01                  568    568
     D CF02                  569    569
     D CF03                  570    570
     D CF04                  571    571
     D CF05                  572    572
     D CF06                  573    573
     D CF07                  574    574
     D CF08                  575    575
     D CF00800               621    621
     D CF00830               622    622
     D CF09                  576    576
     D CF10                  577    577
     D CF11                  578    578
     D CF12                  579    579
     D CF13                  533    533
     D                 DS
     D  RRAPOL                 1      9  0
     D  RR                     1      2  0
     D  APOL                   3      9  0
     D  WRR                    1      2
     D  WAPOL                  3      9
     D  WPtrData       S               *
     D  WDatInicio     S              8S 0
     D  WModo          S               N
     D
     D*==> Manutenção das Descrições Fixas
     D CallOption      PR                  EXTPGM('PPRDO100')
     D   RRAPOL                       9S 0 CONST
     D   ptrOR                         *   CONST
     D   DatInicio                    8S 0 CONST
     D   WModo                         N   CONST
     D
     D********************************************************************
     I* FICHEIRO DE RECOLHA DE APÓLICES                  *
     I****************************************************
     IGBASEDIO  AA
     I                                  1    2 0RR
     I                                  1    9 0RRAPOL
     I                                 10   10  REF
     I                                 11   50  SEG
     I                                 51   90  MOR
     I                             P   91   93 3COM1
     I                                 94  113  CPMOR2
     I                                114  114 0IM
     I                                115  154  LCB
     I                             P  155  157 3COM2
     I                                158  177  CPLCB2
     I                                178  178 0SIT
     I                                179  184 0DTINIC
     I                                179  180 0DDINIC
     I                                181  182 0MMINIC
     I                                183  184 0AAINIC
     I                                185  185 0DUR
     I                                186  186 0FRAC
     I                                187  188 0MES1
     I                                189  192 0MACONT
     I                                189  190 0MMCONT
     I                                191  192 0AACONT
     I                                193  193 0CEMIS
     I                                194  194 0ISEN
     I                             P  195  196 0CONC
     I                             P  197  198 0MED1
     I                                199  199  EXCONT
     I                                200  200  PILOTO
     I                             P  201  203 0MED2
     I*                                     204 205 FILLER
     I                                206  206 0TALT
     I                                207  212 0XDTALT
     I                                207  208 0DDALT
     I                                209  210 0MMALT
     I                                211  212 0AAALT
     I                                213  214 0MALT
     I                             P  215  216 0ACTA
     I                                217  217 0CR1
     I                             P  218  223 0CAP1
     I                             P  224  226 4TX1
     I                             P  224  226 3TX1AT
     I                             P  227  231 2PR1
     I                                232  232 0CR2
     I                             P  233  238 0CAP2
     I                             P  239  241 4TX2
     I                             P  242  246 2PR2
     I                                247  247 0CR3
     I                             P  248  253 0CAP3
     I                             P  254  256 4TX3
     I                             P  257  261 2PR3
     I                             P  262  267 0CAPTOT
     I                             P  268  270 4TXMED
     I                             P  268  270 3TXMAT
     I                             P  271  275 2PRSACT
     I                             P  276  279 0FRANQO
     I                                280  324  LRPS
     I                             P  325  326 1TXIND
     I                             P  327  328 2TXSNB
     I                                329  330  NASC
     I                             P  331  332 0PROFSL
     I                             P  333  334 2PFRC
     I                                335  336 0CL01
     I                                337  338 0CL02
     I                                339  340 0CL03
     I                                341  342 0CL04
     I                                343  344 0CL05
     I                                345  346 0CL06
     I                                335  346 0WCL
     I                                347  391  OBS1
     I                                392  436  OBS2
     I                             P  437  441 2TOTREC
     I                             P  442  446 2PRSANT
     I                             P  447  450 2PRIANT
     I                             P  451  453 0NSEG
     I                                454  459 0DTFIM
     I                                454  455 0DDFIM
     I                                456  457 0MMFIM
     I                                458  459 0AAFIM
     I                                460  460 0TEST
     I                             P  461  462 0TXEST
     I                                463  463 0CS
     I                                464  464  RA                              RISCO
     I*                                                                   APLICA
     I                                465  465  INDEX
     I                                466  470 0NSEG2
     I                                471  500  NATUR                           NATURE
     I                                501  501  CLASS                           CLASSE
     I                                502  502  PERDCQ                          PERDAS
     I                                503  503  TPERD                           TIPO P
     I*                                                                        I
     I*                                                                        F
     I                                504  505 0PPERDI                          PERCEN
     I                                506  514 0ENCFIX                          TOTAL
     I                                515  516 0PINDMN                          PERIOD
     I*                                                                   APLICA
     I                                517  517  MOD                             MODALI
     I*                                                                     F=PR
     I*                                                                     V=PR
     I                                518  518  TPT                             TIPO D
     I*                                                                     P=CO
     I*                                                                     O=CO
     I                                519  524  CDCAE                           CODIGO
     I                             P  525  530 0CAPFAT                          CODIGO
     I                                531  531  AVISN                           RECIBO
     I                                532  532  RECS                            RECIBO
     I                                535  535 0CGRUPO
     I                             P  536  538 0NPESS
     I                                539  539  TPRISC
     I                                540  541  CLTAR
     I*                                     542 551 FILLER
     I                                546  551  FILO3
     I                             P  596  599 0CPMOR1
     I                             P  600  603 0CPLCB1
     I                                604  608 0CRHIP
     I* cedência de Comissões em JM
     I                                609  611 0JMM1                            CEDÊNC
     I                             P  612  614 3JMC1                            CEDÊNC
     I                                615  617 0JMM2                            CEDÊNC
     I                             P  618  620 3JMC2                            CEDÊNC
     I************************************************************************
     I* ASILVA, Dezembro 2006
     I* novos campos dos novos produtos RESIDENTIA, MERCANTILE e CONDOMINIO
     I                                204  204  CONDOM
     I                                205  205  NEWPRD
     I* coberturas facultativas
     I                                568  568  CF01
     I                                569  569  CF02
     I                                570  570  CF03
     I                                571  571  CF04
     I                                572  572  CF05
     I                                573  573  CF06
     I                                574  574  CF07
     I                                575  575  CF08
     I                                576  576  CF09
     I                                577  577  CF10
     I                                578  578  CF11
     I                                579  579  CF12
     I                                533  533  CF13
     I* condições especiais
     I                                580  580  CE001
     I                                534  534  CE002
     I                                581  581  CE003
     I* cláusulas aplicáveis
     I                                582  582  CA030
     I                                583  583  CA031
     I                                584  584  CA100
     I                                585  585  CA101
     I                                586  586  CA102
     I* cláusulas especiais
     I                                587  587  CD103
     I                                588  588  CD130
     I                                589  589  CD131
     I                                590  590  CD132
     I                                591  591  CD133
     I                                592  592  CD134
     I                                593  593  CD135
     I                                594  594  CD136
     I                                595  595  CD137
     I* novos campos adicionados em Maio 2007
     I                             P  552  557 0ADC
     I                                558  558  EMPREG
     I                                559  559  MEMO
     I                                560  563 0ANOEDI
     I                                564  567 0CPEDI
      *
     I                                621  621  CF0800
     I                                622  622  CF0830
     I                                623  623  CF21
     I                                624  638  DCF21
     I                                639  639  CF22
     I                                640  640  CE004
     I                                641  641  TIPCON
     I                                642  642  ESTCON
      *
     I                                680  700  NIB
     I                                  1  700  RBASEBK
     I****************************************************
     I* FICHEIRO - BASE DE APÓLICES                      *
     I****************************************************
     IGBASEGRO  AA
     I                                  1    2 0RR
     I                                  1    9 0RRAPOL
     I                                 10   10  REF
     I                                 11   50  SEG
     I                                 51   90  MOR
     I                             P   91   93 3COM1
     I                                 94  113  CPMOR2
     I                                114  114 0IM
     I                                115  154  LCB
     I                             P  155  157 3COM2
     I                                158  177  CPLCB2
     I                                178  178 0SIT
     I                                179  184 0DTINIC
     I                                179  180 0DDINIC
     I                                181  182 0MMINIC
     I                                183  184 0AAINIC
     I                                185  185 0DUR
     I                                186  186 0FRAC
     I                                187  188 0MES1
     I                                189  192 0MACONT
     I                                189  190 0MMCONT
     I                                191  192 0AACONT
     I                                193  193 0CEMIS
     I                                194  194 0ISEN
     I                             P  195  196 0CONC
     I                             P  197  198 0MED1
     I                                199  199  EXCONT
     I                                200  200  PILOTO
     I                             P  201  203 0MED2
     I*                                     204 205 FILLER
     I                                206  206 0TALT
     I                                207  212 0XDTALT
     I                                207  208 0DDALT
     I                                209  210 0MMALT
     I                                211  212 0AAALT
     I                                213  214 0MALT
     I                             P  215  216 0ACTA
     I                                217  217 0CR1
     I                             P  218  223 0CAP1
     I                             P  224  226 4TX1
     I                             P  224  226 3TX1AT
     I                             P  227  231 2PR1
     I                                232  232 0CR2
     I                             P  233  238 0CAP2
     I                             P  239  241 4TX2
     I                             P  242  246 2PR2
     I                                247  247 0CR3
     I                             P  248  253 0CAP3
     I                             P  254  256 4TX3
     I                             P  257  261 2PR3
     I                             P  262  267 0CAPTOT
     I                             P  268  270 4TXMED
     I                             P  268  270 3TXMAT
     I                             P  271  275 2PRSACT
     I                             P  276  279 0FRANQO
     I                                280  324  LRPS
     I                             P  325  326 1TXIND
     I                             P  327  328 2TXSNB
     I                                329  330  NASC
     I                             P  331  332 0PROFSL
     I                             P  333  334 2PFRC
     I                                335  336 0CL01
     I                                337  338 0CL02
     I                                339  340 0CL03
     I                                341  342 0CL04
     I                                343  344 0CL05
     I                                345  346 0CL06
     I                                335  346 0WCL
     I                                347  391  OBS1
     I                                392  436  OBS2
     I                             P  437  441 2TOTREC
     I                             P  442  446 2PRSANT
     I                             P  447  450 2PRIANT
     I                             P  451  453 0NSEG
     I                                454  459 0DTFIM
     I                                454  455 0DDFIM
     I                                456  457 0MMFIM
     I                                458  459 0AAFIM
     I                                460  460 0TEST
     I                             P  461  462 0TXEST
     I                                463  463 0CS
     I                                464  464  RA                              RISCO
     I*                                                                   APLICA
     I                                465  465  INDEX
     I                                466  470 0NSEG2
     I                                471  500  NATUR                           NATURE
     I                                501  501  CLASS                           CLASSE
     I                                502  502  PERDCQ                          PERDAS
     I                                503  503  TPERD                           TIPO P
     I*                                                                        I
     I*                                                                        F
     I                                504  505 0PPERDI                          PERCEN
     I                                506  514 0ENCFIX                          TOTAL
     I                                515  516 0PINDMN                          PERIOD
     I*
     I*                                                                   APLICA
     I                                517  517  MOD                             MODALI
     I*                                                                     F=PR
     I*                                                                     V=PR
     I                                518  518  TPT                             TIPO D
     I*                                                                     P=CO
     I*                                                                     O=CO
     I                                519  524  CDCAE                           CODIGO
     I                             P  525  530 0CAPFAT                          CODIGO
     I                                531  531  AVISN                           RECIBO
     I                                532  532  RECS                            RECIBO
     I                                535  535 0CGRUPO
     I                             P  536  538 0NPESS
      *                                     542 551 FILLER
     I                                546  551  FILO3
     I                             P  596  599 0CPMOR1
     I                             P  600  603 0CPLCB1
     I                                604  608 0CRHIP
     I* cedência de Comissões em JM
     I                                609  611 0JMM1                            CEDÊNC
     I                             P  612  614 3JMC1                            CEDÊNC
     I                                615  617 0JMM2                            CEDÊNC
     I                             P  618  620 3JMC2                            CEDÊNC
     I************************************************************************
     I* ASILVA, Dezembro 2006
     I* novos campos dos novos produtos RESIDENTIA, MERCANTILE e CONDOMINIO
     I                                204  204  CONDOM
     I                                205  205  NEWPRD
     I* coberturas facultativas
     I                                568  568  CF01
     I                                569  569  CF02
     I                                570  570  CF03
     I                                571  571  CF04
     I                                572  572  CF05
     I                                573  573  CF06
     I                                574  574  CF07
     I                                575  575  CF08
     I                                576  576  CF09
     I                                577  577  CF10
     I                                578  578  CF11
     I                                579  579  CF12
     I                                533  533  CF13
     I* condições especiais
     I                                580  580  CE001
     I                                534  534  CE002
     I                                581  581  CE003
     I* cláusulas aplicáveis
     I                                582  582  CA030
     I                                583  583  CA031
     I                                584  584  CA100
     I                                585  585  CA101
     I                                586  586  CA102
     I* cláusulas especiais
     I                                587  587  CD103
     I                                588  588  CD130
     I                                589  589  CD131
     I                                590  590  CD132
     I                                591  591  CD133
     I                                592  592  CD134
     I                                593  593  CD135
     I                                594  594  CD136
     I                                595  595  CD137
     I* novos campos adicionados em Maio 2007
     I                             P  552  557 0ADC
     I                                558  558  EMPREG
     I                                559  559  MEMO
     I                                560  563 0ANOEDI
     I                                564  567 0CPEDI
      *
     I                                621  621  CF0800
     I                                622  622  CF0830
     I                                623  623  CF21
     I                                624  638  DCF21
     I                                639  639  CF22
     I                                640  640  CE004
     I                                641  641  TIPCON
     I                                642  642  ESTCON
      *
     I                                680  700  NIB
     I                                  1  700  RBASEBK
     I********************************************************************
      /FREE
       //------------------------------------
       // Se não estiver autorizado então sai
       //------------------------------------
       IF NOT $ValAccess(S#PGM:'':99999);
          *INLR = *ON;
          RETURN;
       ENDIF;

       *IN45 = *ON;
       ECR_RAMO = 05;
       ECR_APOLICE = 9900006;
       ECR_MODO = '0';
       DOW (NOT *IN03 AND NOT *IN12);
          ECR_DS#PGM   = $getHeader(S#PGM:'RTSTE00105');
          ECR_DS#KEY01 = $getFooter(S#PGM:'RTSTE00105':*BLANKS);
          ECR_S#PGM    = S#PGM;
          WRITE RTSTE00100;
          WRITE RTSTE00199;
          EXFMT RTSTE00105;
          IF (*IN45);
             RR    = ECR_RAMO;
             APOL  = ECR_APOLICE;
             CHAIN RRAPOL GBASEGRO;
             IF (%FOUND());
                ECR_NOSEG  = SEG;
                ECR_NSEG   = NSEG;
                WDatInicio = 20120615;
                WModo      = *ON;
                WPtrData   = %ADDR(RBASEBK);
                *IN45 = *OFF;
             ENDIF;
          ELSE;
             *IN45 = *ON;
             IF (ECR_Modo = '0');
                WModo = *OFF;
             ELSE;
                WModo = *ON;
             ENDIF;
             CallOption(RRAPOL:WPtrData:WDatInicio:WModo);
  |       ENDIF;
  |    ENDDO;

       *INLR = *ON;
       RETURN;
      /END-FREE
