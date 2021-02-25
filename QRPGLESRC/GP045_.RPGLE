     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H CURSYM('¤') DECEDIT(',') DFTNAME(GP045) DATEDIT(*DMY.)
     H*************************************************************************
     H* LISTAR APOLICES DE OUTROS RAMOS
     H*************************************************************************
     H*08/04/86-CC-Rectificar Emissão do Peso Bruto
     H*            Introdução de Minusculas 'c.'
     H*21/10/86-CC-Retirar Emissão do C.R.C.
     H*10/02/87-CC-Alteração das Garantias Ramo Incendio (L0121)
     H*06/09/88-RG-Alteração as linhas de output do ramo incendio (LH20 L0121)
     H*
     H***  CONVERSÃO do Programa 'EMAPOLS'    (S/5280)  ***
     H***Conversão: Indicador 49 substituio o ind.automatico 50
     H*
     H*24/01/89-MC-Conversão dos Ramos actuais p/ codigos dados pelo Instituto.
     H*            Alteração na designação dos ramos.
     H*10/07/89-MC-INCLUSÃO DO ENDEREÇO AO LADO DO TALÃO DE COBRANÇA.
     H*07/12/89-MC-INCLUSÃO DOS RAMOS
     H*13/02/90-MC-INCLUSÃO DOS RISCOS 6 E 7 NO RAMO AUTOAAEML.
     H*03/05/90-MC-INCLUSÃO DO RISCO 7 NO RAMO AC.PESSOAIS (02).
     H*16/04/91-RG-Alteração de constantes ao ramo 05 (excpts L05xxx)
     H*20/05/91-PC-Inclusão no output ( MGP045 ), Nro. do Segurado (NSEG).
     H*30/10/91-PC-Alteração do tamanho do FAPOLORD de 540 p/ 576 Bytes.
     H*            Alteração dos array's, Output's e Calculos.
     H*            Inclusão do Ramo 55 ---> MultiRisco Establecimentos.
     H*            Inclus{o dos Ramos 25 e 35 "RESSEGURO CEDIDO E ACEITE DO
     H*            RM=55. Altera~{o das Tabelas e Array's de forma a acomodar
     H*            os RESPECTIVOS OUTPUT'S.
     H*14/12/95-PC-Alteração das denominações de campos.
     H*            Prémio Comercial = Prémio Simples + Encargos
     H*            Prémio p/Comiss. = Prémio Simples
     H*19/12/93-PC-DISPLAY DOS CAMPOS VALOR FRACIONAMENTO E FUNGA
     H*           -CÁLCULO DO PREMIO COMERCIAL E PREMIO BRUTO.
     H*           -EXPANSÃO DO FICHEIRO DE 616 BYTES PARA 680 BYTES
     H*
     H*01/10/96-PC-DESCODIFICAÇÃO DAS CLÁUSULAS ESPECIAIS E IMPRESSÃO
     H*           DO TEXTO RESPECTIVO.
     H*
     H*26/12/96-PC-INCLUSÃO DOS RISCOS CATASTRÓFICOS
     H*
     H*21/12/98-RG-RAMO 50 O CAPDP=CAPDPI+VLEXTR.
     H*            DATA EXPANDIAS A 8 DIGITOS.
     H*17/02/99-RG EXPANSÃO FICH. APOLORD (680 A 720 BYTES)
     H*            INTRODUÇÃO NOVOS CAMPOS (PROTECÇÃO JURIDICA).
     H*17/06/99-RG EXPANSÃO APOLORD (DE 720 A 750 BYTES).
     H*            INTRODUÇÃO DO CAMPO APÓLICE- REFERÊNCIA 'APREF' NO APOLORD.
     H*01/03/00-PC INCLUSÃO DAS APÓLICES DO RAMO DE ACID. TRABALHO
     H*17/03/03-RG ALTERAÇÃO DA DENOMINAÇÃO RC "ILIMITADO" PARA ¤ 50.000.000
     H*            A PARTIR DE 01.05.2003.
     H*15/04/03-RG INTRODUÇÃO DE TEXTO DA LEI-SOCRATES, APLICÁVEL A VEÍCULOS
     H*            COM A COBERTURA DE DANOS PRÓPRIOS.
     H*            INTRODUÇÃO NOVO RISCO DE FR+IRE (CR2=7).
     H*24/06/04-RG INTRODUÇÃO DE TEXTO (TXT2,12 A TXT2,24) PARA AC.TRABALHO
     H*************************************************************************
     FMEDSAT    IT   F    4        DISK
     FFAPOLORD  IF   F 1000        DISK
     FFTEXTOS   IF   F   46     6AIDISK    KEYLOC(1)
     FTABELAS   IF   F   64    12AIDISK    KEYLOC(2)
     FCRHIPOT   IF   F  400     7AIDISK    KEYLOC(1)
     FFTABX     IF   F  274     7AIDISK    KEYLOC(1)
     FMGP045    O    F   96        PRINTER OFLIND(*INOF)
     F                                     FORMLEN(100)
     F                                     FORMOFL(90)
     F                                     INFDS(PRTFINF)
      * INICIO ALTERAÇÃO - ASILVA - MARÇO 2006
     FGBASEGRA  IF   F  700     9AIDISK    KEYLOC(1)
     FGBASEGRO  IF   F  700     9AIDISK    KEYLOC(1)
     F*GBASEGRO  IF   E           K DISK    PREFIX(OR_)
     FFCATP010  IF   E           K DISK
     F* FIM ALTERAÇÃO **********
     D*=======================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SGRLO014
     DPRTFINF          DS
     D RCD_LEN               125    126I 0
     D CUR_LINE              367    368I 0
     D CUR_PAGE              369    372I 0
     D
     D*==> Indices de Indexação
     D WIndiceDs       DS
     D   WFilo3                1      6
     D   WIndBase              1      3  0
     D   WIndVenc              4      6  0
     D WObs1           S             45A
     D WObs2           S             45A
     D WTextObs        S             12A   INZ('OBSERVAÇÕES:')
     D WIcErro         S               N   INZ(*ON)
     D*-----------------------------------------
     D TABMS           S              4  0 DIM(40) FROMFILE(MEDSAT) PERRCD(1)
     D                                     ASCEND
     D RAMO            S              2  0 DIM(36) CTDATA PERRCD(1)
     D NOMRR           S             34    DIM(36) ALT(RAMO)
     D CINST           S              6  0 DIM(36) CTDATA PERRCD(1)
     D CR01            S              1  0 DIM(5) CTDATA PERRCD(1)
     D NCR01           S             40    DIM(5) ALT(CR01)
     D CR02            S              1  0 DIM(8) CTDATA PERRCD(1)
     D NCR02           S             40    DIM(8) ALT(CR02)
     D CR04            S              1  0 DIM(3) CTDATA PERRCD(1)
     D NCR04           S             40    DIM(3) ALT(CR04)
     D CR05            S              1  0 DIM(8) CTDATA PERRCD(1)
     D NCR05           S             40    DIM(8) ALT(CR05)
     D CR06            S              1  0 DIM(4) CTDATA PERRCD(1)
     D NCR06           S             40    DIM(4) ALT(CR06)
     D CR07            S              1  0 DIM(3) CTDATA PERRCD(1)
     D NCR07           S             40    DIM(3) ALT(CR07)
     D CR08            S              1  0 DIM(3) CTDATA PERRCD(1)
     D NCR08           S             40    DIM(3) ALT(CR08)
     D CR09            S              1  0 DIM(3) CTDATA PERRCD(1)
     D NCR09           S             40    DIM(3) ALT(CR09)
     D CR10            S              1  0 DIM(3) CTDATA PERRCD(1)
     D NCR10           S             40    DIM(3) ALT(CR10)
     D CR15            S              1  0 DIM(2) CTDATA PERRCD(1)
     D NCR15           S             40    DIM(2) ALT(CR15)
     D CR17            S              1  0 DIM(7) CTDATA PERRCD(1)
     D NCR17           S             40    DIM(7) ALT(CR17)
     D CR18            S              1  0 DIM(4) CTDATA PERRCD(1)
     D NCR18           S             43    DIM(4) ALT(CR18)
     D CR19            S              1  0 DIM(2) CTDATA PERRCD(1)
     D NCR19           S             40    DIM(2) ALT(CR19)
     D CR55            S              1  0 DIM(4) CTDATA PERRCD(1)
     D NCR55           S             40    DIM(4) ALT(CR55)
     D CR65            S              1  0 DIM(1) CTDATA PERRCD(1)
     D NCR65           S             40    DIM(1) ALT(CR65)
     D CL01            S              2  0 DIM(44) CTDATA PERRCD(1)
     D NCL01           S             70    DIM(44) ALT(CL01)
     D CL02            S              2  0 DIM(7) CTDATA PERRCD(1)
     D NCL02           S             70    DIM(7) ALT(CL02)
     D CL03            S              2  0 DIM(3) CTDATA PERRCD(1)
     D NCL03           S             70    DIM(3) ALT(CL03)
     D CL04            S              2  0 DIM(65) CTDATA PERRCD(1)
     D NCL04           S             70    DIM(65) ALT(CL04)
     D CL05            S              2  0 DIM(8) CTDATA PERRCD(1)
     D NCL05           S             70    DIM(8) ALT(CL05)
     D CL06            S              2  0 DIM(48) CTDATA PERRCD(1)
     D NCL06           S             70    DIM(48) ALT(CL06)
     D CL15            S              2  0 DIM(7) CTDATA PERRCD(1)
     D NCL15           S             70    DIM(7) ALT(CL15)
     D CL17            S              2  0 DIM(7) CTDATA PERRCD(1)
     D NCL17           S             70    DIM(7) ALT(CL17)
     D CL19            S              2  0 DIM(15) CTDATA PERRCD(1)
     D NCL19           S             70    DIM(15) ALT(CL19)
     D CL55            S              2  0 DIM(15) CTDATA PERRCD(1)
     D NCL55           S             70    DIM(15) ALT(CL55)
     D TCATV           S              1  0 DIM(7) CTDATA PERRCD(1)
     D NCATV           S             31    DIM(7) ALT(TCATV)
     D TCIL            S              1  0 DIM(5) CTDATA PERRCD(1)
     D VCIL            S              4  0 DIM(5) ALT(TCIL)
     D TPB             S              1  0 DIM(5) CTDATA PERRCD(1)
     D VPB             S              5  0 DIM(5) ALT(TPB)
     D TC51            S              1  0 DIM(3) CTDATA PERRCD(1)
     D NTC51           S             24    DIM(3) ALT(TC51)
     D CODDUR          S              1  0 DIM(2) CTDATA PERRCD(1)
     D NOMDUR          S             18    DIM(2) ALT(CODDUR)
     D AFP             S              1  0 DIM(5) CTDATA PERRCD(1)
     D NAFP            S             20    DIM(5) ALT(AFP)
     D TXT1            S             77    DIM(9) CTDATA PERRCD(1)
     D TXT2            S             77    DIM(33) CTDATA PERRCD(1)
     D TXT3            S             77    DIM(4) CTDATA PERRCD(1)
     D TXT4            S             82    DIM(4) CTDATA PERRCD(1)
     D TXT5            S             82    DIM(6) CTDATA PERRCD(1)
     D TXT6            S             82    DIM(6) CTDATA PERRCD(1)
     D WCL             S              2  0 DIM(6)
+    D MERC            S             80    DIM(33) CTDATA PERRCD(1)
+    D WR              S             80    DIM(50)
+    D WM              S             80    DIM(33)
+    D WC              S             80    DIM(26)
+    D MTR             S             34    DIM(3) CTDATA PERRCD(1)
     D MSG             S             80    DIM(2) CTDATA PERRCD(1)
     D RC1             S              5  0 DIM(15) CTDATA PERRCD(1) ASCEND
     D RC2             S             57    DIM(15) ALT(RC1)
     D CA1             S             83    DIM(2) CTDATA PERRCD(1)
     D CA2             S             83    DIM(3) CTDATA PERRCD(1)
     D TXT7            S             77    DIM(3) CTDATA PERRCD(1)
     D****************************************************
[LG] D*DS1            UDS
[LG] D                UDS
     D  NCOP                  20     27
     D*
     D  DTEMIS                46     51  0
     D  DDEMIS                46     47  0
     D  MMEMIS                48     49  0
     D  AAEMIS                50     51  0
     D                 DS
     D  WDTEMI                 1      8  0
     D  WDDEMI                 1      2  0
     D  WMMEMI                 3      4  0
     D  WAAEMI                 5      8  0
     D                 DS
     D  DTINIC                 1      6  0
     D  DDINIC                 1      2  0
     D  MMINIC                 3      4  0
     D  AAINIC                 5      6  0
     D                 DS
     D  WDTINI                 1      8  0
     D  WDDINI                 1      2  0
     D  WMMINI                 3      4  0
     D  WAAINI                 5      8  0
     D                 DS
     D  DTALT                  1      6  0
     D  DDALT                  1      2  0
     D  MMALT                  3      4  0
     D  AAALT                  5      6  0
     D                 DS
     D  WDTALT                 1      8  0
     D  WDDALT                 1      2  0
     D  WMMALT                 3      4  0
     D  WAAALT                 5      8  0
     D                 DS
     D  DTFIM                  1      6  0
     D  DDFIM                  1      2  0
     D  MMFIM                  3      4  0
     D  AAFIM                  5      6  0
     D                 DS
     D  WDTFIM                 1      8  0
     D  WDDFIM                 1      2  0
     D  WMMFIM                 3      4  0
     D  WAAFIM                 5      8  0
     D                 DS
     D  AAMMDD                 1      6  0
     D  WAA                    1      2  0
     D  WMM                    3      4  0
     D  WDD                    5      6  0
     D                 DS
     D  KTAB                   1      7
     D  TPTAB                  1      2
     D  CDTAB                  3      7
     D                 DS
     D  WCATV                  1      3  0
     D  CATV                   1      1  0
     D  CIL                    2      2  0
     D  PB                     3      3  0
     D
     D WTxtFim         S             80A   INZ(*BLANKS)
     D WTxtFrac        S             16A   INZ(*BLANKS)
     D WIcExiste       S               N
     D WCrdHipKey      S              7S 0
     D WIdxText        S              2S 0 INZ(*ZEROS)
     D WIdx            S              2S 0 INZ(*ZEROS)
     D WIdx2           S              2S 0 INZ(*ZEROS)
     D WPtrHistorico   S               *
     D WPtrDiario      S               *
     D WCdGrupo        S              5S 0 INZ(*ZEROS)
     D
     D WIdxTxt         S              5S 0 INZ(*ZEROS)
     D WDetail         S             90A
     D*=====================================================================
      * INICIO ALTERAÇÃO - ASILVA - MARÇO 2006
     IGBASEGRO  WW
     I                                  1  700  RBASEDIO
     I                                206  206 0OR_TALT
     I                                213  214 0OR_MALT
     I                                546  551  OR_FILO3
     I                                186  186 0OR_FRAC
     I                                533  533  OR_CF13
     I                             P  325  326 1OR_TXIND
     IGBASEGRA  WW
     I                                206  206 0TALT
     I                                654  654  FROTA
      * FIM ALTERAÇÃO **********
     IFAPOLORD  AA  11
     I                                  1 1000  RGEXAP
     I                                  1    3 0MEDCOB
     I                                  4    5 0RR
     I                                  6   12 0APOL
      * INICIO ALTERAÇÃO - ASILVA - MARÇO 2006
     I                                  4   12 0RRAPOL
      * FIM ALTERAÇÃO **********
     I                                 13   21 0NREC
     I                                 22   61  SEG
     I                                 62  101  MOR
     I*                                     102 104 FILLER
     I                                105  124  CPMOR2
     I                                125  164  LCB
     I*                                     165 167 FILLER
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
     I                                236  236  VSA
     I*                                           1 - ¤  32,86 (AV.ATÉ 1900C
     I*                                           2 - ¤  45,72 (AV.ATÉ 2500C
     I                                237  237  VIA
     I*                                           1 - ¤  16,43 (AC.ATÉ 1900C
     I*                                           2 - ¤  23,58 (AC.ATÉ 2500C
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
     I                                528  557  NATUR
     I                                558  558  CLASS
     I                                559  559  PERDCQ
     I                                560  560  TPERD
     I                                561  562 0PPERDI
     I                                563  571 0ENCFIX
     I                                572  573 0PINDMN
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
     I*
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
     I* mudou posição                     P 851 8532PRQIV
     I                                854  854  PRIV
     I* mudou posição                     P 855 8572PRPRIV
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
+    I* novos campos do projecto de novos produtos
+    I* RESIDENTIA e MERCANTILE, ASILVA, Janeiro 2007
+    I* indicador de novo produto e de condominio
+    I                                907  907  CONDOM
+    I                                908  908  NEWPRD
+    I* coberturas facultativas
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
+    I* TODAS as coberturas facultativas
+    I                                909  920  CF999
+    I* condições especiais
+    I                                921  921  CE001
+    I                                938  938  CE002
+    I                                922  922  CE003
+    I* clausulas aplicáveis
+    I                                923  923  CA030
+    I                                924  924  CA031
+    I                                925  925  CA100
+    I                                926  926  CA101
+    I                                927  927  CA102
+    I* clausulas diversas
+    I                                928  928  CD103
+    I                                929  929  CD130
+    I                                930  930  CD131
+    I                                931  931  CD132
+    I                                932  932  CD133
+    I                                933  933  CD134
+    I                                934  934  CD135
+    I                                935  935  CD136
+    I                                936  936  CD137
+    I* TODAS as condições especiais e clausulas aplicáveis e diversas
     I                                921  936  CEAD99
     I*
     IFTEXTOS   AA
     I                                  1  006 0CHTEXT
     I                                  7  046  DESC
     I****************************************************
     I*  TABELA GERAIS (COD. CAE)                        *
     I****************************************************
     ITABELAS   NS
     I                                  1    1  ACTREG
     I                                  2    3 0NTAB
     I                                  4   13  CTAB
     I                                 14   53  DCAE
     I*                                      54  64 FILLER
     ICRHIPOT   NS
     I                                  8   47  NOMCRH
     I****************************************************
     I* GUARDIAN - TABELAS PU E VG                       *
     I****************************************************
     IFTABX     NS
     I                                  1    2  TPTAB
     I                                  3    7  CDTAB
     I                                 12   91  DCTAB1
     I                                 92  171  DCTAB2
     I                                187  266  DCTAB3
     C**************************************************************************
     C***       INICIO DE CALCULO - TESTE DAS SUBROTINAS                     ***
     C**************************************************************************
     C                   MOVE      DDEMIS        WDDEMI
     C                   MOVE      MMEMIS        WMMEMI
[LG] C*                  MOVE      AAEMIS        WAAEMI
     C                   EVAL      WAAEMI = AAEMIS
     C     AAEMIS        IFLT      40
     C                   MOVEL     20            WAAEMI
     C                   ELSE
     C                   MOVEL     19            WAAEMI
     C                   END
     C                   MOVEL     DTEMIS        WK4               4 0
     C                   MOVEL     WK4           WUDATE            8 0
     C                   MOVE      DTEMIS        WK4
     C                   MOVEL     20            WK4
     C                   MOVE      WK4           WUDATE
     C                   EVAL      CUR_PAGE = *ZEROS
     C*
     C     LER           TAG
     C                   IF        (%INT(CUR_PAGE/2)*2 <> CUR_PAGE)
     C                   EXCEPT    BRKPAGE
     C                   ENDIF
     C
     C                   READ      FAPOLORD                               99
     C   99              GOTO      FIM
      /FREE
         WIcExiste = *OFF;
         IF (RR <> 50);
            CHAIN RRAPOL GBASEGRO;
            IF (%FOUND);
               WIcExiste = *ON; // Existe registo para este contrato
            ENDIF;
         ENDIF;
      /END-FREE
      * INICIO ALTERAÇÃO - ASILVA - MARÇO 2006 *********************************
     C     RR            IFEQ      50
     C                   SETOFF                                       98
     C     RRAPOL        CHAIN     GBASEGRA                           98
     C   98              DO
     C     FROTA         IFEQ      'F'
     C                   GOTO      LER
     C                   END
     C                   END
     C     FROTA         IFEQ      'F'
     C                   GOTO      LER
     C                   END
     C                   END
      * FIM ALTERAÇÃO **********************************************************
     C*
     C     MEDCOB        IFGE      900
     C**                   Z-ADD0         JMM1
     C**                   Z-ADD0         JMC1
     C                   Z-ADD     MEDCOB        JMM1
     C                   Z-ADD     COM1          JMC1
     C                   END
     C*
     C                   MOVE      DDALT         WDDALT
     C                   MOVE      MMALT         WMMALT
[LG] C*                  MOVE      AAALT         WAAALT
[LG] C                   EVAL      WAAALT = AAALT
     C     AAALT         IFLT      40
     C                   MOVEL     20            WAAALT
     C                   ELSE
     C                   MOVEL     19            WAAALT
     C                   END
     C*
     C                   MOVE      DDINIC        WDDINI
     C                   MOVE      MMINIC        WMMINI
[LG] C*                  MOVE      AAINIC        WAAINI
[LG] C                   EVAL      WAAINI = AAINIC
     C     AAINIC        IFLT      40
     C                   MOVEL     20            WAAINI
     C                   ELSE
     C                   MOVEL     19            WAAINI
     C                   END
     C*
     C                   MOVE      DDFIM         WDDFIM
     C                   MOVE      MMFIM         WMMFIM
[LG] C*                  MOVE      AAFIM         WAAFIM
[LG] C                   EVAL      WAAFIM = AAFIM
     C     AAFIM         IFLT      40
     C                   MOVEL     20            WAAFIM
     C                   ELSE
     C                   MOVEL     19            WAAFIM
     C                   END
     C
     C*=> Acertar data de termo com o incremento do ano
[LG] C                   IF        ((WAAALT*10000+WMMALT*100+WDDALT) >
[LG] C                              (WAAFIM*10000+WMMFIM*100+WDDFIM))
[LG] C                   EVAL      WAAFIM = WAAFIM + 1
[LG] C                   ENDIF
     C
[LG] C     RRTAG         TAG
     C     RR            COMP      01                                     01
     C     RR            COMP      02                                     02
     C     RR            COMP      03                                     03
     C     RR            COMP      04                                     04
     C     RR            COMP      05                                     05
     C     RR            COMP      06                                     06
     C     RR            COMP      07                                     07
     C     RR            COMP      08                                     08
     C     RR            COMP      09                                     09
     C     RR            COMP      10                                     10
     C     RR            COMP      15                                     15
     C     RR            COMP      17                                     57
     C     RR            COMP      18                                     58
     C     RR            COMP      19                                     59
     C     RR            COMP      20                                     20
     C     RR            COMP      21                                     21
     C     RR            COMP      22                                     22
     C     RR            COMP      24                                     64
     C     RR            COMP      25                                     25
     C     RR            COMP      26                                     26
     C     RR            COMP      27                                     27
     C     RR            COMP      28                                     68
     C     RR            COMP      29                                     29
     C     RR            COMP      30                                     30
     C     RR            COMP      31                                     31
     C     RR            COMP      32                                     32
     C     RR            COMP      34                                     74
     C     RR            COMP      35                                     35
     C     RR            COMP      36                                     36
     C     RR            COMP      37                                     37
     C     RR            COMP      38                                     78
     C     RR            COMP      39                                     39
     C     RR            COMP      50                                     50
     C     RR            COMP      51                                     51
     C     RR            COMP      55                                     55
     C     RR            COMP      65                                     65
     C*
     C     COMUNS        TAG
     C                   SETOFF                                       8070
     C                   Z-ADD     0             CTCOP             1 0
     C                   Z-ADD     0             SNAB              7 2
     C                   MOVE      *BLANKS       DSTIT            10
     C*
     C                   Z-ADD     0             WMED1             4 0
     C                   Z-ADD     0             WMED2             4 0
     C                   Z-ADD     0             WCOM1             7 2
     C                   Z-ADD     0             WCOM2             7 2
     C     JMM1          IFEQ      *ZEROS
     C     JMM2          IFEQ      *ZEROS
     C                   Z-ADD     MEDCOB        WMED1
     C                   Z-ADD     COM1          WCOM1
     C                   ELSE
     C                   Z-ADD     JMM2          WMED2
     C                   Z-ADD     JMCOM2        WCOM2
     C                   END
     C                   ELSE
     C                   Z-ADD     JMM1          WMED1
     C                   Z-ADD     JMCOM1        WCOM1
     C                   END
     C*
     C                   MOVE      MEDCOB        WMED              4 0
     C     WMED          LOOKUP    TABMS                                  88
     C   88              DO
     C     JMM1          IFGT      *ZEROS
     C                   MOVE      MEDCOB        WMED1
     C                   END
     C     JMM2          IFGT      *ZEROS
     C                   MOVE      MEDCOB        WMED2
     C                   END
     C                   END
     C*
     C     TREG          COMP      0                                      41
     C     TREG          COMP      1                                      42
     C     TREG          COMP      5                                      43
     C     RR            COMP      20                                 44  44
     C   44RR            COMP      40                                   4444
     C     TOTREC        COMP      0                                      46
     C*
     C   41
     CANN46              MOVEL     'SEGURO '     TIPREG           12
     C   41
     CANN46              MOVE      'NOVO '       TIPREG
     C   42
     CANN46              MOVEL     'ADICION'     TIPREG
     C   42
     CANN46              MOVE      'AL   '       TIPREG
     C   43
     CANN46              MOVEL     'ESTORNO'     TIPREG
     C   43
     CANN46              MOVE      '     '       TIPREG
     C   46              MOVEL     'ACTA S/'     TIPREG
     C   46              MOVE      'VERBA'       TIPREG
     C*
     C     RTEMIS        TAG
     C     CTCOP         ADD       1             CTCOP             1 0
     C     NCOP          IFEQ      'ORIGINAL'
     C     CTCOP         IFGE      2
     C                   SETON                                            80
     C                   END
     C                   END
     C   57
     COR 58
     COR 59              EXSR      SR00
     C   59              EXSR      SR01
     C* código do ramo
     C     L0311         TAG
     C                   Z-ADD     1             IND               2 0
     C     RR            LOOKUP    RAMO(IND)                              16
     C                   MOVEL     *BLANKS       NRAMO            34
     C   16              MOVEL     NOMRR(IND)    NRAMO
     C   16              MOVE      CINST(IND)    CODRM             6 0
     C     CONDOM        IFEQ      'C'
     C                   MOVE      340103        CODRM
     C                   END
     C* nome dos ramos dos novos produtos
     C     NEWPRD        IFEQ      'S'
     C     RR            IFEQ      55
     C                   MOVEL     MTR(2)        NRAMO
     C                   END
     C     RR            IFEQ      05
     C     CONDOM        IFEQ      'C'
     C                   MOVEL     MTR(3)        NRAMO
     C                   ELSE
     C                   MOVEL     MTR(1)        NRAMO
     C                   END
     C                   END
     C                   END
     C*
     C* alteração em 31.7.06, ver memo de 20.7.06 de Ana Paula Craveiro
     C     JMM1          IFEQ      817
     C     APOL          IFEQ      9908553
     C                   MOVE      *BLANK        JMM1
     C                   END
     C     APOL          IFEQ      9983107
     C                   MOVE      *BLANK        JMM1
     C                   END
     C                   END
     C* alteração em 31.7.06, ver memo de 20.7.06 de Ana Paula Craveiro
     C*                           -2-
     C                   EXCEPT    LH0313
     C                   EXSR      DURAC
     C                   EXSR      SRFPAG
     C     DUR           COMP      2                                      49
     C                   EXCEPT    LH37
     C     TREG          COMP      0                                      49
     C   49              EXCEPT    LH38
     C  N49              EXCEPT    LH38A
     C  N41              EXSR      MOTALT
     C   50
     CAN 41
     CANN46              EXCEPT    LH17
      /FREE
         IF (*IN46 = *OFF);
           IF (RR = 05) AND (FRAC > 1) AND
              (OR_TALT = 0 OR (OR_TALT = 1 AND MALT = 70));
              EXCEPT LH21P;
           ELSE;
              EXCEPT LH20P;
           ENDIF;
         ENDIF;
      /END-FREE
     C*
     C     TRATRR        TAG
     C                   MOVEL     *BLANKS       NCR
     C   01              EXSR      TRAT01
     C   02              EXSR      TRAT02
     C   03              EXSR      TRAT03
     C   04              EXSR      TRAT04
     C   05              EXSR      TRAT05
     C   06              EXSR      TRAT06
     C   07              EXSR      TRAT07
     C   08              EXSR      TRAT08
     C   09              EXSR      TRAT09
     C   10              EXSR      TRAT10
     C   15              EXSR      TRAT15
     C   57              EXSR      TRAT17
     C   58              EXSR      TRAT18
     C   59              EXSR      TRAT19
     C   20              EXSR      TRAT10
     C   21              EXSR      TRAT01
     C   22              EXSR      TRAT02
     C   64              EXSR      TRAT04
     C   25              EXSR      TRAT55
     C   26              EXSR      TRAT06
     C   27              EXSR      TRAT07
     C   29              EXSR      TRAT09
     C   30              EXSR      TRAT10
     C   68              EXSR      TRAT18
     C   31              EXSR      TRAT01
     C   32              EXSR      TRAT02
     C   74              EXSR      TRAT04
     C   35              EXSR      TRAT55
     C   36              EXSR      TRAT06
     C   37              EXSR      TRAT07
     C   39              EXSR      TRAT09
     C   78              EXSR      TRAT18
     C   50              EXSR      TRAT50
     C   51              EXSR      TRAT51
     C   55              EXSR      TRAT55
     C   65              EXSR      TRAT65
     C*
     C     L32           TAG
     C   02              EXCEPT    L0233
     C   02              GOTO      L33
     C
     C     OBS1          COMP      ' '                                    49
     C                   IF        (*IN49 OR *IN05)
     C                   GOTO      L35
     C                   ENDIF
     C                   EXCEPT    LH33
     C*
     C     L33           TAG
     C                   EXCEPT    LH34
     C*
     C     L35           TAG
     C   25
     COR 35
     COR 55              GOTO      L35MRE
     C* procurar e escrever nome credor hipotecário
     C     CRHIP         IFGT      *ZEROS
     C                   MOVEL     20            K7                7 0
     C                   MOVE      CRHIP         K7
     C     K7            CHAIN     CRHIPOT                            88
+    C* escreve nome credor hipotecário quando não é produto novo
+[LG]C* N88NEWPRD        IFEQ      ' '
+[LG]C* N88              EXCEPT    LH35
+[LG]C* N88              END
+[LG]C                   IF        (NEWPRD = ' ') AND NOT *IN88
+[LG]C                   EXCEPT    LH35
+[LG]C                   END
     C                   END
     C*
     C* ----------------------------------------------------
     C* *    CALCULO SÓ PARA O RAMO 55 - M.RISCOS EST.     *
     C* ----------------------------------------------------
     C     L35MRE        TAG
     C  N50
     CANN51
     CAN 25
     CORN50
     CANN51
     CAN 35
     CORN50
     CANN51
     CAN 55TPERD         IFEQ      'I'
     C                   EXCEPT    LH35OA
     C                   ELSE
     C     TPERD         IFEQ      'F'
     C                   EXCEPT    LH35OB
     C                   END
     C                   END
     C* -----------------------------------------------------
     C*
     C     NCOP          IFNE      'ORIGINAL'
     C   01
     COR 21
     COR 31
     COR 05
     COR 06
     COR 26              DO
     C                   EXCEPT    LH36O
     C                   END
     C   36
     COR 57
     COR 59
     COR 25
     COR 35
     COR 55              DO
     C                   EXCEPT    LH36O
     C                   END
     C                   END
     C* ----------------------------------------------------
     C* *                * * * * * * * *                   *
     C* ----------------------------------------------------
     C     L40           TAG
     C   41DUR           COMP      2                                      49
     C                   EXCEPT    LH44
     C*
     C   15              DO
     C                   Z-ADD     1             IND
     C     06            LOOKUP    WCL(IND)                               16
     C   16              EXCEPT    LH45
     C   16              Z-ADD     10            T                 2 0
     C   16T             DOWLT     15
     C                   ADD       1             T
     C                   EXCEPT    LH45T
     C                   END
     C                   Z-ADD     1             IND
     C     07            LOOKUP    WCL(IND)                               16
     C   16              EXCEPT    LH45
     C   16              Z-ADD     15            T                 2 0
     C   16T             DOWLT     24
     C                   ADD       1             T
     C                   EXCEPT    LH45T
     C                   END
     C                   Z-ADD     1             IND
     C     08            LOOKUP    WCL(IND)                               16
     C   16              EXCEPT    LH45
     C   16              Z-ADD     24            T                 2 0
     C   16T             DOWLT     33
     C                   ADD       1             T
     C                   EXCEPT    LH45T
     C                   END
     C                   END
     C*
     C     TALOES        TAG
     C     CTCOP         COMP      2                                  49  49
     C   49              GOTO      FEMISS
     C*
     C   80              DO
     C                   SETOFF                                       80
     C*
     C* alteração em 31.7.06, ver memo de 20.7.06 de Ana Paula Craveiro
     C     JMM1          IFEQ      817
     C     APOL          IFEQ      9908553
     C                   MOVE      *BLANK        JMM1
     C                   END
     C     APOL          IFEQ      9983107
     C                   MOVE      *BLANK        JMM1
     C                   END
     C                   END
     C* alteração em 31.7.06, ver memo de 20.7.06 de Ana Paula Craveiro
     C*                         -1-
     C                   EXCEPT    LH0313
     C                   EXCEPT    LH14
     C                   EXCEPT    L5013A
     C                   EXCEPT    L5015
     C     MARC2         COMP      ' '                                    49
     C  N49MATR2         COMP      ' '                                    49
     C  N49              EXCEPT    L5017
     C                   MOVE      *BLANKS       BLKS50           50
     C                   MOVE      BLKS50        WOUT20
     C                   EXCEPT    LH21
     C   81              EXCEPT    LH50OC
     C   81              SETOFF                                       81
     C   82              EXCEPT    LHAV
     C   82              SETOFF                                       82
     C   83              EXCEPT    LHPJ
     C   83              SETOFF                                       83
     C   84              EXCEPT    SOCRAT
     C   84              SETOFF                                       8485
     C                   END
     C*
     C     FEMISS        TAG
     C     CTCOP         COMP      CE                                     49
     C  N49              GOTO      RTEMIS
     C                   GOTO      LER
     C*
     C     FIM           TAG
     C                   Z-ADD     0             CTACER            1 0
     C                   Z-ADD     0             CTEMIS            1 0
     C                   Z-ADD     0             CTCOP             1 0
     C                   Z-ADD     0             IND               2 0
     C                   Z-ADD     0             CR                1 0
     C                   Z-ADD     0             WCHTEX            4 0
     C                   Z-ADD     0             WMALT             3 0
     C                   SETON                                        LR
     C* ----------------------------------------------------
     C* -           TRATAMENTO DO RAMO INCENDIO            -
     C* ----------------------------------------------------
     C     TRAT01        BEGSR
     C                   MOVE      *BLANKS       WOUT13           77
     C                   MOVE      TXT1(2)       WOUT13
     C                   EXCEPT    LH14
     C                   MOVE      CR1           CR                1 0
     C                   EXSR      TCR01
     C  N15              MOVE      TX1           TX                5 4
     C   15              MOVE      TX1AT         TXAT              5 3
     C                   MOVE      CAP1          CAP               9 0
     C                   EXCEPT    LHCR01
     C     CR2           COMP      0                                      49
     C   49              GOTO      LR01
     C                   MOVE      CR2           CR                1 0
     C                   EXSR      TCR01
     C                   MOVE      TX2           TX
     C                   MOVE      CAP2          CAP               9 0
     C                   EXCEPT    LHCR01
     C     CR3           COMP      0                                      49
     C   49              GOTO      LR01
     C                   MOVE      CR3           CR                1 0
     C                   EXSR      TCR01
     C                   MOVE      TX3           TX
     C                   MOVE      CAP3          CAP               9 0
     C                   EXCEPT    LHCR01
     C*
     C     LR01          TAG
     C                   EXCEPT    LH19
     C                   MOVE      *BLANKS       WOUT20           77
     C                   MOVE      TXT2(1)       WOUT20
     C                   EXCEPT    LH21
     C                   EXCEPT    L0122
     C                   EXSR      SRCL
     C                   ENDSR
     C* ----------------------------------------------------
     C* -   TRATAMENTO DO RAMO ACIDENTES PESSOAIS          -
     C* ----------------------------------------------------
     C     TRAT02        BEGSR
     C                   MOVE      *BLANKS       WOUT13
     C                   MOVE      TXT1(3)       WOUT13
     C                   EXCEPT    LH14
     C     CR1           COMP      9                                      49
     C   49              GOTO      L19
     C                   MOVEL     00            CHTEXT            6 0
     C                   MOVEL     PROF          WCHTEX            4 0
     C                   MOVE      1             WCHTEX            4 0
     C                   MOVE      WCHTEX        CHTEXT            6 0
     C                   EXSR      LETEXT
     C   17              MOVEL     'INVALIDO'    NPROF            30
     C  N17              MOVEL     DESC          NPROF            30
     C                   MOVE      NASC          WNASC             4 0
     C     NASC          IFLT      10
     C                   MOVEL     20            WNASC
     C                   ELSE
     C                   MOVEL     19            WNASC
     C                   END
     C                   EXCEPT    L0216
     C*
     C     L19           TAG
     C     RR            IFNE      02
     C                   EXCEPT    L0220
     C                   END
     C                   MOVE      *BLANKS       WOUT20
     C                   MOVE      TXT2(2)       WOUT20
     C                   EXCEPT    LH21
     C                   MOVE      CR1           CR                1 0
     C                   EXSR      TCR02
     C                   MOVE      CAP1          CAP               9 0
     C                   EXCEPT    LHCR
     C     CR2           COMP      0                                      49
     C   49              GOTO      RC02
     C                   MOVE      CR2           CR                1 0
     C                   EXSR      TCR02
     C                   MOVE      CAP2          CAP               9 0
     C                   EXCEPT    LHCR
     C     CR3           COMP      0                                      49
     C   49              GOTO      RC02
     C                   MOVE      CR3           CR                1 0
     C                   EXSR      TCR02
     C                   MOVE      CAP3          CAP               9 0
     C                   EXCEPT    LHCR
      *
     C     RC02          TAG
     C                   EXSR      SRCL
     C                   ENDSR
     C* ----------------------------------------------------
     C* - TRATAMENTO DO RAMO RESP.CIVIL PESSOAL - 03       -
     C* ----------------------------------------------------
     C     TRAT03        BEGSR
     C                   MOVE      *BLANKS       WOUT13
     C                   MOVE      TXT1(4)       WOUT13
     C                   EXCEPT    LH14
     C                   MOVE      CAP1          CAP               9 0
     C                   EXCEPT    L0315
     C                   MOVE      *BLANKS       WOUT20
     C                   MOVE      TXT2(1)       WOUT20
     C                   EXCEPT    LH21
     C                   EXCEPT    LH22
     C                   EXSR      SRCL
     C                   ENDSR
     C* ----------------------------------------------------
     C* - TRATAMENTO DO RAMO R.CIVIL-ACT.COM.IND. - 04     -
     C* ----------------------------------------------------
     C     TRAT04        BEGSR
     C                   MOVE      *BLANKS       WOUT13
     C                   MOVE      TXT1(6)       WOUT13
     C                   EXCEPT    LH14
     C                   EXCEPT    L0415
     C                   MOVE      *BLANKS       WOUT20
     C                   MOVE      TXT2(3)       WOUT20
     C                   EXCEPT    LH21
     C                   MOVE      CR1           CR                1 0
     C                   EXSR      TCR04
     C                   MOVE      CAP1          CAP               9 0
     C                   EXCEPT    LHCR
     C     CR2           COMP      0                                      49
     C   49              GOTO      RC04
     C                   MOVE      CR2           CR                1 0
     C                   EXSR      TCR04
     C                   MOVE      CAP2          CAP               9 0
     C                   EXCEPT    LHCR
     C                   EXCEPT    SALTO1
     C*
     C     RC04          TAG
     C                   EXSR      SRCL
     C                   ENDSR
     C* ----------------------------------------------------
     C* - TRATAMENTO DO RAMO RISCOS MULTIPLOS - 05         -
     C* ----------------------------------------------------
+----C     TRAT05        BEGSR
|    C* Posicionar-se na zona de escrita
|    C                   EVAL      WDetail = *BLANKS
|    C                   EXCEPT    LDETBRK05
|    C
|    C* Formar os parametros
|+---C                   MONITOR
||   C                   EVAL      WPtrHistorico = %ADDR(RGEXAP)
||   C                   EVAL      WPtrDiario    = %ADDR(RBASEDIO)
     C                   EVAL      WCdGrupo      = *ZEROS
||   C                   CALL      'RPRDO008'
||   C                   PARM                    WPtrHistorico
||   C                   PARM                    WPtrDiario
||   C                   PARM                    #RetTexto
||   C                   PARM                    WCdGrupo
||   C
||   C* Tratar o Texto e escrever na Acta
||   C                   EVAL      WIdxTxt = 1
||+--C                   DOW       (NOT #RetTexto(WIdxTxt).IsEmpty)
|||+-C                   IF        (CUR_LINE = 82)
|||| C                   EVAL      WDetail = $AlignText('.../...':90:'R')
|||| C                   EXCEPT    LDETLIN05
|||| C                   EXCEPT    BRKPAGE
|||+-C                   ENDIF
|||  C                   EVAL      WDetail = #RetTexto(WIdxTxt).Texto
|||  C                   EXCEPT    LDETLIN05
|||  C                   EVAL      WIdxTxt += 1
||+--C                   ENDDO
||   C                   ON-ERROR
|+---C                   ENDMON
+----C                   ENDSR
     C* ----------------------------------------------------
     C* - TRATAMENTO DO RAMO PERDA DE LUCROS - 06          -
     C* ----------------------------------------------------
     C     TRAT06        BEGSR
     C                   MOVE      *BLANKS       WOUT13
     C                   MOVE      TXT1(6)       WOUT13
     C                   EXCEPT    LH14
     C                   EXCEPT    L0415
     C                   EXCEPT    L0620
     C                   MOVE      *BLANKS       WOUT20
     C                   MOVE      TXT2(2)       WOUT20
     C                   EXCEPT    LH21
     C                   MOVE      CR1           CR                1 0
     C                   EXSR      TCR06
     C                   MOVE      CAP1          CAP               9 0
     C                   EXCEPT    LHCR
     C     CR2           COMP      0                                      49
     C   49              GOTO      RC06
     C                   MOVE      CR2           CR                1 0
     C                   EXSR      TCR06
     C                   MOVE      CAP2          CAP               9 0
     C                   EXCEPT    LHCR
     C     CR3           COMP      0                                      49
     C   49              GOTO      RC06
     C                   MOVE      CR3           CR                1 0
     C                   EXSR      TCR06
     C                   MOVE      CAP3          CAP               9 0
     C                   EXCEPT    LHCR
     C*
     C     RC06          TAG
     C                   EXSR      SRCL
     C                   ENDSR
     C* ----------------------------------------------------
     C* - TRATAMENTO DO RAMO DINH. EM TRANSITO - 07        -
     C* ----------------------------------------------------
     C     TRAT07        BEGSR
     C                   MOVE      *BLANKS       WOUT13
     C                   MOVE      TXT1(5)       WOUT13
     C                   EXCEPT    LH14
     C                   MOVE      CR1           CR                1 0
     C                   EXSR      TCR07
     C                   MOVE      CAP1          CAP               9 0
     C                   EXCEPT    LHCR
     C     CR2           COMP      0                                      49
     C   49              GOTO      L0720
     C                   MOVE      CR2           CR                1 0
     C                   EXSR      TCR07
     C                   MOVE      CAP2          CAP               9 0
     C                   EXCEPT    LHCR
     C*
     C     L0720         TAG
     C                   MOVE      *BLANKS       WOUT20
     C                   MOVE      TXT2(1)       WOUT20
     C                   EXCEPT    LH21
     C                   EXCEPT    LH22
     C                   ENDSR
     C* ----------------------------------------------------
     C* - TRATAMENTO DO RAMO ROUBO RESIDENCIAS - 08        -
     C* ----------------------------------------------------
     C     TRAT08        BEGSR
     C                   MOVE      *BLANKS       WOUT13
     C                   MOVE      TXT1(1)       WOUT13
     C                   EXCEPT    LH14
     C                   MOVE      CR1           CR                1 0
     C                   EXSR      TCR08
     C                   MOVE      CAP1          CAP               9 0
     C                   EXCEPT    LHCR
     C     CR2           COMP      0                                      49
     C   49              GOTO      LR08
     C                   MOVE      CR2           CR                1 0
     C                   EXSR      TCR08
     C                   MOVE      CAP2          CAP               9 0
     C                   EXCEPT    LHCR
     C*
     C     LR08          TAG
     C                   EXCEPT    LH19
     C                   MOVE      *BLANKS       WOUT20
     C                   MOVE      TXT2(1)       WOUT20
     C                   EXCEPT    LH21
     C                   EXCEPT    LH22
     C                   ENDSR
     C* ----------------------------------------------------
     C* - TRATAMENTO DO RAMO ROUBO ESTABELECIMENTOS - 09   -
     C* ----------------------------------------------------
     C     TRAT09        BEGSR
     C                   MOVE      *BLANKS       WOUT13
     C                   MOVE      TXT1(1)       WOUT13
     C                   EXCEPT    LH14
     C                   MOVE      CR1           CR                1 0
     C                   EXSR      TCR09
     C                   MOVE      CAP1          CAP               9 0
     C                   EXCEPT    LHCR
     C     CR2           COMP      0                                      49
     C   49              GOTO      LR09
     C                   MOVE      CR2           CR                1 0
     C                   EXSR      TCR09
     C                   MOVE      CAP2          CAP               9 0
     C                   EXCEPT    LHCR
     C*
     C     LR09          TAG
     C                   EXCEPT    LH19
     C                   MOVE      *BLANKS       WOUT20
     C                   MOVE      TXT2(1)       WOUT20
     C                   EXCEPT    LH21
     C                   EXCEPT    LH22
     C                   ENDSR
     C* -------------------------------
     C* TRATAMENTO DO RAMO VIAGENS - 10
     C* -------------------------------
     C     TRAT10        BEGSR
     C                   MOVE      *BLANKS       WOUT13
     C                   MOVE      TXT1(1)       WOUT13
     C                   EXCEPT    LH14
     C                   MOVE      CR1           CR                1 0
     C                   EXSR      TCR10
     C                   MOVE      CAP1          CAP               9 0
     C                   EXCEPT    LHCR
     C     CR2           COMP      0                                      49
     C   49              GOTO      LR10
     C                   MOVE      CR2           CR                1 0
     C                   EXSR      TCR10
     C                   MOVE      CAP2          CAP               9 0
     C                   EXCEPT    LHCR
     C*
     C     LR10          TAG
     C                   EXCEPT    LH19
     C                   MOVE      *BLANKS       WOUT20
     C                   MOVE      TXT2(1)       WOUT20
     C                   EXCEPT    LH21
     C                   EXCEPT    LH22
     C                   ENDSR
     C* ----------------------------------------------------
     C* -   TRATAMENTO DO RAMO ACIDENTES DE TRABALHO - 15  -
     C* ----------------------------------------------------
     C     TRAT15        BEGSR
     C                   MOVE      *BLANKS       WOUT13
     C                   MOVE      TXT1(3)       WOUT13
     C                   EXCEPT    LH14
     C                   MOVEL     00            CHTEXT            6 0
     C                   MOVEL     PROF          WCHTEX            4 0
     C                   MOVE      1             WCHTEX            4 0
     C                   MOVE      WCHTEX        CHTEXT            6 0
     C                   EXSR      LETEXT
     C   17              MOVEL     'INVALIDA'    NPROF            30
     C   17
     CAN 15              MOVEL     'INVALIDO'    WPROF            40
     C  N17              MOVEL     DESC          NPROF            30
     C  N17
     CAN 15              MOVEL     DESC          WPROF            40
     C                   EXCEPT    L0216
     C*
     C                   MOVE      *BLANKS       KCAE             12
     C                   MOVEL     20            WKCAE             8
     C                   MOVE      CDCAE         WKCAE
     C                   MOVEL     WKCAE         KCAE
     C     KCAE          CHAIN     TABELAS                            40
     C   40              MOVEL     *BLANKS       DCAE
     C   40              MOVEL     'INVÁLIDA'    DCAE
     C*
     C     L1519         TAG
     C     RR            IFNE      02
     C                   EXCEPT    L0220
     C                   END
     C                   MOVE      *BLANKS       WOUT20
     C                   MOVE      TXT2(7)       WOUT20
     C                   EXCEPT    LH21
     C                   MOVE      CR1           CR                1 0
     C                   EXSR      TCR15
     C                   MOVE      CAP1          CAP               9 0
     C                   Z-ADD     0             TXCOM             6 3
     C                   Z-ADD     TX1AT         TXCOM
     C                   EXCEPT    LHCR
     C                   MOVE      *BLANKS       WOUT20
     C                   EXCEPT    LHCR15
     C                   MOVE      TXT2(8)       WOUT20
     C                   EXCEPT    LHCR15
     C                   MOVE      *BLANKS       WOUT20
     C                   MOVE      TXT2(9)       WOUT20
     C                   EXCEPT    LHCR15
     C*
     C                   MOVE      *BLANKS       WOUT20
     C                   EXCEPT    LHCR15
     C     TPT           IFEQ      'P'
     C                   MOVE      TXT2(10)      WOUT20
     C                   ELSE
     C     TPT           IFEQ      'O'
     C                   MOVE      TXT2(11)      WOUT20
     C                   ELSE
     C                   MOVE      *BLANKS       WOUT20
     C                   END
     C                   END
     C                   EXCEPT    LHCR15
     C                   EXSR      SRCL
     C*
     C     CR2           COMP      0                                      49
     C   49              GOTO      END15
     C     CR3           COMP      0                                      49
     C   49              GOTO      END15
     C     END15         ENDSR
     C* ----------------------------------------------------
     C* - TRATAMENTO DO RAMO EQUIPAMENTOS ELECTRONICOS-17  -
     C* ----------------------------------------------------
     C     TRAT17        BEGSR
     C                   MOVE      *BLANKS       WOUT13
     C                   MOVE      TXT1(2)       WOUT13
     C                   EXCEPT    LH14
     C                   MOVE      CR1           CR                1 0
     C                   EXSR      TCR17
     C  N15              MOVE      TX1           TX
     C   15              MOVE      TX1AT         TXAT
     C                   MOVE      CAP1          CAP               9 0
     C                   EXCEPT    LHCR01
     C     CR2           COMP      0                                      49
     C   49              GOTO      LR17
     C                   MOVE      CR2           CR                1 0
     C                   EXSR      TCR17
     C                   MOVE      TX2           TX                5 4
     C                   MOVE      CAP2          CAP               9 0
     C                   EXCEPT    LHCR01
     C     CR3           COMP      0                                      49
     C   49              GOTO      LR17
     C                   MOVE      CR3           CR                1 0
     C                   EXSR      TCR17
     C                   MOVE      TX3           TX                5 4
     C                   MOVE      CAP3          CAP               9 0
     C                   EXCEPT    LHCR01
     C*
     C     LR17          TAG
     C                   EXCEPT    LH19
     C* GARANTIAS
     C                   MOVE      *BLANKS       WOUT20
     C                   MOVE      TXT2(1)       WOUT20
     C                   EXCEPT    LH21
     C                   EXCEPT    LH22
     C                   EXSR      SRCL
     C                   ENDSR
     C* ----------------------------------------------------
     C* - TRATAMENTO DO RAMO AVARIA DE MAQUINAS - 18       -
     C* ----------------------------------------------------
     C     TRAT18        BEGSR
     C                   MOVE      *BLANKS       WOUT13
     C                   MOVE      TXT1(2)       WOUT13
     C                   EXCEPT    LH14
     C                   MOVE      CR1           CR                1 0
     C                   EXSR      TCR18
     C  N15              MOVE      TX1           TX
     C   15              MOVE      TX1AT         TXAT
     C                   MOVE      CAP1          CAP               9 0
     C                   EXCEPT    LHCR01
     C     CR2           COMP      0                                      49
     C   49              GOTO      LR18
     C                   MOVE      CR2           CR                1 0
     C                   EXSR      TCR18
     C                   MOVE      TX2           TX                5 4
     C                   MOVE      CAP2          CAP               9 0
     C                   EXCEPT    LHCR01
     C     CR3           COMP      0                                      49
     C   49              GOTO      LR18
     C                   MOVE      CR3           CR                1 0
     C                   EXSR      TCR18
     C                   MOVE      TX3           TX                5 4
     C                   MOVE      CAP3          CAP               9 0
     C                   EXCEPT    LHCR01
     C*
     C     LR18          TAG
     C                   EXCEPT    LH19
     C* GARANTIAS
     C                   MOVE      *BLANKS       WOUT20
     C                   MOVE      TXT2(1)       WOUT20
     C                   EXCEPT    LH21
     C                   EXCEPT    LH22
     C                   ENDSR
     C* ----------------------------------------------------
     C* - TRATAMENTO DO RAMO OBRAS E MONTAGENS - 19        -
     C* ----------------------------------------------------
     C     TRAT19        BEGSR
     C                   MOVE      *BLANKS       WOUT13
     C                   MOVE      TXT1(2)       WOUT13
     C                   EXCEPT    LH14
     C                   MOVE      CR1           CR                1 0
     C                   EXSR      TCR19
     C  N15              MOVE      TX1           TX
     C   15              MOVE      TX1AT         TXAT
     C                   MOVE      CAP1          CAP               9 0
     C                   EXCEPT    LHCR01
     C     CR2           COMP      0                                      49
     C   49              GOTO      LR19
     C                   MOVE      CR2           CR                1 0
     C                   EXSR      TCR19
     C                   MOVE      TX2           TX                5 4
     C                   MOVE      CAP2          CAP               9 0
     C                   EXCEPT    LHCR01
     C     CR3           COMP      0                                      49
     C   49              GOTO      LR19
     C                   MOVE      CR3           CR                1 0
     C                   EXSR      TCR19
     C                   MOVE      TX3           TX                5 4
     C                   MOVE      CAP3          CAP               9 0
     C                   EXCEPT    LHCR01
     C*
     C     LR19          TAG
     C                   EXCEPT    LH19
     C* GARANTIAS
     C                   MOVE      *BLANKS       WOUT20
     C                   MOVE      TXT2(1)       WOUT20
     C                   EXCEPT    LH21
     C                   EXCEPT    LH22
     C                   EXSR      SRCL
     C     END19         ENDSR
     C* ----------------------------------------------------
     C* - TRATAMENTO DO RAMO AUTOMOVEL - 50                -
     C* ----------------------------------------------------
     C     TRAT50        BEGSR
     C                   SETOFF                                       808182
     C                   SETOFF                                       838485
     C*
[LG] C     CATVTAG       TAG
     C                   Z-ADD     1             IND               2 0
     C     CATV          LOOKUP    TCATV(IND)                             16
     C   16              MOVEL     NCATV(IND)    TVEIC            31
     C                   MOVE      *BLANKS       WOUT13
     C                   MOVE      TXT1(6)       WOUT13
     C                   EXCEPT    LH14
     C*
[LG] C*    CILIND        TAG
     C     CIL           COMP      0                                      49
[LG] C   49              GOTO      PBRUTT
     C                   Z-ADD     1             IND               2 0
     C     CIL           LOOKUP    TCIL(IND)                              16
     C   16              MOVE      VCIL(IND)     CILIND            4 0
     C     CIL           COMP      5                                    19
     C   19              MOVE      'ATÉ '        LIT1              4
     C  N19              MOVE      'ALÉM'        LIT1
     C                   SETOFF                                       5253
     C     WCATV         IFEQ      640
     C                   SETON                                        53
     C                   MOVE      'ALÉM'        LIT1
     C                   Z-ADD     1500          CILIND
     C                   ELSE
     C     WCATV         IFEQ      740
     C                   SETON                                        52
     C                   MOVE      'ALÉM'        LIT1
     C                   Z-ADD     1500          CILIND
     C                   ENDIF
     C                   ENDIF
     C                   EXCEPT    L5013A
     C*
[LG] C     PBRUTT        TAG
     C     PB            COMP      0                                      49
     C   49              GOTO      TLH14
     C                   Z-ADD     1             IND               2 0
     C     PB            LOOKUP    TPB(IND)                               16
     C   16              MOVE      VPB(IND)      PBRUT             5 0
     C     PB            COMP      5                                    19
     C   19              MOVE      ' ATÉ'        LIT1              4
     C  N19              MOVE      'ALÉM'        LIT1              4
     C                   EXCEPT    L5013B
     C*
     C     TLH14         TAG
     C                   MOVE      AC            WAC               4 0
     C     AC            IFLE      20
     C                   MOVEL     20            WAC
     C                   ELSE
     C                   MOVEL     19            WAC
     C                   END
     C                   EXCEPT    L5015
     C*
     C     VEICII        TAG
     C     MARC2         COMP      ' '                                    49
     C   49MATR2         COMP      ' '                                    49
     C  N49              EXCEPT    L5017
     C   50              EXCEPT    L5019
     C*
     C     L5020         TAG
     C     CR2           COMP      0                                      19
     C                   MOVE      *BLANKS       WOUT20
     C   19              MOVE      TXT2(4)       WOUT20
     C   19              EXCEPT    LH21
     C  N19              MOVE      TXT2(5)       WOUT20
     C  N19              EXCEPT    LH21
     C*
     C     LHRC          TAG
     C                   SETOFF                                       86
     C                   Z-ADD     CAP1          CAPRC
     C                   DO        15            X                 2 0
     C     CAPRC         LOOKUP    RC1(X)                                 69
     C   69              MOVEL     RC2(X)        DCRCA            57
     C   69              EXCEPT    LHRCA
     C  N69              GOTO      FORA
     C                   END
     C     FORA          TAG
      *
     C   19              DO
     C                   MOVE      DDINIC        WDD
     C                   MOVE      MMINIC        WMM
     C                   MOVE      AAINIC        WAA
     C   41AAMMDD        IFGE      030501
     C                   SETON                                        86
     C                   END
     C                   MOVE      DDALT         WDD
     C                   MOVE      MMALT         WMM
     C                   MOVE      AAALT         WAA
     C  N41AAMMDD        IFGE      030501
     C                   SETON                                        86
     C                   END
     C                   END
      *
     C                   SETOFF                                       87
     C                   MOVE      DDINIC        WDD
     C                   MOVE      MMINIC        WMM
     C                   MOVE      AAINIC        WAA
     C   41AAMMDD        IFGE      110201
     C                   SETON                                        87
     C                   END
     C*
     C******     LHDP      TAG
     C     CR2           COMP      0                                      49
     C   49              DO
     C     QIV           IFEQ      'S'
     C                   EXCEPT    LHQIV
     C                   END
     C     CR3           IFGT      *ZERO
     C                   MOVEL     TXT1(8)       WOUT51           59
     C                   Z-ADD     1             IND
     C     CR3           LOOKUP    TC51(IND)                              16
     C   16              MOVE      NTC51(IND)    WOUT51
     C                   Z-ADD     CAP3          CAPMIP            5 0
     C     CAPMIP        MULT      .10           CAPDME            5 0
     C                   EXCEPT    LH50OC
     C                   END
     C                   GOTO      END50
     C                   END
     C*
     C     CAPDPI        ADD       VLEXTR        CAPDP             6 0
     C     CAPDPI        IFEQ      *ZEROS
     C     CAP2          ADD       VLEXTR        CAPDP
     C                   END
     C     CR2           COMP      3                                      49
     C  N49              GOTO      CR4
     C*
     C                   EXCEPT    LHCCC
     C                   EXCEPT    LHIRE
     C                   EXCEPT    LHFR
     C     CR3           IFGT      *ZERO
     C                   MOVEL     TXT1(8)       WOUT51
     C                   Z-ADD     1             IND
     C     CR3           LOOKUP    TC51(IND)                              16
     C   16              MOVE      NTC51(IND)    WOUT51
     C                   Z-ADD     CAP3          CAPMIP            5 0
     C     CAPMIP        MULT      .10           CAPDME            5 0
     C                   EXCEPT    LH50OC
     C                   END
     C                   GOTO      END50
     C*
     C     CR4           TAG
     C     CR2           COMP      4                                      49
     C  N49              GOTO      CR5
     C                   EXCEPT    LHCCC
     C                   EXCEPT    LHIRE
     C     QIV           IFEQ      'S'
     C                   EXCEPT    LHQIV
     C                   END
     C     CR3           IFGT      *ZERO
     C                   MOVEL     TXT1(8)       WOUT51
     C                   Z-ADD     1             IND
     C     CR3           LOOKUP    TC51(IND)                              16
     C   16              MOVE      NTC51(IND)    WOUT51
     C                   Z-ADD     CAP3          CAPMIP            5 0
     C     CAPMIP        MULT      .10           CAPDME            5 0
     C                   EXCEPT    LH50OC
     C                   END
     C                   GOTO      END50
     C*
     C     CR5           TAG
     C     CR2           COMP      5                                      49
     C  N49              GOTO      CR6
     C                   EXCEPT    LHIRE
     C     QIV           IFEQ      'S'
     C                   EXCEPT    LHQIV
     C                   END
     C     CR3           IFGT      *ZERO
     C                   MOVEL     TXT1(8)       WOUT51
     C                   Z-ADD     1             IND
     C     CR3           LOOKUP    TC51(IND)                              16
     C   16              MOVE      NTC51(IND)    WOUT51
     C                   Z-ADD     CAP3          CAPMIP            5 0
     C     CAPMIP        MULT      .10           CAPDME            5 0
     C                   EXCEPT    LH50OC
     C                   END
     C                   GOTO      END50
     C*
     C     CR6           TAG
     C     CR2           COMP      6                                      49
     C  N49              GOTO      CR7
     C                   EXCEPT    LHFR
     C     QIV           IFEQ      'S'
     C                   EXCEPT    LHQIV
     C                   END
     C     CR3           IFGT      *ZERO
     C                   MOVEL     TXT1(8)       WOUT51
     C                   Z-ADD     1             IND
     C     CR3           LOOKUP    TC51(IND)                              16
     C   16              MOVE      NTC51(IND)    WOUT51
     C                   Z-ADD     CAP3          CAPMIP            5 0
     C     CAPMIP        MULT      .10           CAPDME            5 0
     C                   EXCEPT    LH50OC
     C                   END
     C                   GOTO      END50
     C*
     C     CR7           TAG
     C     CR2           COMP      7                                      49
     C  N49              GOTO      CR8
     C                   EXCEPT    LHFR
     C                   EXCEPT    LHIRE
     C     QIV           IFEQ      'S'
     C                   EXCEPT    LHQIV
     C                   END
     C     CR3           IFGT      *ZERO
     C                   MOVEL     TXT1(8)       WOUT51
     C                   Z-ADD     1             IND
     C     CR3           LOOKUP    TC51(IND)                              16
     C   16              MOVE      NTC51(IND)    WOUT51
     C                   Z-ADD     CAP3          CAPMIP            5 0
     C     CAPMIP        MULT      .10           CAPDME            5 0
     C                   EXCEPT    LH50OC
     C                   END
     C                   GOTO      END50
     C*
     C     CR8           TAG
     C                   EXCEPT    LHCCC
     C                   EXCEPT    LHIRE
     C                   EXCEPT    LHFR
     C                   EXCEPT    LHCSP
     C*
     C     CR3           IFGT      *ZERO
     C                   MOVEL     TXT1(8)       WOUT51
     C                   Z-ADD     1             IND
     C     CR3           LOOKUP    TC51(IND)                              16
     C   16              MOVE      NTC51(IND)    WOUT51
     C                   Z-ADD     CAP3          CAPMIP            5 0
     C     CAPMIP        MULT      .10           CAPDME            5 0
     C                   END
     C     CR3           IFGT      *ZERO
     C                   EXCEPT    LH50OC
     C                   END
     C*
     C     END50         TAG
     C*
      *
     C     PRIV          IFNE      ' '
     C     VSA           ORNE      ' '
     C     VIA           ORNE      ' '
     C                   EXSR      SRPU
     C                   END
      *
     C     VIAG          IFNE      ' '
     C                   EXSR      SRAV
     C                   EXCEPT    LHAV
     C                   END
     C*
     C     PJUR          IFEQ      'S'
     C                   EXCEPT    LHPJ
     C                   END
     C*
     C     QIV           IFEQ      'S'
     C                   EXCEPT    LHQIV2
     C                   ELSE
     C     CR2           COMP      3                                      14
     C  N14CR2           COMP      4                                      14
     C  N14CR2           COMP      8                                      14
     C   14              EXCEPT    LHQIV2
     C                   END
     C*
     C     CR2           IFGT      *ZERO
     C                   SETON                                        84
     C     JMM1          IFEQ      272
     C                   SETON                                        85
     C                   END
     C     CODFRQ        IFEQ      '09'
     C                   EXCEPT    LHFRQ
     C                   END
     C                   EXCEPT    SOCRAT
     C                   EXCEPT    LHDP
     C                   END
     C*
     C                   MOVE      *BLANK        IDCOM1            5
     C                   MOVE      *BLANK        IDCOM2            5
     C                   MOVE      *ZEROS        WCOM1X            7 2
     C                   MOVE      *ZEROS        WCOM2X            7 2
     C*
     C     JMAVC1        IFNE      *ZEROS
     C                   MOVEL     'AV'          IDCOM1
     C                   ADD       JMAVC1        WCOM1X
     C     JMPJC1        IFNE      *ZEROS
     C                   MOVE      '+PJ'         IDCOM1
     C                   ADD       JMPJC1        WCOM1X
     C                   END
     C                   ELSE
     C     JMPJC1        IFNE      *ZEROS
     C                   MOVEL     'PJ'          IDCOM1
     C                   ADD       JMPJC1        WCOM1X
     C                   END
     C                   END
     C     IDCOM1        IFNE      *BLANK
     C                   GOTO      FUNGA
     C                   END
     C*
     C     JMAVC2        IFNE      *ZEROS
     C                   MOVEL     'AV'          IDCOM2
     C                   ADD       JMAVC2        WCOM2X
     C     JMPJC2        IFNE      *ZEROS
     C                   MOVE      '+PJ'         IDCOM2
     C                   ADD       JMPJC2        WCOM2X
     C                   END
     C                   ELSE
     C     JMPJC2        IFNE      *ZEROS
     C                   MOVEL     'PJ'          IDCOM2
     C                   ADD       JMPJC2        WCOM2X
     C                   END
     C                   END
     C     IDCOM2        IFNE      *BLANK
     C                   GOTO      FUNGA
     C                   END
     C*
     C     AVCOM1        IFNE      *ZEROS
     C                   MOVEL     'AV'          IDCOM1
     C                   ADD       AVCOM1        WCOM1X
     C     PJCOM1        IFNE      *ZEROS
     C                   MOVE      '+PJ'         IDCOM1
     C                   ADD       PJCOM1        WCOM1X
     C                   END
     C                   ELSE
     C     PJCOM1        IFNE      *ZEROS
     C                   MOVEL     'PJ'          IDCOM1
     C                   ADD       PJCOM1        WCOM1X
     C                   END
     C                   END
     C*
     C     FUNGA         TAG
     C                   MOVE      *BLANK        IDFGA             5
     C                   MOVE      *ZEROS        WFGA              7 2
     C     AVFGA         IFNE      *ZEROS
     C                   MOVEL     'AV'          IDFGA
     C                   ADD       AVFGA         WFGA
     C     PJFGA         IFNE      *ZEROS
     C                   MOVE      '+PJ'         IDFGA
     C                   ADD       PJFGA         WFGA
     C                   END
     C                   ELSE
     C     PJFGA         IFNE      *ZEROS
     C                   MOVEL     'PJ'          IDFGA
     C                   ADD       PJFGA         WFGA
     C                   END
     C                   END
     C                   ENDSR
     C* ----------------------------------------------------
     C* - TRATAMENTO DO RAMO OCUPANTES - 51                -
     C* ----------------------------------------------------
     C     TRAT51        BEGSR
     C                   Z-ADD     1             IND               2 0
     C     TC            LOOKUP    TC51(IND)                              16
     C   16              MOVEL     NTC51(IND)    TCONTR           24
     C                   MOVE      *BLANKS       WOUT13
     C                   MOVE      TXT1(7)       WOUT13
     C                   EXCEPT    LH14
     C                   EXCEPT    L5115
     C*
     C     VEIC2         TAG
     C     MARC2         COMP      ' '                                    49
     C   49MATR2         COMP      ' '                                    49
     C   49              GOTO      LH0220
     C                   EXCEPT    L5116
     C                   EXCEPT    L5117
     C*
     C     LH0220        TAG
     C                   MOVE      *BLANKS       WOUT20
     C                   MOVE      TXT2(2)       WOUT20
     C                   EXCEPT    LH21
     C                   MOVE      CAP1          CAPRC             5 0
     C                   MOVE      CAP2          CAPDP             6 0
     C                   EXCEPT    L51223
     C                   ENDSR
     C* ----------------------------------------------------
     C* - TRATAMENTO RAMO MULTIRISCOS ESTABLECIMENTOS - 55 -
     C* ----------------------------------------------------
     C     TRAT55        BEGSR
     C                   MOVE      *BLANKS       WOUT13
     C                   MOVE      TXT1(1)       WOUT13
     C                   EXCEPT    LH14
     C                   MOVE      CR1           CR                1 0
     C                   EXSR      TCR55
     C                   MOVE      CAP1          CAP               9 0
     C                   EXCEPT    LHCR
     C     CR2           COMP      0                                      49
     C   49              GOTO      LR55
     C                   MOVE      CR2           CR                1 0
     C                   EXSR      TCR55
     C                   MOVE      CAP2          CAP               9 0
     C                   EXCEPT    LHCR
     C     CR3           COMP      0                                      49
     C   49              GOTO      LR55
     C                   MOVE      CR3           CR                1 0
     C                   EXSR      TCR55
     C                   MOVE      CAP3          CAP               9 0
     C                   EXCEPT    LHCR
     C*
     C     LR55          TAG
     C                   EXCEPT    LH19
     C                   EXCEPT    LH20
     C                   MOVE      *BLANKS       WOUT20
+    C* se é novo produto, não escreve "LIMITE(S)/FRANQUIA(S)
+    C     NEWPRD        IFEQ      'S'
+    C                   MOVE      TXT2(1)       WOUT20
+    C                   ELSE
     C                   MOVE      TXT2(6)       WOUT20
+    C                   END
     C                   EXCEPT    LH21
     C* se é novo produto, não escreve "QUADRO ANEXO"
+    C*          NEWPRD    IFNE 'S'
+    C*                    EXCPTLH5522                       PC-31/10/91
+    C*                    END
     C*
     C                   MOVE      *BLANKS       WOUT21           77
     C     CR1           COMP      9                                      18
     C   18              MOVE      TXT7(3)       WOUT21
     C   18              EXCEPT    L5522
     C   18              GOTO      RC55
     C     CR1           COMP      1                                      18
+    C  N18CR1           COMP      2                                      18
+    C  N18CR2           COMP      2                                      18
+    C   18NEWPRD        IFEQ      'S'
+    C                   MOVE      TXT7(1)       WOUT21
+    C                   ELSE
+    C                   MOVE      TXT1(2)       WOUT21
+    C                   END
+    C   18              EXCEPT    L5522
     C*
     C     RC55          TAG
     C                   EXSR      SRCL
     C     CRHIP         IFGT      *ZEROS
     C                   MOVEL     20            K7                7 0
     C                   MOVE      CRHIP         K7
     C     K7            CHAIN     CRHIPOT                            88
+    C* escreve nome credor hipotecário quando não é produto novo
+[LG]C* N88NEWPRD        IFEQ      ' '
+[LG]C* N88              EXCEPT    LH31
+[LG]C* N88              END
+[LG]C                   IF        (NEWPRD = ' ') AND NOT *IN88
+[LG]C                   EXCEPT    LH31
+[LG]C                   END
     C*
     C                   END
     C     END55         ENDSR
     C* ----------------------------------------------------
     C* - TRATAMENTO DO RAMO CRISTAIS - 65                 -
     C* ----------------------------------------------------
     C     TRAT65        BEGSR
     C                   MOVE      *BLANKS       WOUT13
     C                   MOVE      TXT1(1)       WOUT13
     C                   EXCEPT    LH14
     C                   MOVE      CR1           CR                1 0
     C                   EXSR      TCR65
     C                   MOVE      CAP1          CAP               9 0
     C                   EXCEPT    LHCR
     C                   EXCEPT    LH19
     C                   MOVE      *BLANKS       WOUT20
     C                   MOVE      TXT2(1)       WOUT20
     C                   EXCEPT    LH21
     C                   EXCEPT    LH22
     C                   ENDSR
[LG] C
     C* ----------------------------------------------------
     C* - MOTIVO DA ALTERAÇÃO                              -
     C* ----------------------------------------------------
     C     MOTALT        BEGSR
     C                   MOVEL     00            CHTEXT            6 0
     C                   MOVEL     0             WMALT             3 0
     C                   MOVE      MALT          WMALT             3 0
     C                   MOVEL     WMALT         WCHTEX            4 0
     C                   MOVE      1             WCHTEX            4 0
     C                   MOVE      WCHTEX        CHTEXT            6 0
     C                   EXSR      LETEXT
     C   17              MOVEL     'INVALIDO'    MALTER           40
     C  N17              MOVEL     DESC          MALTER           40
     C                   EXCEPT    LH32
     C                   ENDSR
     C* ----------------------------------------------------
     C* - DO ANO DE CONSTRUÇÃO                             -
     C* ----------------------------------------------------
     C     DURAC         BEGSR
     C                   Z-ADD     1             IND               2 0
     C     DUR           LOOKUP    CODDUR(IND)                            16
     C   16              MOVEL     NOMDUR(IND)   TIPDUR           18
     C                   ENDSR
     C* ----------------------------------------------------
     C* - CHAIN AO FICHEIRO DE TEXTOS                      -
     C* ----------------------------------------------------
     C     LETEXT        BEGSR
     C     CHTEXT        CHAIN     FTEXTOS                            17
     C                   ENDSR
     C* ----------------------------------------------------
     C* - PESQUISA A TABELA DE CR - RAMO 01                -
     C* ----------------------------------------------------
     C     TCR01         BEGSR
     C                   Z-ADD     1             IND               2 0
     C     CR            LOOKUP    CR01(IND)                              16
     C   16              MOVEL     NCR01(IND)    NCR              43
     C                   ENDSR
     C* ----------------------------------------------------
     C* - PESQUISA A TABELA DE CR - RAMO 02                -
     C* ----------------------------------------------------
     C     TCR02         BEGSR
     C                   Z-ADD     1             IND               2 0
     C     CR            LOOKUP    CR02(IND)                              16
     C   16              MOVEL     NCR02(IND)    NCR
     C                   ENDSR
     C* ----------------------------------------------------
     C* - PESQUISA A TABELA DE CR - RAMO 04                -
     C* ----------------------------------------------------
     C     TCR04         BEGSR
     C                   Z-ADD     1             IND               2 0
     C     CR            LOOKUP    CR04(IND)                              16
     C   16              MOVEL     NCR04(IND)    NCR
     C                   ENDSR
     C* ----------------------------------------------------
     C* - PESQUISA A TABELA DE CR - RAMO 05                -
     C* ----------------------------------------------------
+----C     TCR05         BEGSR
|    C                   Z-ADD     1             IND               2 0
|    C     CR            LOOKUP    CR05(IND)                              16
|    C   16              MOVEL     NCR05(IND)    NCR
+----C                   ENDSR
     C*
     C* ----------------------------------------------------
     C* - PESQUISA A TABELA DE CR - RAMO 06                -
     C* ----------------------------------------------------
     C     TCR06         BEGSR
     C                   Z-ADD     1             IND               2 0
     C     CR            LOOKUP    CR06(IND)                              16
     C   16              MOVEL     NCR06(IND)    NCR
     C                   ENDSR
     C* ----------------------------------------------------
     C* - PESQUISA A TABELA DE CR - RAMO 07                -
     C* ----------------------------------------------------
     C     TCR07         BEGSR
     C                   Z-ADD     1             IND               2 0
     C     CR            LOOKUP    CR07(IND)                              16
     C   16              MOVEL     NCR07(IND)    NCR
     C                   ENDSR
     C* ----------------------------------------------------
     C* - PESQUISA A TABELA DE CR - RAMO 08                -
     C* ----------------------------------------------------
     C     TCR08         BEGSR
     C                   Z-ADD     1             IND               2 0
     C     CR            LOOKUP    CR08(IND)                              16
     C   16              MOVEL     NCR08(IND)    NCR
     C                   ENDSR
     C* ----------------------------------------------------
     C* - PESQUISA A TABELA DE CR - RAMO 09                -
     C* ----------------------------------------------------
     C     TCR09         BEGSR
     C                   Z-ADD     1             IND               2 0
     C     CR            LOOKUP    CR09(IND)                              16
     C   16              MOVEL     NCR09(IND)    NCR
     C                   ENDSR
     C* ----------------------------------------------------
     C* - PESQUISA A TABELA DE CR - RAMO 10                -
     C* ----------------------------------------------------
     C     TCR10         BEGSR
     C                   Z-ADD     1             IND               2 0
     C     CR            LOOKUP    CR10(IND)                              16
     C   16              MOVEL     NCR10(IND)    NCR
     C                   ENDSR
     C* ----------------------------------------------------
     C* - PESQUISA A TABELA DE CR - RAMO 15                -
     C* ----------------------------------------------------
     C     TCR15         BEGSR
     C                   Z-ADD     1             IND               2 0
     C     CR            LOOKUP    CR15(IND)                              16
     C   16              MOVEL     NCR15(IND)    NCR
     C                   ENDSR
     C* ----------------------------------------------------
     C* - PESQUISA A TABELA DE CR - RAMO 17                -
     C* ----------------------------------------------------
     C     TCR17         BEGSR
     C                   Z-ADD     1             IND               2 0
     C     CR            LOOKUP    CR17(IND)                              16
     C   16              MOVEL     NCR17(IND)    NCR
     C                   ENDSR
     C* ----------------------------------------------------
     C* - PESQUISA A TABELA DE CR - RAMO 18                -
     C* ----------------------------------------------------
     C     TCR18         BEGSR
     C                   Z-ADD     1             IND               2 0
     C     CR            LOOKUP    CR18(IND)                              16
     C   16              MOVEL     NCR18(IND)    NCR
     C                   ENDSR
     C* ----------------------------------------------------
     C* - PESQUISA A TABELA DE CR - RAMO 19                -
     C* ----------------------------------------------------
     C     TCR19         BEGSR
     C                   Z-ADD     1             IND               2 0
     C     CR            LOOKUP    CR19(IND)                              16
     C   16              MOVEL     NCR19(IND)    NCR
     C                   ENDSR
     C* ----------------------------------------------------
     C* - PESQUISA A TABELA DE CR - RAMO 55                -
     C* ----------------------------------------------------
     C     TCR55         BEGSR
     C                   Z-ADD     1             IND               2 0
     C     CR            LOOKUP    CR55(IND)                              16
     C   16              MOVEL     NCR55(IND)    NCR
     C                   ENDSR
     C* ----------------------------------------------------
     C* - PESQUISA A TABELA DE CR - RAMO 65                -
     C* ----------------------------------------------------
     C     TCR65         BEGSR
     C                   Z-ADD     1             IND               2 0
     C     CR            LOOKUP    CR65(IND)                              16
     C   16              MOVEL     NCR65(IND)    NCR
     C                   ENDSR
     C* ----------------------------------------------------
     C* -      TESTA PREENCHIMENTO DE CAMPO DE FRANQUIA    -
     C* ----------------------------------------------------
     C     SR00          BEGSR
     C     FRANQ         IFGT      *ZEROS
     C                   SETON                                        70
     C                   END
     C                   ENDSR
     C* ----------------------------------------------------
     C* -      TESTA SUB-TITULO DE RAMO                    -
     C* ----------------------------------------------------
     C     SR01          BEGSR
     C     CL1           IFEQ      11
     C                   MOVEL     'MAQ.CASC'    DSTIT
     C                   MOVE      'OS'          DSTIT
     C                   ELSE
     C     CL2           IFEQ      11
     C                   MOVE      'MAQ.CASC'    DSTIT
     C                   MOVEL     'OS'          DSTIT
     C                   ELSE
     C     CL3           IFEQ      11
     C                   MOVE      'MAQ.CASC'    DSTIT
     C                   MOVEL     'OS'          DSTIT
     C                   ELSE
     C     CL4           IFEQ      11
     C                   MOVE      'MAQ.CASC'    DSTIT
     C                   MOVEL     'OS'          DSTIT
     C                   ELSE
     C     CL5           IFEQ      11
     C                   MOVE      'MAQ.CASC'    DSTIT
     C                   MOVEL     'OS'          DSTIT
     C                   ELSE
     C     CL6           IFEQ      11
     C                   MOVE      'MAQ.CASC'    DSTIT
     C                   MOVEL     'OS'          DSTIT
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   END
     C                   ENDSR
     C* ----------------------------------------------------
     C* -      TESTA PREENCHIMENTO DE CLÁUSULAS            -
     C* ----------------------------------------------------
+----C     SRCL          BEGSR
|    C     CL1           COMP      0                                      49
|    C  N49              MOVE      CL1           CL                2 0
|    C  N49              EXSR      SRRCL
|    C     CL2           COMP      0                                      49
|    C  N49              MOVE      CL2           CL
|    C  N49              EXSR      SRRCL
|    C     CL3           COMP      0                                      49
|    C  N49              MOVE      CL3           CL
|    C  N49              EXSR      SRRCL
|    C     CL4           COMP      0                                      49
|    C  N49              MOVE      CL4           CL
|    C  N49              EXSR      SRRCL
|    C     CL5           COMP      0                                      49
|    C  N49              MOVE      CL5           CL
|    C  N49              EXSR      SRRCL
|    C     CL6           COMP      0                                      49
|    C  N49              MOVE      CL6           CL
|    C  N49              EXSR      SRRCL
|    C* escreve textos das condições particulares dos Novos Produtos
|+---C                   IF        (NEWPRD = 'S') AND (RR = 55)
||   C                   EXSR      TXTM
|----C                   END
+----C                   ENDSR
     C* ----------------------------------------------------
     C* -  DETERMINA RAMO P/ DESCODIFICAÇÃO DE CLÁUSULAS   -
     C* ----------------------------------------------------
     C     SRRCL         BEGSR
     C   01              EXSR      DCL01
     C   21              EXSR      DCL01
     C   31              EXSR      DCL01
      *
     C   02              EXSR      DCL02
     C   22              EXSR      DCL02
     C   32              EXSR      DCL02
      *
     C   03              EXSR      DCL03
      *
     C   04              EXSR      DCL04
      * RESIDENTIA
     C   05              EXSR      DCL05
      *
     C   06              EXSR      DCL06
     C   26              EXSR      DCL06
     C   36              EXSR      DCL06
      *
     C   57              EXSR      DCL17
      *
     C   59              EXSR      DCL19
      *
     C   25              EXSR      DCL55
     C   35              EXSR      DCL55
     C* MERCANTILE
     C   55              EXSR      DCL55
      *
     C   15              EXSR      DCL15
     C                   ENDSR
     C* ----------------------------------------------------
     C* -      DESCODIFICA AS CLÁUSULAS  - RAMO 01         -
     C* ----------------------------------------------------
     C     DCL01         BEGSR
     C                   Z-ADD     1             WI2               2 0
     C     CL            LOOKUP    CL01(WI2)                              16
     C   16              MOVEL     NCL01(WI2)    DSGCL            70
     C  N16              MOVE      *BLANK        DSGCL
     C                   EXCEPT    LHCL
     C                   ENDSR
     C* ----------------------------------------------------
     C* -      DESCODIFICA AS CLÁUSULAS  - RAMO 02         -
     C* ----------------------------------------------------
     C     DCL02         BEGSR
     C                   Z-ADD     1             WI2
     C     CL            LOOKUP    CL02(WI2)                              16
     C   16              MOVEL     NCL02(WI2)    DSGCL
     C  N16              MOVE      *BLANK        DSGCL
     C                   EXCEPT    LHCL
     C                   ENDSR
     C* ----------------------------------------------------
     C* -      DESCODIFICA AS CLÁUSULAS  - RAMO 03         -
     C* ----------------------------------------------------
     C     DCL03         BEGSR
     C                   Z-ADD     1             WI2
     C     CL            LOOKUP    CL03(WI2)                              16
     C   16              MOVEL     NCL03(WI2)    DSGCL
     C  N16              MOVE      *BLANK        DSGCL
     C                   EXCEPT    LHCL
     C                   ENDSR
     C* ----------------------------------------------------
     C* -      DESCODIFICA AS CLÁUSULAS  - RAMO 04         -
     C* ----------------------------------------------------
     C     DCL04         BEGSR
     C                   Z-ADD     1             WI2
     C     CL            LOOKUP    CL04(WI2)                              16
     C   16              MOVEL     NCL04(WI2)    DSGCL
     C  N16              MOVE      *BLANK        DSGCL
     C                   EXCEPT    LHCL
     C                   ENDSR
     C* ----------------------------------------------------
     C* -      DESCODIFICA AS CLÁUSULAS  - RAMO 05         -
     C* ----------------------------------------------------
     C     DCL05         BEGSR
     C                   Z-ADD     1             WI2
     C     CL            LOOKUP    CL05(WI2)                              16
     C   16              MOVEL     NCL05(WI2)    DSGCL
     C  N16              MOVE      *BLANK        DSGCL
+    C   16              EXCEPT    LHCL
     C                   ENDSR
     C* ----------------------------------------------------
     C* -      DESCODIFICA AS CLÁUSULAS  - RAMO 06         -
     C* ----------------------------------------------------
     C     DCL06         BEGSR
     C                   Z-ADD     1             WI2
     C     CL            LOOKUP    CL06(WI2)                              16
     C   16              MOVEL     NCL06(WI2)    DSGCL
     C  N16              MOVE      *BLANK        DSGCL
     C                   EXCEPT    LHCL
     C                   ENDSR
     C* ----------------------------------------------------
     C* -      DESCODIFICA AS CLÁUSULAS  - RAMO 17         -
     C* ----------------------------------------------------
     C     DCL17         BEGSR
     C                   MOVE      *BLANK        FRQ17            16
     C                   Z-ADD     1             WI2
     C     CL            LOOKUP    CL17(WI2)                              16
     C   16              MOVEL     NCL17(WI2)    DSGCL
     C   16CL            IFEQ      01
[LG] C*                    MOVEL'2% S/CAP'FRQ17
[LG] C                   MOVEL     '5% S/CAP'    FRQ17
     C                   MOVE      '.SEGURO '    FRQ17
     C                   END
     C  N16              MOVE      *BLANK        DSGCL
     C                   EXCEPT    LHCL
     C                   ENDSR
     C* ----------------------------------------------------
     C* -      DESCODIFICA AS CLÁUSULAS  - RAMO 19         -
     C* ----------------------------------------------------
     C     DCL19         BEGSR
     C                   Z-ADD     1             WI2
     C     CL            LOOKUP    CL19(WI2)                              16
     C   16              MOVEL     NCL19(WI2)    DSGCL
     C  N16              MOVE      *BLANK        DSGCL
     C                   EXCEPT    LHCL
     C                   ENDSR
     C* ----------------------------------------------------
     C* -      DESCODIFICA AS CLÁUSULAS  - RAMO 55         -
     C* ----------------------------------------------------
     C     DCL55         BEGSR
     C                   Z-ADD     1             WI2
     C     CL            LOOKUP    CL55(WI2)                              16
     C   16              MOVEL     NCL55(WI2)    DSGCL
     C  N16              MOVE      *BLANK        DSGCL
+    C   16              EXCEPT    LHCL
     C                   ENDSR
     C* ----------------------------------------------------
     C* -      DESCODIFICA AS CLÁUSULAS  - RAMO 15         -
     C* ----------------------------------------------------
     C     DCL15         BEGSR
     C     CL            IFNE      06
     C     CL            IFNE      07
     C     CL            IFNE      08
     C                   Z-ADD     1             WI2
     C     CL            LOOKUP    CL15(WI2)                              16
     C   16              MOVEL     NCL15(WI2)    DSGCL
     C  N16              MOVE      *BLANK        DSGCL
     C                   EXCEPT    LHCL
     C                   END
     C                   END
     C                   END
     C                   ENDSR
     C* ----------------------------------------------------
     C* -      DESCODIFICA AS FORMAS DE PAGAMENTO          -
     C* ----------------------------------------------------
     C     SRFPAG        BEGSR
     C                   Z-ADD     1             WFP               1 0
     C     FRAC          LOOKUP    AFP(WFP)                               16
     C   16              MOVEL     NAFP(WFP)     DSCFP            20
     C  N16              MOVE      *BLANK        DSCFP
     C                   ENDSR
+    C*************************************************************************
+    C* NOVO PRODUTO MERCANTILE, JANEIRO 2007, ASILVA
+    C* formatar textos das coberturas, cláusulas e condições especiais
+    C* do ramo 55-MERCANTILE para a impresão no bloco das Condições Especiais
+    C*************************************************************************
+    C     TXTM          BEGSR
+    C                   MOVE      *BLANK        WM
+    C* COBERTURAS FACULTATIVAS
+    C* escreve titulo se houver alguma cobertura facultativa
+    C     CF999         IFNE      *BLANK
+    C                   MOVE      MERC(1)       WM(1)
+    C                   END
+    C* greves, tumultos e alterações da ordem pública
+    C     CF01          IFEQ      'S'
+    C                   MOVE      MERC(2)       WM(2)
+    C                   END
+    C* actos de vandalismo ou maliciosos
+    C     CF02          IFEQ      'S'
+    C                   MOVE      MERC(3)       WM(3)
+    C                   END
+    C* prejuizos indirectos (conteudo)
+    C     CF03          IFEQ      'S'
+    C                   MOVE      MERC(4)       WM(4)
+    C                   END
+    C* perda de rendas (conteudo)
+    C     CF04          IFEQ      'S'
+    C                   MOVE      MERC(5)       WM(5)
+    C                   END
+    C* aluimento de terras
+    C     CF05          IFEQ      'S'
+    C                   MOVE      MERC(6)       WM(6)
+    C                   END
+    C* riscos eléctricos
+    C     CF06          IFEQ      'S'
+    C                   MOVE      MERC(7)       WM(7)
+    C                   END
+    C* fenómenos sísmicos
+    C     CF07          IFEQ      'S'
+    C                   MOVE      MERC(8)       WM(8)
+    C                   END
+    C* encargos fixos (conteudo)
+    C     CF08          IFEQ      'S'
+    C                   MOVE      MERC(9)       WM(9)
+    C                   END
+    C* equipamento electronico (conteudo)
+    C     CF09          IFEQ      'S'
+    C                   MOVE      MERC(10)      WM(10)
+    C                   END
+    C* avaria de máquinas (conteudo)
+    C     CF10          IFEQ      'S'
+    C                   MOVE      MERC(11)      WM(11)
+    C                   END
+    C* deterioração de bens refrigerados (conteudo)
+    C     CF11          IFEQ      'S'
+    C                   MOVE      MERC(12)      WM(12)
+    C                   END
+    C* trespasse (conteudo)
+    C     CF12          IFEQ      'S'
+    C                   MOVE      MERC(13)      WM(13)
+    C                   END
+    C* actos de terrorismo
+    C     CF13          IFEQ      'S'
+    C                   MOVE      MERC(14)      WM(14)
+    C                   END
+    C* texto que sai sempre
+    C                   MOVE      MERC(15)      WM(15)
+    C                   MOVE      MERC(16)      WM(16)
+    C                   MOVE      MERC(17)      WM(17)
+    C* titulo que sai se houver alguma condição especial, aplicável ou diversa
+    C     CEAD99        IFNE      *BLANK
+    C                   MOVE      MERC(18)      WM(18)
+    C                   MOVE      MERC(19)      WM(19)
+    C                   MOVE      MERC(20)      WM(20)
+    C                   MOVE      MERC(21)      WM(21)
+    C                   END
+    C* CONDIÇÕES ESPECIAIS
+    C* actualização convencionada de capitais
+    C     CE001         IFEQ      'S'
+    C                   MOVE      MERC(22)      WM(22)
+    C                   END
+    C* apólices de prémio variável
+    C     CE002         IFEQ      'S'
+    C                   MOVE      MERC(23)      WM(23)
+    C                   END
+    C* CLAUSULAS APLICÁVEIS
+    C* apólices em regime de co-seguro
+    C     CA030         IFEQ      'S'
+    C                   MOVE      MERC(24)      WM(24)
+    C                   END
+    C* credor hipotecário
+    C     CA100         IFEQ      'S'
+    C                   MOVE      MERC(25)      WM(25)
+    C* nome do credor hipotecário
+    C     CRHIP         IFGT      *ZEROS
+    C                   SETOFF                                       88
+    C                   MOVEL     20            K7                7 0
+    C                   MOVE      CRHIP         K7
+    C     K7            CHAIN     CRHIPOT                            88
+    C  N88              MOVE      NOMCRH        WM(25)
+    C                   END
     C                   END
+    C* valor de substituição em novo (mobiliario e equipamento)
+    C     CA101         IFEQ      'S'
+    C                   MOVE      MERC(26)      WM(26)
     C                   END
+    C* apolices de capital variável (flutuantes)
+    C     CA102         IFEQ      'S'
+    C                   MOVE      MERC(27)      WM(27)
+    C                   END
+    C* CLAUSULAS DIVERSAS
+    C* livros
+    C     CD103         IFEQ      'S'
+    C                   MOVE      MERC(28)      WM(28)
+    C                   END
+    C* existência pólvora (até 25Kg)
+    C     CD133         IFEQ      'S'
+    C                   MOVE      MERC(29)      WM(29)
+    C                   END
+    C* cumprimento regras segurança para estabelecimentos que recebem publico
+    C     CD134         IFEQ      'S'
+    C                   MOVE      MERC(30)      WM(30)
+    C                   END
+    C* existência liquidos inflamáveis
+    C     CD135         IFEQ      'S'
+    C                   MOVE      MERC(31)      WM(31)
+    C                   END
+    C* existência produtos inflamáveis diversos
+    C     CD136         IFEQ      'S'
+    C                   MOVE      MERC(32)      WM(32)
+    C                   END
+    C* lotarias
+    C     CD137         IFEQ      'S'
+    C                   MOVE      MERC(33)      WM(33)
+    C                   END
+    C* escreve as linhas das coberturas, clausulas e condições especiais
+    C* do ramo MERCANTILE
+    C     1             DO        33            M                 2 0
+    C     WM(M)         IFNE      *BLANK
+    C     WM(M)         IFEQ      '|'
+    C                   MOVE      *BLANK        WM(M)
+    C                   END
+    C                   EXCEPT    WRTMER
+    C                   END
+    C                   END
+    C                   ENDSR
     C****************************************************************
     C* PRIVAÇÃO DE USO                                              *
     C****************************************************************
     CSR   SRPU          BEGSR
      *
     C                   MOVEL     *BLANKS       DCPU1
     C                   MOVEL     *BLANKS       DCPU2
     C                   MOVEL     *BLANKS       DCPU3
      *
     C     PRIV          IFNE      ' '
     C                   MOVEL     *BLANKS       CDTAB
     C                   MOVE      'PU'          TPTAB
     C                   MOVEL     PRIV          CDTAB
     C     KTAB          CHAIN     FTABX                              69
     C  N69              MOVEL     DCTAB1        DCPU1            80
     C   69              MOVEL     *BLANKS       DCPU1
     C                   END
      *
     C     VSA           IFNE      ' '
     C                   MOVEL     *BLANKS       CDTAB
     C                   MOVE      'VS'          TPTAB
     C                   MOVEL     VSA           CDTAB
     C     KTAB          CHAIN     FTABX                              69
     C  N69              MOVEL     DCTAB1        DCPU2            80
     C   69              MOVEL     *BLANKS       DCPU2
     C                   END
      *
     C     VIA           IFNE      ' '
     C                   MOVEL     *BLANKS       CDTAB
     C                   MOVE      'VI'          TPTAB
     C                   MOVEL     VIA           CDTAB
     C     KTAB          CHAIN     FTABX                              69
     C  N69              MOVEL     DCTAB1        DCPU3            80
     C   69              MOVEL     *BLANKS       DCPU3
     C                   END
      *
     C                   SETOFF                                       73
     C     DCPU1         IFNE      *BLANKS
     C                   SETON                                        73
     C                   END
     C                   SETOFF                                       75
     C     DCPU2         IFNE      *BLANKS
     C                   SETON                                        75
     C                   END
     C                   SETOFF                                       77
     C     DCPU3         IFNE      *BLANKS
     C                   SETON                                        77
     C                   END
      *
     C                   SETOFF                                       76
     C     VSA           IFNE      ' '
     C     VIA           ORNE      ' '
     C                   SETON                                        76
     C                   END
      *
     C                   EXCEPT    LHPRIV
      *
     CSR                 ENDSR
     C****************************************************************
     C* ASSISTÊNCIA EM VIAGEM                                        *
     C****************************************************************
     CSR   SRAV          BEGSR
     C                   SETOFF                                       1213
     C                   MOVEL     *BLANKS       CDTAB
     C                   MOVE      'AV'          TPTAB
     C                   MOVEL     VIAG          CDTAB
     C     KTAB          CHAIN     FTABX                              69
     C   69              MOVEL     *BLANKS       DCTAB1
     C   69              MOVEL     *BLANKS       DCTAB2
     C   69              MOVEL     *BLANKS       DCTAB3
      *
     C                   EXSR      SRAV2
     CSR                 ENDSR
     C****************************************************************
     C* ASSISTÊNCIA EM VIAGEM - PARTE 2                              *
     C****************************************************************
     CSR   SRAV2         BEGSR
     C                   SETOFF                                       1213
     C     VIAG          IFEQ      'B'
     C     CATV          COMP      1                                      12
     C   12NLUG1         COMP      4                                    13
     C  N12CATV          COMP      7                                      12
     C  N12CATV          COMP      6                                      13
     C   13              SETOFF                                       12
     C                   ELSE
     C     VIAG          IFEQ      'V'
     C     CATV          COMP      1                                      12
     C   12NLUG1         COMP      4                                    13
     C  N12CATV          COMP      7                                      12
     C  N12CATV          COMP      6                                      13
     C   13              SETOFF                                       12
     C                   END
     C                   END
     CSR                 ENDSR
      *
     OMGP045    E            LH0313           08
     O* nº segurado e mediadores
     O              N80      NCOP                14
     O               80                          18 '( DUPLICADO )'
     O                       NSEG                30
     O                       JMM1                35
     O                       JMM2                40
     O* codigo LST do ramo
+    O          E            LH0313           10
+    O                       CODRM               18 '  .  .  '
     O* nome do ramo
     O          E            LH0313           12
     O***                      CODRM     18 '  .  .  '
     O***                                20 '-'
     O                       NRAMO          B    44
     O* ramo, apolice e nº acta
     O          E            LH0313      3
     O                       RR                  15 '0  '
     O                                           16 '-'
     O                       APOL          4     23
     O                       ACTA          4     37
     O* data, tipo registo, nome segurado
     O          E            LH0313      3
     O                       WUDATE              24 '0  /  /    '
     O                       TIPREG              38
     O                       SEG                 92
     O* morada
     O          E            LH0313      1
     O                       MOR                 92
     O* localidade
     O          E            LH0313      1
     O                       CPMOR1              60 '    -   '
     O                       CPMOR2              82
     O* data inicio e fim, duração
     O          E            LH37        0
     O                                           13 'INÍCIO:'
     O                       WDTINI              24 '0  /  /    '
     O                       TIPDUR              43
     O                  49                       39 'ATÉ'
     O                  49   WDTFIM              50 '0  /  /    '
     O* forma pagamento
     O          E            LH38        2
     O                                           17 'FORMA PAG.:'
     O                       DSCFP               38
     O* datas de alteração
     O          E            LH38A       2
     O                                           16 'ALTERAÇÃO:'
     O                       WDTALT              27 '0  /  /    '
     O                                           29 '-'
     O                       WDTFIM              40 '0  /  /    '
     O* motivo da alteração
     O          E            LH32        2  1
     O                                           13 'MOTIVO:'
     O                       MALTER              54
     O* bonificação
     O          E            LH17        1  1
     O                                           18 'BONIFICAÇÃO:'
     O                       PBONUS              22 '0 %'
     O* premio comercial e total
     O          E            LH21P            26
     O                       NREC                12 '0         '
     O                                           32 'PRÉMIO COMERCIAL'
     O                       PRS           J     47 '¤'
     O                                           62 'PRÉMIO TOTAL'
     O                                           78 'DE CADA FRACÇÃO'
     O                       TOTREC        J     93 '¤'
     O          E            LH20P            26
     O                       NREC                15 '0         '
     O                                           35 'PRÉMIO COMERCIAL'
     O                       PRS           J     50 '¤'
     O                                           65 'PRÉMIO TOTAL'
     O                       TOTREC        J     80 '¤'
     O          E            LH14             30
     O                       DSTIT               16
     O*
     O          E            LH14        1
     O                       WOUT13              83
     O*                   ---------------------------> SÓ PARA RM=02
     O                  02   LRPS                73
     O*                   ---------------------------> SÓ PARA RM=15
     O                  15   LRPS                73
     O*                   ---------------------------> SÓ PARA RM=50
     O                  50   TVEIC               54
     O*                   ---------------------------> SÓ PARA RM=51
     O                  51   TCONTR              53
     O                  51   MATR1               76
     O* local do risco
     O          E            LH19        1
     O                                           22 'LOCAL DO RISCO:'
     O                       LRPS                68
     O* natureza do estabelecimento e classe
     O          E            LH20        1
     O                                           22 'NAT.ESTABELEC.:'
     O                       NATUR               53
     O                                           66 'CLASSE:'
     O                       CLASS               68
     O*
     O          E            LH21        1
     O                       WOUT20              83
     O*                -----------------------------------> OUTPUT CONDICIONADO
     O               01                          80 'FRANQUIA'
     O               57                          87 'FRANQUIA(S)'
     O               58                          87 'FRANQUIA(S)'
     O               59                          87 'FRANQUIA(S)'
     O*
     O          E            LH22        1
     O              N57N58N59                    31 'CONFORME CONDIÇÕES DA AP'
     O              N57N58N59                    37 'ÓLICE '
     O              N57N58N59                    43 'ANEXAS'
     O               57                          31 'CONFORME CONDIÇÕES GERAI'
     O               57                          55 'S DA APÓLICE E ESPECIAIS'
     O               57                          74 'QUANDO MENCIONADAS'
     O**               57 70   FRANQ     81 '¤  . 0 '
     O               57 70   FRANQ         1     81 '¤'
     O               57 70                       86 ',00  '
     O               57N70                       86 'VER OBS.'
     O               58                          31 'CONFORME CONDIÇÕES GERAI'
     O               58                          55 'S DA APÓLICE E ESPECIAIS'
     O               58                          74 'QUANDO MENCIONADAS'
     O**               58 70   FRANQ     81 '¤  . 0 '
     O               58 70   FRANQ         1     81 '¤'
     O               58 70                       86 ',00  '
     O               58N70                       85 'VER OBS.'
     O               59                          31 'CONFORME CONDIÇÕES GERAI'
     O               59                          55 'S DA APÓLICE E ESPECIAIS'
     O               59                          74 'QUANDO MENCIONADAS'
     O**               59 70   FRANQ     81 '¤  . 0 '
     O               59 70   FRANQ         1     81 '¤'
     O               59 70                       86 ',00  '
     O               59N70                       85 'VER OBS.'
     O          E            LHCR        1
     O              N15      NCR                 50
     O              N15      CAP                 78 '¤   .   . 0 '
     O**              N15      CAP   1   78 '¤'
     O              N15                          83 ',00  '
     O* ------------------------------------------------------
     O* ------> PARA ACID.TRABALHO
     O               15      NCR                 49
     O               15      CAP                 64 '¤   .   . 0 '
     O**               15      CAP   1   64 '¤'
     O               15                          69 ',00  '
     O               15      TXCOM         1     80
     O* ------------------------------------------------------
     O          E            SALTO1      1
     O* ------------------------------------------------------
     O          E            LHCR15      2
     O                       WOUT20              83
     O* ------------------------------------------------------
     O*
     O          E            L0522       1
     O                       WOUT21              83
     O*
     O          E            LHCR01      1
     O                       NCR                 50
     O                 N15   TX                  63 '0.    '
     O                  15   TXAT                63 '0 .   '
     O                       CAP                 78 '¤   .   . 0 '
     O**                       CAP   1   78 '¤'
     O                                           83 ',00  '
     O*
     O          E            L0122       1
     O                                           31 'INCÊNDIO, ACÇÃO MECÂNICA'
     O                                           55 ' DE QUEDA DE RAIO E EXPL'
     O                                           75 'OSÃO, CONFORME Nº 1 '
     O          E            L0122       1
     O                                           31 'DA CLÁUSULA 2ª DAS CONDI'
     O                                           42 'IÇÕES GERAIS'
     O*
     O          E            L0216       1
     O               02                          18 'PROFISSÃO:  '
     O               02      NPROF               49
     O               02                          65 'ANO NASC. - '
     O               02      WNASC               69
     O               15                          18 'PROFISSÃO:  '
     O               15      WPROF               57
     O*
     O          E            L0220       1
     O              N15                          45 'RISCOS PROFISSIONAIS E E'
     O              N15                          63 'XTRA-PROFISSIONAIS'
     O               15                          18 'ACTIVIDADE: '
     O               15      DCAE                58
     O*
     O          E            L0233       1
     O                                           27 'BENEFICIÁRIO(S):'
     O                       OBS1                74
     O*
     O          E            L0315       1
     O                                           31 'CONFORME CONDIÇÕES PARTI'
     O                                           39 'CULARES '
     O                                           45 'ANEXAS'
     O**                       CAP   1   78 '¤'
     O                       CAP                 78 '¤   .   . 0 '
     O                                           83 ',00  '
     O*
     O          E            L0415       1
     O                                           30 'NEGÓCIO/LOCAL DO RISCO:'
     O                       LRPS                76
     O*
     O          E            L0620       1
     O                                           56 'PERÍODO DE INDEMNIZAÇÃO:'
     O                       NASC          4     60
     O                                           65 'MÊSES'
     O*
     O          E            L5013A      0
     O                       LIT1                59
     O                       CILIND        4     65
     O                                           68 'CC.'
     O*
     O          E            L5013B      0
     O                       LIT1                73
     O                       PBRUT         4     80
     O                                           83 'Kg.'
     O*
     O          E            L5015       1
     O                                           17 'MARCA'
     O                       MARC1               37
     O                                           58 'ANO CONSTRUÇÃO'
     O                       WAC           4     65
     O                                           89 'NºLUGARES'
     O                       NLUG1         4     91
     O*
     O          E            L5015       1
     O                                           18 'MODELO'
     O                       MODELO              43
     O                                           53 'MATRÍCULA'
     O                       MATR1               65
     O*
     O          E            L5017       1
     O                                           20 'MARCA-II'
     O                       MARC2               37
     O                                           56 'MATRÍCULA-II'
     O                       MATR2               71
     O*
     O          E            L5019       1
     O                                           16 'CLÁUSULAS:'
     O                                           18 'A'
     O                       C1                  20
     O                       C2                  22
     O                       C3                  24
     O                       C4                  26
     O                       C5                  28
     O                       C6                  30
     O                       C7                  32
     O                       C8                  34
     O*
     O          E            LHRCA       1
     O                       DCRCA               64
     O*
     O          E            LHQIV       1
     O                                           31 'QUEBRA ISOLADA DE VIDROS'
     O                                           60 'ATÉ   ¤  1.000,00'
     O               87                          86 'FRANQUIA ¤50,00 POR SINI'
     O               87                          90 'STRO'
     O          E    87      LHQIV2      2
     O                                           31 'A FRANQUIA DA COBERTURA '
     O                                           55 'DE QUEBRA ISOLADA  DE VI'
     O                                           79 'DROS É APLICÁVEL  APENAS'
     O                                           87 ' EM CASO'
     O          E    87      LHQIV2      1
     O                                           31 'DE SUBSTITUIÇÃO DO VIDRO'
     O                                           32 '.'
     O*
     O          E            LHCCC       1
     O                                           31 'CHOQUE,COLISÃO OU CAPOTA'
     O                                           36 'MENTO'
     O                                           48 'ATÉ'
     O                       CAPDP               57 '¤   . 0 '
     O**                       CAPDP 1   57 '¤'
     O                                           62 ',00  '
     O**                       FRANQ     73 '¤  . 0 '
     O                       FRANQ         1     73 '¤'
     O                                           91 ',00 POR SINISTRO  '
     O*
     O          E            LHCCC       1
     O                                           31 '-INCLUINDO QUEBRA ISOLAD'
     O                                           42 'A DE VIDROS'
     O               87                          89 '¤50,00 POR SINISTRO'
     O*
     O          E            LHIRE       1
     O                                           31 'INCÊNDIO,RAIO OU EXPLOSÃ'
     O                                           32 'O'
     O                                           48 'ATÉ'
     O                       CAPDP               57 '¤   . 0 '
     O**                       CAPDP 1   57 '¤'
     O                                           62 ',00  '
     O**                       FRANQ     73 '¤  . 0 '
     O                       FRANQ         1     73 '¤'
     O                                           91 ',00 POR SINISTRO  '
     O*
     O          E            LHFR        1
     O                                           21 'FURTO OU ROUBO'
     O                                           48 'ATÉ'
     O                       CAPDP               57 '¤   . 0 '
     O**                       CAPDP 1   57 '¤'
     O                                           62 ',00  '
     O*
     O          E            LHFR        1
     O                                           31 '-INCLUINDO ROUBO ISOLADO'
     O                                           35 ' DE '
     O                                           45 'ACESSÓRIOS'
     O*
     O          E            LHFR        1
     O                                           28 'QUANDO DISCRIMINADOS'
     O*
     O          E            LHCSP       1
     O                                           25 'FENÓMENOS NATUREZA'
     O                                           48 'ATÉ'
     O                       CAPDP               57 '¤   . 0 '
     O**                       CAPDP 1   57 '¤'
     O                                           62 ',00  '
     O          E            LHCSP       1
     O                                           23 'ACTOS MALICIOSOS'
     O                                           48 'ATÉ'
     O                       CAPDP               57 '¤   . 0 '
     O**                       CAPDP 1   57 '¤'
     O                                           62 ',00  '
     O**                       FRANQ     73 '¤  . 0 '
     O                       FRANQ         1     73 '¤'
     O                                           91 ',00 POR SINISTRO  '
      *
     O          E            LHAV        1
     O                       DCTAB1              87
     O          E            LHAV        1
     O                       DCTAB2              88
      *
     O          E    12      LHAV        1
     O                       CA1(1)              91
     O               52                          91 'ALÉM 1500CC.'
      *
     O          E    12      LHAV        1
     O                       CA1(2)              91
      *
     O          E    13      LHAV        1
     O                       CA2(1)              91
     O               53                          91 'ALÉM 1500CC.'
      *
     O          E    13      LHAV        1
     O                       CA2(2)              92
     O                                           93 '-'
      *
     O          E    13      LHAV        1
     O                       CA2(3)              92
      *
     O          E            LHPJ        1
     O                                           28 'INCLUI PROTECÇÃO JURÍ'
     O                                           49 'DICA  TELEF. 351 217 '
     O                                           72 '806 225, EM PORTUGAL OU'
     O                                           87 'NO ESTRANGEIRO'
     O*
     O          E            LHFRQ       2
     O                       TXT4(1)             89
     O*
     O          E            LHFRQ       1
     O                       TXT4(2)             89
     O*
     O          E            LHFRQ       1
     O                       TXT4(3)             89
     O*
     O          E            LHFRQ       1
     O                       TXT4(4)             89
     O*
     O          E            SOCRAT      2
     O                       TXT5(1)             89
     O*
     O          E            SOCRAT      1
     O                       TXT5(2)             89
     O*
     O          E            SOCRAT      1
     O                       TXT5(3)             89
     O*
     O          E            SOCRAT      1
     O                       TXT5(4)             89
     O*
     O          E    85      SOCRAT      1
     O                       TXT5(5)             89
     O*
     O          E    85      SOCRAT      1
     O                       TXT5(6)             89
      *
     O          E            LHDP        2
     O                       TXT6(1)             89
      *
     O          E            LHDP        1
     O                       TXT6(2)             89
      *
     O          E            LHDP        1
     O                       TXT6(3)             89
      *
     O          E            LHDP        1
     O                       TXT6(4)             89
      *
     O          E            LHDP        1
     O                       TXT6(5)             89
      *
     O          E            LHDP        1
     O                       TXT6(6)             89
     O*
     O          E            L5115       1
     O                                           38 'NºLUGARES'
     O                       NLUG1               43
     O                                           75 'UTILIZAÇÃO PARTICULAR'
     O*
     O          E            L5116       1
     O                                           37 'MARCA-II'
     O                       MARC2               53
     O                                           66 'MATRÍCULA-II'
     O                       MATR2               78
     O*
     O          E            L5117       1
     O                                           38 'NºLUGARES'
     O                       NLUG2               43
     O                                           75 'UTILIZAÇÃO PARTICULAR'
     O*
     O          E            L51223      1
     O                                           32 'MORTE E INVALIDEZ PERMAN'
     O                                           36 'ENTE'
     O                       CAPRC               58 '¤&  . 0 '
     O**                       CAPRC 1   58 '¤'
     O                                           63 ',00  '
     O*
     O          E            L51223      1
     O                                           30 'DESPESAS DE TRATAMENTO'
     O                       CAPDP               58 '¤&   . 0 '
     O**                       CAPDP 1   58 '¤'
     O                                           63 ',00  '
     O*
     O          E            L51223      1  0
     O                                           32 '(EXCLUINDO DESPESAS REPA'
     O                                           42 'TRIAMENTO)'
     O*
     O          E            LH0522      1  0
     O*                                  81 'QUADRO ANEXO  '
     O                       TXT3(4)             81
     O*
     O          E            LH5522      1  0
     O*                                  81 'QUADRO ANEXO  '
     O                       TXT7(2)             81
     O*
     O          E            L5522       1
     O                       WOUT21              83
     O* escreve clausulas dos ramos
     O          E            LHCL        1
     O                       DSGCL               76
     O               57      FRQ17               90
+    O* escreve coberturas, clausulas e condições especiais do ramo RESIDENTIA
+    O          E            WRTRES      1
+    O                       WR(WIdx)            86
+    O* escreve coberturas, clausulas e condições especiais do ramo MERCANTILE
+    O          E            WRTMER      1
+    O                       WM(M)               86
+    O* escreve coberturas, clausulas e condições especiais do ramo CONDOMINIO
+    O          E            WRTCON      1
+    O                       WR(WIDX)            86
     O*
     O          E            LH31        1
     O                                           25 'CREDOR HIPOTECÁRIO:'
     O                       NOMCRH              67
     O*
     O          E            LH33        2
     O                                           18 'OBSERVAÇÕES:'
     O                       OBS1                65
     O*
     O          E            LH39        2
     O                       WTextObs            18
     O                       OBS1                65
     O*
     O          E            LH34        1
     O                       OBS2                65
     O*
     O          E            LH40        1
     O                       WTxtFim             86
     O*
     O          E            LH35        1
     O                                           25 'CREDOR HIPOTECÁRIO:'
     O                       NOMCRH              67
     O*
     O          E            LH35OA      1
     O                                           56 'T.PERDA:INDIRECTA'
     O                                           70 'PERCENTAGEM:'
     O                       PPERDI              73
     O                                           74 '%'
     O*
     O          E            LH35OB      1
     O                                           51 'T.PERDA:ENC.FIXOS'
     O                       ENCFIX              65 '¤   .   . 0 '
     O**                       ENCFIX1   65 '¤'
     O                                           70 ',00  '
     O                                           80 'P.INDEMN.:'
     O                       PINDMN              83
     O*
     O          E            LH36O       1
     O***                N05             23 'NÃO TEM'
     O                 N05                       53 '(CÓD. DE COND. ESP./PART'
     O                 N05                       58 '.  - '
     O                 N05                       79 ')'
     O                 N05   CL1                 61 '0 '
     O                 N05   CL2                 64 '0 '
     O                 N05   CL3                 67 '0 '
     O                 N05   CL4                 70 '0 '
     O                 N05   CL5                 73 '0 '
     O                 N05   CL6                 76 '0 '
     O**                  05             37 'CONFORME DESCRITO NAS '
     O**                  05             48 'OBSERVAÇÕES'
     O*
     O          E    15N46   LH44        1
     O                                           20 'CAPITAL P/FAT:'
     O                       CAPFAT              34 '¤     .   . 0 '
     O**                       CAPFAT1   34 '¤'
     O                                           39 ',00  '
     O*
     O          E    15      LH45        2
     O          E    15      LH45T          1
     O                       TXT2(T)             83
     O*
     O          E    73      LHPRIV      1
     O                       DCPU1               87
     O          E    75      LHPRIV      1
     O                       DCPU2               87
     O          E    77      LHPRIV      1
     O                       DCPU3               87
     O          E    76      LHPRIV      1
     O                       MSG(1)              87
     O          E    76      LHPRIV      1
     O                       MSG(2)              87
     O*
     O          E            LH50OC      1  1
     O                       WOUT51              66
     O*
     O          E            LH50OC         1
     O                                           32 'MORTE OU INVALIDEZ PERMA'
     O                                           37 'NENTE'
     O                       CAPMIP              58 '¤&  . 0 '
     O**                       CAPMIP1   58 '¤'
     O                                           63 ',00  '
     O*
     O          E            LH50OC         1
     O                                           30 'DESPESAS DE TRATAMENTO'
     O                       CAPDME              58 '¤&  . 0 '
     O**                       CAPDME1   58 '¤'
     O                                           63 ',00  '
     O*
     O          E            LH50OC         0
     O                                           32 '(EXCLUINDO DESPESAS REPA'
     O                                           42 'TRIAMENTO)'
     O          E            LDETBRK05      6
     O                       WDETAIL             96
     O          E            LDETLIN05      1
     O                       WDETAIL             93
     O          E            BRKPAGE          10
     O                                            1 '.'
** TRAMOS    [RAMO (36)]
01INC.E ELEM.NATUREZA                *
02ACIDENTES PESSOAIS                 *
03RESP.CIVIL PESSOAL                 *
04RESPONSABILIDADE CIVIL GERAL       *
05RISCOS MULTIPLOS                   *
06PERDA DE LUCROS                    *
07DINHEIRO EM TRANSITO               *
08ROUBO RESIDENCIAS                  *
09ROUBO ESTABELECIMENTOS             *
10VIAGENS                            *
15ACIDENTES DE TRABALHO              *
17EQUIP. ELECTRÓNICO                 *
18AVARIA DE MÁQUINAS                 *
19OBRAS E MONTAGENS                  *
20VIAGENS/RC                         *
21INCÊNDIO/RC                        *
22ACIDENT.PESSOAIS/RC                *
24R.CIVIL-ACT.COM.IND/RC             *
25MULTIRISCO-ESTABEL./RC             *
26PERDA DE LUCROS/RC                 *
27DINH.EM TRANSITO/RC                *
28AVARIA DE MÁQUINAS/RC              *
29ROUBO ESTABELEC./RC                *
30VIAGENS/RA                         *
31INCÊNDIO/RA                        *
32ACIDENT.PESSOAIS/RA                *
34R.CIVIL-ACT.COM.IND/RA             *
35MULTIRISCO-ESTABEL./RA             *
36PERDA DE LUCROS/RA                 *
37DINH.EM TRANSITO/RA                *
38AVARIA DE MÁQUINAS/RA              *
39ROUBO ESTABELEC./RA                *
50AUTOMÓVEL                          *
51OCUPANTES                          *
55MULTIRISCO-ESTABELECIM.            *
65CRISTAIS                           *
**       [CINST (36)]                                      TABRMINS
080000     01
010200     02
130300     03
130500     04
340100     05
160100     06
070300     07
090605     08
090604     09
010205     10
010200     15
340700     17
090304     18
340600     19
000020     20
000021     21
000022     22
000024     24
000025     25
000026     26
000027     27
000028     28
000029     29
000030     30
000031     31
000032     32
000034     34
000035     35
000036     36
000037     37
000038     38
000039     39
310000     50
010351     51
340200     55
090400     65
**     [CR01 (5)]                                          TABCR01
1EDIFÍCIO(S)
2CONTEÚDO(S)
3ESTABELECIMENTO(S) COMERCIAL(S)
4RISCO INDUSTRIAL CONFORME NOTA ANEXA
9CONFORME NOTA ANEXA
**     [CR02 (8)]                                          TABCR02
1MORTE E INVALIDEZ PERMANENTE
2DESPESAS DE TRATAMENTO E REPATRIAMENTO
3INCAPACIDADE TEMPORÁRIA ABSOLUTA
4DESPESAS DE FUNERAL
5MORTE
6INVALIDEZ PERMANENTE
7MORTE OU INVALIDEZ PERMANENTE
9CONFORME NOTA ANEXA
**     [CR04 (3)]                                          TABCR04
1POR CADA ACIDENTE OU OCORRÊNCIA
2POR CADA PERÍODO DE SEGURO
9CONFORME NOTA ANEXA
**     [CR05 (8)]                                          TABCR05
1EDIFÍCIO(S)
2CONTEÚDO(S)
3RESPONSABILIDADE CIVIL PESSOAL
4RISCOS ELÉCTRICOS
5CONTEÚDOS - OBJECTOS DE RISCO AGRAVADO
6CONDIÇÃO ESPECIAL-SEGURO OBJ. ESPECIAIS
7CONTEÚDOS - VEÍCULO(S) EM GARAGEM
9CONFORME NOTA ANEXA
**     [CR06 (4)]                                          TABCR06
1LUCRO BRUTO
2DESPESAS C/ AUDITORES
3DESPESAS GERAIS SEGURAS
9CONFORME NOTA ANEXA
**     [CR07 (3)]                                          TABCR07
1DINHEIRO EM TRÂNSITO
2DINHEIRO EM COFRE
9CONFORME NOTA ANEXA
**     [CR08 (3)]                                          TABCR08
1CONTEÚDO(S)
2OBJECTOS DE METAL PRECIOSO E PELES
9CONFORME NOTA ANEXA
**     [CR09 (3)]                                          TABCR09
1MERCADORIAS E MATÉRIAS PRIMAS
2MOBILIARIO E UTENSÍLIOS
9CONFORME NOTA ANEXA
**                                                         TABCR10
1EDIFÍCIO(S)
2CONTEÚDO(S)
9CONFORME NOTA ANEXA
**                                                         TABCR15
1CONTRATO EFECTUADO A PRÉMIO FIXO
2CONTRATO EFECTUADO A PRÉMIO VARIÁVEL/F.F
**                                                         TABCR17
1EQUIP.ELECTRÓNICO TRATAMENTO DE DADOS
2EQUIP.ESCRITÓRIO DIVERSO
3INST. TELECOMUNICAÇÕES
4INST. MÉDICO-CIRURGICAS
5INST.FIXAS CONTROLE NAVEG.AE./TERR./MAR.
6INST.RÁDIO-TRANS.(CONV/SAT) RADIOA./OUT.
9CONFORME NOTA ANEXA
**                                                         TABCR18
1CONFORME DESCRIÇÃO EM OBSERVAÇÕES
2MAQUINARIA INDUSTRIAL CONFORME DESCRIÇÃO
8DESPESAS EXTRAORDINÁRIAS CONFORME DESCRIÇÃO
9CONFORME NOTA ANEXA
**                                                         TABCR19
1CONFORME DESCRIÇÃO EM OBSERVAÇÕES
9CONFORME NOTA ANEXA
**                                                         TABCR55
1EDIFÍCIO(S)
2CONTEÚDO(S)
4RISCOS ELÉCTRICOS
9CONFORME NOTA ANEXA
**                                                         TABCR65
1VIDROS E CRISTAIS
**                                                         TABCL01
01PROPRIEDADE HORIZONTAL (SEG. FRACÇÕES AUTÓN. ) - (COND.ESP. 001)
02CONSTRUÇÕES FEITAS EM TERRENO ALHEIO-(COND.ESPECIAL 002)XXX
03DANOS EM BENS DO SENHORIO-(COND.ESPECIAL 003)
04ACTUALIZAÇÃO INDEXADA DE CAPITAIS (COND.ESPECIAL 004)
05APÓLICES DE CAPITAL VARIÁVEL - FLUTUANTE (COND.ESPECIAL 005)
06ACTUALIZAÇÃO CONVENCIONADA DE CAPITAIS (COND.ESPECIAL 006)
07VALOR DE SUSTITUIÇÃO (EQUIP.INDUST.)-(COND.ESPECIAL 007)
08DESCONTOS P/SIST.PREV./PROT.C. INCÊNDIO-(COND.ESPECIAL 008)
09OFICINAS E FABRICAS SEM LABORAÇÃO-(COND.ESPECIAL 009)
10FRACCIONAMENTO DO PRÉMIO-(COND.ESPECIAL 010)XXX
11ACTUALIZAÇÃO PROGRESSIVA DE CAPITAL-(COND.ESPECIAL 011)
12INCL.NOVOS BENS OU BENEF. NOS EXISTENTES-(COND.ESPECIAL 012)
13REGIME DE FRANQUIAS-(COND.ESPECIAL 013)
14TEMPESTADES - CONFORME Nº 2 DA CLÁUSULA 2ª DAS COND. GERAIS
15INUNDAÇÕES - CONFORME Nº 3 DA CLÁUSULA 2ª DAS COND. GERAIS
16FENÓMENOS SÍSMICOS - CONFORME Nº 4 DA CLÁUSULA 2ª DAS COND. GERAIS
17ALUIMENTO DE TERRAS - CONFORME Nº 5 DA CLÁUSULA 2ª DAS COND. GERAIS
18DEMOLIÇÃO E REMOÇÃO DE ESCOMBROS-(COND.ESPECIAL 120)
19PREJUÍZOS INDIRECTOS-(COND.ESPECIAL 121)
20PRIVAÇÃO TEMP.USO LOC.ARREND./OCUP. - F.HAB. (COND.ESPECIAL 122)
21PRIVAÇÃO TEMP.USO LOC.ARREND./OCUP. - F.PROF/RURAL. (COND.ESP.123)
22PERDAS DE RENDAS-(COND.ESPECIAL 124)
23R.CIVIL EMERGENTE INC. E/OU EXPLOSÃO - SENHORIO (COND.ESPECIAL 125)
24R.CIVIL EMERGENTE INC. E/OU EXPLOSÃO - INQUILINO (COND.ESPECIAL 126)
25DESENHOS E DOCUMENTOS-(COND.ESPECIAL 140)
26RISCOS ELÉCTRICOS-(COND.ESPECIAL 141)
27GREVES,TUMULTOS E ALTERAÇÃO DA O.PÚBLICA-(COND.ESPECIAL 142)
28ACTOS TERRORISMO - CONFORME CONDIÇÃO ESPECIAL ANEXA
29QUEDA DE AERONAVES-(COND.ESPECIAL 144)
30CHOQUE OU IMPACTO DE VEÍCULOS TERRESTRES-(COND.ESPECIAL 145)
31COMBUSTÃO ESPONTÂNEA-(COND.ESPECIAL 146)
32DANOS POR ÁGUA-(COND.ESPECIAL 150)
33DERRAME ACIDENTAL-(COND.ESPECIAL 151)
34DERRAME SIST.HIDRÁUL. DE PROTEC.INCÊNDIO-(COND.ESPECIAL 152)
35EXTRAVASAMENTO/DERRAME MAT. ESTADO FUSÃO-(COND.ESPECIAL 153)
36PRÉDIO DEVOLUTO-(CLAUS.PART. 900 ANEXA)
37CLÁUSULA DE BENFEITORIAS-(CLAUS.PART. 901 ANEXA)
38CLÁUSULA UNIFORME DE CO-SEGURO- CONFORME ANEXO
39CLÁUSULA APLICAVÉL VEÍCULO(S) SEGURO(S)-(CLAUS.PART 903 ANEXA)
40CREDOR HIPOTECÁRIO - (CONFORME CLÁUSULA PARTICULAR ANEXA)
41ACTOS VANDALISMO OU  MALICIOSOS, (CONDIÇÃO  ESPECIAL 147)
42APÓLICE DE PRÉMIO VARIÁVEL/APÓLICE ABERTA (COND.ESP. 015)XXX
43PROPRIEDADE HORIZONTAL-SEG.ADMN.CONDOMÍNIO (COND.ESP. 014)
99CONFORME NOTA ANEXA.
**                                                         TABCL02
01APÓLICE DE PRÉMIO VAR./APÓLICE ABERTA - CONF.COND.ESP.ANEXA
02SEGURO ESCOLAR - CONF.COND.ESP.ANEXA
03RISCOS PROFISSIONAIS E EXTRA-PROFISSIONAIS
04RISCOS PROFISSIONAIS
05RISCOS EXTRA-PROFISSIONAIS
06ACIDENTES PESSOAIS DESPORTO, CULTURA E RECREIO - CONF.COND.ESP.ANEXA
07APÓLICE NÃO NOMINATIVA
**                                                         TABCL03
01INCLUI COND.ESPECIAL 001 ANEXA - FAMILIAR
60RESPONSA.CIVIL GERAL NOS TERMOS DAS CONDIÇÕES GERAIS DA APÓLICE
64R.C.DETENTORES DE ANIMAIS DE COMPANHIA CONFORME COND.GERAIS DA APÓLICE
**                                                         TABCL04
01INCLUI COND.ESPECIAL OO1 ANEXA - FAMILIAR
02INCLUI COND.ESPECIAL 002 ANEXA - PROPRIETÁRIO DE IMÓVEIS
03INCLUI COND.ESPECIAL 003 ANEXA - ESTABELECIMENTOS COMERCIAIS
04INCLUI COND.ESPECIAL 004 ANEXA - CONSTRUÇÃO CIVIL
05INCLUI COND.ESPECIAL 005 ANEXA - GADO
06INCLUI COND.ESPECIAL 006 ANEXA-R.C.ANIMAIS
07INCLUI COND.ESP.OO7 ANEXA - AUTOMÓVEIS-S.VENDAS,OFIC.REP., EST.SERVIÇO
08INCLUI COND.ESPECIAL 008 ANEXA - CÂMARAS MUNICIPAIS
09INCLUI COND.ESPECIAL 009 ANEXA - MÉDICOS
10INCLUI COND.ESPECIAL 010 ANEXA - ADVOGADOS
11INCLUI COND.ESPECIAL 011 ANEXA - ENFERMEIROS
12INCLUI COND.ESPECIAL 012 ANEXA - FOGO DE ARTIFÍCIO-FOGUETES, MORTEIROS
13INCLUI COND.ESPECIAL 019 ANEXA - NOTÁRIOS
14INCLUI COND.ESPECIAL 020 ANEXA - JUNTAS DE FREGUESIA
15INCLUI COND.ESPECIAL 021 ANEXA - UTILIZAÇÃO DE EXPLOSIVOS
16INCLUI COND.ESPECIAL ANEXA - CLÍNICAS
17INCLUI COND.ESPECIAL ANEXA - ELEVADORES
18INCLUI COND.ESPECIAL ANEXA - HOSPITAIS
19INCLUI COND.ESPECIAL ANEXA - PARQUES ESTACIONAMENTO AUTOMÓVEL
20INCLUI COND.ESPECIAL ANEXA - RESPONSABILIDADE CIVIL EMPREGADOR
21INCLUI COND. ESPECIAL ANEXA-R.C. CÃES
22INCLUI COND.ESPECIAL 016 ANEXA - ENGENHEIROS / PROJECTISTAS
23INCLUI COND. ESPECIAL ANEXA-R.C. ESPAÇOS JOGO E RECREIO
24INCLUI COND. ESPECIAL ANEXA-R.C. AGENTES E CORRETORES DE SEGUROS
25INCLUI COND.ESPECIAL 014 ANEXA - ARQUITECTOS
26INCLUI COND.PARTICULAR ANEXA-R.C.GERAL INFRA ESTRUT. SUBTERRÂNEAS
27INCLUI COND.ESPECIAL 015 ANEXA - ENGENHEIROS
28INCLUI COND.ESPECIAL 024 ANEXA - PROFESSORES EDUCAÇÃO FÍSICA E AFINS
29INCLUI COND.ESPECIAL 023 ANEXA - SOLICITADORES
30INCLUI CLÁUSULA ANEXA - MÁQUINAS EM LABORAÇÃO
31INCLUI CLÁUSULA ANEXA - CABELEIREIROS E SALÕES DE BELEZA
32INCLUI CLÁUSULA ANEXA - RESPONSABILIDADE CIVIL CRUZADA
33INCLUI CLÁUSULA ANEXA - ABATE, ARRANQUE E CORTE DE ÁRVORES
34INCLUI CLÁUSULA ANEXA - RETIRADA DE PRODUTOS
35INCLUI CLÁUSULA ANEXA - EMPRESAS DE LIMPEZA
36INCLUI COND.PARTICULAR ANEXA - R.C.TRANSPORTADOR
37INCLUI CLÁUSULA ANEXA - ESTABELECIMENTOS DE ENSINO
38INCLUI CLÁUSULA ANEXA - EMPRESAS TRABALHO TEMPORÁRIO
39INCLUI CLÁUSULA ANEXA - POLUIÇÃO SÚBITA E ACIDENTAL
40APÓLICE SUJ.ÁS EXCLUSÕES ESPECÍFICAS EM ANEXO-ORG.DE EVENTOS
41INCLUI CLÁUSULA ANEXA - INDÚSTRIA HOTELEIRA
42INCLUI CLÁUSULA ANEXA - ESPECTÁCULOS
43INCLUI CLÁUSULA ANEXA - SOCIEDADES E CLUBES DESPORTIVOS
44INCLUI CLÁUSULA ANEXA -  EMPRESAS DE ANIMAÇÃO TURÍSTICA
45INCLUI CLÁUSULA ANEXA-ACTIVIDADES DE INSTALAÇÃO,REPARAÇÃO E MANUTENÇÃO
46INCLUI CLÁUSULA ANEXA - INEFICÁCIA - NON EFFICACY
50APÓLICE DE PRÉMIO VARIÁVEL EM FUNÇÃO DE TAXA DE AJUSTE
60RESPONS.CIVIL GERAL NOS TERMOS DAS CONDIÇÕES GERAIS DA APÓLICE
61RESPONSABILIDADE CIVIL EXPLORAÇÃO
62R.C. EXPLORAÇÃO E PRODUTOS - BASE OCORRÊNCIA CONF.COND.GERAIS APÓLICE
63R.C. EXPLORAÇÃO E PRODUTOS - BASE RECLAMAÇÃO CONF.COND.GERAIS APÓLICE
64R.C.DETENTORES DE ANIMAIS DE COMPANHIA CONFORME COND.GERAIS DA APÓLICE
65R.C.AGENTES E CORRETORES DE SEGUROS CONFORME COND.GERAIS DA APÓLICE
66R.C. ENTIDADES RESPONS.POR EQUIP.DESPORTIVOS CONF.COND.GERAIS APÓLICE
67RESPONS.CIVIL ESPAÇO DE JOGO/RECREIO CONFORME COND.GERAIS DA APÓLICE
68R.C. TRANSPORTADOR RODOVIÁRIO NAC.MERCADORIAS CONF.COND.GERAIS APOLICE
69RESPONS.CIVIL ADMINISTRADORES E DIRECTORES  CONF.COND.GERAIS APÓLICE
70RESPONS.CIVIL AUDITORES INDEPENDENTES CONFORME COND.GERAIS APÓLICE
71RESPONS.CIVIL INSTALADORES REDES DE GÁS CONFORME COND.GERAIS APÓLICE
72R.C.ENTIDADES MANUTENÇÃO DE ASCENSORES CONFORME COND.GERAIS APÓLICE
73R.C.ENTIDADES MEDIADORAS IMOBILIÁRIAS CONFORME COND.GERAIS APÓLICE
74RESPONS.CIVIL INSTALAÇÕES DE GÁS CONFORME CONDIÇÕES GERAIS APÓLICE
75RESPONSABILIDADE CIVIL PRESTAMISTAS CONFORME CONDIÇÕES GERAIS APÓLICE
76RESPONS. CIVIL SEGURANÇA PRIVADA CONFORME CONDIÇÕES GERAIS APÓLICE
77RESPONS. CIVIL EMPRESAS TRANSITÁRIAS CONFORME COND.GERAIS APÓLICE
**                                                          TABCL05
01FENÓMENOS SÍSMICOS, CONFORME CONDIÇÃO ESPECIAL 01.
02GREVES,TUMULTOS E ALT. DE ORDEM PÚBLICA CONF.COND.ESP. 02.
03A.TERRORISMO,VANDALISMO,MALIC./SABOT. CONF.COND.ESP. 03.
04RISCOS ELÉCTRICOS, CONFORME CONDIÇÃO ESPECIAL 04.
05ACTOS VANDALISMO, MALICIOSOS, CONFORME COND.ESPECIAL ANEXA
06ALUIMENTO DE TERRAS, CONFORME COND. ESPECIAL ANEXA
07ACTOS DE TERRORISMO, CONFORME COND. ESPECIAL ANEXA
99RESP.CIVIL FAMILIAR, CONFORME CONDIÇÃO ESPECIAL 05.
**                                                         TABCL06
01PROPRIEDADE HORIZONTAL
02CONSTRUÇÕES FEITAS EM TERRENO ALHEIO
03DANOS EM BENS DO SENHORIO
04ACTUALIZAÇÃO DE CAPITAIS, TIPO INDEXADA
05APÓLICES DE CAPITAL VARIÁVEL
06ACTUALIZ. DE CAPITAIS,TIPO CONVÊNCIONADA
07VALOR DE SUSTITUIÇÃO (EQUIP.INDUST.)
08DESCONTOS P/SIST.PREVENÇÃO/PROTECÇÃO CONTRA INCÊNDIO
09OFICINAS E FABRICAS SEM LABORAÇÃO
10FRACCIONAMENTO DO PRÉMIO
11ACTUALIZAÇÃO PROGRESSIVA DE CAPITAL
12INCL.NOVOS BENS OU BENEF. JÁ EXISTENTES
13REGIME DE FRANQUIAS
14TEMPESTADES
15INUNDAÇÕES
16FENÓMENOS SÍSMICOS
17ALUIMENTOS DE TERRA
18DEMOLIÇÃO E REMOÇÃO DE ESCOMBROS
19PREJUÍZOS INDIRECTOS
20PRIVAÇÃO TEMPORÁRIO USO LOCAL ARRENDADO/OCUPADO
21PRIVAÇÃO TEMPORÁRIO USO LOCAL ARRENDADO/OCUPADO
22PERDAS DE RENDAS
23R. CIVIL EMERGENTE DE INCÊNDIO E/OU EXPLOSÃO
24R. CIVIL EMERGENTE DE INCÊNDIO E/OU EXPLOSÃO
25DESENHOS E DOCUMENTOS
26RISCOS ELÉCTRICOS
27GREVES,TUMULTOS E ALTERAÇÃO DE ORDEM PÚBLICA
28A.TERRORISMO,VANDALISMO,MALICIOSOS/SABOTAGEM
29QUEDA DE AERONAVES
30CHOQUE OU IMPACTO DE VEÍCULOS TERRESTRES
31COMBUSTÃO ESPONTÂNEA
32DANOS POR ÁGUA
33DERRAME ACIDENTAL
34DERRAME SIST.HIDRÁUL. DE PROTEC.INCÊNDIO
35EXTRAVASAMENTO/DERRAME MATERIAL EM ESTADO DE FUSÃO
36PRÉDIO DEVOLUTO
37CLÁUSULA DE BENFEITORIAS
38CLÁUSULA UNIFORME DE CO-SEGURO
39CLÁU. APLICAVÉL AO(S) VEÍCULO(S) SEGURO(S)
40CRÉDITO HIPOTECÁRIO
41ACTOS DE VANDALISMO, MALICIOSOS, CONDIÇÃO ESPECIAL ANEXA
93INCLUI CLÁUSULA "LEEWAY CLAUSE"
94CONFORME APÓLICE DE INCÊNDIO E ELEMENTOS DA NATUREZA
95CONFORME APÓLICE DE AVARIAS DE MÁQUINAS
96CONFORME APÓLICE DE EQUIPAMENTO ELECTRÓNICO
97CONFORME APÓLICE DE RISCOS MULTIPLOS
98CONFORME CLÁUSULA DE VALOR DE TRESPASSE
99CONFORME NOTA ANEXA.
**                                                         TABCL15
01SEGUROS DE PRÉMIO VARIÁVEL (CONDIÇÃO ESPECIAL 01)
02CONSTRUÇÃO CIVIL DE EDÍFICIOS (CONDIÇÃO ESPECIAL 02)
03SEGURO DE AGRICULTURA (CONDIÇÃO ESPECIAL 03)
04DESCONTOS/AGRAVAMENTOS P/ANÁLISE DO RISCO (COND.ESPECIAL 04)
05DESCONTOS POR BAIXA SINISTRALIDADE (CONDIÇÃO ESPECIAL 05)
06 (RESERVADO TEXTO TXT2,12 a TXT2,15 TRABALHADORES NO ESTRANGEIRO)
07 (RESERVADO TEXTO TXT2,16 a TXT2,24 TRABALHADORES NOS E.U.A.)
**                                                         TABCL17
01FENÓMENOS SÍSMICOS (COND. ESPECIAL 01)
02TRANSPORTES TERRESTRES (COND. ESPECIAL 02)
03CUSTOS RECONST.MEMÓRIAS EXTERNAS (COND. ESPECIAL 03)
04DESPESAS ADICIONAIS DE LABORAÇÃO (COND. ESPECIAL 04)
05RISCO DE GREVES, ASSALTOS E TUMULTOS (COND. ESPECIAL 05)
06CLAUSULA PARTICULAR-EQUIPAMENTOS MÓVEIS OU PORTÁTEIS
99CONFORME NOTA ANEXA
**                                                         TABCL19
01REMOÇÃO DE ESCOMBROS (COND. ESPECIAL 01)
02DESP.EXTRAS C/TRAB.EXTRAOR./TRANSP.ESPEC. (COND.ESPECIAL 02)
03GREVES,TUMULTOS,C.CIVIS,ACT.TERR./SABOT. (COND.ESPECIAL 03)
04ACID.CONSEQ. DE ERRO,PROJ.,DESENV./CÁLC. (COND.ESPECIAL 04)
05ENSAIOS APÓS MONTAGEM (COND. ESPECIAL 05)
06PERÍODO GARANTIA (GARANTIA DE VISITA) - (COND. ESPECIAL 06)
07PERÍODO GARANTIA (GARANTIA ALARGADA)  - (COND. ESPECIAL 07)
08PERDAS/DANOS BENS ADJACENTES-OUTRAS PROP.(COND. ESPECIAL 08)
09RISCO DE FABRICANTES (COND. ESPECIAL 09)
10RESPONSABILIDADE CIVIL CRUZADA (COND.ESPECIAL 10)
11MÁQ.EQUIP. E INST.AUX. DA EXEC.TRABALHOS (COND. ESPECIAL 11)
12ACTOS VANDALISMO, MALICIOSOS, CONFORME COND.ESPECIAL ANEXA
13APÓLICE DE PRÉMIO VAR./APÓLICE ABERTA - CONF.COND.ESP. ANEXA
90INCLUI FENÓMENOS SÍSMICOS
99CONFORME NOTA ANEXA
**                                                         TABCL55
01FENÓMENOS SÍSMICOS (CONDIÇÃO ESPECIAL 01)
02GREVES, TUMÚLTOS E ALTERAÇ. DE O.PÚBLICA (COND. ESPECIAL 02)
03ACTOS DE TERRORISMO,VANDALISMO,SABOTAGEM (COND. ESPECIAL 03)
04RISCO ELÉCTRICO (COND. ESPECIAL 04)
05PREJUIZOS INDIRECTOS (COND. ESPECIAL 05)
06ENCARGOS PERMANENTES (COND. ESPECIAL 06)
07ACTUALIZAÇÃO CONVENCIONADA DE CAPITAIS (CLÁU. ESPECIAL 002)
08APÓLICES DE CAPITAL VARIÁVEL-FLUTUANTES (CLÁU. ESPECIAL 003)
09APÓLICES EM REGIME DE CO-SEGURO (CLÁU. ESPECIAL 004)
10VALOR DE SUBSTITUIÇÃO EM NOVO-MOB./EQUIP. (CL. ESPECIAL 001)
11ACTOS VANDALISMO, MALICIOSOS, CONFORME COND.ESPECIAL ANEXA
12ALUIMENTO DE TERRAS, CONFORME COND. ESPECIAL ANEXA
13ACTOS DE TERRORISMO, CONFORME COND. ESPECIAL ANEXA
14APÓLICE DE PRÉMIO VAR./APÓLICE ABERTA - CONF.COND.ESP. ANEXA
99CONFORME NOTA ANEXA
**                                                         TCATV
1LIGEIRO PARTICULAR
2MISTOS/CAMINHETA PARTICULAR
3PESADOS
4VELOCIPEDE/CICLOMOTOR/MOTOCICLO
5DIVERSAS VIATURAS
6JEEP'S/TODO O TERRENO
7CARRINHA TIPO FAMILIAR
**                                                         TCIL
10050
20500
31500
42500
52500
**                                                         TPB
101600
202500
303500
410000
510000
**                                                         TC51
1FAMILIARES SEM CONDUTOR
2FAMILIARES COM CONDUTOR
3TODOS OS OCUPANTES
**                                                         TABDUR
1UM ANO E SEGUINTES
2TEMPORÁRIO
**                                                         AFP   (F.PAG.)
1PAGAMENTO ANUAL                                                             1
2PAGAMENTO SEMESTRAL                                                         1
3PAGAMENTO TRIMESTRAL                                                        1
4PAGAMENTO MENSAL                                                            1
5PAGAMENTO ÚNICO                                                             1
** TXT1  (TEXTOS)
OBJECTO(S) SEGURO(S)                                              CAPITAL    1
OBJECTO(S) SEGURO(S)                               TAXA %         CAPITAL    2
PESSOA(S) SEGURA(S)                                                          3
OBJECTO SEGURO                                      LIMITE MAX. INDEMNIZAÇÃO 4
OBJECTO(S) SEGURO(S)                                 CAPITAL/ESTIMATIVA ANUAL5
OBJECTO SEGURO                                                               6
MODALIDADE DO SEGURO                            DO VEÍCULO                   7
PESSOAS TRANSPORTADAS: MODALIDADE:                                           8
BENS SEGUROS:                                                     CAPITAL
** TXT2  (TEXTOS)
GARANTIA(S)                                                                  01
GARANTIA(S)                                                       CAPITAL    02
GARANTIA(S)                                         LIMITE MAX. INDEMNIZAÇÃO 03
GARANTIA(S)                                  CAPITAIS                        04
GARANTIA(S)                                  CAPITAIS        FRANQUIAS       05
GARANTIA(S)                                             LIMITE(S)/FRANQUIA(S)06
GARANTIA(S)                                          CAPITAL     Tx.COMERCIAL07
ESTE CONTRATO REGE-SE PELAS CONDIÇÕES GERAIS ANEXAS E PRESENTES CONDIÇÕES    08
PARTICULARES.                                                                09
TRABALHADOR POR CONTA PRÓPRIA.                                               10
TRABALHADOR POR CONTA DE OUTREM.                                             11
 FICAM GARANTIDOS OS ACIDENTES DE TRABALHO, OCORRIDOS EM TERRITÓRIO ESTRAN-  12
GEIRO; AS DESPESAS AÍ EFECTUADAS RELATIVAS A ASSISTÊNCIA MÉDICA, MEDICAMEN-  13
TOSA OU HOSPITALAR, FICARÃO A CARGO DA SEGURADORA.                           14
                                                                             15
PARA OS DEVIDOS EFEITOS SE DECLARA QUE:                                      16
 - AS COBERTURAS DO SEGURO, INCLUINDO AS DESPESAS MÉDICAS FEITAS NO ESTRAN-  17
GEIRO E OS CUSTOS DE REPATRIAMENTO, PASSARÃO A ABRANGER AUTOMATICAMENTE TODO 18
O PESSOAL SEGURO AO ABRIGO DESTA APÓLICE QUANDO  EM SERVIÇO EM ANGOLA, DESDE 19
QUE AS DESLOCAÇÕES NÃO IMPLIQUEM ALTERAÇÃO DAS RESPECTIVAS REMUNERAÇÕES.     20
 - QUANTO AO(S) FUNCIONÁRIO(S) QUE SE ENCONTRA(M) DESLOCADO(S) PELO PRAZO DE 21
UM ANO, OU OUTROS EM SITUAÇÃO SEMELHANTE (ESTATUTO DE TRABALHADOR DESLOCADO),22
PERMANECERÃO ABRANGIDOS PELAS GARANTIAS DA APÓLICE MEDIANTE INFORMAÇÃO PRÉVIA23
DOS RESPECTIVOS NOMES, SALÁRIOS AUFERIDOS E PERÍODO DA PERMANÊNCIA.          24
PARA OS DEVIDOS EFEITOS SE DECLARA QUE:                                      25
 - AS COBERTURAS DO SEGURO, INCLUINDO AS DESPESAS MÉDICAS FEITAS NO ESTRAN-  26
GEIRO E OS CUSTOS DE REPATRIAMENTO, PASSARÃO A ABRANGER AUTOMATICAMENTE TODO 27
O PESSOAL SEGURO AO ABRIGO DESTA APÓLICE  QUANDO EM SERVIÇO  EM  MOÇAMBIQUE, 28
DESDE QUE AS DESLOCAÇÕES NÃO IMPLIQUEM ALTERAÇÃO DAS RESPECTIVAS REMUNERAÇÕES29
 - QUANTO AO(S) FUNCIONÁRIO(S) QUE SE ENCONTRA(M) DESLOCADO(S) PELO PRAZO DE 30
UM ANO, OU OUTROS EM SITUAÇÃO SEMELHANTE (ESTATUTO DE TRABALHADOR DESLOCADO),31
PERMANECERÃO ABRANGIDOS PELAS GARANTIAS DA APÓLICE MEDIANTE INFORMAÇÃO PRÉVIA32
DOS RESPECTIVOS NOMES, SALÁRIOS AUFERIDOS E PERÍODO DA PERMANÊNCIA.          33
** TXT3  (TEXTOS)
                                                                             1
 COBERTURA BASE, CONFORME CLÁUSULA 2ª DAS CONDIÇÕES GERAIS.                  2
 CONFORME NOTA ANEXA                                                         3
 COBERTURA BASE, CONFORME CLÁUSULA 2ª DAS CONDIÇÕES GERAIS.  QUADRO ANEXO    4
** TXT4  (TEXTOS)
O CONTRATO VIGORA SEM FRANQUIA NOS RISCOS QUE GARANTEM  DANOS AO VEÍCULO
ASSINALADOS COM ESSA CONDIÇÃO ATÉ QUE O  VEÍCULO SEGURO ATINJA OITO ANOS
DE IDADE. PARA VEÍCULOS COM IDADE IGUAL OU SUPERIOR A 8 ANOS É APLICÁVEL
A FRANQUIA DE 3% NO MÍNIMO DE ¤ 188,00.
** TXT5  (TEXTOS)
DE ACORDO COM O DECRETO-LEI 214/97 CONJUGADO  COM A NORMA REGULAMENTAR Nº8/98 DO
INSTITUTO DE SEGUROS DE PORTUGAL, O CAPITAL  EM DANOS  PRÓPRIOS SERÁ ACTUALIZADO
AO LONGO DO CONTRATO E ATÉ QUE O VEÍCULO COMPLETE 9 ANOS DE ANTIGUIDADE,CONFORME
TABELA DE DESVALORIZAÇÃO ANEXA.
PARA VEÍCULOS NOVOS E EM CASO  DE PERDA TOTAL, MANTÉM-SE  O VALOR CONTRATADO NOS
PRIMEIROS 2 ANOS.
** TXT6  (TEXTOS)
QUANDO À DATA DO SINISTRO O CONDUTOR DO VEÍCULO SEGURO TIVER MENOS DE 25 ANOS E/
OU MENOS DE 2 ANOS DE CARTA E QUALQUER DOS CONDUTORES  DECLARADOS NA APÓLICE NÃO
TIVER ESSAS CARACTERÍSTICAS, APLICAR-SE-Á A SEGUINTE REGRA:
O VALOR DA FRANQUIA EM VIGOR  SERÁ DUPLICADO SENDO O RESPECTIVO  VALOR MÍNIMO 4%
DO CAPITAL SEGURO. ESTA DISPOSIÇÃO  APENAS SE APLICA AOS CONTRATOS CUJOS TOMADO-
RES SEJAM PESSOAS SINGULARES.
** textos das coberturas facultativas do ramo MERCANTILE - array MERC
 COBERTURAS FACULTATIVAS REFERIDAS NO ARTº 4º DAS CONDIÇÕES GERAIS:
   GREVES, TUMULTOS E ALTERAÇÕES DA ORDEM PÚBLICA
   ACTOS DE VANDALISMO OU MALICIOSOS
   PREJUÍZOS INDIRECTOS (CONTEÚDO)
   PERDA DE RENDAS (EDIFÍCIO)
   ALUIMENTO DE TERRAS
   RISCOS ELÉCTRICOS
   FENÓMENOS SÍSMICOS
   ENCARGOS FIXOS (CONTEÚDO)
   EQUIPAMENTO ELECTRÓNICO (CONTEÚDO)
   AVARIA DE MÁQUINAS (CONTEÚDO)
   DETERIORAÇÃO DE BENS REFRIGERADOS (CONTEÚDO)
   TRESPASSE (CONTEÚDO)
   ACTOS DE TERRORISMO
|
LIMITES DE INDEMNIZAÇÃO POR  SINISTRO E ANUIDADE E FRANQUIAS A  DEDUZIR, EM CASO
DE SINISTRO, NA INDEMNIZAÇÃO A PAGAR AO SEGURADO: CONFORME QUADRO ANEXO.
|
CONDIÇÕES ESPECIAIS E CLÁUSULAS APLICÁVEIS
 TÊM APLICAÇÃO NESTE CONTRATO AS CONDIÇÕES ESPECIAIS E CLÁUSULAS CUJOS
 NÚMEROS ABAIXO SE INDICAM:
   001. ACTUALIZAÇÃO CONVENCIONADA DE CAPITAIS
   002. APÓLICES DE PRÉMIO VARIÁVEL
   030. APÓLICES EM REGIME DE CO-SEGURO
   100. CREDOR HIPOTECÁRIO
   101. VALOR DE SUBSTITUIÇÃO EM NOVO (MOBILIÁRIO E EQUIPAMENTO)
   102. APÓLICES DE CAPITAL VARIÁVEL (FLUTUANTES)
   103. LIVROS
   133. EXISTÊNCIA DE PÓLVORA (ATÉ 25 KG)
   134. CUMPRIMENTO REGRAS SEGURANÇA PARA "ESTABELECIMENTOS QUE RECEBEM PÚBLICO"
   135. EXISTÊNCIA DE LÍQUIDOS INFLAMÁVEIS (ATÉ 100 LITROS)
   136. EXISTÊNCIA DE PRODUTOS INFLAMÁVEIS DIVERSOS (ATÉ 400 KG OU LITROS)
   137. LOTARIAS
** novos PRODUTOS                                     ARRAY MTR
MULTIRRISCOS HABITAÇÃO RESIDENTIA
MULTIRRISCOS COM.E SERV.MERCANTILE
MULTIRRISCOS CONDOMÍNIO
**
PERÍODO MÁXIMO DE INDEMNIZAÇÃO 5 DIAS, SEGUIDOS  OU INTERPOLADOS, NO MÁXIMO DE 3
OCORRÊNCIAS POR ANO.
**
01800RESPONSABILIDADE CIVIL OBRIGATÓRIA    ATÉ
01800RESPONSABILIDADE CIVIL POR DANOS CORPORAIS     ¤1.200.000
01800RESPONSABILIDADE CIVIL POR DANOS MATERIAIS     ¤  600.000
02500RESPONSABILIDADE CIVIL OBRIGATÓRIA    ATÉ   ¤2.500.000,00
03250RESPONSABILIDADE CIVIL OBRIGATÓRIA (ATÉ)
03250   DANOS CORPORAIS                        ¤ 2.500.000
03250   DANOS MATERIAIS                        ¤   750.000
06000RESPONSABILIDADE CIVIL OBRIGATÓRIA (ATÉ)
06000   DANOS CORPORAIS                        ¤ 5.000.000
06000   DANOS MATERIAIS                        ¤ 1.000.000
99999RESPONSABILIDADE CIVIL OBRIGATÓRIA (ATÉ)
99999   DANOS CORPORAIS                        ¤ 5.000.000
99999   DANOS MATERIAIS                        ¤ 1.000.000
99999RESPONSABILIDADE CIVIL FACULTATIVA (ATÉ)
99999   DANOS CORPORAIS E/OU MATERIAIS         ¤44.000.000
** CAT 1 7
  VEÍCULO SUBSTITUIÇÃO AVARIA EM PORTUGAL C.E.078-CC VEÍCULO SEGURO, ATÉ 2500CC.
  LIMITE MÁXIMO 5 DIAS /ANO, NO MÁXIMO DE 3 SINISTROS/ANO.
** CAT 6
  VEÍCULO SUBSTITUIÇÃO AVARIA EM PORTUGAL C.E.078-CC VEÍCULO SEGURO, ATÉ 2500CC.
  LIMITE MÁXIMO 5 DIAS/ANO, NO MÁXIMO DE 3 SINISTROS/ANO EXCLUSIVAMENTE PARA VEÍ
  CULOS LIGEIROS DE PASSAGEIROS.
** TXT7  (TEXTOS) do Ramo 55
 COBERTURA BASE, CONFORME ARTº 3º DAS CONDIÇÕES GERAIS.                      2
 COBERTURA BASE, CONFORME ARTº 3º DAS CONDIÇÕES GERAIS.      QUADRO ANEXO    4
 CONFORME NOTA ANEXA                                                         3
