     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*=====================================================================
     H* Programa...: TRSF60
     H* Objectivo..: Carregar Interface diario de CobranÁas para fornecer
     H*              ‡ Lusitania.
     H*
     H* ObservaÁıes: Este programa È invocado diariamente no JOBSCDE
     H*=====================================================================
     F*=====================================================================
     FINTCOBD   UF A F  314        DISK    USROPN
     FSD1RCA    IF   F  326     6AIDISK    KEYLOC(5)
     F                                     EXTFILE('SD.1.RCA')
     FGBASEGRA  IF   E           K DISK    PREFIX(APL_)
     FGCOBGR    IF   E           K DISK    PREFIX(COB_)
     FGFSEG     IF   E           K DISK    PREFIX(PES_)
     D*===================================================================
      /Copy QRPGLESRC,SGRLO010

     D*---------------------------------------------
     D* Tabela de EquivalÍncia de Categorias
     D*---------------------------------------------
     D CVG             S              3  0 DIM(20) CTDATA PERRCD(1) ASCEND      CATEGORIAS DE VEÕCUL
     D CVL             S              4  0 DIM(20) ALT(CVG)
     D*---------------------------------------------
     D* Invocar um comando de AS/400 externo:
     D*---------------------------------------------
     D shell           PR                  EXTPGM('QCMDEXC')
     D   StdCmd                     200A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5
     D
     D WCPOSTAL        DS
     D   WCodComp              1      7S 0
     D   WCodigo               1      4S 0
     D   WExtensao             5      7S 0
     D
     D WStdCmd         S            200A
     D WStdLen         S             15P 5
     D WIcErro         S               N
     D WDatCobr        S              8S 0
     D WCodCatJM       S              3S 0
     D WIdx            S              2S 0
     D WDatDia         S              8S 0
     D
     D* Conversao de caracteres Portugueses
     D StrPT           C                   '·‡„‚¡¿√¬ÈËÍ…» ÌÏÓÕÃŒÛÚÙı+
     D                                      ”“‘’˘˙˚⁄Ÿ€Á«∫™'
     D StrISO          C                   'aaaaAAAAeeeEEEiiiIIIoooo+
     D                                      OOOOuuuUUUcC..'
     I*===================================================================
     IINTCOBD   NS  01
     I                                  1    8 0INT_DTINTEGR                    #01 Data IntegraÁ„o
     I                                  9   13  INT_NCONTA                      #02 N∫. Conta
     I                                 14   15  INT_CDACCAO                     #03 AcÁ„o
     I                                 16   21 0INT_CDRAMO                      #04 Ramo
     I                                 22   28 0INT_NUAPOL                      #05 ApÛlice
     I                                 29   35  INT_CERTIF                      #06 Certificado
     I                                 36   44 0INT_NIF                         #07 N∫.Contribuinte
     I                                 45   45  INT_CDTIPO                      #08 Tipo
     I                                 46   95  INT_NOME                        #09 Nome Pessoa
     I                                 96  103 0INT_DTNASC                      #10 Data Nascimento
     I                                104  118  INT_TPMORA                      #11 Tipo Morada
     I                                119  158  INT_MORADA                      #12 Morada
     I                                159  162 0INT_CPOST4                      #13 CÛdigo Postal
     I                                163  165 0INT_CPOST3                      #14 Localidade Postal
     I                                166  205  INT_LOCALID                     #15 Localidade
     I                                206  211 0INT_NURECIB                     #16 Recibo
     I                                212  219 0INT_NUPERINI                    #17 Inicio
     I                                220  227 0INT_NUPERFIM                    #18 Termo
     I                                228  239  INT_NUMATRIC                    #19 Matricula
     I                                240  289  INT_NUCHASS                     #20 Chassis
     I                                290  294 0INT_CDCATEG                     #21 Categoria
     I                                295  314  INT_NUSEGUR                     #22 N∫. SegurNet
     I
     I*---------------------------------
     I* Ficheiro di·rio de CobranÁas
     I*---------------------------------
     ISD1RCA    AA
     I                             A    1    2  REC_ALFA01
     I                             S    3    4 0REC_CODMOV
     I                             S    5   10 0REC_NUMORDEM
     I                             S   11   20 0REC_NUAPOL
     I                             S   24   28 0REC_NUMRECIB
     I                             S   29   31 0REC_CCOMP
     I                             S   32   37 0REC_CSEGUR
     I                             S   38   40 0REC_CRAMO
     I                             S   41   46 0REC_DTINI
     I                             S   47   52 0REC_DTFIM
     I                             P   53   59 2REC_VALREC
     I                             S  178  183 0REC_DTCOB
     I                             S  194  199 0REC_DTRCOB
     I                             S  309  309 0REC_CSIT
     I                             S  277  285 0REC_RECIBO
     C*===========================================================
      /FREE
        //------------------------------------------------
        // Formatar data do dia
        //------------------------------------------------
        WDatDia  = (%SubDt(%Date():*Years) * 10000) +
                   (%SubDt(%Date():*Months) * 100) +
                    %SubDt(%Date():*Days);


        //------------------------------------------------
        // Formatar data do dia
        //------------------------------------------------
        EXSR CrtIntCobD;
        IF (NOT WIcErro);
           OPEN INTCOBD;
           EXSR AddIntCobD;
           CLOSE INTCOBD;
        ENDIF;

        IF (NOT WIcErro);
           MONITOR;

              WStdCmd = 'QSYS/CPYTOSTMF FROMMBR(''/qsys.lib/qtemp.lib/+
                                     INTCOBD.FILE/INTCOBD.MBR'') +
                                     TOSTMF(''/interfaces/pendente/+
                                     BD_SEGURNET_JM_05700_'+
                      %CHAR(%subdt(%date():*YEARS))+'_'+
                      %CHAR(%subdt(%date():*MONTHS))+'_'+
                      %CHAR(%subdt(%date():*DAYS))+''') +
                                     STMFOPT(*REPLACE) +
                                     STMFCCSID(*STDASCII)';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);
           ON-ERROR;
              WIcErro = *ON;
           ENDMON;
        ENDIF;

        *INLR = *ON;
        Return;

        //==============================================================
        // Subrotina.: addIntCobD
        // DescriÁ„o.: Adicionar registos ao Interface de CobranÁas
        //             di·rias.
        //==============================================================
        BEGSR AddIntCobD;
           SETLL *LOVAL SD1RCA;
           READ SD1RCA;
           DOW (NOT %EOF());
              WDatCobr = $CvtData(REC_DTCOB:'DMY':'YYMD');
              IF (REC_CCOMP=54) AND (REC_CRAMO=310) AND (REC_CODMOV=15)
                  AND (REC_CSIT = 2) AND (WDatCobr = WDatDia);
                 INT_DTINTEGR = WDatDia;
                 INT_NCONTA   = '05700';
                 EXSR GetCodAccao;
                 INT_CDRAMO   = 310000;
                 INT_NUAPOL   = REC_NUAPOL;
                 INT_CERTIF   = *BLANKS;
                 INT_CDTIPO   = 'I'; // Individual

                 INT_NURECIB  = 800000 + REC_NUMRECIB;
                 INT_NUPERINI = $CvtData(REC_DTINI:'DMY':'YYMD');
                 INT_NUPERFIM = $CvtData(REC_DTFIM:'DMY':'YYMD');
                 CHAIN (50:INT_NUAPOL) GBASEGRA;
                 IF (%FOUND());
                    INT_NUMATRIC = APL_MATR1;
                    INT_NUCHASS  = *BLANKS;
                    EXSR GetCategoria;
                 ELSE;
                    INT_NUMATRIC = '????';
                    INT_NUCHASS  = '????';
                    INT_CDCATEG  = *ZEROS;
                 ENDIF;
                 INT_NUSEGUR  = *BLANKS;
                 EXSR GetSegurado;
                 EXCEPT ADDRec;
              ENDIF;
              READ SD1RCA;
           ENDDO;

        ENDSR;

        //==============================================================
        // Subrotina.: GetCodAccao
        // DescriÁ„o.: Obter o CÛdigo de Accao
        //==============================================================
        BEGSR GetCodAccao;
           CHAIN (REC_RECIBO) GCOBGR;
           IF (%FOUND());
              IF (COB_GCTIPR = 0);
                 INT_CDACCAO  = 'C1'; // Novo (C1)
              ELSEIF (COB_GCTIPR = 1);
                 INT_CDACCAO  = 'CA'; // Adicional (CA)
              ELSEIF (COB_GCTIPR = 2);
                 INT_CDACCAO  = 'CO'; // Continuado (CO)
               ENDIF;
           ENDIF;
        ENDSR;

        //==============================================================
        // Subrotina.: GetCategoria
        // DescriÁ„o.: Formatar a Categoria da Viatura
        //==============================================================
        BEGSR GetCategoria;
           INT_CDCATEG = *ZEROS;
           WCodCatJM = *ZEROS;
           WCodCatJM = (APL_CATV*100) + (APL_CL*10) + APL_PB;
           WIdx = *ZEROS;
           DOW (WIdx < 20);
              WIdx += 1;
              IF (WCodCatJM = CVG(WIdx));
                 INT_CDCATEG = CVL(WIdx);
              ENDIF;
           ENDDO;
        ENDSR;

        //==============================================================
        // Subrotina.: GetSegurado
        // DescriÁ„o.: Obter a informaÁ„o do Segurado
        //==============================================================
        BEGSR GetSegurado;
           CHAIN (APL_NSEG) FSEGF1;
           IF (%FOUND());
              INT_NIF     = PES_NIF;
              INT_NOME    = %XLATE(StrPT:StrISO:PES_NOM);
              INT_DTNASC  = PES_DTNASC;
              INT_TPMORA  = *BLANKS;
              INT_MORADA  = %XLATE(StrPT:StrISO:PES_MORAD);
              WCodComp    = PES_CPOST;
              INT_CPOST4  = WCodigo;
              INT_CPOST3  = WExtensao;
              INT_LOCALID = %XLATE(StrPT:StrISO:PES_DESCP);
           ELSE;
              INT_NIF     = *ZEROS;
              INT_NOME    = *BLANKS;
              INT_DTNASC  = *ZEROS;
              INT_TPMORA  = *BLANKS;
              INT_MORADA  = *BLANKS;
              INT_CPOST4  = *ZEROS;
              INT_CPOST3  = *ZEROS;
              INT_LOCALID = *BLANKS;
           ENDIF;
        ENDSR;

        //==============================================================
        // Subrotina.: CrtIntCobD
        // DescriÁ„o.: Construir o ficheiro Interface de CobranÁas
        //==============================================================
        BEGSR CrtIntCobD;
           //--------------------------------------------------------
           // Cria Script de TransferÍncia
           //--------------------------------------------------------
           MONITOR;
              WStdCmd = 'DLTF FILE(QTEMP/INTCOBD)';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);
           ON-ERROR;
           ENDMON;

           IF (NOT WIcErro);
              MONITOR;
                 WStdCmd = 'CRTPF FILE(QTEMP/INTCOBD) RCDLEN(314)';
                 WStdLen = %LEN(WStdCmd);
                 shell(WStdCmd:WStdLen);
              ON-ERROR;
                 WIcErro = *ON;
              ENDMON;
           ENDIF;

        ENDSR;
      /END-FREE
     C*====================================================================
     OINTCOBD   EADD         ADDREC
     O                       INT_DTINTEGR         8
     O                       INT_NCONTA          13
     O                       INT_CDACCAO         15
     O                       INT_CDRAMO          21
     O                       INT_NUAPOL          28
     O                       INT_CERTIF          35
     O                       INT_NIF             44
     O                       INT_CDTIPO          45
     O                       INT_NOME            95
     O                       INT_DTNASC         103
     O                       INT_TPMORA         118
     O                       INT_MORADA         158
     O                       INT_CPOST4         162
     O                       INT_CPOST3         165
     O                       INT_LOCALID        205
     O                       INT_NURECIB        211
     O                       INT_NUPERINI       219
     O                       INT_NUPERFIM       227
     O                       INT_NUMATRIC       239
     O                       INT_NUCHASS        289
     O                       INT_CDCATEG        294
     O                       INT_NUSEGUR        314
** 08-CATEGORIAS DE VEÕCULOS
1300100
1400100
1500100
2420500
2430600
2520500
2530600
3540800
3550800
3560800
4101300
4201200
4301200
5009990
5011400
5021400
5031400
5041400
6400100
7400100
