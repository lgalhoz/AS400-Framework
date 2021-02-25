     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*===========================================================
     F*-----------------------------------------
     F*    Interface Standard
     F*-----------------------------------------
     F
     F
     F*-----------------------------------------
     F*    Recibos Corretor
     F*-----------------------------------------
     FSD1CG3    IF   F   15        DISK    RECNO(WR) EXTFILE('SD.1.CG3')
     FSD1AP     IF   F  319     7AIDISK    KEYLOC(1) EXTFILE('SD.1.AP')
     FSD1RCA    IF   F  316     6AIDISK    KEYLOC(5) EXTFILE('SD.1.RCA')
     FSD1RVD    IF   F   15     8AIDISK    KEYLOC(1) EXTFILE('SD.1.RVD')
     F
     F*-----------------------------------------
     F*    Recibos Lusitânia Azul
     F*-----------------------------------------
     FGCOBGR    IF   E           K DISK    PREFIX(REC_)
     FGFGUIAM1  IF   E           K DISK    PREFIX(AM1_)
     FGFNDEV1   IF   E           K DISK    PREFIX(DEV_)
     FGC506     IF   E           K DISK    PREFIX(GC5_)
     F*===========================================================
     D R#AP          E DS                  EXTNAME('SD.1.AP') PREFIX('AP_')
     D R#RCA         E DS                  EXTNAME('SD.1.REC') PREFIX('RCA_')
     D R#RVD         E DS                  EXTNAME('SD.1.RVD') PREFIX('RVD_')
     D R#CG3           DS
     D   CG3_CODMOV            1      2S 0
     D   CG3_FILLER            3      9A
     D   CG3_NO               10     15S 0
     D
     D
     D*==> Campos de Trabalho
     D WAction         S              1A
     D WR              S              5S 0
     D WMsgErro        S              7A
     D WNumOrdem       S              6S 0
     D
     D*==> Constantes
     D KTE#Companhia   C                   1
     D KTE#Corretor    C                   2
     D*===========================================================
      /FREE
        IF (WAction = 1);
           WR = KTE#Companhia;
           CHAIN WR SD1CG3 R#CG3;
           IF (%FOUND());
              CG3_NO += 1;
              WNumOrdem = CG3_NO;
              UPDATE SD1CG3 R#CG3;
              EXSR AddCompanhia;
           ENDIF;
        ELSE;
           WR = KTE#Corretor;
           CHAIN WR SD1CG3 R#CG3;
           IF (%FOUND());
              CG3_NO += 1;
              WNumOrdem = CG3_NO;
              UPDATE SD1CG3 R#CG3;
              EXSR AddCorretor;
           ENDIF;
        ENDIF;

        *INLR = *ON;

       //=========================================================
       // Subrotina.: AddCompanhia
       // Objectivo.: Criar um recibo manual na Companhia.
       //=========================================================
       BEGSR AddCompanhia;
       ENDSR;

       //=========================================================
       // Subrotina.: AddCorretor
       // Objectivo.: Criar um recibo manual no Corretor.
       //=========================================================
       BEGSR AddCorretor;
          //-------------------------------------------------
          // Criar um recibo no ficheiro de Recibos SD.1.RCA
          //-------------------------------------------------
          REC_TPCOB   = ' ';
          REC_CODFACT = ' ';
          REC_CODMOV  = 15;
          REC_NORD    = WNumOrdem;
          REC_APOLIC  = '<Apólice>';
          REC_NUMREC  = '<Recibo original>';
          REC_CCOMP   = '<Companhia do recibo>';
          REC_CSEGUR  = '<Código do Segurado>';
          REC_CRAMO   = '<Ramo do Recibo>';
          REC_DTINIC  = '<Data inicio do recibo>';
          REC_DTFIM   = '<Data Termo do Recibo>';
          REC_VTREC   = '<Valor do Recibo>';
          REC_VCOMIS  = '<Valor da Comissão>';
          REC_DTREC   = '<Data do Recibo>';
          REC_NUMGR   = *ZEROS;
          REC_DTGR    = *ZEROS;
          REC_DTENTR  = *ZEROS;
          REC_DTLIM   = *ZEROS;
          REC_NUMAD   = *ZEROS;
          REC_DTAVD   = *ZEROS;
          REC_CDF     = *ZEROS;
          REC_DVAR    = *ZEROS;
          REC_MCCOB   = *ZEROS;
          REC_COMIS1  = *ZEROS;
          REC_MSCOB   = *ZEROS;
          REC_COMIS2  = *ZEROS;
          REC_MED3    = *ZEROS;
          REC_COMIS3  = *ZEROS;
          REC_CGEST   = *ZEROS;
          REC_NODEV   = *ZEROS;
          REC_DTDEV   = *ZEROS;
          REC_DTCOB   = *ZEROS;
          REC_ORIGEM  = *ZEROS;
          REC_NRCOB   = *ZEROS;
          REC_DTRCOB  = *ZEROS;
          REC_USER    = *ZEROS;
          REC_DPAG    = *ZEROS;
          REC_FP      = *ZEROS;
          REC_NDP     = *ZEROS;
          REC_VPAG    = *ZEROS;
          REC_VDESC   = *ZEROS;
          REC_PRSIMP  = *ZEROS;
          REC_VALIE   = *ZEROS;
          REC_DIFPAG  = *ZEROS;
          REC_UNACHX  = *ZEROS;
          REC_CSEGX   = *ZEROS;
          REC_CARTA   = *ZEROS;
          REC_NORD2   = 1000000 + WNumOrdem;
          REC_NREC2   = *ZEROS;
          REC_FILL1   = *ZEROS;
          REC_FILL2   = *ZEROS;
          REC_FILL3   = *ZEROS;
          REC_CDFACT  = *ZEROS;
          REC_FILL4   = *ZEROS;
          REC_CDALT   = *ZEROS;
          REC_FILL5   = *ZEROS;
          REC_SNDAVD  = *ZEROS;
          REC_FILL6   = *ZEROS;
          REC_PANUL   = *ZEROS;
          REC_SAIDA   = *ZEROS;
          REC_ERC     = *ZEROS;
          REC_CODSIT  = *ZEROS;
          REC_CDEL    = *ZEROS;
          REC_FILL7   = *ZEROS;
          REC_CODEV   = *ZEROS;
          REC_SC      = *ZEROS;
          REC_CCONT   = *ZEROS;
          REC_FILL8   = *ZEROS;








       ENDSR;
      /END-FREE
