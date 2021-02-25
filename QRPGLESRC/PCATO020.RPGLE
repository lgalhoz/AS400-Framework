     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*===========================================================
     H* Programa..: PCATO020
     H* Descrição.: Carregar Informaçao dos Ramo Corretor
     H*===========================================================
     FFCATP205  UF A E           K DISK    PREFIX(CAT_)
     FSD1RM     IF   F  128     3AIDISK    KEYLOC(5) EXTFILE('QS36F/SD.1.RM')
     F*===========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010

     D*==> Registo de Apólices - Corretor
     D R#1RM           DS
     D   RM_CDRAMO             5      7  0
     D   RM_NOMRM              8     30
     D   RM_CDALT             31     31
     D   RM_ABREV             32     40
     D   RM_SEQSU             41     43  0
     D   RM_FIXOS             44     46P 0
     D   RM_ANGAR             47     49P 0
     D   RM_SELOS             50     52P 0
     D   RM_COBRA             53     55P 0
     D   RM_DESPE             56     58P 0
     D   RM_CORRE             59     61P 0
     D   RM_SA11             112    114  0
     D   RM_SA12             115    122
     D   RM_SA13             123    128  0
     D
     D*==> Campos de Trabalho
     D WCodCompanhia   S              5S 0
     D*=======================================================
      /FREE
         SETLL *LOVAL SD1RM;
         READ SD1RM R#1RM;
         DOW (NOT %EOF(SD1RM));
            IF (RM_CDRAMO <> *ZEROS);
               WCodCompanhia = 26;
               EXSR AddRecord;
               WCodCompanhia = 27;
               EXSR AddRecord;
            ENDIF;
            READ SD1RM R#1RM;
         ENDDO;

         *INLR = *ON;

         //=====================================================
         // Subrotina.: AddRecord;
         // Objectivo.: Adicionar um novo registo
         //=====================================================
         BEGSR AddRecord;
            CHAIN(N) (WCodCompanhia:RM_CDRAMO) RCATP205;
            IF (NOT %FOUND(FCATP205));
               CAT_CDCOMP  = WCodCompanhia;
               CAT_CDRAMO  = RM_CDRAMO;
               CAT_DSRAMO  = RM_NOMRM;
               CAT_NUSEQU  = 1;
               CAT_DTINICIO= $CvtData(RM_SA13:'DMY':'YYMD');
               CAT_DTTERMO  = 29991231;
               CAT_PRANGA   = RM_ANGAR; // Percentagem de Angariação
               CAT_PRCOBR   = RM_COBRA; // Percentgem de Cobrança
               CAT_PRCORR   = RM_CORRE; // Percentagem de Corretagem
               CAT_IMSELO   = RM_SELOS; // Imposto de Selo
               CAT_IMANPC   = *ZEROS;
               CAT_IMINEM   = *ZEROS;
               CAT_IMFGA    = *ZEROS;
               CAT_COEFIC   = *ZEROS;
               CAT_ENFIXO   = *ZEROS;
               CAT_CAT20501 = %DATE('2000-01-01');
               CAT_CAT20502 = %DATE('2999-12-31');
               CAT_CAT20503 = '1';
               CAT_CAT20504 = S#USR;
               WRITE RCATP205;
            ENDIF;
         ENDSR;
      /END-FREE
