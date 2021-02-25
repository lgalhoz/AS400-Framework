     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PCOBO207
     H* Descrição.: Facturar o recibo. Este programa foi adaptado, para
     H*             facturar qualquer tipo de recibo, e pode ser
     H*             executado na forma de Rotina ou Programa.
     P*==================================================================
     FSD1AP1    IF   F  320    16AIDISK    KEYLOC(8) EXTFILE('SD.1.AP1')        Apólices Activas em J.Comp
     FSD1AP2    IF   F  320    16AIDISK    KEYLOC(8) EXTFILE('SD.1.AP2')        Apólice em Arquivo em J.Comp
     FSD1CR     IF   F  128     3AIDISK    KEYLOC(4) EXTFILE('SD.1.CR')         Ficheiro de Mediadores
     FSD1RCA    IF   F  326     6AIDISK    KEYLOC(5) EXTFILE('SD.1.RCA')        Ficheiro de Recibos
     FFCATP205  IF   E           K DISK    PREFIX(CAT_)
     FFINTP028  UF   E           K DISK    PREFIX(INT_)
     FFINTP029  IF A E           K DISK    PREFIX(INT_)
     FFCOBE207  CF   E             WORKSTN PREFIX(ECR_)
     F                                     INFDS(INFD#KEYS)
     F*==================================================================*
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SCATO010
      /Copy QRPGLESRC,SSECO010
      /Copy QRPGLESRC,SENTO010
      /Copy QRPGLESRC,SGRLO013

     D*==> Registo de Apólices - Corretor
     D R#SD1RCA      E DS                  EXTNAME('SD.1.REC') PREFIX('REC_')
     D R#AP1         E DS                  EXTNAME('SD.1.AP1') PREFIX('AP_')
     D R#CR          E DS                  EXTNAME('SD.1.CR') PREFIX('MED_')
     D R#APKEY         DS
     D   AP_KEY                1     16
     D   APOLI                 1     10A
     D   CCOMP                11     13S 0
     D   CRAMO                14     16S 0
     D
     D*==> Rotina de Facturação
     D RunFactura      PR                  EXTPGM('RCOBO012')
     D   NumOrdRC                     9S 0 CONST
     D   NumOrdAP                     8S 0 CONST
     D   Forcar                       1A   CONST
     D   IsToFacturar                  N   CONST
     D   CodErro                      7A   CONST
     D   DescFixa01                   3S 0 CONST
     D   DescFixa02                   3S 0 CONST
     D   DescVaria                   35A   CONST
     D   CodFactura                   1S 0 CONST
     D   Origem                       6A   CONST
     D
     D*==> Campos de Trabalho
     D WDesOperacao    S            100A
     D WNoFunction     S             10A
     D WSelected       S               N
     D WExtKeys        S             24A
     D WCdRegis        S              9S 0
     D PCdRegis        S              9S 0
     D PMsgErro        S              7A
     D WMsgErro        S              7A
     D WDataDMY        S              6A
     D WFactor         S              4S 3
     D WIsHistorico    S               N
     D WNumOrdem       S              6S 0
     D WNumOrdAP       S              8S 0
     D WForcar         S              1A
     D WModo           S              1A
     D WTaxaCC         S              5S 5
     D WTaxaSC         S              5S 5
     D
     D
     D*==> Campos de Trabalho
     D MOD#Consulta    C                   'C'
     D MOD#Altera      C                   'A'
     D EST#Aberto      C                   'A'
     D CAL#Recibos     C                   12346
     D CAL#Anomalia    C                   12345
     D REC#Estorno     C                   21
     D REC#Premio      C                   15
     D REC#Indemniza   C                   43
     D REC#Compensa    C                   32
     D*==================================================================*
     C     *ENTRY        PLIST
     C                   PARM                    PCdRegis
     C                   PARM                    PMsgErro
      /FREE
       //------------------------------------
       // Validar o acesso a esta função
       //------------------------------------
       IF NOT $ValAccess(S#PGM:'':99999);
          *INLR = *ON;
          RETURN;
       ENDIF;

       EXSR ValParms;
       WModo = MOD#Consulta;
       IF (WCdRegis = *ZEROS);
          EXSR ShowAll;
       ELSE;
          EXSR ShowRecibo;
          IF (WMsgerro <> *BLANKS);
             // Actualizar recibo Standard
          ENDIF;
          PMsgErro = WMsgErro;
       ENDIF;

       *INLR = *ON;

       //========================================================
       // Subrotina.: ValParms
       // Objectivo.: Validar os Parâmetros Recebidos
       //========================================================
       BEGSR ValParms;
          WMsgErro = *BLANKS;
          MONITOR;
            WCdRegis = PCdRegis;
          ON-ERROR;
            WCdRegis = *ZEROS;
          ENDMON;

          MONITOR;
             WMsgErro = PMsgErro;
          ON-ERROR;
          ENDMON;
       ENDSR;

       //========================================================
       // Subrotina.: ShowAll
       // Objectivo.: Apresentar todos os recibos pendentes
       //========================================================
       BEGSR ShowAll;

       ENDSR;

       //========================================================
       // Subrotina.: ShowRecibo
       // Objectivo.: Apresentar o detalhe do Recibo
       //========================================================
       BEGSR ShowRecibo;
          *IN12 = *OFF;
          EXSR LoadData;
          IF (WMsgErro <> *BLANKS);
             LEAVESR;
          ENDIF;
          DOW (NOT *IN03 AND NOT *IN12);
             //--------------------------------------------------------------
             // Identificação do Header
             //--------------------------------------------------------------
             IF (REC_CODMOV = REC#Premio);
                IF (WModo = MOD#Consulta);
                   *IN50 = *OFF;
                   WNoFunction = 'RCOBE20705'; // Recibo de Prémio
                ELSE;
                   *IN50 = *ON;
                   WNoFunction = 'RCOBE20721'; // Recibo de Prémio
                ENDIF;
                *IN60 = *OFF;               // Ocultar parametros extra
             ELSEIF (REC_CODMOV = REC#Estorno);
                IF (WModo = MOD#Consulta);
                   *IN50 = *OFF;
                   WNoFunction = 'RCOBE20707'; // Recibo de Estorno
                ELSE;
                   *IN50 = *ON;
                   WNoFunction = 'RCOBE20723'; // Recibo de Estorno
                ENDIF;
                *IN60 = *ON;                // Apresentar parametros extra
             ELSEIF (REC_CODMOV = REC#Indemniza);
                IF (WModo = MOD#Consulta);
                   *IN50 = *OFF;
                   WNoFunction = 'RCOBE20708'; // Recibo de Indemnização
                ELSE;
                   *IN50 = *ON;
                   WNoFunction = 'RCOBE20724'; // Recibo de Indemnização
                ENDIF;
                *IN60 = *ON;                // Apresentar parametros extra
             ELSE;
                WMsgErro = 'COBXXXX';  // Tipo de recibo inválido
                LEAVESR;
             ENDIF;

             $KeysToOmit(S#PGM:WNoFunction:WExtKeys);
             DS#HEADER   = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER   = $getFooter(S#PGM:WNoFunction:WExtKeys);
             DS#OPTIONS  = $getOption(S#PGM:WNoFunction:WExtKeys);
             ECR_S#PGM   = S#PGM;
             WRITE RCOBE20700;
             WRITE RCOBE20799;
             EXFMT RCOBE20705;
             IF ($ValidKey(Func#Key:KEY#F04:*BLANKS:S#PGM:WNoFunction));
                EXSR RunPopUp;
             ELSEIF ($ValidKey(Func#Key:KEY#F21:*BLANKS:S#PGM:WNoFunction));
                *IN50 = *ON;
                WModo = MOD#Altera;
             ELSEIF ($ValidKey(Func#Key:KEY#F22:*BLANKS:S#PGM:WNoFunction));
             ELSEIF (Func#Key = KEY#F12);
                IF (WModo = MOD#Altera);
                   *IN12 = *OFF;
                   WModo = MOD#Consulta;
                ENDIF;
             ELSEIF (Func#Key = KEY#Enter);
                IF (WModo = MOD#Altera);
                   EXSR Alterar;
                   IF (WMsgErro = *BLANKS);
                      *IN12 = *OFF;
                      WModo = MOD#Consulta;
                      EXSR LoadData;
                   ENDIF;
                ELSE;
                   LEAVE;
                ENDIF;
             ENDIF;
          ENDDO;
       ENDSR;

       //========================================================
       // Subrotina.: LoadData
       // Objectivo.: Carregar a informação do Interface
       //========================================================
       BEGSR LoadData;
          WExtKeys = *ZEROS;
          %SUBST(WExtKeys:21:1) = '1';
          %SUBST(WExtKeys:22:1) = '1';

          //--------------------------------------------------------------
          // Obtém o número de ordem do Recibo Standard
          //--------------------------------------------------------------
          IF (WCdRegis > *ZEROS);
             WNumOrdem = *ZEROS;
             ECR_CDREGIS  = INT_CDREGIS;
             CHAIN(N) (WCdRegis) RINTP028;
             IF (%FOUND(FINTP028));
                IF (INT_CDESTAD = '4');
                   %SUBST(WExtKeys:21:1) = '0';
                ELSEIF (INT_CDESTAD = '5');
                   %SUBST(WExtKeys:22:1) = '0';
                ENDIF;

                IF (INT_NUMORDRC > 1000000);
                   WNumOrdem = (INT_NUMORDRC - 1000000);
                ELSE;
                   WNumOrdem = INT_NUMORDRC;
                ENDIF;
             ENDIF;
          ELSE;
             WNumOrdem = ECR_NUMORDEM;
          ENDIF;

          //--------------------------------------------------------------
          // Obter o número de Ordem
          //--------------------------------------------------------------
          IF (WNumOrdem <= *ZEROS);
             WMsgErro = 'COBXXXX';
             LEAVESR;
          ENDIF;

          CHAIN WNumOrdem SD1RCA R#SD1RCA;
          IF (NOT %FOUND(SD1RCA));
             WMsgErro = 'COBXXXX';
             LEAVESR;
          ENDIF;

          IF (REC_CODSIT <> *ZEROS);
             WMsgErro = 'COBXXXX';  // O Recibo tem de estar pendente
             LEAVESR;
          ENDIF;

          ECR_DSESTADO = $GetDescricao(94:%CHAR(REC_CODSIT));
          ECR_DSTPREC  = $GetDescricao(93:%TRIM(%EDITW(REC_CODMOV:'0  ')));

          //--------------------------------------------------------------
          // Informação do Recbo
          //--------------------------------------------------------------
          ECR_CDREGIS  = WCdRegis;

          ECR_CCOMP    = REC_CCOMP;
          ECR_DSCOMP   = $GetDescricao(33:%TRIM(%EDITW(REC_CCOMP:'0   ')));
          ECR_CDRAMO   = REC_CRAMO;
          ECR_NUAPOL   = REC_APOLIC;
          EXSR GetDescRamo;
          ECR_NUMSEG   = REC_CSEGUR;
          ECR_NOMESEG  = $GetDescEnt(ECR_NUMSEG);
          ECR_DTINICIO = $CvtData(REC_DTINIC:'DMY':'DMYY');
          ECR_DTTERMO  = $CvtData(REC_DTFIM:'DMY':'DMYY');
          ECR_DTENTRAD = $CvtData(REC_DTENTR:'DMY':'DMYY');
          ECR_DTLIMITE = $CvtData(%DEC(REC_DTLIM:6:0):'DMY':'DMYY');
          ECR_NUMREC   = REC_NREC2;
          ECR_NUMORDEM = REC_NORD2;

          //--------------------------------------------------------------
          // Codificação
          //--------------------------------------------------------------
          MONITOR;
             ECR_SNDAVD = %DEC(REC_SNDAVD:1:0);
          ON-ERROR;
             ECR_SNDAVD = *ZEROS;
          ENDMON;
          ECR_DSSNDAVD = $GetDescricao(102:%CHAR(ECR_SNDAVD));
          MONITOR;
             ECR_ORIGEM = %DEC(REC_ORIGEM:6:0);
          ON-ERROR;
             ECR_ORIGEM = *ZEROS;
          ENDMON;
          ECR_CDF      = REC_CDF;
          ECR_DSCDF    = $GetDescricao(98:%TRIM(%EDITW(ECR_CDF:'0   ')));
          MONITOR;
             ECR_FILL3 = %DEC(REC_FILL3:3:0);
          ON-ERROR;
             ECR_FILL3 = *ZEROS;
          ENDMON;
          ECR_DSFILL3  = $GetDescricao(98:%TRIM(%EDITW(ECR_FILL3:'0   ')));
          ECR_DVAR     = REC_DVAR;

          //--------------------------------------------------------------
          // Valores do Recibo
          //--------------------------------------------------------------
          ECR_IMTOTREC = REC_VTREC;
          ECR_IMPRCOM  = REC_PRSIMP;
          ECR_IMCOMISS = REC_VCOMIS;
          ECR_TXCOMISS  = (REC_VCOMIS / REC_PRSIMP);

          ECR_CODMEDIACC = REC_MCCOB;
          IF (ECR_CODMEDIACC > *ZEROS);
             ECR_COMMEDIACC = REC_COMIS1;
             ECR_TAXMEDIACC = (REC_COMIS1 / REC_PRSIMP);
             CHAIN ECR_CODMEDIACC SD1CR R#CR;
             IF (%FOUND(SD1CR));
                ECR_NOMMEDIACC = MED_ABREV;
             ENDIF;
          ELSE;
             ECR_NOMMEDIACC = *BLANKS;
             ECR_TAXMEDIACC = *ZEROS;
             ECR_CODMEDIACC = *ZEROS;
          ENDIF;

          ECR_CODMEDIASC = REC_MSCOB;
          IF (ECR_CODMEDIASC > *ZEROS);
             ECR_COMMEDIASC = REC_COMIS2;
             ECR_TAXMEDIASC = (REC_COMIS2 / REC_PRSIMP);
             CHAIN ECR_CODMEDIASC SD1CR R#CR;
             IF (%FOUND(SD1CR));
                ECR_NOMMEDIASC = MED_ABREV;
             ENDIF;
          ELSE;
             ECR_NOMMEDIASC = *BLANKS;
             ECR_TAXMEDIASC = *ZEROS;
             ECR_CODMEDIASC = *ZEROS;
          ENDIF;

          //--------------------------------------------------------------
          // Caracterizar a Apólice
          //--------------------------------------------------------------
          WIsHistorico = *OFF;
          APOLI = INT_NUAPOL;
          CCOMP = INT_CCOMP;
          CRAMO = INT_CDRAMO;
          CHAIN AP_KEY SD1AP1 R#AP1;
          IF (NOT %FOUND(SD1AP1));
             CHAIN AP_KEY SD1AP2 R#AP1;
             IF (NOT %FOUND(SD1AP2));
                WMsgErro = 'COBXXXX';
             ELSE;
                WIsHistorico = *ON;
             ENDIF;
          ENDIF;
          WTaxaCC = AP_RJ1;
          WTaxaSC = AP_RJ2;
          ECR_TAXMEDIACC = WTaxaCC;
          ECR_TAXMEDIASC = WTaxaSC;
          WNumOrdAP = AP_NORD;
       ENDSR;

       //========================================================
       // Subrotina.: GetDescRamo
       // Objectivo.: Obter a descrição do Ramo
       //========================================================
       BEGSR GetDescRamo;
          CHAIN (ECR_CCOMP:ECR_CDRAMO) RCATP205;
          IF (%FOUND(FCATP205));
             ECR_DSRAMO = CAT_DSRAMO;
          ELSE;
             ECR_DSRAMO = *BLANKS;      // Obter a descrição para o Ramo
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: Alterar
       // Objectivo.: Alterar o registo
       //========================================================
       BEGSR Alterar;
          RunFactura(WnumOrdem:WNumOrdAP:WForcar:*ON:WMsgErro:
                     ECR_CDF:ECR_FILL3:ECR_DVAR:ECR_SNDAVD:*BLANKS);
          IF (WMsgErro = *BLANKS);
             CHAIN (WCdRegis) RINTP028;
             IF (%FOUND(FINTP028));
                INT_CDESTAD = '5';
                INT_DTHSTAT = %DATE()+%TIME();
                UPDATE RINTP028 %FIELDS(INT_CDESTAD:INT_DTHSTAT);

                INT_NUOPERA  = $GetNextKey('FINTP029':'NUOPERA':
                            '(CDREGIS = '''+%CHAR(INT_CDREGIS)+''')');
                INT_CDERRO  = *BLANKS;
                INT_DTOPERA = %DATE();
                INT_HROPERA = %TIME();
                INT_USOPERA = S#USR;
                WRITE RINTP029;
             ENDIF;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: RunPopUp
       // Objectivo.: Obter uma lista de Valores Possíveis
       //========================================================
       BEGSR RunPopUp;
          IF (ECR_CM#NMFLD = 'SNDAVD');
             ECR_SNDAVD   = %DEC($runPopUp(102:%CHAR(ECR_SNDAVD)):1:0);
             ECR_DSSNDAVD = $GetDescricao(102:%CHAR(ECR_SNDAVD));
          ELSEIF (ECR_CM#NMFLD = 'CDF');
             ECR_CDF   = %DEC($runPopUp(98:%CHAR(ECR_CDF)):3:0);
             ECR_DSCDF = $GetDescricao(98:%TRIM(%EDITW(ECR_CDF:'0   ')));
          ELSEIF (ECR_CM#NMFLD = 'FILL3');
             ECR_FILL3   = %DEC($runPopUp(98:%CHAR(ECR_FILL3)):3:0);
             ECR_DSFILL3 = $GetDescricao(98:%TRIM(%EDITW(ECR_FILL3:'0   ')));
          ENDIF;
          ECR_CM#ROW = CM#CROW;
          ECR_CM#COL = CM#CCOL;
       ENDSR;
      /END-FREE
