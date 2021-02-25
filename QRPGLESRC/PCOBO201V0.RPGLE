     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PCOBO201
     H* Descrição.: Manutenção da Informação do Recibo Standard
     P*==================================================================
     FFINTP02501IF   E           K DISK    PREFIX(COM_)
     F                                     RENAME(RINTP025:RINTP02501)
     FFINTP023  IF   E           K DISK    PREFIX(COM_)
     FSD1AP1    IF   F  320    16AIDISK    KEYLOC(8) EXTFILE('SD.1.AP1')        Apólices Activas em J.Comp
     FSD1AP2    IF   F  320    16AIDISK    KEYLOC(8) EXTFILE('SD.1.AP2')        Apólice em Arquivo em J.Comp
     FSD1CR     IF   F  128     3AIDISK    KEYLOC(4) EXTFILE('SD.1.CR')         Ficheiro de Mediadores
     FSD1RE2    IF   E           K DISK    PREFIX(REC_)
     FFCATP205  IF   E           K DISK    PREFIX(CAT_)
     FFINTP028  UF   E           K DISK    PREFIX(INT_)
     FFINTP029  IF A E           K DISK    PREFIX(INT_)
     FFINTP022  IF   E           K DISK    PREFIX(INT_)
     FFCOBE201  CF   E             WORKSTN PREFIX(ECR_)
     F                                     INFDS(INFD#KEYS)
     F*==================================================================*
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SCATO010
      /Copy QRPGLESRC,SSECO010
      /Copy QRPGLESRC,SENTO010
      /Copy QRPGLESRC,SGRLO013

     D*==> Chamada de função Dynamic, para trabalhar detalhe do Item
     D ShellExec       PR                  EXTPGM('QCMDEXC')
     D   stdcmd                     300A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5 CONST
     D WComando        S            300A   INZ(*BLANKS)
     D
     D*==> Registo de Apólices - Corretor
     D R#AP1         E DS                  EXTNAME('SD.1.AP1') PREFIX('AP_')
     D R#CR          E DS                  EXTNAME('SD.1.CR') PREFIX('MED_')
     D R#APKEY         DS
     D   AP_KEY                1     16
     D   APOLI                 1     10A
     D   CCOMP                11     13S 0
     D   CRAMO                14     16S 0
     D
     D*==> Rotina para o registo Chamador
     D CallApolice     PR                  EXTPGM('PCOBO203')
     D   NumOrdem                     7S 0 CONST
     D   ModoIO                       1N   CONST
     D   WChamador                    5S 0 CONST
     D
     D*==> Rotina para o registo Chamador
     D ForceFase       PR                  EXTPGM(WDynamic)
     D   CodRecibo                    9S 0 CONST
     D   CodErro                      7A   CONST
     D WDynamic        S             10A
     D
     D*==> Campos de Trabalho
     D WCdEstado       S              1A
     D WNuOpera        S              5S 0
     D WDesOperacao    S            100A
     D WNoFunction     S             10A
     D WSelected       S               N
     D WExtKeys        S             24A
     D WCdRegis        S              9S 0
     D WModo           S               N
     D WChamador       S              5S 0
     D WMsgErro        S              7A
     D WDataDMY        S              6A
     D WFactor         S              4S 3
     D
     D*==> Campos de Trabalho
     D EST#Aberto      C                   'A'
     D CAL#Recibos     C                   12346
     D CAL#Anomalia    C                   12345
     D REC#Indemniza   C                   '7'
     D*==================================================================*
     C     *ENTRY        PLIST
     C                   PARM                    WCdRegis
     C                   PARM                    WModo
     C                   PARM                    WChamador
      /FREE
       //------------------------------------
       // Validar o acesso a esta função
       //------------------------------------
       IF NOT $ValAccess(S#PGM:'':99999);
          *INLR = *ON;
          RETURN;
       ENDIF;

       EXSR ShowRecibo;

       *INLR = *ON;

       //========================================================
       // Subrotina.: ShowRecibo
       // Objectivo.: Apresentar o detalhe do Recibo
       //========================================================
       BEGSR ShowRecibo;
          *IN12 = *OFF;
          DOW (NOT *IN03 AND NOT *IN12);
             EXSR LoadData;
             IF (INT_TPRECIB = REC#Indemniza);
                WNoFunction = 'RCOBE20108'; // Mostrar o Detalhe
             ELSE;
                IF (WChamador = CAL#Recibos);
                   WNoFunction = 'RCOBE20105'; // Mostrar o Detalhe
                ELSEIF (WChamador = CAL#Anomalia);
                   WNoFunction = 'RCOBE20107'; // Mostrar o Detalhe
                ENDIF;
             ENDIF;
             $KeysToOmit(S#PGM:WNoFunction:WExtKeys);
             DS#HEADER   = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER   = $getFooter(S#PGM:WNoFunction:WExtKeys);
             DS#OPTIONS  = $getOption(S#PGM:WNoFunction:WExtKeys);
             ECR_S#PGM   = S#PGM;
             WRITE RCOBE20100;
             WRITE RCOBE20199;
             IF (INT_TPRECIB = REC#Indemniza);
                EXFMT RCOBE20108;
             ELSE;
                EXFMT RCOBE20105;
             ENDIF;
             IF ($ValidKey(Func#Key:KEY#F07));
                CallApolice(INT_NUMORDAP:*ON:WChamador);
             ELSEIF ($ValidKey(Func#Key:KEY#F08));
                MONITOR;
                   WComando = 'STRS36PRC PRC(SD611) CURLIB(LIB.SD)';
   29              ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
                ON-ERROR;
                ENDMON;
             ELSEIF ($ValidKey(Func#Key:KEY#F09));
                MONITOR;
                   WComando = 'STRS36PRC PRC(SD095X) CURLIB(LIB.SD)';
   29              ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
                ON-ERROR;
                ENDMON;
             ELSEIF ($ValidKey(Func#Key:KEY#F10));
                MONITOR;
                   WComando = 'STRS36PRC PRC(SD071) CURLIB(LIB.SD)';
   29              ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
                ON-ERROR;
                ENDMON;
             ELSEIF ($ValidKey(Func#Key:KEY#F20:*BLANKS:S#PGM:WNoFunction));
                IF (WChamador = CAL#Recibos);
                   EXSR MudarEstado;
                ELSE;
                ENDIF;
             ELSEIF ($ValidKey(Func#Key:KEY#F21:*BLANKS:S#PGM:WNoFunction));
                IF (WChamador = CAL#Anomalia);
                ENDIF;
             ELSEIF ($ValidKey(Func#Key:KEY#F22:*BLANKS:S#PGM:WNoFunction));
                IF ($ShowDialog('COB0066':*BLANKS:'NS') = 'S');
                   EXSR Anular;
                ENDIF;
             ELSEIF ($ValidKey(Func#Key:KEY#F23:*BLANKS:S#PGM:WNoFunction));
                IF ($ShowDialog('COB0067':*BLANKS:'NS') = 'S');
                   EXSR Cobrar;
                ENDIF;
             ELSEIF ($ValidKey(Func#Key:KEY#F24:*BLANKS:S#PGM:WNoFunction));
                IF ($ShowDialog('COB0068':*BLANKS:'NS') = 'S');
                   EXSR Repor;
                ENDIF;
             ELSEIF (Func#Key = KEY#Enter);
                LEAVE;
             ENDIF;
          ENDDO;
       ENDSR;

       //========================================================
       // Subrotina.: Cobrar
       // Objectivo.: Marcar o registo para ser Cobrado
       //========================================================
       BEGSR Cobrar;
          CHAIN (ECR_CDREGIS) RINTP028;
          INT_CDESTAD = '6';
          INT_DTHSTAT = %DATE()+%TIME();
          UPDATE RINTP028 %FIELDS(INT_CDESTAD:INT_DTHSTAT);

          INT_NUOPERA  = $GetNextKey('FINTP029':'NUOPERA':
                         '(CDREGIS = '''+%CHAR(INT_CDREGIS)+''')');
          INT_CDERRO  = *BLANKS;
          INT_DTOPERA = %DATE();
          INT_HROPERA = %TIME();
          INT_USOPERA = S#USR;
          WRITE RINTP029;
       ENDSR;

       //========================================================
       // Subrotina.: Anular
       // Objectivo.: Marcar o registo para ser Anulado
       //========================================================
       BEGSR Anular;
          CHAIN (ECR_CDREGIS) RINTP028;
          INT_CDESTAD = '8';
          INT_DTHSTAT = %DATE()+%TIME();
          UPDATE RINTP028 %FIELDS(INT_CDESTAD:INT_DTHSTAT);

          INT_NUOPERA  = $GetNextKey('FINTP029':'NUOPERA':
                         '(CDREGIS = '''+%CHAR(INT_CDREGIS)+''')');
          INT_CDERRO  = *BLANKS;
          INT_DTOPERA = %DATE();
          INT_HROPERA = %TIME();
          INT_USOPERA = S#USR;
          WRITE RINTP029;
       ENDSR;

       //========================================================
       // Subrotina.: Repor
       // Objectivo.: Marcar o registo para ser Reposto na situação
       //             anterior.
       //========================================================
       BEGSR Repor;
          //------------------------------------------------------
          // Obter o estado anterior
          //------------------------------------------------------
          WNuOpera = $GetNextKey('FINTP029':'NUOPERA':
                     '(CDREGIS = '''+%CHAR(ECR_CDREGIS)+''')');
          IF (INT_NUOPERA < 3);
             INT_NUOPERA = 1;
          ELSE;
             INT_NUOPERA = WNuOpera - 2;
          ENDIF;
          CHAIN (ECR_CDREGIS:INT_NUOPERA) RINTP029;
          IF (%FOUND());
             WCdEstado = INT_CDESTAD;

             CHAIN (ECR_CDREGIS) RINTP028;
             INT_CDESTAD = WCdEstado;
             INT_DTHSTAT = %DATE()+%TIME();
             UPDATE RINTP028 %FIELDS(INT_CDESTAD:INT_DTHSTAT);

             INT_NUOPERA  = WNuOpera;
             INT_CDERRO  = *BLANKS;
             INT_DTOPERA = %DATE();
             INT_HROPERA = %TIME();
             INT_USOPERA = S#USR;
             WRITE RINTP029;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: LoadData
       // Objectivo.: Carregar a informação do Interface
       //========================================================
       BEGSR LoadData;
          WExtKeys = *ZEROS;
          CHAIN(N) (WCdRegis) RINTP028;
          IF (%FOUND(FINTP028));
             //--------------------------------------------------------------
             // Identifica do Header
             //--------------------------------------------------------------
             ECR_CDREGIS  = INT_CDREGIS;
             ECR_DSESTADO = $GetDescricao(88:INT_CDESTAD);
             ECR_DSTPREC  = $GetDescricao(91:INT_TPRECIB);
             %SUBST(WextKeys:07:1) = '1';
             %SUBST(WextKeys:08:1) = '1';
             %SUBST(WextKeys:09:1) = '1';
             %SUBST(WextKeys:10:1) = '1';
             %SUBST(WextKeys:14:1) = '1';
             %SUBST(WextKeys:22:1) = '1';
             %SUBST(WextKeys:24:1) = '1';
             //ECR_DSORIGEM = $GetDescricao(89:INT_ICORIG);
             IF (INT_TPRECIB <> REC#Indemniza);
                IF (WChamador = CAL#Recibos);
                   IF (INT_CDESTAD > '4');
                      %SUBST(WextKeys:20:1) = '1';
                      %SUBST(WextKeys:21:1) = '1';
                      IF (INT_CDESTAD <> '5');
                         %SUBST(WextKeys:22:1) = '1';
                         %SUBST(WextKeys:23:1) = '1';
                      ENDIF;
                      IF (INT_CDESTAD = '6') OR (INT_CDESTAD = '8');
                         %SUBST(WextKeys:24:1) = '0';
                      ENDIF;
                   ELSE;
                      %SUBST(WextKeys:23:1) = '1';
                   ENDIF;
                ELSEIF (WChamador = CAL#Anomalia);
                ENDIF;
             ENDIF;

             //--------------------------------------------------------------
             // Identifica do Recibo Original
             //--------------------------------------------------------------
             ECR_CCOMP    = INT_CCOMP;
             ECR_DSCOMP   = $GetDescricao(33:%TRIM(%EDITW(ECR_CCOMP:'0   ')));
             ECR_NOMSEG   = INT_NOMSEG;
             ECR_NOMBENEF = INT_NOMSEG;
             ECR_CDRMORG  = INT_CDRMORG;
             ECR_NUAPOL   = INT_NUAPOL;
             ECR_NUAPOLOR = INT_NUAPOLOR;
             ECR_NUMREC   = INT_NUMREC;
             ECR_NUMRECOR = INT_NUMRECOR;
             ECR_DTINICIO = $CvtData($DateToInt(INT_DTINICIO):'YYMD':'DMYY');
             ECR_DTTERMO  = $CvtData($DateToInt(INT_DTTERMO):'YYMD':'DMYY');
             ECR_DTENTRAD = $CvtData($DateToInt(INT_DTENTRAD):'YYMD':'DMYY');
             ECR_DTLIMITE = $CvtData($DateToInt(INT_DTLIMITE):'YYMD':'DMYY');

             //--------------------------------------------------------------
             // Informação do Recibo Standard
             //--------------------------------------------------------------
             *IN60 = *OFF;
             IF (INT_CDRAMO > *ZEROS);
                EXSR GetInfApolice;
             ENDIF;
             *IN62 = *OFF;
             IF (INT_NUMORDRC > *ZEROS);
                EXSR GetInfRecibo;
             ENDIF;

             //--------------------------------------------------------------
             // Importâncias do Recibo
             //--------------------------------------------------------------
             ECR_IMPRCOM  = INT_IMPRCOM;
             ECR_IMCAPITAL= *ZEROS;
             ECR_IMTOTREC = INT_IMTOTREC;

             //--------------------------------------------------------------
             //  Valores de Comissões do Recibo
             //--------------------------------------------------------------
             IF (INT_TPRECIB <> REC#Indemniza);
                ECR_IMCOMTOT = *ZEROS;
                ECR_IMCOMDIF = *ZEROS;
                ECR_PERCOM01 = *ZEROS;
                ECR_PERCOM02 = *ZEROS;
                SETLL (WCdRegis) RINTP02501;
                READE (WCdRegis) RINTP02501;
                DOW (NOT %EOF(FINTP02501));
                   IF (COM_CDESTAD = EST#Aberto);
                      CHAIN (COM_CDEXCEPT) RINTP023;
                      IF (COM_CDESTADO = EST#Aberto) AND (COM_TPEXCEPT = 'E');
                         ECR_IMCOMDIF = COM_DFTOCOM;
                         ECR_PERCOM01 = COM_PRTOTCL;
                         ECR_PERCOM02 = COM_PRTOTPR;
                         LEAVE;
                      ENDIF;
                   ENDIF;
                   READE (WCdRegis) RINTP02501;
                   ECR_IMCOMDIF = *ZEROS;
                ENDDO;
                ECR_IMCOMTOT = INT_IMCOMIS1 + INT_IMCOMIS2 + INT_IMCOMIS3;
                IF (ECR_IMCOMDIF = *ZEROS);
                   WFactor = 1;
                ELSE;
                   WFactor = 1,0005 + (ECR_IMCOMDIF/ECR_IMCOMTOT);
                ENDIF;

                ECR_IMCOMIS1 = INT_IMCOMIS1;
                ECR_COMJM1   = INT_IMCOMIS1 * WFactor;
                ECR_DIFCOM1  = ECR_COMJM1 - ECR_IMCOMIS1;
                ECR_IMCOMIS2 = INT_IMCOMIS2;
                ECR_COMJM2   = INT_IMCOMIS2 * WFactor;
                ECR_DIFCOM2  = ECR_COMJM2 - ECR_IMCOMIS2;
                ECR_IMCOMIS3 = INT_IMCOMIS3;
                ECR_COMJM3   = INT_IMCOMIS3 * WFactor;
                ECR_DIFCOM3  = ECR_COMJM3 - ECR_IMCOMIS3;
                ECR_IMCOMJM  = ECR_COMJM1 + ECR_COMJM2 + ECR_COMJM3;

                IF (ECR_PERCOM01 = *ZEROS) AND (ECR_IMPRCOM > *ZEROS);
                   ECR_PERCOM01 = ((ECR_IMCOMTOT/ECR_IMPRCOM)+0,000005)*100;
                   ECR_PERCOM02 = ECR_PERCOM01;
                ELSE;
                   ECR_PERCOM01 = *ZEROS;
                   ECR_PERCOM02 = *ZEROS;
                ENDIF;

                ECR_DSCOM1 = 'Cobrança..:';
                ECR_DSCOM2 = 'Ang.&Corr.:';
             ELSE;
                CHAIN (WCdRegis) RINTP022;
                ECR_BENEFNIF = INT_NUNIF;
                ECR_NUPROCES = INT_NUPROCES;
                ECR_DTPROCES = $CvtData($DateToInt(INT_DTPROCES):'YYMD':'DMYY');
             ENDIF;

             IF (INT_NUMORDRC > *ZEROS);
                %SUBST(WextKeys:09:1) = '0';
             ENDIF;

             //---------------------------------------------------------
             // Obter informação da Apólice
             //---------------------------------------------------------
             ECR_TAXMEDIACC = *ZEROS;
             ECR_CODMEDIACC = *ZEROS;
             ECR_NOMMEDIACC = *BLANKS;
             ECR_TAXMEDIASC = *ZEROS;
             ECR_CODMEDIASC = *ZEROS;
             ECR_NOMMEDIASC = *BLANKS;
             IF (INT_NUMORDAP > *ZEROS);
                %SUBST(WextKeys:07:1) = '0';
                %SUBST(WextKeys:08:1) = '0';
                %SUBST(WextKeys:10:1) = '0';
                APOLI = INT_NUAPOL;
                CCOMP = INT_CCOMP;
                CRAMO = INT_CDRAMO;
                CHAIN AP_KEY SD1AP1 R#AP1;
                IF (NOT %FOUND(SD1AP1));
                   CHAIN AP_KEY SD1AP2 R#AP1;
                   IF (NOT %FOUND(SD1AP2));
                      LEAVESR;
                   ENDIF;
                ENDIF;
                IF (AP_RI1 > *ZEROS);
                   ECR_CODMEDIACC = AP_RI1;
                   ECR_TAXMEDIACC = AP_RJ1;
                   IF (AP_RI1 > *ZEROS);
                      CHAIN AP_RI1 SD1CR R#CR;
                      IF (%FOUND(SD1CR));
                         ECR_NOMMEDIACC = MED_ABREV;
                      ENDIF;
                   ENDIF;
                ENDIF;
                IF (AP_RI2 > *ZEROS);
                   ECR_CODMEDIASC = AP_RI2;
                   ECR_TAXMEDIASC = AP_RJ2;
                   IF (AP_RI2 > *ZEROS);
                      CHAIN AP_RI2 SD1CR R#CR;
                      IF (%FOUND(SD1CR));
                         ECR_NOMMEDIASC = MED_ABREV;
                      ENDIF;
                   ENDIF;
                ENDIF;
                ECR_IMCAPITAL= AP_CAPT;
             ELSE;
                %SUBST(WExtKeys:14:1) = '0';
             ENDIF;
          ELSE;
             RETURN;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: GetInfApolice
       // Objectivo.: Obter informação da Apólice
       //========================================================
       BEGSR GetInfApolice;
          *IN60 = *ON;
          ECR_CDRAMO = INT_CDRAMO;
          ECR_NUMSEG = INT_NUMSEG;
          ECR_NOME   = $getDescEnt(INT_NUMSEG);
          CHAIN (ECR_CCOMP:ECR_CDRAMO) RCATP205;
          IF (%FOUND(FCATP205));
             ECR_DSRAMO = CAT_DSRAMO;
          ELSE;
             ECR_DSRAMO = *BLANKS;      // Obter a descrição para o Ramo
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: GetInfRecibo
       // Objectivo.: Obter informação sobre o estado do Recibo
       //========================================================
       BEGSR GetInfRecibo;
          *IN62 = *ON;
          WDataDMY = *BLANKS;
          ECR_NUMORDEM = INT_NUMORDRC;
          ECR_DTPAGAAN = $CvtData($dateToInt(INT_DTPAGAAN):'YYMD':'DMYY');
          SETLL (INT_CCOMP:INT_NUMREC) SD1RE2;
          READE (INT_CCOMP:INT_NUMREC) SD1RE2;
          DOW (NOT %EOF(SD1RE2));
             IF (INT_NUAPOL = REC_APOLIC);
                ECR_DSSTREC  = $GetDescricao(94:%CHAR(REC_CODSIT));
                IF (REC_CODSIT = 2);
                   WDataDMY = REC_DTCOB;
                ELSEIF (REC_CODSIT = 1) OR (REC_CODSIT = 3);
                   WDataDMY = REC_DTDEV;
                ENDIF;
                MONITOR;
                   ECR_DTPAGAAN = $CvtData(%DEC(WDataDMY:6:0):'DMY':'DMYY');
                ON-ERROR;
                   ECR_DTPAGAAN = *ZEROS;
                ENDMON;

                ECR_COMMEDCC = REC_COMIS1;
                ECR_COMMEDSC = REC_COMIS2;
                // Obter Tipo
               // Obter Data da Situação
               LEAVE;
             ELSEIF (REC_APOLIC = *BLANKS) OR (INT_NUAPOL = *BLANKS);
                LEAVE;
             ENDIF;
             READE (INT_CCOMP:INT_NUMREC) SD1RE2;
          ENDDO;
       ENDSR;

       //========================================================
       // Subrotina.: MudarEstado
       // Objectivo.: Mudar o estado do Recibo (fases)
       //========================================================
       BEGSR MudarEstado;
          WDynamic = *BLANKS;
          IF (INT_CDESTAD = '1');
             WDynamic = 'RINTO014';
             WDesOperacao = 'Ligar este recibo à Apólice (se esta já existir)';
          ELSEIF (INT_CDESTAD = '2');
             WDynamic = 'RINTO015';
             WDesOperacao = 'Localizar o Recibo em Demo Comp (se já existir)';
          ELSEIF (INT_CDESTAD = '3');
             IF (INT_TPRECIB <> REC#Indemniza);
             WDynamic = 'RINTO016';
             WDesOperacao = 'Criar o Recibo em Demo Comp';
             ENDIF;
          ELSEIF (INT_CDESTAD = '4');
             IF (INT_TPRECIB <> REC#Indemniza);
             WDynamic = 'PCOBO207';
             WDesOperacao = 'Facturar o Recibo';
             ENDIF;
          ENDIF;

          IF (WDynamic <> *BLANKS);
             IF ($ShowDialog('COB0046':%TRIM(WDesOperacao):'NS') = 'S');
                MONITOR;
                   ForceFase(INT_CDREGIS:WMsgErro);
                   IF (WMsgErro = 'INT0003');
                      IF ($ShowDialog(WMsgErro:*BLANKS:'NS') = 'S');
                         EXSR RelacaoManual;
                      ENDIF;
                   ELSEIF (WMsgErro <> *BLANKS);
                      $ShowDialog(WMsgErro);
                      LEAVESR;
                   ENDIF;
                ON-ERROR;
                   WMsgErro = 'COB0047';
                   $ShowDialog(WMsgErro:WDynamic);
                   LEAVESR;
                ENDMON;
             ENDIF;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: RelacaoManual
       // Objectivo.: Associar manualmente a apólice
       //========================================================
       BEGSR RelacaoManual;

       ENDSR;
      /END-FREE
