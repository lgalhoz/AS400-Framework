     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PCOBO203
     H* Descrição.: Manutenção de Apólices
     P*==================================================================
     FSD1AP     IF   F  320     7AIDISK    KEYLOC(1) EXTFILE('SD.1.AP')         Apólices Activas em J.Comp
     FSD1APM    IF   F  320     7AIDISK    KEYLOC(1) EXTFILE('SD.1.APM')        Apólice em Arquivo em J.Comp
     FSD1CR     IF   F  128     3AIDISK    KEYLOC(4) EXTFILE('SD.1.CR')         Ficheiro de Mediadores
     FFCOBE203  CF   E             WORKSTN PREFIX(ECR_)
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
     D R#AP1         E DS                  EXTNAME('SD.1.AP') PREFIX('AP_')
     D R#CR          E DS                  EXTNAME('SD.1.CR') PREFIX('MED_')
     D R#APKEY         DS
     D   AP_KEY                1     16
     D   APOLI                 1     10A
     D   CCOMP                11     13S 0
     D   CRAMO                14     16S 0
     D
     D*==> Campos de Trabalho
     D WDesOperacao    S            100A
     D WNoFunction     S             10A
     D WSelected       S               N
     D WExtKeys        S             24A
     D WNumOrdem       S              7S 0
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
     D*==================================================================*
     C     *ENTRY        PLIST
     C                   PARM                    WNumordem
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

       EXSR ShowApolice;

       *INLR = *ON;

       //========================================================
       // Subrotina.: ShowApolice
       // Objectivo.: Apresentar o detalhe da Apólice
       //========================================================
       BEGSR ShowApolice;
          *IN12 = *OFF;
          DOW (NOT *IN03 AND NOT *IN12);
             EXSR LoadData;
             WNoFunction = 'RCOBE20305'; // Mostrar o Detalhe

             $KeysToOmit(S#PGM:WNoFunction:WExtKeys);
             DS#HEADER   = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER   = $getFooter(S#PGM:WNoFunction:WExtKeys);
             DS#OPTIONS  = $getOption(S#PGM:WNoFunction:WExtKeys);
             ECR_S#PGM   = S#PGM;
             WRITE RCOBE20300;
             WRITE RCOBE20399;
             EXFMT RCOBE20305;
             IF ($ValidKey(Func#Key:KEY#F20));
                IF (WChamador = CAL#Recibos);
                ELSE;
                ENDIF;
             ELSEIF ($ValidKey(Func#Key:KEY#F08));
                MONITOR;
                   WComando = 'STRS36PRC PRC(SD612) CURLIB(LIB.SD)';
   29              ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
                ON-ERROR;
                ENDMON;
             ELSEIF ($ValidKey(Func#Key:KEY#F21));
                IF (WChamador = CAL#Anomalia);
                ENDIF;
             ELSEIF ($ValidKey(Func#Key:KEY#F22));
             ELSEIF (Func#Key = KEY#Enter);
                LEAVE;
             ENDIF;
          ENDDO;
       ENDSR;

       //========================================================
       // Subrotina.: LoadData
       // Objectivo.: Carregar a informação do Interface
       //========================================================
       BEGSR LoadData;
          WExtKeys = *ZEROS;
          CHAIN WNumOrdem SD1AP R#AP1;
          IF (NOT %FOUND(SD1AP));
             CHAIN WNumOrdem SD1APM R#AP1;
             IF (NOT %FOUND(SD1APM));
                LEAVESR;
             ENDIF;
          ENDIF;

          //-----------------------------------------------------------
          // Informação da Apólice
          //-----------------------------------------------------------
          ECR_NORD     = WNumOrdem;
          IF (WChamador = CAL#Recibos);
             IF (AP_DTFIM > *ZEROS);
                %SUBST(WextKeys:20:1) = '1';
                %SUBST(WextKeys:21:1) = '1';
                %SUBST(WextKeys:22:1) = '1';
             //ELSEIF (INT_CDESTAD >= '1') AND (INT_CDESTAD <= '3');
             ENDIF;
          ELSEIF (WChamador = CAL#Anomalia);
          ENDIF;
          ECR_DIMSR  = AP_DIMSR;
          ECR_CSEG   = AP_CSEG;
          ECR_CAPT   = AP_CAPT;
          ECR_CCOMP  = AP_CCOMP;
          ECR_DSCOMP = $GetDescricao(33:%TRIM(%EDITW(AP_CCOMP:'0   ')));
          ECR_NOME   = $getDescEnt(ECR_CSEG);
          ECR_CRAMO  = AP_CRAMO;
          ECR_APOLI  = AP_APOLI;
          ECR_CDFAC  = AP_CDFAC;
          ECR_DSFAC  = $GetDescricao(102:%CHAR(ECR_CDFAC));
          ECR_FRAC   = AP_FRAC;
          ECR_DSFRAC = $GetDescricao(101:ECR_FRAC);
          ECR_DTINI  = $CvtData(AP_DTINI:'DMY':'DMYY');
          IF (ECR_DTFIM > *ZEROS);
             ECR_DTFIM  = $CvtData(AP_DTFIM:'DMY':'DMYY');
             ECR_DSESTADO = 'Anulada'; // $GetDescricao(88:INT_CDESTAD);
          ELSE;
             ECR_DTFIM = *ZEROS;
             ECR_DSESTADO = 'Activa'; // $GetDescricao(88:INT_CDESTAD);
          ENDIF;

          //-----------------------------------------------------------
          // Plano de comissionamento e informação do Mediador
          //-----------------------------------------------------------
          ECR_ICDDIR = 'N';
          ECR_CDCOM = AP_CDCOM;
          ECR_COMTO = AP_COMTO;
          ECR_IMCOMTT = ((AP_COMTO * AP_PRSIN) / 100) + 0,005;
          ECR_RI1 = AP_RI1;
          ECR_RJ1 = AP_RJ1;
          ECR_IMCOMCC = (AP_RJ1 * AP_PRSIN) + 0,005;
          IF (AP_RI1 > *ZEROS);
             CHAIN AP_RI1 SD1CR R#CR;
             IF (%FOUND(SD1CR));
                ECR_NOMRI1 = MED_ABREV;
             ENDIF;
          ENDIF;

          ECR_RI2 = AP_RI2;
          ECR_RJ2 = AP_RJ2;
          ECR_IMCOMSC = (AP_RJ2 * AP_PRSIN) + 0,005;
          IF (AP_RI2 > *ZEROS);
             CHAIN AP_RI2 SD1CR R#CR;
             IF (%FOUND(SD1CR));
                ECR_NOMRI2 = MED_ABREV;
             ENDIF;
          ENDIF;

          //-----------------------------------------------------------
          // Valores Previstos
          //-----------------------------------------------------------
          ECR_PRTIN = AP_PRTIN;
          ECR_PRSIN = AP_PRSIN;

          //--------------------------------------------------------------
          // Outras Informações
          //--------------------------------------------------------------
          ECR_DFIX  = AP_DFIX;
          ECR_DESDFIX = $GetDescricao(98:%TRIM(%EDITW(ECR_DFIX:'0   ')));
          ECR_DVAR  = AP_DVAR;
          ECR_MATRI = AP_MATRI;
          ECR_UTIUL = AP_UTIUL;
          ECR_DTULT = $CvtData(AP_DTULT:'DMY':'DMYY');
          ECR_HORUL = AP_HORUL;
          ECR_NUMUL = AP_NUMUL;
          ECR_CDSUM = AP_CDSUM;
       ENDSR;
      /END-FREE
