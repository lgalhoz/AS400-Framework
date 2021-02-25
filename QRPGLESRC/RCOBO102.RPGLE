     H AUT(*ALL) ALWNULL(*USRCTL) DFTACTGRP(*NO) ACTGRP(*CALLER)
     H BNDDIR('JMDIR')
     H*==================================================================
     H* Programa..: RCOBO102
     H* DescriÁ„o.: Gerar Ficheiro XML com os registos
     H*==================================================================
     FGCOBGR    IF   E           K DISK    PREFIX(REC_)
     FGFSEG     IF   E           K DISK    PREFIX(SEG_)
     FFCOBT230  IF   E           K DISK    PREFIX(INP_)
     FTBIDDFF   UF A E           K DISK
     F*==================================================================
      /COPY QRPGLESRC,SGRLO010
      /COPY QRPGLESRC,SSECO010
      /COPY QRPGLESRC,SDBSO010

     D*==> Estrutura do NIB
     D                 DS
     D WStrNIB                 1     21S 0
     D WCodBanco               1      4A
     D WCodBalcao              5      8A
     D
     D*==> Campos de Trabalho
     D  WNuseqSaved    S              5S 0
     D  WMsgErro       S              7A
     D  WCodFileAct    S             10P 0
     D  WNumNIB        S             21S 0
     D  WMnteRecibo    S             13S 2
     D  WCodRecibo     S              9S 0
     D  WCodADC        S             11S 0
     D  WSitADC        S              1A
     D  WDatADC        S              8S 0
     D  WCodSegura     S              8S 0
     D  PCodFileAnt    S             10P 0
     D  PQtdeRegist    S              6S 0
     D  PMnteRegist    S             13S 2
     D  PDatDiaAlf     S              8S 0
     D  WCodFileAnt    S             10P 0
     D  WQtdeRegist    S              6S 0
     D  WMnteRegist    S             13S 2
     D  WDatDiaAlf     S              8S 0
     D
     D WNumRecord      S              5S 0
     D WNuSequ         S              5S 0
     D WNuSequSup      S              5S 0
     D WNuSequTmp      S              5S 0
     D WNumLevel       S              5S 0
     D WBlanks         S             40A
     D WSwiftCode      S              8A
     D WSession        S              5S 0
     D WADCSEPA        S             12A
     D WADCEsperado    S             12A
     D
     D*==> DefiniÁ„o de constantes
     D ADC#Registado   C                   '1'
     D ADC#Enviada     C                   '2'
     D ADC#SemEfeito   C                   '3'
     D ADC#Pendente    C                   '4'
     D ADC#Cancelada   C                   '5'
     D ADC#Alterada    C                   '6'
     D ADC#Aprovado    C                   '7'
     D SEPA#First      C                   'FRST'
     D SEPA#Next       C                   'RCUR'
     D StrPT           C                   '¡¿√¬·‡„‚…» ÈËÍÕÃŒÌÏÓ”“‘’ıÙÙı⁄Ÿ€˙˘˚+
     D                                      «Á&∫™<>'
     D StrISO          C                   'AAAAaaaaEEEeeeIIIiiiOOOOooooUUUuuu+
     D                                      Cce..[]'
     D*==================================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCodFileAct
     C                   PARM                    WNumNIB
     C                   PARM                    WMnteRecibo
     C                   PARM                    WCodRecibo
     C                   PARM                    WCodADC
     C                   PARM                    WADCSepa
     C                   PARM                    WSitADC
     C                   PARM                    WDatADC
     C                   PARM                    WCodSegura
     C                   PARM                    WNuseqSaved
     C                   PARM                    PDatDiaAlf
     C                   PARM                    WMsgErro
     C                   PARM                    PCodFileAnt
     C                   PARM                    PQtdeRegist
     C                   PARM                    PMnteRegist
      /FREE
       //------------------------------------------------
       // Criar o CabeÁalho se for o Primeiro Registo
       //------------------------------------------------
       WMsgErro = *BLANKS;
       IF (WSitADC = ADC#Enviada) OR (WSitADC = ADC#SemEfeito) OR
          (WSitADC = ADC#Cancelada);
          WMsgErro = 'COB0103';
          RETURN;
       ENDIF;

       //---------------------------------------------------
       // Assimur valores por defeito para a data CobranÁa
       //---------------------------------------------------
       MONITOR;
         WDatDiaAlf = PDatDiaAlf;
       ON-ERROR;
          WDatdiaAlf = $AddToDate($DateToInt(%DATE()):5:'D'); // Dia Seguinte
       ENDMON;

       //------------------------------------------------
       // Validar a ADC - Isto È tempor·rio e serve
       // apenas para certificar que o CÛdigo ADC est·
       // a ser bem forCompdo nas rotinas anteriores.
       //------------------------------------------------
       IF (WNumNIB > *ZEROS);
          IF (WADCSepa <> *BLANKS);
             WADCEsperado = ' ' + %TRIM(%EDITW(WCodADC:'0           '));
             IF (WADCSepa <> WADCEsperado);
                WADCEsperado = 'JMADC' + %CHAR(WCodADC);
                IF (WADCSepa <> WADCEsperado);
                   WMsgErro = 'COB0200';
                   RETURN;
                ENDIF;
             ENDIF;
          ELSE;
             WMsgErro = 'COB0201';
             RETURN;
          ENDIF;
       ENDIF;

       //------------------------------------------------
       // Criar o CabeÁalho se for o Primeiro Registo
       //------------------------------------------------
       CHAIN(N) (WCodFileAct) TBIDDF;
       IF (NOT %FOUND(TBIDDFF));
          MONITOR;
             WCodFileAnt = PCodFileAnt;
          ON-ERROR;
             RETURN;
          ENDMON;
          EXSR CrtHeader;
          WMsgErro = *BLANKS;
       ENDIF;

       //------------------------------------------------
       // Criar o Registo de Detalhe
       //------------------------------------------------
       IF (WNumNIB > *ZEROS);
          WMsgErro = 'COB0029'; // N„o existe cÛdigo Swift para o registo
          WstrNIB  = WNumNIB;
          WSession = $SetFiles('FTBLP012');
          $SetCriteria('(Codigo='''+%TRIM(WCodBanco)+''') +
                         AND (CdIndice = 45)');
          IF ($GetRecord() > *ZEROS);
             WSwiftCode = $GetField('ALIAS');
             IF (WSwiftCode <> *BLANKS) AND (WSwiftCode <> '????');
                WMsgErro = *BLANKS;
                EXSR CrtDetLote;
             ENDIF;
          ENDIF;
          $RmvDBSession(WSession);
       ENDIF;

       //------------------------------------------------
       // Criar o Registo de Resumo
       //------------------------------------------------
       IF (WNumNIB = *ZEROS);
          MONITOR;
             WQtdeRegist = PQtdeRegist;
          ON-ERROR;
             RETURN;
          ENDMON;
          MONITOR;
             WMnteRegist = PMnteRegist;
          ON-ERROR;
             RETURN;
          ENDMON;
          EXSR CrtResumo;
          WMsgErro = *BLANKS;
       ENDIF;
       RETURN;

       //===============================================================
       // Subrotina.: CrtHeader;
       // DescriÁ„o.: Criar o Header do Ficheiro XML
       //===============================================================
       BEGSR CrtHeader;
          WNuSequ     = *ZEROS;
          WNumLevel   = *ZEROS;
          WNuSequSup  = *ZEROS;
          WBlanks     = *BLANKS;
          SETLL (*LOVAL) RCOBT230;
          READ FCOBT230;
          DOW (NOT %EOF(FCOBT230));
             IF (INP_TPREGIS = 'PR');
                IF (%TRIM(INP_TXTXML) = '#DETRECORD#');
                   WNuSequTmp = WNuSequ;
                   WNuseqSaved = WNuSequTmp;
                   WNuSequ = 900;
                ELSE;
                   IF (WNuSequSup < INP_NUSUPER);
                      WNumLevel += 1;
                   ELSEIF (WNuSequSup > INP_NUSUPER);
                      WNumLevel -= 1;
                   ENDIF;
                   WNuSequSup = INP_NUSUPER;
                   WNuSequ += 1;
                   TFFENV = WCodFileAct;
                   TFNLIN = WNuSequ;
                   TFTEXT = INP_TXTXML;
                   EXSR ReplaceVars;
                   IF (WNuSequ <> 1);
                      TFTEXT = %SUBST(WBlanks:1:(WNumLevel*2)) + %TRIM(TFTEXT);
                   ELSE;
                      WNumLevel = *ZEROS;
                   ENDIF;
                   TFORIG = 'E';
                   WRITE TBIDDF;
                ENDIF;
             ENDIF;
             READ FCOBT230;
          ENDDO;
       ENDSR;

       //===============================================================
       // Subrotina.: ReplaceVars
       // DescriÁ„o.: substituir Campos de Trabalho
       //===============================================================
       BEGSR ReplaceVars;
          TFTEXT = $StrReplace('#ANO#':%CHAR(S#ANO):TFTEXT);
          TFTEXT = $StrReplace('#CDINT#':'DD'+%CHAR(WCodFileAct):TFTEXT);
          TFTEXT = $StrReplace('#DT001#':%CHAR(%DATE):TFTEXT);
          TFTEXT = $StrReplace('#HR001#':%CHAR(%TIME():*HMS:):TFTEXT);
          IF (WSitADC = ADC#Registado) OR (WSitADC = ADC#Pendente);
             TFTEXT = $StrReplace('#TIPENV#':SEPA#First:TFTEXT);
          ELSE;
             TFTEXT = $StrReplace('#TIPENV#':SEPA#Next:TFTEXT);
          ENDIF;
          TFTEXT = $StrReplace('#DATDIA+1#':
                       %EDITW(WDatDiaAlf:'    -  -  '):TFTEXT);
       // TFTEXT = $StrReplace('#ADCCODE#':'ADC'+%CHAR(WCodADC):TFTEXT);
          TFTEXT = $StrReplace('#ADCCODE#':%TRIM(WADCSepa):TFTEXT);
          TFTEXT = $StrReplace('#ADCDATE#':%TRIM(%EDITW(WDatADC:'0    -  -  ')):
                               TFTEXT);
          TFTEXT = $StrReplace('#NIBCLT#':
                   %TRIM(%EDITW(WNumNIB:'0                     ')):TFTEXT);
          TFTEXT = $StrReplace('#BICCLT#':WSwiftCode:TFTEXT);
          TFTEXT = $StrReplace('#VALREC#':%TRIM(%EDITW(WMnteRecibo:
                                   '0           .  ')):TFTEXT);

          TFTEXT = $StrReplace('#RECIBO#':%CHAR(WCodRecibo):TFTEXT);
          CHAIN (WCodSegura) GFSEG;
          IF (%FOUND());
             SEG_NOM = %XLATE(StrPT:StrISO:SEG_NOM);
             TFTEXT  = $StrReplace('#NOMECLT#':SEG_NOM:TFTEXT);
          ENDIF;
          CHAIN (WCodRecibo) GCOBGR;
          IF (%FOUND());
             TFTEXT = $StrReplace('#RAMO#':%TRIM(%EDITW(REC_GCRAMO:'0  ')):
                                  TFTEXT);
             TFTEXT = $StrReplace('#APOLICE#':%TRIM(%EDITW(REC_GCAPOL:
                                  '0       ')):TFTEXT);
          ENDIF;
       ENDSR;

       //===============================================================
       // Subrotina.: CrtDetLote
       // DescriÁ„o.: Criar o Detalhe do Registo
       //===============================================================
       BEGSR CrtDetLote;
          WNuSequ     = WNuSequTmp;
          SETLL (*LOVAL) RCOBT230;
          READ FCOBT230;
          DOW (NOT %EOF(FCOBT230));
             IF (INP_TPREGIS = 'DT');
                WNuSequ += 1;
                IF (WNuSequSup < INP_NUSUPER);
                   WNumLevel += 1;
                ELSEIF (WNuSequSup > INP_NUSUPER);
                   WNumLevel -= 1;
                ENDIF;
                WNuSequSup = INP_NUSUPER;
                TFFENV = WCodFileAct;
                TFNLIN = WNuSequ;
                TFTEXT = INP_TXTXML;
                EXSR ReplaceVars;
                TFTEXT = %SUBST(WBlanks:1:(WNumLevel*2)) + TFTEXT;
                TFORIG = 'E';
                WRITE TBIDDF;
             ENDIF;
             READ FCOBT230;
          ENDDO;
          WNuSequTmp = WNuSequ;
       ENDSR;

       //===============================================================
       // Subrotina.: CrtResumo
       // DescriÁ„o.: Actualizar a informaÁ„o de Resumo do Registo
       //===============================================================
       BEGSR CrtResumo;
          SETLL (WCodFileAct) TBIDDF;
          READE (WCodFileAct) TBIDDF;
          DOW (NOT %EOF(TBIDDFF));
             TFTEXT = $StrReplace('#QTDEMOV#':%TRIM(%EDITW(WQtdeRegist:
                                   '0      ')):TFTEXT);
             TFTEXT = $StrReplace('#VALMOV#':%TRIM(%EDITW(WMnteRegist:
                                   '0           .  ')):TFTEXT);
             UPDATE TBIDDF %FIELDS(TFTEXT);
             READE (WCodFileAct) TBIDDF;
          ENDDO;
          *INLR = *ON;
       ENDSR;
      /END-FREE
