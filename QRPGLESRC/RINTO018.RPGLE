     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*===========================================================
     H* Programa..: RINTO018
     H* Descrição.: Actualzar o estado do Recibo Standard.
     H*
     H*===========================================================
     FSD1REC    IF   F  326     7AIDISK    KEYLOC(270) EXTFILE('SD.1.REC')
     FFINTP028  UF   E           K DISK    PREFIX(INT_)
     FFINTP029  IF A E           K DISK    PREFIX(INT_)
     F*===========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010

     D*==> Estrutura do registo de Recibos
     D R#SD1REC      E DS                  EXTNAME('SD.1.REC') PREFIX('REC_')
     D
     D*==> Campos de Trabalho
     D WDataYMD        S              8S 0
     D WIstoChange     S               N
     D WCdRegis        S              9S 0
     D WCodErro        S              7A
     D WConvRamo       S              3S 0
     D WNuApolice      S             10A
     D WCodCompanhia   S              3S 0
     D WNumOrdem       S              7S 0
     D*===========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCdRegis
     C                   PARM                    WCodErro
     C
      /FREE
        //-----------------------------------------------------------------
        // Validar o estado do Recibo
        //-----------------------------------------------------------------
        WIsToChange = *OFF;
        WCodErro = *BLANKS;
        CHAIN (WCdRegis) RINTP028;
        IF (%FOUND(FINTP028));
           IF (INT_CDESTAD='4') OR (INT_CDESTAD='5') OR (INT_CDESTAD='6');
              WNumOrdem = INT_NUMORDRC;
              CHAIN WNumOrdem SD1REC R#SD1REC;
              IF (%FOUND(SD1REC));
                 IF (REC_CODSIT = 2);
                    INT_CDESTAD = '7'; // Cobrado
                    MONITOR;
                       WDataYMD = %DEC(REC_DTCOB:6:0);
                       WDataYMD = $CvtData(WDataYMD:'DMY':'YYMD');
                    ON-ERROR;
                       WDataYMD = *ZEROS;
                    ENDMON;
                    WIsToChange = *ON;
                 ELSEIF (REC_CODSIT = 1) OR (REC_CODSIT = 3);
                    INT_CDESTAD = '9'; // Anulado
                    MONITOR;
                       WDataYMD    = %DEC(REC_DTDEV:6:0);
                       WDataYMD    = $CvtData(WDataYMD:'DMY':'YYMD');
                    ON-ERROR;
                       WDataYMD = *ZEROS;
                    ENDMON;
                    WIsToChange = *ON;
                 ELSEIF (REC_CODSIT = *ZEROS) AND (((REC_NUMAD <> *ZEROS) AND
                   (REC_DTAVD <> *ZEROS)) OR REC_SNDAVD > '0') AND
                   (INT_CDESTAD = '4');
                    INT_CDESTAD = '5'; // Facturado
                    WIsToChange = *ON;
                 ENDIF;
                 INT_DTPAGAAN = $IntToDate(WDataYMD);
              ENDIF;

              IF (WIsToChange);
                 UPDATE RINTP028 %FIELDS(INT_CDESTAD:INT_DTPAGAAN);
                 INT_NUOPERA  = $GetNextKey('FINTP029':'NUOPERA':
                                '(CDREGIS = '''+%CHAR(INT_CDREGIS)+''')');
                 INT_CDERRO  = WCodErro;
                 INT_DTOPERA = %DATE();
                 INT_HROPERA = %TIME();
                 INT_USOPERA = S#USR;
                 WRITE RINTP029;
              ENDIF;
           ENDIF;
        ENDIF;

        *INLR = *ON;
      /END-FREE
