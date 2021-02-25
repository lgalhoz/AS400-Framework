     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*===========================================================
     H* Programa..: PINTO016
     H* Descrição.: Carregar Recibos de Indemnização de A.Trabalho
     H* Para Compilar e
     H*
     H*    CRTPF FILE(QTEMP/FINTT026) SRCFILE(#JMPRDSRC/QDDSSRC)
     H*    CRTPF FILE(QTEMP/INTFILE) SRCFILE(#JMPRDSRC/QDDSSRC) SRCMBR(FINTT026)
     H*===========================================================
     FFINTP020  UF A E           K DISK    PREFIX(INT_)                         Interface de Recibos
     FFINTP02001IF   E           K DISK    PREFIX(INT_)
     F                                     RENAME(RINTP020:RINTP02001)
     FFINTP027  UF A E           K DISK    PREFIX(INT_)                         Relação Interfaces/Recibos Standard
     FFINTP028  UF A E           K DISK    PREFIX(INT_)                         Recibos Standard de Indemnização
     FFINTP022  UF A E           K DISK    PREFIX(INT_)                         Informação Complementar dos Recibos
     FFINTP02801IF   E           K DISK    PREFIX(INT_)
     F                                     RENAME(RINTP028:RINTP02801)
     FFINTP029  UF A E           K DISK    PREFIX(INT_)                         Operações realizadas sobre o registo
     F
     FFINTT026  IF   E           K DISK    PREFIX(TMP_) USROPN                  Temporário de Recibos Indemnização
     F                                     EXTFILE('QTEMP/INTFILE')
     F*===========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010

     D*==> Campos de Trabalho
     D WNomFile        S             40A
     D WCdInter        S              9S 0
     D WAction         S              1A
     D WResposta       S              1A
     D WMsgErro        S              7A
     D
     D WQtdeRegis      S              5S 0
     D WNomPasta       S             40A
     D WCodErro        S              7A
     D WDatFile        S              8S 0
     D WHorFile        S               T
     D WCodCompanhia   S              3S 0
     D WCdFile         S              9S 0
     D WCdRamo         S              5S 0
     D WCdRegis        S              9S 0
     D WNuApol         S             10A
     D WNumRec         S             10A
     D WNuApolOrig     S             10A
     D WNumRecOrig     S             15A
     D WNomSeg         S             40A
     D WDatInicio      S               D
     D WDatTermo       S               D
     D WTotRecibo      S             11S 2
     D WDatEntrada     S               D
     D WDatRecibo      S               D
     D WDatProcesso    S               D
     D WNumProcesso    S              7S 0
     D WNumNIF         S              9S 0
     D WDsObserva      S            100A
     D WIsFirst        S               N
     D WCdChvOr        S             20A
     D WDBSession      S              5S 0
     D WIcAlterou      S               N
     D WLenStr         S              2S 0
     D
     D*==> Declaração de Constantes
     D ACT#Validar     C                   'V'
     D ACT#Processar   C                   'P'
     D StrAlfa         C                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ+
     D                                      abcdefghijklmnopqrstuvwxyz-'
     D StrNumero       C                   '00000000000000000000000000+
     D                                      00000000000000000000000000.'
     D*===========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WNomFile
     C                   PARM                    WCdInter
     C                   PARM                    WAction
     C                   PARM                    WResposta
     C                   PARM                    WMsgErro
      /FREE
        //-----------------------------------------------------------------
        // Validar se o Ficheiro ja foi Processado
        //-----------------------------------------------------------------
        CHAIN (WNomFile) RINTP02001;
        IF (%FOUND());
           WMsgErro = 'INT0001'; // O ficheiro já foi processado
           WResposta = 'N'; // Não é para processar
           RETURN;
        ELSEIF (WAction = 'V'); // Validar
           WMsgErro = '*BLANKS';
           WResposta = 'S'; // É para processar
           RETURN;
        ENDIF;

        EXSR RegisFile;
        EXSR ReadInterface;
        CHAIN (WCdFile) RINTP020;
        INT_QTREGIS = WQtdeRegis;
        UPDATE RINTP020 %FIELDS(INT_QTREGIS);

        *INLR = *ON;

       //=================================================================
       // SubRotina..: ReadInterface
       // Objectivo..: Ler o ficheiro de Interface recebido
       //=================================================================
       BEGSR ReadInterface;
          OPEN FINTT026;
          SETLL *LOVAL RINTT026;
          READ RINTT026;
          DOW (NOT %EOF(FINTT026));
             //-----------------------------------
             // Processar o Cabeçalho do Ficheiro
             //-----------------------------------
             MONITOR;
                WCdRamo = %DEC(TMP_CDRAMO:5:0);
             ON-ERROR;
                WCdRamo = *ZEROS;
             ENDMON;
             IF (WCdRamo > *ZEROS);
                //-----------------------------------------
                // Criar o registo no formato Standard
                //-----------------------------------------
                EXSR ConvRegisto;
                CHAIN (WCodCompanhia:WCdChvOr:WNuApolOrig:WNumRecOrig)
                                                                 RINTP02801;
                IF (NOT %FOUND(FINTP02801));
                   WQtdeRegis  += 1;
                   INT_CDREGIS  = WCdRegis;
                   INT_ICORIG   = 'A'; // Automático
                   INT_CDESTAD  = '1';
                   INT_DTHSTAT  = %TimeStamp();

                   //------------------------------------------------
                   // Identificação do Registo Original
                   //------------------------------------------------
                   INT_CCOMP    = WCodCompanhia;
                   INT_CDRMORG  = WCdChvOr;
                   INT_NUAPOLOR = WNuApolOrig;
                   INT_NUMRECOR = WNumRecOrig;
                   INT_CDCANAL  = *BLANKS;

                   //------------------------------------------------
                   // Correspondência com Demo Comp, Lda
                   //------------------------------------------------
                   INT_NUMORDAP = *ZEROS;
                   INT_NUMORDRC = *ZEROS;
                   INT_CDRAMO   = *ZEROS;
                   INT_NUAPOL   = WNuApol;
                   INT_NUMSEG   = *ZEROS;
                   INT_NOMSEG   = WNomSeg;
                   INT_NUMREC   = WNumRec;
                   INT_TPRECIB  = '7'; // Recibo de Indemnização
                   INT_STRECIB  = '0'; // Situação Pendente

                   //------------------------------------------------
                   // Valores do Recibo (Datas e Importâncias)
                   //------------------------------------------------
                   INT_DTINICIO = WDatInicio;
                   INT_DTTERMO  = WDatTermo;
                   INT_DTENTRAD = WDatEntrada;
                   INT_DTPAGAAN = %DATE('0001-01-01');
                   INT_IMPRCOM  = *ZEROS;
                   INT_IMPRSIM  = *ZEROS;
                   INT_IMTOTREC = WTotRecibo;

                   //------------------------------------------------
                   // Valores de Comissão
                   //------------------------------------------------
                   INT_TPCALC   = *BLANKS;     // (B) (A+C) (---)
                   INT_IMCOMIS1 = *ZEROS;      // Comissão de Cobrança
                   INT_IMCOMIS2 = *ZEROS;      // Comissão de Mediação
                   INT_IMCOMIS3 = *ZEROS;      // Comissão de Corretagem
                   WRITE RINTP028;
                   WCdRegis    += 1;

                   //-----------------------------------------
                   // Actualizar Informação Complementar
                   //-----------------------------------------
                   INT_DTRECIBO = WDatRecibo;
                   INT_NUPROCES = WNumProcesso;
                   INT_DTPROCES = WDatProcesso;
                   INT_NUNIF    = WNumNIF;
                   WRITE RINTP022;

                   //-----------------------------------------
                   // Actualizar a Operação do Recibo
                   //-----------------------------------------
                   INT_NUOPERA = 1;
                   INT_CDERRO  = *BLANKS;
                   INT_DTOPERA = %DATE();
                   INT_HROPERA = %TIME();
                   INT_USOPERA = S#USR;
                   WRITE RINTP029;
                ENDIF;

                //-----------------------------------------
                // Actualizar a relação com o ficheiro
                //-----------------------------------------
                WCodErro   = *BLANKS;
                INT_CDFILE = WCdFile;
                IF (INT_CDESTAD = '7');
                   WCodErro = 'INT0004'; // O Recibo já está cobrado
                ELSEIF (INT_CDESTAD = '9');
                   WCodErro = 'INT0005'; // O Recibo já está anulado
                ENDIF;
                INT_CDERRO  = WCodErro;
                WRITE RINTP027;
             ENDIF;
             READ RINTT026;
          ENDDO;
          CLOSE FINTT026;
       ENDSR;

       //=================================================================
       // SubRotina..: ConvRegisto
       // Objectivo..: Converter o Registo
       //=================================================================
       BEGSR ConvRegisto;
          //-----------------------------------------------
          // forCompr Chave (Ramo/Modalidade/Submod)
          //-----------------------------------------------
          WCdChvOr    = TMP_CDRAMO;

          WNuApol     = TMP_NUAPOL;
          WLenStr     = %LEN(%TRIM(WNuApol));
          WNuApolOrig = *BLANKS;
          %SUBST(WNuApolOrig:((10-WLenStr)+1)) = %TRIM(WNuApol);
          WNuApol     = %EDITW(%DEC(WNuApolOrig:10:0):'          ');

          WNumRec     = TMP_NUMREC;
          WLenStr     = %LEN(%TRIM(WNumRec));
          WNumRecOrig = *BLANKS;
          %SUBST(WNumRecOrig:((15-WLenStr)+1)) = %TRIM(WNumRec);
          MONITOR;
             WNumRec = %EDITW(%DEC(
                                %XLATE(StrAlfa:StrNumero:
                                  WNumRecOrig):9:0):'         ');
          ON-ERROR;
             WNumRec = *ZEROS;
          ENDMON;

          WNomSeg       = TMP_NOMBENEF;

          MONITOR;
             TMP_DTINICIO  = %XLATE(StrAlfa:StrNumero:TMP_DTINICIO);
             WDatInicio    = %DATE(TMP_DTINICIO:*EUR);
          ON-ERROR;
             WDatInicio    = %DATE('0001-01-01');
          ENDMON;

          MONITOR;
             TMP_DTTERMO   = %XLATE(StrAlfa:StrNumero:TMP_DTTERMO);
             WDatTermo     = %DATE(TMP_DTTERMO:*EUR);
          ON-ERROR;
             WDatTermo     = %DATE('0001-01-01');
          ENDMON;

          WTotRecibo    = %DEC(TMP_VALRECIB:11:2);

          MONITOR;
             TMP_DATENTRA  = %XLATE(StrAlfa:StrNumero:TMP_DATENTRA);
             WDatEntrada   = %DATE(TMP_DATENTRA:*EUR);
          ON-ERROR;
             WDatEntrada   = %DATE('0001-01-01');
          ENDMON;

          MONITOR;
             TMP_DATRECIBO = %XLATE(StrAlfa:StrNumero:TMP_DATRECIBO);
             WDatRecibo    = %DATE(TMP_DATRECIBO:*EUR);
          ON-ERROR;
             WDatRecibo    = %DATE('0001-01-01');
          ENDMON;

          MONITOR;
             TMP_DATPROC   = %XLATE(StrAlfa:StrNumero:TMP_DATPROC);
             WDatProcesso  = %DATE(TMP_DATPROC:*EUR);
          ON-ERROR;
             WDatProcesso  = %DATE('0001-01-01');
          ENDMON;

          MONITOR;
             WNumProcesso = %DEC(TMP_NUMPROC:7:0);
          ON-ERROR;
             WNumProcesso = *ZEROS;
          ENDMON;
          MONITOR;
             WNumNIF = %DEC(TMP_NIFBENEF:9:0);
          ON-ERROR;
             WNumNIF = *ZEROS;
          ENDMON;
       ENDSR;

       //=================================================================
       // SubRotina..: RegisFile
       // Objectivo..: Registar o Ficheiro
       //=================================================================
       BEGSR RegisFile;
          EXSR GetDatHorFile;
          WQtdeRegis  = *ZEROS;
          WCdRegis    = $GetNextKey('FINTP028':'CDREGIS');
          WCdFile     = $GetNextKey('FINTP020':'CDFILE');
          INT_CDESTAD = 'P'; // Processado
          INT_DTESTAD = $DateToInt(%DATE());
          INT_CDFILE  = WCdFile;
          INT_CCOMP   = WCodCompanhia;
          INT_CDINTER = WCdInter;
          INT_TPFILE  = 'EMI'; // Emissão
          INT_NOFILE  = WNomFile;
          INT_ORNAME  = *BLANKS;
          INT_DTFILE  = WDatFile;
          INT_CDCONTA = *ZEROS;
          INT_QTREGIS = *ZEROS;
          WRITE RINTP020;
       ENDSR;

       //=================================================================
       // SubRotina..: GetDatHorFile
       // Objectivo..: Obter a data e Hora do ficheiro
       //=================================================================
       BEGSR GetDatHorFile;
         // REC_IND_AT_20140404.csv
         WCodCompanhia = 54;
         MONITOR;
            WDatFile = %DEC(%SUBST(WNomFile:12:8):8:0);
         ON-ERROR;
            WDatFile = *ZEROS;
         ENDMON;
         WHorFile  = %TIME();
       ENDSR;
      /END-FREE
