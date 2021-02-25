     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*===========================================================
     H* Programa..: PINTO013
     H* Descrição.: Carregar os Recibos da Zurich
     H*             Este programa actualiza a tabela de conversão de
     H*             de uma forma automática.
     H*
     H* Para Compilar este programa fazer:
     H*    CRTPF FILE(QTEMP/FINTT023) SRCFILE(#JMPRDSRC/QDDSSRC)
     H*    CRTPF FILE(QTEMP/INTFILE) SRCFILE(#JMPRDSRC/QDDSSRC) SRCMBR(FINTT023)
     H*===========================================================
     FFINTP020  UF A E           K DISK    PREFIX(INT_)
     FFINTP02001IF   E           K DISK    PREFIX(INT_)
     F                                     RENAME(RINTP020:RINTP02001)
     FFINTP027  UF A E           K DISK    PREFIX(INT_)
     FFINTP028  UF A E           K DISK    PREFIX(INT_)
     FFINTP02801IF   E           K DISK    PREFIX(INT_)
     F                                     RENAME(RINTP028:RINTP02801)
     FFINTP029  UF A E           K DISK    PREFIX(INT_)
     F
     FFINTT023  IF   E           K DISK    PREFIX(TMP_) USROPN
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
     D WOrName         S             40A
     D WNomPasta       S             40A
     D WCodErro        S              7A
     D WDatFile        S              8S 0
     D WHorFile        S               T
     D WCodCompanhia   S              3S 0
     D WCodConta       S              5S 0
     D WCdFile         S              9S 0
     D WCdRegis        S              9S 0
     D WCdRamo         S              3S 0
     D WNuApol         S             10A
     D WNumRec         S              9A
     D WNuApolOrig     S             10A
     D WNumRecOrig     S             15A
     D WNumSeg         S              8S 0
     D WNomSeg         S             60A
     D WDatInicio      S               D
     D WDatTermo       S               D
     D WPremComerc     S             11S 2
     D WPremSimpl      S             11S 2
     D WTotRecibo      S             11S 2
     D WTotComiss1     S             11S 2
     D WTotComiss2     S             11S 2
     D WTotComiss3     S             11S 2
     D WDatEntrada     S               D
     D WDatLimite      S               D
     D WDsObserva      S            100A
     D WIsFirst        S               N
     D WCdChvOr        S             20A
     D WTpFile         S              3A
     D WRamo           S              2S 0
     D WDBSession      S              5S 0
     D WTipRecibo      S              1A
     D WSitRecibo      S              1A
     D WIcAlterou      S               N
     D WLenStr         S              2S 0
     D
     D*==> Declaração de Constantes
     D ACT#Validar     C                   'V'
     D ACT#Processar   C                   'P'
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
        ELSEIF (WAction = ACT#Validar);
           WResposta = 'S'; // É para processar
           WMsgErro = *BLANKS;
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
          OPEN FINTT023;
          SETLL *LOVAL RINTT023;
          READ RINTT023;
          DOW (NOT %EOF(FINTT023));
             EXSR ConvRegisto;
             CHAIN (WCodCompanhia:WCdChvOr:WNuApolOrig:WNumRecOrig) RINTP02801;
             IF (NOT %FOUND(FINTP02801));
                WQtdeRegis  += 1;
                INT_CDREGIS  = WCdRegis;
                INT_CDESTAD  = '1';
                INT_DTHSTAT  = %TimeStamp();
                INT_ICORIG   = 'A'; // Automático

                INT_CCOMP    = WCodCompanhia;
                INT_CDRMORG  = WCdChvOr;
                INT_NUAPOL   = WNuApol;
                INT_NUAPOLOR = WNuApolOrig;
                INT_NUMRECOR = WNumRecOrig;
                INT_NUMREC   = WNumRec;

                INT_NUMORDAP = *ZEROS;
                INT_NUMORDRC = *ZEROS;
                INT_CDRAMO   = WCdRamo;
                INT_NUMSEG   = WNumSeg;
                INT_NOMSEG   = WNomSeg;
                INT_TPRECIB  = WTipRecibo;
                INT_STRECIB  = WSitRecibo;

                INT_DTINICIO = WDatInicio;
                INT_DTTERMO  = WDatTermo;
                INT_DTENTRAD = WDatEntrada;
                INT_DTLIMITE = WDatLimite;
                INT_DTPAGAAN = %DATE('0001-01-01');
                INT_IMPRCOM  = WPremComerc;
                INT_IMPRSIM  = WPremSimpl;
                INT_IMTOTREC = WTotRecibo;
                INT_TPCALC   = '250';        // (B) (A+C) (---)
                INT_IMCOMIS1 = WTotComiss1;  // Comissão de Cobrança
                INT_IMCOMIS2 = WTotComiss2;  // Comissão de Mediação
                INT_IMCOMIS3 = WTotComiss3;  // Comissão de Corretagem
                WRITE RINTP028;
                WCdRegis    += 1;

                //-----------------------------------------
                // Actualizar a Operação do Recibo
                //-----------------------------------------
                INT_CDESTAD = '1'; // Registo do recibo
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
             ELSEIF (INT_CDESTAD > '3');
                WCodErro = 'INT0006'; // O Recibo já está inserido
             ENDIF;
             INT_CDERRO  = WCodErro;
             WRITE RINTP027;

             READ RINTT023;
          ENDDO;
          CLOSE FINTT023;
       ENDSR;

       //=================================================================
       // SubRotina..: ConvRegisto
       // Objectivo..: Converter o Registo
       //=================================================================
       BEGSR ConvRegisto;
          WCdChvOr   = %TRIM(TMP_Ramo);

          WNuApol     = TMP_NUAPOL;
          WLenStr     = %LEN(%TRIM(WNuApol));
          WNuApolOrig = *BLANKS;
          %SUBST(WNuApolOrig:((10-WLenStr)+1)) = %TRIM(WNuApol);
          WNuApol     = %EDITW(%DEC(WNuApolOrig:10:0):'          ');

          WNumRec     = TMP_NURECIB;
          WLenStr     = %LEN(%TRIM(WNumRec));
          WNumRecOrig = *BLANKS;
          %SUBST(WNumRecOrig:((15-WLenStr)+1)) = %TRIM(WNumRec);
          WNumRec     = %EDITW(%DEC(WNumRecOrig:9:0):'         ');

          IF (%TRIM(TMP_TIPREC) = '00');     // Novo
             WTipRecibo = '0';
          ELSEIF (%TRIM(TMP_TIPREC) = '01'); // Adicional (Suplementar)
             WTipRecibo = '1';
          ELSEIF (%TRIM(TMP_TIPREC) = '02'); // Estorno
             WTipRecibo = '5';
          ELSEIF (%TRIM(TMP_TIPREC) = '03'); // Continuado
             WTipRecibo = '2';
          ELSE;
             WTipRecibo = 'X';
          ENDIF;

          EXSR ConvCharct;
          WDatInicio  = $IntToDate(%DEC(TMP_DATINI:8:0));
          WDatTermo   = $IntToDate(%DEC(TMP_DATFIM:8:0));
          WPremComerc = %DEC(TMP_PRCOMER:13:2);
          WPremSimpl  = %DEC(TMP_PRCOMER:13:2);
          WTotRecibo  = %DEC(TMP_PRTOTAL:13:2);
          WTotComiss1 = %DEC(TMP_COMCOB:13:2);
          WTotComiss2 = %DEC(TMP_COMANG:13:2);
          WTotComiss3 = *ZEROS;
          WDatEntrada = $IntToDate(%DEC(TMP_DATEMI:8:0));
          WDatLimite  = $IntToDate(%DEC(TMP_DATVENC:8:0));
       ENDSR;

       //=================================================================
       // SubRotina..: ConvCharct
       // Objectivo..: Converter os Caracteres mal formatados.
       //=================================================================
       BEGSR ConvCharct;
          WNomSeg = TMP_TOMADOR;
          WNomSeg = $StrReplace('Ã§':'ç':WNomSeg);
          WNomSeg = $StrReplace('Ã£':'ã':WNomSeg);
          WNomSeg = $StrReplace('Âª':'ª.':WNomSeg);
          WNomSeg = $StrReplace('Âº':'º.':WNomSeg);
          WNomSeg = $StrReplace('Ãµ':'õ':WNomSeg);
          WNomSeg = $StrReplace('ï»¿':'Dr. ':WNomSeg);
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
          INT_CDINTER = WCdInter;
          INT_CCOMP   = WCodCompanhia;
          INT_TPFILE  = WTpFile;
          INT_NOFILE  = WNomFile;
          INT_ORNAME  = WOrName;
          INT_DTFILE  = WDatFile;
          INT_CDCONTA = WCodConta;
          INT_QTREGIS = *ZEROS;
          WRITE RINTP020;
       ENDSR;

       //=================================================================
       // SubRotina..: GetDatHorFile
       // Objectivo..: Obter a data e Hora do ficheiro
       //=================================================================
       BEGSR GetDatHorFile;
         //  ZURICH_RECEMI_20140220_101219705
         WCodConta = *ZEROS;
         WCodCompanhia = 15;

         MONITOR;
            WDatFile = %DEC(%SUBST(WNomFile:15:8):8:0);
         ON-ERROR;
            WDatFile = *ZEROS;
         ENDMON;
         WTpFile  = %SUBST(WNomFile:11:3);
         WOrName  = %SUBST(WNomFile:24:9)+'.txt';
         WHorFile = %TIME();
       ENDSR;
      /END-FREE
