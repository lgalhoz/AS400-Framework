     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*===========================================================
     H* Programa..: PINTO012
     H* Descrição.: Carregar a informação dos recibos cobrados
     H*             Este programa actualiza a tabela de conversão de
     H*             de uma forma automática.
     H*
     H* Para Compilar este programa fazer:
     H*    CRTPF FILE(QTEMP/FINTT024) SRCFILE(#JMPRDSRC/QDDSSRC)
     H*    CRTPF FILE(QTEMP/INTFILE) SRCFILE(#JMPRDSRC/QDDSSRC) SRCMBR(FINTT024)
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
     FFINTT022  IF   E           K DISK    PREFIX(TMP_) USROPN
     F                                     EXTFILE('QTEMP/INTFILE')
     F*===========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010

     D*==> Processos
     D ExtFieldAlfa    PR            20A
     D   FieldValue                  20A   CONST
     D   FieldMask                   20A   CONST
     D
     D ExtFieldNum     PR            15S 5
     D   FieldValue                  20A   CONST
     D   FieldMask                   20A   CONST
     D
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
     D WCodConta       S              5S 0
     D WCdFile         S              9S 0
     D WCdRegis        S              9S 0
     D WCdRamo         S              3S 0
     D WNuApol         S             10A
     D WNumRec         S              9A
     D WNumSeg         S              8S 0
     D WNomSeg         S             40A
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
     D WModali         S              2S 0
     D WSubMod         S              2S 0
     D WRamo           S              2S 0
     D WDBSession      S              5S 0
     D WTipRecibo      S              1A
     D WSitRecibo      S              1A
     D WIcAlterou      S               N
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
          OPEN FINTT022;
          SETLL *LOVAL RINTT022;
          READ RINTT022;
          DOW (NOT %EOF(FINTT022));
             //-----------------------------------
             // Processar o Cabeçalho do Ficheiro
             //-----------------------------------
             IF (TMP_RAMO <> 'Ramo');
                //-----------------------------------------
                // Criar o registo no formato Standard
                //-----------------------------------------
                EXSR ConvRegisto;
                CHAIN (WCodCompanhia:WCdChvOr:WNuApol:WNumRec) RINTP02801;
                IF (NOT %FOUND(FINTP02801));
                   WQtdeRegis  += 1;
                   INT_CDREGIS  = WCdRegis;
                   INT_CDESTAD  = '1';
                   INT_ICORIG   = 'A'; // Automático

                   INT_CCOMP    = WCodCompanhia;
                   INT_CDRMORG  = WCdChvOr;
                   INT_NUAPOL   = WNuApol;
                   INT_NUMREC   = WNumRec;

                   INT_NUMORDRC = *ZEROS;
                   INT_NUMORDAP = *ZEROS;
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
                   INT_IMCOMIS1 = WTotComiss1; // Comissão de Cobrança
                   INT_IMCOMIS2 = WTotComiss2; // Comissão de Mediação
                   INT_IMCOMIS3 = WTotComiss3; // Comissão de Corretagem
                   WRITE RINTP028;
                   WCdRegis    += 1;

                   //-----------------------------------------
                   // Actualizar a Operação do Recibo
                   //-----------------------------------------
                   INT_NUOPERA = 1;
                   INT_CDESTAD = '1'; // Registo do recibo
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
                IF (INT_STRECIB = '2');
                   WCodErro = 'INT0004'; // O Recibo já está cobrado
                ELSEIF (INT_STRECIB = '1');
                   WCodErro = 'INT0005'; // O Recibo já está anulado
                ELSEIF (INT_NUMORDRC > *ZEROS);
                   WCodErro = 'INT0006'; // O Recibo já está inserido
                ENDIF;
                INT_CDERRO  = WCodErro;
                WRITE RINTP027;

             ENDIF;
             READ RINTT022;
          ENDDO;
          CLOSE FINTT022;
       ENDSR;

       //=================================================================
       // SubRotina..: ConvRegisto
       // Objectivo..: Converter o Registo
       //=================================================================
       BEGSR ConvRegisto;
          //-----------------------------------------------
          // formatar Chave (Ramo/Modalidade/Submod)
          //-----------------------------------------------
          WModali     = ExtFieldNum(TMP_MODAL:'="#"');
          WSubMod     = ExtFieldNum(TMP_SUBMOD:'="#"');
          WRamo       = ExtFieldNum(TMP_RAMO:'="#"');
          WCdChvOr    = %TRIM(%EDITW(WRamo:'0  ')) +
                        %TRIM(%EDITW(WModali:'0  ')) +
                        %TRIM(%EDITW(WSubMod:'0  '));

          WNuApol     = ExtFieldAlfa(TMP_APOL:'="#"');
          WNuApol     = %EDITW(%DEC(WNuApol:10:0):'          ');
          WNumRec     = ExtFieldAlfa(TMP_RECIBO:'="#"');
          WNumRec     = %EDITW(%DEC(WNumRec:9:0):'         ');
          WNumSeg     = *ZEROS;
          WNomSeg     = TMP_TOMADOR;

          EXSR GetTipRecib;

          WDatInicio  = $IntToDate(ExtFieldNum(TMP_DTINIC:'"="#"'));
          WDatTermo   = $IntToDate(ExtFieldnum(TMP_DTTERMO:'="#"'));
          WPremComerc = ExtFieldNum(TMP_PREMIO:'="#"');
          WPremSimpl  = ExtFieldNum(TMP_PREMIO:'="#"');
          WTotRecibo  = ExtFieldNum(TMP_TOTAL:'="#"');
          WTotComiss1 = ExtFieldNum(TMP_COMISS1:'="#"');
          WTotComiss2 = ExtFieldNum(TMP_COMISS2:'="#"');
          WTotComiss3 = ExtFieldNum(TMP_COMISS3:'="#"');
          WDatEntrada = %DATE();
          WDatLimite  = $IntToDate(ExtFieldnum(TMP_DTLIMIT:'="#"'));
       ENDSR;

       //=================================================================
       // SubRotina..: GetTipRecib
       // Objectivo..: Formatar o Tipo de Recibo
       //=================================================================
       BEGSR GetTipRecib;
          WTipRecibo = 'P';
          IF (%TRIM(TMP_TIPO) = 'ESTORNO');                     // "ESTORNO"
             WTipRecibo = 'E';
          ELSEIF (%SUBST(%TRIM(TMP_TIPO):1:10) = 'CONTINUADO'); // "CONTINUADO FIXO"
             WTipRecibo = 'C';
          ELSEIF (%SUBST(%TRIM(TMP_TIPO):1:6) = 'PREMIO');      // "PREMIO FIXO - 1.RECIBO"
             WTipRecibo = 'P';
          ENDIF;
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
          INT_CDFILE  = WCdFile;
          INT_CCOMP   = WCodCompanhia;
          INT_CDINTER = WCdInter;
          INT_TPFILE  = 'EMI'; // Emissão
          INT_NOFILE  = WNomFile;
          INT_ORNAME  = *BLANKS;
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
         //  REC_EMITIDOS_05703_20140227.csv
         MONITOR;
            WCodConta = %DEC(%SUBST(WNomFile:14:5):5:0);
         ON-ERROR;
            WCodConta = *ZEROS;
         ENDMON;
         IF (WCodConta = 09286);
             WCodCompanhia = 27;
         ELSEIF (WCodConta = 05703);
             WCodCompanhia = 26;
         ELSE;
             WCodCompanhia = *ZEROS;
         ENDIF;

         MONITOR;
            WDatFile = %DEC(%SUBST(WNomFile:20:8):8:0);
         ON-ERROR;
            WDatFile = *ZEROS;
         ENDMON;
         WHorFile  = %TIME();
       ENDSR;
      /END-FREE
     P*==================================================================
     P* Processo..: ExtFieldNum
     P* Descrição.: Extrair o valor númerico
     P*==================================================================
     P ExtFieldNum     B                   EXPORT
     D ExtFieldNum     PI            15S 5
     D   FieldValue                  20A   CONST
     D   FieldMask                   20A   CONST
     D
     D*==> Campos de Trabalho
     D WResult         S             15S 5
     D*=================================================================
      /FREE
        MONITOR;
           RETURN %DEC(ExtFieldAlfa(FieldValue:FieldMask):15:5);
        ON-ERROR;
           RETURN *ZEROS;
        ENDMON;
      /END-FREE
     P ExtFieldNum     E

     P*==================================================================
     P* Processo..: ExtFieldAlfa
     P* Descrição.: Extrair o valor Alfanumérico
     P*==================================================================
     P ExtFieldAlfa    B                   EXPORT
     D ExtFieldAlfa    PI            20A
     D   FieldValue                  20A   CONST
     D   FieldMask                   20A   CONST
     D
     D*==> Campos de Trabalho
     D WPosIni         S              3S 0
     D WPosFim         S              3S 0
     D WTagIni         S              5A
     D WTagFim         S              5A
     D WResultado      S             20A
     D*=================================================================
      /FREE
         WResultado = FieldValue;

         //--------------------------------------
         // Extrair TAG's delimitadoras
         //--------------------------------------
         WPosIni = 1;
         WPosFim = %SCAN('#':%TRIM(FieldMask):WPosIni);
         IF (WPosFim = *ZEROS);
            RETURN WResultado;
         ENDIF;

         WTagIni = %SUBST(%TRIM(FieldMask):WPosIni:(WPosFim-WPosIni));
         WPosIni = WPosFim + 1;
         WPosFim = %LEN(%TRIM(FieldMask));
         WTagFim = %SUBST(%TRIM(FieldMask):WPosIni:(WPosFim-WPosIni)+1);

         //--------------------------------------
         // Extrair a Palavra
         //--------------------------------------
         WPosIni = 1;
         WPosFim = %SCAN(%TRIM(WTagIni):%TRIM(FieldValue):WPosIni);
         IF (WPosFim = *ZEROS);
            RETURN WResultado;
         ENDIF;
         WPosIni = %LEN(%TRIM(WTagIni))+1;
         WPosFim = %SCAN(%TRIM(WTagFim):%TRIM(FieldValue):WPosIni);
         IF (WPosFim = *ZEROS);
            RETURN WResultado;
         ENDIF;
         WResultado =  %SUBST(%TRIM(FieldValue):WPosIni:(WPosFim - WPosIni));
         RETURN WResultado;
      /END-FREE
     P ExtFieldAlfa    E
