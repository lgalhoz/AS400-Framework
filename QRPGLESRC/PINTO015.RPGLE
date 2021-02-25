     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*===========================================================
     H* Programa..: PINTO015
     H* Descrição.: Carregar a informação dos da Fidelidade Mundial
     H*             Este programa actualiza a tabela de conversão de
     H*             de uma forma automática.
     H*
     H* Para Compilar este programa fazer:
     H*    CRTPF FILE(QTEMP/FINTT025) SRCFILE(#JMPRDSRC/QDDSSRC)
     H*    CRTPF FILE(QTEMP/INTFILE) SRCFILE(#JMPRDSRC/QDDSSRC) SRCMBR(FINTT025)
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
     FFINTT025  IF   E           K DISK    PREFIX(TMP_) USROPN
     F                                     EXTFILE('QTEMP/INTFILE')
     F*===========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010

     D*==> Processos
     D ExtAlfaDigits   PR            20A
     D   FieldValue                  20A   CONST
     D
     D ConvNumeros     PR            15S 5
     D   FieldValue                  15A   CONST
     D
     D*==> Listas de Trabalho
     D TDSRAMO         S             50    DIM(9) CTDATA PERRCD(1)
     D TCDRAMO         S             10    DIM(9) ALT(TDSRAMO)
     D TDSRECIBO       S             30    DIM(4) CTDATA PERRCD(1)
     D TCDRECIBO       S              1    DIM(4) ALT(TDSRECIBO)
     D
     D*==> Campos de Trabalho
     D WNomFile        S             40A
     D WCdInter        S              9S 0
     D WAction         S              1A
     D WResposta       S              1A
     D WMsgErro        S              7A
     D
     D WIdx            S              3S 0
     D WNumLinha       S              5S 0
     D WCdChvOr        S             10A
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
     D WNuApolOrig     S             10A
     D WNumRecOrig     S             15A
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
          WNumLinha = *ZEROS;
          OPEN FINTT025;
          SETLL *LOVAL RINTT025;
          READ RINTT025;
          DOW (NOT %EOF(FINTT025));
             WNumLinha += 1;
             IF (TMP_APOLICE <> *BLANKS)
                AND (%TRIM(TMP_APOLICE) <> 'Nº Apólice');
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
                   INT_NUAPOLOR = WNuApolOrig;
                   INT_NUMRECOR = WNumRecOrig;

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
                   INT_IMCOMIS1 = WTotComiss1; // Comissão de Cobrança
                   INT_IMCOMIS2 = WTotComiss2; // Comissão de Mediação
                   INT_IMCOMIS3 = WTotComiss3; // Comissão de Corretagem
                   WRITE RINTP028;
                   WCdRegis    += 1;

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
             READ RINTT025;
          ENDDO;
          CLOSE FINTT025;
       ENDSR;

       //=================================================================
       // SubRotina..: ConvRegisto
       // Objectivo..: Converter o Registo
       //=================================================================
       BEGSR ConvRegisto;
          //-----------------------------------------------
          // ForCompr o Código de Ramo
          //-----------------------------------------------
          WCdChvOr = '??????????';
          WIdx = 1;
          DOW (WIdx < 9);
             IF (%TRIM(TMP_DESCRAMO) = %TRIM(TDSRAMO(WIdx)));
                WCdChvOr = TCDRAMO(WIdx);
                LEAVE;
             ENDIF;
             WIdx += 1;
          ENDDO;

          WLenStr     = %LEN(%TRIM(TMP_APOLICE)) - 1;
          WNuApolOrig = *BLANKS;
          %SUBST(WNuApolOrig:((10-WLenStr)+1)) = %SUBST(TMP_APOLICE:2);
          WNuApol     = ExtAlfaDigits(WNuApolOrig);
          WNuApol     = %EDITW(%DEC(WNuApol:10:0):'          ');

          WLenStr     = %LEN(%TRIM(TMP_RECIBO)) - 1;
          WNumRecOrig = *BLANKS;
          %SUBST(WNumRecOrig:((15-WLenStr)+1)) = %SUBST(TMP_RECIBO:2);
          WNumRec     = %SUBST(WNumRecOrig:7:9);

          WNumSeg     = *ZEROS;
          WNomSeg     = TMP_NOME;

          EXSR GetTipRecib;

          WDatInicio  = $IntToDate(%DEC(TMP_DATINICIO:8:0));
          WDatTermo   = $IntToDate(%DEC(TMP_DATTERMO:8:0));
          WPremComerc = ConvNumeros(TMP_PRMCOMER);
          WPremSimpl  = ConvNumeros(TMP_PRMCOMER);
          WTotRecibo  = ConvNumeros(TMP_PRMTOTAL);
          WTotComiss1 = ConvNumeros(TMP_COMCOBRA);
          WTotComiss2 = ConvNumeros(TMP_COMANGAR);
          WTotComiss3 = ConvNumeros(TMP_COMESPECI);
          WDatEntrada = %DATE();
          WDatLimite  = $IntToDate(%DEC(TMP_DATLIMITE:8:0));
       ENDSR;

       //=================================================================
       // SubRotina..: GetTipRecib
       // Objectivo..: ForCompr o Tipo de Recibo
       //=================================================================
       BEGSR GetTipRecib;
          WTipRecibo = 'P';
          WIdx = 1;
          DOW (WIdx < 4);
             IF (%TRIM(TMP_TIPRECIB) = %TRIM(TDSRECIBO(WIdx)));
                WTipRecibo = TCDRECIBO(WIdx);
                LEAVE;
             ENDIF;
             WIdx += 1;
          ENDDO;
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
         // FIDELIDADE_20140408.csv
         // IMPERIO_20140408.csv

         IF (%SUBST(WNomFile:1:7) = 'IMPERIO');
            WCodConta = 32432; // 41032432;
            WCodCompanhia = 106;
            MONITOR;
               WDatFile = %DEC(%SUBST(WNomFile:9:8):8:0);
            ON-ERROR;
               WDatFile = *ZEROS;
            ENDMON;
         ELSEIF (%SUBST(WNomFile:1:7) = 'FIDELID');
             WCodConta = 73165;
             WCodCompanhia = 011;
            MONITOR;
               WDatFile = %DEC(%SUBST(WNomFile:12:8):8:0);
            ON-ERROR;
               WDatFile = *ZEROS;
            ENDMON;
         ENDIF;

         WHorFile  = %TIME();
       ENDSR;
      /END-FREE

     P*==================================================================
     P* Processo..: ExtAlfaDigits
     P* Descrição.: Extrair a parte Alfanúmerica do campo
     P*==================================================================
     P ExtAlfaDigits   B                   EXPORT
     D ExtAlfaDigits   PI            20A
     D   FieldValue                  20A   CONST
     D
     D*==> Campos de Trabalho
     D WLenField       S              2S 0
     D WChar           S              1A
     D WNumField       S             20A
     D WIdx            S              2S 0
     D*=================================================================
      /FREE
        WNumField = *BLANKS;
        WIdx = 1;
        WLenField = %LEN(FieldValue);
        DOW (WIdx < WLenField);
           WChar = %SUBST(FieldValue:WIdx:1);
           IF ((WChar >= '0') AND (WChar <= '9'));
              WNumField = %TRIM(WNumField) + WChar;
           ENDIF;
           WIdx += 1;
        ENDDO;

        MONITOR;
           RETURN WNumField;
        ON-ERROR;
           RETURN FieldValue;
        ENDMON;
      /END-FREE
     P ExtAlfaDigits   E

     P*==================================================================
     P* Processo..: ConvNumeros
     P* Descrição.: Converter Números
     P*==================================================================
     P ConvNumeros     B                   EXPORT
     D ConvNumeros     PI            15S 5
     D   FieldValue                  15A   CONST
     D
     D*==> Campos de Trabalho
     D WLenField       S              2S 0
     D WChar           S              1A
     D WNumField       S             20A
     D WIdx            S              2S 0
     D*=================================================================
      /FREE
        WNumField = *BLANKS;
        WIdx = 1;
        WLenField = %LEN(FieldValue);
        DOW (WIdx < WLenField);
           WChar = %SUBST(FieldValue:WIdx:1);
           IF (((WChar >= '0') AND (WChar <= '9')) OR (WChar = ','));
              WNumField = %TRIM(WNumField) + WChar;
           ENDIF;
           WIdx += 1;
        ENDDO;

        MONITOR;
           RETURN %DEC(WNumField:15:5);
        ON-ERROR;
           RETURN *ZEROS;
        ENDMON;
      /END-FREE
     P ConvNumeros     E
** Descts ão dos Ramos
Ac. Pessoais Viagem Grupo                         AC.PESS.VG
Ac. Trabalho                                      AC.TRABALH
Automóveis                                        AUTOMOVEL
Embarcações de recreio                            EMB.RECREI
Multi-Risc. Hab.(Sup)Casa+Famíl./Condomn.         MR.HABITA
Multiriscos Comercial                             MR.COMERC
Ocupantes                                         OCUPANTES
Produto Saude Multicare INDIVIDUAL                SAUDE MC
Resp. Civil Familiar                              RC FAMILIA
** Tipo de Recibo
Prémios                       P1
Prémios de Cancelamento       P2
Estorno                       E3
Estornos de Cancelamento      E4
