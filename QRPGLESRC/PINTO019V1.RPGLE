     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*===========================================================
     H* Programa..: PINTO019
     H* Descrição.: Carregar Sinistros IDS Automóvel
     H*
     H* Para Compilar este programa fazer:
     H*    CRTPF FILE(QTEMP/FINTT030) SRCFILE(#JMPRDSRC/QDDSSRC) SIZE(*NOMAX)
     H*    CRTPF FILE(QTEMP/INTFILE) SRCFILE(#JMPRDSRC/QDDSSRC) SRCMBR(FINTT030)
     H*
     H* Para correr este programa em DEBUG fazer:
     H* 1 - CLRPFM FILE(QTEMP/FINTT030)
     H* 2 - CALL PGM(#JMPRDOBJ/PSINT998) PARM('QS36F/FICI150824')
     H* 3 - STRDBG PGM(#JMPRDOBJ/PINTO019)
     H* 4 - CALL PGM(#JMPRDOBJ/PINTO019) PARM('FICI150824' X'F0' 'D' '' '')
     H*===========================================================
     FFINTP010  UF A E           K DISK    PREFIX(INT_)
     FFINTP01001IF   E           K DISK    PREFIX(INT_)
     F                                     RENAME(RINTP010:RINTP01001)
     FFSINT100  UF A E           K DISK    PREFIX(SIN_)
     FFSINT10001IF   E           K DISK    PREFIX(SIN_)
     F                                     RENAME(RSINT100:RSINT10001)
     FFSINT101  UF A E           K DISK    PREFIX(SIN_)
     FFSINT102  UF A E           K DISK    PREFIX(SIN_)
     FFSINT118  UF A E           K DISK    PREFIX(SIN_)
     FFSINT128  UF A E           K DISK    PREFIX(SIN_)
     FFICIDS    UF A E           K DISK    PREFIX(IDS_) USROPN
     F                                     EXTFILE(WExtFile)
     FFINTT030  IF   E           K DISK    PREFIX(TMP_) USROPN
     F                                     EXTFILE(WExtIntFile)
     F*===========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010

     D*==> converter um Campo alfa num número
     D CATN            PR            15S 5
     D   ALFA                        15A   Const
     D   DFT                         15S 5 Const
     D
     D*--> Conversão de Numéricos
     D ConvN           PR            15S 5
     D    PARMPRO                    15S 5 Const
     D    PARMSIN                    15S 5 Const
     D    TIPO                        1A   Const Options(*NOPASS)
     D
     D*--> Conversão de Alfanuméricos
     D ConvA           PR           100A
     D    PARMPRO                   100A   Const
     D    PARMSIN                   100A   Const
     D
     D*==> Chamada de função Dynamic, para trabalhar detalhe do Item
     D ShellExec       PR                  EXTPGM('QCMDEXC')
     D   stdcmd                     300A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5 CONST
     D
     D*==> Campos de Trabalho
     D WNomFile        S             40A
     D WCdInter        S              9S 0
     D WAction         S              1A
     D WResposta       S              1A
     D WMsgErro        S              7A
     D WAAPROC         S                   LIKE(SIN_AAPROC)
     D WNSEQ           S                   LIKE(SIN_NSEQ)
     D WIProc          S                   LIKE(SIN_IPROC)
     D WNordSin        S                   LIKE(SIN_NORDSIN) INZ(*ZEROS)
     D WLastNordSin    S                   LIKE(SIN_NORDSIN) INZ(*ZEROS)
     D WSProc          S                   LIKE(SIN_SPROC) INZ(*ZEROS)
     D WDTINIC         S                   LIKE(SIN_DTINIC) INZ(*ZEROS)
     D WDTFIM          S                   LIKE(SIN_DTFIM) INZ(*ZEROS)
     D WDtSin          S                   LIKE(SIN_DTSIN) INZ(*ZEROS)
     D WNumApol        S                   LIKE(SIN_APOLICE) INZ(*ZEROS)
     D WIsToChange     S              1A   INZ('N')
     D
     D WComando        S            300A   INZ(*BLANKS)
     D WDatFile        S             10A
     D WAAMMDD         S              6A
     D WExtFile        S             21A
     D WExtIntFile     S             21A
     D WNumProc        S              5A
     D WAnoProc        S              4A
     D WCdFile         S              7S 0
     D WNuSequ         S              5S 0
     D WCdRegis        S              9S 0
     D WIsFirst        S               N
     D WCodErro        S              7A
     D
     D*==> Declaração de Constantes
     D MOD#Validar     C                   'V'
     D MOD#Processar   C                   'P'
     D MOD#Debug       C                   'D'
     D ACT#Update      C                   'U'
     D ACT#Insert      C                   'I'
     D ACT#Delete      C                   'D'
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
        IF (WAction  <> MOD#Debug);
           WExtIntFile = 'QTEMP/INTFILE';
           CHAIN (WNomFile) RINTP01001;
           IF (%FOUND());
              WMsgErro = 'INT0001'; // O ficheiro já foi processado
              WResposta = 'N'; // Não é para processar
              RETURN;
           ELSEIF (WAction = MOD#Validar);
              WResposta = 'S'; // É para processar
              WMsgErro = *BLANKS;
              RETURN;
           ENDIF;
        ELSE;
           WExtIntFile = 'QTEMP/FINTT030';
           WExtFile  = 'QS36F/' + %TRIM(WNomFile);
        ENDIF;

        WMsgErro = *BLANKS;
        WIsFirst = *ON;
        EXSR ZapFile;
        IF (WMsgErro = *BLANKS);
           WLastNordSin = *ZEROS;
           EXSR ReadInterface;
        ENDIF;
        *INLR = *ON;

       //=================================================================
       // SubRotina..: ReadInterface
       // Objectivo..: Ler o ficheiro de Interface recebido
       //=================================================================
       BEGSR ReadInterface;
          OPEN FICIDS;
          OPEN FINTT030;
          SETLL *LOVAL RINTT030;
          READ FINTT030;
          DOW (NOT %EOF(FINTT030));
             //-----------------------------------
             // Registar Cabeçalho do Registo
             //-----------------------------------
             IF (WIsFirst) AND (WAction  <> MOD#Debug);
                WIsFirst = *OFF;
                EXSR RegisFile;
             ENDIF;

             //---------------------------------------------
             // Carregar Interface e actualizar Base Dados
             //---------------------------------------------
             IF (CATN(TMP_NUMPROC:0) > *ZEROS);
                IF (WAction  <> MOD#Debug);
                   IDS_TIPPRO  = TMP_TIPO;            // Tipo de Registo
                   IDS_NUMPRO  = CATN(TMP_NUMPROC:0); // Número do Processo (NNNNNYYYY)
                   IDS_DATPRO  = CATN(TMP_DATPROC:0); // Data do Processo
                   IDS_DATSIN  = CATN(TMP_DATSINO:0); // Data do Sinistro
                   IDS_NUMAPO  = CATN(TMP_NUAPOL:0);  // Número da Apólice
                   IDS_MATRIC  = TMP_MATRIC;          // Número da Apólice
                   IDS_CUSPRO  = CATN(TMP_IMPPROC:0); // Custo do Processo
                   IDS_DATENC  = CATN(TMP_DATENCE:0); // Data de Encerramento
                   IDS_NUMSEG  = CATN(TMP_NUMSEGU:0); // Código do Segurado
                   IDS_NOMSEG  = TMP_NOMSEGU;         // Nome do Segurado
                   WRITE FICIDSF1;
                ENDIF;
                EXSR UpdSinistros;
             ENDIF;
             READ RINTT030;
          ENDDO;
          CLOSE FINTT030;
          CLOSE FICIDS;
       ENDSR;

       //=================================================================
       // SubRotina..: UpdSinistros
       // Objectivo..: Actualizar a base de dados de Sinistros
       //=================================================================
       BEGSR UpdSinistros;
          WCodErro = *BLANKS;
          WIProc   = 'P';
          WNumApol = CATN(TMP_NUAPOL:0);  // Número da Apólice
          WNumProc = %SUBST(TMP_NUMPROC:1:5);
          WAnoProc = %SUBST(TMP_NUMPROC:6:4);
          WDtInic  =  CATN(TMP_DATPROC:0);
          WDtFim   =  CATN(TMP_DATENCE:0);
          WDtSin   =  CATN(TMP_DATSINO:0);
          IF (WDtFim <> *ZEROS);
             WSProc  = 5;
          ELSE;
             WSProc  = *ZEROS;
          ENDIF;
          EXSR ActProcessos;
          IF (WCodErro = *BLANKS);
             EXSR ActDetAuto;
          ENDIF;
          IF (WCodErro = *BLANKS);
             EXSR ActViaturas;
          ENDIF;
          IF (WCodErro = *BLANKS);
             IF (CATN(TMP_DATENCE:0) > *ZEROS);
                EXSR ActPagamentos;
             ENDIF;
          ENDIF;
          IF (WCodErro = *BLANKS);
             EXSR ActProvisoes;
          ENDIF;
       ENDSR;

       //==============================================================
       // Subrotina..: ActProcessos
       // Objectivo..: Actualizar os Processos de Sinistros Automóvel
       //==============================================================
       BEGSR ActProcessos;
          //--------------------------------------------------------------
          // Determinar a acção a executar
          //--------------------------------------------------------------
          WAAPROC = CATN(WAnoProc:0);
          WNSEQ   = CATN(WNumProc:0);
          CHAIN (WAAPROC:WNSEQ:50:WNumApol) RSINT10001;
          IF (%FOUND(FSINT10001));
             WAction  = ACT#Update;
             WNordSin = SIN_NORDSIN;
             CHAIN (WNordSin) RSINT100;
             WIsToChange = 'N';
          ELSE;
             WIsToChange = 'S';
             WAction = ACT#Insert;
             IF (WLastNordSin = *ZEROS);
                WLastNordSin  = $GetNextKey('FSINT100':'NORDSIN');
                WLastNordSin += 1;
             ENDIF;
             WNordSin = WLastNordSin;
          ENDIF;

          //--------------------------------------------------------------
          // Formatar campos dos Processos de Sinistro
          //--------------------------------------------------------------
          SIN_DTRINI  = *ZEROS;
          SIN_TOTEST  = *ZEROS;
          SIN_PROV    = *ZEROS;
          SIN_PAGAM   = *ZEROS;
          SIN_RECUP   = *ZEROS;
          SIN_DESPQP  = *ZEROS;
          SIN_RECUPQ  = *ZEROS;
          SIN_LOCSIN  = *BLANKS;
          SIN_CODCON  = *ZEROS;
          SIN_TESTM   = *BLANKS;
          SIN_AUTORI  = *BLANKS;
          SIN_DTPART  = *ZEROS;
          SIN_FPART   = *BLANKS;
          SIN_TIPOAP  = *ZEROS;
          SIN_SITAP   = *ZEROS;
          SIN_DTAPOL  = *ZEROS;

          SIN_FERID   = 'N';
          SIN_RAMO    = 50;
          SIN_IPROC   = 'P';
          SIN_AAPROC  = WAAPROC;
          SIN_NSEQ    = WNSEQ;
          SIN_CAUSA   = '88';
          SIN_NOMPAR  = TMP_NOMSEGU;
          SIN_APOLICE = CATN(TMP_NUAPOL:0);
          SIN_NSEGUR  = CATN(TMP_NUMSEGU:0) - 900000;
          SIN_SPROC   = ConvN(WSPROC     :SIN_SPROC  );
          SIN_DTINIC  = ConvN(WDtInic    :SIN_DTINIC );
          SIN_DTFIM   = ConvN(WDtFim     :SIN_DTFIM  );
          SIN_DTSIN   = ConvN(WDtSin     :SIN_DTSIN  );

          //--------------------------------------------------------------
          // Actualizar ou criar o registo
          //--------------------------------------------------------------
          IF (WIsToChange = 'S');
             SIN_DTUACT  = $DateToInt(%DATE());
             SIN_USERID  = 'LUSITANIA';
             SIN_NORDSIN = WNordSin;
             IF (WAction = ACT#Insert);
                MONITOR;
                   WRITE RSINT100;
                   WLastNordSin += 1;
                ON-ERROR;
                   WCodErro = 'IOE1111'; // Erro na inserção do registo FSINT100
                ENDMON;
             ELSEIF (WAction = ACT#Update);
                MONITOR;
                   UPDATE RSINT100 %FIELDS(SIN_IPROC:SIN_AAPROC:SIN_NSEQ:
                                           SIN_SPROC:SIN_DTINIC:SIN_DTFIM:
                                           SIN_CAUSA:SIN_DTSIN:
                                           SIN_NOMPAR:SIN_TIPOAP:
                                           SIN_SITAP:SIN_APOLICE:
                                           SIN_NSEGUR:SIN_DTUACT:
                                           SIN_USERID);
                   WIsToChange = 'N';
                ON-ERROR;
                   WCodErro = 'IOE1112'; // Erro na Alteração do registo FSINT100
                ENDMON;
             ENDIF;
          ENDIF;
       ENDSR;

       //==============================================================
       // Subrotina..: ActDetAuto
       // Objectivo..: Actualizar o Detalhe de Automóvel
       //==============================================================
       BEGSR ActDetAuto;
          //--------------------------------------------------------------
          // Determinar a acção a executar
          //--------------------------------------------------------------
          CHAIN (WNordSin) RSINT101;
          IF (%FOUND(FSINT101));
             WAction  = ACT#Update;
          ELSE;
             WAction  = ACT#Insert;
          ENDIF;

          //--------------------------------------------------------------
          // Formatar campos de Detalhe de Automóvel
          //--------------------------------------------------------------
          SIN_CAPINI  = *ZEROS;
          SIN_QIVSN   = *BLANKS;
          SIN_CAPQIV  = *ZEROS;
          SIN_PRIV    = *ZEROS;
          SIN_CAPPRV  = *ZEROS;
          SIN_VEXTRA  = *ZEROS;
          SIN_FILL2   = *BLANKS;
          SIN_R1      = *ZEROS;
          SIN_CAP1    = *ZEROS;
          SIN_FRQ1    = *ZEROS;
          SIN_R2      = *ZEROS;
          SIN_CAP2    = *ZEROS;
          SIN_FRQ2    = *ZEROS;
          SIN_R3      = *ZEROS;
          SIN_CAP3    = *ZEROS;
          SIN_FRQ3    = *ZEROS;
          SIN_TIPBON  = *BLANKS;
          SIN_VALBON  = *ZEROS;
          SIN_TIPOPR  = *BLANKS;
          SIN_ACORDO  = *BLANKS;
          SIN_REGSIN  = *BLANKS;
          SIN_AGRAVA  = *BLANKS;
          SIN_NRPROC  = *BLANKS;
          SIN_FLAG1   = *BLANKS;
          SIN_NNPROC  = *BLANKS;
          SIN_FLAG2   = *BLANKS;
          SIN_FILL3   = *BLANKS;
          SIN_AJFIN   = *BLANKS;
          SIN_NPARTI  = *BLANKS;
          SIN_AJUDIC  = *BLANKS;
          SIN_DTRJUD  = *ZEROS;
          SIN_CODFRQ  = *ZEROS;
          SIN_IDS     = 'S';
          SIN_CRDR    = 'D';
          SIN_TPR     = *ZEROS;
          SIN_PRESP   = *ZEROS;

          SIN_DUR     = *ZEROS;  // ConvN(PRO_DUR    :SIN_DUR    );
          SIN_FRAC    = *ZEROS;  // ConvN(PRO_FRAC   :SIN_FRAC   );
          SIN_NMED    = *ZEROS;  // ConvN(PRO_NMED   :SIN_NMED   );
          SIN_TMED    = *BLANKS; // ConvA(PRO_TMED   :SIN_TMED   );

          //--------------------------------------------------------------
          // Actualizar ou criar o registo
          //--------------------------------------------------------------
          IF (WIsToChange = 'S');
             SIN_NORDSIN = WNordSin;
             IF (WAction = ACT#Insert);
                MONITOR;
                   WRITE RSINT101;
                ON-ERROR;
                   WCodErro = 'IOE1113'; // Erro na Criação do registo FSINT101
                ENDMON;
             ELSEIF (WAction = ACT#Update);
                MONITOR;
                   UPDATE RSINT101 %FIELDS(SIN_DUR:SIN_FRAC:SIN_NMED:SIN_TMED);
                   WIsToChange = 'N';
                ON-ERROR;
                   WCodErro = 'IOE1114'; // Erro na Alteração do registo FSINT101
                ENDMON;
             ENDIF;
          ENDIF;
       ENDSR;

       //==============================================================
       // Subrotina..: ActViaturas
       // Objectivo..: Actualizar as Viaturas do Processo
       //==============================================================
       BEGSR ActViaturas;
          //--------------------------------------------------------------
          // Formatar campos de Detalhe da Viatura
          //--------------------------------------------------------------
          WNuSequ = 1;
          CHAIN (WNordSin:WNuSequ) FSINT102;
          IF (%FOUND(FSINT102));
             WAction = ACT#Update;
          ELSE;
             WAction = ACT#Insert;
             WIsToChange = 'S';
          ENDIF;

          //--------------------------------------------------------------
          // Formatar campos de Detalhe da Viatura
          //--------------------------------------------------------------
          SIN_NORDSIN = WNordSin;
          SIN_NUSEQU  = WNuSequ;
          SIN_NUSEGU  = *ZEROS;
          SIN_MARCA   = *BLANKS;
          SIN_CATV    = *ZEROS;
          SIN_AAV     = *ZEROS;
          SIN_MATRIC  = ConvA(TMP_MATRIC:SIN_MATRIC );

          //--------------------------------------------------------------
          // Actualizar o registo
          //--------------------------------------------------------------
          IF (WIsToChange = 'S');
             IF (WAction = ACT#Insert);
                MONITOR;
                   WRITE RSINT102;
                ON-ERROR;
                   WCodErro = 'IO10011'; // Erro na criação do FSINT102
                ENDMON;
             ELSEIF (WAction = ACT#Update);
                MONITOR;
                   UPDATE RSINT102 %FIELDS(SIN_MATRIC);
                ON-ERROR;
                   WCodErro = 'IO10011'; // Erro na criação do FSINT102
                ENDMON;
             ENDIF;
          ENDIF;
       ENDSR;

       //==============================================================
       // Subrotina..: ActProvisoes
       // Objectivo..: Actualizar os Provisoes de Sinistros Automóvel
       //==============================================================
       BEGSR ActProvisoes;
          //--------------------------------------------------------------
          // Obter o Código da Linha da Provisão
          //--------------------------------------------------------------
          CHAIN (WNordSin:1) RSINT118;
          IF (%FOUND(FSINT118));
             WAction  = ACT#Update;
             WIsToChange = 'N';
          ELSE;
             WIsToChange = 'S';
             WAction = ACT#Insert;
          ENDIF;

          //--------------------------------------------------------------
          // Formatar campos dos Provisões de Sinistro
          //--------------------------------------------------------------
          SIN_DESCR   = *BLANKS;
          SIN_VALR3   = *ZEROS;
          SIN_VALR45  = *ZEROS;
          SIN_VLMIP   = *ZEROS;
          SIN_VLDTRA  = *ZEROS;
          SIN_CTAIND  = *BLANKS;
          SIN_SIT1    = *BLANKS;
          SIN_SIT2    = *BLANKS;
          SIN_SIT3    = *BLANKS;
          SIN_VLDCOR  = *ZEROS;

          SIN_NORDSIN = WNordSin;
          SIN_VALOR   = 1050;
          SIN_VALR1   = 1050;
          SIN_NLINHA  = 1;
          SIN_TIPMOV  = 1;
          SIN_VLDMAT  = 1050;
          SIN_DTMOV   = CATN(TMP_DATPROC:0);           // Data do Processo
          SIN_AASIN   = CATN(%SUBST(TMP_DATSINO:1:4):0); // Ano do Sinistro;
          SIN_MMSIN   = CATN(%SUBST(TMP_DATSINO:5:2):0); // Mes do Sinistros;

          //--------------------------------------------------------------
          // Actualizar ou criar o registo
          //--------------------------------------------------------------
          IF (WIsToChange = 'S');
             SIN_DTUACT  = $DateToInt(%DATE());
             SIN_USERID  = 'LUSITANIA';
             IF (WAction = ACT#Insert);
                MONITOR;
                   WRITE RSINT118;
                ON-ERROR;
                   WCodErro = 'IOE1111'; // Erro na inserção do registo FSINT100
                ENDMON;
             ELSEIF (WAction = ACT#Update);
                MONITOR;
                   UPDATE RSINT118 %FIELDS(SIN_VALOR:SIN_VALR1:SIN_DTMOV:
                                           SIN_AASIN:SIN_MMSIN:
                                           SIN_DTUACT:SIN_USERID);
                ON-ERROR;
                   WCodErro = 'IOE1112'; // Erro na Alteração do registo FSINT100
                ENDMON;
             ENDIF;
          ENDIF;
       ENDSR;

       //==============================================================
       // Subrotina..: ActPagamentos
       // Objectivo..: Actualizar os Pagamentos de Sinistros Automóvel
       //==============================================================
       BEGSR ActPagamentos;
          //--------------------------------------------------------------
          // Obter o Código da Linha da Provisão
          //--------------------------------------------------------------
          CHAIN (WNordSin:1) RSINT128;
          IF (%FOUND(FSINT128));
             WAction  = ACT#Update;
             WIsToChange = 'N';
          ELSE;
             WIsToChange = 'S';
             WAction = ACT#Insert;
          ENDIF;

          //--------------------------------------------------------------
          // Formatar campos dos Pagamentos de Sinistro
          //--------------------------------------------------------------
          SIN_DESCR   = *BLANKS;
          SIN_VALR3   = *ZEROS;
          SIN_VALR45  = *ZEROS;
          SIN_VLMIP   = *ZEROS;
          SIN_VLDTRA  = *ZEROS;
          SIN_CTAIND  = *BLANKS;
          SIN_PAGFIN  = *BLANKS;
          SIN_P100    = *BLANKS;
          SIN_FORPAG  = *BLANKS;
          SIN_DTPAG   = *ZEROS;
          SIN_DOCREF  = *BLANKS;
          SIN_CD08    = *BLANKS;
          SIN_BANCO   = *ZEROS;
          SIN_DEPEN   = *ZEROS;
          SIN_TIPENT  = *ZEROS;
          SIN_NUMENT  = *ZEROS;
          SIN_NOMBEN  = *BLANKS;
          SIN_NORBEN  = *BLANKS;
          SIN_LOCBEN  = *BLANKS;
          SIN_CPBEN   = *ZEROS;
          SIN_DTFACT  = *ZEROS;
          SIN_NODOC   = *BLANKS;
          SIN_VLDCOR  = *ZEROS;
          SIN_NRECIB  = *BLANKS;
          SIN_RECEMI  = *BLANKS;
          SIN_ENVIO   = *BLANKS;
          SIN_CHEMI   = *BLANKS;

          SIN_NORDSIN = WNordSin;
          SIN_NLINHA  = 1;
          SIN_TIPMOV  = 112;
          SIN_DTMOV   = CATN(TMP_DATENCE:0);           // Data de Encerramento
          SIN_VALOR   = CATN(TMP_IMPPROC:0) / 100;     // Valor do Registo
          SIN_VALR1   = CATN(TMP_IMPPROC:0) / 100;     // Valor do Registo
          SIN_AASIN   = CATN(%SUBST(TMP_DATSINO:1:4):0); // Ano do Sinistro;
          SIN_MMSIN   = CATN(%SUBST(TMP_DATSINO:5:2):0); // Mes do Sinistros;
          SIN_VLDMAT  = CATN(TMP_IMPPROC:0) / 100;     // Valor do Pagamento

          //--------------------------------------------------------------
          // Actualizar ou criar o registo
          //--------------------------------------------------------------
          IF (WIsToChange = 'S');
             SIN_DTUACT  = $DateToInt(%DATE());
             SIN_USERID  = 'LUSITANIA';
             IF (WAction = ACT#Insert);
                MONITOR;
                   WRITE RSINT128;
                ON-ERROR;
                   WCodErro = 'IOE1111'; // Erro na inserção do registo FSINT100
                ENDMON;
             ELSEIF (WAction = ACT#Update);
                MONITOR;
                   UPDATE RSINT128 %FIELDS(SIN_DTMOV:SIN_VALOR:SIN_VALR1:
                                           SIN_AASIN:SIN_MMSIN:SIN_VLDMAT:
                                           SIN_DTUACT:SIN_USERID);
                ON-ERROR;
                   WCodErro = 'IOE1112'; // Erro na Alteração do registo FSINT100
                ENDMON;
             ENDIF;
          ENDIF;
       ENDSR;

       //=================================================================
       // SubRotina..: ZapFile
       // Objectivo..: Limpar ficheiro antes de carregar com dados
       //=================================================================
       BEGSR ZapFile;
          IF (WAction  <> MOD#Debug);
          MONITOR;
             WAAMMDD  = %SUBST(WNomFile:17:6);
             WExtFile = 'QS36F/FICI' + %TRIM(WAAMMDD);
             WComando = 'CRTPF FILE(' + %TRIM(WExtFile) + ') +
                               SRCFILE(SOURCE/QDDSSRC) +
                               SRCMBR(FICIDS) SIZE(*NOMAX)';
             ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
          ON-ERROR;
             WMsgErro = 'XXXXXXX';
          ENDMON;

          MONITOR;
             WComando = 'CLRPFM FILE(' + %TRIM(WExtFile) + ')';
             ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
          ON-ERROR;
             WMsgErro = 'XXXXXXX';
          ENDMON;
          ENDIF;
       ENDSR;

       //=================================================================
       // SubRotina..: RegisFile
       // Objectivo..: Registar o Ficheiro
       //=================================================================
       BEGSR RegisFile;
          WNuSequ     = 1;
          WCdRegis    = $GetNextKey('FENTP010':'CDREGIS');
          WCdFile     = $GetNextKey('FINTP010':'CDFILE');
          INT_CDFILE  = WCdFile;
          INT_NOMFILE = WNomFile;
          MONITOR;
             WDatFile    = %SUBST(WNomFile:15:4) + '-' +
                           %SUBST(WNomFile:19:2) + '-' +
                           %SUBST(WNomFile:21:2);
             INT_DTFILE  = %DATE(WDatFile:*ISO); // Formatar com a data do ficheiro
          ON-ERROR;
             INT_DTFILE  = %DATE();
          ENDMON;
          INT_DTPROC  = %DATE();
          WRITE RINTP010;
       ENDSR;
      /END-FREE
     P*=======================================================
     P* Processo..: CATN
     P* Descrição.: Converter campo alfa num Numero
     P*=======================================================
     P CATN            B                   EXPORT
     D                 PI            15S 5
     D   ALFA                        15A   Const
     D   DFT                         15S 5 Const
     D WResult         S             15S 0
     D*===========================================================
      /FREE
         WResult = DFT;
         IF (ALFA <> *BLANKS);
            MONITOR;
               WResult = %DEC(ALFA:15:5);
            ON-ERROR;
            ENDMON;
         ENDIF;

         RETURN WResult;
      /END-FREE
     P CATN            E

     P*=========================================================
     P* Processo..: ConvN
     P* Descrição.: Converter Campos numéricos, e validar se
     P*             existem alterações, em relação ao original.
     P*=========================================================
     P ConvN           B                   EXPORT
     D                 PI            15S 5
     D    PARMPRO                    15S 5 Const
     D    PARMSIN                    15S 5 Const
     D    TIPO                        1A   Const Options(*NOPASS)
     D
     D*==> Campos de Trabalho
     D WNumero         S             15S 5
     D WTipo           S              1A
     D*===========================================================
      /FREE
       MONITOR;
          WNumero = PARMPRO;
       ON-ERROR;
          WNumero = *ZEROS;
       ENDMON;
       WTipo = 'G';
       IF (%PARMS() > 2);
          WTipo = TIPO;
       ENDIF;

       IF (WTipo = 'D') AND (WNumero > *ZEROS);
          WNumero += 20000000;
       ELSEIF (WTipo = 'A') AND (WNumero > *ZEROS);
         IF (WNumero >= 30);
            WNumero += 1900;
         ELSE;
            WNumero += 2000;
         ENDIF;
       ENDIF;
       IF (WNumero <> PARMSIN);
          WIsToChange = 'S';
       ENDIF;
       RETURN WNumero;
      /END-FREE
     P ConvN           E

     P*=======================================================
     P* Processo..: ConvA
     P* Descrição.: Converter Campos Alfanumericos
     P*=======================================================
     P ConvA           B                   EXPORT
     D                 PI           100A
     D    PARMPRO                   100A   Const
     D    PARMSIN                   100A   Const
     D*===========================================================
      /FREE
       IF (%TRIM(PARMPRO) <> %TRIM(PARMSIN));
          WIsToChange = 'S';
       ENDIF;
       RETURN PARMPRO;
      /END-FREE
     P ConvA           E
