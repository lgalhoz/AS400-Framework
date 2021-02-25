     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*===========================================================
     H* Programa..: PINTO019
     H* Descrição.: Carregar Sinistros IDS Automóvel
     H*
     H* Para Compilar este programa fazer:
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
     FFICIDS    UF A E           K DISK    PREFIX(TMP_) USROPN
     F                                     EXTFILE(WExtFile)
     F*===========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010

     D*==> converter um Campo alfa num número
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
     D*==> Decompor Campo Processo
     D FLD#NumPro      DS                  QUALIFIED
     D   Numero                1      9S 0
     D   Sequencia             1      5S 0
     D   Ano                   6      9S 0
     D
     D*==> Decompor Campo Data   o
     D FLD#Data        DS                  QUALIFIED
     D   Data                  1      8S 0
     D   Ano                   1      4S 0
     D   Mes                   5      6S 0
     D   Dia                   7      8S 0
     D
     D*==> Campos de Trabalho
     D WMsgErro        S              7A
     D WAAPROC         S                   LIKE(SIN_AAPROC)
     D WNSEQ           S                   LIKE(SIN_NSEQ)
     D WIProc          S                   LIKE(SIN_IPROC)
     D WNordSin        S                   LIKE(SIN_NORDSIN) INZ(*ZEROS)
     D WLastNordSin    S                   LIKE(SIN_NORDSIN) INZ(*ZEROS)
     D WSProc          S                   LIKE(SIN_SPROC) INZ(*ZEROS)
     D WNumApol        S                   LIKE(SIN_APOLICE) INZ(*ZEROS)
     D WAction         S              1A
     D WIsToChange     S              1A   INZ('N')
     D
     D WComando        S            300A   INZ(*BLANKS)
     D WDatFile        S             10A
     D WAAMMDD         S              6A
     D WExtFile        S             21A
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
     C                   PARM                    WExtFile
     C                   PARM                    WMsgErro
      /FREE
        WMsgErro = *BLANKS;
        WIsFirst = *ON;
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
          READ FICIDS;
          DOW (NOT %EOF(FICIDS));
             //---------------------------------------------
             // Carregar Interface e actualizar Base Dados
             //---------------------------------------------
             IF (TMP_NUMPRO > *ZEROS);
                EXSR UpdSinistros;
             ENDIF;
             READ FICIDS;
          ENDDO;
          CLOSE FICIDS;
       ENDSR;

       //=================================================================
       // SubRotina..: UpdSinistros
       // Objectivo..: Actualizar a base de dados de Sinistros
       //=================================================================
       BEGSR UpdSinistros;
          WCodErro = *BLANKS;
          WIProc   = 'P';
          WNumApol = TMP_NUMAPO;  // Número da Apólice
          FLD#NumPro.Numero = TMP_NUMPRO;
          IF (TMP_DATENC > *ZEROS);
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
             IF (TMP_DATENC > *ZEROS);
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
          WAAPROC = FLD#NumPro.Ano;
          WNSEQ   = FLD#NumPro.Sequencia;
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
          SIN_NOMPAR  = TMP_NOMSEG;
          SIN_APOLICE = TMP_NUMAPO;
          SIN_NSEGUR  = TMP_NUMSEG - 900000;
          SIN_SPROC   = ConvN(WSPROC     :SIN_SPROC  );
          SIN_DTINIC  = ConvN(TMP_DATPRO :SIN_DTINIC );
          SIN_DTFIM   = ConvN(TMP_DATENC :SIN_DTFIM  );
          SIN_DTSIN   = ConvN(TMP_DATSIN :SIN_DTSIN  );

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

          FLD#Data.data = TMP_DATSIN;
          SIN_NORDSIN   = WNordSin;
          SIN_VALOR     = 1050;
          SIN_VALR1     = 1050;
          SIN_NLINHA    = 1;
          SIN_TIPMOV    = 1;
          SIN_VLDMAT    = 1050;
          SIN_DTMOV     = FLD#Data.data; // Data do Processo
          SIN_AASIN     = FLD#Data.Ano;  // Ano do Sinistro;
          SIN_MMSIN     = FLD#Data.Mes;  // Mes do Sinistros;

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

          FLD#Data.data = TMP_DATSIN;
          SIN_NORDSIN   = WNordSin;
          SIN_NLINHA   = 1;
          SIN_TIPMOV   = 112;
          SIN_DTMOV    = TMP_DATENC;       // Data de Encerramento
          SIN_VALOR    = TMP_CUSPRO / 100; // Valor do Registo
          SIN_VALR1    = TMP_CUSPRO / 100; // Valor do Registo
          SIN_AASIN    = FLD#Data.ano;     // Ano do Sinistro;
          SIN_MMSIN    = FLD#Data.mes;     // Mes do Sinistros;
          SIN_VLDMAT   = TMP_CUSPRO / 100; // Valor do Pagamento

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
      /END-FREE

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
