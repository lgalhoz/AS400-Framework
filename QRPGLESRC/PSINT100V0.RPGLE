     H*====================================================================
     H* Programa...: PSINT100
     H* Objectivo..: Normalizar Informação dos Processos de Sinistros
     H*              Automóvel
     H*
     H*====================================================================
     H ALWNULL(*USRCTL) DFTACTGRP(*NO) BNDDIR('JMDIR') ACTGRP(*NEW)
     H FIXNBR(*ZONED)
     H*====================================================================
     FGSINPRO   IF   E           K DISK    EXTFILE('G.SINPRO') PREFIX(PRO_)
     FIDSPRO    IF   E           K DISK    PREFIX(PRO_)
     F                                     RENAME(GSINPROF:IDSNPROF)
     FFSINT100  UF A E           K DISK    PREFIX(SIN_)
     FFSINT10001IF   E           K DISK    PREFIX(SIN_)
     F                                     RENAME(RSINT100:RSINT10001)
     FFSINT101  UF A E           K DISK    PREFIX(SIN_)
     FFSINT102  UF A E           K DISK    PREFIX(SIN_)
     F*====================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SSECO010

     D*--> Processos
     D ConvN           PR            15S 5
     D    PARMPRO                    15S 5 Const
     D    PARMSIN                    15S 5 Const
     D    TIPO                        1A   Const Options(*NOPASS)
     D
     D ConvA           PR           100A
     D    PARMPRO                   100A   Const
     D    PARMSIN                   100A   Const
     D
     D*--> Campos de Trabalho
     D WAAPROC         S                   LIKE(SIN_AAPROC)
     D WNSEQ           S                   LIKE(SIN_NSEQ)
     D WNordSin        S                   LIKE(SIN_NORDSIN) INZ(*ZEROS)
     D WNuSequ         S                   LIKE(SIN_NUSEQU) INZ(*ZEROS)
     D WLastNordSin    S                   LIKE(SIN_NORDSIN) INZ(*ZEROS)
     D WAction         S              1A   INZ(*BLANKS)
     D WIsToChange     S              1A   INZ('N')
     D WBkMARC         S                   LIKE(PRO_MARC1)
     D WBkMATR         S                   LIKE(PRO_MATR1)
     D WBkCATV         S                   LIKE(PRO_CATV1)
     D WBkAAV          S                   LIKE(PRO_AAV1)
     D WCodErro        S              7A   INZ(*BLANKS)
     D
     D*--> Constantes
     D ACT#Update      C                   'U'
     D ACT#Insert      C                   'I'
     D ACT#Delete      C                   'D'
     D*====================================================================
     IIDSNPROF
     I              PRIV                        PRO_PRV4
     I*====================================================================
      /FREE
       //------------------------------------------
       // Actualizar informação a partir do GSINPRO
       //------------------------------------------
       SETLL *LOVAL GSINPRO;
       READ GSINPRO;
       DOW NOT %EOF(GSINPRO);
          WAAPROC = 2000 + PRO_AAPROC;
          WNSEQ   = PRO_NSEQ;
          EXSR ReadProcessos;
          READ GSINPRO;
       ENDDO;

       //------------------------------------------
       // Actualizar informação a partir do GSINPRO
       //------------------------------------------
       SETLL *LOVAL IDSPRO;
       READ GSINPRO;
       DOW NOT %EOF(IDSPRO);
          WAAPROC    = 2000 + PRO_AAPROC;
          IF (PRO_RM = 15);
             WNSEQ = PRO_NSEQ + 80000;
          ELSE;
             WNSEQ = PRO_NSEQ + 90000;
          ENDIF;

          PRO_TOTEST = *ZEROS;
          PRO_FRQ1   = *ZEROS;
          PRO_CAP2   = *ZEROS;
          PRO_FRQ2   = *ZEROS;
          PRO_CAP3   = *ZEROS;
          PRO_FRQ3   = *ZEROS;
          PRO_CAP1   = *ZEROS;
          PRO_TOTEST = *ZEROS;
          PRO_PROV   = *ZEROS;
          PRO_PAGAM  = *ZEROS;
          PRO_RECUP  = *ZEROS;
          PRO_DESPQP = *ZEROS;
          PRO_RECUPQ = *ZEROS;
          PRO_CAPINI = *ZEROS;
          PRO_CAPQIV = *ZEROS;
          PRO_CAPPRV = *ZEROS;
          PRO_VEXTRA = *ZEROS;
          PRO_VALBON = *ZEROS;
          PRO_PRESP  = *ZEROS;

          EXSR ReadProcessos;
          READ IDSPRO;
       ENDDO;

       *INLR = *ON;

       //==============================================================
       // Subrotina..: ReadProcessos
       // Objectivo..: Procede à leitura dos Processos de Sinistros
       //==============================================================
       BEGSR ReadProcessos;
          WCodErro    = *BLANKS;
          WIsToChange = 'N';
          EXSR ActProcessos;

          //------------------------------------------
          // Actualizar informação de detalhe
          //------------------------------------------
          IF (WCodErro = *BLANKS);
             EXSR ActDetAuto;
          ENDIF;

          //------------------------------------------
          // Actualizar informação da primeira viatura
          //------------------------------------------
          IF (WCodErro = *BLANKS);
             WIsToChange = 'N';
             WNuSequ  = 1;
             WBkMARC  = PRO_MARC1;
             WBkMATR  = PRO_MATR1;
             WBkCATV  = PRO_CATV1;
             WBkAAV   = PRO_AAV1;
             EXSR ActViaturas;
          ENDIF;

          //------------------------------------------
          // Actualizar informação da segunda viatura
          //------------------------------------------
          IF (WCodErro = *BLANKS);
             WIsToChange = 'N';
             WNuSequ  = 2;
             WBkMARC  = PRO_MARC2;
             WBkMATR  = PRO_MATR2;
             WBkCATV  = PRO_CATV2;
             WBkAAV   = PRO_AAV2;
             EXSR ActViaturas;
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
          CHAIN (PRO_IPROC:WAAPROC:WNSEQ) RSINT10001;
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
          // ForCompr campos dos Processos de Sinistro
          //--------------------------------------------------------------
          SIN_IPROC   = ConvA(PRO_IPROC  :SIN_IPROC  );
          SIN_SPROC   = ConvN(PRO_SPROC  :SIN_SPROC  );
          SIN_DTINIC  = ConvN(PRO_DTINIC :SIN_DTINIC :'D');
          SIN_DTRINI  = ConvN(PRO_DTRINI :SIN_DTRINI :'D');
          SIN_DTFIM   = ConvN(PRO_DTFIM  :SIN_DTFIM  :'D');
          SIN_TOTEST  = ConvN(PRO_TOTEST :SIN_TOTEST );
          SIN_PROV    = ConvN(PRO_PROV   :SIN_PROV   );
          SIN_PAGAM   = ConvN(PRO_PAGAM  :SIN_PAGAM  );
          SIN_RECUP   = ConvN(PRO_RECUP  :SIN_RECUP  );
          SIN_DESPQP  = ConvN(PRO_DESPQP :SIN_DESPQP );
          SIN_RECUPQ  = ConvN(PRO_RECUPQ :SIN_RECUPQ );
          SIN_CAUSA   = ConvA(PRO_CAUSA  :SIN_CAUSA  );
          SIN_DTSIN   = ConvN(PRO_DTSIN  :SIN_DTSIN  :'D');
          SIN_LOCSIN  = ConvA(PRO_LOCSIN :SIN_LOCSIN );
          SIN_CODCON  = ConvN(PRO_CODCON :SIN_CODCON );
          SIN_FERID   = ConvA(PRO_FERID  :SIN_FERID  );
          SIN_TESTM   = ConvA(PRO_TESTM  :SIN_TESTM  );
          SIN_AUTORI  = ConvA(PRO_AUTORI :SIN_AUTORI );
          SIN_DTPART  = ConvN(PRO_DTPART :SIN_DTPART :'D');
          SIN_FPART   = ConvA(PRO_FPART  :SIN_FPART  );
          SIN_NOMPAR  = ConvA(PRO_NOMPAR :SIN_NOMPAR );
          SIN_TIPOAP  = ConvN(PRO_TIPOAP :SIN_TIPOAP );
          SIN_SITAP   = ConvN(PRO_SITAP  :SIN_SITAP  );
          SIN_RAMO    = ConvN(PRO_RM     :SIN_RAMO   );
          SIN_APOLICE = ConvN(PRO_AP     :SIN_APOLICE);
          SIN_NSEGUR  = ConvN(PRO_NSEG   :SIN_NSEGUR );
          SIN_DTAPOL  = ConvN(PRO_DTAPOL :SIN_DTAPOL :'D');
          SIN_DTUACT  = ConvN(PRO_DTUACT :SIN_DTUACT :'D');
          SIN_USERID  = ConvA(PRO_USERID :SIN_USERID );
          SIN_AAPROC  = WAAPROC;
          SIN_NSEQ    = WNSEQ;

          //--------------------------------------------------------------
          // Actualizar ou criar o registo
          //--------------------------------------------------------------
          IF (WIsToChange = 'S');
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
                                           SIN_SPROC:SIN_DTINIC:SIN_DTRINI:
                                           SIN_DTFIM:SIN_TOTEST:SIN_PROV:
                                           SIN_PAGAM:SIN_RECUP:SIN_DESPQP:
                                           SIN_RECUPQ:SIN_CAUSA:SIN_DTSIN:
                                           SIN_LOCSIN:SIN_CODCON:SIN_FERID:
                                           SIN_TESTM:SIN_AUTORI:SIN_DTPART:
                                           SIN_FPART:SIN_NOMPAR:SIN_TIPOAP:
                                           SIN_SITAP:SIN_RAMO:SIN_APOLICE:
                                           SIN_NSEGUR:SIN_DTAPOL:SIN_DTUACT:
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
          // ForCompr campos de Detalhe de Automóvel
          //--------------------------------------------------------------
          SIN_CAPINI  = ConvN(PRO_CAPINI :SIN_CAPINI );
          SIN_QIVSN   = ConvA(PRO_QIVSN  :SIN_QIVSN  );
          SIN_CAPQIV  = ConvN(PRO_CAPQIV :SIN_CAPQIV );
          SIN_PRIV    = ConvA(PRO_PRIV   :SIN_PRIV   );
          SIN_CAPPRV  = ConvN(PRO_CAPPRV :SIN_CAPPRV );
          SIN_VEXTRA  = ConvN(PRO_VEXTRA :SIN_VEXTRA );
          SIN_DUR     = ConvN(PRO_DUR    :SIN_DUR    );
          SIN_FRAC    = ConvN(PRO_FRAC   :SIN_FRAC   );
          SIN_FILL2   = ConvA(PRO_FILL2  :SIN_FILL2  );
          SIN_R1      = ConvN(PRO_R1     :SIN_R1     );
          SIN_CAP1    = ConvN(PRO_CAP1   :SIN_CAP1   );
          SIN_FRQ1    = ConvN(PRO_FRQ1   :SIN_FRQ1   );
          SIN_R2      = ConvN(PRO_R2     :SIN_R2     );
          SIN_CAP2    = ConvN(PRO_CAP2   :SIN_CAP2   );
          SIN_FRQ2    = ConvN(PRO_FRQ2   :SIN_FRQ2   );
          SIN_R3      = ConvN(PRO_R3     :SIN_R3     );
          SIN_CAP3    = ConvN(PRO_CAP3   :SIN_CAP3   );
          SIN_FRQ3    = ConvN(PRO_FRQ3   :SIN_FRQ3   );
          SIN_TIPBON  = ConvA(PRO_TIPBON :SIN_TIPBON );
          SIN_VALBON  = ConvN(PRO_VALBON :SIN_VALBON );
          SIN_TIPOPR  = ConvA(PRO_TIPOPR :SIN_TIPOPR );
          SIN_ACORDO  = ConvA(PRO_ACORDO :SIN_ACORDO );
          SIN_REGSIN  = ConvA(PRO_REGSIN :SIN_REGSIN );
          SIN_AGRAVA  = ConvA(PRO_AGRAVA :SIN_AGRAVA );
          SIN_NRPROC  = ConvA(PRO_NRPROC :SIN_NRPROC );
          SIN_FLAG1   = ConvA(PRO_FLAG1  :SIN_FLAG1  );
          SIN_NNPROC  = ConvA(PRO_NNPROC :SIN_NNPROC );
          SIN_FLAG2   = ConvA(PRO_FLAG2  :SIN_FLAG2  );
          SIN_FILL3   = ConvA(PRO_FILL3  :SIN_FILL3  );
          SIN_AJFIN   = ConvA(PRO_AJFIN  :SIN_AJFIN  );
          SIN_NPARTI  = ConvA(PRO_NPARTI :SIN_NPARTI );
          SIN_AJUDIC  = ConvA(PRO_AJUDIC :SIN_AJUDIC );
          SIN_DTRJUD  = ConvN(PRO_DTRJUD :SIN_DTRJUD :'D');
          SIN_NMED    = ConvN(PRO_NMED   :SIN_NMED   );
          SIN_TMED    = ConvA(PRO_TMED   :SIN_TMED   );
          SIN_CODFRQ  = ConvN(PRO_CODFRQ :SIN_CODFRQ );
          SIN_IDS     = ConvA(PRO_IDS    :SIN_IDS    );
          SIN_CRDR    = ConvA(PRO_CRDR   :SIN_CRDR   );
          SIN_TPR     = ConvN(PRO_TPR    :SIN_TPR    );
          SIN_PRESP   = ConvN(PRO_PRESP  :SIN_PRESP  );

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
                   UPDATE RSINT101 %FIELDS(SIN_CAPINI:SIN_QIVSN:SIN_CAPQIV:
                                            SIN_PRIV:SIN_CAPPRV:SIN_VEXTRA:
                                            SIN_DUR:SIN_FRAC:SIN_FILL2:SIN_R1:
                                            SIN_CAP1:SIN_FRQ1:SIN_R2:SIN_CAP2:
                                            SIN_FRQ2:SIN_R3:SIN_CAP3:SIN_FRQ3:
                                            SIN_TIPBON:SIN_VALBON:SIN_TIPOPR:
                                            SIN_ACORDO:SIN_REGSIN:SIN_AGRAVA:
                                            SIN_NRPROC:SIN_FLAG1:SIN_NNPROC:
                                            SIN_FLAG2:SIN_FILL3:SIN_AJFIN:
                                            SIN_NPARTI:SIN_AJUDIC:SIN_DTRJUD:
                                            SIN_NMED:SIN_TMED:SIN_CODFRQ:
                                            SIN_IDS:SIN_CRDR:SIN_TPR:SIN_PRESP);
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
          // ForCompr campos de Detalhe da Viatura
          //--------------------------------------------------------------
          IF ((%TRIM(WBkMARC) <> *BLANKS) OR
              (%TRIM(WBkMATR) <> *BLANKS));
             CHAIN (WNordSin:WNuSequ) FSINT102;
             IF (%FOUND(FSINT102));
                WAction = ACT#Update;
             ELSE;
                WAction = ACT#Insert;
                WIsToChange = 'S';
             ENDIF;

             //--------------------------------------------------------------
             // ForCompr campos de Detalhe da Viatura
             //--------------------------------------------------------------
             SIN_NORDSIN = WNordSin;
             SIN_NUSEQU  = WNuSequ;
             SIN_NUSEGU  = *ZEROS;
             SIN_MARCA   = ConvA(WBkMARC  :SIN_MARCA  );
             SIN_MATRIC  = ConvA(WBkMATR  :SIN_MATRIC );
             SIN_CATV    = ConvN(WBkCATV  :SIN_CATV   );
             SIN_AAV     = ConvN(WBkAAV   :SIN_AAV    :'A');

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
                      UPDATE RSINT102 %FIELDS(SIN_MARCA:SIN_MATRIC:SIN_CATV:
                                              SIN_AAV);
                   ON-ERROR;
                      WCodErro = 'IO10011'; // Erro na criação do FSINT102
                   ENDMON;
                ENDIF;
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
