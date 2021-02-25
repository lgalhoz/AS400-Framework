     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PGRLO004
     H* Descrição.: Controlo de Certificados Provisórios
     H*==================================================================
     FFGRLT040  UF   E           K DISK    PREFIX(TMP_)
     FFGRLP040  UF A E           K DISK    PREFIX(GRL_)
     FFGRLP042  UF A E           K DISK    PREFIX(GRL_)
     FAUDCPROV  O    F  120        PRINTER OFLIND(PrintOf) PRTCTL(PrintDS)
     F                                     EXTFILE('QSYSPRT')
     F                                     USROPN
     F*==================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010

     D*==> Estrura de dado para controlo da Impressora
     D PrintDS         DS
     D   $Fill                 1     12
     D   $CurLin              13     15  0
     D
     D*==> Campos de Trabalho
     D WNuSegu         S                   LIKE(GRL_NUSEGU)
     D WNuApol         S                   LIKE(GRL_NUAPOL)
     D WCdEstado       S                   LIKE(GRL_CDESTADO)
     D WCodSessao      S              5S 0 INZ(*ZEROS)
     D WDatAltera      S               D
     D WDatHoje        S               D
     D WMsgErro        S             80A
     D WIsPrintOn      S               N   INZ(*OFF)
     D WIcErro         S               N   INZ(*OFF)
     D*==================================================================
      /FREE
        //----------------------------------------------------------------------
        // Proceder á leitura dos Certificados Provisórios
        //----------------------------------------------------------------------
        WIsPrintOn = *OFF;
        WDatHoje   = %DATE();
        SETLL *LOVAL FGRLT040;
        READ FGRLT040;
        DOW (NOT %EOF(FGRLT040));
           WIcErro  = *OFF;
           WMsgErro = *OFF;
           CHAIN (TMP_CDCPROV) RGRLP040;
           IF (NOT %FOUND());
              GRL_CDCPROV    = TMP_CDCPROV;    // Certificado Provisório
              GRL_NUSEGNET   = *BLANKS;        // Número SegurNet
              GRL_TPSEGURO   = ' ';            // Tipo de Viatura
              GRL_MATRICULA  = TMP_MATRICULA;  // Matricula
              GRL_MARCA      = TMP_MARCA;      // Marca
              GRL_CDNIF      = TMP_CODNIF;     // Número de Contribuinte
              GRL_TOMADOR    = TMP_TOMADOR;    // Nome do Tomador de Seguro
              GRL_NUSEGU     = *ZEROS;
              GRL_LOCALIDADE = TMP_LOCALIDADE; // Localidade
              GRL_DTINICIO   = TMP_DTEMISSAO;  // Data de Emissão
              GRL_NUAPOL     = *ZEROS;
              IF (TMP_APOLICE <> 'Emitir');
                 GRL_NUAPOL = %DEC(TMP_APOLICE:7:0);
              ENDIF;
              EXSR GetMediador;                // Código do Mediador
              GRL_GRL04001 = %DATE();
              GRL_GRL04002 = %DATE();

              IF (NOT WIcErro);
                 WRITE RGRLP040;
                 GRL_NUSEQU  = $GetNextKey('FGRLP042':'NUSEQU':
                                       '(CDCPROV='''+%CHAR(GRL_CDCPROV)+''')');
                 EXSR GetEstado;
                 GRL_DTESTADO = %DATE();
                 GRL_GRL04201 = %DATE();
                 GRL_GRL04202 = %DATE();
                 WRITE RGRLP042;
              ENDIF;
           ELSE;
              GRL_NUSEQU  = $GetNextKey('FGRLP042':'NUSEQU':
                                   '(CDCPROV='''+%CHAR(GRL_CDCPROV)+''')');
              GRL_NUSEQU -= 1;
              CHAIN (GRL_CDCPROV:GRL_NUSEQU) RGRLP042;
              IF (%FOUND());
                 WCdEstado = GRL_CDESTADO;
                 ExSR GetEstado;
                 IF (WCdEstado <> GRL_CDESTADO);
                    GRL_NUSEQU += 1;
                    GRL_DTESTADO = %DATE();
                    GRL_GRL04201 = %DATE();
                    GRL_GRL04202 = %DATE();
                    WRITE RGRLP042;
                 ENDIF;
              ENDIF;
           ENDIF;

           IF (NOT WIcErro);
              DELETE RGRLT040;
           ENDIF;

           READ FGRLT040;
        ENDDO;

        //----------------------------------------------------------------------
        // Proceder à actualização da Informação
        //----------------------------------------------------------------------
        SETLL *LOVAL FGRLP040;
        READ FGRLP040;
        DOW (NOT %EOF(FGRLP040));
           WMsgErro = *BLANKS;
           WNuApol = GRL_NUAPOL;
           WNuSegu = GRL_NUSEGU;
           IF ((GRL_NUAPOL=*ZEROS) AND (GRL_MATRICULA<>*BLANKS) AND
               (GRL_MATRICULA<>'S/EFEITO')) OR ((GRL_NUSEGU=*ZEROS) AND
               (GRL_CDNIF<>*ZEROS));
              EXSR GetNumApolice;              // Código da Apólice
              EXSR GetSegurado;                // Código do Tomador de Seguro
              IF (GRL_NuSegu <> WNuSegu) OR (GRL_NuApol <> WNuApol);
                 //--------------------------------------------------
                 // Validar data de inicio da alteração
                 //--------------------------------------------------
                 IF (GRL_NuApol <> WNuApol) AND (WNuApol = *ZEROS);
                    IF (WDatAltera > GRL_DTINICIO)
                       AND (GRL_DTINICIO >= $IntToDate(20130701));
                       WMsgErro = 'Data de alteração ('+%CHAR(WDatAltera)+')'
                                 +' da Apólice ('+%CHAR(GRL_NUAPOL)+') '
                                 +', Superior à Data Inicio.';
                       EXSR PrintLine;
                       IF (GRL_NUSEGU = *ZEROS);
                          WMsgErro = 'O Segurado com o NIF='+%CHAR(GRL_CDNIF)+
                                     ', não está definido para a Apólice '+
                                     %CHAR(GRL_NUAPOL);
                          EXSR PrintLine;
                       ENDIF;
                    ENDIF;
                 ENDIF;
                 GRL_GRL04002 = %DATE();
                 UPDATE RGRLP040 %FIELDS(GRL_NUAPOL:GRL_NUSEGU:GRL_GRL04002);
              ELSE;
                 IF (GRL_NUAPOL=*ZEROS) AND (GRL_DTINICIO>$IntToDate(20130501));
                    WMsgErro = 'Não está definida a Apólice, para a'+
                               ' matricula '+%TRIM(GRL_MATRICULA);
                    EXSR PrintLine;
                 ENDIF;
              ENDIF;
           ENDIF;
           READ FGRLP040;
        ENDDO;
        CLOSE AUDCPROV;
        *INLR = *ON;

        //=======================================================================
        // Subrotina.: PrintLine
        // Objectivo.: Escrever uma ocorrência
        //=======================================================================
        BEGSR PrintLine;
           IF (NOT WIsPrintOn);
              OPEN AUDCPROV;
              EXCEPT  PRT_HEADER;
              WIsPrintOn = *ON;
           ENDIF;

           EXCEPT  PRT_DETAIL;
           IF (PrintOf);
              EXCEPT PRT_HEADER;
              PrintOf = *OFF;
           ENDIF;
           WMsgErro = *BLANKS;
        ENDSR;

        //=======================================================================
        // Subrotina.: GetEstado
        // Objectivo.: Obter o Estado do Certificado
        //=======================================================================
        BEGSR GetEstado;
           IF (TMP_Estado = 'Crítico');
              GRL_CDESTADO = 'CR';
           ELSEIF (TMP_Estado = 'Sem Efeito');
              GRL_CDESTADO = 'SE';
           ELSEIF (TMP_Estado = 'Pendente');
              GRL_CDESTADO = 'PN';
           ELSEIF (TMP_Estado = 'Tratado');
              GRL_CDESTADO = 'TR';
           ELSE;
              GRL_CDESTADO = '??';
              WMsgErro = 'O Código '+%TRIM(TMP_ESTADO)+', é desconhecido';
              EXSR PrintLine;
           ENDIF;
        ENDSR;

        //=======================================================================
        // SubroCina.: GetSegurado
        // Objectivo.: Obter o Código do Segurado
        //=======================================================================
        BEGSR GetSegurado;
           GRL_NUSEGU = *ZEROS;
           WCodSessao = $SetFiles('GFSEG');
           $SetCriteria('(NIF = '''+%CHAR(GRL_CDNIF)+''')');
           IF ($GetRecord() > *ZEROS);
              GRL_NUSEGU = $GetFieldNum('NSEG');
           ENDIF;
           $RmvDBSession(WCodSessao);
        ENDSR;

        //=======================================================================
        // Subrotina.: GetNumApolice
        // Objectivo.: Obter o Código da Apólice
        //=======================================================================
        BEGSR GetNumapolice;
           IF (TMP_MATRICULA <> 'S/MATRICULA');
              WCodSessao = $SetFiles('GBASEDIA');
              $SetFields('APOL,MATR1,DTALT,MALT');
              $SetCriteria('(MATR1 = '''+%TRIM(GRL_MATRICULA)+''')');
              IF ($GetRecord() > *ZEROS);
                 GRL_NUAPOL = $GetFieldNum('APOL');
                 WDatAltera = $IntToDate($CvtData($GetFieldNum('DTALT'):
                                         'DMY':'YYMD'));
              ELSE;
                 $RmvDBSession(WCodSessao);
                 WCodSessao = $SetFiles('GBASEGRA');
                 $SetFields('APOL,MATR1,DTALT,MALT');
                 $SetCriteria('(MATR1 = '''+%TRIM(GRL_MATRICULA)+''')');
                 IF ($GetRecord() > *ZEROS);
                    GRL_NUAPOL = $GetFieldNum('APOL');
                    WDatAltera = $IntToDate($CvtData($GetFieldNum('DTALT'):
                                         'DMY':'YYMD'));
                 ENDIF;
              ENDIF;
              $RmvDBSession(WCodSessao);
           ENDIF;
        ENDSR;

        //=======================================================================
        // Subrotina.: GetMediador;
        // Objectivo.: Obter o Código de Mediador
        //=======================================================================
        BEGSR GetMediador;
           IF (TMP_CDLOGIN = 'Adalberto Falcao (Tecn. de Informação');
              GRL_CDMEDIA = 70000;
           ELSEIF (TMP_CDLOGIN = 'JOSÉ MATA - CONSULTORES DE SEGUROS');
              GRL_CDMEDIA = 60000;
           ELSEIF (TMP_CDLOGIN = 'Med_40001 Null (J. Mata)');
              GRL_CDMEDIA = 60924;
           ELSEIF (TMP_CDLOGIN = 'Med_50003 Null (J. Mata)');
              GRL_CDMEDIA = 60001;
           ELSEIF (TMP_CDLOGIN = 'Med_60178 Null (J. Mata)');
              GRL_CDMEDIA = 60178;
           ELSEIF (TMP_CDLOGIN = 'Med_60186 Null (J. Mata)');
              GRL_CDMEDIA = 60186;
           ELSEIF (TMP_CDLOGIN = 'Med_60227 Null (J. Mata)');
              GRL_CDMEDIA = 60227;
           ELSEIF (TMP_CDLOGIN = 'Med_60272 Null (J. Mata)');
              GRL_CDMEDIA = 60272;
           ELSEIF (TMP_CDLOGIN = 'Med_60339 Null (J. Mata)');
              GRL_CDMEDIA = 60000;
           ELSEIF (TMP_CDLOGIN = 'Med_60363 Null (J. Mata)');
              GRL_CDMEDIA = 60363;
           ELSEIF (TMP_CDLOGIN = 'Med_60366 Null (J. Mata)');
              GRL_CDMEDIA = 60366;
           ELSEIF (TMP_CDLOGIN = 'Med_60415 Null (J. Mata)');
              GRL_CDMEDIA = 60415;
           ELSEIF (TMP_CDLOGIN = 'Med_60826 Null (J. Mata)');
              GRL_CDMEDIA = 60826;
           ELSEIF (TMP_CDLOGIN = 'Med_60827 Null (J. Mata)');
              GRL_CDMEDIA = 60827;
           ELSEIF (TMP_CDLOGIN = 'Med_60838 Null (J. Mata)');
              GRL_CDMEDIA = 60838;
           ELSEIF (TMP_CDLOGIN = 'Med_60900 Null (J. Mata)');
              GRL_CDMEDIA = 60900;
           ELSEIF (TMP_CDLOGIN = 'Med_60918 Null (J. Mata)');
              GRL_CDMEDIA = 60918;
           ELSEIF (TMP_CDLOGIN = 'Med_60919 Null (J. Mata)');
              GRL_CDMEDIA = 60919;
           ELSE;
              WMsgErro = 'O Mediador '+%TRIM(TMP_CDLOGIN)+', não está definido';
              EXSR PrintLine;
              EVAL WIcErro = *ON;
           ENDIF;
        ENDSR;
      /END-FREE
     C*============================================================
     OAUDCPROV  E            PRT_HEADER     1  1
     O                                            9 'PGRLO004'
     O                                           84 'Auditoria aos Certificados'
     O                                          103 'Pag.:'
     O                       PAGE          Z    114
     O          E            PRT_HEADER     2
     O                                          103 'Data:'
     O                       WDatHoje           114
     O          E            PRT_HEADER     1
     O                                           15 '--------------'
     O                                           27 '-----------'
     O                                           36 '--------'
     O                                          117 '---------------------'
     O          E            PRT_HEADER     1
     O                                           15 'Nº.Certificado'
     O                                           27 'Data Inicio'
     O                                           36 'Mediador'
     O                                          117 'Mensagem'
     O          E            PRT_HEADER     1
     O                                           15 '--------------'
     O                                           27 '-----------'
     O                                           36 '--------'
     O                                          117 '---------------------'
     OAUDCPROV  E            PRT_DETAIL
     O                       GRL_CDCPROV         15
     O                       GRL_DTINICIO        27
     O                       GRL_CDMEDIA         36
     O                       WMsgErro           117
