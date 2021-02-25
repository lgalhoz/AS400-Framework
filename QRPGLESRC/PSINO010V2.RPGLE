     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*===========================================================
     H* Programa..: PSINO010
     H* Descrição.: Informação de sinistralidade da Apólice
     H*
     H* CALL PGM(PSINO010) PARM('02' '9901847' '00000000' '00000000' ' ')
     H*===========================================================
     FSD1RE1    IF   E           K DISK    PREFIX(JM_) USROPN
     FGCOBGRH4  IF   E           K DISK    PREFIX(LS_) USROPN
     F
     FFGRLP200  IF   E           K DISK    PREFIX(GRL_) USROPN
     F
     FPROSINOR  IF   E           K DISK    PREFIX(OPRC_) USROPN
     FPRVSINOR  IF   F  128    10AIDISK    EXTFILE('O.SINPRV') USROPN
     FPAGSINOR  IF   F  300    10AIDISK    EXTFILE('O.SINPAG') USROPN
     F
     FPROSINAU  IF   E           K DISK    PREFIX(APRC_) USROPN
     FPRVSINAU  IF   F  128     8AIDISK    EXTFILE('G.SINPRV') USROPN
     FPAGSINAU  IF   F  300     8AIDISK    EXTFILE('G.SINPAG') USROPN
     F
     FGBASEGRL  IF   E           K DISK    PREFIX(BAS_)
     F
     FTMPFILE   UF A F  400        DISK    USROPN EXTFILE(WFileName)
     F*===========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SSECO010

     D*==> Formatos de Registo
     D R#PRVOR       E DS                  EXTNAME('O.SINPRV') QUALIFIED
     D K#PRVOR         DS                  QUALIFIED
     D   KEY                   1     10A
     D   Ramo                  1      2S 0
     D   Ano                   3      4S 0
     D   Sequ                  5      8S 0
     D   Linha                 9     10S 0
     D
     D R#PAGOR       E DS                  EXTNAME('O.SINPAG') QUALIFIED
     D K#PAGOR         DS                  QUALIFIED
     D   KEY                   1     10A
     D   Ramo                  1      2S 0
     D   Ano                   3      4S 0
     D   Sequ                  5      8S 0
     D   Linha                 9     10S 0
     D
     D R#PRVAU       E DS                  EXTNAME('G.SINPRV') PREFIX('APRV_')
     D K#PRVAU         DS                  QUALIFIED
     D   KEY                   1      8A
     D   Ano                   1      2S 0
     D   Sequ                  3      6S 0
     D   Linha                 7      8S 0
     D
     D R#PAGAU       E DS                  EXTNAME('G.SINPAG') PREFIX('APAG_')
     D K#PAGAU         DS                  QUALIFIED
     D   KEY                   1      8A
     D   Ano                   1      2S 0
     D   Sequ                  3      6S 0
     D   Linha                 7      8S 0
     D
     D*==> Registo a inserir no ficheiro
     D R#TEMP          DS                  QUALIFIED
     D   Texto                 1    400
     D
     D*==> lista de Trabalo com o Resultado
     D LST#Recibo      DS                  QUALIFIED DIM(500)
     D   Ramo                         2S 0
     D   Apolice                      7S 0
     D   Ano                          4S 0
     D   ValRecibo                   13S 2
     D   ValComiss                   13S 2
     D   IsEmpty                       N
     D WIdxRec         S              3S 0
     D
     D LST#Sinistro    DS                  QUALIFIED DIM(500)
     D   Ramo                         2S 0
     D   Apolice                      7S 0
     D   Ano                          4S 0
     D   Processo                     4S 0
     D   DataSinistro                 8S 0
     D   TotProvis                   13S 2
     D   TotPagam                    13S 2
     D   TotRecupera                 13S 2
     D   IsEmpty                       N
     D Widx            S              3S 0
     D
     D*==>  Campos de Trabalho
     D WCodRamo        S              2S 0
     D WNuApolice      S              7S 0
     D WDatInicio      S              8S 0
     D WDatTermo       S              8S 0
     D WCodErro        S              7A
     D
     D WTotProvis      S             13S 2
     D WTotPagam       S             13S 2
     D WTotRecup       S             13S 2
     D WFileName       S             10A
     D WPCName         S            100A
     D WCodTransf      S              5S 0
     D WQtdeRegis      S              5S 0
     D WNumRecAlfa     S              9A
     D WAnoRec         S              4S 0
     D WValRec         S             13S 2
     D WComRec         S             13S 2
     D WStsRec         S              1A
     D WIcExiste       S               N
     D WIsHeader       S               N
     D*===========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCodRamo
     C                   PARM                    WNuApolice
     C                   PARM                    WDatInicio
     C                   PARM                    WDatTermo
     C                   PARM                    WCodErro
      /FREE
       //------------------------------------------------------------------------
       // Validar os Parâmetros Recebidos
       //------------------------------------------------------------------------
       EXSR ValParametros;
       IF (WCodErro <> *BLANKS);
          RETURN;
       ENDIF;

       //------------------------------------------------------------------------
       // Obter Processos para a Apólice
       //------------------------------------------------------------------------
       WQtdeRegis  = *ZEROS;
       IF (WCodRamo = 50);
          EXSR GetProcAUT;
       ELSE;
          EXSR GetProcOR;
       ENDIF;
       IF (WQtdeRegis > *ZEROS);
          EXSR GetRecibos;
          EXSR SaveResultado;
       ELSE;
          $ShowDialog('SIN0002');
       ENDIF;

       RETURN;

       //=================================================================================
       // Subrotina..: GetRecibos
       // Descrição..: Obter Todos os recibos pagos
       //=================================================================================
       BEGSR GetRecibos;
          OPEN GCOBGRH4;
          OPEN SD1RE1;
          WIdx = 1;
          LST#Recibo(Widx).IsEmpty = *ON;
          SETLL (WCodRamo:WNuApolice) GCOBGRH4;
          READE (WCodRamo:WNuApolice) GCOBGRH4;
          DOW (NOT %EOF(GCOBGRH4));
             WStsRec = *BLANKS;
             IF ((LS_GCSITR=1) OR (LS_GCSITR=0));
                IF (LS_GCTIPR = 5);
                   WValRec = LS_GCTREC * -1;
                   WComRec = LS_GCCMED * -1;
                ELSE;
                   WValRec = LS_GCTREC;
                   WComRec = LS_GCCMED;
                ENDIF;
                WNumRecAlfa = %TRIM(%EDITW(LS_GCNREC:'0         '));

                //-------------------------------------
                // Obter a Data de PAgamento
                //-------------------------------------
                CHAIN (WNumRecAlfa) SD1RE1;
                IF (%FOUND(SD1RE1));
                   IF (JM_CODSIT = 2);
                      IF (JM_DTCOB <> *BLANKS) AND (JM_DTCOB <> '000000');
                         WStsRec = 'P';
                         WAnoRec = 2000+%DEC(%SUBST(JM_DTCOB:5:2):2:0);
                      ENDIF;
                    ENDIF;
                ENDIF;

                //-------------------------------------
                // Actualizar acumulados por ano
                //-------------------------------------
                IF (WStsRec = 'P');
                   WIcExiste = *OFF;
                   WIdx = 1;
                   DOW (NOT LST#Recibo(WIdx).IsEmpty);
                      IF (LST#Recibo(WIdx).Ano = WAnoRec);
                         WIcExiste = *ON;
                         LST#Recibo(WIdx).ValRecibo += WValRec;
                         LST#Recibo(WIdx).Valcomiss += WComRec;
                         LEAVE;
                      ENDIF;
                      WIdx += 1;
                   ENDDO;

                   IF (NOT WIcExiste);
                      LST#Recibo(WIdx).Ramo      = WCodRamo;
                      LST#Recibo(WIdx).Apolice   = WNuApolice;
                      LST#Recibo(WIdx).Ano       = WAnoRec;
                      LST#Recibo(WIdx).ValRecibo = WValRec;
                      LST#Recibo(WIdx).ValComiss = WComRec;
                      LST#Recibo(WIdx).IsEmpty   = *OFF;
                      WIdx += 1;
                      LST#Recibo(WIdx).IsEmpty   = *ON;
                   ENDIF;
                ENDIF;
             ENDIF;
             READE (WCodRamo:WNuApolice) GCOBGRH4;
          ENDDO;
          CLOSE GCOBGRH4;
          CLOSE SD1RE1;
       ENDSR;

       //=================================================================================
       // Subrotina..: ValParametros
       // Descrição..: Validar os Parâmetros Recebidos
       //=================================================================================
       BEGSR ValParametros;
          WCodErro = *BLANKS;

          //------------------------------------------------------------------------
          // Validar a Existência da Apólice
          //------------------------------------------------------------------------
          CHAIN (WCodRamo:WNuApolice) GBASEGRL;
          IF (NOT %FOUND(GBASEGRL));
             WCodErro = 'PRDXXXX';
             LEAVESR;
          ENDIF;

          //------------------------------------------------------------------------
          // Validar o Período
          //------------------------------------------------------------------------
          IF (WDatInicio > WDatTermo);
             WCodErro = 'PRDXXXX';
             LEAVESR;
          ENDIF;
       ENDSR;

       //=================================================================================
       // Subrotina..: GetProcAUT
       //  Descrição.: Obter a Informação sobre os Processo Automóvel
       //=================================================================================
       BEGSR GetProcAUT;
          OPEN PROSINAU;
          OPEN PAGSINAU;
          OPEN PRVSINAU;
          CLOSE PROSINAU;
          CLOSE PRVSINAU;
          CLOSE PROSINAU;
       ENDSR;

       //=================================================================================
       // Subrotina..: GetProcOR
       //  Descrição.: Obter a Informação sobre os Processo Outros Ramos
       //=================================================================================
       BEGSR GetProcOR;
          OPEN PROSINOR;
          OPEN PAGSINOR;
          OPEN PRVSINOR;

          Widx = 1;
          LST#Sinistro(WIdx).IsEmpty = *ON;
          SETLL (WCodRamo:WNuApolice) PROSINOR;
          READE (WCodRamo:WNuApolice) PROSINOR;
          DOW (NOT %EOF(PROSINOR));
             IF (OPRC_IPROC = 'P');
                WQtdeRegis += 1;
                EXSR GetProvOR;
                EXSR GetPagaOR;
                LST#Sinistro(WIdx).Ramo         = WCodRamo;
                LST#Sinistro(WIdx).Apolice      = WNuApolice;
                LST#Sinistro(WIdx).Ano          = OPRC_AAPROC + 2000;
                LST#Sinistro(WIdx).Processo     = OPRC_NSEQ;
                LST#Sinistro(WIdx).DataSinistro = $CvtData(OPRC_DTSIN:'YMD'
                                                                     :'YYMD');
                LST#Sinistro(WIdx).TotProvis    = WTotProvis;
                LST#Sinistro(WIdx).TotPagam     = WTotPagam;
                LST#Sinistro(WIdx).TotRecupera  = WTotPagam - WTotProvis;
                LST#Sinistro(WIdx).IsEmpty      = *OFF;
                WIdx += 1;
                LST#Sinistro(WIdx).IsEmpty      = *ON;
             ENDIF;
             READE (WCodRamo:WNuApolice) PROSINOR;
          ENDDO;
          CLOSE PROSINOR;
          CLOSE PRVSINOR;
          CLOSE PAGSINOR;
       ENDSR;

       //=================================================================================
       // Subrotina..: GetProvOR
       //  Descrição.: Obter as provisões para o Processo
       //=================================================================================
       BEGSR GetProvOR;
          WTotProvis = *ZEROS;
          K#PRVOR.Ramo  = WCodRamo;
          K#PRVOR.Ano   = OPRC_AAPROC;
          K#PRVOR.Sequ  = OPRC_NSEQ;
          K#PRVOR.Linha = *ZEROS;
          SETLL K#PRVOR.Key PRVSINOR;
          READ PRVSINOR R#PRVOR;
          DOW (NOT %EOF(PRVSINOR));
             IF (R#PRVOR.RM = WCodRamo) AND (R#PRVOR.NSEQ = OPRC_NSEQ)
               AND (R#PRVOR.AAPROC = OPRC_AAPROC);
                WTotProvis += R#PRVOR.VALOR;
             ELSE;
                LEAVE;
             ENDIF;
             READ PRVSINOR R#PRVOR;
          ENDDO;
       ENDSR;

       //=================================================================================
       // Subrotina..: GetPagaOR
       //  Descrição.: Obter os pagamebntos para o Processo
       //=================================================================================
       BEGSR GetPagaOR;
          WTotPagam = *ZEROS;
          K#PAGOR.Ramo  = WCodRamo;
          K#PAGOR.Ano   = OPRC_AAPROC;
          K#PAGOR.Sequ  = OPRC_NSEQ;
          K#PAGOR.Linha = *ZEROS;
          SETLL K#PAGOR.Key PAGSINOR;
          READ PAGSINOR R#PAGOR;
          DOW (NOT %EOF(PAGSINOR));
             IF (R#PAGOR.RM = WCodRamo) AND (R#PAGOR.NSEQ = OPRC_NSEQ)
               AND (R#PAGOR.AAPROC = OPRC_AAPROC);
                WTotPagam += R#PAGOR.VALOR;
             ELSE;
                LEAVE;
             ENDIF;
             READ PAGSINOR R#PAGOR;
          ENDDO;
       ENDSR;

       //=================================================================================
       // Subrotina..: SaveResultado
       // Descrição..: Guardar o resultado num temporario
       //=================================================================================
       BEGSR SaveResultado;
          WCodTransf = 18;
          WFileName = 'TMPFILE';
          WPcName = 'SIN'+%TRIM(%EDITW(WCodRamo:'0  '))+
                    %CHAR(WNuApolice)+'.csv';
          WCodErro  = $CrtFile(WFileName:400);
          IF (WCodErro <> *BLANKS);
             LEAVESR;
          ENDIF;

          //---------------------------------------------------
          // Carregar listagem com os registos a enviar
          //---------------------------------------------------
          OPEN TMPFILE;
          WTotProvis = *ZEROS;
          WTotPagam  = *ZEROS;
          WTotRecup  = *ZEROS;
          WValRec    = *ZEROS;
          WComRec    = *ZEROS;
          WIsHeader  = *ON;
          WAnoRec    = *ZEROS;
          WIdx       = 1;
          DOW (NOT LST#Sinistro(WIdx).IsEmpty);
             IF (WIsHEader);
                WIsHeader = *OFF;
                R#TEMP.Texto = 'Ramo;Apólice;Ano Processo;Nº.Processo;+
                                Data Sinistro;Provisões;Pagamentos;+
                                Recuperações;Premios pagos; Comissoes Pagas';
                WRITE TMPFILE R#TEMP;
             ENDIF;
             WTotProvis += LST#Sinistro(WIdx).TotProvis;
             WTotPagam  += LST#Sinistro(WIdx).TotPagam;
             WTotRecup  += LST#Sinistro(WIdx).TotRecupera;
             R#TEMP.Texto = %CHAR(LST#Sinistro(WIdx).Ramo)+';'+
                            %CHAR(LST#Sinistro(WIdx).Apolice)+';'+
                            %CHAR(LST#Sinistro(WIdx).Ano)+';'+
                            %CHAR(LST#Sinistro(WIdx).Processo)+';'+
                            %CHAR(LST#Sinistro(WIdx).DataSinistro) + ';' +
                    %EDITW(LST#Sinistro(WIdx).TotProvis:'           ,  ')+';'+
                    %EDITW(LST#Sinistro(wIdx).TotPagam:'           ,  ') + ';'+
                    %EDITW(LST#Sinistro(WIdx).TotRecupera:'           ,  ');
             IF (WAnoRec <> LST#Sinistro(WIdx).Ano);
                WAnoRec = LST#Sinistro(WIdx).Ano;
                WIdxRec = 1;
                DOW (NOT LST#Recibo(WIdxRec).IsEmpty);
                   IF (LST#Recibo(WIdxRec).Ano = WAnoRec);
                     WValRec = LST#Recibo(WIdxRec).ValRecibo;
                     WComRec = LST#Recibo(WIdxRec).Valcomiss;
                     R#TEMP.Texto = %TRIM(R#TEMP.Texto)+';'+
                    %EDITW(LST#Recibo(WIdxRec).ValRecibo:'           ,  ')+';'+
                    %EDITW(LST#Recibo(WIdxRec).Valcomiss:'           ,  ');
                     LEAVE;
                   ENDIF;
                   WIdxRec += 1;
                ENDDO;
             ELSE;
                R#TEMP.Texto = %TRIM(R#TEMP.Texto) + ';;';
             ENDIF;
             WRITE TMPFILE R#TEMP;
             WIdx += 1;
          ENDDO;
          CLOSE TMPFILE;

          //---------------------------------------------------
          // Enviar os ficheros para o servidor e abrir
          //---------------------------------------------------
          IF ($SendFTP(WCodTransf:'QTEMP':WFileName:%TRIM(WPcName)) <> *BLANKS);
             $ShowDialog('PRD0156');
          ELSE;
             MONITOR;
                OPEN FGRLP200;
                Chain (WCodTransf) RGRLP200;
                WPcName = '\\'+%TRIM(GRL_IPSERVER)+'\'+
                           %TRIM($StrReplace('/':'\':%TRIM(GRL_NoPasta)))+
                           %TRIM(WPcName);
                $ShellExec('strpco');
                $ShellExec('strpccmd pccmd('''+
                                    %TRIM(WPcName)+''') pause(*NO)');
             ON-ERROR;
             ENDMON;
             CLOSE FGRLP200;
             $ShowDialog('SIN0001':WPcName);
          ENDIF;
       ENDSR;
      /END-FREE
