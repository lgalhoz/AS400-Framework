     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*===========================================================
     H* Programa..: PSINO010
     H* Descri��o.: Informa��o de sinistralidade da Ap�lice
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
     D R#PRVAU       E DS                  EXTNAME('G.SINPRV') QUALIFIED
     D K#PRVAU         DS                  QUALIFIED
     D   KEY                   1      8A
     D   Ano                   1      2S 0
     D   Sequ                  3      6S 0
     D   Linha                 7      8S 0
     D
     D R#PAGAU       E DS                  EXTNAME('G.SINPAG') QUALIFIED
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
     D   ValPremio                   13S 2
     D   ValEstorno                  13S 2
     D   ValComiss                   13S 2
     D   Escreveu                      N
     D   IsEmpty                       N
     D WIdxRec         S              3S 0
     D
     D LST#Sinistro    DS                  QUALIFIED DIM(500)
     D   Ramo                         2S 0
     D   Apolice                      7S 0
     D   Ano                          4S 0
     D   Processo                     4S 0
     D   DataPart                     8S 0
     D   DataSinistro                 8S 0
     D   DataEncerra                  8S 0
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
     D WPrmRec         S             13S 2
     D WEstRec         S             13S 2
     D
     D WSumProvis      S             13S 2
     D WSumPagam       S             13S 2
     D WSumRecup       S             13S 2
     D WSumValRec      S             13S 2
     D WSumPrmRec      S             13S 2
     D WSumEstRec      S             13S 2
     D
     D WRacio          S              8S 5
     D WStsRec         S              1A
     D WIcExiste       S               N
     D WIsHeader       S               N
     D WSelTodos       S               N
     D*===========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCodRamo
     C                   PARM                    WNuApolice
     C                   PARM                    WDatInicio
     C                   PARM                    WDatTermo
     C                   PARM                    WCodErro
      /FREE
       //------------------------------------------------------------------------
       // Validar os Par�metros Recebidos
       //------------------------------------------------------------------------
       EXSR ValParametros;
       IF (WCodErro <> *BLANKS);
          RETURN;
       ENDIF;

       //------------------------------------------------------------------------
       // Confirmar se pretende o Relat�rio para todas as Ap�lices do Segurado
       // Desenvolver esta funcionalidade quando tiver mais tempo
       //------------------------------------------------------------------------
       // IF ($ShowDialog('SIN0003':BAS_SEG:'NS') = 'S');
       //    WSelTodos = *ON;
       // ENDIF;

       //------------------------------------------------------------------------
       // Obter Processos para a Ap�lice
       //------------------------------------------------------------------------
       WQtdeRegis  = *ZEROS;
       WSumProvis  = *ZEROS;
       WSumPagam   = *ZEROS;
       WSumRecup   = *ZEROS;
       WSumValRec  = *ZEROS;
       WSumPrmRec  = *ZEROS;
       WSumEstRec  = *ZEROS;
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
       // Subrotina..: ValParametros
       // Descri��o..: Validar os Par�metros Recebidos
       //=================================================================================
       BEGSR ValParametros;
          WCodErro  = *BLANKS;
          WSelTodos = *OFF;

          //------------------------------------------------------------------------
          // Validar a Exist�ncia da Ap�lice
          //------------------------------------------------------------------------
          CHAIN (WCodRamo:WNuApolice) GBASEGRL;
          IF (NOT %FOUND(GBASEGRL));
             WCodErro = 'PRDXXXX';
             LEAVESR;
          ENDIF;

          //------------------------------------------------------------------------
          // Validar o Per�odo
          //------------------------------------------------------------------------
          IF (WDatInicio > WDatTermo);
             WCodErro = 'PRDXXXX';
             LEAVESR;
          ENDIF;
       ENDSR;

       //=================================================================================
       // Subrotina..: GetProcAUT
       //  Descri��o.: Obter a Informa��o sobre os Processo Autom�vel
       //=================================================================================
       BEGSR GetProcAUT;
          OPEN PROSINAU;
          OPEN PAGSINAU;
          OPEN PRVSINAU;
          Widx = 1;
          LST#Sinistro(WIdx).IsEmpty = *ON;
          SETLL (WCodRamo:WNuApolice) PROSINAU;
          READE (WCodRamo:WNuApolice) PROSINAU;
          DOW (NOT %EOF(PROSINAU));
             IF (APRC_IPROC = 'P');
                WQtdeRegis += 1;
                EXSR GetProvAU;
                EXSR GetPagaAU;
                LST#Sinistro(WIdx).Ramo         = WCodRamo;
                LST#Sinistro(WIdx).Apolice      = WNuApolice;
                LST#Sinistro(WIdx).Ano          = APRC_AAPROC + 2000;
                LST#Sinistro(WIdx).Processo     = APRC_NSEQ;
                LST#Sinistro(WIdx).DataPart     = $CvtData(APRC_DTPART:'YMD'
                                                                     :'YYMD');
                LST#Sinistro(WIdx).DataEncerra  = $CvtData(APRC_DTFIM:'YMD'
                                                                     :'YYMD');
                LST#Sinistro(WIdx).DataSinistro = $CvtData(APRC_DTSIN:'YMD'
                                                                     :'YYMD');
                LST#Sinistro(WIdx).TotProvis    = WTotProvis;
                LST#Sinistro(WIdx).TotPagam     = WTotPagam;
                LST#Sinistro(WIdx).TotRecupera  = WTotPagam - WTotProvis;
                LST#Sinistro(WIdx).IsEmpty      = *OFF;
                WIdx += 1;
                LST#Sinistro(WIdx).IsEmpty      = *ON;
                WSumProvis += WTotProvis;
                WSumPagam  += WTotPagam;
             ENDIF;
             READE (WCodRamo:WNuApolice) PROSINAU;
          ENDDO;
          CLOSE PROSINAU;
          CLOSE PRVSINAU;
          CLOSE PROSINAU;
       ENDSR;

       //=================================================================================
       // Subrotina..: GetProcOR
       //  Descri��o.: Obter a Informa��o sobre os Processo Outros Ramos
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
                LST#Sinistro(WIdx).DataPart     = $CvtData(OPRC_DTPART:'YMD'
                                                                     :'YYMD');
                LST#Sinistro(WIdx).DataEncerra  = $CvtData(OPRC_DTFIM:'YMD'
                                                                     :'YYMD');
                LST#Sinistro(WIdx).DataSinistro = $CvtData(OPRC_DTSIN:'YMD'
                                                                     :'YYMD');
                LST#Sinistro(WIdx).TotProvis    = WTotProvis;
                LST#Sinistro(WIdx).TotPagam     = WTotPagam;
                LST#Sinistro(WIdx).TotRecupera  = WTotPagam - WTotProvis;
                LST#Sinistro(WIdx).IsEmpty      = *OFF;
                WIdx += 1;
                LST#Sinistro(WIdx).IsEmpty      = *ON;
                WSumProvis += WTotProvis;
                WSumPagam  += WTotPagam;
             ENDIF;
             READE (WCodRamo:WNuApolice) PROSINOR;
          ENDDO;
          CLOSE PROSINOR;
          CLOSE PRVSINOR;
          CLOSE PAGSINOR;
       ENDSR;

       //=================================================================================
       // Subrotina..: GetProvAU
       //  Descri��o.: Obter as provis�es para o Processo
       //=================================================================================
       BEGSR GetProvAU;
          WTotProvis = *ZEROS;
          K#PRVAU.Ano   = APRC_AAPROC;
          K#PRVAU.Sequ  = APRC_NSEQ;
          K#PRVAU.Linha = *ZEROS;
          SETLL K#PRVAU.Key PRVSINAU;
          READ PRVSINAU R#PRVAU;
          DOW (NOT %EOF(PRVSINAU));
             IF (R#PRVAU.NSEQ = APRC_NSEQ) AND (R#PRVAU.AAPROC = APRC_AAPROC);
                WTotProvis += R#PRVAU.VALOR;
             ELSE;
                LEAVE;
             ENDIF;
             READ PRVSINAU R#PRVAU;
          ENDDO;
       ENDSR;

       //=================================================================================
       // Subrotina..: GetPagaAU
       //  Descri��o.: Obter os pagamebntos para o Processo
       //=================================================================================
       BEGSR GetPagaAU;
          WTotPagam = *ZEROS;
          K#PAGAU.Ano   = APRC_AAPROC;
          K#PAGAU.Sequ  = APRC_NSEQ;
          K#PAGAU.Linha = *ZEROS;
          SETLL K#PAGAU.Key PAGSINAU;
          READ PAGSINAU R#PAGAU;
          DOW (NOT %EOF(PAGSINAU));
             IF (R#PAGAU.NSEQ = APRC_NSEQ) AND (R#PAGAU.AAPROC = APRC_AAPROC);
                WTotPagam += R#PAGAU.VALOR;
             ELSE;
                LEAVE;
             ENDIF;
             READ PAGSINAU R#PAGAU;
          ENDDO;
       ENDSR;

       //=================================================================================
       // Subrotina..: GetProvOR
       //  Descri��o.: Obter as provis�es para o Processo
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
       //  Descri��o.: Obter os pagamebntos para o Processo
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
       // Subrotina..: GetRecibos
       // Descri��o..: Obter Todos os recibos pagos
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
                   WEstRec = LS_GCTREC;
                   WValRec = LS_GCTREC * -1;
                   WPrmRec = *ZEROS;
                ELSE;
                   WPrmRec = LS_GCTREC;
                   WValRec = LS_GCTREC;
                   WEstRec = *ZEROS;
                ENDIF;
                WNumRecAlfa = %TRIM(%EDITW(LS_GCNREC:'0         '));
                WAnoRec = 2000+%DEC(%SUBST(%CHAR(LS_GCDEMI+1000000):2:2):2:0);

                //-------------------------------------
                // Obter a Data de PAgamento
                //-------------------------------------
                CHAIN (WNumRecAlfa) SD1RE1;
                IF (%FOUND(SD1RE1));
                   IF (JM_CODSIT = 2);
                      WStsRec = 'P';
                    ENDIF;
                ENDIF;

                //-------------------------------------
                // Actualizar acumulados por ano
                //-------------------------------------
                IF (WStsRec = 'P');
                   WSumValRec += WValRec;
                   WSumPrmRec += WPrmRec;
                   WSumEstRec += WEstRec;
                   WIcExiste = *OFF;
                   WIdx = 1;
                   DOW (NOT LST#Recibo(WIdx).IsEmpty);
                      IF (LST#Recibo(WIdx).Ano = WAnoRec);
                         WIcExiste = *ON;
                         LST#Recibo(WIdx).ValRecibo += WValRec;
                         LST#Recibo(WIdx).ValEstorno += WEstRec;
                         LST#Recibo(WIdx).ValPremio += WPrmRec;
                         LEAVE;
                      ENDIF;
                      WIdx += 1;
                   ENDDO;

                   IF (NOT WIcExiste);
                      LST#Recibo(WIdx).Ramo      = WCodRamo;
                      LST#Recibo(WIdx).Apolice   = WNuApolice;
                      LST#Recibo(WIdx).Ano       = WAnoRec;
                      LST#Recibo(WIdx).ValRecibo = WValRec;
                      LST#Recibo(WIdx).ValEstorno= WEstRec;
                      LST#Recibo(WIdx).ValPremio = WPrmRec;
                      LST#Recibo(WIdx).Escreveu  = *OFF;
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
       // Subrotina..: SaveResultado
       // Descri��o..: Guardar o resultado num temporario
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
          WPrmRec    = *ZEROS;
          WEstRec    = *ZEROS;
          WIsHeader  = *ON;
          WAnoRec    = *ZEROS;
          WIdx       = 1;
          DOW (NOT LST#Sinistro(WIdx).IsEmpty);
             //-------------------------------------------------------
             // Escrever a Identifica��o das Colunas
             //-------------------------------------------------------
             IF (WIsHEader);
                WIsHeader = *OFF;
                R#TEMP.Texto = 'Ramo;Ap�lice;Ano Processo;N�.Processo;+
                                Data Partic.;Data Sinistro;+
                                Data Encerra;Provis�es;Pagamentos;+
                                Recupera��es;Premios pagos; Estornos Pagos;+
                                Total Liquido;Racio Sinistralidade';
                WRITE TMPFILE R#TEMP;
                WRacio = *ZEROS;
                IF (WSumValRec > *ZEROS);
                   WRacio = (WSumPagam / WSumValRec);
                ENDIF;
                WSumRecup = WSumProvis - WSumPagam;
                R#TEMP.Texto = ';;;;;;'+
                     'RESUMO GERAL;'+
                      %EDITW(WSumProvis:'0           ,  ')+';'+
                      %EDITW(WSumPagam:'0           ,  ')+';'+
                      %EDITW(WSumRecup:'0           ,  ')+';'+
                      %EDITW(WSumPrmRec:'0           ,  ')+';'+
                      %EDITW(WSumEstRec:'0           ,  ')+';'+
                      %EDITW(WSumValRec:'0           ,  ')+';'+
                      %EDITW(WRacio:'0   ,     ');
                WRITE TMPFILE R#TEMP;
             ENDIF;

             //-------------------------------------------------------
             // Escrever o resumo do ano
             //-------------------------------------------------------
             IF (WAnoRec <> LST#Sinistro(WIdx).Ano);
                IF (WAnoRec <> *ZEROS);
                   EXSR EscrevePremios;
                   WRacio = *ZEROS;
                   IF (WValRec > *ZEROS);
                      WRacio = (WTotPagam / WValRec);
                   ENDIF;
                   R#TEMP.Texto = ';;;;;;'+
                                  'RESUMO DO ANO '+%CHAR(WAnoRec) +';'+
                                  %EDITW(WTotProvis:'0           ,  ')+';'+
                                  %EDITW(WTotPagam:'0           ,  ')+';'+
                                  %EDITW(WTotRecup:'0           ,  ')+';'+
                                  %EDITW(WPrmRec:'0           ,  ')+';'+
                                  %EDITW(WEstRec:'0           ,  ')+';'+
                                  %EDITW(WValRec:'0           ,  ')+';'+
                                  %EDITW(WRacio:'0   ,     ');
                   WRITE TMPFILE R#TEMP;
                ELSE;
                   WAnoRec = LST#Sinistro(WIdx).Ano;
                   EXSR EscrevePremios;
                ENDIF;

                WTotProvis = *ZEROS;
                WTotPagam  = *ZEROS;
                WTotRecup  = *ZEROS;
                WAnoRec = LST#Sinistro(WIdx).Ano;
             ENDIF;

             //-------------------------------------------------------
             // Escrever o detalhe dos processos
             //-------------------------------------------------------
             WTotProvis += LST#Sinistro(WIdx).TotProvis;
             WTotPagam  += LST#Sinistro(WIdx).TotPagam;
             WTotRecup  += LST#Sinistro(WIdx).TotRecupera;
             R#TEMP.Texto = %CHAR(LST#Sinistro(WIdx).Ramo)+';'+
                            %CHAR(LST#Sinistro(WIdx).Apolice)+';'+
                            %CHAR(LST#Sinistro(WIdx).Ano)+';'+
                            %CHAR(LST#Sinistro(WIdx).Processo)+';'+
                            %CHAR(LST#Sinistro(WIdx).DataPart) + ';' +
                            %CHAR(LST#Sinistro(WIdx).DataSinistro) + ';' +
                            %CHAR(LST#Sinistro(WIdx).DataEncerra) + ';' +
                    %EDITW(LST#Sinistro(WIdx).TotProvis:'0           ,  ')+';'+
                    %EDITW(LST#Sinistro(wIdx).TotPagam:'0           ,  ')+';'+
                    %EDITW(LST#Sinistro(WIdx).TotRecupera:'0           ,  ');
             WRITE TMPFILE R#TEMP;
             WIdx += 1;
          ENDDO;

          IF (WAnoRec <> *ZEROS);
             EXSR EscrevePremios;
             WRacio = *ZEROS;
             IF (WValRec > *ZEROS);
                WRacio = (WTotPagam / WValRec);
             ENDIF;
             R#TEMP.Texto = ';;;;;;'+
                            'RESUMO DO ANO '+%CHAR(WAnoRec) +';'+
                            %EDITW(WTotProvis:'0           ,  ')+';'+
                            %EDITW(WTotPagam:'0           ,  ')+';'+
                            %EDITW(WTotRecup:'0           ,  ')+';'+
                            %EDITW(WPrmRec:'0           ,  ')+';'+
                            %EDITW(WEstRec:'0           ,  ')+';'+
                            %EDITW(WValRec:'0           ,  ')+';'+
                            %EDITW(WRacio:'0   ,     ');
             WRITE TMPFILE R#TEMP;
          ENDIF;
          WAnoRec = 9999;
          EXSR EscrevePremios;
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

       //=================================================================================
       // Subrotina..: EscrevePremios
       // Descri��o..: Escrever os pr�mios pagos no Per�odo
       //=================================================================================
       BEGSR EscrevePremios;
          WIdxRec = 1;
          WValRec = *ZEROS;
          WPrmRec = *ZEROS;
          WEstRec = *ZEROS;
          DOW (NOT LST#Recibo(WIdxRec).IsEmpty);
             IF (LST#Recibo(WIdxRec).Ano = WAnoRec);
                WPrmRec = LST#Recibo(WIdxRec).ValPremio;
                WEstRec = LST#Recibo(WIdxRec).ValEstorno;
                WValRec = LST#Recibo(WIdxRec).ValRecibo;
                LST#Recibo(WIdxRec).Escreveu  = *ON;
                LEAVE;
             ELSEIF (LST#Recibo(WIdxRec).Ano < WAnoRec) AND
                (NOT LST#Recibo(WIdxRec).Escreveu);
                LST#Recibo(WIdx).Escreveu  = *ON;
                 R#TEMP.Texto = ';;;;;;RESUMO DO ANO '+
                   %CHAR(LST#Recibo(WIdxRec).Ano) +';;;;'+
                   %EDITW(LST#Recibo(WIdxRec).ValPremio:'0           ,  ')+';'+
                   %EDITW(LST#Recibo(WIdxRec).ValEstorno:'0           ,  ')+';'+
                   %EDITW(LST#Recibo(WIdxRec).ValRecibo:'0           ,  ');
                   WRITE TMPFILE R#TEMP;
                 LST#Recibo(WIdxRec).Escreveu  = *ON;
             ENDIF;
             WIdxRec += 1;
          ENDDO;
       ENDSR;
      /END-FREE
