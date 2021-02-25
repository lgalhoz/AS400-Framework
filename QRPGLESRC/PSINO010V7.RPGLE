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
     FIDSPRO    IF   E           K DISK    RENAME(GSINPROF:IDSPROF)
     F                                     PREFIX(LPRC_) USROPN
     FIDSPRV    IF   E           K DISK    PREFIX(LPRV_) USROPN
     FIDSPAG    IF   E           K DISK    PREFIX(LPAG_) USROPN
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
     D   Origem                      10A
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
     D WAnoInicio      S              4S 0
     D WAnoSinist      S              4S 0
     D
     D WAnoProces      S              4S 0
     D WNumSeq         S              5S 0
     D WDataPart       S              8S 0
     D WDataEncer      S              8S 0
     D WDataSini       S              8S 0
     D WOrigem         S             10A
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
     D WRacio          S              8S 3
     D WStsRec         S              1A
     D WIcExiste       S               N
     D WIsHeader       S               N
     D WSelTodos       S               N
     D
     D*==> Definição das Constantes
     D OR#CmpSeg       C                   
     D OR#DemoComp     C                   
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
       // Confirmar se pretende o Relatório para todas as Apólices do Segurado
       // Desenvolver esta funcionalidade quando tiver mais tempo
       //------------------------------------------------------------------------
       // IF ($ShowDialog('SIN0003':BAS_SEG:'NS') = 'S');
       //    WSelTodos = *ON;
       // ENDIF;

       //------------------------------------------------------------------------
       // Obter Processos para a Apólice
       //------------------------------------------------------------------------
       WAnoInicio  = %SUBDT(%DATE():*YEARS);
       WQtdeRegis  = *ZEROS;
       WSumProvis  = *ZEROS;
       WSumPagam   = *ZEROS;
       WSumRecup   = *ZEROS;
       WSumValRec  = *ZEROS;
       WSumPrmRec  = *ZEROS;
       WSumEstRec  = *ZEROS;
       WIdx = 1;
       LST#Sinistro(WIdx).IsEmpty = *ON;
       IF (WCodRamo = 50);
          EXSR GetProcLusit;
          EXSR GetProcAUT;
       ELSE;
          IF (WCodRamo = 15);
             EXSR GetProcLusit;
          ENDIF;
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
       // Descrição..: Validar os Parâmetros Recebidos
       //=================================================================================
       BEGSR ValParametros;
          WCodErro  = *BLANKS;
          WSelTodos = *OFF;

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
       // Subrotina..: FillListProc;
       //  Descrição.: Actualizar a lista de Processos de Sinistro.
       //=================================================================================
       BEGSR FillListProc;
          IF (WAnoInicio > WAnoProces);
             WAnoInicio = WAnoProces;
          ENDIF;

          LST#Sinistro(WIdx).Ramo         = WCodRamo;
          LST#Sinistro(WIdx).Apolice      = WNuApolice;
          LST#Sinistro(WIdx).Ano          = WAnoProces;
          LST#Sinistro(WIdx).Origem       = WOrigem;
          LST#Sinistro(WIdx).Processo     = WNumSeq;
          LST#Sinistro(WIdx).DataPart     = $CvtData(WDataPart:'YMD':'YYMD');
          LST#Sinistro(WIdx).DataEncerra  = $CvtData(WDataEncer:'YMD':'YYMD');
          LST#Sinistro(WIdx).DataSinistro = $CvtData(WDataSini:'YMD':'YYMD');
          LST#Sinistro(WIdx).TotProvis    = WTotProvis;
          LST#Sinistro(WIdx).TotPagam     = WTotPagam;
          LST#Sinistro(WIdx).TotRecupera  = WTotRecup;
          LST#Sinistro(WIdx).IsEmpty      = *OFF;
          WIdx += 1;
          LST#Sinistro(WIdx).IsEmpty      = *ON;

          WSumProvis += WTotProvis;
          WSumPagam  += WTotPagam;
          WSumRecup  += WTotRecup;
       ENDSR;

       //=================================================================================
       // Subrotina..: GetProcLusit
       //  Descrição.: Actualizar a lista de Processos da Lusitânia
       //=================================================================================
       BEGSR GetProcLusit;
          OPEN IDSPRO;
          OPEN IDSPAG;
          OPEN IDSPRV;

          SETLL *LOVAL IDSPRO;
          READ IDSPRO;
          DOW (NOT %EOF(IDSPRO));
             IF (LPRC_RM = WCodRamo) AND (LPRC_AP = WNuApolice);
                WQtdeRegis += 1;
                EXSR GetProvLusit;
                EXSR GetPagaLusit;

                WOrigem    = OR#CmpSeg;
                WAnoProces = LPRC_AAPROC + 2000;
                WNumSeq    = LPRC_NSEQ;
                WDataPart  = LPRC_DTPART;
                WDataEncer = LPRC_DTFIM;
                WDataSini  = LPRC_DTSIN;
                EXSR FillListProc;
             ENDIF;
             READ IDSPRO;
          ENDDO;
          CLOSE IDSPRO;
          CLOSE IDSPAG;
          CLOSE IDSPRV;
       ENDSR;

       //=================================================================================
       // Subrotina..: GetProcAUT
       //  Descrição.: Obter a Informação sobre os Processo Automóvel
       //=================================================================================
       BEGSR GetProcAUT;
          OPEN PROSINAU;
          OPEN PAGSINAU;
          OPEN PRVSINAU;

          SETLL (WCodRamo:WNuApolice) PROSINAU;
          READE (WCodRamo:WNuApolice) PROSINAU;
          DOW (NOT %EOF(PROSINAU));
             IF (APRC_IPROC = 'P');
                WQtdeRegis += 1;
                EXSR GetProvAU;
                EXSR GetPagaAU;

                WOrigem    = OR#DemoComp;
                WAnoProces = APRC_AAPROC + 2000;
                WNumSeq    = APRC_NSEQ;
                WDataPart  = APRC_DTPART;
                WDataEncer = APRC_DTFIM;
                WDataSini  = APRC_DTSIN;
                EXSR FillListProc;
             ENDIF;
             READE (WCodRamo:WNuApolice) PROSINAU;
          ENDDO;
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

          SETLL (WCodRamo:WNuApolice) PROSINOR;
          READE (WCodRamo:WNuApolice) PROSINOR;
          DOW (NOT %EOF(PROSINOR));
             IF (OPRC_IPROC = 'P');
                WQtdeRegis += 1;
                EXSR GetProvOR;
                EXSR GetPagaOR;

                WOrigem    = OR#DemoComp;
                WAnoProces = OPRC_AAPROC + 2000;
                WNumSeq    = OPRC_NSEQ;
                WDataPart  = OPRC_DTPART;
                WDataEncer = OPRC_DTFIM;
                WDataSini  = OPRC_DTSIN;
                EXSR FillListProc;
             ENDIF;
             READE (WCodRamo:WNuApolice) PROSINOR;
          ENDDO;
          CLOSE PROSINOR;
          CLOSE PRVSINOR;
          CLOSE PAGSINOR;
       ENDSR;

       //=================================================================================
       // Subrotina..: GetProvLusit
       //  Descrição.: Obter as provisões para o Processo
       //=================================================================================
       BEGSR GetProvLusit;
          WTotProvis = *ZEROS;
          SETLL (LPRC_AAPROC:LPRC_NSEQ) IDSPRV;
          READE (LPRC_AAPROC:LPRC_NSEQ) IDSPRV;
          DOW (NOT %EOF(IDSPRV));
             IF (LPRV_NSEQ = LPRC_NSEQ) AND (LPRV_AAPROC = LPRC_AAPROC);
                WTotProvis += LPRV_VALOR;
             ELSE;
                LEAVE;
             ENDIF;
             READE (LPRC_AAPROC:LPRC_NSEQ) IDSPRV;
          ENDDO;
       ENDSR;

       //=================================================================================
       // Subrotina..: GetPagaLusit
       //  Descrição.: Obter os pagamebntos para o Processo
       //=================================================================================
       BEGSR GetPagaLusit;
          WTotPagam = *ZEROS;
          WTotRecup = *ZEROS;
          SETLL (LPRC_AAPROC:LPRC_NSEQ) IDSPAG;
          READE (LPRC_AAPROC:LPRC_NSEQ) IDSPAG;
          DOW (NOT %EOF(IDSPAG));
             IF (LPAG_NSEQ = LPRC_NSEQ) AND (LPAG_AAPROC = LPRC_AAPROC);
                IF (LPAG_TIPMOV >= 200) AND (LPAG_TIPMOV <= 299);
                   WTotRecup += LPAG_VALOR;
                ELSE;
                   WTotPagam += LPAG_VALOR;
                ENDIF;
             ELSE;
                LEAVE;
             ENDIF;
             READE (LPRC_AAPROC:LPRC_NSEQ) IDSPAG;
          ENDDO;
       ENDSR;

       //=================================================================================
       // Subrotina..: GetProvAU
       //  Descrição.: Obter as provisões para o Processo
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
       //  Descrição.: Obter os pagamebntos para o Processo
       //=================================================================================
       BEGSR GetPagaAU;
          WTotPagam = *ZEROS;
          WTotRecup = *ZEROS;
          K#PAGAU.Ano   = APRC_AAPROC;
          K#PAGAU.Sequ  = APRC_NSEQ;
          K#PAGAU.Linha = *ZEROS;
          SETLL K#PAGAU.Key PAGSINAU;
          READ PAGSINAU R#PAGAU;
          DOW (NOT %EOF(PAGSINAU));
             IF (R#PAGAU.NSEQ = APRC_NSEQ) AND (R#PAGAU.AAPROC = APRC_AAPROC);
                IF (R#PAGAU.TIPMOV >= 200) AND (R#PAGAU.TIPMOV <= 299);
                   WTotRecup += R#PAGAU.VALOR;
                ELSE;
                   WTotPagam += R#PAGAU.VALOR;
                ENDIF;
             ELSE;
                LEAVE;
             ENDIF;
             READ PAGSINAU R#PAGAU;
          ENDDO;
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
          WTotRecup = *ZEROS;
          K#PAGOR.Ramo  = WCodRamo;
          K#PAGOR.Ano   = OPRC_AAPROC;
          K#PAGOR.Sequ  = OPRC_NSEQ;
          K#PAGOR.Linha = *ZEROS;
          SETLL K#PAGOR.Key PAGSINOR;
          READ PAGSINOR R#PAGOR;
          DOW (NOT %EOF(PAGSINOR));
             IF (R#PAGOR.RM = WCodRamo) AND (R#PAGOR.NSEQ = OPRC_NSEQ)
               AND (R#PAGOR.AAPROC = OPRC_AAPROC);
                IF (R#PAGOR.TIPMOV >= 200) AND (R#PAGOR.TIPMOV <= 299);
                   WTotRecup += R#PAGOR.VALOR;
                ELSE;
                   WTotPagam += R#PAGOR.VALOR;
                ENDIF;
             ELSE;
                LEAVE;
             ENDIF;
             READ PAGSINOR R#PAGOR;
          ENDDO;
       ENDSR;

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
                WNumRecAlfa = %TRIM(%EDITW(LS_GCNREC:'0         '));
                WAnoRec = 2000+%DEC(%SUBST(%CHAR(LS_GCDEMI+1000000):2:2):2:0);
                IF (WAnoInicio > WAnoRec);
                   WAnoInicio = WAnoRec;
                ENDIF;

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
                   IF (LS_GCTIPR = 5);
                      WEstRec = JM_PRSIMP;
                      WValRec = JM_PRSIMP * -1;
                      WPrmRec = *ZEROS;
                   ELSE;
                      WPrmRec = JM_PRSIMP;
                      WValRec = JM_PRSIMP;
                      WEstRec = *ZEROS;
                   ENDIF;

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
       // Descrição..: Guardar o resultado num Temporario
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
          WAnoSinist = *ZEROS;
          WIdx       = 1;
          DOW (WAnoInicio < (%SUBDT(%DATE():*YEARS) + 1));
             //-------------------------------------------------------
             // Escrever a Identificação das Colunas
             //-------------------------------------------------------
             IF (WIsHEader);
                WIsHeader = *OFF;
                R#TEMP.Texto = 'Ramo;Apólice;Ano Processo;Nº.Processo;+
                                Origem;Data Partic.;Data Sinistro;+
                                Data Encerra;Provisões;Pagamentos;+
                                Recuperações;Premios pagos; Estornos Pagos;+
                                Total Liquido;Racio Sinistralidade';
                WRITE TMPFILE R#TEMP;
                WRacio = *ZEROS;
                IF (WSumValRec > *ZEROS);
                // WRacio = (WSumPagam / WSumValRec);
                   WRacio = ((WSumProvis-WSumRecup)/WSumValRec)*100;
                ENDIF;

                R#TEMP.Texto = ';;;;;;;'+
                     'RESUMO GERAL;'+
                      %EDITW(WSumProvis:'0           ,  ')+';'+
                      %EDITW(WSumPagam:'0           ,  ')+';'+
                      %EDITW(WSumRecup:'0           ,  ')+';'+
                      %EDITW(WSumPrmRec:'0           ,  ')+';'+
                      %EDITW(WSumEstRec:'0           ,  ')+';'+
                      %EDITW(WSumValRec:'0           ,  ')+';'+
                      %EDITW(WRacio:'  .   ,   ');
                WRITE TMPFILE R#TEMP;
             ENDIF;

             //-------------------------------------------------------
             // Escrever o detalhe
             //-------------------------------------------------------
             IF (NOT LST#Sinistro(WIdx).IsEmpty);
                EXSR EscResumAno;
                IF (LST#Sinistro(WIdx).Ano = WAnoInicio);
                   WAnoSinist = WAnoInicio;

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
                               LST#Sinistro(WIdx).Origem+';'+
                               %CHAR(LST#Sinistro(WIdx).DataPart) + ';' +
                               %CHAR(LST#Sinistro(WIdx).DataSinistro) + ';' +
                               %CHAR(LST#Sinistro(WIdx).DataEncerra) + ';' +
                    %EDITW(LST#Sinistro(WIdx).TotProvis:'0           ,  ')+';'+
                    %EDITW(LST#Sinistro(wIdx).TotPagam:'0           ,  ')+';'+
                    %EDITW(LST#Sinistro(WIdx).TotRecupera:'0           ,  ');
                   WRITE TMPFILE R#TEMP;
                ENDIF;
                WIdx += 1;
             ELSE;
                WAnoInicio += 1;
                WIdx = 1;
             ENDIF;
          ENDDO;

          IF (WAnoRec <> *ZEROS);
             EXSR EscrevePremios;
             WRacio = *ZEROS;
             IF (WValRec > *ZEROS);
             // WRacio = (WTotPagam / WValRec);
                WRacio = ((WTotProvis-WTotRecup)/WValRec)*100;
             ENDIF;
             R#TEMP.Texto = ';;;;;;;'+
                            'RESUMO DO ANO '+%CHAR(WAnoRec) +';'+
                            %EDITW(WTotProvis:'0           ,  ')+';'+
                            %EDITW(WTotPagam:'0           ,  ')+';'+
                            %EDITW(WTotRecup:'0           ,  ')+';'+
                            %EDITW(WPrmRec:'0           ,  ')+';'+
                            %EDITW(WEstRec:'0           ,  ')+';'+
                            %EDITW(WValRec:'0           ,  ')+';'+
                            %EDITW(WRacio:'  .   ,   ');
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
       // Subrotina..: EscResumAno;
       // Descrição..: Escrever os prémios pagos no Período
       //=================================================================================
       BEGSR EscResumAno;
          IF (WAnoRec <> WAnoInicio);
             IF (WAnoRec <> *ZEROS);
                EXSR EscrevePremios;
                WRacio = *ZEROS;
                IF (WValRec > *ZEROS);
                // WRacio = (WTotPagam / WValRec);
                   WRacio = ((WTotProvis-WTotRecup)/WValRec)*100;
                ENDIF;
                R#TEMP.Texto = ';;;;;;;'+
                                  'RESUMO DO ANO '+%CHAR(WAnoRec) +';'+
                                  %EDITW(WTotProvis:'0           ,  ')+';'+
                                  %EDITW(WTotPagam:'0           ,  ')+';'+
                                  %EDITW(WTotRecup:'0           ,  ')+';'+
                                  %EDITW(WPrmRec:'0           ,  ')+';'+
                                  %EDITW(WEstRec:'0           ,  ')+';'+
                                  %EDITW(WValRec:'0           ,  ')+';'+
                                  %EDITW(WRacio:'  .   ,   ');
                WRITE TMPFILE R#TEMP;
             ENDIF;

             WTotProvis = *ZEROS;
             WTotPagam  = *ZEROS;
             WTotRecup  = *ZEROS;
             WAnoRec    = WAnoInicio;
          ENDIF;
       ENDSR;

       //=================================================================================
       // Subrotina..: EscrevePremios
       // Descrição..: Escrever os prémios pagos no Período
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
                 R#TEMP.Texto = ';;;;;;;RESUMO DO ANO '+
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
