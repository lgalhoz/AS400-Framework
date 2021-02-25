     H*===========================================================
     H* Objectivo..: Seleccionar os meses dos continuados (reais)
     H*              e os meses previstos para a uma nova emissão
     H*              dos continuados.
     H*===========================================================
     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP('JMGRP')
     H*===========================================================
     FFVNCP020  UF A E           K DISK
     FFVNCP02001IF   E           K DISK    RENAME(RVNCP020:RVNCP02001)
     FFVNCE011  CF   E             WORKSTN SFILE(RVNCE01102:REC#)
     F                                     SFILE(RVNCE01132:REC#)
     F*===========================================================
     F* Indicadores  Descrição
     F* -----------  ---------------------------------------------
     F*      03      Tecla F3
     F*      06      Tecla F6
     F*      12      Tecla F12
     F*      30      FTBLP010
     F*      50      Tecla PagDn
     F*      51      Tecla PagUp
     F*      52      DSP
     F*      53      CLR
     F*      54      END
     F*===========================================================
     D PSDS           SDS
     D   S#PGM                 1     10
     D   S#JOB               244    253
     D   S#USR               254    263
     D   S#JOB#              264    269  0
     D
     D REC#            S              4  0
     D WIdx            S              5  0
     D WBotPage        S               N   INZ(*OFF)
     D WTopPage        S               N   INZ(*OFF)
     D WSelKey         S               N   INZ(*OFF)
     D
     D*==> Obter a descrição para o Header e Footer
     D getHeader       PR            50A
     D   AppName                     10A   CONST
     D   Function                    10A   CONST
     D
     D getFooter       PR           156A
     D   AppName                     10A   CONST
     D   Function                    10A   CONST
     D
     D*==> Validar se está autorizado
     D ValAccess       PR              N
     D  AppName                      10A   CONST
     D  Function                     10A   CONST
     D  InVal                         5S 0 CONST
     D
     D*==> Obter a descrição para o Código
     D getDescricao    PR            40A
     D   PCdIndice                    5S 0 CONST
     D   PCodigo                     15A   CONST
     D
     D*==> Formatar a Intrução SQL
     D getSQLStmt      PR          1000A
     D    Fields                    300A   Const
     D    Files                     300A   Const
     D    Where                     300A   Const
     D    OrderBy                   100A   Const
     D    PosIni                      3P 0 Const
     D    Qtde                        3P 0 Const
     D    Resumo                       N   Const
     D
     D*==> Apresentar mensagem de erro
     D QUILNGTX        PR                  ExtPgm('QUILNGTX')
     D   text                     65535a   const options(*varsize)
     D   length                      10i 0 const
     D   msgid                        7a   const
     D   qualmsgf                    20a   const
     D   errorCode                32767a   options(*varsize)
     D ErrorCode       ds                  qualified
     D   bytesProv                   10i 0 inz(0)
     D   bytesAvail                  10i 0 inz(0)
     D
     D*==> Resultado dos Recibos
     D RecRecibo       DS                  QUALIFIED DIM(12)
     D    NumApol                    10A
     D    NumRec                      8A
     D    CSegur                      6S 0
     D    ValRec                     13P 2
     D    CRamo                       3S 0
     D    CodEmp                     50A
     D    Nome                       36A
     D    IcdVen                      1A
     D
     D*==> Restantes campos de Trabalho
     D WRetCode        S              5S 0
     D WRow            S              3S 0
     D WCol            S              3S 0
     D WData           S               D   INZ(*JOB) DATFMT(*ISO)
     D Wano            S              4S 0
     D WMes            S              2S 0
     D DescMes         S             10    DIM(12) CTDATA PERRCD(1)
     D WPeriodo        S              1A
     D WSQLStmt        S           1000A
     D WRecAct         S              5S 0
     D WRecActBK       S              5S 0
     D WQtdeReg        S              5S 0
     D WErrorMsg       S            300A
     D*===========================================================
     C
      /FREE

       //------------------------------------
       // Se não estiver autorizado então sai
       //------------------------------------
       IF NOT ValAccess(S#PGM:'':99999);
          *INLR = *ON;
          RETURN;
       ENDIF;

       //----------------------------------------
       // Afixar o ecrã para selecção do Periodo
       //----------------------------------------
       DOU       (*In12 = *on) OR (*In03 = *on);
          WOperacao = getHeader(S#PGM:'RVNCE01103');
          WRITE     RVNCE01100;
          ERRMSG  = *BLANKS;
          FNCKEY01  = getFooter(S#PGM:'RVNCE01103');
          WRITE     RVNCE01199;
          EXSR      GetPeriodo; // Carregar Lista com Registos
          EXFMT     RVNCE01103;
          IF (*IN03) OR (*IN12);
             LEAVE;
          ELSEIF (*IN06) AND ValAccess(S#PGM:'RVNCE01103':6);
             Exsr AddPerCont;
          ENDIF;
          WPeriodo = 'N';
          EXSR selPeriodo;
          IF (WPeriodo <> 'N');
             EXSR DspRecord;
          ENDIF;
       ENDDO;

       *InLR = *ON;

       //==============================================================
       // Subrotina.: ErrorMsg
       // Objectivo.: Apresentar uma mensagem de erro
       //==============================================================
       BEGSR ErrorMsg;
         WErrorMsg = 'Ocorreram Erros na execução do Comando.';
         QUILNGTX(WErrorMsg
                 : %len(WErrorMsg)
                 : *BLANKS
                 : *BLANKS
                 : errorCode );
       ENDSR;

       //==============================================================
       // Subrotina.: dspRecord
       // Objectivo.: Apresentar os registos para o periodo seleccionado
       //==============================================================
       BEGSR dspRecord;
          //------------------------------------
          // Inicializar as variáveis de Trabalho
          //------------------------------------
          WSelKey  = *Off;
          WBotPage = *Off;
          WTopPage = *Off;
          WRecAct  = 1;
          WRecActBk= WRecAct;
          WQtdeReg = -1; // Inicializar totalizador

          //-----------------------------------------
          // Afixar registos que obedecem à selecção
          //-----------------------------------------
          DOW (NOT *In12) AND (NOT *In03);
             WOperacao = getHeader(S#PGM:'RVNCE01133');
             WRITE     RVNCE01100;
             FNCKEY01  = getFooter(S#PGM:'RVNCE01133');
             WRITE     RVNCE01199;
             EXSR      GetRecords; // Carregar Lista com Registos
             IF (REC# = 0);
                EXSR ErrorMsg;
                LEAVE;
             ELSE;
                EXFMT     RVNCE01133;
                IF (*IN50);
                   WRecAct += 12;
                ELSEIF (*IN51);
                   WRecAct -= 12;
                ELSEIF (*IN06) AND ValAccess(S#PGM:'RVNCE01133':6);
                // EXSR addRecord;
                ELSE;
                   EXSR selRecords;
                ENDIF;
             ENDIF;
          ENDDO;
          *IN12 = *OFF;
       ENDSR;

       //==============================================================
       // Subrotina.: delRecord
       // Objectivo.: Alterar os dados do Registo
       //==============================================================
       BEGSR delRecord;
          WOption = 'N';
          EXFMT RVNCE01122;
          IF (*IN12);
             *IN12 = *OFF;
             WOption = 'N';
          ENDIF;

          IF (WOption = 'S');
          // CHAIN CDUSER RSECP020;
          // DELETE RSECP020;
             *IN12 = *ON;
          ENDIF;
       ENDSR;

       //==============================================================
       // Subrotina.: InzRecords
       // Objectivo.: Inicializar a lista de registos
       //==============================================================
       BEGSR inzRecords;
          IF (WQtdeReg < 0);
             EXSR GetQtdeReg;  // Obter o total de registos
          ENDIF;

          //-------------------------
          // Inicializar o Array
          //-------------------------
          FOR WIdx = 1 TO 12;
             RecRecibo(WIdx).CRAMO = 0;
             RecRecibo(WIdx).NUMAPOL = '';
             RecRecibo(Widx).VALREC = 0;
             RecRecibo(Widx).CODEMP = '';
             RecRecibo(Widx).NOME = '';
             RecRecibo(Widx).NUMREC = '';
             RecRecibo(Widx).CSEGUR = 0;
             RecRecibo(Widx).ICDVEN = '';
          ENDFOR;

          REC# = 0;         // Controlo de Registos da Lista de Ecrã
          *In53 = *On;      // Limpar Lista de Ecrã
          *In52 = *On;      // Mostrar a Lista de Ecrã
          WRITE RVNCE01133; // Limpar Lista
          *In53 = *Off;     // Repor Indicador

          //------------------------------------------------
          // Controlar fim de página
          //------------------------------------------------
          IF (WRecAct > WQtdeReg);
             ERRMSG  = 'Fim de página';
             WRecAct = WRecAct - 11;  // Fim de Página
             WRecAct = WRecActBk;     // Fim de Página
             *In54 = *ON;
          ELSEIF (WRecAct < 1);
             WRecAct = 1;
             *In54 = *OFF;
              ERRMSG = 'Inicio de página.';
          ELSE;
             ERRMSG  = *BLANKS;
             *In54 = *OFF;
          ENDIF;
       ENDSR;

       //==============================================================
       // Subrotina.: getQtdeReg
       // Objectivo.: Obter a Quantidade de Registos a extrair
       //==============================================================
       BEGSR getQtdeReg;
          //------------------------------------------------
          // Seleccionar os registos a apresentar
          //------------------------------------------------
          WSqlStmt = getSQLStmt('NUMAPOL, NUMREC, CSEGUR, VALREC, CRAMO':
                                'FRECT010':
                                '(DTREC='+%CHAR(WSelData)+') AND (CC0=1)':
                                '':WRecAct:12:*On);

          // DSPLY WRecAct;
          EXEC SQL PREPARE STMTOT FROM :WSqlStmt;
          EXEC SQL DECLARE TOT CURSOR FOR STMTOT;
          EXEC SQL OPEN TOT;
          DOW (sqlcod = 0);
             EXEC SQL FETCH TOT INTO: WQtdeReg;
          ENDDO;
          EXEC SQL CLOSE TOT;
       ENDSR;

       //==============================================================
       // Subrotina.: SelRecords
       // Objectivo.: Seleccionar os registos
       //==============================================================
       BEGSR selRecords;
          WIdx = 0;
          READC RVNCE01102;
          DOW (NOT %EOF);
             IF (WSelect = '1');
             // EXSR dspRecord;
                WSelect = ' ';
                LEAVE;
             ELSE;
                WSelect = ' ';
             ENDIF;
             READC RVNCE01102;
          ENDDO;
       ENDSR;

       //==============================================================
       // Subrotina.: GetRecords
       // Objectivo.: Obter todos os registos de tabelas disponíveis
       //==============================================================
       BEGSR getRecords;
          EXSR      InzRecords;
          //------------------------------------------------
          // Seleccionar os registos a apresentar
          //------------------------------------------------
          WSqlStmt = getSQLStmt('NUMAPOL, NUMREC, CSEGUR, VALREC, CRAMO,+
                                 CODEMP, NOME, ICDVEN':
                                'frect010 INNER JOIN (fentt010 INNER JOIN +
                                 qs36f/gfsegext on (fentt010.nseg = +
                                 gfsegext.nseg)) on (frect010.csegur = +
                                 fentt010.nseg)':
                                '(DTREC='+%CHAR(WSelData)+') AND (CC0=1)':
                                'ORDER BY CODEMP DESC':
                                WRecAct:12:*Off);

          // DSPLY WRecAct;
          EXEC SQL PREPARE STMREC FROM :WSqlStmt;
          EXEC SQL DECLARE REC CURSOR FOR STMREC;
          EXEC SQL OPEN REC;
          DOW (sqlcod = 0);
             EXEC SQL FETCH NEXT FROM REC FOR 12 ROWS INTO: RecRecibo;
          ENDDO;
          EXEC SQL CLOSE REC;

          //------------------------------------------------
          // Actualizar a listagem de ecrã com o resultado
          //------------------------------------------------
          //dsply %ELEM(RecRecibo);
          monitor;
             FOR WIdx = 1 TO 12;
                IF (RecRecibo(WIdx).CRAMO > 0);
                   WRamo    = RecRecibo(Widx).CRAMO;
                   WApolice = %TRIM(RecRecibo(WIdx).NUMAPOL);
                   WPremio  = RecRecibo(WIdx).VALREC;
                   WChave   = RecRecibo(WIdx).CODEMP;
                   WNome    = RecRecibo(WIdx).NOME;
                   REC# += 1;
                   WRITE RVNCE01132;
                ENDIF;
             ENDFOR;
          on-error;
             // REC# = 0;
          endmon;
       ENDSR;

       //==============================================================
       // Subrotina.: GetPeriodo
       // Objectivo.: Obter os meses do processamento
       //==============================================================
       BEGSR getPeriodo;
          //----------------------------
          // Inicializar as variáveis
          //----------------------------
          REC# = 0;         // Controlo de Registos da Lista de Ecrã
          *In53 = *On;      // Limpar Lista de Ecrã
          *In52 = *On;      // Mostrar a Lista de Ecrã
          WRITE RVNCE01103; // Limpar Lista
          *In53 = *Off;     // Repor Indicador
          WAno = %SUBDT(WData:*YEARS);
          WMes = %SUBDT(WData:*MONTHS);
          WMes += 5;
          IF (WMes > 12);
             WMes = 12 - WMes;
             WAno = Wano + 1;
          ENDIF;
          DOU (REC# = 12);
             REC# += 1;
             WSelAno  = WAno;
             WSelMes  = WMes;
             WMesAno  = 'Continuados de '+ %Trim(DescMes(WMes)) +
                       ' de ' + %Char(WAno);
             SETLL (WAno:WMes) RVNCP02001;
             READE (WAno:Wmes) RVNCP02001;
             IF (NOT %EOF);
                WObserva = 'Já foi processado.';
                WIcProc  ='S';
                WSelData = %SUBDT(DTEMISS:*YEARS) * 10000;
                WSelData += %SUBDT(DTEMISS:*MONTHS) * 100;
                WSelData += %SUBDT(DTEMISS:*DAYS);
             ELSE;
                WObserva = 'Ainda não foi processado.';
                WIcProc  ='N';
                WSelData = 19700101;
             ENDIF;
             WMes = WMes - 1;
             IF (WMes < 1);
                WMes = 12;
                WAno = WAno - 1;
             ENDIF;
             WRITE RVNCE01102;
          ENDDO;
       ENDSR;

       //==============================================================
       // Subrotina.: SelPeriodo
       // Objectivo.: Forncer o Periodo Seleccionado
       //==============================================================
       BEGSR selPeriodo;
          WIdx = 0;
          READC RVNCE01102;
          DOW (NOT %EOF);
             IF (WSelect = '1');
                WMes = WSelMes;
                WAno = WSelAno;
                WPeriodo = 'S';
                WSelect = ' ';
                LEAVE;
             ELSE;
                WSelect = ' ';
             ENDIF;
             READC RVNCE01102;
          ENDDO;
       ENDSR;
       //==============================================================
       // Subrotina.: addPerCont
       // Objectivo.: Adicionar o Periodo dos Continuados
       //==============================================================
       BEGSR addPerCont;
          WOperacao = getHeader(S#PGM:'RVNCE01106');
          WRITE     RVNCE01100;
          FNCKEY01  = getFooter(S#PGM:'RVNCE01106');
          WRITE     RVNCE01199;
          EXFMT   RVNCE01106;
          IF (NOT *IN03) AND (NOT *IN12);
             ANEMISS = %SUBDT(DTEMISS:*YEARS);
             MSEMISS = %SUBDT(DTEMISS:*MONTHS);
             MSEMISS = MSEMISS + 2;
             IF (MSEMISS > 12);
                MSEMISS = MSEMISS - 12;
                ANEMISS = ANEMISS + 1;
             ENDIF;
             WRITE   RVNCP020;
          ENDIF;
          *IN12 = *OFF;
       ENDSR;
      /END-FREE
**
Janeiro
Fevereiro
Março
Abril
Maio
Junho
Julho
Agosto
Setembro
Outubro
Novembro
Dezembro
