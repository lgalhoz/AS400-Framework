     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PPRDO015
     H* Descrição.: Manutenção de Tabelas e Códigos
     H*==================================================================
     FCSF02     UF A E           K DISK    PREFIX(COS_)
     FGBASEGRO  IF   E           K DISK    PREFIX(OR_)
     FGBASEGRA  IF   E           K DISK    PREFIX(AUT_)
     FFPRDE015  CF   E             WORKSTN SFILE(RPRDE01502:REC#)
     F                                     PREFIX(ECR_)
     F                                     INFDS(INFD#KEYS)
     F*==========================================================
     F* Indicadores  Descrição
     F* -----------  --------------------------------------------
     F*      45      Condicionar campos com Input/Output
     F*      52      DSP
     F*      53      CLR
     F*      54      END
     D*==========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SSECO010

     D*==> Chamada de função Dynamic, para trabalhar detalhe do Item
     D ShellExec       PR                  EXTPGM('QCMDEXC')
     D   stdcmd                     300A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5 CONST
     D
     D*==> Base Auto (Original)
     D AppAuto         PR                  EXTPGM('GP010X')
     D   Option                       1A   CONST
     D   RamApol                      9A   CONST
     D   Data                         8S 0 CONST Options(*NOPASS)
     D
     D*==> Base Outros Ramos (Original)
     D AppOutros       PR                  EXTPGM('GP015X')
     D   Option                       1A   CONST
     D   RamApol                      9A   CONST
     D   Data                         8S 0 CONST Options(*NOPASS)
     D
     D*==> Apresentar uma query com os Recibos da Apólice
     D AppRecibos      PR                  EXTPGM('PPRDO016')
     D   Ramo                         2S 0 CONST
     D   Apolice                      7S 0 CONST
     D
     D*==> Estrutura de Tabelas ou Códigos
     D MntCoSegur      PR
     D   PCdRamo                      2S 0 CONST
     D   PNuApolice                   7S 0 CONST
     D
     D*==> Listar Planos Co-Seguro Aceite
     D PLANOCA         PR                  EXTPGM('LP002CSCL')
     D
     D*==> Listar Planos Co-Seguro Cedido
     D PLANOCC         PR                  EXTPGM('LP003CSCL')
     D
     D*==> Estrutura de dados do Registo
     D WIcErro         S               N   INZ(*OFF)
     D WCdMessage      S              7A   INZ(*BLANKS)
     D WIcOrigem       S              1A
     D REC#            S              4S 0
     D
     D WComando        S            300A   INZ(*BLANKS)
     D WDimMin         S              5S 0
     D WDimMax         S              5S 0
     D WDimensao       S              5S 0
     D WDominio        S              1A
     D WIncrem         S              5S 0
     D
     D KTE#Apolic      S              5S 0
     D KTE#CoSegu      S              5S 0
     C*========================================================
      /FREE

       //------------------------------------
       // Se não estiver autorizado então sai
       //------------------------------------
       IF NOT $ValAccess(S#PGM:'':99999);
          *INLR = *ON;
          RETURN;
       ENDIF;

       //----------------------------------------------
       // Lista de Apólices de Co-Seguro Existentes
       //----------------------------------------------
       KTE#Apolic = $SetFiles('CSF02 LEFT  JOIN CSF03 ON (CSF02.NUMSEG = +
                      CSF03.NUMSEGJM)');
       $SetFields('CSF02':*BLANKS);
       $SetFields('CSF03':*BLANKS:*OFF);
       $addCriteria('Segurador..:':'NUMSEGLS':'LK');
       $addCriteria('Apólice....:':'APOL':'GE');
       $addCriteria('Referencia.:':'APLDR':'LK');
       $SetLstFld('RAMO':'Ramo':4:'D');
       $SetLstFld('APOL':'Apólice':8:'D');
       $SetLstFld('CS':'Cedido':2:'C');
       $SetLstFld('NUMSEGLS':'Segurador':10:'D');
       $SetLstFld('DATA':'Data':8:'D');
       $SetLstFld('APLDR':'Referencia':14:'D');
       $SetCriteria('LIDER = ''S''');
       $SetSort('DATA');
       $SetPage(12);

       //----------------------------------------------
       // Lista de Intervinientes da Apólice
       //----------------------------------------------
       KTE#CoSegu = $SetFiles('CSF02 INNER JOIN CSF03 ON +
                               (CSF02.NUMSEG = CSF03.NUMSEGJM)');
       $SetFields('CSF02':*BLANKS);
       $SetFields('CSF03':*BLANKS:*OFF);
       $addCriteria('Segurador..:':'NUMSEGLS':'LK');
       $addCriteria('Apólice....:':'APOL':'GE');
       $SetLstFld('RAMO':'Ramo':4:'D');
       $SetLstFld('APOL':'Apólice':8:'D');
       $SetLstFld('CS':'Cedido':2:'C');
       $SetLstFld('NUMSEGLS':'Segurador':10:'D');
       $SetLstFld('LIDER':'Lider':5:'C');
       $SetLstFld('DATA':'Data':8:'D');
       $SetLstFld('QPER':'Respons.':8:'D');
       $SetPage(12);

       //----------------------------------------------
       // Iniciar Apresentação dos Dados
       //----------------------------------------------
       $SetDBSession(KTE#Apolic); // Repor Sessão das Funções
       ECR_DSCRITER = $getCritText();
       MntCoSegur(*ZEROS:*ZEROS);

       $RmvDBSession(KTE#Apolic);
       $RmvDBSession(KTE#CoSegu);
       *INLR = *ON;
       RETURN;
      /END-FREE

     P*===================================================================
     P* Processo..: MntCoSegur
     P* Descrição.: Realizar a manutenção das Tabelas de CoSeguro
     P*===================================================================
     P MntCoSegur      B                   EXPORT
     D MntCoSegur      PI
     D   PCdRamo                      2S 0 CONST
     D   PNuapolice                   7S 0 CONST
     D
     D WIOMode         S              1S 0 INZ(*ZEROS)
     D WNoFunction     S             10A   INZ(*BLANKS)
     D WIsEmpty        S               N
     D WSelected       S               N
     D WRamoApolice    S              9A
     D WSession        S              5S 0
     D
     D*==> Definição das constantes
     D KTE_Display     C                   0
     D KTE_Insert      C                   1
     D KTE_Change      C                   2
     D KTE_Delete      C                   3
     D*===================================================================
      /FREE

       WIOMode    = KTE_Display;

       EXSR getRecords;
       DOW (NOT *IN03 AND NOT *IN12);
          IF ($GetDBSession() = KTE#Apolic);
             WNoFunction = 'RPRDE01503'; // Listar Apólices
          ELSE;
             WNoFunction = 'RPRDE01504'; // Listar Intervenientes
          ENDIF;
          DS#HEADER   = $getHeader(S#PGM:WNoFunction);
          DS#FOOTER   = $getFooter(S#PGM:WNoFunction:*BLANKS);
          DS#OPTIONS   = $getOption(S#PGM:WNoFunction:*BLANKS);
          ECR_S#PGM   = S#PGM;
          ECR_ERR#MSG = *BLANKS;
          WRITE     RPRDE01500;
          WRITE     RPRDE01599;
          WRITE     RPRDE01598;
          EXFMT RPRDE01503;
          IF (Func#Key = KEY#F05);
             $ChgCriteria();
             ECR_DSCRITER = $getCritText();
          ELSEIF ($ValidKey(Func#Key:KEY#F06));
             WIOMode    = KTE_Insert;
             EXSR DspInterven;
             *IN12 = *OFF;
             WIOMode    = KTE_Display;
          ELSEIF ($ValidKey(Func#Key:KEY#F14) AND (WNoFunction = 'RPRDE01503'));
             WComando = 'RUNQRY SOURCE/PLANOCS';
   29        ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
             WComando = 'DSPSPLF FILE(QPQUPRFIL) JOB('+%CHAR(S#JOB#)+'/'+
                               %TRIM(S#USR)+'/'+%TRIM(S#JOB)+') SPLNBR(*LAST)';
   29        ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
          ELSEIF ($ValidKey(Func#Key:KEY#F15) AND (WNoFunction = 'RPRDE01503'));
             PlanoCA();
             WComando = 'DSPSPLF FILE(LP002CS) JOB('+%CHAR(S#JOB#)+'/'+
                               %TRIM(S#USR)+'/'+%TRIM(S#JOB)+') SPLNBR(*LAST)';
   29        ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
          ELSEIF ($ValidKey(Func#Key:KEY#F16) AND (WNoFunction = 'RPRDE01503'));
             PlanoCC();
             WComando = 'DSPSPLF FILE(LP003CS) JOB('+%CHAR(S#JOB#)+'/'+
                               %TRIM(S#USR)+'/'+%TRIM(S#JOB)+') SPLNBR(*LAST)';
   29        ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
          ELSEIF ((Func#Key = KEY#PgDown) AND (NOT $IsLastPage())) OR
             ((Func#Key = KEY#PgUp) AND ($GetPageNbr() > 1));
             EXSR getRecords;
          ELSEIF (Func#Key = KEY#Enter);
             EXSR SelRec;
             EXSR getRecords;
             *IN12 = *OFF;
          ENDIF;
       ENDDO;

       //========================================================
       // Subrotina.: GetRecords
       // Objectivo.: Obter todos os registos de tabelas disponív
       //========================================================
       BEGSR getRecords;
          REC#      = *ZEROS;   // Controlo de Registos da Lista
          *In53     = *On;      // Limpar Lista de Ecrã
          *In52     = *On;      // Mostrar a Lista de Ecrã
          WRITE RPRDE01503;     // Limpar Lista de Funções
          *In53     = *Off;     // Repor Indicador
          *In52     = *Off;     // Mostrar a Lista de Ecrã
          IF ($GetPage(Func#Key) > *ZEROS);
             DOW (NOT $IsEOF());
                //------------------------------------------------
                // Escreve os Registo da Listagem
                //------------------------------------------------
                ECR_OPTION   = *BLANKS;
                ECR_LSTHDR = $GetLstHdr();
                ECR_LSTDET = $GetLstLine();

                //---------------------------------------------
                // Formatar as Chaves das Tabelas
                //---------------------------------------------
                ECR_APLDR    = $GetField('APLDR');
                MONITOR;
                   ECR_RAMO     = %DEC($GetField('RAMO'):2:0);
                   ECR_APOL     = %DEC($GetField('APOL'):7:0);
                   ECR_DATA     = %DEC($GetField('DATA'):8:0);
                   ECR_NUMSEG   = %DEC($GetField('NUMSEG'):4:0);
                   ECR_NUMSEGLS = %DEC($GetField('NUMSEGLS'):5:0);
                ON-ERROR;
                   ECR_RAMO     = *ZEROS;
                   ECR_APOL     = *ZEROS;
                   ECR_DATA     = *ZEROS;
                   ECR_NUMSEG   = *ZEROS;
                   ECR_NUMSEGLS = *ZEROS;
                ENDMON;

                REC# += 1;
                WRITE RPRDE01502;
                *IN52 = *ON;       // Encontrou registos
                $getNext();
             ENDDO;
          ENDIF;
          *IN54 = $IsLastPage();   // Indicação de Fim de Página
       ENDSR;

       //========================================================
       // Subrotina.: SelRec
       // Objectivo.: Seleccionar os registos
       //========================================================
       BEGSR SelRec;
          WSelected = *OFF;
          READC RPRDE01502;
          DOW (NOT %EOF());
             IF ($ValidOption(ECR_OPTION:'5')); // Intervenientes
                IF ($GetDBSession() = KTE#Apolic);
                   $SetDBSession(KTE#CoSegu);
                   $SetCriteria('(RAMO = ''' + %CHAR(ECR_RAMO) + ''') AND +
                                 (APOL = ''' + %CHAR(ECR_APOL) + ''')');
                   MntCoSegur(ECR_RAMO:ECR_APOL);
                   $SetDBSession(KTE#Apolic);
                ELSE;
                   EXSR DspInterven;
                ENDIF;
                LEAVESR;
                //-----------------------------------------
                // Consultar os dados da Apólice
                //-----------------------------------------
             ELSEIF ($ValidOption(ECR_OPTION:'2'));
                WRamoApolice = %TRIM(%EDITW(ECR_RAMO:'0  ')) +
                               %CHAR(ECR_APOL);
                IF (ECR_RAMO = 50) OR (ECR_RAMO = 51);
                   EXSR OpenDiario;
                   AppAuto('2':WRamoApolice);
                ELSE;
                   EXSR OpenDiario;
                   AppOutros('2':WRamoApolice);
                ENDIF;
             ELSEIF ($ValidOption(ECR_OPTION:'7'));
                WSession = $GetDBSession();
                AppRecibos(ECR_RAMO:ECR_APOL);
                $SetDBSession(WSession);
             ENDIF;
             READC RPRDE01502;
          ENDDO;
          IF (NOT WSelected AND WIOMode = KTE_Display);
             IF (ECR_CRITERIA <> *BLANKS);
                $SetCriteria(ECR_CRITERIA:*ON);
             ELSE;
                $SetCriteria(*BLANKS:*ON);
             ENDIF;
          ENDIF;
       ENDSR;

       //==================================================================
       // Subrotina.: DspInterven
       // Objectivo.: Apresentar Detalhe do Interveniente
       //==================================================================
       BEGSR DspInterven;
          ECR_DSLIDER  = *BLANKS;
          ECR_DSNUMSEG = *BLANKS;
          ECR_NOMSEG   = *BLANKS;
          ECR_DSCS     = *BLANKS;
          IF (WIOMode <> KTE_Insert);
             CHAIN(N) (ECR_RAMO:ECR_APOL:ECR_DATA:ECR_NUMSEG) CSF02F1;
             IF (%FOUND());
                ECR_RAMO     = COS_RAMO;
                ECR_APOL     = COS_APOL;
                ECR_CS       = COS_CS;
                ECR_DATA     = COS_DATA;
                ECR_APLDR    = COS_APLDR;
                ECR_NUMSEGLS = %DEC($GetCodigo(70:%Char(COS_NUMSEG):*ON):5  :0);
                ECR_DSNUMSEG = $GetDescricao(70:%Char(COS_NUMSEG):*ON);
                IF (COS_LIDER = 'S');
                   ECR_DSLIDER  = 'Sim';
                ELSE;
                   ECR_DSLIDER  = 'Não';
                ENDIF;
                ECR_DSCS     = $GetDescricao(69:%Char(COS_CS));

                ECR_NOMSEG   = *BLANKS;
                CHAIN (COS_RAMO:COS_APOL) GBASEGRA;
                IF (%FOUND());
                   ECR_NOMSEG = AUT_SEG;
                ELSE;
                   CHAIN (COS_RAMO:COS_APOL) GBASEGRO;
                   IF (%FOUND());
                      ECR_NOMSEG = OR_SEG;
                   ENDIF;
                ENDIF;
                ECR_LIDER    = COS_LIDER;
                ECR_QPER     = COS_QPER;
                ECR_TXGEST   = COS_TXGEST;
                ECR_TXGESS   = COS_TXGESS;
                IF ($GetDBSession()=KTE#CoSegu);
                   SETLL (ECR_RAMO:ECR_APOL) CSF02F1;
                   READE(N) (ECR_RAMO:ECR_APOL) CSF02F1;
                   DOW NOT %EOF();
                      IF (COS_LIDER = 'S');
                         ECR_APLDR    = COS_APLDR;
                         LEAVE;
                      ENDIF;
                      READE(N) (ECR_RAMO:ECR_APOL) CSF02F1;
                   ENDDO;
                ENDIF;
             ELSE;
                LEAVESR;
             ENDIF;
          ELSE;
             IF ($GetDBSession()=KTE#CoSegu);
                ECR_LIDER = 'N';
                //--------------------------------------------------
                // Formatar a quota parte de responsabilidade
                //--------------------------------------------------
                ECR_QPER = *ZEROS;
                SETLL (ECR_RAMO:ECR_APOL) CSF02F1;
                READE(N) (ECR_RAMO:ECR_APOL) CSF02F1;
                DOW NOT %EOF();
                   IF (COS_LIDER = 'S');
                      ECR_APLDR = COS_APLDR;
                      ECR_CS    = COS_CS;
                   ENDIF;
                   ECR_QPER    += COS_QPER;
                   READE(N) (ECR_RAMO:ECR_APOL) CSF02F1;
                ENDDO;
                ECR_QPER = 100 - ECR_QPER;
             ELSE;
                ECR_LIDER = 'S';
                ECR_RAMO     = *ZEROS;
                ECR_APOL     = *ZEROS;
                ECR_APLDR    = *BLANKS;
                ECR_QPER     = 100;
                ECR_CS       = *ZEROS;
             ENDIF;

             ECR_DATA     = *ZEROS; // Data do dia
             ECR_NUMSEG   = *ZEROS;
             ECR_NUMSEGLS = *ZEROS;
             ECR_TXGEST   = *ZEROS;
             ECR_TXGESS   = *ZEROS;
             *IN45 = *ON;
          ENDIF;
          IF (ECR_LIDER = 'S');
             *IN48 = *ON;
          ELSE;
             *IN48 = *OFF;
          ENDIF;
          DOW NOT (*IN03 OR *IN12);
             IF (WIOMode = KTE_Insert);
                WNoFunction = 'RPRDE01506'; // Adicionar um registo
             ELSEIF (WIOMode = KTE_Display);
                WNoFunction = 'RPRDE01505'; // Mostrar o registo
             ELSE;
                WNoFunction = 'RPRDE01521'; // Alterar o Registo
             ENDIF;
             DS#HEADER   = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER   = $getFooter(S#PGM:WNoFunction:*BLANKS);
             ECR_S#PGM   = S#PGM;
             WRITE     RPRDE01500;
             WRITE     RPRDE01599;
             EXFMT RPRDE01505;
             IF (Func#Key=KEY#F04) AND (WIOMode <> KTE_Display);
                EXSR RunPopUp;
             ELSEIF (Func#Key=KEY#F12) AND (WIOMode <> KTE_Display)
                AND (WIOMode <> KTE_Insert);
                WIOMode = KTE_Display;
                *IN45 = *OFF;
                *IN21 = *OFF;
                *IN12 = *OFF;
             ELSEIF ($ValidKey(Func#Key:KEY#F21)) AND (WIOMode = KTE_Display);
                WIOMode = KTE_Change;
                *IN45 = *ON;
             ELSEIF ($ValidKey(Func#Key:KEY#F22)) AND (WIOMode = KTE_Display);
                ECR_OPTION = 'N';
                EXFMT RPRDE01522;
                IF (Func#Key = KEY#Enter) AND (%TRIM(ECR_OPTION) = 'S');
                   IF ($GetDBSession()<>KTE#CoSegu);
                      SETLL (ECR_RAMO:ECR_APOL) CSF02F1;
                      READE (ECR_RAMO:ECR_APOL) CSF02F1;
                      DOW NOT %EOF();
                         IF (COS_LIDER = 'S');
                         // DELETE CSF02F1;
                         ENDIF;
                         READE (ECR_RAMO:ECR_APOL) CSF02F1;
                      ENDDO;
                   ELSE;
                   // DELETE (ECR_RAMO:ECR_APOL:ECR_DATA:
                   //         ECR_NUMSEG) CSF02F1;
                   ENDIF;
                   LEAVESR;
                ENDIF;
                *IN12 = *OFF;
             ELSEIF (Func#Key = KEY#ENTER);
                IF (WIOMode <> KTE_Display);
                   WIcErro = *OFF;
                   WCdMessage = *BLANKS;
                   ECR_ERR#MSG = *BLANKS;
                   EXSR ValCoSeguro;
                   IF (WIcErro);
                      ECR_ERR#MSG = $GetMessage(WCdMessage);
                      ITER;
                   ENDIF;
                   EXSR MntCoSeguro;
                   WIOMode = KTE_Display;
                   *IN45 = *OFF;
                ELSE;
                   LEAVESR;
                ENDIF;
             ENDIF;
          ENDDO;
          WIOMode = KTE_Display;
          *IN12 = *OFF;
          *IN45 = *OFF;
       ENDSR;

       //==================================================================
       // Subrotina.: ValCoSeguro
       // Objectivo.: Validar a criaçã ou alteração do Co-Seguro
       //==================================================================
       BEGSR ValCoSeguro;
          IF (ECR_RAMO = *ZEROS);
             WIcerro = *ON;
             WCdMessage = 'PRD0100';  // O Ramo não está Preenchidos
|            ECR_CM#COL = CM#CCOL;
             $getFldLoc('FPRDE015':'RPRDE01505':'RAMO':
                        ECR_CM#ROW:ECR_CM#COL);
          ELSEIF (ECR_APOL = *ZEROS);
             WIcerro = *ON;
             WCdMessage = 'PRD0101';  // A Apólice não está preenchida
|            ECR_CM#COL = CM#CCOL;
             $getFldLoc('FPRDE015':'RPRDE01505':'APOL':
                        ECR_CM#ROW:ECR_CM#COL);
          ELSEIF (ECR_APLDR = *BLANKS);
             WIcerro = *ON;
             WCdMessage = 'PRD0102';  // A Apólice de Referência sem valores
|            ECR_CM#COL = CM#CCOL;
             $getFldLoc('FPRDE015':'RPRDE01505':'APLDR':
                        ECR_CM#ROW:ECR_CM#COL);
          ELSEIF (ECR_CS = *ZEROS);
             WIcerro = *ON;
             WCdMessage = 'PRD0103';  // Tipo de CoSeguro não está Preenchido
|            ECR_CM#COL = CM#CCOL;
             $getFldLoc('FPRDE015':'RPRDE01505':'CS':
                        ECR_CM#ROW:ECR_CM#COL);
          ELSEIF (ECR_QPER = *ZEROS);
             WIcerro = *ON;
             WCdMessage = 'PRD0104';  // Quota parte de reponsabilidade sem valores
|            ECR_CM#COL = CM#CCOL;
             $getFldLoc('FPRDE015':'RPRDE01505':'QPER':
                        ECR_CM#ROW:ECR_CM#COL);
          ELSEIF (ECR_QPER > 100);
             WIcerro = *ON;
             WCdMessage = 'PRD0105';  // Validar se a Quota Parte superior a 100%
|            ECR_CM#COL = CM#CCOL;
             $getFldLoc('FPRDE015':'RPRDE01505':'QPER':
                          ECR_CM#ROW:ECR_CM#COL);
          ELSEIF (ECR_QPER > 100);
             WIcerro = *ON;
             WCdMessage = 'PRD0106';  // Validar se o Código de Co-Seguro é válido
|            ECR_CM#COL = CM#CCOL;
             $getFldLoc('FPRDE015':'RPRDE01505':'QPER':
                          ECR_CM#ROW:ECR_CM#COL);
          ELSE;
             IF (NOT $ValCodigo(70:%CHAR(ECR_NUMSEGLS)));
                WIcErro = *ON;
                WCdMessage = 'PRD0107';  // Código de Seguradora Inválido
|               ECR_CM#COL = CM#CCOL;
                $getFldLoc('FPRDE015':'RPRDE01505':'NUMSEGLS':
                             ECR_CM#ROW:ECR_CM#COL);
             ENDIF;
             IF (NOT $ValCodigo(69:%CHAR(ECR_CS)));
                WIcErro = *ON;
                WCdMessage = 'PRD0108';  // Código de Tipo Co-Seguro Inválido
|               ECR_CM#COL = CM#CCOL;
                $getFldLoc('FPRDE015':'RPRDE01505':'CS':
                             ECR_CM#ROW:ECR_CM#COL);
             ENDIF;
          ENDIF;
       ENDSR;

       //==================================================================
       // Subrotina.: MntCoSeguro
       // Objectivo.: Alterar ou criar os dados do Co-Seguro
       //==================================================================
       BEGSR MntCoSeguro;
          IF (WIOMode = KTE_Insert);
             COS_RAMO   = ECR_RAMO;
             COS_APOL   = ECR_APOL;
             COS_CS     = ECR_CS;
             COS_DATA   = ECR_DATA;
             COS_NUMSEG = %DEC($GetCodigo(70:%Char(ECR_NUMSEGLS)):4:0);
             COS_LIDER  = ECR_LIDER;
             COS_QPER   = ECR_QPER;
             COS_TXGEST = ECR_TXGEST;
             COS_TXGESS = ECR_TXGESS;
             COS_APLDR  = ECR_APLDR;
             WRITE CSF02F1;
          ELSEIF (WIOMode = KTE_Change);
             CHAIN (ECR_RAMO:ECR_APOL:ECR_DATA:ECR_NUMSEG) CSF02F1;
             IF (%FOUND());
                COS_NUMSEG = %DEC($GetCodigo(70:%Char(ECR_NUMSEGLS)):4:0);
                COS_DATA   = ECR_DATA;     // Data
                COS_QPER   = ECR_QPER;     // Percentagem;
                COS_TXGEST = ECR_TXGEST;   // Taxa Administrativa
                COS_TXGESS = ECR_TXGESS;   // Taxa de Gestão
                COS_APLDR  = ECR_APLDR;    // Apólice Lider
                UPDATE CSF02F1 %FIELDS(COS_NUMSEG:COS_DATA:COS_QPER:
                                       COS_TXGEST:COS_TXGESS:
                                       COS_APLDR);
             ENDIF;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: OpenDiario
       // Objectivo.: Abrir Ficheiro diário em função do Base
       //========================================================
       BEGSR OpenDiario;
          //--------------------------------------------------
          // Se o base estiver fechado então usar temporarios
          //--------------------------------------------------
          MONITOR;
             IF ($checkObject('GP010A':'QS36F':'*FILE')); // GP010A
                WComando = 'OVRDBF FILE(GBASEDIA) TOFILE(SEXTRAS/JBASEDIA)';
   29           ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
             ELSE;
                WComando = 'OVRDBF FILE(GBASEDIA) +
                           TOFILE('+%TRIM('QS36F')+'/GBASEDIA)';
   29           ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
             ENDIF;

             IF ($checkObject('GP015A':'QS36F':'*FILE')); // GP015A
                WComando = 'OVRDBF FILE(GBASEDIO) TOFILE(SEXTRAS/JBASEDIO)';
   29           ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
             ELSE;
                WComando = 'OVRDBF FILE(GBASEDIO) +
                            TOFILE('+%TRIM('QS36F')+'/GBASEDIO)';
   29           ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
             ENDIF;
          ON-ERROR;
          ENDMON;
       ENDSR;


       //==================================================================
       // Subrotina.: RunPopUp
       // Objectivo.: Adicionar um novo registo de uma Apólice
       //==================================================================
       BEGSR RunPopUp;
          MONITOR;
          IF (ECR_CM#NMFLD = 'CS');
             ECR_CS = %DEC($runPopUp(69:%CHAR(ECR_CS)):1:0);
             ECR_DSCS = $GetDescricao(69:%CHAR(ECR_CS));
          ELSEIF (ECR_CM#NMFLD = 'NUMSEGLS');
             ECR_NUMSEGLS = %DEC($runPopUp(70:%CHAR(ECR_NUMSEGLS)):5:0);
             ECR_DSNUMSEG = $GetDescricao(70:%CHAR(ECR_NUMSEGLS));
          ENDIF;
          ON-ERROR;
          ENDMON;
          ECR_CM#ROW = CM#CROW;
          ECR_CM#COL = CM#CCOL;
       ENDSR;
      /END-FREE
     PMntCoSegur       E
