     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PPRDO112
     H* Descrição.: Manutenção de Intruções de Débito em Conta
     H*------------------------------------------------------------------
     H* A1. Incluir o númer SegurNet, e se não existir então não
     H*     cria a carta verde.
     H*==================================================================
     FGBASEGRA  IF   E           K DISK    PREFIX(BAS_)
     FGBASEDIA  IF   E           K DISK    PREFIX(BAS_)
     FGFSEG     IF   E           K DISK    PREFIX(SEG_)
     FFDOCP060  UF A E           K DISK    PREFIX(DOC_)
     FFPRDE114  CF   E             WORKSTN SFILE(RPRDE11402:REC#)
     F                                     SFILE(RPRDE11408:REC#)
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
      /Copy QRPGLESRC,SGRLO013
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SCATO010
      /Copy QRPGLESRC,SSECO010
      /Copy QRPGLESRC,SENTO010

     D*==> Parâmetros de Entrada
     D WCodProduto     S              2S 0
     D WDatInicio      S              8S 0
     D
     D*==> Campos de Trabalho
     D REC#            S              4S 0
     D WIcOrigem       S              1A
     D WMsgErro        S              7A
     D WDatSelect      S              8S 0
     D WRow            S              3S 0
     D WCol            S              3S 0
     D WNofunction     S             10A
     D WOptions        S             24A
     D WIcInsert       S               N
     D WSelected       S               N
     D WSessScr        S              3S 0
     D WIdcRet         S              2S 0
     D WCancelar       S              1A
     D WKeysTohide     S             24A   INZ(*ZEROS)
     D WOptTohide      S             24A   INZ(*ZEROS)
     D WIsFrota        S               N
     D WIsMae          S               N
     D
     D KTE#CV          S              5S 0
     D KTE#FR          S              5S 0
     C*========================================================
      /FREE

       //------------------------------------
       // Validar o acesso a esta função
       //------------------------------------
       IF NOT $ValAccess(S#PGM:'':99999);
          $ShowDialog('SEC0001');
          *INLR = *ON;
          RETURN;
       ENDIF;

       //--------------------------------------------------
       // Transferir controlo do Ecrã para a API
       //--------------------------------------------------

       //----------------------------------------------
       // Registos de Cartas Verde
       //----------------------------------------------
       KTE#CV = $SETFILES('FDOCP060');
       $addCriteria('Apólice....:':'NUAPOL':'GE':'D');
       $addCriteria('Carta Verde:':'NCV':'GE');
       $addCriteria('Segurado...:':'NUMSEG':'GE');
       $addCriteria('Nome.......:':'NOMSEG':'LK');
       $addCriteria('Matricula..:':'NUMATR1':'LK');
       $SetLstFld('ANNCV':'Ano':4:'D');
       $SetLstFld('NCV':'Número':9:'D');
       $SetLstFld('NUAPOL':'Apólice':7:'D');
       $SetLstFld('NOMSEG':'Nome':40:'E');
       $SetLstFld('NUMATR1':'Matricula':15:'E');
       $SetPage(12);

       //----------------------------------------------
       // Registos de Apólices Frota
       //----------------------------------------------
       KTE#FR = $SETFILES('GBASEGRA');
       $addCriteria('Apólice....:':'NUAPOL':'GE':'D');
       $addCriteria('Matricula..:':'MATR1':'LK');
       $SetLstFld('NUAPOL':'Apólice':7:'D');
       $SetLstFld('SEG':'Nome':40:'E');
       $SetLstFld('MATR1':'Matricula':15:'E');
       $SetPage(12);

       $SetDBSession(KTE#CV);
       ECR_DSCRITER = $getCritText();
       EXSR ShowAll;

       $RmvDBSession(KTE#CV);
       $RmvDBSession(KTE#FR);
       *INLR = *ON;
       RETURN;

       //========================================================
       // Subrotina.: ShowAll
       // Objectivo.: Mostrar todas as Cartas Verdes
       //========================================================
       BEGSR ShowAll;
          DOW (NOT *IN03 AND NOT *IN12);
             EXSR getRecords;
             WOptions = *BLANKS;
             WNoFunction = 'RPRDE11403'; // Listar Componentes
             DS#HEADER   = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER   = $getFooter(S#PGM:WNoFunction:WOptions);
             DS#OPTIONS  = $getOption(S#PGM:WNoFunction:WOptions);
             ECR_S#PGM   = S#PGM;
             ECR_ERR#MSG = *BLANKS;
             WRITE RPRDE11400;
             WRITE RPRDE11499;
             WRITE RPRDE11498;
             EXFMT RPRDE11403;
             IF (Func#Key = KEY#F05);
                $ChgCriteria();
                ECR_DSCRITER = $getCritText();
             ELSEIF ($ValidKey(Func#Key:KEY#F06));
                WIcInsert  = *ON;
                EXSR AddCVerde;
                *IN12 = *OFF;
                WIcInsert  = *OFF;
             ELSEIF (Func#Key = KEY#Enter);
                EXSR SelRec;
                *IN12 = *OFF;
             ENDIF;
          ENDDO;
       ENDSR;

       //========================================================
       // Subrotina.: GetRecords
       // Objectivo.: Obter todos os registos de tabelas disponív
       //========================================================
       BEGSR getRecords;
          REC#      = *ZEROS;   // Controlo de Registos da Lista
          *In53     = *On;      // Limpar Lista de Ecrã
          *In52     = *On;      // Mostrar a Lista de Ecrã
          WRITE RPRDE11403;     // Limpar Lista de Funções
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
                ECR_NCV    = $GetFieldNum('NCV');
                ECR_ANNCV  = $GetFieldNum('ANNCV');
                REC# += 1;
                WRITE RPRDE11402;
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
          READC RPRDE11402;
          DOW (NOT %EOF());
             WSelected = *ON;
             IF ($ValidOption(ECR_OPTION:'5'));
                WIcInsert = *OFF;
                EXSR ShowDet;
             ELSEIF ($ValidOption(ECR_OPTION:'6'));
                LEAVESR;
             ENDIF;
             READC RPRDE11402;
          ENDDO;
          IF (NOT WSelected);
             IF (ECR_CRITERIA <> *BLANKS);
                $SetCriteria(ECR_CRITERIA:*ON);
             ELSE;
                $SetCriteria(*BLANKS:*ON);
             ENDIF;
          ENDIF;
       ENDSR;

       //==================================================================
       // Subrotina.: ADDCVerde
       // Objectivo.: Adicionar uma Carta Verde
       //==================================================================
       BEGSR ADDCVerde;
          *IN50 = *ON;
          ECR_ANNCV    = *YEAR;
          ECR_CRAMO    = 50;
          ECR_NUAPOL   = *ZEROS;
          ECR_DTINICIO = *ZEROS;
          ECR_DTTERMO  = *ZEROS;
          WSessScr = $AddScreen('FPRDE114':'RPRDE11406');
          $AddScPFld(%ADDR(ECR_F#01):'NUAPOL');  // Número da Apólice
          $AddScPFld(%ADDR(ECR_F#02):'DTINICIO');// Data de Inicio
          $AddScPFld(%ADDR(ECR_F#03):'DTTERMO'); // Data de Termo
          $AddScPFld(%ADDR(ECR_F#04):'ANNCV');   // Ano da Carta Verde
          $AddScCoord(%ADDR(ECR_CM#ROW):%ADDR(ECR_CM#COL));
          $AddScMsg(%ADDR(ECR_ERR#MSG));
          WOptions  = *ZEROS;
          WKeysToHide = *ZEROS;
          DOW (NOT *IN03 AND NOT *IN12);
             ECR_NCV  = $GetNextKey('FDOCP060':'NCV':'(ANNCV = '''+
                                      %CHAR(ECR_ANNCV)+''') +
                                      AND (NCV >= 990000)');
             IF (ECR_NCV = 1);
                ECR_NCV += 990000;
             ENDIF;
             IF (*IN50);
                WNoFunction = 'RPRDE11406';
                DS#HEADER = $getHeader(S#PGM:WNoFunction);
                DS#FOOTER = $getFooter(S#PGM:WNoFunction:WKeysToHide);
                ECR_S#PGM = S#PGM;
                WRITE     RPRDE11400;
                WRITE     RPRDE11499;
                EXFMT RPRDE11406;
             ELSE;
                IF (WIsMae = *OFF);
                   WNoFunction = 'RPRDE11407';
                   DS#HEADER = $getHeader(S#PGM:WNoFunction);
                   DS#FOOTER = $getFooter(S#PGM:WNoFunction:WKeysToHide);
                   ECR_S#PGM = S#PGM;
                   WRITE     RPRDE11400;
                   WRITE     RPRDE11499;
                   WRITE     RPRDE11406;
                   EXFMT RPRDE11407;
                ELSE;
                   WNoFunction = 'RPRDE11409';
                   EXSR FillRecords;
                   DS#HEADER = $getHeader(S#PGM:WNoFunction);
                   DS#FOOTER = $getFooter(S#PGM:WNoFunction:WKeysToHide);
                   ECR_S#PGM = S#PGM;
                   WRITE     RPRDE11400;
                   WRITE     RPRDE11499;
                   WRITE     RPRDE11406;
                   EXFMT RPRDE11409;
                ENDIF;
             ENDIF;

             IF ($ValidKey(Func#Key:KEY#F04) AND *IN50);
                WRow = ECR_CM#CROW;
                WCol = ECR_CM#CCOL;
                EXSR RunPopUp;
             ELSEIF (Func#Key=KEY#F12) AND (NOT *IN50);
                *IN50 = *ON;
                *IN12 = *OFF;
             ELSEIF ($ValidKey(Func#Key:KEY#F21));
                EXSR Gravar;
                LEAVE;
             ELSEIF (Func#Key=KEY#Enter);
                IF (*IN50);
                   EXSR Validar;
                   IF ($GetScStatus() = 'O');
                      IF (WIsMae = *OFF);
                         EXSR LoadParms;
                      ELSE;
                      ENDIF;
                      *IN50 = *OFF;
                   ENDIF;
                ELSEIF ($ShowDialog('PRD0167':*BLANKS:'NS') = 'S');
                   LEAVE;
                ENDIF;
             ENDIF;
          ENDDO;

          $RmvScreen(WSessScr);
       ENDSR;

       //==================================================================
       // Subrotina.: FillRecords
       // Objectivo.: Actualizar lista com registo das filhas
       //==================================================================
       BEGSR FillRecords;
          REC#      = *ZEROS;   // Controlo de Registos da Lista
          *In53     = *On;      // Limpar Lista de Ecrã
          *In52     = *On;      // Mostrar a Lista de Ecrã
          WRITE RPRDE11403;     // Limpar Lista de Funções
          //*In53     = *Off;     // Repor Indicador
          //*In52     = *Off;     // Mostrar a Lista de Ecrã

          //%SUBST(ECR_HDR#LST:1)  = 'Op.Apólice';
          //%SUBST(ECR_HDR#LST:11) = 'Nome';
          //%SUBST(ECR_HDR#LST:50) = 'Matricula';

          //SETLL (50:ECR_NUAPOL) GBASEDIA;
          //DOW (NOT $IsEOF());
             //------------------------------------------------
             // Escreve os Registo da Listagem
             //------------------------------------------------
          // ECR_OPTION   = *BLANKS;
          // ECR_DET#LST  = $GetLstLine();
          // ECR_APFILH   = $GetFieldNum('NCV');
          // REC# += 1;
          // WRITE RPRDE11402;
          // *IN52 = *ON;       // Encontrou registos
          // $getNext();
          //ENDDO;
       ENDSR;

       //==================================================================
       // Subrotina.: ShowDet
       // Objectivo.: Apresentar o Detalhe
       //==================================================================
       BEGSR ShowDet;
          EXSR LoadParms;
          WNoFunction = 'RPRDE11405';
          WKeysToHide = *ZEROS;
          DOW (NOT *IN03 AND NOT *IN12);
             WOptions  = *ZEROS;
             DS#HEADER = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER = $getFooter(S#PGM:WNoFunction:WKeysToHide);
             ECR_S#PGM = S#PGM;
             WRITE     RPRDE11400;
             WRITE     RPRDE11499;
             EXFMT RPRDE11405;
             IF (Func#Key=KEY#Enter);
                LEAVE;
             ENDIF;
          ENDDO;
          *IN12 = *OFF;
       ENDSR;

       //==================================================================
       // Subrotina.: LoadParms
       // Objectivo.: Carregar ShowDet da Apólice
       //==================================================================
       BEGSR LoadParms;
          IF (WIcInsert);
             CHAIN (ECR_CRAMO:ECR_NUAPOL) GBASEDIA;
             IF (NOT %FOUND(GBASEDIA));
                CHAIN (ECR_CRAMO:ECR_NUAPOL) GBASEGRA;
             ENDIF;
             ECR_NUMATR1  = BAS_MATR1;
             IF (BAS_CATV=1) OR (BAS_CATV=2) OR (BAS_CATV=6) OR (BAS_CATV=7);
                ECR_CATVIA   = 'A';
             ELSEIF (BAS_CATV = 3);
                ECR_CATVIA   = 'C';
             ELSEIF (BAS_CATV = 4);
                ECR_CATVIA   = 'B';
             ELSEIF (BAS_CATV = 5);
                ECR_CATVIA   = 'F';
             ENDIF;

             ECR_MARCA1   = BAS_MARC1;
             ECR_NUMATR2  = BAS_MATR2;
             ECR_MARCA2   = BAS_MARC2;
             ECR_ASTVIAG  = BAS_VIAG;
             ECR_NUMSEG   = BAS_NSEG;
             ECR_NUMED1   = BAS_MED1;
             ECR_NUMED2   = BAS_MED2;

             CHAIN (ECR_NUMSEG) GFSEG;
             ECR_NOMSEG   = SEG_NOM;
             ECR_MORSEG   = SEG_MORAD;
             ECR_CODPOS   = SEG_CPOST;
             ECR_LOCPOS   = SEG_DESCP;
          ELSE;
             CHAIN (ECR_NCV:ECR_ANNCV) RDOCP060;
             IF (%FOUND());
                ECR_NCV      = DOC_NCV;
                ECR_ANNCV    = DOC_ANNCV;
                ECR_CRAMO    = DOC_CRAMO;
                ECR_NUAPOL   = DOC_NUAPOL;
                ECR_DTINICIO = DOC_DTINICIO;
                ECR_DTTERMO  = DOC_DTTERMO;
                ECR_NUMATR1  = DOC_NUMATR1;
                ECR_CATVIA   = DOC_CATVIA;
                ECR_MARCA1   = DOC_MARCA1;
                ECR_NUMATR2  = DOC_NUMATR2;
                ECR_MARCA2   = DOC_MARCA2;
                ECR_NUMSEG   = DOC_NUMSEG;
                ECR_NOMSEG   = DOC_NOMSEG;
                ECR_MORSEG   = DOC_MORSEG;
                ECR_CODPOS   = DOC_CODPOS;
                ECR_LOCPOS   = DOC_LOCPOS;
                ECR_PAIMOR   = DOC_PAIMOR;
                ECR_NUMED1   = DOC_NUMED1;
                ECR_NUMED2   = DOC_NUMED2;
                ECR_ASTVIAG  = DOC_ASTVIAG;
             ENDIF;
          ENDIF;
       ENDSR;

       //============================================================
       // Subrotina..: RunPopUp
       // Objectivo..: Apresentar lista de valores possíveis para o
       //              campo.
       //============================================================
       BEGSR RunPopUp;
          MONITOR;
             IF (ECR_CM#NMFLD = 'CPRISC');
             // ECR_TPRISC = $runPopUp(75:ECR_TPRISC);
             // ECR_DSRIS = $GetDescricao(75:ECR_TPRISC);
             ENDIF;
          ON-ERROR;
          ENDMON;
          ECR_CM#ROW = WRow;
          ECR_CM#COL = WCol;
       ENDSR;

       //==================================================================
       // Subrotina.: Gravar
       // Objectivo.: Gravar os ShowDet da Apólice
       //==================================================================
       BEGSR Gravar;
          DOC_NCV      = $GetNextKey('FDOCP060':'NCV':'(ANNCV = '''+
                                         %CHAR(ECR_ANNCV)+''') +
                                      AND (NCV >= 990000)');
          IF (DOC_NCV = 1);
             DOC_NCV += 990000;
          ENDIF;
          DOC_CRAMO    = 50;
          DOC_ANNCV    = ECR_ANNCV;
          DOC_NUAPOL   = ECR_NUAPOL;
          DOC_DTINICIO = ECR_DTINICIO;
          DOC_DTTERMO  = ECR_DTTERMO;
          DOC_NUMATR1  = ECR_NUMATR1;
          DOC_CATVIA   = ECR_CATVIA;
          DOC_MARCA1   = ECR_MARCA1;
          DOC_NUMATR2  = ECR_NUMATR2;
          DOC_MARCA2   = ECR_MARCA2;
          DOC_NUMSEG   = ECR_NUMSEG;
          DOC_NOMSEG   = ECR_NOMSEG;
          DOC_MORSEG   = ECR_MORSEG;
          DOC_CODPOS   = ECR_CODPOS;
          DOC_LOCPOS   = ECR_LOCPOS;
          DOC_PAIMOR   = 'Portugal';
          DOC_NUMED1   = ECR_NUMED1;
          DOC_NUMED2   = ECR_NUMED2;
          DOC_ASTVIAG  = ECR_ASTVIAG;
          WRITE RDOCP060;
       ENDSR;

       //==================================================================
       // Subrotina.: Validar
       // Objectivo.: Validar os registos inseridos
       //==================================================================
       BEGSR Validar;
          $InzScStatus();                      // Inicalizar campos em controlo

          //-----------------------------------------------
          // Validar a Apólice
          //-----------------------------------------------
          IF (ECR_NUAPOL = *ZEROS);
             $SetScError('NUAPOL':'PRD0101');
          ELSE;
             WMsgErro = *BLANKS;
             CHAIN (50:ECR_NUAPOL) GBASEDIA;
             IF (NOT %FOUND(GBASEDIA));
                CHAIN (50:ECR_NUAPOL) GBASEGRA;
                IF (NOT %FOUND(GBASEGRA));
                   WMsgErro = 'PRD0161';
                ENDIF;
             ENDIF;

             WIsFrota = *OFF;
             WIsMae   = *OFF;
             IF (WMsgErro <> *BLANKS);
                $SetScError('NUAPOL':WMsgErro);
             ELSE;
                IF (BAS_SIT = 3);
                   $SetScError('NUAPOL':'PRD0122');
                ELSEIF (BAS_FROTA = 'F');
                   WIsFrota = *ON;
                   IF (BAS_APMAE = *ZEROS);
                      WIsMae   = *ON;
                   ELSE;
                      WIsMae   = *OFF;
                   ENDIF;
                ENDIF;
             ENDIF;
          ENDIF;

          //-----------------------------------------------
          // Validar o ano da Carta Verde
          //-----------------------------------------------
          IF (ECR_ANNCV = *ZEROS);
             $SetScError('ANNCV':'PRD0169');
          ELSEIF (ECR_ANNCV < *YEAR);
             $SetScError('ANNCV':'PRD0168');
          ELSEIF (ECR_ANNCV > (*YEAR+1));
             $SetScError('ANNCV':'PRD0170');
          ENDIF;

          //-----------------------------------------------
          // Validar as data de inicio e termo
          //-----------------------------------------------
          IF (ECR_DTINICIO = *ZEROS);
             $SetScError('DTINICIO':'PRD0171');
          ELSE;
             TEST(DE) *ISO ECR_DTINICIO;
             IF (%ERROR());
                $SetScError('DTINICIO':'PRD0172');
             ELSEIF (ECR_ANNCV <> %DEC(%SUBST(%CHAR(ECR_DTINICIO):1:4):4:0));
                $SetScError('DTINICIO':'PRD0173');
             ENDIF;
          ENDIF;

          IF (ECR_DTTERMO = *ZEROS);
             $SetScError('DTTERMO':'PRD0174');
          ELSEIF (ECR_DTTERMO <= ECR_DTINICIO);
             $SetScError('DTTERMO':'PRD0175');
          ELSE;
             TEST(DE) *ISO ECR_DTTERMO;
             IF (%ERROR());
                $SetScError('DTTERMO':'PRD0176');
             ELSEIF (ECR_DTTERMO < $DateToInt(%DATE()));
                $SetScError('DTTERMO':'PRD0177');
             ENDIF;
          ENDIF;
       ENDSR;
      /END-FREE
