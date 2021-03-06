     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PPRDO112
     H* Descri��o.: Manuten��o de Intru��es de D�bito em Conta
     H*------------------------------------------------------------------
     H* A1. Incluir o n�mer SegurNet, e se n�o existir ent�o n�o
     H*     cria a carta verde.
     H*==================================================================
     FAPLFLH    IF   E           K DISK    PREFIX(FLH_) RENAME(BASEGRFA:APFILHA)
     F*APLFLHD   IF   E           K DISK    PREFIX(FLH_) RENAME(BASEDIFA:APFILHD)
     FGBASEGRA  IF   E           K DISK    PREFIX(BAS_)
     FGBASEDIA  IF   E           K DISK    PREFIX(BAS_)
     FGFSEG     IF   E           K DISK    PREFIX(SEG_)
     FFDOCP060  UF A E           K DISK    PREFIX(DOC_)
     FFPRDE114  CF   E             WORKSTN SFILE(RPRDE11402:REC#)
     F                                     SFILE(RPRDE11408:REC#)
     F                                     PREFIX(ECR_)
     F                                     INFDS(INFD#KEYS)
     F*==========================================================
     F* Indicadores  Descri��o
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

     D*==> Par�metros de Entrada
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
     D WNumCVerde      S              9S 0
     D WFstCVerde      S              9S 0
     D WLstCVerde      S              9S 0
     D WIcBase         S               N
     D WIdx            S              4S 0
     D WREC#           S              4S 0
     D
     D KTE#CV          S              5S 0
     D KTE#FR          S              5S 0
     C*========================================================
      /FREE

       //------------------------------------
       // Validar o acesso a esta fun��o
       //------------------------------------
       IF NOT $ValAccess(S#PGM:'':99999);
          $ShowDialog('SEC0001');
          *INLR = *ON;
          RETURN;
       ENDIF;

       //--------------------------------------------------
       // Transferir controlo do Ecr� para a API
       //--------------------------------------------------

       //----------------------------------------------
       // Registos de Cartas Verde
       //----------------------------------------------
       KTE#CV = $SETFILES('FDOCP060');
       $addCriteria('Ap�lice....:':'NUAPOL':'GE':'D');
       $addCriteria('Carta Verde:':'NCV':'GE');
       $addCriteria('Segurado...:':'NUMSEG':'GE');
       $addCriteria('Nome.......:':'NOMSEG':'LK');
       $addCriteria('Matricula..:':'NUMATR1':'LK');
       $SetLstFld('ANNCV':'Ano':4:'D');
       $SetLstFld('NCV':'N�mero':9:'D');
       $SetLstFld('NUAPOL':'Ap�lice':7:'D');
       $SetLstFld('NOMSEG':'Nome':40:'E');
       $SetLstFld('NUMATR1':'Matricula':15:'E');
       $SetPage(12);

       //----------------------------------------------
       // Registos de Ap�lices Frota
       //----------------------------------------------
       KTE#FR = $SETFILES('GBASEGRA');
       $addCriteria('Ap�lice....:':'NUAPOL':'GE':'D');
       $addCriteria('Matricula..:':'MATR1':'LK');
       $SetLstFld('NUAPOL':'Ap�lice':7:'D');
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
       // Objectivo.: Obter todos os registos de tabelas dispon�v
       //========================================================
       BEGSR getRecords;
          REC#      = *ZEROS;   // Controlo de Registos da Lista
          *In53     = *On;      // Limpar Lista de Ecr�
          *In52     = *On;      // Mostrar a Lista de Ecr�
          WRITE RPRDE11403;     // Limpar Lista de Fun��es
          *In53     = *Off;     // Repor Indicador
          *In52     = *Off;     // Mostrar a Lista de Ecr�
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
          *IN54 = $IsLastPage();   // Indica��o de Fim de P�gina
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
          $AddScPFld(%ADDR(ECR_F#01):'NUAPOL');  // N�mero da Ap�lice
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
                   EXSR FillRecords;
                   DOW (NOT *IN03 AND NOT *IN12);
                      WNoFunction = 'RPRDE11409';
                      DS#HEADER = $getHeader(S#PGM:WNoFunction);
                      DS#FOOTER = $getFooter(S#PGM:WNoFunction:WKeysToHide);
                      ECR_S#PGM = S#PGM;
                      WRITE     RPRDE11400;
                      WRITE     RPRDE11499;
                      WRITE     RPRDE11406;
                      EXFMT RPRDE11409;
                      IF ($ValidKey(Func#Key:KEY#F21));
                         EXSR Gravar;
                         LEAVESR;
                      ENDIF;
                   ENDDO;
                   *IN12 = *OFF;
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
          *In53     = *On;      // Limpar Lista de Ecr�
          *In52     = *On;      // Mostrar a Lista de Ecr�
          WRITE RPRDE11409;     // Limpar Lista de Fun��es
          *In53     = *Off;     // Repor Indicador
          *In52     = *Off;     // Mostrar a Lista de Ecr�

          %SUBST(ECR_HDR#LST:1)  = 'Op.Ramo Ap�lice';
          %SUBST(ECR_HDR#LST:17) = 'Nome';
          %SUBST(ECR_HDR#LST:62) = 'Matricula';

          WIcBase = *ON;
        //CHAIN (50:ECR_NUAPOL) APFILHD;
        //IF (%FOUND());
        //   WIcBase = *OFF;
        //ENDIF;

          IF (WIcBase);
             SETLL (50:ECR_NUAPOL) APFILHA;
             READE (50:ECR_NUAPOL) APFILHA;
          ELSE;
         //  SETLL (50:ECR_NUAPOL) APFILHD;
         //  READE (50:ECR_NUAPOL) APFILHD;
          ENDIF;
          DOW (NOT %EOF());
             //------------------------------------------------
             // Escreve os Registo da Listagem
             //------------------------------------------------
             IF (FLH_SIT = 2) OR (NOT WIcBase AND (FLH_SIT = 1));
                ECR_OPTFROTA = 'X';
                %SUBST(ECR_DET#LST:1:2)   = %TRIM(%EDITW(FLH_RAMO:'0  '));
                %SUBST(ECR_DET#LST:4:8)   = %TRIM(%EDITW(FLH_APOL:'0       '));
                %SUBST(ECR_DET#LST:13:40) = FLH_SEG;
                %SUBST(ECR_DET#LST:62:15) = FLH_MATR1;
                ECR_APFILH                = FLH_APOL;
                REC# += 1;
                WRITE RPRDE11408;
                *IN52 = *ON;       // Encontrou registos
             ENDIF;
             IF (WIcBase);
                READE (50:ECR_NUAPOL) APFILHA;
             ELSE;
             // READE (50:ECR_NUAPOL) APFILHD;
             ENDIF;
          ENDDO;
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
       // Objectivo.: Carregar ShowDet da Ap�lice
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
       // Objectivo..: Apresentar lista de valores poss�veis para o
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
       // Objectivo.: Gravar os ShowDet da Ap�lice
       //==================================================================
       BEGSR Gravar;
          WNumCVerde = $GetNextKey('FDOCP060':'NCV':'(ANNCV = '''+
                              %CHAR(ECR_ANNCV)+''') AND (NCV >= 990000)');
          IF (WNumCVerde = 1);
             WNumCVerde += 990000;
          ENDIF;
          WFstCVerde = WNumCVerde;
          IF (WIsMae = *OFF);
             DOC_NCV      = WNumCVerde;
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
          ELSE;
             WREC# = REC#;
             WIdx  = 1;
             DOW (WIdx <= WREC#);
                CHAIN WIdx RPRDE11408;
                IF (ECR_OPTFROTA <> *BLANKS);
                   IF (WIcBase);
                      CHAIN (50:ECR_APFILH) GBASEGRA;
                   ELSE;
                      CHAIN (50:ECR_APFILH) GBASEDIA;
                   ENDIF;
                   IF (%FOUND());
                      DOC_NCV      = WNumCVerde;
                      DOC_CRAMO    = 50;
                      DOC_ANNCV    = ECR_ANNCV;
                      DOC_NUAPOL   = BAS_APOL;
                      DOC_DTINICIO = ECR_DTINICIO;
                      DOC_DTTERMO  = ECR_DTTERMO;
                      DOC_NUMATR1  = BAS_MATR1;
                      DOC_MARCA1   = BAS_MARC1;
                      DOC_NUMATR2  = BAS_MATR2;
                      DOC_MARCA2   = BAS_MARC2;

                      CHAIN (BAS_NSEG) GFSEG;
                      DOC_NOMSEG   = SEG_NOM;
                      DOC_MORSEG   = SEG_MORAD;
                      DOC_CODPOS   = SEG_CPOST;
                      DOC_LOCPOS   = SEG_DESCP;
                      DOC_NUMSEG   = BAS_NSEG;
                      DOC_PAIMOR   = 'Portugal';
                      DOC_NUMED1   = ECR_NUMED1;
                      DOC_NUMED2   = ECR_NUMED2;
                      DOC_ASTVIAG  = BAS_VIAG;
                      IF (BAS_CATV=1) OR (BAS_CATV=2) OR (BAS_CATV=6) OR
                        (BAS_CATV=7);
                         DOC_CATVIA   = 'A';
                      ELSEIF (BAS_CATV = 3);
                         DOC_CATVIA   = 'C';
                      ELSEIF (BAS_CATV = 4);
                         DOC_CATVIA   = 'B';
                      ELSEIF (BAS_CATV = 5);
                         DOC_CATVIA   = 'F';
                      ENDIF;
                      WRITE RDOCP060;
                      WNumCVerde += 1;
                   ENDIF;
                ENDIF;
                WIdx += 1;
             ENDDO;
             WNumCVerde -= 1;
          ENDIF;
          WLstCVerde = WNumCverde;
          $ShowDialog('PRD0189':' de ('+%CHAR(WFstCVerde)+') a +
                                ('+%CHAR(WLstCVerde)+')');
       ENDSR;

       //==================================================================
       // Subrotina.: Validar
       // Objectivo.: Validar os registos inseridos
       //==================================================================
       BEGSR Validar;
          $InzScStatus();                      // Inicalizar campos em controlo

          //-----------------------------------------------
          // Validar a Ap�lice
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
          // $SetScError('ANNCV':'PRD0168');
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
