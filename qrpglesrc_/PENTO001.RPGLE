     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PCATO001
     H* Descrição.: Manutenção de Produtos
     H*==================================================================
     FSD1CP     IF   E           K DISK    PREFIX(CP_)
     FSD1PE     UF A E           K DISK    PREFIX(SD_)
     FGFSEG3    IF A E           K DISK    RENAME(FSEGF1:FSEGF3)
     FGFSEG     UF A E           K DISK    PREFIX(ENT_)
     FGFSEGEXT  UF A E           K DISK    PREFIX(ENT_)
     FFSECP060  IF   E           K DISK    PREFIX(SEC_)
     FFENTE001  CF   E             WORKSTN SFILE(RENTE00102:REC#)
     F                                     PREFIX(ECR_)
     F                                     INFDS(INFD#KEYS)
     F*==========================================================
     F* Indicadores  Descrição
     F* -----------  --------------------------------------------
     F*      45      Condicionar Campos com Input/Output
     F*      52      DSP
     F*      53      CLR
     F*      54      END
     D*==========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SSECO010

     D*==> Estrutura da Informação de Segurados
     D MntEntidade     PR
     D   PCodEntidade                 9S 0 CONST
     D   PNomEntidade                40A   CONST
     D
     D*==> Estrutura de dados do Registo
     D REC#            S              4S 0
     D
     C*========================================================
      /FREE

       //------------------------------------
       // Se não estiver autorizado então sai
       //------------------------------------
       IF NOT $ValAccess(S#PGM);
          *INLR = *ON;
          RETURN;
       ENDIF;

       MntEntidade(*ZEROS:*BLANKS);

       *INLR = *ON;
       RETURN;

      /END-FREE
     P*===================================================================
     P* Processo..: MntEntidades
     P* Descrição.: Realizar a manutenção de Entidades
     P*===================================================================
     P MntEntidade     B                   EXPORT
     D MntEntidade     PI
     D   PCodEntidade                 9S 0 CONST
     D   PNomEntidade                40A   CONST
     D
     D WFldErro        S             15A
     D WIcOrigem       S              1A
     D WIcDisplay      S               N   INZ(*ON)
     D WModo           S              1S 0 INZ(*ZEROS)
     D WNoFunction     S             10A   INZ(*BLANKS)
     D WSelected       S               N
     D WSessionID      S              5S 0
     D
     D IO_Consultar    C                   0
     D IO_Criar        C                   1
     D IO_Alterar      C                   2
     D IO_Eliminar     C                   4
     D*===================================================================
      /FREE
       WSessionID = $AddDBSession();

       IF (PCodEntidade = *ZEROS);
          $SETFILES('GFSEG A LEFT JOIN GFSEGEXT B ON (A.NSEG = B.NSEG)');
          $SETFIELDS('A.NSEG, APELID, NOM, DESCP, NIF');
          $SetPage(12);
          $addCriteria('Nome.......:':'NOM':'GE':*ON);
          $addCriteria('NIF........:':'NIF':'GE');
          $addCriteria('Número.....:':'A.NSEG':'GE');
          $addCriteria('Apelido....:':'APELID':'GE');
          ECR_DSCRITER = $getCritText();
          $SetLstFld('A.NSEG':'Número':9:'D');
          $SetLstFld('NOM':'Nome':40:'E');
          $SetLstFld('NIF':'NIF':9:'D');
          $SetLstFld('APELID':'Apelido':14:'E');
       ELSE;
          $SETFILES('GFSEG A LEFT JOIN GFSEGEXT B ON (A.NSEG = B.NSEG)');
          $SETFIELDS('A.NSEG, APELID, NOM, DESCP, NIF');
          $SetPage(12);
          $addCriteria('Nome.......:':'NOM':'GE':*ON);
          $addCriteria('NIF........:':'NIF':'GE');
          $addCriteria('Número.....:':'A.NSEG':'GE');
          $addCriteria('Apelido....:':'APELID':'GE');
          ECR_DSCRITER = $getCritText();
          $SetLstFld('A.NSEG':'Número':9:'D');
          $SetLstFld('NOM':'Nome':40:'E');
          $SetLstFld('NIF':'NIF':9:'D');
          $SetLstFld('APELID':'Apelido':14:'E');
       ENDIF;
       WIcDisplay =*ON;
       EXSR inzRecords;
       EXSR getRecords;

       DOW (NOT *IN03 AND NOT *IN12);
          ECR_ERR#MSG = *BLANKS;
          IF (PCodEntidade = *ZEROS);
             WNoFunction = 'RENTE00103'; // Listar Componentes
          ELSE;
             WNoFunction = 'RENTE00103'; // Listar Moradas
          ENDIF;
          DS#HEADER    = $getHeader(S#PGM:WNoFunction);
          DS#FOOTER    = $getFooter(S#PGM:WNoFunction:*BLANKS);
          DS#OPTIONS   = $getOption(S#PGM:WNoFunction:*BLANKS);
          ECR_S#PGM    = S#PGM;
          WRITE     RENTE00100;
          WRITE     RENTE00199;
          EXFMT RENTE00103;
          IF (Func#Key = KEY#F05);
             $ChgCriteria();
             ECR_DSCRITER = $getCritText();
             EXSR inzRecords;
             EXSR getRecords;
          ELSEIF ($ValidKey(Func#Key:KEY#F06));
             WModo = IO_Criar;
             EXSR DspEntidade;
             $SetDBSession(WsessionID);
             EXSR inzRecords;
             EXSR getRecords;
          ELSEIF ((Func#Key = KEY#PgDown) AND (NOT $IsLastPage())) OR
             ((Func#Key = KEY#PgUp) AND ($GetPageNbr() > 1));
             EXSR inzRecords;
             EXSR getRecords;
          ELSEIF (Func#Key = KEY#Enter);
             WModo = IO_Consultar;
             EXSR SelRec;
             *IN12 = *OFF;
             EXSR inzRecords;
             EXSR getRecords;
          ENDIF;
       ENDDO;
       $RmvDBSession(WSessionID);

       //========================================================
       // Subrotina.: InzRecTxt
       // Objectivo.: Inicializar a lista de registos
       //========================================================
       BEGSR inzRecords;
          REC#      = *ZEROS;   // Controlo de Registos da Lista
          *In53     = *On;      // Limpar Lista de Ecrã
          *In52     = *On;      // Mostrar a Lista de Ecrã
          WRITE RENTE00103;     // Limpar Lista
          *In53     = *Off;     // Repor Indicador
       ENDSR;

       //========================================================
       // Subrotina.: GetRecords
       // Objectivo.: Obter todos os registos de tabelas disponív
       //========================================================
       BEGSR getRecords;
          IF ($GetPage(Func#Key) > *ZEROS);
             DOW (NOT $IsEOF());

                //------------------------------------------------
                // Escreve os Registo da Listagem
                //------------------------------------------------
                ECR_OPTION   = *BLANKS;
                ECR_LSTHDR = $GetLstHdr();
                ECR_LSTDET = $GetLstLine();
                MONITOR;
                   ECR_CDSEG  = %DEC($GetField('A.NSEG'):5:0);
                   ECR_NIF    = %DEC($GetField('NIF'):5:0);
                ON-ERROR;
                ENDMON;

                REC# += 1;
                WRITE RENTE00102;
                $getNext();
             ENDDO;
          ENDIF;

          //---------------------------------------
          // Validar se ainda existem mais registos
          //---------------------------------------
          IF (REC# = *ZEROS);
             *IN52 = *OFF;
          ELSE;
             *IN54 = $IsLastPage();
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: SelRec
       // Objectivo.: Seleccionar os registos
       //========================================================
       BEGSR SelRec;
       IF (REC# = *ZEROS);
          LEAVESR;
       ENDIF;
          WSelected = *OFF;
          READC RENTE00102;
          DOW (NOT %EOF());
             IF ($ValidOption(ECR_OPTION:'5'));
                WSelected = *ON;
                EXSR DspEntidade;
             ELSEIF ($ValidOption(ECR_OPTION:'7'));
                WSelected = *ON;
                // EXSR DspMoradas;
             ENDIF;
             READC RENTE00102;
          ENDDO;
          IF (NOT WSelected);
             IF (%TRIM(ECR_CRITERIA) <> *BLANKS);
                $SetCriteria(ECR_CRITERIA:*ON);
             ELSE;
                $SetCriteria(*BLANKS);
             ENDIF;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: DspEntidade
       // Objectivo.: Apresentar os dados da Entidade
       //========================================================
       BEGSR DspEntidade;
          IF (WModo = IO_Criar);
             ECR_TIPO = 'I';
             WFldErro = 'Dummy';
             DOW (WFldErro <> *BLANKS);
                EXFMT RENTE00107;
                IF (Func#Key=KEY#F12);
                   *IN12 = *OFF;
                   LEAVESR;
                ENDIF;
                IF (ECR_TIPO <> 'I') AND (ECR_TIPO <> 'S');
                   ECR_ERR#MSG = $GetMessage('ENT0001');
                   WFldErro = 'TIPO';
                ELSE;
                   WFldErro    = *BLANKS;
                   ECR_ERR#MSG = *BLANKS;
                ENDIF;
             ENDDO;

             WIcDisplay    = *OFF;
             ECR_CDSEG     = *ZEROS;
             ECR_NOM       = *BLANKS;
             ECR_APELID    = *BLANKS;
             ECR_NIF       = *ZEROS;
             ECR_BI        = *ZEROS;
             ECR_CDBI      = *ZEROS;
             ECR_DTNASC    = *ZEROS;
             ECR_MORAD     = *BLANKS;
             ECR_CPOST     = *ZEROS;
             ECR_DESCP     = *BLANKS;
             ECR_TELEF     = *BLANKS;
             ECR_NFAX      = *BLANKS;
             ECR_TLM       = *ZEROS;
             ECR_WEBMAIL   = *BLANKS;
             ECR_GEST      = *BLANKS;
             ECR_DSPROFIS  = *BLANKS;
             ECR_CODEMP    = *BLANKS;
             ECR_TECN      = *BLANKS;
             ECR_CONTEN    = 'N';
             ECR_CORSEL    = 'N';
             ECR_ICDVEN    = 'N';
             ECR_PILOTO    = *BLANKS;
             ECR_STESP     = 'N';
             ECR_USERID    = *BLANKS;
             ECR_DTALT     = *ZEROS;
             ECR_TIMALT    = *ZEROS;
          ELSE;
             CHAIN(N) (ECR_CDSEG) GFSEG;
             ECR_CDSEG     = ENT_NSEG;
             ECR_NOM       = ENT_NOM;
             ECR_APELID    = ENT_APELID;
             ECR_NIF       = ENT_NIF;
             ECR_BI        = ENT_BI;
             IF (ENT_CDBI = *BLANKS);
                ECR_CDBI      = *ZEROS;
             ELSE;
                ECR_CDBI      = %DEC(ENT_CDBI:1:0);
             ENDIF;
             ECR_DTNASC    = ENT_DTNASC;
             ECR_MORAD     = ENT_MORAD;
             ECR_CPOST     = ENT_CPOST;
             ECR_DESCP     = ENT_DESCP;
             ECR_TELEF     = ENT_TELEF;
             ECR_NFAX      = ENT_NFAX;
             ECR_TLM       = ENT_TLM;
             ECR_TIPO      = ENT_TIPO;
             ECR_GEST      = ENT_GEST;
             ECR_TECN      = ENT_TECN;
             ECR_CONTEN    = ENT_CONTEN;
             ECR_CORSEL    = ENT_CORSEL;
             ECR_PILOTO    = ENT_PILOTO;
             ECR_STESP     = ENT_STESP;
             ECR_USERID    = ENT_USERID;
             ECR_DTALT     = ENT_DATALT;
             ECR_TIMALT    = ENT_TIMALT;
             CHAIN(N) (ECR_CDSEG) GFSEGEXT;
             IF (%FOUND());
                ECR_CODEMP  = ENT_CODEMP;
                ECR_ICDVEN  = ENT_ICDVEN;
                ECR_WEBMAIL = ENT_EMAIL01;
             ELSE;
                ECR_CODEMP  = *ZEROS;
                ECR_ICDVEN  = *BLANKS;
                ECR_WEBMAIL = *BLANKS;
             ENDIF;
          ENDIF;

          DOW (NOT *IN03) AND (NOT *IN12);
             IF (WIcDisplay);
                WNoFunction = 'RENTE00105'; // Listar
                *IN45 = *OFF;
             ELSE;
                *IN45 = *ON;
                IF (WModo = IO_CRIAR);
                   WNoFunction = 'RENTE00106'; // Criar
                ELSE;
                   WNoFunction = 'RENTE00121'; // Alterar
                ENDIF;
             ENDIF;
             DS#HEADER    = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER    = $getFooter(S#PGM:WNoFunction:*BLANKS);
             ECR_S#PGM    = S#PGM;
             WRITE     RENTE00100;
             WRITE     RENTE00199;
             EXFMT RENTE00105;
             IF (($ValidKey(Func#Key:KEY#F21)) AND (WIcDisplay));
                WIcDisplay = *OFF;
             ELSEIF ($ValidKey(Func#Key:KEY#F22)) AND (WIcDisplay);
                ECR_OPTION = 'N';
                EXFMT RENTE00122;
                IF (Func#Key = KEY#Enter) AND (ECR_OPTION = 'S');
                   // DELETE (ECR_CDPROD) RCATP100;
                   LEAVESR;
                ENDIF;
                *IN12 = *OFF;
             ELSEIF (Func#Key = KEY#F12) AND (NOT WIcDisplay)
                AND (WModo <> IO_Criar);
                WIcDisplay = *ON;
                *IN12 = *OFF;
             ELSEIF (Func#Key = KEY#F04) AND (NOT WIcDisplay);
                EXSR RunPopUp;
             ELSEIF (Func#Key = KEY#Enter);
                IF (NOT WIcDisplay);
                   EXSR ValidateFlds;
                   IF (WFldErro <> *BLANKS);
                      ITER;
                   ENDIF;
                   EXSR ChangeEntid;
                ENDIF;
                $SetDBSession(WsessionID);
                WIcDisplay = *ON;
                LEAVESR;
             ENDIF;
          ENDDO;
          *IN12 = *OFF;
          WIcDisplay = *ON;
       ENDSR;

       //========================================================
       // Subrotina.: ValidateFlds
       // Objectivo.: Validar os Campos
       //========================================================
       BEGSR ValidateFlds;
          ECR_ERR#MSG = *BLANKS;
          WFldErro    = *BLANKS;

          //-------------------------------------------------------
          //  Validar Campos Preenchidos
          //-------------------------------------------------------
          IF (ECR_NOM = *BLANKS);
             ECR_ERR#MSG = $GetMessage('ENT0001');
             WFldErro    = 'NOM';
          ELSEIF (ECR_MORAD = *BLANKS);
             ECR_ERR#MSG = $GetMessage('ENT0004');
             WFldErro    = 'MORAD';
          ELSEIF (ECR_TELEF = *ZEROS AND ECR_TLM = *ZEROS);
             ECR_ERR#MSG = $GetMessage('ENT0006');
             WFldErro    = 'TELEF';
          ELSEIF (ECR_STESP <>'S' AND ECR_STESP<>'N');
             ECR_ERR#MSG = $GetMessage('ENT0010');
             WFldErro    = 'STESP';
          ENDIF;

          //-------------------------------------------------------
          //  Informação especifica da Corsel
          //-------------------------------------------------------
          IF (WFldErro = *BLANKS);
             IF (ECR_CORSEL<>'S' AND ECR_CORSEL<>'N');
                ECR_ERR#MSG = $GetMessage('ENT0010');
                WFldErro = ECR_CORSEL;
             ELSEIF (ECR_CORSEL='S' AND $valCodigo(60:ECR_PILOTO));
                ECR_ERR#MSG = $GetMessage('ENT0008');
                WFldErro = ECR_CORSEL;
             ELSEIF (ECR_CORSEL='N' AND ECR_PILOTO <> *BLANKS);
                ECR_ERR#MSG = $GetMessage('ENT0009');
                WFldErro = ECR_CORSEL;
             ENDIF;
          ENDIF;

          //-------------------------------------------------------
          //  Informação especifica dos descontos em Vencimento
          //-------------------------------------------------------
          IF (WFldErro = *BLANKS);
             IF (ECR_ICDVEN <>'S' AND ECR_ICDVEN<>'N');
                ECR_ERR#MSG = $GetMessage('ENT0010');
                WFldErro    = 'ICDVEN';
             ELSEIF (ECR_ICDVEN = 'S' AND ECR_CODEMP = *BLANKS);
                ECR_ERR#MSG = $GetMessage('ENT0007');
                WFldErro    = 'ICDVEN';
             ELSEIF (ECR_ICDVEN = 'N' AND ECR_CODEMP <> *BLANKS);
                ECR_ERR#MSG = $GetMessage('ENT0013');
                WFldErro    = 'ICDVEN';
             ENDIF;
          ENDIF;

          //-------------------------------------------------------
          // Validar se o Código Postal
          //-------------------------------------------------------
          IF (WFldErro = *BLANKS);
             IF (ECR_CPOST = *ZEROS);
                ECR_ERR#MSG = $GetMessage('ENT0005');
                WFldErro    = 'CPOST';
             ELSE;
                CHAIN (ECR_CPOST) SD1CP;
                IF (NOT %FOUND());
                   ECR_ERR#MSG = $GetMessage('ENT0012');
                WFldErro    = 'CPOST';
                ENDIF;
            ENDIF;
          ENDIF;

          //-------------------------------------------------------
          //  Validar estrutura do NIF
          //-------------------------------------------------------
          IF (WFldErro = *BLANKS);
             IF (ECR_NIF = *ZEROS); // Campo preenchido com valores ?
                ECR_ERR#MSG = $GetMessage('ENT0002');
                WFldErro    = 'NIF';
             ELSEIF (ECR_CDSEG = *ZEROS); // Já existe outro igual
                CHAIN (ECR_NIF) FSEGF3;
                IF (%FOUND());
                   ECR_ERR#MSG = $GetMessage('ENT0015');
                   WFldErro    = 'NIF';
                ENDIF;
             ELSE; // A estrutura do número é válida
              //IF (Erro);
              //   ECR_ERR#MSG = $GetMessage('ENT0003');
              //   WFldErro    = 'NIF';
              //ELSE; // O número está na lista negra
                   CHAIN (ECR_NIF) RSECP060;
                   IF (%FOUND);
                      ECR_ERR#MSG = $GetMessage('ENT0014');
                      WFldErro    = 'NIF';
                   ENDIF;
              //ENDIF;
             ENDIF;
          ENDIF;

          //-------------------------------------------------------
          //  Validar estrutura do BI
          //-------------------------------------------------------
          IF (WFldErro = *BLANKS);
             WFldErro    = 'BI';
          ENDIF;

          //-------------------------------------------------------
          //  Validar estrutura da Data Nascimento
          //-------------------------------------------------------
          IF (WFldErro = *BLANKS) AND (ECR_DTNASC > 9999);
             TEST(DE) *ISO ECR_DTNASC;
             IF (%ERROR);
                ECR_ERR#MSG = $GetMessage('ENT0011');
                WFldErro    = 'DTNASC';
             ENDIF;
          ENDIF;

          IF (WFldErro <> *BLANKS);
             $getFldLoc('FENTE001':'RENTE00105':WFldErro:
                         ECR_CM#ROW:ECR_CM#COL);
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: ChangeEntid
       // Objectivo.: Alterar o ficheiro de Entidades
       //========================================================
       BEGSR ChangeEntid;
       ENDSR;

       //==================================================================
       // Subrotina.: RunPopUp
       // Objectivo.: Adicionar um novo registo de uma Apólice
       //==================================================================
+----  BEGSR RunPopUp;
|+---     IF (ECR_CM#NMFLD = 'PILOTO');
             ECR_PILOTO = $runPopUp(60:ECR_PILOTO);
             ECR_DSPROFIS = $getDescricao(60:ECR_PILOTO);
          ENDIF;
|         ECR_CM#ROW = CM#CROW;
|         ECR_CM#COL = CM#CCOL;
+----  ENDSR;
      /END-FREE
     P MntEntidade     E
