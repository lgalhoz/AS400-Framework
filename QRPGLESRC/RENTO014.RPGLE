     H NOMAIN
     H*=======================================================
     H* Dat.Cria...: 2013-10-17
     H* Processo...: runPopUp
     H* Objectivo..: Executar uma Pop-Up, para seleccionar
     H*              valores possíveis Tabela de Códigos.
     H*=======================================================
     FSENTE014  CF   E             WORKSTN SFILE(RENTE01402:REC#) USROPN
     F                                     INFDS(INFD#KEYS)
     F                                     PREFIX(ECR_)
     F*=======================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SSECO010
      /Copy QRPGLESRC,SENTO010

     D*==> Campos de Trabalho
     D  REC#           S              3S 0
     D*=======================================================

     P*=======================================================
     P* Processo..: $RunPSPopUp
     P* Descrição.: Apresentar uma laista de Pessoas Seguras
     P*             qure respeitem o critério.
     P*
     P* Tipo - Tipo de registos a extrair
     P*   A = Activos
     P*   E = Desactivos
     P*   T = Todos
     C*=======================================================
     P $RunPSPopUp     B                   EXPORT
     D                 PI             8S 0
     D  Codigo                        8S 0 CONST
     D  Ramo                          2S 0 Const Options(*NOPASS)
     D  Apolice                       7S 0 Const Options(*NOPASS)
     D  Tipo                          1A   Const Options(*NOPASS)
     D
     D*==> Campos de Trabalho
     D WCriteria       S            100A   INZ(*BLANKS)
     D WCodigo         S              8S 0 INZ(*ZEROS)
     D WRamo           S              2S 0 INZ(*ZEROS)
     D WApolice        S              7S 0 INZ(*ZEROS)
     D WTipo           S              1A   INZ(*BLANKS)
     D WIcSelected     S               N   INZ(*OFF)
     D WSessDBS        S              3S 0 INZ(*ZEROS)
     D*===========================================================
      /FREE
       //----------------------------------------
       // ForCompr parametros recebidos
       //----------------------------------------
       MONITOR;
          OPEN SENTE014;
       ON-ERROR;
       ENDMON;
       WRamo    = *ZEROS;
       WApolice = *ZEROS;
       WTipo    = 'T';
       WCodigo  = Codigo;
       IF (%PARMS() > 1);
          WRamo = Ramo;
          IF (%PARMS() > 2);
             WApolice = Apolice;
             IF (%PARMS() > 3);
                WTipo = Tipo;
             ENDIF;
          ENDIF;
       ENDIF;

       //------------------------------------
       // Inicializar as variáveis de Trabalho
       //------------------------------------
       WSessDBS = $SetFiles('OPLSEGF INNER JOIN OPLAPOF ON (OPNSEG = LANSEG)');
       IF (WRamo > *ZEROS);
          WCriteria = '(LARAMO='''+%CHAR(WRamo)+''') AND +
                        (LAAPOL='''+%CHAR(WApolice)+''')';
       ENDIF;
       $SetFields('OPNSEG,OPRAMO,OPAPOL,OPNOME,OPNIF,OPDTNA');
       IF (WTipo = 'A'); // Activos
          WCriteria = %TRIM(WCriteria) +
                      'AND ((LASIT = ''0'') OR (LASIT='' ''))';
       ELSEIF (WTipo = 'E'); // Anulados
          WCriteria = %TRIM(WCriteria) +
                      'AND ((LASIT <> ''0'') AND (LASIT<>'' ''))';
       ENDIF;
       $SetCriteria(WCriteria);
       $addCriteria('Código....:':'OPNSEG':'GE');
       $addCriteria('Nome......:':'OPNOME':'LK');
       $addCriteria('NIF.......:':'OPNIF':'GE');
       $SetLstFld('OPNSEG':'Código'  : 8:'D');
       $SetLstFld('OPNOME':'Nome'    :28:'E');
       $SetLstFld('OPNIF' :'NIF'     : 9:'D');
       $SetLstFld('OPDTNA':'Dt.Nasc.':10:'D':'L');
       ECR_LB#CRIT = $getCritText();
       ECR_DS#CRIT = %CHAR(Codigo);
       $SetPage(10);

       EXSR DspList;
       $RmvDBSession(WSessDBS);
       CLOSE SENTE014;
       RETURN WCodigo;

       //==============================================================
       // Subrotina.: dspList
       // Objectivo.: Mostrar Lista com os Códigos
       //==============================================================
       BEGSR dspList;
          //------------------------------------
          // Escrever Header Standard
          //------------------------------------
          DS#HEADER   = $getHeader(S#PGM:'RENTE01401');
          ECR_DS#PGMR = ECR_DS#PGM;
          ECR_DS#FNCR = ECR_DS#FNC;
          WRITE RENTE01400;
          DS#FOOTER     = $GetFooter(S#PGM:'RENTE01401':*BLANKS);
          ECR_DS#KEY01R = ECR_DS#KEY01;
          ECR_DS#KEY02R = ECR_DS#KEY02;
          WRITE RENTE01499;
          WRITE RENTE01498;
          EXSR getRecords; // Carregar Lista com Registos
          DOW (NOT *IN12) AND (NOT *IN03);
             EXFMT RENTE01401;
             IF ((Func#Key = KEY#PgDown) AND (NOT $IsLastPage())) OR
                ((Func#Key = KEY#PgUp) AND ($GetPageNbr() > 1));
                EXSR getRecords;
             ELSEIF (Func#Key = KEY#F05);
                $ChgCriteria();
                ECR_LB#CRIT = $getCritText();
                EXSR getRecords;
             ELSEIF (Func#Key = KEY#Enter);
                EXSR selRecords;
                IF (WIcSelected);
                   LEAVE;
                ENDIF;
                EXSR getRecords; // Carregar Lista com Registos
             ENDIF;
          ENDDO;
          *IN03 = *OFF;
          *IN12 = *OFF;
       ENDSR;

       //==============================================================
       // Subrotina.: GetRecords
       // Objectivo.: Obter Registos para apresentar
       //==============================================================
       BEGSR getRecords;
          REC#  = *ZEROS;   // Controlo de Registos da Lista de Ecrã
          *In53 = *On;      // Limpar Lista de Ecrã
          *In52 = *On;      // Mostrar a Lista de Ecrã
          WRITE RENTE01401; // Limpar Lista
          *In53 = *Off;     // Repor Indicador
          *In54 = *Off;     // Repor Indicador

          //----------------------------------------------
          //-  Invocar Processo para obter os registos
          //----------------------------------------------
          IF ($GetPage(Func#Key) > *ZEROS);
             DOW (NOT $IsEOF());
                REC# += 1;
                ECR_LS#HEAD = $GetLstHdr();
                ECR_LS#DET  = $GetLstLine();
                ECR_CODIGO = $GetFieldNum('OPNSEG');
                ECR_NOME   = $GetField('OPNOME');
                IF (REC# = 1);
                   WCodigo = ECR_CODIGO;
                ENDIF;
                ECR_LS#OPT = *BLANKS;
                WRITE RENTE01402;
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

       //==============================================================
       // Subrotina.: SelRecords
       // Objectivo.: Seleccionar os registos
       //==============================================================
       BEGSR selRecords;
          WIcSelected = *OFF;
          IF (REC# > *ZEROS);
             READC RENTE01402;
             DOW (NOT %EOF());
                IF (ECR_LS#OPT <> *BLANKS);
                   WIcSelected = *ON;
                   WCodigo = ECR_CODIGO;
                   LEAVESR;
                ENDIF;
                READC RENTE01402;
             ENDDO;
          ENDIF;
          $SetCriteria(ECR_DS#CRIT:*ON);
       ENDSR;
      /END-FREE
     P $RunPSPopUp     E

     P*=======================================================
     P* Processo..: $ChkPSCode
     P* Descrição.: Validar se o código da Pessoa seguro ou
     P*             Herdeiro Legal, está correcto.
     P*
     P* Tipo - Tipo de registos a extrair
     P*   A = Activos
     P*   E = Desactivos
     P*   T = Todos
     C*=======================================================
     P $ChkPSCode      B                   EXPORT
     D                 PI             8S 0
     D  Codigo                        8S 0 CONST
     D  Ramo                          2S 0 Const Options(*NOPASS)
     D  Apolice                       7S 0 Const Options(*NOPASS)
     D  Tipo                          1A   Const Options(*NOPASS)
     D
     D*==> Campos de Trabalho
     D WCriteria       S            100A   INZ(*BLANKS)
     D WCodigo         S              8S 0 INZ(*ZEROS)
     D WRamo           S              2S 0 INZ(*ZEROS)
     D WApolice        S              7S 0 INZ(*ZEROS)
     D WTipo           S              1A   INZ(*BLANKS)
     D WSessDBS        S              3S 0 INZ(*ZEROS)
     D*===========================================================
      /FREE
       //----------------------------------------
       // ForCompr parametros recebidos
       //----------------------------------------
       WRamo    = *ZEROS;
       WApolice = *ZEROS;
       WTipo    = 'T';
       WCodigo  = Codigo;
       IF (%PARMS() > 1);
          WRamo = Ramo;
          IF (%PARMS() > 2);
             WApolice = Apolice;
             IF (%PARMS() > 3);
                WTipo = Tipo;
             ENDIF;
          ENDIF;
       ENDIF;

       //------------------------------------
       // Inicializar as variáveis de Trabalho
       //------------------------------------
       WSessDBS = $SetFiles('OPLSEGF INNER JOIN OPLAPOF ON (OPNSEG = LANSEG)');
       WCriteria = '(OPNSEG = '''+%CHAR(WCodigo)+''')';
       IF (WRamo > *ZEROS);
          WCriteria = %TRIM(WCriteria) +
                      'AND (LARAMO='''+%CHAR(WRamo)+''') AND +
                        (LAAPOL='''+%CHAR(WApolice)+''')';
       ENDIF;
       $SetFields('OPNSEG,OPRAMO,OPAPOL,OPNOME,OPNIF,OPDTNA');
       IF (WTipo = 'A'); // Activos
          WCriteria = %TRIM(WCriteria) +
                      'AND ((LASIT = ''0'') OR (LASIT='' ''))';
       ELSEIF (WTipo = 'E'); // Anulados
          WCriteria = %TRIM(WCriteria) +
                      'AND ((LASIT <> ''0'') AND (LASIT<>'' ''))';
       ENDIF;
       $SetCriteria(WCriteria);

       //---------------------------------------------------------
       // Substituir esta parte por uma variavel booleana
       //---------------------------------------------------------
       IF ($GetRecord(1) > *ZEROS);
          WCodigo = *ZEROS;
       ELSE;
          WCodigo = 1;
       ENDIF;

       $RmvDBSession(WSessDBS);
       RETURN WCodigo;
      /END-FREE
     P $ChkPSCode      E
