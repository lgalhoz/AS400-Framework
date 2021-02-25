     H NOMAIN
     H*=======================================================
     H* Dat.Cria...: 2011-05-12
     H* Autor......: Luis Galhoz
     H* Processo...: runPopUp
     H* Objectivo..: Executar uma Pop-Up, para seleccionar
     H*              valores possíveis Tabela de Códigos.
     H*=======================================================
     FFTBLP010  IF   E           K DISK    PREFIX(TBL_)
     FFTBLP011  IF   E           K DISK    PREFIX(TBL_)
     FFTBLP01202UF A E           K DISK    PREFIX(TBL_)
     F                                     RENAME(RTBLP012:RTBLP01202)
     FFTBLP012  UF A E           K DISK
     FSTBLE012  CF   E             WORKSTN SFILE(RTBLE01202:REC#) USROPN
     F                                     INFDS(INFD#KEYS)
     F                                     PREFIX(ECR_)
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
     F*=====================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SSECO010
     D
     D  WRetCode       S             15A
     D  REC#           S              4  0
     D
     P*=======================================================
     P* Processo..: $RunPopUp
     P* Descrição.: Executar uma Pop e devolver os valores
     P*             da Selecção.
     C*=======================================================
     P $RunPopUp       B                   EXPORT
     D                 PI            15A
     D    RCdIndice                   5S 0 CONST
     D    RCodigo                    15A   CONST
     D    RModo                        N   CONST Options(*NOPASS)
     D
     D WSelKey         S               N   INZ(*OFF)
     D WCdIndice       S              5S 0
     D WCodigo         S             15A
     D WRetCode        S             15A
     D WResult         S              5S 0 INZ(*ZEROS)
     D WSessionID      S              5S 0 INZ(*ZEROS)
     D WModo           S               N   INZ(*OFF)
     D
     D*==> Obter o numero livre para a Tabela
     D getChave        PR             5S 0
     D   PCdIndice                    5S 0 CONST
     D
     D*===========================================================
      /FREE
       //------------------------------------
       // VALIDAR EXISTÊNCIA DA TABELA
       //------------------------------------
       CHAIN (RCdIndice) RTBLP010;
       IF (NOT %FOUND());
          RETURN WRetCode;
       ENDIF;
       OPEN STBLE012;

       WModo = *OFF; // Modo Alteração
       IF (%Parms > 2);
          WModo = RModo; // Modo Leitura
       ENDIF;

       *IN12     = *Off;
       *IN03     = *Off;
       *IN04     = *Off;
       WSelKey   = *Off;
       WCdIndice = RCdIndice;
       WSessionID = $SetFiles('FTBLP012');
       IF (WModo = *ON);
          ExSr DSPCodigo;
       ELSE;
          //------------------------------------
          // Inicializar as variáveis de Trabalho
          //------------------------------------
          $SetCriteria('CDINDICE = '+ %CHAR(WCdIndice));
          $addCriteria('Codigo....:':'CODIGO':'GE');
          $addCriteria('Descrição.:':'DESCCOD':'LK');
          ECR_LABEL = $getCritText();
          $SetPage(10);
          EXSR DspList;
       ENDIF;
       $RmvDBSession(WSessionID);
       CLOSE STBLE012;
       RETURN WRetCode;

       //==============================================================
       // Subrotina.: dspCodigo
       // Objectivo.: Mostrar a informação sobre o Código
       //==============================================================
       BEGSR dspCodigo;
          CHAIN (RCDIndice:RCodigo) RTBLP01202;
          IF (%FOUND());
             ECR_CODIGO = TBL_CODIGO;
             ECR_DSCOD01 = TBL_DESCCOD;
             CHAIN (RCDIndice) RTBLP011;
             IF (%FOUND());
                ECR_TEXTO01 = %SUBST(TBL_TEXTO:1);
                ECR_TEXTO02 = %SUBST(TBL_TEXTO:61);
                ECR_TEXTO03 = %SUBST(TBL_TEXTO:121);
                ECR_TEXTO04 = %SUBST(TBL_TEXTO:181);
             ELSE;
                ECR_TEXTO01 = *BLANKS;
                ECR_TEXTO02 = *BLANKS;
                ECR_TEXTO03 = *BLANKS;
                ECR_TEXTO04 = *BLANKS;
             ENDIF;
          ELSE;
             LEAVESR;
          ENDIF;
          DS#HEADER   = $getHeader(S#PGM:'RTBLE01201');
          ECR_DS#FNC = $ALIGNTEXT(TBL_DSINDICE:60:'C');
          ECR_DS#PGMR = ECR_DS#PGM;
          ECR_DS#FNCR = ECR_DS#FNC;
          WRITE RTBLE01200;
          DS#FOOTER     = $getFooter(S#PGM:'RTBLE01208':*BLANKS);
          ECR_DS#KEY01R = ECR_DS#KEY01;
          ECR_DS#KEY02R = ECR_DS#KEY02;
          WRITE RTBLE01299;
          EXFMT RTBLE01208;
          IF (Func#Key = KEY#F03) OR (Func#Key = KEY#F12);
             WRetCode = RCodigo;
             LEAVESR;
          ENDIF;
          WRetCode = RCodigo;
       ENDSR;

       //==============================================================
       // Subrotina.: dspList
       // Objectivo.: Mostrar Lista com os Códigos
       //==============================================================
       BEGSR dspList;
          DOW (NOT *IN12) AND (NOT *IN03);
             //------------------------------------
             // Escrever Header Standard
             //------------------------------------
             DS#HEADER   = $getHeader(S#PGM:'RTBLE01201');
             ECR_DS#FNC = $ALIGNTEXT(TBL_DSINDICE:60:'C');
             ECR_DS#PGMR = ECR_DS#PGM;
             ECR_DS#FNCR = ECR_DS#FNC;
             WRITE RTBLE01200;
             DS#FOOTER     = $getFooter(S#PGM:'RTBLE01201':*BLANKS);
             ECR_DS#KEY01R = ECR_DS#KEY01;
             ECR_DS#KEY02R = ECR_DS#KEY02;
             EXSR inzRecords;
             EXSR getRecords; // Carregar Lista com Registos
             WRITE RTBLE01299;
             WRITE RTBLE01298;
             EXFMT RTBLE01201;
             IF (Func#Key = KEY#F03) OR (Func#Key = KEY#F12);
                WRetCode = RCodigo;
                LEAVE;
             ELSEIF ((Func#Key = KEY#PgDown) AND (NOT $IsLastPage())) OR
               ((Func#Key = KEY#PgUp) AND ($GetPageNbr() > 1));
                EXSR inzRecords;
                EXSR getRecords;
             ELSEIF (Func#Key = KEY#F05);
                $ChgCriteria();
                ECR_LABEL = $getCritText();
                EXSR inzRecords;
                EXSR getRecords; // Carregar Lista com Registos
             ELSEIF ($ValidKey(Func#Key:KEY#F06));
                EXSR addRecord;
                EXSR inzRecords;
                EXSR getRecords; // Carregar Lista com Registos
                *IN12 = *OFF;
             ELSEIF (Func#Key = KEY#Enter);
                EXSR selRecords;
                IF (WRetCode <> *BLANKS);
                   LEAVE;
                ENDIF;
                EXSR inzRecords;
                EXSR getRecords; // Carregar Lista com Registos
             ENDIF;
          ENDDO;
       ENDSR;

       //==============================================================
       // Subrotina.: addRecord
       // Objectivo.: Adicionar um novo registo
       //==============================================================
       BEGSR addRecord;
          DS#HEADER = $getHeader(S#PGM:'RTBLE01206');
          ECR_DS#FNC = $ALIGNTEXT(TBL_DSINDICE:60:'C');
          ECR_DS#PGMR= ECR_DS#PGM;
          ECR_DS#FNCR= ECR_DS#FNC;
          WRITE     RTBLE01200;
          DS#FOOTER = $getFooter(S#PGM:'RTBLE01206':*BLANKS);
          ECR_DS#KEY01R= ECR_DS#KEY01;
          ECR_DS#KEY02R= ECR_DS#KEY02;
          WRITE     RTBLE01299;
          ECR_CDINDICE = WCdIndice;
          ECR_DSCOD01  = *BLANKS;
          DOW (NOT *IN03) AND (NOT *IN12);
             IF (TBL_CDINCREM > *ZEROS);
                *IN46 = *OFF;
                SETGT (WCdIndice:'02199') RTBLP01202;
                READP RTBLP01202;
                IF (WCdIndice = TBL_CDINDICE);
                   ECR_Codigo='0'+%CHAR(((%DEC(TBL_CODIGO:5:0)-2100)+1)+2100);
                   ECR_DSCOD01=*BLANKS;
                ENDIF;
             ELSE;
                *IN46 = *ON;
             ENDIF;
             EXFMT RTBLE01206;
             IF (Func#Key = KEY#Enter);
                CDINDICE = WCdIndice;
                CODIGO   = ECR_Codigo;
                DESCCOD  = ECR_DSCOD01;
                CDCODIGO = getChave(WCdIndice);
                ALIAS    = CODIGO;
                WRITE RTBLP012;
                LEAVE;
             ENDIF;
          ENDDO;
       ENDSR;

       //==============================================================
       // Subrotina.: InzRecords
       // Objectivo.: Inicializar a lista de registos
       //==============================================================
       BEGSR inzRecords;
          REC#  = *ZEROS;   // Controlo de Registos da Lista de Ecrã
          *In53 = *On;      // Limpar Lista de Ecrã
          *In52 = *On;      // Mostrar a Lista de Ecrã
          WRITE RTBLE01201; // Limpar Lista
          *In53 = *Off;     // Repor Indicador
          *In54 = *Off;     // Repor Indicador
       ENDSR;

       //==============================================================
       // Subrotina.: GetRecords
       // Objectivo.: Obter Registos para apresentar
       //==============================================================
       BEGSR getRecords;
          //----------------------------------------------
          //-  Invocar Processo para obter os registos
          //----------------------------------------------
          IF ($GetPage(Func#Key) > *ZEROS);
             DOW (NOT $IsEOF());
                REC# += 1;
                ECR_CODIGO  = $GetField('CODIGO');
                ECR_DSCOD01 = $GetField('DESCCOD');
                ECR_Select  = *BLANKS;
                WRITE RTBLE01202;
                $getNext();
             ENDDO;
          ENDIF;

          //---------------------------------------
          // Validar se ainda existem mais registos
          //---------------------------------------
          IF (REC# = *ZEROS);
             *IN52 = *OFF;
          ELSE;
             IF (NOT $IsLastPage());
                *IN54    = *OFF;
             ELSE;
                *IN54    = *ON;
             ENDIF;
          ENDIF;
       ENDSR;

       //==============================================================
       // Subrotina.: SelRecords
       // Objectivo.: Seleccionar os registos
       //SelRec========================================================
       BEGSR selRecords;
          IF (REC# = *ZEROS);
             LEAVESR;
          ENDIF;
          READC RTBLE01202;
          DOW (NOT %EOF());
             IF (ECR_Select <> *BLANKS);
                WRetCode = ECR_CODIGO;
                LEAVESR;
             ENDIF;
             READC RTBLE01202;
          ENDDO;
          $SetCriteria(ECR_CRITERIA:*ON);
       ENDSR;
      /END-FREE
     P $RunPopUp       E
     P*=======================================================
     P* Processo..: $ExeAjuda
     P* Descrição.: Obter a ajuda para o campo.
     P*
     C*=======================================================
     P $ExeAjuda       B                   EXPORT
     D                 PI
     D    RCdIndice                   5S 0 CONST
     D    RCodigo                    15A   CONST
     D
     P $ExeAjuda       E
