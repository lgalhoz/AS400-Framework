     H*ALWNULL(*USRCTL)                  AUT(*ALL)
     H NOMAIN BNDDIR('JMDIR')
     H*=======================================================
     H* Dat.Cria...: 2011-05-12
     H* Autor......: Luis Galhoz
     H* Processo...: runPopUp
     H* Objectivo..: Executar uma Pop-Up, para seleccionar
     H*              os Valors possíveis para o Risco.
     H*=======================================================
     FSCATE014  CF   E             WORKSTN SFILE(RCATE01402:REC#)
     F                                     INFDS(INFD#KEYS)
     F                                     USROPN PREFIX(ECR_)
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
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SSECO010
      /Copy QRPGLESRC,SCATO010
     D
     D  REC#           S              4  0
     D
     P*=======================================================
     P* Processo..: $RunRiskPopUp
     P* Descrição.: Executar uma Pop e devolver os valores
     P*             da Selecção.
     C*=======================================================
     P $RunRiskPopUp   B                   EXPORT
     D                 PI             5S 0
     D    RCdProd                     5S 0 CONST
     D    RCdRisco                    5S 0 CONST
     D
     D WSelKey         S               N   INZ(*OFF)
     D WCdIndice       S              5S 0
     D WCodigo         S             15A
     D WRetCode        S              5S 0
     D WResult         S              5S 0 INZ(*ZEROS)
     D WSessionID      S              5S 0 INZ(*ZEROS)
     D
     D*==> Obter o numero livre para a Tabela
     D getChave        PR             5S 0
     D   PCdIndice                    5S 0 CONST
     D
     D*===========================================================
      /FREE

       //------------------------------------
       // Inicializar as variáveis de Trabalho
       //------------------------------------
       OPEN  SCATE014;
       *IN12     = *Off;
       *IN03     = *Off;
       *IN04     = *Off;
       WSelKey   = *Off;
       WCdIndice = RCdProd  ;
       WSessionID = $SetFiles('FCATP101');
       $SetCriteria('CDPROD = '+ %CHAR(RCdProd));
       $addCriteria('Codigo....:':'CDRISCO':'GE');
       $addCriteria('Descrição.:':'DSRISCO':'LK');
       ECR_LABEL = $getCritText();
       $SetPage(10);

       EXSR DspList;
       $RmvDBSession(WSessionID);
       CLOSE SCATE014;
       RETURN WRetCode;

       //==============================================================
       // Subrotina.: dspList
       // Objectivo.: Mostrar Lista com os Códigos
       //==============================================================
       BEGSR dspList;
          //------------------------------------
          // Escrever Header Standard
          //------------------------------------
          DS#HEADER   = $getHeader(S#PGM:'RCATE01401');
          //ECR_DS#FNC = $ALIGNTEXT(PRD_DSINDICE:60:'C');
          ECR_DS#PGMR = ECR_DS#PGM;
          ECR_DS#FNCR = ECR_DS#FNC;
          WRITE RCATE01400;
          DS#FOOTER     = $getFooter(S#PGM:'RCATE01401':*BLANKS);
          ECR_DS#KEY01R = ECR_DS#KEY01;
          ECR_DS#KEY02R = ECR_DS#KEY02;
          WRITE RCATE01499;
          WRITE RCATE01498;
          EXSR inzRecords;
          EXSR getRecords; // Carregar Lista com Registos
          DOW (NOT *IN12) AND (NOT *IN03);
             EXFMT RCATE01401;
             IF (Func#Key = KEY#F03) OR (Func#Key = KEY#F12);
                WRetCode = RCdRisco;
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
             ELSEIF (Func#Key = KEY#Enter);
                EXSR selRecords;
                IF (WRetCode <> *ZEROS);
                   LEAVE;
                ENDIF;
                EXSR inzRecords;
                EXSR getRecords; // Carregar Lista com Registos
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
          WRITE RCATE01401; // Limpar Lista
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
                MONITOR;
                   ECR_CDRISCO = %DEC($GetField('CDRISCO'):5:0);
                ON-ERROR;
                ENDMON;
                ECR_DSRISCO = $GetField('DSRISCO');
                ECR_Select  = *BLANKS;
                WRITE RCATE01402;
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
          READC RCATE01402;
          DOW (NOT %EOF());
             IF (ECR_Select <> *BLANKS);
                WRetCode = ECR_CDRISCO;
                LEAVESR;
             ENDIF;
             READC RCATE01402;
          ENDDO;
          $SetCriteria(ECR_CRITERIA:*ON);
       ENDSR;
      /END-FREE
     P $RunRiskPopUp   E
