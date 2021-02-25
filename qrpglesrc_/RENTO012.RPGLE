     H*=======================================================
     H* Dat.Cria...: 2011-05-12
     H* Autor......: Luis Galhoz
     H* Processo...: runPopUp
     H* Objectivo..: Executar uma Pop-Up, para seleccionar
     H*              valores possíveis Tabela de Códigos.
     H*=======================================================
     FSENTE012  CF   E             WORKSTN SFILE(RENTE01202:REC#)
     F                                     INFDS(INFD#KEYS)
     F                                     PREFIX(ECR_)
     F*===========================================================
     F* Indicadores  Descrição
     F* -----------  ---------------------------------------------
     F*      03      Tecla F3
     F*      06      Tecla F6
     F*      12      Tecla F12
     F*      50      Tecla PagDn
     F*      51      Tecla PagUp
     F*      52      DSP
     F*      53      CLR
     F*      54      END
     F*=====================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SENTO010
      /Copy QRPGLESRC,SSECO010
     D
     D REC#            S              4  0
     P*=======================================================
     P* Processo..: $RunEntPopUp
     P* Descrição.: Executar uma Pop e devolver os valores
     P*             da Selecção.
     C*=======================================================
     P $RunEntPopUp    B                   EXPORT
     D                 PI             8S 0
     D    RCodigo                     8S 0 CONST
     D
     D WIsEmpty        S               N   INZ(*ON)
     D WIsSelect       S               N   INZ(*ON)
     D WRetCode        S              8S 0
     D WNome           S             40A
     D WNIF            S              9S 0
     D WCodigo         S              9S 0
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
       *IN12     = *Off;
       *IN03     = *Off;
       *IN04     = *Off;
       WSessionID = $AddDBSession();
       $SetFiles('GFSEG');
       $SetFields('NSEG, NIF, NOM');
       $SetPage(10);
       $addCriteria('Código....:':'NSEG':'GE':*ON);
       $addCriteria('Nome......:':'NOM':'LK');
       $addCriteria('NIF.......:':'NIF':'GE');
       ECR_LABEL = $getCritText();
       ECR_OPTION = %CHAR(RCodigo);
       EXSR DspList;
       $RmvDBSession(WSessionID);
       *INLR = *ON;
       RETURN WRetCode;

       //==============================================================
       // Subrotina.: dspList
       // Objectivo.: Mostrar Lista com os Códigos
       //==============================================================
       BEGSR dspList;
          //------------------------------------
          // Escrever Header Standard
          //------------------------------------
          DS#HEADER   = $getHeader(S#PGM:'RENTE01201');
          ECR_DS#PGMR = ECR_DS#PGM;
          ECR_DS#FNCR = ECR_DS#FNC;
          WRITE RENTE01200;
          DS#FOOTER     = $getFooter(S#PGM:'RENTE01201':*BLANKS);
          ECR_DS#KEY01R = ECR_DS#KEY01;
          ECR_DS#KEY02R = ECR_DS#KEY02;
          WRITE RENTE01299;
          WRITE RENTE01298;
          EXSR inzRecords;
          EXSR getRecords; // Carregar Lista com Registos
          DOW (NOT *IN12) AND (NOT *IN03);
             EXFMT RENTE01201;
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
                EXSR getRecords;
             ELSEIF (Func#Key = KEY#Enter);
                WIsSelect   = *OFF;
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
          WRITE RENTE01201; // Limpar Lista
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
                IF (REC# = 1);
                   WCodigo  = %DEC($GetField('NSEG'):8:0);
                   WNIF     = %DEC($GetField('NIF'):9:0);
                   WNOME    = $GetField('NOM');
                ENDIF;
                ECR_CODIGO  = %DEC($GetField('NSEG'):8:0);
                ECR_NIF     = %DEC($GetField('NIF'):9:0);
                ECR_NOME    = $GetField('NOM');
                ECR_Select  = *BLANKS;
                WRITE RENTE01202;
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
       //==============================================================
       BEGSR selRecords;
          IF (REC# = *ZEROS);
             LEAVESR;
          ENDIF;
          READC RENTE01202;
          DOW (NOT %EOF());
             IF (ECR_Select <> *BLANKS);
                WRetCode = ECR_CODIGO;
                LEAVESR;
             ENDIF;
             READC RENTE01202;
          ENDDO;
          $SetCriteria(ECR_OPTION:*ON);
       ENDSR;
      /END-FREE
     P $RunEntPopUp    E
     P*=======================================================
     P* Processo..: $getDescEnt
     P* Descrição.: Obter o Nome da Entidade
     C*=======================================================
     P $GetDescEnt     B                   EXPORT
     D                 PI            40A
     D    RCodigo                     8S 0 CONST
     D
     D WResult         S             40A   INZ(*BLANKS)
     D WSessionID      S              5S 0 INZ(*ZEROS)
     D*===========================================================
      /FREE
        WSessionID = $AddDBSession();
        $SETFILES('GFSEG');
        $SETFIELDS('NOM, NSEG');
        $SETCRITERIA('NSEG = ''' + %CHAR(RCODIGO) + '''');
        $SETSORT(*BLANKS);
        $GetRecord(1:1);
        IF (NOT $IsEOF());
            WResult = $GetField('NOM');
        ELSE;
            WResult = *BLANKS;
        ENDIF;
        $RmvDBSession(WSessionID);
        RETURN WResult;
      /END-FREE
     P $GetDescEnt     E
