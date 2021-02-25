     H BNDDIR('JMDIR')
     H*=======================================================
     H* Dat.Cria...: 2012-07-20
     H* Autor......: Luis Galhoz
     H* Processo...: $PopUpComp
     H* Objectivo..: Executar uma Pop-Up, para seleccionar
     H*=======================================================
     FSSECE012  CF   E             WORKSTN SFILE(RSECE01202:REC#)
     F                                     INFDS(INFD#KEYS)
     F                                     PREFIX(ECR_)
     F*===========================================================
     F* Indicadores  Descrição
     F* -----------  ---------------------------------------------
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

     D  REC#           S              4  0
      /FREE

       RETURN;
      /END-FREE
     P*=======================================================
     P* Processo..: $RunPopUpTempl
     P* Descrição.: Executar uma Pop e devolver os valores
     P*             da Selecção.
     C*=======================================================
     P $RunPopUpTempl  B                   EXPORT
     D                 PI             5S 0
     D    RTemplate                   5S 0 CONST
     D    RCodigo                     5S 0 CONST
     D
     D  WCdItem        S              4  0 INZ(*ZEROS)
     D WRetCode        S              5S 0
     D WSessionID      S              5S 0 INZ(*ZEROS)
     D*===========================================================
      /FREE

       //------------------------------------
       // Inicializar as variáveis de Trabalho
       //------------------------------------
       WSessionID = $SETFILES('FSECP010');
       $addCriteria('Nome':'NMPROGR':'GE');
       $addCriteria('Descrição':'DSPROGR':'LK');
       ECR_LABEL = $getCritText();
       $SetCriteria('CDTMPLT = ''' + %CHAR(RTemplate) + '''');
       $SetPage(10);
       EXSR DspList;

       $RmvDBSession(WSessionID);
       RETURN WRetCode;

       //==============================================================
       // Subrotina.: dspList
       // Objectivo.: Mostrar Lista com os Códigos
       //==============================================================
       BEGSR dspList;
          //------------------------------------
          // Escrever Header Standard
          //------------------------------------
          DS#HEADER   = $getHeader(S#PGM:'RSECE01201');
          ECR_DS#PGMR = ECR_DS#PGM;
          ECR_DS#FNCR = ECR_DS#FNC;
          WRITE RSECE01200;
          DS#FOOTER     = $getFooter(S#PGM:'RSECE01201':*BLANKS);
          ECR_DS#KEY01R = ECR_DS#KEY01;
          ECR_DS#KEY02R = ECR_DS#KEY02;
          WRITE RSECE01299;
          WRITE RSECE01298;
          EXSR inzRecords;
          EXSR getRecords; // Carregar Lista com Registos
          DOW (NOT *IN12) AND (NOT *IN03);
             EXFMT RSECE01201;
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
                WRetCode = *ZEROS;
                EXSR selRecords;
                IF (WRetCode = *ZEROS);
                   EXSR inzRecords;
                   EXSR getRecords;
                ELSE;
                   LEAVE;
                ENDIF;
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
          WRITE RSECE01201; // Limpar Lista
          *In53 = *Off;     // Repor Indicador
          *In54 = *Off;     // Repor Indicador
          *IN12 = *OFF;
          *IN03 = *OFF;
       ENDSR;

       //==============================================================
       // Subrotina.: GetRecords
       // Objectivo.: Obter Registos para apresentar
       //==============================================================
       BEGSR getRecords;
          IF ($GetPage(Func#Key) > *ZEROS);
             DOW (NOT $IsEOF());
                REC# += 1;
                ECR_CDPROGR = %DEC($GetField('CDPROGR'):5:0);
                ECR_CDTIPO  = $GetField('CDTIPO');
                ECR_NMPROGR = $GetField('NMPROGR');
                ECR_DSPROGR = $GetField('DSPROGR');
                ECR_Select  = *BLANKS;
                WRITE RSECE01202;
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
          READC RSECE01202;
          DOW (NOT %EOF());
             IF (ECR_Select <> *BLANKS);
                WRetCode = ECR_CDPROGR;
                ECR_Select  = *BLANKS;
                LEAVE;
             ELSE;
                ECR_Select = *BLANKS;
             ENDIF;
             READC RSECE01202;
          ENDDO;
          IF (%TRIM(ECR_OPTION) <> *BLANKS);
             $SetCriteria(ECR_OPTION:*ON);
          ENDIF;
       ENDSR;
      /END-FREE
     P $RunPopUpTempl  E
