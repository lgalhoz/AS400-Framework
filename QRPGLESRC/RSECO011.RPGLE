     H BNDDIR('JMDIR')
     H*=======================================================
     H* Dat.Cria...: 2012-07-20
     H* Autor......: Luis Galhoz
     H* Processo...: $PopUpComp
     H* Objectivo..: Executar uma Pop-Up, para seleccionar
     H*=======================================================
     FSSECE011  CF   E             WORKSTN SFILE(RSECE01102:REC#)
     F                                     INFDS(INFD#KEYS)
     F                                     PREFIX(ECR_)
     F*===========================================================
     F* Indicadores  Descri��o
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
     P* Processo..: $RunPopUpComp
     P* Descri��o.: Executar uma Pop e devolver os valores
     P*             da Selec��o.
     C*=======================================================
     P $RunPopUpComp   B                   EXPORT
     D                 PI             5S 0
     D    RCodigo                     5S 0 CONST
     D
     D  WCdItem        S              4  0 INZ(*ZEROS)
     D WRetCode        S              5S 0
     D WSessionID      S              5S 0 INZ(*ZEROS)
     D*===========================================================
      /FREE

       //------------------------------------
       // Inicializar as vari�veis de Trabalho
       //------------------------------------
       WSessionID = $SETFILES('FSECP010');
       $addCriteria('Nome':'NMPROGR':'GE');
       $addCriteria('Descri��o':'DSPROGR':'LK');
       ECR_LABEL = $getCritText();
       $SetPage(10);
       EXSR DspList;

       $RmvDBSession(WSessionID);
       RETURN WRetCode;

       //==============================================================
       // Subrotina.: dspList
       // Objectivo.: Mostrar Lista com os C�digos
       //==============================================================
       BEGSR dspList;
          //------------------------------------
          // Escrever Header Standard
          //------------------------------------
          DS#HEADER   = $getHeader(S#PGM:'RSECE01101');
          ECR_DS#PGMR = ECR_DS#PGM;
          ECR_DS#FNCR = ECR_DS#FNC;
          WRITE RSECE01100;
          DS#FOOTER     = $getFooter(S#PGM:'RSECE01101':*BLANKS);
          ECR_DS#KEY01R = ECR_DS#KEY01;
          ECR_DS#KEY02R = ECR_DS#KEY02;
          WRITE RSECE01199;
          WRITE RSECE01198;
          EXSR inzRecords;
          EXSR getRecords; // Carregar Lista com Registos
          DOW (NOT *IN12) AND (NOT *IN03);
             EXFMT RSECE01101;
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
          REC#  = *ZEROS;   // Controlo de Registos da Lista de Ecr�
          *In53 = *On;      // Limpar Lista de Ecr�
          *In52 = *On;      // Mostrar a Lista de Ecr�
          WRITE RSECE01101; // Limpar Lista
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
                WRITE RSECE01102;
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
          READC RSECE01102;
          DOW (NOT %EOF());
             IF (ECR_Select <> *BLANKS);
                WRetCode = ECR_CDPROGR;
                ECR_Select  = *BLANKS;
                LEAVE;
             ELSE;
                ECR_Select = *BLANKS;
             ENDIF;
             READC RSECE01102;
          ENDDO;
          IF (%TRIM(ECR_OPTION) <> *BLANKS);
             $SetCriteria(ECR_OPTION:*ON);
          ENDIF;
       ENDSR;
      /END-FREE
     P $RunPopUpComp   E
