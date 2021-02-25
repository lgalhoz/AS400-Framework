     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: RPRDO015
     H* Descrição.: Comentários e Observações
     H*
     H* 04-05-2014: Apenas escreve o texto das Observações se a Primeira linha
     H* P.Craveiro  estiver preenchida, se a primeira estiver preenchida e a
     H*             segunda preenchida, então não escreve a segunda linha.
     H*
     H*==================================================================
     FFCATP010  IF   E           K DISK    PREFIX(CAT_)
     D*==========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SCATO010
      /Copy QRPGLESRC,SPRDO002

     D*==> Estrutura de dados do Registo
     D RBASEDIO      E DS                  ExtName(GBASEDIO)
     D                                     Prefix(OR_)
     D                                     Based(ptrOR)
     D
     D*==> Estrutura de dados do Histórico
     D RGEXAP        E DS                  ExtName(GEXAP)
     D                                     Prefix(HST_)
     D                                     Based(ptrHist)
     D
     D*==> Indices de Indexação
     D WIndiceDs       DS
     D   WFilo3                1      6
     D   WIndBase              1      3  0
     D   WIndVenc              4      6  0
     D WLinha          S            500A
     D WptrHist        S               *
     D WptrOr          S               *
     D WCdProd         S              5S 0
     D WIcTitulo       S               N
     D WCodErro        S              7A
     D
     D WOBS1           S                    LIKE(HST_OBS1)
     D WOBS2           S                    LIKE(HST_OBS2)
     D WCE003          S                    LIKE(HST_CE003)
     F*==========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCdProd
     C                   PARM                    WRamo
     C                   PARM                    WApolice
     C                   PARM                    WDatInicio
     C                   PARM                    WCodErro
     C                   PARM                    WPtrHist
     C                   PARM                    WPtrOR
      /FREE
       WCoderro = *BLANKS;
       ptrHist  = WptrHist;
       ptrOr    = WptrOR;
       MONITOR;
          WOBS1  = HST_OBS1;
          WOBS2  = HST_OBS2;
          WCE003 = HST_CE003;
       ON-ERROR;
          WOBS1  = OR_OBS1;
          WOBS2  = OR_OBS2;
          WCE003 = OR_CE003;
       ENDMON;
       WIcTitulo = *OFF;
       EXSR WrtIndx;

       RETURN;

       //================================================================
       // Subrotina..: WrtIndx
       // Objectivo..: Escrever os textos nas Apólices referente às
       //              indexações.
       //================================================================
       BEGSR WRTINDX;
|        //------------------------------------
|        // Salvaguardar texto de Observações
|        //------------------------------------
|        IF (WOBS1 <> *BLANKS);
            ExSr AddTitulo;
            $addLineType('T1');
            WLinha =  %TRIMR(WOBS1);
            $AddTextLine(WLinha);
|           IF (WOBS2 <> *BLANKS);
               ExSr AddTitulo;
               $addLineType('T1');
               WLinha = %TRIMR(WOBS2);
               $AddTextLine(WLinha);
|           ENDIF;
         ENDIF;
|
|        //-----------------------------------------------------
|        // Obter os Valores para os Indices Base e Vencimento
|        //-----------------------------------------------------
|        IF (WCE003 = 'S');
            ExSr AddTitulo;
            $addLineType('T1');
            WLinha = 'Aumento anual conf. ISP: ';
            $AddTextLine(WLinha);
|           WIndiceDS  = OR_FILO3;
            MONITOR;
|           CHAIN (WIndBase) RCATP010;
|           IF (%FOUND);
               $addLineType('B1');
               WLinha = 'Ind.Base.' + %CHAR(CAT_VALOR);
               $AddTextLine(WLinha);

|              CHAIN (WIndVenc) RCATP010;
               IF (%FOUND);
                  $addLineType('B1');
                  WLinha = 'Ind.Venc.' + %CHAR(CAT_VALOR);
                  $AddTextLine(WLinha);
               ENDIF;
           ENDIF;
           ON-ERROR;
           ENDMON;
         ENDIF;
+----  ENDSR;

       //================================================================
       // Subrotina..: AddTitulo
       // Objectivo..: Adicionar O titulo da observação
       //================================================================
       BEGSR AddTitulo;
          IF (Not WIcTitulo);
             $addLineType('H1');
             $AddTextLine();
|            WLinha = 'OBSERVAÇÕES:';
             $AddTextLine(WLinha);
             WIcTitulo = *ON;
          ENDIF;
       ENDSR;
      /END-FREE
