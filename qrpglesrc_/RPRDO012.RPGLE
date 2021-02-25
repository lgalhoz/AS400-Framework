     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: RRDO012
     H* Descrição.: Actas - Limites e Franquias
     H*
     H*==================================================================
     FFCATP10301IF   E           K DISK    PREFIX(CAT_)
     F                                     RENAME(RCATP103:RCATP10301)
     FFPRDP01401IF   E           K DISK    PREFIX(PRD_)
     F                                     RENAME(RPRDP014:RPRDP01401)
     FFCATP103  IF   E           K DISK    PREFIX(CAT_)
     FFCATP102  IF   E           K DISK    PREFIX(CAT_)
     D*==========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SCATO010
      /Copy QRPGLESRC,SPRDO002
      /Copy QRPGLESRC,STBLO010

     D*==> Estrutura de dados do Histórico
     D RGEXAP        E DS                  ExtName(GEXAP)
     D                                     Prefix(HST_)
     D                                     Based(ptrHist)
     D
     D*==> Estrutura de dados do Registo
     D RBASEDIO      E DS                  ExtName(GBASEDIO)
     D                                     Prefix(OR_)
     D                                     Based(ptrOR)
     D
     D*==> Validar se é para seleccionar
     D DigTexto        PR              N
     D   PRM_CdRamo                   2S 0 CONST
     D   PRM_Apolice                  7S 0 CONST
     D   PRM_CdSuper                  5S 0 CONST
     D   PRM_Tipo                     1A   CONST
     D   PRM_Nivel                    5S 0 CONST
     D
     D*==> Validar se é para seleccionar
     D IsToSelect      PR              N
     D   PRM_CdRamo                   2S 0 CONST
     D   PRM_Apolice                  7S 0 CONST
     D   PRM_CdCobert                 5S 0 CONST
     D   PRM_CdRisco                  5S 0 CONST
     D
     D*==> Controlo de Niveis
     D $LstNivel       DS                  QUALIFIED DIM(10)
     D   Titulo                     100A   INZ(*BLANKS)
     D   CdAlias                      6A   INZ(*BLANKS)
     D   IsWrite                       N   INZ(*OFF)
     D WActNivel       S              5S 0
     D
     D*==> Restantes Vairáveis Globais
     D WDesCobert      S            100A
     D WLinhaTxt       S            500A
     D WLinhaHdr       S            500A
     D WCdProd         S              5S 0
     D WIcTitulo       S               N
     D WTipCobert      S              1A
     D WptrHist        S               *
     D WptrOr          S               *
     D
     D WCR1            S                   LIKE(HST_CR1)
     D WCAP1           S                   LIKE(HST_CAP1)
     D WCR2            S                   LIKE(HST_CR2)
     D WCAP2           S                   LIKE(HST_CAP2)
     D WCR3            S                   LIKE(HST_CR3)
     D WCAP3           S                   LIKE(HST_CAP3)
     D WCF01           S                   LIKE(HST_CF01)
     D WCF02           S                   LIKE(HST_CF02)
     D WCF03           S                   LIKE(HST_CF03)
     D WCF04           S                   LIKE(HST_CF04)
     D WCF05           S                   LIKE(HST_CF05)
     D WCF06           S                   LIKE(HST_CF06)
     D WCF07           S                   LIKE(HST_CF07)
     D WCF0800         S                   LIKE(OR_CF0800)
     D WCF0830         S                   LIKE(OR_CF0830)
     D WCF08           S                   LIKE(HST_CF08)
     D WCF09           S                   LIKE(HST_CF09)
     D WCF10           S                   LIKE(HST_CF10)
     D WCF11           S                   LIKE(HST_CF11)
     D WCF12           S                   LIKE(HST_CF12)
     D WCF13           S                   LIKE(HST_CF13)
     D
     F*==========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCdProd
     C                   PARM                    WRamo
     C                   PARM                    WApolice
     C                   PARM                    WDatInicio
     C                   PARM                    WPtrHist
     C                   PARM                    WPtrOR
      /FREE

         //----------------------------------------------------------
         // Escreve Limites de Indemnização
         //----------------------------------------------------------
         WIcTitulo  = *ON;
         WTipCobert = *BLANKS;
         DigTexto(WRamo:WApolice:*ZEROS:'L':*ZEROS); // Escrever Limites

         //----------------------------------------------------------
         // Escreve Franquias
         //----------------------------------------------------------
         WIcTitulo = *ON;
         WTipCobert = *BLANKS;
         DigTexto(WRamo:WApolice:*ZEROS:'F':*ZEROS); // Escrever Franquias

         $AddTextLine();

         $AddLineType('T1');
         $AddTextLine('O Limite de Indemnização para Objectos de risco +
                       agravado não discriminados (Cláus. 19ª. das Condiçõe+
                       s Gerais): 30% do valor total do Conteúdo. No seu +
                       conjunto e ¤1.500,00 por Objectos ou conjunto de +
                       Objectos da mesma Espécie.');
         $AddTextLine();
         *INLR = *ON;
         RETURN;

      /END-FREE
     P*=======================================================
     P* Processo..: DigTexto
     P* Objectivo.: Precorre a árvore e escreve o que for
     P*             Seleccionado para escrever
     P*=======================================================
     P DigTexto        B                   EXPORT
     D DigTexto        PI              N
     D   PRM_CdRamo                   2S 0 CONST
     D   PRM_Apolice                  7S 0 CONST
     D   PRM_CdSuper                  5S 0 CONST
     D   PRM_Tipo                     1A   CONST
     D   PRM_Nivel                    5S 0 CONST
     D
     D*==> Registos de Catalogo
     D Lst#CAT         DS                  QUALIFIED DIM(50)
     D   CDPROD                            LIKE(CAT_CDPROD)
     D   CDLIMITE                          LIKE(CAT_CDLIMITE)
     D   CDSUPER                           LIKE(CAT_CDSUPER)
     D   CDCOBERT                          LIKE(CAT_CDCOBERT)
     D   CDRISCO                           LIKE(CAT_CDRISCO)
     D   DSLIMITE                          LIKE(CAT_DSLIMITE)
     D   CDINCIND                          LIKE(CAT_CDINCIND)
     D   VLPERIND                          LIKE(CAT_VLPERIND)
     D   VLMAXIND                          LIKE(CAT_VLMAXIND)
     D   ICESCIND                          LIKE(CAT_ICESCIND)
     D   CDINCFRQ                          LIKE(CAT_CDINCFRQ)
     D   VLPERFRQ                          LIKE(CAT_VLPERFRQ)
     D   VLMINFRQ                          LIKE(CAT_VLMINFRQ)
     D   ICESCFRQ                          LIKE(CAT_ICESCFRQ)
     D   DTINICIO                          LIKE(CAT_CAT10301)
     D   DTTERMO                           LIKE(CAT_CAT10302)
     D   IsEmpty                       N   INZ(*ON)
     D WIdxCat         S              5S 0
     D WCdRisco        S                   LIKE(CAT_CDRISCO)
     D WCdCobert       S                   LIKE(CAT_CDCOBERT)
     D WCdIncInd       S                   LIKE(PRD_CDINCIND)
     D WVlPerInd       S                   LIKE(PRD_VLPERIND)
     D WVlMaxInd       S                   LIKE(PRD_VLMAXIND)
     D WCdIncFrq       S                   LIKE(PRD_CDINCFRQ)
     D WVlPerFrq       S                   LIKE(PRD_VLPERFRQ)
     D WVlMinFrq       S                   LIKE(PRD_VLMINFRQ)
     D WDsIncFrq       S             40A
     D WDsIncInd       S             40A
     D WDsCobert       S            500A
     D WLineType       S              2A
     D WResult         S               N
     D*=======================================================
      /FREE

        //-------------------------------------------------------------
        // Escrever o nó principal e depois os dependentes
        //-------------------------------------------------------------
        CHAIN (WCdProd:PRM_CdSuper) RCATP103;
        IF (%FOUND());
           WCdRisco  = CAT_CDRISCO;
           WCdCobert = CAT_CDCOBERT;
           WDsCobert = CAT_DSLIMITE;

           //-------------------------------------------
           // Obter a descrição para os Códigos
           //-------------------------------------------
           CHAIN (WCdProd:WCdCobert) RCATP102;
           IF (%FOUND());
              IF (WDsCobert = *BLANKS);
                 WDsCobert = CAT_DSCOBERT;
               ENDIF;
           ENDIF;
           $LstNivel(PRM_Nivel).Titulo  = WDsCobert;
           $LstNivel(PRM_Nivel).IsWrite = *OFF;

           //-------------------------------------------
           // Confirmar se é para escrever
           //-------------------------------------------
           IF (IsToSelect(PRM_CdRamo:PRM_Apolice:CAT_CDCOBERT:CAT_CDRISCO));
              CHAIN (PRM_CdRamo:PRM_Apolice:PRM_CdSuper) RPRDP01401;
              IF (%FOUND());
                 WCdIncInd = PRD_CDINCIND;
                 WVlPerInd = PRD_VLPERIND;
                 WVlMaxInd = PRD_VLMAXIND;
                 WCdIncFrq = PRD_CDINCFRQ;
                 WVlPerFrq = PRD_VLPERFRQ;
                 WVlMinFrq = PRD_VLMINFRQ;
                 EXSR WRiteText;
              ELSEIF ((CAT_ICESCIND = 'S') AND (PRM_TIPO = 'L')) OR
                     ((CAT_ICESCFRQ = 'S') AND (PRM_TIPO = 'F'));
                 WCdIncInd = CAT_CDINCIND;
                 WVlPerInd = CAT_VLPERIND;
                 WVlMaxInd = CAT_VLMAXIND;
                 WCdIncFrq = CAT_CDINCFRQ;
                 WVlPerFrq = CAT_VLPERFRQ;
                 WVlMinFrq = CAT_VLMINFRQ;
                 EXSR WRiteText;
              ENDIF;
           ENDIF;
        ENDIF;

        //-------------------------------------------------------------
        // Carregar o Conteudo da Tabela para permitir a recursividade
        //-------------------------------------------------------------
        WIdxCat = *ZEROS;
        //SETLL (WCdProd:PRM_CdSuper) RCATP10301;
        //READE (WCdProd:PRM_CdSuper) RCATP10301;
        IF (PRM_CDSUPER > *ZEROS);
           SETLL (WCdProd:PRM_CdSuper) RCATP10301;
           READE (WCdProd:PRM_CdSuper) RCATP10301;
        ELSE;
           SETLL (WCdProd) RCATP10301;
           READE (WCdProd) RCATP10301;
        ENDIF;
        DOW (NOT %EOF());
           IF (PRM_CDSUPER=*ZEROS AND (CAT_CDLIMITE = CAT_CDSUPER)) OR
              (PRM_CDSUPER <> *ZEROS AND (CAT_CDLIMITE<> CAT_CDSUPER));
           IF (IsToSelect(PRM_CdRamo:PRM_Apolice:CAT_CDCOBERT:CAT_CDRISCO));
              WIdxCat += 1;
              Lst#CAT(WIdxCat).CDPROD    = CAT_CDPROD;
              Lst#CAT(WIdxCat).CDLIMITE  = CAT_CDLIMITE;
              Lst#CAT(WIdxCat).CDSUPER   = CAT_CDSUPER;
              Lst#CAT(WIdxCat).CDCOBERT  = CAT_CDCOBERT;
              Lst#CAT(WIdxCat).CDRISCO   = CAT_CDRISCO;
              Lst#CAT(WIdxCat).DSLIMITE  = CAT_DSLIMITE;
              Lst#CAT(WIdxCat).CDINCIND  = CAT_CDINCIND;
              Lst#CAT(WIdxCat).VLPERIND  = CAT_VLPERIND;
              Lst#CAT(WIdxCat).VLMAXIND  = CAT_VLMAXIND;
              Lst#CAT(WIdxCat).ICESCIND  = CAT_ICESCIND;
              Lst#CAT(WIdxCat).CDINCFRQ  = CAT_CDINCFRQ;
              Lst#CAT(WIdxCat).VLPERFRQ  = CAT_VLPERFRQ;
              Lst#CAT(WIdxCat).VLMINFRQ  = CAT_VLMINFRQ;
              Lst#CAT(WIdxCat).ICESCFRQ  = CAT_ICESCFRQ;
              Lst#CAT(WIdxCat).DTINICIO  = CAT_CAT10301;
              Lst#CAT(WIdxCat).DTTERMO   = CAT_CAT10302;
              Lst#CAT(WIdxCat).IsEmpty   = *OFF;
           ENDIF;
           ENDIF;
        // READE (WCdProd:PRM_CdSuper) RCATP10301;
           IF (PRM_CDSUPER > *ZEROS);
              READE (WCdProd:PRM_CdSuper) RCATP10301;
           ELSE;
              READE (WCdProd) RCATP10301;
           ENDIF;
        ENDDO;


        WIdxCat = 1;
        DOW (NOT Lst#CAT(WIdxCat).IsEmpty);
           DigTexto(PRM_CdRamo:PRM_Apolice:Lst#CAT(WIdxCat).CDLIMITE:
                 PRM_Tipo:(PRM_NIVEL + 1));
           WIdxCat += 1;
        ENDDO;

       RETURN WResult;

       //==================================================================
       // Subrotina..: WriteText
       // Objectivo..: Escrever a linha de Texto
       //==================================================================
       BEGSR WriteText;
          //-------------------------------------------
          // Obter a descrição para os Códigos
          //-------------------------------------------
          WDsCobert = CAT_DSLIMITE;
          CHAIN (WCdProd:WCdCobert) RCATP102;
          IF (%FOUND());
             IF (WDsCobert = *BLANKS);
                WDsCobert = CAT_DSCOBERT;
             ENDIF;
          ENDIF;

          //-------------------------------------------
          // Formatar os limites de Indemnização
          //-------------------------------------------
          WLinhaTxt = *BLANKS;
          IF (PRM_Tipo = 'L'); // Limites
             WVlMinFrq = *ZEROS;
             WDsIncInd = $GetDescricao(23:WCdIncInd);
             IF (WVlPerInd > *ZEROS);
                IF (WVlPerInd = 100);
                   WLinhaTxt = %TRIM(WDsIncInd);
                ELSE;
                   WLinhaTxt = %TRIM(%EDITW(WVlPerInd:'  0,  %'))+
                                      ' sobre o '+%TRIM(WDsIncInd);
                ENDIF;
             ELSEIF (WCdIncInd <> *BLANKS);
                WlinhaTxt = %TRIM(WDsIncInd);
             ENDIF;
          ELSE;
             //-------------------------------------------
             // Formatar as Franquias
             //-------------------------------------------
             WVlMaxInd = *ZEROS;
             WDsIncFrq = $GetDescricao(24:WCdIncFrq);
             IF (WVlPerFrq > *ZEROS);
                IF (WVlPerFrq = 100);
                   WLinhaTxt=%TRIM(WDsIncFrq);
                ELSE;
                   WLinhaTxt = %EDITW(WVlPerFrq:'  0,  %')+
                               ' sobre o '+%TRIM(WDsIncFrq);
                ENDIF;
             ELSEIF (WCdIncFrq <> *BLANKS);
                WlinhaTxt = %TRIM(WDsIncFrq);
             ENDIF;
          ENDIF;

          //-------------------------------------------
          // Escrever o Texto
          //-------------------------------------------
          IF ((%TRIM(WLinhaTxt) <> *BLANKS) OR (WVlMinFrq > *ZEROS)
                OR (WVlMaxInd > *ZEROS));
             EXSR WriteHDR;
             $AddLineType('B' + %CHAR(PRM_Nivel));
             IF (%TRIM(WLinhaTxt) <> *BLANKS);
                IF (WVlMaxInd > *ZEROS);
                   $AddTextLine(WDsCobert);
                   $AddLineType('R1');
                   WLinhaTxt = %TRIM(WLinhaTxt) + ' até um máximo de';
                   $AddTextLine(WLinhaTxt:1:*BLANKS:
                           %EditW(WVlMaxInd:'             '):'VL');
                ELSEIF (WVlMinFrq > *ZEROS);
                   $AddTextLine(WDsCobert);
                   $AddLineType('R1');
                   WLinhaTxt = %TRIM(WLinhaTxt) + ' no mínimo de';
                   $AddTextLine(WLinhaTxt:1:*BLANKS:
                           %EditW(WVlMinFrq:'             '):'VL');
                ELSEIF (WCdIncFrq <> *BLANKS);
                   $AddTextLine(WDsCobert);
                   $AddLineType('R1');
                   $AddTextLine(WLinhaTxt);
                ELSEIF (WCdIncInd <> *BLANKS);
                   $AddTextLine(WDsCobert);
                   $AddLineType('R1');
                   $AddTextLine(WLinhaTxt);
                ELSE;
                   $AddTextLine(WDsCobert:1:*BLANKS:WLinhaTxt);
                ENDIF;
             ELSE;
                IF (WVlMaxInd > *ZEROS);
                   $AddTextLine(WDsCobert:1:*BLANKS:
                           %EditW(WVlMaxInd:'             '):'VL');
                ELSEIF (WVlMinFrq > *ZEROS);
                   $AddTextLine(WDsCobert:1:*BLANKS:
                           %EditW(WVlMinFrq:'             '):'VL');
                ENDIF;
             ENDIF;
          ENDIF;
          WLinhaTxt = *BLANKS;
       ENDSR;

       //==================================================================
       // Subrotina..: WriteHdr
       // Objectivo..: Escrever o Titulo e Subtitulo
       //==================================================================
       BEGSR WriteHDR;
          IF (WIcTitulo);
             $AddTextLine();
             IF (PRM_Tipo = 'L'); // Limites
                $AddLineType('H1');
                $AddTextLine('LIMITES DE INDEMNIZAÇÃO');
                $AddLineType('T1');
                $AddTextLine(
                       'As Coberturas Abaixo enumeradas estão sujeitas aos +
                        limites de Indemnização (por sinistro e anuidade) +
                        indicados:');
                $AddLineType('H2');
                $addException(*ON);
                WLinhaHdr = *BLANKS;
                %SUBST(WLinhaHdr:3) = 'COBERTURAS';
                %SUBST(WLinhaHdr:78) = 'INDEMNIZAÇÃO';
                $AddTextLine(WLinhaHdr);
                $addException(*OFF);
                $AddLineType('T2');
             ELSE;
                $AddLineType('H1');
                $AddTextLine('FRANQUIAS');
                $AddLineType('T1');
                $AddTextLine(
                        'Franquias a deduzir, em cada Sinistro, na Indemniz+
                         ação a pagar:');
                $AddLineType('H2');
                $addException(*ON);
                WLinhaHdr = *BLANKS;
                %SUBST(WLinhaHdr:3) = 'COBERTURAS';
                %SUBST(WLinhaHdr:82) = 'FRANQUIA';
                $AddTextLine(WLinhaHdr);
                $addException(*OFF);
                $AddLineType('T2');
             ENDIF;
             WIcTitulo = *OFF;
          ENDIF;

          //-------------------------------------------------
          // Escrever o Header do Parágrafo se não existir
          //-------------------------------------------------
          $LstNivel(PRM_Nivel).IsWrite = *ON;
          IF (PRM_Nivel > 1);
             WActNivel = PRM_Nivel - 1;
             DOW (WActNivel > *ZEROS);
                IF (NOT $LstNivel(WActNivel).IsWrite);
                   $LstNivel(WActNivel).IsWrite = *ON;
                   $AddLineType('B'+%CHAR(WActNivel));
                   $AddTextLine($LstNivel(WActNivel).Titulo:1:
                                $LstNivel(WActNivel).CdAlias);
                ENDIF;
                WActNivel -= 1;
             ENDDO;
          ENDIF;
       ENDSR;
      /END-FREE
     P DigTexto        E
     P*=======================================================
     P* Processo..: IsToSelect
     P* Objectivo.: É para seleccionar porque tem os riscos
     P*=======================================================
     P IsToSelect      B                   EXPORT
     D IsToSelect      PI              N
     D   PRM_CdRamo                   2S 0 CONST
     D   PRM_Apolice                  7S 0 CONST
     D   PRM_CdCobert                 5S 0 CONST
     D   PRM_CdRisco                  5S 0 CONST
     D
     D WResult         S               N
      /FREE
         WResult = *OFF;
         ptrHist = WptrHist;
         ptrOr   = WptrOR;
         MONITOR;
            WCR1  = HST_CR1;
            WCAP1 = HST_CAP1;
            WCR2  = HST_CR2;
            WCAP2 = HST_CAP2;
            WCR3  = HST_CR3;
            WCAP3 = HST_CAP3;
            WCF01 = HST_CF01;
            WCF02 = HST_CF02;
            WCF03 = HST_CF03;
            WCF04 = HST_CF04;
            WCF05 = HST_CF05;
            WCF06 = HST_CF06;
            WCF07 = HST_CF07;
            WCF0800=OR_CF0800;
            WCF0830=OR_CF0830;
            WCF08 = HST_CF08;
            WCF09 = HST_CF09;
            WCF10 = HST_CF10;
            WCF11 = HST_CF11;
            WCF12 = HST_CF12;
            WCF13 = HST_CF13;
         ON-ERROR;
            WCR1  = OR_CR1;
            WCAP1 = OR_CAP1;
            WCR2  = OR_CR2;
            WCAP2 = OR_CAP2;
            WCR3  = OR_CR3;
            WCAP3 = OR_CAP3;
            WCF01 = OR_CF01;
            WCF02 = OR_CF02;
            WCF03 = OR_CF03;
            WCF04 = OR_CF04;
            WCF05 = OR_CF05;
            WCF06 = OR_CF06;
            WCF07 = OR_CF07;
            WCF0800=OR_CF0800;
            WCF0830=OR_CF0830;
            WCF08 = OR_CF08;
            WCF09 = OR_CF09;
            WCF10 = OR_CF10;
            WCF11 = OR_CF11;
            WCF12 = OR_CF12;
            WCF13 = OR_CF13;
         ENDMON;


         //-----------------------------------------------
         // Validar se o Risco está contratado
         //-----------------------------------------------
         IF (PRM_CdRisco <> *ZEROS);
            IF ((WCR1 = PRM_CdRisco) OR (WCR2 = PRM_CdRisco) OR
               (WCR3 = PRM_CdRisco));
               WResult = *ON;
            ENDIF;
         ELSE;
            WResult = *ON;
         ENDIF;

         //-----------------------------------------------
         // Validar se a Cobertura faz parte do Contrato
         //-----------------------------------------------
         IF (WResult = *ON);
            CHAIN (WCdProd:PRM_CdCobert) RCATP102;
            IF (%FOUND());
               IF (CAT_CDNATURA <> 'B');
                  IF (CAT_ALIAS = '01') AND (WCF01<>'S');
                     WResult = *OFF;
                  ELSEIF (CAT_ALIAS='02') AND (WCF02<>'S');
                     WResult = *OFF;
                  ELSEIF (CAT_ALIAS='03') AND (WCF03<>'S');
                     WResult = *OFF;
                  ELSEIF (CAT_ALIAS='04') AND (WCF04<>'S');
                     WResult = *OFF;
                  ELSEIF (CAT_ALIAS='05') AND (WCF05<>'S');
                     WResult = *OFF;
                  ELSEIF (CAT_ALIAS='06') AND (WCF06<>'S');
                     WResult = *OFF;
              //  ELSEIF (CAT_ALIAS='07') AND (WCF07<>'S');
              //     WResult = *OFF;
                  ELSEIF (CAT_ALIAS='0800') AND (WCF0800<>'S');
                     WResult = *OFF;
                  ELSEIF (CAT_ALIAS='0830') AND (WCF0830<>'S');
                     WResult = *OFF;
                  ELSEIF (CAT_ALIAS='08') AND (WCF08<>'S');
                     WResult = *OFF;
                  ELSEIF (CAT_ALIAS='09') AND (WCF09<>'S');
                     WResult = *OFF;
                  ELSEIF (CAT_ALIAS='10') AND (WCF10<>'S');
                     WResult = *OFF;
                  ELSEIF (CAT_ALIAS='11') AND (WCF11<>'S');
                     WResult = *OFF;
                  ELSEIF (CAT_ALIAS='12') AND (WCF12<>'S');
                     WResult = *OFF;
                  ELSEIF (CAT_ALIAS='13') AND (WCF13<>'S');
                     WResult = *OFF;
                  ENDIF;
               ENDIF;
            ENDIF;
         ENDIF;
         RETURN WResult;
      /END-FREE
     P IsToSelect      E
