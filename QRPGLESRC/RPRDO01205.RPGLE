     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: RPRDO012
     H* Descri��o.: Actas - Limites e Franquias
     H* Estilos...: L1, L2, L3, R1, H1, T1, H2, T2
     H*==================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SCATO010
      /Copy QRPGLESRC,SPRDO002
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SDBSO010

     D*==> Estrutura de dados do Hist�rico
     D RGEXAP        E DS                  ExtName(GEXAP)
     D                                     Prefix(HST_)
     D                                     Based(ptrHist)
     D
     D*==> Estrutura de dados do Registo
     D RBASEDIO      E DS                  ExtName(GBASEDIO)
     D                                     Prefix(OR_)
     D                                     Based(ptrOR)
     D
     D*==> Validar se � para seleccionar
     D DigTexto        PR              N
     D   PRM_CdRamo                   2S 0 CONST
     D   PRM_Apolice                  7S 0 CONST
     D   PRM_CdSuper                  5S 0 CONST
     D   PRM_Tipo                     1A   CONST
     D   PRM_Nivel                    5S 0 CONST
     D   PRM_DatIni                   8S 0 CONST
     D
     D*==> Validar se � para seleccionar
     D IsToSelect      PR              N
     D   PRM_CdRamo                   2S 0 CONST
     D   PRM_Apolice                  7S 0 CONST
     D   PRM_CdCobert                 5S 0 CONST
     D   PRM_CdRisco                  5S 0 CONST
     D   PRM_DatIni                   8S 0 CONST
     D   PRM_CdNatura                 1A   CONST
     D
     D*==> Controlo de Niveis
     D $LstNivel       DS                  QUALIFIED DIM(10)
     D   Titulo                     100A   INZ(*BLANKS)
     D   CdAlias                      6A   INZ(*BLANKS)
     D   IsWrite                       N   INZ(*OFF)
     D WActNivel       S              5S 0
     D
     D*==> Restantes Vair�veis Globais
     D WDesCobert      S            100A
     D WLinhaTxt       S            500A
     D WLinhaHdr       S            500A
     D WCdProd         S              5S 0
     D WIcTitulo       S               N
     D WTipCobert      S              1A
     D WptrHist        S               *
     D WptrOr          S               *
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
         // Escreve Limites de Indemniza��o
         //----------------------------------------------------------
         WIcTitulo  = *ON;
         WTipCobert = *BLANKS;
         DigTexto(WRamo:WApolice:*ZEROS:'L':*ZEROS:WDatInicio);

         //----------------------------------------------------------
         // Escreve Franquias
         //----------------------------------------------------------
         WIcTitulo = *ON;
         WTipCobert = *BLANKS;
         DigTexto(WRamo:WApolice:*ZEROS:'F':*ZEROS:WDatInicio);

         $AddTextLine();

         RETURN;

      /END-FREE
     P*=======================================================
     P* Processo..: DigTexto
     P* Objectivo.: Precorre a �rvore e escreve o que for
     P*             Seleccionado para escrever
     P*=======================================================
     P DigTexto        B                   EXPORT
     D DigTexto        PI              N
     D   PRM_CdRamo                   2S 0 CONST
     D   PRM_Apolice                  7S 0 CONST
     D   PRM_CdSuper                  5S 0 CONST
     D   PRM_Tipo                     1A   CONST
     D   PRM_Nivel                    5S 0 CONST
     D   PRM_DatIni                   8S 0 CONST
     D
     D*==> Registos de Catalogo
     D Lst#CAT         DS                  QUALIFIED DIM(50)
     D   CDPROD                       5S 0 INZ(*ZEROS)
     D   CDLIMITE                     5S 0 INZ(*ZEROS)
     D   CDSUPER                      5S 0 INZ(*ZEROS)
     D   CDCOBERT                     5S 0 INZ(*ZEROS)
     D   CDRISCO                      5S 0 INZ(*ZEROS)
     D   ICSELECT                     1A   INZ(*BLANKS)
     D   DSLIMITE                   100A   INZ(*BLANKS)
     D   CDINCIND                     2A   INZ(*BLANKS)
     D   VLPERIND                     5S 0 INZ(*ZEROS)
     D   VLMAXIND                    11S 2 INZ(*ZEROS)
     D   ICESCIND                     1A   INZ(*BLANKS)
     D   CDINCFRQ                     2A   INZ(*BLANKS)
     D   VLPERFRQ                     5S 2 INZ(*ZEROS)
     D   VLMINFRQ                    11S 2 INZ(*ZEROS)
     D   ICESCFRQ                     1A   INZ(*BLANKS)
     D   IsEmpty                       N   INZ(*ON)
     D WIdxCat         S              5S 0 INZ(*ZEROS)
     D
     D*==> Campos de Trabalho
     D WCdRisco        S              5S 0 INZ(*ZEROS)
     D WCdCobert       S              5S 0 INZ(*ZEROS)
     D WCdSuper        S              5S 0 INZ(*ZEROS)
     D WCdLimite       S              5S 0 INZ(*ZEROS)
     D WCdIncInd       S              2A   INZ(*BLANKS)
     D WVlPerInd       S              5S 2 INZ(*ZEROS)
     D WVlMaxInd       S             11S 2 INZ(*ZEROS)
     D WCdIncFrq       S              2A   INZ(*BLANKS)
     D WVlPerFrq       S              5S 2 INZ(*ZEROS)
     D WVlMinFrq       S             11S 2 INZ(*ZEROS)
     D WDsIncFrq       S             40A   INZ(*BLANKS)
     D WDsIncInd       S             40A   INZ(*BLANKS)
     D WDsCobert       S            500A   INZ(*BLANKS)
     D WLineType       S              2A   INZ(*BLANKS)
     D WIcEscFrq       S              1A   INZ(*BLANKS)
     D WIcEscInd       S              1A   INZ(*BLANKS)
     D WSessCat1       S              5S 0 INZ(*ZEROS)
     D WSessCat2       S              5S 0 INZ(*ZEROS)
     D WSessPrd1       S              5S 0 INZ(*ZEROS)
     D WIdxAct         S              3S 0 INZ(*ZEROS)
     D WCdNatura       S              1A   INZ(*BLANKS)
     D WResult         S               N   INZ(*OFF)
     D*=======================================================
      /FREE
        //-------------------------------------------------------------
        // Escrever o n� principal e depois os dependentes
        //-------------------------------------------------------------
        WSessCat1 = $SetFiles('FCATP103 A INNER JOIN FCATP102 B +
                               ON (A.CDPROD = B.CDPROD) AND +
                               (A.CDCOBERT = B.CDCOBERT)');
        $AddDBDate($IntToDate(PRM_DatIni));
        $SETFIELDS('FCATP103':'A');
        $SETFIELDS('FCATP102':'B':*OFF);
        // $SETFIELDS('B.CDNATURA,B.DSCOBERT':'B':*OFF);
        $SetCriteria('(A.CDPROD='''+%CHAR(WCdProd)+''') AND +
                      (A.CDLIMITE='''+%CHAR(PRM_CdSuper)+''')');
        IF ($GetRecord() > *ZEROS);
           WCdRisco  = $GetFieldNum('A.CDRISCO');
           WCdCobert = $GetFieldNum('A.CDCOBERT');
           WCdIncInd = $GetField('A.CDINCIND');
           WVlPerInd = $GetFieldNum('A.VLPERIND');
           WCdIncFrq = $GetField('A.CDINCFRQ');
           WCdLimite = $GetFieldNum('A.CDLIMITE');
           WVlPerFrq = $GetFieldNum('A.VLPERFRQ');
           WVlMinFrq = $GetFieldNum('A.VLMINFRQ');
           WDsCobert = $GetField('A.DSLIMITE');
           IF (WDsCobert = *BLANKS);
               WDsCobert = $GetField('B.DSCOBERT');
           ENDIF;
           WIcEscInd = $GetField('A.ICESCIND');
           WIcEscFrq = $GetField('A.ICESCFRQ');
           WCdNatura = $GetField('B.CDNATURA');
           WVlMaxInd = $GetFieldNum('A.VLMAXIND');

           $LstNivel(PRM_Nivel).Titulo  = WDsCobert;
           $LstNivel(PRM_Nivel).IsWrite = *OFF;

           //-------------------------------------------
           // Confirmar se � para escrever
           //-------------------------------------------
           IF (IsToSelect(PRM_CdRamo:PRM_Apolice:WCdCobert:WCdRisco:
                          PRM_DatIni:WCdNatura));
              WSessPrd1 = $SetFiles('FPRDP014');
              $AddDBDate($IntToDate(PRM_DatIni));
              $SetCriteria('(CDRAMO ='''+%CHAR(PRM_CDRAMO)+''') AND +
                            (NUAPOL ='''+%CHAR(PRM_Apolice)+''') AND +
                            (CDLIMITE='''+%CHAR(PRM_CdSuper)+''')');
              IF ($GetRecord() > *ZEROS);
                 WCdIncInd = $GetField('CDINCIND');
                 WVlPerInd = $GetFieldNum('VLPERIND');
                 WCdIncFrq = $GetField('CDINCFRQ');
                 WVlPerFrq = $GetFieldNum('VLPERFRQ');
                 WVlMinFrq = $GetFieldNum('VLMINFRQ');
                 WVlMaxInd = $GetFieldNum('VLMAXIND');
                 EXSR WRiteText;
              ELSEIF ((WIcEscInd = 'S') AND (PRM_TIPO = 'L')) OR
                     ((WICescFrq = 'S') AND (PRM_TIPO = 'F'));
                 EXSR WRiteText;
              ENDIF;
              $RmvDBSession(WSessPrd1);
           ENDIF;
        ENDIF;
        $RmvDBSession(WSessCat1);

        //-------------------------------------------------------------
        // Carregar o Conteudo da Tabela para permitir a recursividade
        //-------------------------------------------------------------
        WIdxCat   = *ZEROS;
        WIdxAct   = 1;
        WSessCat2 = $SetFiles('FCATP103 A INNER JOIN FCATP102 B +
                               ON (A.CDPROD = B.CDPROD) AND +
                               (A.CDCOBERT = B.CDCOBERT)');
        $AddDBDate($IntToDate(PRM_DatIni));
        $SETFIELDS('FCATP103':'A');
        $SETFIELDS('FCATP102':'B':*OFF);
        IF (PRM_CDSUPER > *ZEROS);
           $SetCriteria('(A.CDPROD='''+%CHAR(WCdProd)+''') AND +
                         (A.CDSUPER='''+%CHAR(PRM_CDSUPER)+''') AND +
                         (A.CDLIMITE <> A.CDSUPER)');
        ELSE;
           $SetCriteria('(A.CDPROD='''+%CHAR(WCdProd)+''') AND +
                         (A.CDLIMITE = A.CDSUPER)');
        ENDIF;
        DOW ($GetRecord(WIdxAct) > *ZEROS);
           WCdNatura = $GetField('B.CDNATURA');
           WCdSuper  = $GetFieldNum('A.CDSUPER');
           WCdLimite = $GetFieldNum('A.CDLIMITE');
           WCdCobert = $GetFieldNum('A.CDCOBERT');
           WCdRisco  = $GetFieldNum('A.CDRISCO');
           IF (IsToSelect(PRM_CdRamo:PRM_Apolice:WCdCobert:WCdRisco:
                          PRM_DatIni:WCdNatura));
              WIdxCat += 1;
              Lst#CAT(WIdxCat).CDPROD    = $GetFieldNum('A.CDPROD');
              Lst#CAT(WIdxCat).CDLIMITE  = $GetFieldNum('A.CDLIMITE');
              Lst#CAT(WIdxCat).CDSUPER   = $GetFieldNum('A.CDSUPER');
              Lst#CAT(WIdxCat).CDCOBERT  = $GetFieldNum('A.CDCOBERT');
              Lst#CAT(WIdxCat).CDRISCO   = $GetFieldNum('A.CDRISCO');
              Lst#CAT(WIdxCat).ICSELECT  = $GetField('A.ICSELECT');
              Lst#CAT(WIdxCat).DSLIMITE  = $GetField('A.DSLIMITE');
              Lst#CAT(WIdxCat).CDINCIND  = $GetField('A.CDINCIND');
              Lst#CAT(WIdxCat).VLPERIND  = $GetFieldNum('A.VLPERIND');
              Lst#CAT(WIdxCat).VLMAXIND  = $GetFieldNum('A.VLMAXIND');
              Lst#CAT(WIdxCat).ICESCIND  = $GetField('A.ICESCIND');
              Lst#CAT(WIdxCat).CDINCFRQ  = $GetField('A.CDINCFRQ');
              Lst#CAT(WIdxCat).VLPERFRQ  = $GetFieldNum('A.VLPERFRQ');
              Lst#CAT(WIdxCat).VLMINFRQ  = $GetFieldNum('A.VLMINFRQ');
              Lst#CAT(WIdxCat).ICESCFRQ  = $GetField('ICESCFRQ');
              Lst#CAT(WIdxCat).IsEmpty   = *OFF;
           ENDIF;
           WIdxAct += 1;
        ENDDO;
        $RmvDBSession(WSessCat2);


        WIdxCat = 1;
        DOW (NOT Lst#CAT(WIdxCat).IsEmpty);
           DigTexto(PRM_CdRamo:PRM_Apolice:Lst#CAT(WIdxCat).CDLIMITE:
                 PRM_Tipo:(PRM_NIVEL + 1):PRM_DatIni);
           WIdxCat += 1;
        ENDDO;

       RETURN WResult;

       //==================================================================
       // Subrotina..: WriteText
       // Objectivo..: Escrever a linha de Texto
       //==================================================================
       BEGSR WriteText;

          //-------------------------------------------
          // ForCompr os limites de Indemniza��o
          //-------------------------------------------
          WLinhaTxt = *BLANKS;
          IF (PRM_Tipo = 'L'); // Limites
             WVlMinFrq = *ZEROS;
          // WDsIncInd = $GetDescricao(23:WCdIncInd);
          // IF (WVlPerInd > *ZEROS);
          //    IF (WVlPerInd = 100);
          //       EXSR GetCapRisco;
          //       WLinhaTxt = %TRIM(WDsIncInd);
          //    ELSE;
          //       WLinhaTxt = %TRIM(%EDITW(WVlPerInd:'  0,  %'))+
          //                          ' sobre o '+%TRIM(WDsIncInd);
          //    ENDIF;
          // ELSEIF (WCdIncInd <> *BLANKS);
          //    WlinhaTxt = %TRIM(WDsIncInd);
          // ENDIF;
          ELSE;
             //-------------------------------------------
             // ForCompr as Franquias
             //-------------------------------------------
             WVlMaxInd = *ZEROS;
          // WDsIncFrq = $GetDescricao(24:WCdIncFrq);
          // IF (WVlPerFrq > *ZEROS);
          //    IF (WVlPerFrq = 100);
          //       WLinhaTxt=%TRIM(WDsIncFrq);
          //    ELSE;
          //       WLinhaTxt = %EDITW(WVlPerFrq:'  0,  %')+
          //                   ' sobre o '+%TRIM(WDsIncFrq);
          //    ENDIF;
          // ELSEIF (WCdIncFrq <> *BLANKS);
          //    WlinhaTxt = %TRIM(WDsIncFrq);
          // ENDIF;
          ENDIF;

          //-------------------------------------------
          // Escrever o Texto
          //-------------------------------------------
          IF ((%TRIM(WLinhaTxt) <> *BLANKS) OR (WVlMinFrq > *ZEROS)
                OR (WVlMaxInd > *ZEROS));
             EXSR WriteHDR;
             $AddLineType('L' + %CHAR(PRM_Nivel));
             IF (%TRIM(WLinhaTxt) = *BLANKS);
                //-------------------------------------------------------------
                // Obter o valor real de Indemniza��o
                //-------------------------------------------------------------
                IF (WVlMaxInd > *ZEROS);
                   WVlMaxInd = $GetCapitalNum(WCdProd:WRamo:WApolice:
                                              'L':WCdCobert:WCdLimite:*ON:
                                              WDatInicio);
                ENDIF;
                IF (WVlMinFrq > *ZEROS);
                   WVlMinFrq = $GetCapitalNum(WCdProd:WRamo:WApolice:
                                              'F':WCdCobert:WCdLimite:*ON:
                                              WDatInicio);
                ENDIF;

                IF (WVlMaxInd > *ZEROS);
                   $AddLineType('R1');
                   WLinhaTxt = %TRIM(WDsCobert) + ' ' +
                               %TRIM(WLinhaTxt); // + ' at� um m�ximo de';
                   $AddTextLine(WLinhaTxt:1:*BLANKS:
                           %EditW(WVlMaxInd:'             '):'VL');
                ELSEIF (WVlMinFrq > *ZEROS);
                   $AddLineType('R1');
                   WLinhaTxt = %TRIM(WDsCobert) + ' ' +
                               %TRIM(WLinhaTxt); // + ' no m�nimo de';
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
             // IF (WVlMaxInd > *ZEROS);
             //    $AddTextLine(WDsCobert:1:*BLANKS:
             //            %EditW(WVlMaxInd:'             '):'VL');
             // ELSEIF (WVlMinFrq > *ZEROS);
             //    $AddTextLine(WDsCobert:1:*BLANKS:
             //            %EditW(WVlMinFrq:'             '):'VL');
             // ENDIF;
             ENDIF;
          ENDIF;
          WLinhaTxt = *BLANKS;
       ENDSR;

       //==================================================================
       // Subrotina..: GetCapRisco
       // Objectivo..: Obter o valor para o Capital de Risco
       //==================================================================
       BEGSR getCapRisco;
          WLinhaTxt = %TRIM(WDsIncInd);
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
                $AddTextLine('LIMITES DE INDEMNIZA��O');
                $AddLineType('T1');
                $AddTextLine(
                       'As Coberturas Abaixo enumeradas est�o sujeitas aos +
                        limites de Indemniza��o (por sinistro e anuidade) +
                        indicados:');
                $AddLineType('H2');
                $addException(*ON);
                WLinhaHdr = *BLANKS;
                %SUBST(WLinhaHdr:3) = 'COBERTURAS';
                %SUBST(WLinhaHdr:81) = 'INDEMNIZA��O';
                $AddTextLine(WLinhaHdr);
                $addException(*OFF);
                $AddLineType('T2');
             ELSE;
                $AddLineType('H1');
                $AddTextLine('FRANQUIAS');
                $AddLineType('T1');
                $AddTextLine(
                        'Franquias a deduzir, em cada Sinistro, na Indemniz+
                         a��o a pagar:');
                $AddLineType('H2');
                $addException(*ON);
                WLinhaHdr = *BLANKS;
                %SUBST(WLinhaHdr:3) = 'COBERTURAS';
                %SUBST(WLinhaHdr:85) = 'FRANQUIA';
                $AddTextLine(WLinhaHdr);
                $addException(*OFF);
                $AddLineType('T2');
             ENDIF;
             WIcTitulo = *OFF;
          ENDIF;

          //-------------------------------------------------
          // Escrever o Header do Par�grafo se n�o existir
          //-------------------------------------------------
          $LstNivel(PRM_Nivel).IsWrite = *ON;
          IF (PRM_Nivel > 1);
             WActNivel = PRM_Nivel - 1;
             DOW (WActNivel > *ZEROS);
                IF (NOT $LstNivel(WActNivel).IsWrite);
                   $LstNivel(WActNivel).IsWrite = *ON;
                   $AddLineType('L'+%CHAR(WActNivel));
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
     P* Objectivo.: � para seleccionar porque tem os riscos
     P*=======================================================
     P IsToSelect      B                   EXPORT
     D IsToSelect      PI              N
     D   PRM_CdRamo                   2S 0 CONST
     D   PRM_Apolice                  7S 0 CONST
     D   PRM_CdCobert                 5S 0 CONST
     D   PRM_CdRisco                  5S 0 CONST
     D   PRM_DatIni                   8S 0 CONST
     D   PRM_CdNatura                 1A   CONST
     D
     D*==> Campos de Trabalho
     D WResult         S               N
     D WSession        S              5S 0
     D*=======================================================
      /FREE

         //-----------------------------------------------
         // Validar se o Risco est� contratado
         //-----------------------------------------------
         WResult = *OFF;
         IF (PRM_CdRisco <> *ZEROS);
            WSession = $SetFiles('FPRDP011');
            $AddDBDate($IntToDate(PRM_DatIni));
            $SetCriteria('(CDRAMO = '''+%CHAR(PRM_CdRamo)+''') AND +
                          (NUAPOL = '''+%CHAR(PRM_Apolice)+''') AND +
                          (CDRISCO= '''+%CHAR(PRM_CdRisco)+''')');
            IF ($GetRecord() > *ZEROS);
               WResult = *ON;
            ENDIF;
            $RmvDBSession(WSession);
         ELSE;
            WResult = *ON;
         ENDIF;

         //-----------------------------------------------
         // Validar se a Cobertura faz parte do Contrato
         //-----------------------------------------------
         IF (WResult = *ON) AND (PRM_CdNatura <> 'B');
            WSession = $SetFiles('FPRDP008');
            $AddDBDate($IntToDate(PRM_DatIni));
            $SetCriteria('(CDRAMO = '''+%CHAR(PRM_CdRamo)+''') AND +
                          (NUAPOL = '''+%CHAR(PRM_Apolice)+''') AND +
                          (CDCOBERT= '''+%CHAR(PRM_CdCobert)+''')');
            IF ($GetRecord() > *ZEROS);
               WResult = *ON;
            ELSE;
               WResult = *OFF;
            ENDIF;
            $RMvDBSession(WSession);
         ENDIF;
         RETURN WResult;
      /END-FREE
     P IsToSelect      E
