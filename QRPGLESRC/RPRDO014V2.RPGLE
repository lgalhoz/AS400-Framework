     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL) FIXNBR(*ZONED)
     H*==================================================================
     H* Programa...: RRDO014
     H* Descrição..: Informação Sobre Coberturas
     H* Observação.: Nova Geração de Programas, com a utilização dos
     H*              Módulos IO para acesso aos dados.
     H*==================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SCATO010
      /Copy QRPGLESRC,SPRDO002
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SDBSO010

     D
     D RGEXAP        E DS                  ExtName(GEXAP)
     D                                     Prefix(HST_)
     D                                     Based(ptrHist)
     D
     D*==> Histórico de Capitais de Risco
     D $CapRisco       DS                  QUALIFIED DIM(10)
     D   Valor                       11S 2 INZ(*ZEROS)
     D   Codigo                       2S 0 INZ(*ZEROS)
     D WIdx            S              2S 0 INZ(*ZEROS)
     D WIdxAct         S              2S 0 INZ(*ZEROS)
     D
     D*==> Estrutura de dados do Registo
     D RBASEDIO      E DS                  ExtName(GBASEDIO)
     D                                     Prefix(OR_)
     D                                     Based(ptrOR)
     D
     D WCdAlias        S             10A
     D WCdProd         S              5S 0
     D WCdNatura       S              1A   INZ('F')
     D WTotCap         S             11S 2
     D WValor          S             11S 2
     D WLinha          S            500A
     D WIcTitulo       S               N
     D WptrHist        S               *
     D WptrOR          S               *
     D WCdCobert       S              5S 0
     D WCdRisco        S              5S 0
     D WVlPerInd       S              5S 2
     D WVlMaxInd       S             11S 2
     D WCapRisco       S             11S 2
     D WCdIdent        S              2A
     D WTexto          S            500A
     D
     D WSession        S              5S 0
     D WSessCatCob     S              5S 0
     D
     D WSessCatTxt     S              5S 0
     D WSessCatLim     S              5S 0
     D WSessPrdLim     S              5S 0
     D WCdLimite       S              5S 0
     D WRegAct1        S              5S 0
     D WRegAct2        S              5S 0
     D WRegAct3        S              5S 0
     F*==========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCdProd
     C                   PARM                    WRamo
     C                   PARM                    WApolice
     C                   PARM                    WDatInicio
     C                   PARM                    WPtrHist
     C                   PARM                    WPtrOR
      /FREE

       ptrHist   = WptrHist;
       ptrOr     = WptrOR;

       WIcTitulo = *OFF;
       EXSR AddTexto;

       RETURN;

       //==================================================================
       // Subrotina..: AddTexto
       // Objectivo..: Adicionar o texto das Coberturas Base e Coberturas
       //              Facultativas.
       //==================================================================
       BEGSR AddTexto;
          //---------------------------------------------------------------
          // Titulo Principal
          //---------------------------------------------------------------
          $AddTextLine();
          $AddLineType('H1');
          $addException(*ON);
          WLinha = 'BENS SEGURO/GARANTIAS';
          %SUBST(WLinha:82) = 'CAPITAIS';
          $AddTextLine(WLinha);

          //---------------------------------------------------------------
          // Riscos da Apólice
          //---------------------------------------------------------------
          $addException(*OFF);
          WRegAct1 = 1;
          $AddLineType('B1');
          WTotCap  = *ZEROS;
          WSession = $SetFiles('FPRDP011');
          $AddDBDate($IntToDate(WDatInicio));
          $SetCriteria('(CDRAMO = '''+%CHAR(WRamo)+''') AND +
                        (NUAPOL = '''+%CHAR(WApolice)+''')');
          DOW ($GetRecord(WRegAct1) > *ZEROS);
             WCdRisco  = $GetFieldNum('CDRISCO');
             WLinha    = $GetDescricao(20:%CHAR(WCdRisco));
             WCapRisco = $GetFieldNum('IMCPRISC');
             WTotCap  += WCapRisco;
             WIdx += 1;
             $CapRisco(Widx).Valor = WCapRisco;
             $CapRisco(Widx).Codigo = $GetFieldNum('CDRISCO');
             $AddTextLine(WLinha:1:*BLANKS:
                          %EditW(WCapRisco:'           '):'VL');
             WRegAct1 += 1;
          ENDDO;
          $RmvDbSession(WSession);

          //---------------------------------------------------------------
          // Coberturas Base
          //---------------------------------------------------------------
          $addException(*OFF);
          $AddLineType('T1');
          WLinha = 'Cobertura Base, conforme Cláusula 2º das +
                    Condições Gerais.';
          $AddTextLine(WLinha);
          $AddTextLine(*BLANKS);

          //---------------------------------------------------------------
          // Coberturas Facultativas
          //---------------------------------------------------------------
          WIcTitulo = *OFF;
          $addException(*OFF);
          WRegAct2 = 1;
          WSession = $SetFiles('FPRDP008 A INNER JOIN FCATP102 B ON +
                                (A.CDCOBERT = B.CDCOBERT)');
          $AddDBDate($IntToDate(WDatInicio));
          $SetCriteria('(A.CDRAMO = '''+%CHAR(WRamo)+''') AND +
                        (B.CDPROD = '''+%CHAR(WCdProd)+''') AND +
                        (A.NUAPOL = '''+%CHAR(WApolice)+''') AND +
                        (B.CDNATURA = ''F'')');
          $SetFields('FPRDP008':'A');
          $SetFields('FCATP102':'B':*OFF);
          DOW ($GetRecord(WRegAct2) > *ZEROS);
             IF (NOT WIcTitulo);
                WIcTitulo = *ON;
                $AddLineType('T1');
                WLinha = 'Coberturas Facultativas';
                $AddTextLine(WLinha);
                $AddLineType('T2');
                WLinha = 'Referias na Cláusula 3ª. das Condições Gerais:';
                $AddTextLine(WLinha);
             ENDIF;
             WCdCobert = $GetFieldNum('A.CDCOBERT');
             EXSR GetCapRisco;  // Calcular o valor do Capital Seguro
             EXSR getCobText;
             WRegAct2 += 1;
             $SetDbSession(WSession);
          ENDDO;
          $AddTextLine(*BLANKS);

          //---------------------------------------------------------------
          // Coberturas Complementares
          //---------------------------------------------------------------
          WRegAct2 = 1;
          WIcTitulo = *OFF;
          $SetCriteria('(A.CDRAMO = '''+%CHAR(WRamo)+''') AND +
                        (B.CDPROD = '''+%CHAR(WCdProd)+''') AND +
                        (A.NUAPOL = '''+%CHAR(WApolice)+''') AND +
                        (B.CDNATURA = ''C'')');
          DOW ($GetRecord(WRegAct2) > *ZEROS);
             IF (NOT WIcTitulo);
                WIcTitulo = *ON;
                $AddLineType('T1');
                WLinha = 'Coberturas Complementares, descritas em anexo';
                $AddTextLine(WLinha);
             ENDIF;
             WCdCobert = $GetFieldNum('A.CDCOBERT');
             WValor = *ZEROS;
             EXSR GetCapRisco;  // Calcular o valor do Capital Seguro
             EXSR getCobText;
             WRegAct2 += 1;
             $SetDbSession(WSession);
          ENDDO;
          $RmvDbSession(WSession);
          $AddTextLine(*BLANKS);
       ENDSR;

       //==================================================================
       // Subrotina..: getCobText
       // Objectivo..: Obter o texto para a Cobertura Facultativa
       // Observação.: Só Escreve os dependentes.
       //==================================================================
       BEGSR getCobText;
          WRegAct3    = 1;
          WSessCatTxt = $SetFiles('FCATP107');
          $AddDBDate($IntToDate(WDatInicio));
          $SetCriteria('(CDPROD = '''+%CHAR(WCdProd)+''') AND +
                        (CDCOBERT = '''+%CHAR(WCdCobert)+''') AND +
                        (CDDOCUM  = ''1'')');
          DOW ($GetRecord(WRegAct3) > *ZEROS);
             WCdIdent  = $GetField('CDIDENT');
             WTexto    = $GetField('TEXTO');
             $AddLineType(WCdIdent);
             IF (WValor > *ZEROS);
                $AddTextLine(WTexto:1:*BLANKS:
                                   %EditW(WValor :'           '):'VL');
                WValor = *ZEROS;
             ELSE;
                $AddTextLine(WTexto);
             ENDIF;
             WRegAct3 += 1;
          ENDDO;
          $RmvDBSession(WSessCatTxt);
       ENDSR;

       //==================================================================
       // Subrotina..: getCapRisco
       // Objectivo..: Calcular o valor do Capital de Risco da Cobertura
       //==================================================================
       BEGSR getCapRisco;
          //--------------------------------------------
          //  Calcular o Valor para o Capital Seguro
          //--------------------------------------------
          WValor = WTotCap;
          WSessCatLim = $SetFiles('FCATP103');
          $AddDBDate($IntToDate(WDatInicio));
          $SetCriteria('(CDPROD = '''+%CHAR(WCdProd)+''') AND +
                        (CDCOBERT = '''+%CHAR(WCdcobert)+''')');
          IF ($GetRecord() > *ZEROS);
             WCdRisco  = $GetFieldNum('CDRISCO');
             WCdLimite = $GetFieldNum('CDLIMITE');
             WVlPerInd = $GetFieldNum('VLPERIND');
             WVlMaxInd = $GetFieldNum('VLMAXIND');

             WSessPrdLim = $SetFiles('FPRDP014');
             $AddDBDate($IntToDate(WDatInicio));
             $SetCriteria('(CDRAMO = '''+%CHAR(WRamo)+''') AND +
                           (NUAPOL = '''+%CHAR(WApolice)+''') AND +
                           (CDLIMITE = '''+%CHAR(WCdLimite)+''')');
             IF ($GetRecord() > *ZEROS);
                WVlPerInd = $GetFieldNum('VLPERIND');
                WVlMaxInd = $GetFieldNum('VLMAXIND');
             ENDIF;
             $RmvDBSession(WSessPrdLim);

             WIdxAct = *zeros;
             DOW (WIdxAct < WIdx);
                WIdxAct += 1;
                IF ($CapRisco(WIdxAct).Codigo = WCdRisco);
                   WValor = $CapRisco(WidxAct).Valor;
                   LEAVE;
                ENDIF;
             ENDDO;
             IF (WVlPerInd > *ZEROS);
                WValor *= (WVlPerInd/100);
             ENDIF;
             IF ((WVlMaxInd < WValor) AND (WVlMaxInd > *ZEROS));
                WValor = WVlMaxInd;
             ENDIF;
          ENDIF;
          $RmvDBSession(WSessCatLim);
       ENDSR;
      /END-FREE
