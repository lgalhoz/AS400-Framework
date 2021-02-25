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

     D*==> Histórico
     D RGEXAP        E DS                  ExtName(GEXAP)
     D                                     Prefix(HST_)
     D                                     Based(ptrHist)
     D
     D*==> Estrutura de dados do Registo
     D RBASEDIO      E DS                  ExtName(GBASEDIO)
     D                                     Prefix(OR_)
     D                                     Based(ptrOR)
     D
     D*==> Histórico de Capitais de Risco
     D $CapRisco       DS                  QUALIFIED DIM(10)
     D   Valor                       11S 2 INZ(*ZEROS)
     D   Codigo                       2S 0 INZ(*ZEROS)
     D WIdx            S              2S 0 INZ(*ZEROS)
     D WIdxAct         S              2S 0 INZ(*ZEROS)
     D
     D WCdAlias        S             10A
     D WCdProd         S              5S 0
     D WCdNatura       S              1A   INZ('F')
     D WTotCap         S             11S 2
     D WValorAlfa      S             15A
     D WValorAlfaC     S             11A
     D WLinha          S            500A
     D WIcTitulo       S               N
     D WptrHist        S               *
     D WptrOR          S               *
     D WCdCobert       S              5S 0
     D WCdRisco        S              5S 0
     D WCapRisco       S             11S 2
     D WCdIdent        S              2A
     D WTexto          S            500A
     D
     D WSession        S              5S 0
     D WSessCatCob     S              5S 0
     D
     D WIcEscrever     S              1A
     D WSessCatTxt     S              5S 0
     D WSessCatLim     S              5S 0
     D WSessPrdLim     S              5S 0
     D WCdLimite       S              5S 0
     D WRegAct1        S              5S 0
     D WRegAct2        S              5S 0
     D WRegAct3        S              5S 0
     D WCodErro        S              7A
     D*==========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCdProd
     C                   PARM                    WRamo
     C                   PARM                    WApolice
     C                   PARM                    WDatInicio
     C                   PARM                    WCodErro
     C                   PARM                    WPtrHist
     C                   PARM                    WPtrOR
     C*==========================================================
      /FREE
       ptrHist   = WptrHist;
       ptrOr     = WptrOR;
       WCodErro  = *BLANKS;

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
          WLinha = 'BENS SEGUROS/GARANTIAS';
          %SUBST(WLinha:83) = 'CAPITAIS';
          $AddTextLine(WLinha);

          //---------------------------------------------------------------
          // Riscos da Apólice
          //---------------------------------------------------------------
          $addException(*OFF);
          WRegAct1 = 1;
          $AddLineType('B1');
          WTotCap  = *ZEROS;
          WSession = $SetFiles('FPRDP011 A INNER JOIN FCATP101 B  +
                                ON (A.CDRAMO='''+%CHAR(WRamo)+''') AND +
                                (B.CDPROD='''+%CHAR(WCdProd)+''') AND +
                                (A.CDRISCO = B.CDRISCO)');
          $AddDBDate($IntToDate(WDatInicio));
          $SetCriteria('(A.CDRAMO = '''+%CHAR(WRamo)+''') AND +
                        (A.NUAPOL = '''+%CHAR(WApolice)+''')');
          $SetFields('FPRDP011':'A');
          $SetFields('FCATP101':'B':*OFF);
          DOW ($GetRecord(WRegAct1) > *ZEROS);
             IF ($GetField('B.ICCAPTOT') = 'S');
                WCdRisco  = $GetFieldNum('A.CDRISCO');
                WLinha    = $GetDescricao(20:%CHAR(WCdRisco));
                WCapRisco = $GetFieldNum('A.IMCPRISC');
                WTotCap  += WCapRisco;
                WIdx += 1;
                $CapRisco(Widx).Valor = WCapRisco;
                $CapRisco(Widx).Codigo = $GetFieldNum('A.CDRISCO');
                $AddTextLine(WLinha:1:*BLANKS:
                             %EditW(WCapRisco:'           '):'VL');
             ENDIF;
             WRegAct1 += 1;
          ENDDO;
          $RmvDbSession(WSession);

          //---------------------------------------------------------------
          // Coberturas Base
          //---------------------------------------------------------------
          $addException(*OFF);
          $AddLineType('H1');
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
                        (B.CDNATURA = ''F'') AND (ICESCREVE = ''S'')');
          $SetFields('FPRDP008':'A');
          $SetFields('FCATP102':'B':*OFF);
          DOW ($GetRecord(WRegAct2) > *ZEROS);
             IF (NOT WIcTitulo);
                WIcTitulo = *ON;
                $AddLineType('H1');
                WLinha = 'Coberturas Facultativas +
                          Referias na Cláusula 3ª. das Condições Gerais:';
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
                        (B.CDNATURA = ''C'') AND (ICESCREVE = ''S'')');
          DOW ($GetRecord(WRegAct2) > *ZEROS);
             IF (NOT WIcTitulo);
                WIcTitulo = *ON;
                $AddLineType('B1');
                WLinha = 'Coberturas Complementares, descritas em anexo';
                $AddTextLine(WLinha);
             ENDIF;
             WCdCobert = $GetFieldNum('A.CDCOBERT');
             WValorAlfa = *BLANKS;
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
             WTexto    = $GetFieldText('TEXTO');
             $AddLineType('B1');
             IF (WValorAlfa <> *BLANKS);
                $AddTextLine(WTexto:1:*BLANKS:WValorAlfaC:'VL');
                WValorAlfa = *BLANKS;
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
          WValorAlfa  = $GetCapitalAlf(WCdProd:WRamo:WApolice:'L':WCdCobert:
                                      *ZEROS:*OFF:WDatInicio);
          WValorAlfaC = %SUBST(WValorAlfa:2:3)+
                        %SUBST(WValorAlfa:6:3)+
                        %SUBST(WValorAlfa:10:3)+
                        %SUBST(WValorAlfa:14:2);
       ENDSR;
      /END-FREE
