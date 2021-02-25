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
     D WCodErro        S              7A
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
       WCodErro  = *BLANKS;
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
          // Coberturas Complementares
          //---------------------------------------------------------------
          WRegAct2 = 1;
          WIcTitulo = *OFF;
          WSession = $SetFiles('FPRDP008 A INNER JOIN (FCATP102 B +
                                INNER JOIN FCATP117 C ON +
                                ((B.CDCOBERT = C.CDCOBERT) AND +
                                 (B.CDPROD = C.CDPROD))) ON +
                                (A.CDCOBERT = B.CDCOBERT)');
          $AddDBDate($IntToDate(WDatInicio));
          $SetCriteria('(A.CDRAMO = '''+%CHAR(WRamo)+''') AND +
                        (B.CDPROD = '''+%CHAR(WCdProd)+''') AND +
                        (A.NUAPOL = '''+%CHAR(WApolice)+''') AND +
                        (C.CDDOCUM = 2)');
          $SetFields('C.ICESCREVE,A.CDCOBERT,B.CDPROD,B.DSCOBERT,C.CDDOCUM');
          WCdCobert = *ZEROS;
          DOW ($GetRecord(WRegAct2) > *ZEROS);
             IF ($GetField('C.ICESCREVE') = 'S');
                IF (WCdCobert <> $GetFieldNum('A.CDCOBERT'));
                   //--------------------------------------
                   // Forçar aqui a quebra de página
                   //--------------------------------------
                   $AddLineType('EX');
                   $AddTextLine('[BRK]');
                   $AddLineType('H3');
                   $AddTextLine('CONDIÇÃO ESPECIAL');
                   WCdCobert = $GetFieldNum('A.CDCOBERT');
                   WIcTitulo = *ON;
                   $AddLineType('H3');
                   WLinha = %TRIM($GetField('B.DSCOBERT'));
                   $AddTextLine(WLinha);
                   $AddTextLine(*BLANKS);

                   WValor = *ZEROS;
                   WCdCobert = $GetFieldNum('A.CDCOBERT');
                   EXSR getCobText;
                ENDIF;
             ENDIF;
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
                        (CDDOCUM  = ''2'')');
          DOW ($GetRecord(WRegAct3) > *ZEROS);
             WCdIdent  = $GetField('CDIDENT');
             WTexto    = $GetFieldText('TEXTO');
             $AddLineType(WCdIdent);
             $AddTextLine(WTexto);
             WRegAct3 += 1;
          ENDDO;
          $RmvDBSession(WSessCatTxt);
       ENDSR;
      /END-FREE
