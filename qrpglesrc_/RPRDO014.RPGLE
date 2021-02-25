     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL) FIXNBR(*ZONED)
     H*==================================================================
     H* Programa..: RRDO010
     H* Descrição.: Informação Standard de Multiriscos Habitação
     H*
     H*==================================================================
     FFCATP10201IF   E           K DISK    PREFIX(CAT_)
     F                                     RENAME(RCATP102:RCATP10201)
     FFCATP10202IF   E           K DISK    PREFIX(CAT_)
     F                                     RENAME(RCATP102:RCATP10202)
     FFCATP107  IF   E           K DISK    PREFIX(CAT_)
     FFCATP10302IF   E           K DISK    PREFIX(CAT_)
     F                                     RENAME(RCATP103:RCATP10302)
     FFPRDP01401IF   E           K DISK    PREFIX(PRD_)
     F                                     RENAME(RPRDP014:RPRDP01401)
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
     D WCdAlias        S                   LIKE(CAT_ALIAS)
     D WCdProd         S              5S 0
     D WCdNatura       S                   LIKE(CAT_CDNATURA) INZ('F')
     D WTotCap         S             11S 2
     D WValor          S             11S 2
     D WLinha          S            500A
     D WIcTitulo       S               N
     D WptrHist        S               *
     D WptrOR          S               *
     D WCdCobert       S                   LIKE(CAT_CDCOBERT)
     D WCdRisco        S                   LIKE(CAT_CDRISCO)
     D WVlPerInd       S                   LIKE(CAT_VlPerInd)
     D WVlMaxInd       S                   LIKE(CAT_VlMaxInd)
     D
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

       ptrHist   = WptrHist;
       ptrOr     = WptrOR;
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
          WCF0800 = OR_CF0800;
          WCF0830 = OR_CF0830;
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
          WCF0800 = OR_CF0800;
          WCF0830 = OR_CF0830;
          WCF08 = OR_CF08;
          WCF09 = OR_CF09;
          WCF10 = OR_CF10;
          WCF11 = OR_CF11;
          WCF12 = OR_CF12;
          WCF13 = OR_CF13;
       ENDMON;

       WIcTitulo = *OFF;
       EXSR AddTexto; // Formatar as Linhas de Texto

       *INLR = *ON;
       RETURN;

       //==================================================================
       // Subrotina..: AddTexto
       // Objectivo..: Adicionar o texto das Coberturas Base e Coberturas
       //              Facultativas.
       //==================================================================
       BEGSR AddTexto;
          $AddTextLine();
          $AddLineType('H1');
          $addException(*ON);
          WLinha = 'BENS SEGURO/GARANTIAS';
          %SUBST(WLinha:82) = 'CAPITAIS';
          $AddTextLine(WLinha);
          $addException(*OFF);
          $AddLineType('T1');
          WLinha = 'Cobertura Base, conforme Cláusula 2º das +
                    Condições Gerais.';
          $AddTextLine(WLinha);
          $AddLineType('B1');

          WTotCap = *ZEROS;
          IF (WCR1 > *ZEROS);
             WLinha    = $GetDescricao(20:%CHAR(WCR1));
             WTotCap  += WCAP1;
             WIdx += 1;
             $CapRisco(Widx).Valor = WCap1;
             $CapRisco(Widx).Codigo = WCR1;
             WCAP1 *= 100;
             $AddTextLine(WLinha:1:*BLANKS:
                          %EditW(WCAP1:'           '):'VL');
          ENDIF;
          IF (WCR2 > *ZEROS);
             WLinha    = $GetDescricao(20:%CHAR(WCR2));
             WTotCap += WCAP2;
             WIdx += 1;
             $CapRisco(Widx).Valor = WCap2;
             $CapRisco(Widx).Codigo = WCR2;
             WCAP2 *= 100;
             $AddTextLine(WLinha:1:*BLANKS:
                          %EditW(WCAP2:'           '):'VL');
          ENDIF;
          IF (WCR3 > *ZEROS);
             WLinha    = $GetDescricao(20:%CHAR(WCR3));
             WTotCap += WCAP3;
             WIdx += 1;
             $CapRisco(Widx).Valor = WCap3;
             $CapRisco(Widx).Codigo = WCR3;
             WCAP3 *= 100;
             $AddTextLine(WLinha:1:*BLANKS:
                          %EditW(WCAP3:'           '):'VL');
          ENDIF;

          IF (WCF01 = 'S');
             EXSR AddTitulo;
             WCdAlias = '01';
             EXSR getCobText;
          ENDIF;
          IF (WCF02 = 'S');
             EXSR AddTitulo;
             WCdAlias = '02';
             EXSR getCobText;
          ENDIF;
          IF (WCF03 = 'S');
             EXSR AddTitulo;
             WCdAlias = '03';
             EXSR getCobText;
          ENDIF;
          IF (WCF04 = 'S');
             EXSR AddTitulo;
             WCdAlias = '04';
             EXSR getCobText;
          ENDIF;
          IF (WCF05 = 'S');
             EXSR AddTitulo;
             WCdAlias = '05';
             EXSR getCobText;
          ENDIF;
          IF (WCF06 = 'S');
             EXSR AddTitulo;
             WCdAlias = '06';
             EXSR getCobText;
          ENDIF;
          IF (WCF07 = 'S');
             EXSR AddTitulo;
             WCdAlias = '07';
             EXSR getCobText;
          ENDIF;
          IF (WCF0800 = 'S');
             EXSR AddTitulo;
             WCdAlias = '0800';
             EXSR getCobText;
          ENDIF;
          IF (WCF0830 = 'S');
             EXSR AddTitulo;
             WCdAlias = '0830';
             EXSR getCobText;
          ENDIF;
          IF (WCF08 = 'S');
             EXSR AddTitulo;
             WCdAlias = '08';
             EXSR getCobText;
          ENDIF;
          IF (WCF09 = 'S');
             EXSR AddTitulo;
             WCdAlias = '09';
             EXSR getCobText;
          ENDIF;
          IF (WCF10 = 'S');
             EXSR AddTitulo;
             WCdAlias = '10';
             EXSR getCobText;
          ENDIF;
          IF (WCF11 = 'S');
             EXSR AddTitulo;
             WCdAlias = '11';
             EXSR getCobText;
          ENDIF;
          IF (WCF12 = 'S');
             EXSR AddTitulo;
             WCdAlias = '12';
             EXSR getCobText;
          ENDIF;
          IF (WCF13 = 'S');
             EXSR AddTitulo;
             WCdAlias = '13';
             EXSR getCobText;
          ENDIF;
          $AddTextLine(*BLANKS);
       ENDSR;

       //==================================================================
       // Subrotina..: getCobText
       // Objectivo..: Obter o texto para a Cobertura Facultativa
       // Observação.: Só Escreve os dependentes.
       //==================================================================
       BEGSR getCobText;
          CHAIN (WCdProd:WCdAlias:WCdNatura) RCATP10201;
          IF (%FOUND());
             WCdCobert = CAT_CDCOBERT;
             //--------------------------------------------
             //  Se o nó for superior então não escreve
             //--------------------------------------------
             SETLL (WCdProd:WCdCobert) RCATP10202;
             READE (WCdProd:WCdCobert) RCATP10202;
             DOW (NOT %EOF());
                IF (CAT_CDCOBERT <> CAT_CDSUPER);
                   LEAVESR;
                ENDIF;
                READE (WCdProd:WcdCobert) RCATP10202;
             ENDDO;

             //---------------------------------------
             // Calcular o valor do Capital Seguro
             //---------------------------------------
             WValor = *ZEROS;
             EXSR GetCapRisco;

             //---------------------------------------
             // Escrever o  Texto das Coberturas
             //---------------------------------------
             SETLL (WCdProd:WCdCobert) RCATP107;
             READE (WCdProd:WCdCobert) RCATP107;
             DOW (NOT %EOF());
                $AddLineType(CAT_CdIdent);
                IF (WValor > *ZEROS);
                   $AddTextLine(CAT_Texto:1:*BLANKS:
                                      %EditW(WValor :'           '):'VL');
                   WValor = *ZEROS;
                ELSE;
                   $AddTextLine(CAT_Texto);
                ENDIF;
                READE (WCdProd:WCdCobert) RCATP107;
             ENDDO;
          ENDIF;
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
          CHAIN (WCdProd:WCdcobert) RCATP10302;
          IF (%FOUND());
             WCdRisco  = CAT_CdRisco;
             CHAIN (WRamo:WApolice:CAT_CDLIMITE) RPRDP01401;
             IF (%FOUND());
                WVlPerInd = PRD_VLPerInd;
                WVlMaxInd = PRD_VlMaxInd;
             ELSE;
                WVlPerInd = CAT_VLPerInd;
                WVlMaxInd = CAT_VlMaxInd;
             ENDIF;

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
       ENDSR;

       //==================================================================
       // Subrotina..: addTitulo
       // Objectivo..: Adicionar o Cabeçalho.
       //==================================================================
       BEGSR AddTitulo;
          IF (NOT WIcTitulo);
             $AddTextLine(*BLANKS);
             $AddLineType('T1');
             WLinha = 'Coberturas Facultativas referidas na Cláu+
                       sula 3ª das Condições Gerais:';
             $AddTextLine(WLinha);
             $AddLineType('B1');
          ENDIF;
          WIcTitulo = *ON;
       ENDSR;
      /END-FREE
