     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: RPRDO013
     H* Descrição.: Condições Especiais Clausulas
     H*
     H*==================================================================
     FFCATP108  IF   E           K DISK    PREFIX(CAT_)
     FFCATP10801IF   E           K DISK    PREFIX(CAT_)
     F                                     RENAME(RCATP108:RCATP10801)
     FFCATP109  IF   E           K DISK    PREFIX(CAT_)
     D*==========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SCATO010
      /Copy QRPGLESRC,SPRDO002
      /Copy QRPGLESRC,SENTO010

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
     D WCdItem         S                   LIKE(CAT_CDITEM)
     D WTpItem         S                   LIKE(CAT_TPITEM)
     D WCdProd         S              5S 0
     D WVar01          S             15A   INZ(*BLANKS)
     D WVar02          S            100A   INZ(*BLANKS)
     D WLinha          S            500A
     D WptrHist        S               *
     D WptrOr          S               *
     D
     D WCRHIP          S                   LIKE(HST_CRHIP)
     D WCE001          S                   LIKE(HST_CE001)
     D WCE002          S                   LIKE(HST_CE002)
     D WCE003          S                   LIKE(HST_CE003)
     D WCE004          S                   LIKE(OR_CE004)
     D WCA030          S                   LIKE(HST_CA030)
     D WCA031          S                   LIKE(HST_CA031)
     D WCA100          S                   LIKE(HST_CA100)
     D WCA101          S                   LIKE(HST_CA101)
     D WCA102          S                   LIKE(HST_CA102)
     D WCD103          S                   LIKE(HST_CD103)
     D WCD130          S                   LIKE(HST_CD130)
     D WCD131          S                   LIKE(HST_CD131)
     D WCD132          S                   LIKE(HST_CD132)
     D WCD133          S                   LIKE(HST_CD133)
     D WCD134          S                   LIKE(HST_CD134)
     D WCD135          S                   LIKE(HST_CD135)
     D WCD136          S                   LIKE(HST_CD136)
     D WCD137          S                   LIKE(HST_CD137)
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

       ptrHist = WptrHist;
       ptrOr   = WptrOR;
       MONITOR;
          WCRHIP = HST_CRHIP;
          WCE001 = HST_CE001;
          WCE002 = HST_CE002;
          WCE003 = HST_CE003;
       // WCE004 = HST_CE004;
          WCA030 = HST_CA030;
          WCA031 = HST_CA031;
          WCA100 = HST_CA100;
          WCA101 = HST_CA101;
          WCA102 = HST_CA102;
          WCD103 = HST_CD103;
          WCD130 = HST_CD130;
          WCD131 = HST_CD131;
          WCD132 = HST_CD132;
          WCD133 = HST_CD133;
          WCD134 = HST_CD134;
          WCD135 = HST_CD135;
          WCD136 = HST_CD136;
          WCD137 = HST_CD137;
       ON-ERROR;
          WCRHIP = OR_CRHIP;
          WCE001 = OR_CE001;
          WCE002 = OR_CE002;
          WCE003 = OR_CE003;
          WCE004 = OR_CE004;
          WCA030 = OR_CA030;
          WCA031 = OR_CA031;
          WCA100 = OR_CA100;
          WCA101 = OR_CA101;
          WCA102 = OR_CA102;
          WCD103 = OR_CD103;
          WCD130 = OR_CD130;
          WCD131 = OR_CD131;
          WCD132 = OR_CD132;
          WCD133 = OR_CD133;
          WCD134 = OR_CD134;
          WCD135 = OR_CD135;
          WCD136 = OR_CD136;
          WCD137 = OR_CD137;
       ENDMON;

       MONITOR;
          WVar01 = %EDITW(OR_TXIND:' 0, %');
       ON-ERROR;
       ENDMON;
       MONITOR;
          WVar02 = $getDescCred(WCRHIP);
       ON-ERROR;
       ENDMON;
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
          $AddTextLine('Condições Especiais e Cláusulas Aplicáveis');

          $AddLineType('T1');
          $AddTextLine('Tem aplicação neste contrato as Condições +
                  Especiais e Cláusulas cujos números abaixo se indicam:');
          //=> Cóndições Especiais
          IF (WCE001 = 'S');
             WTpItem = 'E';
             WCdItem = '001';
             EXSR getCobText;
          ENDIF;
          IF (WCE002 = 'S');
             WTpItem = 'E';
             WCdItem = '002';
             EXSR getCobText;
          ENDIF;
          IF (WCE003 = 'S');
             WTpItem = 'E';
             WCdItem = '003';
             EXSR getCobText;
          ENDIF;
          IF (WCE004 = 'S');
             WTpItem = 'E';
             WCdItem = '004';
             EXSR getCobText;
          ENDIF;

          //=> Clausulas Aplicáveis
          IF (WCA030 = 'S');
             WTpItem = 'A';
             WCdItem = '030';
             EXSR getCobText;
          ENDIF;
          IF (WCA031 = 'S');
             WTpItem = 'A';
             WCdItem = '031';
             EXSR getCobText;
          ENDIF;
          IF (WCA100 = 'S');
             WTpItem = 'A';
             WCdItem = '100';
             EXSR getCobText;
          ENDIF;
          IF (WCA101 = 'S');
             WTpItem = 'A';
             WCdItem = '101';
             EXSR getCobText;
          ENDIF;
          IF (WCA102 = 'S');
             WTpItem = 'A';
             WCdItem = '102';
             EXSR getCobText;
          ENDIF;

          // ...
          IF (WCD103 = 'S');
             WTpItem = 'D';
             WCdItem = '103';
             EXSR getCobText;
          ENDIF;
          IF (WCD130 = 'S');
             WTpItem = 'D';
             WCdItem = '130';
             EXSR getCobText;
          ENDIF;
          IF (WCD131 = 'S');
             WTpItem = 'D';
             WCdItem = '131';
             EXSR getCobText;
          ENDIF;
          IF (WCD132 = 'S');
             WTpItem = 'D';
             WCdItem = '132';
             EXSR getCobText;
          ENDIF;
          IF (WCD133 = 'S');
             WTpItem = 'D';
             WCdItem = '133';
             EXSR getCobText;
          ENDIF;
          IF (WCD134 = 'S');
             WTpItem = 'D';
             WCdItem = '134';
             EXSR getCobText;
          ENDIF;
          IF (WCD135 = 'S');
             WTpItem = 'D';
             WCdItem = '135';
             EXSR getCobText;
          ENDIF;
          IF (WCD136 = 'S');
             WTpItem = 'D';
             WCdItem = '136';
             EXSR getCobText;
          ENDIF;
          IF (WCD137 = 'S');
             WTpItem = 'D';
             WCdItem = '137';
             EXSR getCobText;
          ENDIF;
          $AddTextLine();
       ENDSR;

       //==================================================================
       // Subrotina..: getCobText
       // Objectivo..: Obter o texto para a Cobertura Facultativa
       //==================================================================
       BEGSR getCobText;
          CHAIN (WCdProd:WTpItem:WCdItem) RCATP10801;
          IF (%FOUND());
             SETLL (WCdProd:CAT_NUSEQU) RCATP109;
             READE (WCdProd:CAT_NUSEQU) RCATP109;
             DOW (NOT %EOF());
                WLinha = CAT_TEXTO;
                WLinha = %TRIM($StrReplace('&VAR01':%TRIM(WVar01):WLinha));
                WLinha = %TRIM($StrReplace('&VAR02':%TRIM(WVar02):WLinha));
                $AddTextLine(WLinha:1:WCdItem);
                WCdItem = '???';
                READE (WCdProd:CAT_NUSEQU) RCATP109;
             ENDDO;
          ENDIF;
       ENDSR;
      /END-FREE
