     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP('JMGRP')
     H FIXNBR(*ZONED)
     H*==================================================================
     FGBASEGRO  IF   E           K DISK    PREFIX(B_)
     FGBASEDIO  IF   E           K DISK    PREFIX(D_)
     F                                     RENAME(BASEDIFO:BASEDIFO)
     FFCATP010  IF   E           K DISK
     FFCATP01001IF   E           K DISK    RENAME(RCATP010:RCATP01001)
     FFPRDE101  CF   E             WORKSTN PREFIX(ECR_)
     F*========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SSECO010

     D
     D WParmRamApol    S              9S 0
     D WIndice         S              1A
     D WDescIndice     S             30A
     D WIcOrigem       S              1A
     D WIcErro         S               N   INZ(*OFF)
     D
     D WDatVencim      S              8S 0
     D WDatHoje        S              8S 0
     D WAnoIndice      S              4S 0
     D WTrimestre      S              1S 0
     D WIncidencia     S              1A
     D
     D WCvtDtini       DS
     D   WData6A               1      6S 0
     D   WCvtAno               5      6S 0
     D   WCvtMes               3      4S 0
     D   WCvtDia               1      2S 0
     D
     D WRamoApolice    DS
     D   WRamApol              1      9S 0
     D   WRamo                 1      2S 0
     D   WApolice              3      9S 0
     D
     D WIndiceDs       DS
     D   WFilo3                1      6
     D   WIndBase              1      3  0
     D   WIndVenc              4      6  0
     C*========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WParmRamApol
      /FREE
       //------------------------------------
       // Se não estiver autorizado então sai
       //------------------------------------
       IF NOT $ValAccess(S#PGM:'':99999);
          *INLR = *ON;
          RETURN;
       ENDIF;

       WRamApol = WParmRamApol;
       WIcOrigem = 'D'; // Diario
       CHAIN (WRamo:WApolice) BASEDIFO;
       IF (NOT %FOUND);
          WIcOrigem = 'B'; // Base
          CHAIN (WRamo:WApolice) BASEGRFO;
          ECR_NOMSEG = B_SEG;
          WIndiceDS  = B_Filo3;
       ELSE;
          ECR_NOMSEG = D_SEG;
          WIndiceDS  = D_Filo3;
       ENDIF;

       WIcErro = *OFF;
       ECR_Ramo = Wramo;
       ECR_Apolice = WApolice;
       IF (WIndiceDS = '') AND ((WIcOrigem = 'D' AND D_CE003 = 'S') OR
                                (WIcOrigem = 'B' AND B_CE003 = 'S'));
          WIndBase = *ZEROS;
          WIndVenc = *ZEROS;
          EXSR MakeIndices;
       ENDIF;
       EXSR ShowIndices;

       *INLR = *ON;
       RETURN;

       //============================================================
       // Subrotina..: MakeIndices
       // Objectivo..: Formatar os campos de indices automaticamente
       //============================================================
       BEGSR MakeIndices;
          //------------------------------------------
          // Formatar data Inicio e Obter data do dia
          //------------------------------------------
          IF (WIcOrigem='B');
             WData6A = B_DTINI;
          ELSE;
             WData6A = D_DTINI;
          ENDIF;
          WCvtAno += 1;
          IF (WCvtAno > 40);
              WAnoIndice = (1900 + WCvTAno);
          ELSE;
              WAnoIndice = (2000 + WCvTAno);
          ENDIF;
          WDatVencim = (WAnoIndice * 10000) + (WCvtMes * 100) + WCvtDia;
          WDatHoje = %SubDt(%Date():*Years) * 10000 +
                     %SubDt(%Date():*Months) * 100 +
                     %SubDt(%Date():*Days);

          //------------------------------------------
          // Obter o indice correspondente
          //------------------------------------------
          IF (WDatVencim > WDatHoje);
             //--------------------------------
             // Determinar o Trimestre
             //--------------------------------
             IF (WCvtMes >= 1) AND (WCvtMes <= 3);
                WTrimestre = 1;
             ELSEIF (WCvtMes >= 4) AND (WCvtMes <= 6);
                WTrimestre = 2;
             ELSEIF (WCvtMes >= 7) AND (WCvtMes <= 9);
                WTrimestre = 3;
             ELSE;
                WTrimestre = 4;
             ENDIF;

             //--------------------------------
             // Determinar a Incidência
             //--------------------------------
             WIncidencia = *BLANKS;
             IF ((D_CR1=1 OR D_CR2=1 OR D_CR3=1) AND WIcOrigem='D') OR
                ((B_CR1=1 OR B_CR2=1 OR B_CR3=1) AND WIcOrigem='B');
                WIncidencia = 'E';
             ENDIF;
             IF ((D_CR1=2 OR D_CR2=2 OR D_CR3=2) AND WIcOrigem='D') OR
                ((B_CR1=2 OR B_CR2=2 OR B_CR3=2) AND WIcOrigem='B');
                WIncidencia = 'R';
             ENDIF;
             IF ((D_CR1=3 OR D_CR2=3 OR D_CR3=3) AND WIcOrigem='D') OR
                ((B_CR1=3 OR B_CR2=3 OR B_CR3=3) AND WIcOrigem='B');
                WIncidencia = 'T';
             ENDIF;

             //--------------------------------
             // Obter o Valor para o Indice
             //--------------------------------
             CHAIN (WAnoIndice:WTrimestre:WIncidencia) RCATP01001;
             IF (%FOUND);
                WIndBase = CodIndice;
             ENDIF;
          ENDIF;
       ENDSR;

       //============================================================
       // Subrotina..: ShowIndices
       // Objectivo..: Mostrar os Indices num ecrã
       //============================================================
       BEGSR ShowIndices;
          //-----------------------------------------
          // Obter os valores para o Indice Base
          //-----------------------------------------
          IF (WIndBase <> *ZEROS);
             MONITOR;
                CHAIN (WIndBase) RCATP010;
                ECR_AnoBase  = (ANO * 100) + TRIMESTRE;
                ECR_ValBase  = VALOR;
                ECR_DescBase = $getDescricao(22:INCIDENCIA);
             ON-ERROR;
                WIcErro = *ON;
             ENDMON;
          ELSE;
             WIcErro = *ON;
          ENDIF;

          //-----------------------------------------
          // Obter os valores para o Indice Vencimento
          //-----------------------------------------
          IF (NOT WIcErro);
             MONITOR;
                IF (WIndVenc > *ZEROS);
                   CHAIN (WIndVenc) RCATP010;
                   ECR_AnoVenc  = (ANO * 100) + TRIMESTRE;
                   ECR_ValVenc  = VALOR;
                   ECR_DescVenc = $getDescricao(22:INCIDENCIA);
                ELSE;
                   ECR_AnoVenc  = *ZEROS;
                   ECR_ValVenc  = *ZEROS;
                   ECR_DescVenc = *BLANKS;
                ENDIF;
             ON-ERROR;
                WIcErro = *ON;
                ECR_AnoVenc = *ZEROS;
                ECR_ValVenc = *ZEROS;
                ECR_DescVenc = *BLANKS;
             ENDMON;
          ENDIF;

          //-----------------------------------------
          // Apresentar os valores no Ecrã
          //-----------------------------------------
          IF (NOT WIcErro);
             ECR_ValIndice = %INTH(((ECR_ValVenc / ECR_ValBase)-1)*1000)/1000;

             //---------------------------
             // Descodificar o Indice
             //---------------------------
             ECR_DescInc = ' ';
             IF (ECR_DescVenc<> ECR_DescBase) AND (ECR_DescVenc <> *BLANKS);
                ECR_DescInc = 'Erro ';
             ELSEIF (ECR_DescVenc <> *BLANKS);
                ECR_DescInc = ECR_DescBase;
                ECR_DescVenc= *BLANKS;
                ECR_DescBase = *BLANKS;
             ENDIF;
             ECR_DS#PGM   = $getHeader(S#PGM:'RPRDE10105');
             ECR_DS#KEY01 = $getFooter(S#PGM:'RPRDE10105':*BLANKS);
             ECR_S#PGM    = S#PGM;
             WRITE RPRDE10100;
             WRITE RPRDE10199;
             EXFMT RPRDE10105;
          ENDIF;
       ENDSR;
      /END-FREE
