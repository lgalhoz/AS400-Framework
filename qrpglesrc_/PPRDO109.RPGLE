     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP('JMGRP')
     H FIXNBR(*ZONED)
     H*==================================================================
     FFCATP010  IF   E           K DISK
     FFCATP01001IF   E           K DISK    RENAME(RCATP010:RCATP01001)
     FFPRDE109  CF   E             WORKSTN PREFIX(ECR_)
     F                                     INFDS(INFD#KEYS)
     F*========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SSECO010

     D*==> Pré-visualização das Actas
     D  PrintPreview   PR                  EXTPGM('RPRDO005')
     D   ptrOR                         *   CONST
     D   Cdfunction                   5S 0 CONST
     D
     D*==> Estrutura de dados do Registo
     D RBASEDIO      E DS                  ExtName(GBASEDIO)
     D                                     Prefix(OR_)
     D                                     Based(ptrOR)
     D WCodProduto     S              5S 0
     D WDatInicio      S              8S 0
     D WptrOr          S               *
     D WModo           S               N
     D WNomFunction    S             10A
     D
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
     D WIndiceDs       DS
     D   WFilo3                1      6
     D   WIndBase              1      3  0
     D   WIndVenc              4      6  0
     C*========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCodProduto
     C                   PARM                    WDatInicio
     C                   PARM                    WPtrOR
     C                   PARM                    WModo
      /FREE
       //------------------------------------
       // Se não estiver autorizado então sai
       //------------------------------------
       IF NOT $ValAccess(S#PGM:'':99999);
          *INLR = *ON;
          RETURN;
       ENDIF;
       ptrOr   = WptrOR;

       ECR_NOMSEG  = OR_SEG;
       WIndiceDS   = OR_Filo3;
       ECR_Ramo    = OR_Ramo;
       ECR_Apolice = OR_Apol;
       ECR_OBS1    = OR_OBS1;
       ECR_OBS2    = OR_OBS2;
       ECR_DescInc = *BLANKS;

       IF (WIndiceDS = '') AND (OR_CE003 = 'S');
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
          WData6A = OR_DTINI;
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
             IF (OR_CR1=1 OR OR_CR2=1 OR OR_CR3=1);
                WIncidencia = 'E';
             ENDIF;
             IF (OR_CR1=2 OR OR_CR2=2 OR OR_CR3=2);
                WIncidencia = 'R';
             ENDIF;
             IF (OR_CR1=3 OR OR_CR2=3 OR OR_CR3=3);
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
                ECR_DescBase = DESCRICAO;
                ECR_DescInc  = $getDescricao(22:INCIDENCIA);
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
                   ECR_DescVenc = DESCRICAO;
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
             WNomFunction = 'RPRDE10905';
             DS#HEADER    = $getHeader(S#PGM:WNomFunction);
             DS#FOOTER    = $getFooter(S#PGM:WNomFunction:*BLANKS);
             ECR_S#PGM    = S#PGM;
             DOW (NOT (*IN03 OR *IN12));
                WRITE RPRDE10900;
                WRITE RPRDE10999;
                EXFMT RPRDE10905;
                IF (Func#Key = KEY#F14);
                MONITOR;
                   PrintPreview(WPtrOr:30);
                ON-ERROR;
                ENDMON;
                ENDIF;
             ENDDO;
          ENDIF;
       ENDSR;
      /END-FREE
