     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PRDO108
     H* Descrição.: Caracterização do risco do Edificio, quanto à natureza
     H*             da sua contrução e edificios adjacentes.
     H*==================================================================
     FFPRDP016  UF A E           K DISK    PREFIX(PRD_)
     FFPRDP018  UF A E           K DISK    PREFIX(PRD_)
     FFPRDE108  CF   E             WORKSTN PREFIX(ECR_)
     F                                     INFDS(INFD#KEYS)
     F*==========================================================
     F* Indicadores  Descrição
     F* -----------  --------------------------------------------
     F*      03      Tecla F3
     F*      12      Tecla F12
     D*==========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SCATO010
      /Copy QRPGLESRC,SSECO010

     D*==> Estrutura de dados do Registo
     D RBASEDIO      E DS                  ExtName(GBASEDIO)
     D                                     Prefix(OR_)
     D                                     Based(ptrOR)
     D
     D*==> Pré-visualização das Actas
     D  PrintPreview   PR                  EXTPGM('RPRDO005')
     D   ptrOR                         *   CONST
     D   Cdfunction                   5S 0 CONST
     D
     D*==> Parametros recebidos
     D WCodProduto     S              5S 0
     D WIcErro         S               N   INZ(*OFF)
     D WIcOrigem       S              1A
     D REC#            S              4S 0
     D
     D WIdx            S              5S 0
     D WIcExtract      S               N
     D WIcDisplay      S               N
     D WModo           S               N
     D WExceptKeys     S             24A   INZ(*BLANKS)
     D DynamicCall     S             10A   INZ(*BLANKS)
     D WMoreText       S              4A   INZ(*BLANKS)
     D WDatInicio      S              8S 0
     D WRow            S              3S 0
     D WCol            S              3S 0
     D WNomFunction    S             10A
     D WptrOr          S               *
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

       ptrOr       = WptrOR;
       ECR_NOMSEG  = OR_SEG;
       ECR_RAMO    = OR_RAMO;
       ECR_APOLICE = OR_APOL;
       WIcDisplay = WModo; // *ON
       IF (NOT WIcDisplay);
          *IN60 = *ON;
       ELSE;
          *IN60 = *OFF;
       ENDIF;
       EXSR ShowCapRisco;
       EXSR ShowInforma;

       *INLR = *ON;
       RETURN;

       //============================================================
       // Subrotina..: ShowInforma
       // Objectivo..: Apresentar valoers para poder gerir
       //============================================================
       BEGSR ShowInforma;
          IF (WIcDisplay);
             WNomfunction = 'RPRDE10805';
          ELSE;
             WNomFunction = 'RPRDE10821';
          ENDIF;
          CHAIN(N) (OR_RAMO:OR_APOL) RPRDP016;
          IF (%FOUND);
             ECR_CDPOST    = PRD_CDPOST;
             ECR_CDTIPCOB  = PRD_CDTIPCOB;
             ECR_CDESTCONS = PRD_CDESTCONS;
             ECR_CDTIPCONS = PRD_CDTIPCONS;
             ECR_CDEDIADJ  = PRD_CDEDIADJ;
          ELSE;
             ECR_CDTIPCOB  = OR_CF22;
             ECR_CDESTCONS = OR_ESTCON;
             ECR_CDTIPCONS = OR_TIPCON;
             ECR_CDEDIADJ  = OR_CF21;
          ENDIF;
          CHAIN(N) (OR_RAMO:OR_APOL) RPRDP018;
          IF (%FOUND);
             ECR_CPEDIFIC  = PRD_CPEDIFIC;
             ECR_IMPREMIO  = PRD_IMPREMIO;
             ECR_PRCOBERT  = PRD_PRCOBERT;
          // ECR_TXCLCPRM  = PRD_TXCLCPRM;
          ENDIF;
          ECR_ANO       = OR_ANOEDI;
          ECR_LCRISCO   = OR_LRPS;
          ECR_CDPOST    = OR_CPEDI*1000;

          ECR_DSPOST    = $GetCodPost(ECR_CDPOST);
          ECR_DSTIPCOB  = $getDescricao(51:ECR_CDTIPCOB);
          ECR_DSESTCONS = $getDescricao(52:ECR_CDESTCONS);
          ECR_DSTIPCONS = $getDescricao(50:ECR_CDTIPCONS);
          ECR_DSEDIADJ  = $getDescricao(49:ECR_CDEDIADJ);
          ECR_S#PGM = S#PGM;
          DOW (NOT *IN03 AND NOT *IN12);
             DS#HEADER    = $getHeader(S#PGM:WNomFunction);
             DS#FOOTER    = $getFooter(S#PGM:WNomFunction:*BLANKS);
             WRITE RPRDE10800;
             WRITE RPRDE10899;
             EXFMT RPRDE10805;
             IF ($ValidKey(Func#Key:KEY#F21)) AND WIcDisplay;
                //WIcDisplay = *OFF;
                //WNomFunction = 'RPRDE10821';
             ELSEIF (Func#Key = KEY#F04) AND (NOT WIcDisplay);
                WRow    = ECR_CM#CROW;
                WCol    = ECR_CM#CCOL;
                EXSR PopUp;
             ELSEIF (Func#Key = KEY#F14);
                MONITOR;
                   PrintPreview(WPtrOr:83);
                ON-ERROR;
             ENDMON;
             ELSEIF (Func#Key = KEY#Enter) AND (NOT WIcDisplay);
                EXSR SaveRegisto;
                ECR_DSPOST    = $GetCodPost(ECR_CDPOST);
                ECR_DSTIPCOB  = $getDescricao(51:ECR_CDTIPCOB);
                ECR_DSESTCONS = $getDescricao(52:ECR_CDESTCONS);
                ECR_DSTIPCONS = $getDescricao(50:ECR_CDTIPCONS);
                ECR_DSEDIADJ  = $getDescricao(49:ECR_CDEDIADJ);
             // LEAVE;  // Sair para o programa chamador
             ENDIF;
          ENDDO;
       ENDSR;

       //========================================================
       // Subrotina.: SaveRegisto
       // Objectivo.: Salvar o registo
       //========================================================
       BEGSR SaveRegisto;
          CHAIN (OR_RAMO:OR_APOL) RPRDP016;
          IF (%FOUND);
             PRD_CDTIPCOB  = ECR_CDTIPCOB;
             PRD_CDESTCONS = ECR_CDESTCONS;
             PRD_CDTIPCONS = ECR_CDTIPCONS;
             PRD_CDEDIADJ  = ECR_CDEDIADJ;
             UPDATE RPRDP016 %FIELDS(PRD_CDTIPCOB:PRD_CDESTCONS:
                                     PRD_CDTIPCONS:PRD_CDEDIADJ);
             CHAIN (OR_RAMO:OR_APOL) RPRDP018;
             IF (%FOUND);
                PRD_CPEDIFIC  = ECR_CPEDIFIC;
                PRD_PRCOBERT  = ECR_PRCOBERT;
             // PRD_TXCLCPRM  = ECR_TXCLCPRM;
                PRD_IMPREMIO  = ECR_IMPREMIO;
                UPDATE RPRDP018 %FIELDS(PRD_CPEDIFIC:PRD_PRCOBERT:
                                        PRD_QTDFRAC:PRD_IMPREMIO);
             ELSE;
                PRD_CDRAMO    = OR_RAMO;
                PRD_NUAPOL    = OR_APOL;
                PRD_CPEDIFIC  = ECR_CPEDIFIC;
                PRD_PRCOBERT  = ECR_PRCOBERT;
             // PRD_TXCLCPRM  = ECR_TXCLCPRM;
                PRD_IMPREMIO  = ECR_IMPREMIO;
                WRITE RPRDP018;
             ENDIF;
          ELSE;
             //----> Actualizar o detalhe do edificio
             PRD_CDRAMO    = OR_RAMO;
             PRD_NUAPOL    = OR_APOL;
             PRD_CDTIPCOB  = ECR_CDTIPCOB;
             PRD_CDESTCONS = ECR_CDESTCONS;
             PRD_CDTIPCONS = ECR_CDTIPCONS;
             PRD_CDEDIADJ  = ECR_CDEDIADJ;
             WRITE RPRDP016;

             //----> Actualizar o detalhe do condominio
             PRD_CPEDIFIC  = ECR_CPEDIFIC;
             PRD_PRCOBERT  = ECR_PRCOBERT;
             WRITE RPRDP018;
          ENDIF;
          OR_ANOEDI = ECR_ANO;
          OR_LRPS   = ECR_LCRISCO;
          OR_CPEDI  = %INT(ECR_CDPOST / 1000);
          OR_CF22   = %SUBST(ECR_CDTIPCOB:3:1);
          OR_ESTCON = ECR_CDESTCONS;
          OR_TIPCON = %SUBST(ECR_CDTIPCONS:3:1);
          OR_CF21   = %SUBST(ECR_CDEDIADJ:5:1);
          OR_DCF21  = $getDescricao(49:ECR_CDEDIADJ);
       ENDSR;

       //========================================================
       // Subrotina.: popup
       // Objectivo.: Afixar uma lista de valoers possíveis
       //========================================================
       BEGSR popup;
          //----------------------------------------------
          // Validar qual o campo onde foi seleccionado
          //----------------------------------------------
          IF (ECR_CM#NMFLD = 'CDTIPCOB');
             ECR_CDTIPCOB = $runPopUp(51:ECR_CDTIPCOB);
             ECR_DSTIPCOB = $getDescricao(51:ECR_CDTIPCOB);
          ELSEIF (ECR_CM#NMFLD = 'CDESTCONS');
             ECR_CDESTCONS= $runPopUp(52:ECR_CDESTCONS);
             ECR_DSESTCONS= $getDescricao(52:ECR_CDESTCONS);
          ELSEIF (ECR_CM#NMFLD = 'CDTIPCONS');
             ECR_CDTIPCONS= $runPopUp(50:ECR_CDTIPCONS);
             ECR_DSTIPCONS= $getDescricao(50:ECR_CDTIPCONS);
          ELSEIF (ECR_CM#NMFLD = 'CDEDIADJ');
             ECR_CDEDIADJ = $runPopUp(49:ECR_CDEDIADJ);
             ECR_DSEDIADJ = $getDescricao(49:ECR_CDEDIADJ);
          ELSEIF (ECR_CM#NMFLD = 'CDPOST');
             ECR_CDPOST   = $runCPPopUp(ECR_CDPOST);
             ECR_DSPOST   = $GetCodPost(ECR_CDPOST);
          ENDIF;
          ECR_CM#ROW  = WRow;
          ECR_CM#COL  = WCol;
       ENDSR;


       //============================================================
       // Subrotina..: ShowCapRisco
       // Objectivo..: repor Capitais de Risco
       //============================================================
       BEGSR ShowCapRisco;
         IF (OR_CR1 > *ZEROS);
            WIdx += 1;
            ECR_DSRISC001 = $getDescRisco(WCodProduto:OR_CR1);
            ECR_CPRISC001 = OR_CAP1;
         ELSE;
            ECR_DSRISC001 = *BLANKS;
            ECR_CPRISC001 = *ZEROS;
         ENDIF;
         IF (OR_CR2 > *ZEROS);
            ECR_DSRISC002 = $getDescRisco(WCodProduto:OR_CR2);
            ECR_CPRISC002 = OR_CAP2;
         ELSE;
            ECR_DSRISC002 = *BLANKS;
            ECR_CPRISC002 = *ZEROS;
         ENDIF;
         IF (OR_CR3 > *ZEROS);
            ECR_DSRISC003 = $getDescRisco(WCodProduto:OR_CR3);
            ECR_CPRISC003 = OR_CAP3;
         ELSE;
            ECR_DSRISC003 = *BLANKS;
            ECR_CPRISC003 = *ZEROS;
         ENDIF;
       ENDSR;

      /END-FREE
