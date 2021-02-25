     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PRDO105
     H* Descrição.: Manutenção de notas Desctrivas Habitações.
     H*             Condominio.
     H*==================================================================
     FFPRDP010  UF   E           K DISK    PREFIX(PRD_) COMMIT
     FFPRDP012  UF   E           K DISK    PREFIX(PRD_) COMMIT
     FFPRDP017  UF A E           K DISK    PREFIX(PRD_) COMMIT
     FFPRDP018  IF   E           K DISK    PREFIX(PRD_)
     FSPRDE105  CF   E             WORKSTN INFDS(INFD#KEYS)
     F                                     PREFIX(ECR_)
     F*==========================================================
     F* Indicadores  Descrição
     F* -----------  --------------------------------------------
     F*      03      Tecla F3
     F*      06      Tecla F6
     F*      12      Tecla F12
     F*      45      IO Condicional - Capital
     F*      46      IO Condicional - Texto
     F*      50      Tecla PagDn
     F*      51      Tecla PagUp
     F*      52      DSP
     F*      53      CLR
     F*      54      END
     D*==========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SENTO010
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SCATO010
      /Copy QRPGLESRC,SSECO010

     D*==> Estrutura de dados do Registo
     D RBASEDIO      E DS                  ExtName(GBASEDIO)
     D                                     Prefix(OR_)
     D                                     Based(ptrOR)
     D
     D*==> Parametros recebidos
     D WApolice        S              7S 0
     D WNome           S             40A
     D WNuSequ         S              5S 0
     D WDisplayMode    S               N
     D
     D WIdx            S              5S 0
     D WIcExtract      S               N
     D WIcDisplay      S               N
     D WExceptKeys     S             24A   INZ(*BLANKS)
     D WNomFunction    S             10A   INZ(*BLANKS)
     D WIcExiste       S               N
     D WIcErro         S               N
     D WDatInicio      S              8S 0
     D WRow            S              3S 0
     D WCol            S              3S 0
     D WPtrOR          S               *
     D WCodProduto     S              5S 0
     D WImPrmEdific    S                   LIKE(PRD_IMPREMIO)
     D WModoIO         S              1A   INZ(*BLANKS)
     D WCpSeguro       S                   LIKE(PRD_CPSEGURO)
     D WPremio         S              5S 2 INZ(*ZEROS)
     D WDsFrac         S             40A   INZ(*BLANKS)
     F*==========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCodProduto
     C                   PARM                    WNuSequ
     C                   PARM                    WPtrOr
     C                   PARM                    WDatInicio
     C                   PARM                    WDisplayMode
      /FREE

       //------------------------------------
       // Se não estiver autorizado então sai
       //------------------------------------
       IF NOT $ValAccess(S#PGM:'':99999);
          *INLR = *ON;
          RETURN;
       ENDIF;

       ptrOR = WPtrOR;
       ECR_NOMSEG  = OR_SEG;
       ECR_RAMO    = OR_Ramo;
       ECR_APOLICE = OR_Apol;
       WIcDisplay = WDisplayMode;
       WIcErro    = *ON;
       EXSR SaveCapRisco;

       EXSR GetDetail;
       IF (NOT WIcErro) AND ((NOT WIcDisplay) OR (WIcExiste AND WIcDisplay  ));
          EXSR ShowItem;
       ENDIF;

       *INLR = *ON;
       RETURN;

       //============================================================
       // Subrotina..: SaveCapRisco
       // Objectivo..: repor Capitais de Risco
       //============================================================
       BEGSR SaveCapRisco;
          ECR_DSRISC001 = *BLANKS;
          ECR_CPRISC001 = *ZEROS;
          IF (OR_CR1 > *ZEROS);
             ECR_DSRISC001 = $getDescRisco(WCodProduto:OR_CR1);
             ECR_CPRISC001 = OR_CAP1;
          ENDIF;

          ECR_DSRISC002 = *BLANKS;
          ECR_CPRISC002 = *ZEROS;
          IF (OR_CR2 > *ZEROS);
             ECR_DSRISC002 = $getDescRisco(WCodProduto:OR_CR2);
             ECR_CPRISC002 = OR_CAP2;
          ENDIF;

          ECR_DSRISC003 = *BLANKS;
          ECR_CPRISC003 = *ZEROS;
          IF (OR_CR3 > *ZEROS);
             ECR_DSRISC003 = $getDescRisco(WCodProduto:OR_CR3);
             ECR_CPRISC003 = OR_CAP3;
          ENDIF;
       ENDSR;

       //============================================================
       // Subrotina..: GetDetail
       // Objectivo..: Obter o Detalhe do Item
       //============================================================
       BEGSR getDetail;
           WModoIO = 'S'; // Gravação
           CHAIN    (OR_Ramo:OR_Apol) RPRDP018;
           IF (%FOUND());
              ECR_CPEDIFIC = PRD_CPEDIFIC;
              WImPrmEdific = PRD_IMPREMIO;
           ELSE;
           ENDIF;

           CHAIN(N) (OR_Ramo:OR_Apol:WNuSequ) RPRDP017;
           IF (%FOUND());
               WModoIO = 'R'; // Leitura
               ECR_CDSEGURA = PRD_CDSEGUR;
               ECR_CDCRED   = PRD_CDCREDOR;
               ECR_DSOBS01  = PRD_DSOBSERV1;
               ECR_DSOBS02  = PRD_DSOBSERV2;
               ECR_CPSEGURO = PRD_CPSEGURO;
               ECR_CPSEGUROBK = PRD_CPSEGURO;
               ECR_IMPREMIO = PRD_IMPREMIO;
               ECR_PRFRAC   = PRD_PRFRAC;
               ECR_PRFRACBK = PRD_PRFRAC;
               ECR_CDFRAC   = PRD_CDFRAC;
               ECR_DSFRAC   = PRD_DSFRAC;
               ECR_NOSEGURA = $getDescEnt(PRD_CDSEGUR);
               ECR_NOCRED   = $getDescCred(PRD_CDCREDOR);
               WIcErro      = *OFF;
               WIcExiste    = *ON;
           ENDIF;

           CHAIN(N) (OR_Ramo:OR_Apol:WNuSequ) RPRDP010;
           IF (%FOUND());
              ECR_CPSEGURO = PRD_CPSEGURO;
              WIcErro      = *OFF;
           ENDIF;

           IF (NOT WicExiste);
              MONITOR;
                 CHAIN(N) (OR_Ramo:OR_Apol:WNuSequ) RPRDP012;
                 IF (%FOUND());
                    ECR_DSOBS01 = PRD_DSDETAIL;
                 ENDIF;
              ON-ERROR 1218; // Registo Bloqueado
              ENDMON;
           ENDIF;
       ENDSR;

       //============================================================
       // Subrotina..: ShowItem
       // Objectivo..: Mostrar detalhe do Item
       //============================================================
       BEGSR ShowItem;
          WExceptKeys = *BLANKS;
          DOW (NOT *IN03 AND NOT *IN12);
             IF (WDisplayMode);
                WNomFunction = 'RPRDE10505';
                WModoIO = 'R'; // Leitura
             ELSE;
                *IN45 = *ON;
                WNomFunction = 'RPRDE10521';
                WModoIO = 'S'; // Alteração
             ENDIF;

             ECR_NOSEGURA = $getDescEnt(ECR_CDSEGURA);
             ECR_NOCRED   = $getDescCred(ECR_CDCRED);
             DS#HEADER    = $getHeader(S#PGM:WNomFunction);
             DS#FOOTER    = $getFooter(S#PGM:WnomFunction:WExceptKeys);
             ECR_S#PGM    = S#PGM;
             WRITE     RPRDE10500;
             WRITE     RPRDE10599;
             EXFMT RPRDE10505;
             IF (Func#Key = KEY#F04) AND (WModoIO = 'S');
                 WRow    = ECR_CM#CROW;
                 WCol    = ECR_CM#CCOL;
                 EXSR PopUp;
                 ECR_CM#ROW  = WRow;
                 ECR_CM#COL  = WCol;
             ELSEIF ($ValidKey(Func#Key:KEY#F22)) AND (NOT WDisplayMode);
                WModoIO = 'D'; // Eliminação
                EXFMT RPRDE10522;
                IF (ECR_OPTION = 'S');
                   EXSR SaveRecord;
                   LEAVE;
                ENDIF;
                *IN12 = *ON;
             ELSEIF (Func#Key = KEY#Enter);
                EXSR SaveRecord;
                *IN12 = *ON;
             ENDIF;
          ENDDO;
       ENDSR;

       //========================================================
       // Subrotina.: popup
       // Objectivo.: Afixar uma lista de valoers possíveis
       //========================================================
       BEGSR popup;
          //----------------------------------------------
          // Validar qual o campo onde foi seleccionado
          //----------------------------------------------
          IF (ECR_CM#NMFLD = 'CDSEGURA');
             ECR_CDSEGURA = $runEntPopUp(ECR_CDSEGURA);
             ECR_NOSEGURA = $getDescEnt(ECR_CDSEGURA);
          ELSEIF (ECR_CM#NMFLD = 'CDCRED');
             ECR_CDCRED = $runCredPopUp(ECR_CDCRED);
             ECR_NOCRED = $getDescCred(ECR_CDCRED);
          ELSEIF (ECR_CM#NMFLD = 'CDPOSTAL');
          ENDIF;
       ENDSR;


       //============================================================
       // Subrotina..: SaveRecord
       // Objectivo..: Salvar o Registo
       //============================================================
       BEGSR SaveRecord;
          IF (WModoIO = 'S');
             EVAL(RH)WCpSeguro = ECR_CPEDIFIC*(ECR_PRFRAC/1000);
             IF (WCpSeguro > ECR_CPSEGURO);
                ECR_CPSEGURO = WCpSeguro;
             ENDIF;
             EVAL(RH)ECR_IMPREMIO = ECR_CPSEGURO*
                            (WImPrmEdific/PRD_CPEDIFIC);
             IF (WIcExiste);
                CHAIN (OR_Ramo:OR_Apol:WNuSequ) RPRDP017;
                PRD_CDSEGUR    = ECR_CDSEGURA;
                PRD_CDCREDOR   = ECR_CDCRED;
                PRD_DSOBSERV1  = ECR_DSOBS01;
                PRD_DSOBSERV2  = ECR_DSOBS02;
                PRD_IMPREMIO   = ECR_IMPREMIO;
                PRD_CPSEGURO   = ECR_CPSEGURO;
                PRD_PRFRAC     = ECR_PRFRAC;
                PRD_CDFRAC     = ECR_CDFRAC;
                PRD_DSFRAC     = ECR_DSFRAC;
                UPDATE RPRDP017 %FIELDS(PRD_CDSEGUR:PRD_CPSEGURO:
                                        PRD_CDCREDOR:PRD_PRFRAC:
                                        PRD_DSOBSERV1:PRD_CDFRAC:
                                        PRD_IMPREMIO:PRD_PRFRAC:
                                        PRD_DSOBSERV2:PRD_DSFRAC);
                EXSR UpdateRamo;
             ELSE;
                PRD_CDRAMO     = OR_RAMO;
                PRD_NUAPOL     = OR_Apol;
                PRD_NUSEQU     = WNuSequ;
                PRD_CDSEGUR    = ECR_CDSEGURA;
                PRD_CDCREDOR   = ECR_CDCRED;
                PRD_DSOBSERV1  = ECR_DSOBS01;
                PRD_DSOBSERV2  = ECR_DSOBS02;
                PRD_IMPREMIO   = ECR_IMPREMIO;
                PRD_CPSEGURO   = ECR_CPSEGURO;
                PRD_PRFRAC     = ECR_PRFRAC;
                PRD_CDFRAC     = ECR_CDFRAC;
                PRD_DSFRAC     = ECR_DSFRAC;
                WRITE RPRDP017;
                EXSR UpdateRamo;
             ENDIF;
          ELSEIF (WModoIO = 'D');
             DELETE (OR_Ramo:OR_Apol:WNuSequ) RPRDP010;
          ENDIF;
       ENDSR;

       //================================================================
       // Subrotina.: UpdateRamo
       // Objectivo.: Actualizar detalhe
       //================================================================
       BEGSR UpdateRamo;
          //-----------------------------------------------
          // Actualizar nó Principal
          //-----------------------------------------------
          CHAIN (OR_Ramo:OR_Apol:WNuSequ) RPRDP010;
          PRD_CPSEGURO   = ECR_CPSEGURO;
          UPDATE RPRDP010 %FIELDS(PRD_CPSEGURO);

          //-----------------------------------------------
          // Actualizar nó Secundário
          //-----------------------------------------------
          CHAIN (OR_Ramo:OR_Apol:WNuSequ) RPRDP012;
          WPremio = ECR_IMPREMIO;
          WDsFrac = '('+%TRIM(PRD_CDFRAC)+') '+PRD_DSFRAC;
          PRD_DSDETAIL = %SUBST(WDsFrac:1:20) +
                      ', '+%SUBST($getDescEnt(PRD_CDSEGUR):1:28)+
                      ', Prem.='+%EDITW(WPremio:'    ,  ¤');
          PRD_ICDETAIL = 'S';
          UPDATE RPRDP012 %FIELDS(PRD_DSDETAIL:PRD_ICDETAIL);
       ENDSR;
      /END-FREE
