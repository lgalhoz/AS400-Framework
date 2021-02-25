     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PRDO103
     H* Descrição.: Manutenção de notas Desctrivas Habitações.
     H*
     H*==================================================================
     FFPRDP010  UF   E           K DISK    PREFIX(PRD_) COMMIT
     FFPRDP012  UF   E           K DISK    PREFIX(PRD_) COMMIT
     FFPRDP013  UF A E           K DISK    PREFIX(PRD_) COMMIT
     FSPRDE103  CF   E             WORKSTN INFDS(INFD#KEYS)
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
     D
     D*==> Parametros recebidos
     D WRamo           S              5S 0
     D WApolice        S              7S 0
     D WNome           S             40A
     D WNuSequ         S              5S 0
     D WpLstRisco      S               *
     D WDisplayMode    S               N
     D
     D #LstRisco       DS                  QUALIFIED DIM(12)
     D   CdRisco                      5S 0
     D   CapSeguroOr                 11S 2
     D   CapSeguroNv                 11S 2
     D pLstRisco       S               *   INZ(*NULL)
     D WIdxRisco       S              5S 0
     D WIdx            S              5S 0
     D WIcExtract      S               N
     D WIcDisplay      S               N
     D WExceptKeys     S             24A   INZ(*BLANKS)
     D WNomFunction    S             10A   INZ(*BLANKS)
     D WIcExiste       S               N
     D WIcErro         S               N
     D WRow            S              3S 0
     D WCol            S              3S 0
     D WModoIO         S              1A   INZ(*BLANKS)
     F*==========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WRamo
     C                   PARM                    WApolice
     C                   PARM                    WNome
     C                   PARM                    WNuSequ
     C                   PARM                    WpLstRisco
     C                   PARM                    WDisplayMode
      /FREE

       //------------------------------------
       // Se não estiver autorizado então sai
       //------------------------------------
       IF NOT ValAccess(S#PGM:'':99999);
          *INLR = *ON;
          RETURN;
       ENDIF;

       ECR_NOMSEG  = WNome;
       ECR_RAMO    = WRamo;
       ECR_APOLICE = WApolice;
       pLstRisco  = WpLstRisco;
       WIcDisplay = WDisplayMode;
       WIcErro    = *ON;

       EXSR GetDetail;
       IF (NOT WIcErro) AND ((NOT WIcDisplay) OR (WIcExiste AND WIcDisplay  ));
          EXSR ShowItem;
       ENDIF;

       *INLR = *ON;
       RETURN;

       //============================================================
       // Subrotina..: GetDetail
       // Objectivo..: Obter o Detalhe do Item
       //============================================================
       BEGSR getDetail;
           WModoIO = 'S'; // Gravação
           CHAIN(N) (WRamo:WApolice:WNuSequ) RPRDP013;
           IF (%FOUND());
               WModoIO = 'R'; // Leitura
               ECR_CDSEGURA = PRD_CDSEGUR;
               ECR_MORADA   = PRD_MORADA;
               ECR_CDPOSTAL = PRD_CDPOSTAL;
               ECR_LOCAL    = PRD_LOCALIDADE;
               ECR_CDCRED   = PRD_CDCREDOR;
               ECR_DSOBS01  = PRD_DSOBSERV1;
               ECR_DSOBS02  = PRD_DSOBSERV2;
               WIcErro     = *OFF;
               WIcExiste   = *ON;
           ENDIF;

           CHAIN(N) (WRamo:WApolice:WNuSequ) RPRDP010;
           IF (%FOUND());
              ECR_CPSEGURO = PRD_CPSEGURO;
              WIcErro      = *OFF;
           ENDIF;

           IF (NOT WicExiste);
              MONITOR;
                 CHAIN(N) (WRamo:WApolice:WNuSequ) RPRDP012;
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
                WNomFunction = 'RPRDE10305';
                WModoIO = 'R'; // Leitura
             ELSE;
                *IN45 = *ON;
                WNomFunction = 'RPRDE10321';
                WModoIO = 'S'; // Alteração
             ENDIF;

             DS#HEADER    = getHeader(S#PGM:WNomFunction);
             DS#FOOTER    = getFooter(S#PGM:WnomFunction:WExceptKeys);
             ECR_S#PGM    = S#PGM;
             WRITE     RPRDE10300;
             WRITE     RPRDE10399;
             EXFMT RPRDE10305;
             IF (Func#Key = KEY#F04) AND (WModoIO = 'S');
                 WRow    = ECR_CM#CROW;
                 WCol    = ECR_CM#CCOL;
                 EXSR PopUp;
                 ECR_CM#ROW  = WRow;
                 ECR_CM#COL  = WCol;
             ELSEIF (Func#Key = KEY#F22) AND (NOT WDisplayMode);
                WModoIO = 'D'; // Eliminação
                EXFMT RPRDE10322;
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
          IF (ECR_CM#NMFLD = 'WDOMINIO');
          // WDominio = runPopUp(3:WDominio);
          ELSEIF (ECR_CM#NMFLD = 'WCDMODULO');
          // WCdModulo = runPopUp(4:WCdModulo);
          ENDIF;
       ENDSR;


       //============================================================
       // Subrotina..: SaveRecord
       // Objectivo..: Salvar o Registo
       //============================================================
       BEGSR SaveRecord;
          IF (WModoIO = 'S');
             IF (WIcExiste);
                CHAIN (WRamo:WApolice:WNuSequ) RPRDP013;
                PRD_CDSEGUR    = ECR_CDSEGURA;
                PRD_MORADA     = ECR_MORADA;
                PRD_CDPOSTAL   = ECR_CDPOSTAL;
                PRD_LOCALIDADE = ECR_LOCAL;
                PRD_CDCREDOR   = ECR_CDCRED;
                PRD_DSOBSERV1  = ECR_DSOBS01;
                PRD_DSOBSERV2  = ECR_DSOBS02;
                UPDATE RPRDP013 %FIELDS(PRD_CDSEGUR:PRD_MORADA:PRD_CDPOSTAL:
                                        PRD_LOCALIDADE:PRD_CDCREDOR:
                                        PRD_DSOBSERV1:PRD_DSOBSERV2);

                CHAIN (WRamo:WApolice:WNuSequ) RPRDP010;
                PRD_CPSEGURO   = ECR_CPSEGURO;
                UPDATE RPRDP010 %FIELDS(PRD_CPSEGURO);
                CHAIN (WRamo:WApolice:WNuSequ) RPRDP012;
                PRD_DSDETAIL = PRD_MORADA;
                PRD_ICDETAIL = 'S';
                UPDATE RPRDP012 %FIELDS(PRD_DSDETAIL:PRD_ICDETAIL);
             ELSE;
                PRD_CDPROD     = WRamo;
                PRD_NUAPOL     = WApolice;
                PRD_NUSEQU     = WNuSequ;
                PRD_CDSEGUR    = ECR_CDSEGURA;
                PRD_MORADA     = ECR_MORADA;
                PRD_CDPOSTAL   = ECR_CDPOSTAL;
                PRD_LOCALIDADE = ECR_LOCAL;
                PRD_CDCREDOR   = ECR_CDCRED;
                PRD_DSOBSERV1  = ECR_DSOBS01;
                PRD_DSOBSERV2  = ECR_DSOBS02;
                PRD_CPSEGURO   = ECR_CPSEGURO;
                WRITE RPRDP013;
                CHAIN (WRamo:WApolice:WNuSequ) RPRDP010;
                PRD_CPSEGURO   = ECR_CPSEGURO;
                UPDATE RPRDP010 %FIELDS(PRD_CPSEGURO);
                CHAIN (WRamo:WApolice:WNuSequ) RPRDP012;
                PRD_DSDETAIL = PRD_MORADA;
                PRD_ICDETAIL = 'S';
                UPDATE RPRDP012 %FIELDS(PRD_DSDETAIL:PRD_ICDETAIL);
             ENDIF;
          ELSEIF (WModoIO = 'D');
             DELETE (WRamo:WApolice:WNuSequ) RPRDP010;
          ENDIF;
       ENDSR;
      /END-FREE
