     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PPRDO111
     H* Descrição.: Alterar ou definir um novo número SegurNET.
     H*==================================================================
     FGBASEGRX  UF A E           K DISK    PREFIX(X_)
     FFPRDE111  CF   E             WORKSTN PREFIX(ECR_)
     F                                     INFDS(INFD#KEYS)
     F*==========================================================
     F* Indicadores  Descrição
     F* -----------  --------------------------------------------
     F*      03      Tecla F3
     F*      12      Tecla F12
     D*==========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SCATO010
      /Copy QRPGLESRC,SSECO010

     D*==> Estrutura de dados do Registo
     D RBASEDIA      E DS                  ExtName(GBASEDIA)
     D                                     Prefix(AU_)
     D                                     Based(ptrOR)
     D
     D*==> Pré-visualização das Actas
     D  PrintPreview   PR                  EXTPGM('RPRDO005')
     D   ptrOR                         *   CONST
     D   Cdfunction                   2A   CONST
     D   Data                         8S 0 CONST
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
     D WDBSession      S              5S 0 INZ(*ZEROS)
     D WSessCapRis     S              5S 0 INZ(*ZEROS)
     D WRecAct         S              5S 0 INZ(*ZEROS)
     D WCdRisco        S              5S 0 INZ(*ZEROS)
     D WCapRisco       S             11S 2 INZ(*ZEROS)
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
          $ShowDialog('SEC0001');
          RETURN;
       ENDIF;

       ptrOr       = WptrOR;
       ECR_NOMSEG  = AU_SEG;
       ECR_RAMO    = AU_RAMO;
       ECR_APOLICE = AU_APOL;
       WIcDisplay = WModo; // *ON
       IF (NOT WIcDisplay);
          *IN45 = *ON;
       ELSE;
          *IN45 = *OFF;
       ENDIF;
       EXSR ShowInforma;
       *IN12 = *OFF;
       *IN03 = *OFF;
       RETURN;

       //============================================================
       // Subrotina..: ShowInforma
       // Objectivo..: Apresentar valores para poder gerir
       //============================================================
       BEGSR ShowInforma;
          IF (WIcDisplay);
             WNomfunction = 'RPRDE11105';
          ELSE;
             WNomFunction = 'RPRDE11121';
          ENDIF;

          CHAIN (AU_RAMO:AU_APOL) GBASEGRX;
          IF (NOT %FOUND());
             ECR_NUMSEGNET = *BLANKS;
          ELSE;
             ECR_NUMSEGNET = X_NOSNET;
          ENDIF;

          ECR_MARCA     = AU_MARC1;
          ECR_MODELO    = AU_MODELO;
          ECR_MATRIC    = AU_MATR1;

          ECR_S#PGM = S#PGM;
          DOW (NOT *IN03 AND NOT *IN12);
             DS#HEADER    = $getHeader(S#PGM:WNomFunction);
             DS#FOOTER    = $getFooter(S#PGM:WNomFunction:*BLANKS);
             WRITE RPRDE11100;
             WRITE RPRDE11199;
             EXFMT RPRDE11105;
             IF ($ValidKey(Func#Key:KEY#F21)) AND WIcDisplay;
                //WIcDisplay = *OFF;
                //WNomFunction = 'RPRDE11121';
             ELSEIF (Func#Key = KEY#F04) AND (NOT WIcDisplay);
                WRow    = ECR_CM#CROW;
                WCol    = ECR_CM#CCOL;
                EXSR PopUp;
          ELSEIF (Func#Key = KEY#F14);
             MONITOR;
                PrintPreview(WPtrOr:'NI':WdatInicio);
             ON-ERROR;
             ENDMON;
             ELSEIF (Func#Key = KEY#Enter) AND (NOT WIcDisplay);
                EXSR SaveRegisto;
                LEAVE;  // Sair para o programa chamador
             ENDIF;
          ENDDO;
       ENDSR;

       //========================================================
       // Subrotina.: SaveRegisto
       // Objectivo.: Salvar o registo
       //========================================================
       BEGSR SaveRegisto;
          CHAIN (AU_RAMO:AU_APOL) GBASEGRX;
          IF (NOT %FOUND());
             X_NOSNET = ECR_NUMSEGNET;
             X_RAMO   = AU_RAMO;
             X_APOL   = AU_APOL;
             WRITE BASEGRFX;
          ELSE;
             X_NOSNET = ECR_NUMSEGNET;
             UPDATE BASEGRFX %FIELDS(X_NOSNET);
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: PopUp
       // Objectivo.: Afixar uma lista de valoers possíveis
       //========================================================
       BEGSR PopUp;
          //----------------------------------------------
          // Validar qual o campo onde foi seleccionado
          //----------------------------------------------
          IF (ECR_CM#NMFLD = 'CDTIPCOB');
          ENDIF;
          ECR_CM#ROW  = WRow;
          ECR_CM#COL  = WCol;
       ENDSR;
      /END-FREE
