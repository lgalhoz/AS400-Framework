     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PRDO103
     H* Descrição.: Manutenção de notas Desctrivas Habitações.
     H*
     H*==================================================================
     FFPRDP012  UF   E           K DISK    PREFIX(PRD_) COMMIT
     FFPRDP015  UF A E           K DISK    PREFIX(PRD_) COMMIT
     FSPRDE104  CF   E             WORKSTN INFDS(INFD#KEYS)
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
      /Copy QRPGLESRC,SGRLO013
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SCATO010
      /Copy QRPGLESRC,SSECO010
      /Copy QRPGLESRC,SDBSO010

     D*==> Estrutura de dados do Registo
     D RBASEDIO      E DS                  ExtName(GBASEDIO)
     D                                     Prefix(OR_)
     D                                     Based(ptrOR)
     D
     D*==> Parametros recebidos
     D WNuSequ         S              5S 0
     D WpLstRisco      S               *
     D WDisplayMode    S               N
     D
     D WIdx            S              5S 0
     D WIcExtract      S               N
     D WIcDisplay      S               N
     D WExceptKeys     S             24A   INZ(*BLANKS)
     D WNomFunction    S             10A   INZ(*BLANKS)
     D WDesRisco       S             20A   INZ(*BLANKS)
     D WDatInicio      S              8S 0
     D WPtrOR          S               *
     D WIcExiste       S               N
     D WIcErro         S               N
     D WRow            S              3S 0
     D WCol            S              3S 0
     D WCodProduto     S              5S 0
     D WHstNuSeq15     S              5S 0
     D WHstNuSeq02     S              5S 0
     D WSession        S              5S 0
     D WModoIO         S              1A   INZ(*BLANKS)
     D WSessScr        S              5S 0
     F*==========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCodProduto
     C                   PARM                    WNuSequ
     C                   PARM                    WptrOR
     C                   PARM                    WDatInicio
     C                   PARM                    WDisplayMode
      /FREE

       //------------------------------------
       // Se não estiver autorizado então sai
       //------------------------------------
       IF NOT $ValAccess(S#PGM:'':99999);
          RETURN;
       ENDIF;

       //------------------------------------
       // Preparar o ecrã
       //------------------------------------
       WSessScr = $AddScreen('SPRDE104':'RPRDE10405');
       $AddScPFld(%ADDR(ECR_F#00):'CPSEGURO');// Capital Seguro
       $AddScPFld(%ADDR(ECR_F#01):'DSITEM'); // Descrição do Item
       $AddScCoord(%ADDR(ECR_CM#ROW):%ADDR(ECR_CM#COL));
       $AddScMsg(%ADDR(ECR_ERR#MSG));

       ptrOr       = WptrOR;
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
       *IN12 = *OFF;
       *IN03 = *OFF;
       $RmvScreen(WSessScr);
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
           WModoIO   = 'S'; // Gravação
           WIcExiste = *OFF;
           WIcErro   = *OFF;
           WSession  = $SetFiles('FPRDP012');
           $AddDBDate($IntToDate(WDatInicio));
           $SetCriteria('(CDRAMO = '''+%CHAR(OR_Ramo)+''') AND +
                         (NUAPOL = '''+%CHAR(OR_Apol)+''') AND +
                         (NUSEQU = '''+%CHAR(WNuSequ)+''')');
           IF ($GetRecord(1) > *ZEROS);
              WHstNuSeq02  = %DEC($GetField('NUSEQHST'):5:0);
              ECR_CPSEGURO = %DEC($GEtField('CPSEGURO'):11:2);
              ECR_DSOBS01  = $GetField('DSDETAIL');
              WIcErro      = *OFF;
           ENDIF;
           $RmvDBSession(WSession);

           WSession  = $SetFiles('FPRDP015');
           $AddDBDate($IntToDate(WDatInicio));
           $SetCriteria('(CDRAMO = '''+%CHAR(OR_Ramo)+''') AND +
                         (NUAPOL = '''+%CHAR(OR_Apol)+''') AND +
                         (NUSEQU = '''+%CHAR(WNuSequ)+''')');
           IF ($GetRecord(1) > *ZEROS);
              WModoIO        = 'R'; // Leitura
              ECR_DSITEM     = $GetField('DSITEM');
              ECR_DSOBS01    = $GetField('DSOBSERV1');
              ECR_DSOBS02    = $GetField('DSOBSERV2');
              ECR_SELRISCO04 = $GetField('CDRISCO01');
              ECR_SELRISCO05 = $GetField('CDRISCO02');
              ECR_SELRISCO06 = $GetField('CDRISCO03');
              WHstNuSeq15    = %DEC($GetField('NUSEQHST'):5:0);
              WIcErro     = *OFF;
              WIcExiste   = *ON;
           ENDIF;
           $RmvDBSession(WSession);
       ENDSR;

       //============================================================
       // Subrotina..: ShowItem
       // Objectivo..: Mostrar detalhe do Item
       //============================================================
       BEGSR ShowItem;
          WExceptKeys = *BLANKS;
          DOW (NOT *IN03 AND NOT *IN12);
             IF (WDisplayMode);
                WNomFunction = 'RPRDE10405';
                WModoIO = 'R'; // Leitura
             ELSE;
                *IN45 = *ON;
                WNomFunction = 'RPRDE10421';
                WModoIO = 'S'; // Alteração
             ENDIF;

             DS#HEADER    = $getHeader(S#PGM:WNomFunction);
             DS#FOOTER    = $getFooter(S#PGM:WnomFunction:WExceptKeys);
             ECR_S#PGM    = S#PGM;
             WRITE     RPRDE10400;
             WRITE     RPRDE10499;
             EXFMT RPRDE10405;
             IF (Func#Key = KEY#F04) AND (WModoIO = 'S');
                 WRow    = ECR_CM#CROW;
                 WCol    = ECR_CM#CCOL;
                 EXSR PopUp;
                 ECR_CM#ROW  = WRow;
                 ECR_CM#COL  = WCol;
             ELSEIF ($ValidKey(Func#Key:KEY#F22)) AND (NOT WDisplayMode);
                WModoIO = 'D'; // Eliminação
                IF ($ShowDialog('PRD0191':*BLANKS:'NS') = 'S');
                   EXSR SaveRecord;
                   LEAVE;
                ENDIF;
                *IN12 = *ON;
             ELSEIF (Func#Key = KEY#Enter);
                IF (WModoIO = 'S');
                   EXSR Validar;
                   IF ($GetScStatus = 'O');
                      EXSR SaveRecord;
                      *IN12 = *ON;
                   ENDIF;
                ELSE;
                   *IN12 = *ON;
                ENDIF;
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
                CHAIN (OR_Ramo:OR_Apol:WNuSequ:WHstNuSeq15) RPRDP015;
                PRD_DSOBSERV1  = ECR_DSOBS01;
                PRD_DSOBSERV2  = ECR_DSOBS02;
                PRD_DSITEM     = ECR_DSITEM;
                PRD_CDRISCO01  = ECR_SELRISCO04;
                PRD_CDRISCO02  = ECR_SELRISCO05;
                PRD_CDRISCO03  = ECR_SELRISCO06;
                UPDATE RPRDP015 %FIELDS(PRD_DSOBSERV1:PRD_DSOBSERV2:
                                       PRD_DSITEM:PRD_CDRISCO01:
                                       PRD_CDRISCO02:PRD_CDRISCO03);
                CHAIN (OR_Ramo:OR_Apol:WNuSequ:WHstNuSeq02) RPRDP012;
                PRD_CPSEGURO   = ECR_CPSEGURO;
                PRD_DSDETAIL = ECR_DSITEM;
                PRD_ICDETAIL = 'S';
                 UPDATE RPRDP012 %FIELDS(PRD_DSDETAIL:PRD_ICDETAIL:
                                        PRD_CPSEGURO);
             ELSE;
                PRD_CDRAMO     = OR_RAMO;
                PRD_NUAPOL     = OR_Apol;
                PRD_NUSEQU     = WNuSequ;
                PRD_DSITEM     = ECR_DSITEM;
                PRD_DSOBSERV1  = ECR_DSOBS01;
                PRD_DSOBSERV2  = ECR_DSOBS02;
                PRD_CPSEGURO   = ECR_CPSEGURO;
                PRD_CDRISCO01  = ECR_SELRISCO04;
                PRD_CDRISCO02  = ECR_SELRISCO05;
                PRD_CDRISCO03  = ECR_SELRISCO06;
                WRITE RPRDP015;

                CHAIN (OR_Ramo:OR_Apol:WNuSequ:WHstNuSeq02) RPRDP012;
                PRD_CPSEGURO   = ECR_CPSEGURO;
                PRD_DSDETAIL = ECR_DSITEM;
                PRD_ICDETAIL = 'S';
                UPDATE RPRDP012 %FIELDS(PRD_DSDETAIL:PRD_ICDETAIL:
                                        PRD_CPSEGURO);
             ENDIF;
          ELSEIF (WModoIO = 'D');
             DELETE (OR_Ramo:OR_Apol:WNuSequ:WHstNuSeq15) RPRDP015;
             CHAIN (OR_Ramo:OR_Apol:WNuSequ:WHstNuSeq02) RPRDP012;
             PRD_CPSEGURO   = ECR_CPSEGURO;
             PRD_DSDETAIL = ECR_DSITEM;
             PRD_ICDETAIL = 'N';
             UPDATE RPRDP012 %FIELDS(PRD_DSDETAIL:PRD_ICDETAIL:PRD_CPSEGURO);
          ENDIF;
       ENDSR;

       //============================================================
       // Subrotina..: Validar
       // Objectivo..: Validar se o registo pode ser gravado
       //============================================================
       BEGSR Validar;
          $InzScStatus();
          //-----------------------------------------
          // Validar Capial Seguro preenchido
          //-----------------------------------------
          IF (ECR_CPSEGURO <= *ZEROS);
             $SetScError('CPSEGURO':'PRD0192');
          ENDIF;

          //-----------------------------------------
          // Validar Descrição do Item preenchida
          //-----------------------------------------
          IF (ECR_DSITEM = *BLANKS);
             $SetScError('DSITEM':'PRD0193');
          ENDIF;
       ENDSR;
      /END-FREE
