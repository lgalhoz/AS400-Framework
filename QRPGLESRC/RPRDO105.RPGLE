     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PRDO105
     H* Descrição.: Manutenção de notas Desctrivas Habitações.
     H*             Condominio.
     H*==================================================================
     FFPRDP012  UF   E           K DISK    PREFIX(PRD_) COMMIT
     FFPRDP017  UF A E           K DISK    PREFIX(PRD_) COMMIT
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
      /Copy QRPGLESRC,SGRLO013
      /Copy QRPGLESRC,SENTO010
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
     D WHstNuSeq17     S              5S 0
     D WHstNuSeq02     S              5S 0
     D WSession        S              5S 0
     D WCpEdific       S             11S 2
     D WImPrmEdific    S                   LIKE(PRD_IMPREMIO)
     D WModoIO         S              1A   INZ(*BLANKS)
     D WCpSeguro       S                   LIKE(PRD_CPSEGURO)
     D WPremio         S              5S 2 INZ(*ZEROS)
     D WDsFrac         S             40A   INZ(*BLANKS)
     D WSessScr        S              5S 0
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
          RETURN;
       ENDIF;

       //------------------------------------
       // Preparar o ecrã
       //------------------------------------
       WSessScr = $AddScreen('SPRDE105':'RPRDE10505');
       $AddScPFld(%ADDR(ECR_F#00):'CDSEGURA');  // Proprietário do Imóvel
       $AddScPFld(%ADDR(ECR_F#01):'CDFRAC');    // Código da Fracção
       $AddScPFld(%ADDR(ECR_F#02):'DSFRAC');    // Descrição da Fracção
       $AddScPFld(%ADDR(ECR_F#03):'PRFRAC');    // Permilagem da Fracção
       $AddScPFld(%ADDR(ECR_F#04):'CPSEGURO');  // Capital Seguro
       $AddScPFld(%ADDR(ECR_F#05):'CDCRED');    // Credor Hipotecário
       $AddScPFld(%ADDR(ECR_F#06):'IMCREDOR');  // Responsabilidade do Credor
       $AddScPFld(%ADDR(ECR_F#07):'DSOBS01');   // Texto de Observações 1
       $AddScPFld(%ADDR(ECR_F#08):'DSOBS02');   // Texto de Observações 2
       $AddScCoord(%ADDR(ECR_CM#ROW):%ADDR(ECR_CM#COL));
       $AddScMsg(%ADDR(ECR_ERR#MSG));

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
       $RmvScreen(WSessScr);
       *IN12 = *OFF;
       *IN03 = *OFF;
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
           WIcExiste = *OFF;
           WIcErro   = *OFF;
           WSession  = $SetFiles('FPRDP018');
           $AddDBDate($IntToDate(WDatInicio));
           $SetCriteria('(CDRAMO = '''+%CHAR(OR_Ramo)+''') AND +
                         (NUAPOL = '''+%CHAR(OR_Apol)+''')');
           IF ($GetRecord() > *ZEROS);
              ECR_CPEDIFIC = %DEC($GetField('CPEDIFIC'):11:2);
              WCpEdific    = ECR_CPEDIFIC;
              WImPrmEdific = %DEC($GetField('IMPREMIO'):11:2);
           ELSE;
              EXSR ShowInfo;
              LEAVESR;
              $RmvDBSession(WSession);
           ENDIF;
           $RmvDBSession(WSession);

           WSession  = $SetFiles('FPRDP012');
           $AddDBDate($IntToDate(WDatInicio));
           $SetCriteria('(CDRAMO = '''+%CHAR(OR_Ramo)+''') AND +
                         (NUAPOL = '''+%CHAR(OR_Apol)+''') AND +
                         (NUSEQU = '''+%CHAR(WNuSequ)+''')');
           IF ($GetRecord() > *ZEROS);
              WHstNuSeq02  = %DEC($GetField('NUSEQHST'):5:0);
              ECR_CPSEGURO = %DEC($GEtField('CPSEGURO'):11:2);
              ECR_DSOBS01  = $GetField('DSDETAIL');
              WIcErro      = *OFF;
           ENDIF;
           $RmvDBSession(WSession);

           WSession  = $SetFiles('FPRDP017');
           $AddDBDate($IntToDate(WDatInicio));
           $SetCriteria('(CDRAMO = '''+%CHAR(OR_Ramo)+''') AND +
                         (NUAPOL = '''+%CHAR(OR_Apol)+''') AND +
                         (NUSEQU = '''+%CHAR(WNuSequ)+''')');
           IF ($GetRecord() > *ZEROS);
              WHstNuSeq17  = $GetFieldNum('NUSEQHST');
              WModoIO      = 'R'; // Leitura
              ECR_CDSEGURA = $GetFieldNum('CDSEGUR');
              ECR_CDCRED   = $GetFieldNum('CDCREDOR');
              ECR_IMCREDOR = $GetFieldNum('IMCREDOR');
              ECR_DSOBS01  = $GetField('DSOBSERV1');
              ECR_DSOBS02  = $GetField('DSOBSERV2');
              ECR_CPSEGURO = $GetFieldNum('CPSEGURO');
              ECR_CPSEGUROBK = PRD_CPSEGURO;
              ECR_IMPREMIO = $GetFieldNum('IMPREMIO');
              ECR_PRFRAC   = $GetFieldNum('PRFRAC');
              ECR_PRFRACBK = ECR_PRFRAC;
              ECR_CDFRAC   = $GetField('CDFRAC');
              ECR_DSFRAC   = $GetField('DSFRAC');
              ECR_NOSEGURA = $getDescEnt(ECR_CDSEGURA);
              ECR_NOCRED   = $getDescCred(ECR_CDCRED);
              WIcErro      = *OFF;
              WIcExiste    = *ON;
           ELSE;
              ECR_CDSEGURA = OR_NSEG;
              ECR_NOSEGURA = $getDescEnt(ECR_CDSEGURA);
           ENDIF;
           $RmvDBSession(WSession);
       ENDSR;

       //============================================================
       // Subrotina..: ShowInfo
       // Objectivo..: Mostrar mensagem de informação
       //============================================================
       BEGSR ShowInfo;
          WExceptKeys = *BLANKS;
          WNomFunction = 'RPRDE10508';
          WModoIO = 'R'; // Leitura

          ECR_NOSEGURA = $getDescEnt(ECR_CDSEGURA);
          ECR_NOCRED   = $getDescCred(ECR_CDCRED);
          DS#HEADER    = $getHeader(S#PGM:WNomFunction);
          DS#FOOTER    = $getFooter(S#PGM:WnomFunction:WExceptKeys);
          ECR_S#PGM    = S#PGM;
          WRITE     RPRDE10500;
          WRITE     RPRDE10599;
          EXFMT RPRDE10508;
       ENDSR;

       //============================================================
       // Subrotina..: ShowItem
       // Objectivo..: Mostrar detalhe do Item
       //============================================================
       BEGSR ShowItem;
          $InzScStatus();
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
                IF ($ShowDialog('PRD0191':*BLANKS:'NS') = 'S');
                   EXSR SaveRecord;
                   LEAVE;
                ENDIF;
                *IN12 = *ON;
             ELSEIF (Func#Key = KEY#Enter);
                EXSR Validar;
                IF ($GetScStatus = 'O');
                   EXSR SaveRecord;
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
                            (WImPrmEdific/WCpEdific);
             IF (WIcExiste);
                CHAIN (OR_Ramo:OR_Apol:WNuSequ:WHstNuSeq17) RPRDP017;
                PRD_CDSEGUR    = ECR_CDSEGURA;
                PRD_CDCREDOR   = ECR_CDCRED;
                IF (PRD_CDCREDOR = *ZEROS);
                   PRD_IMCREDOR = *ZEROS;
                ELSE;
                   IF (PRD_IMCREDOR <> *ZEROS);
                      PRD_IMCREDOR = ECR_IMCREDOR;
                   ELSE;
                      PRD_IMCREDOR = ECR_CPSEGURO;
                   ENDIF;
                ENDIF;
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
                                        PRD_IMCREDOR:
                                        PRD_DSOBSERV2:PRD_DSFRAC);
             ELSE;
                PRD_CDRAMO     = OR_RAMO;
                PRD_NUAPOL     = OR_Apol;
                PRD_NUSEQU     = WNuSequ;
                PRD_CDSEGUR    = ECR_CDSEGURA;
                PRD_CDCREDOR   = ECR_CDCRED;
                IF (PRD_CDCREDOR = *ZEROS);
                   PRD_IMCREDOR = *ZEROS;
                ELSE;
                   IF (PRD_IMCREDOR <> *ZEROS);
                      PRD_IMCREDOR = ECR_IMCREDOR;
                   ELSE;
                      PRD_IMCREDOR = ECR_CPSEGURO;
                   ENDIF;
                ENDIF;
                PRD_DSOBSERV1  = ECR_DSOBS01;
                PRD_DSOBSERV2  = ECR_DSOBS02;
                PRD_IMPREMIO   = ECR_IMPREMIO;
                PRD_CPSEGURO   = ECR_CPSEGURO;
                PRD_PRFRAC     = ECR_PRFRAC;
                PRD_CDFRAC     = ECR_CDFRAC;
                PRD_DSFRAC     = ECR_DSFRAC;
                WRITE RPRDP017;
             ENDIF;

             CHAIN (OR_Ramo:OR_Apol:WNuSequ:WHstNuSeq02) RPRDP012;
             PRD_CPSEGURO = ECR_CPSEGURO;
             WPremio = ECR_IMPREMIO;
             WDsFrac = '('+%TRIM(PRD_CDFRAC)+') '+PRD_DSFRAC;
             IF (PRD_CDSEGUR > *ZEROS);
                PRD_DSDETAIL = %TRIM(%SUBST(WDsFrac:1:20)) +
                               ','+%SUBST($getDescEnt(PRD_CDSEGUR):1);
             ELSE;
                PRD_DSDETAIL = %TRIM(%SUBST(WDsFrac:1:20));
             ENDIF;
             %SUBST(PRD_DSDETAIL:43)=',Prem.'+%EDITW(WPremio:'    ,  ¤');
             PRD_ICDETAIL = 'S';
             UPDATE RPRDP012 %FIELDS(PRD_DSDETAIL:PRD_ICDETAIL:
                                     PRD_CPSEGURO);
          ELSEIF (WModoIO = 'D');
             DELETE (OR_Ramo:OR_Apol:WNuSequ:WHstNuSeq17) RPRDP017;
             CHAIN (OR_Ramo:OR_Apol:WNuSequ:WHstNuSeq02) RPRDP012;
             PRD_CPSEGURO = ECR_CPSEGURO;
             WPremio = ECR_IMPREMIO;
             WDsFrac = '('+%TRIM(PRD_CDFRAC)+') '+PRD_DSFRAC;
             IF (PRD_CDSEGUR > *ZEROS);
                PRD_DSDETAIL = %TRIM(%SUBST(WDsFrac:1:20)) +
                               ','+%SUBST($getDescEnt(PRD_CDSEGUR):1);
             ELSE;
                PRD_DSDETAIL = %TRIM(%SUBST(WDsFrac:1:20));
             ENDIF;
             %SUBST(PRD_DSDETAIL:43)=',Prem.'+%EDITW(WPremio:'    ,  ¤');
             PRD_ICDETAIL = 'N';
             UPDATE RPRDP012 %FIELDS(PRD_DSDETAIL:PRD_ICDETAIL:
                                     PRD_CPSEGURO);
          ENDIF;
       ENDSR;

       //============================================================
       // Subrotina..: Validar
       // Objectivo..: Validar se o registo pode ser gravado
       //============================================================
       BEGSR Validar;
          $InzScStatus();
          //-----------------------------------------
          //
          //-----------------------------------------
          IF (ECR_CDSEGURA <= *ZEROS);
             $SetScError('CDSEGURA':'PRD0208');
          ENDIF;
          IF (ECR_CDFRAC = *BLANKS);
             $SetScError('CDFRAC':'PRD0209');
          ENDIF;
          IF (ECR_DSFRAC = *BLANKS);
             $SetScError('DSFRAC':'PRD0210');
          ENDIF;
          IF (ECR_PRFRAC <= *ZEROS);
             $SetScError('PRFRAC':'PRD0211');
          ENDIF;

          IF (ECR_CDCRED > *ZEROS);
          // IF (NOT $ValCredor(ECR_CDCRED));
          //    $SetScError('IMCRED':'PRD0213');
          // ENDIF;
             IF (ECR_IMCREDOR <= *ZEROS);
                $SetScError('IMCREDOR':'PRD0212');
             ENDIF;
          ENDIF;
       ENDSR;
      /END-FREE
