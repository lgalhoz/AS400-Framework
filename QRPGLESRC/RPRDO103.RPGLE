     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PRDO103
     H* Descrição.: Manutenção de notas Desctrivas Habitações.
     H*
     H*==================================================================
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
      /Copy QRPGLESRC,SGRLO013
      /Copy QRPGLESRC,SENTO010
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SCATO010
      /Copy QRPGLESRC,SDBSO010
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
     D WHstNuSeq13     S              5S 0
     D WHstNuSeq02     S              5S 0
     D WSession        S              5S 0
     D WModoIO         S              1A   INZ(*BLANKS)
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
       WSessScr = $AddScreen('SPRDE103':'RPRDE10305');
       $AddScPFld(%ADDR(ECR_F#00):'CPSEGURO');// Capital Seguro
       $AddScPFld(%ADDR(ECR_F#01):'CDSEGURA');// Código de Segurado
       $AddScPFld(%ADDR(ECR_F#02):'MORADA');  // Morada
       $AddScPFld(%ADDR(ECR_F#03):'CDPOSTAL');// Código Postal
       $AddScPFld(%ADDR(ECR_F#04):'LOCAL');   // Local da Morada
       $AddScPFld(%ADDR(ECR_F#05):'CDCRED');  // Credor Hipotecário
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
           WModoIO = 'S'; // Gravação
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

           WSession  = $SetFiles('FPRDP013');
           $AddDBDate($IntToDate(WDatInicio));
           $SetCriteria('(CDRAMO = '''+%CHAR(OR_Ramo)+''') AND +
                         (NUAPOL = '''+%CHAR(OR_Apol)+''') AND +
                         (NUSEQU = '''+%CHAR(WNuSequ)+''')');
           IF ($GetRecord(1) > *ZEROS);
              WModoIO = 'R'; // Leitura
              ECR_CDSEGURA = %DEC($GetField('CDSEGUR'):6:0);
              ECR_MORADA   = $GetField('MORADA');
              ECR_CDPOSTAL = %DEC($GetField('CDPOSTAL'):7:0);
              ECR_LOCAL    = $GetField('LOCALIDADE');
              ECR_CDCRED   = %DEC($GetField('CDCREDOR'):6:0);
              ECR_DSOBS01  = $GetField('DSOBSERV1');
              ECR_DSOBS02  = $GetField('DSOBSERV2');
              ECR_NOSEGURA = $getDescEnt(ECR_CDSEGURA);
              ECR_NOCRED   = $getDescCred(ECR_CDCRED);
              WHstNuSeq13    = %DEC($GetField('NUSEQHST'):5:0);
              WIcErro      = *OFF;
              WIcExiste    = *ON;
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
                WNomFunction = 'RPRDE10305';
                WModoIO = 'R'; // Leitura
             ELSE;
                *IN45 = *ON;
                WNomFunction = 'RPRDE10321';
                WModoIO = 'S'; // Alteração
             ENDIF;

             ECR_NOSEGURA = $getDescEnt(ECR_CDSEGURA);
             ECR_NOCRED   = $getDescCred(ECR_CDCRED);
             ECR_DSPOSTAL = $GetCodPost(ECR_CDPOSTAL);
             DS#HEADER    = $getHeader(S#PGM:WNomFunction);
             DS#FOOTER    = $getFooter(S#PGM:WnomFunction:WExceptKeys);
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
          IF (ECR_CM#NMFLD = 'CDSEGURA');
             ECR_CDSEGURA = $runEntPopUp(ECR_CDSEGURA);
             ECR_NOSEGURA = $getDescEnt(ECR_CDSEGURA);
          ELSEIF (ECR_CM#NMFLD = 'CDCRED');
             ECR_CDCRED = $runCredPopUp(ECR_CDCRED);
             ECR_NOCRED = $getDescCred(ECR_CDCRED);
          ELSEIF (ECR_CM#NMFLD = 'CDPOSTAL');
             ECR_CDPOSTAL = $runCPPopUp(ECR_CDPOSTAL);
             ECR_DSPOSTAL = $GetCodPost(ECR_CDPOSTAL);
          ENDIF;
       ENDSR;


       //============================================================
       // Subrotina..: SaveRecord
       // Objectivo..: Salvar o Registo
       //============================================================
       BEGSR SaveRecord;
          IF (WModoIO = 'S');
             IF (WIcExiste);
                CHAIN (OR_Ramo:OR_Apol:WNuSequ:WHstNuSeq13) RPRDP013;
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

                CHAIN (OR_Ramo:OR_Apol:WNuSequ:WHstNuSeq02) RPRDP012;
                PRD_CPSEGURO = ECR_CPSEGURO;
                PRD_DSDETAIL = PRD_MORADA;
                PRD_ICDETAIL = 'S';
                UPDATE RPRDP012 %FIELDS(PRD_DSDETAIL:PRD_ICDETAIL:
                                        PRD_CPSEGURO);
             ELSE;
                PRD_CDRAMO     = OR_RAMO;
                PRD_NUAPOL     = OR_Apol;
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

                CHAIN (OR_Ramo:OR_Apol:WNuSequ:WHstNuSeq02) RPRDP012;
                PRD_CPSEGURO = ECR_CPSEGURO;
                PRD_DSDETAIL = ECR_MORADA;
                PRD_ICDETAIL = 'S';
                UPDATE RPRDP012 %FIELDS(PRD_DSDETAIL:PRD_ICDETAIL:
                                        PRD_CPSEGURO);
             ENDIF;
          ELSEIF (WModoIO = 'D');
             DELETE (OR_Ramo:OR_Apol:WNuSequ:WHstNuSeq13) RPRDP013;
             CHAIN (OR_Ramo:OR_Apol:WNuSequ:WHstNuSeq02) RPRDP012;
             PRD_CPSEGURO   = ECR_CPSEGURO;
             PRD_DSDETAIL = ECR_MORADA;
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
          // Validar Código do Segurado
          //-----------------------------------------
          IF (ECR_CDSEGURA <= *ZEROS);
             $SetScError('CDSEGURA':'PRD0194');
          ENDIF;

          //-----------------------------------------
          // Validar Se Morada Preenchida
          //-----------------------------------------
          IF (ECR_MORADA = *BLANKS);
             $SetScError('MORADA':'PRD0195');
          ENDIF;

          //-----------------------------------------
          // Validar Código Postal
          //-----------------------------------------
          IF (ECR_CDPOSTAL <= *ZEROS);
             $SetScError('CDPOSTAL':'PRD0186');
          ENDIF;

          //-----------------------------------------
          // Validar Local
          //-----------------------------------------
          IF (ECR_LOCAL = *BLANKS);
             $SetScError('LOCAL':'PRD0197');
          ENDIF;

          //-----------------------------------------
          // Validar Código do Credor Hipotecário
          //-----------------------------------------
          IF (ECR_CDCRED < *ZEROS);
             $SetScError('CDCRED':'PRD0198');
          ENDIF;
       ENDSR;
      /END-FREE
