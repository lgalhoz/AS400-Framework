     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PPRDO107
     H* Descri��o.: Caracteriza��o do risco do Edificio, quanto � natureza
     H*             da sua contru��o e edificios adjacentes.
     H* Observa��o: Gest�o inteligente da informa��o de Historico
     H*==================================================================
     FFPRDT001  IF   E           K DISK    PREFIX(PRD_)
     FFPRDP016  UF A E           K DISK    PREFIX(PRD_) COMMIT
     FFPRDE107  CF   E             WORKSTN PREFIX(ECR_)
     F                                     INFDS(INFD#KEYS)
     F*==========================================================
     F* Indicadores  Descri��o
     F* -----------  --------------------------------------------
     F*      03      Tecla F3
     F*      12      Tecla F12
     D*==========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SGRLO013
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SCATO010
      /Copy QRPGLESRC,SSECO010

     D*==> Estrutura de dados do Registo
     D RBASEDIO      E DS                  ExtName(GBASEDIO)
     D                                     Prefix(OR_)
     D                                     Based(ptrOR)
     D
     D*==> Pr�-visualiza��o das Actas
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
     D WSessScr        S              5S 0
     C*========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCodProduto
     C                   PARM                    WDatInicio
     C                   PARM                    WPtrOR
     C                   PARM                    WModo
      /FREE

       //------------------------------------
       // Se n�o estiver autorizado ent�o sai
       //------------------------------------
       IF NOT $ValAccess(S#PGM:'':99999);
          $ShowDialog('SEC0001');
          RETURN;
       ENDIF;

       ptrOr       = WptrOR;

       //---------------------------------------------
       // Validar se os dados est�o convertidos
       //---------------------------------------------
       CHAIN (OR_RAMO:OR_APOL) RPRDP001;
       IF (%FOUND());
          IF (PRD_TPACTA = 'A');
             $ShowDialog('PRD0160');
             RETURN;
          ENDIF;
       ELSE;
          $ShowDialog('PRD0161');
          RETURN;
       ENDIF;

       //---------------------------------------------
       // Preparar o tratamento do ecr�
       //---------------------------------------------
       WSessScr = $AddScreen('FPRDE107':'RPRDE10705');
       $AddScPFld(%ADDR(ECR_F#00):'CDESTCONS');// Estado de Conserva��o
       $AddScPFld(%ADDR(ECR_F#01):'CDTIPCONS');// Tipo de Constru��o
       $AddScPFld(%ADDR(ECR_F#02):'CDTIPCOB'); // Tipo de Cobertura
       $AddScPFld(%ADDR(ECR_F#03):'CDEDIADJ'); // Edificio Adjacente
       $AddScPFld(%ADDR(ECR_F#04):'LCRISCO');  // Local  de Risco
       $AddScPFld(%ADDR(ECR_F#05):'ANO');      // Ano de Constru��o
       $AddScPFld(%ADDR(ECR_F#06):'CDPOST');   // C�digo Postal
       $AddScCoord(%ADDR(ECR_CM#ROW):%ADDR(ECR_CM#COL));
       $AddScMsg(%ADDR(ECR_ERR#MSG));

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
       *IN12 = *OFF;
       *IN03 = *OFF;

       $RmvScreen(WSessScr);
       RETURN;

       //============================================================
       // Subrotina..: ShowInforma
       // Objectivo..: Apresentar valores para poder gerir
       //============================================================
       BEGSR ShowInforma;
          IF (WIcDisplay);
             WNomfunction = 'RPRDE10705';
          ELSE;
             WNomFunction = 'RPRDE10721';
          ENDIF;

          WDBSEssion = $SetFiles('FPRDP016');
          $AddDBDate($IntToDate(WDatInicio));
          $SetCriteria('(CDRAMO='''+%CHAR(OR_RAMO)+''') +
                    AND (NUAPOL='''+%CHAR(OR_Apol)+''')');
          IF ($getRecord(1:1) > *ZEROS);
             MONITOR;
                ECR_CDPOST = %DEC($Getfield('CDPOST'):7:0);
             ON-ERROR;
                ECR_CDPOST = *ZEROS;
             ENDMON;
             MONITOR;
                ECR_NUSEQU = %DEC($GetField('NUSEQU'):5:0);
             ON-ERROR;
                ECR_NUSEQU = *ZEROS;
             ENDMON;
             ECR_CDTIPCOB  = $GetField('CDTIPCOB');
             ECR_CDESTCONS = $GetField('CDESTCONS');
             ECR_CDTIPCONS = $GetField('CDTIPCONS');
             ECR_CDEDIADJ  = $GetField('CDEDIADJ');
          ELSE;
             ECR_CDTIPCOB  = OR_CF22;
             ECR_CDESTCONS = OR_ESTCON;
             ECR_CDTIPCONS = OR_TIPCON;
             ECR_CDEDIADJ  = OR_CF21;
          ENDIF;
          $RmvDBSession(WDBSession);

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
             WRITE RPRDE10700;
             WRITE RPRDE10799;
             EXFMT RPRDE10705;
             IF ($ValidKey(Func#Key:KEY#F21)) AND WIcDisplay;
                //WIcDisplay = *OFF;
                //WNomFunction = 'RPRDE10721';
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
                EXSR Validar;
                IF ($GetScStatus = 'O');
                   EXSR SaveRegisto;
                   LEAVE;  // Sair para o programa chamador
                ENDIF;
                ECR_DSPOST    = $GetCodPost(ECR_CDPOST);
                ECR_DSTIPCOB  = $getDescricao(51:ECR_CDTIPCOB);
                ECR_DSESTCONS = $getDescricao(52:ECR_CDESTCONS);
                ECR_DSTIPCONS = $getDescricao(50:ECR_CDTIPCONS);
                ECR_DSEDIADJ  = $getDescricao(49:ECR_CDEDIADJ);
             ENDIF;
          ENDDO;
       ENDSR;

       //========================================================
       // Subrotina.: Validar
       // Objectivo.: Validar os Campos do Ecr�
       //========================================================
       BEGSR Validar;
          $InzScStatus();
          //-----------------------------------------
          // Validar Estado de Conserva��o
          //-----------------------------------------
          IF (ECR_CDESTCONS = *BLANKS);
             $SetScError('CDESTCONS':'PRD0181');
          ELSE;
             IF (NOT $ValCodigo(52:ECR_CDESTCONS));
                $SetScError('CDESTCONS':'PRD0181');
             ENDIF;
          ENDIF;

          //-----------------------------------------
          // Validar Tipo de Constru��o
          //-----------------------------------------
          IF (ECR_CDTIPCONS = *BLANKS);
             $SetScError('CDTIPCONS':'PRD0182');
          ELSE;
             IF (NOT $ValCodigo(50:ECR_CDTIPCONS));
                $SetScError('CDTIPCONS':'PRD0182');
             ENDIF;
          ENDIF;

          //-----------------------------------------
          // Validar Tipo de Cobertura
          //-----------------------------------------
          IF (ECR_CDTIPCOB = *BLANKS);
             $SetScError('CDTIPCOB':'PRD0183');
          ELSE;
             IF (NOT $ValCodigo(51:ECR_CDTIPCOB));
                $SetScError('CDTIPCOB':'PRD0183');
             ENDIF;
          ENDIF;

          //-----------------------------------------
          // Validar Edificio Adjacente
          //-----------------------------------------
          IF (ECR_CDEDIADJ = *BLANKS);
             $SetScError('CDEDIADJ':'PRD0187');
          ELSE;
             IF (NOT $ValCodigo(49:ECR_CDEDIADJ));
                $SetScError('CDEDIADJ':'PRD0187');
             ENDIF;
          ENDIF;

          //-----------------------------------------
          // Validar Local de Risco
          //-----------------------------------------
          IF (ECR_LCRISCO = *BLANKS);
             $SetScError('LCRISCO':'PRD0185');
          ENDIF;

          //-----------------------------------------
          // Validar C�digo Postal
          //-----------------------------------------
          IF (ECR_CDPOST <= *ZEROS);
             $SetScError('CDPOST':'PRD0186');
          ENDIF;

          //-----------------------------------------
          // Validar ano do Edificio
          //-----------------------------------------
          IF (ECR_ANO > %SUBDT(%DATE():*YEARS) OR (ECR_ANO < 1800));
             $SetScError('ANO':'PRD0184');
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: SaveRegisto
       // Objectivo.: Salvar o registo
       //========================================================
       BEGSR SaveRegisto;

          IF (ECR_NUSEQU > *ZEROS);
             CHAIN (OR_RAMO:OR_APOL:ECR_NUSEQU) RPRDP016;
             PRD_CDTIPCOB  = ECR_CDTIPCOB;
             PRD_CDESTCONS = ECR_CDESTCONS;
             PRD_CDTIPCONS = ECR_CDTIPCONS;
             PRD_CDEDIADJ  = ECR_CDEDIADJ;
             UPDATE RPRDP016 %FIELDS(PRD_CDTIPCOB:PRD_CDESTCONS:
                                     PRD_CDTIPCONS:PRD_CDEDIADJ);
          ELSE;
             PRD_CDRAMO    = OR_RAMO;
             PRD_NUAPOL    = OR_APOL;
             PRD_CDTIPCOB  = ECR_CDTIPCOB;
             PRD_CDESTCONS = ECR_CDESTCONS;
             PRD_CDTIPCONS = ECR_CDTIPCONS;
             PRD_CDEDIADJ  = ECR_CDEDIADJ;
             WRITE RPRDP016;
          ENDIF;
          OR_ANOEDI = ECR_ANO;
          OR_LRPS   = ECR_LCRISCO;
          OR_CPEDI  = %INT(ECR_CDPOST / 1000);

          OR_CF22   = 'S';

          IF (ECR_CDESTCONS = 'B');
             OR_ESTCON = '1';
          ELSEIF (ECR_CDESTCONS = 'D');
             OR_ESTCON = '2';
          ELSEIF (ECR_CDESTCONS = 'M');
             OR_ESTCON = '3';
          ENDIF;

          OR_TIPCON = %SUBST(ECR_CDTIPCONS:3:1);

          IF (PRD_CDEDIADJ <> '00000');
             OR_CF21   = 'S';
             OR_DCF21  = $getDescricao(49:ECR_CDEDIADJ);
          ELSE;
             OR_CF21   = *BLANKS;
             OR_DCF21  = *BLANKS;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: PopUp
       // Objectivo.: Afixar uma lista de valoers poss�veis
       //========================================================
       BEGSR PopUp;
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
          ECR_DSRISC001 = *BLANKS;
          ECR_CPRISC001 = *ZEROS;
          ECR_DSRISC002 = *BLANKS;
          ECR_CPRISC002 = *ZEROS;
          ECR_DSRISC003 = *BLANKS;
          ECR_CPRISC003 = *ZEROS;
          ECR_DSRISC004 = *BLANKS;
          ECR_CPRISC004 = *ZEROS;
          WRecAct = 1;
          WSessCapRis = $SetFiles('FPRDP011');
          $AddDBDate($IntToDate(WDatInicio));
          $SetCriteria('(CDRAMO='''+%CHAR(OR_RAMO)+''') +
                    AND (NUAPOL='''+%CHAR(OR_Apol)+''')');
          DOW ($GetRecord(WRecAct) > *ZEROS);
             WCdRisco = $GetFieldNum('CDRISCO');
             WCapRisco = $GetFieldNum('IMCPRISC');
             IF (ECR_DSRISC001 = *BLANKS);
                ECR_DSRISC001 = $getDescRisco(WCodProduto:WCdRisco);
                ECR_CPRISC001 = WCapRisco;
             ELSEIF (ECR_DSRISC002 = *BLANKS);
                ECR_DSRISC002 = $getDescRisco(WCodProduto:WCdRisco);
                ECR_CPRISC002 = WCapRisco;
             ELSEIF (ECR_DSRISC003 = *BLANKS);
                ECR_DSRISC003 = $getDescRisco(WCodProduto:WCdRisco);
                ECR_CPRISC003 = WCapRisco;
             ELSEIF (ECR_DSRISC004 = *BLANKS);
                ECR_DSRISC004 = $getDescRisco(WCodProduto:WCdRisco);
                ECR_CPRISC004 = WCapRisco;
             ENDIF;
             WRecAct += 1;
          ENDDO;
          $RmvDBSession(WSessCapRis);
       ENDSR;
      /END-FREE
