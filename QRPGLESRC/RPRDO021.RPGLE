     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: RPRDO021
     H* Descri��o.: Informa��o sobre os Credores Hipotec�rios
     H*
     H*==================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SCATO010
      /Copy QRPGLESRC,SPRDO002
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SENTO010

     D WptrHist        S               *
     D WptrOr          S               *
     D WLinha          S            500A
     D WCdProd         S              5S 0
     D WSession        S              5S 0
     D WRecAct         S              3S 0
     D WIcTitulo       S               N
     D WDsCredor       S             50A
     D WCodErro        S              7A
     C*==========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCdProd
     C                   PARM                    WRamo
     C                   PARM                    WApolice
     C                   PARM                    WDatInicio
     C                   PARM                    WCodErro
     C                   PARM                    WPtrHist
     C                   PARM                    WPtrOR
      /FREE
       WCodErro  = *BLANKS;
       WIcTitulo = *ON;
       WRecAct = 1;
       WSession = $SetFiles('FPRDP017');
       $AddDBDate($IntToDate(WDatInicio));
       $SetCriteria('(CDRAMO = '''+%CHAR(WRamo)+''') AND +
                     (NUAPOL = '''+%CHAR(WApolice)+''')');
       DOW ($GetRecord(WRecAct) > *ZEROS);
          IF ($GetFieldNum('CDCREDOR') > *ZEROS);
             EXSR AddTexto; // ForCompr as Linhas de Texto
          ENDIF;
          WRecAct += 1;
       ENDDO;
       $RmvDBSession(WSession);

       IF (NOT WIcTitulo);
          $addLineType('H2');
          $AddTextLine();
          WLinha  = 'Esta Ap�lice n�o pode ser Anulada, Alterada nem +
                     Cancelada sem o Pr�vio Consentimento dos Credores +
                     em cima Referenciados.';
          $AddTextLine(WLinha);
          WIcTitulo = *OFF;
       ENDIF;

       RETURN;

       //================================================================
       // Subrotina..: AddTexto
       // Objectivo..: Escrever os textos nas Ap�lices referente �s
       //              indexa��es.
       //================================================================
       BEGSR AddTexto;
          //------------------------------------
          // Escrever o Cabe�alho
          //------------------------------------
          IF (WIcTitulo);
             $addLineType('H1');
             $AddTextLine();
             WLinha  = 'Frac��es com Credor Hipotec�rio';
             $AddTextLine(WLinha);
             WIcTitulo = *OFF;
          ENDIF;

          //------------------------------------
          // Escrever informa��o da Frac��o
          //------------------------------------
          $addLineType('T1');
          WDsCredor = $getDescCred($GetFieldNum('CDCREDOR'));
          WLinha = 'O '+%TRIM(WDsCredor)+', tem interesse neste Seguro, +
                    na qualidade de Credor Hipotec�rio da Frac��o ['+
                    %TRIM($GetField('CDFRAC'))+'] '+%TRIM($GetField('DSFRAC'))+
                    '.';
          $AddTextLine(WLinha);
       ENDSR;
      /END-FREE
