     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL) FIXNBR(*ZONED)
     H*==================================================================
     H* Programa..: PCVSO002
     H* Descrição.: Importar Base para a Nova Estrutura.
     H*==================================================================
      /Copy QRPGLESRC,SGRLO010

     D*==> Estrutura dos comandos AS400
     D Exec            PR                  EXTPGM('QCMDEXC')
     D   stdcmd                     300A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5
     D WStdCmd         S            300A
     D WStdLen         S             15P 5
     D
     D*==> Adicionar/Actualizar um registo de uma Cobertura
     D AddCobertura    PR                  EXTPGM(WDynamicCall)
     D  Ramo                          2S 0 CONST
     D  Data                          8S 0 CONST
     D  Registo                        *   CONST
     D
     D*==> Adicionar Riscos da Apólice e Capitais
     D AddRiscos       PR                  EXTPGM(WDynamicCall)
     D  Ramo                          2S 0 CONST
     D  Data                          8S 0 CONST
     D  Registo                        *   CONST
     D
     D*==> Adicionar Clausulas
     D AddClausulas    PR                  EXTPGM(WDynamicCall)
     D  Ramo                          2S 0 CONST
     D  Data                          8S 0 CONST
     D  Registo                        *   CONST
     D
     D*==> Adicionar Clausulas
     D AddCaracter     PR                  EXTPGM(WDynamicCall)
     D  Ramo                          2S 0 CONST
     D  Data                          8S 0 CONST
     D  Registo                        *   CONST
     D
     D*==> Definição dos Campos de Trabalho
     D WRamo           S              2S 0
     D WData           S              8S 0
     D PData           S              8S 0
     D WHoje           S              8S 0
     D WDatDia         S              6A
     D WDat6Chr        S              6A
     D WDat6Num        S              6S 0
     D WRegisto        S               *
     D WDynamicCall    S             10A
     D WMsgErro        S              7A
     D*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    WRamo
     C                   PARM                    WRegisto
     C                   PARM                    WMsgErro
     C                   PARM                    PData
     C
      /FREE
        WMsgErro = *BLANKS;
        WHoje    = $DateToInt(%DATE());
        MONITOR;
           WData = PData;
        ON-ERROR;
           WData = Whoje;
        ENDMON;

        //----------------------------------------------------
        // Salvaguardar a data do Sistema
        //----------------------------------------------------
        IF (WData < Whoje);
           WDat6Num = $CvtData($DateToInt($Today()):'YYMD':'DMY');
           WDatDia = %TRIM(%EDITW(WDat6Num:'0      '));

           //----------------------------------------------------
           // Actualizar Data do Sistema
           //----------------------------------------------------
           MONITOR;
              WDat6Num = $CvtData(WData:'YYMD':'DMY');
              WDat6Chr = %TRIM(%EDITW(WDat6Num:'0      '));
              WStdCmd  = 'CHGJOB DATE('+WDat6Chr+')';
              WStdLen  = %LEN(WStdCmd);
              EXEC(WStdCmd:WStdLen);
           ON-ERROR;
              WMsgErro = 'CVS0001';
              $ShowDialog(WMsgErro);
              RETURN;
           ENDMON;
        ENDIF;

        //----------------------------------------------------
        // Actualizar Coberturas
        //----------------------------------------------------
        MONITOR;
           WDynamicCall = 'RCVSO'+%TRIM(%EDITW(WRamo:'0  '))+'1';
           Addcobertura(WRamo:Wdata:WRegisto);
        ON-ERROR;
           WMsgErro = 'CVS0002';
           RETURN;
        ENDMON;

        //----------------------------------------------------
        // Actualizar Riscos
        //----------------------------------------------------
        MONITOR;
           WDynamicCall = 'RCVSO'+%TRIM(%EDITW(WRamo:'0  '))+'2';
           AddRiscos(WRamo:Wdata:WRegisto);
        ON-ERROR;
           WMsgErro = 'CVS0003';
           RETURN;
        ENDMON;

        //----------------------------------------------------
        // Actualizar Clausulas
        //----------------------------------------------------
        MONITOR;
           WDynamicCall = 'RCVSO'+%TRIM(%EDITW(WRamo:'0  '))+'3';
           AddClausulas(WRamo:Wdata:WRegisto);
        ON-ERROR;
           WMsgErro = 'CVS0004';
           RETURN;
        ENDMON;

        //----------------------------------------------------
        // Actualizar Característico do Risco
        //----------------------------------------------------
        MONITOR;
           WDynamicCall = 'RCVSO'+%TRIM(%EDITW(WRamo:'0  '))+'4';
           AddCaracter(WRamo:Wdata:WRegisto);
        ON-ERROR;
           WMsgErro = 'CVS0005';
           RETURN;
        ENDMON;

        //----------------------------------------------------
        // Repor a data do Sistema
        //----------------------------------------------------
        IF (WData < Whoje);
           MONITOR;
              WStdCmd  = 'CHGJOB DATE('+WDatDia+')';
              WStdLen  = %LEN(WStdCmd);
              EXEC(WStdCmd:WStdLen);
           ON-ERROR;
              WMsgErro = 'CVS0006';
              RETURN;
           ENDMON;
        ENDIF;
        RETURN;
      /END-FREE
