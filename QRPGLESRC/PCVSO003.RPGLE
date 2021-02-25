     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL) FIXNBR(*ZONED)
     H*==================================================================
     H* Programa..: PCVSO003
     H* Descrição.: Sincronizar alterações na nova Estrutura.
     H*             Para o efeito, lê as alterações do Base e Aplica as
     H*             alterações na nova estrutura.
     H*==================================================================
      /Copy QRPGLESRC,SGRLO010

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
     D*==> Definição dos Campos de Trabalho
     D  WRamo          S              2S 0
     D  WData          S              8S 0
     D  WRegisto       S               *
     D  WDynamicCall   S             10A
     D*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    WRamo
     C                   PARM                    WData
     C                   PARM                    WRegisto
     C
      /FREE
        //----------------------------------------------------
        // Actualizar Coberturas
        //----------------------------------------------------
        MONITOR;
           WDynamicCall = 'RCVSO'+%TRIM(%EDITW(WRamo:'0  '))+'1';
           Addcobertura(WRamo:Wdata:WRegisto);
        ON-ERROR;
        ENDMON;

        //----------------------------------------------------
        // Actualizar Riscos
        //----------------------------------------------------
        MONITOR;
           WDynamicCall = 'RCVSO'+%TRIM(%EDITW(WRamo:'0  '))+'2';
           AddRiscos(WRamo:Wdata:WRegisto);
        ON-ERROR;
        ENDMON;

        //----------------------------------------------------
        // Actualizar Clausulas
        //----------------------------------------------------
        MONITOR;
           WDynamicCall = 'RCVSO'+%TRIM(%EDITW(WRamo:'0  '))+'3';
           AddClausulas(WRamo:Wdata:WRegisto);
        ON-ERROR;
        ENDMON;

        *INLR = *ON;
        RETURN;
      /END-FREE
