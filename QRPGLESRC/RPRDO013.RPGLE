     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: RPRDO013
     H* Descrição.: Condições Especiais Clausulas
     H*
     H*==================================================================
     FFSECP010  IF   E           K DISK    PREFIX('SEC_')
     F*==================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SCATO010
      /Copy QRPGLESRC,SPRDO002
      /Copy QRPGLESRC,SENTO010
      /Copy QRPGLESRC,SDBSO010

     D*==> Estrutura de dados do Registo
     D RBASEDIO      E DS                  ExtName(GBASEDIO)
     D                                     Prefix(OR_)
     D                                     Based(ptrOR)
     D
     D*==> Estrutura de dados do Histórico
     D RGEXAP        E DS                  ExtName(GEXAP)
     D                                     Prefix(HST_)
     D                                     Based(ptrHist)
     D
     D*==> Parametros definidos para a Clausula ou Condição
     D GetExtrParm     PR                  EXTPGM(DynamicCall)
     D   CodProduto                   5S 0 CONST
     D   DatInicio                    8S 0 CONST
     D   ptrOR                         *   CONST
     D   Modo                          N   CONST
     D   Resultado                  100A   CONST
     D   Bullet                       3A   CONST
     D   CodErro                      7A   CONST
     D DynamicCall     S             10A   INZ(*BLANKS)
     D
     D*==> Riscos da Apólice
     D Lst#Riscos      DS                  QUALIFIED DIM(25)
     D   CdRisco                      5S 0 INZ(*ZEROS)
     D   CapRisco                    11S 2 INZ(*ZEROS)
     D   IsEmpty                       N   INZ(*ON)
     D WIdxRisco       S              5S 0 INZ(*ZEROS)
     D
     D*==> Campos de Trabalho
     D WptrHist        S               *
     D WptrOr          S               *
     D
     D WCdItem         S              3A
     D WBullet         S              3A
     D WLastItem       S              3A
     D WVariavel       S            100A
     D WTexto          S            500A
     D WTpItem         S              1A
     D WCdProd         S              5S 0
     D WCdRegra        S              5S 0
     D WSessPRD        S              5S 0 INZ(*ZEROS)
     D WSessCAT        S              5S 0 INZ(*ZEROS)
     D WRecAct         S              5S 0 INZ(*ZEROS)
     D WCdRisco        S              5S 0 INZ(*ZEROS)
     D WCapRisco       S             11S 2 INZ(*ZEROS)
     D WIsToSelect     S               N
     D WIcObriga       S               N
     D WCodErro        S              7A
     F*==========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCdProd
     C                   PARM                    WRamo
     C                   PARM                    WApolice
     C                   PARM                    WDatInicio
     C                   PARM                    WCodErro
     C                   PARM                    WPtrHist
     C                   PARM                    WPtrOR
      /FREE
       EXSR InzVariaveis; // Limpara campos de Trabalho
       EXSR GetRiscApol;  // Obter os Riscos da Apólice
       EXSR AddTexto;     // Formatar as Linhas de Texto

       $RmvDBSession(WSessPRD);
       $RmvDBSession(WSessCAT);
       RETURN;

       //==================================================================
       // Subrotina..: InzVariaveis
       // Objectivo..: Limpara as variáveis para poderem ser reutilizadas
       //==================================================================
       BEGSR InzVariaveis;
          WCodErro = *BLANKS;
          WIdxRisco = 1;
          DOW (WIdxRisco < 25);
             LST#Riscos(WIdxRisco).CdRisco = *ZEROS;
             LST#Riscos(WIdxRisco).CapRisco = *ZEROS;
             LST#Riscos(WIdxRisco).IsEmpty = *ON;
             WIdxRisco += 1;
          ENDDO;
       ENDSR;

       //==================================================================
       // Subrotina..: getRiscApol
       // Objectivo..: Obter os Riscos da Apólice
       //==================================================================
       BEGSR getRiscApol;
          WIdxRisco = 1;
          WRecAct   = 1;
          LST#Riscos(WIdxRisco).CdRisco = *ZEROS;
          LST#Riscos(WIdxRisco).CapRisco = *ZEROS;
          LST#Riscos(WIdxRisco).IsEmpty = *OFF;

          WSessPRD = $SetFiles('FPRDP011');
          $AddDBDate($IntToDate(WDatInicio));
          $SetCriteria('(CDRAMO='''+%CHAR(WRamo)+''') +
                    AND (NUAPOL='''+%CHAR(WApolice)+''')');
          DOW ($GetRecord(WRecAct) > *ZEROS);
             WCdRisco = $GetFieldNum('CDRISCO');
             WCapRisco = $GetFieldNum('IMCPRISC');
             WIdxRisco += 1;
             LST#Riscos(WIdxRisco).CdRisco = WCdRisco;
             LST#Riscos(WIdxRisco).CapRisco = WCapRisco;
             LST#Riscos(WIdxRisco).IsEmpty = *OFF;
             LST#Riscos(1).CapRisco += WCapRisco;
             WRecAct += 1;
          ENDDO;
       ENDSR;

       //==================================================================
       // Subrotina..: AddTexto
       // Objectivo..: Adicionar o texto das Coberturas Base e Coberturas
       //              Facultativas.
       //==================================================================
       BEGSR AddTexto;
          $AddTextLine();
          $AddLineType('H1');
          $AddTextLine('CONDIÇÕES ESPECIAIS E CLÁUSULAS APLICÁVEIS');

          $AddLineType('T1');
          $AddTextLine('Tem aplicação neste contrato as Condições +
                  Especiais e Cláusulas cujos números abaixo se indicam:');
          $AddLineType('B6');

          //------------------------------------------
          // Obter Condições Especiais e Clausulas
          //------------------------------------------
          WLastItem = *BLANKS;
          WRecAct   = 1;
          WSessCAT  = $SetFiles('FCATP108 A INNER JOIN FCATP109 B ON +
                                 (A.CDPROD = B.CDPROD) AND +
                                 (A.NUSEQU = B.NUSEQU)');
          $SetFields('FCATP108':'A');
          $SetFields('FCATP109':'B':*OFF);
          $AddDBDate($IntToDate(WDatInicio));
          $SetCriteria('(A.CDPROD='''+%CHAR(WCdProd)+''')');
          DOW ($GetRecord(WRecAct) > *ZEROS);
             WCdItem = $GetField('A.CDITEM');
             WTexto  = $GetFieldText('B.TEXTO');
             IF (WLastItem <> WCdItem);
                WLastItem = WCdItem;

                //------------------------------------------------------------------------
                // Seleccionar apenas as Condições e Clausulas para os riscos da Apólice
                //------------------------------------------------------------------------
                WCdRisco  = $GetfieldNum('A.CDRISCO');
                WIcObriga = $GetField('A.ICOBRIGA');
                WTpItem   = $GetField('A.TPITEM');
                WCdRegra  = $GetFieldNum('A.CDVAR');

                WIsToSelect = *ON;
                IF (WCdRisco <> *ZEROS);
                   WIsToSelect = *OFF;
                   WIdxRisco = 1;
                   DOW (NOT LST#Riscos(WIdxRisco).IsEmpty);
                      IF (WCdRisco = LST#Riscos(WIdxRisco).CdRisco);
                         WIsToSelect = *ON;
                         LEAVE;
                      ENDIF;
                      WIdxRisco += 1;
                   ENDDO;
                ENDIF;

                //------------------------------------------------------------------------
                // Selecionar os Registos
                //------------------------------------------------------------------------
                IF (WIsToSelect) AND (WIcObriga <> 'S');
                   WSessPRD = $SetFiles('FPRDP009');
                   $AddDBDate($IntToDate(WDatInicio));
                   $SetCriteria('(CDRAMO='''+%CHAR(WRamo)+''') AND +
                                 (NUAPOL='''+%CHAR(WApolice)+''') AND +
                                 (CDITEM='''+%TRIM(WCdItem)+''')');
                   IF ($GetRecord() = *ZEROS);
                      WIsToSelect = *OFF;
                   ENDIF;
                   $rmvDBSession(WSessPRD);
                ENDIF;
                WBullet = WCdItem;

                WVariavel  = *BLANKS;
                IF (WCdRegra > *ZEROS) AND (WIsToSelect);
                   WVariavel  = '<Não definido>';
                   EXSR GetVariavel;
                   IF (WCodErro <> *BLANKS);
                      LEAVESR;
                   ENDIF;
                ENDIF;
             ELSE;
                WBullet = '???';
             ENDIF;

             IF (WIsToSelect);
                IF (WCdRegra > *ZEROS);
                   WTexto = %TRIM($StrReplace('&VAR01':%TRIM(WVariavel):
                                               WTexto));
                ENDIF;
                IF (WVariavel <> '<NOP>');
                   $AddTextLine(WTexto:1:WBullet);
                ENDIF;
             ENDIF;
             WRecAct += 1;
          ENDDO;
       ENDSR;

       //==================================================================
       // Subrotina..: getVariavel
       // Objectivo..: Obter a variável definida para a Clausula.
       //==================================================================
       BEGSR getVariavel;
          CHAIN (WCdRegra) RSECP010;
          IF (%FOUND());
             DynamicCall = %TRIM(SEC_NMPROGR);
             MONITOR;
                // WPtrHist
                // WPtrOR
                GetExtrParm(WCdProd:WDatInicio:WPtrOR:*OFF:
                            WVariavel:WBullet:WCodErro);
                DynamicCall = *BLANKS;
             ON-ERROR;
             ENDMON;
          ENDIF;
       ENDSR;
      /END-FREE
