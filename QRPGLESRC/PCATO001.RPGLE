     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PCATO001
     H* Descrição.: Manutenção de Produtos
     H*==================================================================
     FFSECP010  IF   E           K DISK    PREFIX(SEC_)
     FFCATP100  UF A E           K DISK    PREFIX(CAT_) COMMIT
     FFCATE001  CF   E             WORKSTN SFILE(RCATE00102:REC#)
     F                                     PREFIX(ECR_)
     F                                     INFDS(INFD#KEYS)
     F*==========================================================
     F* Indicadores  Descrição
     F* -----------  --------------------------------------------
     F*      45      Condicionar Campos com Input/Output
     F*      52      DSP
     F*      53      CLR
     F*      54      END
     D*==========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SGRLO013
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SSECO010

     D*==> Descontos e Agravamentos
     D DspDescontos    PR                  EXTPGM('PCATO015')
     D   WCdProd                      5S 0 CONST
     D   WDsProd                     50A   CONST
     D
     D*==> Coberturas
     D DspCobertura    PR                  EXTPGM('PCATO002')
     D   WCdProd                      5S 0 CONST
     D   WDsProd                     50A   CONST
     D
     D*==> Códigos de Risco
     D DspRisco        PR                  EXTPGM('PCATO003')
     D   WCdProd                      5S 0 CONST
     D   WDsProd                     50A   CONST
     D
     D*==> Cláusulas e Condições
     D DspClausulas    PR                  EXTPGM('PCATO005')
     D   WCdProd                      5S 0 CONST
     D   WDsProd                     50A   CONST
     D
     D*==> Limites e Franquias
     D DspLimites      PR                  EXTPGM('PCATO006')
     D   WCdProd                      5S 0 CONST
     D   WDsProd                     50A   CONST
     D
     D*==> Textos
     D DspTextos       PR                  EXTPGM('PCATO004')
     D   WCdProd                      5S 0 CONST
     D   WDsProd                     50A   CONST
     D
     D*==> Estrutura de dados do Registo
     D WIcErro         S               N   INZ(*OFF)
     D WIcOrigem       S              1A
     D REC#            S              4S 0
     D
     D WIcDisplay      S               N   INZ(*ON)
     D WModo           S               N
     D WNoFunction     S             10A   INZ(*BLANKS)
     D WSelected       S               N
     D WModoIO         S              1A
     D WSessionID      S              5S 0
     D WSessScr        S              5S 0
     D
     D*==> Definição das Constantes
     D IO#Criar        C                   'I'
     D IO#Consultar    C                   'C'
     D IO#Alterar      C                   'A'
     D IO#Eliminar     C                   'E'
     C*========================================================
      /FREE

       //------------------------------------
       // Se não estiver autorizado então sai
       //------------------------------------
       IF NOT $ValAccess(S#PGM);
          *INLR = *ON;
          RETURN;
       ENDIF;

       //------------------------------------
       // Preparar o ecrã
       //------------------------------------
       WSessScr = $AddScreen('FCATE001':'RCATE00105');
       $AddScPFld(%ADDR(ECR_F#00):'DSPROD');    // Descrição do Produto
       $AddScPFld(%ADDR(ECR_F#01):'CDRAMO');    // Código do Ramo
       $AddScPFld(%ADDR(ECR_F#02):'CDVARIA');   // Código de Variante
       $AddScPFld(%ADDR(ECR_F#03):'CDSEGURA');  // Seguradora
       $AddScPFld(%ADDR(ECR_F#04):'CDFUNCT');   // Código do Programa
       $AddScPFld(%ADDR(ECR_F#05):'ALIAS');     // Código Alternativo
       $AddScPFld(%ADDR(ECR_F#06):'CONVERSAO'); // Código de Conversão
       $AddScPFld(%ADDR(ECR_F#07):'ICMIGRAR');  // Código de Conversão
       $AddScCoord(%ADDR(ECR_CM#ROW):%ADDR(ECR_CM#COL));
       $AddScMsg(%ADDR(ECR_ERR#MSG));

       WModoIO = IO#Consultar;
       WSessionID = $SetFiles('FCATP100');
       $SetPage(12);
       $addCriteria('Ramo.......:':'CDRAMO':'GE');
       $addCriteria('Descrição..:':'DSPROD':'LK');
       ECR_DSCRITER = $getCritText();
       WIcDisplay =*ON;

       DOW (NOT *IN03 AND NOT *IN12);
          EXSR inzRecords;
          EXSR getRecords;
          WNoFunction = 'RCATE00103'; // Listar Componentes
          DS#HEADER    = $getHeader(S#PGM:WNoFunction);
          DS#FOOTER    = $getFooter(S#PGM:WNoFunction:*BLANKS);
          DS#OPTIONS   = $getOption(S#PGM:WNoFunction:*BLANKS);
          ECR_S#PGM    = S#PGM;
          WRITE     RCATE00100;
          WRITE     RCATE00199;
          WRITE     RCATE00198;
          EXFMT RCATE00103;
          IF (Func#Key = KEY#F05);
             $ChgCriteria();
             ECR_DSCRITER = $getCritText();
          ELSEIF ($ValidKey(Func#Key:KEY#F06));
             WModoIO = IO#Criar;
             EXSR DspProduto;
             $SetDBSession(WsessionID);
          ELSEIF ((Func#Key = KEY#PgDown) AND (NOT $IsLastPage())) OR
             ((Func#Key = KEY#PgUp) AND ($GetPageNbr() > 1));
          ELSEIF (Func#Key = KEY#Enter);
             EXSR SelRec;
             *IN12 = *OFF;
          ENDIF;
       ENDDO;

       $RmvDBSession(WSessionID);
       *INLR = *ON;
       $RmvScreen(WSessScr);
       RETURN;

       //========================================================
       // Subrotina.: InzRecTxt
       // Objectivo.: Inicializar a lista de registos
       //========================================================
       BEGSR inzRecords;
          REC#      = *ZEROS;   // Controlo de Registos da Lista
          *In53     = *On;      // Limpar Lista de Ecrã
          *In52     = *On;      // Mostrar a Lista de Ecrã
          WRITE RCATE00103;     // Limpar Lista
          *In53     = *Off;     // Repor Indicador
       ENDSR;

       //========================================================
       // Subrotina.: GetRecords
       // Objectivo.: Obter todos os registos de tabelas disponív
       //========================================================
       BEGSR getRecords;
          IF ($GetPage(Func#Key) > *ZEROS);
             DOW (NOT $IsEOF());

                //------------------------------------------------
                // Escreve os Registo da Listagem
                //------------------------------------------------
                REC# += 1;
                ECR_OPTION   = *BLANKS;
                MONITOR;
                   ECR_CDPROD   = %DEC($GetField('CDPROD'):5:0);
                   ECR_CDSEGURA = %DEC($GetField('CDSEGURA'):3:0);
                   ECR_CDRAMO   = %DEC($GetField('CDRAMO'):5:0);
                ON-ERROR;
                ENDMON;
                ECR_DSPROD   = $GetField('DSPROD');
                WRITE RCATE00102;
                $getNext();
             ENDDO;
          ENDIF;

          //---------------------------------------
          // Validar se ainda existem mais registos
          //---------------------------------------
          IF (REC# = *ZEROS);
             *IN52 = *OFF;
          ELSE;
             IF (NOT $IsLastPage());
                *IN54    = *OFF;
             ELSE;
                *IN54    = *ON;
             ENDIF;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: SelRec
       // Objectivo.: Seleccionar os registos
       //========================================================
       BEGSR SelRec;
          IF (REC# = *ZEROS);
             LEAVESR;
          ENDIF;
          WSelected = *OFF;
          READC RCATE00102;
          DOW (NOT %EOF());
             IF ($ValidOption(ECR_OPTION:'5'));
                WModoIO = IO#Consultar;
                WSelected = *ON;
                EXSR DspProduto;
             ELSEIF ($ValidOption(ECR_OPTION:'7'));
                MONITOR;
                   DspCobertura(ECR_CDPROD:ECR_DSPROD);
                ON-ERROR;
                ENDMON;
                $SetDBSession(WSessionID);
             ELSEIF ($ValidOption(ECR_OPTION:'8'));
                MONITOR;
                   DspRisco(ECR_CDPROD:ECR_DSPROD);
                ON-ERROR;
                ENDMON;
                $SetDBSession(WSessionID);
             ELSEIF ($ValidOption(ECR_OPTION:'9'));
                MONITOR;
                   DspClausulas(ECR_CDPROD:ECR_DSPROD);
                ON-ERROR;
                ENDMON;
                $SetDBSession(WSessionID);
             ELSEIF ($ValidOption(ECR_OPTION:'10'));
                MONITOR;
                   DspLimites(ECR_CDPROD:ECR_DSPROD);
                ON-ERROR;
                ENDMON;
                $SetDBSession(WSessionID);
             ELSEIF ($ValidOption(ECR_OPTION:'11'));
                MONITOR;
                   DspDescontos(ECR_CDPROD:ECR_DSPROD);
                ON-ERROR;
                ENDMON;
                $SetDBSession(WSessionID);
             ELSE;
                // Não pode usar a função (Mensagem Erro)
             ENDIF;
             READC RCATE00102;
          ENDDO;
          IF (NOT WSelected);
             IF (%TRIM(ECR_CRITERIA) <> *BLANKS);
                $SetCriteria(ECR_CRITERIA:*ON);
             ELSE;
                $SetCriteria(*BLANKS);
             ENDIF;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: DspProduto
       // Objectivo.: Apresentar os dados do Programa
       //========================================================
       BEGSR DspProduto;
          EXSR LoadParms;
          DOW (NOT *IN03) AND (NOT *IN12);
             IF (WModoIO = IO#Criar);
                WIcDisplay = *OFF;
                *IN45 = *ON;
                WNoFunction = 'RCATE00106';
             ELSEIF (WModoIO = IO#Consultar);
                WIcDisplay = *ON;
                *IN45 = *OFF;
                WNoFunction = 'RCATE00105'; // Listar Componentes
             ELSEIF (WModoIO = IO#Alterar);
                WIcDisplay = *OFF;
                *IN45 = *ON;
                WNoFunction = 'RCATE00121'; // Alterar Componentes
             ENDIF;
             DS#HEADER    = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER    = $getFooter(S#PGM:WNoFunction:*BLANKS);
             ECR_S#PGM    = S#PGM;
             WRITE     RCATE00100;
             WRITE     RCATE00199;
             EXFMT RCATE00105;
             IF ($ValidKey(Func#Key:KEY#F21));
                WModoIO = IO#Alterar;
             ELSEIF ($ValidKey(Func#Key:KEY#F07));
                MONITOR;
                   DspDescontos(ECR_CDPROD:ECR_DSPROD);
                ON-ERROR;
                ENDMON;
                $SetDBSession(WSessionID);
             ELSEIF ($ValidKey(Func#Key:KEY#F08));
                MONITOR;
                   DspCobertura(ECR_CDPROD:ECR_DSPROD);
                ON-ERROR;
                ENDMON;
                $SetDBSession(WSessionID);
             ELSEIF ($ValidKey(Func#Key:KEY#F09));
                MONITOR;
                   DspRisco(ECR_CDPROD:ECR_DSPROD);
                ON-ERROR;
                ENDMON;
                $SetDBSession(WSessionID);
             ELSEIF ($ValidKey(Func#Key:KEY#F10));
                MONITOR;
                   DspClausulas(ECR_CDPROD:ECR_DSPROD);
                ON-ERROR;
                ENDMON;
                $SetDBSession(WSessionID);
             ELSEIF ($ValidKey(Func#Key:KEY#F11));
                MONITOR;
                   DspLimites(ECR_CDPROD:ECR_DSPROD);
                ON-ERROR;
                ENDMON;
                $SetDBSession(WSessionID);
             ELSEIF (Func#Key = KEY#F12);
                IF (WModoIO = IO#Alterar);
                   WModoIO = IO#Consultar;
                   *IN12 = *OFF;
                   EXSR LoadParms;
                ENDIF;
             ELSEIF ($ValidKey(Func#Key:KEY#F22));
                IF ($ShowDialog('CAT0003':*BLANKS:'NS') = 'S');
                   DELETE (ECR_CDPROD) RCATP100;
                   LEAVESR;
                ENDIF;
                *IN12 = *OFF;
             ELSEIF (Func#Key = KEY#F04);
                EXSR RunPopUp;
                $SetDBSession(WSessionID);
             ELSEIF (Func#Key = KEY#Enter);
                IF (WModoIO = IO#Criar) OR (WModoIO = IO#Alterar);
                   EXSR Validar;
                   IF ($GetScStatus = 'O');
                      EXSR Gravar;
                      $SetDBSession(WsessionID);
                      WModoIO    = IO#Consultar;
                      WIcDisplay = *ON;
                      *IN45      = *OFF;
                      *IN12      = *OFF;
                      EXSR LoadParms;
                   ENDIF;
                ELSE;
                   LEAVESR;
                ENDIF;
             ENDIF;
          ENDDO;
          *IN12 = *OFF;
          WModoIO = IO#Consultar;
          WIcDisplay = *ON;
       ENDSR;

       //========================================================
       // Subrotina.: LoadParms
       // Objectivo.: Carregar a informação do Produto
       //========================================================
       BEGSR LoadParms;
          $InzScStatus();
          IF (WModoIO = IO#Criar);
             SETGT (*HIVAL) RCATP100;
             READP(N) RCATP100;
             ECR_CDPROD    = CAT_CDPROD + 1;
             ECR_DSPROD    = *BLANKS;
             ECR_CDRAMO    = *ZEROS;
             ECR_CDVARIA   = *ZEROS;
             ECR_ALIAS     = *BLANKS;
             ECR_CDSEGURA  = *ZEROS;
             ECR_CDFUNCT   = *ZEROS;
             ECR_DSFUNCT   = *BLANKS;
             ECR_CONVERSAO = *BLANKS;
             ECR_ICMIGRAR  = 'N';
             ECR_CAT10001  = 20000101;
             ECR_CAT10002  = 99991231;
          ELSEIF (WModoIO = IO#Consultar);
             CHAIN(N) (ECR_CDPROD) RCATP100;
             ECR_CDPROD    = CAT_CDPROD;
             ECR_DSPROD    = CAT_DSPROD;
             ECR_CDRAMO    = CAT_CDRAMO;
             ECR_CDSEGURA  = CAT_CDSEGURA;
             ECR_DSSEGURA  = $getDescricao(33:
                             %TRIM(%TRIM(%EDITW(CAT_CDSEGURA:'0   '))));
             ECR_CDVARIA   = CAT_CDVARIA;
             ECR_ALIAS     = CAT_ALIAS;
             ECR_CDFUNCT   = CAT_CDFUNCT;
             ECR_DSFUNCT   = $GetDescFunct(CAT_CDFUNCT);
             ECR_CONVERSAO = CAT_CONVERSAO;
             ECR_ICMIGRAR  = CAT_ICMIGRAR;
             ECR_CAT10001  = $DateToInt(CAT_CAT10001);
             ECR_CAT10002  = $DateToInt(CAT_CAT10002);
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: Gravar
       // Objectivo.: Gravar o registo
       //========================================================
       BEGSR Gravar;
          IF (WModoIO = IO#Alterar);
             CHAIN (ECR_CDPROD) RCATP100;
             IF (%FOUND(FCATP100));
                CAT_DSPROD   = ECR_DSPROD;
                CAT_CDRAMO   = ECR_CDRAMO;
                CAT_CDVARIA  = ECR_CDVARIA;
                CAT_ALIAS    = ECR_ALIAS;
                CAT_CDFUNCT  = ECR_CDFUNCT;
                CAT_CDSEGURA = ECR_CDSEGURA;
                CAT_CONVERSAO= ECR_CONVERSAO;
                CAT_ICMIGRAR = ECR_ICMIGRAR;
                UPDATE RCATP100 %FIELDS(CAT_DSPROD:CAT_CDRAMO:CAT_CDVARIA:
                                        CAT_ALIAS:CAT_CDSEGURA:CAT_CONVERSAO:
                                        CAT_CDFUNCT:CAT_ICMIGRAR);
                COMMIT;
             ENDIF;
          ELSEIF (WModoIO = IO#Criar);
             CAT_CDPROD   = ECR_CDPROD;
             CAT_DSPROD   = ECR_DSPROD;
             CAT_CDRAMO   = ECR_CDRAMO;
             CAT_CDVARIA  = ECR_CDVARIA;
             CAT_ALIAS    = ECR_ALIAS;
             CAT_CDSEGURA = ECR_CDSEGURA;
             CAT_CONVERSAO= ECR_CONVERSAO;
             CAT_ICMIGRAR = ECR_ICMIGRAR;
             CAT_CDFUNCT  = ECR_CDFUNCT;
             CAT_CAT10001 = %DATE('2000-01-01');
             CAT_CAT10002 = %DATE('2999-12-31');
             WRITE RCATP100;
             COMMIT;
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
          IF (ECR_DSPROD = *BLANKS);
             $SetScError('DSPROD':'CAT0004');
          ENDIF;
          IF (ECR_CDPROD <= *ZEROS);
             $SetScError('CDPROD':'CAT0005');
          ENDIF;
          IF (ECR_CDVARIA <= *ZEROS);
             $SetScError('CDVARIA':'CAT0006');
          ENDIF;
          IF (ECR_CDSEGURA <= *ZEROS);
             $SetScError('CDSEGURA':'CAT0007');
          ELSEIF (NOT $ValCodigo(33:%TRIM(%EDITW(ECR_CDSEGURA:'0   '))));
             $SetScError('CDSEGURA':'CAT0007');
          ENDIF;
          IF (ECR_ICMIGRAR <> 'N') AND (ECR_ICMIGRAR <> 'S');
             $SetScError('ICMIGRAR':'CAT0010');
          ENDIF;
          IF (ECR_CDFUNCT <= *ZEROS);
             $SetScError('CDFUNCT':'CAT0008');
          ELSE;
             CHAIN (ECR_CDFUNCT) RSECP010;
             IF (%FOUND(FSECP010));
                IF (SEC_CDTMPLT <> 132);
                   $SetScError('CDFUNCT':'CAT0008');
                ENDIF;
             ELSE;
                $SetScError('CDFUNCT':'CAT0008');
             ENDIF;
          ENDIF;
       ENDSR;
       //==================================================================
       // Subrotina.: RunPopUp
       // Objectivo.: Adicionar um novo registo de uma Apólice
       //==================================================================
+----  BEGSR RunPopUp;
|+---     IF (ECR_CM#NMFLD = 'CDSEGURA');
             ECR_CDSEGURA = %DEC($runPopUp(33:
                            %TRIM(%EDITW(ECR_CDSEGURA:'0   '))):3:0);
          ELSEIF (ECR_CM#NMFLD = 'CDFUNCT');
             ECR_CDFUNCT = $RunPopUpTempl(132:ECR_CDFUNCT);
             ECR_DSFUNCT = $GetDescFunct(ECR_CDFUNCT);
          ENDIF;
|         ECR_CM#ROW = CM#CROW;
|         ECR_CM#COL = CM#CCOL;
+----  ENDSR;
      /END-FREE
