     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PCATO015
     H* Descrição.: Manutenção de Agravamentos e Descontos
     H*==================================================================
     FFCATP120  UF A E           K DISK    PREFIX(CAT_)
     FFCATE015  CF   E             WORKSTN SFILE(RCATE01502:REC#)
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
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SSECO010

     D DspRegras       PR                  EXTPGM('PCATO016')
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
     D
     D WCdProd         S              5S 0
     D WDsProd         S             50A
     D WSessionID      S              5S 0
     C*========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCdProd
     C                   PARM                    WDsProd
      /FREE

       //------------------------------------
       // Se não estiver autorizado então sai
       //------------------------------------
       IF NOT $ValAccess(S#PGM);
          *INLR = *ON;
          RETURN;
       ENDIF;

       WSessionID = $SetFiles('FCATP120');
       $SetPage(12);
       $addCriteria('Descrição..:':'DSITEM':'LK');
       $addCriteria('Tipo.......:':'TPITEM':'EQ');
       $SetCriteria('(CDPROD = '''+%CHAR(WCdProd)+''')');
       ECR_DSCRITER = $getCritText();
       WIcDisplay =*ON;
       EXSR inzRecords;
       EXSR getRecords;

       ECR_DSPROD  = WDsProd;
       ECR_CDPROD  = WCdProd;
       DOW (NOT *IN03 AND NOT *IN12);
          WNoFunction = 'RCATE01503'; // Listar Componentes
          DS#HEADER    = $getHeader(S#PGM:WNoFunction);
          DS#FOOTER    = $getFooter(S#PGM:WNoFunction:*BLANKS);
          DS#OPTIONS   = $getOption(S#PGM:WNoFunction:*BLANKS);
          ECR_S#PGM    = S#PGM;
          WRITE     RCATE01500;
          WRITE     RCATE01599;
          WRITE     RCATE01598;
          EXFMT RCATE01503;
          IF (Func#Key = KEY#F05);
             $ChgCriteria();
             ECR_DSCRITER = $getCritText();
             EXSR inzRecords;
             EXSR getRecords;
          ELSEIF ($ValidKey(Func#Key:KEY#F06));
             EXSR addComponente;
             $SetDBSession(WsessionID);
             EXSR inzRecords;
             EXSR getRecords;
          ELSEIF ((Func#Key = KEY#PgDown) AND (NOT $IsLastPage())) OR
             ((Func#Key = KEY#PgUp) AND ($GetPageNbr() > 1));
             EXSR inzRecords;
             EXSR getRecords;
          ELSEIF (Func#Key = KEY#Enter);
             EXSR SelRec;
             *IN12 = *OFF;
             EXSR inzRecords;
             EXSR getRecords;
          ENDIF;
       ENDDO;

       $RmvDBSession(WSessionID);
       *INLR = *ON;
       RETURN;

       //========================================================
       // Subrotina.: InzRecTxt
       // Objectivo.: Inicializar a lista de registos
       //========================================================
       BEGSR inzRecords;
          REC#      = *ZEROS;   // Controlo de Registos da Lista
          *In53     = *On;      // Limpar Lista de Ecrã
          *In52     = *On;      // Mostrar a Lista de Ecrã
          WRITE RCATE01503;     // Limpar Lista
          *In53     = *Off;     // Repor Indicador
       ENDSR;

       //========================================================
       // Subrotina.: GetRecords
       // Objectivo.: Obter todos os registos de tabelas disponív
       //========================================================
       BEGSR getRecords;
          IF ($GetPage(Func#Key) > *ZEROS);
             %SUBST(ECR_LSTHDR:1) = 'Desconto/Agravamento';
             %SUBST(ECR_LSTHDR:40) = *BLANKS;
             %SUBST(ECR_LSTHDR:41) = 'Tipo';
             %SUBST(ECR_LSTHDR:54) = *BLANKS;
             %SUBST(ECR_LSTHDR:55) = 'Risco';
             DOW (NOT $IsEOF());

                //------------------------------------------------
                // Escreve os Registo da Listagem
                //------------------------------------------------
                REC# += 1;
                ECR_OPTION     = *BLANKS;
                ECR_CDITEM     = $GetFieldNum('CDITEM');
                %SUBST(ECR_LSTDET:1) = $GetField('DSITEM');
                %SUBST(ECR_LSTDET:40) = *BLANKS;
                %SUBST(ECR_LSTDET:41) = $GetDescricao(71:$GetField('TPITEM'));
                IF ($GetFieldNum('CDRISCO') = *ZEROS);
                   %SUBST(ECR_LSTDET:55) = 'Todos';
                ELSE;
                   %SUBST(ECR_LSTDET:55) = $GetDescricao(20:
                                                      $GetField('CDRISCO'));
                ENDIF;
                WRITE RCATE01502;
                $getNext();
             ENDDO;
          ENDIF;

          //---------------------------------------
          // Validar se ainda existem mais registos
          //---------------------------------------
          IF (REC# = *ZEROS);
             *IN52 = *OFF;
          ELSE;
             *IN54 = $IsLastPage();
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
          READC RCATE01502;
          DOW (NOT %EOF());
             IF ($ValidOption(ECR_OPTION:'5'));
                WSelected = *ON;
                EXSR DspItem;
             ENDIF;
             READC RCATE01502;
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
       // Subrotina.: DspItem
       // Objectivo.: Apresentar os dados do Item
       //========================================================
       BEGSR DspItem;
          CHAIN(N) (WCdProd:ECR_CDITEM) RCATP120;
          ECR_CDPROD   = CAT_CDPROD;
          ECR_CDITEM   = CAT_CDITEM;
          ECR_DSITEM   = CAT_DSITEM;
          ECR_TPITEM   = CAT_TPITEM;
          ECR_ICDEFEITO= CAT_ICDEFEITO;
          ECR_DSTPITEM = $GetDescricao(71:CAT_TPITEM);
          ECR_CDRISCO  = CAT_CDRISCO;
          IF (CAT_CDRISCO = *ZEROS);
             ECR_DSRISCO = 'Todos';
          ELSE;
             ECR_DSRISCO  = $GetDescricao(20:%CHAR(CAT_CDRISCO));
          ENDIF;
          ECR_TXVALOR  = CAT_TXVALOR;
          ECR_CDMANUT  = CAT_CDMANUT;
          ECR_CDREGRA  = CAT_CDREGRA;

          DOW (NOT *IN03) AND (NOT *IN12);
             IF (WIcDisplay);
                WNoFunction = 'RCATE01505'; // Listar Componentes
                *IN45 = *OFF;
             ELSE;
                WNoFunction = 'RCATE01521'; // Alterar Componentes
                *IN45 = *ON;
             ENDIF;
             DS#HEADER    = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER    = $getFooter(S#PGM:WNoFunction:*BLANKS);
             ECR_S#PGM    = S#PGM;
             WRITE     RCATE01500;
             WRITE     RCATE01599;
             EXFMT RCATE01505;
             IF (($ValidKey(Func#Key:KEY#F21)) AND (WIcDisplay));
                WIcDisplay = *OFF;
             ELSEIF ($ValidKey(Func#Key:KEY#F22)) AND (WIcDisplay);
                ECR_OPTION = 'N';
                EXFMT RCATE01522;
                IF (Func#Key = KEY#Enter) AND (%TRIM(ECR_OPTION) = 'S');
                   DELETE (WCdProd:ECR_CDITEM) RCATP120;
                   LEAVESR;
                ENDIF;
                *IN12 = *OFF;
             ELSEIF (Func#Key = KEY#F12) AND (NOT WIcDisplay);
                WIcDisplay = *ON;
                *IN12 = *OFF;
             ELSEIF (Func#Key = KEY#F04) AND (NOT WIcDisplay);
                EXSR RunPopUp;
                $SetDBSession(WSessionID);
             ELSEIF (Func#Key = KEY#Enter);
                IF (NOT WIcDisplay);
                   CHAIN (WCdProd:ECR_CDITEM) RCATP120;
                   IF (%FOUND());
                      CAT_DSITEM   = ECR_DSITEM;
                      CAT_TPITEM   = ECR_TPITEM;
                      CAT_ICDEFEITO= ECR_ICDEFEITO;
                      CAT_CDRISCO  = ECR_CDRISCO;
                      CAT_TXVALOR  = ECR_TXVALOR;
                      CAT_CDMANUT  = ECR_CDMANUT;
                      CAT_CDREGRA  = ECR_CDREGRA;
                      UPDATE RCATP120 %FIELDS(CAT_DSITEM:CAT_TPITEM:
                                              CAT_CDRISCO:CAT_TXVALOR:
                                              CAT_CDMANUT:CAT_CDREGRA:
                                              CAT_ICDEFEITO);
                   ENDIF;
                   $SetDBSession(WsessionID);
                   WIcDisplay = *ON;
                   *IN45 = *OFF;
                   *IN12 = *OFF;
                ELSE;
                   LEAVESR;
                ENDIF;
             ENDIF;
          ENDDO;
          *IN12 = *OFF;
          WIcDisplay = *ON;
       ENDSR;

       //========================================================
       // Subrotina.: AddComponente;
       // Objectivo.: Adicionar um novo Item
       //========================================================
       BEGSR AddComponente;
          ECR_CDPROD   = WCdProd;
          ECR_CDITEM   = $GetNextKey('FCATP120':'CDITEM':'(CDPROD='''+
                           %TRIM(%CHAR(WCdProd)) + ''')');
          ECR_DSITEM   = *BLANKS;
          ECR_TPITEM   = *BLANKS;
          ECR_ICDEFEITO= *BLANKS;
          ECR_DSTPITEM = *BLANKS;
          ECR_CDRISCO  = *ZEROS;
          ECR_DSRISCO  = 'Todos';
          ECR_TXVALOR  = *ZEROS;
          ECR_CDMANUT  = *BLANKS;
          ECR_CDREGRA  = *ZEROS;
          DOW (NOT *IN03) AND (NOT *IN12);
             WNoFunction = 'RCATE01506';
             WIcDisplay = *OFF;
             *IN45 = *ON;
             DS#HEADER    = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER    = $getFooter(S#PGM:WNoFunction:*BLANKS);
             ECR_S#PGM    = S#PGM;
             WRITE     RCATE01500;
             WRITE     RCATE01599;
             EXFMT RCATE01505;
             IF (Func#Key = KEY#F04) AND (NOT WIcDisplay);
                EXSR RunPopUp;
                $SetDBSession(WSessionID);
             ELSEIF (Func#Key = KEY#Enter);
                CAT_CDPROD   = WCdProd;
                CAT_CDITEM   = ECR_CDITEM;
                CAT_DSITEM   = ECR_DSITEM;
                CAT_TPITEM   = ECR_TPITEM;
                CAT_ICDEFEITO= ECR_ICDEFEITO;
                CAT_CDRISCO  = ECR_CDRISCO;
                CAT_TXVALOR  = ECR_TXVALOR;
                CAT_CDMANUT  = ECR_CDMANUT;
                CAT_CDREGRA  = ECR_CDREGRA;
                CAT_CAT12001 = $IntToDate(20001501);
                CAT_CAT12002 = $IntToDate(99991231);
                WRITE RCATP120;
                WIcDisplay = *ON;
                LEAVESR;
             ENDIF;
          ENDDO;
          *IN12 = *OFF;
          WIcDisplay = *ON;
       ENDSR;

       //==================================================================
       // Subrotina.: RunPopUp
       // Objectivo.: Adicionar um novo registo de uma Apólice
       //==================================================================
+----  BEGSR RunPopUp;
|+---     IF (ECR_CM#NMFLD = 'TPITEM');
             ECR_TPITEM = $runPopUp(71:ECR_TPITEM);
             ECR_DSTPITEM = $GetDescricao(71:ECR_TPITEM);
          ELSEIF (ECR_CM#NMFLD = 'CDRISCO');
          ELSEIF (ECR_CM#NMFLD = 'CDMANUT');
          ELSEIF (ECR_CM#NMFLD = 'CDREGRA');
             ECR_CDREGRA = $RunPopUpTempl(124:ECR_CDREGRA);
          ENDIF;
|         ECR_CM#ROW = CM#CROW;
|         ECR_CM#COL = CM#CCOL;
+----  ENDSR;
      /END-FREE
