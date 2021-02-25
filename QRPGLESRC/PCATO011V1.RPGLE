     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PCATO011
     H* Descrição.: Parameterização Técnica
     H*==================================================================
     FFCATP100  UF A E           K DISK    PREFIX(CAT_)
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
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SSECO010

     D DspBlocos       PR                  EXTPGM('PCATO012')
     D   WCdProd                      5S 0 CONST
     D   WDsProd                     50A   CONST
     D DspEstilos      PR                  EXTPGM('PCATO013')
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
     D WSessionID      S              5S 0
     C*========================================================
      /FREE

       //------------------------------------
       // Se não estiver autorizado então sai
       //------------------------------------
       IF NOT $ValAccess(S#PGM);
          *INLR = *ON;
          RETURN;
       ENDIF;

       WSessionID = $SETFILES('FCATP100');
       $SetPage(12);
       $addCriteria('Ramo.......:':'CDRAMO':'GE');
       $addCriteria('Descrição..:':'DSPROD':'LK');
       ECR_DSCRITER = $getCritText();
       WIcDisplay =*ON;
       EXSR inzRecords;
       EXSR getRecords;

       DOW (NOT *IN03 AND NOT *IN12);
          WNoFunction = 'RCATE01103'; // Listar Componentes
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
                   ECR_CDSEGURA = %DEC($GetField('CDSEGURA'):5:0);
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
                WSelected = *ON;
                EXSR DspProduto;
             ELSEIF ($ValidOption(ECR_OPTION:'7'));
                MONITOR;
                   DspEstilos(ECR_CDPROD:ECR_DSPROD);
                ON-ERROR;
                ENDMON;
                $SetDBSession(WSessionID);
             ELSEIF ($ValidOption(ECR_OPTION:'8'));
                MONITOR;
                   DspBlocos(ECR_CDPROD:ECR_DSPROD);
                ON-ERROR;
                ENDMON;
                $SetDBSession(WSessionID);
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
          CHAIN(N) (ECR_CDPROD) RCATP100;
          ECR_CDPROD    = CAT_CDPROD;
          ECR_DSPROD    = CAT_DSPROD;
          ECR_CDRAMO    = CAT_CDRAMO;
          ECR_DSSEGURA  = $getDescricao(33:
                          %TRIM(%EDITW(ECR_CDSEGURA:'0   ')));
          ECR_CDVARIA   = CAT_CDVARIA;
          ECR_ALIAS     = CAT_ALIAS;
          ECR_CDSEGURA  = CAT_CDSEGURA;
          ECR_CONVERSAO = CAT_CONVERSAO;
          ECR_CAT10001  = $DateToInt(CAT_CAT10001);
          ECR_CAT10002  = $DateToInt(CAT_CAT10002);

          DOW (NOT *IN03) AND (NOT *IN12);
             IF (WIcDisplay);
                WNoFunction = 'RCATE01105'; // Listar Componentes
                *IN45 = *OFF;
             ELSE;
                WNoFunction = 'RCATE01121'; // Alterar Componentes
                *IN45 = *ON;
             ENDIF;
             DS#HEADER    = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER    = $getFooter(S#PGM:WNoFunction:*BLANKS);
             ECR_S#PGM    = S#PGM;
             WRITE     RCATE00100;
             WRITE     RCATE00199;
             EXFMT RCATE00105;
             IF ($ValidKey(Func#Key:KEY#F07)) AND (WIcDisplay);
                MONITOR;
                   DspEstilos(ECR_CDPROD:ECR_DSPROD);
                ON-ERROR;
                ENDMON;
                $SetDBSession(WSessionID);
             ELSEIF ($ValidKey(Func#Key:KEY#F08)) AND (WIcDisplay);
                MONITOR;
                   DspBlocos(ECR_CDPROD:ECR_DSPROD);
                ON-ERROR;
                ENDMON;
                $SetDBSession(WSessionID);
             ELSEIF (Func#Key = KEY#F12) AND (NOT WIcDisplay);
                WIcDisplay = *ON;
                *IN12 = *OFF;
             ELSEIF (Func#Key = KEY#Enter);
                IF (NOT WIcDisplay);
                   CHAIN (ECR_CDPROD) RCATP100;
                   IF (%FOUND());
                      CAT_DSPROD   =ECR_DSPROD;
                      CAT_CDRAMO   =ECR_CDRAMO;
                      CAT_CDVARIA  =ECR_CDVARIA;
                      CAT_ALIAS    =ECR_ALIAS;
                      CAT_CDSEGURA =ECR_CDSEGURA;
                      CAT_CONVERSAO=ECR_CONVERSAO;
                      CAT_CAT10001 =$IntToDate(20000101);
                      CAT_CAT10002 =$IntToDate(99991231);
                      UPDATE RCATP100 %FIELDS(CAT_DSPROD:CAT_CDRAMO:
                                              CAT_CDVARIA:CAT_ALIAS:
                                              CAT_CDSEGURA:CAT_CONVERSAO:
                                              CAT_CAT10001:CAT_CAT10002);
                   ENDIF;
                ENDIF;
                $SetDBSession(WsessionID);
                WIcDisplay = *ON;
                LEAVESR;
             ENDIF;
          ENDDO;
          *IN12 = *OFF;
          WIcDisplay = *ON;
       ENDSR;
      /END-FREE
