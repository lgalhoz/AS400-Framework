     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PSECO001
     H* Descrição.: Manuenção de Componentes do Sistema
     H*==================================================================
     FFSECP030  UF A E           K DISK    PREFIX(SEC_)
     FFSECE005  CF   E             WORKSTN SFILE(RSECE00502:REC#)
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

     D*==> Estrutura de dados do Registo
     D WIcErro         S               N   INZ(*OFF)
     D WIcOrigem       S              1A
     D REC#            S              4S 0
     D
     D WIcDisplay      S               N   INZ(*ON)
     D WModo           S               N
     D WNoFunction     S             10A   INZ(*BLANKS)
     D WSelected       S               N
     D WCdProgr        S              5S 0
     D WNmProgr        S             10A
     D WDsProgr        S             40A
     D WTipAcess       S              1A
     D PCdFunct        S              5S 0
     D WCdFunct        S              5S 0
     D WNmFunct        S             10A
     D WDsFunct        S             40A
     D WCdOpera        S              5S 0
     D PCdOpera        S              5S 0
     D WDsOpera        S             20A
     D WSessionID      S              5S 0
     C*========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WCdProgr
     C                   PARM                    WNmProgr
     C                   PARM                    WDsProgr
     C                   PARM                    WTipAcess
     C                   PARM                    PCdFunct
     C                   PARM                    WNmFunct
     C                   PARM                    WDsFunct
     C                   PARM                    PCdOpera
     C                   PARM                    WDsOpera
      /FREE

       //------------------------------------
       // Se não estiver autorizado então sai
       //------------------------------------
       IF NOT $ValAccess(S#PGM);
          *INLR = *ON;
          RETURN;
       ENDIF;

       ECR_CDPROGR = WCdProgr;
       ECR_NMPROGR = WNmProgr;
       ECR_DSPROGR = WDsProgr;
       MONITOR;
          ECR_CDFUNCT = PCdFunct;
          WCdFunct    = PCdFunct;
          ECR_NMFUNCT = WNmFunct;
          ECR_DSFUNCT = WDsFunct;
          ECR_CDOPERA = PCdOpera;
          WCdOpera    = PCdOpera;
          ECR_DSOPERA = WDsOpera;
       ON-ERROR;
          ECR_CDFUNCT = 99999;
          WCdFunct    = 99999;
          ECR_NMFUNCT = WNmProgr;
          ECR_DSFUNCT = WDsProgr;
          ECR_CDOPERA = 99999;
          WCdOpera    = 99999;
          ECR_DSOPERA = 'F99=Genérico';
       ENDMON;

       WSessionID = $SETFILES('FSECP020');
       $SetCriteria(*BLANKS);
       $SetPage(12);
       $addCriteria('Nome':'CDUSER':'GE');
       $addCriteria('Descrição':'NMUSER':'LK');
       WIcDisplay =*ON;
       EXSR inzRecords;
       EXSR getRecords;

       DOW (NOT *IN03 AND NOT *IN12);
          IF (WIcDisplay);
             WNoFunction = 'RSECE00503'; // Listar Componentes
          ELSE;
             WNoFunction = 'RSECE00504'; // Alterar Componentes
          ENDIF;
          DS#HEADER    = $getHeader(S#PGM:WNoFunction);
          DS#FOOTER    = $getFooter(S#PGM:WNoFunction:*BLANKS);
          ECR_S#PGM    = S#PGM;
          WRITE     RSECE00500;
          WRITE     RSECE00599;
          WRITE     RSECE00598;
          EXFMT RSECE00503;
          IF ($ValidKey(Func#Key:KEY#F21)) AND (WIcDisplay);
             WIcDisplay = *OFF;
             *IN45 = *ON;
             EXSR inzRecords;
             EXSR getRecords;
          ELSEIF (Func#Key = KEY#F05) AND (WIcDisplay);
             $ChgCriteria();
             EXSR inzRecords;
             EXSR getRecords;
          ELSEIF (Func#Key = KEY#F12) AND (NOT WIcDisplay);
             *IN12 = *OFF;
             WIcDisplay = *ON;
             *IN45 = *OFF;
             EXSR inzRecords;
             EXSR getRecords;
          ELSEIF ((Func#Key = KEY#PgDown) AND (NOT $IsLastPage())) OR
             ((Func#Key = KEY#PgUp) AND ($GetPageNbr() > 1));
             EXSR inzRecords;
             EXSR getRecords;
          ELSEIF (Func#Key = KEY#Enter);
             EXSR SelRec;
             IF (NOT WIcDisplay);
                $SetDBSession(WsessionID);
                WIcDisplay = *ON;
                *IN45 = *OFF;
                *IN12 = *OFF;
                EXSR inzRecords;
                EXSR getRecords;
             ELSE;
                *IN12 = *ON;
             ENDIF;
          ENDIF;
       ENDDO;

       $SetDBSession(WSessionID);
       $RmvDBSession();
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
          WRITE RSECE00503;     // Limpar Lista
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
                ECR_CDUSER   = $GetField('CDUSER');
                ECR_NMUSER   = $GetField('NMUSER');
                ECR_TIPACESS= WTipAcess;
                CHAIN(N) (WCdProgr:WCdFunct:WCdOpera:ECR_CDUSER)
                         RSECP030;
                IF (%FOUND());
                   ECR_TIPACESS = SEC_TPACCES;
                ENDIF;
                WRITE RSECE00502;
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
          WSelected = *OFF;
          READC RSECE00502;
          DOW (NOT %EOF());
             EXSR ChgPermissao;
             READC RSECE00502;
          ENDDO;
       ENDSR;

       //========================================================
       // Subrotina.: ChgPermissao
       // Objectivo.: Alterar as Permissões
       //========================================================
       BEGSR ChgPermissao;
          CHAIN (WCdProgr:WCdFunct:WCdOpera:ECR_CDUSER) RSECP030;
          IF (%FOUND());
             IF (ECR_TIPACESS = *BLANKS);
                DELETE (WCdProgr:WCdFunct:WCdOpera:ECR_CDUSER)
                RSECP030;
             ELSE;
                SEC_TPACCES = ECR_TIPACESS;
                UPDATE RSECP030 %FIELDS(SEC_TPACCES);
             ENDIF;
          ELSE;
             SEC_CDPROGR = WCdProgr;
             SEC_CDFUNCT = WCdFunct;
             SEC_CDOPERA = WCdOpera;
             SEC_CDUSER  = ECR_CDUSER;
             SEC_TPACCES = ECR_TIPACESS;
             WRITE RSECP030;
          ENDIF;
       ENDSR;
      /END-FREE
