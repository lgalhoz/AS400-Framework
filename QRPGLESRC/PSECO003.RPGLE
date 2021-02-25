     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*CALLER)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PSECO001
     H* Descrição.: Manuenção de Componentes do Sistema
     H*==================================================================
     FFSECP020  UF A E           K DISK    PREFIX(SEC_)
     FFSECE003  CF   E             WORKSTN SFILE(RSECE00302:REC#)
     F                                     PREFIX(ECR_)
     F                                     INFDS(INFD#KEYS)
     F*==========================================================
     F* Indicadores  Descrição
     F* -----------  --------------------------------------------
     F*      45/46   Condicionar Campos com Input/Output
     F*      52      DSP
     F*      53      CLR
     F*      54      END
     D*==========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,STBLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SSECO010

     D DspFuncoes      PR                  EXTPGM('PSECO004')
     D   WCdUser                           CONST LIKE(SEC_CDUSER)
     D   WNmUser                           CONST LIKE(SEC_NMUSER)
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
     D WCdUser         S                   LIKE(SEC_CDUSER)
     D WSessionID      S              5S 0 INZ(*ZEROS)
     C*========================================================
      /FREE

       //------------------------------------
       // Se não estiver autorizado então sai
       //------------------------------------
       IF NOT $ValAccess(S#PGM);
          *INLR = *ON;
          RETURN;
       ENDIF;

       WSessionID = $SETFILES('FSECP020');
       $SetCriteria(*BLANKS);
       $SetPage(12);
       $addCriteria('Utilizador.:':'CDUSER':'GE');
       $addCriteria('Nome.......:':'NMUSER':'LK');
       $addCriteria('Departam...:':'CDDEPART':'GE');
       ECR_DSCRITER = $getCritText();
       WIcDisplay =*ON;
       EXSR inzRecords;
       EXSR getRecords;

       DOW (NOT *IN03 AND NOT *IN12);
          WNoFunction = 'RSECE00303'; // Listar Componentes
          DS#HEADER    = $getHeader(S#PGM:WNoFunction);
          DS#FOOTER    = $getFooter(S#PGM:WNoFunction:*BLANKS);
          DS#OPTIONS   = $getOption(S#PGM:WNoFunction:*BLANKS);
          ECR_S#PGM    = S#PGM;
          WRITE     RSECE00300;
          WRITE     RSECE00399;
          WRITE     RSECE00398;
          EXFMT RSECE00303;
          IF (Func#Key = KEY#F05);
             $ChgCriteria();
             ECR_DSCRITER = $getCritText();
             EXSR inzRecords;
             EXSR getRecords;
          ELSEIF ($ValidKey(Func#Key:KEY#F06));
             EXSR addComponente;
             EXSR inzRecords;
             EXSR getRecords;
          ELSEIF ((Func#Key = KEY#PgDown) AND (NOT $IsLastPage())) OR
             ((Func#Key = KEY#PgUp) AND ($GetPageNbr() > 1));
             EXSR inzRecords;
             EXSR getRecords;
          ELSEIF (Func#Key=KEY#Enter);
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
          WRITE RSECE00303;     // Limpar Lista
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
                ECR_CDUSER   = $GetField('CDUSER');
                ECR_NMUSER   = $GetField('NMUSER');
                ECR_CDGRUPO  = $GetField('CDGRUPO');
                ECR_CDDEPART = $GetField('CDDEPART');
                ECR_CDNIVEL  = $GetField('CDNIVEL');
                ECR_CDPROGR  = %DEC($GetField('CDPROGR'):5:0);
                WRITE RSECE00302;
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
          READC RSECE00302;
          DOW (NOT %EOF());
             IF (ECR_OPTION='05') OR (%TRIM(ECR_OPTION)='5');
                WSelected = *ON;
                EXSR DspComponente;
             ELSEIF (ECR_OPTION='07') OR (%TRIM(ECR_OPTION)='7');
                WSelected = *ON;
                MONITOR;
                   DspFuncoes(SEC_CDUSER:SEC_NMUSER);
                ON-ERROR;
                ENDMON;
                $SetDBSession(WSessionID);
             ENDIF;
             READC RSECE00302;
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
       // Subrotina.: DspComponente
       // Objectivo.: Apresentar os dados do Programa
       //========================================================
       BEGSR DspComponente;
          CHAIN(N) (ECR_CDUSER) RSECP020;
          ECR_CDUSER  = SEC_CDUSER;
          ECR_NMUSER  = SEC_NMUSER;
          ECR_CDGRUPO = SEC_CDGRUPO;
          ECR_CDDEPART= SEC_CDDEPART;
          ECR_CDNIVEL = SEC_CDNIVEL;
       // ECR_DSGRUPO = $GetDescricao();
          ECR_DSDEPART= $getDescricao(4:ECR_CDDEPART);
          ECR_DSNIVEL = $getDescricao(6:SEC_CDNIVEL);
       // ECR_DSPROGR = $getDescricao(55:SEC_CDPROGR);
          ECR_CDPROGR = SEC_CDPROGR;

          DOW (NOT *IN03) AND (NOT *IN12);
             IF (WIcDisplay);
                WNoFunction = 'RSECE00305'; // Listar Componentes
                *IN45 = *OFF;
             ELSE;
                WNoFunction = 'RSECE00321'; // Alterar Componentes
                *IN45 = *ON;
             ENDIF;
             DS#HEADER    = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER    = $getFooter(S#PGM:WNoFunction:*BLANKS);
             ECR_S#PGM    = S#PGM;
             WRITE     RSECE00300;
             WRITE     RSECE00399;
             EXFMT RSECE00305;
             IF ($ValidKey(Func#Key:KEY#F21) AND (WIcDisplay));
                WIcDisplay = *OFF;
             ELSEIF ((Func#Key = KEY#F12) AND (NOT WIcDisplay));
                WIcDisplay = *ON;
                *IN12 = *OFF;
             ELSEIF ($ValidKey(Func#Key:KEY#F08) AND (WIcDisplay));
                MONITOR;
                   DspFuncoes(SEC_CDUSER:SEC_NMUSER);
                ON-ERROR;
                ENDMON;
                $SetDBSession(WSessionID);
             ELSEIF ($ValidKey(Func#Key:KEY#F22)) AND (WIcDisplay);
                ECR_OPTION = 'N';
                EXFMT RSECE00322;
                IF (%TRIM(ECR_OPTION) = 'S');
                   CHAIN (SEC_CDUSER) RSECP020;
                   IF (%FOUND());
                      DELETE RSECP020;
                      LEAVESR;
                   ENDIF;
                ENDIF;
             ELSEIF (Func#Key = KEY#F04) AND (NOT WIcDisplay);
                EXSR RunPopUp;
             ELSEIF (Func#Key = KEY#Enter);
                IF (NOT WIcDisplay);
                   CHAIN (ECR_CDUSER) RSECP020;
                   IF (%FOUND());
                      SEC_NMUSER  = ECR_NMUSER;
                      SEC_CDGRUPO = ECR_CDGRUPO;
                      SEC_CDDEPART= ECR_CDDEPART;
                      SEC_CDNIVEL = ECR_CDNIVEL;
                      SEC_CDPROGR = ECR_CDPROGR;
                      UPDATE RSECP020 %FIELDS(SEC_NMUSER:SEC_CDGRUPO:
                                              SEC_CDDEPART:SEC_CDNIVEL:
                                              SEC_CDPROGR);
                   ENDIF;
                ENDIF;
                WIcDisplay = *ON;
                LEAVESR;
             ENDIF;
          ENDDO;
          *IN12 = *OFF;
          WIcDisplay = *ON;
       ENDSR;

       //========================================================
       // Subrotina.: AddComponente;
       // Objectivo.: Adicionar um novo Programa
       //========================================================
       BEGSR AddComponente;
          ECR_CDUSER  = *BLANKS;
          ECR_NMUSER  = *BLANKS;
          ECR_CDGRUPO = *BLANKS;
          ECR_CDDEPART= *BLANKS;
          ECR_CDNIVEL = *BLANKS;
          ECR_CDPROGR = *ZEROS;

          DOW (NOT *IN03) AND (NOT *IN12);
             WNoFunction = 'RSECE00306';
             WIcDisplay = *OFF;
             *IN46 = *ON;
             *IN45 = *ON;
             DS#HEADER    = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER    = $getFooter(S#PGM:WNoFunction:*BLANKS);
             ECR_S#PGM    = S#PGM;
             WRITE     RSECE00300;
             WRITE     RSECE00399;
             EXFMT RSECE00305;
             IF (Func#Key = KEY#F04) AND (NOT WIcDisplay);
                EXSR RunPopUp;
             ELSEIF (Func#Key = KEY#Enter) AND (%TRIM(ECR_CDUSER)<>*BLANKS);
                SEC_CDUSER  = ECR_CDUSER;
                SEC_NMUSER  = ECR_NMUSER;
                SEC_CDGRUPO = ECR_CDGRUPO;
                SEC_CDDEPART= ECR_CDDEPART;
                SEC_CDNIVEL = ECR_CDNIVEL;
                SEC_CDPROGR = ECR_CDPROGR;
                WRITE RSECP020;
                *IN46 = *OFF;
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
|+---     IF (ECR_CM#NMFLD = 'CDGRUPO');
          // ECR_CDGRUPO = $runPopUp(4:ECR_CDGRUPO);
|+---     ELSEIF (ECR_CM#NMFLD = 'CDDEPART');
             ECR_CDDEPART = $runPopUp(4:ECR_CDDEPART);
|+---     ELSEIF (ECR_CM#NMFLD = 'CDNIVEL');
             ECR_CDNIVEL = $runPopUp(6:ECR_CDNIVEL);
|+---     ELSEIF (ECR_CM#NMFLD = 'CDPROGR');
          // ECR_CDPROGR = $runPopUp(56:ECR_CDPROGR);
          ENDIF;
|         ECR_CM#ROW = CM#CROW;
|         ECR_CM#COL = CM#CCOL;
+----  ENDSR;
      /END-FREE
