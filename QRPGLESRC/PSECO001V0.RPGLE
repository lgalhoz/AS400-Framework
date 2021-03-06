     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PSECO001
     H* Descri��o.: Manuen��o de Componentes do Sistema
     H*==================================================================
     FFSECP010  UF A E           K DISK    PREFIX(SEC_)
     FFSECP011  UF A E           K DISK    PREFIX(SEC_)
     FFSECP012  UF A E           K DISK    PREFIX(SEC_)
     FFSECE001  CF   E             WORKSTN SFILE(RSECE00102:REC#)
     F                                     PREFIX(ECR_)
     F                                     INFDS(INFD#KEYS)
     F*==========================================================
     F* Indicadores  Descri��o
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

     D DspFuncoes      PR                  EXTPGM('PSECO002')
     D   WCdProgr                     5S 0 CONST
     D   WNmProgr                    10A   CONST
     D   WDsProgr                    40A   CONST
     D
     D DspUsers        PR                  EXTPGM('PSECO005')
     D   WCdProgr                     5S 0 CONST
     D   WNmProgr                    10A   CONST
     D   WDsProgr                    40A   CONST
     D   WTipAcess                    1A   CONST
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
     D WCdTemplate     S              5S 0
     D WSessionID      S              5S 0
     C*========================================================
      /FREE

       //------------------------------------
       // Se n�o estiver autorizado ent�o sai
       //------------------------------------
       IF NOT $ValAccess(S#PGM);
          *INLR = *ON;
          RETURN;
       ENDIF;

       WSessionID = $SETFILES('FSECP010');
       $SetCriteria(*BLANKS);
       $SetPage(12);
       $addCriteria('Nome.......:':'NMPROGR':'GE');
       $addCriteria('Descri��o..:':'DSPROGR':'LK');
       $addCriteria('Comando....:':'CMITEM':'GE');
       $addCriteria('Dominio....:':'CDOMIN':'EQ');
       ECR_DSCRITER = $getCritText();
       WIcDisplay =*ON;
       EXSR inzRecords;
       EXSR getRecords;

       DOW (NOT *IN03 AND NOT *IN12);
          WNoFunction = 'RSECE00103'; // Listar Componentes
          DS#HEADER    = $getHeader(S#PGM:WNoFunction);
          DS#FOOTER    = $getFooter(S#PGM:WNoFunction:*BLANKS);
          DS#OPTIONS   = $getOption(S#PGM:WNoFunction:*BLANKS);
          ECR_S#PGM    = S#PGM;
          WRITE     RSECE00100;
          WRITE     RSECE00199;
          WRITE     RSECE00198;
          EXFMT RSECE00103;
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
          *In53     = *On;      // Limpar Lista de Ecr�
          *In52     = *On;      // Mostrar a Lista de Ecr�
          WRITE RSECE00103;     // Limpar Lista
          *In53     = *Off;     // Repor Indicador
       ENDSR;

       //========================================================
       // Subrotina.: GetRecords
       // Objectivo.: Obter todos os registos de tabelas dispon�v
       //========================================================
       BEGSR getRecords;
          IF ($GetPage(Func#Key) > *ZEROS);
             DOW (NOT $IsEOF());

                //------------------------------------------------
                // Escreve os Registo da Listagem
                //------------------------------------------------
                REC# += 1;
                ECR_OPTION   = *BLANKS;
                ECR_CDPROGR  = $GetFieldNum('CDPROGR');
                ECR_NMPROGR  = $GetField('NMPROGR');
                ECR_DSPROGR  = $GetField('DSPROGR');
                ECR_CDOMIN   = $GetField('CDOMIN');
                ECR_CMITEM   = $GetField('CMITEM');
                ECR_CDTIPO   = $GetField('CDTIPO');
                WRITE RSECE00102;
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
          READC RSECE00102;
          DOW (NOT %EOF());
             IF (ECR_OPTION='05') OR (%TRIM(ECR_OPTION)='5');
                WSelected = *ON;
                EXSR DspComponente;
             ELSEIF (ECR_OPTION='07') OR (%TRIM(ECR_OPTION)='7');
                IF (ECR_CDTIPO = 'P') OR (ECR_CDTIPO = 'R');
                   WSelected = *ON;
                   MONITOR;
                      DspFuncoes(ECR_CDPROGR:ECR_NMPROGR:ECR_DSPROGR);
                   ON-ERROR;
                   ENDMON;
                   $SetDBSession(WSessionID);
                 ENDIF;
             ELSEIF (ECR_OPTION='08') OR (%TRIM(ECR_OPTION)='8');
                MONITOR;
                   WSelected = *ON;
                   DspUsers(ECR_CDPROGR:ECR_NMPROGR:ECR_DSPROGR:
                          SEC_DFACESS);
                ON-ERROR;
                ENDMON;
                $SetDBSession(WSessionID);
             ENDIF;
             READC RSECE00102;
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
          CHAIN(N) (ECR_CDPROGR) RSECP010;
          ECR_NMPROGR = SEC_NMPROGR;
          ECR_DSPROGR = SEC_DSPROGR;
          ECR_CDOMIN  = SEC_CDOMIN;
          ECR_DDOMIN  = $getDescricao(4:SEC_CDOMIN);
          ECR_CDTIPO  = SEC_CDTIPO;
          ECR_DSTIPO  = $getDescricao(55:SEC_CDTIPO);
          ECR_CDTMPLT = SEC_CDTMPLT;
          ECR_CMITEM  = SEC_CMITEM;
          ECR_NOFRAME = SEC_NOFRAME;
          ECR_DFACESS = SEC_DFACESS;
          ECR_DSACESS = $getDescricao(56:SEC_DFACESS);

          DOW (NOT *IN03) AND (NOT *IN12);
             IF (WIcDisplay);
                WNoFunction = 'RSECE00105'; // Listar Componentes
                *IN45 = *OFF;
             ELSE;
                WNoFunction = 'RSECE00121'; // Alterar Componentes
                *IN45 = *ON;
             ENDIF;
             DS#HEADER    = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER    = $getFooter(S#PGM:WNoFunction:*BLANKS);
             IF (WIcDisplay);
                IF (ECR_CDTIPO='C');
                   ECR_DS#KEY01=$strReplace('F08=Fun��es':'F08=Comando':
                                             ECR_DS#KEY01);
                ELSEIF (ECR_CDTIPO <> 'R') AND (ECR_CDTIPO <> 'P');
                   ECR_DS#KEY01=$strReplace('F08=Fun��es':'':
                                             ECR_DS#KEY01);
                ENDIF;
             ENDIF;
             ECR_S#PGM    = S#PGM;
             WRITE     RSECE00100;
             WRITE     RSECE00199;
             EXFMT RSECE00105;
             IF ($VAlidKey(Func#Key:KEY#F21)) AND (WIcDisplay);
                WIcDisplay = *OFF;
             ELSEIF ($ValidKey(Func#Key:KEY#F08));
                IF (ECR_CDTIPO = 'P') OR (ECR_CDTIPO = 'R');
                   MONITOR;
                      DspFuncoes(SEC_CDPROGR:SEC_NMPROGR:SEC_DSPROGR);
                   ON-ERROR;
                   ENDMON;
                   $SetDBSession(WSessionID);
                ELSEIF (ECR_CDTIPO <> 'C');
                ENDIF;
             ELSEIF ($ValidKey(Func#Key:KEY#F09));
                MONITOR;
                   DspUsers(SEC_CDPROGR:SEC_NMPROGR:SEC_DSPROGR:
                          SEC_DFACESS);
                ON-ERROR;
                ENDMON;
                $SetDBSession(WSessionID);
             ELSEIF ($ValidKey(Func#Key:KEY#F22))
               AND (SEC_NMPROGR <> 'PSECO001') AND
                (SEC_NMPROGR <> 'PSECO002');
                ECR_OPTION = 'N';
                EXFMT RSECE00122;
                IF (Func#Key = KEY#Enter) AND (ECR_OPTION = 'S');
                   CHAIN (SEC_CDPROGR) RSECP010;
                   IF (%FOUND());
                      DELETE RSECP010;
                      LEAVESR;
                   ENDIF;
                ENDIF;
                *IN12 = *OFF;
             ELSEIF (Func#Key = KEY#F12) AND (NOT WIcDisplay);
                WIcDisplay = *ON;
                *IN12 = *OFF;
             ELSEIF (Func#Key = KEY#F04) AND (NOT WIcDisplay);
                EXSR RunPopUp;
             ELSEIF (Func#Key = KEY#Enter);
                IF (NOT WIcDisplay);
                   CHAIN (ECR_CDPROGR) RSECP010;
                   IF (%FOUND());
                      SEC_NMPROGR = ECR_NMPROGR;
                      SEC_DSPROGR = ECR_DSPROGR;
                      SEC_CDOMIN  = ECR_CDOMIN;
                      SEC_CDTIPO  = ECR_CDTIPO;
                      SEC_CDTMPLT = ECR_CDTMPLT;
                      SEC_CMITEM  = ECR_CMITEM;
                      SEC_NOFRAME = ECR_NOFRAME;
                      SEC_DFACESS = ECR_DFACESS;
                      UPDATE RSECP010 %FIELDS(SEC_NMPROGR:SEC_DSPROGR:
                                              SEC_CDOMIN:SEC_CDTIPO:
                                              SEC_CDTMPLT:SEC_CMITEM:
                                              SEC_DFACESS:SEC_NOFRAME);
                      CHAIN (ECR_CDPROGR:99999:99999) RSECP012;
                      SEC_DFACCESS= ECR_DFACESS;
                      IF (%FOUND());
                         UPDATE RSECP012 %FIELDS(SEC_DFACCESS);
                      ELSE;
                         CHAIN (ECR_CDPROGR:99999) RSECP011;
                         IF (NOT %FOUND());
                            SEC_CDPROGR = ECR_CDPROGR;
                            SEC_CDFUNCT = 99999;
                            SEC_NMFUNCT = ECR_NMPROGR;
                            SEC_DSFUNCT = ECR_DSPROGR;
                            WRITE RSECP011;
                         ENDIF;
                         SEC_CDPROGR = ECR_CDPROGR;
                         SEC_CDFUNCT = 99999;
                         SEC_CDOPERA = 99999;
                         SEC_VLINDICA = 99;
                         SEC_DSOPERA  = 'F99=Generico';
                         WRITE RSECP012;
                      ENDIF;
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
       // Objectivo.: Adicionar um novo Programa
       //========================================================
       BEGSR AddComponente;
          SETGT (*HIVAL) RSECP010;
          READP(N) RSECP010;
          ECR_CDPROGR = SEC_CDPROGR + 1;
          ECR_NMPROGR = *BLANKS;
          ECR_DSPROGR = *BLANKS;
          ECR_CDOMIN  = *BLANKS;
          ECR_DDOMIN  = *BLANKS;
          ECR_CDTIPO  = *BLANKS;
          ECR_DSTIPO  = *BLANKS;
          ECR_CDTMPLT = *ZEROS;
          ECR_DSTMPLT = *BLANKS;
          ECR_CMITEM  = *BLANKS;
          ECR_NOFRAME = *BLANKS;
          ECR_DFACESS = 'A';
          ECR_DSACESS = *BLANKS;
          ECR_CDLIBL  = *ZEROS;
          ECR_DSLIBL  = *BLANKS;

          DOW (NOT *IN03) AND (NOT *IN12);
             WNoFunction = 'RSECE00106';
             WIcDisplay = *OFF;
             *IN45 = *ON;
             DS#HEADER    = $getHeader(S#PGM:WNoFunction);
             DS#FOOTER    = $getFooter(S#PGM:WNoFunction:*BLANKS);
             ECR_S#PGM    = S#PGM;
             WRITE     RSECE00100;
             WRITE     RSECE00199;
             EXFMT RSECE00105;
             IF (Func#Key = KEY#F04) AND (NOT WIcDisplay);
                EXSR RunPopUp;
             ELSEIF (Func#Key = KEY#Enter);
                SEC_CDPROGR = ECR_CDPROGR;
                SEC_NMPROGR = ECR_NMPROGR;
                SEC_DSPROGR = ECR_DSPROGR;
                SEC_CDOMIN  = ECR_CDOMIN;
                SEC_CDTIPO  = ECR_CDTIPO;
                SEC_CDTMPLT = ECR_CDTMPLT;
                SEC_CMITEM  = ECR_CMITEM;
                SEC_NOFRAME = ECR_NOFRAME;
                SEC_DFACESS = ECR_DFACESS;
                WRITE RSECP010;
                SEC_CDFUNCT = 99999;
                SEC_NMFUNCT = ECR_NMPROGR;
                SEC_DSFUNCT = ECR_DSPROGR;
                WRITE RSECP011;
                SEC_CDOPERA = 99999;
                SEC_VLINDICA = 99;
                SEC_DSOPERA  = 'F99=Generico';
                SEC_DFACCESS = SEC_DFACESS;
                WRITE RSECP012;
                WIcDisplay = *ON;
                LEAVESR;
             ENDIF;
          ENDDO;
          *IN12 = *OFF;
          WIcDisplay = *ON;
       ENDSR;

       //==================================================================
       // Subrotina.: RunPopUp
       // Objectivo.: Adicionar um novo registo de uma Ap�lice
       //==================================================================
+----  BEGSR RunPopUp;
|+---     IF (ECR_CM#NMFLD = 'CDOMIN');
             ECR_CDOMIN = $runPopUp(4:ECR_CDOMIN);
|+---     ELSEIF (ECR_CM#NMFLD = 'CDTIPO');
             ECR_CDTIPO = $runPopUp(55:ECR_CDTIPO);
|+---     ELSEIF (ECR_CM#NMFLD = 'DFACESS');
             ECR_DFACESS = $runPopUp(56:ECR_DFACESS);
|+---     ELSEIF (ECR_CM#NMFLD = 'CDTMPLT');
             ECR_CDTMPLT = $RunPopUpTempl(126:ECR_CDTMPLT);
          ENDIF;
|         ECR_CM#ROW = CM#CROW;
|         ECR_CM#COL = CM#CCOL;
+----  ENDSR;
      /END-FREE
