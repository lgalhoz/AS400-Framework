     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PCOBO143
     H* Descrição.: Actualizar Base Dados de Descontos em Vencimento,
     H*
     H*==================================================================
     FGCOBGR    IF   E           K DISK    PREFIX('LA_')
     FSD1RE1    IF   E           K DISK    PREFIX('REC_')
     FFCOBP141  UF A E           K DISK    PREFIX('COB_')
     FFCOBP142  UF A E           K DISK    PREFIX('COB_')
     FFCOBP143  UF A E           K DISK    PREFIX('COB_')
     FFCOBP14201IF   E           K DISK    PREFIX('COB_')
     F                                     RENAME(RCOBP142:RCOBP14201)
     FFCOBP144  UF A E           K DISK    PREFIX('COB_')
     FFCOBP145  UF A E           K DISK    PREFIX('COB_')
     D*==================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010

     D*==> Controlo de Apólices
     D LST#APOL        DS                  QUALIFIED DIM(20)
     D   NumOrdem                     8S 0
     D   CodSeg                       8S 0
     D   CodRamo                      3S 0
     D   Numapol                      7S 0
     D   Companh                      3S 0
     D   IsFirst                       N
     D   IsEmpty                       N
     D WIdx            S              3S 0
     D
     D*==> Campos de Trabalho
     D WNumRecibo      S              9A
     D WCodErro        S              7A
     D WCodMov         S                   LIKE(REC_CODMOV)
     D WCodSit         S                   LIKE(REC_CODSIT)
     D WVtRec          S                   LIKE(REC_VTREC)
     D WMacont         S                   LIKE(COB_MACONT)
     D WTotMovim       S                   LIKE(COB_IMREAL)
     D WSaldo          S                   LIKE(COB_IMSALDO)
     D WBkSaldo        S                   LIKE(COB_IMSALDO)
     D WNumReciboOrig  S                   LIKE(COB_NUMRECIBO)
     D WNumOrdem       S                   LIKE(COB_NUMORDEM)
     D WLastOrdem      S                   LIKE(COB_NUMORDEM)
     D WNumSegurado    S                   LIKE(COB_CODSEG)
     D WLastSegurado   S                   LIKE(COB_CODSEG)
     D WNumPlano       S                   LIKE(COB_NUMPLANO)
     D WTipRecibo      S                   LIKE(COB_TIPREC)
     D WImEstimado     S                   LIKE(COB_IMESTIMAD)
     D WQtdePrst       S              3S 0
     D WQtdeApol       S              3S 0
     D*==================================================================
      /FREE

       EXSR AuditPlanos;
       EXSR UpdSegurados;
       *INLR = *ON;
       RETURN;

       //========================================================
       // Subrotina.: UpdSegurados
       // Descrição.: Actualizar o saldo dos Empregados
       //========================================================
       BEGSR UpdSegurados;
          WQtdeApol     = *ZEROS;
          WSaldo        = *ZEROS;
          WLastSegurado = *ZEROS;
          SETLL *LOVAL RCOBP14201;
          READ RCOBP14201;
          DOW (NOT %EOF(FCOBP14201));
             WNumSegurado = COB_CODSEG;
             WBkSaldo     = COB_IMSALDO;
             IF (WLastSegurado = *ZEROS);
                WLastSegurado = WNumSegurado;
             ENDIF;
             IF (WLastSegurado <> WNumSegurado);
                CHAIN (WLastSegurado) RCOBP141;
                IF (%FOUND(FCOBP141));
                   COB_IMSALDO  = WSaldo;
                   COB_QTDEAPOL = WQtdeApol;
                   UPDATE RCOBP141 %FIELDS(COB_IMSALDO:COB_QTDEAPOL);
                ENDIF;
                WQtdeApol     = *ZEROS;
                WLastSegurado = WNumSegurado;
                WSaldo = *ZEROS;
             ENDIF;
             WSaldo += WBkSaldo;
             WQtdeApol += 1;
             READ RCOBP14201;
          ENDDO;
          IF (WLastSegurado <> WNumSegurado);
             CHAIN (WLastSegurado) RCOBP141;
             IF (%FOUND(FCOBP141));
                COB_IMSALDO = WSaldo;
                COB_QTDEAPOL = WQtdeApol;
                UPDATE RCOBP141 %FIELDS(COB_IMSALDO:COB_QTDEAPOL);
             ENDIF;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: AuditPlanos
       // Descrição.: Auditar planos e movimentos
       //========================================================
       BEGSR AuditPlanos;
          //----------------------------------------------------
          // Seleccionar Segurados com descontos em vencimento
          //----------------------------------------------------
          WLastOrdem = *ZEROS;
          WQtdePrst = *ZEROS;
          WSaldo = *ZEROS;
          SETLL *LOVAL RCOBP144;
          READ RCOBP144;
          DOW (NOT %EOF(FCOBP144));
             WNumOrdem      = COB_NUMORDEM;
             WNumPlano      = COB_NUMPLANO;
             WNumReciboOrig = COB_NUMRECIBO;
             WNumSegurado   = COB_CODSEG;
             WTipRecibo     = COB_TIPREC;
             WMaCont        = COB_MACONT;
             WImEstimado    = COB_IMESTIMAD;
             IF (WLastOrdem = *ZEROS);
                WLastOrdem = WNumOrdem;
             ENDIF;

             //----------------------------------
             // Actualizar a Apólice
             //----------------------------------
             IF (WNumOrdem <> WLastOrdem);
                CHAIN (WLastOrdem) RCOBP142;
                IF (%FOUND(FCOBP142));
                   COB_IMSALDO = WSaldo;
                   IF (WQtdePrst > 1);
                      COB_PRESTACOES = WQtdePrst;
                   ENDIF;
                   UPDATE RCOBP142 %FIELDS(COB_PRESTACOES:COB_IMSALDO);
                ENDIF;
                WSaldo = *ZEROS;
                WLastOrdem = WNumOrdem;
                WQtdePrst = *ZEROS;
             ENDIF;

             //----------------------------------------------------------
             // Actualizar informação do Plano em função do Corretor
             //----------------------------------------------------------
             IF (WNumReciboOrig <> 999999999);
                //----------------------------------------------
                // Obter situação do recibo do Corretor
                //----------------------------------------------
                WNumRecibo = %TRIM(%CHAR(WNumReciboOrig));
                CHAIN (WNumRecibo) SD1RE1;
                IF (NOT %EOF(SD1RE1));
                   WTotMovim = *ZEROS;
                   WCodMov   = REC_CODMOV;
                   WCodSit   = REC_CODSIT;
                   WVtRec    = REC_VTREC;
                   WMacont   = *ZEROS;
                   CHAIN (WNumReciboOrig) GCOBGR;
                   IF (%FOUND(GCOBGR));
                      WCodMov = LA_GCTIPR;
                      IF (WCodMov = 2); // Continuado
                         WMacont = 200000 + %DEC(%SUBST(WNumRecibo:1:4):4:0);
                      ENDIF;
                   ENDIF;

                   //----------------------------------------------
                   // Calcula o total dos movimentos lançados
                   //----------------------------------------------
                   EXSR ReadMovimentos;

                   //----------------------------------------------
                   // Actualizar os valores do Plano
                   // Se o recibo for anulado então inverte o
                   // Sinal do movimento para devolver dinheiro
                   //----------------------------------------------
                   COB_IMREAL    = WTotMovim;
                   COB_IMESTIMAD = WVtRec;
                   IF (WCodSit = 0) OR (WCodSit = 2);
                      COB_IMSALDO   = COB_IMREAL - COB_IMESTIMAD;
                   ELSE;
                      COB_IMSALDO   = COB_IMREAL;
                   ENDIF;
                   WSaldo       += COB_IMSALDO;

                   //----------------------------------------------
                   // ForCompr restantes campos e actualizar
                   //----------------------------------------------
                   COB_TIPRECIBO = WCodMov;
                   COB_ESTRECIBO = WCodSit;
                   COB_MACONT    = WMaCont;
                   COB_NUMPREST  = WQtdePrst;
                   UPDATE RCOBP144 %FIELDS(COB_TIPRECIBO:COB_ESTRECIBO:
                                           COB_IMESTIMAD:COB_IMSALDO:
                                           COB_IMREAL:COB_MACONT:
                                           COB_NUMPREST);
                ENDIF;
             ENDIF;

             //----------------------------------------------------------
             // Actualizar número do Plano no Recibo
             //----------------------------------------------------------
             CHAIN (WNumReciboOrig:WNumOrdem) RCOBP143;
             IF (%FOUND(FCOBP143));
                COB_NUMPLANO = WNumPlano;
                UPDATE RCOBP143 %FIELDS(COB_NUMPLANO);
             ELSE;
                COB_NUMRECIBO  = WNumReciboOrig;
                COB_NUMORDEM   = WNumOrdem;
                COB_CODSEG     = WNumSegurado;
                COB_TIPREC     = WTipRecibo;
                COB_MACONT     = WMaCont;
                COB_ESTADO     = 0;
                COB_DATEMISSAO = %DATE('9999-12-31');
                COB_DATESTADO  = %DATE('9999-12-31');
                COB_IMREAL     = WImEstimado;
                COB_NUMPLANO   = WNumPlano;
                COB_ISTOSELECT = 'S';
                COB_COB14301  = %DATE('2000-01-01');
                COB_COB14302  = %DATE('9999-12-31');
                WRITE RCOBP143;
             ENDIF;
             READ RCOBP144;
          ENDDO;

          //---------------------------------------------------
          // Actualizar o último registo
          //---------------------------------------------------
          IF (WNumOrdem <> WLastOrdem);
             CHAIN (WLastOrdem) RCOBP142;
             IF (%FOUND(FCOBP142));
                COB_IMSALDO = WSaldo;
                IF (WQtdePrst > 1);
                   COB_PRESTACOES = WQtdePrst;
                ENDIF;
                UPDATE RCOBP142 %FIELDS(COB_PRESTACOES:COB_IMSALDO);
             ENDIF;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: ReadMovimentos
       // Descrição.: COntabilizar os movimentos Lançados
       //========================================================
       BEGSR ReadMovimentos;
          SETLL (WNumOrdem:WNumPlano) RCOBP145;
          READE (WNumOrdem:WNumPlano) RCOBP145;
          DOW (NOT %EOF(FCOBP145));
             IF (COB_NUMPREST > WQtdePrst) AND (COB_NUMPREST < 20);
                WQtdePrst = COB_NUMPREST;
             ENDIF;
             WTotMovim += COB_IMMOVIM;
             READE (WNumOrdem:WNumPlano) RCOBP145;
          ENDDO;
       ENDSR;
      /END-FREE
