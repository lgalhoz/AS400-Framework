     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*==================================================================
     H* Programa..: PCOBO141
     H* Descrição.: Actualizar Base Dados de Descontos em Vencimento,
     H*
     H*==================================================================
     FGCOBGRX1  IF   E           K DISK    PREFIX('LA_')
     FSD1RE1    IF   E           K DISK    PREFIX('REC_')
     FFCOBP142  UF A E           K DISK    PREFIX('COB_')
     FFCOBP143  UF A E           K DISK    PREFIX('COB_')
     FFCOBP144  UF A E           K DISK    PREFIX('COB_')
     FFCOBP14401IF   E           K DISK    PREFIX('COB_')
     F                                     RENAME(RCOBP144:RCOB14401)
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
     D WSession        S              5S 0
     D WRecord         S              5S 0
     D WCodSegurado    S              8S 0
     D WNumOrdem       S              8S 0
     D WMaCont         S              6S 0
     D WNumRecibo      S              9A
     D
     D WCodErro        S              7A
     D WDatEstado      S               D
     D WIsToSelect     S              1A
     D WIsFirstTime    S               N
     D WNumPlano       S              8S 0
     D WSegIsToSelect  S              1A
     D*==================================================================
      /FREE

       //----------------------------------------------------
       // Seleccionar Segurados com descontos em vencimento
       //----------------------------------------------------
       WCodSegurado = *ZEROS;
       WSession = $SetFiles('FCOBP141 A INNER JOIN FCOBP142 B +
                             ON A.CODSEG = B.CODSEG');
       $SetFields('B.NUMORDEM,B.CODCOMP,B.CODRAMO,B.NUAPOL,A.CODSEG,+
                   B.CODCOMP');
       $SetCriteria('B.ISTOSELECT = ''S''');
       WRecord = 1;
       DOW ($GetRecord(WRecord) > *ZEROS);
          IF (WCodSegurado = *ZEROS);
             WCodSegurado = $GetFieldNum('A.CODSEG');
             WIdx = 1;
             LST#APOL(WIdx).IsEmpty = *ON;
          ENDIF;
          IF (WCodSegurado <> $GetFieldNum('A.CODSEG'));
             EXSR ReadRecibos;

             WCodSegurado = $GetFieldNum('A.CODSEG');
             WIdx = 1;
             LST#APOL(WIdx).IsEmpty = *ON;
          ENDIF;

          LST#APOL(Widx).NumOrdem = $GetFieldNum('B.NUMORDEM');
          LST#APOL(Widx).CodSeg   = $GetFieldNum('A.CODSEG');
          LST#APOL(Widx).CodRamo  = $GetFieldNum('B.CODRAMO');
          LST#APOL(Widx).Numapol  = $GetFieldNum('B.NUAPOL');
          LST#APOL(Widx).Companh  = $GetFieldNum('B.CODCOMP');
          LST#APOL(Widx).IsFirst  = *ON;
          LST#APOL(Widx).IsEmpty  = *OFF;
          WIdx += 1;
          LST#APOL(WIdx).IsEmpty = *ON;

          WRecord += 1;
       ENDDO;
       $RmvDBSession(WSession);

       IF (WCodSegurado <> *ZEROS);
          EXSR ReadRecibos;
       ENDIF;

       *INLR = *ON;
       RETURN;

       //========================================================
       // Subrotina.: ReadRecibos
       // Descrição.: Obter todos os recibos do Seguurado
       //========================================================
       BEGSR ReadRecibos;
          SETLL (WCodSegurado) GCOBGRX1;
          READE (WCodSegurado) GCOBGRX1;
          DOW (NOT %EOF(GCOBGRX1));
             EXSR ValRecibo;
             IF (WCodErro = *BLANKS);
                EXSR UpdRecibo;
             ENDIF;
             READE (WCodSegurado) GCOBGRX1;
          ENDDO;
       ENDSR;

       //========================================================
       // Subrotina.: ValRecibo
       // Descrição.: Validar se de ve registar o Recibo
       //========================================================
       BEGSR ValRecibo;
          WCodErro = *BLANKS;

          //---------------------------------------------------
          // Confirmar se é de uma apólice com desc.Vencimento
          //---------------------------------------------------
          WNumOrdem = *ZEROS;
          WIdx = 1;
          DOW (NOT LST#APOL(Widx).IsEmpty);
             IF (LA_GCRAMO = LST#APOL(WIdx).CodRamo) AND
               (LA_GCAPOL = LST#APOL(WIdx).NumApol) AND
               (LST#APOL(WIdx).Companh = 54);
               WNumOrdem = LST#APOL(WIdx).NumOrdem;
               LEAVE;
             ENDIF;
             WIdx += 1;
          ENDDO;
          IF (WNumOrdem = *ZEROS);
             WCodErro = 'ERRXXXX'; // O Registo não existe na Companhia
             LEAVESR;
          ENDIF;

          //---------------------------------------------------
          // Procurar existência do Recibo na Companhia
          //---------------------------------------------------
          WNumRecibo = %CHAR(LA_GCNREC);
          CHAIN (WNumRecibo) SD1RE1;
          IF (NOT %FOUND(SD1RE1));
             WCodErro = 'ERRXXXX'; // O Registo não existe na Companhia
             LEAVESR;
          ENDIF;
       ENDSR;

       //========================================================
       // Subrotina.: UpdRecibo
       // Descrição.: Obter todos os recibos do Seguurado
       //========================================================
       BEGSR UpdRecibo;
          //---------------------------------------------------
          // Actualizar ou inicializar o registo de Apólices
          //---------------------------------------------------
          IF (LA_GCTIPR = 0 OR LA_GCTIPR = 2);
             CHAIN (WNumOrdem) RCOBP142;
             IF (LST#APOL(WIdx).IsFirst);
                LST#APOL(WIdx).IsFirst = *OFF;
                COB_QTDEREC = 1;
             ELSE;
                COB_QTDEREC += 1;
             ENDIF;
             COB_ULTCONTI = REC_VTREC; // Obter o último recibo
             UPDATE RCOBP142 %FIELDS(COB_QTDEREC:COB_ULTCONTI);
          ENDIF;

          //---------------------------------------------------
          // Formatar a data do Estado
          //---------------------------------------------------
          WDatEstado = %DATE('0001-01-01');
          IF (REC_CODSIT = 2);
             WDatEstado = %DATE('20'+%SUBST(REC_DTCOB:5:2)+'-'
                                    +%SUBST(REC_DTCOB:3:2)+'-'
                                    +%SUBST(REC_DTCOB:1:2));
          ELSEIF (REC_CODSIT = 1 OR REC_CODSIT = 3);
             IF (REC_DTDEV <> *BLANKS);
                WDatEstado = %DATE('20'+%SUBST(REC_DTDEV:5:2)+'-'
                                      +%SUBST(REC_DTDEV:3:2)+'-'
                                      +%SUBST(REC_DTDEV:1:2));
             ENDIF;
          ENDIF;
          WMaCont = *ZEROS;
          WNumPlano   = *ZEROS;
          IF (LA_GCTIPR = 2);
             WMacont = %DEC('20'+%SUBST(REC_NREC2:1:4):6:0);

             //---------------------------------------------------
             // Actualizar a informação do Plano de pagamentos
             //---------------------------------------------------
             CHAIN (WNumOrdem:WMacont) RCOB14401;
             IF (%FOUND(FCOBP14401));
                WNumPlano = COB_NUMPLANO;
                CHAIN (WNumOrdem:WNumPlano) RCOBP144;
                COB_NUMRECIBO = LA_GCNREC;
                COB_IMREAL    = REC_VTREC;
                UPDATE RCOBP144 %FIELDS(COB_NUMRECIBO:COB_IMREAL);
             ENDIF;
          ENDIF;

          //---------------------------------------------------
          // Criar ou actualizar o registo da Apólice
          //---------------------------------------------------
          CHAIN (LA_GCNREC) RCOBP143;
          IF (NOT %FOUND(FCOBP143));
             COB_NUMRECIBO = LA_GCNREC;
             COB_NUMORDEM  = WNumOrdem;
             COB_CODSEG    = LA_GCNSEG;
             COB_TIPREC    = LA_GCTIPR;
             COB_MACONT    = WMACONT;
             COB_DATEMISSAO= $IntToDate(
                             $CvtData(REC_DTENTR:'DMY':'YYMD')); // DDMMAA
             COB_ESTADO    = REC_CODSIT;
             COB_DATESTADO = WDatEstado;
             COB_IMREAL    = REC_VTREC;
             COB_ISTOSELECT= WIsToSelect;
             COB_NUMPLANO  = WNumPlano;
             COB_COB14301  = %DATE('2000-01-01');
             COB_COB14302  = %DATE('9999-12-31');
             WRITE RCOBP143;
          ELSE;
             COB_NUMPLANO  = WNumPlano;
             COB_ESTADO    = REC_CODSIT;
             COB_DATESTADO = WDatEstado;
             UPDATE RCOBP143 %FIELDS(COB_ESTADO:COB_DATESTADO:
                                     COB_NUMPLANO);
          ENDIF;
       ENDSR;
      /END-FREE
