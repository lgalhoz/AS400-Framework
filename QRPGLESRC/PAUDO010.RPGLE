     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*=====================================================================
     H* Programa...: PAUDO010
     H* Objectivo..: Converter Extracto de Movimentos da Lusitânia.
     H* Observação.:
     H*   CRTPF FILE(QTEMP/FAUDP020) SRCFILE(#JMPRDSRC/QDDSSRC) SIZE(*NOMAX)
     H*   CRTPF FILE(QTEMP/FAUDP022) SRCFILE(#JMPRDSRC/QDDSSRC) SIZE(*NOMAX)
     H*=====================================================================
     FFAUDP020  UF   E           K DISK    PREFIX(INT_)
     FFAUDP022  UF A E           K DISK    PREFIX(OUT_) USROPN
     FFAUDP010  IF   E           K DISK    PREFIX(RUL_)
     F*=====================================================================
     D*==> Decompor Códigos
     D DS#REFERENC     DS
     D   WRef#Comp             1     50A
     D   WRef#Ramo             2      3S 0
     D   WRef#Apol             5     11S 0
     D   WRef#Recibo          13     18S 0
     D   WRef#Last            14     18S 0
     D
     D*==> Lista de Parametros
     D shell           PR                  EXTPGM('QCMDEXC')
     D   StdCmd                     200A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5
     D
     D*==> Campos de Trabalho
     D WStdCmd         S            200A
     D WStdLen         S             15P 5
     D
     D WChave          S              9S 0
     D WTotValAlfa     S             11A
     D WTotValor       S             11S 2
     D WIcExiste       S               N
     D WIsToSelect     S               N
     D WActRubrica     S              1A
     D WCodigo         S              4A
     D WMsgErro        S              7A
     D WIdx            S              3S 0
     D*=====================================================================
      /FREE
        EXSR PrepAudit;

        WChave = *ZEROS;
        READ RAUDP020;
        DOW (NOT %EOF());
           //--------------------------------------
           // Validar se existe alguma regra para o
           // Código de movimento
           //--------------------------------------
           WIsToSelect = *OFF;
           WCodigo = '  ' + %TRIM(INT_CODIGO);
           SETLL (WCodigo) RAUDP010;
           READE (WCodigo) RAUDP010;
           DOW (NOT %EOF(FAUDP010));
              IF (INT_DSCRITIVO = RUL_DSCRITIVO);
                 WIsToSelect = *ON;
                 LEAVE;
              ENDIF;
              READE (WCodigo) RAUDP010;
           ENDDO;

           IF (INT_TOMADOR = 'JOSE MATA, LDA / COMISSOES');
              WIsToSelect = *OFF;
           ENDIF;

           //-------------------------------------------------
           // Converter o registo - Lusitânia
           //-------------------------------------------------
           IF (WIsToSelect);
              WIcExiste = *OFF;
              WRef#Comp = INT_REFERENC;
              SETLL (WRef#Recibo:WRef#Apol) RAUDP022;
              READE (WRef#Recibo:WRef#Apol) RAUDP022;
              DOW (NOT %EOF(FAUDP022));
                 IF (INT_REFERENC = OUT_REFERENC) AND
                     (INT_DATA = %CHAR(OUT_DTOPER));
                    WActRubrica = *BLANKS;
                    IF (OUT_TOTREC > *ZEROS);
                       WActRubrica = 'T';
                    ENDIF;
                    IF (OUT_TOTCOM > *ZEROS);
                      IF (WActRubrica <> *BLANKS);
                         WActRubrica = 'A';
                      ELSE;
                         WActRubrica = 'C';
                      ENDIF;
                    ENDIF;

                    WIcExiste = *ON;
                    EXSR GetValor;
                    UPDATE RAUDP022 %FIELDS(OUT_TOTREC:OUT_TOTCOM:OUT_OTHERS:
                                           OUT_MSGERRO);
                    LEAVE;
                 ENDIF;
                 READE (WRef#Recibo:WRef#Apol) RAUDP022;
              ENDDO;

              IF (NOT WIcExiste);
                 WChave += 1;
                 OUT_TOTREC = *ZEROS;
                 OUT_TOTCOM = *ZEROS;
                 OUT_OTHERS = *ZEROS;

                 WActRubrica = *BLANKS;
                 EXSR GetValor;
                 OUT_LSCHAVE  = WChave;
                 OUT_NUMREC   = WRef#Recibo;
                 OUT_NUAPOL   = WRef#Apol;
                 OUT_CDRAMO   = WRef#Ramo;
                 OUT_LSNREC   = WRef#Recibo;
                 OUT_LSAPOL   = WRef#Apol;
                 OUT_LSRAMO   = WRef#Ramo;
                 OUT_TIPMOV   = RUL_TIPRECIBO;
                 OUT_SITMOV   = RUL_SITRECIBO;
                 OUT_NUSEGU   = *ZEROS;
                 OUT_DTOPER   = %DEC(INT_DATA:8:0);
                 OUT_REFERENC = INT_REFERENC;
                 OUT_DTMOVI   = *ZEROS;
                 WRITE RAUDP022;
              ENDIF;
              INT_CHAVE  = WChave;
              INT_ICCONV = 'S';
              INT_NUMREC = WRef#Recibo;
              INT_CDRAMO = WRef#Ramo;
              INT_NUAPOL = WRef#Apol;
              UPDATE RAUDP020 %FIELDS(INT_NUMREC:INT_CDRAMO:INT_NUAPOL
                                      :INT_ICCONV:INT_CHAVE);
           ENDIF;
           READ RAUDP020;
        ENDDO;

        CLOSE FAUDP022;

        *INLR = *ON;

        //===================================================================
        // Subrotina.: GetValor
        // Objectivo.: Calcular os valores
        //===================================================================
        BEGSR GetValor;
           OUT_MSGERRO = *BLANKS;
           IF (RUL_TIPO = 'D');
              WTotValAlfa = INT_DEBITO;
           ELSE;
              WTotValAlfa = INT_CREDITO;
           ENDIF;
           EXSR FixString;
           WTotValor = %DEC(WTotValAlfa:11:2);

           IF (RUL_INCIDENC = 'R');
              OUT_TOTREC  += WTotValor;
              IF (WActRubrica = 'T') OR (WActRubrica = 'A');
                 OUT_MSGERRO = '0000001';
              ENDIF;
           ELSEIF (RUL_INCIDENC = 'C');
              OUT_TOTCOM  += WTotValor;
              IF (WActRubrica = 'C') OR (WActRubrica = 'A');
                 OUT_MSGERRO = '0000002';
              ENDIF;
           ELSEIF (RUL_INCIDENC = 'O');
              OUT_OTHERS  += WTotValor;
           ENDIF;
        ENDSR;

        //===================================================================
        // Subrotina.: PrepAudit
        // Objectivo.: Preparar Auditoria aos movimentos
        //===================================================================
        BEGSR PrepAudit;
           //------------------------------------------------------
           // Controlo de Registos
           //------------------------------------------------------
           MONITOR;
              WStdCmd = 'CLRPFM FILE(QTEMP/FAUDP022)';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);
           ON-ERROR;
           ENDMON;
           OPEN FAUDP022;
        ENDSR;

        //===================================================================
        // Subrotina.: FixString
        // Objectivo.: Corrigir a String, eliminando caracteres
        //===================================================================
        BEGSR FixString;
           WIdx = *ZEROS;
           DOW (WIdx < 10);
              WIdx += 1;
              IF (%SUBST(WTotValAlfa:WIdx:1) = '.');
                 WTotValAlfa = %SUBST(WTotValAlfa:1:(WIdx - 1)) +
                            %SUBST(WTotValAlfa:(WIdx + 1));
              ENDIF;
           ENDDO;
        ENDSR;
      /END-FREE
