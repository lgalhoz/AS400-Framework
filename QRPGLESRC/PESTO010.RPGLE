     H AUT(*ALL) ALWNULL(*USRCTL) DFTACTGRP(*NO) ACTGRP(*CALLER)
     H BNDDIR('JMDIR')
     H*=================================================
     H* Programa..: PRPTO020
     H* Objectivo.: Extrair valores para o cálculo do prémio comercial
     H*
     H* Para compilar fazer:
     H*   1. CPYF FROMFILE(QS36F/GREGAH) TOFILE(QTEMP/TMPFILE) MBROPT(*NONE) CRTFILE(*YES)
     H*   3. CRTPF FILE(QTEMP/FINTT010) SRCFILE(#JMPRDSRC/QDDSSRC) SIZE(*NOMAX)
     H*
     H* Para executar fazer:
     H*   1. CLRPFM FILE(QTEMP/TMPFILE)
     H*   2. CPYF FROMFILE(TLUSIT2015/REC08A) TOFILE(QTEMP/FINTT010) MBROPT(*REPLACE) FMTOPT(*NOCHK)
     H*   3. CPYF FROMFILE(TLUSIT2015/REC08B) TOFILE(QTEMP/FINTT010) MBROPT(*ADD) FMTOPT(*NOCHK)
     H*   4. CPYF FROMFILE(TLUSIT2015/REC08C) TOFILE(QTEMP/FINTT010) MBROPT(*ADD) FMTOPT(*NOCHK)
     H*   5. CPYF FROMFILE(TLUSIT2015/REC08D) TOFILE(QTEMP/FINTT010) MBROPT(*ADD) FMTOPT(*NOCHK)
     H*=================================================
     FGCOBGR    IP   E           K DISK    PREFIX(LUS_)
     FGREGAH    IF   E           K DISK    PREFIX(REC_)
     F                                     EXTFILE('QS36F/GREGA15')
     FTMPFILE   UF A E           K DISK    PREFIX(REC_)
     F                                     RENAME(REGA:RTMP)
     F                                     EXTFILE('QTEMP/TMPFILE')
     F*=================================================
     D*==> Campos de Trabalho
     D WCodRamo        S                   LIKE(LUS_GCRAMO)
     D WDtCobra        S              8S 0                                      Formato (YYYYMMDD)
     D WLInferior      S              8S 0                                      Formato (YYYYMMDD)
     D WLSuperior      S              8S 0                                      Formato (YYYYMMDD)
     D WAnoMes         S              6S 0                                      Formato (YYYYMM)
     D WFTime          S              3S 0 INZ(385)
     D
     D*==> Constantes
     D S#Pendente      C                   0                                    Por cobrar
     D S#Cobrado       C                   1                                    Cobrados
     D S#Anulado       C                   3                                    Anulados
     D S#PreAnulacao   C                   4                                    Em Pré-Anulação
     D S#PreContecio   C                   6                                    Pré-Contencioso
     D T#Novo          C                   0                                    Novos
     D T#Adicional     C                   1                                    Adicionais
     D T#Continuado    C                   2                                    Continuados
     D T#Estorno       C                   5                                    Estornos
     D*=================================================
     C* //-----------------------------------------
     C* // Receber Periodo no formato YYYYMM
     C* //-----------------------------------------
     C     *ENTRY        PLIST
     C                   PARM                    WAnoMes
     C*=================================================
      /FREE
        //-----------------------------------------
        // Preparar ambiente de trabalho
        //-----------------------------------------
        IF (WFTime <> 645);
           EXSR StartEnv;
           WFTime = 645;
        ENDIF;

        //-----------------------------------------
        // Seleccionar recibos cobrados no período
        //-----------------------------------------
        IF (LUS_GCSITR = S#Cobrado);
           WDatCobra = LUS_GCDPAG + 20000000;
           IF (WDatCobra  <= WLSuperior) AND (WDatCobra >= WLInferior);
              SETLL (INT_KRAMO:INT_KAPOL) REGA;
              READE (INT_KRAMO:INT_KAPOL) REGA;
              DOW (NOT %EOF(GREGAH));
                 IF (REC_NREC = INT_KRECIBO);
                    IF (INT_KRAMO = 50);
                       EXSR AddRecord;
                    ENDIF;
                 ENDIF;
                 READE (INT_KRAMO:INT_KAPOL) REGA;
              ENDDO;
           ENDIF;
        ENDIF;

        //======================================================================
        // Subrotina.: AddRecord
        // Objectivo.: Adicionar o registo
        //======================================================================
        BEGSR StartEnv;
           WLInferior = (WAnoMes * 100);
           WLSuperior = WLInferior + 99;

           //--------------------------------------------
           // Criar ficheiro para receber os dados
           //--------------------------------------------
        ENDSR;

        //======================================================================
        // Subrotina.: AddRecord
        // Objectivo.: Adicionar o registo
        //======================================================================
        BEGSR AddRecord;
           FOR WCodRamo = 51 By 1 TO 54;
              SETLL (WCodRamo:INT_KAPOL) REGA;
              READE (WCodRamo:INT_KAPOL) REGA;
              DOW (NOT %EOF(GREGAH));
                 IF (REC_NREC = INT_KRECIBO);
                    WRITE RTMP;
                 ENDIF;
                 READE (INT_KRAMO:INT_KAPOL) REGA;
              ENDDO;
           ENDFOR;
        ENDSR;
      /END-FREE
