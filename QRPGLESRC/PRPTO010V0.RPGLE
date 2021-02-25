     H*=================================================
     H* Programa..: PPRTO010
     H* Objectivo.: Escrever Carta/Acta para acompanhar
     H*             os Avisos Cobrança.
     H* Observação: A última é em Fevereiro de 2015
     H* Compilar com a opção
     H*   CRTLF FILE(QTEMP/SINP01) SRCFILE(LIB.SINO/QDDSSRC)
     H*   CPYF FROMFILE(QS36F/GBASEGRO) TOFILE(QTEMP/GBASEGRO) MBROPT(*REPLACE) CRTFILE(*YES)
     H*=================================================
     FGBASEGRO  UF   E           K DISK    EXTFILE('QTEMP/GBASEGRO')
     F                                     PREFIX(PRD_)
     FFPRDP120  IF   E           K DISK    PREFIX(TBL_)
     FSINP01    IF   E           K DISK    USROPN PREFIX(SIN_)
     FGFSEG     IF   E           K DISK    PREFIX(SEG_)
     FFPRTI000  O    E             PRINTER USROPN
     FFPRTI010  O    E             PRINTER USROPN
     FFPRTI011  O    E             PRINTER USROPN
     FFPRTI012  O    E             PRINTER USROPN
     F*=================================================
     D
     D*==> Lista de Parametros
     D shell           PR                  EXTPGM('QCMDEXC')
     D   StdCmd                     200A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5
     D
     D*==> Campos de Trabalho
     D WCartaSel       S              4A
     D WNomeCarta      S             10A
     D WCodCarta       S              1S 0
     D
     D WStdCmd         S            200A
     D WStdLen         S             15P 5
     D WMesIni         S              2S 0
     D WQtdeSin        S              3S 0
     D WAnoCont        S              4S 0 INZ(*ZEROS)
     D WMesCont        S              2S 0 INZ(*ZEROS)
     D WMesAlfa        S             12A
     D WDataContin     S             30A
     D WMsgErro        S              7A   INZ(*BLANKS)
     D*=================================================
      /FREE
        EXSR GetLastProc;
        EXSR CrtLogical;
        IF (WMSgErro <> *BLANKS);
           RETURN;
        ENDIF;

        SETLL *LOVAL GBASEGRO;
        READ GBASEGRO;
        DOW (NOT %EOF(GBASEGRO));
           WMesIni = %DEC(%SUBST(%CHAR((PRD_DTINI+1000000)):4:2):2:0);
           EXSR SelApolice;
           READ GBASEGRO;
        ENDDO;

        //--------------------------------------------------
        // Fechar os ficheiros de Impressão das Cartas
        //--------------------------------------------------
        IF (%SUBST(WCartaSel:1:1) = 'X');
           CLOSE FPRTI010;
        ENDIF;
        IF (%SUBST(WCartaSel:2:1) = 'X');
           CLOSE FPRTI011;
        ENDIF;
        IF (%SUBST(WCartaSel:3:1) = 'X');
           CLOSE FPRTI012;
        ENDIF;
        IF (%SUBST(WCartaSel:4:1) = 'X');
           CLOSE FPRTI000;
        ENDIF;
        CLOSE SINP01;
        RETURN;

        //=================================================================
        // Subrotina..: SelApolice
        // Objectivo..: Selecionar as apólices a escrever e em conformidade
        //              com o critério de selecção
        //=================================================================
        BEGSR SelApolice;
           IF (PRD_RAMO=5) AND (PRD_MED1=924) AND (PRD_SIT = 1) AND
             (PRD_CONDOM <> 'C') AND ((PRD_CF05='S') OR
             (PRD_CF06='S')) AND (WMesIni = WMesCont);

              WQtdeSin = *ZEROS;
              SETLL (PRD_RAMO:PRD_APOL) SINP01;
              READE (PRD_RAMO:PRD_APOL) SINP01;
              DOW (NOT %EOF(SINP01));
                 IF (SIN_CAUSA = '28');
                    WQtdeSin += 1;
                 ENDIF;
                 READE (PRD_RAMO:PRD_APOL) SINP01;
              ENDDO;

              IF (WQtdeSin > 1);
                 LEAVESR;
              ENDIF;

              IF (PRD_CF05 = 'S') AND (PRD_CF06 = 'S');
                 WNomeCarta = 'FPRTI010';
                 WCodCarta = 1;
              ELSEIF (PRD_CF05 = 'S');
                 WNomeCarta  = 'FPRTI011';
                 WCodCarta = 2;
              ELSEIF (PRD_CF06 = 'S');
                 WNomeCarta = 'FPRTI012';
                 WCodCarta = 3;
              ENDIF;

              IF (%SUBST(WCartaSel:WCodCarta:1) <> 'X');
                 MONITOR;
                    WStdCmd = 'OVRPRTF FILE('+%TRIM(WNomeCarta)+') +
                               PAGRTT(0) +
                               OUTQ(PDFARCH) +
                               HOLD(*YES) SAVE(*YES)';
                    WStdLen = %LEN(WStdCmd);
                    shell(WStdCmd:WStdLen);
                 ON-ERROR;
                 ENDMON;

                 IF (WCodCarta = 1);
                    OPEN FPRTI010;
                 ELSEIF (WCodCarta = 2);
                    OPEN FPRTI011;
                 ELSEIF (WCodCarta = 3);
                    OPEN FPRTI012;
                 ENDIF;

                 IF (%SUBST(WCartaSel:4:1) <> 'X');
                    OPEN FPRTI000;
                    %SUBST(WCartaSel:4:1) = 'X';
                    WRITE RPRTI00000;
                 ENDIF;
              ENDIF;
              %SUBST(WCartaSel:WCodCarta:1) = 'X';
              EXSR ESCREVER;
           ENDIF;
        ENDSR;

        //=================================================================
        // Subrotina..: Escrever
        // Objectivo..: Escrever o texto das Cartas
        //=================================================================
        BEGSR Escrever;
           CHAIN (PRD_NSEG) GFSEG;
           IF (%FOUND(GFSEG));
              DATA  = WDataContin;
              CF05 = 'Não';
              CF06 = 'Não';
              IF (PRD_CF05 = 'S');
                 CF05 = 'Sim';
              ENDIF;
              IF (PRD_CF06 = 'S');
                 CF06 = 'Sim';
              ENDIF;
              ANO   = WAnoCont;
              MES   = WMesCont;
              Qtde  = WQtdeSin;
              APOL  = PRD_APOL;
              SEG   = SEG_NOM;
              SEG   = SEG_NOM;
              MORAD = SEG_MORAD;
              CPOST = SEG_CPOST;
              DESCP = SEG_DESCP;
              ACTA  = PRD_ACTA + 1;
              //---------------------------------------------
              // Escrever Controlo das Cartas
              //---------------------------------------------
              WRITE RPRTI00001;

              //---------------------------------------------
              // Escrever a Carta
              //---------------------------------------------
              IF (WCodCarta = 1);
                 WRITE RPRTI01001;
              ELSEIF (WCodCarta = 2);
                 WRITE RPRTI01101;
              ELSEIF (WCodCarta = 3);
                 WRITE RPRTI01201;
              ENDIF;
           ENDIF;
        ENDSR;

        //=================================================================
        // Subrotina..: CrtLogical
        // Objectivo..: Criar Ficheiro Lógico
        //=================================================================
        BEGSR CrtLogical;
           WMsgErro = *BLANKS;
           MONITOR;
              WStdCmd = 'DLTF FILE(QTEMP/SINP01)';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);
           ON-ERROR;
           ENDMON;

           MONITOR;
              WStdCmd = 'CRTLF FILE(QTEMP/SINP01) +
                               SRCFILE(LIB.SINO/QDDSSRC) +
                               SRCMBR(SINP01)';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);
           ON-ERROR;
              WMsgErro = 'PRDXXXX';
              LEAVESR;
           ENDMON;

           MONITOR;
              WStdCmd = 'DLTSPLF FILE(SINP01)';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);
           ON-ERROR;
           ENDMON;

           OPEN SINP01;
        ENDSR;

        //=================================================================
        // Subrotina..: GetLastProc
        // Objectivo..: Obter o último ano e mês Processados.
        //=================================================================
        BEGSR GetLastProc;
           IF (WAnoCont = *ZEROS);
              SETLL *LOVAL RPRDP120;
              READ RPRDP120;
              DOW NOT %EOF;
                 IF ((TBL_ANOIDX*100)+TBL_MESIDX) > ((WAnoCont*100)+WMesCont);
                    WAnoCont = TBL_ANOIDX;
                    WMesCont = TBL_MESIDX;
                    EXSR GetMesAlfa;
                    WdataContin = %CHAR(%SUBDT(TBL_DATEXEC:*DAYS)) + ' de '
                                  + %TRIM(WMesAlfa) + ' de ' +
                                  %CHAR(%SUBDT(TBL_DATEXEC:*YEARS));
                 ENDIF;
                 READ RPRDP120;
              ENDDO;
           ENDIF;
        ENDSR;
        BEGSR GetMesAlfa;
           IF (%SUBDT(TBL_DATEXEC:*MONTHS) = 1);
              WMesAlfa = 'Janeiro';
           ELSEIF (%SUBDT(TBL_DATEXEC:*MONTHS) = 2);
              WMesAlfa = 'Fevereiro';
           ELSEIF (%SUBDT(TBL_DATEXEC:*MONTHS) = 2);
              WMesAlfa = 'Março';
           ELSEIF (%SUBDT(TBL_DATEXEC:*MONTHS) = 2);
              WMesAlfa = 'Abril';
           ELSEIF (%SUBDT(TBL_DATEXEC:*MONTHS) = 2);
              WMesAlfa = 'Maio';
           ELSEIF (%SUBDT(TBL_DATEXEC:*MONTHS) = 2);
              WMesAlfa = 'Junho';
           ELSEIF (%SUBDT(TBL_DATEXEC:*MONTHS) = 2);
              WMesAlfa = 'Julho';
           ELSEIF (%SUBDT(TBL_DATEXEC:*MONTHS) = 2);
              WMesAlfa = 'Agosto';
           ELSEIF (%SUBDT(TBL_DATEXEC:*MONTHS) = 2);
              WMesAlfa = 'Setembro';
           ELSEIF (%SUBDT(TBL_DATEXEC:*MONTHS) = 2);
              WMesAlfa = 'Outubro';
           ELSEIF (%SUBDT(TBL_DATEXEC:*MONTHS) = 2);
              WMesAlfa = 'Novembro';
           ELSEIF (%SUBDT(TBL_DATEXEC:*MONTHS) = 2);
              WMesAlfa = 'Dezembro';
           ENDIF;
        ENDSR;
      /END-FREE
