     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     **====================================================================
     ** Programa..: PRPTO010
     ** Objectivo.: Extracto das Cobranças realizadas no dia para o Mediador
     **
     **====================================================================
     FSD1REC    IF   F  326     6AIDISK    KEYLOC(5) EXTFILE('SD.1.REC')
     FGFSEG     IF   E           K DISK
     FRCOBI016  O    F  212        PRINTER OFLIND(PrintOf) PRTCTL(PrintDS)
     F                                     USROPN
     F*====================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SSECO010

     D*==> Foramto de registo das Cobranças
     D R#SD1REC      E DS                  EXTNAME('SD.1.REC') PREFIX('REC_')
     D
     D*==>
     D PrintDS         DS
     D   $Fill                 1     12
     D   $CurLin              13     15  0
     D
     D*==> Chamada de função Dynamic, para trabalhar detalhe do Item
     D ShellExec       PR                  EXTPGM('QCMDEXC')
     D   stdcmd                     300A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5 CONST
     D WComando        S            300A   INZ(*BLANKS)
     D
     D*==> Restantes campos de trabalho
     D PDatSelect      S              8A
     D PCodMediador    S              6A
     D WDatSelect      S              8S 0
     D WCodMediador    S              6S 0
     D PrintOf         S               N
     D WIcErro         S               N   INZ(*ON)
     D WMsgErro        S              7A
     D*====================================================================
     C*==> Mês do processamento
     C     *ENTRY        PLIST
     C                   PARM                    PDatSelect
     C                   PARM                    PCodMediador
     C
      /FREE
        //------------------------------------
        // Se não estiver autorizado então sai
        //------------------------------------
        IF NOT $ValAccess(S#PGM:'':99999);
           *INLR = *ON;
           RETURN;
        ENDIF;

        //------------------------------------------------------------------
        // Tratar parametros de entrada, Por defeito cobranças do dia para
        // todos os Mediadores
        //------------------------------------------------------------------
        MONITOR;
           WDatSelect = 0; // PDatSelect;
        ON-ERROR;
           WDatSelect = 4; // Data do dia
        ENDMON;
        MONITOR;
           WCodMediador =  0; // PCodMediador;
        ON-ERROR;
           WCodMediador = *ZEROS;
        ENDMON;

        MONITOR;
           WComando = 'OVRPRTF FILE(RCOBI016) OUTQ(PRT06) DRAWER(1) +
                       SAVE(*YES) CPI(16.7) +
                       TOFILE(QSYSPRT) DUPLEX(*YES) PAGESIZE(*N 198) +
                       PRTQLTY(*NLQ) LPI(8) SPLFNAME(RCOB16) +
                       FRONTMGN(.075 .075) USRDTA(''RPGILE'') HOLD(*YES)';
           ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
        ON-ERROR;
        ENDMON;
        OPEN RCOBI016;

        READ  SD1REC R#SD1REC;
        DOW (NOT %EOF(SD1REC));
            EXCEPT PRT_HEADER;
            READ  SD1REC R#SD1REC;
            LEAVE;
        ENDDO;
        CLOSE RCOBI016;

        *INLR = *ON;
        RETURN;
      /END-FREE
     C*============================================================
     ORCOBI016  E            PRT_HEADER     1  1
     O                                            9 'RCATP010'
     O                                           84 'Extracto de Cobranças do '
     O                                          103 'Pag.:'
     O                       PAGE          Z    114
     O          E            PRT_HEADER     2
     O                                          103 'Data:'
     O*                      WDatHoje           114
     O          E            PRT_HEADER     1
     O                                            4 '----'
     O                                           12 '-------'
     O                                           39 '--------------------------'
     O                                           53 '--------------'
     O                                           63 '---------'
     O                                           73 '--------'
     O                                           84 '----------'
     O                                           93 '--------'
     O                                          104 '----------'
     O                                          114 '---------'
     O                                          159 '--------------------------'
     O                                          205 '--------------------------'
     O                                          207 '-'
     O                                          212 '----'
     O          E            PRT_HEADER     1
     O                                           73 'Ano/Tri.'
     O                                           84 'Valor'
     O                                           93 'Ano/Tri.'
     O                                          104 'Valor'
     O          E            PRT_HEADER     1
     O                                            4 'Ramo'
     O                                           12 'Apólice'
     O                                           17 'Nome'
     O                                           63 'Aplicação'
     O                                           73 '  Base  '
     O                                           84 'Base'
     O                                           93 'Vencim. '
     O                                          104 'Venc.'
     O                                          114 'Aplicacao'
     O                                          133 'Observação Linha 1'
     O                                          179 'Observação Linha 2'
     O                                          207 'I'
     O                                          212 'Taxa'
     O          E            PRT_HEADER     1
     O                                            4 '----'
     O                                           12 '-------'
     O                                           39 '--------------------------'
     O                                           53 '--------------'
     O                                           63 '---------'
     O                                           73 '--------'
     O                                           84 '----------'
     O                                           93 '--------'
     O                                          104 '----------'
     O                                          114 '---------'
     O                                          159 '--------------------------'
     O                                          205 '--------------------------'
     O                                          207 '-'
     O                                          212 '----'
     ORCOBI016  E            PRT_DETAIL
     O                       REC_CRAMO            3
     O                       REC_APOLIC          12
     O                       REC_CSEGUR          53
