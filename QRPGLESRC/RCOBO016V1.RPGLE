     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     **====================================================================
     ** Programa..: PRPTO010
     ** Objectivo.: Extracto das Cobran�as realizadas no dia para o Mediador
     **
     H* Execu��o..: http://20.0.0.1/DemoComp?frame=login&user=apache
     H*             http://20.0.0.1/DemoComp?frame=recdia&data=20150707
     H*
     **====================================================================
     FRCOBI016  O    F  212        PRINTER OFLIND(PrintOf) PRTCTL(PrintDS)
     F                                     USROPN
     F*====================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SSECO010

     D*==>
     D PrintDS         DS
     D   $Fill                 1     12
     D   $CurLin              13     15  0
     D
     D*==> Chamada de fun��o Dynamic, para trabalhar detalhe do Item
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
     D WSession        S              5S 0
     D WRecDIR         S              5S 0
     D WIcErro         S               N   INZ(*ON)
     D WMsgErro        S              7A
     D*====================================================================
     C*==> M�s do processamento
     C     *ENTRY        PLIST
     C                   PARM                    PDatSelect
     C                   PARM                    PCodMediador
     C
      /FREE
        //------------------------------------
        // Se n�o estiver autorizado ent�o sai
        //------------------------------------
        IF NOT $ValAccess(S#PGM:'':99999);
           *INLR = *ON;
           RETURN;
        ENDIF;

        //------------------------------------------------------------------
        // Tratar parametros de entrada, Por defeito cobran�as do dia para
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

        WMsgErro = *BLANKS;
        EXSR OpenPRTF;
        IF (WMsgErro = *BLANKS);
           EXSR ExtractData;
        ENDIF;

        *INLR = *ON;
        RETURN;


        //=============================================================
        // Subrotina.: ExtractData
        // Objectivo.: Extair os registos do ficheiro de Recibos
        //=============================================================
        BEGSR ExtractData;
           WSession = $SetFiles('QS36F/"SD.1.REC"');
           $SetFields('NDP,UNACHX,FP,DPAG,DTCOB,NORD2,CODMOV,CRAMO,+
                       APOLIC,NREC2,DTINIC,DTFIM,VTREC,COMIS1,+
                       MCCOB');
           $SetSort('MCCOB,UNACHX');
           $SetCriteria('(MCCOB = 1) AND (DTCOB=''070715'')');
           DOW ($GetRecord(WRecDIR) > *ZEROS);
           // WNomFile    = $GetField('NREC2');
              WRecDIR += 1;
           ENDDO;
           $RmvDBSession(WSession);
        ENDSR;

        //=============================================================
        // Subrotina.: OpenPRTF
        // Objectivo.: Abrir o ficheiro de Impress�o
        //=============================================================
        BEGSR OpenPRTF;
           MONITOR;
              WComando = 'OVRPRTF FILE(RCOBI016) OUTQ(PRT06) DRAWER(1) +
                          SAVE(*YES) CPI(16.7) +
                          TOFILE(QSYSPRT) DUPLEX(*YES) PAGESIZE(*N 198) +
                          PRTQLTY(*NLQ) LPI(8) SPLFNAME(RCOB16) +
                          FRONTMGN(.075 .075) USRDTA(''RPGILE'') HOLD(*YES)';
              ShellExec(%TRIM(WComando):%LEN(%TRIM(WComando)));
              OPEN RCOBI016;
           ON-ERROR;
              WMsgErro = 'COBXXXX';
           ENDMON;
        ENDSR;
      /END-FREE
     C*============================================================
     ORCOBI016  E            PRT_HEADER     1  1
     O                                            9 'RCATP010'
     O                                           84 'Extracto de Cobran�as do '
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
     O                                           12 'Ap�lice'
     O                                           17 'Nome'
     O                                           63 'Aplica��o'
     O                                           73 '  Base  '
     O                                           84 'Base'
     O                                           93 'Vencim. '
     O                                          104 'Venc.'
     O                                          114 'Aplicacao'
     O                                          133 'Observa��o Linha 1'
     O                                          179 'Observa��o Linha 2'
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
     O*                      REC_CRAMO            3
     O*                      REC_APOLIC          12
     O*                      REC_CSEGUR          53
