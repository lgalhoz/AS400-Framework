     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     **====================================================================
     ** Programa..: RCOBO005
     ** Objectivo.: Emitir uma listagem de recibos Pagos
     **====================================================================
     FFINTP020  IF   E           K DISK    PREFIX('INT_')
     FFINTP027  IF   E           K DISK    PREFIX('INT_')
     FFINTP028  IF   E           K DISK    PREFIX('INT_')
     FFCOBR005  O    F  212        PRINTER OFLIND(PrintOf) PRTCTL(PrintDS)
     F                                     USROPN
     F*====================================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010
      /Copy QRPGLESRC,SSECO010

     D*==> Estrutura de dados de controlo da Impressora
     D PrintDS         DS
     D   $Fill                 1     12
     D   $CurLin              13     15  0
     D
     D*==> Invocar API de comandos
     D shell           PR                  EXTPGM('QCMDEXC')
     D   StdCmd                     200A   CONST OPTIONS(*VARSIZE)
     D   StdLen                      15P 5
     D
     D*==> Lista de Campos do Mapa
     D WCodComp        S              3S 0
     D WDesComp        S             10A
     D WNumOrdem       S              7S 0
     D WCodRamo        S              3S 0
     D WNumApol        S             10A
     D WNomSeg         S             30A
     D WNumRecibo      S              9A
     D WValRecibo      S              9S 2
     D WDataHoje       S               D
     D
     D*==> Campos de trabalho
     D WStdCmd         S            200A
     D WStdLen         S             15P 5
     D WCodFile        S              9S 0
     D PCodFile        S              9S 0
     D WCodErro        S              7A
     D*====================================================================
     C     *ENTRY        PLIST
     C                   PARM                    PCodFile
     C                   PARM                    WCodErro
      /FREE
        //-----------------------------------------
        // Formatar os valores pode defeito
        //-----------------------------------------
        WCodErro = *BLANKS;
        MONITOR;
           WCodFile = PCodFile;
        ON-ERROR;
           WCodFile  = 115;
           WCodErro = 'COB0062';
        ENDMON;

        //-----------------------------------------
        // Validar se o registo é válido
        //-----------------------------------------
        IF (WCodErro = *BLANKS);
           CHAIN (WCodFile) RINTP020;
           IF (%FOUND(FINTP020));
              IF (INT_TPFILE <> 'COB') AND (INT_TPFILE <> 'PAG');
                 WCodErro = 'COB0063';
              ENDIF;
           ELSE;
              WCodErro = 'COB0064';
           ENDIF;
        ENDIF;

        //-----------------------------------------
        // Processar o registo
        //-----------------------------------------
        IF (WCodErro = *BLANKS);
           EXSR OpenReport;

           SETLL (WCodFile) RINTP027;
           READE (WCodFile) RINTP027;
           DOW (NOT %EOF(FINTP027));
              CHAIN (INT_CDREGIS) RINTP028;
              IF (%FOUND(FINTP028));
                 EXSR PrtLinha;
              ELSE;
                 WCodErro = 'COB0065';
              ENDIF;
              READE (WCodFile) RINTP027;
           ENDDO;
        ENDIF;

        *INLR = *ON;

        //==========================================================
        // Subrotina.: PrtLinha
        // Objectivo.: Imprimir a linha do Mapa
        //==========================================================
        BEGSR PrtLinha;
           WCodComp   = INT_CCOMP;
           WDesComp   = *BLANKS;
           WNumOrdem  = INT_NUMORDRC;
           WCodRamo   = INT_CDRAMO;
           WNumApol   = INT_NUAPOL;
           WNomSeg    = INT_NOMSEG;
           WNumRecibo = INT_NUMREC;
           WValRecibo = INT_IMTOTREC;

           IF (INT_NUMORDRC > *ZEROS);
           ELSE;
           ENDIF;

           EXCEPT  PRT_DETAIL;
           IF (PrintOf);
              EXCEPT PRT_HEADER;
              PrintOf = *OFF;
           ENDIF;
        ENDSR;

        //==========================================================
        // Subrotina.: OpenReport
        // Objectivo.: Abrir o Mapa para Escrita
        //==========================================================
        BEGSR OpenReport;
           WDataHoje  = %DATE();
           MONITOR;
              WStdCmd = 'OVRPRTF FILE(FCOBR005) TOFILE(QSYS/QSYSPRT) +
                         PAGESIZE(*N 212) OUTQ(PDFARCH)';
              WStdLen = %LEN(WStdCmd);
              shell(WStdCmd:WStdLen);
              OPEN FCOBR005;
           ON-ERROR;
           ENDMON;
           EXCEPT  PRT_HEADER;
        ENDSR;
      /END-FREE
     C*============================================================
     OFCOBR005  E            PRT_HEADER     1  1
     O                                            9 'FCOBR005'
     O                                           84 'Recibos Pagos      '
     O                                          103 'Pag.:'
     O                       PAGE          Z    114
     O          E            PRT_HEADER     2
     O                                          103 'Data:'
     O                       WDataHoje          114
     O          E            PRT_HEADER     1
     O                                           24 'Número'
     O                                           91 'Número'
     O          E            PRT_HEADER     1
     O                                            5 'Comp.'
     O                                           16 'Descrição'
     O                                           24 'Ordem'
     O                                           29 'Ramo'
     O                                           40 'Apólice'
     O                                           81 'Segurado'
     O                                           91 'Recibo.'
     O                                          101 'Valor'
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
     OFCOBR005  E            PRT_DETAIL
     O                       WCodComp             5
     O                       WDesComp            16
     O                       WNumOrdem           24 '       '
     O                       WCodRamo            29
     O                       WNumApol            40
     O                       WNomSeg             81
     O                       WNumRecibo          91
     O                       WValRecibo         101 ' .   .   ,  '
     O                                          103 '¤'
