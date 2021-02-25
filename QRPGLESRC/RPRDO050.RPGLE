     H ALWNULL(*USRCTL)  BNDDIR('JMDIR') DFTACTGRP(*NO) ACTGRP(*NEW)
     H AUT(*ALL)
     H*===========================================================
     H* Programa..: RPRDO050
     H* Descrição.: Importar registo para a Apólice Condominio
     H*
     H* Para Compilar e
     H*    CRTPF FILE(QTEMP/FINTT027) SRCFILE(#JMPRDSRC/QDDSSRC)
     H*    CRTPF FILE(QTEMP/INTFILE) SRCFILE(#JMPRDSRC/QDDSSRC) SRCMBR(FINTT027)
     H*===========================================================
     FFPRDP010  UF A E           K DISK    PREFIX(PRD_) COMMIT
     FFPRDP012  UF A E           K DISK    PREFIX(PRD_) COMMIT
     FFPRDP017  UF A E           K DISK    PREFIX(PRD_) COMMIT
     F
     FFPRDP01001IF   E           K DISK    PREFIX(PRD_)
     F                                     RENAME(RPRDP010:RPRDP01001)
     FFCATP105  IF   E           K DISK    PREFIX(CAT_)
     FFINTT027  IF   E           K DISK    PREFIX(TMP_) USROPN
     F                                     EXTFILE('QTEMP/INTFILE')
     F*===========================================================
      /Copy QRPGLESRC,SGRLO010
      /Copy QRPGLESRC,SDBSO010

     D*==> Campos de Trabalho
     D WDatSelect      S               D
     D WCdRamo         S              2S 0
     D WNuApol         S              7S 0
     D WCdTemplate     S              5S 0
     D WMsgErro        S              7A
     D WDatInicio      S              8S 0
     D WNomFile        S             40A
     D WNuSequ         S              5S 0
     D WNuSqSup        S              5S 0
     D WRisco          S              5S 0
     D WCdTexto        S              5S 0
     D WCdProd         S              5S 0
     D WPremio         S              6S 2
     D*===========================================================
     C     *ENTRY        PLIST
     C                   PARM                    WNomFile
     C*                  PARM                    WDatInicio
     C                   PARM                    WMsgErro
      /FREE
       //----------------------------------------------
       // Procede `a Leitura do ficheiro de Interface
       //----------------------------------------------
       WDatInicio = 20110826;
       MONITOR;
          WCdTemplate = 17;
          WMsgErro = $GetFTP(WCdTemplate:WNomFile:'QTEMP':'INTFILE');
          IF (WMsgErro = *BLANKS);
             EXSR LerInterface;
          ENDIF;
       ON-ERROR;
       ENDMON;
       RETURN;

       //=======================================================================
       // Subrotina.: LerInterface
       // Objectivo.: Proceder à leitura do ficheiro de Interface
       //=======================================================================
       BEGSR LerInterface;
          WCdTexto = *ZEROS;
          OPEN FINTT027;
          READ FINTT027;
          DOW (NOT %EOF(FINTT027));
             IF (TMP_CDRAMO <> 'Ramo') AND (TMP_CDRAMO <> *BLANKS);
                EXSR GetRaiz;
                IF (WMsgErro <> *BLANKS);
                   LEAVE;
                ENDIF;

                PRD_CDRAMO   = WCdRamo;
                PRD_NUAPOL   = WNuApol;
                PRD_NUSEQU   = WNuSequ;
                PRD_NUSQSUP  = WNuSqSup;
                PRD_CDTEXTO  = *ZEROS;
                PRD_PRD01001 = $IntToDate(WDatInicio);
                PRD_PRD01002 = %DATE('9999-12-31');
                PRD_PRD01003 = 1;
                PRD_PRD01004 = 'AUTOM-CI';
                PRD_PRD01005 = 'AUTOM-CI';
                WRITE RPRDP010;

                PRD_CDRAMO   = WCdRamo;
                PRD_NUAPOL   = WNuApol;
                PRD_NUSEQU   = WNuSequ;
                PRD_NUSEQHST = 1;
                MONITOR;
                   WPremio = %DEC(TMP_PRTOTAL:6:2);
                ON-ERROR;
                   WPremio = *ZEROS;
                ENDMON;
                PRD_DSDETAIL = '('+%TRIM(TMP_CDFRAC)+') '+ %TRIM(TMP_DSFRAC) +
                               ', Prem.' + %EDITW(WPremio:'    ,  ¤');
                PRD_ICDETAIL = 'S';
                MONITOR;
                   PRD_CPSEGURO = %DEC(TMP_CPSEGUR:11:2);
                ON-ERROR;
                   PRD_CPSEGURO = *ZEROS;
                ENDMON;
                PRD_PRD01201 = $IntToDate(WDatInicio);
                PRD_PRD01202 = %DATE('9999-12-31');
                PRD_PRD01203 = 1;
                PRD_PRD01204 = 'AUTOM-CI';
                PRD_PRD01205 = 'AUTOM-CI';
                WRITE RPRDP012;

                PRD_CDRAMO    = WCdRamo;
                PRD_NUAPOL    = WNuApol;
                PRD_NUSEQU    = WNuSequ;
                PRD_NUSEQHST  = 1;
                PRD_CDSEGUR   = *ZEROS;
                PRD_CDFRAC    = TMP_CDFRAC;
                PRD_DSFRAC    = TMP_DSFRAC;
                MONITOR;
                   PRD_PRFRAC = %DEC(TMP_PERMIL:6:2);
                ON-ERROR;
                   PRD_PRFRAC = *ZEROS;
                ENDMON;
                MONITOR;
                   PRD_CPSEGURO = %DEC(TMP_CPSEGUR:11:2);
                ON-ERROR;
                   PRD_CPSEGURO = *ZEROS;
                ENDMON;
                MONITOR;
                   PRD_IMPREMIO = %DEC(TMP_PRTOTAL:11:2);
                ON-ERROR;
                   PRD_IMPREMIO = *ZEROS;
                ENDMON;
                PRD_CDCREDOR = *ZEROS;
                PRD_IMCREDOR = *ZEROS;
                PRD_DSOBSERV1 = *BLANKS;
                PRD_DSOBSERV2 = *BLANKS;
                PRD_PRD01701 = $IntToDate(WDatInicio);
                PRD_PRD01702 = %DATE('9999-12-31');
                PRD_PRD01703 = 1;
                PRD_PRD01704 = 'AUTOM-CI';
                PRD_PRD01705 = 'AUTOM-CI';
                WRITE RPRDP017;

                WNuSequ += 1;
             ENDIF;
             READ FINTT027;
          ENDDO;
          CLOSE FINTT027;
       ENDSR;

       //=======================================================================
       // Subrotina.: GetRaiz
       // Objectivo.: Proceder à leitura do ficheiro de Interface
       //=======================================================================
       BEGSR GetRaiz;
          //------------------------------------------
          // Obtém o Código de Texto
          //------------------------------------------
          IF (WCdTexto = *ZEROS);
             WDatSelect = %DATE();
             WMsgErro = 'PRDXXXX';
             WRisco   = 1;
             WCdProd  = 11;
             CHAIN (WCdProd:WRisco) RCATP105;
             IF (%FOUND(FCATP105));
                WCdTexto   = CAT_CDTEXTO;
                MONITOR;
                   WCdRamo = %DEC(%TRIM(TMP_CDRAMO):2:0);
                ON-ERROR;
                   WCdRamo = *ZEROS;
                   LEAVESR;
                ENDMON;
                MONITOR;
                   WNuApol = %DEC(%TRIM(TMP_NUAPOL):7:0);
                ON-ERROR;
                   WNuApol = *ZEROS;
                   LEAVESR;
                ENDMON;

                SETLL (WCdRamo:WNuApol:WCdTexto) RPRDP01001;
                READE (WCdRamo:WNuApol:WCdTexto) RPRDP01001;
                DOW (NOT %EOF(FPRDP01001));
                   IF (PRD_PRD01001<=WDatSelect) AND (PRD_PRD01002>=WDatSelect);
                      WNuSqSup = PRD_NUSEQU;
                      WNuSequ  = $GetNextKey('FPRDP010':'NUSEQU':
                         '(CDRAMO = '''+%CHAR(WCdRamo)+''') AND +
                          (NUAPOL = '''+%CHAR(WNuApol)+''')');
                      WMsgErro = *BLANKS;
                   ENDIF;
                   READE (WCdRamo:WNuApol:WCdTexto) RPRDP01001;
                ENDDO;
             ENDIF;
          ENDIF;
       ENDSR;
      /END-FREE
