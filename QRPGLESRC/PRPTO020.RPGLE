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
     FFINTT010  IP   E           K DISK    PREFIX(INT_)
     FGREGAH    IF   E           K DISK    PREFIX(REC_)
     F                                     EXTFILE('QS36F/GREGA15')
     FTMPFILE   UF A E           K DISK    PREFIX(REC_)
     F                                     RENAME(REGA:RTMP)
     F                                     EXTFILE('QTEMP/TMPFILE')
     F*=================================================
     D*==> Campos de Trabalho
     D WCodRamo        S                   LIKE(INT_KRAMO)
     D*=================================================
      /FREE
        IF (INT_LCANUL = '00') AND (INT_LDTANU <> *ZEROS) AND
           ((INT_LEREC <> 9) AND (INT_LEREC <> 0));
           SETLL (INT_KRAMO:INT_KAPOL) REGA;
           READE (INT_KRAMO:INT_KAPOL) REGA;
           DOW (NOT %EOF(GREGAH));
              IF (REC_NREC = INT_KRECIBO);
                 WRITE RTMP;
                 IF (INT_KRAMO = 50);
                    EXSR ADD_COBERT;
                 ENDIF;
              ENDIF;
              READE (INT_KRAMO:INT_KAPOL) REGA;
           ENDDO;
        ENDIF;

        BEGSR ADD_COBERT;
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
